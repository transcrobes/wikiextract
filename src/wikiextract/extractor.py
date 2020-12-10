#!/usr/bin/env python
# -*- coding: utf-8 -*-

# FIXME: at least the global variables need to go
# pylint: disable=W0601,W0602,W0603

"""Wikipedia Extractor:
Extracts and cleans text from a Wikipedia database dump and stores output in a
number of files of similar size in a given directory.
Each file will contain several documents in the format:

    <doc id="" url="" title="">
        ...
        </doc>

This version performs template expansion by preprocesssng the whole dump and
collecting template definitions.
"""

import argparse
import bz2
import gzip
import logging
import os.path
import re
import sys
from io import BytesIO
from timeit import default_timer

from .extract import Extractor, define_template, ignore_tag

if os.getenv("WE_DEBUG", "false").lower() == "true":
    from multiprocessing.dummy import Process, Queue

    def cpu_count():
        return 1


else:
    from multiprocessing import Process, Queue, cpu_count


logger = logging.getLogger(__name__)


# ===========================================================================

# Program version
# FIXME: how to deal with this
# version = importlib.metadata.version("wikiextract")
version = "0.1.0"

##
# Defined in <siteinfo>
# We include as default Template, when loading external template file.
known_namespaces = set(["Template"])

##
# The namespace used for template definitions
# It is the name associated with namespace key=10 in the siteinfo header.
templateNamespace = ""
templatePrefix = ""

##
# The namespace used for module definitions
# It is the name associated with namespace key=828 in the siteinfo header.
moduleNamespace = ""

# This is obtained from <siteinfo>
urlbase = ""


# ----------------------------------------------------------------------
# Modules

# Only minimal support
# FIXME: import Lua modules.

modules = {
    "convert": {
        "convert": lambda x, u, *rest: x + " " + u,  # no conversion
    }
}


MIN_OUTPUT_FILE_SIZE = 200 * 1024  # Minimum size of output files

TAG_RE = re.compile(r"(.*?)<(/?\w+)[^>]*>(?:([^<]*)(<.*?>)?)?")
#                    1     2               3      4

# ------------------------------------------------------------------------------
# Output


class NextFile:
    """
    Synchronous generation of next available file name.
    """

    FILES_PER_DIR = 100

    def __init__(self, path_name):
        self.path_name = path_name
        self.dir_index = -1
        self.file_index = -1

    def next(self):
        self.file_index = (self.file_index + 1) % NextFile.FILES_PER_DIR
        if self.file_index == 0:
            self.dir_index += 1
        dirname = self._dirname()
        if not os.path.isdir(dirname):
            os.makedirs(dirname)
        return self._filepath()

    def _dirname(self):
        char1 = self.dir_index % 26
        char2 = self.dir_index / 26 % 26
        return os.path.join(self.path_name, "{}{}".format(ord("A") + char2, ord("A") + char1))

    def _filepath(self):
        return "%s/wiki_%02d" % (self._dirname(), self.file_index)


class OutputSplitter:
    """
    File-like object, that splits output to multiple files of a given max size.
    """

    def __init__(self, next_file, max_file_size=0, compress=True):
        """
        :param next_file: a NextFile object from which to obtain filenames
            to use.
        :param max_file_size: the maximum size of each file.
        :para compress: whether to write data with bzip compression.
        """
        self.next_file = next_file
        self.compress = compress
        self.max_file_size = max_file_size
        self.file = self.open(self.next_file.next())

    def reserve(self, size):
        if self.file.tell() + size > self.max_file_size:
            self.close()
            self.file = self.open(self.next_file.next())

    def write(self, data):
        self.reserve(len(data))
        self.file.write(data)

    def close(self):
        self.file.close()

    def open(self, filename):
        if self.compress:
            return bz2.BZ2File(filename + ".bz2", "wb")
        return open(filename, "wb")


def load_templates(file_handle, output_file=None):  # noqa: C901 # pylint: disable=R0912,R0915
    """
    Load templates from :param file:.
    :param output_file: file where to save templates and modules.
    """
    global templateNamespace, templatePrefix
    templatePrefix = templateNamespace + ":"
    global moduleNamespace, modulePrefix
    modulePrefix = moduleNamespace + ":"
    articles = 0
    templates = 0
    page = []
    in_text = False
    if output_file:
        output = open(output_file, "wb")
    for line in file_handle:
        if "<" not in line:  # faster than doing re.search()
            if in_text:
                page.append(line)
            continue
        m = TAG_RE.search(line)
        if not m:
            continue
        tag = m.group(2)
        if tag == "page":
            page = []
        elif tag == "title":
            title = m.group(3)
        elif tag == "text":
            in_text = True
            line = line[m.start(3) : m.end(3)]
            page.append(line)
            if m.lastindex == 4:  # open-close
                in_text = False
        elif tag == "/text":
            if m.group(1):
                page.append(m.group(1))
            in_text = False
        elif in_text:
            page.append(line)
        elif tag == "/page":
            if not output_file and not templateNamespace:  # do not know it yet
                # we reconstruct it from the first title
                colon = title.find(":")
                if colon > 1:
                    templateNamespace = title[:colon]
                    templatePrefix = title[: colon + 1]
            # FIXME: should reconstruct also moduleNamespace
            if title.startswith(templatePrefix):
                # FIXME: define_template was undefined so must have been removed by the original author
                # at some point. However, there is a "define_template" method in extract.py. Hopefully
                # this is what was meant, as I have imported it above!
                define_template(title, page)
            # save templates and modules to file
            if output_file and (title.startswith(templatePrefix) or title.startswith(modulePrefix)):
                output.write("<page>\n")
                output.write("   <title>%s</title>\n" % title.encode("utf-8"))
                output.write("   <ns>10</ns>\n")
                output.write("   <text>")
                for aline in page:
                    output.write(aline.encode("utf-8"))
                output.write("   </text>\n")
                output.write("</page>\n")
                templates += 1
            page = []
            articles += 1
            if articles % 100000 == 0:
                logger.info("Preprocessed %d pages", articles)
    if output_file:
        output.close()
        logger.info("Saved %d templates to '%s'", templates, output_file)
    return templates


def decode_open(filename, mode="rt", encoding="utf-8"):
    """
    Open a file, decode and decompress, depending on extension `gz`, or 'bz2`.
    """
    ext = os.path.splitext(filename)[1]
    if ext == ".gz":
        return gzip.open(filename, mode)
    if ext == ".bz2":
        return bz2.open(filename, mode=mode, encoding=encoding)
    return open(filename, mode, encoding=encoding)


def process_dump(  # noqa: C901
    input_file, template_file, out_file, file_size, file_compress, process_count
):  # pylint: disable=R0912,R0914,R0915
    """
    :param input_file: name of the wikipedia dump file; '-' to read from stdin
    :param template_file: optional file with template definitions.
    :param out_file: directory where to store extracted data, or '-' for stdout
    :param file_size: max size of each extracted file, or None for no max (one file)
    :param file_compress: whether to compress files with bzip.
    :param process_count: number of extraction processes to spawn.
    """
    global urlbase
    # global knownNamespaces
    global templateNamespace, templatePrefix
    global moduleNamespace, modulePrefix

    input_source = decode_open(input_file)
    # collect siteinfo
    for line in input_source:
        m = TAG_RE.search(line)
        if not m:
            continue
        tag = m.group(2)
        if tag == "base":
            # discover urlbase from the xml dump file
            # /mediawiki/siteinfo/base
            base = m.group(3)
            urlbase = base[: base.rfind("/")]
        elif tag == "namespace":
            known_namespaces.add(m.group(3))
            if re.search('key="10"', line):
                templateNamespace = m.group(3)
                templatePrefix = templateNamespace + ":"
            elif re.search('key="828"', line):
                moduleNamespace = m.group(3)
                modulePrefix = moduleNamespace + ":"
        elif tag == "/siteinfo":
            break

    if expand_templates:
        # preprocess
        template_load_start = default_timer()
        if template_file and os.path.exists(template_file):
            logger.info("Preprocessing '%s' to collect template definitions: this may take some time.", template_file)
            file_input = decode_open(template_file)
            templates = load_templates(file_input)
            file_input.close()
        else:
            if input_file == "-":
                # can't scan then reset stdin; must error w/ suggestion to specify template_file
                raise ValueError("to use templates with stdin dump, must supply explicit template-file")
            logger.info("Preprocessing '%s' to collect template definitions: this may take some time.", input_file)
            templates = load_templates(input_source, template_file)
            input_source.close()
            input_source = decode_open(input_file)
        template_load_elapsed = default_timer() - template_load_start

        # FIXME: get rid of this horrible global variable
        logger.info(
            "Loaded %d templates in %.1fs", templates, template_load_elapsed  # noqa: F821  # pylint: disable=E0602
        )

    if out_file == "-":
        output = sys.stdout
        if file_compress:
            logger.warning("writing to stdout, so no output compression (use an external tool)")
    else:
        next_file = NextFile(out_file)
        output = OutputSplitter(next_file, file_size, file_compress)
    # process pages
    logger.info("Starting page extraction from %s.", input_file)
    extract_start = default_timer()

    # Parallel Map/Reduce:
    # - pages to be processed are dispatched to workers
    # - a reduce process collects the results, sort them and print them.

    maxsize = 10 * process_count
    # output queue
    output_queue = Queue(maxsize=maxsize)

    # Reduce job that sorts and prints output
    reduce_job = Process(target=reduce_process, args=(output_queue, output))
    reduce_job.start()

    # initialize jobs queue
    jobs_queue = Queue(maxsize=maxsize)

    # start worker processes
    logger.info("Using %d extract processes.", process_count)
    workers = []
    for _ in range(max(1, process_count)):
        extractor = Process(target=extract_process, args=(jobs_queue, output_queue))
        extractor.daemon = True  # only live while parent process lives
        extractor.start()
        workers.append(extractor)

    # Mapper process

    # we collect individual lines, since str.join() is significantly faster
    # than concatenation
    page = []
    article_id = None
    last_id = None
    ordinal = 0  # page count
    in_text = False
    redirect = False
    for line in input_source:
        if "<" not in line:  # faster than doing re.search()
            if in_text:
                page.append(line)
            continue
        m = TAG_RE.search(line)
        if not m:
            continue
        tag = m.group(2)
        if tag == "page":
            page = []
            redirect = False
        elif tag == "id" and not article_id:
            article_id = m.group(3)
        elif tag == "title":
            title = m.group(3)
        elif tag == "redirect":
            redirect = True
        elif tag == "text":
            in_text = True
            line = line[m.start(3) : m.end(3)]
            page.append(line)
            if m.lastindex == 4:  # open-close
                in_text = False
        elif tag == "/text":
            if m.group(1):
                page.append(m.group(1))
            in_text = False
        elif in_text:
            page.append(line)
        elif tag == "/page":
            colon = title.find(":")
            if (
                (colon < 0 or title[:colon] in acceptedNamespaces)
                and article_id != last_id
                and not redirect
                and not title.startswith(templateNamespace)
            ):
                job = (article_id, title, page, ordinal)
                jobs_queue.put(job)  # goes to any available extract_process
                last_id = article_id
                ordinal += 1
            else:
                logger.debug(
                    f"{colon=}, {acceptedNamespaces=}, {title=}, {article_id=}, {templateNamespace=}"
                    f"{redirect=}, {(colon < 0 or title[:colon] in acceptedNamespaces)=}, {article_id != last_id=}"
                    f"{redirect=}, {title.startswith(templateNamespace)=}"
                )
            article_id = None
            page = []

    input_source.close()

    # signal termination
    for _ in workers:
        jobs_queue.put(None)
    # wait for workers to terminate
    for w in workers:
        w.join()

    # signal end of work to reduce_job process
    output_queue.put(None)
    # wait for it to finish
    reduce_job.join()

    if output != sys.stdout:
        output.close()
    extract_duration = default_timer() - extract_start
    extract_rate = ordinal / extract_duration
    logger.info(
        "Finished %d-process extraction of %d articles in %.1fs (%.1f art/s)",
        process_count,
        ordinal,
        extract_duration,
        extract_rate,
    )


# ----------------------------------------------------------------------
# Multiprocess support


def extract_process(jobs_queue, output_queue):
    """Pull tuples of raw page content, do CPU/regex-heavy fixup, push finished text
    :param jobs_queue: where to get jobs.
    :param output_queue: where to queue extracted text for output.
    """
    while True:
        job = jobs_queue.get()  # job is (id, title, page, ordinal)
        if job:
            out = BytesIO()  # memory buffer
            Extractor(*job[:3]).extract(out, escape_doc)  # (id, title, page)
            text = out.getvalue()
            output_queue.put((job[3], text))  # (ordinal, extracted_text)
            out.close()
        else:
            break


def reduce_process(output_queue, output):
    """Pull finished article text, write series of files (or stdout)
    :param output_queue: text to be output.
    :param output: file object where to print.
    """

    interval_start = default_timer()
    period = 100000
    # FIXME: use a heap
    ordering_buffer = {}  # collected pages
    next_ordinal = 0  # sequence number of pages
    while True:
        if next_ordinal in ordering_buffer:
            output.write(ordering_buffer.pop(next_ordinal))
            next_ordinal += 1
            # progress report
            if next_ordinal % period == 0:
                interval_rate = period / (default_timer() - interval_start)
                logger.info("Extracted %d articles (%.1f art/s)", next_ordinal, interval_rate)
                interval_start = default_timer()
        else:
            # mapper puts None to signal finish
            pair = output_queue.get()
            if not pair:
                break
            ordinal, text = pair
            ordering_buffer[ordinal] = text


def main():  # noqa: C901 # pylint: disable=R0912,R0914,R0915
    global urlbase, acceptedNamespaces
    global expand_templates, templateCache, escape_doc

    parser = argparse.ArgumentParser(
        prog=os.path.basename(sys.argv[0]), formatter_class=argparse.RawDescriptionHelpFormatter, description=__doc__
    )
    parser.add_argument("input", help="XML wiki dump file")
    output_group = parser.add_argument_group("Output")
    output_group.add_argument(
        "-o", "--output", default="text", help="directory for extracted files (or '-' for dumping to stdout)"
    )
    output_group.add_argument(
        "-b", "--bytes", default="1M", help="maximum bytes per output file (default %(default)s)", metavar="n[KMG]"
    )
    output_group.add_argument("-c", "--compress", action="store_true", help="compress output files using bzip")

    processing_group = parser.add_argument_group("Processing")
    processing_group.add_argument("--html", action="store_true", help="produce HTML output, subsumes --links")
    processing_group.add_argument("-l", "--links", action="store_true", help="preserve links")
    processing_group.add_argument("-ns", "--namespaces", default="", metavar="ns1,ns2", help="accepted namespaces")
    processing_group.add_argument("--templates", help="use or create file containing templates")
    processing_group.add_argument("--no-templates", action="store_false", help="Do not expand templates")
    processing_group.add_argument(
        "--escapedoc", action="store_true", help="use to escape the contents of the output <doc>...</doc>"
    )
    default_process_count = cpu_count() - 1
    parser.add_argument(
        "--processes", type=int, default=default_process_count, help="Number of processes to use (default %(default)s)"
    )

    special_group = parser.add_argument_group("Special")
    special_group.add_argument("-q", "--quiet", action="store_true", help="suppress reporting progress info")
    special_group.add_argument("--debug", action="store_true", help="print debug info")
    special_group.add_argument(
        "-a", "--article", action="store_true", help="analyze a file containing a single article (debug option)"
    )
    special_group.add_argument(
        "-v", "--version", action="version", version="%(prog)s " + version, help="print program version"
    )

    args = parser.parse_args()

    Extractor.keep_links = args.links
    Extractor.to_html = args.html
    if args.html:
        Extractor.keep_links = True

    expand_templates = args.no_templates
    escape_doc = args.escapedoc

    try:
        power = "kmg".find(args.bytes[-1].lower()) + 1
        file_size = int(args.bytes[:-1]) * 1024 ** power
        if file_size < MIN_OUTPUT_FILE_SIZE:
            raise ValueError()
    except ValueError:
        logger.error("Insufficient or invalid size: %s", args.bytes)
        return

    if args.namespaces:
        acceptedNamespaces = set(args.namespaces.split(","))
    else:
        acceptedNamespaces = []

    FORMAT = "%(levelname)s: %(message)s"
    logging.basicConfig(format=FORMAT)

    if not args.quiet:
        logger.setLevel(logging.INFO)
    if args.debug:
        logger.setLevel(logging.DEBUG)

    input_file = args.input

    if not Extractor.keep_links:
        ignore_tag("a")

    # sharing cache of parser templates is too slow:
    # manager = Manager()
    # templateCache = manager.dict()

    if args.article:
        if args.templates:
            if os.path.exists(args.templates):
                with open(args.templates) as file_handle:
                    load_templates(file_handle)

        with open(input_file) as file_handle:
            page = file_handle.read().decode("utf-8")
            m = re.search(r"<id>(.*)</id>", page)
            article_id = m.group(1) if m else 0
            m = re.search(r"<title>(.*)</title>", page)
            if m:
                title = m.group(1)
            else:
                logger.error("Missing title element")
                return
            Extractor(article_id, title, [page]).extract(sys.stdout, escape_doc)
        return

    output_path = args.output
    if output_path != "-" and not os.path.isdir(output_path):
        try:
            os.makedirs(output_path)
        except Exception:  # pylint: disable=W0703
            logger.error("Could not create: %s", output_path)
            return

    process_dump(input_file, args.templates, output_path, file_size, args.compress, args.processes)


if __name__ == "__main__":
    main()
