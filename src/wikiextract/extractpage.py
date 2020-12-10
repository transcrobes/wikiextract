#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""Wikipedia Page Extractor:
Extracts a single page from a Wikipedia dump file.
"""

import argparse
import bz2
import os.path
import re
import sys

# Program version
# FIXME: how to deal with this
# version = importlib.metadata.version("wikiextract")
version = "0.1.0"

# ----------------------------------------------------------------------
# READER

TAG_RE = re.compile(r"(.*?)<(/?\w+)[^>]*>(?:([^<]*)(<.*?>)?)?")


def process_data(input_file, article_id, templates=False):  # noqa: C901 # pylint: disable=R0912
    """
    :param input_file: name of the wikipedia dump file.
    :param id: article id
    """

    opener = bz2.BZ2File if input_file.lower().endswith("bz2") else open

    with opener(input_file) as input_filehandle:
        page = []
        for line in input_filehandle:
            # FIXME: the following may or may not be required, it is hard to tell
            line = line.decode("utf-8")
            if "<" not in line:  # faster than doing re.search()
                if page:
                    page.append(line)
                continue
            m = TAG_RE.search(line)
            if not m:
                continue
            tag = m.group(2)
            if tag == "page":
                page = []
                page.append(line)
                in_article = False
            elif tag == "id":
                curid = m.group(3)
                if article_id == curid:
                    page.append(line)
                    in_article = True
                elif not in_article and not templates:
                    page = []
            elif tag == "title":
                if templates:
                    if m.group(3).startswith("Template:"):
                        page.append(line)
                    else:
                        page = []
                else:
                    page.append(line)
            elif tag == "/page":
                if page:
                    page.append(line)
                    print("".join(page))
                    if not templates:
                        break
                page = []
            elif page:
                page.append(line)


def main():
    parser = argparse.ArgumentParser(
        prog=os.path.basename(sys.argv[0]), formatter_class=argparse.RawDescriptionHelpFormatter, description=__doc__
    )
    parser.add_argument("input", help="XML wiki dump file")
    parser.add_argument("--article_id", default="1", help="article number")
    parser.add_argument("--template", action="store_true", help="template number")
    parser.add_argument(
        "-v", "--version", action="version", version="%(prog)s " + version, help="print program version"
    )

    args = parser.parse_args()

    process_data(args.input, args.article_id, args.template)


if __name__ == "__main__":
    main()
