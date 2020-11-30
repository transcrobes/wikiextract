# -*- coding: utf-8 -*-

import argparse
import glob
import logging
import os
import sys
from operator import itemgetter
from random import randrange

from bs4 import BeautifulSoup
from sortedcollection import SortedCollection

logger = logging.getLogger(__name__)


def get_article_text(filename, index):
    with open(filename) as fh:
        xml_parser = BeautifulSoup("<root>" + fh.read() + "</root>", "xml")
        return xml_parser.findAll("doc")[index].text


# FIXME: this should be combined with string_larger_than_or_equal_to!!!
def string_shorter_than_or_equal_to(string, required_size):
    power = "kmg".find(required_size[-1].lower()) + 1
    if str(required_size)[-1].lower() in "kmg":
        return len(string.encode("utf-8")) <= (int(required_size[-1]) * 1024 ** power)
    return len(string) <= int(required_size)


def string_larger_than_or_equal_to(string, required_size):
    power = "kmg".find(required_size[-1].lower()) + 1
    if str(required_size)[-1].lower() in "kmg":
        return len(string.encode("utf-8")) >= (int(required_size[-1]) * 1024 ** power)
    return len(string) >= int(required_size)


def find_space_remaining(string, required_size):
    if required_size[-1].lower() in "kmg":
        power = "kmg".find(required_size[-1].lower()) + 1
        return (int(required_size[:-1]) * 1024 ** power) - len(string.encode("utf-8"))
    return int(required_size) - len(string)


def find_size(string, required_size):
    if required_size[-1].lower() in "kmg":
        return len(string.encode("utf-8"))
    return len(string)


def get_articles(
    input_glob, required_size, max_files, max_article_size, min_article_size, has_text, doesnt_have_text
):  # pylint: disable=R0914
    all_articles = []
    j = 0
    for f in glob.glob(input_glob):
        j += 1
        logger.debug(f"Loading file info from {f}, file no. {j}")
        if j > max_files:
            break
        with open(f) as fh:
            xml_parser = BeautifulSoup("<root>" + fh.read() + "</root>", "xml")
            articles = xml_parser.findAll("doc")
            for i, article in enumerate(articles):
                article_size = find_size(article.text, required_size)
                if max_article_size and not string_shorter_than_or_equal_to(article.text, max_article_size):
                    logger.debug(f"Ignoring article {f}:{i} due to article size" f"greater than max {max_article_size}")
                    continue
                if min_article_size and not string_larger_than_or_equal_to(article.text, min_article_size):
                    logger.debug(f"Ignoring article {f}:{i} due to article size" f"smaller than min {min_article_size}")
                    continue
                if has_text and has_text not in article.text:
                    logger.debug(f"Ignoring article {f}:{i} due to article not having text {has_text}")
                    continue
                if doesnt_have_text and doesnt_have_text in article.text:
                    logger.debug(f"Ignoring article {f}:{i} due to article having text {doesnt_have_text}")
                    continue
                all_articles.append([article_size, f, i])

    return all_articles


def process_dump(
    input_glob,
    required_size,
    output_path="",
    max_files=1000000,
    min_article_size=0,
    max_article_size=100000000,
    has_text="",
    doesnt_have_text="",
    article_separator="",
):  # pylint: disable=R0914
    all_articles = get_articles(
        input_glob, required_size, max_files, max_article_size, min_article_size, has_text, doesnt_have_text
    )
    logger.info(f"Found a total of {len(all_articles)=}")

    sorted_articles = SortedCollection(iterable=all_articles, key=itemgetter(0))

    random_texts = ""
    while True:
        space_left = find_space_remaining(random_texts, required_size)
        try:
            max_index = sorted_articles.index(sorted_articles.find_le(space_left))
            rrange = randrange(0, max_index)
            next_article = sorted_articles[rrange]
            sorted_articles.remove(next_article)
            logger.debug(f"{space_left=} {max_index=} {rrange=} {len(random_texts)=} {next_article=}")
            random_texts += get_article_text(next_article[1], next_article[2]) + article_separator
        except ValueError as ex:
            random_texts = random_texts.rstrip(article_separator)
            logger.info(ex)
            break  # we have arrived at the end

    if output_path:
        with open(output_path, "w") as output_file:
            output_file.write(random_texts)
    else:
        print(random_texts)


def main():
    parser = argparse.ArgumentParser(
        prog=os.path.basename(sys.argv[0]),
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )

    parser.add_argument("input_glob", help="Input glob to find files to process")

    output_group = parser.add_argument_group("Output")
    output_group.add_argument(
        "-o", "--output_path", default="", help="Filepath for the output file (if not supplied output to stdout)"
    )

    output_group.add_argument(
        "--required_size",
        default="100000",  # 100k chars
        help="Output size, nb of characters if an int, or (kilo|mega|giga)bytes if followed by (K|M|G)",
    )

    filtering_group = parser.add_argument_group("Filtering")
    filtering_group.add_argument(
        "--max_files", type=int, default=1000000, help="Maximum number of input files to process"
    )
    filtering_group.add_argument(
        "--min_article_size",
        default="0",
        help=(
            "Minimum article size or ignore, nb of characters if an int, "
            "or (kilo|mega|giga)bytes if followed by (K|M|G)"
        ),
    )
    filtering_group.add_argument(
        "--max_article_size",
        default="10000000",
        help=(
            "Maximum article size or ignore , nb of characters if an int, "
            "or (kilo|mega|giga)bytes if followed by (K|M|G)"
        ),
    )
    filtering_group.add_argument("--has_text", default="", help="Ignore articles unless they contain this text")
    filtering_group.add_argument("--doesnt_have_text", default="", help="Ignore articles if they contain this text")
    filtering_group.add_argument(
        "--article_separator", default="", help="Add this separator between articles in the resulting output"
    )

    logging_group = parser.add_argument_group("logging")
    logging_group.add_argument("-q", "--quiet", action="store_true", help="Suppress reporting progress info")
    logging_group.add_argument("--debug", action="store_true", help="Print debug info")

    args = parser.parse_args()

    if not args.quiet:
        logger.setLevel(logging.INFO)
    if args.debug:
        logger.setLevel(logging.DEBUG)

    process_dump(
        args.input_glob,
        required_size=args.required_size,
        output_path=args.output_path,
        max_files=args.max_files,
        min_article_size=args.min_article_size,
        max_article_size=args.max_article_size,
        has_text=args.has_text,
        doesnt_have_text=args.doesnt_have_text,
        article_separator=args.article_separator,
    )


if __name__ == "__main__":
    main()
