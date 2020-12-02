# WikiExtract

# Introduction
extractor.py is a Python script that extracts and cleans text from a [Wikipedia database dump](http://download.wikimedia.org/).

The tool is written in Python and requires Python 3.8+ but no additional library for runtime. Additional libraries are required for development (see below).

## Original WikiExtractor
This project started as a fork of https://github.com/attardi/wikiextractor/commit/87549a91a63b77c442e6592949fc965a28755d99

The original project had a large number of bugs, including Python3 syntax errors, no testing and highly variable code standards. Very simple commands from the `readme` failed after installing from PyPI. PyPI claimed that it required Python 3.6 or later but there were many Python2-only functions (`print` statements (not function), `unichr`, `cgi.unescape`, etc.). Functionality documented in the `readme` had been removed (from the code, not the `readme`).

The intention of this project is to take the codebase at commit `87549a91a63b77c442e6592949fc965a28755d99` and clean up the code and documentation so it can be used by others without each person spending several hours debugging and fixing before being able to use it, even for something basic.

Non throw-away code, particularly open source, should be tested and a minimum of care taken to avoid surprises for users.

## Cleanup Status
The code and documentation cleanup has not yet been completed and is ongoing, notably around splitting up the code to be more modular and adding tests.

Any bugs, usability issues, including code style issues, will be dealt with promptly.

# Details

`wikiextract` performs template expansion by preprocessing the whole dump and extracting template definitions.

In order to speed up processing:

- multiprocessing is used for dealing with articles in parallel
- a cache is kept of parsed templates (only useful for repeated extractions).

## Installation

however it can also be installed from `PyPi` by doing:

    pip install wikiextract

The script may be invoked directly:

    python -m wikiextract.extractor

## Usage

### wikiextract

The programme may be invoked from the project root directory with a Wikipedia dump file as an argument:

    python -m src.wikiextract.extractor <Wikipedia dump file>

You can download dump files from https://dumps.wikimedia.org (for example for Chinese https://dumps.wikimedia.org/zhwiki/latest/zhwiki-latest-pages-articles.xml.bz2)

The output is stored in several files of similar size in a directory supplied to the programme.

    usage: extractor.py [-h] [-o OUTPUT] [-b n[KMG]] [-c] [--html]
                            [-l] [-s] [--lists] [-ns ns1,ns2]
                            [--templates TEMPLATES] [--no-templates] [-r]
                            [--min_text_length MIN_TEXT_LENGTH]
                            [--filter_category path_of_categories_file]
                            [--filter_disambig_pages] [-it abbr,b,big]
                            [-de gallery,timeline,noinclude] [--keep_tables]
                            [--processes PROCESSES] [-q] [--debug] [-a] [-v]
                            [--log_file]
                            input

    Wikipedia Extractor:
    Extracts and cleans text from a Wikipedia database dump and stores output in a
    number of files of similar size in a given directory.
    Each file will contain several documents in the format:

        <doc id="" revid="" url="" title="">
            ...
            </doc>

    Template expansion requires preprocessing first the whole dump and
    collecting template definitions.

    positional arguments:
      input                 XML wiki dump file

    optional arguments:
      -h, --help            show this help message and exit
      --processes PROCESSES
                            Number of processes to use (default = nb cores - 1)

    Output:
      -o OUTPUT, --output OUTPUT
                            directory for extracted files (or '-' for dumping to
                            stdout)
      -b n[KMG], --bytes n[KMG]
                            maximum bytes per output file (default 1M)
      -c, --compress        compress output files using bzip

    Processing:
      --html                produce HTML output, subsumes --links
      -l, --links           preserve links
      -s, --sections        preserve sections
      --lists               preserve lists
      -ns ns1,ns2, --namespaces ns1,ns2
                            accepted namespaces in links
      --templates TEMPLATES
                            use or create file containing templates
      --no-templates        Do not expand templates
      -r, --revision        Include the document revision id (default=False)
      --min_text_length MIN_TEXT_LENGTH
                            Minimum expanded text length required to write
                            document (default=0)
      --filter_category path_of_categories_file
                            Include or exclude specific categories from the dataset. Specify the categories in
                            file 'path_of_categories_file'. Format:
                            One category one line, and if the line starts with:
                                1) #: Comments, ignored;
                                2) ^: the categories will be in excluding-categories
                                3) others: the categories will be in including-categories.
                            Priority:
                                1) If excluding-categories is not empty, and any category of a page exists in excluding-categories, the page will be excluded; else
                                2) If including-categories is not empty, and no category of a page exists in including-categories, the page will be excluded; else
                                3) the page will be included

      --filter_disambig_pages
                            Remove pages from output that contain disabmiguation
                            markup (default=False)
      -it abbr,b,big, --ignored_tags abbr,b,big
                            comma separated list of tags that will be dropped,
                            keeping their content
      -de gallery,timeline,noinclude, --discard_elements gallery,timeline,noinclude
                            comma separated list of elements that will be removed
                            from the article text
      --keep_tables         Preserve tables in the output article text
                            (default=False)

    Special:
      -q, --quiet           suppress reporting progress info
      --debug               print debug info
      -a, --article         analyze a file containing a single article (debug
                            option)
      -v, --version         print program version
      --log_file            specify a file to save the log information.


Saving templates to a file will speed up performing extraction the next time,
assuming template definitions have not changed.

Option --no-templates significantly speeds up the extractor, avoiding the cost
of expanding [MediaWiki templates](https://www.mediawiki.org/wiki/Help:Templates).

## Development
This project uses `pyproject.toml` and `poetry` for project management, `pre-commit` (including `black`) and `pylint` for style and static checking, and `pytest` for testing.

Install for development with `poetry install`

The `scripts` directory contains a number of useful scripts for running development tasks, such as `runlint.sh` for running `pylint`, `runtests.sh` for running `pytest`, etc.

These scripts should be launched with `poetry run scripts/script-name.sh` (after running `poetry install`).

If you find any bugs or have suggestions for feature improvements, please create a ticket. You are advised to create a ticket *before* starting to write any code, to make sure that everyone is on the same page, and so that we can avoid further forking of projects!

## License
The code is made available under the [GNU Affero General Public License v3.0](LICENSE).

This project started as a fork of https://github.com/attardi/wikiextractor/commit/87549a91a63b77c442e6592949fc965a28755d99. This project is available under the same conditions as the original code.

The original authors obviously retain copyright over their contributions.
