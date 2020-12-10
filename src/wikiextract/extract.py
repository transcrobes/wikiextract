# -*- coding: utf-8 -*-

# FIXME: split module into several
# FIXME: get rid of the global variables!
# pylint: disable=C0302,W0603

import html
import logging
import re
import time
import urllib
import urllib.parse
from html.entities import name2codepoint
from itertools import zip_longest

logger = logging.getLogger(__name__)


# match tail after wikilink
TAIL_RE = re.compile(r"\w+")
SYNTAX_HIGHLIGHT = re.compile(r"&lt;syntaxhighlight .*?&gt;(.*?)&lt;/syntaxhighlight&gt;", re.DOTALL)

# Matches bold/italic
BOLD_ITALIC = re.compile(r"'''''(.*?)'''''")
BOLD = re.compile(r"'''(.*?)'''")
ITALIC_QUOTE = re.compile(r"''\"([^\"]*?)\"''")
ITALIC = re.compile(r"''(.*?)''")
QUOTE_QUOTE = re.compile(r'""([^"]*?)""')

# Matches space
SPACES = re.compile(r" {2,}")

# Matches dots
DOTS = re.compile(r"\.{4,}")

# ======================================================================

SUBST_WORDS = "subst:|safesubst:"

RE_NO_INCLUDE = re.compile(r"<noinclude>(?:.*?)</noinclude>", re.DOTALL)
RE_INCLUDE_ONLY = re.compile(r"<includeonly>|</includeonly>", re.DOTALL)

## PARAMS ####################################################################

# This is obtained from <siteinfo>
urlbase = ""

##
# Defined in <siteinfo>
# We include as default Template, when loading external template file.
knownNamespaces = set(["Template"])

##
# Drop these elements from article text
#
DISCARD_ELEMENTS = [
    "gallery",
    "timeline",
    "noinclude",
    "pre",
    "table",
    "tr",
    "td",
    "th",
    "caption",
    "div",
    "form",
    "input",
    "select",
    "option",
    "textarea",
    "ul",
    "li",
    "ol",
    "dl",
    "dt",
    "dd",
    "menu",
    "dir",
    "ref",
    "references",
    "img",
    "imagemap",
    "source",
    "small",
]

SELF_CLOSING_TAGS = ("br", "hr", "nobr", "ref", "references", "nowiki")

# Match self closing HTML tags
# FIXME: this is buggy, see https://github.com/transcrobes/wikiextract/issues/1
SELF_CLOSING_TAG_PATTERNS = [
    re.compile(r"<\s*%s\b[^>]*/\s*>" % tag, re.DOTALL | re.IGNORECASE) for tag in SELF_CLOSING_TAGS
]

##
# Recognize only these namespaces
# w: Internal links to the Wikipedia
# wiktionary: Wiki dictionary
# wikt: shortcut for Wiktionary
#
acceptedNamespaces = ["w", "wiktionary", "wikt"]


def get_url(uid):
    return "%s?curid=%s" % (urlbase, uid)


# ======================================================================


def clean_spans(text):
    # Collect spans
    spans = []
    # Drop HTML comments
    for m in COMMENT.finditer(text):
        spans.append((m.start(), m.end()))

    # Drop self-closing tags
    for pattern in SELF_CLOSING_TAG_PATTERNS:
        for m in pattern.finditer(text):
            spans.append((m.start(), m.end()))

    # Drop ignored tags
    for left, right in ignored_tag_patterns:
        for m in left.finditer(text):
            spans.append((m.start(), m.end()))
        for m in right.finditer(text):
            spans.append((m.start(), m.end()))

    # Bulk remove all spans
    return drop_spans(spans, text)


def clean(extractor, text, expand_templates=False, escape_doc=True):  # noqa: C901
    """
    Transforms wiki markup. If the command line flag --escapedoc is set then the text is also escaped
    @see https://www.mediawiki.org/wiki/Help:Formatting
    """

    if expand_templates:
        # expand templates
        # See: http://www.mediawiki.org/wiki/Help:Templates
        text = extractor.expand_templates(text)
    else:
        # Drop transclusions (template, parser functions)
        text = drop_nested(text, r"{{", r"}}")

    # Drop tables
    text = drop_nested(text, r"{\|", r"\|}")

    # replace external links
    text = replace_external_links(text)

    # replace internal links
    text = replace_internal_links(text)

    # drop MagicWords behavioral switches
    text = MAGIC_WORDS_RE.sub("", text)

    # ############### Process HTML ###############

    # turn into HTML, except for the content of <syntaxhighlight>
    res = ""
    cur = 0
    for m in SYNTAX_HIGHLIGHT.finditer(text):
        end = m.end()
        res += unescape(text[cur : m.start()]) + m.group(1)
        cur = end
    text = res + unescape(text[cur:])

    # Handle bold/italic/quote
    if extractor.to_html:
        text = BOLD_ITALIC.sub(r"<b>\1</b>", text)
        text = BOLD.sub(r"<b>\1</b>", text)
        text = ITALIC.sub(r"<i>\1</i>", text)
    else:
        text = BOLD_ITALIC.sub(r"\1", text)
        text = BOLD.sub(r"\1", text)
        text = ITALIC_QUOTE.sub(r'"\1"', text)
        text = ITALIC.sub(r'"\1"', text)
        text = QUOTE_QUOTE.sub(r'"\1"', text)
    # residuals of unbalanced quotes
    text = text.replace("'''", "").replace("''", '"')

    text = clean_spans(text)

    # Drop discarded elements
    for atag in DISCARD_ELEMENTS:
        text = drop_nested(text, r"<\s*%s\b[^>/]*>" % atag, r"<\s*/\s*%s>" % atag)

    if not extractor.to_html:
        # Turn into text what is left (&amp;nbsp;) and <syntaxhighlight>
        text = unescape(text)

    # Expand placeholders
    for pattern, placeholder in placeholder_tag_patterns:
        index = 1
        for match in pattern.finditer(text):
            text = text.replace(match.group(), "%s_%d" % (placeholder, index))
            index += 1

    text = text.replace("<<", "«").replace(">>", "»")

    #############################################

    # Cleanup text
    text = text.replace("\t", " ")
    text = SPACES.sub(" ", text)
    text = DOTS.sub("...", text)
    text = re.sub(r" (,:\.\)\]»)", r"\1", text)
    text = re.sub(r"(\[\(«) ", r"\1", text)
    text = re.sub(r"\n\W+?\n", "\n", text, flags=re.U)  # lines with only punctuations
    text = text.replace(",,", ",").replace(",.", ".")
    if escape_doc:
        text = html.escape(text, quote=False)

    return text


# skip level 1, it is page name level
SECTION = re.compile(r"(==+)\s*(.*?)\s*\1")

LIST_OPEN = {"*": "<ul>", "#": "<ol>", ";": "<dl>", ":": "<dl>"}
LIST_CLOSE = {"*": "</ul>", "#": "</ol>", ";": "</dl>", ":": "</dl>"}
LIST_ITEM = {"*": "<li>%s</li>", "#": "<li>%s</<li>", ";": "<dt>%s</dt>", ":": "<dd>%s</dd>"}


def compact(text, mark_headers=False):  # noqa: C901 # pylint: disable=R1702,R0912,R0915
    """Deal with headers, lists, empty sections, residuals of tables.
    :param text: convert to HTML
    """

    # FIXME: this is a horribly ugly method but it is sometimes hard not to write such methods when doing
    # text processing like this. In any case, without comprehensive tests it is far too risky to refactor,
    # so leaving for the moment

    page = []  # list of paragraph
    headers = {}  # Headers for unfilled sections
    empty_section = False  # empty sections are discarded
    list_level = ""  # nesting of lists

    for line in text.split("\n"):  # pylint: disable=R1702

        if not line:
            continue
        # Handle SECTION titles
        m = SECTION.match(line)
        if m:
            title = m.group(2)
            lev = len(m.group(1))
            if Extractor.to_html:
                page.append("<h%d>%s</h%d>" % (lev, title, lev))
            if title and title[-1] not in "!?":
                title += "."

            if mark_headers:
                title = "## " + title

            headers[lev] = title
            # drop previous headers
            headers = {k: v for k, v in headers.items() if k <= lev}
            empty_section = True
            continue
        # Handle page title
        if line.startswith("++"):
            title = line[2:-2]
            if title:
                if title[-1] not in "!?":
                    title += "."
                page.append(title)
        # handle indents
        elif line[0] == ":":
            # page.append(line.lstrip(':*#;'))
            continue
        # handle lists
        elif line[0] in "*#;:":
            if Extractor.to_html:
                i = 0
                for c, n in zip_longest(list_level, line, fillvalue=""):
                    if not n or n not in "*#;:":
                        if c:
                            page.append(LIST_CLOSE[c])
                            list_level = list_level[:-1]
                            continue
                        break
                    # n != ''
                    if c != n and (not c or (c not in ";:" and n not in ";:")):
                        if c:
                            # close level
                            page.append(LIST_CLOSE[c])
                            list_level = list_level[:-1]
                        list_level += n
                        page.append(LIST_OPEN[n])
                    i += 1
                n = line[i - 1]  # last list char
                line = line[i:].strip()
                if line:  # FIXME: n is '"'
                    page.append(LIST_ITEM[n] % line)
            else:
                continue
        elif len(list_level) > 0:
            for c in reversed(list_level):
                page.append(LIST_CLOSE[c])
            list_level = []

        # Drop residuals of lists
        elif line[0] in "{|" or line[-1] == "}":
            continue
        # Drop irrelevant lines
        elif (line[0] == "(" and line[-1] == ")") or line.strip(".-") == "":
            continue
        elif len(headers) > 0:
            if Extractor.keep_sections:
                items = sorted(headers.items())
                for (i, v) in items:
                    page.append(v)
            headers.clear()
            page.append(line)  # first line
            empty_section = False
        elif not empty_section:
            page.append(line)
            # dangerous
            # # Drop preformatted
            # elif line[0] == ' ':
            #     continue

    return page


def drop_nested(text, open_delimiter, close_delimiter):  # noqa: C901 # pylint: disable=R0912
    """
    A matching function for nested expressions, e.g. namespaces and tables.
    """
    open_re = re.compile(open_delimiter, re.IGNORECASE)
    close_re = re.compile(close_delimiter, re.IGNORECASE)
    # partition text in separate blocks { } { }
    spans = []  # pairs (s, e) for each partition
    nest = 0  # nesting level
    start = open_re.search(text, 0)
    if not start:
        return text
    end = close_re.search(text, start.end())
    next_value = start
    while end:
        next_value = open_re.search(text, next_value.end())
        if not next_value:  # termination
            while nest:  # close all pending
                nest -= 1
                end0 = close_re.search(text, end.end())
                if end0:
                    end = end0
                else:
                    break
            spans.append((start.start(), end.end()))
            break
        while end.end() < next_value.start():
            # { } {
            if nest:
                nest -= 1
                # try closing more
                last = end.end()
                end = close_re.search(text, end.end())
                if not end:  # unbalanced
                    if spans:
                        span = (spans[0][0], last)
                    else:
                        span = (start.start(), last)
                    spans = [span]
                    break
            else:
                spans.append((start.start(), end.end()))
                # advance start, find next close
                start = next_value
                end = close_re.search(text, next_value.end())
                break  # { }
        if next_value != start:
            # { { }
            nest += 1
    # collect text outside partitions
    return drop_spans(spans, text)


def drop_spans(spans, text):
    """
    Drop from text the blocks identified in :param spans:, possibly nested.
    """
    spans.sort()
    res = ""
    offset = 0
    for s, e in spans:
        if offset <= s:  # handle nesting
            if offset < s:
                res += text[offset:s]
            offset = e
    res += text[offset:]
    return res


# ----------------------------------------------------------------------
# External links

# from: https://doc.wikimedia.org/mediawiki-core/master/php/DefaultSettings_8php_source.html

WG_URL_PROTOCOLS = [
    "bitcoin:",
    "ftp://",
    "ftps://",
    "geo:",
    "git://",
    "gopher://",
    "http://",
    "https://",
    "irc://",
    "ircs://",
    "magnet:",
    "mailto:",
    "mms://",
    "news:",
    "nntp://",
    "redis://",
    "sftp://",
    "sip:",
    "sips:",
    "sms:",
    "ssh://",
    "svn://",
    "tel:",
    "telnet://",
    "urn:",
    "worldwind://",
    "xmpp:",
    "//",
]

# from: https://doc.wikimedia.org/mediawiki-core/master/php/Parser_8php_source.html

# Constants needed for external link processing
# Everything except bracket, space, or control characters
# \p{Zs} is unicode 'separator, space' category. It covers the space 0x20
# as well as U+3000 is IDEOGRAPHIC SPACE for bug 19052
EXT_LINK_URL_CLASS = r'[^][<>"\x00-\x20\x7F\s]'
EXT_LINK_BRACKETED_REGEX = re.compile(
    r"\[(((?i)" + "|".join(WG_URL_PROTOCOLS) + ")" + EXT_LINK_URL_CLASS + r"+)\s*([^\]\x00-\x08\x0a-\x1F]*?)\]",
    re.S | re.U,
)
EXT_IMAGE_REGEX = re.compile(
    r"""^(http://|https://)([^][<>"\x00-\x20\x7F\s]+)
    /([A-Za-z0-9_.,~%\-+&;#*?!=()@\x80-\xFF]+)\.((?i)gif|png|jpg|jpeg)$""",
    re.X | re.S | re.U,
)


def replace_external_links(text):
    s = ""
    current = 0
    for m in EXT_LINK_BRACKETED_REGEX.finditer(text):
        s += text[current : m.start()]
        current = m.end()

        url = m.group(1)
        label = m.group(3)

        # # The characters '<' and '>' (which were escaped by
        # # removeHTMLtags()) should not be included in
        # # URLs, per RFC 2396.
        # m2 = re.search('&(lt|gt);', url)
        # if m2:
        #     link = url[m2.end():] + ' ' + link
        #     url = url[0:m2.end()]

        # If the link text is an image URL, replace it with an <img> tag
        # This happened by accident in the original parser, but some people used it extensively
        m = EXT_IMAGE_REGEX.match(label)
        if m:
            label = make_external_image(label)

        # Use the encoded URL
        # This means that users can paste URLs directly into the text
        # Funny characters like ö aren't valid in URLs anyway
        # This was changed in August 2004
        s += make_external_link(url, label)  # + trail

    return s + text[current:]


def make_external_link(url, anchor):
    """Function applied to wikiLinks"""
    if Extractor.keep_links:
        return '<a href="%s">%s</a>' % (urllib.parse.quote(url), anchor)
    return anchor


def make_external_image(url, alt=""):
    if Extractor.keep_links:
        return '<img src="%s" alt="%s">' % (url, alt)
    return alt


# ----------------------------------------------------------------------
# WikiLinks
# See https://www.mediawiki.org/wiki/Help:Links#Internal_links

# Can be nested [[File:..|..[[..]]..|..]], [[Category:...]], etc.
# Also: [[Help:IPA for Catalan|[andora]]]


def replace_internal_links(text):  # pylint: disable=R0914
    """
    Replaces external links of the form:
    [[title |...|label]]trail

    with title concatenated with trail, when present, e.g. 's' for plural.
    """
    # call this after removal of external links, so we need not worry about
    # triple closing ]]].
    current = 0
    result = ""
    for s, e in find_balanced(text, ["[["], ["]]"]):
        m = TAIL_RE.match(text, e)
        if m:
            trail = m.group(0)
            end = m.end()
        else:
            trail = ""
            end = e
        inner = text[s + 2 : e - 2]
        # find first |
        pipe = inner.find("|")
        if pipe < 0:
            title = inner
            label = title
        else:
            title = inner[:pipe].rstrip()
            # find last |
            curp = pipe + 1
            for s1, e1 in find_balanced(inner, ["[["], ["]]"]):
                last = inner.rfind("|", curp, s1)
                if last >= 0:
                    pipe = last  # advance
                curp = e1
            label = inner[pipe + 1 :].strip()
        result += text[current:s] + makeInternalLink(title, label) + trail
        current = end
    return result + text[current:]


def makeInternalLink(title, label):
    colon = title.find(":")
    if colon > 0 and title[:colon] not in acceptedNamespaces:
        return ""
    if colon == 0:
        # drop also :File:
        colon2 = title.find(":", colon + 1)
        if colon2 > 1 and title[colon + 1 : colon2] not in acceptedNamespaces:
            return ""
    if Extractor.keep_links:
        return '<a href="%s">%s</a>' % (urllib.parse.quote(title.encode("utf-8")), label)
    return label


# ----------------------------------------------------------------------
# variables


class MagicWords:

    """
    One copy in each Extractor.

    @see https://doc.wikimedia.org/mediawiki-core/master/php/MagicWord_8php_source.html
    """

    names = [
        "!",
        "currentmonth",
        "currentmonth1",
        "currentmonthname",
        "currentmonthnamegen",
        "currentmonthabbrev",
        "currentday",
        "currentday2",
        "currentdayname",
        "currentyear",
        "currenttime",
        "currenthour",
        "localmonth",
        "localmonth1",
        "localmonthname",
        "localmonthnamegen",
        "localmonthabbrev",
        "localday",
        "localday2",
        "localdayname",
        "localyear",
        "localtime",
        "localhour",
        "numberofarticles",
        "numberoffiles",
        "numberofedits",
        "articlepath",
        "pageid",
        "sitename",
        "server",
        "servername",
        "scriptpath",
        "stylepath",
        "pagename",
        "pagenamee",
        "fullpagename",
        "fullpagenamee",
        "namespace",
        "namespacee",
        "namespacenumber",
        "currentweek",
        "currentdow",
        "localweek",
        "localdow",
        "revisionid",
        "revisionday",
        "revisionday2",
        "revisionmonth",
        "revisionmonth1",
        "revisionyear",
        "revisiontimestamp",
        "revisionuser",
        "revisionsize",
        "subpagename",
        "subpagenamee",
        "talkspace",
        "talkspacee",
        "subjectspace",
        "subjectspacee",
        "talkpagename",
        "talkpagenamee",
        "subjectpagename",
        "subjectpagenamee",
        "numberofusers",
        "numberofactiveusers",
        "numberofpages",
        "currentversion",
        "rootpagename",
        "rootpagenamee",
        "basepagename",
        "basepagenamee",
        "currenttimestamp",
        "localtimestamp",
        "directionmark",
        "contentlanguage",
        "numberofadmins",
        "cascadingsources",
    ]

    def __init__(self):
        self.values = {"!": "|"}

    def __getitem__(self, name):
        return self.values.get(name)

    def __setitem__(self, name, value):
        self.values[name] = value

    switches = (
        "__NOTOC__",
        "__FORCETOC__",
        "__TOC__",
        "__TOC__",
        "__NEWSECTIONLINK__",
        "__NONEWSECTIONLINK__",
        "__NOGALLERY__",
        "__HIDDENCAT__",
        "__NOCONTENTCONVERT__",
        "__NOCC__",
        "__NOTITLECONVERT__",
        "__NOTC__",
        "__START__",
        "__END__",
        "__INDEX__",
        "__NOINDEX__",
        "__STATICREDIRECT__",
        "__DISAMBIG__",
    )


MAGIC_WORDS_RE = re.compile("|".join(MagicWords.switches))


# =========================================================================
#
# MediaWiki Markup Grammar
# https://www.mediawiki.org/wiki/Preprocessor_ABNF

# xml-char = %x9 / %xA / %xD / %x20-D7FF / %xE000-FFFD / %x10000-10FFFF
# sptab = SP / HTAB

# ; everything except ">" (%x3E)
# attr-char = %x9 / %xA / %xD / %x20-3D / %x3F-D7FF / %xE000-FFFD / %x10000-10FFFF

# literal         = *xml-char
# title           = wikitext-L3
# part-name       = wikitext-L3
# part-value      = wikitext-L3
# part            = ( part-name "=" part-value ) / ( part-value )
# parts           = [ title *( "|" part ) ]
# tplarg          = "{{{" parts "}}}"
# template        = "{{" parts "}}"
# link            = "[[" wikitext-L3 "]]"

# comment         = "<!--" literal "-->"
# unclosed-comment = "<!--" literal END
# ; the + in the line-eating-comment rule was absent between MW 1.12 and MW 1.22
# line-eating-comment = LF LINE-START *SP +( comment *SP ) LINE-END

# attr            = *attr-char
# nowiki-element  = "<nowiki" attr ( "/>" / ( ">" literal ( "</nowiki>" / END ) ) )

# wikitext-L2     = heading / wikitext-L3 / *wikitext-L2
# wikitext-L3     = literal / template / tplarg / link / comment /
#                   line-eating-comment / unclosed-comment / xmlish-element /
#                   *wikitext-L3

# ------------------------------------------------------------------------------

# These tags are dropped, keeping their content.
# handle 'a' separately, depending on keep_links
IGNORED_TAGS = (
    "abbr",
    "b",
    "big",
    "blockquote",
    "center",
    "cite",
    "div",
    "em",
    "font",
    "h1",
    "h2",
    "h3",
    "h4",
    "hiero",
    "i",
    "kbd",
    "nowiki",
    "p",
    "plaintext",
    "s",
    "span",
    "strike",
    "strong",
    "sub",
    "sup",
    "tt",
    "u",
    "var",
)


# Match HTML comments
# The buggy template {{Template:T}} has a comment terminating with just "->"
COMMENT = re.compile(r"<!--.*?-->", re.DOTALL)

# Match ignored tags
ignored_tag_patterns = []


def ignore_tag(tag):
    left = re.compile(r"<%s\b.*?>" % tag, re.IGNORECASE | re.DOTALL)  # both <ref> and <reference>
    right = re.compile(r"</\s*%s>" % tag, re.IGNORECASE)
    ignored_tag_patterns.append((left, right))


for gtag in IGNORED_TAGS:
    ignore_tag(gtag)

placeholder_tags = {"math": "formula", "code": "codice"}


def normalize_title(title):
    """Normalize title"""
    # remove leading/trailing whitespace and underscores
    title = title.strip(" _")
    # replace sequences of whitespace and underscore chars with a single space
    title = re.sub(r"[\s_]+", " ", title)

    m = re.match(r"([^:]*):(\s*)(\S(?:.*))", title)
    if m:
        prefix = m.group(1)
        if m.group(2):
            optional_whitespace = " "
        else:
            optional_whitespace = ""
        rest = m.group(3)

        ns = normalize_namespace(prefix)
        if ns in knownNamespaces:
            # If the prefix designates a known namespace, then it might be
            # followed by optional whitespace that should be removed to get
            # the canonical page name
            # (e.g., "Category:  Births" should become "Category:Births").
            title = ns + ":" + ucfirst(rest)
        else:
            # No namespace, just capitalize first letter.
            # If the part before the colon is not a known namespace, then we
            # must not remove the space after the colon (if any), e.g.,
            # "3001: The_Final_Odyssey" != "3001:The_Final_Odyssey".
            # However, to get the canonical page name we must contract multiple
            # spaces into one, because
            # "3001:   The_Final_Odyssey" != "3001: The_Final_Odyssey".
            title = ucfirst(prefix) + ":" + optional_whitespace + ucfirst(rest)
    else:
        # no namespace, just capitalize first letter
        title = ucfirst(title)
    return title


def unescape(text):
    """
    Removes HTML or XML character references and entities from a text string.

    :param text The HTML (or XML) source text.
    :return The plain text, as a Unicode string, if necessary.
    """

    def fixup(m):
        text = m.group(0)
        code = m.group(1)
        try:
            if text[1] == "#":  # character reference
                if text[2] == "x":
                    return chr(int(code[1:], 16))
                return chr(int(code))
            # named entity
            return chr(name2codepoint[code])
        except Exception:  # pylint: disable=W0703
            logger.warning(f"there is a bug trying to convert {text}")
            return text  # leave as is

    return re.sub(r"&#?(\w+);", fixup, text)


# Match HTML placeholder tags
placeholder_tag_patterns = [
    (re.compile(r"<\s*%s(\s*| [^>]+?)>.*?<\s*/\s*%s\s*>" % (tag, tag), re.DOTALL | re.IGNORECASE), repl)
    for tag, repl in placeholder_tags.items()
]


class Extractor:  # pylint: disable=R0902

    """
    An extraction task on a article.
    """

    ##
    # Whether to preserve links in output
    keep_links = False

    ##
    # Whether to preserve section titles
    keep_sections = True

    ##
    # Whether to output HTML instead of text
    to_html = False

    def __init__(self, article_id, title, page):
        """
        :param page: a list of lines.
        """
        self.id = article_id
        self.title = title
        self.page = page
        self.magic_words = MagicWords()
        self.frame = []
        self.recursion_exceeded_1_errs = 0  # template recursion within expand_templates()
        self.recursion_exceeded_2_errs = 0  # template recursion within expand_template()
        self.recursion_exceeded_3_errs = 0  # parameter recursion
        self.template_title_errs = 0

    def clean_text(self, text, mark_headers=False, expand_templates=False, escape_doc=True):
        """
        :param mark_headers: True to distinguish headers from paragraphs
          e.g. "## Section 1"
        """
        self.magic_words["pagename"] = self.title
        self.magic_words["fullpagename"] = self.title
        self.magic_words["currentyear"] = time.strftime("%Y")
        self.magic_words["currentmonth"] = time.strftime("%m")
        self.magic_words["currentday"] = time.strftime("%d")
        self.magic_words["currenthour"] = time.strftime("%H")
        self.magic_words["currenttime"] = time.strftime("%H:%M:%S")

        text = clean(self, text, expand_templates=expand_templates, escape_doc=escape_doc)

        text = compact(text, mark_headers=mark_headers)
        return text

    def extract(self, out, escape_doc=True):
        """
        :param out: a memory file.
        """
        logger.debug("%s\t%s", self.id, self.title)
        text = "".join(self.page)

        url = get_url(self.id)
        header = '<doc id="%s" url="%s" title="%s">\n' % (self.id, url, self.title)
        # Separate header from text with a newline.
        header += self.title + "\n\n"
        footer = "\n</doc>\n"
        out.write(header)

        text = self.clean_text(text, escape_doc=escape_doc)

        for line in text:
            out.write(line)
            out.write("\n")
        out.write(footer)
        errs = (
            self.template_title_errs,
            self.recursion_exceeded_1_errs,
            self.recursion_exceeded_2_errs,
            self.recursion_exceeded_3_errs,
        )
        if any(errs):
            logger.warning(
                "Template errors in article '%s' (%s): title(%d) recursion(%d, %d, %d)", self.title, self.id, *errs
            )

    # ----------------------------------------------------------------------
    # Expand templates

    MAX_TEMPLATE_RECURSION_LEVELS = 30

    # check for template beginning
    re_open = re.compile("(?<!{){{(?!{)", re.DOTALL)

    def expand_templates(self, wikitext):
        """
        :param wikitext: the text to be expanded.

        Templates are frequently nested. Occasionally, parsing mistakes may
        cause template insertion to enter an infinite loop, for instance when
        trying to instantiate Template:Country

        {{country_{{{1}}}|{{{2}}}|{{{2}}}|size={{{size|}}}|name={{{name|}}}}}

        which is repeatedly trying to insert template 'country_', which is
        again resolved to Template:Country. The straightforward solution of
        keeping track of templates that were already inserted for the current
        article would not work, because the same template may legally be used
        more than once, with different parameters in different parts of the
        article.  Therefore, we limit the number of iterations of nested
        template inclusion.

        """
        # Test template expansion at:
        # https://en.wikipedia.org/wiki/Special:ExpandTemplates

        res = ""
        if len(self.frame) >= self.MAX_TEMPLATE_RECURSION_LEVELS:
            self.recursion_exceeded_1_errs += 1
            return res

        # logger.debug('<expand_templates ' + str(len(self.frame)))

        cur = 0
        # look for matching {{...}}
        for s, e in find_matching_braces(wikitext, 2):
            res += wikitext[cur:s] + self.expand_template(wikitext[s + 2 : e - 2])
            cur = e
        # leftover
        res += wikitext[cur:]
        # logger.debug('   expand_templates> %d %s', len(self.frame), res)
        return res

    @staticmethod
    def template_paramaters(parameters):
        """
        Build a dictionary with positional or name key to expanded parameters.
        :param parameters: the parts[1:] of a template, i.e. all except the title.
        """

        if not parameters:
            return {}
        logger.debug("<template_paramaters: %s", "|".join(parameters))

        # Parameters can be either named or unnamed. In the latter case, their
        # name is defined by their ordinal position (1, 2, 3, ...).

        unnamed_parameter_counter = 0

        # It's legal for unnamed parameters to be skipped, in which case they
        # will get default values (if available) during actual instantiation.
        # That is {{template_name|a||c}} means parameter 1 gets
        # the value 'a', parameter 2 value is not defined, and parameter 3 gets
        # the value 'c'.  This case is correctly handled by function 'split',
        # and does not require any special handling.
        out_parameters = {}

        for paramater in parameters:
            # Spaces before or after a parameter value are normally ignored,
            # UNLESS the parameter contains a link (to prevent possible gluing
            # the link to the following text after template substitution)

            # Parameter values may contain "=" symbols, hence the parameter
            # name extends up to the first such symbol.

            # It is legal for a parameter to be specified several times, in
            # which case the last assignment takes precedence. Example:
            # "{{t|a|b|c|2=B}}" is equivalent to "{{t|a|B|c}}".
            # Therefore, we don't check if the parameter has been assigned a
            # value before, because anyway the last assignment should override
            # any previous ones.
            # FIXME: Don't use DOTALL here since parameters may be tags with
            # attributes, e.g. <div class="templatequotecite">
            # Parameters may span several lines, like:
            # {{Reflist|colwidth=30em|refs=
            # &lt;ref name=&quot;Goode&quot;&gt;Title&lt;/ref&gt;

            # The '=' might occurr within an HTML attribute:
            #   "&lt;ref name=value"
            # but we stop at first.
            m = re.match(" *([^=]*?) *=(.*)", paramater, re.DOTALL)
            if m:
                # This is a named parameter.  This case also handles parameter
                # assignments like "2=xxx", where the number of an unnamed
                # parameter ("2") is specified explicitly - this is handled
                # transparently.

                parameter_name = m.group(1).strip()
                parameter_value = m.group(2)

                if "]]" not in parameter_value:  # if the value does not contain a link, trim whitespace
                    parameter_value = parameter_value.strip()
                out_parameters[parameter_name] = parameter_value
            else:
                # this is an unnamed parameter
                unnamed_parameter_counter += 1

                if "]]" not in paramater:  # if the value does not contain a link, trim whitespace
                    paramater = paramater.strip()
                out_parameters[str(unnamed_parameter_counter)] = paramater
        logger.debug("   template_paramaters> %s", "|".join(out_parameters.values()))
        return out_parameters

    def expand_template(self, body):
        """Expands template invocation.
        :param body: the parts of a template.

        :see http://meta.wikimedia.org/wiki/Help:Expansion for an explanation
        of the process.

        See in particular: Expansion of names and values
        http://meta.wikimedia.org/wiki/Help:Expansion#Expansion_of_names_and_values

        For most parser functions all names and values are expanded,
        regardless of what is relevant for the result. The branching functions
        (#if, #ifeq, #iferror, #ifexist, #ifexpr, #switch) are exceptions.

        All names in a template call are expanded, and the titles of the
        tplargs in the template body, after which it is determined which
        values must be expanded, and for which tplargs in the template body
        the first part (default).

        In the case of a tplarg, any parts beyond the first are never
        expanded.  The possible name and the value of the first part is
        expanded if the title does not match a name in the template call.

        :see code for braceSubstitution at
        https://doc.wikimedia.org/mediawiki-core/master/php/html/Parser_8php_source.html#3397:

        """

        # template        = "{{" parts "}}"

        # Templates and tplargs are decomposed in the same way, with pipes as
        # separator, even though eventually any parts in a tplarg after the first
        # (the parameter default) are ignored, and an equals sign in the first
        # part is treated as plain text.
        # Pipes inside inner templates and tplargs, or inside double rectangular
        # brackets within the template or tplargs are not taken into account in
        # this decomposition.
        # The first part is called title, the other parts are simply called parts.

        # If a part has one or more equals signs in it, the first equals sign
        # determines the division into name = value. Equals signs inside inner
        # templates and tplargs, or inside double rectangular brackets within the
        # part are not taken into account in this decomposition. Parts without
        # equals sign are indexed 1, 2, .., given as attribute in the <name> tag.

        if len(self.frame) >= self.MAX_TEMPLATE_RECURSION_LEVELS:
            self.recursion_exceeded_2_errs += 1
            # logger.debug('   INVOCATION> %d %s', len(self.frame), body)
            return ""

        logger.debug("INVOCATION %d %s", len(self.frame), body)

        parts = split_parts(body)
        # title is the portion before the first |
        logger.debug("TITLE %s", parts[0].strip())
        title = self.expand_templates(parts[0].strip())

        # SUBST
        # Apply the template tag to parameters without
        # substituting into them, e.g.
        # {{subst:t|a{{{p|q}}}b}} gives the wikitext start-a{{{p|q}}}b-end
        # @see https://www.mediawiki.org/wiki/Manual:Substitution#Partial_substitution
        subst = False
        if re.match(SUBST_WORDS, title, re.IGNORECASE):
            title = re.sub(SUBST_WORDS, "", title, 1, re.IGNORECASE)
            subst = True

        if title.lower() in self.magic_words.values:
            return self.magic_words[title.lower()]

        # Parser functions
        # The first argument is everything after the first colon.
        # It has been evaluated above.
        colon = title.find(":")
        if colon > 1:
            funct = title[:colon]
            parts[0] = title[colon + 1 :].strip()  # side-effect (parts[0] not used later)
            # arguments after first are not evaluated
            ret = call_parser_function(funct, parts, self.frame)
            return self.expand_templates(ret)

        title = fully_qualified_template_title(title)
        if not title:
            self.template_title_errs += 1
            return ""

        redirected = redirects.get(title)
        if redirected:
            title = redirected

        # get the template
        if title in templateCache:
            template = templateCache[title]
        elif title in templates:
            # FIXME: The "Template" class (module?) was deleted at some point by
            # the original author and this wasn't cleaned up. This looks like it might be useful though
            # so we should probably look into the original project history to find out when it was removed
            # and consider adding it back again.
            #
            # template = Template.parse(templates[title])
            # add it to cache
            # templateCache[title] = template
            # del templates[title]
            raise NotImplementedError
        else:
            # The page being included could not be identified
            return ""

        # logger.debug('TEMPLATE %s: %s', title, template)

        # tplarg          = "{{{" parts "}}}"
        # parts           = [ title *( "|" part ) ]
        # part            = ( part-name "=" part-value ) / ( part-value )
        # part-name       = wikitext-L3
        # part-value      = wikitext-L3
        # wikitext-L3     = literal / template / tplarg / link / comment /
        #                   line-eating-comment / unclosed-comment /
        #           	    xmlish-element / *wikitext-L3

        # A tplarg may contain other parameters as well as templates, e.g.:
        #   {{{text|{{{quote|{{{1|{{error|Error: No text given}}}}}}}}}}}
        # hence no simple RE like this would work:
        #   '{{{((?:(?!{{{).)*?)}}}'
        # We must use full CF parsing.

        # the parameter name itself might be computed, e.g.:
        #   {{{appointe{{#if:{{{appointer14|}}}|r|d}}14|}}}

        # Because of the multiple uses of double-brace and triple-brace
        # syntax, expressions can sometimes be ambiguous.
        # Precedence rules specifed here:
        # http://www.mediawiki.org/wiki/Preprocessor_ABNF#Ideal_precedence
        # resolve ambiguities like this:
        #   {{{{ }}}} -> { {{{ }}} }
        #   {{{{{ }}}}} -> {{ {{{ }}} }}
        #
        # :see: https://en.wikipedia.org/wiki/Help:Template#Handling_parameters

        params = parts[1:]

        if not subst:
            # Evaluate parameters, since they may contain templates, including
            # the symbol "=".
            # {{#ifexpr: {{{1}}} = 1 }}
            params = [self.expand_templates(p) for p in params]

        # build a dict of name-values for the parameter values
        params = self.template_paramaters(params)

        # Perform parameter substitution
        # extend frame before subst, since there may be recursion in default
        # parameter value, e.g. {{OTRS|celebrative|date=April 2015}} in article
        # 21637542 in enwiki.
        self.frame.append((title, params))
        instantiated = template.subst(params, self)
        # logger.debug('instantiated %d %s', len(self.frame), instantiated)
        value = self.expand_templates(instantiated)
        self.frame.pop()
        # logger.debug('   INVOCATION> %s %d %s', title, len(self.frame), value)
        return value


# ----------------------------------------------------------------------
# parameter handling


def split_parts(params_list):
    """
    :param params_list: the parts of a template or tplarg.

    Split template parameters at the separator "|".
    separator "=".

    Template parameters often contain URLs, internal links, text or even
    template expressions, since we evaluate templates outside in.
    This is required for cases like:
      {{#if: {{{1}}} | {{lc:{{{1}}} | "parameter missing"}}
    Parameters are separated by "|" symbols. However, we
    cannot simply split the string on "|" symbols, since these
    also appear inside templates and internal links, e.g.

     {{if:|
      |{{#if:the president|
           |{{#if:|
               [[Category:Hatnote templates|A{{PAGENAME}}]]
            }}
       }}
     }}

    We split parts at the "|" symbols that are not inside any pair
    {{{...}}}, {{...}}, [[...]], {|...|}.
    """

    # Must consider '[' as normal in expansion of Template:EMedicine2:
    # #ifeq: ped|article|[http://emedicine.medscape.com/article/180-overview|[http://www.emedicine.com/ped/topic180.htm#{{#if: |section~}}  # noqa: E501 # pylint: disable=C0301
    # as part of:
    # {{#ifeq: ped|article|[http://emedicine.medscape.com/article/180-overview|[http://www.emedicine.com/ped/topic180.htm#{{#if: |section~}}}} ped/180{{#if: |~}}]  # noqa: E501 # pylint: disable=C0301

    # should handle both tpl arg like:
    #    4|{{{{{subst|}}}CURRENTYEAR}}
    # and tpl parameters like:
    #    ||[[Category:People|{{#if:A|A|{{PAGENAME}}}}]]

    sep = "|"
    parameters = []
    cur = 0
    for s, e in find_matching_braces(params_list):
        par = params_list[cur:s].split(sep)
        if par:
            if parameters:
                # portion before | belongs to previous parameter
                parameters[-1] += par[0]
                if len(par) > 1:
                    # rest are new parameters
                    parameters.extend(par[1:])
            else:
                parameters = par
        elif not parameters:
            parameters = [""]  # create first param
        # add span to last previous parameter
        parameters[-1] += params_list[s:e]
        cur = e
    # leftover
    par = params_list[cur:].split(sep)
    if par:
        if parameters:
            # portion before | belongs to previous parameter
            parameters[-1] += par[0]
            if len(par) > 1:
                # rest are new parameters
                parameters.extend(par[1:])
        else:
            parameters = par

    # logger.debug('split_parts %s %s\nparams: %s', sep, params_list, str(parameters))
    return parameters


def find_matching_braces(text, ldelim=0):  # noqa: C901 # pylint: disable=R0912,R0915
    """
    :param ldelim: number of braces to match. 0 means match [[]], {{}} and {{{}}}.
    """
    # Parsing is done with respect to pairs of double braces {{..}} delimiting
    # a template, and pairs of triple braces {{{..}}} delimiting a tplarg.
    # If double opening braces are followed by triple closing braces or
    # conversely, this is taken as delimiting a template, with one left-over
    # brace outside it, taken as plain text. For any pattern of braces this
    # defines a set of templates and tplargs such that any two are either
    # separate or nested (not overlapping).

    # Unmatched double rectangular closing brackets can be in a template or
    # tplarg, but unmatched double rectangular opening brackets cannot.
    # Unmatched double or triple closing braces inside a pair of
    # double rectangular brackets are treated as plain text.
    # Other formulation: in ambiguity between template or tplarg on one hand,
    # and a link on the other hand, the structure with the rightmost opening
    # takes precedence, even if this is the opening of a link without any
    # closing, so not producing an actual link.

    # In the case of more than three opening braces the last three are assumed
    # to belong to a tplarg, unless there is no matching triple of closing
    # braces, in which case the last two opening braces are are assumed to
    # belong to a template.

    # We must skip individual { like in:
    #   {{#ifeq: {{padleft:|1|}} | { | | &nbsp;}}
    # We must resolve ambiguities like this:
    #   {{{{ }}}} -> { {{{ }}} }
    #   {{{{{ }}}}} -> {{ {{{ }}} }}
    #   {{#if:{{{{{#if:{{{nominee|}}}|nominee|candidate}}|}}}|...}}

    # Handle:
    #   {{{{{|safesubst:}}}#Invoke:String|replace|{{{1|{{{{{|safesubst:}}}PAGENAME}}}}}|%s+%([^%(]-%)$||plain=false}}
    # as well as expressions with stray }:
    #   {{{link|{{ucfirst:{{{1}}}}}} interchange}}}

    if ldelim:  # 2-3
        re_open = re.compile("[{]{%d,}" % ldelim)  # at least ldelim
        re_next = re.compile("[{]{2,}|}{2,}")  # at least 2
    else:
        re_open = re.compile(r"{{2,}|\[{2,}")
        re_next = re.compile(r"{{2,}|}{2,}|\[{2,}|]{2,}")  # at least 2

    cur = 0
    while True:  # pylint: disable=R1702
        m1 = re_open.search(text, cur)
        if not m1:
            return
        lmatch = m1.end() - m1.start()
        if m1.group()[0] == "{":
            stack = [lmatch]  # stack of opening braces lengths
        else:
            stack = [-lmatch]  # negative means [
        end = m1.end()
        while True:
            m2 = re_next.search(text, end)
            if not m2:
                return  # unbalanced
            end = m2.end()
            brac = m2.group()[0]
            lmatch = m2.end() - m2.start()

            if brac == "{":
                stack.append(lmatch)
            elif brac == "}":
                while stack:
                    open_count = stack.pop()  # opening span
                    if open_count == 0:  # illegal unmatched [[
                        continue
                    if lmatch >= open_count:
                        lmatch -= open_count
                        if lmatch <= 1:  # either close or stray }
                            break
                    else:
                        # put back unmatched
                        stack.append(open_count - lmatch)
                        break
                if not stack:
                    yield m1.start(), end - lmatch
                    cur = end
                    break
                if len(stack) == 1 and 0 < stack[0] < ldelim:
                    # ambiguous {{{{{ }}} }}
                    yield m1.start() + stack[0], end
                    cur = end
                    break
            elif brac == "[":  # [[
                stack.append(-lmatch)
            else:  # ]]
                while stack and stack[-1] < 0:  # matching [[
                    open_count = -stack.pop()
                    if lmatch >= open_count:
                        lmatch -= open_count
                        if lmatch <= 1:  # either close or stray ]
                            break
                    else:
                        # put back unmatched (negative)
                        stack.append(lmatch - open_count)
                        break
                if not stack:
                    yield m1.start(), end - lmatch
                    cur = end
                    break
                # unmatched ]] are discarded
                cur = end


def find_balanced(text, open_delimiter, close_delimiter):
    """
    Assuming that text contains a properly balanced expression using
    :param open_delimiter: as opening delimiters and
    :param close_delimiter: as closing delimiters.
    :return: an iterator producing pairs (start, end) of start and end
    positions in text containing a balanced expression.
    """
    open_pattern = "|".join([re.escape(x) for x in open_delimiter])
    # patter for delimiters expected after each opening delimiter
    after_pattern = {o: re.compile(open_pattern + "|" + c, re.DOTALL) for o, c in zip(open_delimiter, close_delimiter)}
    stack = []
    start = 0
    cur = 0
    # end = len(text)
    start_set = False
    start_pattern = re.compile(open_pattern)
    next_pattern = start_pattern
    while True:
        next_value = next_pattern.search(text, cur)
        if not next_value:
            return
        if not start_set:
            start = next_value.start()
            start_set = True
        delim = next_value.group(0)
        if delim in open_delimiter:
            stack.append(delim)
            next_pattern = after_pattern[delim]
        else:
            stack.pop()
            # opening = stack.pop()
            # assert opening == open_delimiter[close_delimiter.index(next_value.group(0))]
            if stack:
                next_pattern = after_pattern[stack[-1]]
            else:
                yield start, next_value.end()
                next_pattern = start_pattern
                start = next_value.end()
                start_set = False
        cur = next_value.end()


# ----------------------------------------------------------------------
# parser functions utilities


def ucfirst(string):
    """:return: a string with just its first character uppercase
    We can't use title() since it coverts all words.
    """
    if string:
        if len(string) > 1:
            return string[0].upper() + string[1:]
        return string.upper()
    return ""


def lcfirst(string):
    """:return: a string with its first character lowercase"""
    if string:
        if len(string) > 1:
            return string[0].lower() + string[1:]
        return string.lower()
    return ""


def fully_qualified_template_title(template_title):
    """
    Determine the namespace of the page being included through the template
    mechanism
    """
    if template_title.startswith(":"):
        # Leading colon by itself implies main namespace, so strip this colon
        return ucfirst(template_title[1:])

    m = re.match("([^:]*)(:.*)", template_title)
    if m:
        # colon found but not in the first position - check if it
        # designates a known namespace
        prefix = normalize_namespace(m.group(1))
        if prefix in knownNamespaces:
            return prefix + ucfirst(m.group(2))

    # The title of the page being included is NOT in the main namespace and
    # lacks any other explicit designation of the namespace - therefore, it
    # is resolved to the Template namespace (that's the default for the
    # template inclusion mechanism).

    # This is a defense against pages whose title only contains UTF-8 chars
    # that are reduced to an empty string. Right now I can think of one such
    # case - <C2><A0> which represents the non-breaking space.
    # In this particular case, this page is a redirect to [[Non-nreaking
    # space]], but having in the system a redirect page with an empty title
    # causes numerous problems, so we'll live happier without it.
    if template_title:
        # FIXME: remove this horrible global variable
        return templatePrefix + ucfirst(template_title)  # noqa: F821 # pylint: disable=E0602

    return ""  # caller may log as error


def normalize_namespace(ns):
    return ucfirst(ns)


# ----------------------------------------------------------------------
# Parser functions
# see http://www.mediawiki.org/wiki/Help:Extension:ParserFunctions
# https://github.com/Wikia/app/blob/dev/extensions/ParserFunctions/ParserFunctions_body.php


class Infix:

    """Infix operators.
    The calling sequence for the infix is:
      x |op| y
    """

    def __init__(self, function):
        self.function = function

    def __ror__(self, other):
        return Infix(lambda x, self=self, other=other: self.function(other, x))

    def __or__(self, other):
        return self.function(other)

    def __rlshift__(self, other):
        return Infix(lambda x, self=self, other=other: self.function(other, x))

    def __rshift__(self, other):
        return self.function(other)

    def __call__(self, value1, value2):
        return self.function(value1, value2)


# FIXME: pylint complains that it is *probably* not necessary to use a lambda here but I am not sure
# enough to remove it without tests
# WARNING! this is used in the "eval()" below, so only LOOKS like it isn't used!!!
ROUND = Infix(lambda x, y: round(x, y))  # pylint: disable=W0108


def sharp_expr(expr):
    try:
        expr = re.sub("=", "==", expr)
        expr = re.sub("mod", "%", expr)
        expr = re.sub("\bdiv\b", "/", expr)
        expr = re.sub("\bround\b", "|ROUND|", expr)
        # FIXME: see whether there might be a better way than `eval` here
        return str(eval(expr))  # pylint: disable=W0123
    except Exception:  # pylint: disable=W0703
        # FIXME: find out what exceptions can be raised here and catch them properly
        logger.debug("Exception trying to create sharp_expr", exc_info=True)
        return '<span class="error"></span>'


def sharp_if(test_value, value_if_true, value_if_false=None, *_args):  # pylint: disable=W1113
    # FIXME: What is the point of the *_args here? Can it simply be removed?

    # In theory, we should evaluate the first argument here,
    # but it was evaluated while evaluating part[0] in expand_template().
    if test_value.strip():
        # The {{#if:}} function is an if-then-else construct.
        # The applied condition is: "The condition string is non-empty".
        value_if_true = value_if_true.strip()
        if value_if_true:
            return value_if_true
    elif value_if_false:
        return value_if_false.strip()
    return ""


def sharp_ifeq(lvalue, rvalue, value_if_true, value_if_false=None, *_args):  # pylint: disable=W1113
    rvalue = rvalue.strip()
    if rvalue:
        # lvalue is always defined
        if lvalue.strip() == rvalue:
            # The {{#ifeq:}} function is an if-then-else construct. The
            # applied condition is "is rvalue equal to lvalue". Note that this
            # does only string comparison while MediaWiki implementation also
            # supports numerical comparissons.

            if value_if_true:
                return value_if_true.strip()
        else:
            if value_if_false:
                return value_if_false.strip()
    return ""


def sharp_iferror(test, then="", else_condition=None, *_args):  # pylint: disable=W1113
    if re.match(r'<(?:strong|span|p|div)\s(?:[^\s>]*\s+)*?class="(?:[^"\s>]*\s+)*?error(?:\s[^">]*)?"', test):
        return then
    if else_condition is None:
        return test.strip()
    return else_condition.strip()


def sharp_switch(primary, *params):
    # FIXME: we don't support numeric expressions in primary

    # {{#switch: comparison string
    #  | case1 = result1
    #  | case2
    #  | case4 = result2
    #  | 1 | case5 = result3
    #  | #default = result4
    # }}

    primary = primary.strip()
    found = False  # for fall through cases
    default = None
    rvalue = None
    lvalue = ""
    for param in params:
        # handle cases like:
        #  #default = [http://www.perseus.tufts.edu/hopper/text?doc=Perseus...]
        pair = param.split("=", 1)
        lvalue = pair[0].strip()
        rvalue = None
        if len(pair) > 1:
            # got "="
            rvalue = pair[1].strip()
            # check for any of multiple values pipe separated
            if found or primary in [v.strip() for v in lvalue.split("|")]:
                # Found a match, return now
                return rvalue
            if lvalue == "#default":
                default = rvalue
            rvalue = None  # avoid defaulting to last case
        elif lvalue == primary:
            # If the value matches, set a flag and continue
            found = True
    # Default case
    # Check if the last item had no = sign, thus specifying the default case
    if rvalue is not None:
        return lvalue
    if default is not None:
        return default
    return ""


# Extension Scribuntu
def sharp_invoke(module, function, frame):
    # FIXME: there was a "modules" at some point but the original author removed it for some reason
    # It might be useful, so it is better to go and look and see what it did...
    #
    # functions = modules.get(module)
    # if functions:
    #     funct = functions.get(function)
    #     if funct:
    #         # find parameters in frame whose title is the one of the original
    #         # template invocation
    #         templateTitle = fully_qualified_template_title(function)
    #         if not templateTitle:
    #             logger.warning("Template with empty title")
    #         pair = next((x for x in frame if x[0] == templateTitle), None)
    #         if pair:
    #             params = pair[1]
    #             # extract positional args
    #             params = [params.get(str(i + 1)) for i in range(len(params))]
    #             return funct(*params)
    #         else:
    #             return funct()
    # return ""
    raise NotImplementedError


PARSER_FUNCTIONS = {
    "#expr": sharp_expr,
    "#if": sharp_if,
    "#ifeq": sharp_ifeq,
    "#iferror": sharp_iferror,
    "#ifexpr": lambda *args: "",  # not supported
    "#ifexist": lambda *args: "",  # not supported
    "#rel2abs": lambda *args: "",  # not supported
    "#switch": sharp_switch,
    "# language": lambda *args: "",  # not supported
    "#time": lambda *args: "",  # not supported
    "#timel": lambda *args: "",  # not supported
    "#titleparts": lambda *args: "",  # not supported
    # This function is used in some pages to construct links
    # http://meta.wikimedia.org/wiki/Help:URL
    "urlencode": lambda string, *rest: urllib.parse.quote(string.encode("utf-8")),
    "lc": lambda string, *rest: string.lower() if string else "",
    "lcfirst": lambda string, *rest: lcfirst(string),
    "uc": lambda string, *rest: string.upper() if string else "",
    "ucfirst": lambda string, *rest: ucfirst(string),
    "int": lambda string, *rest: str(int(string)),
}


def call_parser_function(function_name, args, frame):
    """
    Parser functions have similar syntax as templates, except that
    the first argument is everything after the first colon.
    :return: the result of the invocation, None in case of failure.

    http://meta.wikimedia.org/wiki/Help:ParserFunctions
    """

    try:
        if function_name == "#invoke":
            # special handling of frame
            ret = sharp_invoke(args[0].strip(), args[1].strip(), frame)
            # logger.debug('PARSER_FUNCTIONS> %s %s', function_name, ret)
            return ret
        if function_name in PARSER_FUNCTIONS:
            ret = PARSER_FUNCTIONS[function_name](*args)
            # logger.debug('parserFunction> %s %s', function_name, ret)
            return ret
    except Exception:  # pylint: disable=W0703
        # FIXME: find what exceptions can occur and catch them specifically
        logger.warning("There are some errors in call_parser_function")
        return ""  # FIXME: fix errors

    return ""


# These are built before spawning processes, hence thay are shared.
templates = {}
redirects = {}
# cache of parser templates
# FIXME: sharing this with a Manager slows down.
templateCache = {}


def define_template(title, page):
    """
    Adds a template defined in the :param page:.
    @see https://en.wikipedia.org/wiki/Help:Template#Noinclude.2C_includeonly.2C_and_onlyinclude
    """
    global templates
    global redirects

    # title = normalize_title(title)

    # check for redirects
    m = re.match(r"#REDIRECT.*?\[\[([^\]]*)]]", page[0], re.IGNORECASE)
    if m:
        redirects[title] = m.group(1)  # normalize_title(m.group(1))
        return

    text = unescape("".join(page))

    # We're storing template text for future inclusion, therefore,
    # remove all <noinclude> text and keep all <includeonly> text
    # (but eliminate <includeonly> tags per se).
    # However, if <onlyinclude> ... </onlyinclude> parts are present,
    # then only keep them and discard the rest of the template body.
    # This is because using <onlyinclude> on a text fragment is
    # equivalent to enclosing it in <includeonly> tags **AND**
    # enclosing all the rest of the template body in <noinclude> tags.

    # remove comments
    text = COMMENT.sub("", text)

    # eliminate <noinclude> fragments
    text = RE_NO_INCLUDE.sub("", text)
    # eliminate unterminated <noinclude> elements
    text = re.sub(r"<noinclude\s*>.*$", "", text, flags=re.DOTALL)
    text = re.sub(r"<noinclude/>", "", text)

    only_include_accumulator = ""
    for m in re.finditer("<onlyinclude>(.*?)</onlyinclude>", text, re.DOTALL):
        only_include_accumulator += m.group(1)
    if only_include_accumulator:
        text = only_include_accumulator
    else:
        text = RE_INCLUDE_ONLY.sub("", text)

    if text:
        if title in templates:
            logger.warning("Redefining: %s", title)
        templates[title] = text
