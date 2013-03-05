"""Regular expression tests specific to _sre.py and accumulated during TDD."""
import locale, sys, unittest
import re, _sre, sre_constants
from sre_constants import ATCODES, OPCODES, CHCODES

# constants for testing various locale and unicode stuff
UPPER_AE = "\xc4"
LOWER_AE = "\xe4"
UPPER_PI = u"\u03a0"
LOWER_PI = u"\u03c0"
INDIAN_DIGIT = u"\u0966" # a unicode digit
EM_SPACE = u"\u2001" # a unicode space
LINE_SEP = u"\u2028" # a unicode linebreak

# get types in a way independent of sre.c/sre.py
SRE_Pattern = type(re.compile("b"))
SRE_Match = type(re.match("b", "b"))

py23 = sys.version_info[:2] == (2, 3)

class SrePyTest(unittest.TestCase):

    def test_magic(self):
        self.assertEquals(sre_constants.MAGIC, _sre.MAGIC)

    def test_codesize(self):
        self.assertEquals(_sre.getcodesize(), _sre.CODESIZE)


class SrePatternTest(unittest.TestCase):

    def get_instance(self):
        return re.compile("b")

    def test_copy(self):
        # copy support is disabled by default in _sre.c
        p = self.get_instance()
        self.assertRaises(TypeError, p.__copy__)
        self.assertRaises(TypeError, p.__deepcopy__)

    def test_creation_attributes(self):
        pattern_string = "(b)l(?P<g>a)"
        p = re.compile(pattern_string, re.I | re.M)
        self.assertEquals(SRE_Pattern, type(p))
        self.assertEquals(pattern_string, p.pattern)
        self.assertEquals(re.I | re.M, p.flags)
        self.assertEquals(2, p.groups)
        self.assertEquals({"g": 2}, p.groupindex)

    def test_match_none(self):
        p = re.compile("bla")
        none_matches = ["b", "bl", "blub", "jupidu"]
        for string in none_matches:
            self.assertEquals(None, p.match(string))

    def test_pos_endpos(self):
        # XXX maybe fancier tests here
        p = re.compile("bl(a)")
        tests = [("abla", 0, 4), ("abla", 1, 4), ("ablaa", 1, 4)]
        for string, pos, endpos in tests:
            self.assert_(p.search(string, pos, endpos))
        tests = [("abla", 0, 3), ("abla", 2, 4)]
        for string, pos, endpos in tests:
            self.failIf(p.search(string, pos, endpos))

    def test_findall(self):
        self.assertEquals(["b"], re.findall("b", "bla"))
        self.assertEquals(["a", "u"], re.findall("b(.)", "abalbus"))
        self.assertEquals([("a", "l"), ("u", "s")], re.findall("b(.)(.)", "abalbus"))
        self.assertEquals([("a", ""), ("s", "s")], re.findall("b(a|(s))", "babs"))

    def test_finditer(self):
        it = re.finditer("b(.)", "brabbel")
        self.assertEquals("br", it.next().group(0))
        self.assertEquals("bb", it.next().group(0))
        self.assertRaises(StopIteration, it.next)

    def test_split(self):
        self.assertEquals(["a", "o", "u", ""], re.split("b", "abobub"))
        self.assertEquals(["a", "o", "ub"], re.split("b", "abobub", 2))
        self.assertEquals(['', 'a', 'l', 'a', 'lla'], re.split("b(a)", "balballa"))
        self.assertEquals(['', 'a', None, 'l', 'u', None, 'lla'],
            re.split("b([ua]|(s))", "balbulla"))


class SreMatchTest(unittest.TestCase):

    def test_copy(self):
        # copy support is disabled by default in _sre.c
        m = re.match("bla", "bla")
        self.assertRaises(TypeError, m.__copy__)
        self.assertRaises(TypeError, m.__deepcopy__)

    def test_match_attributes(self):
        c = re.compile("bla")
        m = c.match("blastring")
        self.assertEquals(SRE_Match, type(m))
        self.assertEquals("blastring", m.string)
        self.assertEquals(c, m.re)
        self.assertEquals(0, m.pos)
        self.assertEquals(9, m.endpos)
        self.assertEquals(None, m.lastindex)
        self.assertEquals(None, m.lastgroup)
        self.assertEquals(((0, 3),), m.regs)

    def test_match_attributes_with_groups(self):
        m = re.search("a(b)(?P<name>c)", "aabcd")
        self.assertEquals(0, m.pos)
        self.assertEquals(5, m.endpos)
        self.assertEquals(2, m.lastindex)
        self.assertEquals("name", m.lastgroup)
        self.assertEquals(((1, 4), (2, 3), (3, 4)), m.regs)

    def test_regs_overlapping_groups(self):
        m = re.match("a((b)c)", "abc")
        self.assertEquals(((0, 3), (1, 3), (1, 2)), m.regs)

    def test_start_end_span(self):
        m = re.search("a((b)c)", "aabcd")
        self.assertEquals((1, 4), (m.start(), m.end()))
        self.assertEquals((1, 4), m.span())
        self.assertEquals((2, 4), (m.start(1), m.end(1)))
        self.assertEquals((2, 4), m.span(1))
        self.assertEquals((2, 3), (m.start(2), m.end(2)))
        self.assertEquals((2, 3), m.span(2))
        self.assertRaises(IndexError, m.start, 3)
        self.assertRaises(IndexError, m.end, 3)
        self.assertRaises(IndexError, m.span, 3)
        self.assertRaises(IndexError, m.start, -1)

    def test_groups(self):
        m = re.search("a((.).)", "aabcd")
        self.assertEquals(("ab", "a"), m.groups())
        self.assertEquals(("ab", "a"), m.groups(True))
        m = re.search("a((\d)|(\s))", "aa1b")
        self.assertEquals(("1", "1", None), m.groups())
        self.assertEquals(("1", "1", True), m.groups(True))
        m = re.search("a((\d)|(\s))", "a ")
        self.assertEquals((" ", None, " "), m.groups())
        m = re.match("(a)", "a")
        self.assertEquals(("a",), m.groups())

    def test_groupdict(self):
        m = re.search("a((.).)", "aabcd")
        self.assertEquals({}, m.groupdict())
        m = re.search("a((?P<first>.).)", "aabcd")
        self.assertEquals({"first": "a"}, m.groupdict())
        m = re.search("a((?P<first>\d)|(?P<second>\s))", "aa1b")
        self.assertEquals({"first": "1", "second": None}, m.groupdict())
        self.assertEquals({"first": "1", "second": True}, m.groupdict(True))

    def test_group(self):
        m = re.search("a((?P<first>\d)|(?P<second>\s))", "aa1b")
        self.assertEquals("a1", m.group())
        self.assertEquals(("1", "1", None), m.group(1, 2, 3))
        self.assertEquals(("1", None), m.group("first", "second"))
        self.assertRaises(IndexError, m.group, 1, 4)

    def test_expand(self):
        m = re.search("a(..)(?P<name>..)", "ab1bc")
        self.assertEquals("b1bcbc", m.expand(r"\1\g<name>\2"))

    def test_sub(self):
        self.assertEquals("bbbbb", re.sub("a", "b", "ababa"))
        self.assertEquals(("bbbbb", 3), re.subn("a", "b", "ababa"))
        self.assertEquals("dddd", re.sub("[abc]", "d", "abcd"))
        self.assertEquals(("dddd", 3), re.subn("[abc]", "d", "abcd"))
        self.assertEquals("rbd\nbr\n", re.sub("a(.)", r"b\1\n", "radar"))
        self.assertEquals(("rbd\nbr\n", 2), re.subn("a(.)", r"b\1\n", "radar"))
        self.assertEquals(("bbbba", 2), re.subn("a", "b", "ababa", 2))

    def test_sub_callable(self):
        def call_me(match):
            ret = ""
            for char in match.group():
                ret += chr(ord(char) + 1)
            return ret
        self.assertEquals(("bbbbb", 3), re.subn("a", call_me, "ababa"))


class SreScannerTest(unittest.TestCase):

    def test_scanner_attributes(self):
        p = re.compile("bla")
        s = p.scanner("blablubla")
        self.assertEquals(p, s.pattern)

    def test_scanner_match(self):
        p = re.compile(".").scanner("bla")
        self.assertEquals(("b", "l", "a"), (p.match().group(0),
                                    p.match().group(0), p.match().group(0)))
        self.assertEquals(None, p.match())

    def test_scanner_search(self):
        p = re.compile("\d").scanner("bla23c5a")
        self.assertEquals(("2", "3", "5"), (p.search().group(0),
                                    p.search().group(0), p.search().group(0)))
        self.assertEquals(None, p.search())

    def test_scanner_zero_width_match(self):
        if py23:
            return
        p = re.compile(".*").scanner("bla")
        self.assertEquals(("bla", ""), (p.search().group(0), p.search().group(0)))
        self.assertEquals(None, p.search())


class GetlowerTest(unittest.TestCase):

    def setUp(self):
        locale.setlocale(locale.LC_ALL, (None, None))
        
    def tearDown(self):
        locale.setlocale(locale.LC_ALL, (None, None))

    def assertLowerEqual(self, tests, flags):
        for arg, expected in tests:
            self.assertEquals(ord(expected), _sre.getlower(ord(arg), flags))

    def test_getlower_no_flags(self):
        self.assertLowerEqual([("a", "a"), ("A", "a"), (UPPER_AE, UPPER_AE),
            (u"\u00c4", u"\u00c4"), (u"\u4444", u"\u4444")], 0)

    def test_getlower_locale(self):
        try:
            locale.setlocale(locale.LC_ALL, "de_DE")
            self.assertLowerEqual([("a", "a"), ("A", "a"), (UPPER_AE, LOWER_AE),
                (u"\u00c4", u"\u00e4"), (UPPER_PI, UPPER_PI)],
                sre_constants.SRE_FLAG_LOCALE)
        except locale.Error:
            # skip test
            pass

    def test_getlower_unicode(self):
        self.assertLowerEqual([("a", "a"), ("A", "a"), (UPPER_AE, LOWER_AE),
            (u"\u00c4", u"\u00e4"), (UPPER_PI, LOWER_PI),
            (u"\u4444", u"\u4444")], sre_constants.SRE_FLAG_UNICODE)
        

class SimpleSearchesTest(unittest.TestCase):

    def test_search_simple_literal(self):
        self.assert_(re.search("bla", "bla"))
        self.assert_(re.search("bla", "blab"))
        self.failIf(re.search("bla", "blu"))

    def test_search_simple_ats(self):
        self.assert_(re.search("^bla", "bla"))
        self.assert_(re.search("^bla", "blab"))
        self.failIf(re.search("^bla", "bbla"))
        self.assert_(re.search("bla$", "abla"))
        self.assert_(re.search("bla$", "bla\n"))
        self.failIf(re.search("bla$", "blaa"))

    def test_search_simple_boundaries(self):
        self.assert_(re.search(r"bla\b", "bla"))
        self.assert_(re.search(r"bla\b", "bla ja"))
        self.assert_(re.search(r"bla\b", u"bla%s" % UPPER_PI))
        self.failIf(re.search(r"bla\b", "blano"))
        self.failIf(re.search(r"bla\b", u"bla%s" % UPPER_PI, re.UNICODE))

    def test_search_simple_categories(self):
        self.assert_(re.search(r"bla\d\s\w", "bla3 b"))
        self.assert_(re.search(r"b\d", u"b%s" % INDIAN_DIGIT, re.UNICODE))
        self.failIf(re.search(r"b\D", u"b%s" % INDIAN_DIGIT, re.UNICODE))
        self.assert_(re.search(r"b\s", u"b%s" % EM_SPACE, re.UNICODE))
        self.failIf(re.search(r"b\S", u"b%s" % EM_SPACE, re.UNICODE))
        self.assert_(re.search(r"b\w", u"b%s" % LOWER_PI, re.UNICODE))
        self.failIf(re.search(r"b\W", u"b%s" % LOWER_PI, re.UNICODE))
        self.assert_(re.search(r"b\w", "b%s" % LOWER_AE, re.UNICODE))

    def test_search_simple_any(self):
        self.assert_(re.search(r"b..a", "jboaas"))
        self.failIf(re.search(r"b..a", "jbo\nas"))
        self.assert_(re.search(r"b..a", "jbo\nas", re.DOTALL))

    def test_search_simple_in(self):
        self.assert_(re.search(r"b[\da-z]a", "bb1a"))
        self.assert_(re.search(r"b[\da-z]a", "bbsa"))
        self.failIf(re.search(r"b[\da-z]a", "bbSa"))
        self.assert_(re.search(r"b[^okd]a", "bsa"))
        self.failIf(re.search(r"b[^okd]a", "bda"))
        self.assert_(re.search(u"b[%s%s%s]a" % (LOWER_PI, UPPER_PI, EM_SPACE),
            u"b%sa" % UPPER_PI)) # bigcharset
        self.assert_(re.search(u"b[%s%s%s]a" % (LOWER_PI, UPPER_PI, EM_SPACE),
            u"b%sa" % EM_SPACE))
        self.failIf(re.search(u"b[%s%s%s]a" % (LOWER_PI, UPPER_PI, EM_SPACE),
            u"b%sa" % LINE_SEP))

    def test_search_simple_literal_ignore(self):
        self.assert_(re.search(r"ba", "ba", re.IGNORECASE))
        self.assert_(re.search(r"ba", "BA", re.IGNORECASE))
        self.assert_(re.search(u"b%s" % UPPER_PI, u"B%s" % LOWER_PI,
            re.IGNORECASE | re.UNICODE))

    def test_search_simple_in_ignore(self):
        self.assert_(re.search(r"ba[A-C]", "bac", re.IGNORECASE))
        self.assert_(re.search(r"ba[a-c]", "baB", re.IGNORECASE))
        self.assert_(re.search(u"ba[%s]" % UPPER_PI, "ba%s" % LOWER_PI,
            re.IGNORECASE | re.UNICODE))
        self.assert_(re.search(r"ba[^A-C]", "bar", re.IGNORECASE))
        self.failIf(re.search(r"ba[^A-C]", "baA", re.IGNORECASE))
        self.failIf(re.search(r"ba[^A-C]", "baa", re.IGNORECASE))

    def test_search_simple_branch(self):
        self.assert_(re.search(r"a(bb|d[ef])b", "adeb"))
        self.assert_(re.search(r"a(bb|d[ef])b", "abbb"))

    def test_search_simple_repeat_one(self):
        self.assert_(re.search(r"aa+", "aa")) # empty tail
        self.assert_(re.search(r"aa+ab", "aaaab")) # backtracking
        self.assert_(re.search(r"aa*ab", "aab")) # empty match
        self.assert_(re.search(r"a[bc]+", "abbccb"))
        self.assertEquals("abbcb", re.search(r"a.+b", "abbcb\nb").group())
        self.assertEquals("abbcb\nb", re.search(r"a.+b", "abbcb\nb", re.DOTALL).group())
        self.assert_(re.search(r"ab+c", "aBbBbBc", re.IGNORECASE))
        self.failIf(re.search(r"aa{2,3}", "aa")) # string too short
        self.failIf(re.search(r"aa{2,3}b", "aab")) # too few repetitions
        self.failIf(re.search(r"aa+b", "aaaac")) # tail doesn't match

    def test_search_simple_min_repeat_one(self):
        self.assert_(re.search(r"aa+?", "aa")) # empty tail
        self.assert_(re.search(r"aa+?ab", "aaaab")) # forward tracking
        self.assert_(re.search(r"a[bc]+?", "abbccb"))
        self.assertEquals("abb", re.search(r"a.+?b", "abbcb\nb").group())
        self.assertEquals("a\nbb", re.search(r"a.+b", "a\nbbc", re.DOTALL).group())
        self.assert_(re.search(r"ab+?c", "aBbBbBc", re.IGNORECASE))
        self.failIf(re.search(r"aa+?", "a")) # string too short
        self.failIf(re.search(r"aa{2,3}?b", "aab")) # too few repetitions
        self.failIf(re.search(r"aa+?b", "aaaac")) # tail doesn't match
        self.assertEquals(re.match(".*?cd", "abcabcde").end(0), 7)

    def test_search_simple_repeat_maximizing(self):
        self.failIf(re.search(r"(ab){3,5}", "abab"))
        self.failIf(re.search(r"(ab){3,5}", "ababa"))
        self.assert_(re.search(r"(ab){3,5}", "ababab"))
        self.assertEquals(re.search(r"(ab){3,5}", "abababababab").end(0), 10)
        self.assertEquals("ad", re.search(r"(a.)*", "abacad").group(1))
        self.assertEquals(("abcg", "cg"),
            re.search(r"(ab(c.)*)+", "ababcecfabcg").groups())
        self.assertEquals(("cg", "cg"),
            re.search(r"(ab|(c.))+", "abcg").groups())
        self.assertEquals(("ab", "cf"),
            re.search(r"((c.)|ab)+", "cfab").groups())
        self.assert_(re.search(r".*", ""))

    def test_search_simple_repeat_minimizing(self):
        self.failIf(re.search(r"(ab){3,5}?", "abab"))
        self.assert_(re.search(r"(ab){3,5}?", "ababab"))
        self.assert_(re.search(r"b(a){3,5}?b", "baaaaab"))
        self.failIf(re.search(r"b(a){3,5}?b", "baaaaaab"))
        self.assert_(re.search(r"a(b(.)+?)*", "abdbebb"))

    def test_search_simple_groupref(self):
        self.assert_(re.match(r"((ab)+)c\1", "ababcabab"))
        self.failIf(re.match(r"((ab)+)c\1", "ababcab"))
        self.failIf(re.search(r"(a|(b))\2", "aa"))
        self.assert_(re.match(r"((ab)+)c\1", "aBAbcAbaB", re.IGNORECASE))
        self.assert_(re.match(r"((a.)+)c\1", u"a%sca%s" % (UPPER_PI, LOWER_PI),
            re.IGNORECASE | re.UNICODE))

    def test_search_simple_groupref_exists(self):
        if not py23:
            self.assert_(re.search(r"(<)?bla(?(1)>)", "<bla>"))
            self.assert_(re.search(r"(<)?bla(?(1)>)", "bla"))
            self.failIf(re.match(r"(<)?bla(?(1)>)", "<bla"))
            self.assert_(re.search(r"(<)?bla(?(1)>|u)", "blau"))

    def test_search_simple_assert(self):
        self.assert_(re.search(r"b(?=\d\d).{3,}", "b23a"))
        self.failIf(re.search(r"b(?=\d\d).{3,}", "b2aa"))
        self.assert_(re.search(r"b(?<=\d.)a", "2ba"))
        self.failIf(re.search(r"b(?<=\d.)a", "ba"))

    def test_search_simple_assert_not(self):
        self.assert_(re.search(r"b(?<!\d.)a", "aba"))
        self.assert_(re.search(r"b(?<!\d.)a", "ba"))
        self.failIf(re.search(r"b(?<!\d.)a", "11ba"))


class MarksStackTest(unittest.TestCase):

    def test_mark_stack_branch(self):
        m = re.match("b(.)a|b.b", "bob")
        self.assertEquals(None, m.group(1))
        self.assertEquals(None, m.lastindex)

    def test_mark_stack_repeat_one(self):
        m = re.match("\d+1((2)|(3))4", "2212413")
        self.assertEquals(("2", "2", None), m.group(1, 2, 3))
        self.assertEquals(1, m.lastindex)

    def test_mark_stack_min_repeat_one(self):
        m = re.match("\d+?1((2)|(3))44", "221341244")
        self.assertEquals(("2", "2", None), m.group(1, 2, 3))
        self.assertEquals(1, m.lastindex)

    def test_mark_stack_max_until(self):
        m = re.match("(\d)+1((2)|(3))4", "2212413")
        self.assertEquals(("2", "2", None), m.group(2, 3, 4))
        self.assertEquals(2, m.lastindex)

    def test_mark_stack_min_until(self):
        m = re.match("(\d)+?1((2)|(3))44", "221341244")
        self.assertEquals(("2", "2", None), m.group(2, 3, 4))
        self.assertEquals(2, m.lastindex)
        

class OpcodesTest(unittest.TestCase):

    def search(self, opcodes, string):
        pattern = _sre.compile("ignore", 0, opcodes, 0, {}, None)
        return pattern.search(string)

    def encode_literal(self, string):
        opcodes = []
        for character in string:
            opcodes.extend([OPCODES["literal"], ord(character)])
        return opcodes

    def void_locale(self):
        locale.setlocale(locale.LC_ALL, (None, None))

    def assertMatch(self, opcodes, strings):
        self.assertSomethingAboutMatch(self.assert_, opcodes, strings)

    def assertNoMatch(self, opcodes, strings):
        self.assertSomethingAboutMatch(self.failIf, opcodes, strings)

    def assertSomethingAboutMatch(self, assert_callable, opcodes, strings):
        if isinstance(strings, str):
            strings = [strings]
        for string in strings:
            assert_callable(self.search(opcodes, string))

    def test_length_optimization(self):
        pattern = "bla"
        opcodes = [OPCODES["info"], 3, 3, len(pattern)] \
            + self.encode_literal(pattern) + [OPCODES["success"]]
        self.assertNoMatch(opcodes, ["b", "bl", "ab"])

    def test_literal(self):
        opcodes = self.encode_literal("bla") + [OPCODES["success"]]
        self.assertNoMatch(opcodes, ["bl", "blu"])
        self.assertMatch(opcodes, ["bla", "blab", "cbla", "bbla"])

    def test_not_literal(self):
        opcodes = self.encode_literal("b") \
            + [OPCODES["not_literal"], ord("a"), OPCODES["success"]]
        self.assertMatch(opcodes, ["bx", "ababy"])
        self.assertNoMatch(opcodes, ["ba", "jabadu"])

    def test_unknown(self):
        self.assertRaises(RuntimeError, self.search, [55555], "b")

    def test_at_beginning(self):
        for atname in ["at_beginning", "at_beginning_string"]:
            opcodes = [OPCODES["at"], ATCODES[atname]] \
                + self.encode_literal("bla") + [OPCODES["success"]]
            self.assertMatch(opcodes, "bla")
            self.assertNoMatch(opcodes, "abla")

    def test_at_beginning_line(self):
        opcodes = [OPCODES["at"], ATCODES["at_beginning_line"]] \
            + self.encode_literal("bla") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["bla", "x\nbla"])
        self.assertNoMatch(opcodes, ["abla", "abla\nubla"])

    def test_at_end(self):
        opcodes = self.encode_literal("bla") \
            + [OPCODES["at"], ATCODES["at_end"], OPCODES["success"]]
        self.assertMatch(opcodes, ["bla", "bla\n"])
        self.assertNoMatch(opcodes, ["blau", "abla\nblau"])

    def test_at_end_line(self):
        opcodes = self.encode_literal("bla") \
            + [OPCODES["at"], ATCODES["at_end_line"], OPCODES["success"]]
        self.assertMatch(opcodes, ["bla\n", "bla\nx", "bla"])
        self.assertNoMatch(opcodes, ["blau"])

    def test_at_end_string(self):
        opcodes = self.encode_literal("bla") \
            + [OPCODES["at"], ATCODES["at_end_string"], OPCODES["success"]]
        self.assertMatch(opcodes, "bla")
        self.assertNoMatch(opcodes, ["blau", "bla\n"])

    def test_at_boundary(self):
        for atname in "at_boundary", "at_loc_boundary", "at_uni_boundary":
            opcodes = self.encode_literal("bla") \
                + [OPCODES["at"], ATCODES[atname], OPCODES["success"]]
            self.assertMatch(opcodes, ["bla", "bla ha", "bla,x"])
            self.assertNoMatch(opcodes, ["blaja", ""])
            opcodes = [OPCODES["at"], ATCODES[atname]] \
                + self.encode_literal("bla") + [OPCODES["success"]]
            self.assertMatch(opcodes, "bla")
            self.assertNoMatch(opcodes, "")

    def test_at_non_boundary(self):
        for atname in "at_non_boundary", "at_loc_non_boundary", "at_uni_non_boundary":
            opcodes = self.encode_literal("bla") \
                + [OPCODES["at"], ATCODES[atname], OPCODES["success"]]
            self.assertMatch(opcodes, "blan")
            self.assertNoMatch(opcodes, ["bla ja", "bla"])

    def test_at_loc_boundary(self):
        try:
            self.void_locale()
            opcodes1 = self.encode_literal("bla") \
                + [OPCODES["at"], ATCODES["at_loc_boundary"], OPCODES["success"]]
            opcodes2 = self.encode_literal("bla") \
                + [OPCODES["at"], ATCODES["at_loc_non_boundary"], OPCODES["success"]]
            self.assertMatch(opcodes1, "bla\xFC")
            self.assertNoMatch(opcodes2, "bla\xFC")
            locale.setlocale(locale.LC_ALL, "de_DE")
            self.assertNoMatch(opcodes1, "bla\xFC")
            self.assertMatch(opcodes2, "bla\xFC")
            locale.resetlocale() # is this the right way to rest the locale?
        except locale.Error:
            # skip test
            pass

    def test_at_uni_boundary(self):
        opcodes = self.encode_literal("bl") + [OPCODES["any"], OPCODES["at"],
            ATCODES["at_uni_boundary"], OPCODES["success"]]
        self.assertMatch(opcodes, ["bla ha", u"bl%s ja" % UPPER_PI])
        self.assertNoMatch(opcodes, [u"bla%s" % LOWER_PI])
        opcodes = self.encode_literal("bl") + [OPCODES["any"], OPCODES["at"],
            ATCODES["at_uni_non_boundary"], OPCODES["success"]]
        self.assertMatch(opcodes, ["blaha", u"bl%sja" % UPPER_PI])

    def test_category_digit(self):
        opcodes = [OPCODES["category"], CHCODES["category_digit"]] \
            + self.encode_literal("b") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["1b", "a1b"])
        self.assertNoMatch(opcodes, ["bb", "b1", u"%sb" % INDIAN_DIGIT])

    def test_category_not_digit(self):
        opcodes = [OPCODES["category"], CHCODES["category_not_digit"]] \
            + self.encode_literal("b") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["bb", "1ab", u"%sb" % INDIAN_DIGIT])
        self.assertNoMatch(opcodes, ["1b", "a1b"])

    def test_category_space(self):
        opcodes = self.encode_literal("b") \
             + [OPCODES["category"], CHCODES["category_space"], OPCODES["success"]]
        self.assertMatch(opcodes, ["b ", "b\n", "b\t", "b\r", "b\v", "b\f"])
        self.assertNoMatch(opcodes, ["bb", "b1", u"b%s" % EM_SPACE])

    def test_category_not_space(self):
        opcodes = self.encode_literal("b") \
             + [OPCODES["category"], CHCODES["category_not_space"], OPCODES["success"]]
        self.assertMatch(opcodes, ["bb", "b1", u"b%s" % EM_SPACE])
        self.assertNoMatch(opcodes, ["b ", "b\n", "b\t", "b\r", "b\v", "b\f"])

    def test_category_word(self):
        opcodes = self.encode_literal("b") \
             + [OPCODES["category"], CHCODES["category_word"], OPCODES["success"]]
        self.assertMatch(opcodes, ["bl", "b4", "b_"])
        self.assertNoMatch(opcodes, ["b ", "b\n", u"b%s" % LOWER_PI])

    def test_category_not_word(self):
        opcodes = self.encode_literal("b") \
             + [OPCODES["category"], CHCODES["category_not_word"], OPCODES["success"]]
        self.assertMatch(opcodes, ["b ", "b\n", u"b%s" % LOWER_PI])
        self.assertNoMatch(opcodes, ["bl", "b4", "b_"])

    def test_category_linebreak(self):
        opcodes = self.encode_literal("b") \
             + [OPCODES["category"], CHCODES["category_linebreak"], OPCODES["success"]]
        self.assertMatch(opcodes, ["b\n"])
        self.assertNoMatch(opcodes, ["b ", "bs", "b\r", u"b%s" % LINE_SEP])
        opcodes = self.encode_literal("b") \
             + [OPCODES["category"], CHCODES["category_uni_linebreak"], OPCODES["success"]]
        self.assertMatch(opcodes, ["b\n", u"b%s" % LINE_SEP])

    def test_category_not_linebreak(self):
        opcodes = self.encode_literal("b") \
             + [OPCODES["category"], CHCODES["category_not_linebreak"], OPCODES["success"]]
        self.assertMatch(opcodes, ["b ", "bs", u"b%s" % LINE_SEP])
        self.assertNoMatch(opcodes, ["b\n"])
        opcodes = self.encode_literal("b") \
             + [OPCODES["category"], CHCODES["category_uni_not_linebreak"], OPCODES["success"]]
        self.assertMatch(opcodes, ["b ", "bs"])
        self.assertNoMatch(opcodes, ["b\n", u"b%s" % LINE_SEP, "b\r"])

    def test_category_loc_word(self):
        try:
            self.void_locale()
            opcodes1 = self.encode_literal("b") \
                + [OPCODES["category"], CHCODES["category_loc_word"], OPCODES["success"]]
            opcodes2 = self.encode_literal("b") \
                + [OPCODES["category"], CHCODES["category_loc_not_word"], OPCODES["success"]]
            self.assertNoMatch(opcodes1, "b\xFC")
            self.assertNoMatch(opcodes1, u"b\u00FC")
            self.assertMatch(opcodes2, "b\xFC")
            locale.setlocale(locale.LC_ALL, "de_DE")
            self.assertMatch(opcodes1, "b\xFC")
            self.assertNoMatch(opcodes1, u"b\u00FC")
            self.assertNoMatch(opcodes2, "b\xFC")
            self.void_locale()
        except locale.Error:
            # skip test
            pass

    def test_any(self):
        opcodes = self.encode_literal("b") + [OPCODES["any"]] \
            + self.encode_literal("a") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["b a", "bla", "bboas"])
        self.assertNoMatch(opcodes, ["b\na", "oba", "b"])

    def test_any_all(self):
        opcodes = self.encode_literal("b") + [OPCODES["any_all"]] \
            + self.encode_literal("a") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["b a", "bla", "bboas", "b\na"])
        self.assertNoMatch(opcodes, ["oba", "b"])

    def test_in_failure(self):
        opcodes = self.encode_literal("b") + [OPCODES["in"], 2, OPCODES["failure"]] \
            + self.encode_literal("a") + [OPCODES["success"]]
        self.assertNoMatch(opcodes, ["ba", "bla"])

    def test_in_literal(self):
        opcodes = self.encode_literal("b") + [OPCODES["in"], 7] \
            + self.encode_literal("la") + [OPCODES["failure"], OPCODES["failure"]] \
            + self.encode_literal("a") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["bla", "baa", "blbla"])
        self.assertNoMatch(opcodes, ["ba", "bja", "blla"])

    def test_in_category(self):
        opcodes = self.encode_literal("b") + [OPCODES["in"], 6, OPCODES["category"],
            CHCODES["category_digit"], OPCODES["category"], CHCODES["category_space"],
            OPCODES["failure"]] + self.encode_literal("a") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["b1a", "b a", "b4b\tas"])
        self.assertNoMatch(opcodes, ["baa", "b5"])

    def test_in_charset_ucs2(self):
        if _sre.CODESIZE != 2:
            return
        # charset bitmap for characters "l" and "h"
        bitmap = 6 * [0] + [4352] + 9 * [0]
        opcodes = self.encode_literal("b") + [OPCODES["in"], 19, OPCODES["charset"]] \
            + bitmap + [OPCODES["failure"]] + self.encode_literal("a") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["bla", "bha", "blbha"])
        self.assertNoMatch(opcodes, ["baa", "bl"])

    def _test_in_bigcharset_ucs2(self):
        # disabled because this actually only works on big-endian machines
        if _sre.CODESIZE != 2:
            return
        # constructing bigcharset for lowercase pi (\u03c0)
        bitmap = 6 * [0] + [4352] + 9 * [0]
        opcodes = self.encode_literal("b") + [OPCODES["in"], 164, OPCODES["bigcharset"], 2] \
            + [0, 1] + 126 * [0] \
            + 16 * [0] \
            + 12 * [0] + [1] + 3 * [0] \
            + [OPCODES["failure"]] + self.encode_literal("a") + [OPCODES["success"]]
        self.assertMatch(opcodes, [u"b%sa" % LOWER_PI])
        self.assertNoMatch(opcodes, [u"b%sa" % UPPER_PI])

    # XXX bigcharset test for ucs4 missing here

    def test_in_range(self):
        opcodes = self.encode_literal("b") + [OPCODES["in"], 5, OPCODES["range"],
            ord("1"), ord("9"), OPCODES["failure"]] \
            + self.encode_literal("a") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["b1a", "b56b7aa"])
        self.assertNoMatch(opcodes, ["baa", "b5"])

    def test_in_negate(self):
        opcodes = self.encode_literal("b") + [OPCODES["in"], 7, OPCODES["negate"]] \
            + self.encode_literal("la") + [OPCODES["failure"]] \
            + self.encode_literal("a") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["b1a", "bja", "bubua"])
        self.assertNoMatch(opcodes, ["bla", "baa", "blbla"])

    def test_literal_ignore(self):
        opcodes = self.encode_literal("b") \
            + [OPCODES["literal_ignore"], ord("a"), OPCODES["success"]]
        self.assertMatch(opcodes, ["ba", "bA"])
        self.assertNoMatch(opcodes, ["bb", "bu"])

    def test_not_literal_ignore(self):
        opcodes = self.encode_literal("b") \
            + [OPCODES["not_literal_ignore"], ord("a"), OPCODES["success"]]
        self.assertMatch(opcodes, ["bb", "bu", u"b%s" % UPPER_PI])
        self.assertNoMatch(opcodes, ["ba", "bA"])

    def test_in_ignore(self):
        opcodes = self.encode_literal("b") + [OPCODES["in_ignore"], 8] \
            + self.encode_literal("abc") + [OPCODES["failure"]] \
            + self.encode_literal("a") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["baa", "bAa", "bbbBa"])
        self.assertNoMatch(opcodes, ["ba", "bja", "blla"])

    def test_in_jump_info(self):
        for opname in "jump", "info":
            opcodes = self.encode_literal("b") \
                + [OPCODES[opname], 3, OPCODES["failure"], OPCODES["failure"]] \
                + self.encode_literal("a") + [OPCODES["success"]]
            self.assertMatch(opcodes, "ba")

    def _test_mark(self):
        # XXX need to rewrite this implementation-independent
        opcodes = self.encode_literal("a") + [OPCODES["mark"], 0] \
            + self.encode_literal("b") + [OPCODES["mark"], 1, OPCODES["success"]]
        state = self.create_state("abc")
        _sre._sre_search(state, opcodes)
        self.assertEquals(1, state.lastindex)
        self.assertEquals(1, state.lastmark)
        # NB: the following are indexes from the start of the match
        self.assertEquals([1, 2], state.marks)

    def test_branch(self):
        opcodes = [OPCODES["branch"], 7] + self.encode_literal("ab") \
            + [OPCODES["jump"], 9, 7] + self.encode_literal("cd") \
            + [OPCODES["jump"], 2, OPCODES["failure"], OPCODES["success"]]
        self.assertMatch(opcodes, ["ab", "cd"])
        self.assertNoMatch(opcodes, ["aacas", "ac", "bla"])

    def test_repeat_one(self):
        opcodes = [OPCODES["repeat_one"], 6, 1, 65535] + self.encode_literal("a") \
            + [OPCODES["success"]] + self.encode_literal("ab") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["aab", "aaaab"])
        self.assertNoMatch(opcodes, ["ab", "a"])

    def test_min_repeat_one(self):
        opcodes = [OPCODES["min_repeat_one"], 5, 1, 65535, OPCODES["any"]] \
            + [OPCODES["success"]] + self.encode_literal("b") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["aab", "ardb", "bb"])
        self.assertNoMatch(opcodes, ["b"])

    def test_repeat_maximizing(self):
        opcodes = [OPCODES["repeat"], 5, 1, 65535] + self.encode_literal("a") \
            + [OPCODES["max_until"]] + self.encode_literal("b") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["ab", "aaaab", "baabb"])
        self.assertNoMatch(opcodes, ["aaa", "", "ac"])

    def test_max_until_error(self):
        opcodes = [OPCODES["max_until"], OPCODES["success"]]
        self.assertRaises(RuntimeError, self.search, opcodes, "a")

    def test_max_until_zero_width_match(self):
        # re.compile won't compile prospective zero-with matches (all of them?),
        # so we can only produce an example by directly constructing bytecodes.
        # CPython 2.3 fails with a recursion limit exceeded error here.
        if not py23:
            opcodes = [OPCODES["repeat"], 10, 1, 65535, OPCODES["repeat_one"],
                6, 0, 65535] + self.encode_literal("a") + [OPCODES["success"],
                OPCODES["max_until"], OPCODES["success"]]
            self.assertMatch(opcodes, ["ab", "bb"])
            self.assertEquals((0, 0), self.search(opcodes, "bb").regs[0])

    def test_repeat_minimizing(self):
        opcodes = [OPCODES["repeat"], 4, 1, 65535, OPCODES["any"],
            OPCODES["min_until"]] + self.encode_literal("b") + [OPCODES["success"]]
        self.assertMatch(opcodes, ["ab", "aaaab", "baabb"])
        self.assertNoMatch(opcodes, ["b"])
        self.assertEquals((0, 3), self.search(opcodes, "aabb").regs[0])

    def test_max_until_error(self):
        opcodes = [OPCODES["min_until"], OPCODES["success"]]
        self.assertRaises(RuntimeError, self.search, opcodes, "a")

    def test_groupref(self):
        opcodes = [OPCODES["mark"], 0, OPCODES["any"], OPCODES["mark"], 1] \
            + self.encode_literal("a") + [OPCODES["groupref"], 0, OPCODES["success"]]
        self.assertMatch(opcodes, ["bab", "aaa", "dad"])
        self.assertNoMatch(opcodes, ["ba", "bad", "baad"])

    def test_groupref_ignore(self):
        opcodes = [OPCODES["mark"], 0, OPCODES["any"], OPCODES["mark"], 1] \
            + self.encode_literal("a") + [OPCODES["groupref_ignore"], 0, OPCODES["success"]]
        self.assertMatch(opcodes, ["bab", "baB", "Dad"])
        self.assertNoMatch(opcodes, ["ba", "bad", "baad"])

    def test_assert(self):
        opcodes = self.encode_literal("a") + [OPCODES["assert"], 4, 0] \
            + self.encode_literal("b") + [OPCODES["success"], OPCODES["success"]]
        self.assertEquals("a", self.search(opcodes, "ab").group(0))
        self.assertNoMatch(opcodes, ["a", "aa"])

    def test_assert_not(self):
        opcodes = self.encode_literal("a") + [OPCODES["assert_not"], 4, 0] \
            + self.encode_literal("b") + [OPCODES["success"], OPCODES["success"]]
        self.assertEquals("a", self.search(opcodes, "ac").group(0))
        self.assertMatch(opcodes, ["a"])
        self.assertNoMatch(opcodes, ["ab"])


class OptimizationsTest(unittest.TestCase):
    """These tests try to trigger optmized edge cases."""

    def test_match_length_optimization(self):
        self.assertEquals(None, re.match("bla", "blub"))

    def test_fast_search(self):
        self.assertEquals(None, re.search("bl", "abaub"))
        self.assertEquals(None, re.search("bl", "b"))
        self.assertEquals(["bl", "bl"], re.findall("bl", "blbl"))
        self.assertEquals(["a", "u"], re.findall("bl(.)", "blablu"))

    def test_branch_literal_shortcut(self):
        self.assertEquals(None, re.search("bl|a|c", "hello"))

    def test_literal_search(self):
        self.assert_(re.search("b(\d)", "ababbbab1"))
        self.assertEquals(None, re.search("b(\d)", "ababbbab"))

    def test_repeat_one_literal_tail(self):
        self.assert_(re.search(".+ab", "wowowowawoabwowo"))
        self.assertEquals(None, re.search(".+ab", "wowowaowowo"))


if __name__ == "__main__":
    unittest.main()
