     Three areas must be addressed to provide full Kanji compatibility.
Only #1 (for the non-regular expression case) has been implemented
directly in our grep/egrep-compatible Boyer-Moore-based code.

	(1) false middle match

	   (a) meta-free Kanji
	   (b) Kanji regexprs

Kanji 16-bit "EUC" data codes (see Jung/Kalash, "Yunikkusu wa Nihongo o
Hanasemasu", p. 209, Atlanta Usenix, 1986) have the upper bit on in both
bytes, so as to allow intermixing of ASCII while preserving end-of-string
detection.  'grep' must beware of matching two Kanji byte pairs in
the interior of two unrelated Kanji characters.  e.g.

	text: 		a (k1 k2) b (k3 k4) (k5 k6)
	pattern:        	       (k4   k5)	

is a bad match, given ascii bytes 'a' and 'b', and Kanji characters
(k1 k2), (k3 k4), and (k5 k6).  The solution for Kanji grep using
the traditional algorithm might be to anchor the pattern only at
Kanji pair boundaries while scanning forward.

Boyer-Moore methods cannot afford this.  So we allow false matches, then
scan backwards for legality (the first ascii byte in the text occurring
before the candidate match disambiguates).  Another appealing method,
for "layered" processing via regexp(3), is to convert the meta-free
Kanji to '(^|[^\000-\177])k1k2', assuming Henry Spencer's code is
"8-bit clean".  Case (b) (e.g. regexprs like 'k1k2.*k3k4') is similar,
though syntax translation may be more difficult.

	(2) closures

     Eight-bit egrep '(k1k2)*' [where the '*' may be '+' or '?'], would
wrongly apply the closure to the previous byte instead of the byte pair.
One solution (without touching the existing 'regexp(3)' or 'e?grep' source)
is to simply parenthesize reg exprs 'k1k2*' -> '(k1k2)*'.
[only works with egrep syntax, so should occur after the grep->egrep
expr xlation].

	(3) character classes

	   (a) easy case:  [k1k2k3k4k5k6]

               -- just map to (k1k2|k3k4|k5k6).

	   (b) hard:  ranges [k1k2-k3k4]

fail for byte-oriented char class code.
Kanji interpretation (how do ideograms collate?) is also problematic.
Translation to egrep '.*((k1k2)|(k1k2++)...|(k3k4)).*', where '++'
denotes "16-bit successor" is conceivable, but farfetched.

     Now, translations (1) and (2) may be done [messily] w/o touching
Spencer's code, while (3) could be farmed out to standard Kanji egrep via the
process exec mechanism already established (see pep4grep.doc[123]).
But if (3) were done this way (invoking exec()), then the other cases might
also be done without recourse to the above xlations [just match "regmust"
first, then pass false drops to the Japan Unix std.]  However, r.e.'s handled
in such a manner would make hybrid Boyer-Moore slow for small files, except for
systems running MACH.  We could have ad hoc file size vs. exec() tradeoff
detectors control things for Kanji (it's already done for Anglo exprs), but
previous success has hinged upon having the regexp(3) layer compatible with the
r.e. style of the coarser egrep utility.

     Thus we take the easy way out and make fast grep only apply to simple
non-r.e. Kanji.  The very best approach remains modification of proprietary
Kanji egrep to incorporate Boyer-Moore directly, by doing Boyer-Moore on the
buffers first before rescanning with the Kanji r.e. machine.  Someday.

-- James A. Woods (ames!jaw)

Postscript:  The several articles in the special issue of UNIX Review
(March 1987) have delineated the bewildering variety of codesets
(shifted JIS, HP 15/16, many EUC flavors, etc.).  A late addition to
[ef]?grep Kanji support is capability for intermixed Katakana (SS2).
Full testing on real Kanji files has not been done.  Comments are welcome.
