'so /usr/lib/tmac.s
.if t .tr \(rh-
.if t .tr *\(**=\(eq/\(sl+\(pl
.bd S B 3
.de SM
.if "\\$1"" .ps -2
.if !"\\$1"" \s-2\\$1\s0\\$2
..
.de LG
.if "\\$1"" .ps +2
.if !"\\$1"" \s+2\\$a\s0\\$2
..
.de HP
.nr pd \\n(PD
.nr PD 0
.if \\n(.$=0 .IP
.if \\n(.$=1 .IP "\\$1"
.if \\n(.$>=2 .IP "\\$1" "\\$2"
.nr PD \\n(pd
.rm pd
..
.de LS		\"LS - Literal display, ASCII, constant spaced DS
.if \\n(.$=0 .DS
.if \\n(.$=1 \\$1
.if \\n(.$>1 \\$1 "\\$2"
.if t .tr '\'`\`^\(ua-\(mi
.if t .tr _\(ul
..
.de LE		\"LE - End literal display
.DE
.tr ''``__--^^
..
.de UX
\s-2UNIX\s0\\$1
..
.de UP
\s-2UNIX\s0 Pascal\\$1
..
.de PD
\s-2PDP\s0
.if \\n(.$=0 11/70
.if \\n(.$>0 11/\\$1
..
.de DK
Digital Equipment Corporation\\$1
..
.de IN
.I
.if \\n(.$<2 \\$1 1.0 Implementation Notes
.if \\n(.$>=2 \\$1 1.0 Implementation Notes\\c
.R
.if \\n(.$>=2 \\$2
..
.de UM
.I
User's Manual
.R
..
.de PI
.I pi \\$1
..
.de XP
.I pxp \\$1
..
.de IX
.I pix \\$1
..
.de X
.I px \\$1
..
.de AK
The financial support of \\$1
the National Science Foundation under grant
MCS74-07644-A03,
and the first author's work by an
.SM IBM
Fellowship
are gratefully acknowledged.
..
.de 12
.AK "the first and second authors' work by"
..
.de 1A
.AK "the first author's work by"
..
.de 2A
.AK "the second author's work by"
..
.de RM
.rm WJ
.rm WS
.rm AK
.rm RM
..
.if n .ds dg +
.if t .ds dg \(dg
.if n .ds dd *
.if t .ds dd \(dd
.if n .nr FM 1.12i
.if t .ds b \\fB
.if n .ds b \\fI
.ds i \\fI
.nr xx 1
