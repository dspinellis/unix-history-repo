'so /usr/lib/tmac.s
.if t .tr \(rh-
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
.de LS		\"LS - Literal display, ASCII
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
.de PD
\s-2PDP\s0
.if \\n(.$=0 11/70
.if \\n(.$>0 11/\\$1
..
.de DK
Digital Equipment Corporation\\$1
..
.de UM
.I
User's Manual
.R
..
.de EX
.I ex \\$1
..
.de Ex
.I Ex \\$1
..
.de ED
.I ed \\$1
..
.de Ed
.I Ed \\$1
..
.de AK
The financial support of \\$1
the National Science Foundation under grant
MCS74-07644-A03
and
.SM IBM
under a
.SM IBM
Fellowship
are gratefully acknowledged.
..
.de RM
.rm AK
.rm RM
..
.if n .ds dg +
.if t .ds dg \(dg
.if n .ds dd ~
.if t .ds dd \(dd
.if n .ds ** *
.if t .ds ** \(**
.if n .ds # \u#\d
.if t .ds # \u\s-2#\s0\d
.if n .nr FM 1.12i
.if t .ds b \\fB
.if n .ds b \\fI
.ds i \\fI
.de LC
.br
.ne 7
.LP
.ta 3.0i
..
.if !\n(xx .ND
.nr xx 1
.if t .tr /\(sl
