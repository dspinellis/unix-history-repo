.\" Copyright (c) 1979 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.redist.man%
.\"
.\"	@(#)tmac.p	5.2 (Berkeley) %G%
.\"
'if \n(FM=0 'so /usr/lib/tmac/tmac.s
.if n .nr FM 1.2i
.if t .tr *\(**=\(eq/\(sl+\(pl
.bd S B 3
.de mD
.ta 8n 17n 42n
..
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
..
.de ZP
.nr pd \\n(PD
.nr PD 0
.PP
.nr PD \\n(pd
..
.de LS		\"LS - Literal display; ASCII DS
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
.de UP
Berkeley Pascal\\$1
..
.de PD
\s-2PDP\s0
.if \\n(.$=0 11/70
.if \\n(.$>0 11/\\$1
..
.de DK
Digital Equipment Corporation\\$1
..
.de PI
.I pi \\$1
..
.de Xp
.I Pxp \\$1
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
.de PX
.I px \\$1
..
.if n .ds dg +
.if t .ds dg \(dg
.if n .ds Dg \*(dg
.if t .ds Dg \v'-0.3m'\s-2\*(dg\s0\v'0.3m'
.if n .ds dd *
.if t .ds dd \(dd
.if n .ds Dd \*(dd
.if t .ds Dd \v'-0.3m'\s-2\*(dd\s0\v'0.3m'
.if n .ds b \\fI
.if t .ds b \\fB
.nr xx 1
