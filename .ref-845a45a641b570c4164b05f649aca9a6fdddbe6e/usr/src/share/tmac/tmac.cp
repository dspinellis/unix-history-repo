.\" Copyright (c) 1981 The Regents of the University of California.
.\" All rights reserved.
.\"
.\" %sccs.include.proprietary.roff%
.\"
.\"	@(#)tmac.cp	5.2 (Berkeley) %G%
.\"

.nr PS 9
.nr VS 11
.ps \\n(PS
.vs \\n(VS
.nr DO .4i
.po \n(DOu
.nr CW 3.3i
.nr WG 0.25i
.rm PF
.nr CP 1
.nr NT 1 \" no turnover bar
.nr PQ \n(PS
.nr BM 1i
.de MX \" expand month name
.if \\n(mo-0 .ds mo J\\\\s-2ANUARY\s0
.if \\n(mo-1 .ds mo F\\\\s-2EBRUARY\\\\s0
.if \\n(mo-2 .ds mo M\\\\s-2ARCH\\\\s0
.if \\n(mo-3 .ds mo A\\\\s-2PRIL\\\\s0
.if \\n(mo-4 .ds mo M\\\\s-2AY\\\\s0
.if \\n(mo-5 .ds mo J\\\\s-2UNE\\\\s0
.if \\n(mo-6 .ds mo J\\\\s-2ULY\\\\s0
.if \\n(mo-7 .ds mo A\\\\s-2UGUST\\\\s0
.if \\n(mo-8 .ds mo S\\\\s-2EPTEMBER\\\\s0
.if \\n(mo-9 .ds mo O\\\\s-2CTOBER\\\\s0
.if \\n(mo-10 .ds mo N\\\\s-2OVEMBER\\\\s0
.if \\n(mo-11 .ds mo D\\\\s-2ECEMBER\\\\s0
..
.if \nM=1 .so /usr/mel/aip/countrev
.if \nM=2 .so /usr/mel/aip/findbig
.if \nM=0 .so /usr/lib/tmac/tmac.a
.br
.af % 1
.CM
.sp 1i
.IZ
.rm IZ
.de FS
.nr FP 3
.if \\n(.d*2>\\n(L4 .nr FP 4
.if \\n(NC=1 .nr FP 2
.GS \\n(FP
.br
\l'2i'
.br
.af % 1
..
.de FE
.GE
..
.ad
.de AE  \"abstract end
.ll \\n(LLu
.in \\n(XIu
.ce 0
.if !\\n(.d=\\n(nl .AX
.nr AJ 0
.nr XR 0
.nr ND 0
.sp 1
..
.de AB  \"abstract start
.ce 0
.ll \\n(LLu
.fi
.ft 1
.sp
.nr XI \\n(.i
.in +\\n(.lu/8u
.nr AJ 1
.ll -\\n(.lu/8u
.ti +2n
.nr XR 1
..
.de MH
Bell Laboratories
Murray Hill, New Jersey 07974
..
.de RT \" reset params to normal state
.ce 0
.ul 0
.ti 0
.bd 1
.if !"\\n(.z"" \!.bd 1
.if \\n(IP .in -\\n(IQn
.if \\n(IP .nr IP -1
.ft 1
.if \\n(AJ=0 .ps \\n(PS
.if \\n(AJ=0 .vs \\n(VSp
.RZ
.fi
..
.de SH  \" (unnumbered) section heading
.RT
.ne 3
.SP 1
.ft 3
.ce 1000
.nr XR 1
.nr HS 1
..
.de B
.nr PQ \\n(.f
.if t .ft 3
.if "\\$1"" .if n .ul 1000
.if !"\\$1"" .if n .ul 1
.if t .if !"\\$1"" \\$1\\f\\n(PQ\\$2
.if n .if \\n(.$=1 \\$1
.if n .if \\n(.$>1 \\$1\\c
.if n .if \\n(.$>1 \\&\\$2
..
.de I
.nr PQ \\n(.f
.if t .ft 2
.if "\\$1"" .if n .ul 1000
.if !"\\$1"" .if n .ul 1
.if t .if !"\\$1"" \\$1\\f\\n(PQ\\$2
.if n .if \\n(.$=1 \\$1
.if n .if \\n(.$>1 \\$1\\c
.if n .if \\n(.$>1 \\&\\$2
..
.so /usr/lib/tmac/tmac.srefs
.rm CS CE
.de UX
.ie \\n(GA>0 \s-2UNIX\s0\\$1
.el \{\
.if n UNIX\\$1*
.if t UNIX\\$1\\f1\(dg\\fP
.FS
.if n *UNIX
.if t \(dgUNIX
is a Trademark of Bell Laboratories
.FE
.nr GA 1\}
..
