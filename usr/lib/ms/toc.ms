.	\" @(#)toc.ms	1.2 4/22/88
.	\" XS - begin index entry
.de XS
.if !\\n(1T .BG
.ds XL \\n(PN
.if \\n(.$ .ds XL \\$1
.nr SJ \\n(.j
.nr PF \\n(.f
.nr PX \\n(.s
.nr SL \\n(.l
.ls 1
.br
.da XT
.if \\n(.$-1 \{\
.	nr XI 1
.	in \\$2n
.\}
.ft 1
.ps \\n(PS
.ll \\n(LLu-8n
.na
.sp \\n(PDu
..
.	\" XA - add index entry
.de XA
.if !\\*(XLno \\a\\t\\*(XL
.if \\n(.$ .ds XL \\$1
.sp \\n(PDu
.if \\n(.$-1 \{\
.	nr XI 1
.	in \\$2n
.\}
..
.	\" XE - end index entry
.de XE
.if !\\*(XLno \\a\\t\\*(XL
.br
.da
.ls
.ad \\n(SJ
.ft \\n(PF
.ps \\n(PX
.ll \\n(SLu
.if \\n(XI \{\
.	nr XI 0
.	in 0
.\}
..
.	\" PX - print index (table of contents)
.de PX
.if \\n(VS>=40 .vs \\n(VSu
.if \\n(VS<=39 .vs \\n(VSp
.ll \\n(LLu
.lt \\n(LTu
.ta \\n(LLu-5n \\n(LLuR
.in 0
.nf
.rs
.if !\\$1no \{\
.	sp .5i
.	tl ''\f3\s+2Table of Contents\s-2\f1''
.	sp .5i
.\}
.XT
..
.	\" TC - table of contents at end of paper
.de TC
.af PN i
.bp 1
.PX \\$1
..
.	\" NH - numbered section header
.de NH
.RT
.if \\n(1T .sp
.if !\\n(1T .BG
.RT
.ne 3.1
.ft 3
.if n .ul 1000
.nr NS \\$1
.if !\\n(.$ .nr NS 1
.if !\\n(NS .nr NS 1
.nr H\\n(NS +1
.if !\\n(NS-4 .nr H5 0
.if !\\n(NS-3 .nr H4 0
.if !\\n(NS-2 .nr H3 0
.if !\\n(NS-1 .nr H2 0
.if !\\$1 .if \\n(.$ .nr H1 1
.if \\$1S \{\
.	nr NS \\n(.$-1
.	nr H1 \\$2
.	nr H2 \\$3
.	nr H3 \\$4
.	nr H4 \\$5
.	nr H5 \\$6
.\}
.ds SN \\n(H1.
.ti \\n(.iu
.if \\n(NS-1 .as SN \\n(H2.
.if \\n(NS-2 .as SN \\n(H3.
.if \\n(NS-3 .as SN \\n(H4.
.if \\n(NS-4 .as SN \\n(H5.
\\*(SN
..
