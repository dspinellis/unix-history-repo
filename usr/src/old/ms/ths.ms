.\" This module is believed to contain source code proprietary to AT&T.
.\" Use and redistribution is subject to the Berkeley Software License
.\" Agreement and your Software Agreement with AT&T (Western Electric).
.\"
.\"	@(#)ths.ms	5.1 (Berkeley) 4/17/91
.\"
.	\" TM - U.C. Berkeley thesis mode
.ls 2
.if v .nr PO 1.5i
.rm CH
.ds RH \\n(PN
.rm CF
.P1
.nr TM 1
.	\" CT - chapter title 
.de CT
.rn RH R@
.if \\n(1T .bp
.RT
.if \\n(1T .sp
.if !\\n(1T .BG
.RT
.ft 3
.if n .ul 100
.ps \\n(PS+2
.ce 100
.nr CT 1
.rn R@ RH
..
