.\" @(#)cov.ms	1.3 (Berkeley) %G%
.	\" TL - title and initialization
.de TL
.if \\n(TM .nr ST 1
.if \\n(TM .pn 0
.br
.nr TV 1
.nr YE 1
.nr SJ \\n(.j
.di WT
.na
.fi
.ll 5.0i
.ft 3
.ps 12
.hy 0
..
.	\" TX - actually print title
.de TX
.rs
.sp .5i
.ce 1000
.if n .ul 1000
.ps 12
.ft 3
.vs 15p
.ne 4
.hy 0
.WT
.hy 14
.ce 0
.ul 0
..
.	\" AU - author(s)
.de AU
.nr AV 1
.ad \\n(SJ
.br
.di
.br
.nf
.nr NA +1
.di A\\n(NA
.ll \\n(LLu
.if t .ft 2
.if n .ft 1
.ps 10
..
.	\" AI - author's institution
.de AI
.br
.ft 1
.di
.di I\\n(NA
.nf
..
.	\" AX - actually print authors and institutions
.de AX
.ft 1
.rs
.ce 1000
.if n .ul 0
.ps 10
.vs 12p
.if n .sp 2
.if t .sp
.A1
.if n .sp 
.if t .sp .5
.ns
.I1
.if \\n(NA-1 .if n .sp 2
.if \\n(NA-1 .if t .sp
.A2
.if \\n(NA-1 .if n .sp
.if \\n(NA-1 .if t .sp .5
.ns
.I2
.if \\n(NA-2 .if t .sp
.if \\n(NA-2 .if n .sp 2
.A3
.if \\n(NA-2 .if t .sp .5
.if \\n(NA-2 .if n .sp
.ns
.I3
.if \\n(NA-3 .if t .sp
.if \\n(NA-3 .if n .sp 2
.A4
.if \\n(NA-3 .if t .sp .5
.if \\n(NA-3 .if n .sp
.ns
.I4
.if \\n(NA-4 .if t .sp
.if \\n(NA-4 .if n .sp 2
.A5
.if \\n(NA-4 .if n .sp
.if \\n(NA-4 .if t .sp .5
.ns
.I5
.if \\n(NA-5 .if t .sp
.if \\n(NA-5 .if n .sp 2
.A6
.if \\n(NA-5 .if n .sp
.if \\n(NA-5 .if t .sp .5
.ns
.I6
.if \\n(NA-6 .if t .sp
.if \\n(NA-6 .if n .sp 2
.A7
.if \\n(NA-6 .if n .sp
.if \\n(NA-6 .if t .sp .5
.ns
.I7
.if \\n(NA-7 .if t .sp
.if \\n(NA-7 .if n .sp 2
.A8
.if \\n(NA-7 .if n .sp
.if \\n(NA-7 .if t .sp .5
.ns
.I8
.if \\n(NA-8 .if t .sp
.if \\n(NA-8 .if n .sp 2
.A9
.if \\n(NA-8 .if n .sp
.if \\n(NA-8 .if t .sp .5
.ns
.I9
..
.	\" AB - abstract begin
.de AB
.br
.di
.ul 0
.ce 0
.nr 1T 1
.nr IK 1
.nr KI 1
.di WB
.rs
.nr AJ 1
.ce 1
.ft 2
.if n .ul
.ll \\n(LLu
.if \\n(.$=0 ABSTRACT
.if \\n(.$>0 .if !\\$1no  \\$1
.if \\n(.$=0 .sp
.if \\n(.$>0 .if !\\$1no .sp
.hy 14
.ul 0
.ce 0
.fi
.ft 1
.nr OJ \\n(.i
.in +\\n(.lu/12u
.ll -\\n(.lu/12u
.br
.ps \\n(PS
.if \\n(VS>=40 .vs \\n(VSu
.if \\n(VS<=39 .vs \\n(VSp
.ti +\\n(PIu
.if \\n(TM .ls 1
..
.	\" AE - abstract end
.de AE
.br
.di
.ll \\n(LLu
.ps \\n(PS
.if \\n(VS>=40 .vs \\n(VSu
.if \\n(VS<=39 .vs \\n(VSp
.nr 1T 0
.nr IK 0
.in \\n(OJu
.nr AJ 0
.di
.ce 0
.if \\n(ST>0 .SY
.if \\n(ST<3 .rm SY
.if \\n(TM .ls 2
..
.	\" SY - cover sheet of released paper
.de SY
.ll \\n(LLu
.if !\\n(LT .nr LT \\n(LLu
.ns
.if \\n(TV .TX
.if \\n(AV .AX
.rs
.ce 0
.nf
.sp 3
.pn 2
.af PN i
.ls 1
.WB
.ls 
.sp 3v
\\*(DY
.af PN 1
.sp |9i
.if \\n(FP>0 .FA
.FG
.fi
..
.	\" S2 - cover and page 1 of RP
.de S2
.ce 0
.br
.SY
.rm SY
.bp 1
.if \\n(TV .TX
.if \\n(AV .AX
.rs
.ce 0
.ft 1
.ad \\n(SJ
..
.	\" S1 - cover and page 1 of RP (no p.1 title)
.de S1
.ce 0
.br
.SY
.rm SY
.bp 1
.rs
.ce 0
.ft 1
.ad \\n(SJ
..
.	\" S0 - conserve paper format (without RP)
.de S0
.ce 0
.br
.ll \\n(LLu
.if \\n(TV+\\n(AV>0 .ns
.if \\n(TV .TX
.if \\n(AV .AX
.if \\n(TV+\\n(AV>0 .rs
.ce 0
.if \\n(TV>0 .sp 2
.ls 1
.if \\n(FP>0 \{\
.	FJ
.	nf
.	FG
.	fi
.	FK
.	nr FP 0
.\}
.nf
.WB
.ls
.fi
.ad \\n(SJ
..
