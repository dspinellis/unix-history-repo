.	\" @(#)hed.ms	1.4 %G%
.	\" letter HEad
.de HE
.nr xx \\n(.d
.nr xy \\n(.u
.nr xz \\n(.f
.LP
\0
.nr xw \\n(.s
.nf
.ps 12
.vs 0.14i
.in 0
.ie "\*(.T"ip" \{\
.	po 0.4i
.	sp |0.5i
.	ft R \}
.el .ie "\*(.T"psc" \{\
.	po 0.85i
.	sp |0.7i
.	ft R \}
.el .ie "\*(.T"va" \{\
.	po 0.1i
.	sp |0.3i
.	ft cr\}
.el \{\
.	po 0.4i
.	sp |0.5i
.	ft cr\}
UNIVERSITY OF CALIFORNIA, BERKELEY
.sp 0.3i-1v
.ie "\*(.T"psc" \{\
\l'11.3c' \h'|13.5c'\l'4.2c'
.br
\s6\|BERKELEY    \(bu    DAVIS    \(bu    IRVINE    \(bu    LOS  ANGELES    \(bu    RIVERSIDE    \(bu    SAN  DIEGO    \(bu    SAN  FRANCISCO \h'|11.35c' \z\(uc \h'|13.85c' SANTA  BARBARA    \(bu    SANTA  CRUZ\s10
.sp -0.2c
\l'11.3c' \h'|13.5c'\l'4.2c'\s7 \}
.el \{\
.ps 7
\D't 0.013i'\D'l 4.5i 0'\h'0.83i'\D'l 1.85i 0'
.sp 0.13i-1v
\!s5
\D't 0.024i'\|BERKELEY \v'-0.035i'\D'l 0 0'\v'0.035i' DAVIS \v'-0.035i'\D'l 0 0'\v'0.035i' IRVINE \v'-0.035i'\D'l 0 0'\v'0.035i' LOS ANGELES \v'-0.035i'\D'l 0 0'\v'0.035i' RIVERSIDE \v'-0.035i'\D'l 0 0'\v'0.035i' SAN DIEGO \v'-0.035i'\D'l 0 0'\v'0.035i' SAN FRANCISCO\h'|4.9i'\u\f(slseal\fP\h'|5.475i'\dSANTA BARBARA \v'-0.035i'\D'l 0 0'\v'0.035i' SANTA CRUZ
.sp 0.07i-1v
\D't 0.013i'\D'l 4.5i 0'\h'0.83i'\D'l 1.85i 0' \}
.sp 0.5i-1v
\|COLLEGE OF ENGINEERING\h'|4.1i'BERKELEY, CALIFORNIA  94720
\|DEPARTMENT OF ELECTRICAL ENGINEERING
\0\0AND COMPUTER SCIENCES\D't 3u'
\|COMPUTER SCIENCE DIVISION
.ft \\n(xz
.ps \\n(xw
.if "\\$1"D" \{\.sp 4p
\\h'|4.1i'\\*(DY\}
.if \\n(xy .fi
.ie \\n(xx>\\n(.d .sp |\\n(xxu
.el .sp 0.3i
.vs
.in
.po
..
.	\" LT - letter
.de LT
.nr PO 1.1i
.if "\*(.T"va" .nr PO 0.8i
.po \\n(POu
.LP
.rs
.if n .sp 1i
.ll 8i
.ie n \{\
.ce 4
University of California
Computer Science Division
Berkeley, California  94720
\}
.el .HE \\$1
.if n .if "\\$1"D" \{.ce
\\*(DY\}
.ll
.in 0
.br
.if t .sp 3
.if n .sp
.nf
.if n .na
.rm CF
..
