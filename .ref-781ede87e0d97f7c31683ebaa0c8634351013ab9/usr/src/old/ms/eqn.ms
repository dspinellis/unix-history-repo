.\" @(#)eqn.ms	1.3 (Berkeley) %G%
.bd S B 3
.	\" EQ - equation, breakout and display
.de EQ
.nr UR \\n(.u
.rm EE
.nr LE 1	\" default centered
.ds EL \f1\\$1\fP
.if \\$1C .ds EL \f1\\$2\fP
.if \\$1L \{\
.	ds EL \f1\\$2\fP
.	nr LE 0
.\}
.if \\$1I \{\
.	ds EL \f1\\$2\fP
.	nr LE 0
.	ds EE \\h'|10n'
.	if !\\$3 .ds EE \\h'\\$3'
.\}
.if \\n(YE>0 .nf
.di EZ
..
.	\" EN - end of a displayed equation
.de EN
.if !\\*(10 .br
.di
.rm EZ
.nr ZN \\n(dn
.if \\n(ZN>0 .if \\n(YE=0 .LP
.if \\n(ZN=0 .if !\\*(EL .nr ZN 1
.if \\n(.z .if \\n(ZN>0 .if !\\n(nl=\\n(PE .sp \\n(DDu
.if !\\n(.z .if \\n(ZN>0 .if !\\n(.d=\\n(PE .sp \\n(DDu
.pc
.if \\n(BD>0 .nr LE 0	\" can't mean centering in this case.
.if \\n(MK>0 .if \\n(LE=1 .ds EE \\h'|10n'
.if \\n(MK>0 .nr LE 0	\" don't center if mark/lineup
.lt \\n(.lu
.if \\n(EP=0 .if \\n(ZN>0 \{\
.	if \\n(LE>0 .tl \\*(10\\*(EL
.	if \\n(LE=0 .if \\n(BD=0 .tl \\*(EE\\*(10\\*(EL
.	if \\n(LE=0 .if \\n(BD>0 .if \\n(BD<\\w\\*(10 .nr BD \\w\\*(10
.	if \\n(LE=0 .if \\n(BD>0 \!\\*(10\\t\\*(EL
.\}
.if \\n(EP>0 .if \\n(ZN>0 \{\
.	if \\n(LE>0 .tl \\*(EL\\*(10
.	if \\n(LE=0 .if \\n(BD=0 .tl \\*(EL\\*(EE\\*(10
.	if \\n(LE=0 .if \\n(BD>0 .if \\n(BD<\\w\\*(10 .nr BD \\w\\*(10
.	if \\n(LE=0 .if \\n(BD>0 \!\\h'-\\\\n(.iu'\\*(EL\\h'|0'\\*(10
.\}
.lt \\n(LLu
.pc %
.if \\n(YE>0 .if \\n(UR>0 .fi
.rm 10 11 12 13 14 15 16 17 18 19 20 21 22 23 EL
.rr 10 11 12 13 14 15 16 17 18 19 20 21 22 23
.if \\n(ZN>0 .if t .sp \\n(DDu
.if \\n(.z .nr PE \\n(nl
.if !\\n(.z .nr PE \\n(.d
..
