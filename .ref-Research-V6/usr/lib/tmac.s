.nr DO \n(.o
.nr tm 0
.		RT - reset everything to normal state
.de RT
.if !\\n(1T .BG
.if !\\n(IK .di
.ce 0
.ul 0
.if \\n(IP .in -\\n(IQn
.if !\\n(IP .nr IQ 5
.if \\n(IP .nr IP -1
.ft R
.if t .ta 5n 10n 15n 20n 25n 30n 35n 40n 45n 50n 55n 60n 65n 70n 75n 80n
.if n .ta 6 11 16 21 26 31 36 41 46 61 56 61 66 71 76 81
.ps \\n(PS
.vs \\n(VSp
.fi
..
.	IZ - initialization
.de IZ
.br
.if n .nr y 0-6
.if t .nr y -1i
.nr x 0 1
.nr IP 0
.nr IQ 5
.nr PS 10
.nr VS 12
.ps \\n(PS
.vs \\n(VSp
.nr TB 0
.if t .CM
.if t .nr LL 6i
.if n .nr LL 60
.ll \\n(LLu
.nr LT \\n(.l
.lt \\n(LTu
.if n .tc ~
.if n .tr ~ 
.if n .ds CH "- % -
.if t .ds CH "\(en % \(en
.ds Ch \\*(CH
.if n .ds CF "\\*(DY
.wh 0 NP
.if t .wh -1i FO
.if n .wh -6 FO
.if n .wh -3 BT
.if t .wh -.5i BT
.if t .nr EP 10.i
.if n .nr EP 60
..
.de TM
.pi /usr/bin/col
.nr ST 1
.ds MN \\$1
.nr MM \\n(.$
.nr tm 1
.if \\n(.$-1 .ds CA \\$2
.if \\n(.$-2 .ds CC \\$3
.rm RP
.rm S0
.rm S2
.rm AX
..
.de RP
.nr ST 2
.rm SG
.rm CS
.rm TM
..
.	TL - title and initialization
.de TL
.nr TV 1
.di WT
.na
.fi
.if n .ll 50
.if n .if \\n(tm .ll 30
.if t .ll 5.0i
.if t .if \\n(tm .ll 3.5i
.ft B
.if !\\n(tm .ps 12
..
.de TX
.rs
.if t .sp .5i
.if n .sp 3
.ce 1000
.if n .ul 1000
.ps 12
.ft B
.vs 15p
.ne 4
.WT
..
.		AU - author(s)
.de AU
.nr AV 1
.ad
.br
.di
.br
.nf
.nr NA +1
.ds R\\n(NA \\$1
.ds E\\n(NA \\$2
.di A\\n(NA
.ll \\n(LLu
.if !\\n(tm .ft I
.if \\n(tm .ft B
.if \\n(tm .if n .ll 16
.if \\n(tm .if t .ll 1.4i
.ps 10
..
.de AX
.ft R
.rs
.ce 1000
.if n .ul 0
.ps 10
.vs 12p
.if n .sp 2
.if t .sp
.A1
.if n .sp 
.if t .sp 0.5
.I1
.if \\n(NA-1 .if n .sp 2
.if \\n(NA-1 .if t .sp
.A2
.if \\n(NA-1 .if n .sp
.if \\n(NA-1 .if t .sp 0.5
.I2
.if \\n(NA-2 .if t .sp
.if \\n(NA-2 .if n .sp 2
.A3
.if \\n(NA-2 .if t .sp 0.5
.if \\n(NA-2 .if n .sp
.I3
.if \\n(NA-3 .if t .sp
.if \\n(NA-3 .if n .sp 2
.A4
.if \\n(NA-3 .if t .sp 0.5
.if \\n(NA-3 .if n .sp
.I4
.if \\n(NA-4 .if t .sp
.if \\n(NA-4 .if n .sp 2
.A5
.if \\n(NA-4 .if n .sp
.if \\n(NA-4 .if t .sp 0.5
.I5
..
.	AI - authors institution
.de AI
.br
.ft R
.di
.di I\\n(NA
.nf
..
.	AB - begin an abstract
.de AB
.br
.di
.ll \\n(LTu
.nr 1T 1
.nr IK 1
.di WB
.rs
.ce 1
.if t .ft I
.if n .ul
.ll \\n(LTu
ABSTRACT
.sp
.fi
.if t .ft R
.nr oi \\n(.i
.if n .in +\\n(.l/12
.if t .in +\\n(.lu/12u
.if t .ll -\\n(.lu/12u
.if n .ll -\\n(.l/12
.br
.ps \\n(PS
.if t .ti +5n
.if n .ti +5
..
.	AE - end of an abstract
.de AE
.br
.di
.ll \\n(LLu
.nr 1T 0
.nr IK 0
.if n .in \\n(oi
.if t .in \\n(oiu
.di
.ce 0
.if \\n(ST-1 .SY
.rm SY
..
.	OK - "other keywords" for cover sheet.
.de OK
.br
.di
.di OD
.nf
.ft B
..
.	CS - spew out cover sheet
.de CS
.br
.di
.if t .pl 15i
.tr ~
.if n .sp 6
.if t .po 0.5i
.if t .sp .5i
.if t .lg
.if t .ll 7.0i
.nf
       \s24\(bs\s12\fB  Bell Laboratories        \s16\fRCover Sheet for Technical Memorandum
.sp
.if t \s6\l'7i'
.fi
.ps 9
.ft I
.if t .sp 2p
The information contained herein is for the use of employees of Bell Laboratories and is not for publication.  (See GEI 13.9-3)\p
.ft R
.nf
.if t \s6\l'7i'
.sp
.if t .ll 6.0i
.if n .ll 30
.if t .po 1.0i
.ps 9
.if t .ta 0.5i 4.5i 4.9i
.if n .ta 10 40 47
.mk
Title- \fB\s10
.rt
.if t .in 0.5i
.if n .in 10
.WT
.rt
.ll \\n(LLu
.if t .in 4.5i
.if n .in 40
\s9\fRDate-\s10\fB
.rt
.if t .in 4.9i
.if n .in 47
\\*(DY
.sp
.if t .ti 4.5i
.if n .ti 40
.mk
\s9\fRTM-\fB\s10
.br
.rt
.CT \\*(MN
.rt
.in 0
.sp
.mk
\s9\fROther Keywords-\fB
.rt
.if t .in 1.1i
.if n .in 20
.OD
.rm OK
.rm OD
.in 0
.if t .sp .7i
.if n .sp 3
.ps 9
.if t .ta 1.8i 3.3i 4.3i
.if n .ta 20 35 45
.ft R
Author	Location	Extension	Charging Case- \s10\fB\\*(CA
.ps 10
.nf
.mk
.A1
.rt
	\\*(R1	\\*(E1	\fR\s9Filing Case- \s10\fB\\*(CC
.if \\n(NA-1 .CB 2
.if \\n(NA-2 .CB 3
.if \\n(NA-3 .CB 4
.if \\n(NA-4 .CB 5
.ft R
.if t .sp .4i
.if n .sp 2
.WB
.br
.if t .sp |8.3i
.if n .sp 5
.FF
.rm FF
.if t .sp |9.3i
.nr x 50-\\n(nl
.if n .sp \\nx
.if t .po 0.5i
.if t .ll 7i
.nf
.if t \l'7i'
.if t .ta 1.4i 2.8i 4.2i
.if n .ta 15 30 45
.if t .sp 8p
Pages Text   \\$1	Other   \\$2	Total   \\$3
.if t .sp 8p
.if n .sp 
.if t No. Figures   \\$4	No. Tables   \\$5	No. Refs.   \\$6	\b'|||||~~'
.if n No. Figures   \\$4	No. Tables   \\$5	No. Refs.   \\$6
.if t \l'7i'
.ps 7
.if n .sp
.tl 'E-1932-C (6-73)'SEE REVERSE SIDE FOR DISTRIBUTION LIST''
.if t .po 0.5i
.ll \\n(LLu
.fi
.if t .sp |10.8i
.if t .po 0
.ps 10
.if t .tl '-'''
.if t .pl
.bp 1
.if t .po \\n(DOu
..
.	CB - help with cover sheet
.de CB
.br
.mk
.A\\$1
.rt
.if \\$1-1 .if !\\$1-2 \&	\\*(R2	\\*(E2
.if \\$1-2 .if !\\$1-3 \&	\\*(R3	\\*(E3
.if \\$1-3 .if !\\$1-4 \&	\\*(R4	\\*(E4
.if \\$1-4 .if !\\$1-5 \&	\\*(R5	\\*(E5
.br
..
.de CT
\\$1
\\$2
\\$3
\\$4
..
.	S1 - tm style 
.de S1
.if n .sp 6
.if t .sp .75i
.if t .ta 4.75i
.if n .ta 47
.ll 80
.ps 36
.br
.if t \t\(bs
.vs .25i
.ps 12
.br
.ft B
.nf
\tBell Laboratories
.ft R
.fi
.na
.ps 8
.vs 12p
.if n .sp 3
.if t .sp .5i
.mk
.fi
.ll
.if n .ll 40
.if t .ll 4.0i
Subject:
.ft B
.ps
.WT
.ft R
.br
.if \\n(MM-1 \\s8Case- \\*(CA\\s10
.if \\n(MM-2 --\s8 File- \\*(CC\\s10
.br
.rt
.if n .ll 70
.if t .ll 6.5i
.if t .in 5.10i
.if n .in 52
.ps 8
.mk
.ti -6n
date:
.ps \\n(PS
.ft B
.br
.rt
\\*(DY
.sp
.ft R
.ps 8
.mk
.ti -6n
from:
.ps
.br
.rt
.ft B
.A1
.br
.A2
.br
.A3
.br
.A4
.br
.A5
.ft R
.sp
.mk
.ps 8
.ti -6n
.ft R
.if \\n(MM TM:
.br
.rt
.ps
.ft B
.hy 0
.ll 10n
.if \\n(MM \\*(MN
.hy
.ll
.ft R
.ll \\n(LLu
.in
.sp
.ad
.ce
.ft I
MEMORANDUM FOR FILE
.ft R
.sp
.ad
..
.	S2 - release paper style
.	SY - cover sheet of released paper
.de SY
.ll \\n(LLu
.sp 6
.ns
.if \\n(TV .TX
.if \\n(AV .AX
.ce 0
.nf
.sp 3
.WB
.fi
..
.	S2 - first text page, released paper format
.de S2
.SY
.rm SY
.bp 1
.if \\n(TV .TX
.if \\n(AV .AX
.ce 0
.ft R
.ad
..
.	S0- mike lesk conserve paper style
.de S0
.ll \\n(LLu
.rs
.sp 6
.ns
.if \\n(TV .TX
.if \\n(AV .AX
.ce 0
.nf
.WB
.fi
.ad
..
.	SG - signature
.de SG
.nf
.if n .in +2*\\n(.l/3
.if t .in +2u*\\n(.lu/3u
.sp 2
.A1
.if \\n(NA-1 .sp 2
.A2
.if \\n(NA-2 .sp 2
.A3
.if \\n(NA-3 .sp 2
.A4
.if \\n(NA-4 .sp 2
.A5
.in
\\$1
.br
..
.	Tables.  TS - table start, TE - table end
.de TS
.br
.RT
.ul 0
.if t .sp 0.5
.if n .sp
.nf
..
.de TE
.fi
.if n .sp 1
.if t .sp 0.5
..
.	DS - display.  If .DS C, center; L, left-adjust; I, indent.
.de DS
.KS
.\\$1D
..
.de CD
.ce 1000
.nf
.nr oi \\n(.i
.if t .sp 0.5
.if n .sp 1
..
.de D
.ID
..
.de ID
.nf
.nr oi \\n(.i
.if t .sp 0.5
.if n .sp 1
.if t .in +0.5i
.if n .in +8
..
.de LD
.nf
.nr oi \\n(.i
.if t .sp 0.5
.if n .sp 1
..
.	DE - display end
.de DE
.ce 0
.if n .in \\n(oi
.if t .in \\n(oiu
.KE
.if t .sp 0.5
.if n .sp 1
.fi
..
.	KS keep - for keep release features. As in IBM
.de KS
.if !\\n(IK .KQ
.nr IK +1
..
.	KQ - real keep processor
.de KQ
.br
.di KK
.nr TB 0
..
.	KF - floating keep
.de KF
.if !\\n(IK .FQ
.nr IK +1
..
.	FQ real floating keep processor
.de FQ
.br
.di KK
.nr TB 1
..
.	KE release - everything between keep and release is together
.de KE
.if \\n(IK .if !\\n(IK-1 .RQ
.if \\n(IK .nr IK -1
..
.	RQ real release
.de RQ
.br
.di
.if \\n(dn+\\n(nl-\\n(EP .if !\\n(TB .sp 200
.if !\\n(dn+\\n(nl-\\n(EP .if \\n(TB .nr TB 0
.nf
.in 0
.if !\\n(TB .KK
.in
.fi
..
.	EQ default equation breakout
.de EQ
.if t .sp .5
.if n .sp
.ce
.ne 2
.ds EL \\$1
.if \\n(.$-1 .ds EL \\$2
.nr LE \\n(.$
.@\\$1
..
.	This nonsense permits recognition of .EQ L, .EQ I, .EQ C
.	by invoking macros @L, @I, and so forth.
.de @L
.ce0
.nr LE -1
..
.de @C
.nr LE -1
..
.de @I
.ce 0
.ti 10n
.nr LE -1
..
.de EN
.ce 0
.lt \\n(.lu
.if \\n(LE .if n .sp -1
.if \\n(LE .if n .tl '''\\*(EL'
.if n .sp 1
.if \\n(LE .if t .sp -1-\\n(.au
.if \\n(LE .if t .tl '''\\*(EL'
.if \\n(LE .if t .sp \\n(.au
.if t .sp 0.5
.ns
..
.	EM end up macro - process left over keep-release
.de EM
.if \\n(TB .br
.if \\n(TB 
.if \\n(TB .NP
..
.	NP new page
.de NP
.if t .CM
.if t 'sp 0.5i
.if n 'sp 3
.ft R
.lt \\n(LTu
.ps \\n(PS
.PT
.ps
.ft P
.if t 'sp 0.5i
.if n 'sp 3
.nr x 0 1
.if t .nr y 0-1i
.if n .nr y 0-6
.if \\n(MF .fz
.nr MF 0
.ns
.os
.ev 1
.if \\n(TB .nf
.if \\n(TB .KK
.if \\n(TB .fi
.ev
.nr TB 0
.mk
..
.de PT
.if \\n%-1 .tl '\\*(LH'\\*(Ch'\\*(RH'
..
.	FO - footer of page
.de FO
.nr FC +1
.nr dn 0
.if \\nx .xf
.nr MF 0
.if \\n(dn  .nr MF 1
.if t .ch FO -1i
.if n .ch FO -6
.if !\\n(FC-1 .if \\n(NX .RC
.if !\\n(FC-1 .if !\\n(NX 'bp
.nr FC -1
..
.	2C - begin double column
.de 2C
.if !\\n(1T .if n .sp 4
.if !\\n(1T .if t .sp 2
.RT
.mk
.nr NC 1
.nr NX 1
.nr L1 \\n(.l
.nr L2 \\n(.l*7/15
.if n .ll \\n(L2
.if t .ll \\n(L2u
.if n .FL \\n(L2*11/12
.if t .FL \\n(L2u*11u/12u
.nr RO \\n(L2*8/7
.ns
..
.de RC
.if \\n(NC-1 .C2
.if !\\n(NC-1 .C1
.nr NC \\n(ND
..
.de C1
.rt
.if n .po +\\n(RO
.if t .po +\\n(ROu
.nr ND 2
.nr x 0 1
.ns
..
.de C2
.po \\n(DOu
'bp
.nr ND 1
..
.	1C - return to single column format
.de 1C
.nr NX 0
.br
.po \\n(DOu
.nr ND 1
.if n .ll \\n(L1
.if t .ll \\n(L1u
.bp
..
.	.de R3
.	.ll 120
.	.pl 102
.	.nr LT \\n(.l
.	..
.de MH
Bell Laboratories,
Murray Hill, New Jersey 07974
..
.de BT
.ft R
.lt \\n(LTu
.tl '\\*(LF'\\*(CF'\\*(RF'
.ft P
..
.	PP - paragraph
.de PP
.RT
.ne 2
.if \\n(1T .if t .sp 0.3
.if \\n(1T .if n .sp
.if t .ti +5n
.if n .ti +5
..
.	SH - (unnumbered) section heading
.de SH
.RT
.if \\n(1T .sp 1
.if !\\n(1T .BG
.RT
.ne 7
.ft B
.if n .ul 1000
..
.	NH - numbered heading
.de NH
.RT
.if \\n(1T .sp 1
.if !\\n(1T .BG
.RT
.ne 7
.ft B
.if n .ul 1000
.nr a \\$1
.if !\\n(.$ .nr a 1
.if !\\na .nr a 1
.nr H\\na +1
.if !\\na-4 .nr H5 0
.if !\\na-3 .nr H4 0
.if !\\na-2 .nr H3 0
.if !\\na-1 .nr H2 0
.if !\\$1 .if \\n(.$ .nr H1 1
.ds SN \\n(H1.
.if \\na-1 .as SN \\n(H2.
.if \\na-2 .as SN \\n(H3.
.if \\na-3 .as SN \\n(H4.
.if \\na-4 .as SN \\n(H5.
\\*(SN
..
.	BG - begin, execute at first PP
.de BG
.br
.di
.ce 0
.nr 1T 1
.S\\n(ST
.rm S0
.rm S1
.rm S2
.rm OK
.rm OD
.rm TX
.rm AX
.rm WT
.rm I1
.rm I2
.rm I3
.rm I4
.rm I5
.rm CB
.rm E1
.rm R1
.rm R2
.rm E2
.de TL
.ft B
.sp
.if n .ul 100
.ce 100
.LG
\\..
.de AU
.ft I
.if n .ul 0
.ce 100
.sp
.NL
\\..
.de AI
.ft R
.ce 100
.if n .ul 0
.if n .sp
.if t .sp .5
.NL
\\..
.ds Ch \\*(CH
.if n .sp 2
.if t .sp 1
.fi
.ll \\n(LLu
..
.	IP - indented paragraph
.de IP
.RT
.if !\\n(IP .nr IP +1
.if t .sp 0.3
.if n .sp
.ne 3
.if \\n(.$-1 .nr IQ \\$2
.LB "\\$1"
..
.	LP - left aligned (block) paragraph
.de LP
.RT
.if t .sp 0.3
.if n .sp
.ne 3
.if \\n(.$ .LB \\$1
..
.	IE - synonym for .LP
.de IE
.LP
..
.	LB - label paragraph
.de LB
.in +\\n(IQn
.if n .ta \\n(IQ+1
.if t .ta \\n(IQn
.ti -\\n(IQn
\\$1\t\c
..
.	RS - prepare for double indenting
.de RS
.in +\\n(IQn
.nr IP +1
..
.	RE - retreat to the left
.de RE
.br
.in -\\n(IQn
..
.	CM - cut mark
.de CM
.po 0
.tl '-'''
.po
..
.	B - bold font
.de B
.ft B
.if n .ul 1000
..
.	R - Roman font
.de R
.ft R
.if n .ul 0
..
.	I - italic font
.de I
.ft I
.if n .ul 1000
..
.	TA - tabs set in ens or chars
.de TA
.ta \\$1n \\$2n \\$3n \\$4n \\$5n \\$6n \\$7n \\$8n \\$9n
..
.	SM - make smaller size
.de SM
.ps -2
..
.	LG - make larger size
.de LG
.ps +2
..
.	NL - return to normal size
.de NL
.ps \\n(PS
..
.	DA - force date; ND - no date or new date.
.de DA
.if \\n(.$ .ds DY \\$1 \\$2 \\$3 \\$4
.ds CF \\*(DY
..
.de ND
.ds DY \\$1 \\$2 \\$3 \\$4
.rm CF
..
.if \n(mo-0 .ds mo January
.if \n(mo-1 .ds mo February
.if \n(mo-2 .ds mo March
.if \n(mo-3 .ds mo April
.if \n(mo-4 .ds mo May
.if \n(mo-5 .ds mo June
.if \n(mo-6 .ds mo July
.if \n(mo-7 .ds mo August
.if \n(mo-8 .ds mo September
.if \n(mo-9 .ds mo October
.if \n(mo-10 .ds mo November
.if \n(mo-11 .ds mo December
.if \n(dw-0 .ds dw Sunday
.if \n(dw-1 .ds dw Monday
.if \n(dw-2 .ds dw Tuesday
.if \n(dw-3 .ds dw Wednesday
.if \n(dw-4 .ds dw Thursday
.if \n(dw-5 .ds dw Friday
.if \n(dw-6 .ds dw Saturday
.ds DY \*(mo \n(dy, 19\n(yr
.IZ
.rm IZ
.rm mo
.de FN
.FS
..
.	FS - begin footnote
.de FS
'ce 0
.di
.da FF
.ev1
.if !\\n+x-1 .fs
..
.	FE - footnote end
.de FE
.br
.in 0
.ev
.di
.if t .if !\\nx-1 .nr dn +\\n(.v
.nr y -\\n(dn
.if n .ch FO \\ny
.if t .ch FO \\nyu
.if t .if \\n(nl+\\n(.v-\\n(.p-\\ny .ch FO \\n(nlu+\\n(.vu
.if n .if \\n(nl-\\n(.p-\\ny .ch FO \\n(nl+1
..
.de fs SEPARATOR
.if n __________________________
.if t \l'1i'
.br
..
.de fz
.FS
.nf
.fy
.fi
.FE
..
.de fx
.di fy
..
.de xf
.ev1
.nf
.FF
.rm FF
.di
.fi
.ev
..
.de FL
.ev1
.ll \\$1
.ev
..
.de HO
Bell Laboratories,
Holmdel, New Jersey 07733
..
.de WH
Bell Laboratories,
Whippany, New Jersey 07981
..
.if t .ch FO 12i
.if t .wh -1i fx
.if t .ch FO -1i
.if n .ch FO 100
.if n .wh -6 fx
.if n .ch FO -6
.ev1
.ll \n(LLu
.if n .ll -\\n(.l/12
.if t .ll -\\n(.lu/12u
'ps8
'vs10p
.ev
.br
.em EM
