.\" Copyright (c) 1983 Regents of the University of California.
.\" All rights reserved.  The Berkeley software License Agreement
.\" specifies the terms and conditions for redistribution.
.\"
.\"	@(#)c.t	6.2 (Berkeley) 6/3/86
.\"
.\".ds RH "Sample Config Files
.bp
.LG
.B
.ce
APPENDIX C. SAMPLE CONFIGURATION FILES
.sp
.R
.NL
.PP
The following configuration files are developed in section 5;
they are included here for completeness.
.sp 2
.nf
.ta 1.5i 2.5i 4.0i
#
# ANSEL VAX (a picture perfect machine)
#
machine	vax
cpu	VAX780
timezone	8 dst
ident	ANSEL
maxusers	40

config	vmunix	root on hp0
config	hpvmunix	root on hp0 swap on hp0 and hp2
config	genvmunix	swap generic

controller	mba0	at nexus ?
disk	hp0	at mba? disk ?
disk	hp1	at mba? disk ?
controller	mba1	at nexus ?
disk	hp2	at mba? disk ?
disk	hp3	at mba? disk ?
controller	uba0	at nexus ?
controller	tm0	at uba? csr 0172520	vector tmintr
tape	te0	at tm0 drive 0
tape	te1	at tm0 drive 1
device	dh0	at uba? csr 0160020	vector dhrint dhxint
device	dm0	at uba? csr 0170500	vector dmintr
device	dh1	at uba? csr 0160040	vector dhrint dhxint
device	dh2	at uba? csr 0160060	vector dhrint dhxint
.bp
#
# UCBVAX - Gateway to the world
#
machine	vax
cpu	"VAX780"
cpu	"VAX750"
ident	UCBVAX
timezone	8 dst
maxusers	32
options	INET
options	NS

config	vmunix	root on hp swap on hp and rk0 and rk1
config	upvmunix	root on up
config	hkvmunix	root on hk swap on rk0 and rk1

controller	mba0	at nexus ?
controller	uba0	at nexus ?
disk	hp0	at mba? drive 0
disk	hp1	at mba? drive 1
controller	sc0	at uba? csr 0176700	vector upintr
disk	up0	at sc0 drive 0
disk	up1	at sc0 drive 1
controller	hk0	at uba? csr 0177440	vector rkintr
disk	rk0	at hk0 drive 0
disk	rk1	at hk0 drive 1
pseudo-device	pty
pseudo-device	loop
pseudo-device	imp
device	acc0	at uba? csr 0167600	vector accrint accxint
pseudo-device	ether
device	ec0	at uba? csr 0164330	vector ecrint eccollide ecxint
device	il0	at uba? csr 0164000	vector ilrint ilcint
