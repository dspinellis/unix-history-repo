/ db4 -- debugger

maxsym = 24000.
core:
   <core\0>
a.out:
   <a.out\0>
.even
zero:	0
.bss
regbuf:
	u.sp:	.=.+2
	u.usp:	.=.+2
	u.uusp:	.=.+2
	u.break:.=.+2
	u.r0:	.=.+2
	u.savps:.=.+2
	u.core:	.=.+2
	.=regbuf+512.
	u.pusp:	.=.+2
.data
objmagic: br .+20
namsiz:	nambuf
incdot: 2
nlcom: '/

regnames:
	<sp\0\0\0\0\0\0>; 1; 161000
	<ps\0\0\0\0\0\0>; 1; 160776
	<pc\0\0\0\0\0\0>; 1; 160774
	<r0\0\0\0\0\0\0>; 1; 160772
	<r1\0\0\0\0\0\0>; 1; 160770
	<r2\0\0\0\0\0\0>; 1; 160766
	<r3\0\0\0\0\0\0>; 1; 160764
	<r4\0\0\0\0\0\0>; 1; 160762
	<r5\0\0\0\0\0\0>; 1; 160760
.if fpp
fregnames:
	<fr0\0\0\0\0\0>; 1; 160754
	<fr1\0\0\0\0\0>; 1; 160750
	<fr2\0\0\0\0\0>; 1; 160744
	<fr3\0\0\0\0\0>; 1; 160740
	<fr4\0\0\0\0\0>; 1; 160734
	<fr5\0\0\0\0\0>; 1; 160730
.endif
ereg:

	.bss

starmod:.=.+2
symbol:	.=.+10.
getoff:	.=.+2
namstrt: .=.+2
bytemod: .=.+2
savsp: .=.+2
error: .=.+2
ttyfin: .=.+2
dbfin: .=.+2
dbfout: .=.+2
ch: .=.+2
lastop: .=.+2
addres: .=.+2
taddr: .=.+2
adrflg: .=.+2
f.size:	.=.+2
fpsr:	.=.+2
och:	.=.+2
dot: .=.+2
count: .=.+2
syscnt: .=.+2
temp: .=.+2
temp1: .=.+2
obuf: .=.+8.
ecore = db+8192.
inbuf: .=.+128.
nambuf:	.=.+20

 1; 160750
	<fr2\0\0\0\0\0>; 1; 160744
	<fr3\0\0\0\0\0>; 1; 160740
	<fr4\0\0\0\0\0>; 1; 160734
	<fr5\0\0\0\0\0>; 1; 160730
.endif
ereg:

	.bss

starmod:.=.+2
symbol:	.=.+10.
getoff:	.=.+2
namstrt: .=.+2
bytemod: .=.+2
savsp: .=.+2
error: .=.+2
ttyfin: .=.+2
dbfin: .=.+2
dbfout: .=.+2
ch: .=.+2
lastop: .=.+2
addres: .=.+2
taddr: .=.+2
adrflg: .=.+2
f.size:	.=.+2
fpsr:	.=.+2
och:	.=.+2
dot: .=.+2
c