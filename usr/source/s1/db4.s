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
	.=.+1024.
sigp	= regbuf+310
txtsiz	= regbuf+322
datsiz	= regbuf+324
stksiz	= regbuf+326
rtxtsiz: .=.+2
.data
objmagic: 407
nobjmagic: 410
namsiz:	nambuf
incdot: 2
nlcom: '/

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
symfin:	.=.+2
curfin:	.=.+2
dbfout: .=.+2
ch: .=.+2
lastop: .=.+2
addres: .=.+2
taddr: .=.+2
adrflg: .=.+2
fpsr:	.=.+2
och:	.=.+2
dot: .=.+2
count: .=.+2
syscnt: .=.+2
temp: .=.+2
temp1: .=.+2
obuf: .=.+8.
inbuf: .=.+128.
nambuf:	.=.+20

