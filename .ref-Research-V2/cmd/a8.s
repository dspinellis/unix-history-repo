/ a8 -- pdp-11 assembler pass 1

chartab:
	.byte -14,-14,-14,-14,-02,-14,-14,-14
	.byte -14,-22, -2,-14,-14,-22,-14,-14
	.byte -14,-14,-14,-14,-14,-14,-14,-14
	.byte -14,-14,-14,-14,-14,-14,-14,-14
	.byte -22,-20,-16,-14,-20,-20,-20,-12
	.byte -20,-20,-20,-20,-20,-20,38.,-06
	.byte 27.,28.,29.,30.,31.,32.,33.,34.
	.byte 35.,36.,-20,-02,-00,-20,-14,-14
	.byte -14,01.,02.,03.,04.,05.,06.,07.
	.byte 08.,09.,10.,11.,12.,13.,14.,15.
	.byte 16.,17.,18.,19.,20.,21.,22.,23.
	.byte 24.,25.,26.,-20,-24,-20,-20,37.
	.byte -14,01.,02.,03.,04.,05.,06.,07.
	.byte 08.,09.,10.,11.,12.,13.,14.,15.
	.byte 16.,17.,18.,19.,20.,21.,22.,23.
	.byte 24.,25.,26.,-14,-26,-14,-14,-14

errflg:	.byte 0
namedone:	.byte 0
ch:	.byte 0
a.tmp1:	</tmp/atm1a\0>
a.tmp2:	</tmp/atm2a\0>
a.tmp3:	</tmp/atm3a\0>
qi:	<I\n>
.even

curfb:
	-1;-1;-1;-1;-1;-1;-1;-1;-1;-1
obufp:	outbuf
symend:	usymtab
txtsiz:	.=.+2
datsiz:	.=.+2
bsssiz:	.=.+2
curfbr:	.=.+10.
savdot:	.=.+6
bufcnt:	.=.+2
hshsiz = 1000.
hshtab:	.=2*hshsiz+.
pof:	.=.+1
wordf:	.=.+1
fin:	.=.+1
fbfil:	.=.+1
fileflg:	.=.+1
.even
symbol:	.=.+6
inbuf:	.=.+512.
obufc:	.=.+2
outbuf:	.=.+512.
line:	.=.+2
inbfcnt:	.=.+2
ifflg:	.=.+2
inbfp:	.=.+2
nargs:	.=.+2
curarg:	.=.+2
opfound:	.=.+2
savop:	.=.+2
numval:	.=.+2
nxtfb:	.=.+4
2:	</tmp/atm2a\0>
a.tmp3:	</tmp/atm3a\0>
qi:	<I\n>
.even

curfb:
	-1;-1;-1;-1;-1;-1;-1;-1;-1;-1
obufp:	outbuf
symend:	usymtab
txtsiz:	.=.+2
datsiz:	.=.+2
bsssiz:	.=.+2
curfbr:	.=.+10.
savdot:	.=.+6
bufcnt:	.=.+2
hshsiz = 1000.
hshtab:	.=2*hshsiz+.
pof:	.=.+1
wordf:	.=.+1
fin:	.=.+1
