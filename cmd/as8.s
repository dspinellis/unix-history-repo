/ as8 -- PDP-11 assembler pass 2

chartab:
	<\0abcdefg>
	<hijklmno>
	<pqrstuvw>
	<xyz012345>
	<6789_.>

qnl:	<?\n>
qii:
	<II\n>
a.out:	<a.out\0>
badfil:	<//\0>			/ a file which should never exist

.even
a.outp:	a.out
a.tmp1:	badfil
a.tmp2:	badfil
a.tmp3:	badfil

tseekp:	txtseek
rseekp:	trelseek

txtmagic:
	br	.+20
txtsiz:	.=.+2
datsiz:	.=.+2
bsssiz:	.=.+2
symsiz:	.=.+2
stksiz:	.=.+2
exorig:	.=.+2
	.=.+2

txtseek: 20
datseek: .=.+2
	.=.+2
trelseek: .=.+2
drelseek: .=.+2
	.=.+2
symseek: .=.+2

fbbufp:	.=.+2
defund:	.=.+2
savdot:	.=.+6
datbase: .=.+2
bssbase: .=.+2
fbfil:	.=.+2
fin:	.=.+2
ibufc:	.=.+2
txtfil:	.=.+2
symf:	.=.+2
adrbuf:	.=.+12.
xsymbol:	.=.+2
fout:	.=.+2
ch:	.=.+2
wordf:	.=.+2
argb:	.=.+22.
line:	.=.+2
savop:	.=.+2
curfb:	.=.+20.
nxtfb:	.=.+20.
nxtfbp:	.=.+20.
curfbr:	.=.+10.
nxtfbr:	.=.+10.
numval:	.=.+2
maxtyp:	.=.+2
relfil:	.=.+2
ibufp:	.=.+2
inbuf:	.=.+512.
txtp:	.=.+6+512.
relp:	.=.+6+512.
swapf:	.=.+2
rlimit:	.=.+2

.=.+2
trelseek: .=.+2
drelseek: .=.+2
	.=.+2
symseek: .=.+2

fbbufp:	.=.+2
d