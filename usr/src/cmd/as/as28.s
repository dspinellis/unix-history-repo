/
/

/ as8 -- PDP-11 assembler pass 2

qnl:	<?\n>
a.out:	<a.out\0>
.even
.data
a.outp:	a.out

a.tmp1:	0
a.tmp2:	0
a.tmp3:	0

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

txtseek:0; 20
datseek:.=.+4
	.=.+4
trelseek:.=.+4
drelseek:.=.+4
	.=.+4
symseek:.=.+4

.bss

brlen	= 1024.
brtab:	.=.+[brlen\/8.]
brtabp:	.=.+2
brdelt:	.=.+2
fbbufp:	.=.+2
defund:	.=.+2
savdot:	.=.+6
datbase:.=.+2
bssbase:.=.+2
fbfil:	.=.+2
fin:	.=.+2
ibufc:	.=.+2
txtfil:	.=.+2
symf:	.=.+2
adrbuf:	.=.+12.
xsymbol:.=.+2
fout:	.=.+2
ch:	.=.+2
errflg:	.=.+2
wordf:	.=.+2
argb:	.=.+22.
line:	.=.+2
savop:	.=.+2
curfb:	.=.+20.
nxtfb:	.=.+20.
numval:	.=.+2
maxtyp:	.=.+2
relfil:	.=.+2
ibufp:	.=.+2
txtp:	.=.+8+512.
relp:	.=.+8+512.
swapf:	.=.+2
rlimit:	.=.+2
passno:	.=.+2
endtable:.=.+2
usymtab:.=.+20.
end:

.text
