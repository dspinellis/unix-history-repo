/ ldx -- link editor

a.out:
	<a.out\0>
l.out:
	<l.out\0>
undmes:
	<un: \0>
movemes:
	<Can't move output file\0>
/comconfl:
/	<Common/text conflict: \0>
norel:
	<No relocation bits: \0>
toomany:
	<Too many routines loaded at: \0>
multi:
	<Multiply defined: \0>
locovflo:
	<Too many symbols in: \0>
relerr:
	<Relocation error in: \0>
premeof:
	<Premature EOF on: \0>
outfile:
	<ld: can't create l.out\n\0>
fnotfound:
	<File not found: \0>
format:
	<Format error: \0>
snotfound:
	<Symbol not found: \0>
mulent:
	<Multiple entry point:\0>

libfil:
	</lib/lib>
wlib:
	<a>
	<.a\0>
qnl:
	<\n>
qsemi:
	<;>

	.even
_etext:	<_etext\0\0>
_edata:	<_edata\0\0>
_end:	<_end\0\0\0\0>

dotdot:	..
zero:	0
rlistp:	rlist
esymp:	symtab

reltab:
	zero
	txtorg
	datorg
	bssorg

arcmagic:
	-147.

magic:
	br	.+20
txtsiz:	.=.+2
datsiz:	.=.+2
bsssiz:	.=.+2
symsiz:	.=.+2
stksiz:	.=.+2
exorig:	.=.+2
relflg:	1
clrelflg: .=.+2

	.bss

errcnt:	.=.+2
libflg:	.=.+2
sqflg:	.=.+2
dcom:	.=.+2
nflg:	.=.+2
datoffs:.=.+2
entptr:	.=.+2
entry:	.=.+2
xtflg:	.=.+2
txtorg:	.=.+2
datorg:	.=.+2
bssorg:	.=.+2
fdatorg: .=.+2
fbssorg: .=.+2

ctxtorg:.=.+2
ctxtsiz:.=.+2
cdatorg:.=.+2
cdatsiz:.=.+2
cbsssiz: .=.+2
ctrelorg:.=.+2
ctrelsiz:.=.+2
cdrelorg:.=.+2
cdrelsiz:.=.+2
csymorg: .=.+2
csymsiz: .=.+2

arcmag:	.=.+42

argc:	.=.+2
argp:	.=.+2
oattpt:	.=.+2
locp:	.=.+2
locsymsiz:.=.+2
fout:	.=.+2
fin:	.=.+2
reopened:.=.+2
ndef:	.=.+2
ch:	.=.+2
filnam:	.=.+2
relbas:	.=.+2
libnxt:	.=.+2
symbol:	.=.+14

txtp:	.=.+10+512.
relp:	.=.+10+512.
otxtp:	.=.+6+512.
odatp:	.=.+6+512.
osymp:	.=.+6+512.
otrelp:	.=.+6+512.
odrelp:	.=.+6+512.

rlist:	.=.+512.
rliste:

local:	.=.+1024.
elocal:

hshsiz = 1000.
hshtab:	.=2*hshsiz+.

symtab:
esymtab = orig+16384.-300.

