/
/

/ iox -- io variables

.globl	_nocr
.globl	utable
.globl	btable
.globl	ftable

.globl	_end
.globl	formp

bufp:	_end
filnam:
	<fortxx\0>; .even

	.bss

gflg:	.=.+2
formp:	.=.+2
rdflg:	.=.+2
nflg:	.=.+2
unit:	.=.+2
buffer:	.=.+2
slcnt:	.=.+2
itype:	.=.+1
ilen:	.=.+1
ilval:	.=.+2
width:	.=.+2
twidth:	.=.+2
ndig:	.=.+2
pbuf:	.=.+10
ppar:	.=.+2
llp:	.=.+2
llpcnt:	.=.+2
itmflg:	.=.+2
nspace:	.=.+2
gcflg:	.=.+2
binflg:	.=.+2

utable:	.=.+20.
btable:	.=.+40.
ftable:	.=.+2.
rep:	.=.+2
scale:	.=.+2
itmfnd:	.=.+2
ngflg:	.=.+2
nlflg:	.=.+2
_nocr:	.=.+2

