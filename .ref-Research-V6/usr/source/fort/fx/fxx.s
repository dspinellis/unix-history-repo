/
/

/ fxx -- data segment definition

.data
.globl	holround
holround:	4

.bss

/ pass 1

.globl	dimu
dimu:	.=.+2

/ pass 2

.globl	eqvtab

/ pass 3 stuff

.globl	conu
.globl	dou
.globl	blockp
.globl	dotabp
.globl	dotab
.globl	edotab
.globl	functn
.globl	blocks

conu:	.=.+2
dou:	.=.+2
blockp:	.=.+2
dotabp:	.=.+2
dotab:	.=.+60.
edotab:
functn:	.=.+2

/ pass 4

.globl	negflg
.globl	repfact
.globl	contab
.globl	dattab

negflg:	.=.+2
repfact:.=.+2

/ general buffer

xbufsiz	= 2200.

.globl	xbuf

xbuf:	.=.+xbufsiz

eqvtab	= xbuf+518.	/ for pass 2

blocks	= xbuf		/ for pass 3

dattab	= xbuf+518.	/ for pass 4
contab	= xbuf+xbufsiz

/ for all passes

data:
	ibuf:	.=.+518.
	obuf:	.=.+518.
	tbuf:	.=.+518.
	line:	.=.+linsize
	eline:	.=.+4
	ifno:	.=.+2
	efno:	.=.+2
	errp:	.=.+2	/ init(errb)
	errb:	.=.+12.
	eerrb:		/ size 0 mod 4
	symtab:	.=.+symsize
	esymtab:
	esymp:	.=.+2	/ init(esymtab)
	symtp:	.=.+2
	namebuf:.=.+namsize
	enamebuf:
	namep:	.=.+2	/ init(namebuf)
.=.+40	/fake
	.=.+1		/ make odd
	symbuf:	.=.+smblsize	/ init(<_>)
	esymbuf:
	ch:	.=.+1
	ch1:	.=.+1
	progt:	.=.+2
	holquo:	.=.+2
	nxtaloc:.=.+2
	imptab:	.=.+[26.*2*2]	/ 26 letters, 2 alphabets, 2 bytes
	nerror:	.=.+2
	temp:	.=.+2
	functm:	.=.+2
edata:
dsize	=.-data

