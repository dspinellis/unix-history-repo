.LS .ID
.ta 8n 16n 24n
.nf
/
/ Modified C runtime startoff for pcx
/ with floating point interpreter
/
\&.globl	savr5
\&.globl	fptrap
\&.globl	_exit, _pxpbuf, _main

start:
/
/ If non shared text the following
/ three words will be the beginning of the core image
/
	br	1f
	0			/ 0 = Non-shared text
	_pxpbuf
1:
	sys	signal; 4; fptrap
	setd
	mov	sp,r0
	mov	(r0),-(sp)
	tst	(r0)+
	mov	r0,2(sp)
	jsr	pc,_main
	cmp	(sp)+,(sp)+
	mov	r0,(sp)
	jsr	pc,*$_exit
	sys	exit

\&.bss
savr5:	.=.+2
\&.data
/
/ If pcx is loaded with shared text -n
/ or separate i and d -i, then the
/ following three words will be the first
/ in the core image.  Note that in this
/ case the pointer to the buffer will
/ be deceivingly large.  The first word
/ of offset can be subtracted to correct
/ it without having to do the involved
/ calculations to really calculate the
/ address of the buffer correctly.  We
/ can get away with this because we know the
/ buffer is not in stack space.
/
_info:
	_info
	1		/ 1 = pure text
	_pxpbuf
.fi
.LE
