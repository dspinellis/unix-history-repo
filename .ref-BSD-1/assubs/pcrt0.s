/ Modified C runtime startoff for pcx
/ with floating point interpreter

.globl	savr5
.globl	fptrap
.globl	_exit, _pxpbuf

.globl	_main

start:
	br	1f
	0
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

.bss
savr5:	.=.+2
.data
_info:
	_info
	1
	 _pxpbuf
