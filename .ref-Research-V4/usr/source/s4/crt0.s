/ C runtime startoff

.globl	savr5

.globl	_main

start:
	setd
	mov	sp,r0
	mov	(r0),-(sp)
	tst	(r0)+
	mov	r0,2(sp)
	jsr	pc,_main
	cmp	(sp)+,(sp)+
	clr	r0
	sys	exit

.bss
savr5:	.=.+2
