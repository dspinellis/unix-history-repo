/ C runtime startoff

.globl	retrn

.globl	_main

start:
	mov	$mq,r4
	mov	sp,r0
	mov	(r0),-(sp)
	tst	(r0)+
	mov	r0,2(sp)
	jsr	pc,*_main
	clr	r0
	sys	exit

retrn:
	mov	r5,sp
	mov	(sp)+,r5
	rts	pc

