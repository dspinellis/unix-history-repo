/ C library -- signal

/ signal(n, 0); /* default action on signal(n) */
/ signal(n, odd); /* ignore signal(n) */
/ signal(n, label); /* goto label on signal(n) */

rti	= 2
signal	= 48.
.globl	_signal, retrn, cerror

NSIG	= 13.

_signal:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r1
	mov	r1,0f
	cmp	r1,$NSIG
	bhis	2f
	mov	6(r5),0f+2
	ash	$3,r1
	add	$vect,r1
	mov	4(r1),6(r1)
	mov	0f+2,4(r1)
	beq	1f
	bit	$1,0f+2
	bne	1f
	mov	r1,0f+2
1:
	sys	0; 9f
	bes	3f
	bit	$1,r0
	bne	4f
	tst	r0
	beq	4f
	mov	6(r1),r0
4:
	jmp	retrn
2:
	mov	$22.,r0		/ EINVAL
3:
	jmp	cerror

1:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r4,-(sp)
	jsr	pc,*(r0)+
	mov	(sp)+,r4
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	mov	(sp)+,r0
	rti

.data
vect:
	jsr	r0,1b; ..; ..
	jsr	r0,1b; ..; ..
	jsr	r0,1b; ..; ..
	jsr	r0,1b; ..; ..
	jsr	r0,1b; ..; ..
	jsr	r0,1b; ..; ..
	jsr	r0,1b; ..; ..
	jsr	r0,1b; ..; ..
	jsr	r0,1b; ..; ..
	jsr	r0,1b; ..; ..
	jsr	r0,1b; ..; ..
	jsr	r0,1b; ..; ..
	jsr	r0,1b; ..; ..
evect:
9:
	sys	signal; 0:..; ..
