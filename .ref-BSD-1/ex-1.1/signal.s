/ C library -- signal

/ signal(n, 0); /* default action on signal(n) */
/ signal(n, odd); /* ignore signal(n) */
/ signal(n, label); /* goto label on signal(n) */
/ returns old label, only one level.

rtt	= 6
signal	= 48.
.globl	_signal, cerror

_signal:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r1
	cmp	r1,$NSIG
	bhis	2f
	mov	6(r5),r0
	mov	r1,0f
	asl	r1
	mov	dvect(r1),-(sp)
	mov	r0,dvect(r1)
	mov	r0,0f+2
	beq	1f
	bit	$1,r0
	bne	1f
	asl	r1
	add	$tvect,r1
	mov	r1,0f+2
1:
	sys	0; 9f
	bes	3f
	bit	$1,r0
	beq	1f
	mov	r0,(sp)
1:
	mov	(sp)+,r0
	mov	(sp)+,r5
	rts	pc
2:
	mov	$22.,r0		/ EINVAL
3:
	jmp	cerror

NSIG = 0
tvect:
	jsr	r0,1f; NSIG=NSIG+1
	jsr	r0,1f; NSIG=NSIG+1
	jsr	r0,1f; NSIG=NSIG+1
	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1
/	jsr	r0,1f; NSIG=NSIG+1

1:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r4,-(sp)
	sub	$tvect+4,r0
	asr	r0
	mov	r0,-(sp)
	asr	(sp)
	jsr	pc,*dvect(r0)
	tst	(sp)+
	mov	(sp)+,r4
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	mov	(sp)+,r0
	rtt
.data
9:
	sys	signal; 0:..; ..
.bss
dvect:	.=.+[NSIG*2]
