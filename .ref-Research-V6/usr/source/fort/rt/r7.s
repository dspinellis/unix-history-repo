/
/

/ r7 -- fortran runtime

achk	= 1 / 1 to check array bounds
.globl	lvalp
.globl	rval1p
.globl	rval2p
.globl	rval4p
.globl	rval8p
.globl	alval
.globl	alvalp
.globl	arval
.globl	arvalp

.globl	rerr

lvalp:
	mov	r3,r0
	add	(r4)+,r0
	mov	(r0)+,-(sp)
	jmp	*(r4)+

rval1p:
	mov	r3,r0
	add	(r4)+,r0
	movb	*(r0)+,-(sp)
	jmp	*(r4)+

rval2p:
	mov	r3,r0
	add	(r4)+,r0
	mov	*(r0)+,-(sp)
	jmp	*(r4)+

rval4p:
	mov	r3,r0
	add	(r4)+,r0
	mov	(r0),r0
	cmp	(r0)+,(r0)+
	mov	-(r0),-(sp)
	mov	-(r0),-(sp)
	jmp	*(r4)+

rval8p:
	setd
	mov	r3,r0
	add	(r4)+,r0
	movf	*(r0)+,fr0
	movf	fr0,-(sp)
	jmp	*(r4)+

alval:
	jsr	r5,getsub; 1
	mov	(r4)+,-(sp)
	add	r1,(sp)
	jmp	*(r4)+

arvalp:
	jsr	r5,getsub; 0
	mov	r3,r2
	add	(r4)+,r2
	mov	(r2),r2
	br	1f

arval:
	jsr	r5,getsub; 1
	mov	(r4)+,r2
1:
	add	r2,r1
	mov	-(r0),r2
	asr	r2
	bcc	1f
	movb	(r1),-(sp)
	jmp	*(r4)+
1:
	add	(r0),r1
1:
	mov	-(r1),-(sp)
	sob	r2,1b
	jmp	*(r4)+

getsub:
	tst	(sp)+
	mov	(r4)+,r0
	mov	(r0)+,r2
	clr	r1
	tst	(r0)+
1:
	tst	(sp)+
	dec	(sp)
	add	(sp)+,r1
	mpy	(r0)+,r1
	sob	r2,1b

	tst	(r5)+
	beq	2f
	mov	r1,-(sp)
	mov	-2(r4),r0
	mov	(r0)+,r2
	inc	r2
	mov	$1,r1
1:
	mpy	(r0)+,r1
	sob	r2,1b
.if achk
	cmp	r1,(sp)
	bhi	1f
	jsr	r5,rerr; 15.
.endif
1:
	mov	(sp)+,r1

2:
	jmp	(r5)

alvalp:
	jsr	r5,getsub; 0
	mov	r3,r0
	add	(r4)+,r0
	mov	(r0),-(sp)
	add	r1,(sp)
	jmp	*(r4)+
