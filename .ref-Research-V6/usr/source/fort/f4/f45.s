/
/

/ f45 -- constant pool

.globl	constn
.globl	evalcon

.globl	error
.globl	perror
.globl	code
.globl	getcon
.globl	setln
.globl	getln
.globl	xbuf
.globl	symbuf
.globl	negflg

constn:
	jsr	r5,setln
	mov	$xbuf+518.,r3		/ pool max pointer
	mov	$ibuf+518.,r4		/ pool pointer pointer
1:
	jsr	r5,getln
		br 1f
	cmp	r0,$'c
	bne	1b
	jsr	r5,packcon
	mov	r2,-(r4)		/ put p ptr in p p ptr
	cmp	r3,r4
	blo	1b
	jsr	r5,error; 99.
	jsr	r5,perror
1:
	mov	$xbuf+518.,r2
1:
	cmp	r2,r3
	bhis	1f
	mov	$ibuf+518.,r1
2:
	cmp	r1,r4
	blo	2f
	cmp	-(r1),r2
	bne	2b
	mov	r1,r0
	sub	$ibuf+516.,r0
	asr	r0
	neg	r0
	jsr	r5,code
		<c%d:\n\0>; .even
		r0
	br	2b
2:
	mov	(r2)+,r0
	jsr	r5,code
		<	%o\n\0>; .even
		r0
	br	1b
1:
	rts	r5

packcon:
	mov	$line,r1
	jsr	r5,evalcon
	mov	r1,-(sp)
	mov	r3,-(sp)
	sub	r2,(sp)
	asr	(sp)
	mov	r2,-(sp)
	mov	$xbuf+518.,r2
1:
	mov	(sp),r3
	mov	r2,r1
	tst	(r2)+
	mov	2(sp),r0
2:
	cmp	(r1)+,(r3)+
	bne	1b
	dec	r0
	bgt	2b
	tst	-(r2)
	mov	(sp)+,r3
	asl	(sp)
	add	r2,(sp)
	cmp	(sp),r3
	blos	1f
	mov	(sp),r3			/ eureka
1:
	tst	(sp)+
	mov	(sp)+,r1
	rts	r5

evalcon:
	cmpb	efno,$5
	bne	2f
	movb	efno+1,r0
	mov	r3,r2
	br	1f
2:
	jsr	r5,getcon
	tst	negflg
	beq	2f
	negf	fr0
	negf	fr1
2:
	mov	r3,r2
	mov	efno,r0
	mov	r0,r1
	clrb	r0
	swab	r0
	bic	$!7,r1
	cmpb	r1,$realcon
	beq	3f
	cmpb	r1,$cplxcon
	beq	2f
	setl
	movfi	r0,symbuf+1
	seti
	mov	$symbuf+5,r1
	sub	r0,r1
	br	1f
2:
	mov	$symbuf+1,r1
	movf	fr1,symbuf+1
	cmp	r0,$8
	beq	2f
	movf	fr0,symbuf+9.
	br	1f
2:
	movf	fr0,symbuf+5
	br	1f
3:
	movf	fr0,symbuf+1
	mov	$symbuf+1,r1
1:
	movb	(r1)+,(r3)+
	dec	r0
	bgt	1b
	bit	$1,r3
	beq	1f
	clrb	(r3)+
1:
	rts	r5

