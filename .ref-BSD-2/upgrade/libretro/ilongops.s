/ C library-- versions of long *, /, % for non-floating point hardware
/  also =* =% =/

.globl	lmul, almul
.globl	csv, cret

/
/ called:2(sp):LHS 6(sp):RHS
lmul:
	jsr	r5,csv
	mov	4.(r5),r2
	mov	6.(r5),r3
	clr	r0
	clr	r1
	mov	$32.,r4
1:
	asl	r1
	rol	r0
	rol	r3
	rol	r2
	bcc	2f
	add	10.(r5),r1
	adc	r0
	add	8.(r5),r0
2:
	sob	r4,1b
	jmp	cret

/ call is 2(sp):LHSptr 4(sp):RHS
almul:
	jsr	r5,csv
	mov	4.(r5),r3
	mov	(r3)+,r2
	mov	(r3)+,r3
	clr	r0
	clr	r1
	mov	$32.,r4
1:
	asl	r1
	rol	r0
	rol	r3
	rol	r2
	bcc	2f
	add	8.(r5),r1
	adc	r0
	add	6.(r5),r0
2:
	sob	r4,1b
	mov	4.(r5),r3
	mov	r0,(r3)+
	mov	r1,(r3)+
	jmp	cret

.globl	ldiv
.globl	lrem

/ Both called 2(sp):LHS 6(sp):RHS
lrem:
	jsr	r5,csv
	mov	$100,-(sp)
	br	1f

ldiv:
	jsr	r5,csv
	clr	-(sp)
1:
	mov	6.(r5),r3
	mov	4.(r5),r2
	bpl	1f
	inc	(sp)
	neg	r2
	neg	r3
	sbc	r2
1:
	clr	r0
	clr	r1
	tst	8.(r5)
	bge	1f
	neg	8.(r5)
	neg	10.(r5)
	sbc	8.(r5)
	inc	(sp)
1:
	mov	$32.,r4
1:
	ashc	$1,r2
	rol	r1
	rol	r0
	cmp	8.(r5),r0
	blt	2f
	bgt	3f
	cmp	10.(r5),r1
	blos	2f
3:
	sob	r4,1b
	br	1f
2:
	sub	10.(r5),r1
	sbc	r0
	sub	8.(r5),r0
	inc	r3
	sob	r4,1b
1:
	bit	$100,(sp)
	beq	3f
	tst	4.(r5)
	bpl	1f
	br	2f
3:
	mov	r2,r0
	mov	r3,r1
	bit	$1,(sp)+
	beq	1f
2:
	neg	r0
	neg	r1
	sbc	r0
1:
	jmp	cret

.globl	alrem, aldiv

/ Called 2(sp):LHSptr 4(sp):RHS

alrem:
	jsr	r5,csv
	mov	$100,-(sp)
	br	1f

aldiv:
	jsr	r5,csv
	clr	-(sp)
1:
	mov	4.(r5),r2
	mov	2(r2),r3
	mov	(r2),r2
	bpl	1f
	inc	(sp)
	neg	r2
	neg	r3
	sbc	r2
1:
	clr	r0
	clr	r1
	tst	6.(r5)
	bge	1f
	neg	6.(r5)
	neg	8.(r5)
	sbc	6.(r5)
	inc	(sp)
1:
	mov	$32.,r4
1:
	ashc	$1,r2
	rol	r1
	rol	r0
	cmp	6.(r5),r0
	blt	2f
	bgt	3f
	cmp	8.(r5),r1
	blos	2f
3:
	sob	r4,1b
	br	1f
2:
	sub	8.(r5),r1
	sbc	r0
	sub	6.(r5),r0
	inc	r3
	sob	r4,1b
1:
	mov	4(r5),r4
	bit	$100,(sp)
	beq	3f
	tst	(r4)
	bpl	1f
	br	2f
3:
	mov	r2,r0
	mov	r3,r1
	bit	$1,(sp)+
	beq	1f
2:
	neg	r0
	neg	r1
	sbc	r0
1:
	mov	r0,(r4)+
	mov	r1,(r4)
	jmp	cret
