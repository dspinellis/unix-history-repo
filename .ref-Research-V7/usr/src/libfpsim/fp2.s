/ fp2 -- floating point simulation

i.ldx:
	mov	(r3)+,(r2)+
	mov	(r3)+,(r2)+
	bit	$m.ext,fpsr
	beq	1f
	mov	(r3)+,(r2)+
	mov	(r3)+,(r2)+
	rts	pc
1:
	clr	(r2)+
	clr	(r2)+
	rts	pc

i.stx:
	mov	(r2)+,(r3)+
	mov	(r2)+,(r3)+
	bit	$m.ext,fpsr
	beq	1f
	mov	(r2)+,(r3)+
	mov	(r2)+,(r3)+
1:
	jmp	ret			/ does not set cc's

i.clrx:
	clr	(r3)+
	clr	(r3)+
	bit	$m.ext,fpsr
	beq	1f
	clr	(r3)+
	clr	(r3)+
1:
	rts	pc

i.negx:
	tst	(r3)
	beq	1f
	add	$100000,(r3)
1:
	rts	pc

i.absx:
	bic	$!77777,(r3)
	rts	pc

i.tstx:
	rts	pc

i.cmpx:
	mov	$areg,r5
	tst	(r2)
	bge	1f
	tst	(r3)
	bge	1f
	cmp	(r2),(r3)
	bgt	4f
	blt	3f
1:
	cmp	(r2)+,(r3)+
	bgt	3f
	blt	4f
	cmp	(r2)+,(r3)+
	bne	1f
	bit	$m.ext,fpsr
	beq	2f
	cmp	(r2)+,(r3)+
	bne	1f
	cmp	(r2)+,(r3)+
	beq	2f
1:
	bhi	3f
4:
	movb	$1,1(r5)
	rts	pc
3:
	mov	$-1,(r5)
	rts	pc
2:
	clr	(r5)
	rts	pc

i.ldcyx:
	mov	(r3)+,(r2)+
	mov	(r3)+,(r2)+
	bit	$m.ext,fpsr
	bne	1f
	mov	(r3)+,(r2)+
	mov	(r3)+,(r2)+
	rts	pc
1:
	clr	(r2)+
	clr	(r2)+
	rts	pc

i.stcxy:
	mov	(r2)+,(r3)+
	mov	(r2)+,(r3)+
	bit	$m.ext,fpsr
	bne	1f
	clr	(r3)+
	clr	(r3)+
1:
	rts	pc

i.ldcjx:
	mov	$asign,r0
	mov	$1,(r0)+
	mov	(r3)+,(r0)+
	bit	$m.lngi,fpsr
	beq	1f
	mov	(r3)+,(r0)+
	clr	(r0)+
	clr	(r0)+
	mov	$32.-8,(r0)+
	jmp	saret
1:
	clr	(r0)+
	clr	(r0)+
	clr	(r0)+
	mov	$16.-8,(r0)
	jmp	saret

i.stcxj:
	mov	r3,r5
	mov	$asign,r0
	jsr	pc,seta
	clr	r4
	mov	$areg,r0
	mov	(r0)+,r1
	mov	(r0)+,r2
	mov	(r0)+,r3
	mov	aexp,r0
1:
	cmp	r0,$48.-8
	bge	1f
	clc
	ror	r1
	ror	r2
	ror	r3
	inc	r0
	br	1b
1:
	bgt	7f
	tst	r1
	beq	1f
7:
	bis	$1,r4			/ C-bit
1:
	bit	$m.lngi,fpsr
	beq	1f
	tst	asign
	bge	2f
	neg	r3
	adc	r2
	bcs	2f
	neg	r2
	bis	$10,r4			/ N-bit
2:
	mov	r2,(r5)
	mov	r3,2(r5)
	bis	r2,r3
	br	8f
1:
	tst	r2
	beq	1f
	bis	$1,r4			/ C-bit
1:
	tst	asign
	bge	2f
	neg	r3
	bis	$10,r4			/ N-bit
2:
	mov	r3,(r5)
8:
	bne	1f
	bis	$4,r4			/ Z-bit
1:
	bic	$17,sps
	bic	$17,fpsr
	bis	r4,sps
	bis	r4,fpsr
	jmp	ret

xoflo:
	bis	$1,fpsr			/ set fixed overflow (carry)
	jmp	ret

i.ldexp:
	mov	$asign,r0
	jsr	pc,seta
	mov	(r3),aexp
	jsr	pc,reta
	jmp	sret

i.stexp:
	mov	$asign,r0
	jsr	pc,seta
	mov	aexp,(r3)
	mov	r3,r5
	bic	$17,sps
	tst	(r3)
	bmi	1f
	bne	2f
	bis	$4,sps			/ Z-bit
	br	2f
1:
	bis	$10,sps			/ N-bit
2:
	jmp	sret

i.ldfps:
	mov	(r3),fpsr
	jmp	ret

i.stfps:
	mov	fpsr,(r3)
	jmp	ret

