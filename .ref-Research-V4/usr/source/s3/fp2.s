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
	rts	pc

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
	mov	$1,(r5)
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
	bgt	xoflo
	tst	r1
	bne	xoflo
1:
	bit	$m.lngi,fpsr
	beq	1f
	tst	asign
	bge	2f
	neg	r3
	adc	r2
	bcs	2f
	neg	r2
2:
	mov	r2,(r5)
	mov	r3,2(r5)
	rts	pc
1:
	tst	r2
	bne	xoflo
	tst	asign
	bge	2f
	neg	r3
2:
	mov	r3,(r5)
	rts	pc

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
	jmp	sret

i.ldfps:
	mov	(r3),fpsr
	jmp	ret

i.stfps:
	mov	fpsr,(r3)
	jmp	ret

