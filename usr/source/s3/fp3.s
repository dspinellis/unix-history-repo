/ fp3 -- floating simulation

i.addx:
	jsr	pc,setab
	br	1f

i.subx:
	jsr	pc,setab
	neg	bsign
1:
	tst	bsign
	beq	reta
	tst	asign
	beq	retb
	mov	areg+8,r1
	sub	breg+8,r1
	blt	1f
	beq	2f
	cmp	r1,$56.
	bge	reta
	mov	$breg,r0
	br	4f
1:
	neg	r1
	cmp	r1,$56.
	bge	retb
	mov	$areg,r0
4:
	mov	r1,-(sp)
	mov	(r0)+,r1
	mov	(r0)+,r2
	mov	(r0)+,r3
	mov	(r0)+,r4
	add	(sp),(r0)
1:
	clc
	ror	r1
	ror	r2
	ror	r3
	ror	r4
	dec	(sp)
	bgt	1b
	mov	r4,-(r0)
	mov	r3,-(r0)
	mov	r2,-(r0)
	mov	r1,-(r0)
	tst	(sp)+
2:
	mov	$areg+8,r1
	mov	$breg+8,r2
	mov	$4,r0
	cmp	asign,bsign
	bne	4f
	clc
1:
	adc	-(r1)
	bcs	3f
	add	-(r2),(r1)
2:
	dec	r0
	bne	1b
	br	5f
3:
	add	-(r2),(r1)
	sec
	br	2b
	br	5f
4:
	clc
1:
	sbc	-(r1)
	bcs	3f
	sub	-(r2),(r1)
2:
	dec	r0
	bne	1b
	br	5f
3:
	sub	-(r2),(r1)
	sec
	br	2b

saret:
	mov	$areg,r1
5:
	tst	(r1)
	bge	3f
	mov	$areg+8,r1
	mov	$4,r0
	clc
1:
	adc	-(r1)
	bcs	2f
	neg	(r1)
2:
	dec	r0
	bne	1b
	neg	-(r1)
3:
	jsr	pc,norm
	br	reta

retb:
	mov	$bsign,r1
	mov	$asign,r2
	mov	$6,r0
1:
	mov	(r1)+,(r2)+
	dec	r0
	bne	1b

reta:
	mov	r5,r2
	mov	$asign,r0
	tst	(r0)
	beq	unflo
	mov	aexp,r1
	cmp	r1,$177
	bgt	ovflo
	cmp	r1,$-177
	blt	unflo
	add	$200,r1
	swab	r1
	clc
	ror	r1
	tst	(r0)+
	bge	1f
	bis	$100000,r1
1:
	bic	$!177,(r0)
	bis	(r0)+,r1
	mov	r1,(r2)+
	mov	(r0)+,(r2)+
	bit	$m.ext,fpsr
	beq	1f
	mov	(r0)+,(r2)+
	mov	(r0)+,(r2)+
1:
	rts	pc

unflo:
	clr	(r2)+
	clr	(r2)+
	bit	$m.ext,fpsr
	beq	1f
	clr	(r2)+
	clr	(r2)+
1:
	rts	pc

ovflo:
	bis	$2,fpsr			/ set v-bit (overflow)
	jmp	ret

i.mulx:
	jsr	pc,i.mul
	br	saret

i.modx:
	jsr	pc,i.mul
	jsr	pc,norm
	mov	$asign,r0
	mov	$bsign,r1
	mov	$6,r2
1:
	mov	(r0)+,(r1)+
	dec	r2
	bne	1b
	clr	r0		/ count
	mov	$200,r1		/ bit
	clr	r2		/ reg offset
1:
	cmp	r0,aexp
	bge	2f		/ in fraction
	bic	r1,areg(r2)
	br	3f
2:
	bic	r1,breg(r2)
3:
	inc	r0
	clc
	ror	r1
	bne	1b
	mov	$100000,r1
	add	$2,r2
	cmp	r2,$8
	blt	1b
	jsr	pc,norm
	jsr	pc,reta
	cmp	r5,$ac1
	beq	1f
	cmp	r5,$ac3
	beq	1f
	bit	$200,breg
	bne	2f
	clr	bsign
2:
	add	$8,r5
	jsr	pc,retb
	sub	$8,r5
1:
	rts	pc

i.divx:
	jsr	pc,setab
	tst	bsign
	beq	ovflo
	sub	bexp,aexp
	jsr	pc,xorsign
	mov	r5,-(sp)
	mov	$areg,r0
	mov	(r0),r1
	clr	(r0)+
	mov	(r0),r2
	clr	(r0)+
	mov	(r0),r3
	clr	(r0)+
	mov	(r0),r4
	clr	(r0)+
	mov	$areg,r5
	mov	$400,-(sp)
1:
	mov	$breg,r0
	cmp	(r0)+,r1
	blt	2f
	bgt	3f
	cmp	(r0)+,r2
	blo	2f
	bhi	3f
	cmp	(r0)+,r3
	blo	2f
	bhi	3f
	cmp	(r0)+,r4
	bhi	3f
2:
	mov	$breg,r0
	sub	(r0)+,r1
	clr	-(sp)
	sub	(r0)+,r2
	adc	(sp)
	clr	-(sp)
	sub	(r0)+,r3
	adc	(sp)
	sub	(r0)+,r4
	sbc	r3
	adc	(sp)
	sub	(sp)+,r2
	adc	(sp)
	sub	(sp)+,r1
	bis	(sp),(r5)
3:
	asl	r4
	rol	r3
	rol	r2
	rol	r1
	clc
	ror	(sp)
	bne	1b
	mov	$100000,(sp)
	add	$2,r5
	cmp	r5,$areg+8
	blo	1b
	tst	(sp)+
	mov	(sp)+,r5
	jmp	saret


i.mul:
	jsr	pc,setab
	add	bexp,aexp
	dec	aexp
	jsr	pc,xorsign
	mov	r5,-(sp)
	mov	$breg+4,r5
	bit	$m.ext,fpsr
	beq	1f
	add	$4,r5
1:
	clr	r0
	clr	r1
	clr	r2
	clr	r3
	clr	r4
1:
	asl	r0
	bne	2f
	inc	r0
	tst	-(r5)
2:
	cmp	r0,$400
	bne	2f
	cmp	r5,$breg
	bhi	2f
	mov	$areg,r0
	mov	r1,(r0)+
	mov	r2,(r0)+
	mov	r3,(r0)+
	mov	r4,(r0)+
	mov	(sp)+,r5
	rts	pc
2:
	clc
	ror	r1
	ror	r2
	ror	r3
	ror	r4
	bit	r0,(r5)
	beq	1b
	mov	r0,-(sp)
	mov	$areg,r0
	add	(r0)+,r1
	clr	-(sp)
	add	(r0)+,r2
	adc	(sp)
	clr	-(sp)
	add	(r0)+,r3
	adc	(sp)
	add	(r0)+,r4
	adc	r3
	adc	(sp)
	add	(sp)+,r2
	adc	(sp)
	add	(sp)+,r1
	mov	(sp)+,r0
	br	1b

xorsign:
	cmp	asign,bsign
	beq	1f
	mov	$-1,asign
	rts	pc
1:
	mov	$1,asign
	rts	pc

