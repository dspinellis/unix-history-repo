/
/

/ a6 -- pdp-11 assembler pass 2

opline:
	mov	r4,r0
	jsr	r5,betwen; 0; 177
		br 2f
	cmp	r4,$5
	beq	opeof
	cmp	r4,$'<
	bne	xpr
	jmp	opl17
xxpr:
	tst	(sp)+
xpr:
	jsr	pc,expres
	jsr	pc,outw
	rts	pc
2:
	movb	(r4),r0
	cmp	r0,$24		/reg
	beq	xpr
	cmp	r0,$33		/est text
	beq	xpr
	cmp	r0,$34		/ est data
	beq	xpr
	jsr	r5,betwen; 5; 36
		br xpr
	mov	2(r4),-(sp)
	mov	r0,-(sp)
	jsr	pc,readop
	mov	(sp)+,r0
	asl	r0
	mov	$adrbuf,r5
	clr	swapf
	mov	$-1,rlimit
	jmp	*1f-10.(r0)

1:
	opl5
	opl6
	opl7
	opl10
	opl11
	opl12
	opl13
	opl14
	opl15
	opl16
	opl17
	opl20
	opl21
	opl22
	opl23
	xxpr
	opl25
	opl26
	opl27
	opl30
	opl31
	opl32
	xxpr
	xxpr
	opl35
	opl36

opeof:
	mov	$1,line
	mov	$20,-(sp)
	mov	$argb,r1
1:
	jsr	pc,getw
	tst	r4
	bmi	1f
	movb	r4,(r1)+
	dec	(sp)
	bgt	1b
	tstb	-(r1)
	br	1b
1:
	movb	$'\n,(r1)+
	clrb	(r1)+
	tst	(sp)+
	rts	pc

opl30:	/ mpy, dvd etc
	inc	swapf
	mov	$1000,rlimit
	br	opl13

opl14:		/ flop freg,fsrc
	inc	swapf

opl5:		/ flop src,freg
	mov	$400,rlimit

/double
opl13:
	jsr	pc,addres
op2a:
	mov	r2,-(sp)
	jsr	pc,readop
op2b:
	jsr	pc,addres
	tst	swapf
	beq	1f
	mov	(sp),r0
	mov	r2,(sp)
	mov	r0,r2
1:
	swab	(sp)
	asr	(sp)
	asr	(sp)
	cmp	(sp),rlimit
	blo	1f
	jsr	r5,error; 'x
1:
	bis	(sp)+,r2
	bis	(sp)+,r2
	clr	r3
	jsr	pc,outw
	mov	$adrbuf,r1
1:
	cmp	r1,r5
	bhis	1f
	mov	(r1)+,r2
	mov	(r1)+,r3
	mov	(r1)+,xsymbol
	jsr	pc,outw
	br	1b
1:
	rts	pc

opl15:		/ single operand
	clr	-(sp)
	br	op2b

opl12:		/ movf
	mov	$400,rlimit
	jsr	pc,addres
	cmp	r2,$4		/ see if source is fregister
	blo	1f
	inc	swapf
	br	op2a
1:
	mov	$174000,(sp)
	br	op2a

/ jbr
opl35:
/ jeq, jne, etc
opl36:
	jsr	pc,expres
	tstb	passno
	bne	1f
	mov	r2,r0
	jsr	pc,setbr
	tst	r2
	beq	2f
	cmp	(sp),$br
	beq	2f
	add	$2,r2
2:
	add	r2,dot		/ if doesn't fit
	add	$2,dot
	tst	(sp)+
	rts	pc
1:
	jsr	pc,getbr
	bcc	dobranch
	mov	(sp)+,r0
	mov	r2,-(sp)
	mov	r3,-(sp)
	cmp	r0,$br
	beq	2f
	mov	$402,r2
	xor	r0,r2		/ flip cond, add ".+6"
	mov	$1,r3
	jsr	pc,outw
2:
	mov	$1,r3
	mov	$jmp+37,r2
	jsr	pc,outw
	mov	(sp)+,r3
	mov	(sp)+,r2
	jsr	pc,outw
	rts	pc

/sob
opl31:	/ sob
	jsr	pc,expres
	jsr	pc,checkreg
	swab	r2
	asr	r2
	asr	r2
	bis	r2,(sp)
	jsr	pc,readop
	jsr	pc,expres
	tstb	passno
	beq	3f
	sub	dot,r2
	neg	r2
	mov	r2,r0
	jsr	r5,betwen; -2; 175
		br 2f
	add	$4,r2
	br	1f

/branch
opl6:
	jsr	pc,expres
	tstb	passno
	beq	3f
dobranch:
	sub	dot,r2
	mov	r2,r0
	jsr	r5,betwen; -254.; 256.
		br 2f
1:
	bit	$1,r2
	bne	2f
	cmp	r3,dot-2	/ same relocation as .
	bne	2f
	asr	r2
	dec	r2
	bic	$177400,r2
3:
	bis	(sp)+,r2
	clr	r3
	jsr	pc,outw
	rts	pc
2:
	jsr	r5,error; 'b
	clr	r2
	br	3b

/jsr
opl7:
	jsr	pc,expres
	jsr	pc,checkreg
	jmp	op2a

/ rts
opl10:
	jsr	pc,expres
	jsr	pc,checkreg
	br	1f

/ sys, emt etc
opl11:
	jsr	pc,expres
	cmp	r2,$64.
	bhis	0f
	cmp	r3,$1
	ble	1f
0:
	jsr	pc,errora
1:
	bis	(sp)+,r2
	jsr	pc,outw
	rts	pc

/ .byte
opl16:
	jsr	pc,expres
	jsr	pc,outb
	cmp	r4,$',
	bne	1f
	jsr	pc,readop
	br	opl16
1:
	tst	(sp)+
	rts	pc

/ < (.ascii)
opl17:
	jsr	pc,getw
	mov	$1,r3
	mov	r4,r2
	bmi	2f
	bic	$!377,r2
	jsr	pc,outb
	br	opl17
2:
	jsr	pc,getw
	rts	pc

/.even
opl20:
	bit	$1,dot
	beq	1f
	cmp	dot-2,$4
	beq	2f		/ bss mode
	clr	r2
	clr	r3
	jsr	pc,outb
	br	1f
2:
	inc	dot
1:
	tst	(sp)+
	rts	pc
opl21:	/if
	jsr	pc,expres
opl22:
oplret:
	tst	(sp)+
	rts	pc


/.globl
opl23:
	cmp	r4,$200
	blo	1f
	bisb	$40,(r4)
	jsr	pc,readop
	cmp	r4,$',
	bne	1f
	jsr	pc,readop
	br	opl23
1:
	tst	(sp)+
	rts	pc

/ .text, .data, .bss
opl25:
opl26:
opl27:
	inc	dot
	bic	$1,dot
	mov	r0,-(sp)
	mov	dot-2,r1
	asl	r1
	mov	dot,savdot-4(r1)
	tstb	passno
	beq	1f
	jsr	r5,flush; txtp
	jsr	r5,flush; relp
	mov	(sp),r2
	add	$txtseek-[2*25],r2
	mov	r2,tseekp
	mov	(r2),r0
	jsr	r5,oset; txtp
	add	$trelseek-txtseek,r2
	mov	(r2),r0
	mov	r2,rseekp
	jsr	r5,oset; relp
1:
	mov	(sp)+,r0
	mov	savdot-[2*25](r0),dot
	asr	r0
	sub	$25-2,r0
	mov	r0,dot-2	/ new . relocation
	tst	(sp)+
	rts	pc

opl32:
	cmp	r4,$200
	blo	1f
	mov	r4,-(sp)
	jsr	pc,readop
	jsr	pc,readop
	jsr	pc,expres
	mov	(sp)+,r0
	bit	$37,(r0)
	bne	1f
	bis	$40,(r0)
	mov	r2,2(r0)
1:
	tst	(sp)+
	rts	pc

addres:
	clr	-(sp)
4:
	cmp	r4,$'(
	beq	alp
	cmp	r4,$'-
	beq	amin
	cmp	r4,$'$
	beq	adoll
	cmp	r4,$'*
	bne	getx
	jmp	astar
getx:
	jsr	pc,expres
	cmp	r4,$'(
	bne	2f
	jsr	pc,readop
	mov	r2,(r5)+
	mov	r3,(r5)+
	mov	xsymbol,(r5)+
	jsr	pc,expres
	jsr	pc,checkreg
	jsr	pc,checkrp
	bis	$60,r2
	bis	(sp)+,r2
	rts	pc

2:
	cmp	r3,$24
	bne	1f
	jsr	pc,checkreg
	bis	(sp)+,r2
	rts	pc
1:
	mov	r3,-(sp)
	bic	$40,r3
	mov	(sp)+,r3
	bis	$100000,r3
	sub	dot,r2
	sub	$4,r2
	cmp	r5,$adrbuf
	beq	1f
	sub	$2,r2
1:
	mov	r2,(r5)+		/ index
	mov	r3,(r5)+		/ index reloc.
	mov	xsymbol,(r5)+		/ index global
	mov	$67,r2			/ address mode
	bis	(sp)+,r2
	rts	pc

alp:
	jsr	pc,readop
	jsr	pc,expres
	jsr	pc,checkrp
	jsr	pc,checkreg
	cmp	r4,$'+
	beq	1f
	tst	(sp)+
	beq	2f
	bis	$70,r2
	clr	(r5)+
	clr	(r5)+
	mov	xsymbol,(r5)+
	rts	pc
2:
	bis	$10,r2
	rts	pc
1:
	jsr	pc,readop
	bis	$20,r2
	bis	(sp)+,r2
	rts	pc

amin:
	jsr	pc,readop
	cmp	r4,$'(
	beq	1f
	mov	r4,savop
	mov	$'-,r4
	br	getx
1:
	jsr	pc,readop
	jsr	pc,expres
	jsr	pc,checkrp
	jsr	pc,checkreg
	bis	(sp)+,r2
	bis	$40,r2
	rts	pc

adoll:
	jsr	pc,readop
	jsr	pc,expres
	mov	r2,(r5)+
	mov	r3,(r5)+
	mov	xsymbol,(r5)+
	mov	(sp)+,r2
	bis	$27,r2
	rts	pc

astar:
	tst	(sp)
	beq	1f
	jsr	r5,error; '*
1:
	mov	$10,(sp)
	jsr	pc,readop
	jmp	4b

errora:
	jsr	r5,error; 'a
	rts	pc

checkreg:
	cmp	r2,$7
	bhi	1f
	cmp	r1,$1
	blos	2f
	cmp	r3,$5
	blo	1f
2:
	rts	pc
1:
	jsr	pc,errora
	clr	r2
	clr	r3
	rts	pc

errore:
	jsr	r5,error; 'e
	rts	pc

checkrp:
	cmp	r4,$')
	beq	1f
	jsr	r5,error; ')
	rts	pc
1:
	jsr	pc,readop
	rts	pc

setbr:
	mov	brtabp,r1
	cmp	r1,$brlen
	blt	1f
	mov	$2,r2
	rts	pc
1:
	inc	brtabp
	clr	-(sp)
	sub	dot,r0
	ble	1f
	sub	brdelt,r0
1:
	jsr	r5,betwen; -254.; 256.
		br 1f
	br	2f
1:
	mov	r1,-(sp)
	bic	$!7,(sp)
	mov	$1,r0
	ash	(sp)+,r0
	ash	$-3,r1
	bisb	r0,brtab(r1)
	mov	$2,(sp)
2:
	mov	(sp)+,r2
	rts	pc

getbr:
	mov	brtabp,r1
	cmp	r1,$brlen
	blt	1f
	sec
	rts	pc
1:
	mov	r1,-(sp)
	bic	$!7,(sp)
	neg	(sp)
	inc	brtabp
	ash	$-3,r1
	movb	brtab(r1),r1
	ash	(sp)+,r1
	ror	r1		/ 0-bit into c-bit
	rts	pc
