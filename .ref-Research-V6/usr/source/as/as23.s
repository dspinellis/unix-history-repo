/
/

/ a3 -- pdp-11 assembler pass 2

assem:
	jsr	pc,readop
	cmp	r4,$5
	beq	2f
	cmp	r4,$'<
	beq	2f
	jsr	pc,checkeos
		br eal1
	mov	r4,-(sp)
	cmp	(sp),$1
	bne	1f
	mov	$2,(sp)
	jsr	pc,getw
	mov	r4,numval
1:
	jsr	pc,readop
	cmp	r4,$'=
	beq	4f
	cmp	r4,$':
	beq	1f
	mov	r4,savop
	mov	(sp)+,r4
2:
	jsr	pc,opline
dotmax:
	tstb	passno
	bne	eal1
	movb	dotrel,r0
	asl	r0
	cmp	dot,txtsiz-4(r0)
	blos	ealoop
	mov	dot,txtsiz-4(r0)
eal1:
	jmp	ealoop
1:
	mov	(sp)+,r4
	cmp	r4,$200
	bhis	1f
	cmp	r4,$2
	beq	3f
	jsr	r5,error; 'x
	br	assem
1:
	tstb	passno
	bne	2f
	movb	(r4),r0
	bic	$!37,r0
	beq	5f
	cmp	r0,$33
	blt	6f
	cmp	r0,$34
	ble	5f
6:
	jsr	r5,error; 'm
5:
	bic	$37,(r4)
	bis	dotrel,(r4)
	mov	2(r4),brdelt
	sub	dot,brdelt
	mov	dot,2(r4)
	br	assem
2:
	cmp	dot,2(r4)
	beq	assem
	jsr	r5,error; 'p
	br	assem
3:
	mov	numval,r4
	jsr	pc,fbadv
	asl	r4
	mov	curfb(r4),r0
	movb	dotrel,(r0)
	mov	2(r0),brdelt
	sub	dot,brdelt
	mov	dot,2(r0)
	br	assem
4:
	jsr	pc,readop
	jsr	pc,expres
	mov	(sp)+,r1
	cmp	r1,$symtab	/test for dot
	bne	1f
	bic	$40,r3
	cmp	r3,dotrel	/ can't change relocation
	bne	2f
	cmp	r3,$4		/ bss
	bne	3f
	mov	r2,dot
	br	dotmax
3:
	sub	dot,r2
	bmi	2f
	mov	r2,-(sp)
3:
	dec	(sp)
	bmi	3f
	clr	r2
	mov	$1,r3
	jsr	pc,outb
	br	3b
3:
	tst	(sp)+
	br	dotmax
2:
	jsr	r5,error; '.
	br	ealoop
1:
	cmp	r3,$40
	bne	1f
	jsr	r5,error; 'r
1:
	bic	$37,(r1)
	bic	$!37,r3
	bne	1f
	clr	r2
1:
	bisb	r3,(r1)
	mov	r2,2(r1)

ealoop:
	cmp	r4,$'\n
	beq	1f
	cmp	r4,$'\e
	bne	9f
	rts	pc
1:
	inc	line
9:
	jmp	assem

checkeos:
	cmp	r4,$'\n
	beq	1f
	cmp	r4,$';
	beq	1f
	cmp	r4,$'\e
	beq	1f
	add	$2,(sp)
1:
	rts	pc

fbadv:
	asl	r4
	mov	nxtfb(r4),r1
	mov	r1,curfb(r4)
	bne	1f
	mov	fbbufp,r1
	br	2f
1:
	add	$4,r1
2:
	cmpb	1(r1),r4
	beq	1f
	tst	(r1)
	bpl	1b
1:
	mov	r1,nxtfb(r4)
	asr	r4
	rts	pc

