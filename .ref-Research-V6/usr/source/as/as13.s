/
/

/ a3 -- pdp-11 assembler pass 1

assem:
	jsr	pc,readop
	jsr	pc,checkeos
		br ealoop
	tst	ifflg
	beq	3f
	cmp	r4,$200
	blos	assem
	cmpb	(r4),$21	/if
	bne	2f
	inc	ifflg
2:
	cmpb	(r4),$22   /endif
	bne	assem
	dec	ifflg
	br	assem
3:
	mov	r4,-(sp)
	jsr	pc,readop
	cmp	r4,$'=
	beq	4f
	cmp	r4,$':
	beq	1f
	mov	r4,savop
	mov	(sp)+,r4
	jsr	pc,opline
	br	ealoop
1:
	mov	(sp)+,r4
	cmp	r4,$200
	bhis	1f
	cmp	r4,$1		/ digit
	beq	3f
	jsr	r5,error; 'x
	br	assem
1:
	bitb	$37,(r4)
	beq	1f
	jsr	r5,error; 'm
1:
	bisb	dot-2,(r4)
	mov	dot,2(r4)
	br	assem
3:
	mov	numval,r0
	jsr	pc,fbcheck
	movb	dotrel,curfbr(r0)
	asl	r0
	movb	dotrel,nxtfb
	mov	dot,nxtfb+2
	movb	r0,nxtfb+1
	mov	dot,curfb(r0)
	movb	fbfil,r0
	sys	write; nxtfb; 4
	br	assem
4:
	jsr	pc,readop
	jsr	pc,expres
	mov	(sp)+,r1
	cmp	r1,$200
	bhis	1f
	jsr	r5,error; 'x
	br	ealoop
1:
	cmp	r1,$dotrel
	bne	2f
	bic	$40,r3
	cmp	r3,dotrel
	bne	1f
2:
	bicb	$37,(r1)
	bic	$!37,r3
	bne	2f
	clr	r2
2:
	bisb	r3,(r1)
	mov	r2,2(r1)
	br	ealoop
1:
	jsr	r5,error; '.
	movb	$2,dotrel
ealoop:
	cmp	r4,$';
	beq	assem1
	cmp	r4,$'\n
	bne	1f
	inc	line
	br	assem1
1:
	cmp	r4,$'\e
	bne	2f
	tst	ifflg
	beq	1f
	jsr	r5,error; 'x
1:
	rts	pc
2:
	jsr	r5,error; 'x
2:
	jsr	pc,checkeos
		br assem1
	jsr	pc,readop
	br	2b
assem1:
	jmp	assem

fbcheck:
	cmp	r0,$9.
	bhi	1f
	rts	pc
1:
	jsr	r5,error; 'f
	clr	r0
	rts	pc

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

