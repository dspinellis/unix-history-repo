/
/

/ a5 -- pdp-11 assembler pass 1

readop:
	mov	savop,r4
	beq	1f
	clr	savop
	rts	pc
1:
	jsr	pc,8f
	jsr	pc,putw
	rts	pc

8:
	jsr	pc,rch
_readop:
	mov	r0,r4
	movb	chartab(r0),r1
	bgt	rdname
	jmp	*1f-2(r1)

	fixor
	escp
	8b
	retread
	dquote
	garb
	squote
	rdname
	skip
	rdnum
	retread
	string
1:

escp:
	jsr	pc,rch
	mov	$esctab,r1
1:
	cmpb	r0,(r1)+
	beq	1f
	tstb	(r1)+
	bne	1b
	rts	pc
1:
	movb	(r1),r4
	rts	pc

esctab:
	.byte '/, '/
	.byte '\<, 035
	.byte '>, 036
	.byte '%, 037
	.byte 0, 0

fixor:
	mov	$037,r4
retread:
	rts	pc

rdname:
	movb	r0,ch
	cmp	r1,$'0
	blo	1f
	cmp	r1,$'9
	blos	rdnum
1:
	jmp	rname

rdnum:
	jsr	pc,number
		br 1f
	rts	pc

squote:
	jsr	pc,rsch
	br	1f
dquote:
	jsr	pc,rsch
	mov	r0,-(sp)
	jsr	pc,rsch
	swab	r0
	bis	(sp)+,r0
1:
	mov	r0,numval
	mov	$1,r4
	jsr	pc,putw
	mov	numval,r4
	jsr	pc,putw
	mov	$1,r4
	tst	(sp)+
	rts	pc

skip:
	jsr	pc,rch
	mov	r0,r4
	cmp	r0,$'\e
	beq	1f
	cmp	r0,$'\n
	bne	skip
1:
	rts	pc

garb:
	jsr	r5,error; 'g
	br	8b

string:
	mov	$'<,r4
	jsr	pc,putw
	clr	numval
1:
	jsr	pc,rsch
	tst	r1
	bne	1f
	mov	r0,r4
	bis	$400,r4
	jsr	pc,putw
	inc	 numval
	br	1b
1:
	mov	$-1,r4
	jsr	pc,putw
	mov	$'<,r4
	tst	(sp)+
	rts	pc

rsch:
	jsr	pc,rch
	cmp	r0,$'\e
	beq	4f
	cmp	r0,$'\n
	beq	4f
	clr	r1
	cmp	r0,$'\\
	bne	3f
	jsr	pc,rch
	mov	$schar,r2
1:
	cmpb	(r2)+,r0
	beq	2f
	tstb	(r2)+
	bpl	1b
	rts	pc
2:
	movb	(r2)+,r0
	clr	r1
	rts	pc
3:
	cmp	r0,$'>
	bne	1f
	inc	r1
1:
	rts	pc
4:
	jsr	r5,error; '<
	jmp	aexit

schar:
	.byte 'n, 012
	.byte 't, 011
	.byte 'e, 004
	.byte '0, 000
	.byte 'r, 015
	.byte 'a, 006
	.byte 'p, 033
	.byte 0,  -1

