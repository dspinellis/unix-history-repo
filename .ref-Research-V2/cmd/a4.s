/ a4 -- pdp-11 assembler pass1

rname:
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	$2,r5
	mov	$symbol,r2
	clr	-(sp)
1:
	jsr	pc,rnch
	mov	r3,r1
	mpy	$40.,r1
	jsr	pc,rnch
	add	r3,r1
	mpy	$40.,r1
	jsr	pc,rnch
	add	r1,r3
	add	r3,(sp)
	mov	r3,(r2)+
	sob	r5,1b
	jsr	pc,rnch
	mov	r3,r1
	add	r3,(sp)
	als	$10.,r1
	mov	r1,(r2)
1:
	jsr	pc,rnch
	tst	r3
	bne	1b
	mov	(sp)+,r1
	clr	r0
	dvd	$hshsiz,r0
	mov	r1,r0
	asl	r0
	add	$hshtab,r0
1:
	cmp	r0,$hshtab
	bhi	2f
	mov	$2*hshsiz+hshtab,r0
2:
	mov	$symbol,r2
	mov	-(r0),r4
	beq	3f
	cmp	(r2)+,(r4)+
	bne	1b
	cmp	(r2)+,(r4)+
	bne	1b
	cmpb	1(r4),1(r2)
	bne	1b
	br	1f
3:
	mov	symend,r4
	mov	r4,(r0)
	mov	r4,-(sp)
	add	$20,r4
	cmp	r4,0f
	blos	4f
	add	$512.,0f
	sys	break; 0:end
4:
	mov	(sp)+,r4
	mov	(r2)+,(r4)+
	mov	(r2)+,(r4)+
	mov	(r2)+,(r4)+
	clr	(r4)+
	mov	r4,symend
	sub	$4,r4
1:
	mov	r4,-(sp)
	sub	$symtab-374,r4
	asr	r4
	jsr	pc,putw
	mov	(sp)+,r4
	mov	(sp)+,r3
	mov	(sp)+,r2
	tst	(sp)+
	mov	(sp)+,r1
	rts	pc

rnch:
	jsr	pc,rch
	movb	chartab(r0),r3
	ble	1f
	rts	pc
1:
	movb	r0,ch
	clr	r3
	rts	pc

number:
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r5,-(sp)
	clr	r1
	clr	r5
1:
	jsr	pc,rch
	jsr	r5,betwen; '0; '9
		br 1f
	sub	$'0,r0
	mpy	$10.,r5
	add	r0,r5
	als	$3,r1
	add	r0,r1
	br	1b
1:
	cmp	r0,$'b
	beq	1f
	cmp	r0,$'f
	beq	1f
	cmp	r0,$'.
	bne	2f
	mov	r5,r1
	clr	r0
2:
	movb	r0,ch
	mov	r1,r0
	mov	(sp)+,r5
	mov	(sp)+,r3
	mov	(sp)+,r2
	rts	pc
1:
	mov	r0,r3
	mov	r5,r0
	jsr	pc,fbcheck
	add	$141,r0
	cmp	r3,$'b
	beq	1f
	add	$10.,r0
1:
	mov	r0,r4
	mov	(sp)+,r5
	mov	(sp)+,r3
	mov	(sp)+,r2
	add	$2,(sp)
	rts	pc

rch:
	movb	ch,r0
	beq	1f
	clrb	ch
	rts	pc
1:
	dec	inbfcnt
	blt	2f
	movb	*inbfp,r0
	inc	inbfp
	bic	$!177,r0
	beq	1b
	rts	pc
2:
	movb	fin,r0
	beq	3f
	sys	read; inbuf;512.
	bcs	2f
	tst	r0
	beq	2f
	mov	r0,inbfcnt
	mov	$inbuf,inbfp
	br	1b
2:
	movb	fin,r0
	clrb	fin
	sys	close
3:
	decb	nargs
	bgt	2f
	mov	$'\e,r0
	rts	pc
2:
	tst	ifflg
	beq	2f
	jsr	r5,error; 'i
	jmp	aexit
2:
	mov	curarg,r0
	tst	(r0)+
	mov	(r0),0f
	mov	r0,curarg
	incb	fileflg
	sys	open; 0:0; 0
	bec	2f
	mov	0b,0f
	jsr	r5,filerr; 0:0; <?\n>
	jmp	 aexit
2:
	movb	r0,fin
	mov	$1,line
	mov	r4,-(sp)
	mov	r1,-(sp)
	mov	$5,r4
	jsr	pc,putw
	mov	*curarg,r1
2:
	movb	(r1)+,r4
	beq	2f
	jsr	pc,putw
	br	2b
2:
	mov	$-1,r4
	jsr	pc,putw
	mov	(sp)+,r1
	mov	(sp)+,r4
	br	1b


	mov	$inbuf,inbfp
	br	1b
2:
	movb	fin,r0
	clrb	fin
	sys	close
3:
	decb	nargs
	bgt	2f
	mov	$'\e,r0
	rts	pc
2:
	tst	ifflg
	beq	2f
	jsr	r5,error; 'i
	jmp	aexit
2:
	mov	curarg,r0
	tst	(r0)+
	mov	(r0),0f
	mov	r0,curarg
	incb	fileflg
	sys	open; 0:0; 0
	bec	2f
	mov	0b,0f
	jsr	r5,filerr; 0:0; <?\n>
	jmp	 aexi