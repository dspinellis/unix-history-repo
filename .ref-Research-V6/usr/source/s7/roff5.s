/
/

/ hyp1 -- driver

hyphen:
	tst	hypedf
	bne	3f
	tst	hyf
	beq	3f
	inc	hypedf
	mov	wordp,r0
	clr	nhyph
1:
	jsr	pc,punct
	bne	1f
	inc	r0
	br	1b
1:
	jsr	pc,alph
	bne	3f
1:
	inc	r0
	jsr	pc,alph
	beq	1b
	dec	r0
	mov	r0,hstart
1:
	inc	r0
	tstb	(r0)
	beq	2f
	jsr	pc,punct
	bne	3f
	br	1b
2:
	jsr	r5,suffix
	jsr	r5,digram
3:
	rts	pc


punct:
	tst	old
	bne 4f
	cmpb	(r0),$010
	beq	0f
	movb	(r0),r2
	jsr	pc,alph2
	beq	0f
	sez
	rts	 pc
0:
	clz
	rts	pc
4:
	mov	 $3f,r2
1:
	cmpb	(r0),(r2)+
	beq	2f
	tstb	(r2)
	bne	1b
	clz
2:
	rts	pc
3: < .,()"\'`\0>	/should be more
.even
maplow:
	cmp	r2,$'a
	bhis	1f
	add	$'a-'A,r2
1:
	rts	pc

vowel:
	cmp	r2,$'a
	beq	1f
	cmp	r2,$'e
	beq	1f
	cmp	r2,$'i
	beq	1f
	cmp	r2,$'o
	beq	1f
	cmp	r2,$'u
	beq	1f
	cmp	r2,$'y
1:
	rts	pc

checkvow:
	mov	r0,-(sp)
1:
	movb	-(r0),r2
	jsr	pc,vowel
	beq	1f
	jsr	pc,alph
	beq	1b
	mov	(sp)+,r0
	clz
	rts	r5
1:
	mov	(sp)+,r0
	sez
	rts	r5

/ hyp2 -- suffix and digram

digram:
	mov	hstart,r0
1:
	jsr	pc,alph
	bne	3f
	jsr	pc,vowel
	beq	1f
	dec	r0
	br	1b
1:
	mov	r0,hstart
1:
	movb	-(r0),r2
	jsr	pc,alph2
	bne	3f
	jsr	pc,vowel
	bne	1b
	clr	maxdig
	mov	r0,nhstart
1:
	mov	$1,r3
	movb	-1(r0),r2
	jsr	pc,alph2
	beq	2f
	movb	(r0),r2
	mov	$'a,r1
	jsr	r5,dilook; bxh
	br	4f
2:
	movb	-2(r0),r2
	mov	$xxh,0f
	jsr	pc,alph2
	beq	2f
	mov	$bxxh,0f
2:
	movb	-1(r0),r1
	movb	(r0),r2
	jsr	r5,dilook; 0:xxh
4:
	movb	(r0)+,r1
	movb	(r0),r2
	jsr	r5,dilook; xhx
	movb	(r0),r1
	movb	1(r0),r2
	jsr	r5,dilook; hxx
	cmp	r3,maxdig
	blos	2f
	mov	r3,maxdig
	mov	r0,maxloc
2:
	cmp	r0,hstart
	blo	1b
	mov	nhstart,hstart
	cmp	maxdig,thresh
	blo	digram
	bisb	$200,*maxloc
	inc	nhyph
/	mov	maxdig,*octbufp
/	inc	octcnt
/	add	$2,octbufp
	br	digram
3:
	rts	r5

dilook:
	mov	r4,-(sp)
	bic	$!177,r2
	bic	$!177,r1
	jsr	pc,maplow
	sub	$'a,r2
	cmp	r2,$'z-'a
	bhi	3f
	mov	r2,r4
	mov	r1,r2
	jsr	pc,maplow
	sub	$'a,r2
	cmp	r2,$'z-'a
	bhi	3f
	mov	r3,-(sp)
	mov	r2,r3
	mpy	$13.,r3
	clr	r2
	clc
	ror	r4
	adc	r2
	add	r3,r4
	add	(r5)+,r4
	movb	(r4),r4
	tst	r2
	bne	1f
	asr	r4
	asr	r4
	asr	r4
	asr	r4
1:
	bic	$!17,r4
	mov	r4,r3
	mpy	(sp)+,r3
	br	4f
3:
	clr	r3
	tst	(r5)+
4:
	mov	(sp)+,r4
	rts	r5

suffix:
	mov	hstart,r0
	jsr	pc,alph
	bne	4f
	jsr	pc,maplow
	sub	$'a,r2
	asl	r2
	mov	suftab(r2),-(sp)
	bic	$!37777,(sp)
	beq	3f
1:
	mov	hstart,r0
	mov	(sp),r1
	jsr	pc,rdsuf
	movb	(r1),r3
	beq	3f
	bic	$!17,r3
	add	r3,(sp)
	add	r1,r3
2:
	movb	-(r3),r2
	cmp	r3,r1
	ble	2f
	bic	$!177,r2
	mov	r2,-(sp)
	movb	-(r0),r2
	jsr	pc,maplow
	cmp	r2,(sp)+
	bne	1b
	br	2b
2:
	mov	hstart,r0
	tst	(sp)+
	movb	(r1),r3
	bic	$!17,r3
	add	r1,r3
	bitb	$200,(r1)+
	bne	1f
2:
	dec	r0
	cmp	r3,r1
	ble	2f
	tstb	-(r3)
	bpl	2b
1:
	mov	r0,hstart
	dec	hstart
	bitb	$100,-1(r1)
	bne	2b
	jsr	r5,checkvow
	bne	4f
	bisb	$200,(r0)
	br	2b
2:
	bitb	$40,-(r1)
	beq	suffix
	br	4f
3:
	tst	(sp)+
4:
	rts	r5

rdsuf:
	mov	r0,-(sp)
	mov	suff,nfile
	mov	4(sp),r1
	jsr	pc,rdsufb
	mov	$sufb,r2
	movb	r0,(r2)+
	mov	r0,r3
	bic	$!17,r3
1:
	dec	r3
	blt	1f
	inc	r1
	jsr	pc,rdsufb
	movb	r0,(r2)+
	br	1b
1:
	mov	$sufb,r1
	mov	(sp)+,r0
	rts	pc
