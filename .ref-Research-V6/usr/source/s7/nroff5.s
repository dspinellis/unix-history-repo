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
	mov	r0,wordstart
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
	mov	hstart,wordend
	jsr	pc,exword
	beq	0f
	jsr	r5,suffix
	tst	exf
	bne	0f
	jsr	r5,digram
0:
	bit	$4,hyf
	beq	0f
	mov	wordend,r0
	bicb	$!177,-1(r0)
0:
	mov	wordstart,r0
	bicb	$!177,1(r0)
	bit	$10,hyf
	beq	3f
	bicb	$!177,2(r0)
3:
	rts	pc

casehw:
	jsr	pc,skipcont
	bne	2f
	mov	nexth,r1
	cmp	r1,$ehbuf-2
	bhis	3f
1:
	jsr	pc,getchar
	bmi	1f
	cmpb	r0,$' /
	beq	4f
	cmpb	r0,$012
	beq	1f
	movb	r0,(r1)+
	cmp	r1,$ehbuf-2
	blo	1b
	br	3f
1:
	clrb	(r1)+
	mov	r1,nexth
	clrb	(r1)+
2:
	rts	pc
3:
	jsr	r5,string;hmess
	clrb	*nexth
	br	2b
4:
	jsr	pc,1b
	br	casehw
hmess: <Exception word space full.\n\0>
.even

exword:
	clr	exf
	clr	-(sp)
	mov	r1,-(sp)
	mov	r2,-(sp)
	mov	r3,-(sp)
	mov	r4,-(sp)
	mov	$hbuf,r2
	mov	hstart,r0
	cmpb	(r0)+,(r0)+
0:
	mov	r2,8(sp)
	mov	wordstart,r1
	tstb	(r2)
	beq	4f
1:
	movb	(r2)+,r4
	cmpb	r4,$'-
	beq	1b
	movb	(r1)+,r3
/	tstb	r3
/	bne	2f
	cmp	r1,r0
	blo	2f
	tstb	r4
	beq	3f
2:
	tstb	r4
	bne	2f
/	tstb	(r1)
	tstb	-1(r0)
	bne	2f
	cmpb	r3,$'s
	beq	3f
2:
	tstb	r4
	beq	0b
/	tstb	r3
/	beq	2f
	cmp	r1,r0
	bhis	2f
	cmpb	r4,r3
	beq	1b
2:
	tstb	(r2)+
	bne	2b
	br	0b
3:
	mov	wordstart,r1
	mov	8(sp),r2
	clr	8(sp)
	inc	exf
1:
	tstb	(r1)+
	cmpb	(r2)+,$'-
	bne	0f
	bisb	$200,-1(r1)
	tstb	(r2)+
0:
	tstb	(r2)
	bne	1b
4:
	mov	(sp)+,r4
	mov	(sp)+,r3
	mov	(sp)+,r2
	mov	(sp)+,r1
	tst	(sp)+
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
	cmpb	r2,$'a
	beq	1f
	cmpb	r2,$'e
	beq	1f
	cmpb	r2,$'i
	beq	1f
	cmpb	r2,$'o
	beq	1f
	cmpb	r2,$'u
	beq	1f
	cmpb	r2,$'y
	beq	1f
	cmpb	r2,$'A
	beq	1f
	cmpb	r2,$'E
	beq	1f
	cmpb	r2,$'I
	beq	1f
	cmpb	r2,$'O
	beq	1f
	cmpb	r2,$'U
	beq	1f
	cmpb	r2,$'Y
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
	jmp	7f
.data
7:
	jsr	r5,dilook; 0:xxh
	jmp	8f
.text
8:
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
	bne	4f
	jsr	pc,exword
	bne	suffix
	br	4f
/	beq	suffix
/	br	4f
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
