/
/ copyright 1972 bell telephone laboratories inc.
/

/ bas1 -- compile

compile:
	clr	forp
	mov	$space,r4
	tst	lineno
	bne	retpc
1:
	jsr	pc,nextlin
		br 1f
	mov	lineno,r0
	jsr	pc,getloc
	mov	r4,4(r1)
	jsr	pc,statement
		br .+2
	inc	lineno
	cmp	r4,$espace-20.
	blo	1b
	jsr	r5,error
		<out of code space\n\0>; .even
1:
	tst	forp
	bne	2f
	mov	$loop,(r4)+

retpc:
	rts	pc

singstat:
	clr	forp
	mov	$exline,r4
	jsr	pc,statement
		br 1f
	cmp	-2(r4),$_asgn
	beq	1f
	mov	$_print,(r4)+
	mov	$_nline,(r4)+
1:
	tst	forp
	bne	2f
	cmp	r4,$eexline
	blo	1f
	jsr	r5,error
		<out of code space\n\0>; .even
1:
	mov	$loop,(r4)+
	mov	r4,exprloc
	mov	$exline,r4
	jmp	execute
2:
	jmp	forer

statement:
	mov	$line,r3
	movb	(r3)+,r0
	jsr	pc,digit
		br stat1
	dec	r3
	jsr	r5,atoi; nextc
	cmp	r0,$' /
	beq	1f
	mov	$line,r3
	movb	(r3)+,r0
	br	stat1
1:
	mov	$_line,(r4)+
	mov	r1,(r4)+

stat1:
	jsr	pc,skip
	cmp	r0,$'\n
	beq	retpc
	mov	r3,-(sp)
	jsr	pc,alpha
		br 1f
	jsr	pc,name
		br 1f
	tst	(sp)+
	jsr	pc,skip
	dec	r3
	jmp	*2f(r1)
2:
	stlist
	stdone
	strun
	stprint
	stdisp
	stif
	stgoto
	streturn
	stfor
	stnext
	stoctl
	stdraw
	steras
	stpromp
	stsave
	stdump

1:
	mov	(sp)+,r3
	dec	r3
	jsr	pc,expr
	cmp	r0,$'\n
	jne	joe
	add	$2,(sp)
	rts	pc

stsave:
	jsr	pc,1f
	mov	$_save,(r4)+
	rts	pc

stlist:
	jsr	pc,1f
	mov	$_list,(r4)+
	rts	pc

1:
	cmp	r0,$'\n
	bne	1f
	clrf	r0
	jsr	pc,const
	movif	$77777,r0
	jsr	pc,const
	br	2f
1:
	jsr	pc,expr
	cmp	r0,$'\n
	bne	1f
	mov	$_dup,(r4)+
	br	2f
1:
	dec	r3
	jsr	pc,expr
	cmp	r0,$'\n
	bne	joe
2:
	rts	pc

stdone:
	cmp	r0,$'\n
	bne	joe
	mov	$_done,(r4)+
	rts	pc

strun:
	cmp	r0,$'\n
	bne	joe
	mov	$_run,(r4)+
	rts	pc

stdisp:
	mov	$_sdisp,(r4)+
	jsr	pc,stprint
	mov	$_fdisp,(r4)+
	rts	pc

stprint:
	jsr	pc,stpromp
	mov	$_nline,(r4)+
	rts	pc

stpromp:
	jsr	pc,skip
	cmp	r0,$'\n
	jeq	retpc
	cmp	r0,$'"
	beq	1f
	dec	r3
	jsr	pc,expr
	mov	$_print,(r4)+
	dec	r3
	br	stpromp
1:
	mov	$_ascii,(r4)+
	inc	r3
1:
	movb	(r3)+,(r4)
	cmpb	(r4),$'"
	beq	1f
	cmpb	(r4)+,$'\n
	bne	1b
	br	joe
1:
	add	$2,r4
	bic	$1,r4
	movb	(r3)+,r0
	br	stpromp

stif:
	jsr	pc,expr
	mov	$_if,(r4)+
	mov	r4,-(sp)
	tst	(r4)+
	jsr	pc,stat1
		br .+2
	mov	(sp)+,r1
	mov	r4,(r1)
	rts	pc

stgoto:
	jsr	pc,expr
	mov	$_goto,(r4)+
	rts	pc

streturn:
	cmp	r0,$'\n
	beq	1f
	jsr	pc,expr
	cmp	r0,$'\n
	bne	joe
	br	2f
1:
	clrf	r0
	jsr	pc,const
2:
	mov	$_return,(r4)+
	rts	pc

joe:
	jsr	pc,serror

stfor:
	mov	r4,-(sp)
	jsr	pc,e2
	mov	r4,-(sp)
	cmp	r0,$'=
	bne	joe
	tst	val
	bne	joe
	jsr	pc,expr
	mov	forp,(r4)+	/ overlay w _asgn
	mov	r4,forp
	cmp	(r4)+,(r4)+	/ _tra ..
	mov	(sp)+,r0
	mov	(sp)+,r1
1:
	mov	(r1)+,(r4)+
	cmp	r1,r0
	blo	1b
	mov	$_fori,(r4)+
	mov	forp,r1
	mov	$_tra,(r1)+
	mov	r4,(r1)+
	dec	r3
	jsr	pc,expr
	mov	$_lesseq,(r4)+
	mov	$_if,(r4)+
	mov	forp,(r4)+
	mov	r4,forp
	cmp	r0,$'\n
	beq	1f
	jsr	pc,stat1
		br .+2
	br	stnext
1:
	rts	pc

forer:
	jsr	r5,error; <for/next imbalance\n\0>; .even

stnext:
	mov	forp,r1
	beq	forer
	mov	-(r1),r0
	mov	-(r0),forp
	mov	$_ptra,(r4)+
	mov	$_asgn,(r0)+
	cmp	(r0)+,(r0)+
	mov	r0,(r4)+
	mov	r4,(r1)+
	rts	pc

stoctl:
	jsr	pc,expr
	mov	$_octal,(r4)+
	rts	pc

stdraw:
	jsr	pc,expr
	dec	r3
	jsr	pc,expr
	cmp	r0,$'\n
	bne	1f
	movf	$one,r0
	jsr	pc,const
	br	2f
1:
	dec	r3
	jsr	pc,expr
2:
	mov	$_draw,(r4)+
	rts	pc

steras:
	mov	$_erase,(r4)+
	rts	pc

stdump:
	cmp	r0,$'\n
	bne	joe
	mov	$_dump,(r4)+
	rts	pc
