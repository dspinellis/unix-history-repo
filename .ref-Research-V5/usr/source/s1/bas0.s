/
/ copyright 1972 bell telephone laboratories inc.
/

/ bas0 -- basic

.globl	main
.globl	sin, cos, log, exp, atan, pow, sqrt
.globl	rand, srand
.globl	fptrap

one = 40200

main:
	sys	signal; 4; fptrap
	setd
	sys	time
	mov	r1,r0
	mov	r0,randx
	jsr	pc,srand
	sys	signal; 2; 1
	ror	r0
	bes	1f
	sys	signal; 2; intrup
1:
	mov	sp,gsp
	clr	seeka
	mov	$'a,r1
1:
	movb	r1,tmpf+8
	sys	stat; tmpf; line
	bes	1f
	inc	r1
	cmp	r1,$'z
	blos	1b
	br	2f
1:
	sys	creat; tmpf; 600
	bes	2f
	mov	r0,tfo
	sys	open; tmpf; 0
	bec	1f
2:
	mov	$3f,r0
	jsr	pc,print
	sys	exit
3:
	<Tmp file?\n\0>; .even
1:
	mov	r0,tfi
	jsr	pc,isymtab
	cmp	(sp),$2
	blt	loop
	mov	4(sp),argname
	sys	0; 9f
.data
9:
	sys	open; argname: b.out; 0
.text
	bes	1f
	mov	r0,fi
	br	loop
1:
	mov	$1f,r0
	jsr	pc,print
	br	loop
1:
	<Cannot open file\n\0>; .even

intrup:
	sys	signal; 2; intrup
	mov	$'\n,r0
	jsr	r5,putc
	jsr	r5,error
		<ready\n\0>; .even

loop:
	mov	gsp,sp
	clr	lineno
	jsr	pc,rdline
	mov	$line,r3
1:
	movb	(r3),r0
	jsr	pc,digit
		br 1f
	jsr	r5,atoi; nextc
	cmp	r0,$' /
	bne	1f
	mov	$lintab,r3
	mov	r1,r0
	bgt	2f
	jsr	pc,serror
2:
	cmp	r0,(r3)
	beq	2f
	tst	(r3)
	beq	2f
	add	$6,r3
	br	2b
2:
	cmp	r3,$elintab-12.
	blo	2f
	jsr	r5,error
		<too many lines\n\0>; .even
2:
	mov	r0,(r3)+
	mov	seeka,(r3)+
	mov	tfo,r0
	sys	0; 9f
.data
9:
	sys	seek; seeka:..; 0
.text
	mov	$line,r0
	jsr	pc,size
	inc	r0
	add	r0,seeka
	mov	r0,0f
	mov	tfo,r0
	sys	0; 9f
.data
9:
	sys	write; line; 0:..
.text
	br	loop
1:
	mov	$line,r3
	jsr	pc,singstat
	br	loop

nextc:
	movb	(r3)+,r0
	rts	r5

size:
	clr	-(sp)
1:
	tstb	(r0)
	beq	1f
	inc	(sp)
	cmpb	(r0)+,$'\n
	bne	1b
1:
	mov	(sp)+,r0
	rts	pc

rdline:
	mov	$line,0f
1:
	mov	fi,r0
	sys	0; 9f
.data
9:
	sys	read; 0:..; 1
.text
	bes	2f
	tst	r0
	beq	2f
	cmp	0b,$line+99.
	bhis	2f			/ bad check, but a check
	movb	*0b,r0
	inc	0b
	cmp	r0,$'\n
	bne	1b
	clrb	*0b
	rts	pc
2:
	mov	fi,r0
	beq	1f
	sys	close
	clr	fi
	br	1b
1:
	jmp	_done

error:
	mov	fi,r0
	beq	1f
	sys	close
	clr	fi
1:
	mov	fo,r0
	cmp	r0,$1
	beq	1f
	sys	close
	mov	$1,fo
1:
	tst	lineno
	beq	1f
	jsr	pc,nextlin
		br 1f
	mov	$line,r0
	jsr	pc,print
1:
	mov	r5,r0
	jsr	pc,print
	jmp	loop

serror:
	dec	r3
	tst	fi
	beq	1f
	sys	close
	clr	fi
1:
	mov	$line,r1
1:
	cmp	r1,r3
	bne	2f
	mov	$'_,r0
	jsr	r5,putc
	mov	$10,r0
	jsr	r5,putc
2:
	movb	(r1),r0
	jsr	r5,putc
	cmpb	(r1)+,$'\n
	bne	1b
	jmp	loop

print:
	mov	r0,0f
	jsr	pc,size
	mov	r0,0f+2
	mov	fo,r0
	sys	0; 9f
.data
9:
	sys	write; 0:..; ..
.text
	rts	pc

digit:
	cmp	r0,$'0
	blo	1f
	cmp	r0,$'9
	bhi	1f
	add	$2,(sp)
1:
	rts	pc

alpha:
	cmp	r0,$'a
	blo	1f
	cmp	r0,$'z
	bhi	1f
	add	$2,(sp)
1:
	rts	pc

name:
	mov	$nameb,r1
	clr	(r1)
	clr	2(r1)
1:
	cmp	r1,$nameb+4
	bhis	2f
	movb	r0,(r1)+
2:
	movb	(r3)+,r0
	jsr	pc,alpha
		br 2f
	br	1b
2:
	jsr	pc,digit
		br 2f
	br	1b
2:
	mov	$resnam,r1
1:
	cmp	nameb,(r1)
	bne	2f
	cmp	nameb+2,2(r1)
	bne	2f
	sub	$resnam,r1
	asr	r1
	add	$2,(sp)
	rts	pc
2:
	add	$4,r1
	cmp	r1,$eresnam
	blo	1b
	mov	$symtab,r1
1:
	tst	(r1)
	beq	1f
	cmp	nameb,(r1)
	bne	2f
	cmp	nameb+2,2(r1)
	bne	2f
	rts	pc
2:
	add	$14.,r1
	br	1b
1:
	cmp	r1,$esymtab-28.
	blo	1f
	jsr	r5,error
		<out of symbol space\n\0>; .even
1:
	mov	nameb,(r1)
	mov	nameb+2,2(r1)
	clr	4(r1)
	clr	14.(r1)
	rts	pc

skip:
	cmp	r0,$' /
	bne	1f
	movb	(r3)+,r0
	br	skip
1:
	rts	pc

putc:
	tstb	drflg
	beq	1f
	jsr	pc,drput
	rts	r5
1:
	mov	r0,ch
	mov	$1,r0
	sys	write; ch; 1
	rts	r5

nextlin:
	clr	-(sp)
	mov	$lintab,r1
1:
	tst	(r1)
	beq	1f
	cmp	lineno,(r1)
	bhi	2f
	mov	(sp),r0
	beq	3f
	cmp	(r0),(r1)
	blos	2f
3:
	mov	r1,(sp)
2:
	add	$6,r1
	br	1b
1:
	mov	(sp)+,r1
	beq	1f
	mov	(r1)+,lineno
	mov	(r1)+,0f
	mov	tfi,r0
	sys	0; 9f
.data
9:
	sys	seek; 0:..; 0
.text
	mov	tfi,r0
	sys	read; line; 100.
	add	$2,(sp)
1:
	rts	pc

getloc:
	mov	$lintab,r1
1:
	tst	(r1)
	beq	1f
	cmp	r0,(r1)
	beq	2f
	add	$6,r1
	br	1b
1:
	jsr	r5,error
		<label not found\n\0>; .even
2:
	rts	pc

isymtab:
	mov	$symtab,r0
	mov	$symtnam,r1
	clrf	fr0
	movf	$one,fr1
1:
	mov	(r1)+,(r0)+
	mov	(r1)+,(r0)+
	mov	$1,(r0)+
	subf	r1,r0
	movf	r0,(r0)+
	cmp	r1,$esymtnam
	blo	1b
	clr	(r0)+
	rts	pc

