/
/ copyright 1972 bell telephone laboratories inc.
/

/ bas3 -- execution

execute:
	mov	$estack,r3
	mov	r3,sstack
	jmp	*(r4)+

_if:
	tstf	(r3)+
	cfcc
	beq	_tra
	tst	(r4)+
	jmp	*(r4)+

_ptra:
	mov	sstack,r3

_tra:
	mov	(r4)+,r4
	jmp	*(r4)+

_funct:
	mov	r4,-(r3)
	mov	sstack,-(r3)
	mov	r3,sstack
	inc	sublev
	clr	r0
	jsr	pc,arg
	tstf	r0
	cfcc
	bge	1f
	jmp	builtin

_goto:
	movf	(r3),r0
1:
	movfi	r0,-(sp)
	jsr	pc,compile
	mov	(sp)+,r0
	jsr	pc,getloc
	mov	4(r1),r4
	jmp	*(r4)+

_run:
	jsr	pc,isymtab
	mov	randx,r0
	jsr	pc,srand
	jsr	pc,compile
	mov	$space,r4
	jmp	*(r4)+

_save:
	mov	argname,0f
	sys	0; 9f
.data
9:
	sys	creat; 0:..; 0666
.text
	bec	1f
	4
1:
	mov	r0,fo

_list:
	movf	(r3)+,r0
	movfi	r0,-(sp)
	movf	(r3),r0
	movfi	r0,lineno
1:
	jsr	pc,nextlin
		br 1f
	cmp	lineno,(sp)
	bhi	1f
	mov	$line,r0
	jsr	pc,print
	inc	lineno
	br	1b
1:
	tst	(sp)+
	mov	fo,r0
	cmp	r0,$1
	beq	1f
	sys	close
	mov	$1,fo
1:
	jmp	*(r4)+

_done:
	sys	unlink; tmpf
	sys	exit

_sdisp:
	mov	$2,r0
	jsr	pc,drput
	jsr	pc,drxy
	mov	$1,r0
	jsr	pc,drput
	mov	$3,r0
	jsr	pc,drput
	incb	drflg
	jmp	*(r4)+

_fdisp:
	clr	r0
	jsr	pc,drput
	clrb	drflg
	jmp	*(r4)+

_print:
	movf	(r3)+,r0
	jsr	r5,ftoa; putc
	jmp	*(r4)+

_octal:
	movf	(r3)+,r0
	jsr	r5,ftoo; putc
	jmp	*(r4)+

_draw:
	movf	(r3)+,r2
	movf	(r3)+,r1
	movf	(r3)+,r0
	jsr	r5,draw
	jmp	*(r4)+

_erase:
	mov	$1,r0
	jsr	pc,drput
	mov	$1,r0
	jsr	pc,drput
	jmp	*(r4)+

_nline:
	mov	$'\n,r0
	jsr	r5,putc
	jmp	*(r4)+

_ascii:
	movb	(r4)+,r0
	cmp	r0,$'"
	beq	1f
	jsr	r5,putc
	br	_ascii
1:
	inc	r4
	bic	$1,r4
	jmp	*(r4)+

_line:
	mov	sstack,r3
	cmp	r3,$stack+20.
	bhi	1f
	jsr	r5,error
		<out of space\n\0>; .even
1:
	mov	(r4)+,lineno
	jmp	*(r4)+

_or:
	tstf	(r3)+
	cfcc
	bne	stone
	tstf	(r3)
	cfcc
	bne	stone
	br	stzero

_and:
	tstf	(r3)+
	cfcc
	beq	stzero
	tstf	(r3)
	cfcc
	beq	stzero
	br	stone

_great:
	jsr	pc,bool
	bgt	stone
	br	stzero

_greateq:
	jsr	pc,bool
	bge	stone
	br	stzero

_less:
	jsr	pc,bool
	blt	stone
	br	stzero

_lesseq:
	jsr	pc,bool
	ble	stone
	br	stzero

_noteq:
	jsr	pc,bool
	bne	stone
	br	stzero

_equal:
	jsr	pc,bool
	beq	stone

stzero:
	clrf	r0
	br	advanc

stone:
	movf	$one,r0
	br	advanc

_extr:
	movf	r1,r0		/ dup for _and in extended rel
	br	subadv

_asgn:
	movf	(r3)+,r0
	mov	(r3)+,r0
	add	$4,r0
	bis	$1,(r0)+
	movf	r0,(r0)
	br	subadv

_add:
	movf	(r3)+,r0
	addf	(r3),r0
	br	advanc

_negat:
	negf	(r3)
	jmp	*(r4)+

_sub:
	movf	(r3)+,r0
	negf	r0
	addf	(r3),r0
	br	advanc

_mult:
	movf	(r3)+,r0
	mulf	(r3),r0
	br	advanc

_divid:
	movf	(r3)+,r1
	movf	(r3),r0
	divf	r1,r0
	br	advanc

_expon:
	movf	(r3)+,fr1
	movf	(r3),fr0
	jsr	r5,pow
	bec	advanc
	jsr	r5,error
		<Bad exponentiation\n\0>; .even

_const:
	movf	(r4)+,r0

subadv:
	movf	r0,-(r3)
	jmp	*(r4)+

advanc:
	movf	r0,(r3)
	jmp	*(r4)+

_rval:
	jsr	pc,getlv
	br	subadv

_fori:
	jsr	pc,getlv
	addf	$one,r0
	movf	r0,(r0)
	br	subadv

_lval:
	mov	(r4)+,-(r3)
	jmp	*(r4)+

_dup:
	movf	(r3),r0
	br	subadv

_return:
	dec	sublev
	bge	1f
	jsr	r5,error
		<bad return\n\0>; .even
1:
	movf	(r3),r0
	mov	sstack,r3
	mov	(r3)+,sstack
	mov	(r3)+,r4
	mov	(r4)+,r0
1:
	dec	r0
	blt	advanc
	add	$8,r3
	br	1b

_subscr:
	mov	(r4),r1
	mpy	$8.,r1
	add	r1,r3
	mov	r3,-(sp)
	mov	(r3),r0
	mov	(r4)+,-(sp)
1:
	dec	(sp)
	blt	1f
	movf	-(r3),r0
	movfi	r0,r2
	com	r2
	blt	2f
	jsr	r5,error
		<subscript out of range\n\0>; .even
2:
	mov	r0,r1
	mov	4(r0),r0
	bic	$1,r0
2:
	beq	2f
	cmp	r2,(r0)+
	bne	3f
	tst	-(r0)
	br	1b
3:
	mov	(r0),r0
	br	2b
2:
	mov	$symtab,r0
2:
	tst	(r0)
	beq	2f
	add	$14.,r0
	br	2b
2:
	cmp	r0,$esymtab-28.
	blo	2f
	jsr	r5,error
		<out of symbol space\n\0>; .even
2:
	cmp	(r1)+,(r1)+
	mov	r0,-(sp)
	clr	14.(r0)
	mov	r2,(r0)+
	mov	(r1),r2
	bic	$1,r2
	mov	r2,(r0)+
	clr	(r0)+
	mov	(sp)+,r0
	bic	$!1,(r1)
	bis	r0,(r1)
	br	1b
1:
	tst	(sp)+
	mov	(sp)+,r3
	mov	r0,(r3)
	jmp	*(r4)+

bool:
	movf	(r3)+,r1	/ r1 used in extended rel
	cmpf	(r3),r1
	cfcc
	rts	pc

getlv:
	mov	(r3)+,r0
	add	$4,r0
	bit	$1,(r0)+
	bne	1f
	jsr	r5,error;<used before set\n\0>; .even
1:
	movf	(r0),r0
	rts	pc

_dump:
	mov	r4,-(sp)
	mov	$9.*14.+symtab-14.,r4
1:
	add	$14.,r4
	tst	(r4)
	beq	1f
	bit	$1,4(r4)
	beq	1b
	jsr	pc,dmp1
	mov	$'=,r0
	jsr	r5,putc
	movf	6(r4),r0
	jsr	r5,ftoa; putc
	mov	$'\n,r0
	jsr	r5,putc
	br	1b
1:
	mov	(sp)+,r4
	jmp	*(r4)+

dmp1:
	tst	(r4)
	blt	1f
	mov	(r4),nameb
	mov	2(r4),nameb+2
	mov	$nameb,r0
	jsr	pc,print
	rts	pc
1:
	mov	r4,-(sp)
	mov	$symtab-14.,r4
1:
	add	$14.,r4
	tst	(r4)
	beq	1f
	mov	4(r4),r0
	bic	$1,r0
2:
	beq	1b
	cmp	r0,(sp)
	beq	2f
	mov	2(r0),r0
	br	2b
2:
	jsr	pc,dmp1
	mov	$'[,r0
	jsr	r5,putc
	mov	*(sp),r0
	com	r0
	movif	r0,r0
	jsr	r5,ftoa; putc
	mov	$'],r0
	jsr	r5,putc
1:
	mov	(sp)+,r4
	rts	pc
