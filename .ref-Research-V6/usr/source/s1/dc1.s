.globl log2
.globl	getchar
.globl	lookchar
.globl	fsfile
.globl	seekchar
.globl	backspace
.globl	putchar
.globl	alterchar
.globl	move
.globl	rewind
.globl	create
.globl	zero
.globl	allocate
.globl	release
.globl	collect
.globl	w, r, a, l
/
	cmp	(sp)+,$2
	blo	1f
	tst	(sp)+
	mov	(sp)+,0f
	cmpb	*0f,$'-
	beq	8f
	sys	0; 9f
.data
9:
	sys	open; 0:.=.+2; 0
.text
	bec	2f
	mov	$1,r0
	sys	write; 4f; 5f-4f
	sys	exit

/
4:	<Input file.\n>
5:	.even
/
2:
	mov	r0,source
1:
	sys	signal; 2; 1
	ror	r0
	bcs	1f
	sys	signal; 2; case177
1:
8:
	clr	delflag
	mov	$pdl,r5
/
	mov	$10.,r0
	jsr	pc,log2
	mov	r0,log10
	mov	$1,r0
	jsr	pc,allocate
	mov	r1,scalptr
	clr	r0
	jsr	pc,putchar
	clr	r0
	jsr	pc,allocate
	mov	r1,basptr
	mov	$10.,r0
	jsr	pc,putchar
	mov	$1,r0
	jsr	pc,allocate
	mov	r1,inbas
	mov	$10.,r0
	jsr	pc,putchar
	mov	$1,r0
	jsr	pc,allocate
	mov	$10.,r0
	jsr	pc,putchar
	mov	r1,tenptr
	clr	r0
	jsr	pc,allocate
	mov	r1,chptr
	clr	r0
	jsr	pc,allocate
	mov	r1,strptr
	mov	$1,r0
	jsr	pc,allocate
	mov	$2,r0
	jsr	pc,putchar
	mov	r1,sqtemp
	clr	r0
	jsr	pc,allocate
	mov	r1,divxyz
loop:
	tst	delflag
	bne	in177
	mov	sp,errstack
	jsr	pc,readc
	mov	$casetab,r1
1:	tst	(r1)+
	beq	2f
	cmp	r0,(r1)+
	bne	1b
	jmp	*-4(r1)
2:	jmp	eh
/
/
/	case for new line (which is special for apl box)
/
case012:
	br	loop
/
/
/	case q for quit
/
case161:
	cmp	readptr,$readstack+2
	blos	1f
	mov	*readptr,r1
	beq	2f
	jsr	pc,release
2:
	sub	$2,readptr
	mov	*readptr,r1
	beq	2f
	jsr	pc,release
2:
	sub	$2,readptr
	jmp	loop
1:
	sys	exit
/
/
/	case Q for controlled quit
/
case121:
	jsr	pc,pop
	jes	eh
	jsr	pc,length
	cmp	r0,$2
	jhi	eh1
	jsr	pc,rewind
	jsr	pc,getchar
	jmi	eh1
	jsr	pc,release
1:
	cmp	readptr,$readstack
	jlos	eh
	mov	*readptr,r1
	beq	2f
	jsr	pc,release
2:
	sub	$2,readptr
	sob	r0,1b
	jbr	loop
/
/
/	case of delete character
/
case177:
	sys	signal; 2; case177
	mov	$1,delflag
	mov	r0,-(sp)
	mov	2(sp),r0
	cmp	-6(r0),$sys+read
	bne	1f
	sub	$6,2(sp)
	clr	delflag
1:
	mov	(sp)+,r0
	2			/rti
/
in177:
	mov	$' ,ch
	mov	$1,r0
	sys	write; 1f; 1
	clr delflag
	jmp	eh
/
.bss
delflag: .=.+2
.text
1:	<\n>
	.even
/
/
/	case digit
/
case060:
	movb	r0,savec
	jsr	pc,readin
	jsr	pc,push
	br	loop
/
/
/	case _ for negative numbers
/
case137:
	jsr	pc,readin
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,savk
	dec	w(r1)
	jsr	pc,chsign
	mov	savk,r0
	jsr	pc,putchar
	jsr	pc,push
	jbr	loop
/
/
/	case screamer
/
case041:
	jsr	pc,in041
	jbr	loop
/
in041:
	jsr	pc,readc
	cmp	r0,$'<
	jeq	in74a
	cmp	r0,$'=
	jeq	in75a
	cmp	r0,$'>
	jeq	in76a
/
	mov	$field,r1
	movb	r0,(r1)+
1:
	jsr	pc,readc
	movb	r0,(r1)+
	cmpb	r0,$'\n
	bne	1b
	clrb	(r1)+
/
	sys	fork
		br	9f
	sys	wait
	mov	$1,r0
	sys	write; screamer; 2
	rts	pc
9:	sys	exec; 6f; 8f
	sys	exit
.data
8:	6f; 7f; field; 0
6:	</bin/sh\0>
7:	<-c\0>
screamer: <!\n>
	.even
.bss
field:	.=.+70.
.text
/
/
/	case d for duplicate
/
case144:
	cmp	r5,$pdl
	jeq	eh
	clr	r0
	jsr	pc,allocate
	mov	-2(r5),r0
	jsr	pc,move
	jsr	pc,push
	jmp	loop
/
/
/	case z for stack size
/
case172:
	clr	r0
	jsr	pc,allocate
	mov	r5,r3
	sub	$pdl,r3
	asr	r3
2:
	beq	2f
	clr	r2
	dvd	$100.,r2
	mov	r3,r0
	jsr	pc,putchar
	mov	r2,r3
	br	2b
2:
	clr	r0
	jsr	pc,putchar
	jsr	pc,push
	jmp	loop
/
/
/	case c for flush
/
case143:
2:	jsr	pc,pop
	jes	loop
	jsr	pc,release
	br	2b
/
/	case s for save
/
case163:
	tst	sfree
	bne	1f
	jsr	pc,sinit
1:
	jsr	pc,readc
	cmp	r5,$pdl
	bne	2f
	movb	$'s,ch
	jmp	eh
2:
	clr	r2
	cmpb	r0,$128.	/ check for array
	blo	1f
	inc	r2
1:
	asl	r0
	mov	stable(r0),r1
	beq	2f
	mov	r1,r0
	mov	2(r0),r1
	tst	r2
	beq	4f
	mov	r1,-(sp)	/ have array - release elements
	jsr	pc,rewind
1:
	mov	(sp),r1
3:
	jsr	pc,getword
	bes	1f
	tst	r0
	beq	3b
	mov	r0,r1
	jsr	pc,release
	br	1b
1:
	mov	(sp)+,r1
4:
	jsr	pc,release
	jsr	pc,pop
	mov	r1,2(r0)
	jbr	loop
2:
	mov	sfree,stable(r0)
	mov	stable(r0),r0
	mov	(r0),sfree
	beq	symout
	clr	(r0)
	jsr	pc,pop
	mov	r1,2(r0)
	jmp	loop
/
symout:
	mov	$1,r0
	sys	write; 7f; 8f-7f
	jmp	reset
/
7:	<Symbol table overflow.\n>
8:	.even
/
/
sinit:
	mov	$sfree+4,r0
1:
	mov	r0,-4(r0)
	clr	-2(r0)
	add	$4,r0
	cmp	r0,$sfend
	blos	1b
	clr	sfend-4
	rts	pc
/
/
.bss
sfree:	.=.+512.
sfend:
.text
/
/
/	case S for save
/
case123:
	tst	sfree
	bne	1f
	jsr	pc,sinit
1:
	jsr	pc,readc
	cmp	r5,$pdl
	bne	2f
	movb	$'S,ch
	jbr	eh
2:
	clr	r3
	cmp	r0,$128.	/ check for array
	blo	1f
	inc	r3
1:
	asl	r0
	mov	stable(r0),r1
	beq	2f
	mov	sfree,r2
	mov	(r2),sfree
	beq	symout
	mov	stable(r0),(r2)
	mov	r2,stable(r0)
	jsr	pc,pop
	tst	r3
	beq	1f
	jsr	pc,length	/ to make auto arrays work
	cmp	r0,$1
	bhi	1f
	jsr	pc,zero
1:
	mov	r1,2(r2)
	jbr	loop
2:
	mov	sfree,stable(r0)
	mov	stable(r0),r2
	mov	(r2),sfree
	beq	symout
	clr	(r2)
	jsr	pc,pop
	tst	r3
	beq	1f
	jsr	pc,length
	cmp	r0,$1
	bhi	1f
	jsr	pc,zero
1:
	mov	r1,2(r2)
	jbr	loop
/
/
/	case l for load
/
case154:
	jsr	pc,in154
	jmp	loop
/
in154:
	jsr	pc,readc
	clr	r2
	cmp	r0,$128.	/ check for array
	blo	1f
	inc	r2
1:
	asl	r0
	mov	stable(r0),r1
	beq	1f
	mov	2(r1),r1
	mov	r1,-(sp)
	jsr	pc,length
	jsr	pc,allocate
	tst	r2
	beq	2f
	mov	r1,-(sp)	/ have array - copy elements
	mov	2(sp),r1
	jsr	pc,rewind
3:
	mov	2(sp),r1
	jsr	pc,getword
	bes	3f
	tst	r0
	beq	4f
	mov	r0,-(sp)
	mov	r0,r1
	jsr	pc,length
	jsr	pc,allocate
	mov	(sp)+,r0
	jsr	pc,move
	mov	r1,r0
	mov	(sp),r1
	jsr	pc,putword
	br	3b
4:
	clr	r0
	mov	(sp),r1
	jsr	pc,putword
	br	3b
3:
	mov	(sp)+,r1
	jsr	pc,push
	tst	(sp)+
	rts	pc
2:
	mov	(sp)+,r0
	jsr	pc,move
	jsr	pc,push
	rts	pc
1:
	clr	r0
	jsr	pc,allocate
	jsr	pc,putword
	jsr	pc,push
	rts	pc
/
/	case : for save array
/
case072:
	tst	sfree
	bne	1f
	jsr	pc,sinit
1:
	jsr	pc,pop
	jes	eh
	jsr	pc,scalint
	jsr	pc,fsfile
	jsr	pc,backspace
	tst	r0
	jmi	eh1	/ neg. index
	jsr	pc,length
	cmp	r0,$2
	jhi	eh1	/ index too high
	jsr	pc,fsfile
	clr	r3
	cmp	r0,$1
	blo	1f
	beq	2f
	jsr	pc,backspace
	mov	r0,r3
	mul	$100.,r3
2:
	jsr	pc,backspace
	add	r0,r3
	cmp	r3,$2048.
	jhis	eh1	/ index too high
	asl	r3
1:
	jsr	pc,release
	jsr	pc,readc
	cmp	r5,$pdl
	bne	2f
	movb	$':,ch
	jmp	eh
2:
	asl	r0
	mov	stable(r0),r1
	beq	2f
	mov	r1,-(sp)
	mov	2(r1),r1
	mov	l(r1),r0
	sub	a(r1),r0
	sub	$2,r0
	cmp	r3,r0
	blos	1f
	mov	r1,-(sp)	/ need more space
	mov	r3,r0
	add	$2,r0
	jsr	pc,allocate
	jsr	pc,zero
	mov	(sp)+,r0
	jsr	pc,move
	mov	r1,-(sp)
	mov	r0,r1
	jsr	pc,release
	mov	(sp)+,r1
1:
	mov	r1,-(sp)
	mov	r3,r0
	jsr	pc,seekchar
	jsr	pc,getword
	bes	1f
	sub	$2,r(r1)
	tst	r0
	beq	1f
	mov	r0,r1
	jsr	pc,release
1:
	jsr	pc,pop
	jes	eh
	mov	r1,r0
	mov	(sp)+,r1
	jsr	pc,alterchar
	swab	r0
	jsr	pc,alterchar
	mov	(sp)+,r0
	mov	r1,2(r0)
	jmp	loop
2:
	mov	sfree,stable(r0)
	mov	stable(r0),r0
	mov	(r0),sfree
	jeq	symout
	clr	(r0)
	mov	r0,-(sp)
	mov	r3,r0
	add	$2,r0
	jsr	pc,allocate
	jsr	pc,zero
	sub	$2,r0
	jsr	pc,seekchar
	mov	r1,-(sp)
	br	1b
/
/	case ; for load array
/
case073:
	tst	sfree
	bne	1f
	jsr	pc,sinit
1:
	jsr	pc,pop
	jes	eh
	jsr	pc,scalint
	jsr	pc,fsfile
	jsr	pc,backspace
	tst	r0
	jmi	eh1	/ neg. index
	jsr	pc,length
	cmp	r0,$2
	jhi	eh1
	jsr	pc,fsfile
	clr	r3
	cmp	r0,$1
	blo	1f
	beq	2f
	jsr	pc,backspace
	mov	r0,r3
	mul	$100.,r3
2:
	jsr	pc,backspace
	add	r0,r3
	cmp	r3,$2048.
	jhis	eh1	/ index too high
	asl	r3
1:
	jsr	pc,release
	jsr	pc,readc
	asl	r0
	mov	stable(r0),r1
	beq	1f
	mov	2(r1),r1
	jsr	pc,length
	sub	$2,r0
	cmp	r3,r0
	bhi	1f	/ element not here
	mov	r3,r0
	jsr	pc,seekchar
	jsr	pc,getword
	tst	r0
	beq	1f
	mov	r0,r1
	mov	r1,-(sp)
	jsr	pc,length
	jsr	pc,allocate
	mov	(sp)+,r0
	jsr	pc,move
	jsr	pc,push
	jmp	loop
1:
	clr	r0
	jsr	pc,allocate
	jsr	pc,putword
	jsr	pc,push
	jmp	loop
/
/
/	case L for load
/
case114:
	jsr	pc,readc
	clr	r2
	cmp	r0,$128.	/ check for array
	blo	1f
	inc	r2
1:
	asl	r0
	mov	stable(r0),r1
	beq	4f
	mov	(r1),stable(r0)
	mov	sfree,(r1)
	mov	r1,sfree
	mov	2(r1),r1
	tst	r2
	beq	2f
	mov	r1,-(sp)	/ have array - assume a throw away
	jsr	pc,rewind
1:
	mov	(sp),r1
3:
	jsr	pc,getword
	bes	1f
	tst	r0
	beq	3b
	mov	r0,r1
	jsr	pc,release
	br	1b
1:
	mov	(sp)+,r1
2:
	jsr	pc,push
	jbr	loop
4:
	movb	$'L,ch
	jbr	eh
/
/
/	case - for subtract
/
case055:
	jsr	pc,in055
	jmp	loop
/
in055:
	jsr	pc,pop
	jes	eh
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,savk
	dec	w(r1)
	jsr	pc,chsign
	mov	savk,r0
	jsr	pc,putchar
	jsr	pc,push
	br	in053
/
/
/	case + for add
/
case053:
	jsr	pc,in053
	jmp	loop
/
in053:
	jsr	pc,eqk
	mov	$add3,r0
	jsr	pc,binop
	jsr	pc,pop
	mov	savk,r0
	jsr	pc,putchar
	jsr	pc,push
	rts	pc
/
/
/	case * for multiply
/
case052:
	jsr	pc,pop
	jes	eh
	mov	r1,-(sp)
	jsr	pc,pop
	jec	1f
	mov	(sp)+,r1
	jsr	pc,push
	jbr	eh
1:
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,savk2
	dec	w(r1)
	mov	r1,r2
	mov	(sp)+,r1
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,savk1
	dec	w(r1)
	mov	r1,r3
	mov	$mul3,r0
	jsr	pc,binop
	jsr	pc,pop
	cmp	savk1,savk2
	blo	1f
	mov	savk1,r2
	br	2f
1:
	mov	savk2,r2
2:
	cmp	r2,k
	bhis	1f
	mov	k,r2
1:
	add	savk2,savk1
	cmp	r2,savk1
	bhis	1f
	mov	r2,-(sp)
	neg	r2
	add	savk1,r2
	jsr	pc,removc
	mov	(sp)+,r0
2:
	jsr	pc,putchar
	jsr	pc,push
	jmp	loop
1:
	mov	savk1,r0
	br	2b
/
/	r1 = string
/	r2 = count
/	result returned in r1 (old r1 released)
/
removc:
	mov	r1,-(sp)
	jsr	pc,rewind
1:
	cmp	r2,$1
	blos	1f
	jsr	pc,getchar
	sub	$2,r2
	br	1b
1:
	mov	$2,r0
	jsr	pc,allocate
	mov	r1,-(sp)
1:
	mov	2(sp),r1
	jsr	pc,getchar
	bes	1f
	mov	(sp),r1
	jsr	pc,putchar
	mov	r1,(sp)
	br	1b
1:
	cmp	r2,$1
	bne	1f
	mov	(sp),r3
	mov	tenptr,r2
	jsr	pc,div3
	mov	r1,(sp)
	mov	r3,r1
	jsr	pc,release
	mov	r4,r1
	jsr	pc,release
1:
	mov	2(sp),r1
	jsr	pc,release
	mov	(sp)+,r1
	tst	(sp)+
	rts	pc
/
/	case / for divide
/
case057:
	jsr	pc,dscale
	mov	$div3,r0
	jsr	pc,binop
	mov	r4,r1
	jsr	pc,release
	jsr	pc,pop
	mov	savk,r0
	jsr	pc,putchar
	jsr	pc,push
	jmp	loop
/
/
dscale:
	jsr	pc,pop
	jes	eh
	mov	r1,-(sp)
	jsr	pc,pop
	bec	1f
	mov	(sp)+,r1
	jsr	pc,push
	jmp	eh
1:
	mov	r1,-(sp)
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,savk1
	dec	w(r1)
	jsr	pc,rewind
	mov	2(sp),r1
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,savk2
	dec	w(r1)
	mov	k,r2
	sub	savk1,r2
	add	savk2,r2
	mov	k,savk
	mov	(sp)+,r1
	tst	r2
	bmi	1f
	jsr	pc,add0
	br	2f
1:
	neg	r2
	jsr	pc,removc
2:
	mov	r1,r3
	mov	(sp)+,r2
	rts	pc
/
/
/	case % for remaindering
/
case045:
	jsr	pc,dscale
	mov	$div3,r0
	jsr	pc,binop
	jsr	pc,pop
	jsr	pc,release
	mov	r4,r1
	mov	savk1,r0
	add	k,r0
	jsr	pc,putchar
	jsr	pc,push
	jmp	loop
/
/
binop:
	jsr	pc,(r0)
	jsr	pc,push
	mov	r2,r1
	jsr	pc,release
	mov	r3,r1
	jsr	pc,release
	rts	pc
/
eqk:
	jsr	pc,pop
	jes	eh
	mov	r1,-(sp)
	jsr	pc,pop
	bec	1f
	mov	(sp)+,r1
	jsr	pc,push
	jbr	eh
1:
	mov	r1,-(sp)
	mov	2(sp),r1
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,savk1
	dec	w(r1)
	mov	(sp),r1
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,savk2
	dec	w(r1)
	cmp	r0,savk1
	beq	1f
	blo	2f
	mov	savk2,savk
	mov	r0,r2
	sub	savk1,r2
	mov	2(sp),r1
	jsr	pc,add0
	mov	r1,2(sp)
	br	4f
2:
	mov	savk1,r2
	sub	savk2,r2
	mov	(sp),r1
	jsr	pc,add0
	mov	r1,(sp)
1:
	mov	savk1,savk
4:
	mov	2(sp),r3
	mov	(sp)+,r2
	tst	(sp)+
	rts	pc
.bss
savk1:	.=.+2
savk2:	.=.+2
savk:	.=.+2
.text
/
/
/	r2 = count
/	r1 = ptr
/	returns ptr in r1
add0:
	mov	r1,-(sp)
	jsr	pc,length
	jsr	pc,allocate
	clr	r0
1:
	cmp	r2,$1
	blos	1f
	jsr	pc,putchar
	sub	$2,r2
	br	1b
1:
	mov	r1,-(sp)
	mov	2(sp),r1
	jsr	pc,rewind
1:
	jsr	pc,getchar
	bes	1f
	mov	(sp),r1
	jsr	pc,putchar
	mov	r1,(sp)
	mov	2(sp),r1
	br	1b
1:
	cmp	r2,$1
	bne	1f
	mov	(sp),r3
	mov	tenptr,r2
	jsr	pc,mul3
	mov	r1,(sp)
	mov	r3,r1
	jsr	pc,release
1:
	mov	2(sp),r1
	jsr	pc,release
	mov	(sp)+,r1
	tst	(sp)+
	rts	pc
/	case i for input base
/
case151:
	jsr	pc,in151
	jmp	loop
/
in151:
	jsr	pc,pop
	jes	eh
	jsr	pc,scalint
	mov	r1,-(sp)
	mov	inbas,r1
	mov	(sp)+,inbas
	jsr	pc,release
	rts	pc
case111:
	mov	inbas,r1
	jsr	pc,length
	inc	r0
	jsr	pc,allocate
	mov	inbas,r0
	jsr	pc,move
	clr	r0
	jsr	pc,putchar	/scale
	jsr	pc,push
	jmp	loop
/
.bss
inbas:	.=.+2
.data
/
/
/	case o for output base
/
case157:
	jsr	pc,in157
	jmp	loop
/
in157:
	jsr	pc,pop
	jes	eh
	jsr	pc,scalint
	mov	r1,-(sp)
	jsr	pc,length
	jsr	pc,allocate
	mov	(sp),r0
	jsr	pc,move
	jsr	pc,fsfile
	jsr	pc,length
1:
	cmp	r0,$1
	beq	1f
	jsr	pc,backspace
	bpl	2f
	jsr	pc,chsign
	jsr	pc,length
	br	1b
2:
	clr	sav
	mov	r0,-(sp)
2:
	jsr	pc,backspace
	bes	2f
	mov	(sp),r2
	clr	r3
	mul	$100.,r2
	add	r0,r3
	mov	r3,(sp)
	tst	sav
	beq	3f
	mov	r2,r0
	clr	r3
	mov	sav,r2
	mul	$100.,r2
	mov	r3,sav
	add	r0,sav
	br	2b
3:
	mov	r2,sav
	br	2b
2:
	mov	(sp)+,r0
	tst	sav
	beq	2f
	mov	sav,r0
	jsr	pc,log2
	add	$16.,r0
	mov	r0,logo
	br	3f
1:
	jsr	pc,backspace
2:
	tst	r0
	bpl	1f
	mov	$15.,logo
	br	3f
1:
	jsr	pc,log2
	mov	r0,logo
3:
	jsr	pc,release
	mov	basptr,r1
	jsr	pc,release
	mov	(sp),basptr
/
/	set field widths for output
/	and set output digit handling routines
/
	mov	(sp),r1
	mov	$bigout,outdit
	jsr	pc,length
	cmp	r0,$1.
	bne	2f
	jsr	pc,fsfile
	jsr	pc,backspace
	cmp	r0,$16.
	bhi	2f
	mov	$hexout,outdit
2:
	jsr	pc,length
	jsr	pc,allocate
	mov	(sp),r0
	jsr	pc,move
	clr	(sp)
	jsr	pc,fsfile
	jsr	pc,backspace
	bpl	2f
	add	$1.,(sp)
	jsr	pc,chsign
2:
	mov	r1,r2
	mov	$1,r0
	jsr	pc,allocate
	mov	$-1,r0
	jsr	pc,putchar
	mov	r1,r3
	jsr	pc,add3
	jsr	pc,length
	asl	r0
	add	r0,(sp)
	jsr	pc,fsfile
	jsr	pc,backspace
	cmp	r0,$9.
	blos	2f
	add	$1,(sp)
2:
	jsr	pc,release
	mov	r2,r1
	jsr	pc,release
	mov	r3,r1
	jsr	pc,release
	mov	(sp)+,fw
	mov	fw,fw1
	dec	fw1
	cmp	outdit,$hexout
	bne	2f
	mov	$1,fw
	clr	fw1
2:
	mov	$70.,ll
	cmp	fw,$70.
	blo 9f; rts pc; 9:
	mov	$70.,r1
	clr	r0
	dvd	fw,r0
	mov	r0,r1
	mpy	fw,r1
	mov	r1,ll
	rts	pc
case117:
	mov	basptr,r1
	jsr	pc,length
	inc	r0
	jsr	pc,allocate
	mov	basptr,r0
	jsr	pc,move
	clr	r0
	jsr	pc,putchar	/scale
	jsr	pc,push
	jmp	loop
/
.data
fw:	1			/field width for digits
fw1:	0
ll:	70.			/line length
.text
/
/
/	case k for skale factor
/
case153:
	jsr	pc,pop
	jes	eh
	jsr	pc,scalint
	mov	w(r1),r0
	sub	a(r1),r0
	cmp	r0,$1
	jhi	eh1
	jsr	pc,rewind
	jsr	pc,getchar
	jmi	eh1
	mov	r0,k
	mov	r1,-(sp)
	mov	scalptr,r1
	jsr	pc,release
	mov	(sp)+,scalptr
	jmp	loop
/
case113:
	mov	scalptr,r1
	jsr	pc,length
	inc	r0
	jsr	pc,allocate
	mov	scalptr,r0
	jsr	pc,move
	clr	r0
	jsr	pc,putchar
	jsr	pc,push
	jmp	loop
scalint:
	jsr	pc,fsfile
	jsr	pc,backspace
	dec	w(r1)
	mov	r0,r2
	jsr	pc,removc
	rts	pc
/
/	case ^ for exponentiation
/
case136:
	jsr	pc,pop
	jes	eh
	jsr	pc,scalint
	jsr	pc,fsfile
	jsr	pc,backspace
	tst	r0
	bge	1f
	inc	negexp
	jsr	pc,chsign
1:
	jsr	pc,length
	cmp	r0,$3
	jhis	eh1
	mov	r1,r3
	jsr	pc,pop
	jes	eh
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,savk
	dec	w(r1)
	mov	r1,r2
	jsr	pc,exp3
	mov	r1,-(sp)
	mov	r2,r1
	jsr	pc,release
	mov	r3,r1
	jsr	pc,rewind
	jsr	pc,getchar
	mov	r0,-(sp)
	jsr	pc,getchar
	bes	2f
	mov	r0,r1
	mul	$100.,r1
	add	(sp)+,r1
	br	3f
2:
	mov	(sp)+,r1
3:
	mul	savk,r1
	mov	r1,r2
	mov	r3,r1
	jsr	pc,release
	tst	negexp
	bne	4f
	cmp	k,savk
	blo	1f
	mov	k,r3
	br	2f
1:
	mov	savk,r3
2:
	cmp	r3,r2
	bhis	4f
	sub	r3,r2
	mov	(sp)+,r1
	mov	r3,-(sp)
	jsr	pc,removc
	mov	(sp)+,r0
	jsr	pc,putchar
	jsr	pc,push
	br	3f
4:
	mov	(sp)+,r1
	mov	r2,r0
	jsr	pc,putchar
	jsr	pc,push
3:
	tst	negexp
	jeq	loop
	clr	negexp
	jsr	pc,pop
	mov	r1,-(sp)
	mov	$2,r0
	jsr	pc,allocate
	mov	$1,r0
	jsr	pc,putchar
	clr	r0
	jsr	pc,putchar
	jsr	pc,push
	mov	(sp)+,r1
	jsr	pc,push
	jmp	case057
/
.bss
sav:	.=.+2
negexp:	.=.+2
.text
/
/	case v for square root
/
case166:
	jsr	pc,pop
	jes	eh
/
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,savk
	dec	w(r1)
	mov	w(r1),r2
	sub	a(r1),r2
	tst	r2
	beq	sqz
	jsr	pc,backspace
	tst	r0
	jmi	eh1
	mov	k,r2
	asl	r2
	sub	savk,r2
	beq	1f
	blo	2f
	jsr	pc,add0
	br	1f
2:
	neg	r2
	jsr	pc,removc
1:
	jsr	pc,sqrt
	mov	k,r0
	jsr	pc,putchar
	jsr	pc,push
	jmp	loop
/
/
sqz:
	mov	savk,r0
	jsr	pc,putchar
	jsr	pc,push
	jmp	loop
.bss
sqtemp:	.=.+2
.text
/
/
/	case [ for subroutine definition
/
case133:
	clr	-(sp)
	clr	r0
	jsr	pc,allocate
	jsr	pc,push
1:	jsr	pc,readc
	cmp	r0,$']
	bne	3f
	tst	(sp)
	beq	1f
	dec	(sp)
	br	2f
3:
	cmp	r0,$'[
	bne	2f
	inc	(sp)
2:
	jsr	pc,putchar
	br	1b
/
1:	tst	(sp)+
	jmp	loop
/
/
/	case x for execute top of stack
/
case170:
	jsr	pc,in170
	jmp	loop
/
in170:
	jsr	pc,pop
	jes	eh
	mov	r1,-(sp)
	tst	*readptr
	beq	1f
	mov	*readptr,r1
	cmp	r(r1),w(r1)
	bne	1f
	jsr	pc,release
	br	2f
1:
	add	$2,readptr
	cmp	readptr,$readtop
	bhis	1f
2:	mov	(sp)+,r1
	mov	r1,*readptr
	beq	2f
	jsr	pc,rewind
	rts	pc
2:
	jsr	pc,readc
	cmp	r0,$'\n
	beq	3f
	mov	r0,savec
3:
	rts	pc
1:
nderr:
	mov	$1,r0
	sys	write; 1f; 2f-1f
	jmp	reset
1:	<Nesting depth.\n>
2:	.even
/
.data
readptr: readstack
.bss
readstack: .=.+100.
readtop:
.text
/
/	case ? for apl box function
/
case077:
	add	$2,readptr
	cmp	readptr,$readtop
	bhis	nderr
	clr	*readptr
in077:
	mov	source,-(sp)
	clr	source
	jsr	pc,readc
	cmp	r0,$'!
	bne	1f
	jsr	pc,in041
	mov	(sp)+,source
	br	in077
1:
	mov	r0,savec
	clr	r0
	jsr	pc,allocate
2:
	jsr	pc,readc
	jsr	pc,putchar
1:
	jsr	pc,readc
	jsr	pc,putchar
	cmp	r0,$'\\
	beq	2b
	cmp	r0,$'\n
	bne	1b
	mov	(sp)+,source
	mov	r1,*readptr
	jmp	loop
/
/
/	case < for conditional execution
/
case074:
	jsr	pc,in074
	ble	neg074
	jmp	aff074
/
/
/	case !< for conditional execution
/
in74a:
	jsr	pc,in074
	bgt	inneg
	jmp	inaff
/
in074:
	jsr	pc,in055	/go subtract
	jsr	pc,pop
	jsr	pc,length
	tst	r0
	beq	1f
	jsr	pc,fsfile
	jsr	pc,backspace
	jsr	pc,backspace
	tst	r0
1:
	rts	pc
/
aff074:
	jsr	pc,release
	jsr	pc,in154	/load from register
	jmp	case170
/
neg074:
	jsr	pc,release
	jsr	pc,readc
	jmp	loop
/
/
/	case = for conditional execution
/
case075:
	jsr	pc,in074
	beq	aff074
	jmp	neg074
/
/
/	case != for conditional execution
/
in75a:
	jsr	pc,in074
	bne	inaff
	jmp	inneg
/
/
/	case > for conditional execution
/
case076:
	jsr	pc,in074
	bge	neg074
	jmp	aff074
/
/
/	case !> for conditional execution
/
in76a:
	jsr	pc,in074
	blt	inneg
	jmp	inaff
/
inaff:
	jsr	pc,release
	jsr	pc,in154
	jsr	pc,in170
	rts	pc
/
inneg:
	jsr	pc,release
	jsr	pc,readc
	rts	pc
/
err:
	mov	$1,r0
	sys	write; 1f; 2f-1f
	jmp	reset
1:	<Fatal error\n>; 2: .even
/
eh1:
	jsr	pc,release
eh:
	movb	ch,1f+2
	mov	$1,r0
	sys	write; 1f; 2f-1f
	mov	$readstack,readptr
	mov	errstack,sp
	jmp	loop
.data
1:	<(  ) ?\n>
2:	.even
.text
/
/
/	routine to read and convert a number from the
/	input stream.  Numbers beginnig with 0 are
/	converted as octal.  Routine converts
/	up to next nonnumeric.
/
/
readin:
	clr	dp
	clr	dpt
	clr	r0
	jsr	pc,allocate
	mov	r1,-(sp)
	mov	strptr,r1
	jsr	pc,create
	jsr	pc,readc
1:
	cmpb	ch,$'0
	blt	3f
	cmpb	ch,$'9
	bgt	3f
	mov	ch,r0
	sub	$'0,r0
4:
	tst	dp
	beq	8f
	cmp	dpt,$99.
	beq	5f
	inc	dpt
8:
	mov	chptr,r1
	jsr	pc,create
	tst	r0
	beq	2f
	jsr	pc,putchar
2:	mov	r1,chptr
	mov	(sp),r3
	mov	inbas,r2
	jsr	pc,mul3
	mov	r1,(sp)
	mov	r3,r1
	jsr	pc,release
	mov	(sp),r3
	mov	chptr,r2
	jsr	pc,add3
	mov	r1,(sp)
	mov	r3,r1
	jsr	pc,release
5:
	jsr	pc,readc
	mov	r0,ch
	br	1b
3:
	cmpb	ch,$'A
	blt	1f
	cmpb	ch,$'F
	bgt	1f
	mov	ch,r0
	sub	$67,r0
	br	4b
1:
	cmpb	ch,$134		/backslash
	bne	1f
	jsr	pc,readc
	br	5b
1:
	cmpb	ch,$'.
	bne	1f
	tst	dp
	bne	1f
	inc	dp
	clr	dpt
	br	5b
1:
	mov	r0,savec
/
/	scale up or down
2:
	tst	dp
	bne	1f
	mov	(sp)+,r1
	clr	r0
	jsr	pc,putchar
	rts	pc
1:
	mov	(sp),r1
	jsr	pc,scale
	mov	dpt,r0
	jsr	pc,putchar
	tst	(sp)+
	rts	pc
/
.bss
dp:	.=.+2
dpt:	.=.+2
.text
/
scale:
	mov	dpt,r2
	jsr	pc,add0
	mov	r1,-(sp)
	mov	$1,r0
	jsr	pc,allocate
	mov	dpt,r0
	jsr	pc,putchar
	mov	r1,r3
	mov	inbas,r2
	jsr	pc,exp3
	mov	r1,-(sp)
	mov	r3,r1
	jsr	pc,release
	mov	(sp)+,r2
	mov	(sp)+,r3
	jsr	pc,div3
	mov	r1,-(sp)
	mov	r2,r1
	jsr	pc,release
	mov	r3,r1
	jsr	pc,release
	mov	r4,r1
	jsr	pc,release
	mov	(sp)+,r1
	rts	pc
/
/	routine to read another character from the input
/	stream.  If the caller does not want the character,
/	it is to be placed in the cell savec.
/	The routine exits to the system on end of file.
/	Character is returned in r0.
/
/	jsr	pc,readc
/	movb	r0,...
/
/
readc:
	tst	savec
	beq	1f
	movb	savec,r0
	bic	$177400,r0
	clr	savec
	rts	pc
1:
	tst	*readptr
	bne	1f
2:	mov	source,r0
	sys	read; ch; 1
	bes	eof
	tst	r0
	beq	eof
	movb	ch,r0
	bic	$177400,r0
	rts	pc
1:
	mov	r1,-(sp)
	mov	*readptr,r1
	jsr	pc,getchar
	bes	eof1
	bic	$177400,r0
	mov	r0,ch
	mov	(sp)+,r1
	rts	pc
/
eof:
	tst	source
	beq	1f
	clr	source
	br	2b
1:
	sys	exit
/
eof1:
	mov	*readptr,r1
	beq	2f
	jsr	pc,release
2:
	sub	$2,readptr
	mov	(sp)+,r1
	jmp	readc
/
/
/	case p for print
/
case160:
	cmp	r5,$pdl
	jeq	eh
	jsr	pc,in160
	jmp	loop
/
/
in160:
/	mov	$1,r0
/	sys	write; sphdr; 4
	br	1f
/
sphdr:	<    >
	.even
/
1:	cmp	r5,$pdl
	bne	1f
	mov	$1,r0
	sys	write; qm; 1
	mov	$1,r0
	sys	write; nl; 1
	rts	pc
/
/	do the conversion
/
1:
	mov	-2(r5),r1
	jsr	pc,printf
	rts	pc
/
/
/	case f for print the stack
/
case146:
	mov	r5,-(sp)
1:
	cmp	r5,$pdl
	beq	2f
1:
	jsr	pc,in160
	jsr	pc,pop
	cmp	r5,$pdl
	bne	1b
2:
	mov	$stable-2,r2
1:
	tst	(r2)+
	cmp	r2,$stable+254.
	bhi	1f
/
	mov	(r2),r3
	beq	1b
	movb	$'0,7f+3
	mov	r2,r0
	sub	$stable,r0
	asr	r0
	movb	r0,7f+1
3:
	mov	$1,r0
	sys	write; 7f; 8f-7f
.data
7:	<" (0)">
8:	.even
.text
	mov	2(r3),r1
	jsr	pc,printf
	tst	(r3)
	beq	1b
	incb	7b+3
	mov	(r3),r3
	br	3b
1:
	mov	(sp)+,r5
	jbr	loop
/
/
/	routine to convert to decimal and print the
/	top element of the stack.
/
/	jsr	pc,printf
/
/
printf:
	mov	r4,-(sp)
	mov	r3,-(sp)
	mov	r2,-(sp)
	mov	r1,-(sp)
	mov	r0,-(sp)
	clr	-(sp)
	jsr	pc,rewind
2:
	jsr	pc,getchar
	bes	2f
	cmp	r0,$143
	blos	2b
	cmp	r0,$-1
	beq	2b
	bis	$1,(sp)
	br	2b
2:
	tst	(sp)+
	beq	2f
	jsr	pc,length
	mov	r0,0f
	mov	a(r1),3f
	mov	$1,r0
	sys	0; 9f
.data
9:
	sys	write; 3:.=.+2; 0:.=.+2
.text
	jbr	prout
2:
	jsr	pc,fsfile
	jsr	pc,backspace
	bec	1f
	mov	$1,r0
	sys	write; asczero; 1
	jbr	prout
1:
	jsr	pc,length
	mov	r1,-(sp)
	jsr	pc,allocate
	mov	(sp),r0
	mov	r1,(sp)
	jsr	pc,move
	mov	ll,count
/	inc	count
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,savk
	dec	w(r1)
	jsr	pc,backspace
	cmpb	r0,$-1
	bne	2f
	mov	basptr,r1
	jsr	pc,fsfile
	jsr	pc,backspace
	cmp	r0,$-1
	beq	2f
	mov	(sp),r1
	jsr	pc,chsign
	mov	$'-,ch
	jsr	pc,wrchar
	br	1f
2:
/	mov	$' ,ch
/	jsr	pc,wrchar
1:
	mov	strptr,r1
	jsr	pc,create
	mov	basptr,r1
	jsr	pc,length
	cmp	r0,$1
	jlo	dingout
	bne	1f
	jsr	pc,rewind
	jsr	pc,getchar
	cmp	r0,$1.
	jeq	unout
	cmp	r0,$-1
	jeq	dingout
	cmp	r0,$10.
	jeq	tenout
1:
	mov	log10,r1
	mul	savk,r1
	clr	r0
	div	logo,r0
	mov	r0,dout
	clr	ct
1:
	mov	(sp),r3
	mov	savk,r2
	jsr	pc,getdec
	mov	r1,decimal
	clr	dflg
	mov	(sp),r1
	mov	savk,r2
	jsr	pc,removc
	mov	r1,(sp)
1:
	mov	(sp),r3
	mov	basptr,r2
	jsr	pc,div3
	mov	r1,r2
	mov	(sp),r1
	jsr	pc,release
	mov	r2,(sp)
	mov	r4,r1
	jsr	pc,*outdit
	mov	(sp),r1
	jsr	pc,length
	bne	1b
/
	mov	strptr,r1
	jsr	pc,fsfile
1:
	jsr	pc,backspace
	bes	1f
	mov	r0,ch
	jsr	pc,wrchar
	br	1b
1:
	mov	(sp)+,r1
	jsr	pc,release
	tst	savk
	bne	1f
	mov	decimal,r1
	jsr	pc,release
	br	prout
1:
	mov	dot,ch
	jsr	pc,wrchar
	mov	strptr,r1
	jsr	pc,create
	mov	decimal,-(sp)
	inc	dflg
1:
	mov	(sp),r3
	mov	basptr,r2
	jsr	pc,mul3
	mov	r1,(sp)
	mov	r3,r1
	jsr	pc,release
	mov	(sp),r3
	mov	savk,r2
	jsr	pc,getdec
	mov	r1,(sp)
	mov	r3,r1
	mov	savk,r2
	jsr	pc,removc
	jsr	pc,*outdit
	mov	strptr,r1
	inc	ct
	cmp	ct,dout
	blo	1b
	mov	(sp)+,r1
	jsr	pc,release
	mov	strptr,r1
	jsr	pc,rewind
1:
	jsr	pc,getchar
	bes	1f
	mov	r0,ch
	jsr	pc,wrchar
	br	1b
1:
/
/	cleanup, print new line and return
/
prout:	mov	$1,r0
	sys	write; nl; 1
	mov	(sp)+,r0
	mov	(sp)+,r1
	mov	(sp)+,r2
	mov	(sp)+,r3
	mov	(sp)+,r4
	rts	pc
/
/
/
/	r2 = count
/	r3 = pointer (not released)
/
.bss
dflg:	.=.+2
dout:	.=.+2
logo:	.=.+2
log10:	.=.+2
decimal:	.=.+2
.text
getdec:
	mov	r3,-(sp)
	mov	r3,r1
	jsr	pc,rewind
	jsr	pc,length
	jsr	pc,allocate
	mov	r1,-(sp)
1:
	cmp	r2,$1
	blt	1f
	mov	2(sp),r1
	jsr	pc,getchar
	mov	(sp),r1
	jsr	pc,putchar
	mov	r1,(sp)
	sub	$2,r2
	br	1b
1:
	tst	r2
	beq	1f
	mov	tenptr,r2
	mov	(sp),r3
	jsr	pc,mul3
	mov	r1,(sp)
	mov	r3,r1
	jsr	pc,length
	jsr	pc,release
	mov	r0,r3
	jsr	pc,allocate
	mov	r1,-(sp)
	mov	2(sp),r1
	jsr	pc,rewind
2:
	tst	r3
	beq	2f
	jsr	pc,getchar
	mov	(sp),r1
	jsr	pc,putchar
	mov	r1,(sp)
	dec	r3
	mov	2(sp),r1
	br	2b
2:
	clr	r0
	mov	(sp),r1
	jsr	pc,putchar
	mov	2(sp),r1
	jsr	pc,release
	mov	(sp),r3
	mov	tenptr,r2
	jsr	pc,div3
	mov	r1,(sp)
	mov	r3,r1
	jsr	pc,release
	mov	r4,r1
	jsr	pc,release
	mov	(sp)+,r1
	tst	(sp)+
	mov	(sp)+,r3
	rts	pc
1:
	mov	(sp)+,r1
	mov	(sp)+,r3
	rts	pc
tenout:
	mov	savk,ct
	mov	$2,r0
	jsr	pc,allocate
	mov	r1,-(sp)
	mov	2(sp),r1
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,r3
	clr	r2
	dvd	$10.,r2
	beq	1f
3:
	add	$60,r2
	mov	r2,r0
	mov	(sp),r1
	jsr	pc,putchar
	mov	r1,(sp)
1:
	mov	(sp),r1
	add	$60,r3
	mov	r3,r0
	jsr	pc,putchar
	mov	2(sp),r1
1:
	jsr	pc,backspace
	bec	2f
	mov	(sp),r1
	jsr	pc,length
	cmp	r0,ct
	beq	4f
	blo	5f
	sub	ct,r0
	mov	r0,ct
1:
	jsr	pc,getchar
	mov	r0,ch
	jsr	pc,wrchar
	dec	ct
	bne	1b
	jsr	pc,getchar
	bes	6f
	jsr	pc,backspace
4:
	movb	dot,ch
	jsr	pc,wrchar
1:
	jsr	pc,getchar
	bes	1f
	mov	r0,ch
	jsr	pc,wrchar
	br	1b
5:
	sub	r0,ct
	movb	dot,ch
	jsr	pc,wrchar
	mov	$60,ch
5:
	jsr	pc,wrchar
	dec	ct
	bne	5b
	br	1b
1:
6:
	mov	(sp)+,r1
	jsr	pc,release
	mov	(sp)+,r1
	jsr	pc,release
	jbr	prout
2:
	mov	r0,r3
	clr	r2
	dvd	$10.,r2
	br	3b
dot:	<.>
	.even
ct:	.=.+2
/
/
dingout:
	clr	-(sp)
	br	1f
unout:
	mov	$1,-(sp)
1:
	mov	2(sp),r1
	mov	savk,r2
	jsr	pc,removc
	mov	r1,2(sp)
	mov	strptr,r1
	jsr	pc,create
	mov	$-1,r0
	jsr	pc,putchar
	mov	r1,r3
1:
	mov	2(sp),r1
	jsr	pc,length
	beq	1f
	mov	r1,r2
	jsr	pc,add3
	mov	r1,2(sp)
	mov	r2,r1
	jsr	pc,release
	mov	$1,r0
	tst	(sp)
	beq	2f
	mov	$'1,ch
	jsr	pc,wrchar
	br	1b
2:
	tst	delflag
	jne	in177
	sys	write; ding; 3
	br	1b
1:
	tst	(sp)+
	mov	(sp)+,r1
	jsr	pc,release
	jmp	prout
/
ding:	<>			/<bell prefix form feed>
sp5:	<\\\n     >
minus:	<->
one:	<1>
	.even
.bss
count:	.=.+2
.text
/
bigout:
	mov	r1,-(sp)	/big digit
	tst	dflg
	beq	1f
	clr	r0
	jsr	pc,allocate
	mov	r1,tptr
1:
	mov	strptr,r1
	jsr	pc,length
	add	fw,r0
	dec	r0
	mov	r0,-(sp)	/end of field
	clr	-(sp)		/negative
	mov	4(sp),r1
	jsr	pc,length
	bne	2f
	mov	$'0,r0
	tst	dflg
	beq	3f
	mov	tptr,r1
	jsr	pc,putchar
	mov	r1,tptr
	br	1f
3:
	mov	strptr,r1
	jsr	pc,putchar
	br	1f
2:
	mov	4(sp),r1	/digit
	jsr	pc,fsfile
	jsr	pc,backspace
	bpl	2f
	mov	$1,(sp)		/negative
	jsr	pc,chsign
2:
	mov	4(sp),r3	/digit
	mov	r3,r1
	jsr	pc,length
	beq	1f
	mov	tenptr,r2
	jsr	pc,div3
	mov	r1,4(sp)	/digit
	mov	r3,r1
	jsr	pc,release
	mov	r4,r1
	jsr	pc,rewind
	jsr	pc,getchar
	jsr	pc,release
	add	$'0,r0
	tst	dflg
	beq	3f
	mov	tptr,r1
	jsr	pc,putchar
	mov	r1,tptr
	br	2b
3:
	mov	strptr,r1
	jsr	pc,putchar
	br	2b
1:
	tst	dflg
	beq	4f
	mov	tptr,r1
	jsr	pc,length
	cmp	r0,fw1
	bhis	2f
	mov	fw1,r1
	sub	r0,r1
	mov	r1,-(sp)
	mov	strptr,r1
3:
	mov	$'0,r0
	jsr	pc,putchar
	dec	(sp)
	bne	3b
	tst	(sp)+
2:
	mov	tptr,r1
	jsr	pc,fsfile
2:
	mov	tptr,r1
	jsr	pc,backspace
	bes	2f
	mov	strptr,r1
	jsr	pc,putchar
	br	2b
2:
	mov	tptr,r1
	jsr	pc,release
	br	1f
4:
	mov	strptr,r1
	jsr	pc,length
	cmp	r0,2(sp)	/end of field
	bhis	1f
	mov	$'0,r0
	jsr	pc,putchar
	br	1b
1:
	tst	(sp)		/negative
	beq	1f
	mov	$'-,r0
	mov	strptr,r1
	dec	w(r1)
	jsr	pc,putchar
1:
	mov	strptr,r1
	mov	$' ,r0
	jsr	pc,putchar
	tst	(sp)+
	tst	(sp)+
	mov	(sp)+,r1
	jsr	pc,release
	rts	pc
/
.bss
tptr:	.=.+2
tenptr:	.=.+2
.text
/
/
/
hexout:
	mov	r1,-(sp)
	jsr	pc,rewind
	jsr	pc,getchar
	cmp	r0,$16.
	blo	1f
	jmp	err
1:
	add	$60,r0
	cmp	r0,$'9
	blos	2f
	add	$'A-'9-1,r0
2:
	mov	strptr,r1
	jsr	pc,putchar
	mov	(sp)+,r1
	jsr	pc,release
	rts	pc
/
/
wrchar:
	tst	delflag
	jne	in177
	mov	$1,r0
	tst	count
	bne	7f
	sys	write; sp5; 2
	mov	ll,count
	mov	$1,r0
7:
	dec	count
	sys	write; ch; 1
	rts	pc
/
/
/	case P for print an ascii string
/
/
case120:
	jsr	pc,pop
	jes	eh
	jsr	pc,length
	mov	r0,0f
	mov	a(r1),3f
	mov	$1,r0
	sys	0; 9f
	jsr	pc,release
	jmp	loop
.data
9:	sys	write; 3:.=.+2; 0:.=.+2
.text
/
/
/	here for unimplemented stuff
/
junk:
	movb	r0,1f
	mov	$1,r0
	sys	write; 1f; 2f-1f
	jmp	loop
.data
1:	<0 not in switch.\n>
2:	.even
.text
/
/
/
/	routine to place one word onto the pushdown list
/	Error exit to system on overflow.
/
/
push:
	mov	r1,(r5)+
	cmp	r5,$pdltop
	bhis	pdlout
	rts	pc
/
pdlout:
	mov	$1,r0
	sys	write; 1f; 2f-1f
	jmp	reset
1:	<Out of pushdown.\n>
2:	.even
/
/
/	routine to remove one word from the pushdown list
/	carry bit set on empty stack
/
/
/	jsr	pc,pop
/
pop:
	cmp	r5,$pdl
	bhi	1f
	clr	r1
	sec
	rts	pc
1:	mov	-(r5),r1
	clc
	rts	pc
/
/
/
/
.data
outdit:	hexout
.bss
source: .=.+2
savec:	.=.+2
ch:	.=.+2
.text
nl:	<\n>
asczero:	<0>
qm:	<?\n>
	.even
/
.bss
chptr:	.=.+2
strptr:	.=.+2
basptr:	.=.+2
scalptr:	.=.+2
errstack:.=.+2
/
stable:	.=.+512.
.text
casetab:
	case012; 012	/nl
	loop;    040	/sp
	case041; 041	/!
	case045; 045	/%
	case052; 052	/*
	case053; 053	/+
	case055; 055	/-
	case060; 056	/.
	case057; 057	//
	case060; 060	/0
	case060; 061	/1
	case060; 062	/2
	case060; 063	/3
	case060; 064	/4
	case060; 065	/5
	case060; 066	/6
	case060; 067	/7
	case060; 070	/8
	case060; 071	/9
	case072; 072	/:
	case073; 073	/;
	case074; 074	/<
	case075; 075	/=
	case076; 076	/>
	case077; 077	/?
	case060; 101	/A
	case060; 102	/B
	case060; 103	/C
	case060; 104	/D
	case060; 105	/E
	case060; 106	/F
	case111; 111	/I
	case113; 113	/K
	case114; 114	/L
	case117; 117	/O
	case120; 120	/P
	case121; 121	/Q
	case123; 123	/S
	case166;  126	/V
	case170; 130	/X
	case172; 132	/Z
	case133; 133	/[
	case136; 136	/^
	case137; 137	/_
	case143; 143	/c
	case144; 144	/d
	case146; 146	/f
	case151; 151	/i
	case153; 153	/k
	case154; 154	/l
	case157; 157	/o
	case160; 160	/p
	case161; 161	/q
	case163; 163	/s
	case166; 166	/v
	case170; 170	/x
	case172; 172	/z
	0;0
/
.bss
pdl:	.=.+100.
pdltop:
.text

reset:
	clr	r0
	sys	seek; 0; 2
1:
	clr	r0
	sys	read; rathole; 1
	bes	1f
	tst	r0
	beq	1f
	cmpb	rathole,$'q
	bne	1b
1:
	sys	exit
.bss
rathole:	.=.+2
.text
