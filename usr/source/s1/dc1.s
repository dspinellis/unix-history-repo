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
	sys	0; 9f
.data
9:
	sys	open; 0:.=.+2; 0
.text
	bec	2f
	mov	$1,r0
	sys	write; 4f; 5f-4f
	sys	exit

error:
	4
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
	clr	delflag
	mov	$pdl,r5
/
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
	clr	r0
	jsr	pc,allocate
	mov	$1,r0
	jsr	pc,putchar
	mov	r1,kptr
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
	jsr	pc,chsign
	jsr	pc,push
	br	loop
/
/
/	case screamer
/
case041:
	jsr	pc,in041
	br	loop
/
in041:
	sys	fork
		br	9f
	sys	wait
	mov	$1,r0
	sys	write; screamer; 2
	rts	pc
9:	sys	exec; 7f; 8f
	4
8:	7f; 0
7:	</etc/msh\0>
screamer: <!\n>
	.even
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
	br	loop
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
	jsr	pc,readc
	cmp	r5,$pdl
	bne	2f
	movb	$'s,ch
	jmp	eh
2:
	cmpb	r0,$128.
	jhis	err
	asl	r0
	mov	stable(r0),r1
	beq	2f
	jsr	pc,release
2:
	jsr	pc,pop
	mov	r1,stable(r0)
	jmp	loop
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
	cmp	r0,$128.
	jhis	err
	asl	r0
	mov	stable(r0),r1
	beq	1f
	mov	r1,-(sp)
	jsr	pc,length
	jsr	pc,allocate
	mov	(sp)+,r0
	jsr	pc,move
	jsr	pc,push
	rts	pc
1:
	clr	r0
	jsr	pc,allocate
	jsr	pc,push
	rts	pc
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
	jsr	pc,chsign
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
	mov	$add3,r0
	jsr	pc,binop
	rts	pc
/
/
/	case * for multiply
/
case052:
	mov	$mul3,r0
	jsr	pc,binop
	tst	k
	beq	1f
	jsr	pc,pop
	mov	r1,r3
	mov	kptr,r2
	jsr	pc,div3
	jsr	pc,push
	mov	r3,r1
	jsr	pc,release
	mov	r4,r1
	jsr	pc,release
1:	jmp	loop
/
/	case / for divide
/
case057:
	mov	$1f,r0
	jsr	pc,binop
	mov	r4,r1
	jsr	pc,release
	jmp	loop
1:
	tst	k
	beq	1f
	mov	r2,-(sp)
	mov	kptr,r2
	jsr	pc,mul3
	mov	r1,-(sp)
	mov	r3,r1
	jsr	pc,release
	mov	(sp)+,r3
	mov	(sp)+,r2
1:	jsr	pc,div3
	rts	pc
/
/
/	case % for remaindering
/
case045:
	mov	$div3,r0
	jsr	pc,binop
	jsr	pc,pop
	jsr	pc,release
	mov	r4,r1
	jsr	pc,push
	jmp	loop
/
/
binop:
	jsr	pc,pop
	jes	eh
	mov	r1,r2
	jsr	pc,pop
	jec	1f
	mov	r2,r1
	jsr	pc,push
	jbr	eh
1:
	mov	r1,r3
	jsr	pc,(r0)
	jsr	pc,push
	mov	r2,r1
	jsr	pc,release
	mov	r3,r1
	jsr	pc,release
	rts	pc
/
/
/	case i for input base
/
case151:
	jsr	pc,in151
	jmp	loop
/
in151:
	jsr	pc,pop
	jes	eh
	mov	r1,-(sp)
	mov	inbas,r1
	mov	(sp)+,inbas
	jsr	pc,release
	rts	pc
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
	mov	r1,-(sp)
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
	cmp	outdit,$hexout
	bne	2f
	mov	$1,fw
2:
	mov	$60.,ll
	cmp	fw,$60.
	blo 9f; rts pc; 9:
	mov	$60.,r1
	clr	r0
	dvd	fw,r0
	mov	r0,r1
	mpy	fw,r1
	mov	r1,ll
	rts	pc
/
.data
fw:	1			/field width for digits
ll:	60.			/line length
.text
/
/
/	case k for skale factor
/
case153:
	jsr	pc,pop
	jes	eh
	mov	w(r1),r0
	sub	a(r1),r0
	cmp	r0,$1
	jhi	eh
	jsr	pc,rewind
	jsr	pc,getchar
	jmi	eh
	mov	r0,k
	mov	r0,r2
	jsr	pc,release
	mov	kptr,r1
	jsr	pc,create
	clr	r0
2:	cmp	r2,$2
	blo	2f
	jsr	pc,putchar
	sub	$2,r2
	br	2b
2:	mov	$1,r0
	cmp	r2,$1
	blo	2f
	mov	$10.,r0
2:	jsr	pc,putchar
1:	jmp	loop
/
/
/	case ^ for exponentiation
/
case136:
	jsr	pc,pop
	jes	eh
	mov	r1,r3
	jsr	pc,pop
	jes	eh
	mov	r1,r2
	jsr	pc,exp3
	jsr	pc,push
	mov	r2,r1
	jsr	pc,release
	mov	r3,r1
	jsr	pc,release
	jmp	loop
/
/
/	case v for square root
/
case166:
	jsr	pc,pop
	jes	eh
/
/	multiply argument by skale factor
/
	mov	r1,r2
	mov	kptr,r3
	jsr	pc,mul3
	mov	r1,r3
	mov	r2,r1
	jsr	pc,release
/
/	check for zero or negative
/
	mov	w(r3),r2
	sub	a(r3),r2
	tst	r2
	jeq	sqz
/
/	look at the top one or two digits
/
	mov	r3,r1
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,r4
	jmi	eh
	bit	$1,r2
	bne	2f
	mov	r4,r1
	mul	$100.,r1
	mov	r1,r4
	mov	r3,r1
	jsr	pc,backspace
	add	r0,r4
2:
/
/	allocate space for result
/
	inc	r2
	asr	r2
	mov	r2,r0
	jsr	pc,allocate
	jsr	pc,zero
	mov	r2,r0
	jsr	pc,seekchar
	mov	r1,r2
/
/	get high order digit of arg and square root it
/
	mov	$1,r0
2:	sub	r0,r4
	blt	2f
	add	$2,r0
	br	2b
2:	inc	r0
	asr	r0
	mov	r0,r4
	mov	r2,r1
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r4,r0
	jsr	pc,alterchar
	mov	r1,-(sp)
	mov	r3,-(sp)
/
/	get successive approx. from Newton
/
1:	mov	(sp),r3		/arg
	mov	2(sp),r2	/approx
	jsr	pc,div3
	mov	r1,r3
	jsr	pc,add3
	mov	r1,-(sp)
	mov	r3,r1
	jsr	pc,release
	mov	r4,r1
	jsr	pc,release
	mov	(sp)+,r1
	mov	sqtemp,r2
	mov	r1,r3
	jsr	pc,div3
	mov	r1,-(sp)
	mov	r3,r1
	jsr	pc,release
	mov	r4,r1
	jsr	pc,release
	mov	(sp)+,r3
	mov	2(sp),r1
	jsr	pc,length
	jsr	pc,allocate
	mov	2(sp),r0
	jsr	pc,move
	jsr	pc,chsign
	mov	r1,r2
	jsr	pc,add3
	jsr	pc,fsfile
	jsr	pc,backspace
	jsr	pc,release
	mov	r2,r1
	jsr	pc,release
	tst	r0
	bpl	2f
/
/	loop if new < old
/
	mov	2(sp),r1
	jsr	pc,release
	mov	r3,2(sp)
	br	1b
/
2:
	mov	r3,r1
	jsr	pc,release
	mov	2(sp),r1
	jsr	pc,push
	mov	(sp),r1
	jsr	pc,release
	tst	(sp)+
	tst	(sp)+
	jmp	loop
/
sqz:	clr	r0
	jsr	pc,allocate
	jsr	pc,push
	mov	r3,r1
	jsr	pc,release
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
	sys	exit
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
	jsr	pc,readc
	jsr	pc,putchar
1:
	jsr	pc,readc
	jsr	pc,putchar
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
	jsr	pc,in055	/go subtract
	jsr	pc,pop
	jsr	pc,length
	tst	r0
	beq	1f
	jsr	pc,fsfile
	jsr	pc,backspace
	tst	r0
	bmi	1f
	jsr	pc,release
	jsr	pc,in154	/load from register
	br	case170
/
1:
	jsr	pc,release
	jsr	pc,readc
	jmp	loop
/
/
/	case = for conditional execution
/
case075:
	jsr	pc,in055	/go subtract
	jsr	pc,pop
	jsr	pc,length
	tst	r0
	beq	1f	/is zero
	jsr	pc,release
	jsr	pc,readc
	jmp	loop
1:
	jsr	pc,release
	jsr	pc,in154	/load from register
	jmp	case170		/go to execute code
/
/
/	case > for conditional execution
/
case076:
	jsr	pc,in055	/go subtract
	jsr	pc,pop
	jsr	pc,length
	tst	r0
	beq	1f
	jsr	pc,fsfile
	jsr	pc,backspace
	tst	r0
	bpl	1f
	jsr	pc,release
	jsr	pc,in154	/load from register
	jmp	case170		/go to execute code
1:
	jsr	pc,release
	jsr	pc,readc
	jmp	loop
err:	4
/
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
	clr	r0
	jsr	pc,allocate
	mov	r1,-(sp)
	mov	strptr,r1
	jsr	pc,create
	jsr	pc,readc
1:
	cmpb	ch,$'0
	blt	1f
	cmpb	ch,$'9
	bgt	3f
	mov	ch,r0
	sub	$'0,r0
4:
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
	mov	ch,savec
	mov	(sp)+,r1
	rts	pc
/
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
	rts	pc
1:
	mov	r1,-(sp)
	mov	*readptr,r1
	jsr	pc,getchar
	bes	eof1
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
	mov	$1,r0
	sys	write; sphdr; 4
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
	mov	$stable-2,r0
1:
	tst	(r0)+
	cmp	r0,$stable+254.
	bhi	1f
/
	mov	(r0),r1
	beq	1b
	mov	r0,-(sp)
	sub	$stable,r0
	asr	r0
	movb	r0,7f+1
	mov	$1,r0
	sys	write; 7f; 8f-7f
	jsr	pc,printf
	mov	(sp)+,r0
	br	1b
1:
	mov	(sp)+,r5
	jmp	loop
/
.data
7:	<" " >
8:	.even
.text
/
/
/	routine to convert to decimal and print the
/	top element of the stack.
/
/	jsr	pc,printf
/
/
printf:
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
	br	prout
2:
	jsr	pc,fsfile
	jsr	pc,backspace
	bec	1f
	mov	$1,r0
	sys	write; blank; 1
	mov	$1,r0
	sys	write; asczero; 1
	br	prout
1:
	jsr	pc,length
	mov	r1,-(sp)
	jsr	pc,allocate
	mov	(sp),r0
	mov	r1,(sp)
	jsr	pc,move
	mov	ll,count
	inc	count
	jsr	pc,fsfile
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
	mov	$' ,ch
	jsr	pc,wrchar
1:
	mov	strptr,r1
	jsr	pc,create
	mov	basptr,r1
	jsr	pc,length
	cmp	r0,$1
	blo	dingout
	bne	1f
	jsr	pc,rewind
	jsr	pc,getchar
	cmp	r0,$1.
	beq	unout
	cmp	r0,$-1
	beq	dingout
	cmp	r0,$10.
	beq	tenout
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
/
/	cleanup, print new line and return
/
prout:	mov	$1,r0
	sys	write; nl; 1
	mov	(sp)+,r0
	mov	(sp)+,r1
	mov	(sp)+,r2
	rts	pc
/
/
tenout:
	mov	(sp),r1
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,r3
	clr	r2
	dvd	$10.,r2
	beq	1f
3:
	add	$60,r2
	mov	r2,ch
	jsr	pc,wrchar
1:
	add	$60,r3
	mov	r3,ch
	jsr	pc,wrchar
1:
	jsr	pc,backspace
	bec	2f
	mov	(sp)+,r1
	jsr	pc,release
	br	prout
2:
	mov	r0,r3
	clr	r2
	dvd	$10.,r2
	br	3b
/
/
dingout:
	clr	-(sp)
	br	1f
unout:
	mov	$1,-(sp)
1:
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
	br	prout
/
ding:	<>			/<bell prefix form feed>
blank:	< >
sp5:	<\n     >
minus:	<->
one:	<1>
	.even
.bss
count:	.=.+2
.text
/
bigout:
	mov	r1,-(sp)	/big digit
	mov	strptr,r1
	jsr	pc,length
	add	fw,r0
	dec	r0
	mov	r0,-(sp)	/end of field
	clr	-(sp)		/negative
	mov	4(sp),r1
	jsr	pc,length
	bne	2f
	mov	strptr,r1
	mov	$'0,r0
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
	mov	strptr,r1
	jsr	pc,putchar
	br	2b
1:
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
	mov	$' ,r0
	jsr	pc,putchar
	tst	(sp)+
	tst	(sp)+
	mov	(sp)+,r1
	jsr	pc,release
	rts	pc
/
.bss
tenptr:	.=.+2
.text
/
/
/
hexout:
	mov	r1,-(sp)
	jsr	pc,rewind
	jsr	pc,getchar
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
	sys	write; sp5; 6
	mov	ll,count
	mov	$1,r0
7:
	dec	count
	sys	write; ch; 1
	rts	pc
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
	4
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
errstack:.=.+2
/
stable:	.=.+256.
.text
casetab:
	case012; 012	/nl
	loop;    040	/sp
	case041; 041	/!
	case045; 045	/%
	case052; 052	/*
	case053; 053	/+
	case055; 055	/-
	junk;    056	/.
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
	case151; 111	/I
	case153; 113	/K
	case154; 114	/L
	case157; 157	/O
	case160; 120	/P
	case161; 121	/Q
	case163; 123	/S
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
