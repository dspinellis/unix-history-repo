/ ar -- archive/library

	mov	(sp)+,r0
	sub	$2,r0
	ble	userr
	tst	(sp)+
	mov	(sp)+,r1
	clr	r2
1:
	tstb	(r1)
	beq	1f
	cmpb	(r1),$'v
	bne	2f
	inc	r1
	incb	vflg
	br	1b
2:
	tst	r2
	bne	userr
	movb	(r1)+,r2
	br	1b
1:
	tst	r2
	beq	userr
	mov	$arglst,r1
1:
	mov	(sp)+,(r1)+
	dec	r0
	bgt	1b
	clr	(r1)+
	mov	$swlst,r1
1:
	cmp	r2,(r1)+
	beq	1f
	tst	(r1)+
	bne	1b
	br	userr
1:
	jmp	*(r1)

swlst:
	'r; comr
	'u; comu
	'd; comd
	'x; comx
	't; comt
	 0; 0

userr:
	jsr	r5,diag
		<bad usage\n\0>
	.even

putc:
	movb	r0,ch
	mov	$1,r0
	sys	write; ch; 1
	rts	r5

print:
	movb	(r1)+,r0
	beq	1f
	jsr	r5,putc
	br	print
1:
	rts	r5

diag:
	mov	r5,r1
	jsr	r5,print
	tst	tfo
	beq	1f
	sys	unlink; tfil
1:
	sys	exit

getaf:
	mov	arglst,0f
	sys	open; 0:..; 0
	bes	1f
	mov	r0,afi
	sys	read; buf; 2
	cmp	buf,magic
	bne	magerr
	tst	(r5)+
1:
	rts	r5

magerr:
	mov	arglst,r1
	jsr	r5,print
	jsr	r5,diag
		< -- not in archive format\n\0>
	.even

mktmp:
	sys	stat; tfil; buf
	bes	1f
	incb	tfil+8
	cmpb	tfil+8,$'z
	blo	mktmp
	br	tferr
1:
	sys	intr; done
	sys	creat; tfil; 14
	bes	tferr
	mov	r0,tfo
	sys	open; tfil; 0
	bes	tferr
	mov	r0,tfi
	rts	r5

tferr:
	jsr	r5,diag
		<cannot open temp file\n\0>
	.even

getdir:
	mov	afi,r0
	sys	read; dir; 16.
	cmp	r0,$16.
	bne	1f
	jsr	r5,mvname
	tst	(r5)+
1:
	rts	r5

mvname:
	mov	name,rname
	mov	name+2,rname+2
	mov	name+4,rname+4
	mov	name+6,rname+6
	rts	r5

skip:
	mov	size,r0
	inc	r0
	bic	$1,r0
	mov	r0,0f
	mov	afi,r0
	sys	seek; 0:..; 1
	rts	r5

trim:
	mov	r0,r2
1:
	tstb	(r0)
	beq	1f
	cmpb	(r0)+,$'/
	beq	trim
	br	1b
1:
	rts	r5

match:
	mov	$arglst+2,r1
1:
	mov	(r1)+,r0
	beq	1f
	blt	1b
	jsr	r5,trim
	mov	$name,r0
2:
	cmp	r0,$name+8.
	beq	2f
	cmpb	(r0),(r2)+
	bne	1b
	tstb	(r0)+
	bne	2b
2:
	cmp	(r5)+,-(r1)
1:
	rts	r5

mvfil:
	mov	(r1),9f
	mov	(r1),0f
	sys	stat; 0:..; buf
	bes	operr
	sys	open; 9:..; 0
	bes	operr
	mov	r0,fio
	mov	(r1),r0
	mov	$-1,(r1)
	jsr	r5,trim
	mov	$name,r0
1:
	cmp	r0,$name+8.
	beq	1f
	movb	(r2)+,(r0)+
	bne	1b
1:
	mov	buf+28.,mtim
	mov	buf+30.,mtim+2
	movb	buf+5,ouid
	movb	buf+2,mode
	mov	buf+6,size
	mov	tfo,r0
	sys	write; dir; 16.
	mov	size,r2
1:
	mov	fio,r0
	sys	read; buf; 512.
	sub	r0,r2
	mov	r0,0f
	beq	1f
	mov	tfo,r0
	sys	write; buf; 0:..
	br	1b
1:
	tst	r2
	bne	phserr
	bit	$1,size
	beq	1f
	mov	tfo,r0
	sys	seek; 1; 1
1:
	mov	fio,r0
	sys	close
	jsr	r5,mvname
	rts	r5

operr:
	mov	9b,r1
	jsr	r5,print
	jsr	r5,diag
		< -- cannot open\n\0>
	.even

phserr:
	mov	9b,r1
	jsr	r5,print
	jsr	r5,diag
		< -- phase error\n\0>
	.even

copyfl:
	mov	tfo,r0
	sys	write; dir; 16.
	mov	size,r1
	mov	$rname,9b
1:
	mov	r1,0f
	beq	1f
	cmp	r1,$512.
	blo	2f
	mov	$512.,0f
2:
	mov	afi,r0
	sys	read; buf; 0:..
	sub	r0,r1
	mov	r0,0f
	beq	phserr
	mov	tfo,r0
	sys	write; buf; 0:..
	br	1b
1:
	bit	$1,size
	beq	1f
	mov	afi,r0
	sys	seek; 1; 1
	mov	tfo,r0
	sys	seek; 1; 1
1:
	rts	r5

xtract:
	movb	mode,0f
	sys	creat; rname; 0:..
	bes	noxerr
	mov	r0,fio
	mov	size,r1
	mov	$rname,9b
1:
	mov	r1,0f
	beq	1f
	cmp	r1,$512.
	blo	2f
	mov	$512.,0f
2:
	mov	afi,r0
	sys	read; buf; 0:..
	sub	r0,r1
	mov	r0,0f
	beq	phserr
	mov	fio,r0
	sys	write; buf; 0:..
	br	1b
1:
	mov	fio,r0
	sys	close
	bit	$1,size
	beq	1f
	mov	afi,r0
	sys	seek; 1; 1
1:
	mov	r0,-(sp)
	mov	r1,-(sp)
	mov	mtim+2,r1
	mov	mtim,r0
	sys	mdate
	mov	(sp)+,r1
	mov	(sp)+,r1
	rts	r5

noxerr:
	mov	$rname,r1
	jsr	r5,print
	jsr	r5,diag
		< -- cannot create\n\0>
	.even

table:
	mov	$rname,r1
	jsr	r5,print
	mov	$'\n,r0
	jsr	r5,putc
	rts	r5

mesg:
	mov	r1,-(sp)
	mov	(r5)+,r0
	tstb	vflg
	beq	1f
	jsr	r5,putc
	mov	$' ,r0
	jsr	r5,putc
	mov	$rname,r1
	jsr	r5,print
	mov	$'\n,r0
	jsr	r5,putc
1:
	mov	(sp)+,r1
	rts	r5

oldnew:
	sys	stat; rname; buf
	bes	1f
	cmp	buf+28.,mtim
	blo	1f
	bhi	2f
	cmp	buf+30.,mtim+2
	blos	1f
2:
	tst	(r5)+
	mov	$rname,tname
	mov	$tname,r1
1:
	rts	r5

comr:
	jsr	r5,mktmp
	jsr	r5,getaf
		br copfl
1:
	jsr	r5,getdir
		br copfl
	jsr	r5,match
		br 2f
	jsr	r5,mesg; 'r
	jsr	r5,skip
	jsr	r5,mvfil
	br	1b
2:
	jsr	r5,copyfl
	jsr	r5,mesg; 'c
	br	1b

comu:
	jsr	r5,mktmp
	jsr	r5,getaf
		br noaf
1:
	jsr	r5,getdir
		br copfl
	tst	arglst+2
	beq	2f
	jsr	r5,match
		br 3f
	mov	$-1,(r1)
2:
	jsr	r5,oldnew
		br 3f
	jsr	r5,mesg; 'r
	jsr	r5,skip
	jsr	r5,mvfil
	br	1b
3:
	jsr	r5,copyfl
	jsr	r5,mesg; 'c
	br	1b

comd:
	jsr	r5,mktmp
	jsr	r5,getaf
		br noaf
1:
	jsr	r5,getdir
		br 1f
	jsr	r5,match
		br 2f
	mov	$-1,(r1)
	jsr	r5,skip
	jsr	r5,mesg; 'd
	br	1b
2:
	jsr	r5,copyfl
	jsr	r5,mesg; 'c
	br	1b
1:
	jsr	r5,nfound
	br	copfl

noaf:
	jsr	r5,diag
		<no archive file\n\0>
	.even

crterr:
	jsr	r5,diag
		<cannot create archive file\n\0>
	.even

copfl:
	mov	$arglst,r1
	mov	(r1)+,0f
1:
	tst	(r1)+
	beq	1f
	blt	1b
	tst	-(r1)
	jsr	r5,mvfil
	jsr	r5,mesg; 'a
	br	1b
1:
	sys	intr; 0 / no interrups during copy back
	sys	creat; 0:..; 17
	bes	crterr
	mov	r0,afo
	sys	write; magic; 2
1:
	mov	tfi,r0
	sys	read; buf; 512.
	mov	r0,0f
	beq	done
	mov	afo,r0
	sys	write; buf; 0:..
	br	1b

done:
	jsr	r5,diag
		<\0>
	.even

comx:
	jsr	r5,getaf
		br noaf
1:
	jsr	r5,getdir
		br 1f
	tst	arglst+2
	beq	3f
	jsr	r5,match
		br 2f
3:
	mov	$-1,(r1)
	jsr	r5,xtract
	jsr	r5,mesg; 'x
	br	1b
2:
	jsr	r5,skip
	br	1b
1:
	jsr	r5,nfound
	br	done

comt:
	jsr	r5,getaf
		br noaf
1:
	jsr	r5,getdir
		br 1f
	tst	arglst+2
	beq	2f
	jsr	r5,match
		br 3f
	mov	$-1,(r1)
2:
	jsr	r5,table
3:
	jsr	r5,skip
	br	1b
1:
	jsr	r5,nfound
	br	done

nfound:
	mov	$arglst+2,r2
1:
	mov	(r2)+,r1
	beq	1f
	blt	1b
	mov	$-1,-(r2)
	jsr	r5,print
	mov	$notfnd,r1
	jsr	r5,print
	br	1b
1:
	rts	r5

notfnd:
	< -- not found\n\0>
	.even

tfil:	</tmp/vtma\0>
	.even
magic:	-147.

	.bss

afi:	.=.+2
afo:	.=.+2
tfi:	.=.+2
tfo:	.=.+2
fio:	.=.+2
rname:	.=.+9.
ch:	.=.+1
vflg:	.=.+1
	.even
tname:	.=.+2
dir:
	name: .=.+8.
	mtim: .=.+4
	ouid: .=.+1
	mode: .=.+1
	size: .=.+2
arglst:	.=.+200.
buf:	.=.+512.

-1,(r1)
2:
	jsr	r5,table
3:
	jsr	r5,skip
	br	1b
1:
	jsr	r5,nfound
	br	done

nfound:
	mov	$arglst+2,r2
1:
	mov	(r2)+,r1
	beq	1f
	blt	1b
	mov	$-1,-(r2)
	jsr	r5,print
	mov	$notfnd,r1
	jsr	r5,print
	br	1b
1:
	rts	r5

notfnd:
	< -- not found\n\0>
	.even

tfil:	</tmp/vtma\0>
	.even
magic:	-147.

	.bss

afi:	.=.+2
afo:	.=.+2
tfi:	.=.+2
tfo:	.=.+2
fio:	.=.+2
rname:	.=.+9.
ch:	.=.+1
vflg:	.=.+1
	.even
tname:	.=.+2
dir:
	name: .=.+8.
	mtim: .=.+4
	ouid: .=.+1
	mode.globl	getchar
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
	sys	open; 0:.=.+2; 0
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
	sys	intr; case177
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
delflag: .=.+2
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
	bne 9f; jmp eh; 9:
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
	bec 9f; jmp loop; 9:
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
	blo 9f; jmp err; 9:
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
	blo 9f; jmp err; 9:
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
	bec 9f; jmp eh; 9:
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
	bec 9f; jmp eh; 9:
	mov	r1,r2
	jsr	pc,pop
	bec 9f; jmp eh; 9:
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
	bec 9f; jmp eh; 9:
	mov	r1,-(sp)
	mov	inbas,r1
	mov	(sp)+,inbas
	jsr	pc,release
	rts	pc
/
inbas:	.=.+2
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
	bec 9f; jmp eh; 9:
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
fw:	1			/field width for digits
ll:	60.			/line length
/
/
/	case k for skale factor
/
case153:
	jsr	pc,pop
	bec 9f; jmp eh; 9:
	mov	w(r1),r0
	sub	a(r1),r0
	cmp	r0,$1
	blos 9f; jmp eh; 9:
	jsr	pc,rewind
	jsr	pc,getchar
	bpl 9f; jmp eh; 9:
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
	bec 9f; jmp eh; 9:
	mov	r1,r3
	jsr	pc,pop
	bec 9f; jmp eh; 9:
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
	bec	9f; jmp eh; 9:
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
	bne 9f; jmp sqz; 9:
/
/	look at the top one or two digits
/
	mov	r3,r1
	jsr	pc,fsfile
	jsr	pc,backspace
	mov	r0,r4
	bpl 9f; jmp eh; 9:
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
sqtemp:	.=.+2
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
	bec 9f; jmp eh; 9:
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
1:	<(  ) ?\n>
2:	.even
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
	bgt	1f
	mov	ch,r0
	sub	$'0,r0
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
	bne	9f; jmp eh; 9:
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
7:	<" " >
8:	.even
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
	sys	write; 3:.=.+2; 0:.=.+2
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
	beq 9f; jmp in177; 9:
	sys	write; ding; 3
	br	1b
1:
	tst	(sp)+
	mov	(sp)+,r1
	jsr	pc,release
	br	prout
/
ding:	<	>			/<bell prefix tab>
blank:	< >
sp5:	<\n     >
minus:	<->
one:	<1>
	.even
count:	.=.+2
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
tenptr:	.=.+2
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
	beq 9f; jmp in177; 9:
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
1:	<0 not in switch.\n>
2:	.even
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
outdit:	hexout
source: .=.+2
savec:	.=.+2
ch:	.=.+2
nl:	<\n>
asczero:	<0>
qm:	<?\n>
	.even
/
chptr:	.=.+2
strptr:	.=.+2
basptr:	.=.+2
errstack:.=.+2
/
	.bss
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
	case143; 103	/C
	case144; 104	/D
	case146; 106	/F
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
4; 104	/D
	case146; 106	/F
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