/
/
ed:
	mov	(sp)+,r3
	ble	error
econ:
	sys	stat; ftemp; buffer
	bec	2f
	sys	creat; ftemp; 666
	mov	r0,tmpw
	sys	open; ftemp; 0
	mov	r0,tmpr
	br	1f
2:
	cmpb	$'z,ftemp+9.
	bne	2f
	mov	$1,r0
	sys	write; erm; 23.
	jmp	loop
2:
	incb	ftemp+9.
	br	econ
1:
	mov	(sp),r0
	jsr	pc,getspq
	bes	3f
	mov	r0,r4
	mov	r2,r1
	jsr	pc,rewind
	mov	$buffer,r2
1:
	jsr	pc,getchar
	bes	1f
	movb	r0,(r2)+
	br	1b
1:
	movb	$'\n,(r2)+
	jsr	pc,output
	mov	r4,r1
	jsr	pc,rewind
2:
	mov	$buffer,r2
1:
	jsr	pc,getchar
	bes	1f
	movb	r0,(r2)+
	cmp	r2,$bufend
	blo	1b
	mov	tmpw,r0
	sys	write; buffer; 512.
	br	2b
1:
	sub	$buffer,r2
	beq	4f
	mov	r2,0f
	mov	tmpw,r0
	sys	write; buffer; 0:..
4:
	tst	iflag
	beq	1f
	mov	tmpw,r0
	sys	close
	mov	tmpr,r0
	sys	close
	sys	unlink; ftemp
	jmp	loop
1:
	mov	tmpw,r0
	sys	close
	sys	fork
	br	5f
	mov	r1,-(sp)
	sys	wait
	sys	signal; 2; inter
	mov	(sp)+,r1
	mov	a(r1),w(r1)
2:
	mov	tmpr,r0
	sys	read; buffer; 512.
	tst	r0
	beq	2f
	add	$buffer,r0
	mov	r0,bufend
	mov	$buffer,r2
1:
	movb	(r2)+,r0
	jsr	pc,putchar
	cmp	r2,bufend
	blo	1b
	br	2b
2:
	mov	tmpr,r0
	sys	close
	sys	unlink; ftemp
	tst	iflag
	beq	1f
	jmp	loop
1:
	tst	qflag
	bgt	1f
	tst	(sp)+
	dec	r3
	bgt	1f
	jmp	loop
1:	jmp	econ
	jmp	loop
3:
	mov	(sp),r2
	mov	$2,r0
	jsr	pc,allocate
1:
	tstb	(r2)
	beq	1f
	movb	(r2)+,r0
	jsr	pc,putchar
	br	1b
1:
	mov	r1,r0
	mov	asmem,r1
	jsr	pc,putword
	mov	$2,r0
	jsr	pc,allocate
	mov	r1,r0
	mov	asmem,r1
	jsr	pc,putword
	mov	r0,r1
	br	4b
5:
	mov	$2,r0
1:
	sys	close
	inc	r0
	cmp	r0,$10.
	blo	1b
	sys	exec; edd; eda
	sys	exit
eda:	edd; ftemp; 0
edd:	</bin/ed\0>
	.even
ftemp:	</tmp/ftmpa\0>
erm:	<Cannot open temp. file\n>
	.even
tmpw:	.=.+2
tmpr:	.=.+2
/
/	remove a 'file' from memory
/
remove:
	mov	(sp)+,r3
	bgt	5f
	mov	$1,vflag
	mov	asmem,r1
	jsr	pc,rewind
	mov	r(r1),-(sp)
4:
	tst	iflag
	beq	1f
	jmp	loop
1:
	jsr	pc,getword
	bes	4f
	mov	r0,r2
	jsr	pc,ask
	bec	1f
	mov	r(r1),(sp)
	br	4b
1:
	jsr	pc,getword
	br	2f
5:
	mov	(sp),r0
	jsr	pc,getspq
	bec	2f
	tst	qflag
	blt	6f
	mov	(sp),r1
	jsr	pc,nothere
	br	3f
2:
	mov	r0,r1
	jsr	pc,release
	mov	r2,r1
	jsr	pc,release
	jsr	pc,rm
3:
	tst	vflag
	bne	3f
	tst	iflag
	beq	1f
	jmp	loop
1:
	tst	qflag
	bgt	5b
6:
	tst	(sp)+
	dec	r3
	bgt	5b
	jmp	loop
3:
	mov	asmem,r1
	mov	(sp),r(r1)
	br	4b
4:
	tst	(sp)+
	jmp	loop
/
/	rename a 'file'
/
rename:
	mov	(sp)+,r3
	bne	1f
	jmp	error
1:
	sub	$2,r3
	blt	1f
5:
	mov	(sp),r0
	jsr	pc,getsp
	bes	4f
	tst	(sp)+
	sub	$4,r(r1)
	mov	(sp),r0
	mov	r2,-(sp)
	mov	r(r1),-(sp)
	jsr	pc,getsp
	bes	3f
	mov	r2,-(sp)
	mov	r0,r1
	jsr	pc,release
	jsr	pc,rm
	mov	(sp)+,r0
	mov	(sp)+,r(r1)
	jsr	pc,alterword
	mov	(sp)+,r1
	jsr	pc,release
	br	5f
3:
	mov	(sp)+,r(r1)
	mov	(sp)+,r1
	jsr	pc,release
	mov	$2,r0
	jsr	pc,allocate
	mov	(sp)+,r2
2:
	movb	(r2)+,r0
	beq	2f
	jsr	pc,putchar
	br	2b
2:
	mov	r1,r0
	mov	asmem,r1
	jsr	pc,alterword
5:
	tst	iflag
	beq	2f
	jmp	loop
2:
	tst	r3
	beq	2f
	sub	$2,r3
	bge	5b
1:
	tst	(sp)+
	jmp	error
2:
	jmp	loop
4:
	mov	(sp)+,r1
	jsr	pc,nothere
	tst	(sp)+
	br	5b
/
/	list contents of asmem
/
list:
	mov	$buffer,r2
	movb	$'\n,(r2)+
	jsr	pc,output
	mov	(sp)+,r3
	beq	1f
	mov	$1,vflag
5:
	tst	iflag
	beq	2f
	jmp	loop
2:
	mov	(sp),r0
	jsr	pc,getspq
	bes	4f
	mov	r2,r1
	br	3f
1:
	mov	asmem,r1
	jsr	pc,fsfile
	sub	$2,r(r1)
2:
	tst	iflag
	bne	2f
	jsr	pc,backword
	bes	2f
	mov	r0,r1
3:
	jsr	pc,getn
	movb	$'\n,(r2)+
	jsr	pc,output
	tst	vflag
	bne	1f
	mov	asmem,r1
	sub	$2,r(r1)
	br	2b
1:
	tst	qflag
	bgt	5b
4:
	tst	(sp)+
	dec	r3
	bgt	5b
2:
	mov	$buffer,r2
	movb	$'\n,(r2)+
	jsr	pc,output
	jmp	loop
/
/	list a 'file
/
listf:
	mov	(sp)+,r3
	bgt	4f
	jmp	error
4:
	mov	(sp),r0
	jsr	pc,getspq
	bes	3f
	mov	r0,r4
	mov	r2,r1
	jsr	pc,rewind
	mov	$buffer,r2
	movb	$'\n,(r2)+
1:
	jsr	pc,getchar
	bes	1f
	movb	r0,(r2)+
	cmp	r2,$bufend
	blo	1b
	jsr	pc,output
	br	1b
1:
	mov	r4,r1
	jsr	pc,rewind
	movb	$':,(r2)+
	cmp	r2,$bufend
	blo	2f
	jsr	pc,output
2:
	movb	$'\n,(r2)+
1:
	tst	iflag
	beq	2f
	jmp	loop
2:
	cmp	r2,$bufend
	blo	2f
	jsr	pc,output
2:
	jsr	pc,getchar
	bes	1f
	movb	r0,(r2)+
	br	1b
1:
	jsr	pc,output
3:
	tst	qflag
	bgt	4b
	tst	(sp)+
	dec	r3
	bgt	4b
	mov	$buffer,r2
	movb	$'\n,(r2)+
	jsr	pc,output
	jmp	loop
/
fin:
	jsr	pc,flush
	jsr	pc,whead
	4
q:
	jsr	pc,flush
	jsr	pc,whead
	sys	exit
memck:	mov	(sp)+,r3
2:
	ble	2f
	mov	(sp)+,r1
	cmpb	(r1),$'p
	bne	1f
	bis	$1,vflag
	br	3f
1:
	cmpb	(r1),$'f
	bne	3f
	bis	$2,vflag
3:
	dec	r3
	br	2b
2:
	clr	freeh
	mov	$freeb,r2
1:
	clr	(r2)+
	cmp	r2,$freen
	blo	1b
	mov	$headers,r2
2:
	cmp	l(r2),$hsz
	beq	6f
	jsr	pc,ck
	bec	1f
	cmp	r2,asmem
	beq	1f
	mov	asmem,r1
	jsr	pc,rewind
	clr	r3
4:
	inc	r3
	jsr	pc,getword
	bes	5f
	cmp	r0,r2
	bne	4b
6:
	inc	freeh
1:
cont:
	add	$8.,r2
	cmp	r2,$headend-4
	blo	2b
	mov	asmem,r1
	jsr	pc,rewind
4:
	jsr	pc,getword
	bes	4f
	tst	r(r0)
	beq	2f
	mov	r0,r2
	jsr	pc,ck
	bes	4b
2:
	sub	$hblk,r0
	jsr	r5,oct
	mov	$1,r0
	sys	write; re; 24.
	cmp	vflag,$1
	beq	4b
	bit	$1,r3
	beq	2f
	jsr	pc,getword
	mov	r0,r1
	jsr	pc,release
	br	3f
2:
	jsr	pc,backword
	add	$4,r(r1)
	mov	r0,r1
	jsr	pc,release
3:
	jsr	pc,rm
	clr	r3
	br	4b
4:
	mov	freeh,r0
	jsr	r5,decml
	mov	$1,r0
	sys	write; frh; 14.
	mov	$freeb,r2
	mov	$1,r3
6:
	cmp	r2,$freen
	bhis	6f
	mov	(r2)+,r0
	beq	3f
	jsr	r5,decml
	mov	$1,r0
	sys	write; frb; 18.
	mov	r3,r0
	jsr	r5,decml
	mov	$1,r0
	sys	write; lf; 1.
3:
	asl	r3
	br	6b
6:
	mov	$1,r0
	sys	write; lf; 1.
	jmp	loop
5:
	mov	r2,r0
	sub	$hblk,r0
	jsr	r5,oct
	mov	$1,r0
	sys	write; un; 26.
	tst	vflag
	beq	1b
	mov	r2,r1
	cmp	vflag,$2
	beq	3f
	jsr	pc,rewind
	mov	$buffer,r2
2:
	jsr	pc,getchar
	bes	2f
	movb	r0,(r2)+
	cmp	r2,$buffer+80.
	blo	2b
	jsr	pc,output
	br	2b
2:
	movb	$'\n,(r2)+
	jsr	pc,output
	mov	r1,r2
	cmp	vflag,$1
	bne	3f
	jmp	cont
3:
	jsr	pc,release
	jmp	cont
/
interrupt: 4
/
un:	< header not accounted for\n>
re:	< part of asmem released\n>
lf:	<\n>
frh:	< free headers\n >
frb:	< free blocks size >
endc:	.even
