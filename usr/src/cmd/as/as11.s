/
/

/ PDP-11 assembler pass 0

	jmp	start
go:
	jsr	pc,assem
	movb	pof,r0
	sys	write; outbuf; 512.
	jes	wrterr
	movb	pof,r0
	sys	close
	movb	fbfil,r0
	sys	close
	tstb	errflg
	bne	aexit
	jsr	r5,fcreat; a.tmp3
	mov	r0,r1
	mov	symend,0f
	sub	$usymtab,0f
	sys	indir; 9f
	jes	wrterr
	.data
9:	sys	write; usymtab; 0:..
	.text
	mov	r1,r0
	sys	close
	sys	exec; fpass2; 1f
	mov	$fpass2,r0
	jsr	r5,filerr; "?\n

aexit:
	sys	unlink; a.tmp1
	sys	unlink; a.tmp2
	sys	unlink; a.tmp3
	mov	$3,r0
	sys	exit
.data
1:
	fpass2
	globfl
	outfl
outfp:
	outfile
	a.tmp1
	a.tmp2
	a.tmp3
	0
fpass2:
	</lib/as2\0>
globfl:
unglob=.+1
	<-\0\0>
outfl:
	<-o\0>
outfile:
	<a.out\0>
3:
	<-g\0>
	.even
	.text

filerr:
	mov	r4,-(sp)
	mov	r0,r4
	mov	r4,0f
	clr	r0
1:
	tstb	(r4)+
	beq	1f
	inc	r0
	br	1b
1:
	mov	r0,1f
	mov	$1,r0
	sys	indir; 9f
	.data
9:	sys	write; 0:0; 1:0
	.text
	mov	r5,0f
	mov	$1,0f+2
	tstb	1(r5)
	beq	1f
	mov	$2,0f+2
1:
	mov	$1,r0
	sys	indir; 9f
	.data
9:	sys	write; 0:0; 1
	.text
	tst	(r5)+
	mov	(sp)+,r4
	rts	r5

fcreat:
	mov	r4,-(sp)
	mov	(r5)+,r4
	mov	r4,0f
1:
	sys	indir; 9f
	.data
9:	sys	stat; 0:..; outbuf
	.text
	bec	2f
	mov	r4,0f
	sys	indir; 9f
	.data
9:	sys	creat; 0:..; 444
	.text
	bes	2f
	mov	(sp)+,r4
	rts	r5
2:
	incb	9.(r4)
	cmpb	9.(r4),$'z
	blos	1b
	mov	r4,r0
	jsr	r5,filerr; "?\n
	mov	$3,r0
	sys	exit

.=.+2
wrterr:
	mov	$1,r0
	sys	write; 9f; 9f-8f
	inc	errflg
	jbr	aexit
9:	<as: Write error on temp file.\n>; 8:
