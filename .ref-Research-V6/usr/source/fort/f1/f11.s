/
/

/ f11 -- fortran command
/

.globl	pass1

.globl	fptrap
.globl	getline
.globl	flush
.globl	perror
.globl	lookup
.globl	getype
.globl	ssubr
.globl	sfunc
.globl	sbloc
.globl	sextr
.globl	sdime
.globl	scomm
.globl	sequi
.globl	sdata
.globl	sform
.globl	simpl
.globl	stype
.globl	isagn
.globl	ptemp
.globl	funok
.globl	signoff
.globl	fopen
.globl	fcreat
.globl	lookid
.globl	tfil1, tfil2
.globl	tfildiag

pass1:
	sys	signal; 4; fptrap
	cmp	(sp)+,$2
	bge	1f
2:
	mov	$1,r0
	sys	write; mes2; emes2-mes2
	clr	r0
	sys	seek; 0; 2
	mov	$-1,r0		/ bad status
	sys	exit
1:
	tst	(sp)+
	mov	(sp)+,r0
	jsr	r5,fopen; ibuf
	bcs	2b
	mov	$tfil1,r0
	jsr	r5,fcreat; obuf
	bcs	2f
	mov	$tfil2,r0
	jsr	r5,fcreat; tbuf
	bcc	1f
2:
	jmp	tfildiag
1:

/ data segment initialization

	mov	$2,nxtaloc
	mov	$errb,errp
	mov	$esymtab,esymp
	mov	$namebuf,namep
	movb	$'_,symbuf-1
	jsr	r5,lookid; blankc
	bis	$40,symtab(r3)
	mov	$imptab,r3
1:
	mov	$realcon,r0		/ real*4
	cmp	r3,$imptab+[2*['i-'a]]
	blo	2f
	cmp	r3,$imptab+[2*['n-'a]]
	bhi	2f
	mov	$intcon,r0		/ integer*4
2:
	mov	r0,[2*26.](r3)		/ lower case
	mov	r0,(r3)+		/ upper case
	cmp	r3,$imptab+[2*26.]
	blo	1b

/ call pass1

/	main scan loop for pass1
/	picks up non executable statements
/

scan1:
	jsr	r5,getline
	mov	$line,r1
	jsr	r5,isagn
		br 2f
	mov	r1,r2
	jsr	r5,lookup; stmtab
		br 1f
	mov	r2,r1
	jsr	r5,*sublst(r0)
	mov	pc,funok
	jsr	r5,perror
	br	scan1
1:
	jsr	r5,getype
		br 2f
	mov	r2,r1
	jsr	r5,stype
	jsr	r5,perror
	mov	pc,funok
	br	scan1
2:
	jsr	r5,ptemp; 'l; efno; line
	jsr	r5,flush; tbuf
	jsr	r5,signoff; 1

sublst:
	ssubr
	sfunc
	sbloc
	sextr
	sdime
	scomm
	sequi
	sdata
	sform
	simpl

stmtab:
	<subroutine\0>
	<function\0>
	<blockdata\0>
	<external\0>
	<dimension\0>
	<common\0>
	<equivalence\0>
	<data\0>
	<format\0>
	<implicit\0>
	<\0>
	.even


mes2:
	<Input file?\n>
emes2:
blankc:
	<_\n\0>
	.even

