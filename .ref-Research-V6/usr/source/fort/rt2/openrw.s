/
/

/ openr, openw--
/
/ call open(r|w) (string, iunit)
/   or,
/ call open(r|w) (argno, iunit)
/ where argno is an argument number (0 is command name)

.globl	openr.
.globl	openw.

.globl	utable
.globl	btable
.globl	ftable
.globl	getbuf
.globl	fopen
.globl	fcreat

openr.:	temp
	.+2
	mov	pc,-(sp)
	br	1f

openw.:	temp
	.+2
	clr	-(sp)
1:
	mov	4(r3),r1
	mov	2(r1),r1
	jsr	r5,chkunit
	tstb	utable(r1)
	beq	1f
	jsr	r5,rerr; 101.
	sys	exit
1:
	jsr	r5,getbuf
	mov	2(r3),r0
	mov	2(r0),r0
	cmp	r0,$32.
	bhi	1f
	cmp	r0,*argp
	bhi	err
	asl	r0
	add	argp,r0
	br	2f
1:
	tst	(sp)+
	beq	1f
	movb	$1,utable(r1)
