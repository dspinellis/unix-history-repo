/ Fortran library-- setfil

/ call setfil(unit, 5hname )

.globl	setfil.

.globl	getbuf
.globl	chkunit
.globl	retrn
.globl	utable
.globl	temp
.globl	rerr

setfil.:
	temp
	.+2
	mov	2(r3),r1
	mov	2(r1),r1
	jsr	r5,chkunit
	tstb	utable(r1)
	beq	1f
	jsr	r5,rerr; 112.
1:
	jsr	r5,getbuf
	movb	$-1,utable(r1)
	mov	4(r3),r0
	mov	r2,r1
1:
	movb	(r0)+,(r1)
	beq	1f
	cmpb	$' ,(r1)+
	bne	1b
	clrb	-(r1)
1:
	jmp	retrn
