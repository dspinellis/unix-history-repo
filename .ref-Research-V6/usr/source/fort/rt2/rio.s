/
/

/ fortran random I/O

.globl	rread.
.globl	rwrite.

.globl	temp
.globl	retrn
.globl	rerr

rread.:	temp
	.+2
	mov	2(r3),r0
	mov	2(r0),0f
	mov	rfin,r0
	bne	1f
	sys	open; rf; 0
	bes	9f
	mov	r0,rfin
1:
	sys	seek; 0:..; 0
	mov	4(r3),0f
	mov	6(r3),r0
	mov	2(r0),0f+2
	mov	rfin,r0
	sys	read; 0:..; 0
	jmp	retrn

rwrite.:temp
	.+2
	mov	2(r3),r0
	mov	2(r0),0f
	mov	rfout,r0
	bne	1f
	sys	creat; rf; 17
	bes	9f
	mov	r0,rfout
1:
	sys	seek; 0:..; 0
	mov	4(r3),0f
	mov	6(r3),r0
	mov	2(r0),0f+2
	mov	rfout,r0
	sys	write; 0:..; ..
	jmp	retrn

9:
	jsr	r5,rerr; 456.
	sys	exit

rf:
	<fortrf\0>
	.even
rfin:	.=.+2
rfout:	.=.+2

