.globl	ctime., retrn, temp
ctime.:	temp
	.+2
	sys	time
	clr	temp
	mov	r0,temp+2
	setd
	setl
	movif	temp,fr0
	mulf	$44200,fr0
	mov	r1,temp+2
	movif	temp,fr1
	addf	fr1,fr0
	movf	fr0,temp
	jmp	retrn
