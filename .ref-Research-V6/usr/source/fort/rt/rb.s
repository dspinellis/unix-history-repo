/
/

/ rb -- funct/subr stuff

.globl	call
.globl	callp
.globl	retrn
.globl	stsp

callp:
	mov	(r4)+,r0
	add	r3,r0
	mov	(r0),r0
	br	1f

call:
	mov	(r4)+,r0
1:
	mov	r3,-(sp)
	mov	(r4)+,r3
	mov	r4,-(sp)
	mov	r0,r4
	mov	(r4)+,-(sp)		/ lv of funct
	jmp	*(r4)+

retrn:
	mov	r3,r0
	mov	(sp)+,r1		/ lv of funct
	mov	(sp)+,r4
	mov	(sp)+,r3
	mov	(r0),sp
	tst	(r4)+		/ arg count
	mov	(r4)+,r0		/ return byte count
	inc	r0
	bic	$1,r0
	add	r0,r1
1:
	sub	$2,r0
	blt	1f
	mov	-(r1),-(sp)
	br	1b
1:
	jmp	*(r4)+

stsp:
	mov	sp,*(r4)+
	jmp	*(r4)+

