.globl putch,obuild
.globl putoct


putoct:	mov	r0,-(sp)
	bic	$7,r0
	bic	r0,(sp)
	clc
	ror	r0
	ror	r0
	ror	r0
	beq	1f
	jsr	pc,putoct
1:
	mov	(sp)+,r0
	add	$'0,r0
	jsr	pc,putch
	rts	pc
