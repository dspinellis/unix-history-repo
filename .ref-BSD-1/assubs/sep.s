	.globl	_sep
_sep:
	clr	r0
	sys	61.
	cmp	r0,0
	bne	succeed
	inc	0
	clr	r0
	sys	61.
	cmp	r0,0
	beq	fail
	dec	0
succeed:
	mov	$1,r0
	rts	pc
fail:
	dec	0
	clr	r0
	rts	pc
