/ C library -- atof

/ a = atof(string);

/ returns a floating-point number in a_ corresponding to
/ the ascii string______.  See ATOF(III).

.globl	atof,_atof,a_tof,retrn,savr5

_atof:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),ptr
	jsr	r5,atof; a_tof
	clr	savr5
	jmp	retrn

a_tof:
	movb	*ptr,r0
	inc	ptr
	rts	r5

.bss
ptr:	.=.+2
