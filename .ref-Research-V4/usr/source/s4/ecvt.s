/ C interface to ecvt & fcvt

.globl	_ecvt, _fcvt
.globl	ecvt, fcvt, _ndigit
.globl	savr5, retrn

_ecvt:
	jsr	r5,setup
	jsr	pc,ecvt
	br	1f

_fcvt:
	jsr	r5,setup
	jsr	pc,fcvt
1:
	mov	savr5,r5
	clr	savr5
	mov	r2,*16(r5)
	mov	r1,*20(r5)
	jmp	retrn

setup:
	mov	sp,savr5
	movf	4(sp),fr0
	mov	14(sp),_ndigit
	jmp	(r5)

