.globl	_srand, _rand, retrn
.globl	rand, srand

_srand:
	mov	r5,-(sp)
	mov	sp,r5
	mov	4(r5),r0
	jsr	pc,srand
	jmp	retrn

_rand:
	mov	r5,-(sp)
	mov	sp,r5
	jsr	pc,rand
	jmp	retrn

