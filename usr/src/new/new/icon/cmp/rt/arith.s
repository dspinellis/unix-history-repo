#include "../h/config.h"
#ifdef VAX
	# Long integer arithmetic routines with overflow checking

	.globl	_runerr

	.text
  	.align	1
	.globl	_ckadd
_ckadd:	.word	0
	addl3	4(ap),8(ap),r0
	jvs	oflow
	ret

	.align	1
	.globl	_cksub
_cksub:.word	0
	subl3	8(ap),4(ap),r0
	jvs	oflow
	ret

	.align	1
	.globl	_ckmul
_ckmul:	.word	0
	mull3	4(ap),8(ap),r0
	jvs	oflow
	ret

oflow:	pushl   $0
	pushl	$203
	calls	$1,_runerr

#endif VAX

#ifdef PDP11
/ Long integer arithmetic routines with overflow checking

.globl	_runerr
.globl	csv
.globl	cret

.text
.globl	_ckadd
_ckadd:
	mov	r5,-(sp)
	mov	sp,r5
	mov     6(r5),r1
        mov	4(r5),r0
        add	12(r5),r1
        adc	r0
	bvs	oflow
        add	10(r5),r0
	bvs	oflow
        br      return

.globl	_cksub
_cksub:
	mov	r5,-(sp)
	mov	sp,r5
        mov   	6(r5),r1
        mov	4(r5),r0
        sub	12(r5),r1
        sbc	r0
	bvs	oflow
        sub	10(r5),r0
	bvs	oflow
return:
	mov	r5,sp
	mov	(sp)+,r5
	rts	pc

.globl	_ckmul
_ckmul:
	jsr	r5,csv
	mov	6(r5),r2
	sxt	r1
	sub	4(r5),r1
	mov	10.(r5),r0
	sxt	r3
	sub	8.(r5),r3
	mul	r0,r1
	mul	r2,r3
	add	r1,r3
	mul	r2,r0
	sub	r3,r0
	jmp	cret

oflow:
	clr     -(sp)
	mov	$203.,-(sp)
	jsr	pc,_runerr
#endif PDP11
