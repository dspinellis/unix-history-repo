#include "../h/config.h"
/*
 * Long integer arithmetic routines with overflow checking.
 */
Global(_runerr)		/* Run-time error routine */
Global(_ckadd)
Global(_cksub)
Global(_ckmul)
#ifdef VAX
 .text
 .align	1
/*
 * Addition.
 */
_ckadd:	Mask	0
	addl3	4(ap),8(ap),r0	# Perform addition
	jvs	oflow		# Branch if overflow
	ret			# Return result in r0

 .align	1
/*
 * Subtraction.
 */
_cksub:Mask	0
	subl3	8(ap),4(ap),r0	# Perform subtraction
	jvs	oflow		# Branch if overflow
	ret			# Return result in r0

 .align	1
/*
 * Multiplication.
 */
_ckmul:	Mask	0
	mull3	4(ap),8(ap),r0	# Perform multiplication
	jvs	oflow		# Branch if overflow
	ret			# Return result in r0

oflow:				# Got overflow on an operation
	pushl   $0
	pushl	$203
	calls	$1,_runerr	# runerr(203,0)

#endif VAX

#ifdef PORT
DummyFcn(_ckadd)
DummyFcn(_cksub)
DummyFcn(_ckmul)
#endif PORT

#ifdef PDP11
/ Long integer arithmetic routines with overflow checking
Global(csv)
Global(cret)

 .text
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
