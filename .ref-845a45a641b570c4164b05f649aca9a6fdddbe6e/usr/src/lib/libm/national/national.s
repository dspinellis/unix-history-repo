; @(#)national.s	1.1 (ucb.elefunt) %G%
;
; subroutines 
;	add_ulp(x)	... return x+ulp 
;	sub_ulp(x)	... return x-ulp
;	swapENI(i)	... swap inexact enable bit with i
;	swapINX(i)	... swap inexact flag bit with i
;	swapRM(i)	... swap rounding mode with i
;

; add_ulp(x):
; add one ulp to a floating point number
	.align	2
	.globl	_add_ulp
_add_ulp:
	movd	4(sp),r0
	movd	8(sp),r1
	addd	1,r0
	addcd	0,r1
	movd	r0,4(sp)
	movd	r1,8(sp)
	movl	4(sp),f0
	ret	0

; sub_ulp(x)
; subtract one ulp from a floating point number
	.align	2
	.globl	_sub_ulp
_sub_ulp:
	movd	4(sp),r0
	movd	8(sp),r1
	subd	1,r0
	subcd	0,r1
	movd	r0,4(sp)
	movd	r1,8(sp)
	movl	4(sp),f0
	ret	0

; swapENI(i)
; swap the inexact enable bit
	.text
	.align	2
	.globl	_swapENI
_swapENI:
	movd	4(sp),r1
	sfsr	r0
	movd	r0,r2
	bicd 	[5],r2
	andd	[0],r1
	ashd	5,r1
	ord	r1,r2
	lfsr	r2
	ashd	-5,r0
	andd	[0],r0
	ret	0

; swapINX(i)
; swap the inexact flag
	.align	2
	.globl	_swapINX
_swapINX:
	movd	4(sp),r1
	sfsr	r0
	movd	r0,r2
	bicd 	[6],r2
	andd	[0],r1
	ashd	6,r1
	ord	r1,r2
	lfsr	r2
	ashd	-6,r0
	andd	[0],r0
	ret	0

; swapRM(i)
; swap the rounding mode
	.align	2
	.globl	_swapRM
_swapRM:
	movd	4(sp),r1
	sfsr	r0
	movd	r0,r2
	bicd 	[7,8],r2
	andd	[0,1],r1
	ashd	7,r1
	ord	r1,r2
	lfsr	r2
	ashd	-7,r0
	andd	[0,1],r0
	ret	0
