; @(#)support.s	1.1 (ucb.elefunt) %G%
; 
; IEEE recommended functions
;
; 
; double copysign(x,y)
; double x,y;
; IEEE 754 recommended function, return x*sign(y)
; Coded by K.C. Ng in National 32k assembler, 11/9/85.
;
	.vers	2
	.text
	.align	2
	.globl	_copysign
_copysign:
	movl	4(sp),f0
	movd	8(sp),r0
	movd	16(sp),r1
	xord	r0,r1
	ord	0x80000000,r1
	cmpqd	0,r1
	beq	end
	negl	f0,f0
end:	ret	0

; 
; double logb(x)
; double x;
; IEEE p854 recommended function, return the exponent of x (return float(N) 
; such that 1 <= x*2**-N < 2, even for subnormal number.
; Coded by K.C. Ng in National 32k assembler, 11/9/85.
; Note: subnormal number (if implemented) will be taken care of. 
;
	.vers	2
	.text
	.align	2
	.globl	_logb
_logb:
;
; extract the exponent of x
; glossaries:	r0 = high part of x
;		r1 = unbias exponent of x
;		r2 = 20 (first exponent bit position)
;
	movd	8(sp),r0
	movd	20,r2
	extd	r2,r0,r1,11	; extract the exponent of x
	cmpqd	0,r1		; if exponent bits = 0, goto L3
	beq	L3
	cmpd	0x7ff,r1
	beq	L2		; if exponent bits = 0x7ff, goto L2
L1:	subd	1023,r1		; unbias the exponent
	movdl	r1,f0		; convert the exponent to floating value
	ret	0
;
; x is INF or NaN, simply return x
;
L2:
	movl	4(sp),f0	; logb(+inf)=+inf, logb(NaN)=NaN
	ret	0
;
; x is 0 or subnormal
;
L3:
	movl	4(sp),f0
	cmpl	0f0,f0
	beq	L5		; x is 0 , goto L5 (return -inf)
;
; Now x is subnormal
;
	mull	L64,f0		; scale up f0 with 2**64
	movl	f0,tos
	movd	tos,r0
	movd	tos,r0		; now r0 = new high part of x
	extd	r2,r0,r1,11	; extract the exponent of x to r1
	subd	1087,r1		; unbias the exponent with correction 
	movdl	r1,f0		; convert the exponent to floating value
	ret	0
;
; x is 0, return logb(0)= -INF
;
L5:
	movl	0f1.0e300,f0
	mull	0f-1.0e300,f0	; multiply two big numbers to get -INF
	ret	0
L64:	.double	0,0x43f00000	; L64=2**64
	
; 
; double rint(x)
; double x;
; ... delivers integer nearest x in direction of prevailing rounding
; ... mode
; Coded by K.C. Ng in National 32k assembler, 11/9/85.
; Note: subnormal number (if implemented) will be taken care of. 
;
	.vers	2
	.text
	.align	2
	.globl	_rint
_rint:
;
	movd	8(sp),r0
	movd	20,r2
	extd	r2,r0,r1,11	; extract the exponent of x
	cmpd	0x433,r1
	ble	itself
	movl	L52,f2		; f2 = L = 2**52
	cmpqd	0,r0
	ble	L1
	negl	f2,f2		; f2 = s = copysign(L,x)
L1:	addl	f2,f0		; f0 = x + s
	subl	f2,f0		; f0 = f0 - s
	ret	0
itself:	movl	4(sp),f0
	ret	0
L52:	.double	0x0,0x43300000	; L52=2**52
; 
; int finite(x)
; double x;
; IEEE 754 recommended function, return 0 if x is NaN or INF, else 0
; Coded by K.C. Ng in National 32k assembler, 11/9/85.
;
	.vers	2
	.text
	.align	2
	.globl	_finite
_finite:
	movd	4(sp),r1
	andd	0x800fffff,r1
	cmpd	0x7ff00000,r1
	sned	r0		; r0=0 if exponent(x) = 0x7ff
	ret	0
; 
; double scalb(x,N)
; double x; int N;
; IEEE 754 recommended function, return x*2**N by adjusting 
; exponent of x.
; Coded by K.C. Ng in National 32k assembler, 11/9/85. 
; Note: subnormal number (if implemented) will be taken care of 
;
	.vers	2
	.text
	.align	2
	.globl	_scalb
_scalb:
;
; if x=0 return 0
;
	movl	4(sp),f0
	cmpl	0f0,f0
	beq	end		; scalb(0,N) is x itself
;
; extract the exponent of x
; glossaries:	r0 = high part of x, 
;		r1 = unbias exponent of x,
;		r2 = 20 (first exponent bit position).
;
	movd	8(sp),r0	; r0 = high part of x
	movd	20,r2		; r2 = 20
	extd	r2,r0,r1,11	; extract the exponent of x in r1
	cmpd	0x7ff,r1	
;
; if exponent of x is 0x7ff, then x is NaN or INF; simply return x  
;
	beq	end		
	cmpqd	0,r1
;
; if exponent of x is zero, then x is subnormal; goto L19
;
	beq	L19		
	addd	12(sp),r1	; r1 = (exponent of x) + N
	bfs	inof		; if integer overflows, goto inof
	cmpqd	0,r1		; if new exponent <= 0, goto underflow
	bge	underflow
	cmpd	2047,r1		; if new exponent >= 2047 goto overflow
	ble	overflow
	insd	r2,r1,r0,11	; insert the new exponent 
	movd	r0,tos
	movd	8(sp),tos
	movl	tos,f0		; return x*2**N
end:	ret	0
inof:	bcs	underflow	; negative int overflow if Carry bit is set
overflow:
	andd	0x80000000,r0	; keep the sign of x
	ord	0x7fe00000,r0	; set x to a huge number
	movd	r0,tos
	movqd	0,tos
	movl	tos,f0
	mull	0f1.0e300,f0	; multiply two huge number to get overflow
	ret	0
underflow:
	addd	64,r1		; add 64 to exonent to see if it is subnormal
	cmpqd	0,r1
	bge	zero		; underflow to zero
	insd	r2,r1,r0,11	; insert the new exponent 
	movd	r0,tos
	movd	8(sp),tos
	movl	tos,f0
	mull	L30,f0		; readjust x by multiply it with 2**-64
	ret	0
zero:	andd	0x80000000,r0	; keep the sign of x
	ord	0x00100000,r0	; set x to a tiny number
	movd	r0,tos
	movqd	0,tos
	movl	tos,f0
	mull	0f1.0e-300,f0	; underflow to 0  by multipling two tiny nos.
	ret	0
L19:		; subnormal number
	mull	L32,f0		; scale up x by 2**64
	movl	f0,tos
	movd	tos,r0
	movd	tos,r0		; get the high part of new x
	extd	r2,r0,r1,11	; extract the exponent of x in r1
	addd	12(sp),r1	; exponent of x + N
	subd	64,r1		; adjust it by subtracting 64
	cmpqd	0,r1
	bge	underflow
	cmpd	2047,r1
	ble	overflow
	insd	r2,r1,r0,11	; insert back the incremented exponent 
	movd	r0,tos
	movd	8(sp),tos
	movl	tos,f0
end:	ret	0
L30:	.double	0x0,0x3bf00000	; floating point 2**-64
L32:	.double	0x0,0x43f00000	; floating point 2**64
