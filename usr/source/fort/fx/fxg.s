/
/

/ fxg -- declare implicit functions

.globl	funimpl

.globl	getname
.globl	lookup

funimpl:
	bit	$7,symtab(r3)
	bne	1f
	jsr	r5,getname
	mov	r2,-(sp)
	mov	$symbuf,r2
	jsr	r5,lookup; funtab
		br 2f
	cmpb	(r2),$'\n
	bne	2f
	asr	r0
	movb	funtabt(r0),r0
	mov	r0,-(sp)
	bic	$!17,r0
	bis	typtab(r0),symtab(r3)
	mov	(sp)+,r0
	asr	r0
	asr	r0
	asr	r0
	asr	r0
	bic	$!17,r0
	mov	typtab(r0),symtab+2(r3)	/ save argument conversion
2:					/ in dimension pointer
	mov	(sp)+,r2
1:
	rts	r5

typtab:
	intcon
	realcon
	dblcon
	cplxcon
	dcplxcon

funtab:
	<tanh\0>
	<sqrt\0>
	<sngl\0>
	<sin\0>
	<sign\0>
	<real\0>
	<mod\0>
	<min1\0>
	<min0\0>
	<max1\0>
	<max0\0>
	<isign\0>
	<int\0>
	<ifix\0>
	<idint\0>
	<idim\0>
	<iabs\0>
	<float\0>
	<exp\0>
	<dsqrt\0>
	<dsin\0>
	<dsign\0>
	<dreal\0>
	<dmod\0>
	<dmin1\0>
	<dmax1\0>
	<dlog10\0>
	<dlog\0>
	<dimag\0>
	<dim\0>
	<dexp\0>
	<dcsqrt\0>
	<dcsin\0>
	<dcos\0>
	<dconjg\0>
	<dcmplx\0>
	<dclog\0>
	<dcexp\0>
	<dccos\0>
	<dcabs\0>
	<dble\0>
	<datan2\0>
	<datan\0>
	<dabs\0>
	<csqrt\0>
	<csin\0>
	<cos\0>
	<conjg\0>
	<cmplx\0>
	<clog\0>
	<cexp\0>
	<ccos\0>
	<cabs\0>
	<atan2\0>
	<atan\0>
	<amod\0>
	<amin1\0>
	<amin0\0>
	<amax1\0>
	<amax0\0>
	<alog10\0>
	<alog\0>
	<aint\0>
	<aimag\0>
	<abs\0>
	<\0>

/ function type xy
/	x = arg types
/	y = result type
/ 0 = integer
/ 2 = real
/ 4 = double
/ 6 = complex
/ 8 = doublecomplex
funtabt:
	.byte	2\<4+2		/ tanh
	.byte	2\<4+2		/ sqrt
	.byte	4\<4+2		/ sngl
	.byte	2\<4+2		/ sin
	.byte	2\<4+2		/ sign
	.byte	6\<4+2		/ real
	.byte	0\<4+0		/ mod
	.byte	2\<4+0		/ min1
	.byte	0\<4+0		/ min0
	.byte	2\<4+0		/ max1
	.byte	0\<4+0		/ max0
	.byte	0\<4+0		/ isign
	.byte	2\<4+0		/ int
	.byte	2\<4+0		/ ifix
	.byte	4\<4+0		/ idint
	.byte	0\<4+0		/ idim
	.byte	0\<4+0		/ iabs
	.byte	0\<4+2		/ float
	.byte	2\<4+2		/ exp
	.byte	4\<4+4		/ dsqrt
	.byte	4\<4+4		/ dsin
	.byte	4\<4+4		/ dsign
	.byte	8\<4+4		/ dreal
	.byte	4\<4+4		/ dmod
	.byte	4\<4+4		/ dmin1
	.byte	4\<4+4		/ dmax1
	.byte	4\<4+4		/ dlog10
	.byte	4\<4+4		/ dlog
	.byte	8\<4+4		/ dimag
	.byte	2\<4+2		/ dim
	.byte	4\<4+4		/ dexp
	.byte	8\<4+8		/ dcsqrt
	.byte	8\<4+8		/ dcsin
	.byte	4\<4+4		/ dcos
	.byte	8\<4+8		/ dconjg
	.byte	4\<4+8		/ dcmplx
	.byte	8\<4+8		/ dclog
	.byte	8\<4+8		/ dcexp
	.byte	8\<4+8		/ dccos
	.byte	8\<4+4		/ dcabs
	.byte	2\<4+4		/ dble
	.byte	4\<4+4		/ datan2
	.byte	4\<4+4		/ datan
	.byte	4\<4+4		/ dabs
	.byte	6\<4+6		/ csqrt
	.byte	6\<4+6		/ csin
	.byte	2\<4+2		/ cos
	.byte	6\<4+6		/ conjg
	.byte	2\<4+6		/ cmplx
	.byte	6\<4+6		/ clog
	.byte	6\<4+6		/ cexp
	.byte	6\<4+6		/ ccos
	.byte	6\<4+2		/ cabs
	.byte	2\<4+2		/ atan2
	.byte	2\<4+2		/ atan
	.byte	2\<4+2		/ amod
	.byte	2\<4+2		/ amin1
	.byte	0\<4+2		/ amin0
	.byte	2\<4+2		/ amax1
	.byte	0\<4+2		/ amax0
	.byte	2\<4+2		/ alog10
	.byte	2\<4+2		/ alog
	.byte	2\<4+2		/ aint
	.byte	6\<4+2		/ aimag
	.byte	2\<4+2		/ abs
