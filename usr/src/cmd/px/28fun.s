#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)28fun.s 4.1 10/10/80";
#
# BUILT IN FUNCTIONS
#
_LLIMIT:
	incl	r10
	movl	(sp)+,r0	#r0 has line limit
	bgtr	l2811
	movl	$0x7fffffff,r0	#non-positive indicates no limit
l2811:
	movl	*(sp)+,r1	#r1 has file
	movl	r0,LLIMIT(r1)
	jmp	(r8)
_ARGC:
	incl	r10
	pushl	_argc
	jmp	(r8)
_ARGV:
	cvtbl	(r10)+,r2
	bneq	l2801
	movzwl	(r10)+,r2		#r2 has size of character array
l2801:
	movl	(sp)+,r3		#r3 has addr of character array
	movl	(sp)+,r4		#r4 has subscript into argv
	cmpl	r4,_argc
	bgequ	eargv
	movl	*_argv[r4],r4		#r4 has pointer to argv string
	locc	$0,r2,(r4)		#find end of string
	subl3	r0,r2,r0		#calculate actual string length
	movc5	r0,(r4),$blank,r2,(r3)	#move with blank fill
	jmp	(r8)
eargv:
	movw	$EARGV,_perrno
	jbr	error
_WCLCK:
	incl	r10
	pushal	-(sp)		#space for time
	calls	$1,_time
	jmp	(r8)
_SCLCK:
	cvtbl	$1,r6
	brb	l2805
_CLCK:
	clrl	r6
l2805:
	incl	r10
	subl2	$16,sp
	pushl	sp
	calls	$1,_times
	movl	(sp)[r6],r0
	addl2	$16,sp
	mull2	$50,r0		#(60ths * 1000) / 60
	divl3	$3,r0,-(sp)	# == (60ths * 50) / 3
	jmp	(r8)
_DATE:
	incl	r10
	pushl	$O_DATE
	calls	$2,_pdattim
	jmp	(r8)
_TIME:
	incl	r10
	pushl	$O_TIME
	calls	$2,_pdattim
	jmp	(r8)
_STLIM:
	incl	r10
	movl	(sp)+,_stlim
	cmpl	_stcnt,_stlim
	bgeq	l2812
	jmp	(r8)
l2812:
	movw	$ESTLIM,_perrno
	jbr	error
_SEED:
	incl	r10
	calls	$0,_srand
	jmp	(r8)
_RANDOM:
	incl	r10
	calls	$0,_rand
	cvtld	r0,r1
	divd3	$0d2.147483647e+09,r1,(sp)	#div by maxint to get 0..1
	jmp	(r8)
_DISPOSE:
	movzbl	(r10)+,r0	#r0 has size being disposed
	bneq	l2813
	movzwl	(r10)+,r0
l2813:
	movl	(sp)+,r6	#r6 points to pointer
	pushl	(r6)		#fetch pointer value
	calls	$1,_pfree	#free space
	clrl	(r6)		#set pointer to nil
	jmp	(r8)
_NEW:
	movzbl	(r10)+,r6
	bneq	l2806
	movzwl	(r10)+,r6
l2806:
	pushl	r6
	calls	$1,_palloc
	movl	r0,*(sp)+
	movc5	$0,(r1),$0,r6,(r0)
	jmp	(r8)
_EXPO:
	incl	r10
	clrl	4(sp)
	movl	(sp)+,r0
	beql	l2807
	bicl2	$0xffff8000,r0
	ashl	$-7,r0,r0
	subl2	$128,r0
	movl	r0,(sp)
l2807:
	jmp	(r8)
_ATAN:
	incl	r10
	calls	$2,_atan
	movd	r0,-(sp)
	jmp	(r8)
_COS:
	incl	r10
	calls	$2,_cos
	movd	r0,-(sp)
	jmp	(r8)
_EXP:
	incl	r10
	calls	$2,_exp
	movd	r0,-(sp)
	jmp	(r8)
_SIN:
	incl	r10
	calls	$2,_sin
	movd	r0,-(sp)
	jmp	(r8)
_LN:
	incl	r10
	tstd	(sp)
	bleq	eln
	calls	$2,_log
	movd	r0,-(sp)
	jmp	(r8)
eln:
	movw	$ELN,_perrno
	jbr	error
_SQRT:
	incl	r10
	tstd	(sp)
	blss	esqrt
	calls	$2,_sqrt
	movd	r0,-(sp)
	jmp	(r8)
esqrt:
	movw	$ESQRT,_perrno
	jbr	error
_CHR2:
_CHR4:
	incl	r10
	movl	(sp)+,r0
	cmpl	r0,$0x7f
	bgtru	echr
	movw    r0,-(sp)
	jmp	(r8)
echr:
	movw	$ECHR,_perrno
	jbr	error
_ODD4:
_ODD2:
	movw	(sp)+,(sp)
	incl	r10
	bicw2	$0xfffe,(sp)
	jmp	(r8)
_PRED2:
	incl	r10
	movw	(sp)+,(sp)
	decw	(sp)
	jmp	(r8)
_PRED4:
_PRED24:
	incl	r10
	decl	(sp)
	jmp	(r8)
_SUCC2:
	incl	r10
	movw	(sp)+,(sp)
	incw	(sp)
	jmp	(r8)
_SUCC4:
_SUCC24:
	incl	r10
	incl	(sp)
	jmp	(r8)
_ROUND:
	incl	r10
	cvtrdl	(sp)+,-(sp)
	jmp	(r8)
_TRUNC:
	incl	r10
	cvtdl	(sp)+,-(sp)
	jmp	(r8)
_UNDEF:
	incl	r10
	addl2	$8,sp
	clrw	-(sp)
	jmp	(r8)
#
# pack(a,i,z)
#
# with:	a: array[m..n] of t
#	z: packed array[u..v] of t
#
# semantics:	for j := u to v do
#			z[j] := a[j-u+i];
#
# need to check:
#	1. i >= m
#	2. i+(v-u) <= n		(i.e. i-m <= (n-m)-(v-u))
#
# on stack:	lv(z), lv(a), rv(i) (len 4)
#
# move w(t)*(v-u+1) bytes from lv(a)+w(t)*(i-m) to lv(z)
#
_PACK:
	cvtbl	(r10)+,r0
	bneq	l2809
	movzwl	(r10)+,r0		#r0 has size of "a" types
l2809:
	clrl	r1			#r1 := subscript - lower_bound
	subw3	(r10)+,8(sp),r1
	blss	epack
	cmpw	r1,(r10)+		#check upper bound
	bgtru	epack
	mull2	r0,r1			#r1 has byte offset
	movc3	(r10)+,*4(sp)[r1],*(sp)	#make the move
	addl2	$12,sp			#clear the stack
	jmp	(r8)
epack:
	movw	$EPACK,_perrno
	jbr	error
#
# unpack(z,a,i)
#
# with:	z and a as in pack
#
# semantics:	for j := u to v do
#			a[j-u+i] := z[j]
#
_UNPACK:
	cvtbl	(r10)+,r0
	bneq	l2810
	movzwl	(r10)+,r0		#r0 has size of "a" types
l2810:
	clrl	r1			#r1 := subscript - lower_bound
	subw3	(r10)+,8(sp),r1
	blss	eunpack
	cmpw	r1,(r10)+		#check upper bound
	bgtru	eunpack
	mull2	r0,r1			#r1 has byte offset
	movc3	(r10)+,*(sp),*4(sp)[r1]	#make the move
	addl2	$12,sp			#clear the stack
	jmp	(r8)
eunpack:
	movw	$EUNPACK,_perrno
	jbr	error
