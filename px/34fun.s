/
/ BUILT IN FUNCTIONS
/
_LLIMIT:
	add	$4.,sp
	return
_ARGC:
	mov	_argc,-(sp)
	sxt	-(sp)
	return
_ARGV:
	bne	1f
	mov	(lc)+,r3
1:
	mov	(sp)+,r1
	mov	(sp)+,r0
	blt	9f
	cmp	r0,_argc
	bge	9f
	add	r0,r0
	add	_argv,r0
	mov	(r0),r0
	inc	r3
	br	3f
1:
	movb	(r0)+,r2
	bne	2f
	dec	r0
	mov	$' ,r2
2:
	movb	r2,(r1)+
3:
	sob	r3,1b
	return
9:
	mov	$EARGV,_perrno
	error	EARGV
_SCLCK:
	mov	$2,-(sp)
	br	1f
_CLCK:
	clr	-(sp)
1:
	sub	$12.,sp
	mov	sp,0f
	sys	indir; 1f
	mov	sp,r0
	add	12.(sp),r0
	mov	(r0),r0
	add	$14.,sp
	mul	$1000.,r0
	mov	r1,-(sp)
	mov	r0,-(sp)
	movif	(sp)+,fr0
	mov	$60.,-(sp)
	sxt	-(sp)
	movif	(sp)+,fr1
	divf	fr1,fr0
	movfi	fr0,-(sp)
	return
.data
1:
	sys	times; 0:..
.text
_DATE:
_TIME:
	asr	r0
	bic	$!377,r0
	mov	r0,-(sp)
	jsr	pc,_pdattim
	cmp	(sp)+,(sp)+
	return
_SEED:
	mov	(sp)+,r0
	mov	(sp)+,r1
	mov	_seed+2,-(sp)
	mov	_seed,-(sp)
	mov	r0,_seed
	mov	r1,_seed+2
	return
_RANDOM:
	movif	_seed,fr0
	mulf	_randa,fr0
	addf	_randc,fr0
	movf	fr0,fr2
	modf	_randim,fr2
	movf	fr2,(sp)
	mulf	_randm,fr2
	movfi	fr2,_seed
	return
_DISPOSE:
	tst	(sp)
	beq	1f
	jsr	pc,_free
1:
	tst	(sp)+
	return
_NEW:
	bne	1f
	mov	(lc)+,r3
1:
	mov	r3,-(sp)
	jsr	pc,_alloc
	tst	(sp)+
	mov	r0,*(sp)+
	return
_EXPO:
	movf	(sp)+,fr0
	movei	fr0,-(sp)
	sxt	-(sp)
	return
_ATAN:
	movf	(sp)+,fr0
	jsr	pc,atan
	movf	fr0,-(sp)
	return
_COS:
	movf	(sp)+,fr0
	jsr	pc,cos
	movf	fr0,-(sp)
	return
_EXP:
	movf	(sp)+,fr0
	jsr	pc,exp
	movf	fr0,-(sp)
	return
_LN:
	movf	(sp)+,fr0
	cfcc
	bmi	9f
	jsr	pc,log
	movf	fr0,-(sp)
	return
9:
	mov	$ELN,_perrno
	error	ELN
_SIN:
	movf	(sp)+,fr0
	jsr	pc,sin
	movf	fr0,-(sp)
	return
_SQRT:
	movf	(sp)+,fr0
	cfcc
	bmi	9f
	jsr	pc,sqrt
	movf	fr0,-(sp)
	return
9:
	mov	$ESQRT,_perrno
	error	ESQRT
_CHR4:
	tst	(sp)+
	bne	1f
_CHR2:
	cmp	(sp),$177
	bhi	1f
	return
1:
	mov	$ECHR,_perrno
	error	ECHR
_ODD4:
	tst	(sp)+
_ODD2:
	bic	$!1,(sp)
	return
_PRED2:
	dec	(sp)
	return
_PRED4:
	sub	$1,2(sp)
	sbc	(sp)
	return
_PRED24:
	sub	$1,(sp)
	sxt	-(sp)
	return
_STLIM:
	mov	(sp)+,r0
	bge	1f
	tst	(sp)+
	return
1:
	mov	(sp)+,r1
	sub	_stcnt+2,r1
	sbc	r0
	sub	_stcnt,r0
	ashc	$0,r0
	bge	1f
	mov	$ESTLIM,_perrno
	error	ESTLIM
1:
	mov	r0,_stlim
	mov	r1,_stlim+2
	return
_SUCC2:
	inc	(sp)
	return
_SUCC4:
	add	$1,2(sp)
	adc	(sp)
	return
_SUCC24:
	add	$1,(sp)
	sxt	-(sp)
	return
_ROUND:
	movf	(sp)+,fr0
	cfcc
	bmi	1f
	addf	$HALF,fr0
	br	2f
1:
	subf	$HALF,fr0
2:
	movfi	fr0,-(sp)
	return
_TRUNC:
	movf	(sp)+,fr0
	movfi	fr0,-(sp)
	return
_UNDEF:
	add	$8,sp
	clr	-(sp)
	return
/
/ pack(a,i,z)
/
/ with:	a: array[m..n] of t
/	z: packed array[u..v] of t
/
/ semantics:	for j := u to v do
/			z[j] := a[j-u+i];
/
/ need to check:
/	1. i >= m
/	2. i+(v-u) <= n		(i.e. i-m <= (n-m)-(v-u))
/
/ on stack:	lv(z), lv(a), rv(i) (len 2)
/
/ move w(t)*(v-u+1) bytes from lv(a)+w(t)*(i-m) to lv(z)
/
_PACK:
	bne	1f
	mov	(lc)+,r3
1:
	mov	$pack,reta
/
/ check conditions 1 and 2
/
pakunp:
	mov	4(sp),r1
	sub	(lc)+,r1		/ m
	blt	9f
	cmp	r1,(lc)+		/ (n-m)-(u-v)
	bgt	9f
	mul	r3,r1
	mov	(sp)+,r0
	add	(sp)+,r1
	tst	(sp)+
	mov	(lc)+,r3
	inc	r3
	jmp	*reta
unpack:
	mov	r0,r2
	mov	r1,r0
	mov	r2,r1
	br	pack
1:
	movb	(r1)+,(r0)+
pack:
	sob	r3,1b
	return
/
/ unpack(z,a,i)
/
/ with:	z and a as in pack
/
/ semantics:	for j := u to v do
/			a[j-u+i] := z[j]
/
_UNPACK:
	bne	1f
	mov	(lc)+,r3
1:
	mov	$unpack,reta
	br	pakunp
9:
	cmp	reta,$pack
	beq	1f
	mov	$EUNPACK,_perrno
	error	EUNPACK
1:
	mov	$EPACK,_perrno
	error	EPACK
_WCLCK:
	sys	time
	mov	r1,-(sp)
	mov	r0,-(sp)
	return
