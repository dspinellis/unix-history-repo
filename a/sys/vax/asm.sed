s,asm.sed 4.26 82/10/31,asm.sed 4.26 82/10/31,
s/calls	$0,_spl0/mfpr	$18,r0\
	mtpr	$0,$18/
s/calls	$0,_spl1/mfpr	$18,r0\
	mtpr	$0,$18/
s/calls	$0,_splnet/mfpr	$18,r0\
	mtpr	$0xc,$18/
s/calls	$0,_splimp/mfpr	$18,r0\
	mtpr	$0x16,$18/
s/calls	$0,_spl4/mfpr	$18,r0\
	mtpr	$0x14,$18/
s/calls	r[0-9]*,_spl4/mfpr	$18,r0\
	mtpr	$0x14,$18/
s/calls	$0,_spl5/mfpr	$18,r0\
	mtpr	$0x15,$18/
s/calls	r[0-9]*,_spl5/mfpr	$18,r0\
	mtpr	$0x15,$18/
s/calls	$0,_spl6/mfpr	$18,r0\
	mtpr	$0x18,$18/
s/calls	r[0-9]*,_spl6/mfpr	$18,r0\
	mtpr	$0x18,$18/
s/calls	$0,_spl7/mfpr	$18,r0\
	mtpr	$0x1f,$18/
s/calls	$1,_splx/mfpr	$18,r0\
	mtpr	(sp)+,$18/
s/calls	$1,_mfpr/mfpr	(sp)+,r0/
s/calls	$2,_mtpr/mtpr	4(sp),(sp)\
	addl2	$8,sp/
s/calls	$0,_setsoftclock/mtpr	$0x8,$0x14/
s/calls	$1,_resume/ashl	$9,(sp)+,r0 \
	movpsl	-(sp) \
	jsb	_Resume/
s/calls	$3,_bcopy/movc3	8(sp),*(sp),*4(sp)\
	addl2	$12,sp/
s/calls	$3,_ovbcopy/movc3	8(sp),*(sp),*4(sp)\
	addl2	$12,sp/
s/calls	$2,_bzero/movc5	$0,(r0),$0,4(sp),*(sp)\
	addl2	$8,sp/
s/calls	$3,_bcmp/popr	$0x7\
	cmpc3	r2,(r0),(r1)/
s/calls	$3,_strncmp/cmpc3	8(sp),*(sp),*4(sp)\
	addl2	$12,sp/
s/calls	$2,_blkclr/movl	(sp)+,r3\
	jbr	2f\
1:\
	subl2	r0,(sp)\
	movc5	$0,(r3),$0,r0,(r3)\
2:\
	movzwl	$65535,r0\
	cmpl	(sp),r0\
	jgtr	1b\
	movl	(sp)+,r0\
	movc5	$0,(r3),$0,r0,(r3)/
s/calls	$1,_strlen/movl	(sp),r1\
1:\
	locc	$0,$65535,(r1)\
	jeql	1b\
	subl3	(sp)+,r1,r0/
s/calls	$4,_scanc/popr	$0xf\
	scanc	r0,(r1),(r2),r3/
s/calls	$3,_copyin/jsb	_Copyin\
	addl2	$12,sp/
s/calls	$3,_copyout/jsb	_Copyout\
	addl2	$12,sp/
s/calls	$1,_fubyte/movl	(sp)+,r0 \
	jsb	_Fubyte/
s/calls	$1,_fuibyte/movl (sp)+,r0 \
	jsb	_Fubyte/
s/calls	$1,_fuword/movl (sp)+,r0 \
	jsb	_Fuword/
s/calls	$1,_fuiword/movl (sp)+,r0 \
	jsb	_Fuword/
s/calls	$2,_subyte/movl	(sp)+,r0 \
	movl	(sp)+,r1 \
	jsb	_Subyte/
s/calls	$2,_suibyte/movl (sp)+,r0 \
	movl	(sp)+,r1 \
	jsb	_Subyte/
s/calls	$2,_suword/movl (sp)+,r0 \
	movl	(sp)+,r1 \
	jsb	_Suword/
s/calls	$2,_suiword/movl (sp)+,r0 \
	movl	(sp)+,r1 \
	jsb	_Suword/
s/calls	$1,_setrq/movl	(sp)+,r0 \
	jsb	_Setrq/
s/calls	$1,_remrq/movl	(sp)+,r0 \
	jsb	_Remrq/
s/calls	$0,_swtch/movpsl	-(sp)\
	jsb	_Swtch/
s/calls	$1,_setjmp/movl	(sp)+,r0 \
	jsb	_Setjmp/
s/calls	$1,_longjmp/movl	(sp)+,r0 \
	jsb	_Longjmp/
s/calls	$1,_ffs/ffs	$0,$32,(sp)+,r0 \
	bneq	1f \
	mnegl	$1,r0 \
1: \
	incl	r0/
s/calls	$1,_htons/rotl	$8,(sp),r0\
	movb	1(sp),r0\
	movzwl	r0,r0\
	addl2	$4,sp/
s/calls	$1,_ntohs/rotl	$8,(sp),r0\
	movb	1(sp),r0\
	movzwl	r0,r0\
	addl2	$4,sp/
s/calls	$1,_htonl/rotl	$-8,(sp),r0\
	insv	r0,$16,$8,r0\
	movb	3(sp),r0\
	addl2	$4,sp/
s/calls	$1,_ntohl/rotl	$-8,(sp),r0\
	insv	r0,$16,$8,r0\
	movb	3(sp),r0\
	addl2	$4,sp/
s/calls	$2,__insque/insque	*(sp)+,*(sp)+/
s/calls	$1,__remque/remque	*(sp)+,r0/
s/calls	$2,__queue/movl	(sp)+,r0\
	movl	(sp)+,r1\
	insque	r1,*4(r0)/
s/calls	$1,__dequeue/movl	(sp)+,r0\
	remque	*(r0),r0/
