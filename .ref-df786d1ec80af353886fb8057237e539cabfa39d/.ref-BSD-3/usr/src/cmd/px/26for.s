#
# 26for.s
#
# FORS
#
_FOR1U:
	incl	r10
	movl	(sp)+,r0	#r0 ptrs to index variable
	movl	(sp)+,r1	#r1 has limit
	acbb	r1,$1,(r0),repeat
	addl2	$2,r10
	jmp	(r8)
repeat:
	cvtwl	(r10),r1
	addl2	r1,r10
	jmp	(r8)
_FOR2U:
	incl	r10
	movl	(sp)+,r0	#r0 ptrs to index variable
	movl	(sp)+,r1	#r1 has limit
	acbw	r1,$1,(r0),repeat
	addl2	$2,r10
	jmp	(r8)
_FOR4U:
	incl	r10
	movl	(sp)+,r0	#r0 ptrs to index variable
	aobleq	(sp)+,(r0),repeat
	addl2	$2,r10
	jmp	(r8)
_FOR1D:
	incl	r10
	movl	(sp)+,r0	#r0 ptrs to index variable
	movl	(sp)+,r1	#r1 has limit
	acbb	r1,$-1,(r0),repeat
	addl2	$2,r10
	jmp	(r8)
_FOR2D:
	incl	r10
	movl	(sp)+,r0	#r0 ptrs to index variable
	movl	(sp)+,r1	#r1 has limit
	acbw	r1,$-1,(r0),repeat
	addl2	$2,r10
	jmp	(r8)
_FOR4D:
	incl	r10
	movl	(sp)+,r0	#r0 ptrs to index variable
	acbl	(sp)+,$-1,(r0),repeat
	addl2	$2,r10
	jmp	(r8)
