#
# Copyright (c) 1979 Regents of the University of California
#
# char sccsid[] = "@(#)26for.s 4.1 10/10/80";
#
# FORS
#
_FOR1U:
	incl	r10
	movl	(sp)+,r0	#r0 ptrs to index variable
	movl	(sp)+,r1	#r1 has limit
	cmpb	r1,(r0)		#check for done
	bleq	done
	incb	(r0)		#increment pointer
	cvtwl	(r10),r1
	addl2	r1,r10
	jmp	(r8)
done:
	addl2	$2,r10
	jmp	(r8)
_FOR2U:
	incl	r10
	movl	(sp)+,r0	#r0 ptrs to index variable
	movl	(sp)+,r1	#r1 has limit
	cmpw	r1,(r0)		#check for done
	bleq	done
	incw	(r0)		#increment pointer
	cvtwl	(r10),r1
	addl2	r1,r10
	jmp	(r8)
_FOR4U:
	incl	r10
	movl	(sp)+,r0	#r0 ptrs to index variable
	cmpl	(sp)+,(r0)	#check for done
	bleq	done
	incl	(r0)		#increment pointer
	cvtwl	(r10),r1
	addl2	r1,r10
	jmp	(r8)
_FOR1D:
	incl	r10
	movl	(sp)+,r0	#r0 ptrs to index variable
	movl	(sp)+,r1	#r1 has limit
	cmpb	r1,(r0)		#check for done
	bgeq	done
	decb	(r0)		#increment pointer
	cvtwl	(r10),r1
	addl2	r1,r10
	jmp	(r8)
_FOR2D:
	incl	r10
	movl	(sp)+,r0	#r0 ptrs to index variable
	movl	(sp)+,r1	#r1 has limit
	cmpw	r1,(r0)		#check for done
	bgeq	done
	decw	(r0)		#increment pointer
	cvtwl	(r10),r1
	addl2	r1,r10
	jmp	(r8)
_FOR4D:
	incl	r10
	movl	(sp)+,r0	#r0 ptrs to index variable
	cmpl	(sp)+,(r0)	#check for done
	bgeq	done
	decl	(r0)		#increment pointer
	cvtwl	(r10),r1
	addl2	r1,r10
	jmp	(r8)
