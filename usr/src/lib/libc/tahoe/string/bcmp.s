#ifdef LIBC_SCCS
	.asciz	"@(#)bcmp.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/* bcmp(s1, s2, n) */
/* compare exactly 'n' */
#include "DEFS.h"

ENTRY(bcmp, 0)
	movl	4(fp),r0
	movl	8(fp),r1
	mnegl	12(fp),r2
	jeql	3f
1:
	cmpb	(r0),(r1)
	jneq	2f
	incl	r0
	incl	r1
	aoblss	$0,r2,1b
3:
	clrl	r0
	ret
2:
	movl	$1,r0
	ret
