#ifdef LIBC_SCCS
	.asciz	"@(#)strncmp.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/*
 * Compare strings (at most n bytes):  s1>s2: >0  s1==s2: 0  s1<s2: <0
 * 
 * strncmp(s1, s2, n)
 * register char *s1, *s2;
 * register n;
 */
#include "DEFS.h"

ENTRY(strncmp, 0)
	movl	12(fp),r2
	tstl	r2		/* number of bytes to compare */
	jgtr	n_ok
	clrl	r0
	ret			/* for n <= 0 , s1 == s2 */
n_ok:
	movl	4(fp),r0
	movl	8(fp),r1
	cmps3
	jgtr	greater
	jlss	less
equal:	clrl	r0
	ret
less:	movl	$-1,r0
	ret
greater: movl	$1,r0
	ret
