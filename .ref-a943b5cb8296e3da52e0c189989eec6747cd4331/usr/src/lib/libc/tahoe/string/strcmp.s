#ifdef LIBC_SCCS
	.asciz	"@(#)strcmp.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

#include "DEFS.h"

/*
 * Compare strings:  s1>s2: >0  s1==s2: 0  s1<s2: <0
 * 
 * strcmp(s1, s2)
 * register char *s1, *s2;
*/

ENTRY(strcmp, 0)
	movl	4(fp),r0
	movl	8(fp),r1
	cmps2
	jgtr	greater
	jlss	less
equal:	clrl	r0
	ret
less:	movl	$-1,r0
	ret
greater: movl	$1,r0
	ret
