#ifdef LIBC_SCCS
	.asciz	"@(#)strcat.s	1.1 (Berkeley/CCI) %G%"
#endif LIBC_SCCS

/* 
 * Concatenate s2 on the end of s1.  S1's space must be large enough.
 * Return s1.
 * 
 * char * strcat(s1, s2)
 * register char *s1, *s2;
*/
#include "DEFS.h"
	
ENTRY(strcat, 0)
	movl	4(fp),r0
	movl	r0,r1
	cmps2			# r0 and r1 point at null at end of s1
	movl	8(fp),r0	# source string
	movs2
	movl	4(fp),r0
	ret 
