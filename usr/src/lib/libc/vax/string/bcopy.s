/* @(#)bcopy.s	4.2 %G% */
/* bcopy(to, from, size) */

#include "DEFS.h"

ENTRY(bcopy)
	movl	4(ap),r1
	movl	8(ap),r3
	jbr	2f
1:
	subl2	r0,12(ap)
	movc3	r0,(r1),(r3)
2:
	movzwl	$65535,r0
	cmpl	12(ap),r0
	jgtr	1b
	movc3	12(ap),(r1),(r3)
	ret
