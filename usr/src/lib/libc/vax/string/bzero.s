/* @(#)bzero.s	4.2 %G% */
/* bzero(base, length) */

#include "DEFS.h"

ENTRY(bzero)
	movl	4(ap),r3
	jbr	2f
1:
	subl2	r0,8(ap)
	movc5	$0,(r3),$0,r0,(r3)
2:
	movzwl	$65535,r0
	cmpl	8(ap),r0
	jgtr	1b
	movc5	$0,(r3),$0,8(ap),(r3)
	ret
