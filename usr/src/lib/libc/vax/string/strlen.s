/*	strlen.s	4.2	84/11/01	*/

/*
 * Return the length of cp (not counting '\0').
 *
 * strlen(cp)
 *	char *cp;
 */
#include "DEFS.h"

ENTRY(strlen, 0)
	movl	4(ap),r2
0:
	locc	$0,$65535,(r2)	# look for '\0'
	beql	0b
	subl3	r2,r1,r0	# len = cp - base
	ret
