/*	strlen.s	4.3	84/11/07	*/

/*
 * Return the length of cp (not counting '\0').
 *
 * strlen(cp)
 *	char *cp;
 */
#include "DEFS.h"

ENTRY(strlen, 0)
	movl	4(ap),r1
1:
	locc	$0,$65535,(r1)	# look for '\0'
	beql	1b
	subl3	4(ap),r1,r0	# len = cp - base
	ret
