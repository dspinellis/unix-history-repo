/*	rindex.s	4.2	84/11/01	*/

/*
 * Find the last occurence of c in the string cp.
 * Return pointer to match or null pointer.
 *
 * char *
 * rindex(cp, c)
 *	char *cp, c;
 */
#include "DEFS.h"

ENTRY(rindex, 0)
	movq	4(ap),r1	# r1 = cp; r2 = c
	tstl	r2		# check for special case c == '\0'
	bneq	2f
1:
	locc	$0,$65535,(r1)	# just find end of string
	beql	1b		# still looking
	movl	r1,r0		# found it
	ret
2:
	movab	tbl[r2],r5	# table entry for c
	incb	(r5)
	clrl	r4		# last found
3:
	scanc	$65535,(r1),tbl,$1	# look for c or '\0'
	beql	3b		# keep looking
	tstb	(r1)		# if have found '\0'
	beql	4f		#    we are done
	movl	r1,r4		# save most recently found
	incl	r1		# skip over character
	jbr	3b		# keep looking
4:
	movl	r4,r0		# return last found (if any)
	clrb	(r5)		# clean up table
	ret

	.data
tbl:	.byte	1
	.space	255
	.text
