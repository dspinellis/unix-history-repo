/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)strncpy.s	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Copy string s2 over top of string s1.
 * Truncate or null-pad to n bytes.
 *
 * char *
 * strncpy(s1, s2, n)
 *	char *s1, *s2;
 */
#include "DEFS.h"

ENTRY(strncpy, R6)
	movl	12(ap),r6	# r6 = n
	bleq	done		# n <= 0
	movl	4(ap),r3	# r3 = s1
	movl	8(ap),r1	# r1 = s2
1:
	movzwl	$65535,r2	# r2 = bytes in first chunk
	cmpl	r6,r2		# r2 = min(bytes in chunk, n);
	jgeq	2f
	movl	r6,r2
2:
	subl2	r2,r6		# update n
	locc	$0,r2,(r1)	# '\0' found?
	jneq	3f
	subl2	r2,r1		# back up pointer updated by locc
	movc3	r2,(r1),(r3)	# copy in next piece
	tstl	r6		# run out of space?
	jneq	1b
	jbr	done
3:				# copy up to '\0' logic
	addl2	r0,r6		# r6 = number of null-pad bytes
	subl2	r0,r2		# r2 = number of bytes to move
	subl2	r2,r1		# back up pointer updated by locc
	movc3	r2,(r1),(r3)	# copy in last piece
4:				# null-pad logic
	movzwl	$65535,r2	# r2 = bytes in first chunk
	cmpl	r6,r2		# r2 = min(bytes in chunk, n);
	jgeq	5f
	movl	r6,r2
5:
	subl2	r2,r6		# update n
	movc5	$0,(r3),$0,r2,(r3)# pad with '\0's
	tstl	r6		# finished padding?
	jneq	4b
done:
	movl	4(ap),r0	# return s1
	ret
