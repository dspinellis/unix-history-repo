/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 */

#if defined(LIBC_SCCS) && !defined(lint)
	.asciz "@(#)strcat.s	1.4 (Berkeley) 6/1/90"
#endif /* LIBC_SCCS and not lint */

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
