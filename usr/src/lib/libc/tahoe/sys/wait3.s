/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(SYSLIBC_SCCS) && !defined(lint)
	.asciz "@(#)wait3.s	5.3 (Berkeley) 6/27/88"
#endif /* SYSLIBC_SCCS and not lint */

/*
 * C library -- wait3
 *
 * pid = wait3(&status, flags, &rusage);
 *
 * pid == -1 if error
 * status indicates fate of process, if given
 * flags may indicate process is not to hang or
 * that untraced stopped children are to be reported.
 * rusage optionally returns detailed resource usage information
 */
#include "SYS.h"

#define	SYS_wait3	SYS_wait

ENTRY(wait3)
	movl	8(fp),r0	/* make it easy for system to get */
	movl	12(fp),r1	/* these extra arguments */
	bispsw	$0xf		/* flags wait3() (N,Z,V,C)*/
	kcall	$SYS_wait3
	bcc 	noerror
	movl	r0,_errno
	mnegl	$1,r0
	ret
noerror:
	tstl	4(fp)		/* status desired? */
	beql	nostatus	/* no */
	movl	r1,*4(fp)	/* store child's status */
nostatus:
	ret
