/* Copyright (c) 1984 Regents of the University of California */

#ifndef lint
static char sccsid[] = "@(#)libcpats.c	1.2	(Berkeley)	8/18/84";
#endif not lint

#include "inline.h"

/*
 * Pattern table for the C library.
 */
struct pats libc_ptab[] = {

#ifdef vax
	{ "1,_fgetc\n",
"	movl	(sp)+,r2\n\
	sobgeq	(r2),1f\n\
	pushl	r2\n\
	calls	$1,__filbuf\n\
	jbr	2f\n\
1:\n\
	movzbl	*4(r2),r0\n\
	incl	4(r2)\n\
2:\n" },

	{ "2,_fputc\n",
"	movl	(sp)+,r2$\n\
	movl	(sp)+,r3\n\
	sobgeq	(r3),1f\n\
	pushl	r3\n\
	pushl	r2\n\
	calls	$2,__flsbuf\n\
	jbr	2f\n\
1:\n\
	movb	r2,*4(r3)\n\
	incl	4(r3)\n\
2:\n" },

	{ "1,_strlen\n",
"	movl	(sp)+,r5\n\
	movl	r5,r1\n\
1:\n\
	locc	$0,$65535,(r1)\n\
	jeql	1b\n\
	subl3	r5,r1,r0\n" },
#endif vax

#ifdef mc68000
/* someday... */
#endif mc68000

	{ "", "" }
};
