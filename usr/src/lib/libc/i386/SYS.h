/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * William Jolitz.
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
 *	@(#)SYS.h	5.2 (Berkeley) 5/30/90
 */

#include <syscall.h>

#ifdef PROF
#define	ENTRY(x)	.globl _/**/x; \
			.data; 1:; .long 0; .text; _/**/x:	.align 4; \
			movl $1b,%eax; call mcount
#else
#define	ENTRY(x)	.globl _/**/x; .text; .align 4; _/**/x: 
#endif PROF
#define	SYSCALL(x)	2: jmp cerror; ENTRY(x); lea SYS_/**/x,%eax; LCALL(7,0); jb 2b
#define	PSEUDO(x,y)	ENTRY(x); lea SYS_/**/y, %eax; ; LCALL(7,0)
#define	CALL(x,y)	call _/**/y; addl $4*x,%esp
/* gas fucks up offset -- although we don't currently need it, do for BCS */
#define	LCALL(x,y)	.byte 0x9a ; .long y; .word x

#define	ASMSTR		.asciz

	.globl	cerror
