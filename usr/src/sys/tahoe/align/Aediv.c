/*-
 * Copyright (c) 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)Aediv.c	7.1 (Berkeley) 12/6/90
 */

#include "align.h"
int	zzz1,zzz2,zzz3,zzz4,zzz5;
ediv(infop)	process_info *infop;
/*
/*	Extended precision division.
/*
/***************************************/
{
	register long Register_12;	/* Has to be the first reg !! */
	register long Register_11;	/* remainder */
	register long Register_10;	/* quotient */
	register long Register_9;	/* divident least */
	register long Register_8;	/* divident most */
	register long Register_7;	/* divisor */

	Register_7 = operand(infop, 0)->data;
	Register_8 = operand(infop, 1)->data;
	Register_9 = operand(infop, 1)->data2;
	Register_12=psl;
	Set_psl(r12);	/* restore the user psl */
	asm ("	ediv	r7,r8,r10,r11");
	asm ("	movpsl	r12");
	New_cc (Register_12);
	write_back (infop, Register_10, operand(infop, 2));
	write_back (infop, Register_11, operand(infop, 3));
}
