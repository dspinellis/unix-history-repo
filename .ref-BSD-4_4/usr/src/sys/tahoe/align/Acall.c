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
 *	@(#)Acall.c	7.1 (Berkeley) 12/6/90
 */

#include "align.h" 
call(infop)
process_info *infop;
/*
/*	Call a procedure with argument list on stack.
/*
/******************************************************/
{

	register long removed, mask, new_address, i, next, temp_fp;

	printf("entering call\n");
	removed = operand(infop, 0)->data & 0xffff ;
	printf("after first call to operand\n");
	new_address = operand(infop, 1)->address;
	printf("in call, removed = 0x%x , new_address=0x%x \n",removed, new_address);
	push (infop, fp);
	temp_fp = sp;
	mask = get_word (infop, new_address) & 0x1fff;  /* Only bits 12-0 */
	printf("in call, mask = 0x%x , pc=0x%x \n",mask,pc);
	push (infop, mask << 16 | removed);
	push (infop, pc);				/* Next opcode address */
	next = 12;				/* Register # to save */
	for (i = 0x1000; i != 0; i = i >> 1)
	{ 
		if ( i & mask ) push (infop,  Register (infop, next));
		next--; 
	}
	fp = temp_fp;
	pc = new_address + 2;			/* Transfer control */
}  
