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
 *	@(#)exception.c	7.1 (Berkeley) 12/6/90
 */

#include "align.h" 

/*
 * Signal an exception. It will be handled by 'locore.s'. Here, I:
 *	1) Put the exception code where it belongs on the stack.
 *	2) Restore pc and sp to show that the current opcode
 *		'was not executed'.
 *	3) Execute one big non-local-goto. In the process we take care
 *		to reset the current HW fp such that 'alignment' will
 *		indeed return to 'locore.s'.
 *		IMPORTANT NOTE : the process I use will NOT restore
 *		all registers (like normal returns) so the call to the
 *		handling routine HAS TO BE the last thing in 'alignment'.
 *		Otherwise, all its own register variables will be a mess !!
 *		I also know that 'alignment' itself WILL restore all
 *		registers for 'locore.s' since its entry mask is all-1.
 */
exception(infop, type, param1, param2)
process_info *infop;
int	type, param1, param2;
{
	register long *my_fp;
	register long *current_fp, *prev_fp;

	my_fp = (long *)&infop-1 ;
	infop->ret_exception = type;
	switch (type) {
	case ARITHMETIC:
		infop->ret_code = param1;
		break;
	case ILL_ACCESS:
		infop->ret_addr = param1;
		infop->ret_code = param2;
		break;
	case ALIGNMENT:
	case ILL_ADDRMOD:
	case ILL_OPRND:
		break;
	default :
		printf ("Bad exception type %d (alignment code)\n", type);
		break;
	}
/*
 * Now the big trick. Look up the stack until the frame of
 * 'alignment' is found. prev_fp will point to it and current_fp
 * will then point to the frame of whoever 'alignment' called.
 * This should better work ...
 */
	prev_fp = my_fp;
	while (prev_fp != &fp) {
		current_fp = prev_fp;
		prev_fp = (long *) *prev_fp;
	}
/*
 * Found it. Now fool the HW into thinking that 'alignment' called
 * us directly here, so this routine's 'return' will go back
 * all the way to 'alignment', stopping any further emulation
 * for the current offending opcode.
 *  "fool the HW..." ha ha, am I realy fooling myself ?
 */
	*my_fp = *current_fp;
	*(my_fp - 2) = *(current_fp -2);	/* Alter program counter */
/*
 * Without further ado, just go back now !!!!
 */
}

not_needed (infop)
process_info *infop;
{
/*
 * Shouldn't ever come to this routine.
 */

	printf ("Opcode 0x%x should not trap to alignment code.",
		opCODE);
	printf (" OS or machine problem!! \n");
}


cannot_do (infop)
process_info *infop;
{
/*
 * Some opcode-caused alignments cannot be emulated. See table.c for
 * specific reasons. Reflect this back to the process as alignment
 * exception.
 */
	exception (infop, ALIGNMENT);
}
