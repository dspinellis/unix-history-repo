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
 *	@(#)Aregister.c	7.1 (Berkeley) 12/6/90
 */

#include "align.h"

/*
 * Return the specified register from the big structure.
 */

long
Register (infop, number)
process_info	*infop;
long		number;
{

	switch (number) {
		case 0:	return (r0);
		case 1:	return (r1);
		case 2:	return (r2);
		case 3:	return (r3);
		case 4:	return (r4);
		case 5:	return (r5);
		case 6:	return (r6);
		case 7:	return (r7);
		case 8:	return (r8);
		case 9:	return (r9);
		case 10:	return (r10);
		case 11:	return (r11);
		case 12:	return (r12);
		case 13:	return (fp);
		case 14:	return (sp);
		case 15:	return (pc);
	}
}


/*
 * Replace a given register with the given value.
 */
Replace (infop,number, newvalue)
process_info	*infop;
long		number;
long		newvalue;
{

	switch (number) {
		case 0:	r0 = newvalue; return;
		case 1:	r1 = newvalue; return;
		case 2:	r2 = newvalue; return;
		case 3:	r3 = newvalue; return;
		case 4:	r4 = newvalue; return;
		case 5:	r5 = newvalue; return;
		case 6:	r6 = newvalue; return;
		case 7:	r7 = newvalue; return;
		case 8:	r8 = newvalue; return;
		case 9:	r9 = newvalue; return;
		case 10:	r10 = newvalue; return;
		case 11:	r11 = newvalue; return;
		case 12:	r12 = newvalue; return;
		case 13:	fp = newvalue; return;
		case 14:	sp = newvalue & ~3; return;
		case 15:	pc = newvalue; return;
	}
}
