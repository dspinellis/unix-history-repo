/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *	@(#)pxinfo.h	8.1 (Berkeley) 6/6/93
 */

/*
 * This is the information we get after the initial trap that px does.
 * By passing the "-d" flag, we cause px to call a procedure with the
 * the following information:
 *
 *	address of the display
 *	address of the display pointer
 *	address of the beginning of the object code
 *
 *	the address of the program counter used in the interpreter procedure
 *		(px actually keeps the pc in a register if it can, but stores
 *		 it in this location each time around the interpreter loop.)
 *	the address of the main interpreter loop (past the store of pc)
 */

#ifdef tahoe
typedef struct {
	short trp_savemask;
	short trp_removed;
	int trp_oldfp;
	ADDRESS *disp;
	ADDRESS *dp;
	ADDRESS objstart;
	ADDRESS pcaddr;
	ADDRESS loopaddr;
} TRAPARGS;
#else
typedef struct {
	int nargs;
	ADDRESS *disp;
	ADDRESS *dp;
	ADDRESS objstart;
	ADDRESS pcaddr;
	ADDRESS loopaddr;
} TRAPARGS;
#endif

ADDRESS *DISPLAY;
ADDRESS *DP;
ADDRESS ENDOFF;
ADDRESS PCADDR;
ADDRESS LOOPADDR;
#ifdef tahoe
ADDRESS RETLOC;
ADDRESS INTFP;
#endif
