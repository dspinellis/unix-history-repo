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
 */

#ifndef lint
static char sccsid[] = "@(#)pxerrors.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * px error messages
 */

char *pxerrmsg[] ={
	"not an error!",
	"argument to chr out of range",
	"div (integer divide) by zero",
	"real divide by zero",
	"call to procedure halt",
	"reference through a nil pointer",
	"tried to read past end-of-file",
	"negative parameter to sqrt",
	"pi/px error: stack not empty",
	"subscript out of range",
	"reference to an inactive file",
	"pi/px error: write failed",
	"pi/px error: create failed",
	"non-positive argument to ln",
	"pi/px error: bad op",
	"bad data on integer read",
	"pi/px error: active frame not found in goto",
	"label not found in case",
	"pi/px error: seek failed",
	"pi/px error: bad parameter to alloc",
	"out of memory",
	"constructed set parameter exceeds set bounds",
	"too many digits in number",
	"mod (integer remainder) by 0",
	"bad data on real read",
	"pi/px error: remove failed",
	"pi/px error: close failed",
	"pi/px error: open failed",
	"parameter to argv out of range",
	"bad i to pack(a, i, z)",
	"bad i to unpack(z, a, i)",
	"value out of range",
	"assertion failed",
	"tried to read, but open for writing",
	"tried to write, but open for reading",
	"integer number too large",
	"statement limit exceeded",
	"runtime stack overflow",
	"interrupt",
	"overflow, underflow, or divide by zero in arithmetic operation",
};
