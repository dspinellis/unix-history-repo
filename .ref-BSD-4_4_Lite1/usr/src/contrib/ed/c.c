/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Rodney Ruddock of the University of Guelph.
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
static char sccsid[] = "@(#)c.c	8.1 (Berkeley) 5/31/93";
#endif /* not lint */

#include <sys/types.h>

#include <regex.h>
#include <setjmp.h>
#include <stdio.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h"
#include "extern.h"

/*
 * This deletes the range of lines specified and then sets up things
 * for the central input routine.
 */

void
c(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	int l_flag=1;

	if (Start_default && End_default)
		Start = End = current;
	else
		if (Start_default)
			Start = End;
	if (Start == NULL) {
		*errnum = -1;
		return;
	}
	Start_default = End_default = 0;
	if (End == bottom)
		l_flag = 0;

	/* first delete the lines */
	d(inputt, errnum);
	if (*errnum < 0)
		return;
	*errnum = 0;

	/*if ((current != NULL) && (current != bottom)) RMSR */
	if ((current != NULL) && l_flag)
		current = current->above;
	add_flag = 1;
	Start_default = End_default = 1;
	/* now get the "change" lines */
	input_lines(inputt, errnum);
	add_flag = 0;
}
