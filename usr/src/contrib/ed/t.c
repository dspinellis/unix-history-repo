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
static char sccsid[] = "@(#)t.c	8.1 (Berkeley) 5/31/93";
#endif /* not lint */

#include <sys/types.h>

#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h"
#include "extern.h"

/*
 * The transcribe function. POSIX calls it copy, but 't' for transcribe
 * is more mneumonic and that's what I've always called it. Transcribes
 * the spec'd lines into the buffer at the spec'd location.
 */

void
t(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	LINE *l_ptr, *l_tb, *l_te, *l_temp1, *l_temp2, *l_dest=NULL;

	l_tb = NULL;
	l_te = NULL;

	if (((ss = getc(inputt)) != '\n') && (ss != EOF)) {
		for (;;) {
			if (ss != ' ') {
				ungetc(ss, inputt);
				break;
			}
			ss = getc(inputt);
		}
		l_dest = address_conv(NULL, inputt, errnum);
	} else
		(ungetc(ss, inputt), *errnum = -1);

	if (*errnum < 0) {
		strcpy(help_msg, "bad destination address");
		return;
	}
	*errnum = 0;
	if (rol(inputt, errnum))
		return;

	if (Start_default && End_default)
		Start = End = current;
	else
		if (Start_default)
			Start = End;
	if (Start == NULL) {
		strcpy(help_msg, "empty buffer");
		*errnum = -1;
		return;
	}
	Start_default = End_default = 0;

	if (g_flag == 0)
		u_clr_stk();

	sigspecial++;

	for (l_ptr = Start; l_ptr != (End->below); l_ptr = (l_ptr->below)) {
		get_line(l_ptr->handle, l_ptr->len);
		if (sigint_flag && (!sigspecial))
			break;
		l_temp1 = (LINE *) malloc(sizeof(LINE));
		if (l_temp1 == NULL) {
			*errnum = -1;
			strcpy(help_msg, "out of memory error");
			return;
		}
		if (l_tb == NULL) {
			l_tb = l_temp1;
			(l_temp1->above) = NULL;
			(l_temp1->below) = NULL;
		} else {
			(l_temp1->above) = l_te;
			(l_temp1->below) = NULL;
			(l_te->below) = l_temp1;
		}
		l_te = l_temp1;
		(l_temp1->len) = l_ptr->len;
		/* add it into the buffer at the spec'd location */
		(l_temp1->handle) = add_line(text, l_ptr->len);
		if (sigint_flag && (!sigspecial))
			break;
	}

	if (l_dest == NULL)
		l_temp2 = top;
	else {
		u_add_stk(&(l_dest->below));
		l_temp2 = l_dest->below;
	}

	if (l_dest == NULL) {
		u_add_stk(&(top->above));
		(top->above) = l_tb;
		top = l_tb;
		(l_tb->above) = NULL;
	} else {
		(l_tb->above) = l_dest;
		(l_dest->below) = l_tb;
	}

	if (l_dest == bottom) {
		bottom = l_te;
		(l_te->below) = NULL;
	} else {
		(l_te->below) = l_temp2;
		u_add_stk(&(l_temp2->above));
		(l_temp2->above) = l_te;
	}

	current = l_te;
	change_flag = 1;
	sigspecial--;
	*errnum = 1;
	return;
}
