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
static char sccsid[] = "@(#)get_line.c	8.1 (Berkeley) 5/31/93";
#endif /* not lint */

#include <sys/types.h>

#include <regex.h>
#include <setjmp.h>
#include <stdio.h>
#include <string.h>

#ifdef DBI
#include <db.h>
#endif

#include "ed.h"
#include "extern.h"

/*
 * Get the specified line from the buffer. Note that in the buffer
 * we stored lengths of text, not strings (NULL terminated, except
 * under MEMORY). So we want to put a terminating NULL in for
 * whatever command is going to be handling the line.
 */

/* these variables are here (instead of main and ed.h) because they
 * are only used with the buffer when run under STDIO. STDIO is a
 * resource pig with most of the OS's I've tested this with. The
 * use of these variables improves performance up to 100% in several
 * cases. The piggyness is thus: fseek causes the current STDIO buf
 * to be flushed out and a new one read in...even when it is not necessary!
 * Read 512 (or 1024) when you don't have to for almost every access
 * and you'll slow down too. So these variable are used to control unneeded
 * fseeks.
 * I've been told the newer BSD STDIO has fixed this, but don't
 * currently have a copy.
 */
int file_loc=0;

/* Get a particular line of length len from ed's buffer and place it in
 * 'text', the standard repository for the "current" line.
 */
void
get_line(loc, len)
#ifdef STDIO
	long loc;
#endif
#ifdef DBI
	recno_t loc;
#endif
#ifdef MEMORY
	char *loc;
#endif
	int len;
{
#ifdef DBI
	DBT db_key, db_data;
#endif
	int l_jmp_flag;

	if (l_jmp_flag = setjmp(ctrl_position2)) {
		text[0] = '\0';
		return;
	}

	sigspecial2 = 1;
#ifdef STDIO

	if (loc == 0L) {
		text[0] = '\0';
		return;
	}
	if (file_loc != loc)
		fseek(fhtmp, loc, 0);
	file_seek = 1;
	file_loc = loc + fread(text, sizeof(char), len, fhtmp);
#endif

#ifdef DBI
	if (loc == (recno_t)0) {
		text[0] = '\0';
		return;
	}
	(db_key.data) = &loc;
	(db_key.size) = sizeof(recno_t);
	(dbhtmp->get) (dbhtmp, &db_key, &db_data, (u_int) 0);
	strcpy(text, db_data.data);
#endif

#ifdef MEMORY
        if (loc == (char *)NULL) {
                text[0] = '\0';
                return;
        }
	memmove(text, loc, len);
#endif
	text[len] = '\0';
	sigspecial2 = 0;
}
