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
static char sccsid[] = "@(#)add_line.c	8.1 (Berkeley) 5/31/93";
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
 * This is where the lines actually are put into the buffer.
 */
#ifdef STDIO
long
add_line(p, len)
	char *p;
	long len;
{
	extern int file_loc;
	long l_key;

	sigspecial++;
	if (file_seek)  /* x-ref to get_line for what this does */ {
		file_seek = 0;
		fseek(fhtmp, 0L, 2); /* set to end-to-file */
	}
	l_key = ftell(fhtmp);
					/* keeps user time down 20% approx. */
	file_loc = l_key + fwrite(p, sizeof(char), len, fhtmp);
	sigspecial--;
	return (l_key);
}
#endif

#ifdef DBI
recno_t
add_line(p, len)
	char *p;
	long len;
{
	DBT db_key, db_data;
	static recno_t l_key=0;

	sigspecial++;
	l_key++;
	(db_key.data) = &l_key;
	(db_key.size) = sizeof(recno_t);
	(db_data.data) = p;
	(db_data.size) = len;
	(dbhtmp->put)(dbhtmp, &db_key, &db_data, (u_int)(R_NOOVERWRITE));
	sigspecial--;
	return (l_key);
}
#endif

#ifdef MEMORY
char *
add_line(p, len)
	char *p;
	long len;
{
	char *tmp;

	sigspecial++;
	tmp = (char *)calloc(len+1, sizeof(char));
	if (tmp) {
		memmove(tmp, p, len);
		tmp[len] = '\0';
	}
	sigspecial--;
	return (tmp);
}
#endif
