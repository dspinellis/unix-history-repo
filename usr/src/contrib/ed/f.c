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
static char sccsid[] = "@(#)f.c	8.1 (Berkeley) 5/31/93";
#endif /* not lint */

#include <sys/types.h>

#include <limits.h>
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
 * Prints out or sets the remembered filename.
 */
void
f(inputt, errnum)
	FILE *inputt;
	int *errnum;
{
	char *l_temp;

	l_temp = filename(inputt, errnum);
	if (*errnum == 1) {
		sigspecial++;
			/* user wants the name from a sh command */
		if (l_temp && l_temp[FILENAME_LEN+1]) {
			FILE   *namestream, *popen();
			int l_len;

			if (l_temp[0] == '\0') {
				strcpy(help_msg, "no command given");
				*errnum = -1;
				return;
			}
			if (((namestream = popen(l_temp, "r")) == NULL) ||
			    ((fgets(l_temp, FILENAME_LEN - 1, namestream)) == NULL)) {
				strcpy(help_msg, "error executing command");
				*errnum = -1;
				if (namestream != NULL)
					pclose(namestream);
				ungetc('\n', inputt);
				return;
			}
			l_len = strlen(l_temp) - 1;
			if (l_temp[l_len] == '\n')
				l_temp[l_len] = '\0';
			pclose(namestream);
		}
		free(filename_current);
		filename_current = l_temp;
		sigspecial--;
		if (sigint_flag && (!sigspecial))
			SIGINT_ACTION;
	} else
		if (*errnum == -2)
			while (((ss = getc(inputt)) != '\n') || (ss == EOF));
		else
			if (*errnum < 0)
				return;
	if (filename_current)
		fwrite(filename_current,
		    sizeof(char), strlen(filename_current), stdout);
	putchar('\n');
	*errnum = 1;
}
