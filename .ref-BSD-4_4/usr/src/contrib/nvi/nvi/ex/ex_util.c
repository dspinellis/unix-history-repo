/*-
 * Copyright (c) 1993
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
static char sccsid[] = "@(#)ex_util.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"

/*
 * ex_getline --
 *	Return a line from the terminal.
 */
int
ex_getline(sp, fp, lenp)
	SCR *sp;
	FILE *fp;
	size_t *lenp;
{
	size_t off;
	int ch;
	char *p;

	for (off = 0, p = sp->ibp;; ++off) {
		ch = getc(fp);
		if (off >= sp->ibp_len) {
			BINC(sp, sp->ibp, sp->ibp_len, off + 1);
			p = sp->ibp + off;
		}
		if (ch == EOF || ch == '\n') {
			if (ch == EOF && !off)
				return (1);
			*lenp = off;
			return (0);
		}
		*p++ = ch;
	}
	/* NOTREACHED */
}

/*
 * set_altfname --
 *	Set the alternate file name.
 *
 * Swap the alternate file name.  The reason it's a routine is that I wanted
 * some place to hang this comment.  The alternate file name (normally
 * referenced using the special character '#' during file expansion) is set
 * by a large number of operations.  In the historic vi, the commands "ex",
 * "edit" and "next file" obviously set the alternate file name because they
 * switched the underlying file.  Less obviously, the "read", "file", "write"
 * and "wq" commands set it as well.  In this implementation, the new
 * commands "previous", "split", and "mkexrc" have been added to the list.
 * Where it gets interesting is that the alternate file name is set multiple
 * times by some commands.  If an edit attempt fails (for whatever reason,
 * like the current file is modified but as yet unwritten), it is set to the
 * file name that the user didn't get to edit.  If the edit succeeds, it is
 * set to the last file name that was edited.  Good fun.
 *
 * XXX: add errlist if it doesn't get ripped out.
 */
void
set_altfname(sp, altfname)
	SCR *sp;
	char *altfname;
{
	if (sp->altfname != NULL)
		FREE(sp->altfname, strlen(sp->altfname));
	if ((sp->altfname = strdup(altfname)) == NULL)
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
}
