/*-
 * Copyright (c) 1992, 1993
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
static char sccsid[] = "@(#)ex_map.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "excmd.h"

/*
 * ex_map -- :map[!] [key replacement]
 *	Map a key or display mapped keys.
 */
int
ex_map(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	register int ch;
	register char *input, *output;
	enum seqtype stype;
	int key;
	char *name, *s, buf[10];

	stype = F_ISSET(cmdp, E_FORCE) ? SEQ_INPUT : SEQ_COMMAND;

	if (cmdp->string == NULL) {
		if (seq_dump(sp, stype, 1) == 0)
			msgq(sp, M_ERR, "No map entries.");
		return (0);
	}

	/*
	 * Input is the first word, output is everything else, i.e. any space
	 * characters are included.  This is why we can't parse this command
	 * in the main parser.
	 */
	for (input = cmdp->string; isspace(*input); ++input);
	for (output = input; (ch = *output) && !isspace(ch); ++output);
	if (*output != '\0')
		for (*output++ = '\0'; isspace(*output); ++output);
	if (*output == '\0') {
		msgq(sp, M_ERR, "Usage: %s.", cmdp->cmd->usage);
		return (1);
	}
	
	/*
	 * If the mapped string is #[0-9], then map to a function
	 * key.
	 */
	if (input[0] == '#' && isdigit(input[1]) && !input[2]) {
		key = atoi(input + 1);
		(void)snprintf(buf, sizeof(buf), "f%d", key);
#ifdef notdef
		if (FKEY[key]) {		/* CCC */
			input = FKEY[key];
			name = buf;
		} else {
			msgq(sp, M_ERR, "This terminal has no %s key.", buf);
			return (1);
		}
#else
		name = NULL;
#endif
	} else {
		name = NULL;

		/* Some single keys may not be remapped in command mode. */
		if (stype == SEQ_COMMAND && input[1] == '\0')
			switch (sp->special[input[0]]) {
			case K_COLON:
				s = ":";
				goto noremap;
			case K_CR:
				s = "\\r";
				goto noremap;
			case K_ESCAPE:
				s = "^[";
				goto noremap;
			case K_NL:
				s = "\\n";
noremap:			msgq(sp, M_ERR,
				    "The %s character may not be remapped.", s);
				return (1);
			}
	}
	return (seq_set(sp, name, input, output, stype, 1));
}

/*
 * ex_unmap -- (:unmap[!] key)
 *	Unmap a key.
 */
int
ex_unmap(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	char *input;

	input = cmdp->argv[0];
	if (seq_delete(sp,
	    input, F_ISSET(cmdp, E_FORCE) ? SEQ_INPUT : SEQ_COMMAND)) {
		msgq(sp, M_ERR, "\"%s\" isn't mapped.", input);
		return (1);
	}
	return (0);
}

/*
 * map_save --
 *	Save the mapped sequences to a file.
 */
int
map_save(sp, fp)
	SCR *sp;
	FILE *fp;
{
	if (seq_save(sp, fp, "map ", SEQ_COMMAND))
		return (1);
	return (seq_save(sp, fp, "map! ", SEQ_INPUT));
}
