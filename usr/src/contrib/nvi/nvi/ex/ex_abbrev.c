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
static char sccsid[] = "@(#)ex_abbrev.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>

#include "vi.h"
#include "excmd.h"

/*
 * ex_abbr -- :abbreviate [key replacement]
 *	Create an abbreviation or display abbreviations.
 */
int
ex_abbr(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	register char *input, *output;

	if (cmdp->string == NULL) {
		if (seq_dump(sp, SEQ_ABBREV, 0) == 0)
			msgq(sp, M_ERR, "No abbreviations.");
		return (0);
	}

	/*
	 * Abbreviations can't be parsed by the upper-level parser because
	 * input is the first word and output is everything else, i.e. any
	 * space characters are included.
	 */
	for (input = cmdp->string; isspace(*input); ++input);
	for (output = input; *output && !isspace(*output); ++output);
	if (*output != '\0')
		for (*output++ = '\0'; isspace(*output); ++output);
	if (*output == '\0') {
		msgq(sp, M_ERR, "Usage: %s.", cmdp->cmd->usage);
		return (1);
	}

	if (seq_set(sp, NULL, input, output, SEQ_ABBREV, 1))
		return (1);
	F_SET(sp, S_ABBREV);
	return (0);
}

/*
 * ex_unabbr -- :unabbreviate key
 *      Delete an abbreviation.
 */
int
ex_unabbr(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
        EXCMDARG *cmdp;
{
	char *input;

	input = cmdp->argv[0];
	if (!F_ISSET(sp, S_ABBREV) || seq_delete(sp, input, SEQ_ABBREV)) {
		msgq(sp, M_ERR,
		    "\"%s\" was never an abbreviation.", input);
		return (1);
	}
	return (0);
}

/*
 * abbr_save --
 *	Save the abbreviation sequences to a file.
 */
int
abbr_save(sp, fp)
	SCR *sp;
	FILE *fp;
{
	return (seq_save(sp, fp, NULL, SEQ_ABBREV));
}
