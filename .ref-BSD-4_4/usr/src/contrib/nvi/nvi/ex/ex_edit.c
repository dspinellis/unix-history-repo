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
static char sccsid[] = "@(#)ex_edit.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "excmd.h"

/*
 * ex_edit --	:e[dit][!] [+cmd] [file]
 *	Edit a new file.
 */
int
ex_edit(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	EXF *tep;

	switch(cmdp->argc) {
	case 0:
		tep = ep;
		break;
	case 1:
		if ((tep = file_get(sp, ep, (char *)cmdp->argv[0], 1)) == NULL)
			return (1);
		set_altfname(sp, tep->name);
		break;
	default:
		abort();
	}

	MODIFY_CHECK(sp, ep, F_ISSET(cmdp, E_FORCE));

	/* Switch files. */
	F_SET(sp, F_ISSET(cmdp, E_FORCE) ? S_FSWITCH_FORCE : S_FSWITCH);
	sp->enext = tep;

	set_altfname(sp, ep->name);

	if (cmdp->plus)
		if ((tep->icommand = strdup(cmdp->plus)) == NULL)
			msgq(sp, M_ERR, "Command not executed: %s",
			    strerror(errno));
		else
			F_SET(tep, F_ICOMMAND);
	return (0);
}

/*
 * ex_visual --	:[line] vi[sual] [file]
 *		:vi[sual] [-^.] [window_size] [flags]
 *	Switch to visual mode.
 *
 * XXX
 * I have no idea what the legal flags are.
 * The second version of this command isn't implemented.
 */
int
ex_visual(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	EXF *tep;

	switch (cmdp->argc) {
	case 0:
		return (0);
	case 1:
		if ((tep = file_get(sp, ep, (char *)cmdp->argv[0], 1)) == NULL)
			return (1);
		set_altfname(sp, tep->name);
		break;
	default:
		abort();
	}

	MODIFY_CHECK(sp, ep, F_ISSET(cmdp, E_FORCE));

	/* Switch files. */
	F_SET(sp, F_ISSET(cmdp, E_FORCE) ? S_FSWITCH_FORCE : S_FSWITCH);
	sp->enext = tep;

	set_altfname(sp, ep->name);

	if (cmdp->plus)
		if ((tep->icommand = strdup(cmdp->plus)) == NULL)
			msgq(sp, M_ERR, "Command not executed: %s",
			    strerror(errno));
		else
			F_SET(tep, F_ICOMMAND);

	F_CLR(sp, S_MODE_EX);
	F_SET(sp, S_MODE_VI);
	return (0);
}
