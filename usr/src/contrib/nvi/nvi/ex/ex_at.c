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
static char sccsid[] = "@(#)ex_at.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "excmd.h"

/*
 * ex_at -- :@[buffer]
 *	Execute the contents of the buffer.
 */
int
ex_at(sp, ep, cmdp)
	SCR *sp;
	EXF *ep;
	EXCMDARG *cmdp;
{
	CB *cb;
	TEXT *tp;
	int buffer, rval;

	if (cmdp->buffer == OOBCB) {
		if (sp->exat_lbuf == OOBCB) {
			msgq(sp, M_ERR, "No previous buffer to execute.");
			return (1);
		}
		buffer = sp->exat_lbuf;
	} else
		buffer = cmdp->buffer;

	CBNAME(sp, buffer, cb);
	CBEMPTY(sp, buffer, cb);
		
	if (sp->exat_recurse == 0)
		memset(sp->exat_stack, 0, sizeof(sp->exat_stack));
	else if (sp->exat_stack[buffer]) {
		msgq(sp, M_ERR,
		    "Buffer %s already occurs in this command.",
		    charname(sp, buffer));
		return (1);
	}

	sp->exat_stack[buffer] = 1;
	++sp->exat_recurse;

	for (tp = cb->txthdr.next; tp != (TEXT *)&cb->txthdr; tp = tp->next) {
		if (rval = ex_cstring(sp, ep, tp->lb, tp->len))
			break;
		if (F_ISSET(sp, S_MAJOR_CHANGE))
			break;
	}
		
	--sp->exat_recurse;
	return (0);
}
