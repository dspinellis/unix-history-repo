/*-
 * Copyright (c) 1991, 1993
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
static char sccsid[] = "@(#)v_at.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "vcmd.h"

int
v_at(sp, ep, vp, fm, tm, rp)
	SCR *sp;
	EXF *ep;
	VICMDARG *vp;
	MARK *fm, *tm, *rp;
{
	CB *cb;
	TEXT *tp;
	size_t len, remain;
	int key;
	char *p, *start;

	key = vp->character;
	CBNAME(sp, key, cb);
	CBEMPTY(sp, key, cb);

	if (sp->atkey_len == 0)
		memset(sp->atkey_stack, 0, sizeof(sp->atkey_stack));
	else if (sp->atkey_stack[key]) {
		msgq(sp, M_ERR, "Buffer %s already occurs in this command.",
		    charname(sp, key));
		return (1);
	}

	/* Get buffer for rest of at string plus cut buffer. */
	remain = sp->atkey_len ?
	    sp->atkey_len - (sp->atkey_cur - sp->atkey_buf) : 0;

	/* Check for overflow. */
	len = cb->len + remain;
	if (len < cb->len + remain) {
		msgq(sp, M_ERR, "Buffer overflow.");
		return (1);
	}

	if ((start = p = malloc(len)) == NULL) {
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (1);
	}

	/* Copy into the new buffer. */
	for (tp = cb->txthdr.next; tp != (TEXT *)&cb->txthdr; tp = tp->next) {
		memmove(p, tp->lb, tp->len);
		p += tp->len;
		*p++ = '\n';
	}
	
	/* Copy the rest of the current at string into place. */
	if (sp->atkey_len != 0) {
		memmove(p, sp->atkey_cur, remain);
		free(sp->atkey_buf);

	}
	/* Fix the pointers. */
	sp->atkey_buf = sp->atkey_cur = start;
	sp->atkey_len = len;

	sp->atkey_stack[key] = 1;

	*rp = *fm;
	return (0);
}
