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
static char sccsid[] = "@(#)getc.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include "vi.h"
#include "vcmd.h"

#define	GB	ep->getc_bp
#define	GL	ep->getc_blen
#define	GM	ep->getc_m

/*
 * getc_init --
 *	Initialize getc routines.
 */
int
getc_init(sp, ep, fm, chp)
	SCR *sp;
	EXF *ep;
	MARK *fm;
	int *chp;
{
	recno_t lno;

	GM = *fm;
	if ((GB = file_gline(sp, ep, fm->lno, &GL)) == NULL) {
		if (file_lline(sp, ep, &lno))
			return (1);
		if (lno == 0)
			v_eol(sp, ep, NULL);
		else
			GETLINE_ERR(sp, fm->lno);
		return (1);
	}
	*chp = GL == 0 ? EMPTYLINE : GB[GM.cno];
	return (0);
}

/*
 * getc_next --
 *	Retrieve the next character.
 */
int
getc_next(sp, ep, dir, chp)
	SCR *sp;
	EXF *ep;
	enum direction dir;
	int *chp;
{
	MARK save;

	save = GM;
	switch (dir) {
	case FORWARD:
		if (GL == 0 || GM.cno == GL - 1) {
			GM.cno = 0;		/* EOF; restore the cursor. */
			if ((GB = file_gline(sp, ep, ++GM.lno, &GL)) == NULL) {
				GM = save;
				return (0);
			}
			if (GL == 0) {
				*chp = EMPTYLINE;
				return (1);
			}
		} else
			++GM.cno;
		break;
	case BACKWARD:
		if (GM.cno == 0) {		/* EOF; restore the cursor. */
			if ((GB = file_gline(sp, ep, --GM.lno, &GL)) == NULL) {
				GM = save;
				return (0);
			}
			if (GL == 0) {
				*chp = EMPTYLINE;
				return (1);
			}
			GM.cno = GL - 1;
		} else
			--GM.cno;
		break;
	default:
		abort();
	}
	*chp = GB[GM.cno];
	return (1);
}

/*
 * getc_set --
 *	Return the last cursor position.
 */
void
getc_set(sp, ep, rp)
	SCR *sp;
	EXF *ep;
	MARK *rp;
{
	*rp = GM;
}
