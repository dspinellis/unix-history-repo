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
static char sccsid[] = "@(#)svi_split.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <curses.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

#include "vi.h"
#include "svi_screen.h"

/*
 * svi_split --
 *	Split the screen.  Keep the cursor line on the screen, and make
 *	reassembly easy by overallocating on the line maps and linking
 *	from top of the real screen to the bottom.
 */
int
svi_split(sp, ep)
	SCR *sp;
	EXF *ep;
{
	SCR *tsp;
	size_t half;

	/* Check to see if it's possible. */
	half = sp->rows / 2;
	if (half < MINIMUM_SCREEN_ROWS) {
		msgq(sp, M_ERR, "Screen not large enough to split");
		return (1);
	}

	/* Get a new screen, initialize. */
	if ((tsp = malloc(sizeof(SCR))) == NULL) {
		msgq(sp, M_ERR, "Error: %s", strerror(errno));
		return (1);
	}
	if (scr_init(sp, tsp)) {
		free(tsp);
		return (1);
	}

	/* Start the file. */
	if ((tsp->ep = file_start(tsp, ep, NULL)) == NULL) {
		free(tsp);
		return (1);
	}

	/*
	 * Build a screen map for the child -- large enough to accomodate
	 * the entire window.
	 */
	if ((tsp->h_smap = malloc(sp->w_rows * sizeof(SMAP))) == NULL) {
		free(tsp);
		return (1);
	}

	/* Split the screen, and link the screens together. */
	if (sp->sc_row <= half) {		/* Parent is top half. */
		tsp->rows = sp->rows - half;	/* Child. */
		tsp->cols = sp->cols;
		tsp->t_rows = tsp->rows - 1;
		tsp->w_rows = sp->w_rows;
		tsp->s_off = sp->s_off + half;
		tsp->sc_col = tsp->sc_row = 0;
		tsp->t_smap = tsp->h_smap + (tsp->t_rows - 1);

		sp->rows = half;		/* Parent. */
		sp->t_rows = sp->rows - 1;
		sp->t_smap = sp->h_smap + (sp->t_rows - 1);

		tsp->child = sp->child;
		if (sp->child != NULL)
			sp->child->parent = tsp;
		sp->child = tsp;
		tsp->parent = sp;
	} else {				/* Parent is bottom half. */
		tsp->rows = sp->rows - half;	/* Child. */
		tsp->cols = sp->cols;
		tsp->t_rows = tsp->rows - 1;
		tsp->w_rows = sp->w_rows;
		tsp->s_off = sp->s_off;
		tsp->sc_col = tsp->sc_row = 0;
		tsp->t_smap = tsp->h_smap + (tsp->t_rows - 1);

		sp->rows = half;		/* Parent. */
		sp->t_rows = sp->rows - 1;
		sp->s_off = sp->s_off + half;
		sp->t_smap = sp->h_smap + (sp->t_rows - 1);

		tsp->parent = sp->parent;
		if (sp->parent != NULL)
			sp->parent->child = tsp;
		sp->parent = tsp;
		tsp->child = sp;
	}

	/* Init support routines. */
	svi_init(tsp);

	/* Fill the child's screen map. */
	(void)svi_sm_fill(tsp, tsp->ep, sp->lno, P_FILL);

	/* Clear the information lines. */
	MOVE(sp, INFOLINE(sp), 0);
	clrtoeol();
	MOVE(tsp, INFOLINE(sp), 0);
	clrtoeol();

	/* Refresh the parent screens, displaying the status line. */
	(void)status(sp, sp->ep, sp->lno, 0);
	(void)svi_refresh(sp, sp->ep);

	/* Completely redraw the child screen. */
	F_SET(tsp, S_REDRAW);

	/* Switch screens. */
	sp->snext = tsp;
	F_SET(sp, S_SSWITCH);
	return (0);
}
