/*
 * Copyright (c) 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)makemove.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "gomoku.h"

		/* direction deltas */
int     dd[4] = {
	MRIGHT, MRIGHT+MDOWN, MDOWN, MDOWN+MLEFT
};

int	weight[5] = { 0, 1, 7, 22, 100 };

/*
 * Return values:
 *	MOVEOK	everything is OK.
 *	RESIGN	Player resigned.
 *	ILLEGAL	Illegal move.
 *	WIN	The the winning move was just played.
 *	TIE	The game is a tie.
 */
makemove(us, mv)
	int us, mv;
{
	register struct spotstr *sp, *fsp;
	register union combo *cp;
	struct spotstr *osp;
	struct combostr *cbp, *cbp1;
	union combo *cp1;
	register int i, f, r, d, n;
	int space, val, bmask;

	/* check for end of game */
	if (mv == RESIGN)
		return(RESIGN);

	/* check for illegal move */
	sp = &board[mv];
	if (sp->s_occ != EMPTY)
		return(ILLEGAL);

	/* make move */
	sp->s_occ = us;
	movelog[movenum - 1] = mv;
	if (++movenum == BSZ * BSZ)
		return(TIE);

	/* compute new frame values */
	sp->s_wval = 0;
	osp = sp;
	for (r = 4; --r >= 0; ) {			/* for each direction */
	    d = dd[r];
	    fsp = osp;
	    bmask = BFLAG << r;
	    for (f = 5; --f >= 0; fsp -= d) {		/* for each frame */
		if (fsp->s_occ == BORDER)
		    goto nextr;
		if (fsp->s_flg & bmask)
		    continue;

		/* remove this frame from the sorted list of frames */
		cbp = fsp->s_frame[r];
		if (cbp->c_next) {
			if (sortframes[BLACK] == cbp)
			    sortframes[BLACK] = cbp->c_next;
			if (sortframes[WHITE] == cbp)
			    sortframes[WHITE] = cbp->c_next;
			cbp->c_next->c_prev = cbp->c_prev;
			cbp->c_prev->c_next = cbp->c_next;
		}

		/* compute old weight value for this frame */
		cp = &fsp->s_fval[BLACK][r];
		if (cp->s <= 0x500)
		    val = weight[5 - cp->c.a - cp->c.b];
		else
		    val = 0;
		cp = &fsp->s_fval[WHITE][r];
		if (cp->s <= 0x500)
		    val += weight[5 - cp->c.a - cp->c.b];

		/* compute new combo value for this frame */
		sp = fsp;
		space = sp->s_occ == EMPTY;
		n = 0;
		for (i = 5; --i >= 0; sp += d) {	/* for each spot */
		    if (sp->s_occ == us)
			n++;
		    else if (sp->s_occ == EMPTY)
			sp->s_wval -= val;
		    else {
			/* this frame is now blocked, adjust values */
			fsp->s_flg |= bmask;
			fsp->s_fval[BLACK][r].s = MAXCOMBO;
			fsp->s_fval[WHITE][r].s = MAXCOMBO;
			while (--i >= 0) {
			    sp += d;
			    if (sp->s_occ == EMPTY)
				sp->s_wval -= val;
			}
			goto nextf;
		    }
		}

		/* check for game over */
		if (n == 5)
		    return(WIN);

		/* compute new value & combo number for this frame & color */
		fsp->s_fval[!us][r].s = MAXCOMBO;
		cp = &fsp->s_fval[us][r];
		/* both ends open? */
		if (space && sp->s_occ == EMPTY) {
		    cp->c.a = 4 - n;
		    cp->c.b = 1;
		} else {
		    cp->c.a = 5 - n;
		    cp->c.b = 0;
		}
		val = weight[n];
		sp = fsp;
		for (i = 5; --i >= 0; sp += d)		/* for each spot */
		    if (sp->s_occ == EMPTY)
			sp->s_wval += val;

		/* add this frame to the sorted list of frames by combo value */
		cbp1 = sortframes[us];
		if (!cbp1)
		    sortframes[us] = cbp->c_next = cbp->c_prev = cbp;
		else {
		    cp1 = &board[cbp1->c_vertex].s_fval[us][cbp1->c_dir];
		    if (cp->s <= cp1->s) {
			/* insert at the head of the list */
			sortframes[us] = cbp;
		    } else {
			do {
			    cbp1 = cbp1->c_next;
			    cp1 = &board[cbp1->c_vertex].s_fval[us][cbp1->c_dir];
			    if (cp->s <= cp1->s)
				break;
			} while (cbp1 != sortframes[us]);
		    }
		    cbp->c_next = cbp1;
		    cbp->c_prev = cbp1->c_prev;
		    cbp1->c_prev->c_next = cbp;
		    cbp1->c_prev = cbp;
		}

	    nextf:
		;
	    }

	    /* both ends open? */
	    if (fsp->s_occ == EMPTY) {
		cp = &fsp->s_fval[BLACK][r];
		if (cp->c.b) {
		    cp->c.a += 1;
		    cp->c.b = 0;
		}
		cp = &fsp->s_fval[WHITE][r];
		if (cp->c.b) {
		    cp->c.a += 1;
		    cp->c.b = 0;
		}
	    }

	nextr:
	    ;
	}

	return(MOVEOK);
}
