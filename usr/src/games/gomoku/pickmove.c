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
static char sccsid[] = "@(#)pickmove.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <curses.h>

#include "gomoku.h"

struct	combostr *sortcombos[2];	/* combos at higher levels */
int	combolen[2];			/* number of combos in sortcombos */
int	nextcolor;			/* color of next move */

extern char pdir[];

pickmove(us)
	int us;
{
	register struct spotstr *sp, *sp1, *sp2;
	register union combo *Ocp, *Tcp;

	/* first move is easy */
	if (movenum == 1)
		return (PT(K,10));

	/* initialize all the board values */
	for (sp = &board[PT(T,20)]; --sp >= &board[PT(A,1)]; ) {
		sp->s_combo[BLACK].s = MAXCOMBO;
		sp->s_combo[WHITE].s = MAXCOMBO;
		sp->s_level[BLACK] = 255;
		sp->s_level[WHITE] = 255;
		sp->s_nforce[BLACK] = 0;
		sp->s_nforce[WHITE] = 0;
		sp->s_flg &= ~(FFLAGALL | MFLAGALL);
	}

	/* remove old combos */
	removecombos(BLACK);
	removecombos(WHITE);
	removeemptyspots();

	/* compute new values */
	nextcolor = us;
	scanframes(BLACK);
	scanframes(WHITE);

	/* find the spot with the highest value */
	for (sp = sp1 = sp2 = &board[PT(T,19)]; --sp >= &board[PT(A,1)]; ) {
		if (sp->s_occ != EMPTY)
			continue;
		if (debug && (sp->s_combo[BLACK].s < 0x200 ||
		    sp->s_combo[WHITE].s < 0x200)) {
			sprintf(fmtbuf, "- %s %x/%d %d %x/%d %d %d", stoc(sp - board),
				sp->s_combo[BLACK].s, sp->s_level[BLACK],
				sp->s_nforce[BLACK],
				sp->s_combo[WHITE].s, sp->s_level[WHITE],
				sp->s_nforce[WHITE],
				sp->s_wval);
			dlog(fmtbuf);
		}
		/* pick the best black move */
		if (better(sp, sp1, BLACK))
			sp1 = sp;
		/* pick the best white move */
		if (better(sp, sp2, WHITE))
			sp2 = sp;
	}
	if (debug) {
		sprintf(fmtbuf, "B %s %x/%d %d %x/%d %d %d %d",
			stoc(sp1 - board),
			sp1->s_combo[BLACK].s, sp1->s_level[BLACK],
			sp1->s_nforce[BLACK],
			sp1->s_combo[WHITE].s, sp1->s_level[WHITE],
			sp1->s_nforce[WHITE], sp1->s_wval, combolen[BLACK]);
		dlog(fmtbuf);
		sprintf(fmtbuf, "W %s %x/%d %d %x/%d %d %d %d",
			stoc(sp2 - board),
			sp2->s_combo[WHITE].s, sp2->s_level[WHITE],
			sp2->s_nforce[WHITE],
			sp2->s_combo[BLACK].s, sp2->s_level[BLACK],
			sp2->s_nforce[BLACK], sp2->s_wval, combolen[WHITE]);
		dlog(fmtbuf);
	}
	if (us == BLACK) {
		Ocp = &sp1->s_combo[BLACK];
		Tcp = &sp2->s_combo[WHITE];
	} else {
		Tcp = &sp1->s_combo[BLACK];
		Ocp = &sp2->s_combo[WHITE];
		sp = sp1;
		sp1 = sp2;
		sp2 = sp;
	}
	/*
	 * Block their combo only if we have to (i.e., if they are one move
	 * away from completing a force and we don't have a force that
	 * we can complete which takes fewer moves to win).
	 */
	if (Tcp->c.a <= 1 && (Ocp->c.a > 1 ||
	    Tcp->c.a + Tcp->c.b < Ocp->c.a + Ocp->c.b))
		return (sp2 - board);
	return (sp1 - board);
}

/*
 * Return true if spot 'sp' is better than spot 'sp1' for color 'us'.
 */
better(sp, sp1, us)
	struct spotstr *sp;
	struct spotstr *sp1;
	int us;
{
	int them;

	if (sp->s_combo[us].s < sp1->s_combo[us].s)
		return (1);
	if (sp->s_combo[us].s != sp1->s_combo[us].s)
		return (0);
	if (sp->s_level[us] < sp1->s_level[us])
		return (1);
	if (sp->s_level[us] != sp1->s_level[us])
		return (0);
	if (sp->s_nforce[us] > sp1->s_nforce[us])
		return (1);
	if (sp->s_nforce[us] != sp1->s_nforce[us])
		return (0);

	them = !us;
	if (sp->s_combo[them].s < sp1->s_combo[them].s)
		return (1);
	if (sp->s_combo[them].s != sp1->s_combo[them].s)
		return (0);
	if (sp->s_level[them] < sp1->s_level[them])
		return (1);
	if (sp->s_level[them] != sp1->s_level[them])
		return (0);
	if (sp->s_nforce[them] > sp1->s_nforce[them])
		return (1);
	if (sp->s_nforce[them] != sp1->s_nforce[them])
		return (0);

	if (sp->s_wval > sp1->s_wval)
		return (1);
	if (sp->s_wval != sp1->s_wval)
		return (0);

#ifdef SVR4
	return (rand() & 1);
#else
	return (random() & 1);
#endif
}

int		curcolor;	/* implicit parameter to makecombo() */
int		curlevel;	/* implicit parameter to makecombo() */

/*
 * Scan the sorted list of frames and update the minimum combo values.
 */
scanframes(color)
	int color;
{
	register struct combostr *cbp, *ecbp;
	register struct spotstr *sp;
	register union combo *cp;
	register struct elist *ep;
	register int i, r, d, n;
	union combo cb;

	curcolor = color;

	/* check for empty list of frames */
	cbp = sortframes[color];
	if (cbp == (struct combostr *)0)
		return;

	/* quick check for four in a row */
	sp = &board[cbp->c_vertex];
	cb.s = sp->s_fval[color][d = cbp->c_dir].s;
	if (cb.s < 0x101) {
		d = dd[d];
		for (i = 5 + cb.c.b; --i >= 0; sp += d) {
			if (sp->s_occ != EMPTY)
				continue;
			sp->s_combo[color].s = cb.s;
			sp->s_level[color] = 1;
		}
		return;
	}

	/* update the minimum combo value for each spot in the frame */
	n = combolen[color];
	ecbp = cbp;
	do {
		sp = &board[cbp->c_vertex];
		cp = &sp->s_fval[color][r = cbp->c_dir];
		d = dd[r];
		if (cp->c.b) {
			cb.c.a = cp->c.a + 1;
			cb.c.b = 0;
			if (cb.s < sp->s_combo[color].s) {
				sp->s_combo[color].s = cb.s;
				sp->s_level[color] = 1;
			}
			makecombo2(cbp, sp, cb.s);
			if (cp->s != 0x101)
				cb.s = cp->s;
			sp += d;
			i = 4;
		} else {
			cb.s = cp->s;
			i = 5;
		}
		for (; --i >= 0; sp += d) {	/* for each spot */
			if (sp->s_occ != EMPTY)
				continue;
			if (cp->s < sp->s_combo[color].s) {
				sp->s_combo[color].s = cp->s;
				sp->s_level[color] = 1;
			}
			if (cp->s == 0x101)
				sp->s_nforce[color]++;
			makecombo2(cbp, sp, cb.s);
		}
		/* mark frame as having been processed */
		board[cbp->c_vertex].s_flg |= MFLAG << r;
	} while ((cbp = cbp->c_next) != ecbp);

	/* try to make new 3rd level combos, 4th level, etc. */
	d = 2;
	while (combolen[color] > n) {
		if (debug) {
			sprintf(fmtbuf, "%cL%d %d", "BW"[color], d,
				combolen[color] - n);
			dlog(fmtbuf);
			refresh();
		}
		n = combolen[color];
		addframes(d);
		d++;
	}

	/* scan for combos at empty spots */
	for (sp = &board[PT(T,20)]; --sp >= &board[PT(A,1)]; ) {
		for (ep = sp->s_empty[color]; ep; ep = ep->e_next) {
			cbp = ep->e_combo;
			if (cbp->c_combo.s < sp->s_combo[color].s) {
				sp->s_combo[color].s = cbp->c_combo.s;
				sp->s_level[color] = cbp->c_nframes;
			} else if (cbp->c_combo.s == sp->s_combo[color].s &&
			    cbp->c_nframes < sp->s_level[color])
				sp->s_level[color] = cbp->c_nframes;
		}
	}
}

/*
 * Compute all level 2 combos of frames intersecting spot 'osp'
 * within the frame 'ocbp' and combo value 's'.
 */
makecombo2(ocbp, osp, s)
	struct combostr *ocbp;
	struct spotstr *osp;
	int s;
{
	register struct spotstr *sp, *fsp;
	register struct combostr *cbp;
	register int f, r, d, c;
	int baseB, bmask, n;
	union combo ocb, fcb;

	/* try to combine a new frame with those found so far */
	ocb.s = s;
	baseB = ocb.c.a + ocb.c.b - 1;
	for (r = 4; --r >= 0; ) {			/* for each direction */
	    /* don't include frames that overlap ones already included */
	    if (r == ocbp->c_dir)
		continue;
	    d = dd[r];
	    bmask = (BFLAG | FFLAG | MFLAG) << r;
	    fsp = osp;
	    for (f = 5; --f >= 0; fsp -= d) {		/* for each frame */
		/*
		 * Don't include frames that are blocked or
		 * part of a <1,x> combo.
		 */
		if (fsp->s_occ == BORDER)
		    break;
		if (fsp->s_flg & bmask)
		    continue;

		/* don't include frames of the wrong color */
		fcb.s = fsp->s_fval[curcolor][r].s;
		if (fcb.c.a >= MAXA)
		    continue;

		/*
		 * Get the combo value for this frame.
		 * If this is the end point of the frame,
		 * use the closed ended value for the frame.
		 */
		if (f == 4 && fcb.c.b || fcb.s == 0x101) {
		    fcb.c.a++;
		    fcb.c.b = 0;
		}

		/* compute combo value */
		c = fcb.c.a + ocb.c.a - 3;
		if (c > 3)
		    continue;
		n = fcb.c.a + fcb.c.b - 1;
		if (baseB < n)
		    n = baseB;

		/* make a new combo! */
		cbp = (struct combostr *)malloc(sizeof(struct combostr));
		cbp->c_combo.c.a = c;
		cbp->c_combo.c.b = n;
		cbp->c_link[0] = ocbp;
		cbp->c_link[1] = fsp->s_frame[r];
		cbp->c_vertex = osp - board;
		cbp->c_nframes = 2;
		cbp->c_dir = 0;
		cbp->c_flg = 0;
		cbp->c_refcnt = 0;

		if (c == 1 && debug > 1 || debug > 3) {
		    sprintf(fmtbuf, "%c %s %x/2 r%d f%d %x",
			curcolor == BLACK ? 'b' : 'w',
			stoc(osp - board), cbp->c_combo.s, r, f, ocb.s);
		    dlog(fmtbuf);
#ifdef DEBUG
		    markcombo(cbp, curcolor, 0);
		    bdisp();
		    whatsup(0);
		    clearcombo(cbp, curcolor, 0);
#endif
		}
		if (c > 1) {
		    if (ocb.c.a > 2)
			makeempty(cbp, ocbp, ocb.c.b);
		    if (fcb.c.a > 2)
			makeempty(cbp, cbp->c_link[1], fcb.c.b);

		    /* add the new combo to the end of the list */
		    appendcombo(cbp, curcolor);
		} else {
		    updatecombo(cbp, curcolor);
		    free(cbp);
		}
	    }
	}
}

/*
 * Scan the sorted list of frames and try to combine into combos.
 */
addframes(level)
	int level;
{
	register struct combostr *cbp, *ecbp;
	register struct spotstr *sp, *fsp;
	register int i, r, d;
	union combo fcb, cb;

	curlevel = level;

	/* clear MFLAG for this level */
	cbp = ecbp = sortframes[curcolor];
	do {
		board[cbp->c_vertex].s_flg &= ~MFLAGALL;
	} while ((cbp = cbp->c_next) != ecbp);

	cbp = ecbp;
	do {
		fsp = &board[cbp->c_vertex];
		r = cbp->c_dir;
		/* skip frames that are part of a <1,x> combo */
		if (fsp->s_flg & (FFLAG << r))
			continue;

		/*
		 * Don't include <1,x> combo frames,
		 * treat it as a blocked three in a row instead.
		 */
		fcb.s = fsp->s_fval[curcolor][r].s;
		if (fcb.s == 0x101)
			fcb.s = 0x200;

		/*
		 * If this is an open ended frame, use
		 * the combo value with the end closed.
		 */
		if (fsp->s_occ == EMPTY) {
			if (fcb.c.b) {
				cb.c.a = fcb.c.a + 1;
				cb.c.b = 0;
			} else
				cb.s = fcb.s;
			makecombo(cbp, fsp, cb.s);
		}

		/*
		 * The next four spots are handled the same for both
		 * open and closed ended frames.
		 */
		d = dd[r];
		sp = fsp + d;
		for (i = 4; --i >= 0; sp += d) {
			if (sp->s_occ != EMPTY)
				continue;
			makecombo(cbp, sp, fcb.s);
		}

		/* mark frame as having been processed */
		fsp->s_flg |= MFLAG << r;
	} while ((cbp = cbp->c_next) != ecbp);
}

/*
 * Compute all level N combos of frames intersecting spot 'osp'
 * within the frame 'ocbp' and combo value 's'.
 */
makecombo(ocbp, osp, s)
	struct combostr *ocbp;
	struct spotstr *osp;
	int s;
{
	register struct combostr *cbp, *ncbp;
	register struct spotstr *sp;
	register struct elist *ep;
	register int n, c;
	struct elist *nep, **epp;
	struct combostr *fcbp;
	int baseB, verts, d;
	union combo ocb, cb;

	ocb.s = s;
	baseB = ocb.c.a + ocb.c.b - 1;
	for (ep = osp->s_empty[curcolor]; ep; ep = ep->e_next) {
	    /* don't try to combine this combo if it is the wrong level */
	    cbp = ep->e_combo;
	    if (cbp->c_nframes > curlevel)
		continue;
	    if (cbp->c_nframes != curlevel)
		break;

	    /* don't include frames that overlap ones already included */
	    ncbp = ep->e_frame;
	    if (ncbp->c_dir == ocbp->c_dir ||
		(cbp->c_flg & C_LOOP) && cbp->c_dir == ocbp->c_dir ||
		(n = checkframes(cbp, ocbp, osp - board, ncbp)) < 0)
		    continue;

	    /* compute first half of combo value */
	    c = cbp->c_combo.c.a + ocb.c.a - 3;
	    if (c > 3)
		continue;

	    /* check to see if this frame forms a valid loop */
	    verts = 0;
	    if (n) {
		sp = &board[ocbp->c_vertex];
		d = dd[ocbp->c_dir];
		if (n = ocb.c.b)
			sp += d;
		for (; n < 5; n++, sp += d) {
		    if (sp->s_occ != EMPTY || sp == osp)
			continue;
		    for (nep = sp->s_empty[curcolor]; nep; nep = nep->e_next) {
			if (nep->e_combo == cbp) {
			    verts++;
			    fcbp = nep->e_frame;
			    continue;
			}
			if (nep->e_combo->c_nframes < cbp->c_nframes)
			    break;
		    }
		}
#if 0
		{
		char *str;
		sprintf(fmtbuf, "%c v%d %s%c",
		    curcolor == BLACK ? 'b' : 'w', verts,
		    stoc(ocbp->c_vertex), pdir[ocbp->c_dir]);
		str = fmtbuf + strlen(fmtbuf);
		for (; cbp->c_link[1]; cbp = cbp->c_link[0]) {
		    sprintf(str, " %s%c", stoc(cbp->c_link[1]->c_vertex),
			pdir[cbp->c_link[1]->c_dir]);
		    str += strlen(str);
		}
		sprintf(str, " %s%c", stoc(cbp->c_vertex), pdir[cbp->c_dir]);
		dlog(fmtbuf);
		cbp = ep->e_combo;
		}
#endif
		/* frame overlaps but not at a valid spot */
		if (verts == 0 || ocb.c.a < 3)
		    continue;
	    }

	    /* compute second half of combo value */
	    cb.s = board[ncbp->c_vertex].s_fval[curcolor][ncbp->c_dir].s;
	    n = cb.c.a + cb.c.b - 1;
	    if (baseB < n)
		n = baseB;

	    /* make a new combo! */
	    ncbp = (struct combostr *)malloc(sizeof(struct combostr));
	    c -= verts;
	    ncbp->c_combo.c.a = c;
	    ncbp->c_combo.c.b = n;
	    ncbp->c_link[0] = cbp;
	    ncbp->c_link[1] = ocbp;
	    ncbp->c_vertex = osp - board;
	    ncbp->c_nframes = cbp->c_nframes + 1;
	    ncbp->c_refcnt = 0;
	    if (verts) {
		ncbp->c_flg = C_LOOP;
		ncbp->c_dir = fcbp->c_dir;

		/* add the combo to the list of empty spots */
		nep = (struct elist *)malloc(sizeof(struct elist));
		nep->e_combo = ncbp;
		nep->e_frame = ocbp;
		if (debug > 2) {
		    sprintf(fmtbuf, "e %s", stoc(ncbp->c_vertex));
		    dlog(fmtbuf);
		}

		/* sort by the number of frames in the combo */
		epp = &board[ncbp->c_vertex].s_empty[curcolor];
		while (*epp) {
		    if (cbp->c_nframes >= (*epp)->e_combo->c_nframes)
			break;
		    epp = &(*epp)->e_next;
		}
		nep->e_next = *epp;
		*epp = nep;
	    } else {
		ncbp->c_flg = 0;
		ncbp->c_dir = 0;
		if (ocb.c.a > 2)
		    makeempty(ncbp, ocbp, ocb.c.b);
	    }
	    if (verts > 1 && debug || c == 1 && debug > 1 || debug > 2) {
		char *str;

		sprintf(fmtbuf, "%c %s%c %x v%d %x/%d",
		    curcolor == BLACK ? 'b' : 'w',
		    stoc(osp - board), pdir[ocbp->c_dir], ocb.s,
		    verts, ncbp->c_combo.s, ncbp->c_nframes);
		dlog(fmtbuf);
		str = fmtbuf;
		for (cbp = ncbp; cbp->c_link[1]; cbp = cbp->c_link[0]) {
		    sprintf(str, " %s%c", stoc(cbp->c_link[1]->c_vertex),
			pdir[cbp->c_link[1]->c_dir]);
		    str += strlen(str);
		}
		sprintf(str, " %s%c", stoc(cbp->c_vertex), pdir[cbp->c_dir]);
		dlog(fmtbuf);
#ifdef DEBUG
		markcombo(ncbp, curcolor, 0);
		bdisp();
		whatsup(0);
		clearcombo(ncbp, curcolor, 0);
#endif
	    }
	    if (c > 1) {
		/* add the new combo to the end of the list */
		appendcombo(ncbp, curcolor);
	    } else {
		updatecombo(ncbp, curcolor);
		free(ncbp);
	    }
	}
}

/*
 * Add the combostr 'cbp' to the empty spots list for each empty spot
 * in the frame 'fcbp' except for the intersection.
 * 's' is zero if 'fcbp' is a closed ended frame, else it is one.
 * Return the number of valid intersections with ocbp for detecting loops.
 */
makeempty(cbp, fcbp, s)
	struct combostr *cbp;
	struct combostr *fcbp;
	int s;
{
	struct spotstr *sp, *vsp;
	struct elist *ep, **epp;
	int d;

	if (debug > 2) {
		sprintf(fmtbuf, "E %s%c s%d", stoc(fcbp->c_vertex),
			pdir[fcbp->c_dir], s);
		sprintf(fmtbuf + strlen(fmtbuf), " %s", stoc(cbp->c_vertex));
		dlog(fmtbuf);
	}
	vsp = &board[cbp->c_vertex];
	sp = &board[fcbp->c_vertex];
	d = dd[fcbp->c_dir];
	if (s)
		sp += d;
	for (; s < 5; s++, sp += d) {
		if (sp->s_occ != EMPTY || sp == vsp)
			continue;

		/* add the combo to the list of empty spots */
		ep = (struct elist *)malloc(sizeof(struct elist));
		ep->e_combo = cbp;
		ep->e_frame = fcbp;
		if (debug > 2) {
			sprintf(fmtbuf, "e %s", stoc(sp - board));
			dlog(fmtbuf);
		}

		/* sort by the number of frames in the combo */
		epp = &sp->s_empty[curcolor];
		while (*epp) {
			if (cbp->c_nframes >= (*epp)->e_combo->c_nframes)
				break;
			epp = &(*epp)->e_next;
		}
		ep->e_next = *epp;
		*epp = ep;
	}
}

/*
 * Update the board value based on the combostr.
 * This is called only if 'cbp' is a <1,x> combo.
 * We handle things differently depending on whether the next move
 * would be trying to "complete" the combo or trying to block it.
 */
updatecombo(cbp, color)
	struct combostr *cbp;
	int color;
{
	register struct framestr *fp;
	register struct spotstr *sp;
	register struct combostr *tcbp;
	register int i, d;
	int nframes, vertex;
	union combo cb;

	for (; tcbp = cbp->c_link[1]; cbp = cbp->c_link[0]) {
		if (color == nextcolor) {
			/* update the board value for the vertex */
			sp = &board[cbp->c_vertex];
			sp->s_nforce[color]++;
			if (cbp->c_combo.s < sp->s_combo[color].s) {
				sp->s_combo[color].s = cbp->c_combo.s;
				sp->s_level[color] = cbp->c_nframes;
			} else if (cbp->c_combo.s == sp->s_combo[color].s &&
			    cbp->c_nframes < sp->s_level[color])
				sp->s_level[color] = cbp->c_nframes;
		} else {
			/* update the board values for each spot in frame */
			vertex = cbp->c_vertex;
			sp = &board[tcbp->c_vertex];
			if (sp->s_fval[color][tcbp->c_dir].c.b &&
			    tcbp->c_vertex != vertex)
				i = 6;
			else
				i = 5;
			d = dd[tcbp->c_dir];
			cb.s = cbp->c_combo.s;
			nframes = cbp->c_nframes;
			for (; --i >= 0; sp += d) {
				sp->s_nforce[color]++;
				if (cb.s < sp->s_combo[color].s) {
					sp->s_combo[color].s = cb.s;
					sp->s_level[color] = nframes;
				} else if (cb.s == sp->s_combo[color].s &&
				    cbp->c_nframes < sp->s_level[color])
					sp->s_level[color] = nframes;
			}
		}

		/* mark the frame as being part of a <1,x> combo */
		board[tcbp->c_vertex].s_flg |= FFLAG << tcbp->c_dir;
	}

	if (color != nextcolor) {
		/* update the board values for each spot in frame */
		sp = &board[cbp->c_vertex];
		if (sp->s_fval[color][cbp->c_dir].c.b &&
		    cbp->c_vertex != vertex)
			i = 6;
		else
			i = 5;
		d = dd[cbp->c_dir];
		for (; --i >= 0; sp += d) {
			sp->s_nforce[color]++;
			if (cb.s < sp->s_combo[color].s) {
				sp->s_combo[color].s = cb.s;
				sp->s_level[color] = nframes;
			} else if (cb.s == sp->s_combo[color].s &&
			    cbp->c_nframes < sp->s_level[color])
				sp->s_level[color] = nframes;
		}
	}

	/* mark the frame as being part of a <1,x> combo */
	board[cbp->c_vertex].s_flg |= FFLAG << cbp->c_dir;
}

/*
 * Free all elist structures.
 */
removeemptyspots()
{
	register struct elist *ep, *nep;
	register struct spotstr *sp;
	int i;

	for (sp = &board[PT(T,20)]; --sp >= &board[PT(A,1)]; ) {
		for (i = BLACK; i <= WHITE; i++) {
			for (ep = sp->s_empty[i]; ep; ep = nep) {
				nep = ep->e_next;
				free(ep);
			}
			sp->s_empty[i] = (struct elist *)0;
		}
	}
}

/*
 * Remove all combo structures.
 */
removecombos(color)
	int color;
{
	register struct combostr *cbp, *ncbp, *endcbp;

	/* check the list */
	if ((cbp = sortcombos[color]) == (struct combostr *)0)
		return;

	/* scan the list */
	endcbp = cbp;
	do {
		ncbp = cbp->c_next;
		cbp->c_next = (struct combostr *)0;
		cbp->c_prev = (struct combostr *)0;
		free(cbp);
		cbp = ncbp;
	} while (cbp != endcbp);

	sortcombos[color] = (struct combostr *)0;
	combolen[color] = 0;
}

/*
 * Add combo to the end of the list.
 */
appendcombo(cbp, color)
	struct combostr *cbp;
	int color;
{
	struct combostr *pcbp, *ncbp;

	combolen[color]++;
	ncbp = sortcombos[color];
	if (ncbp == (struct combostr *)0) {
		sortcombos[color] = cbp;
		cbp->c_next = cbp;
		cbp->c_prev = cbp;
		return;
	}
	pcbp = ncbp->c_prev;
	cbp->c_next = ncbp;
	cbp->c_prev = pcbp;
	ncbp->c_prev = cbp;
	pcbp->c_next = cbp;
}

/*
 * Return positive if frame 'fcbp' overlaps any of the frames in 'cbp'
 * other than the frame 'ecbp'.
 * Return -1 if any of the frames in 'cbp' are marked or part of a <1,x> combo.
 * Else, return zero.
 */
checkframes(cbp, fcbp, vertex, ecbp)
	struct combostr *cbp;
	struct combostr *fcbp;
	int vertex;
	struct combostr *ecbp;
{
	struct combostr *tcbp;
	char *str;
	int i, n, mask;
	short *ip;

	n = (fcbp - frames) * FAREA;
	str = &overlap[n];
	ip = &intersect[n];
	i = vertex == fcbp->c_vertex ? 2 : 0;
	for (; tcbp = cbp->c_link[1]; cbp = cbp->c_link[0]) {
#if 0
		if (board[tcbp->c_vertex].s_flg & (FFLAG << tcbp->c_dir))
			return (-1);
#endif
		vertex = cbp->c_vertex;
		if (tcbp == ecbp)
			continue;
		n = tcbp - frames;
		if (board[ip[n]].s_occ != EMPTY)
			continue;
		mask = str[n];
		if (mask & (1 << (i + (tcbp->c_vertex == vertex))))
			return (1);
	}
#if 0
	if (board[cbp->c_vertex].s_flg & (FFLAG << cbp->c_dir))
		return (-1);
#endif
	if (cbp == ecbp)
		return (0);
	n = cbp - frames;
	if (board[ip[n]].s_occ != EMPTY)
		return (0);
	mask = str[n];
	return (mask & (1 << (i + (cbp->c_vertex == vertex))));
}

#ifdef DEBUG
markcombo(cbp, color, vertex)
	struct combostr *cbp;
	int color;
	int vertex;
{
	register struct spotstr *sp;
	struct combostr *tcbp;
	int i, d, r, n, mask;

	for (; tcbp = cbp->c_link[1]; cbp = cbp->c_link[0])
		markcombo(tcbp, color, vertex = cbp->c_vertex);
	sp = &board[cbp->c_vertex];
	d = dd[r = cbp->c_dir];
	mask = (IFLAG | CFLAG) << r;
	n = sp->s_fval[color][r].c.b && cbp->c_vertex != vertex ? 6 : 5;
	for (i = 0; i < n; i++, sp += d) {
		if (n == 6 && (i == 0 || i == 5))
			sp->s_flg |= CFLAG << r;
		else
			sp->s_flg |= mask;
	}
}

clearcombo(cbp, color, vertex)
	struct combostr *cbp;
	int color;
	int vertex;
{
	register struct spotstr *sp;
	struct combostr *tcbp;
	int i, d, r, n, mask;

	for (; tcbp = cbp->c_link[1]; cbp = cbp->c_link[0])
		clearcombo(tcbp, color, vertex = cbp->c_vertex);
	sp = &board[cbp->c_vertex];
	d = dd[r = cbp->c_dir];
	mask = ~((IFLAG | CFLAG) << r);
	n = sp->s_fval[color][r].c.b && cbp->c_vertex != vertex ? 6 : 5;
	for (i = 0; i < n; i++, sp += d)
		sp->s_flg &= mask;
}
#endif /* DEBUG */
