/*-
 * Copyright (c) 1980, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)swap.c	5.13 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/dmap.h>

#include <nlist.h>
#include "systat.h"
#include "extern.h"

long ntext, textp;
struct text *xtext;

static void swaplabel __P((int, int, int));
static void vsacct __P((struct dmap *));
static int swapdisplay __P((int, int, int));
static int dmtoindex __P((int));

WINDOW *
openswap()
{
	return (subwin(stdscr, LINES-5-1, 0, 5, 0));
}

void
closeswap(w)
	WINDOW *w;
{
	if (w == NULL)
		return;
	wclear(w);
	wrefresh(w);
	delwin(w);
}

int	dmmin;
int	dmmax;
int	dmtext;
int	nswdev;
#define	MAXSWAPDEV	4
short	buckets[MAXSWAPDEV][NDMAP];
struct	swdevt *swdevt;
int	colwidth;

void
showswap()
{
#ifdef notdef	
	register int i, j;
	register struct proc *pp;
	register struct text *xp;
	register int row;
	register int ts;
	register swblk_t *dp;

	if (xtext == 0)
		return;
	for (xp = xtext; xp < &xtext[ntext]; xp++) {
		if (xp->x_vptr == NULL)
			continue;
		ts = ctod(xp->x_size);
		dp = xp->x_daddr;
		for (i = 0; i < ts; i += dmtext) {
			j = ts - i;
			if (j > dmtext)
				j = dmtext;
#define	swatodev(addr)	(((addr) / dmmax) % nswdev)
			buckets[swatodev(*dp)][dmtoindex(j)]++;
			dp++;
		}
		if ((xp->x_flag & XPAGV) && xp->x_ptdaddr)
			buckets[swatodev(xp->x_ptdaddr)]
			    [dmtoindex(ctod(ctopt(xp->x_size)))]++;
	}
	row = swapdisplay(2, dmtext, 'X');
	if (kprocp == NULL)
		return;
	/* TODO - traverse procs { */
		if (pp->p_stat == 0 || pp->p_stat == SZOMB)
			continue;
		if (pp->p_flag & SSYS)
			continue;
		if (getu(pp) == 0)
			continue;
		vsacct(&u.u_dmap);
		vsacct(&u.u_smap);
		if ((pp->p_flag & SLOAD) == 0)
			vusize(pp);
	}
	(void) swapdisplay(1+row, dmmax, 'X');
#endif
}

#define	OFFSET	5			/* left hand column */

static int
swapdisplay(baserow, dmbound, c)
	int baserow, dmbound, c;
{
#ifdef notdef	
	register int i, j, k, row;
	register short *pb;
	char buf[10];

	for (row = baserow, i = dmmin; i <= dmbound; i *= 2, row++) {
		for (j = 0; j < nswdev; j++) {
			pb = &buckets[j][row - baserow];
			wmove(wnd, row, OFFSET + j * (1 + colwidth));
			k = MIN(*pb, colwidth);
			if (*pb > colwidth) {
				sprintf(buf, " %d", *pb);
				k -= strlen(buf);
				while (k--)
					waddch(wnd, c);
				waddstr(wnd, buf);
			} else {
				while (k--)
					waddch(wnd, c);
				k = MAX(colwidth - *pb, 0);
				while (k--)
					waddch(wnd, ' ');
			}
			*pb = 0;
		}
	}
	return (row);
#endif
}

static void
vsacct(dmp)
	register struct dmap *dmp;
{
#ifdef notdef	
	register swblk_t *ip;
	register int blk = dmmin, index = 0;

	for (ip = dmp->dm_map; dmp->dm_alloc > 0; ip++) {
		if (ip - dmp->dm_map >= NDMAP) {
			error("vsacct NDMAP");
			break;
		}
		if (*ip == 0)
			error("vsacct *ip == 0");
		buckets[swatodev(*ip)][index]++;
		dmp->dm_alloc -= blk;
		if (blk < dmmax) {
			blk *= 2;
			index++;
		}
	}
#endif
}

static int
dmtoindex(dm)
	int dm;
{
#ifdef notdef	
	register int i, j;

	for (j = 0, i = dmmin; i <= dmmax; i *= 2, j++)
		if (dm <= i)
			return (j);
	error("dmtoindex(%d)", dm);
	return (NDMAP - 1);
#endif
}

static struct nlist namelist[] = {
#define X_FIRST		0
#define X_NSWAP         0
	{ "_nswap" },
#define X_DMMIN         1
	{ "_dmmin" },
#define X_DMMAX         2
	{ "_dmmax" },
#define	X_DMTEXT	3
	{ "_dmtext" },
#define X_NSWDEV        4
	{ "_nswdev" },
#define	X_SWDEVT	5
	{ "_swdevt" },
#define	X_NTEXT		6
	{ "_ntext" },
#define	X_TEXT		7
	{ "_text" },
	{ "" }
};

int
initswap()
{
#ifdef notdef	
	if (namelist[X_FIRST].n_type == 0) {
		if (kvm_nlist(kd, namelist)) {
			nlsterr(namelist);
			return(0);
		}
		if (namelist[X_FIRST].n_type == 0) {
			error("namelist on %s failed", _PATH_UNIX);
			return(0);
		}
	}
	if (nswdev == 0) {
		NREAD(X_DMMIN, &dmmin, LONG);
		NREAD(X_DMMAX, &dmmax, LONG);
		NREAD(X_DMTEXT, &dmtext, LONG);
		NREAD(X_NSWDEV, &nswdev, LONG);
		if (nswdev > MAXSWAPDEV)
			nswdev = MAXSWAPDEV;
		swdevt = (struct swdevt *)calloc(nswdev, sizeof (*swdevt));
		NREAD(X_SWDEVT, swdevt, nswdev * sizeof (struct swdevt));
		NREAD(X_NTEXT, &ntext, LONG);
		NREAD(X_TEXT, &textp, LONG);
	}
	if (xtext == NULL)
		xtext = (struct text *)calloc(ntext, sizeof (struct text));
	return(1);
#endif
}

void
fetchswap()
{
#ifdef notdef	
	if (namelist[X_FIRST].n_type == 0)
		return;
	/*
	 * TODO - read procs
	 */
	if (xtext == NULL) {
		xtext = (struct text *)calloc(ntext, sizeof (struct text));
		if (xtext == NULL)
			return;
	}
	if (!KREAD(textp, xtext, ntext * sizeof (struct text)))
		error("couldn't read text table");
#endif
}

void
labelswap()
{
#ifdef notdef	
	register int row;

	if (nswdev == 0) {
		error("Don't know how many swap devices.\n");
		return;
	}
	colwidth = (COLS - OFFSET - (nswdev - 1)) / nswdev;
	row = swaplabel(0, dmtext, 1);
	(void) swaplabel(row, dmmax, 0);
#endif
}

static void
swaplabel(row, dmbound, donames)
	register int row;
	int dmbound, donames;
{
#ifdef notdef	
	register int i, j;

	for (i = 0; i < nswdev; i++) {
		if (donames)
			mvwprintw(wnd,
			    row, OFFSET + i*(1 + colwidth) + (colwidth - 3)/2,
			    "%s", devname(swdevt[i].sw_dev, S_IFBLK));
		for (j = 0; j + 5 < colwidth; j += 5)
			mvwprintw(wnd, row + donames,
			    OFFSET + i*(1 + colwidth) + j, "/%-2d  ", j);
	}
	row += 1 + donames;
	for (j = 0, i = dmmin; i <= dmbound; i *= 2, j++, row++) {
		int k;

		mvwprintw(wnd, row, 0, "%4d|", i);
		for (k = 1; k < nswdev; k++)
			mvwaddch(wnd, row, OFFSET + k*(1 + colwidth) - 1, '|');
	}
	return (row);
#endif
}
