/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)swap.c	5.5 (Berkeley) %G%";
#endif not lint

#include "systat.h"
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/text.h>
#include <sys/conf.h>
#include <sys/vmmac.h>
#include <machine/pte.h>
#include <paths.h>

WINDOW *
openswap()
{
	return (subwin(stdscr, LINES-5-1, 0, 5, 0));
}

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

extern union {
	struct  user user;
	char    upages[UPAGES][NBPG];
} user;
#define u       user.user

showswap()
{
	register int i, j;
	register struct proc *pp;
	register struct text *xp;
	register int row;
	register int ts;
	register swblk_t *dp;

	if (xtext == 0)
		return;
	for (xp = xtext; xp < &xtext[ntext]; xp++) {
		if (xp->x_iptr == NULL)
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
		if ((xp->x_flag & XPAGI) && xp->x_ptdaddr)
			buckets[swatodev(xp->x_ptdaddr)]
			    [dmtoindex(ctod(ctopt(xp->x_size)))]++;
	}
	row = swapdisplay(2, dmtext, 'X');
	if (kprocp == NULL)
		return;
	for (i = 0, pp = kprocp; i < nproc; i++, pp++) {
		if (pp->p_stat == 0 || pp->p_stat == SZOMB)
			continue;
		if (pp->p_flag & SSYS)
			continue;
		if (getu(pp) == 0)
			continue;
		vsacct(&u.u_dmap);
		vsacct(&u.u_smap);
#ifdef notdef
		if ((pp->p_flag & SLOAD) == 0)
			vusize(pp);
#endif
	}
	(void) swapdisplay(1+row, dmmax, 'X');
}

#define	OFFSET	5			/* left hand column */

swapdisplay(baserow, dmbound, c)
	int baserow, dmbound;
	char c;
{
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
}

vsacct(dmp)
	register struct dmap *dmp;
{
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
}

dmtoindex(dm)
	int dm;
{
	register int i, j;

	for (j = 0, i = dmmin; i <= dmmax; i *= 2, j++)
		if (dm <= i)
			return (j);
	error("dmtoindex(%d)", dm);
	return (NDMAP - 1);
}

static struct nlist nlst[] = {
#define X_PROC          0
	{ "_proc" },
#define X_NPROC         1
	{ "_nproc" },
#define X_USRPTMAP      2
	{ "_Usrptmap" },
#define X_USRPT         3
	{ "_usrpt" },
#define X_NSWAP         4
	{ "_nswap" },
#define X_DMMIN         5
	{ "_dmmin" },
#define X_DMMAX         6
	{ "_dmmax" },
#define	X_DMTEXT	7
	{ "_dmtext" },
#define X_NSWDEV        8
	{ "_nswdev" },
#define	X_SWDEVT	9
	{ "_swdevt" },
#define	X_NTEXT		10
	{ "_ntext" },
#define	X_TEXT		11
	{ "_text" },
	{ "" }
};

initswap()
{
	if (nlst[X_PROC].n_type == 0) {
		nlist(_PATH_UNIX, nlst);
		if (nlst[X_PROC].n_type == 0) {
			error("namelist on %s failed", _PATH_UNIX);
			return(0);
		}
	}
	if (nswdev == 0) {
		dmmin = getw(nlst[X_DMMIN].n_value);
		dmmax = getw(nlst[X_DMMAX].n_value);
		dmtext = getw(nlst[X_DMTEXT].n_value);
		nswdev = getw(nlst[X_NSWDEV].n_value);
		if (nswdev > MAXSWAPDEV)
			nswdev = MAXSWAPDEV;
		swdevt = (struct swdevt *)calloc(nswdev, sizeof (*swdevt));
		klseek(kmem, nlst[X_SWDEVT].n_value, L_SET);
		read(kmem, swdevt, nswdev * sizeof (struct swdevt));
		ntext = getw(nlst[X_NTEXT].n_value);
		textp = getw(nlst[X_TEXT].n_value);
	}
	if (procp == NULL) {
		procp = getw(nlst[X_PROC].n_value);
		nproc = getw(nlst[X_NPROC].n_value);
	}
	if (xtext == NULL)
		xtext = (struct text *)calloc(ntext, sizeof (struct text));
	if (kprocp == NULL)
		kprocp = (struct proc *)calloc(nproc, sizeof (struct proc));
	if (usrpt != NULL)
		return(1);
	usrpt = (struct pte *)nlst[X_USRPT].n_value;
	Usrptma = (struct pte *)nlst[X_USRPTMAP].n_value;
	if (pt == NULL)
		pt = (struct p_times *)malloc(nproc * sizeof (struct p_times));
	return(1);
}

fetchswap()
{
	if (nlst[X_PROC].n_type == 0)
		return;
	if (kprocp == NULL) {
		kprocp = (struct proc *)malloc(sizeof (*kprocp) * nproc);
		if (kprocp == NULL)
			return;
	}
	lseek(kmem, procp, L_SET);
	if (read(kmem, kprocp, sizeof (struct proc) * nproc) !=
	    sizeof (struct proc) * nproc) {
		error("couldn't read proc table");
		return;
	}
	if (xtext == NULL) {
		xtext = (struct text *)calloc(ntext, sizeof (struct text));
		if (xtext == NULL)
			return;
	}
	lseek(kmem, textp, L_SET);
	if (read(kmem, xtext, ntext * sizeof (struct text)) !=
	    sizeof (struct text) * ntext)
		error("couldn't read text table");
}

#ifdef vax
char	*devnames[] =
     { "hp", "ht", "up", "rk", "sw", "tm", "ts", "mt", "tu", "ra", "ut",
       "rb", "rx", "rl" };
#endif
#ifdef tahoe
char	*devnames[] = { "ud", "vd", "xp", "cy", "sw" };
#endif

labelswap()
{
	register int row;

	if (nswdev == 0) {
		error("Don't know how many swap devices.\n");
		return;
	}
	colwidth = (COLS - OFFSET - (nswdev - 1)) / nswdev;
	row = swaplabel(0, dmtext, 1);
	(void) swaplabel(row, dmmax, 0);
}

swaplabel(row, dmbound, donames)
	register int row;
	int dmbound, donames;
{
	register int i, j;

	for (i = 0; i < nswdev; i++) {
		if (donames)
			mvwprintw(wnd,
			    row, OFFSET + i*(1 + colwidth) + (colwidth - 3)/2,
			    "%s%d", devnames[major(swdevt[i].sw_dev)],
			        minor(swdevt[i].sw_dev) >> 3);
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
}
