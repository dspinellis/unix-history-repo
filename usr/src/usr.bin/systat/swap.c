#ifndef lint
static char *sccsid = "@(#)swap.c	1.3 (Lucasfilm) %G%";
#endif

#include "systat.h"

#include <sys/map.h>
#include <sys/conf.h>
#include <sys/text.h>

/* these don't belong here */
#define X_PROC          0
#define X_NPROC         1
#define X_USRPTMAP      4
#define X_USRPT         5
#define X_NSWAP         6
#define X_DMMIN         9
#define X_DMMAX         10
#define X_NSWDEV        11
#define	X_SWDEVT	12
#define	X_NTEXT		13
#define	X_TEXT		14
#define	X_DMTEXT	15

int	dmmin;
int	dmmax;
int	dmtext;
int	nswdev;
#define	MAXSWAPDEV	4
short	buckets[MAXSWAPDEV][NDMAP];
struct	swdevt *swdevt;
struct	proc *kprocp;
int	ntext;
int	textaddr;
struct	text *xtext;

initswap()
{

        if (nswdev == 0) {
                dmmin = getw(nlst[X_DMMIN].n_value);
                dmmax = getw(nlst[X_DMMAX].n_value);
                dmtext = getw(nlst[X_DMTEXT].n_value);
                nswdev = getw(nlst[X_NSWDEV].n_value);
		swdevt = (struct swdevt *)calloc(nswdev, sizeof (*swdevt));
		klseek(kmem, nlst[X_SWDEVT].n_value, L_SET);
		read(kmem, swdevt, nswdev * sizeof (struct swdevt));
		ntext = getw(nlst[X_NTEXT].n_value);
		xtext = (struct text *)calloc(ntext, sizeof (struct text));
		textaddr = getw(nlst[X_TEXT].n_value);
        }
        if (procp == NULL) {
                procp = getw(nlst[X_PROC].n_value);
                nproc = getw(nlst[X_NPROC].n_value);
                kprocp = (struct proc *)malloc(sizeof (*kprocp) * nproc);
        }
        if (usrpt != NULL)
                return;
	usrpt = (struct pte *)nlst[X_USRPT].n_value;
	Usrptma = (struct pte *)nlst[X_USRPTMAP].n_value;
	pt = (struct p_times *)malloc(nproc * sizeof (struct p_times));
}

fetchswap()
{

        lseek(kmem, procp, L_SET);
        read(kmem, kprocp, sizeof (struct proc) * nproc);
	lseek(kmem, textaddr, L_SET);
	read(kmem, xtext, ntext * sizeof (struct text));
}

#ifdef vax
char	*devnames[] =
     { "hp", "ht", "up", "rk", "sw", "tm", "ts", "mt", "tu", "ra", "ut",
       "rb", "rx", "rl" };
#endif
int	colwidth;

labelswap()
{
	register int i, j;
	register int row;

	if (nswdev == 0)
		initswap();
	if (nswdev == 0) {
		mvaddstr(22, 0, "Can't find number of swap devices.\n");
		return;
	}
        move(5, 0);
	colwidth = (70 - (nswdev - 1)) / nswdev;
	row = swaplabel(5, dmtext, 1);
	(void) swaplabel(row, dmmax, 0);
}

swaplabel(row, dmbound, donames)
	register int row;
	int dmbound, donames;
{
	register int i, j;

	for (i = 0; i < nswdev; i++) {
		if (donames) {
			move(row, 5 + i * (1 + colwidth) + (colwidth - 3) / 2);
			printw("%s%d", devnames[major(swdevt[i].sw_dev)],
			    minor(swdevt[i].sw_dev) >> 3);
		}
		for (j = 0; j + 5 < colwidth; j += 5) {
			move(row + donames, 5 + i * (1 + colwidth) + j);
			printw("/%-2d  ", j);
		}
	}
	row += 1 + donames;
	for (j = 0, i = dmmin; i <= dmbound; i *= 2, j++, row++) {
		int k;

		mvprintw(row, 0, "%4d|", i);
		for (k = 1; k < nswdev; k++)
			mvwaddch(wnd, row - 3, k * (1 + colwidth) - 1, '|');
	}
	return (row);
}

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

	if (nswdev == 0)
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
	row = swapdisplay(4, dmtext, 'X');
        for (i = 0, pp = kprocp; i < nproc; i++, pp++) {
		if (pp->p_stat == 0 || pp->p_stat == SZOMB)
			continue;
		if (pp->p_flag & SSYS)
			continue;
		if (getu(pp) == 0) {
			error("showswap: getu failed on pid %d", pp->p_pid);
			continue;
		}
		vsacct(&u.u_dmap);
		vsacct(&u.u_smap);
#ifdef notdef
		if ((pp->p_flag & SLOAD) == 0)
			vusize(pp);
#endif
        }
	(void) swapdisplay(row + 1, dmmax, 'X');
}

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
			wmove(wnd, row, j * (1 + colwidth));
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
