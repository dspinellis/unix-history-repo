/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)analyze.c	5.2 (Berkeley) 5/14/86";
#endif not lint

/*
 * Analyze - analyze a core (and optional paging area) saved from
 * a virtual Unix system crash.
 */
#include <stdio.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <machine/pte.h>
#include <nlist.h>
#include <sys/map.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/text.h>
#include <sys/cmap.h>
#include <sys/vm.h>

int	Dflg;
int	dflg;
int	vflg;
int	mflg;
int	fflg;
int	sflg;
int	uflg;

/* use vprintf with care; it plays havoc with ``else's'' */
#define	vprintf	if (vflg) printf

#ifdef vax
#define	clear(x)	((int)x & 0x7fffffff)
#else
#define clear(x)	((int)x)
#endif

struct	proc *proc, *aproc;
int	nproc;
struct	text *text, *atext;
int	ntext;
struct	mapent *swapmap;
int	nswapmap;
int	dmmin, dmmax, dmtext;
struct	cmap *cmap;
int	ecmx;
struct	pte *usrpt;
struct	pte *Usrptma;
int	firstfree;
int	maxfree;
int	freemem;
struct	pte p0br[ctopt(btoc(MAXTSIZ+MAXDSIZ+MAXSSIZ))][NPTEPG];
int	pid;

struct	paginfo {
	char	z_type;
	char	z_count;
	short	z_pid;
	struct	pte z_pte;
} *paginfo;
#define	ZLOST	0
#define	ZDATA	1
#define	ZSTACK	2
#define	ZUDOT	3
#define	ZPAGET	4
#define	ZTEXT	5
#define	ZFREE	6
#define	ZINTRAN	7

struct	dblks {
	short	d_first;
	short	d_size;
	char	d_type;
	char	d_index;
} *dblks;
int	ndblks;

#define	DFREE	0
#define	DDATA	1
#define	DSTACK	2
#define	DTEXT	3
#define	DUDOT	4
#define	DPAGET	5

union	{
	char buf[UPAGES][NBPG];
	struct user U;
} u_area;
#define	u	u_area.U

int	fcore = -1;
int	fswap = -1;

struct	nlist nl[] = {
#define	X_PROC 0
	{ "_proc" },
#define	X_USRPT 1
	{ "_usrpt" },
#define	X_PTMA	2
	{ "_Usrptmap" },
#define	X_FIRSTFREE 3
	{ "_firstfree" },
#define	X_MAXFREE 4
	{ "_maxfree" },
#define	X_TEXT 5
	{ "_text" },
#define	X_FREEMEM 6
	{ "_freemem" },
#define	X_CMAP 7
	{ "_cmap" },
#define	X_ECMAP 8
	{ "_ecmap" },
#define	X_SWAPMAP 9
	{ "_swapmap" },
#define	X_NPROC 10
	{ "_nproc" },
#define	X_NTEXT 11
	{ "_ntext" },
#define	X_NSWAPMAP 12
	{ "_nswapmap" },
#define	X_DMMIN	13
	{ "_dmmin" },
#define	X_DMMAX	14
	{ "_dmmax" },
#define	X_DMTEXT 15
	{ "_dmtext" },
	{ "" }
};

main(argc, argv)
	int argc;
	char **argv;
{
	register struct nlist *np;
	register struct proc *p;
	register struct text *xp;
	register struct pte *pte;
	register int i;
	int w, a;

#ifdef DEBUG
	setbuf(stdout, NULL);
#endif
	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		register char *cp = *argv++;
		argc--;
		while (*++cp) switch (*cp) {

		case 'm':
			mflg++;
			break;

		case 'v':
			vflg++;
			break;

		case 's':
			if (argc < 2)
				goto usage;
			if ((fswap = open(argv[0], 0)) < 0) {
				perror(argv[0]);
				exit(1);
			}
			argc--,argv++;
			sflg++;
			break;

		case 'f':
			fflg++;
			break;

		case 'D':
			Dflg++;
			break;

		case 'd':
			dflg++;
			break;

		case 'u':
			uflg++;
			break;

		default:
			goto usage;
		}
	}
	if (argc < 1) {
usage:
		fprintf(stderr, "usage: analyze [ -vmfd ] [ -s swapfile ] corefile [ system ]\n");
		exit(1);
	}
	close(0);
	if ((fcore = open(argv[0], 0)) < 0) {
		perror(argv[0]);
		exit(1);
	}
	nlist(argc > 1 ? argv[1] : "/vmunix", nl);
	if (nl[0].n_value == 0) {
		fprintf(stderr, "%s: bad namelist\n",
		    argc > 1 ? argv[1] : "/vmunix");
		exit(1);
	}
	for (np = nl; np->n_name && *np->n_name; np++)
		vprintf("%8.8s %x\n", np->n_name ,np->n_value );
	usrpt = (struct pte *)clear(nl[X_USRPT].n_value);
	Usrptma = (struct pte *)clear(nl[X_PTMA].n_value);
	firstfree = get(nl[X_FIRSTFREE].n_value);
	maxfree = get(nl[X_MAXFREE].n_value);
	freemem = get(nl[X_FREEMEM].n_value);
	dmmin = get(nl[X_DMMIN]);
	dmmax = get(nl[X_DMMAX]);
	dmtext = get(nl[X_DMTEXT]);
	paginfo = (struct paginfo *)calloc(maxfree, sizeof (struct paginfo));
	if (paginfo == NULL) {
		fprintf(stderr, "maxfree %x?... out of mem!\n", maxfree);
		exit(1);
	}
	vprintf("usrpt %x\nUsrptma %x\nfirstfree %x\nmaxfree %x\nfreemem %x\n",
		    usrpt, Usrptma, firstfree, maxfree, freemem);
	{
	  lseek(fcore, (long)clear(nl[X_PROC].n_value), 0);
	  read(fcore, (char *)&aproc, sizeof aproc);
	  lseek(fcore, (long)clear(nl[X_NPROC].n_value), 0);
	  read(fcore, (char *)&nproc, sizeof nproc);
	  printf("%d procs\n", nproc);
	  proc = (struct proc *)calloc(nproc, sizeof (struct proc));
	  lseek(fcore, (long)clear(aproc), 0);
	  if (read(fcore, (char *)proc, nproc * sizeof (struct proc))
	    != nproc * sizeof (struct proc)) {
	 	perror("proc read");
		exit(1);
	  }
	}
	{
	  lseek(fcore, (long)clear(nl[X_TEXT].n_value), 0);
	  read(fcore, (char *)&atext, sizeof atext);
	  lseek(fcore, (long)clear(nl[X_NTEXT].n_value), 0);
	  read(fcore, (char *)&ntext, sizeof ntext);
	  printf("%d texts\n", ntext);
	  text = (struct text *)calloc(ntext, sizeof (struct text));
	  lseek(fcore, (long)clear(atext), 0);
	  if (read(fcore, (char *)text, ntext * sizeof (struct text))
	    != ntext * sizeof (struct text)) {
		perror("text read");
		exit(1);
	  }
	}
	i = (get(nl[X_ECMAP].n_value) - get(nl[X_CMAP].n_value));
	ecmx = i / sizeof (struct cmap);
	cmap = (struct cmap *)calloc(i, 1);
	if (cmap == NULL) {
		fprintf(stderr, "not enough mem for %x bytes of cmap\n", i);
		exit(1);
	}
	lseek(fcore, (long)clear(get(nl[X_CMAP].n_value)), 0);
	if (read(fcore, (char *)cmap, i) != i) {
		perror("cmap read");
		exit(1);
	}
	{ struct mapent *aswapmap;
	  lseek(fcore, (long)clear(nl[X_SWAPMAP].n_value), 0);
	  read(fcore, (char *)&aswapmap, sizeof aswapmap);
	  lseek(fcore, (long)clear(nl[X_NSWAPMAP].n_value), 0);
	  read(fcore, (char *)&nswapmap, sizeof nswapmap);
	  nswapmap--;
	  printf("%d swapmap entries\n", nswapmap);
	  swapmap = (struct mapent *)calloc(nswapmap, sizeof (struct mapent));
	  dblks = (struct dblks *)calloc(2 * nswapmap, sizeof (struct dblks));
	  lseek(fcore, (long)clear(aswapmap+1), 0);
	  if (read(fcore, (char *)swapmap, nswapmap * sizeof (struct mapent))
	    != nswapmap * sizeof (struct mapent)) {
		perror("swapmap read");
		exit(1);
	  }
	}
	for (p = &proc[1]; p < proc+nproc; p++) {
		p->p_p0br = (struct pte *)clear(p->p_p0br);
		p->p_addr = (struct pte *)clear(p->p_addr);
		if (p->p_stat == 0)
			continue;
		printf("proc %d ", p->p_pid);
		if (p->p_stat == SZOMB) {
			printf("zombie\n");
			continue;
		}
		if (p->p_flag & SLOAD) {
			printf("loaded, p0br %x, ", p->p_p0br);
			printf("%d pages of page tables:", p->p_szpt);
			a = btokmx(p->p_p0br);
			for (i = 0; i < p->p_szpt; i++) {
				w = get(&Usrptma[a + i]);
				printf(" %x", w & PG_PFNUM);
			}
			printf("\n");
			for(i = 0; i < p->p_szpt; i++) {
				w = get(&Usrptma[a + i]);
				if (getpt(w, i))
					count(p, (struct pte *)&w, ZPAGET);
			}
		} else {
			/* i = ctopt(btoc(u.u_exdata.ux_dsize)); */
			i = clrnd(ctopt(p->p_tsize + p->p_dsize + p->p_ssize));
			printf("swapped, swaddr %x\n", p->p_swaddr);
			duse(p->p_swaddr, ctod(clrnd(UPAGES)), DUDOT, p - proc);
			duse(p->p_swaddr + ctod(UPAGES),
			    ctod(clrnd(i - p->p_tsize / NPTEPG)), 
				DPAGET, p - proc); 
			    /* i, DPAGET, p - proc); */
		}
		p->p_p0br = (struct pte *)p0br;
		p->p_addr = uaddr(p);
		if (p->p_textp)
			p->p_textp = &text[p->p_textp - atext];
		if (p->p_pid == 2)
			continue;
		if (getu(p))
			continue;
		u.u_procp = p;
		pdmap();
		if ((p->p_flag & SLOAD) == 0)
			continue;
		pid = p->p_pid;
		for (i = 0; i < p->p_tsize; i++) {
			pte = tptopte(p, i);
			if (pte->pg_fod || pte->pg_pfnum == 0)
				continue;
			if (pte->pg_pfnum >= firstfree && pte->pg_pfnum < maxfree && cmap[pgtocm(pte->pg_pfnum)].c_intrans)
				count(p, pte, ZINTRAN);
			else
				count(p, pte, ZTEXT);
		}
		vprintf("\n");
		for (i = 0; i < p->p_dsize; i++) {
			pte = dptopte(p, i);
			if (pte->pg_fod || pte->pg_pfnum == 0)
				continue;
			if (pte->pg_pfnum >= firstfree && pte->pg_pfnum < maxfree && cmap[pgtocm(pte->pg_pfnum)].c_intrans)
				count(p, pte, ZINTRAN);
			else
				count(p, pte, ZDATA);
		}
		vprintf("\n");
		for (i = 0; i < p->p_ssize; i++) {
			pte = sptopte(p, i);
			if (pte->pg_fod || pte->pg_pfnum == 0)
				continue;
			if (pte->pg_pfnum >= firstfree && pte->pg_pfnum < maxfree && cmap[pgtocm(pte->pg_pfnum)].c_intrans)
				count(p, pte, ZINTRAN);
			else
				count(p, pte, ZSTACK);
		}
		vprintf("\n");
		for (i = 0; i < UPAGES; i++)
			count(p, &p->p_addr[i], ZUDOT);
		vprintf("\n");
		vprintf("\n");
	}
	for (xp = &text[0]; xp < text+ntext; xp++)
		if (xp->x_iptr) {
			int size = ctod(xp->x_size);

			for (i = 0; i < size; i += dmtext)
				duse(xp->x_daddr[i],
				    (size - i) > dmtext
					? dmtext : size - i,
				    DTEXT, xp - text);
			if (xp->x_flag & XPAGI)
				duse(xp->x_ptdaddr, 
					ctod(clrnd(ctopt(xp->x_size))),
				    DTEXT, xp - text);
		}
	dmcheck();
	fixfree();
	summary();
	exit(0);
}

pdmap()
{
	register struct text *xp;

	if (fswap == -1 && (u.u_procp->p_flag & SLOAD) == 0)
		return;
	if (Dflg)
		printf("disk for pid %d", u.u_procp->p_pid);
	if ((xp = u.u_procp->p_textp) && Dflg)
		ptdmap(xp->x_daddr, xp->x_size);
	pdmseg("data", &u.u_dmap, DDATA);
	pdmseg("stack", &u.u_smap, DSTACK);
	if (Dflg)
		printf("\n");
}

ptdmap(dp, size)
	register daddr_t *dp;
	int size;
{
	register int i;
	int rem;

	if (Dflg)
		printf(" text:");
	for (i = 0, rem = size; rem > 0; i++) {
		if (Dflg)
			printf(" %x<%x>", dp[i], rem < dmtext ? rem : dmtext);
		rem -= rem < dmtext ? rem : dmtext;
	}
}

pdmseg(cp, dmp, type)
	char *cp;
	struct dmap *dmp;
{
	register int i;
	int b, rem;

	if (Dflg)
		printf(", %s:", cp);
	b = dmmin;
	for (i = 0, rem = dmp->dm_size; rem > 0; i++) {
		if (Dflg)
			printf(" %x<%x>", dmp->dm_map[i], rem < b ? rem : b);
		duse(dmp->dm_map[i], b, type, u.u_procp - proc);
		rem -= b;
		if (b < dmmax)
			b *= 2;
	}
}

duse(first, size, type, index)
{
	register struct dblks *dp;

	if (fswap == -1)
		return;
	dp = &dblks[ndblks];
	if (++ndblks > 2*nswapmap) {
		fprintf(stderr, "too many disk blocks\n");
		exit(1);
	}
	dp->d_first = first;
	dp->d_size = size;
	dp->d_type = type;
	dp->d_index = index;
}

dsort(d, e)
	register struct dblks *d, *e;
{

	return (e->d_first - d->d_first);
}

dmcheck()
{
	register struct mapent *smp;
	register struct dblks *d, *e;

	for (smp = swapmap; smp->m_size; smp++)
		duse(smp->m_addr, smp->m_size, DFREE, 0);
	duse(ctod(CLSIZE), dmtext - ctod(CLSIZE), DFREE, 0);
	qsort(dblks, ndblks, sizeof (struct dblks), dsort);
	d = &dblks[ndblks - 1];
	if (d->d_first > 1)
		printf("lost swap map: start %x size %x\n", 1, d->d_first);
	for (; d > dblks; d--) {
		if (dflg)
			dprint(d);
		e = d - 1;
		if (d->d_first + d->d_size > e->d_first) {
			printf("overlap in swap mappings:\n");
			dprint(d);
			dprint(e);
		} else if (d->d_first + d->d_size < e->d_first) {
			printf("lost swap map: start %x size %x\n",
			    d->d_first + d->d_size,
			    e->d_first - (d->d_first + d->d_size)); 
		}
	}
	if (dflg)
		dprint(dblks);
	if (sflg)
		printf("swap space ends at %x\n", d->d_first + d->d_size);
}

char *dnames[] = {
	"DFREE",
	"DDATA",
	"DSTACK",
	"DTEXT",
	"DUDOT",
	"DPAGET",
};

dprint(d)
	register struct dblks *d;
{

	printf("at %4x size %4x type %s", d->d_first, d->d_size,
		dnames[d->d_type]);
	switch (d->d_type) {

	case DSTACK:
	case DDATA:
		printf(" pid %d", proc[d->d_index].p_pid);
		break;
	}
	printf("\n");
}

getpt(x, i)
	int x, i;
{

	lseek(fcore, (long)ctob((x & PG_PFNUM)), 0);
	if (read(fcore, (char *)(p0br[i]), NBPG) != NBPG) {
		perror("read");
		fprintf(stderr, "getpt error reading frame %x\n", clear(x));
		return (0);
	}
	return (1);
}

checkpg(p, pte, type)
	register struct pte *pte;
	register struct proc *p;
	int type;
{
	char corepg[NBPG], swapg[NBPG];
	register int i, count, dblock;
	register int pfnum = pte->pg_pfnum;

	if (type == ZPAGET || type == ZUDOT)
		return (0);
	lseek(fcore, (long)(NBPG * pfnum), 0);
	if (read(fcore, corepg, NBPG) != NBPG){
		perror("read");
		fprintf(stderr, "Error reading core page %x\n", pfnum);
		return (0);
	}
	switch (type) {

	case ZDATA:
		if (ptetodp(p, pte) >= u.u_dmap.dm_size)
			return (0);
		break;

	case ZTEXT:
		break;

	case ZSTACK:
		if (ptetosp(p, pte) >= u.u_smap.dm_size)
			return (0);
		break;

	default:
		return(0);
		break;
	}
	dblock = vtod(p, ptetov(p, pte), &u.u_dmap, &u.u_smap);
	vprintf("   %x", dblock);
	if (pte->pg_fod || pte->pg_pfnum == 0)
		return (0);
	if (cmap[pgtocm(pte->pg_pfnum)].c_intrans || pte->pg_m || pte->pg_swapm)
		return (0);
	lseek(fswap, (long)(DEV_BSIZE * dblock), 0);
	if (read(fswap, swapg, NBPG) != NBPG) {
		fprintf(stderr,"swap page %x: ", dblock);
		perror("read");
	}
	count = 0;
	for (i = 0; i < NBPG; i++)
		if (corepg[i] != swapg[i])
			count++;
	if (count == 0)
		vprintf("\tsame");
	return (count);
}

getu(p)
	register struct proc *p;
{
	int i, w, cc, errs = 0;

	if (uflg && (p->p_flag & SLOAD))
		printf("pid %d u. pages:", p->p_pid);
	for (i = 0; i < UPAGES; i++) {
		if (p->p_flag & SLOAD) {
			if (uflg)
				printf(" %x", p->p_addr[i].pg_pfnum);
			lseek(fcore, ctob(p->p_addr[i].pg_pfnum), 0);
			if (read(fcore, u_area.buf[i], NBPG) != NBPG)
				perror("core u. read"), errs++;
		} else if (fswap >= 0) {
			lseek(fswap, (long)(NBPG * (p->p_swaddr+i)), 0);
			if (read(fswap, u_area.buf[i], NBPG) != NBPG)
				perror("swap u. read"), errs++;
		}
	}
	if (uflg && (p->p_flag & SLOAD))
		printf("\n");
	return (errs);
}

char	*typepg[] = {
	"lost",
	"data",
	"stack",
	"udot",
	"paget",
	"text",
	"free",
	"intransit",
};

count(p, pte, type)
	struct proc *p;
	register struct pte *pte;
	int type;
{
	register int pfnum = pte->pg_pfnum;
	register struct paginfo *zp = &paginfo[pfnum];
	int ndif;
#define	zprintf	if (type==ZINTRAN || vflg) printf

	if (type == ZINTRAN && pfnum == 0)
		return;
	zprintf("page %x %s", pfnum, typepg[type]);
	if (sflg == 0 || (ndif = checkpg(p, pte, type)) == 0) {
		zprintf("\n");
	} else {
		if (vflg == 0 && type != ZINTRAN)
			printf("page %x %s,", pfnum, typepg[type]);
		printf(" %d bytes differ\n",ndif);
	}
	if (pfnum < firstfree || pfnum > maxfree) {
		printf("page number out of range:\n");
		printf("\tpage %x type %s pid %d\n", pfnum, typepg[type], pid);
		return;
	}
	if (bad(zp, type)) {
		printf("dup page pte %x", *(int *)pte);
		dumpcm("", pte->pg_pfnum);
		dump(zp);
		printf("pte %x and as %s in pid %d\n", zp->z_pte, typepg[type], pid);
		return;
	}
	zp->z_type = type;
	zp->z_count++;
	zp->z_pid = pid;
	zp->z_pte = *pte;
}

bad(zp, type)
	struct paginfo *zp;
{
	if (type == ZTEXT) {
		if (zp->z_type != 0 && zp->z_type != ZTEXT)
			return (1);
		return (0);
	}
	return (zp->z_count);
}

dump(zp)
	struct paginfo *zp;
{

	printf("page %x type %s pid %d ", zp - paginfo, typepg[zp->z_type], zp->z_pid);
}

summary()
{
	register int i;
	register struct paginfo *zp;
	register int pfnum;

	for (i = firstfree + UPAGES; i < maxfree; i+= CLSIZE) {
		zp = &paginfo[i];
		if (zp->z_type == ZLOST)
			dumpcm("lost", i);
		pfnum = pgtocm(i);
		if (cmap[pfnum].c_lock && cmap[pfnum].c_type != CSYS)
			dumpcm("locked", i);
		if (mflg)
			dumpcm("mem", i);
	}
}

char	*tynames[] = {
	"sys",
	"text",
	"data",
	"stack"
};
dumpcm(cp, pg)
	char *cp;
	int pg;
{
	int pslot;
	int cm;
	register struct cmap *c;

	cm = pgtocm(pg);
	printf("cm %x %s page %x ", cm, cp, pg);
	c = &cmap[cm];
	printf("\t[%x, %x", c->c_page, c->c_ndx);
	if (c->c_type == CSYS)
		goto skip;
	if (c->c_type != CTEXT) {
		if (c->c_ndx >= nproc) {
			printf(" [text c->c_ndx %d?]", c->c_ndx);
			goto skip;
		}
		printf(" (=pid %d)", proc[c->c_ndx].p_pid);
	} else {
		if (c->c_ndx >= ntext) {
			printf(" [text c->c_ndx %d?]", c->c_ndx);
			goto skip;
		}
		pslot= (text[c->c_ndx].x_caddr - aproc);
		printf(" (=pid");
		for(;;) {
			printf(" %d", proc[pslot].p_pid);
			if (proc[pslot].p_xlink == 0)
				break;
			pslot= (proc[pslot].p_xlink - aproc);
		}
		printf(")");
	}
skip:
	printf("] ");
	printf(tynames[c->c_type]);
	if (c->c_free)
		printf(" free");
	if (c->c_gone)
		printf(" gone");
	if (c->c_lock)
		printf(" lock");
	if (c->c_want)
		printf(" want");
	if (c->c_intrans)
		printf(" intrans");
	if (c->c_blkno)
		printf(" blkno %x mdev %d", c->c_blkno, c->c_mdev);
	if (c->c_hlink) {
		printf(" hlink %x page %x", c->c_hlink, cmtopg(c->c_hlink));
		if (c->c_hlink > ecmx)
			printf(" <<<");
	}
	printf("\n");
}

fixfree()
{
	register int i, next, prev;

	next = CMHEAD;
	for (i=freemem/CLSIZE; --i >=0; ) {
		prev = next;
		next = cmap[next].c_next;
		if (cmap[next].c_free == 0) {
			printf("link to non free block: in %x to %x\n", cmtopg(prev), cmtopg(next));
			dumpcm("bad free link in", cmtopg(prev));
			dumpcm("to non free block", cmtopg(next));
		}
		if (cmtopg(next) > maxfree) {
			printf("free list link out of range: in %x to %x\n", cmtopg(prev), cmtopg(next));
			dumpcm("bad link in", cmtopg(prev));
		}
		paginfo[cmtopg(next)].z_type = ZFREE;
		if (fflg)
			dumpcm("free", cmtopg(next));
		paginfo[cmtopg(next)+1].z_type = ZFREE;
		if (fflg)
			dumpcm("free", cmtopg(next)+1);
	}
}

get(loc)
unsigned loc;
{
	int x;
	
	lseek(fcore, (long)clear(loc), 0);
	if (read(fcore, (char *)&x, sizeof (int)) != sizeof (int)) {
		perror("read");
		fprintf(stderr, "get failed on %x\n", clear(loc));
		return (0);
	}
	return (x);
}
/*
 * Convert a virtual page number 
 * to its corresponding disk block number.
 * Used in pagein/pageout to initiate single page transfers.
 */
vtod(p, v, dmap, smap)
	register struct proc *p;
	register struct dmap *dmap, *smap;
{
	struct dblock db;

	if (isatsv(p, v)) {
		v = ctod(vtotp(p, v));
		return(p->p_textp->x_daddr[v / dmtext] + v % dmtext);
	}
	if (isassv(p, v))
		vstodb(ctod(vtosp(p, v)), ctod(1), smap, &db, 1);
	else
		vstodb(ctod(vtodp(p, v)), ctod(1), dmap, &db, 0);
	return (db.db_base);
}

/* 
 * Convert a pte pointer to
 * a virtual page number.
 */
ptetov(p, pte)
	register struct proc *p;
	register struct pte *pte;
{

	if (isatpte(p, pte))
		return (tptov(p, ptetotp(p, pte)));
	else if (isadpte(p, pte))
		return (dptov(p, ptetodp(p, pte)));
	else
		return (sptov(p, ptetosp(p, pte)));
}

/*
 * Given a base/size pair in virtual swap area,
 * return a physical base/size pair which is the
 * (largest) initial, physically contiguous block.
 */
vstodb(vsbase, vssize, dmp, dbp, rev)
	register int vsbase;
	int vssize;
	register struct dmap *dmp;
	register struct dblock *dbp;
{
	register int blk = dmmin;
	register swblk_t *ip = dmp->dm_map;

	if (vsbase < 0 || vsbase + vssize > dmp->dm_size)
		panic("vstodb");
	while (vsbase >= blk) {
		vsbase -= blk;
		if (blk < dmmax)
			blk *= 2;
		ip++;
	}
	dbp->db_size = min(vssize, blk - vsbase);
	dbp->db_base = *ip + (rev ? blk - (vsbase + vssize) : vsbase);
}

panic(cp)
	char *cp;
{
	printf("panic!: %s\n", cp);
}

min(a, b)
{
	return (a < b ? a : b);
}
