#include <stdio.h>
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/proc.h>
#include <sys/pte.h>
#include <a.out.h>
#include <sys/map.h>
#include <sys/user.h>
#include <sys/text.h>
#include <sys/cmap.h>
#include <sys/vm.h>

/*
 * Analyze - analyze a core (and optional paging area) saved from
 * a virtual Unix system crash.
 */

int	Dflg;
int	dflg;
int	vflg;
int	mflg;
int	fflg;
int	sflg;

/* use vprintf with care; it plays havoc with ``else's'' */
#define	vprintf	if (vflg) printf

#define	clear(x)	((int)x & 0x7fffffff)

struct	proc proc[NPROC];
struct	text text[NTEXT];
struct	map swapmap[SMAPSIZ];
struct	cmap *cmap;
struct	pte *usrpt;
struct	pte *Usrptma;
int	firstfree;
int	maxfree;
int	freemem;
struct	pte p0br[ctopt(MAXTSIZ+MAXDSIZ+MAXSSIZ)][NPTEPG];
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

#define	NDBLKS	(2*SMAPSIZ)
struct	dblks {
	short	d_first;
	short	d_size;
	char	d_type;
	char	d_index;
} dblks[NDBLKS];
int	ndblks;

#define	DFREE	0
#define	DDATA	1
#define	DSTACK	2
#define	DTEXT	3
#define	DUDOT	4
#define	DPAGET	5

union	{
	char buf[UPAGES][512];
	struct user U;
} u_area;
#define	u	u_area.U

int	fcore = -1;
int	fswap = -1;

struct	nlist nl[] = {
#define	X_PROC 0
	"_proc",	0, 0, 0, 0,
#define	X_USRPT 1
	"_usrpt",	0, 0, 0, 0,
#define	X_PTMA	2
	"_Usrptma",	0, 0, 0, 0,
#define	X_FIRSTFREE 3
	"_firstfr",	0, 0, 0, 0,
#define	X_MAXFREE 4
	"_maxfree",	0, 0, 0, 0,
#define	X_TEXT 5
	"_text",	0, 0, 0, 0,
#define	X_FREEMEM 6
	"_freemem",	0, 0, 0, 0,
#define	X_CMAP 7
	"_cmap",	0, 0, 0, 0,
#define	X_ECMAP 8
	"_ecmap",	0, 0, 0, 0,
#define	X_SWAPMAP 9
	"_swapmap",	0, 0, 0, 0,
	0,		0, 0, 0, 0,

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
	for (np = nl; np->n_name[0]; np++)
		vprintf("%8.8s %x\n", np->n_name ,np->n_value );
	usrpt = (struct pte *)clear(nl[X_USRPT].n_value);
	Usrptma = (struct pte *)clear(nl[X_PTMA].n_value);
	firstfree = get(nl[X_FIRSTFREE].n_value);
	maxfree = get(nl[X_MAXFREE].n_value);
	freemem = get(nl[X_FREEMEM].n_value);
	paginfo = (struct paginfo *)calloc(maxfree, sizeof (struct paginfo));
	if (paginfo == NULL) {
		fprintf(stderr, "maxfree %x?... out of mem!\n", maxfree);
		exit(1);
	}
	vprintf("usrpt %x\nUsrptma %x\nfirstfree %x\nmaxfree %x\nfreemem %x\n",
		    usrpt, Usrptma, firstfree, maxfree, freemem);
	lseek(fcore, (long)clear(nl[X_PROC].n_value), 0);
	if (read(fcore, (char *)proc, sizeof proc) != sizeof proc) {
	 	perror("proc read");
		exit(1);
	}
	lseek(fcore, (long)clear(nl[X_TEXT].n_value), 0);
	if (read(fcore, (char *)text, sizeof text) != sizeof text) {
		perror("text read");
		exit(1);
	}
	i = (get(nl[X_ECMAP].n_value) - get(nl[X_CMAP].n_value));
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
	lseek(fcore, (long)clear(nl[X_SWAPMAP].n_value), 0);
	if (read(fcore, (char *)swapmap, sizeof swapmap) != sizeof swapmap) {
		perror("swapmap read");
		exit(1);
	}
	for (p = &proc[1]; p < &proc[NPROC]; p++) {
		p->p_p0br = (struct pte *)clear(p->p_p0br);
		if (p->p_stat == 0)
			continue;
		printf("proc %d ", p->p_pid);
		if (p->p_stat != SZOMB) {
			if (getu(p))
				continue;
			u.u_procp = p;
		}
		if (p->p_stat == SZOMB) {
			printf("zombie\n");
			continue;
		}
		if (p->p_flag & SLOAD) {
			printf("loaded, p0br %x, ", p->p_p0br);
			p->p_szpt = u.u_pcb.pcb_szpt;
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
			duse(p->p_swaddr, clrnd(ctod(UPAGES)), DUDOT, p - proc);
			duse(p->p_swaddr + ctod(UPAGES),
			    clrnd(i - p->p_tsize / NPTEPG), DPAGET, p - proc); 
			    /* i, DPAGET, p - proc); */
		}
		p->p_p0br = (struct pte *)p0br;
		p->p_textp = &text[p->p_textp - (struct text *)nl[X_TEXT].n_value];
		if (p->p_pid == 2)
			continue;
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
		for (w = 0; w < UPAGES; w++) {
			int l = p->p_addr[w];
			count(p, (struct pte *)&l, ZUDOT);
		}
		vprintf("\n");
		vprintf("\n");
	}
	for (xp = &text[0]; xp < &text[NTEXT]; xp++)
		if (xp->x_iptr) {
			duse(xp->x_daddr, xp->x_size, DTEXT, xp - text);
			if (xp->x_flag & XPAGI)
				duse(xp->x_daddr + xp->x_size,
				    clrnd(ctopt(xp->x_size)), DTEXT, xp - text);
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
	if (xp = u.u_procp->p_textp) {
		xp = &text[xp - (struct text *)nl[X_TEXT].n_value];
		if (Dflg)
			printf(", text: %x<%x>", xp->x_daddr, xp->x_size);
	}
	pdmseg("data", &u.u_dmap, DDATA);
	pdmseg("stack", &u.u_smap, DSTACK);
	if (Dflg)
		printf("\n");
}

pdmseg(cp, dmp, type)
	char *cp;
	struct dmap *dmp;
{
	register int i;
	int b, rem;

	if (Dflg)
		printf(", %s:", cp);
	b = DMMIN;
	for (i = 0, rem = dmp->dm_size; rem > 0; i++) {
		if (Dflg)
			printf(" %x<%x>", dmp->dm_map[i], rem < b ? rem : b);
		duse(dmp->dm_map[i], b, type, u.u_procp - proc);
		rem -= b;
		if (b < DMMAX)
			b *= 2;
	}
}

duse(first, size, type, index)
{
	register struct dblks *dp;

	if (fswap == -1)
		return;
	dp = &dblks[ndblks];
	if (++ndblks > NDBLKS) {
		fprintf(stderr, "too many disk blocks, increase NDBLKS\n");
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
	register struct map *smp;
	register struct dblks *d, *e;

	for (smp = swapmap; smp->m_size; smp++)
		duse(smp->m_addr, smp->m_size, DFREE, 0);
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
	lseek(fswap, (long)(NBPG * dblock), 0);
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
	int i, w, errs = 0;

	for (i = 0; i < UPAGES; i++) {
		if (p->p_flag & SLOAD) {
			w = p->p_addr[i];
			lseek(fcore, (long)(NBPG * clear(w)), 0);
			if (read(fcore, u_area.buf[i], NBPG) != NBPG)
				perror("core u. read"), errs++;
		} else if (fswap >= 0) {
			lseek(fswap, (long)(NBPG * (p->p_swaddr+i)), 0);
			if (read(fswap, u_area.buf[i], NBPG) != NBPG)
				perror("swap u. read"), errs++;
		}
	}
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

	for (i = firstfree + UPAGES; i < maxfree; i++) {
		zp = &paginfo[i];
		if (zp->z_type == ZLOST)
			dumpcm("lost", i);
		pfnum = pgtocm(i);
		if ((cmap[pfnum].c_flag & MLOCK) && !(cmap[pfnum].c_flag & MSYS))
			dumpcm("locked", i);
		if (mflg)
			dumpcm("mem", i);
	}
}

dumpcm(cp, pg)
	char *cp;
	int pg;
{
	int pslot;
	int cm;

	printf("%s page %x ", cp, pg);
	cm = pgtocm(pg);
	printf("\t[%x, %x", cmap[cm].c_page, cmap[cm].c_ndx);
	if ((cmap[cm].c_flag&MTEXT) == 0)
		printf(" (=pid %d)", proc[cmap[cm].c_ndx].p_pid);
	else {
		pslot=(text[cmap[cm].c_ndx].x_caddr - (struct proc *)nl[X_PROC].n_value);
		printf(" (=pid");
		for(;;) {
			printf(" %d", proc[pslot].p_pid);
			if (proc[pslot].p_xlink == 0)
				break;
			pslot=(proc[pslot].p_xlink - (struct proc *)nl[X_PROC].n_value);
		}
		printf(")");
	}

#define	Mflag(x,y)	if (cmap[cm].c_flag&x) printf(y);
	Mflag(MTEXT, " MTEXT");
	Mflag(MDATA, " MDATA");
	Mflag(MSTACK, "MSTACK");
	Mflag(MSYS, " MSYS");
	Mflag(MFREE, " MFREE");
	Mflag(MLOCK, " MLOCK");
	printf("]\n");
}

fixfree()
{
	register int i, next, prev;

	next = CMHEAD;
	for (i=freemem/CLSIZE; --i >=0; ) {
		prev = next;
		next = cmap[next].c_next;
		if ((cmap[next].c_flag&MFREE) == 0) {
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

	if (v < p->p_tsize)
		return(p->p_textp->x_daddr + v);
	if (isassv(p, v))
		vstodb(vtosp(p, v), 1, smap, &db, 1);
	else
		vstodb(vtodp(p, v), 1, dmap, &db, 0);
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
	register int blk = DMMIN;
	register swblk_t *ip = dmp->dm_map;

	if (vsbase < 0 || vsbase + vssize > dmp->dm_size)
		panic("vstodb");
	while (vsbase >= blk) {
		vsbase -= blk;
		if (blk < DMMAX)
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
