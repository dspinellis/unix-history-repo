/*
 * ps; VAX 4BSD version
 */

#include <stdio.h>
#include <ctype.h>
#include <olda.out.h>
#include <pwd.h>
#include <sys/param.h>
#include <sys/tty.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <sys/pte.h>
#include <sys/vm.h>
#include <sys/text.h>
#include <sys/stat.h>

struct nlist nl[] = {
	{ "_proc" },
#define	X_PROC		0
	{ "_Usrptma" },
#define	X_USRPTMA	1
	{ "_usrpt" },
#define	X_USRPT		2
	{ "_text" },
#define	X_TEXT		3
	{ "_nswap" },
#define	X_NSWAP		4
	{ 0 },
};

struct	savcom {
	union {
		struct	lsav *lp;
		float	u_pctcpu;
		struct	vsav *vp;
		int	s_ssiz;
	} sun;
	struct	asav *ap;
} savcom[NPROC];

struct	asav {
	char	*a_cmdp;
	int	a_flag;
	short	a_stat, a_uid, a_pid, a_nice;
	size_t	a_size, a_rss;
	char	a_tty[4];
	dev_t	a_ttyd;
	time_t	a_time;
};

char	*lhdr;
/*	    F S UID   PID  PPID CP PRI NICE ADDR  SZ  RSS WCHAN TTY TIME */
struct	lsav {
	short	l_ppid;
	char	l_cpu, l_pri;
	int	l_addr;
	caddr_t	l_wchan;
};

char	*uhdr;
/*	 USER       PID %%CPU NICE  SZ  RSS TTY TIME */

char	*shdr;
/*	 SSIZ   PID TTY TIME */

char	*vhdr;
/*	F     PID TT   TIME TIM SL  MINFLT  MAJFLT SIZE  RSS  SRS TSIZ TRS PF*/
struct	vsav {
	short	v_slptime, v_pri;
	u_int	v_minflt, v_majflt;
	size_t	v_swrss, v_tsiz, v_txtrss, v_txtswrss;
	short	v_xccount;
	short	v_aveflt;
};

struct	proc mproc;
struct	text *text;

union {
	struct	user user;
	char	upages[UPAGES][NBPG];
} user;
#define u	user.user

#define clear(x) 	((int)x & 0x7fffffff)

int	chkpid;
int	aflg, cflg, gflg, kflg, lflg, sflg, uflg, vflg, xflg;
char	*tptr;
char	*gettty(), *getcmd(), *getname(), *savestr(), *alloc();
int	pscomp();
int	nswap;
struct	pte *Usrptma, *usrpt;

int	ndev;
struct devl {
	char	dname[DIRSIZ];
	dev_t	dev;
	struct	devl *next;
	struct	devl *cand;
} devl[256], *cand[16], *cons;

struct	savcom savcom[NPROC];
int	npr;

int	cmdstart;
int	twidth;
char	*kmemf, *memf, *swapf, *nlistf;
int	kmem, mem, swap;

int	pcbpf;
int	argaddr;
extern	char _sobuf[];

main(argc, argv)
	char **argv;
{
	register int i;
	register char *ap;
	int uid;
	off_t procp;

	setbuf(stdout, _sobuf);
	argc--, argv++;
	if (argc > 0) {
		ap = argv[0];
		while (*ap) switch (*ap++) {

		case 'a':
			aflg++;
			break;
		case 'c':
			cflg = !cflg;
			break;
		case 'g':
			gflg++;
			break;
		case 'k':
			kflg++;
			break;
		case 'l':
			lflg++;
			break;
		case 's':
			sflg++;
			break;
		case 't':
			if (*ap)
				tptr = ap;
			aflg++;
			gflg++;
			if (*tptr == '?')
				xflg++;
			while (*ap)
				ap++;
			break;
		case 'u':
			uflg++;
			break;
		case 'v':
			cflg = 1;
			vflg++;
			break;
		case 'w':
			if (twidth == 80)
				twidth = 132;
			else
				twidth = BUFSIZ;
			break;
		case 'x':
			xflg++;
			break;
		default:
			if (!isdigit(ap[-1]))
				break;
			chkpid = atoi(--ap);
			*ap = 0;
			aflg++;
			xflg++;
			break;
		}
	}
	openfiles(argc, argv);
	getkvars(argc, argv);
	getdev();
	uid = getuid();
	printhdr();
	procp = nl[X_PROC].n_value;
	for (i=0; i<NPROC; i++) {
		lseek(kmem, (char *)procp, 0);
		if (read(kmem, (char *)&mproc, sizeof mproc) != sizeof mproc)
			cantread("proc table", kmemf);
		procp += sizeof (mproc);
		if (mproc.p_stat == 0 || mproc.p_pgrp == 0 && xflg == 0)
			continue;
		if (tptr == 0 && gflg == 0 && xflg == 0 &&
		    mproc.p_ppid == 1 && (mproc.p_flag&SDETACH) == 0)
			continue;
		if (uid != mproc.p_uid && aflg==0 ||
		    chkpid != 0 && chkpid != mproc.p_pid)
			continue;
		if (vflg && gflg == 0 && xflg == 0) {
			if (mproc.p_stat == SZOMB)
				continue;
			if (mproc.p_slptime > MAXSLP &&
			    (mproc.p_stat == SSLEEP || mproc.p_stat == SSTOP))
				continue;
		}
		save();
	}
	qsort(savcom, npr, sizeof(savcom[0]), pscomp);
	for (i=0; i<npr; i++) {
		register struct savcom *sp = &savcom[i];
		if (lflg)
			lpr(sp);
		else if (vflg)
			vpr(sp);
		else if (uflg)
			upr(sp);
		else
			spr(sp);
		if (sp->ap->a_pid == 0)
			printf(" swapper");
		else if (sp->ap->a_pid == 2)
			printf(" pagedaemon");
		else
			printf(" %.*s", twidth - cmdstart - 2, sp->ap->a_cmdp);
		printf("\n");
	}
	exit(npr == 0);
}

openfiles(argc, argv)
	char **argv;
{

	kmemf = "/dev/kmem";
	if (kflg)
		kmemf = argc > 1 ? argv[1] : "/vmcore";
	kmem = open(kmemf, 0);
	if (kmem < 0) {
		perror(kmemf);
		exit(1);
	}
	if (kflg)  {
		mem = kmem;
		memf = kmemf;
	} else {
		memf = "/dev/mem";
		mem = open(memf, 0);
		if (mem < 0) {
			perror(memf);
			exit(1);
		}
	}
	swapf = argc>2 ? argv[2]: "/dev/drum";
	swap = open(swapf, 0);
	if (swap < 0) {
		perror(swapf);
		exit(1);
	}
}

getkvars(argc, argv)
	char **argv;
{
	register struct nlist *nlp;

	nlistf = argc > 3 ? argv[3] : "/vmunix";
	nlist(nlistf, nl);
	if (nl[0].n_type == 0) {
		fprintf(stderr, "%s: No namelist\n", nlistf);
		exit(1);
	}
	if (chdir("/dev") < 0) {
		perror("/dev");
		exit(1);
	}
	if (kflg)
		for (nlp = nl; nlp < &nl[sizeof (nl)/sizeof (nl[0])]; nlp++)
			nlp->n_value = clear(nlp->n_value);
	Usrptma = (struct pte *)nl[X_USRPTMA].n_value;
	usrpt = (struct pte *)nl[X_USRPT].n_value;
	lseek(kmem, (long)nl[X_NSWAP].n_value, 0);
	if (read(kmem, &nswap, sizeof (nswap)) != sizeof (nswap)) {
/*###287 [lint] read arg. 2 used inconsistently ps.c(41) :: ps.c(287)%%%*/
		cantread("nswap", kmemf);
/*###288 [lint] cantread arg. 1 used inconsistently ps.c(322) :: ps.c(288)%%%*/
/*###288 [lint] cantread arg. 2 used inconsistently ps.c(322) :: ps.c(288)%%%*/
		exit(1);
	}
	if (vflg) {
		text = (struct text *)alloc(NTEXT * sizeof (struct text));
		if (text == 0) {
			fprintf(stderr, "no room for text table\n");
			exit(1);
		}
		lseek(kmem, (long)nl[X_TEXT].n_value, 0);
		if (read(kmem, text, sizeof (text)) != sizeof (text)) {
/*###298 [lint] read arg. 2 used inconsistently ps.c(41) :: ps.c(298)%%%*/
			cantread("text table", kmemf);
/*###299 [lint] cantread arg. 1 used inconsistently ps.c(322) :: ps.c(299)%%%*/
/*###299 [lint] cantread arg. 2 used inconsistently ps.c(322) :: ps.c(299)%%%*/
			exit(1);
		}
	}
}

printhdr()
{
	char *hdr;

	if (sflg+lflg+vflg+uflg > 1) {
		fprintf(stderr, "ps: specify only one of s,l,v and u\n");
		exit(1);
	}
	hdr = lflg ? lhdr : (vflg ? vhdr : (uflg ? uhdr : shdr));
	if (lflg+vflg+uflg+sflg == 0)
		hdr += strlen(" SSIZ");
	cmdstart = strlen(hdr);
	printf("%s COMMAND\n", hdr);
	fflush(stdout);
}

cantread(what, fromwhat)
	char *what, *fromwhat;
{

	fprintf(stderr, "ps: error reading %s from %s", what, fromwhat);
}

getdev()
{
	register FILE *df;
	struct stat sbuf;
	struct direct dbuf;

	if ((df = fopen("/dev", "r")) == NULL) {
		fprintf(stderr, "Can't open /dev\n");
		exit(1);
	}
	ndev = 0;
	while (fread((char *)&dbuf, sizeof(dbuf), 1, df) == 1) {
		if (dbuf.d_ino == 0)
			continue;
		if (stat(dbuf.d_name, &sbuf) < 0)
			continue;
		if ((sbuf.st_mode&S_IFMT) != S_IFCHR)
			continue;
		strcpy(devl[ndev].dname, dbuf.d_name);
		devl[ndev].dev = sbuf.st_rdev;
		ndev++;
	}
	fclose(df);
}

save()
{
	register struct savcom *sp;
	register struct asav *ap;
	register char *cp;
	char *ttyp, *cmdp;

	if (mproc.p_stat != SZOMB && getu() == 0)
		return;
	ttyp = gettty();
	if (tptr && strcmpn(tptr, ttyp, 2))
		return;
	sp = &savcom[npr];
	cmdp = getcmd();
	if (cmdp == 0)
		return;
	sp->ap = ap = (struct asav *)alloc(sizeof (struct asav));
	sp->ap->a_cmdp = cmdp;
#define e(a,b) ap->a = mproc.b
	e(a_flag, p_flag); e(a_stat, p_stat); e(a_nice, p_nice);
	e(a_uid, p_uid); e(a_pid, p_pid); e(a_rss, p_rssize);
#undef e
	ap->a_size = mproc.p_dsize + mproc.p_ssize;
	ap->a_tty[0] = ttyp[0];
	ap->a_tty[1] = ttyp[1] ? ttyp[1] : ' ';
	ap->a_ttyd = u.u_ttyd;
	ap->a_time = u.u_vm.vm_utime + u.u_vm.vm_stime;
	if (lflg) {
		register struct lsav *lp;

		sp->sun.lp = lp = (struct lsav *)alloc(sizeof (struct lsav));
#define e(a,b) lp->a = mproc.b
		e(l_ppid, p_ppid); e(l_cpu, p_cpu);
		e(l_pri, p_pri); e(l_wchan, p_wchan);
#undef e
		lp->l_addr = pcbpf;
	} else if (vflg) {
		register struct vsav *vp;
		register struct text *xp;

		sp->sun.vp = vp = (struct vsav *)alloc(sizeof (struct vsav));
#define e(a,b) vp->a = mproc.b
		e(v_slptime, p_slptime); e(v_pri, p_pri);
		e(v_swrss, p_swrss); e(v_aveflt, p_aveflt);
#undef e
		vp->v_minflt = u.u_vm.vm_minflt;
		vp->v_majflt = u.u_vm.vm_majflt;
		if (mproc.p_textp) {
			xp = &text[mproc.p_textp -
			    (struct text *)nl[X_TEXT].n_value];
			vp->v_tsiz = xp->x_size;
			vp->v_txtrss = xp->x_rssize;
			vp->v_txtswrss = xp->x_swrss;
			vp->v_xccount = xp->x_ccount;
		} else {
			vp->v_tsiz = 0;
			vp->v_txtrss = 0;
			vp->v_xccount = 0;
			vp->v_txtswrss = 0;
		}
	} else if (uflg)
		sp->sun.u_pctcpu = 0.0;
	else if (sflg) {
		for (cp = (char *)u.u_stack;
		    cp < (char *)&u + ctob(UPAGES); )
			if (*cp++)
				break;
		sp->sun.s_ssiz = (int) ((char *)&u + ctob(UPAGES) - cp);
	}
	npr++;
}

getu()
{
	struct pte *pteaddr, apte, arguutl[UPAGES+CLSIZE];
	register int i;
	int ncl, size;

	size = sflg ? ctob(UPAGES) : sizeof (struct user);
	if ((mproc.p_flag & SLOAD) == 0) {
		lseek(swap, ctob(mproc.p_swaddr), 0);
		if (read(swap, (char *)&u, size) != size) {
			fprintf(stderr, "ps: cant read u for pid %d from %s\n",
			    mproc.p_pid, swapf);
			return (0);
		}
		pcbpf = 0;
		argaddr = 0;
		return (1);
	}
	pteaddr = &Usrptma[btokmx(mproc.p_p0br) + mproc.p_szpt - 1];
	lseek(kmem, kflg ? clear(pteaddr) : (int)pteaddr, 0);
	if (read(kmem, (char *)&apte, sizeof(apte)) != sizeof(apte)) {
		printf("ps: cant read indir pte to get u for pid %d from %s\n",
		    mproc.p_pid, swapf);
		return (0);
	}
	lseek(mem,
	    ctob(apte.pg_pfnum+1) - (UPAGES+CLSIZE) * sizeof (struct pte), 0);
	if (read(mem, (char *)arguutl, sizeof(arguutl)) != sizeof(arguutl)) {
		printf("ps: cant read page table for u of pid %d from %s\n",
		    mproc.p_pid, swapf);
		return (0);
	}
	if (arguutl[0].pg_fod == 0 && arguutl[0].pg_pfnum)
		argaddr = ctob(arguutl[0].pg_pfnum);
	else
		argaddr = 0;
	pcbpf = arguutl[CLSIZE].pg_pfnum;
	ncl = (size + NBPG*CLSIZE - 1) / (NBPG*CLSIZE);
	while (--ncl >= 0) {
		i = ncl * CLSIZE;
		lseek(mem, ctob(arguutl[CLSIZE+i].pg_pfnum), 0);
		if (read(mem, user.upages[CLSIZE+i], CLSIZE*NBPG)
		   != CLSIZE*NBPG) {
			printf("ps: cant read page %d of u of pid %d from %s\n",
			    arguutl[CLSIZE+i].pg_pfnum, mproc.p_pid, memf);
			return(0);
		}
	}
	return (1);
}

char *
getcmd()
{
	char cmdbuf[BUFSIZ];
	union {
		char	argc[CLSIZE*NBPG];
		int	argi[CLSIZE*NBPG/sizeof (int)];
	} argspac;
	register char *cp;
	register int *ip;
	char c;
	int nbad;
	struct dblock db;

	if (mproc.p_stat == SZOMB)
		return ("");
	if (cflg)
		goto retucomm;
	if ((mproc.p_flag & SLOAD) == 0 || argaddr == 0) {
		vstodb(0, CLSIZE, &u.u_smap, &db, 1);
		lseek(swap, ctob(db.db_base), 0);
		if (read(swap, (char *)&argspac, sizeof(argspac))
		    != sizeof(argspac))
			goto bad;
	} else {
		lseek(mem, argaddr, 0);
		if (read(mem, (char *)&argspac, sizeof (argspac))
		    != sizeof (argspac))
			goto bad;
	}
	ip = &argspac.argi[CLSIZE*NBPG/sizeof (int)];
	*--ip = 0;
	while (*--ip)
		if (ip == argspac.argi)
			goto retucomm;
	*(char *)ip = ' ';
	ip++;
	nbad = 0;
	for (cp = (char *)ip; cp < &argspac.argc[CLSIZE*NBPG]; cp++) {
		c = *cp & 0177;
		if (c == 0)
			*cp = ' ';
		else if (c < ' ' || c > 0176) {
			if (++nbad >= 5) {
				*cp++ = ' ';
				break;
			}
			*cp = '?';
		} else if (c == '=') {
			while (*--cp != ' ')
				if (cp <= (char *)ip)
					break;
			break;
		}
	}
	*cp = 0;
	while (*--cp == ' ')
		*cp = 0;
	cp = (char *)ip;
	strncpy(cmdbuf, cp, &argspac.argc[CLSIZE*NBPG] - cp);
	if (cp[0] == '-' || cp[0] == '?' || cp[0] <= ' ') {
		strcat(cp, " (");
		strncat(cp, u.u_comm, sizeof(u.u_comm));
		strcat(cp, ")");
	}
	if (xflg == 0 && gflg == 0 && tptr == 0 && cp[0] == '-')
		return (0);
	return (savestr(cmdbuf));

bad:
	fprintf(stderr, "ps: error locating command name for pid %d\n",
	    mproc.p_pid);
retucomm:
	strcat(cp, " (");
	strncpy(cmdbuf, u.u_comm, sizeof (u.u_comm));
	strcat(cp, ")");
	return (savestr(cmdbuf));
}

char	*lhdr =
"    F S UID   PID  PPID CP PRI NICE ADDR  SZ  RSS WCHAN TTY TIME";
lpr(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;
	register struct lsav *lp = sp->sun.lp;

	printf("%5x %c %4d%5u%6u%3d%4d%4d%6x%4d%5d",
	    ap->a_flag, "0SWRIZT"[ap->a_stat], ap->a_uid,
	    ap->a_pid, lp->l_ppid, lp->l_cpu&0377, lp->l_pri-PZERO,
	    ap->a_nice-NZERO, lp->l_addr, ap->a_size, ap->a_rss);
	printf(lp->l_wchan ? "%6x" : "      ", lp->l_wchan);
	ptty(ap->a_tty);
	ptime(ap);
}

ptty(tp)
	char *tp;
{

	printf(" %-2.2s", tp);
}

ptime(ap)
	struct asav *ap;
{

	if (ap->a_stat == SZOMB)
		printf("  <defunct>");
	else
		printf("%3ld:%02ld", ap->a_time / HZ, ap->a_time % HZ);
}

char	*uhdr =
"USER       PID %%CPU NICE  SZ  RSS TTY TIME";
upr(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;

	printf("%-8.8s%5u%5.1f%4d%4d%5d",
	    getname(ap->a_uid), ap->a_pid, sp->sun.u_pctcpu, ap->a_nice,
	    ap->a_size, ap->a_rss);
	ptty(ap->a_tty);
	ptime(ap);
}

char *vhdr =
"F     PID TT   TIME TIM SL  MINFLT  MAJFLT SIZE  RSS  SRS TSIZ TRS PF";
vpr(sp)
	struct savcom *sp;
{
	register struct vsav *vp = sp->sun.vp;
	register struct asav *ap = sp->ap;
	char stat, nice, anom;

	switch (ap->a_stat) {

	case SSLEEP:
	case SSTOP:
		if ((ap->a_flag & SLOAD) == 0)
			stat = 'W';
		else if (vp->v_pri >= PZERO)
			stat = 'S';
		else if (ap->a_flag & SPAGE)
			stat = 'P';
		else
			stat = 'D';
		break;

	case SRUN:
	case SIDL:
		stat = ap->a_flag & SLOAD ? 'R' : 'W';
		break;
	}
	nice = ap->a_nice > NZERO ? 'N' : ' ';
	anom = ap->a_flag & (SANOM|SUANOM) ? 'A' : ' ';
	printf("%c%c%c%5u", stat, nice, anom, ap->a_pid);
	ptty(ap->a_tty);
	ptime(ap);
	printf("%3d%3d%8d%8d%5d%5d%5d%5d%4d%3d",
	   ap->a_time, vp->v_slptime, vp->v_majflt, vp->v_minflt,
	   ap->a_size, ap->a_rss, vp->v_swrss, vp->v_tsiz, vp->v_txtrss,
	   vp->v_aveflt);
}

char	*shdr =
" SSIZ   PID TTY TIME";
spr(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;

	if (sflg)
		printf("%5d", sp->sun.s_ssiz);
	printf(" %5u", ap->a_pid);
	ptty(ap->a_tty);
	ptime(ap);
}

char *
gettty()
{
	register i;
	register char *p;

	if (u.u_ttyp == 0)
		return("?");
	for (i=0; i<ndev; i++) {
		if (devl[i].dev == u.u_ttyd) {
			p = devl[i].dname;
			if (p[0]=='t' && p[1]=='t' && p[2]=='y')
				p += 3;
			return(p);
		}
	}
	return("?");
}

/*
 * Given a base/size pair in virtual swap area,
 * return a physical base/size pair which is the
 * (largest) initial, physically contiguous block.
 */
vstodb(vsbase, vssize, dmp, dbp, rev)
	register int vsbase;
	int vssize;
	struct dmap *dmp;
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
	if (*ip <= 0 || *ip + blk > nswap)
		panic("vstodb *ip");
	dbp->db_size = min(vssize, blk - vsbase);
	dbp->db_base = *ip + (rev ? blk - (vsbase + dbp->db_size) : vsbase);
}

/*ARGSUSED*/
panic(cp)
	char *cp;
{

#ifdef DEBUG
	printf("%s\n", cp);
#endif
}

min(a, b)
{

	return (a < b ? a : b);
}

pscomp(s1, s2)
	struct savcom *s1, *s2;
{
	register int i;

	if (vflg)
		return (vsize(s2) - vsize(s1));
	i = s1->ap->a_ttyd - s2->ap->a_ttyd;
	if (i == 0)
		i = s1->ap->a_pid - s2->ap->a_pid;
	return (i);
}

vsize(sp)
	struct savcom *sp;
{
	register struct asav *ap = sp->ap;
	register struct vsav *vp = sp->sun.vp;
	
	if (ap->a_flag & SLOAD)
		return (ap->a_rss +
		    vp->v_txtrss / (vp->v_xccount ? vp->v_xccount : 1));
	return (vp->v_swrss + (vp->v_xccount ? 0 : vp->v_txtswrss));
}

#define	NMAX	8
#define	NUID	2048

char	names[NUID][NMAX+1];

/*
 * Stolen from ls...
 */
char *
getname(uid)
{
	register struct passwd *pw;
	static init;
	struct passwd *getpwent();

	if (names[uid][0])
		return (&names[uid][0]);
	if (init == 2)
		return (0);
	if (init == 0)
		setpwent(), init = 1;
	while (pw = getpwent()) {
		if (pw->pw_uid >= NUID)
			continue;
		if (names[pw->pw_uid][0])
			continue;
		strncpy(names[pw->pw_uid], pw->pw_name, NMAX);
		if (pw->pw_uid == uid)
			return (&names[uid][0]);
	}
	init = 2;
	endpwent();
	return (0);
}

char	*freebase;
int	nleft;

char *
alloc(size)
	int size;
{
	register char *cp;
	register int i;

	if (size > nleft) {
		freebase = (char *)sbrk(size > 2048 ? size : 2048);
		if (freebase == 0) {
			fprintf(stderr, "ps: ran out of memory\n");
			exit(1);
		}
	}
	cp = freebase;
	for (i = size; --i >= 0; )
		*cp++ = 0;
	freebase = cp;
	return (cp - size);
}

char *
savestr(cp)
	char *cp;
{
	register int len;
	register char *dp;

	len = strlen(cp);
	dp = (char *)alloc(len+1);
	strcpy(dp, cp);
	return (dp);
}
