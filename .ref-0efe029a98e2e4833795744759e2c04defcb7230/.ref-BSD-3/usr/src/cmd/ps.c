/*
 *	ps - process status
 *	This is the augmented UCB ps for UCB/VM Unix (9/79)
 *	examine and print certain things about processes
 *	usage: ps [acgklrt#uvwx] [corefile] [swapfile] [system]
 */

#include <stdio.h>
#include <a.out.h>
#include <pwd.h>
#include <sys/param.h>
#include <sys/proc.h>
#include <sys/tty.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/pte.h>
#include <sys/vm.h>
#include <sys/text.h>
#include <psout.h>

struct nlist nl[] = {
	{ "_proc" },
#define	X_PROC		0
	{ "_swapdev" },
#define	X_SWAPDEV	1
	{ "_swplo" },
#define	X_SWPLO		2
	{ "_Usrptma" },
#define	X_USRPTMA	3
	{ "_usrpt" },
#define	X_USRPT		4
	{ "_text" },
#define	X_TEXT		5
	{ "_nswap" },
#define	X_NSWAP		6
	{ 0 },
};

struct	proc mproc;
struct	text text[NTEXT];

#define INTPPG		(NBPG/sizeof(int))		/* ints per page */
union {
	struct user yy;
	int xx[INTPPG][UPAGES];
      } zz;
#define clear(x) 	((int)x & 0x7fffffff)
#define u zz.yy
int	chkpid = 0;
int	aflg;	/* -a: all processes, not just mine */
int	cflg;	/* -c: complete listing of args, not just comm. */
int	gflg;	/* -g: complete listing including group headers, etc */
int	kflg;	/* -k: read from core file instead of real memory */
int	lflg;	/* -l: long listing form */
int	rflg;	/* -r: raw output in style <psout.h> */
int	sflg;	/* -s: stack depth */
int	uflg;	/* -u: user name */
int     vflg;	/* -v: virtual memory statistics */
int	wflg;	/* -w[w]: wide terminal */
int	xflg;	/* -x: ALL processes, even those without ttys */
int	login;	/* -: this is a login shell */
char	*tptr;
char	*gettty();
int	pscomp();
struct	pte pagetbl[NPTEPG];
int	kmem;
int	mem;
int	swap;
daddr_t	swplo;
int	nswap;
int	Usrptma;
int	usrpt;

int	ndev;
struct devl {
	char	dname[DIRSIZ];
	dev_t	dev;
} devl[256];

struct psout outargs[NPROC];	/* info for first npr processes */
int npr;	/* number of processes found so far */
int argwidth;	/* number of chars of args to print */

char	*coref;

main(argc, argv)
char **argv;
{
	int i;
	char *ap;
	int uid, puid;
	char obuf[BUFSIZ];
	register struct nlist *nlp;

	setbuf(stdout, obuf);
	argc--, argv++;
	if (argc>0) {
		ap = argv[0];
		while (*ap) switch (*ap++) {
		case '-':
			break;

		case 'a':
			aflg++;
			break;

		case 'c':
			cflg++;
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

		case 'r':
			rflg++;
			break;

		case 's':
			sflg++;
			break;

		case 't':
			if(*ap)
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
			vflg++;
			break;

		case 'w':
			wflg++;
			break;

		case 'x':
			xflg++;
			break;

		default:
			chkpid=atoi(--ap);
			*ap = '\0';
			aflg++;
			xflg++;
			break;
		}
	}
	coref = "/dev/kmem";
	if(kflg)
		coref = argc > 1 ? argv[1] : "/vmcore";

	if ((kmem = open(coref, 0)) < 0) {
		perror(coref);
		done(1);
	}
	if ((mem = open("/dev/mem", 0)) < 0) {
		fprintf(stderr, "No mem\n");
		done(1);
	}
	if (kflg) 
		mem = kmem;

	if ((swap = open(argc>2 ? argv[2]: "/dev/drum", 0)) < 0) {
		fprintf(stderr, "Can't open /dev/drum\n");
		done(1);
	}

	nlist(argc>3 ? argv[3] : "/vmunix", nl);
	if (nl[0].n_type==0) {
		fprintf(stderr, "No namelist\n");
		done(1);
	}

	if(chdir("/dev") < 0) {
		fprintf(stderr, "Can't change to /dev\n");
		done(1);
	}
	if (kflg)
		for (nlp= nl; nlp < &nl[sizeof (nl)/sizeof (nl[0])]; nlp++)
			nlp->n_value &= 0x7ffffffff;
	Usrptma = nl[X_USRPTMA].n_value;
	usrpt = nl[X_USRPT].n_value;
	/*
	 * read kmem to find swap dev.
	 */
	lseek(kmem, (long)nl[X_SWAPDEV].n_value, 0);
	read(kmem, &nl[X_SWAPDEV].n_value, sizeof(nl[X_SWAPDEV].n_value));
	/*
	 * Find base and size of swap
	 */
	lseek(kmem, (long)nl[X_SWPLO].n_value, 0);
	read(kmem, &swplo, sizeof(swplo));
	lseek(kmem, (long)nl[X_NSWAP].n_value, 0);
	read(kmem, &nswap, sizeof (nswap));
	/*
	 * If v flag get text table
	 */
	if (vflg) {
		lseek(kmem, (long)nl[X_TEXT].n_value, 0);
		read(kmem, text, sizeof (text));
	}
	if (kflg)
		swplo = 0;
	getdev();
	uid = getuid();
	if (sflg + lflg + vflg + uflg > 1) {
		printf("Cannot combine s, l, v, and/or u.\n");
		exit(1);
	}
	/* different psout widths depending on how much printed & w flag */
	if (wflg <= 1) {
		argwidth = 63;
		if (wflg) argwidth += 52;	/* 132 col term */
		if (lflg) argwidth -= 49;	/* extra junk printed */
		if (vflg) argwidth -= 48;	/* extra junk for -v */
		if (sflg) argwidth -= 4;	/* 4 cols of stack size */
		if (uflg) argwidth -= 27;	/* user name */
	} else  argwidth = 127;
	if (rflg)
		;	/* No heading for raw output */
	else if (lflg)
		printf("   F S UID   PID  PPID CPU PRI NICE ADDR  SZ  RSS WCHAN TTY TIME COMMAND\n");
	else if (vflg)
		printf("F    PID TT  TIME TIM SL MINFLT MAJFLT SIZE RSS SRS TSIZ TRS PF COMMAND\n");
	else if (uflg)
		printf("USER       PID %%CPU NICE  SZ  RSS TTY TIME COMMAND\n");
	else if (chkpid==0) {
		if (sflg)
			printf(" SSIZ");
		printf("   PID TTY TIME COMMAND\n");
	}
	fflush(stdout);
	for (i=0; i<NPROC; i++) {
		lseek(kmem, (long)(nl[X_PROC].n_value+i*(sizeof mproc)), 0);
		read(kmem, &mproc, sizeof mproc);
		/* skip processes that don't exist */
		if (mproc.p_stat==0)
			continue;
		/* skip those without a tty unless -x */
		if (mproc.p_pgrp==0 && xflg==0)
			continue;
		/* skip group leaders on a tty unless -g, -x, or -t.. */
		if (!gflg && !xflg && !tptr && mproc.p_pid == mproc.p_pgrp)
			continue;
		/* -g also skips those where **argv is "-" - see savcom */
		puid = mproc.p_uid;
		/* skip other peoples processes unless -a or a specific pid */
		if ((uid != puid && aflg==0) ||
		    (chkpid!=0 && chkpid!=mproc.p_pid))
			continue;
		if (savcom(puid))
			npr++;
	}
	fixup(npr);
	for (i=0; i<npr; i++)
		if (prcom(&outargs[i])) {
			putchar('\n');
			fflush(stdout);
		}
	done(!npr);
}

getdev()
{
#include <sys/stat.h>
	register FILE *df;
	struct stat sbuf;
	struct direct dbuf;

	if ((df = fopen("/dev", "r")) == NULL) {
		fprintf(stderr, "Can't open /dev\n");
		done(1);
	}
	ndev = 0;
	while (fread(&dbuf, sizeof(dbuf), 1, df) == 1) {
		if(dbuf.d_ino == 0)
			continue;
		if(stat(dbuf.d_name, &sbuf) < 0)
			continue;
		if ((sbuf.st_mode&S_IFMT) != S_IFCHR)
			continue;
		strcpy(devl[ndev].dname, dbuf.d_name);
		devl[ndev].dev = sbuf.st_rdev;
		ndev++;
	}
	fclose(df);
}

savcom(puid)
{
	int abuf[INTPPG];
	long addr;
	register int *ip;
	register struct psout *a;
	register char *cp, *cp1;
	long tm;
	int cc, nbad;
	int szpt, p0br;
	register char *tp;
	struct dblock db;
	struct pte apte;

	/* skip long sleeping or dead processes if -v unless -g or -x */
	if (!gflg && vflg && !xflg) {
		switch (mproc.p_stat) {

		case SSLEEP:
		case SSTOP:
			if (mproc.p_slptime > MAXSLP)
				return (0);
			break;

		case SRUN:
		case SIDL:
			break;

		case SZOMB:
			return (0);
		}
	}
	/* read in the user structure */
	if ((mproc.p_flag& SLOAD ) == 0) {
		/* not loaded - get from swap */
		addr = (mproc.p_swaddr+swplo)<<9;
		lseek(swap, addr, 0);
		if (read(swap, &u, sizeof(u)) != sizeof(u))
			return(0);
	} else {
		/* loaded, get each page from memory separately */
		for(cc=0; cc<UPAGES; cc++) {	/* get u area */
			int upage = ctob(mproc.p_addr[cc]);
			lseek(mem,upage,0);
			if (read(mem,((int *)&u)+INTPPG*cc,NBPG) != NBPG)	
				return(0);
		}
	}
	tp = gettty();
	if (tptr && strcmpn(tptr, tp, 2))
		return(0);
	a = &outargs[npr];
	/* saving com starts here */
	a->o_uid = puid;
	a->o_pid = mproc.p_pid;
	a->o_flag = mproc.p_flag;
	a->o_ppid = mproc.p_ppid;
	a->o_cpu  = mproc.p_cpu;
	a->o_pctcpu = 0.0;	/* This needs to be fixed later */
	a->o_pri  = mproc.p_pri;
	a->o_nice = mproc.p_nice;
	a->o_addr0 = mproc.p_addr[0];
	a->o_dsize = mproc.p_dsize;
	a->o_ssize = mproc.p_ssize;
	a->o_rssize = mproc.p_rssize;
	a->o_swrss = mproc.p_swrss;
	a->o_wchan = mproc.p_wchan;
	a->o_pgrp = mproc.p_pgrp;
	a->o_tty[0] = tp[0];
	a->o_tty[1] = tp[1] ? tp[1] : ' ';
	a->o_ttyd = u.u_ttyd;
	a->o_stat = mproc.p_stat;
	a->o_flag = mproc.p_flag;
	if (a->o_stat==SZOMB) return(1);
	a->o_utime = u.u_utime;
	a->o_stime = u.u_stime;
	a->o_cutime = u.u_cutime;
	a->o_cstime = u.u_cstime;
	a->o_sigs = u.u_signal[SIGINT] + u.u_signal[SIGQUIT];
	a->o_time = mproc.p_time;
	a->o_slptime = mproc.p_slptime;
	a->o_uname[0] = 0;
	if (sflg) {
		for (cp = (char *)u.u_stack; cp < (char *)&u + ctob(UPAGES); cp++)
			if (*cp)
				break;
		a->o_stksize = (int) ((char *)&u + ctob(UPAGES) - cp);
	}
	if (mproc.p_stat==SZOMB) return(1);
	if (vflg) {
		register struct text *xp;

		if (mproc.p_textp) {
			xp = &text[mproc.p_textp - (struct text *)nl[5].n_value];
			a->o_xsize = xp->x_size;
			a->o_xrssize = xp->x_rssize;
		} else {
			a->o_xsize = 0;
			a->o_xrssize = 0;
		}
		a->o_aveflt = mproc.p_aveflt;
		a->o_minorflt = u.u_minorflt;
		a->o_majorflt = u.u_majorflt;
	}
	strcpy(a->o_comm, u.u_comm);
	if (cflg)
		return (1);
	a->o_args[0] = 0;	/* in case of early return */
	if ((mproc.p_flag & SLOAD) == 0) {
		vstodb(0, 1, &u.u_smap, &db, 1);
		addr = ctob(swplo + db.db_base);
		lseek(swap, addr, 0);
		if (read(swap, abuf, sizeof(abuf)) != sizeof(abuf))
			goto garbage;
	} else {
		szpt = u.u_pcb.pcb_szpt;
		p0br = kflg ? clear((int)mproc.p_p0br) : (int)mproc.p_p0br;
		cc = Usrptma + (p0br + NBPG*(szpt-1) - usrpt)/NPTEPG;
		lseek(kmem, cc, 0);
		if (read(kmem, &apte, sizeof(apte)) != sizeof(apte))
			goto garbage;
		lseek(mem, ctob(apte.pg_pfnum), 0);
		if (read(mem,pagetbl,sizeof(pagetbl)) != sizeof(pagetbl))   
			goto garbage;
		if (pagetbl[NPTEPG-1].pg_fod == 0 && pagetbl[NPTEPG-1].pg_pfnum) {
			lseek(mem,ctob((pagetbl[NPTEPG-1].pg_pfnum)),0);
			if (read(mem,abuf,sizeof(abuf)) != sizeof(abuf))
				goto garbage;
		} else {
			vstodb(0, 1, &u.u_smap, &db, 1);
			addr = ctob(swplo + db.db_base);
			lseek(swap, addr, 0);
			if (read(swap, abuf, sizeof(abuf)) != sizeof(abuf))
				goto garbage;
		}
	}
	abuf[INTPPG] = 0;
	for (ip = &abuf[INTPPG-2]; ip > abuf;) {
		if (*--ip == -1 || *ip == 0) {
			cp = (char *)(ip+1);
			if (*cp==0)
				cp++;
			nbad = 0;
			for (cp1 = cp; cp1 < (char *)&abuf[INTPPG]; cp1++) {
				cc = *cp1&0177;
				if (cc==0)
					*cp1 = ' ';
				else if (cc < ' ' || cc > 0176) {
					if (++nbad >= 5) {
						*cp1++ = ' ';
						break;
					}
					*cp1 = '?';
				} else if (cc=='=') {
					*cp1 = 0;
					while (cp1>cp && *--cp1!=' ')
						*cp1 = 0;
					break;
				}
			}
			while (*--cp1==' ')
				*cp1 = 0;
			strcpy(a->o_args, cp);
garbage:
			cp = a->o_args;
			if (cp[0]=='-'&&cp[1]<=' ' || cp[0]=='?' || cp[0]<=' ') {
				strcat(cp, " (");
				strcat(cp, u.u_comm);
				strcat(cp, ")");
			}
			cp[127] = 0;	/* max room in psout is 128 chars */
			if (xflg || gflg || tptr || cp[0]!='-')
				return(1);
			return(0);
		}
	}
	goto garbage;
}

prcom(a)
	register struct psout *a;
{
	long tm;

	if (rflg) {
		write(1, a, sizeof (*a));
		return(0);
	}
	if (lflg) {
		printf("%4x %c", 0xffff & a->o_flag,
			"0SWRIZT"[a->o_stat]);
		printf("%4d", a->o_uid);
	} else if (vflg) {
		switch (a->o_stat) {

		case SSLEEP:
		case SSTOP:
			if ((a->o_flag & SLOAD) == 0)
				printf("W");
			else if (a->o_pri >= PZERO)
				printf("S");
			else if (a->o_flag & SPAGE)
				printf("P");
			else
				printf("D");
			break;

		case SRUN:
		case SIDL:
			if (a->o_flag & SLOAD)
				printf("R");
			else
				printf("W");
			break;
		}
		if (a->o_nice > NZERO)
			printf("N");
		else
			printf(" ");
	} else if (uflg) {
		printf("%-8.8s", a->o_uname);
	}
	if (sflg) {
		printf("%5d", a->o_stksize);
	}
	printf("%6u", a->o_pid);
	if (lflg)
		printf("%6u%4d%4d%4d%6x", a->o_ppid, a->o_cpu&0377,
			a->o_pri, a->o_nice, a->o_addr0);
	else if (uflg)
		printf("%5.1f%4d ", a->o_pctcpu, a->o_nice);
	if (lflg || uflg)
		printf("%4d%5d", a->o_dsize+a->o_ssize, a->o_rssize);
	if (lflg)
		if (a->o_wchan)
			printf("%6x", clear(a->o_wchan));
		else
			printf("      ");
	printf(" %-2.2s", a->o_tty);
	if (a->o_stat==SZOMB) {
		printf("  <defunct>");
		return(1);
	}
	tm = (a->o_utime + a->o_stime + 30)/60;
	printf("%3ld:", tm/60);
	tm %= 60;
	printf(tm<10?"0%ld":"%ld", tm);
	if (vflg) {
/*
		tm = (a->o_stime + 30) / 60;
		printf(" %2ld:", tm/60);
		tm %= 60;
		printf(tm<10?"0%ld":"%ld", tm);
*/
		printf("%4d%3d", a->o_time, a->o_slptime);
	}
#ifdef notdef
	if (0 && lflg==0) {	/* 0 == old tflg (print long times) */
		tm = (a->o_cstime + 30)/60;
		printf(" %2ld:", tm/60);
		tm %= 60;
		printf(tm<10?"0%ld":"%ld", tm);
		tm = (a->o_cutime + 30)/60;
		printf(" %2ld:", tm/60);
		tm %= 60;
		printf(tm<10?"0%ld":"%ld", tm);
	}
#endif
	if (vflg) {
		printf("%7d%7d",a->o_minorflt,a->o_majorflt);
		printf("%5d%4d%4d", a->o_dsize+a->o_ssize, a->o_rssize, a->o_swrss);
		printf("%5d%4d", a->o_xsize, a->o_xrssize);
		printf("%3d", a->o_aveflt);
	}
	if (a->o_pid == 0) {
		printf(" swapper");
		return(1);
	}
	if (a->o_pid == 2) {
		printf(" pagedaemon");
		return(1);
	}
	if (cflg) {
		printf(" %s", a->o_comm);
		return(1);
	}
	a -> o_args[argwidth] = 0;	/* force it to quit early */
	printf(" %s", a->o_args);
	return (1);
}

char *
gettty()
{
	register i;
	register char *p;

	if (u.u_ttyp==0)
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

done(exitno)
{
	if (login) {
		printf("Press return when done: ");
		getchar();
	}
	exit(exitno);
}

/*
 * fixup figures out everybodys name and sorts into a nice order.
 */
fixup(np) int np; {
	register int i;
	register struct passwd *pw;
	struct passwd *getpwent();

	if (uflg) {
		/*
		 * If we want names, traverse the password file. For each
		 * passwd entry, look for it in the processes.
		 * In case of multiple entries in /etc/passwd, we believe
		 * the first one (same thing ls does).
		 */
		while ((pw=getpwent()) != NULL) {
			for (i=0; i<np; i++)
				if (outargs[i].o_uid == pw -> pw_uid) {
					if (outargs[i].o_uname[0] == 0)
						strcpy(outargs[i].o_uname, pw -> pw_name);
				}
		}
	}

	qsort(outargs, np, sizeof(outargs[0]), pscomp);
}

pscomp(x1, x2) struct psout *x1, *x2; {
	register int c;

	c = (x1)->o_ttyd - (x2)->o_ttyd;
	if (c==0) c = (x1)->o_pid - (x2)->o_pid;
	return(c);
}
