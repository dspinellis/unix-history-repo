/*
 * w - print system status (who and what)
 *
 * This program is similar to the systat command on Tenex/Tops 10/20
 * It needs read permission on /dev/mem, /dev/kmem, and /dev/drum.
 */
#include <sys/param.h>
#include <a.out.h>
#include <stdio.h>
#include <utmp.h>
#include <psout.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/proc.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/pte.h>
#include <sys/vm.h>

#define ARGWIDTH	33	/* # chars left on 80 col crt for args */

struct smproc {
	char	w_flag;			/* proc.p_flag */
	short	w_size;			/* proc.p_size */
	long	w_seekaddr;		/* where to find args */
	long	w_lastpg;		/* disk address of stack */
	int	w_igintr;		/* true if ignores INTR and QUIT */
	time_t	w_time;			/* CPU time used by this process */
	time_t	w_ctime;		/* CPU time used by children */
	dev_t	w_tty;			/* tty device of process */
	char	w_comm[15];		/* user.u_comm, null terminated */
	char	w_args[ARGWIDTH+1];	/* args if interesting process */
} pr[NPROC];

struct	nlist nl[] = {
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
	{ "_nswap" },
#define	X_NSWAP		5
	{ "_avenrun" },
#define	X_AVENRUN	6
	{ "_bootime" },
#define	X_BOOTIME	7
	{ 0 },
};

FILE	*ps;
FILE	*ut;
FILE	*bootfd;
int	kmem;
int	mem;
int	swap;			/* /dev/kmem, mem, and swap */
int	nswap;
dev_t	tty;
char	doing[520];		/* process attached to terminal */
time_t	proctime;		/* cpu time of process in doing */
double	avenrun[3];

#define	DIV60(t)	((t+30)/60)    /* x/60 rounded */ 
#define	TTYEQ		(tty == pr[i].w_tty)

char	*getargs();
char	*fread();
char	*ctime();
FILE	*popen();
struct	tm *localtime();

int	debug;			/* true if -d flag: debugging output */
int	header = 1;		/* true if -h flag: don't print heading */
int	lflag = 1;		/* true if -l flag: long style output */
int	login;			/* true if invoked as login shell */
int	idle;			/* number of minutes user is idle */
time_t	jobtime;		/* total cpu time visible */
time_t	now;			/* the current time of day */
struct	tm *nowt;		/* current time as time struct */
time_t	bootime, uptime;	/* time of last reboot & elapsed time since */
int	np;			/* number of processes currently active */
struct	utmp utmp;
struct	proc mproc;
struct	user up;
char	fill[512];

main(argc, argv)
	char **argv;
{
	int days;
	register int i;
	int empty;
	char obuf[BUFSIZ];

	setbuf(stdout, obuf);
	login = (argv[0][0] == '-');
	while (argc > 1) {
		if (argv[1][0] == '-') {
			for (i=1; argv[1][i]; i++) {
				switch(argv[1][i]) {

				case 'd':
					debug++;
					break;

				case 'h':
					header = 0;
					break;

				case 'l':
					lflag++;
					break;
				case 's':
					lflag = 0;
					break;

				default:
					printf("Bad flag %s\n", argv[1]);
					exit(1);
				}
			}
			argc--; argv++;
		} else {
			printf("Usage: %s [ -lh ]\n", argv[0]);
			exit(1);
		}
	}

	readpr();

	ut = fopen("/etc/utmp","r");
	if (header) {
		time(&now);
		nowt = localtime(&now);
		prtat(nowt);
		lseek(kmem, (long)nl[X_BOOTIME].n_value, 0);
		read(kmem, &bootime, sizeof (bootime));
		uptime = now - bootime;
		printf("  up");
		days = uptime / (60*60*24);
		if (days > 0) {
			printf(" %d day%s, ", days, days>1?"s":"");
			uptime %= (60*60*24);
		}
		prttime(DIV60(uptime), "");
		printf("\t\t");
		printf("load average:");
		lseek(kmem, (long)nl[X_AVENRUN].n_value, 0);
		read(kmem, avenrun, sizeof(avenrun));
		for (i = 0; i < 3; i++) {
			printf(" %.2f", avenrun[i]);
			if (i < 2)
				printf(",");
		}
		printf("\n");
		if (lflag)
			printf("User     tty       login@  idle   JCPU   PCPU  what\n");
		else
			printf("User    tty  idle  what\n");
		fflush(stdout);
	}
	for (;;) {	/* for each entry in utmp */
		if (fread(&utmp, sizeof(utmp), 1, ut) == NULL) {
			fclose(ut);
			exit(0);
		}
		if (utmp.ut_name[0] == '\0')
			continue;	/* that tty is free */
		gettty();
		jobtime = 0;
		proctime = 0;
		strcpy(doing, "-");	/* default act: normally never prints */
		empty = 1;
		idle = findidle();
		for (i=0; i<np; i++) {	/* for each process on this tty */
			if (!(TTYEQ))
				continue;
			jobtime += pr[i].w_time + pr[i].w_ctime;
			proctime += pr[i].w_time;
			if (!pr[i].w_igintr || empty) {
				if (!pr[i].w_igintr)
					empty = 0;
				strcpy(doing, lflag ? pr[i].w_args : pr[i].w_comm);
				if (doing[0]==0 || doing[0]=='-' && doing[1]<=' ' || doing[0] == '?') {
					strcat(doing, " (");
					strcat(doing, pr[i].w_comm);
					strcat(doing, ")");
				}
			}
		}
		putline();
	}
}

/* figure out the major/minor device # pair for this tty */
gettty()
{
	char ttybuf[20];
	struct stat statbuf;

	ttybuf[0] = 0;
	strcpy(ttybuf, "/dev/");
	strcat(ttybuf, utmp.ut_line);
	stat(ttybuf, &statbuf);
	tty = statbuf.st_rdev;
}

/*
 * putline: print out the accumulated line of info about one user.
 */
putline()
{
	register int tm;

	/* print login name of the user */
	printf("%-8.8s ", utmp.ut_name);

	/* print tty user is on */
	if (lflag)
		/* long form: all (up to) 8 chars */
		printf("%-8.8s", utmp.ut_line);
	else {
		/* short form: 2 chars, skipping 'tty' if there */
		if (utmp.ut_line[0]=='t' && utmp.ut_line[1]=='t' && utmp.ut_line[2]=='y')
			printf("%-2.2s", &utmp.ut_line[3]);
		else
			printf("%-2.2s", utmp.ut_line);
	}

	if (lflag)
		/* print when the user logged in */
		prtat(localtime(&utmp.ut_time));

	/* print idle time */
	prttime(idle," ");

	if (lflag) {
		/* print CPU time for all processes & children */
		prttime(DIV60(jobtime)," ");
		/* print cpu time for interesting process */
		prttime(DIV60(proctime)," ");
	}

	/* what user is doing, either command tail or args */
	printf(" %-.32s\n",doing);
	fflush(stdout);
}

/* find & return number of minutes current tty has been idle */
findidle()
{
	struct stat stbuf;
	long lastaction, diff;
	char ttyname[20];

	strcpy(ttyname, "/dev/");
	strcatn(ttyname, utmp.ut_line, 8);
	stat(ttyname, &stbuf);
	time(&now);
	lastaction = stbuf.st_atime;
	diff = now - lastaction;
	diff = DIV60(diff);
	if (diff < 0) diff = 0;
	return(diff);
}

/*
 * prttime prints a time in hours and minutes.
 * The character string tail is printed at the end, obvious
 * strings to pass are "", " ", or "am".
 */
prttime(tim, tail)
	time_t tim;
	char *tail;
{
	register int didhrs = 0;

	if (tim >= 60) {
		printf("%3d:", tim/60);
		didhrs++;
	} else {
		printf("    ");
	}
	tim %= 60;
	if (tim > 0 || didhrs) {
		printf(didhrs&&tim<10 ? "%02d" : "%2d", tim);
	} else {
		printf("  ");
	}
	printf("%s", tail);
}

/* prtat prints a 12 hour time given a pointer to a time of day */
prtat(p)
	struct tm *p;
{
	register int t, pm;

	t = p -> tm_hour;
	pm = (t > 11);
	if (t > 11)
		t -= 12;
	if (t == 0)
		t = 12;
	prttime(t*60 + p->tm_min, pm ? "pm" : "am");
}

/*
 * readpr finds and reads in the array pr, containing the interesting
 * parts of the proc and user tables for each live process.
 */
readpr()
{
	int pn, mf, addr, c;
	int szpt, pfnum, i;
	struct pte *Usrptma, *usrpt, *pte, apte;
	daddr_t swplo;
	struct dblock db;

	nlist("/vmunix", nl);
	if (nl[0].n_type==0) {
		fprintf(stderr, "No namelist\n");
		exit(1);
	}
	Usrptma = (struct pte *) nl[X_USRPTMA].n_value;
	usrpt = (struct pte *) nl[X_USRPT].n_value;
	if ((kmem = open("/dev/kmem", 0)) < 0) {
		fprintf(stderr, "No kmem\n");
		exit(1);
	}
	if((mem = open("/dev/mem", 0)) < 0) {
		fprintf(stderr, "No mem\n");
		exit(1);
	}
	if ((swap = open("/dev/drum", 0)) < 0) {
		fprintf(stderr, "No drum\n");
		exit(1);
	}
	/*
	 * read mem to find swap dev.
	 */
	lseek(kmem, (long)nl[X_SWAPDEV].n_value, 0);
	read(kmem, &nl[X_SWAPDEV].n_value, sizeof(nl[X_SWAPDEV].n_value));
	/*
	 * Find base of swap
	 */
	lseek(kmem, (long)nl[X_SWPLO].n_value, 0);
	read(kmem, &swplo, sizeof(swplo));
	lseek(kmem, (long)nl[X_NSWAP].n_value, 0);
	read(kmem, &nswap, sizeof(nswap));
	/*
	 * Locate proc table
	 */
	np = 0;
	for (pn=0; pn<NPROC; pn++) {
		lseek(kmem, (long)(nl[X_PROC].n_value + pn*(sizeof mproc)), 0);
		read(kmem, &mproc, sizeof mproc);
		/* decide if it's an interesting process */
		if (mproc.p_stat==0 || mproc.p_pgrp==0)
			continue;

		/* find & read in the user structure */
		if ((mproc.p_flag & SLOAD) == 0) {
			/* not in memory - get from swap device */
			addr = (mproc.p_swaddr+swplo)<<9;
			lseek(swap, (long)addr, 0);
			if (read(swap, &up, sizeof(up)) != sizeof(up)) {
				continue;
			}
		} else {
			/* in memory - find pages */
			for(c=0; c<UPAGES; c++) {
				lseek(mem,mproc.p_addr[c]<<9,0);
				addr = (int) ((char *)&up) + 512*c;
				if ((mf=read(mem,addr,512)) != 512)
					continue;
			}
			szpt = up.u_pcb.pcb_szpt;
			pte = &Usrptma[btokmx(mproc.p_p0br) + szpt-1];
			lseek(kmem, (int)pte, 0);
			read(kmem, &apte, sizeof(apte));
			pr[np].w_seekaddr = ctob(apte.pg_pfnum);
		}
		vstodb(0, 1, &up.u_smap, &db, 1);
		pr[np].w_lastpg = ctob(swplo + db.db_base);
		if (up.u_ttyp == NULL)
			continue;

		/* save the interesting parts */
		pr[np].w_flag = mproc.p_flag;
		pr[np].w_size = mproc.p_dsize + mproc.p_ssize;
		pr[np].w_igintr = (up.u_signal[2]==1 && up.u_signal[3]==1);
		pr[np].w_time = up.u_utime + up.u_stime;
		pr[np].w_ctime = up.u_cutime + up.u_cstime;
		pr[np].w_tty = up.u_ttyd;
		up.u_comm[14] = 0;	/* Bug: This bombs next field. */
		strcpy(pr[np].w_comm, up.u_comm);
		if (pr[np].w_igintr == 0) {
			/*
			 * Get args if there's a chance we'll print it.
			 * Cant just save pointer: getargs returns static place.
			 * Cant use strcpyn: that crock blank pads.
			 */
			pr[np].w_args[0] = 0;
			strcatn(pr[np].w_args,getargs(&pr[np]),ARGWIDTH);
		}
		np++;
	}
}

/*
 * getargs: given a pointer to a proc structure, this looks at the swap area
 * and tries to reconstruct the arguments. This is straight out of ps.
 */
char *
getargs(p)
	struct smproc *p;
{
	int c, addr, nbad;
	static int abuf[512/sizeof(int)];
	struct pte pagetbl[NPTEPG];
	register int *ip;
	register char *cp, *cp1;

	if ((p->w_flag & SLOAD) == 0) {
		lseek(swap, p->w_lastpg, 0);
		if (read(swap, abuf, sizeof(abuf)) != sizeof(abuf))
			return(p->w_comm);
	} else {
		c = p->w_seekaddr;
		lseek(mem,c,0);
		if (read(mem,pagetbl,NBPG) != NBPG)
			return(p->w_comm);
		if (pagetbl[NPTEPG-1].pg_fod==0 && pagetbl[NPTEPG-1].pg_pfnum) {
			lseek(mem,ctob(pagetbl[NPTEPG-1].pg_pfnum),0);
			if (read(mem,abuf,sizeof(abuf)) != sizeof(abuf))
				return(p->w_comm);
		} else {
			lseek(swap, p->w_lastpg, 0);
			if (read(swap, abuf, sizeof(abuf)) != sizeof(abuf))
				return(p->w_comm);
		}
	}
	abuf[127] = 0;
	for (ip = &abuf[126]; ip > abuf;) {
		/* Look from top for -1 or 0 as terminator flag. */
		if (*--ip == -1 || *ip == 0) {
			cp = (char *)(ip+1);
			if (*cp==0)
				cp++;
			nbad = 0;	/* up to 5 funny chars as ?'s */
			for (cp1 = cp; cp1 < (char *)&abuf[128]; cp1++) {
				c = *cp1&0177;
				if (c==0)  /* nulls between args => spaces */
					*cp1 = ' ';
				else if (c < ' ' || c > 0176) {
					if (++nbad >= 5) {
						*cp1++ = ' ';
						break;
					}
					*cp1 = '?';
				} else if (c=='=') {	/* Oops - found an
							 * environment var, back
							 * over & erase it. */
					*cp1 = 0;
					while (cp1>cp && *--cp1!=' ')
						*cp1 = 0;
					break;
				}
			}
			while (*--cp1==' ')	/* strip trailing spaces */
				*cp1 = 0;
			return(cp);
		}
	}
	return (p->w_comm);
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

	/* printf("%s\n", cp); */
}

min(a, b)
{

	return (a < b ? a : b);
}
