/*
 *	ps - process status
 *	examine and print certain things about processes
 */

#include <stdio.h>
#include <a.out.h>
#include <sys/param.h>
#include <sys/proc.h>
#include <sys/tty.h>
#include <sys/dir.h>
#include <sys/user.h>

struct nlist nl[] = {
	{ "_proc" },
	{ "_swapdev" },
	{ "_swplo" },
	{ 0 },
};

struct	proc mproc;

union {
	struct user yy;
	int xx[128][UPAGES];
      } zz;
#define u zz.yy
int	chkpid;
int	retcode=1;
int	lflg;
int	kflg;
int	xflg;
char	*tptr;
char	*gettty();
int	aflg;
int	pagetbl[128];
int	mem;
int	swmem;
int	swap;
daddr_t	swplo;

int	ndev;
struct devl {
	char	dname[DIRSIZ];
	dev_t	dev;
} devl[256];

char	*coref, *memf;

main(argc, argv)
char **argv;
{
	int i;
	char *ap;
	int uid, puid;

	if (argc>1) {
		ap = argv[1];
		while (*ap) switch (*ap++) {
		case 'a':
			aflg++;
			break;

		case 't':
			if(*ap)
				tptr = ap;
			aflg++;
			if (*tptr == '?')
				xflg++;
			break;

		case 'x':
			xflg++;
			break;

		case '-':
			break;

		case 'l':
			lflg++;
			break;

		case 'k':
			kflg++;
			break;

		default:
			chkpid=atoi(--ap);
			*ap = '\0';
			break;
		}
	}

	if(chdir("/dev") < 0) {
		fprintf(stderr, "Can't change to /dev\n");
		done(1);
	}
	nlist(argc>2? argv[2]:"/unix", nl);
	if (nl[0].n_type==0) {
		fprintf(stderr, "No namelist\n");
		done(1);
	}
	coref = "/dev/kmem";
	memf = "/dev/mem";
	if(kflg) {
		coref = "/usr/sys/core";
		memf = coref;
		nl[0].n_value = (char *)((int)nl[0].n_value & 0x7fffffff);
		nl[1].n_value = (char *)((int)nl[1].n_value & 0x7fffffff);
		nl[2].n_value = (char *)((int)nl[2].n_value & 0x7fffffff);
	}
	if ((mem = open(coref, 0)) < 0) {
		fprintf(stderr, "No kmem\n");
		done(1);
	}
	if((swmem = open(memf, 0)) < 0) {
		fprintf(stderr, "No mem\n");
		done(1);
	}
	/*
	 * read mem to find swap dev.
	 */
	lseek(mem, (long)nl[1].n_value, 0);
	read(mem, &nl[1].n_value, sizeof(nl[1].n_value));
	/*
	 * Find base of swap
	 */
	lseek(mem, (long)nl[2].n_value, 0);
	read(mem, &swplo, sizeof(swplo));
	/*
	 * Locate proc table
	 */
	lseek(mem, (long)nl[0].n_value, 0);
	getdev();
	uid = getuid();
	if (lflg)
	printf("  F S UID   PID  PPID CPU PRI NICE   ADDR   SZ   WCHAN  TTY TIME COMMAND\n"); else
		if (chkpid==0) printf("   PID TTY TIME COMMAND\n");
	for (i=0; i<NPROC; i++) {
		read(mem, &mproc, sizeof mproc);
		if (mproc.p_stat==0)
			continue;
		if (mproc.p_pgrp==0 && xflg==0 && mproc.p_uid==0)
			continue;
		puid = mproc.p_uid;
		if ((uid != puid && aflg==0) ||
		    (chkpid!=0 && chkpid!=mproc.p_pid))
			continue;
		if(prcom(puid)) {
			printf("\n");
			retcode=0;
		}
	}
	done(retcode);
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
	if ((swap = open("/dev/swap", 0)) < 0) {
		fprintf(stderr, "Can't open /dev/swap\n");
		done(1);
	}
}

prcom(puid)
{
	int abuf[128];
	long addr;
	int mf;
	register int *ip;
	register char *cp, *cp1;
	long tm;
	int c, nbad;
	register char *tp;

	if ((mproc.p_flag& (SLOAD | SSPART)) == 0) {
		addr = (mproc.p_swaddr+swplo)<<9;
		mf = swap;
		lseek(mf, addr, 0);
		if (read(mf, &u, sizeof(u)) != sizeof(u))
			return(0);
	} else {
		for(c=0; c<UPAGES; c++) {
			lseek(swmem,mproc.p_addr[c]<<9,0);
			if (read(swmem,((int *)&u)+128*c,512) != 512)	/* get u page */
				return(0);
		}
	}
	tp = gettty();
	if (tptr && strcmpn(tptr, tp, 2))
		return(0);
	if (lflg) {
		printf("%3o %c%4d", 0377 & mproc.p_flag,
			"0SWRIZT"[mproc.p_stat], puid);
	}
	printf("%6u", mproc.p_pid);
	if (lflg) {
		printf("%6u%4d%4d%5d%8x%4d", mproc.p_ppid, mproc.p_cpu&0377,
			mproc.p_pri,
			mproc.p_nice,
			mproc.p_addr[0], mproc.p_size);
		if (mproc.p_wchan)
			printf("%9x", mproc.p_wchan); else
			printf("         ");
	}
	printf(" %-2.2s", tp);
	if (mproc.p_stat==SZOMB) {
		printf("  <defunct>");
		return(1);
	}
	tm = (u.u_utime + u.u_stime + 30)/60;
	printf(" %2ld:", tm/60);
	tm %= 60;
	printf(tm<10?"0%ld":"%ld", tm);
	if (0 && lflg==0) {	/* 0 == old tflg (print long times) */
		tm = (u.u_cstime + 30)/60;
		printf(" %2ld:", tm/60);
		tm %= 60;
		printf(tm<10?"0%ld":"%ld", tm);
		tm = (u.u_cutime + 30)/60;
		printf(" %2ld:", tm/60);
		tm %= 60;
		printf(tm<10?"0%ld":"%ld", tm);
	}
	if (mproc.p_pid == 0) {
		printf(" swapper");
		return(1);
	}
	c = mproc.p_size - btoc(512);
	if ((mproc.p_flag & SLOAD) == 0) {
		addr += ctob(c);
		lseek(mf, addr, 0);
		if (read(mf, abuf, sizeof(abuf)) != sizeof(abuf))
			return(1);
	} else {
		if (u.u_pcb.pcb_szpt<1 || u.u_pcb.pcb_szpt>20)
			return(1);
		c = ctob((u.u_ptable[u.u_pcb.pcb_szpt-1] & 0x1fffff));
		lseek(swmem,c,0);
		if (read(swmem,pagetbl,512) != 512)	/* get last page table */
			return(1);
		lseek(swmem,ctob((pagetbl[127] & 0x1fffff)),0);
		if (read(swmem,abuf,sizeof(abuf)) != sizeof(abuf))
			return(1);
	}
	abuf[128] = 0;
	for (ip = &abuf[126]; ip > abuf;) {
		if (*--ip == -1 || *ip == 0) {
			cp = (char *)(ip+1);
			if (*cp==0)
				cp++;
			nbad = 0;
			for (cp1 = cp; cp1 < (char *)&abuf[128]; cp1++) {
				c = *cp1&0177;
				if (c==0)
					*cp1 = ' ';
				else if (c < ' ' || c > 0176) {
					if (++nbad >= 5) {
						*cp1++ = ' ';
						break;
					}
					*cp1 = '?';
				} else if (c=='=') {
					*cp1 = 0;
					while (cp1>cp && *--cp1!=' ')
						*cp1 = 0;
					break;
				}
			}
			while (*--cp1==' ')
				*cp1 = 0;
			printf(lflg?" %.30s":" %.60s", cp);
			return(1);
		}
	}
	return(1);
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

done(exitno)
{
	exit(exitno);
}
