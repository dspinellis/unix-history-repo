/*
 *	ps - process status
 *	examine and print certain things about processes
 */

#include <stdio.h>
#include <a.out.h>
#include <core.h>
#include <sys/param.h>
#include <sys/proc.h>
#include <sys/tty.h>
#include <sys/dir.h>
#include <sys/user.h>

struct nlist nl[] = {
	{ "_proc" },
	{ "_swapdev" },
	{ "_swplo" },
	{ "" },
};

struct	proc mproc;

struct	user u;
int	chkpid;
int	retcode=1;
int	lflg;
int	vflg;
int	kflg;
int	xflg;
char	*tptr;
long	lseek();
char	*gettty();
char	*getptr();
char	*strncmp();
int	aflg;
int	mem;
int	swmem;
int	swap;
daddr_t	swplo;

int	ndev;
struct devl {
	char	dname[DIRSIZ];
	dev_t	dev;
} devl[256];

char	*coref;

main(argc, argv)
char **argv;
{
	int i;
	char *ap;
	int uid, puid;

	if (argc>1) {
		ap = argv[1];
		while (*ap) switch (*ap++) {

		case 'v':
			vflg++;
			break;

		case 'a':
			aflg++;
			break;

		case 't':
			if(*ap)
				tptr = ap;
			aflg++;
			if (*tptr == '?')
				xflg++;
			goto bbreak;

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
			chkpid = atoi(ap-1);
			goto bbreak;
			break;
		}
	}

bbreak:
	if(chdir("/dev") < 0) {
		fprintf(stderr, "Can't change to /dev\n");
		exit(1);
	}
	nlist(argc>2? argv[2]:"/unix", nl);
	if (nl[0].n_type==0) {
		fprintf(stderr, "No namelist\n");
		exit(1);
	}
	coref = "/dev/mem";
	if(kflg)
		coref = "/usr/sys/core";
	if ((mem = open(coref, 0)) < 0) {
		fprintf(stderr, "No mem\n");
		exit(1);
	}
	swmem = open(coref, 0);
	/*
	 * read mem to find swap dev.
	 */
	lseek(mem, (long)nl[1].n_value, 0);
	read(mem, (char *)&nl[1].n_value, sizeof(nl[1].n_value));
	/*
	 * Find base of swap
	 */
	lseek(mem, (long)nl[2].n_value, 0);
	read(mem, (char *)&swplo, sizeof(swplo));
	/*
	 * Locate proc table
	 */
	lseek(mem, (long)nl[0].n_value, 0);
	getdev();
	uid = getuid();
	if (lflg)
	printf(" F S UID   PID  PPID CPU PRI NICE  ADDR  SZ  WCHAN TTY TIME CMD\n"); else
		if (chkpid==0) printf("   PID TTY TIME CMD\n");
	for (i=0; i<NPROC; i++) {
		read(mem, (char *)&mproc, sizeof mproc);
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
	exit(retcode);
}

getdev()
{
#include <sys/stat.h>
	register FILE *df;
	struct stat sbuf;
	struct direct dbuf;

	if ((df = fopen("/dev", "r")) == NULL) {
		fprintf(stderr, "Can't open /dev\n");
		exit(1);
	}
	ndev = 0;
	while (fread((char *)&dbuf, sizeof(dbuf), 1, df) == 1) {
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
		exit(1);
	}
}

long
round(a, b)
	long		a, b;
{
	long		w = ((a+b-1)/b)*b;

	return(w);
}

struct map {
	long	b1, e1; long f1;
	long	b2, e2; long f2;
};
struct map datmap;
int	file;
prcom(puid)
{
	char abuf[512];
	long addr;
	register int *ip;
	register char *cp, *cp1;
	long tm;
	int c, nbad;
	register char *tp;
	long txtsiz, datsiz, stksiz;
	int septxt;
	int lw=(lflg?35:80);
	char **ap;

	if (mproc.p_flag&SLOAD) {
		addr = ctob((long)mproc.p_addr);
		file = swmem;
	} else {
		addr = (mproc.p_addr+swplo)<<9;
		file = swap;
	}
	lseek(file, addr, 0);
	if (read(file, (char *)&u, sizeof(u)) != sizeof(u))
		return(0);

	/* set up address maps for user pcs */
	txtsiz = ctob(u.u_tsize);
	datsiz = ctob(u.u_dsize);
	stksiz = ctob(u.u_ssize);
	septxt = u.u_sep;
	datmap.b1 = (septxt ? 0 : round(txtsiz,TXTRNDSIZ));
	datmap.e1 = datmap.b1+datsiz;
	datmap.f1 = ctob(USIZE)+addr;
	datmap.b2 = stackbas(stksiz);
	datmap.e2 = stacktop(stksiz);
	datmap.f2 = ctob(USIZE)+(datmap.e1-datmap.b1)+addr;

	tp = gettty();
	if (tptr && strncmp(tptr, tp, 2))
		return(0);
	if (lflg) {
		printf("%2o %c%4d", mproc.p_flag,
			"0SWRIZT"[mproc.p_stat], puid);
	}
	printf("%6u", mproc.p_pid);
	if (lflg) {
		printf("%6u%4d%4d%5d%6o%4d", mproc.p_ppid, mproc.p_cpu&0377,
			mproc.p_pri,
			mproc.p_nice,
			mproc.p_addr, (mproc.p_size+7)>>3);
		if (mproc.p_wchan)
			printf("%7o", mproc.p_wchan);
		else
			printf("       ");
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
	if (vflg && lflg==0) {	/* 0 == old tflg (print long times) */
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
	addr += ctob((long)mproc.p_size) - 512;

	/* look for sh special */
	lseek(file, addr+512-sizeof(char **), 0);
	if (read(file, (char *)&ap, sizeof(char *)) != sizeof(char *))
		return(1);
	if (ap) {
		char b[82];
		char *bp = b;
		while((cp=getptr(ap++)) && cp && (bp<b+lw) ) {
			nbad = 0;
			while((c=getbyte(cp++)) && (bp<b+lw)) {
				if (c<' ' || c>'~') {
					if (nbad++>3)
						break;
					continue;
				}
				*bp++ = c;
			}
			*bp++ = ' ';
		}
		*bp++ = 0;
		printf(lflg?" %.30s":" %.60s", b);
		return(1);
	}

	lseek(file, addr, 0);
	if (read(file, abuf, sizeof(abuf)) != sizeof(abuf))
		return(1);
	for (ip = (int *)&abuf[512]-2; ip > (int *)abuf; ) {
		if (*--ip == -1 || *ip==0) {
			cp = (char *)(ip+1);
			if (*cp==0)
				cp++;
			nbad = 0;
			for (cp1 = cp; cp1 < &abuf[512]; cp1++) {
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

char *
getptr(adr)
char **adr;
{
	char *ptr;
	register char *p, *pa;
	register i;

	ptr = 0;
	pa = (char *)adr;
	p = (char *)&ptr;
	for (i=0; i<sizeof(ptr); i++)
		*p++ = getbyte(pa++);
	return(ptr);
}

getbyte(adr)
char *adr;
{
	register struct map *amap = &datmap;
	char b;
	long saddr;

	if(!within(adr, amap->b1, amap->e1)) {
		if(within(adr, amap->b2, amap->e2)) {
			saddr = (unsigned)adr + amap->f2 - amap->b2;
		} else
			return(0);
	} else
		saddr = (unsigned)adr + amap->f1 - amap->b1;
	if(lseek(file, saddr, 0)==-1
		   || read(file, &b, 1)<1) {
		return(0);
	}
	return((unsigned)b);
}


within(adr,lbd,ubd)
char *adr;
long lbd, ubd;
{
	return((unsigned)adr>=lbd && (unsigned)adr<ubd);
}
