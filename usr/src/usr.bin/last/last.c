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
static char sccsid[] = "@(#)last.c	5.6 (Berkeley) %G%";
#endif not lint

/*
 * last
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <signal.h>
#include <time.h>
#include <pwd.h>
#include <utmp.h>
#include <strings.h>
#include <stdio.h>
#include <ctype.h>

#define MAXTTYS	200				/* max ttys last can handle */
#define SECDAY	(24*60*60)			/* seconds in a day */
#define NO	0				/* false/no */
#define YES	1				/* true/yes */

static struct utmp	buf[500];		/* utmp read buffer */
#define HMAX	sizeof(buf[0].ut_host)		/* size of utmp host field */
#define LMAX	sizeof(buf[0].ut_line)		/* size of utmp tty field */
#define NMAX	sizeof(buf[0].ut_name)		/* size of utmp name field */

#define lineq(a,b)	(!strncmp(a,b,LMAX))
#define nameq(a,b)	(!strncmp(a,b,NMAX))
#define hosteq(a,b)	(!strncmp(a,b,HMAX))

typedef struct ttytab {
	long	logout;				/* log out time */
	char	tty[LMAX + 1];			/* terminal name */
} TTYS;

static TTYS	tab[MAXTTYS + 1];		/* tty table */
static char	**sargs;			/* start of selections args */

main(argc,argv)
int	argc;
char	**argv;
{
	register struct utmp	*bp;		/* current structure */
	register TTYS	*T;			/* table entry */
	register long	maxrec = -1;		/* records to display */
	register int	indx;			/* array offsets */
	struct stat	stb;			/* stat of file for size */
	long	delta,				/* time difference */
		atol(), lseek(), time();
	int	bl,				/* reads to do */
		bytes,				/* bytes read */
		wtmp,				/* wtmp file descriptor */
		onintr();
	char	*ct,				/* ctime return */
		*crmsg,				/* crash message */
		*file,				/* user specified file */
		*asctime(), *ctime(), *strspl();

	file = "/usr/adm/wtmp";
	for (--argc,sargs = argv = ++argv,indx = 0;indx < argc;++indx) {
		if (argv[indx][0] == '-' && isdigit(argv[indx][1])) {
			if ((maxrec = atol(argv[indx] + 1)) <= 0) {
				fputs("last: bad line count value.\n",stderr);
				exit(1);
			}
			++sargs;
			continue;
		}
		if (!strncmp(argv[indx],"-f",2)) {
			if (argv[indx][2]) {
				file = argv[indx] + 2;
				++sargs;
			}
			else if (++indx == argc) {
				fputs("last: option requires an argument -- f\n",stderr);
				exit(1);
			}
			else {
				file = argv[indx];
				sargs += 2;
			}
			continue;
		}
		if (strlen(argv[indx]) > 2)
			continue;
		if (!strcmp(argv[indx],"~"))
			continue;
		if (getpwnam(argv[indx]))
			continue;
		argv[indx] = strspl(argv[indx]);
	}

	if ((wtmp = open(file,O_RDONLY,0)) < 0 || fstat(wtmp,&stb) == -1) {
		perror(file);
		exit(1);
	}
	bl = (stb.st_size + sizeof(buf) - 1) / sizeof(buf);

	time(&buf[0].ut_time);
	signal(SIGINT,onintr);
	signal(SIGQUIT,onintr);

	tab[MAXTTYS].logout = -1;		/* end flag value */
	while (--bl >= 0) {
		if (lseek(wtmp,(long)(bl * sizeof(buf)),L_SET) == -1 || (bytes = read(wtmp,(char *)buf,sizeof(buf))) == -1) {
			perror(file);
			exit(1);
		}
		for (bp = &buf[bytes / sizeof(buf[0]) - 1];bp >= buf;--bp) {
			if (lineq(bp->ut_line,"~")) {
				/*
				 * if the name is empty and the terminal
				 * line is '~', it's a shutdown of some
				 * sort; see utmp(5) for more info.
				 */
				for (T = tab;T->logout != -1;++T)
					T->logout = -bp->ut_time;
				crmsg = nameq(bp->ut_name,"shutdown") ? "down " : "crash";
				if (!bp->ut_name[0])
					strcpy(bp->ut_name,"reboot");
				if (want(bp,NO)) {
					ct = ctime(&bp->ut_time);
					printf("%-*.*s  %-*.*s %-*.*s %10.10s %5.5s \n",NMAX,NMAX,bp->ut_name,LMAX,LMAX,bp->ut_line,HMAX,HMAX,bp->ut_host,ct,ct + 11);
					if (maxrec != -1 && !--maxrec)
						exit(0);
				}
				continue;
			}
			for (T = tab;;) {
				if (T->logout <= 0) {
					bcopy(bp->ut_line,T->tty,LMAX);
					break;
				}
				if (lineq(T->tty,bp->ut_line))
					break;
				if ((++T)->logout == -1) {
					fputs("last: too many terminals.\n",stderr);
					exit(1);
				}
			}
			if (bp->ut_name[0] && want(bp,YES)) {
				ct = ctime(&bp->ut_time);
				printf("%-*.*s  %-*.*s %-*.*s %10.10s %5.5s ",NMAX,NMAX,bp->ut_name,LMAX,LMAX,bp->ut_line,HMAX,HMAX,bp->ut_host,ct,ct + 11);
				if (!T->logout)
					puts("  still logged in");
				else {
					if (T->logout < 0) {
						T->logout = -T->logout;
						printf("- %s",crmsg);
					}
					else
						printf("- %5.5s",ctime(&T->logout)+11);
					delta = T->logout - bp->ut_time;
					if (delta < SECDAY)
						printf("  (%5.5s)\n",asctime(gmtime(&delta))+11);
					else
						printf(" (%ld+%5.5s)\n",delta / SECDAY,asctime(gmtime(&delta))+11);
				}
				if (maxrec != -1 && !--maxrec)
					exit(0);
			}
			T->logout = bp->ut_time;
		}
	}
	ct = ctime(&buf[0].ut_time);
	printf("\nwtmp begins %10.10s %5.5s \n",ct,ct + 11);
	exit(0);
}

onintr(signo)
int	signo;
{
	char	*ct,
		*ctime();

	ct = ctime(&buf[0].ut_time);
	printf("\ninterrupted %10.10s %5.5s \n",ct,ct + 11);
	fflush(stdout);			/* fix required for rsh */
	if (signo == SIGINT)
		exit(1);
}

want(bp,check)
register struct utmp	*bp;
int	check;
{
	register char	**indx,
			*C;
	register int	cnt;

	if (check)
		/*
		 * when uucp and ftp log in over a network, the entry in the
		 * utmp file is the name plus their process id.  See etc/ftpd.c
		 * and usr.bin/uucp/uucpd.c for more information.
		 */
		if (!strncmp(bp->ut_line,"ftp",3))
			bp->ut_line[3] = '\0';
		else if (!strncmp(bp->ut_line,"uucp",4))
			bp->ut_line[4] = '\0';
	if (!*sargs)
		return(YES);
	/*
	 * match hostname only, case independent;
	 * leave internet numbers alone
	 */
	if (!isdigit(*bp->ut_host)) {
		for (C = bp->ut_host,cnt = HMAX;*C != '.' && cnt--;++C)
			if (isupper(*C))
				*C = tolower(*C);
		if (*C == '.')
			*C = '\0';
		else
			C = NULL;
	}
	else
		C = NULL;
	for (indx = sargs;*indx;++indx)
		if (nameq(*indx,bp->ut_name) || lineq(*indx,bp->ut_line) || hosteq(*indx,bp->ut_host)) {
			if (C)
				*C = '.';
			return(YES);
		}
	return(NO);
}

char *
strspl(str)
char	*str;
{
	register char	*res;
	char	*malloc();

	if (!(res = malloc((u_int)(4 + strlen(str))))) {
		fputs("last: malloc failure.\n",stderr);
		exit(1);
	}
	strcpy(res,"tty");
	strcpy(res + 3,str);
	return(res);
}
