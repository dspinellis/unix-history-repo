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
static char sccsid[] = "@(#)man.c	5.12 (Berkeley) 3/20/86";
#endif not lint

#include <stdio.h>
#include <ctype.h>
#include <sgtty.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <signal.h>
#include <strings.h>

/*
 * man
 * link also to apropos and whatis
 * This version uses more for underlining and paging.
 */
#define	NROFFCAT "nroff -mandoc"	/* for nroffing to cat file */
#define	NROFF	"nroff -mandoc"		/* for nroffing to tty */
#define	MORE	"more -s"		/* paging filter */
#define	CAT_	"/bin/cat"		/* for when output is not a tty */
#define	CAT_S	"/bin/cat -s"		/* for '-' opt (no more) */

#define TROFFCMD "troff -man %s"

#define	ALLSECT	"1nl6823457po"	/* order to look through sections */
#define	SECT1	"1nlo"		/* sections to look at if 1 is specified */
#define	SUBSEC1	"cg"		/* subsections to try in section 1 */
#define	SUBSEC3	"sxmncf"
#define	SUBSEC4	"pfn"
#define	SUBSEC8	"cv"

#define	WHATIS	"whatis.db"

int	nomore;
char	*CAT	= CAT_;
char	*manpath = "/usr/share/man:/usr/local/man:/usr/gnu/man:/usr/X386/man";
char	*strcpy();
char	*strcat();
char	*getenv();
char	*calloc();
char	*trim();
void	nuke(int);
int	apropos();
int	whatis();
int	section;
int	subsec;
int	troffit;
int	mypid;

#define	eq(a,b)	(strcmp(a,b) == 0)

main(argc, argv)
	int argc;
	char *argv[];
{
	char *mp;

	if ((mp = getenv("MANPATH")) != NULL)
		manpath = mp;
	umask(0);
	mypid = getpid();
	if (strcmp(argv[0], "apropos") == 0) {
		runpath(argc-1, argv+1, apropos);
		exit(0);
	}
	if (strcmp(argv[0], "whatis") == 0) {
		runpath(argc-1, argv+1, whatis);
		exit(0);
	}
	if (argc <= 1) {
		fprintf(stderr, "Usage: man [ section ] name ...\n");
		exit(1);
	}
	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		switch(argv[0][1]) {

		case 0:
			nomore++;
			CAT = CAT_S;
			break;

		case 't':
			troffit++;
			break;

		case 'k':
			apropos(argc-1, argv+1);
			exit(0);

		case 'f':
			whatis(argc-1, argv+1);
			exit(0);

		case 'P':		/* backwards compatibility */
		case 'M':
			if (argc < 2) {
				fprintf(stderr, "%s: missing path\n", *argv);
				exit(1);
			}
			argc--, argv++;
			manpath = *argv;
			break;
		}
		argc--, argv++;
	}
	if (troffit == 0 && nomore == 0 && !isatty(1))
		nomore++;
	section = 0;
	do {
		if (eq(argv[0], "local")) {
			section = 'l';
			goto sectin;
		} else if (eq(argv[0], "new")) {
			section = 'n';
			goto sectin;
		} else if (eq(argv[0], "old")) {
			section = 'o';
			goto sectin;
		} else if (eq(argv[0], "public")) {
			section = 'p';
			goto sectin;
		} else if (isdigit(argv[0][0]) &&
		    (argv[0][1] == 0 || argv[0][2] == 0)) {
			section = argv[0][0];
			subsec = argv[0][1];
sectin:
			argc--, argv++;
			if (argc == 0) {
				fprintf(stderr,
				    "But what do you want from section %s?\n",
				    argv[-1]);
				exit(1);
			}
			continue;
		}
		manual(section, argv[0]);
		argc--, argv++;
	} while (argc > 0);
	exit(0);
}

runpath(ac, av, f)
	int ac;
	char *av[];
	int (*f)();
{

	if (ac > 0 && strcmp(av[0], "-M") == 0 || strcmp(av[0], "-P") == 0) {
		if (ac < 2) {
			fprintf(stderr, "%s: missing path\n", av[0]);
			exit(1);
		}
		manpath = av[1];
		ac -= 2, av += 2;
	}
	(*f)(ac, av);
	exit(0);
}

manual(sec, name)
	char sec, *name;
{
	char section = sec;
	char work[100], work2[100];
	char path[MAXPATHLEN+1], realname[MAXPATHLEN+1];
	char cmdbuf[150];
	int ss = 0;
	struct stat stbuf, stbuf2;
	int last;
	char *sp = ALLSECT;
	FILE *it;
	char abuf[BUFSIZ];

	strcpy(work, "manx/");
	strcat(work, name);
	strcat(work, ".x");
	last = strlen(work) - 1;
	if (section == '1') {
		sp = SECT1;
		section = 0;
	}
	if (section == 0) {		/* no section or section 1 given */
		for (section = *sp++; section; section = *sp++) {
			work[3] = section;
			work[last] = section;
			work[last+1] = 0;
			work[last+2] = 0;
			if (pathstat(work, path, &stbuf))
				break;
			if (work[last] >= '1' && work[last] <= '8') {
				char *cp;
search:
				switch (work[last]) {
				case '1': cp = SUBSEC1; break;
				case '3': cp = SUBSEC3; break;
				case '4': cp = SUBSEC4; break;
				case '8': cp = SUBSEC8; break;
				default:  cp = "0"; break;
				}
				while (*cp) {
					work[last+1] = *cp++;
					if (pathstat(work, path, &stbuf)) {
						ss = work[last+1];
						goto found;
					}
				}
				if (ss == 0)
					work[last+1] = 0;
			}
		}
		if (section == 0) {
			if (sec == 0)
				printf("No manual entry for %s.\n", name);
			else
				printf("No entry for %s in section %c%s.\n",
				   name, sec, " of the manual");
			return;
		}
	} else {			/* section given */
		work[3] = section;
		work[last] = section;
		work[last+1] = subsec;
		work[last+2] = 0;
		if (!pathstat(work, path, &stbuf)) {
			if ((section >= '1' && section <= '8') && subsec == 0) {
				sp = "\0";
				goto search;
			}
			else if (section == 'o') {	/* XXX */
				char *cp;
				char sec;
				for (sec = '0'; sec <= '8'; sec++) {
					work[last] = sec;
					if (pathstat(work, path, &stbuf))
						goto found;
					switch (work[last]) {
					case '1': cp = SUBSEC1; break;
					case '3': cp = SUBSEC3; break;
					case '4': cp = SUBSEC4; break;
					case '8': cp = SUBSEC8; break;
					default:  cp = ""; break;
					}
					while (*cp) {
						work[last+1] = *cp++;
						if (pathstat(work, path, &stbuf)) {
							ss = work[last+1];
							goto found;
						}
					}
					if (ss == 0)
						work[last+1] = 0;
				}
			}
			printf("No entry for %s in section %c", name, section);
			if (subsec)
				putchar(subsec);
			printf(" of the manual.\n");
			return;
		}
	}
found:
	sprintf(realname, "%s/%s", path, work);
	if (troffit) {
		troff(path, work);
		return;
	}
	if (!nomore) {
		if ((it = fopen(realname, "r")) == NULL) {
			goto catit;
		}
		if (fgets(abuf, BUFSIZ-1, it) &&
		   strncmp(abuf, ".so ", 4) == 0) {
			register char *cp = abuf+4;
			char *dp;

			while (*cp && *cp != '\n')
				cp++;
			*cp = 0;
			while (cp > abuf && *--cp != '/')
				;
			dp = ".so man";
			if (cp != abuf+strlen(dp)+1) {
tohard:
				fclose(it);
				nomore = 1;
				strcpy(work, abuf+4);
				goto hardway;
			}
			for (cp = abuf; *cp == *dp && *cp; cp++, dp++)
				;
			if (*dp)
				goto tohard;
			strcpy(work, cp-3);
		}
		fclose(it);
	}
catit:
	strcpy(work2, "cat");
	work2[3] = work[3];
	work2[4] = 0;
	sprintf(realname, "%s/%s", path, work2);
	if (stat(realname, &stbuf2) < 0)
		goto hardway;
	strcpy(work2+4, work+4);
	sprintf(realname, "%s/%s", path, work2);
	if (stat(realname, &stbuf2) < 0 || stbuf2.st_mtime < stbuf.st_mtime) {
		if (nomore)
			goto hardway;
		printf("Reformatting page.  Wait...");
		fflush(stdout);
		unlink(work2);
		if (signal(SIGINT, SIG_IGN) == SIG_DFL) {
			(void) signal(SIGINT, nuke);
			(void) signal(SIGQUIT, nuke);
			(void) signal(SIGTERM, nuke);
		}
		sprintf(cmdbuf, "%s %s/%s > /tmp/man%d; trap '' 1 15",
			NROFFCAT, path, work, mypid);
		if (system(cmdbuf)) {
			printf(" aborted (sorry)\n");
			nuke(0);
			/*NOTREACHED*/
		}
		sprintf(cmdbuf, "/bin/mv -f /tmp/man%d %s/%s 2>/dev/null",
			mypid, path, work2);
		if (system(cmdbuf)) {
			sprintf(path,  "/");
			sprintf(work2, "tmp/man%d", mypid);
		}
		printf(" done\n");
	}
	strcpy(work, work2);
hardway:
	nroff(path, work);
	if (work2[0] == 't')
		nuke(0);
}

/*
 * Use the manpath to look for
 * the file name.  The result of
 * stat is returned in stbuf, the
 * successful path in path.
 */
pathstat(name, path, stbuf)
	char *name, path[];
	struct stat *stbuf;
{
	char *cp, *tp, *ep;
	char **cpp;
	static char *manpaths[] = {"man", "cat", 0};
	static char *nopaths[]  = {"", 0};

	if (strncmp(name, "man", 3) == 0)
		cpp = manpaths;
	else
		cpp = nopaths;
	for ( ; *cpp ; cpp++) {
		for (cp = manpath; cp && *cp; cp = tp) {
			tp = index(cp, ':');
			if (tp) {
				if (tp == cp) {
					sprintf(path, "%s%s", *cpp,
						name+strlen(*cpp));
				}
				else {
					sprintf(path, "%.*s/%s%s", tp-cp, cp, 
						*cpp, name+strlen(*cpp));
				}
				ep = path + (tp-cp);
				tp++;
			} else {
				sprintf(path, "%s/%s%s", cp, *cpp,
					name+strlen(*cpp));
				ep = path + strlen(cp);
			}
			if (stat(path, stbuf) >= 0) {
				*ep = '\0';
				return (1);
			}
		}
	}
	return (0);
}

nroff(pp, wp)
	char *pp, *wp;
{
	char cmd[BUFSIZ];

	chdir(pp);
	if (wp[0] == 'c' || wp[0] == 't')
		sprintf(cmd, "%s %s", nomore? CAT : MORE, wp);
	else
		sprintf(cmd, nomore? "%s %s" : "%s %s|%s", NROFF, wp, MORE);
	(void) system(cmd);
}

troff(pp, wp)
	char *pp, *wp;
{
	char cmdbuf[BUFSIZ];

	chdir(pp);
	sprintf(cmdbuf, TROFFCMD, wp);
	(void) system(cmdbuf);
}

any(c, sp)
	register int c;
	register char *sp;
{
	register int d;

	while (d = *sp++)
		if (c == d)
			return (1);
	return (0);
}

void
nuke(sig)
int sig;
{
	char name[15];

	sprintf(name, "/tmp/man%d", mypid);
	unlink(name);
	exit(1);
}

unsigned int
blklen(ip)
	register char **ip;
{
	register unsigned int i = 0;

	while (*ip++)
		i++;
	return (i);
}

apropos(argc, argv)
	int argc;
	char **argv;
{
	char buf[BUFSIZ], file[MAXPATHLEN+1];
	char *gotit, *cp, *tp;
	register char **vp;

	if (argc == 0) {
		fprintf(stderr, "apropos what?\n");
		exit(1);
	}
	gotit = calloc(1, blklen(argv));
	for (cp = manpath; cp; cp = tp) {
		tp = index(cp, ':');
		if (tp) {
			if (tp == cp)
				strcpy(file, WHATIS);
			else
				sprintf(file, "%.*s/%s", tp-cp, cp, WHATIS);
			tp++;
		} else
			sprintf(file, "%s/%s", cp, WHATIS);
		if (freopen(file, "r", stdin) == NULL)
			continue;
		while (fgets(buf, sizeof buf, stdin) != NULL)
			for (vp = argv; *vp; vp++)
				if (match(buf, *vp)) {
					printf("%s", buf);
					gotit[vp - argv] = 1;
					for (vp++; *vp; vp++)
						if (match(buf, *vp))
							gotit[vp - argv] = 1;
					break;
				}
	}
	for (vp = argv; *vp; vp++)
		if (gotit[vp - argv] == 0)
			printf("%s: nothing appropriate\n", *vp);
}

match(bp, str)
	register char *bp;
	char *str;
{

	for (;;) {
		if (*bp == 0)
			return (0);
		if (amatch(bp, str))
			return (1);
		bp++;
	}
}

amatch(cp, dp)
	register char *cp, *dp;
{

	while (*cp && *dp && lmatch(*cp, *dp))
		cp++, dp++;
	if (*dp == 0)
		return (1);
	return (0);
}

lmatch(c, d)
	register int c, d;
{

	if (c == d)
		return (1);
	if (!isalpha(c) || !isalpha(d))
		return (0);
	if (islower(c))
		c = toupper(c);
	if (islower(d))
		d = toupper(d);
	return (c == d);
}

whatis(argc, argv)
	int argc;
	char **argv;
{
	register char *gotit, **vp;
	char buf[BUFSIZ], file[MAXPATHLEN+1], *cp, *tp;

	if (argc == 0) {
		fprintf(stderr, "whatis what?\n");
		exit(1);
	}
	for (vp = argv; *vp; vp++)
		*vp = trim(*vp);
	gotit = calloc(1, blklen(argv));
	for (cp = manpath; cp; cp = tp) {
		tp = index(cp, ':');
		if (tp) {
			if (tp == cp)
				strcpy(file, WHATIS);
			else
				sprintf(file, "%.*s/%s", tp-cp, cp, WHATIS);
			tp++;
		} else
			sprintf(file, "%s/%s", cp, WHATIS);
		if (freopen(file, "r", stdin) == NULL)
			continue;
		while (fgets(buf, sizeof buf, stdin) != NULL)
			for (vp = argv; *vp; vp++)
				if (wmatch(buf, *vp)) {
					printf("%s", buf);
					gotit[vp - argv] = 1;
					for (vp++; *vp; vp++)
						if (wmatch(buf, *vp))
							gotit[vp - argv] = 1;
					break;
				}
	}
	for (vp = argv; *vp; vp++)
		if (gotit[vp - argv] == 0)
			printf("%s: not found\n", *vp);
}

wmatch(buf, str)
	char *buf, *str;
{
	register char *bp, *cp;

	bp = buf;
again:
	cp = str;
	while (*bp && *cp && lmatch(*bp, *cp))
		bp++, cp++;
	if (*cp == 0 && (*bp == '(' || *bp == ',' || *bp == '\t' || *bp == ' '))
		return (1);
	while (isalpha(*bp) || isdigit(*bp))
		bp++;
	if (*bp != ',')
		return (0);
	bp++;
	while (isspace(*bp))
		bp++;
	goto again;
}

char *
trim(cp)
	register char *cp;
{
	register char *dp;

	for (dp = cp; *dp; dp++)
		if (*dp == '/')
			cp = dp + 1;
	if (cp[0] != '.') {
		if (cp + 3 <= dp && dp[-2] == '.' &&
		    any(dp[-1], "cosa12345678npP"))
			dp[-2] = 0;
		if (cp + 4 <= dp && dp[-3] == '.' &&
		    any(dp[-2], "13") && isalpha(dp[-1]))
			dp[-3] = 0;
	}
	return (cp);
}
