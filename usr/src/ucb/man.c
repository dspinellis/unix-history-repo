#ifndef lint
static char sccsid[] = " @(#)man.c	1.5 (Berkeley) 3/7/83";
#endif

#include <stdio.h>
#include <ctype.h>
#include <sgtty.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
/*
 * man
 */
int	nomore;
int	cflag;
char	*manpath = "/usr/man";
char	*strcpy();
char	*strcat();
char	*trim();
int	remove();
int	section;
int	subsec;
int	troffit;
int	killtmp;

#define	eq(a,b)	(strcmp(a,b) == 0)

main(argc, argv)
	int argc;
	char *argv[];
{

	if (signal(SIGINT, SIG_IGN) == SIG_DFL) {
		signal(SIGINT, remove);
		signal(SIGQUIT, remove);
		signal(SIGTERM, remove);
	}
	umask(0);
	if (argc <= 1) {
		fprintf(stderr, "Usage: man [ section ] name ...\n");
		fprintf(stderr, "or: man -k keyword ...\n");
		fprintf(stderr, "or: man -f file ...\n");
		exit(1);
	}
	argc--, argv++;
	while (argc > 0 && argv[0][0] == '-') {
		switch(argv[0][1]) {

		case 0:
			nomore++;
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

		case 'P':
			argc--, argv++;
			manpath = *argv;
			break;
		}
		argc--, argv++;
	}
	if (chdir(manpath) < 0) {
		fprintf(stderr, "Can't chdir to %s.\n", manpath);
		exit(1);
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
		} else if (eq(argv[0], "public")) {
			section = 'p';
			goto sectin;
		} else if (argv[0][0] >= '0' && argv[0][0] <= '9' && (argv[0][1] == 0 || argv[0][2] == 0)) {
			section = argv[0][0];
			subsec = argv[0][1];
sectin:
			argc--, argv++;
			if (argc == 0) {
				fprintf(stderr, "But what do you want from section %s?\n", argv[-1]);
				exit(1);
			}
			continue;
		}
		manual(section, argv[0]);
		argc--, argv++;
	} while (argc > 0);
	exit(0);
}

manual(sec, name)
	char sec;
	char *name;
{
	char section = sec;
	char work[100], work2[100], cmdbuf[100];
	int ss;
	struct stat stbuf, stbuf2;
	int last;
	char *sp = "1nl6823457p";

	strcpy(work, "manx/");
	strcat(work, name);
	strcat(work, ".x");
	last = strlen(work) - 1;
	if (section == '1') {
		sp = "1nlo";
		section = 0;
	}
	if (section == 0) {
		ss = 0;
		for (section = *sp++; section; section = *sp++) {
			work[3] = section;
			work[last] = section;
			work[last+1] = 0;
			if (stat(work, &stbuf) >= 0)
				break;
			if (work[last] >= '1' && work[last] <= '4') {
				char *cp;
search:
				switch (work[last]) {
				case '1': cp = "mcgv"; break;
				case '2': cp = "v"; break;
				case '3': cp = "jxmsfnv"; break;
				case '4': cp = "pfvsn"; break;
				case '8': cp = "vc"; break;
				}
				while (*cp) {
					work[last+1] = *cp++;
					if (stat(work, &stbuf) >= 0) {
						ss = work[last+1];
						goto found;
					}
				}
				if (ss = 0)
					work[last+1] = 0;
			}
		}
		if (section == 0) {
			if (sec == 0)
				printf("No manual entry for %s.\n", name);
			else
				printf("No entry for %s in section %c of the manual.\n", name, sec);
			return;
		}
	} else {
		work[3] = section;
		work[last] = section;
		work[last+1] = subsec;
		if (stat(work, &stbuf) < 0) {
			if ((section >= '1' && section <= '4') && subsec == 0) {
				sp = "\0";
				goto search;
			}
			printf("No entry for %s in section %c", name, section);
			if (subsec)
				putchar(subsec);
			printf(" of the manual.\n");
			return;
		}
	}
found:
	if (troffit)
		troff(work);
	else {
		FILE *it;
		char abuf[BUFSIZ];

		if (!nomore) {
			if ((it = fopen(work, "r")) == NULL) {
				perror(work);
				exit(1);
			}
			if (fgets(abuf, BUFSIZ-1, it) &&
			   abuf[0] == '.' && abuf[1] == 's' &&
			   abuf[2] == 'o' && abuf[3] == ' ') {
				register char *cp = abuf+strlen(".so ");
				char *dp;

				while (*cp && *cp != '\n')
					cp++;
				*cp = 0;
				while (cp > abuf && *--cp != '/')
					;
				dp = ".so man";
				if (cp != abuf+strlen(dp)+1) {
tohard:
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
			strcpy(work2, "cat");
			strcpy(work2+3, work+3);
			work2[4] = 0;
			if (stat(work2, &stbuf2) < 0)
				goto hardway;
			strcpy(work2+3, work+3);
			if (stat(work2, &stbuf2) < 0 || stbuf2.st_mtime < stbuf.st_mtime) {
				printf("Reformatting page.  Wait...");
				fflush(stdout);
				unlink(work2);
				sprintf(cmdbuf,
"nroff -h -man %s > /tmp/man%d; trap '' 1 15; mv /tmp/man%d %s", work, getpid(), getpid(), work2);
				if (system(cmdbuf)) {
					printf(" aborted (sorry)\n");
					remove();
					/*NOTREACHED*/
				}
				printf(" done\n");
			}
			strcpy(work, work2);
		}
hardway:
		nroff(work);
	}
}

nroff(cp)
	char *cp;
{
	char cmd[BUFSIZ];

	sprintf(cmd, nomore ?
	    "%s %s" : "%s %s|/usr/ucb/ul|/usr/ucb/more -f",
	    cp[0] == 'c' ? "cat -s" : "nroff -man", cp);
	system(cmd);
}

troff(cp)
	char *cp;
{
	char cmdbuf[BUFSIZ];

	sprintf(cmdbuf,
"troff -t -man /usr/lib/tmac/tmac.vcat %s|/usr/lib/rvsort|/usr/ucb/vpr -t",
		cp);
	system(cmdbuf);
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

remove()
{
	char name[15];

	sprintf(name, "/tmp/man%d", getpid());
	unlink(name);
	exit(1);
}

apropos(argc, argv)
	int argc;
	char **argv;
{
	char buf[BUFSIZ];
	char *gotit;
	register char **vp;

	if (argc == 0) {
		fprintf(stderr, "man: -k what?\n");
		exit(1);
	}
	if (freopen("/usr/lib/whatis", "r", stdin) == NULL) {
		perror("/usr/lib/whatis");
		exit (1);
	}
	gotit = (char *) calloc(1, blklen(argv));
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
	for (vp = argv; *vp; vp++)
		if (gotit[vp - argv] == 0)
			printf("%s: nothing apropriate\n", *vp);
}

match(buf, str)
	char *buf, *str;
{
	register char *bp, *cp;

	bp = buf;
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
	char c, d;
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

blklen(ip)
	register int *ip;
{
	register int i = 0;

	while (*ip++)
		i++;
	return (i);
}

whatis(argc, argv)
	int argc;
	char **argv;
{
	register char **avp;

	if (argc == 0) {
		fprintf(stderr, "man: -f what?\n");
		exit(1);
	}
	if (freopen("/usr/lib/whatis", "r", stdin) == NULL) {
		perror("/usr/lib/whatis");
		exit (1);
	}
	for (avp = argv; *avp; avp++)
		*avp = trim(*avp);
	whatisit(argv);
	exit(0);
}

whatisit(argv)
	char **argv;
{
	char buf[BUFSIZ];
	register char *gotit;
	register char **vp;

	gotit = (char *)calloc(1, blklen(argv));
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
		if (cp + 3 <= dp && dp[-2] == '.' && any(dp[-1], "cosa12345678npP"))
			dp[-2] = 0;
		if (cp + 4 <= dp && dp[-3] == '.' && any(dp[-2], "13") && isalpha(dp[-1]))
			dp[-3] = 0;
	}
	return (cp);
}

system(s)
char *s;
{
	int status, pid, w;

	if ((pid = vfork()) == 0) {
		execl("/bin/sh", "sh", "-c", s, 0);
		_exit(127);
	}
	while ((w = wait(&status)) != pid && w != -1)
		;
	if (w == -1)
		status = -1;
	return (status);
}
