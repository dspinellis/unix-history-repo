/*
 * unbatchnews: extract news in batched format and process it one article
 * at a time.  The format looks like
 *	#! rnews 1234
 *	article containing 1234 characters
 *	#! rnews 4321
 *	article containing 4321 characters
 *
 *	or
 *
 *	#! command [args]
 *	calls LIBDIR/command [args] to process the news
 */

#ifdef SCCSID
static char	*SccsId = "@(#)unbatch.c	6.3	5/17/86";
#endif /* SCCSID */

#define	MAXARGS		20

#ifdef BSD4_2
#define fork vfork
#endif /* BSD4_2 */

#include <stdio.h>
#include <ctype.h>
#ifdef USG
#include <fcntl.h>
#endif /* USG */

char buf[BUFSIZ];
char sibuf[BUFSIZ];

main()
{
	register int c;
	register FILE *pfn;
	register long size;
	char *filename;
	int pid, wpid, exstat;
	char *mktemp(), *gets();
	long atol();

	filename = mktemp("/tmp/unbnewsXXXXXX");
	setbuf(stdin, (char *)NULL);	/* only for the first line */
	if (gets(buf) == NULL) {
		(void) unlink(filename);
		exit(0);
	}
	if (strncmp(buf, "#! rnews ", 9) != 0) {
		docmd(buf);
		/* should not return */
		logerr("unbatch: docmd returned!");
		exit(1);
	}

	setbuf(stdin, sibuf);	/* buffer the rest of the file */

	do {
		while (strncmp(buf, "#! rnews ", 9) 
		    && strncmp(buf, "! rnews ", 8)) { /* kludge for bug */
			register char *cp;
			for (cp = buf; *cp != '\0'; ++cp)
				if (!isascii(*cp) ||
					(!isprint(*cp) && !isspace(*cp)))
						*cp = '?';
			logerr("out of sync, skipping %s", buf);
			if (gets(buf) == NULL)
				exit(0);
		}
		size = atol(buf + (buf[0] == '#' ? 9 : 8));
		if(size <= 0) {
			logerr("nonsense size %ld", size);
			continue;
		}
#ifdef VMS
		vmsdelete(filename);
#endif /* VMS */
		pfn = fopen(filename, "w");
		while(--size >= 0 && (c = getc(stdin)) != EOF)
			putc(c, pfn);
		(void) fclose(pfn);

		/*
		 * If we got a truncated batch, don't process the
		 * last article; it will probably be received again.
		 */
		if (size > 0)
			break;

		/*
		 * rnews < filename
		 */
		while ((pid = fork()) == -1) {
			logerr("fork failed, waiting...\n");
			sleep(53);
		}
		if (pid == 0) {
			(void) close(0);
			(void) open(filename, 0);
#ifdef IHCC
			(void) sprintf(buf, "%s/%s/rnews", logdir(HOME), LIBDIR);
#else
			(void) sprintf(buf, "%s/rnews", BINDIR);
#endif
			execlp(buf, "rnews", (char *)0);
			perror("rnews");
			exit(1);
		}
		while ((wpid = wait(&exstat)) >= 0 && wpid != pid)
			;
	} while (gets(buf) != NULL);
	(void) unlink(filename);
	exit(0);
}

docmd(p)
register char *p;
{
	char *args[MAXARGS];
	register char **ap = args;
	char path[BUFSIZ];
	char *rindex(), *cp;

	while (*p && !isspace(*p))		/* skip leading #! crud */
		p++;

	while (isspace(*p))
		p++;

	while (*p != '\0') {
		*ap++ = p;
		if (ap >= &args[MAXARGS]) {
			logerr("unbatch: Too many args to %s", args[0]);
			exit(2);
		}
		while (*p && !isspace(*p))
			p++;
		if (*p)
			*p++ = '\0';
		while (isspace(*p))
			p++;
	}
	*ap = (char *)0;

	if (ap == args) {
		logerr("unbatch: no command to execute");
		exit(2);
	}

	/* strip off any leading pathname in case someone gets tricky */
	cp = rindex(args[0], '/');
	if (cp++ == NULL)
		cp = args[0];

	sprintf(path, "%s/%s", LIBDIR, cp);

	/*
	 * "path" is absolute, no searching is needed,  we use
	 * 'execvp' solely so that sh scripts will be handled
	 */
	(void) execvp(path, args);
	perror(path);
	exit(2);
}

/*
 * Log the given message, with printf strings and parameters allowed,
 * on the log file, if it can be written.
 */
/* VARARGS1 */
logerr(fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9)
char *fmt;
{
	FILE *logfile;
	char lfname[BUFSIZ];		/* the log file */
	char bfr[BUFSIZ];
	char *logtime, *ctime(); 
	long t;

	(void) time(&t);
	logtime = ctime(&t);
	logtime[16] = 0;
	logtime += 4;

#ifdef IHCC
	(void) sprintf(lfname, "%s/%s/errlog", logdir(HOME), LIBDIR);
#else
	(void) sprintf(lfname, "%s/errlog", LIBDIR);
#endif

	(void) sprintf(bfr, fmt, a1, a2, a3, a4, a5, a6, a7, a8, a9);
	fprintf(stderr, bfr);
	if (access(lfname, 0) == 0 && (logfile = fopen(lfname, "a")) != NULL) {
#if defined(USG) || defined(BSD4_2) || defined(BSD4_1C)
		int flags;
		flags = fcntl(fileno(logfile), F_GETFL, 0);
		(void) fcntl(fileno(logfile), F_SETFL, flags|O_APPEND);
#else /* v7 */
		(void) lseek(fileno(logfile), 0L, 2);
#endif /* v7 */
		fprintf(logfile, "%s\tbatch\t%s\n", logtime, bfr);
		(void) fclose(logfile);
	}
}
