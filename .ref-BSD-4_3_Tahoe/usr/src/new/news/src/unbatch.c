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
static char	*SccsId = "@(#)unbatch.c	1.26	3/21/87";
#endif /* SCCSID */

#define	MAXARGS		32

#include "defs.h"
#include <stdio.h>
#include <ctype.h>
#if defined(USG) || defined(BSD4_2) || defined(BSD4_1C)
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
/* The loop is to delete all versions. */
		while (unlink(filename) == 0)
			;
#endif /* VMS */
		pfn = fopen(filename, "w");
		while(--size >= 0 && (c = getc(stdin)) != EOF)
			putc(c, pfn);
		if (ferror(pfn) || fclose(pfn)) {	/* disk full? */
			logerr("error writing temporary file");
			break;
		}

		/*
		 * If we got a truncated batch, don't process the
		 * last article; it will probably be received again.
		 */
		if (size > 0) {
			logerr("truncated batch");
			break;
		}

		/*
		 * rnews < filename
		 */
		while ((pid = vfork()) == -1) {
			logerr("fork failed, waiting...\n");
			sleep(60);
		}
		if (pid == 0) {
			(void) close(0);
			(void) open(filename, 0);
#ifdef IHCC
			(void) sprintf(buf, "%s/%s", logdir(HOME), RNEWS);
#else
			strcpy(buf, RNEWS);
#endif
#ifdef SPOOLNEWS
			execlp(buf, "rnews", "-S", (char *)0);
#else /* !SPOOLNEWS */
			execlp(buf, "rnews", (char *)0);
#endif /* !SPOOLNEWS */
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

# ifdef HOME
	sprintf(path, "%s/%s/%s", logdir(HOME), LIBDIR, cp);
# else /* !HOME */
	sprintf(path, "%s/%s", LIBDIR, cp);
# endif /* HOME */

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
long a1, a2, a3, a4, a5, a6, a7, a8, a9;
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
	(void) fprintf(stderr, "%s\n", bfr);
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
