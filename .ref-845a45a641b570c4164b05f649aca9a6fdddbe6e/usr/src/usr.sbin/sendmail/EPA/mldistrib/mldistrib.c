/*
 * Copyright (c) 1992 Eric P. Allman
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
static char sccsid[] = "@(#)mldistrib.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include <sysexits.h>
#include <paths.h>

#define TRUE		1
#define FALSE		0
typedef char		BOOL;

#define CHARNULL	((char *) NULL)
#define MAXMAILOPTS	20

enum copymode {RETAIN, DISCARD};
char *myname;
int  debug;

main(argc, argv)
	int argc;
	char **argv;
{
	BOOL seen_precedence;
	register FILE *mailfp;
	enum copymode mode;
	register char *p;
	char *ml_name;
	char *ml_owner;
	char *mailer_opts[MAXMAILOPTS+1];
	char **next_opt = mailer_opts;
	char c;
	extern FILE *openmailer();
	extern char *readheadertag();
	extern void copyheader();
	extern void dropheader();
	extern void copybody();

	myname = argv[0];
	argc--, argv++;
	while (*argv[0] == '-')
	{
		if (strcmp(argv[0], "-d") == 0)
		{
			debug++;
			argv++;
			argc--;
			continue;
		}
		if (next_opt >= &mailer_opts[MAXMAILOPTS])
		{
			fprintf(stderr,
			    "%s: too many mailer options\n", myname);
			exit(EX_USAGE);
		}
		*next_opt++ = *argv++;
		argc--;
	}
	*next_opt = NULL;

	/* parse arguments */
	if (argc < 3)
	{
		fprintf(stderr,
		    "Usage: %s [-mailopts ...] listname ownername member...\n",
		    myname);
		exit(EX_USAGE);
	}

	ml_name = *argv++;
	ml_owner = *argv++;

	/* consume and discard leading "From_" line */
	while ((c = fgetc(stdin)) != EOF && c != '\n')
		continue;

	/* open the connection to the mailer */
	mailfp = openmailer(ml_owner, next_opt - mailer_opts, mailer_opts,
	    argc, argv);

	/* output the Resent-xxx: fields */
	fprintf(mailfp, "Resent-To:	%s\n", ml_name);
	fprintf(mailfp, "Resent-From:	%s\n", ml_owner);
	fprintf(mailfp, "Sender:	%s\n", ml_owner);

	/*
	**  Consume header
	**
	**	Errors-To:	discard
	**	Precedence:	retain; mark that it has been seen
	**	Received:	discard
	**	Resent-*:	discard
	**	Return-Path:	discard
	**	Via:		discard
	**	X-Mailer:	discard
	**	others		retain
	*/

	seen_precedence = FALSE;

	while ((p = readheadertag(stdin)) != CHARNULL)
	{
		extern BOOL sameword();

		mode = RETAIN;
		switch (p[0])
		{
		  case 'e':
		  case 'E':
			if (sameword(p, "errors-to", 10))
				mode = DISCARD;
			break;

		  case 'p':
		  case 'P':
			if (sameword(p, "precedence", 11))
				seen_precedence = TRUE;
			break;

		  case 'r':
		  case 'R':
			if (sameword(p, "return-path", 12) ||
#ifdef notyet
			    sameword(p, "received", 9) ||
#endif
			    sameword(p, "resent-", 7))
				mode = DISCARD;
			break;

		  case 's':
		  case 'S':
			if (sameword(p, "sender", 7))
				mode = DISCARD;
			break;

		  case 'v':
		  case 'V':
			if (sameword(p, "via", 4))
				mode = DISCARD;
			break;

		  case 'x':
		  case 'X':
			if (sameword(p, "x-mailer", 9))
				mode = DISCARD;
			break;
		}

		switch (mode)
		{
		  case RETAIN:
			fprintf(mailfp, "%s", p);
			copyheader(stdin, mailfp);
			break;

		  case DISCARD:
			dropheader(stdin);
			break;
		}
	}

	/* if no precedence was given, make it bulk mail */
	if (!seen_precedence)
		fprintf(mailfp, "Precedence: bulk\n");

	/* copy the body of the message */
	copybody(stdin, mailfp);

	/* clean up the connection */
	exit (my_pclose(mailfp));
}



/*
**  OPENMAILER -- open a connection to the mailer
*/

FILE *
openmailer(from, nopts, opts, argc, argv)
	char *from;
	int nopts, argc;
	char **opts, **argv;
{
	register char **argp;
	register FILE *mailfp;
	char **args;
	char *name;
	static char mailer[] = _PATH_SENDMAIL;
	extern int strlen();
	extern FILE *my_popen();
	extern char *malloc(), *rindex();

	/*
	 * allocate space for argv; 4 args below, a null,
	 * and options and arguments from caller.
	 */
	args = (char **) malloc((nopts + argc + 5) * sizeof(char *));
	if (args == (char **) NULL)
	{
		fprintf(stderr,
		    "%s: arg list too long; can't allocate memory!?\n", myname);
		exit(EX_SOFTWARE);
	}
	argp = args;
	if ((name = rindex(mailer, '/')) != CHARNULL)
		name++;
	else
		name = mailer;
	*argp++ = name;
	*argp++ = "-f";
	*argp++ = from;
	*argp++ = "-oi";
	bcopy((char *) opts, (char *) argp, nopts * sizeof(*opts));
	argp += nopts;
	bcopy((char *) argv, (char *) argp, argc * sizeof(*argv));
	argp += argc;
	*argp = CHARNULL;

	if (debug)
	{
		printf("| %s, args:\n", _PATH_SENDMAIL);
		for (argp = args; *argp; argp++)
			printf("  %s\n", *argp);
		printf("--------\n");
		return (stdout);
	}
	mailfp = my_popen(mailer, args, "w");
	if (mailfp == NULL)
	{
		fprintf(stderr, "%s: Unable to popen %s\n", myname,
		   _PATH_SENDMAIL);
		exit(EX_OSFILE);
	}

	return (mailfp);
}



/*
**  DROPHEADER -- drop a single header field
*/

void
dropheader(infp)
	register FILE *infp;
{
	register int c;

	while ((c = fgetc(infp)) != EOF)
	{
		if (c == '\n')
		{
			/* look at next character to check for continuation */
			c = fgetc(infp);
			if (c == ' ' || c == '\t')
				continue;
			if (c != EOF)
				ungetc(c, infp);
			break;
		}
	}
}



/*
**  COPYHEADER -- copy a single header field
*/

void
copyheader(infp, outfp)
	register FILE *infp;
	register FILE *outfp;
{
	register int c;

	while ((c = fgetc(infp)) != EOF)
	{
		(void) fputc(c, outfp);
		if (c == '\n')
		{
			/* look at next character to check for continuation */
			c = fgetc(infp);
			if (c == ' ' || c == '\t')
			{
				(void) fputc(c, outfp);
				continue;
			}
			if (c != EOF)
				ungetc(c, infp);
			break;
		}
	}
}



/*
**  READHEADERTAG -- read and return the name of a header field
*/

#define MAXHDRTAG	60

char *
readheadertag(infp)
	register FILE *infp;
{
	register int c;
	register char *bp;
	int i;
	static char buf[MAXHDRTAG + 1];
	extern char *strchr();

	c = fgetc(infp);
	if (c == EOF)
		return (CHARNULL);
	if (c == '\n')
	{
		ungetc(c, infp);
		return (CHARNULL);
	}

	bp = buf;
	i = sizeof buf;
	do
	{
		*bp++ = c;
		c = fgetc(infp);
	} while (--i > 0 && c != EOF && c != '\0' &&
		 strchr(" \t\n:", c) == CHARNULL);
	if (c != EOF)
		ungetc(c, infp);
	*bp++ = '\0';
	return (buf);
}



/*
**  COPYBODY -- copy the body of a message
*/

void
copybody(infp, outfp)
	register FILE *infp;
	register FILE *outfp;
{
	register int c;

	while ((c = fgetc(infp)) != EOF)
		fputc(c, outfp);
}



/*
**  SAMEWORD -- return true if two words are identical.  The first
**		word is case insignificant; the second must be lower case.
*/

BOOL
sameword(test, pat, len)
	register char *test;
	register char *pat;
	int len;
{
	for (; --len >= 0; test++, pat++)
	{
		if (*test == *pat)
			continue;
		if (isupper(*test) && tolower(*test) == *pat)
			continue;
		return (FALSE);
	}
	return (TRUE);
}



/*
 * from libc popen:
static char sccsid[] = "@(#)popen.c	5.12 (Berkeley) 4/6/90";
 *
 * This code is derived from software written by Ken Arnold and
 * published in UNIX Review, Vol. 6, No. 8.
 *
 * modified to avoid sh, be safe for setuid/setgid programs,
 * and simplified to support only one popen'ed stream.
 */


#include <sys/signal.h>
#include <sys/wait.h>
#include <errno.h>
#include <unistd.h>
/*
#include <stdio.h>
#include <paths.h>
*/

static pid_t pid;

FILE *
my_popen(program, args, type)
	char *program, *type;
	char **args;
{
	FILE *iop;
	int pdes[2];
	char *malloc();

	if (*type != 'r' && *type != 'w' || type[1])
		return(NULL);

	if (pipe(pdes) < 0)
		return(NULL);
	switch (pid = vfork()) {
	case -1:			/* error */
		(void)close(pdes[0]);
		(void)close(pdes[1]);
		return(NULL);
		/* NOTREACHED */
	case 0:				/* child */
		if (*type == 'r') {
			if (pdes[1] != STDOUT_FILENO) {
				(void)dup2(pdes[1], STDOUT_FILENO);
				(void)close(pdes[1]);
			}
			(void)close(pdes[0]);
		} else {
			if (pdes[0] != STDIN_FILENO) {
				(void)dup2(pdes[0], STDIN_FILENO);
				(void)close(pdes[0]);
			}
			(void)close(pdes[1]);
		}
		execv(program, args);
		_exit(127);
		/* NOTREACHED */
	}
	/* parent; assume fdopen can't fail...  */
	if (*type == 'r') {
		iop = fdopen(pdes[0], type);
		(void)close(pdes[1]);
	} else {
		iop = fdopen(pdes[1], type);
		(void)close(pdes[0]);
	}
	return (iop);
}

my_pclose(iop)
	FILE *iop;
{
	extern int errno;
	register int fdes;
	int omask;
	int pstat;
	pid_t wpid;

	(void)fclose(iop);
	omask = sigblock(sigmask(SIGINT)|sigmask(SIGQUIT)|sigmask(SIGHUP));
	do {
		wpid = waitpid(pid, &pstat, 0);
	} while (wpid == -1 && errno == EINTR);
	(void)sigsetmask(omask);
	pid = 0;
	return (pid == -1 ? -1 : pstat);
}
