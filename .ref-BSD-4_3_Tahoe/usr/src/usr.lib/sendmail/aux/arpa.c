/*
**  Sendmail
**  Copyright (c) 1983  Eric P. Allman
**  Berkeley, California
**
**  Copyright (c) 1983 Regents of the University of California.
**  All rights reserved.  The Berkeley software License Agreement
**  specifies the terms and conditions for redistribution.
*/

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char	SccsId[] = "@(#)arpa.c	5.1 (Berkeley) 6/7/85";
#endif not lint

# include	<stdio.h>
# include	<ctype.h>
# include	<signal.h>
# include	<sysexits.h>
# include	<whoami.h>
# include	"useful.h"

char Version[] = "@(#)Arpa-mailer version 5.1 of 6/7/85";

# define void	int

/*
**  ARPA MAILER -- Queue ARPANET mail for eventual delivery
**
**	The standard input is stuck away in the outgoing arpanet
**	mail queue for delivery by the true arpanet mailer.
**
**		CUSTOMIZED FOR THE C/70
**
**	Usage:
**		/usr/lib/mailers/arpa from host user
**
**	Positional Parameters:
**		from -- the person sending the mail.
**		host -- the host to send the mail to.
**		user -- the user to send the mail to.
**
**	Flags:
**		-T -- debug flag.
**
**	Files:
**		/usr/spool/netmail/* -- the queue file.
**
**	Return Codes:
**		0 -- all messages successfully mailed.
**		2 -- user or host unknown.
**		3 -- service unavailable, probably temporary
**			file system condition.
**		4 -- syntax error in address.
**
**	Compilation Flags:
**		SPOOLDIR -- the spool directory
**
**	Compilation Instructions:
**		cc -n -O -s arpa-mailer.c -o arpa-mailer -lX
**		chmod 755 arpa-mailer
**		mv arpa-mailer /usr/lib/mailers/arpa
**
**	Author:
**		Eric Allman, UCB/INGRES (eric@berkeley)
*/

# ifdef C70
# define SPOOLDIR	"/usr/netmail"
# else
# define SPOOLDIR	"/usr/spool/netmail"
# endif




char	*From;			/* person sending this mail */
char	*To;			/* current "To:" person */
int	State;			/* the current state (for exit codes) */
# ifdef DEBUG
bool	Tflag;			/* -T given */
# endif DEBUG
char	FromHost[200];		/* string to prepend to addresses */
/*
**  MAIN -- Main program for arpa mailer
**
**	Processes arguments, and calls sendmail successively on
**	the To: list.
**
**	Algorithm:
**		Scan for debug flag.
**		Catch interrupt signals.
**		Collect input file name and from person.
**		If more than one person in the to list, and
**			if the input file is not a real file,
**			collect input into a temp file.
**		For each person in the to list
**			Send to that person.
**
**	Parameters:
**		argc
**		argv -- as usual
**
**	Returns:
**		via exit
**
**	Side Effects:
**		Mail gets sent.
**
**	Author:
**		Eric Allman UCB/INGRES.
*/

main(argc, argv)
	int argc;
	char **argv;
{
	register int i;
	register char *p;
	register int ifd;
	char buf[512];
	extern int done();
	extern char *locv();
	register char *q;
	char *lastmark;

	State = 3;
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		(void) signal(SIGINT, done);
	
	/* process flags */
	argv[argc] = 0;
# ifdef DEBUG
	if (strcmp(argv[1], "-T") == 0)
	{
		Tflag++;
		argv++;
		argc--;
		printf("%s\n", Version);
	}
# endif DEBUG

	if (argc != 4)
	{
		rexit(EX_SOFTWARE);
	}
	
	/* decode parameters */
	From = argv[1];
	lastmark = &FromHost[-1];
	for (p = From, q = FromHost; (*q = *p) != '\0'; p++, q++)
	{
		if (*p == ':')
			*q = *p = '.';
		if (*q == '.' || *q == '!' || *q == '@')
			lastmark = q;
	}
	lastmark[1] = '\0';

	/* start sending mail */
	State = sendmail(argv[2], argv[3]);

	/* all done, clean up */
	done();
}
/*
**  DONE -- Finish up, remove temp files, etc.
**
**	This does basic cleanup on interrupt, error, or
**	normal termination.  It uses "State" to tell which
**	is happening.
**
**	Parameters:
**		none
**
**	Returns:
**		none
**
**	Side Effects:
**		Exit(State).
*/

done()
{
	rexit(State);
}

/*
** REXIT -- exit, reporting error code if -T given
**
**	Parameters:
**		e -- error code to exit with; see sysexits.h
**
**	Returns:
**		none
**
**	Side Effects:
**		Exit(e).
*/
rexit(e)
{

# ifdef DEBUG
	if (Tflag)
		fprintf(stderr, "arpa-mail: return code %d\n", e);
# endif
	exit(e);
}
/*
**  SENDMAIL -- Queue up mail for the arpanet mailer.
**
**	The mail is inserted with proper headers into the
**	arpanet queue directory.
**
**	Algorithm:
**		decode "to" address
**			if error, exit.
**		create a spool file name.
**		output the header information to spool file,
**		  separate names in To:, CC: fields with commas.
**		copy the mail to the spool file.
**
**	Parameters:
**		host -- the host to send to.
**		user -- the user to send to.
**
**	Returns:
**		none
**
**	Side Effects:
**		the mail is copied into a file in the network
**			queue directory (/usr/spool/netmail).
*/

sendmail(host, user)
	char *host;
	char *user;
{
	char spoolfile[50];	/* gets the spool file name */
	register int i;
	register char *p;
	static int callnum;	/* for the final letter on spoolfile */
	char buf[512];
	register FILE *sfp;	/* spool file */
	register int c;
	extern char *matchhdr();

	/* verify that the host exists */
	(void) strcpy(buf, "/dev/net/");
	(void) strcat(buf, host);
# ifndef C70
#ifdef DEBUG
	if (!Tflag)
#endif DEBUG
	if (host[0] == '\0' || access(buf, 0) < 0)
		return (EX_NOHOST);
# endif C70

	/*
	**  Create spool file name.
	**	Format is "username000nnX", where username is
	**	padded on the right with zeros and nn (the process
	**	id) is padded on the left with zeros; X is a unique
	**	sequence character.
	*/

# ifdef DEBUG
	if (Tflag)
		(void) strcpy(spoolfile, "arpa.out");
	else
# endif DEBUG
		(void) sprintf(spoolfile, "%s/arpamail%05d%c", SPOOLDIR, getpid(), 'a' + callnum++);

	/* create spool file */
	sfp = fopen(spoolfile, "w");
	if (sfp == NULL)
	{
	spoolerr:
		return (EX_OSERR);
	}
# ifdef DEBUG
	if (!Tflag)
# endif DEBUG
		(void) chmod(spoolfile, 0400);

	/*
	** Output mailer control lines.
	**	These lines are as follows:
	**		/dev/net/<hostname> {target host}
	**		user-name {at target host}
	**		/mnt/eric {pathname of sender; not used}
	**		eric {name of user who is sending}
	**	These are different (but close) on the C/70.
	*/

# ifdef C70
 	fputs(host, sfp);
 	fputs(":", sfp);
 	fputs(user, sfp);
 	fputs(":", sfp);
 	fputs(From, sfp);
 	fputs(":\n", sfp);
# else
	fputs(buf, sfp);
	fputs("\n", sfp);
	fputs(user, sfp);
	fputs("\n\n", sfp);
	fputs(From, sfp);
	fputs("\n", sfp);
# endif

	/*
	**  Output the mail
	**	Check the first line for the date.  If not found,
	**	assume the message is not in arpanet standard format
	**	and output a "Date:" and "From:" header.
	*/

	if (fgets(buf, sizeof buf, stdin) == NULL)
	{
		/* no message */
		(void) unlink(spoolfile);
		return (EX_OK);
	}
	if (strncmp("From ", buf, 5) == 0)
	{
		/* strip Unix "From" line */
		/* should save the date here */
		(void) fgets(buf, sizeof buf, stdin);
	}
	while (matchhdr(buf, "mail-from") != NULL ||
	       matchhdr(buf, "sender-path") != NULL ||
	       matchhdr(buf, "received") != NULL ||
	       matchhdr(buf, "via") != NULL)
	{
		fputs(buf, sfp);
		(void) fgets(buf, sizeof buf, stdin);
	}
	if (matchhdr(buf, "date") == NULL)
		putdate(sfp);
	else
	{
		fputs(buf, sfp);
		(void) fgets(buf, sizeof buf, stdin);
	}
	if (matchhdr(buf, "from") == NULL)
		putfrom(sfp);
	else
	{
		/* hack to support sendmail -- for a while */
		if (index(buf, '@') == NULL)
			putfrom(sfp);
		else
			fputs(buf, sfp);
		(void) fgets(buf, sizeof buf, stdin);
	}
	if (!ishdr(buf))
	{
		if (buf[0] != '\n')
			putc('\n', sfp);
		goto hdrdone;
	}

	/*
	** At this point, we have a message with REAL headers.
	** We look at each head line and insert commas if it
	** is a To: or Cc: field.
	*/

	do
	{
		if (!ishdr(buf))
			break;
		if (!matchhdr(buf, "to") && !matchhdr(buf, "cc"))
		{
			fputs(buf, sfp);
			continue;
		}
		/* gotcha! */
		fixaddr(buf, 1, sfp);
		while (isspace(c = peekc(stdin)) && c != '\n')
		{
			(void) fgets(buf, BUFSIZ, stdin);
			fixaddr(buf, 0, sfp);
		}
	} while (fgets(buf, BUFSIZ, stdin) != NULL);

hdrdone:
	/* output the rest of the header & the body of the letter */
	do
	{
		fputs(buf, sfp);
		if (ferror(sfp))
			goto spoolerr;
	} while (fgets(buf, sizeof buf, stdin) != NULL);

	/* all done! */
	(void) fclose(sfp);
	return (EX_OK);
}
/*
**  FIXADDR -- Output header line with needed commas.
**
**	Parameters:
**		buf -- header line
**		first -- true if this is not a continuation
**
**	Returns:
**		none
**
**	Side effects:
**		The contents of buf is copied onto the spool file with
**		with the right commas interlaced
**
**	Called by:
**		sendmail
*/

fixaddr(buf, first, spf)
	char buf[];
	register FILE *spf;
{
	register char *cp;
	register int c;
	char word[BUFSIZ], word2[BUFSIZ];
	char *gword();
	static char wsep[] = ", ";

	cp = buf;
	if (first)
	{
		while (*cp != ':' && *cp)
			putc(*cp++, spf);
		if (*cp == ':')
		{
			fputs(": ", spf);
			cp++;
		}
	}
	else
		while (*cp && isspace(*cp))
			putc(*cp++, spf);
	cp = gword(word, cp);
	if (strlen(word) == 0)
	{
		putc('\n', spf);
		goto test;
	}
	for (;;)
	{
		cp = gword(word2, cp);
		if (strlen(word2) == 0)
		{
			putaddr(word, spf);
			break;
		}
		if (strcmp(word2, "%") == 0)
			word2[0] = '@';
		if (strcmp(word2, "@") && strcmp(word2, "at"))
		{
			putaddr(word, spf);
			fputs(wsep, spf);
			(void) strcpy(word, word2);
			continue;
		}
		fputs(word, spf);
		if (word2[0] == '@')
			putc('@', spf);
		else
			fputs(" at ", spf);
		cp = gword(word, cp);
		fputs(word, spf);
		cp = gword(word, cp);
		if (strlen(word))
			fputs(wsep, spf);
	}

test:
	c = peekc(stdin);
	if (isspace(c) && c != '\n')
		fputs(",\n", spf);
	else
		putc('\n', spf);
}
/*
**  PUTADDR -- output address onto file
**
**	Putaddr prepends the network header onto the address
**	unless one already exists.
**
**	Parameters:
**		name -- the name to output.
**		fp -- the file to put it on.
**
**	Returns:
**		none.
**
**	Side Effects:
**		name is put onto fp.
*/

putaddr(name, fp)
	char *name;
	FILE *fp;
{
	register char *p;

	if (strlen(name) == 0)
		return;
	for (p = name; *p != '\0' && *p != ':' && *p != '.' && *p != '@' &&
	     *p != '!' && *p != '^'; p++)
		continue;
	if (*p == ':')
		*p = '.';
	else if (*p == '\0')
		fputs(FromHost, fp);
	fputs(name, fp);
	if (*p != '@')
		fputs("@Berkeley", fp);
}
/*
**  PEEKC -- peek at next character in input file
**
**	Parameters:
**		fp -- stdio file buffer
**
**	Returns:
**		the next character in the input or EOF
**
**	Side effects:
**		None.
*/
peekc(fp)
	register FILE *fp;
{
	register int c;

	c = getc(fp);
	(void) ungetc(c, fp);
	return(c);
}

/*
**  GWORD -- get the next liberal word from a string
**
**	Parameters:
**		buf -- place to put scanned word
**		p -- place to start looking for word
**
**	Returns:
**		updated value of p or 0 if no more left after this
**
**	Side effects:
**		buf gets the liberal word scanned.
**		buf will be length 0 if there is no more input,
**		or if p was passed as 0
*/
char *
gword(buf, p)
	char buf[];
	register char *p;
{
	register char *sp, *dp;
	int	atfound = 0;			/* weither or not a '@' found in the scan */

	(void) strcpy(buf, "");
	if (p == 0)
		return(0);
	sp = p;
	while (*sp && (isspace(*sp) || *sp == ','))
		sp++;
	dp = buf;
	if (*sp != '%' && *sp != '@')
	{
		while (*sp && !isspace(*sp) && *sp != ',' )
		{
			if ( *sp == '@' || *sp == '%' )
				atfound++;
			*dp++ = *sp++;
		}
		if ( atfound )
		{
			dp--;
			while ( *dp != '@' && *dp != '%' )
				dp--,sp--;
			sp--;
		}
			
	}
	else
		*dp++ = *sp++;
	*dp = 0;
	if (*sp == 0)
		return(0);
	return(sp);
}
/*
**  ISHDR -- see if the passed line is a ARPA style header line
**
**	Parameters:
**		buf -- header line
**
**	Returns:
**		non-zero if the line is a header line, else zero
**
**	Side effects:
**		none
**
**	Called by:
**		sendmail
*/
ishdr(buf)
	char buf[];
{
	register char *p;

	p = buf;

	/* check for continuation lines */
	if (isspace(*p))
		return (1);
	else
	{
		while (*p != ':' && !isspace(*p))
			p++;
		while (isspace(*p))
			p++;
		if (*p != ':')
			p = 0;
	}
	return(p != 0);
}
/*
**  PUTDATE -- Put the date field into the message.
**
**	Parameters:
**		fp -- file to put it onto.
**
**	Returns:
**		none
**
**	Side Effects:
**		output onto fp.
*/

putdate(fp)
	register FILE *fp;
{
	extern char *arpadate();

	fputs("Date: ", fp);
	fputs(arpadate(NULL), fp);
	fputs("\n", fp);
}
/*
**  PUTFROM -- Put the from field into the message.
**
**	Parameters:
**		fp -- file to put it onto.
**
**	Returns:
**		none
**
**	Side Effects:
**		output onto fp.
*/

putfrom(fp)
	register FILE *fp;
{

	fputs("From: ", fp);
	fputs(From, fp);
	fputs("@Berkeley\n", fp);
}
