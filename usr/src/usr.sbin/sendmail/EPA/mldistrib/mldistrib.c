#include <stdio.h>
#include <ctype.h>
#include <sysexits.h>
#include <paths.h>

#define TRUE		1
#define FALSE		0
typedef char		BOOL;

#define CHARNULL	((char *) NULL)

enum copymode {RETAIN, DISCARD};

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
	char c;
	extern FILE *openmailer();
	extern char *readheadertag();
	extern void copyheader();
	extern void dropheader();
	extern void copybody();

	/* parse arguments */
	if (argc < 4)
	{
		fprintf(stderr, "Usage: mldistrib listname ownername member...\n");
		exit(EX_USAGE);
	}

	argv++;
	ml_name = *argv++;
	ml_owner = *argv++;

	/* consume and discard leading "From_" line */
	while ((c = fgetc(stdin)) != EOF && c != '\n')
		continue;

	/* open the connection to the mailer */
	mailfp = openmailer(argv);

	/* output the Resent-xxx: fields */
	fprintf(mailfp, "Resent-To:	%s\n", ml_name);
	fprintf(mailfp, "Resent-From:	%s\n", ml_owner);

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
			    sameword(p, "received", 9) ||
			    sameword(p, "resent-", 7))
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
		fprintf(mailfp, "Precedence:	bulk\n");

	/* copy the body of the message */
	copybody(stdin, mailfp);

	/* clean up the connection */
	fclose(mailfp);
	exit(0);
}



/*
**  OPENMAILER -- open a connection to the mailer
*/

FILE *
openmailer(argv)
	char **argv;
{
	register char *bp;
	register FILE *mailfp;
	char buf[10000];
	extern int strlen();

	bp = buf;
	(void) sprintf(bp, "%s", _PATH_SENDMAIL);
	bp += strlen(bp);

	while (*argv != CHARNULL)
	{
		(void) sprintf(bp, " %s", *argv++);
		bp += strlen(bp);
	}

	mailfp = popen(buf, "w");
	if (mailfp == NULL)
	{
		fprintf(stderr, "mldistrib: Unable to popen %s\n", _PATH_SENDMAIL);
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
