# include <stdio.h>
# include <ctype.h>
# include "dlvrmail.h"

static	char SccsId[] = "@(#)newaliases.c	2.1	11/5/80";

typedef struct { char *dptr; int dsize; } datum;
char *aliases = ALIASFILE;
char dirbuf[100];
char pagbuf[100];
int LineNo;
char *To;
int ExitStat;
int Errors;
# ifdef DEBUG
bool Debug;
# endif DEBUG

main(argc, argv)
	int argc;
	char *argv[];
{
	int f;
	char line[BUFSIZ];
	char line2[MAXLINE];
	register char *p;
	char *cp, *p2;
	char *rhs;
	int naliases, bytes, longest;
	datum key, content;
	bool skipping;
	addrq al, bl;
	extern char *prescan();
	extern addrq *parse();
	bool contin;

# ifdef DEBUG
	if (argc > 1 && strcmp(argv[1], "-T") == 0)
	{
		Debug++;
		argc--;
		argv++;
	}
# endif DEBUG
	if (argc > 1)
		aliases = argv[1];

	strcpy(dirbuf, aliases);
	strcat(dirbuf, ".dir");
	strcpy(pagbuf, aliases);
	strcat(pagbuf, ".pag");
	f = creat(dirbuf, 0666);
	if (f < 0) {
		perror(dirbuf);
		exit(1);
	}
	close(f);
	f = creat(pagbuf, 0666);
	if (f < 0) {
		perror(pagbuf);
		exit(1);
	}
	close(f);
	if (dbminit(aliases) < 0)
		exit(1);
	if (freopen(aliases, "r", stdin) == 0) {
		perror(aliases);
		exit(1);
	}

	/* read and interpret lines */
	LineNo = 0;
	naliases = 0;
	bytes = 0;
	longest = 0;
	skipping = FALSE;
	while (fgets(line, sizeof (line), stdin) != NULL)
	{
		LineNo++;
		switch (line[0])
		{
		  case '#':
		  case '\n':
		  case '\0':
			skipping = FALSE;
			continue;

		  case ' ':
		  case '\t':
			if (!skipping)
				usrerr("Non-continuation line starts with space");
			skipping = TRUE;
			continue;
		}
		skipping = FALSE;

		/* process the LHS */
		for (p = line; *p != '\0' && *p != ':' && *p != '\n'; p++)
			continue;
		if (*p == '\0' || *p == '\n')
		{
		 syntaxerr:
			usrerr("missing colon");
			continue;
		}
		*p++ = '\0';
		if (parse(line, &al, 1) == NULL)
		{
			*--p = ':';
			goto syntaxerr;
		}
		rhs = cp = p;
		contin = FALSE;
		for (;;)
		{
			register char c;

			/* do parsing & compression of addresses */
			c = *p;
			while (c != '\0')
			{
				p2 = p;
				while (*p != '\n' && *p != ',' && *p != '\0')
					p++;
				c = *p;
				*p++ = '\0';
				if (prescan(p2, cp, &line[sizeof line - 1], ',') == NULL)
					continue;
				contin = FALSE;
				if (parse(cp, &bl, -1) != NULL)
					cp += strlen(cp);
				if (c == ',')
				{
					*cp++ = ',';
					contin = TRUE;
				}
			}

			/* see if there should be a continuation line */
			if (!contin)
				break;

			/* read continuation line */
			if (fgets(line2, sizeof (line2), stdin) == NULL)
				break;
			LineNo++;

			if (!isspace(line2[0]))
				usrerr("continuation line missing");

			p = line2;
		}
		if (al.q_mailer != &Mailer[0])
		{
			usrerr("cannot alias non-local names");
			continue;
		}
		naliases++;
		key.dsize = strlen(al.q_user) + 1;
		key.dptr = al.q_user;
		content.dsize = strlen(rhs) + 1;
		if (content.dsize > longest)
			longest = content.dsize;
		content.dptr = rhs;
		bytes += key.dsize + content.dsize;
		if (store(key, content), 0)
		/* if (f = store(key, content)) */
			usrerr("Dbm internal error return %d from store\n", f);
	}
	fprintf(stderr, "%d aliases, %d bytes, longest %d bytes, %d errors\n",
	    naliases, bytes, longest, Errors);

	exit(ExitStat);
}

usrerr(fmt, a, b, c, d, e)
	char *fmt;
{
	Errors++;
	fprintf(stderr, "line %d: ", LineNo);
	fprintf(stderr, fmt, a, b, c, d, e);
	fprintf(stderr, "\n");
	return (-1);
}

syserr(fmt, a, b, c, d, e)
	char *fmt;
{
	return (usrerr(fmt, a, b, c, d, e));
}
