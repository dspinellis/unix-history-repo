/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lookbib.c	4.7 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <ctype.h>
#include "pathnames.h"

main(argc, argv)	/* look in biblio for record matching keywords */
int argc;
char **argv;
{
	FILE *fp, *hfp, *fopen(), *popen();
	char s[BUFSIZ], hunt[64];
	int instructions = 1;

	if (strcmp(argv[1],"-n") == 0)
	{
		argv++;
		argc--;
		instructions = 0;
	}
	if (argc == 1 || argc > 2)
	{
		fputs("Usage:  lookbib database\n",
			stderr);
		fputs("\tfinds citations specified on standard input\n",
			stderr);
		exit(1);
	}
	if (!isatty(fileno(stdin)))
		fp = stdin;
	else if ((fp = fopen(_PATH_TTY, "r")) == NULL)
	{
		perror(_PATH_TTY);
		exit(1);
	}
	(void)sprintf(s, "%s.ia", argv[1]);
	if (access(s, 0) == -1) {
		(void)sprintf (s, "%s", argv[1]);
		if (access(s, 0) == -1) {
			perror(s);
			fprintf(stderr, "\tNeither index file %s.ia ", s);
			fprintf(stderr, "nor reference file %s found\n", s);
			exit(1);
		}
	}
	(void)sprintf(hunt, "%s %s", _PATH_HUNT, argv[1]);

	if (instructions && isatty(fileno(fp)))
	{
		fprintf(stderr, "Instructions? ");
		fgets(s, BUFSIZ, fp);
		if (*s == 'y')
			instruct();
	}
   again:
	fprintf(stderr, "> ");
	if (fgets(s, BUFSIZ, fp))
	{
		if (*s == '\n')
			goto again;
		if ((hfp = popen(hunt, "w")) == NULL)
		{
			perror("lookbib: hunt");
			exit(1);
		}
		map_lower(s);
		fputs(s, hfp);
		pclose(hfp);
		goto again;
	}
	fclose(fp);
	fprintf(stderr, "EOT\n");
	exit(0);
}

map_lower(s)		/* map string s to lower case */
char *s;
{
	for ( ; *s; ++s)
		if (isupper(*s))
			*s = tolower(*s);
}

instruct()
{
	fputs("\nType keywords (such as author and date) after the > prompt.\n",
		stderr);
	fputs("References with those keywords are printed if they exist;\n",
		stderr);
	fputs("\tif nothing matches you are given another prompt.\n",
		stderr);
	fputs("To quit lookbib, press CTRL-d after the > prompt.\n\n",
		stderr);
}
