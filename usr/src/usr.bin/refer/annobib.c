#ifndef lint
static char *sccsid = "@(#)annobib.c	4.1 (Berkeley) 5/6/83";
#endif

/*
 * This program has been replaced by "refer -B" (bibliography mode),
 * but is included here for backward compatiblity.
 */

#include <stdio.h>

int noanno = 0;		/* option to suppress .AP from %X field */

main(argc, argv)	/* format (annotated) bibliography for n/troff */
int argc;
char *argv[];
{
	FILE *fp, *fopen();

	if (argv[1][0] == '-' && argv[1][1] == 'x')
	{
		noanno = 1;
		argv++; argc--;
	}
	if (argc == 1)
	{
		annobib(stdin);
		exit(0);
	}
	while (--argc > 0)
	{
		if ((fp = fopen(*++argv, "r")) == NULL)
		{
			perror(*argv);
			exit(1);
		}
		annobib(fp);
		fclose(fp);
	}
	exit(0);
}

annobib(fp)		/* prepare bibliography for refer bare mode */
FILE *fp;
{
	char line[BUFSIZ];
	int begun, ended;

	begun = 0;
	ended = 1;
	while (fgets(line, BUFSIZ, fp))
	{
		if (line[0] == '%' && line[1] == 'X' && !noanno)
		{
			zap_x(line);
			printf(".]\n.AP\n%s", line);
			ended = 1;
			begun = 0;
		}
		else if (line[0] == '%')
		{
			if (!begun)
			{
				puts(".[");
				begun = 1;
				ended = 0;
			}
			fputs(line, stdout);
		}
		else if (line[0] == '\n')
		{
			if (!ended)
			{
				puts(".]");
				ended = 1;
				begun = 0;
			}
		}
		else
			fputs(line, stdout);
	}
	if (!ended)
		puts(".]");
}

zap_x(line)		/* take %X annotation flag out of line */
char line[];
{
	register int i, j;

	for (i = 3, j = 0; line[i] != NULL; i++, j++)
		line[j] = line[i];
	line[j] = NULL;
	return;
}
