/*-
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)deliv1.c	4.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

main(argc, argv)	/* goes from file:begin,l to actual characters */
char *argv[];
{
	FILE *fopen(), *fa = NULL;
	char line[750], *p, name[100], *strcpy();
	long lp;
	int len;

	if (argc > 1 && argv[1] && argv[1][0])
		chdir(argv[1]);
	name[0] = NULL;
	while (gets(line))
	{
		if (line[0] == '$' && line[1] == '$') 
		{
			chdir(line+2);
			continue;
		}
		for (p = line; *p != ':'; p++)
			;
		*p++ = 0;
		sscanf(p, "%ld,%d", &lp, &len);
		if (p == line)
			fa = stdin;
		else
			if (strcmp(name, line) != 0)
			{
				if (fa != NULL)
					fclose(fa);
				fa = fopen(line, "r");
				if (fa == NULL)
					err("Can't open %s", line);
				strcpy(name, line);
			}
		if (fa != NULL)
		{
			fseek (fa, lp, 0);
			fread (line, 1, len, fa);
			line[len] = 0;
			fputs(line, stdout);
		}
	}
}
