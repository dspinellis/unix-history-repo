/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)checkeq.c	4.3 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
FILE	*fin;
int	delim	= '$';

main(argc, argv) char **argv; {

	if (argc <= 1)
		check(stdin);
	else
		while (--argc > 0) {
			if ((fin = fopen(*++argv, "r")) == NULL) {
				perror(*argv);
				exit(1);
			}
			printf("%s:\n", *argv);
			check(fin);
			fclose(fin);
		}
	exit(0);
}

check(f)
FILE	*f;
{
	int start, line, eq, ndel, totdel;
	char in[600], *p;

	start = eq = line = ndel = totdel = 0;
	while (fgets(in, 600, f) != NULL) {
		line++;
		ndel = 0;
		for (p = in; *p; p++)
			if (*p == delim)
				ndel++;
		if (*in=='.' && *(in+1)=='E' && *(in+2)=='Q') {
			if (eq++)
				printf("   Spurious EQ, line %d\n", line);
			if (totdel)
				printf("   EQ in %c%c, line %d\n", delim, delim, line);
		} else if (*in=='.' && *(in+1)=='E' && *(in+2)=='N') {
			if (eq==0)
				printf("   Spurious EN, line %d\n", line);
			else
				eq = 0;
			if (totdel > 0)
				printf("   EN in %c%c, line %d\n", delim, delim, line);
			start = 0;
		} else if (eq && *in=='d' && *(in+1)=='e' && *(in+2)=='l' && *(in+3)=='i' && *(in+4)=='m') {
			for (p=in+5; *p; p++)
				if (*p != ' ') {
					if (*p == 'o' && *(p+1) == 'f')
						delim = 0;
					else
						delim = *p;
					break;
				}
			if (delim == 0)
				printf("   Delim off, line %d\n", line);
			else
				printf("   New delims %c%c, line %d\n", delim, delim, line);
		}
		if (ndel > 0 && eq > 0)
			printf("   %c%c in EQ, line %d\n", delim, delim, line);
		if (ndel == 0)
			continue;
		totdel += ndel;
		if (totdel%2) {
			if (start == 0)
				start = line;
			else {
				printf("   %d line %c%c, lines %d-%d\n", line-start+1, delim, delim, start, line);
				start = line;
			}
		} else {
			if (start > 0) {
				printf("   %d line %c%c, lines %d-%d\n", line-start+1, delim, delim, start, line);
				start = 0;
			}
			totdel = 0;
		}
	}
	if (totdel)
		printf("   Unfinished %c%c\n", delim, delim);
	if (eq)
		printf("   Unfinished EQ\n");
}
