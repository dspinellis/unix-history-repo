/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)vis.c	1.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <vis.h>

int eflags, fold, foldwidth=80, none, markeol, debug;

main(argc, argv) 
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	extern int errno;
	FILE *fp;
	int ch;

	while ((ch = getopt(argc, argv, "nwctsobfF:ld")) != EOF)
		switch((char)ch) {
		case 'n':
			none++;
			break;
		case 'w':
			eflags |= VIS_WHITE;
			break;
		case 'c':
			eflags |= VIS_CSTYLE;
			break;
		case 't':
			eflags |= VIS_TAB;
			break;
		case 's':
			eflags |= VIS_SAFE;
			break;
		case 'o':
			eflags |= VIS_OCTAL;
			break;
		case 'b':
			eflags |= VIS_NOSLASH;
			break;
		case 'F':
			if ((foldwidth = atoi(optarg))<5) {
				fprintf(stderr, 
				 "vis: can't fold lines to less than 5 cols\n");
				exit(1);
			}
			/*FALLTHROUGH*/
		case 'f':
			fold++;		/* fold output lines to 80 cols */
			break;		/* using hidden newline */
		case 'l':
			markeol++;	/* mark end of line with \$ */
			break;
#ifdef DEBUG
		case 'd':
			debug++;
			break;
#endif
		case '?':
		default:
			fprintf(stderr, 
		"usage: vis [-nwctsobf] [-F foldwidth]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	if (*argv)
		while (*argv) {
			if ((fp=fopen(*argv, "r")) != NULL)
				process(fp, *argv);
			else
				fprintf(stderr, "vis: %s: %s\n", *argv,
				    (char *)strerror(errno));
			argv++;
		}
	else
		process(stdin, "<stdin>");
	exit(0);
}
	
process(fp, filename)
	FILE *fp;
	char *filename;
{
	static int col = 0;
	register char *cp = "\0"+1;	/* so *(cp-1) starts out != '\n' */
	register int c, rachar; 
	register char nc;
	char buff[5];
	
	c = getc(fp);
	while (c != EOF) {
		rachar = getc(fp);
		if (none) {
			cp = buff;
			*cp++ = c;
			if (c == '\\')
				*cp++ = '\\';
			*cp = '\0';
		} else if (markeol && c == '\n') {
			cp = buff;
			if ((eflags & VIS_NOSLASH) == 0)
				*cp++ = '\\';
			*cp++ = '$';
			*cp++ = '\n';
			*cp = '\0';
		} else 
			(void) vis(buff, (char)c, eflags, (char)rachar);

		cp = buff;
		if (fold) {
#ifdef DEBUG
			if (debug)
				printf("<%02d,", col);
#endif
			col = foldit(cp, col, foldwidth);
#ifdef DEBUG
			if (debug)
				printf("%02d>", col);
#endif
		}
		do {
			putchar(*cp);
		} while (*++cp);
		c = rachar;
	}
	/*
	 * terminate partial line with a hidden newline
	 */
	if (fold && *(cp-1) != '\n')
		printf("\\\n");
}
