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
static char sccsid[] = "@(#)vis.c	1.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <cencode.h>

int eflags, dflags, invert, strip, fold, foldwidth=80, none;

main(argc, argv) 
	char *argv[];
{
	extern char *optarg;
	extern int optind;
	extern int errno;
	FILE *fp;
	int ch;

	while ((ch = getopt(argc, argv, "nwcgovishfF:")) != EOF)
		switch((char)ch) {
		case 'n':
			none++;
			break;
		case 'w':
			eflags |= CENC_WHITE;
			break;
		case 'c':
			eflags |= CENC_CSTYLE;
			break;
		case 'g':
			eflags |= CENC_GRAPH;
			break;
		case 'o':
			eflags |= CENC_OCTAL;
			break;
		case 'v':	/* vis -v considered harmful */
			eflags |= CENC_GRAPH | CENC_OCTAL;
			break;
		case 'i':
			invert++;
			break;
		case 'F':
			if ((foldwidth = atoi(optarg))<5) {
				fprintf(stderr, 
				 "vis: can't fold lines to less than 2 cols\n");
				exit(1);
			}
			/*FALLTHROUGH*/
		case 'f':
			fold++;		/* fold output lines to 80 cols */
			break;		/* using hidden newline */
		case 's':
			strip++;
			break;
		case 'h':
			dflags |= CDEC_HAT;
			break;
		case '?':
		default:
			fprintf(stderr, 
		"usage: vis [-nwcgovifsh] [-F foldwidth]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

#define ALL	(CENC_CSTYLE | CENC_GRAPH | CENC_OCTAL)
	if (none)
		eflags &= ~ALL;
	else if (!(eflags&ALL))
		eflags |= ALL;
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
	register char *cp = "X"+1;	/* so *(cp-1) starts out != '\n' */
	register int byte = 0;
	register int c, rachar; 
	register char nc;
	
	/*
	 * Encode
	 */
	if (!invert) {
		c = getc(fp);
		while (c != EOF) {
			rachar = getc(fp);
			if (strip)
				c &= 0177;
			cp = cencode((char)c, eflags|CENC_RACHAR, (char)rachar);
			if (fold) {
				/*
				 * Keep track of printables and
				 * space chars (like fold(1)).
				 */
				for (;;) {
					switch(*cp) {
					case '\n':
					case '\r':
						col = 0;
						break;
					case '\t':
						col = col + 8 &~ 07;
						break;
					case '\b':
						col = col ? col - 1 : 0;
						break;
					default:
						col += strlen(cp);
					}
					if (col > (foldwidth-2)) {
						printf("\\\n");
						col = 0;
					} else
						break;
				}
			}
			do {
				putchar(*cp++);
			} while (*cp);
			c = rachar;
		}
		if (fold && *(cp-1) != '\n')
			printf("\\\n");
	/*
	 * Decode
	 */
	} else {
		while ((c = getc(fp)) != EOF) {
			byte++;
		again:
			switch(cdecode((char)c, &nc, dflags)) {
			case CDEC_NEEDMORE:
			case CDEC_NOCHAR:
				break;
			case CDEC_OK:
				putchar(nc);
				break;
			case CDEC_OKPUSH:
				putchar(nc);
				goto again;
			case CDEC_SYNBAD:
				fprintf(stderr, 
				    "vis: %s: offset: %d: can't decode\n", 
				    filename, byte);
				break;
			default:
				fprintf(stderr,
				    "vis: bad return value (can't happen)\n");
				exit(1);
			}
		}
		if (cdecode((char)0, &nc, CDEC_END) == CDEC_OK)
			putchar(nc);
	}
	exit(0);
}
