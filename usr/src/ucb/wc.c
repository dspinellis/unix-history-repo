/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)wc.c	5.1 (Berkeley) 5/31/85";
#endif not lint

/* wc line and word count */

#include <stdio.h>
long	linect, wordct, charct, pagect;
long	tlinect, twordct, tcharct, tpagect;
char	*wd = "lwc";

main(argc, argv)
char **argv;
{
	int i, token;
	register FILE *fp;
	register int c;
	char *p;

	while (argc > 1 && *argv[1] == '-') {
		switch (argv[1][1]) {
		case 'l': case 'w': case 'c': 
			wd = argv[1]+1;
			break;
		default:
		usage:
			fprintf(stderr, "Usage: wc [-lwc] [files]\n");
			exit(1);
		}
		argc--;
		argv++;
	}

	i = 1;
	fp = stdin;
	do {
		if(argc>1 && (fp=fopen(argv[i], "r")) == NULL) {
			perror(argv[i]);
			continue;
		}
		linect = 0;
		wordct = 0;
		charct = 0;
		token = 0;
		for(;;) {
			c = getc(fp);
			if (c == EOF)
				break;
			charct++;
			if(' '<c&&c<0177) {
				if(!token) {
					wordct++;
					token++;
				}
				continue;
			}
			if(c=='\n') {
				linect++;
			}
			else if(c!=' '&&c!='\t')
				continue;
			token = 0;
		}
		/* print lines, words, chars */
		wcp(wd, charct, wordct, linect);
		if(argc>1) {
			printf(" %s\n", argv[i]);
		} else
			printf("\n");
		fclose(fp);
		tlinect += linect;
		twordct += wordct;
		tcharct += charct;
	} while(++i<argc);
	if(argc > 2) {
		wcp(wd, tcharct, twordct, tlinect);
		printf(" total\n");
	}
	exit(0);
}

wcp(wd, charct, wordct, linect)
register char *wd;
long charct; long wordct; long linect;
{
	while (*wd) switch (*wd++) {
	case 'l':
		ipr(linect);
		break;

	case 'w':
		ipr(wordct);
		break;

	case 'c':
		ipr(charct);
		break;

	}
}

ipr(num)
long num;
{
	printf(" %7ld", num);
}

