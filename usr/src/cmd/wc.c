static char *sccsid = "@(#)wc.c	4.3 (Berkeley) 11/7/80";
/* wc line and word count */

#include <stdio.h>
long	linect, wordct, charct, pagect;
long	tlinect, twordct, tcharct, tpagect;
int	baud=300;	/* baud rate */
int	cps=30;		/* # of chars per second */
int	lpp=66;		/* # of lines per page */
char	*wd = "lwc";
int	verbose;
int	uucp;

main(argc, argv)
char **argv;
{
	int i, token;
	register FILE *fp;
	register int c;
	char *p;

	while (argc > 1 && *argv[1] == '-') {
		switch (argv[1][1]) {
		case 'l': case 'w': case 'c': case 'p': case 't':
			wd = argv[1]+1;
			break;
		case 's':
			lpp = atoi(argv[1]+2);
			if (lpp <= 0)
				goto usage;
			break;
		case 'v':
			verbose++;
			wd = "lwcpt";
			break;
		case 'u':
			uucp++;
			break;
		case 'b':
			baud = atoi(argv[1]+2);
			if (baud == 110)
				cps = 10;
			else
				cps = baud / 10;
			if (cps <= 0)
				goto usage;
			break;
		default:
		usage:
			fprintf(stderr, "Usage: wc [-lwcpt] [-v] [-u] [-spagesize] [-bbaudrate]\n");
			exit(1);
		}
		argc--;
		argv++;
	}

	if (uucp)
		cps = cps * 9 / 10;	/* 27 cps at 300 baud */

	if (verbose) {
		for (p=wd; *p; p++)
			switch(*p) {
			case 'l':
				printf("lines\t");
				break;
			case 'w':
				printf("words\t");
				break;
			case 'c':
				printf("chars\t");
				break;
			case 'p':
				printf("pages\t");
				break;
			case 't':
				printf("time@%d\t",baud);
				break;
			}
		printf("\n");
	}

	i = 1;
	fp = stdin;
	do {
		if(argc>1 && (fp=fopen(argv[i], "r")) == NULL) {
			fprintf(stderr, "wc: can't open %s\n", argv[i]);
			continue;
		}
		linect = 0;
		wordct = 0;
		charct = 0;
		pagect = 0;
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
				if (linect % lpp == 1)
					pagect++;
			}
			else if(c!=' '&&c!='\t')
				continue;
			token = 0;
		}
		/* print lines, words, chars */
		wcp(wd, charct, wordct, linect, pagect);
		if(argc>1) {
			printf(" %s\n", argv[i]);
		} else
			printf("\n");
		fclose(fp);
		tlinect += linect;
		twordct += wordct;
		tcharct += charct;
		tpagect += pagect;
	} while(++i<argc);
	if(argc > 2) {
		wcp(wd, tcharct, twordct, tlinect, tpagect);
		printf(" total\n");
	}
	exit(0);
}

wcp(wd, charct, wordct, linect, pagect)
register char *wd;
long charct; long wordct; long linect, pagect;
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

	case 'p':
		ipr(pagect);
		break;

	case 't':
		prttime(charct/cps);
		break;
	}
}

ipr(num)
long num;
{
	if (verbose)
		printf("%ld\t", num);
	else
		printf("%7ld", num);
}

prttime(secs)
long secs;
{
	int hrs,mins;
	float t;
	long osecs;
	char *units;

	osecs = secs;
	hrs = secs / (60*60);
	secs = secs % (60*60);
	mins = secs / 60;
	secs = secs % 60;

	t = osecs;
	if (hrs) {
		t /= (60*60);
		units = "hr";
	} else if (mins) {
		t /= 60;
		units = "mi";
	} else {
		units = "se";
	}
	printf("%4.1f %2s\t", t, units);
}
