/*
 * Concatenate files.
 */
static	char *Sccsid = "@(#)cat.c	4.4 (Berkeley) %G%";

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

char	stdbuf[BUFSIZ];
int	bflg, eflg, nflg, sflg, tflg, vflg;
int	spaced, col, lno, inline;

main(argc, argv)
char **argv;
{
	int fflg = 0;
	register FILE *fi;
	register c;
	int dev, ino = -1;
	struct stat statb;

	lno = 1;
	setbuf(stdout, stdbuf);
	for( ; argc>1 && argv[1][0]=='-'; argc--,argv++) {
		switch(argv[1][1]) {
		case 0:
			break;
		case 'u':
			setbuf(stdout, (char *)NULL);
			continue;
		case 'n':
			nflg++;
			continue;
		case 'b':
			bflg++;
			nflg++;
			continue;
		case 'v':
			vflg++;
			continue;
		case 's':
			sflg++;
			continue;
		case 'e':
			eflg++;
			vflg++;
			continue;
		case 't':
			tflg++;
			vflg++;
			continue;
		}
		break;
	}
	if (fstat(fileno(stdout), &statb) == 0) {
		statb.st_mode &= S_IFMT;
		if (statb.st_mode!=S_IFCHR && statb.st_mode!=S_IFBLK) {
			dev = statb.st_dev;
			ino = statb.st_ino;
		}
	}
	if (argc < 2) {
		argc = 2;
		fflg++;
	}
	while (--argc > 0) {
		if (fflg || (*++argv)[0]=='-' && (*argv)[1]=='\0')
			fi = stdin;
		else {
			if ((fi = fopen(*argv, "r")) == NULL) {
				fprintf(stderr, "cat: can't open %s\n", *argv);
				continue;
			}
		}
		if (fstat(fileno(fi), &statb) == 0) {
			if ((statb.st_mode & S_IFMT) == S_IFREG &&
			    statb.st_dev==dev && statb.st_ino==ino) {
				fprintf(stderr, "cat: input %s is output\n",
				   fflg?"-": *argv);
				fclose(fi);
				continue;
			}
		}
		if (nflg||sflg||vflg)
			copyopt(fi);
		else {
			while ((c = getc(fi)) != EOF)
				putchar(c);
		}
		if (fi!=stdin)
			fclose(fi);
	}
	if (ferror(stdout))
		fprintf(stderr, "cat: output write error\n");
	return(0);
}

copyopt(f)
	register FILE *f;
{
	register int c;

top:
	c = getc(f);
	if (c == EOF)
		return;
	if (c == '\n') {
		if (inline == 0) {
			if (sflg && spaced)
				goto top;
			spaced = 1;
		}
		if (nflg && bflg==0 && inline == 0)
			printf("%6d\t", lno++);
		if (eflg)
			putchar('$');
		putchar('\n');
		inline = 0;
		goto top;
	}
	if (nflg && inline == 0)
		printf("%6d\t", lno++);
	inline = 1;
	if (vflg) {
		if (tflg==0 && c == '\t')
			putchar(c);
		else {
			if (c > 0177) {
				printf("M-");
				c &= 0177;
			}
			if (c < ' ')
				printf("^%c", c+'@');
			else if (c == 0177)
				printf("^?");
			else
				putchar(c);
		}
	} else
		putchar(c);
	spaced = 0;
	goto top;
}
