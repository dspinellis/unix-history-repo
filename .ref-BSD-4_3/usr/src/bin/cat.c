/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)cat.c	5.2 (Berkeley) 12/6/85";
#endif not lint

/*
 * Concatenate files.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

/* #define OPTSIZE BUFSIZ	/* define this only if not 4.2 BSD or beyond */

int	bflg, eflg, nflg, sflg, tflg, uflg, vflg;
int	spaced, col, lno, inline, ibsize, obsize;

main(argc, argv)
char **argv;
{
	int fflg = 0;
	register FILE *fi;
	register c;
	int dev, ino = -1;
	struct stat statb;
	int retval = 0;

	lno = 1;
	for( ; argc>1 && argv[1][0]=='-'; argc--,argv++) {
		switch(argv[1][1]) {
		case 0:
			break;
		case 'u':
			setbuf(stdout, (char *)NULL);
			uflg++;
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
#ifndef	OPTSIZE
		obsize = statb.st_blksize;
#endif
	}
	else
		obsize = 0;
	if (argc < 2) {
		argc = 2;
		fflg++;
	}
	while (--argc > 0) {
		if (fflg || (*++argv)[0]=='-' && (*argv)[1]=='\0')
			fi = stdin;
		else {
			if ((fi = fopen(*argv, "r")) == NULL) {
				perror(*argv);
				retval = 1;
				continue;
			}
		}
		if (fstat(fileno(fi), &statb) == 0) {
			if ((statb.st_mode & S_IFMT) == S_IFREG &&
			    statb.st_dev==dev && statb.st_ino==ino) {
				fprintf(stderr, "cat: input %s is output\n",
				   fflg?"-": *argv);
				fclose(fi);
				retval = 1;
				continue;
			}
#ifndef	OPTSIZE
			ibsize = statb.st_blksize;
#endif
		}
		else
			ibsize = 0;
		if (nflg||sflg||vflg)
			copyopt(fi);
		else if (uflg) {
			while ((c = getc(fi)) != EOF)
				putchar(c);
		} else
			retval |= fastcat(fileno(fi));	/* no flags specified */
		if (fi!=stdin)
			fclose(fi);
		else
			clearerr(fi);		/* reset sticky eof */
		if (ferror(stdout)) {
			fprintf(stderr, "cat: output write error\n");
			retval = 1;
			break;
		}
	}
	exit(retval);
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

fastcat(fd)
register int fd;
{
	register int	buffsize, n, nwritten, offset;
	register char	*buff;
	struct stat	statbuff;
	char		*malloc();

#ifndef	OPTSIZE
	if (obsize)
		buffsize = obsize;	/* common case, use output blksize */
	else if (ibsize)
		buffsize = ibsize;
	else
		buffsize = BUFSIZ;
#else
	buffsize = OPTSIZE;
#endif

	if ((buff = malloc(buffsize)) == NULL) {
		perror("cat: no memory");
		return (1);
	}

	/*
	 * Note that on some systems (V7), very large writes to a pipe
	 * return less than the requested size of the write.
	 * In this case, multiple writes are required.
	 */
	while ((n = read(fd, buff, buffsize)) > 0) {
		offset = 0;
		do {
			nwritten = write(fileno(stdout), &buff[offset], n);
			if (nwritten <= 0) {
				perror("cat: write error");
				exit(2);
			}
			offset += nwritten;
		} while ((n -= nwritten) > 0);
	}

	free(buff);
	if (n < 0) {
		perror("cat: read error");
		return (1);
	}
	return (0);
}
