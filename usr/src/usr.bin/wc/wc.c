/*
 * Copyright (c) 1980, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)wc.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/* wc line, word and char count */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>

#define DEL	0177			/* del char */
#define NL	012			/* newline char */
#define SPACE	040			/* space char */
#define TAB	011			/* tab char */

static long	tlinect, twordct, tcharct;
static int	doline,	doword, dochar;

main(argc,argv)
	int argc;
	char **argv;
{
	extern int optind;
	register int ch;

	/*
	 * wc is unusual in that its flags are on by default, so,
	 * if you don't get any arguments, you have to turn them
	 * all on.
	 */
	if (argc > 1 && argv[1][0] == '-' && argv[1][1]) {
		while ((ch = getopt(argc,argv,"lwc")) != EOF)
			switch((char)ch) {
			case 'l':
				doline = 1;
				break;
			case 'w':
				doword = 1;
				break;
			case 'c':
				dochar = 1;
				break;
			case '?':
			default:
				fputs("usage: wc [-lwc] [files]\n",stderr);
				exit(1);
			}
		argv += optind;
		argc -= optind;
	}
	else {
		++argv;
		--argc;
		doline = doword = dochar = 1;
	}

	/* should print "stdin" as the file name, here */
	if (argc <= 1) {
		if (!*argv || !strcmp(*argv, "-")) {
			cnt((char *)NULL);
			putchar('\n');
		}
		else {
			cnt(*argv);
			printf(" %s\n", *argv);
		}
		exit(0);
	}

	/*
	 * cat allows "-" as stdin anywhere in the arg list,
	 * might as well here, too.  Again, should use "stdin"
	 * as the file name.
	 */
	do {
		if (!strcmp(*argv, "-")) {
			cnt((char *)NULL);
			putchar('\n');
		}
		else {
			cnt(*argv);
			printf(" %s\n", *argv);
		}
	} while(*++argv);

	if (doline)
		printf(" %7ld", tlinect);
	if (doword)
		printf(" %7ld", twordct);
	if (dochar)
		printf(" %7ld", tcharct);
	puts(" total");
	exit(0);
}

static
cnt(file)
	char *file;
{
	register u_char *C;
	register short gotsp;
	register int len;
	register long linect, wordct,charct;	
	struct stat sbuf;
	int fd;
	u_char buf[MAXBSIZE];

	linect = wordct = charct = 0;
	if (file) {
		if ((fd = open(file, O_RDONLY, 0)) < 0) {
			perror(file);
			exit(1);
		}
		if (!doword) {
			/*
			 * line counting is split out because it's a lot
			 * faster to get lines than to get words, since
			 * the word count requires some logic.
			 */
			if (doline) {
				while(len = read(fd, buf, MAXBSIZE)) {
					if (len == -1) {
						perror(file);
						exit(1);
					}
					charct += len;
					for (C = buf; len--; ++C)
						if (*C == '\n')
							++linect;
				}
				tlinect += linect;
				printf(" %7ld", linect);
				if (dochar) {
					tcharct += charct;
					printf(" %7ld", sbuf.st_size);
				}
				close(fd);
				return;
			}
			/*
			 * if all we need is the number of characters and
			 * it's a directory or a regular or linked file, just
			 * stat the puppy.  We avoid testing for it not being
			 * a special device in case someone adds a new type
			 * of inode.
			 */
			if (dochar) {
				if (fstat(fd, &sbuf)) {
					perror(file);
					exit(1);
				}
				if (sbuf.st_mode & (S_IFREG | S_IFLNK | S_IFDIR)) {
					printf(" %7ld", sbuf.st_size);
					tcharct += sbuf.st_size;
					close(fd);
					return;
				}
			}
		}
	}
	else
		fd = 0;
	/* do it the hard way... */
	for (gotsp = 1; len = read(fd, buf, MAXBSIZE);) {
		if (len == -1) {
			perror(file);
			exit(1);
		}
		charct += len;
		for (C = buf; len--; ++C)
			switch(*C) {
				case NL:
					++linect;
				case TAB:
				case SPACE:
					gotsp = 1;
					continue;
				default:
#ifdef notdef
					/*
					 * This line of code implements the
					 * original V7 wc algorithm, i.e.
					 * a non-printing character doesn't
					 * toggle the "word" count, so that
					 * "  ^D^F  " counts as 6 spaces,
					 * while "foo^D^Fbar" counts as 8
					 * characters.
					 *
					 * test order is important -- gotsp
					 * will normally be NO, so test it
					 * first
					 */
					if (gotsp && *C > SPACE && *C < DEL) {
#endif
					/*
					 * This line implements the manual
					 * page, i.e. a word is a "maximal
					 * string of characters delimited by
					 * spaces, tabs or newlines."  Notice
					 * nothing was said about a character
					 * being printing or non-printing.
					 */
					if (gotsp) {
						gotsp = 0;
						++wordct;
					}
			}
	}
	if (doline) {
		tlinect += linect;
		printf(" %7ld", linect);
	}
	if (doword) {
		twordct += wordct;
		printf(" %7ld", wordct);
	}
	if (dochar) {
		tcharct += charct;
		printf(" %7ld", charct);
	}
	close(fd);
}
