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
static char sccsid[] = "@(#)wc.c	5.2 (Berkeley) %G%";
#endif not lint

/* wc line, word and char count */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>

#define DEL	0177			/* del char */
#define ERR	1			/* error exit */
#define NL	012			/* newline char */
#define NO	0			/* no/false */
#define OK	0			/* okay exit */
#define SPACE	040			/* space char */
#define TAB	011			/* tab char */
#define YES	1			/* yes/true */

static long	tlinect,		/* total line count */
		twordct,		/* total word count */
		tcharct;		/* total character count */
static short	doline,			/* if want line count */
		doword,			/* if want word count */
		dochar;			/* if want character count */

main(argc,argv)
int	argc;
char	**argv;
{
	extern char	*optarg;	/* getopt arguments */
	extern int	optind;
	register int	ch;		/* getopt character */

	/*
	 * wc is unusual in that its flags are on by default, so,
	 * if you don't get any arguments, you have to turn them
	 * all on.
	 */
	if (argc > 1 && argv[1][0] == '-' && argv[1][1]) {
		while ((ch = getopt(argc,argv,"lwc")) != EOF)
			switch((char)ch) {
				case 'l':
					doline = YES;
					break;
				case 'w':
					doword = YES;
					break;
				case 'c':
					dochar = YES;
					break;
				case '?':
				default:
					fputs("Usage: wc [-lwc] [files]\n",stderr);
					exit(ERR);
			}
		argv += optind;
		argc -= optind;
	}
	else {
		++argv;
		--argc;
		doline = doword = dochar = YES;
	}

	/* should print "stdin" as the file name, here */
	if (argc <= 1) {
		if (!*argv || !strcmp(*argv,"-")) {
			cnt((char *)NULL);
			putchar('\n');
		}
		else {
			cnt(*argv);
			printf(" %s\n",*argv);
		}
		exit(OK);
	}

	/*
	 * cat allows "-" as stdin anywhere in the arg list,
	 * might as well here, too.  Again, should use "stdin"
	 * as the file name.
	 */
	do {
		if (!strcmp(*argv,"-")) {
			cnt((char *)NULL);
			putchar('\n');
		}
		else {
			cnt(*argv);
			printf(" %s\n",*argv);
		}
	} while(*++argv);

	if (doline)
		printf(" %7ld",tlinect);
	if (doword)
		printf(" %7ld",twordct);
	if (dochar)
		printf(" %7ld",tcharct);
	puts(" total");
	exit(OK);
}

static
cnt(file)
char	*file;
{
	register u_char	*C;		/* traveling pointer */
	register short	gotsp;		/* space toggle */
	register int	len;		/* length of read */
	register long	linect,		/* line count */
			wordct,		/* word count */
			charct;		/* character count */
	struct stat	sbuf;		/* stat buffer */
	int	fd;			/* file descriptor */
	u_char	buf[MAXBSIZE];		/* read buffer */

	linect = wordct = charct = 0;
	if (file) {
		if ((fd = open(file,O_RDONLY)) < 0) {
			perror(file);
			exit(ERR);
		}
		if (!doword) {
			/*
			 * line counting is split out because it's a lot
			 * faster to get lines than to get words, since
			 * the word count requires some logic.
			 */
			if (doline) {
				while(len = read(fd,buf,MAXBSIZE)) {
					if (len == -1) {
						perror(file);
						exit(ERR);
					}
					charct += len;
					for (C = buf;len--;++C)
						if (*C == '\n')
							++linect;
				}
				tlinect += linect;
				printf(" %7ld",linect);
				if (dochar) {
					tcharct += charct;
					printf(" %7ld",sbuf.st_size);
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
				if (fstat(fd,&sbuf)) {
					perror(file);
					exit(ERR);
				}
				if (sbuf.st_mode & (S_IFREG | S_IFLNK | S_IFDIR)) {
					printf(" %7ld",sbuf.st_size);
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
	for (gotsp = YES;len = read(fd,buf,MAXBSIZE);) {
		if (len == -1) {
			perror(file);
			exit(ERR);
		}
		charct += len;
		for (C = buf;len--;++C)
			switch(*C) {
				case NL:
					++linect;
				case TAB:
				case SPACE:
					gotsp = YES;
					continue;
				default:
#ifdef NOT_DEFINED
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
#endif NOT_DEFINED
					/*
					 * This line implements the manual
					 * page, i.e. a word is a "maximal
					 * string of characters delimited by
					 * spaces, tabs or newlines."  Notice
					 * nothing was said about a character
					 * being printing or non-printing.
					 */
					if (gotsp) {
						gotsp = NO;
						++wordct;
					}
			}
	}
	if (doline) {
		tlinect += linect;
		printf(" %7ld",linect);
	}
	if (doword) {
		twordct += wordct;
		printf(" %7ld",wordct);
	}
	if (dochar) {
		tcharct += charct;
		printf(" %7ld",charct);
	}
	close(fd);
}
