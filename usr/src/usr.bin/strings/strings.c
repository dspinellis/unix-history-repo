/*
 * Copyright (c) 1980, 1987 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1987 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)strings.c	5.11 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <fcntl.h>
#include <errno.h>
#include <a.out.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define DEF_LEN		4		/* default minimum string length */
#define ISSTR(ch)	(isascii(ch) && (isprint(ch) || ch == '\t'))

typedef struct exec	EXEC;		/* struct exec cast */

static long	foff;			/* offset in the file */
static int	hcnt,			/* head count */
		head_len,		/* length of header */
		read_len;		/* length to read */
static u_char	hbfr[sizeof(EXEC)];	/* buffer for struct exec */

static void usage();

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register int ch, cnt;
	register u_char *C;
	EXEC *head;
	int exitcode, minlen;
	short asdata, oflg, fflg;
	u_char *bfr;
	char *file, *p;

	/*
	 * for backward compatibility, allow '-' to specify 'a' flag; no
	 * longer documented in the man page or usage string.
	 */
	asdata = exitcode = fflg = oflg = 0;
	minlen = -1;
	while ((ch = getopt(argc, argv, "-0123456789anof")) != EOF)
		switch((char)ch) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			/*
			 * kludge: strings was originally designed to take
			 * a number after a dash.
			 */
			if (minlen == -1) {
				p = argv[optind - 1];
				if (p[0] == '-' && p[1] == ch && !p[2])
					minlen = atoi(++p);
				else
					minlen = atoi(argv[optind] + 1);
			}
			break;
		case '-':
		case 'a':
			asdata = 1;
			break;
		case 'f':
			fflg = 1;
			break;
		case 'n':
			minlen = atoi(optarg);
			break;
		case 'o':
			oflg = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (minlen == -1)
		minlen = DEF_LEN;

	if (!(bfr = malloc((u_int)minlen))) {
		(void)fprintf(stderr, "strings: %s\n", strerror(errno));
		exit(1);
	}
	bfr[minlen] = '\0';
	file = "stdin";
	do {
		if (*argv) {
			file = *argv++;
			if (!freopen(file, "r", stdin)) {
				(void)fprintf(stderr,
				    "strings: %s: %s\n", file, strerror(errno));
				exitcode = 1;
				goto nextfile;
			}
		}
		foff = 0;
#define DO_EVERYTHING()		{read_len = -1; head_len = 0; goto start;}
		read_len = -1;
		if (asdata)
			DO_EVERYTHING()
		else {
			head = (EXEC *)hbfr;
			if ((head_len =
			    read(fileno(stdin), head, sizeof(EXEC))) == -1)
				DO_EVERYTHING()
			if (head_len == sizeof(EXEC) && !N_BADMAG(*head)) {
				foff = N_TXTOFF(*head);
				if (fseek(stdin, foff, SEEK_SET) == -1)
					DO_EVERYTHING()
				read_len = head->a_text + head->a_data;
				head_len = 0;
			}
			else
				hcnt = 0;
		}
start:
		for (cnt = 0; (ch = getch()) != EOF;) {
			if (ISSTR(ch)) {
				if (!cnt)
					C = bfr;
				*C++ = ch;
				if (++cnt < minlen)
					continue;
				if (fflg)
					printf("%s:", file);
				if (oflg)
					printf("%07ld %s",
					    foff - minlen, (char *)bfr);
				else
					printf("%s", bfr);
				while ((ch = getch()) != EOF && ISSTR(ch))
					putchar((char)ch);
				putchar('\n');
			}
			cnt = 0;
		}
nextfile: ;
	} while (*argv);
	exit(exitcode);
}

/*
 * getch --
 *	get next character from wherever
 */
getch()
{
	++foff;
	if (head_len) {
		if (hcnt < head_len)
			return((int)hbfr[hcnt++]);
		head_len = 0;
	}
	if (read_len == -1 || read_len-- > 0)
		return(getchar());
	return(EOF);
}

static void
usage()
{
	(void)fprintf(stderr,
	    "usage: strings [-afo] [-n length] [file ... ]\n");
	exit(1);
}
