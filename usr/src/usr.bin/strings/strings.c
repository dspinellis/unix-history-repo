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
static char sccsid[] = "@(#)strings.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <a.out.h>
#include <stdio.h>
#include <ctype.h>

#define DEF_LEN		4		/* default minimum string length */
#define ISSTR(ch)	(isascii(ch) && (isprint(ch) || ch == '\t'))

typedef struct exec	EXEC;		/* struct exec cast */

static long	foff;			/* offset in the file */
static int	hcnt,			/* head count */
		head_len,		/* length of header */
		read_len;		/* length to read */
static u_char	hbfr[sizeof(EXEC)];	/* buffer for struct exec */

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register int ch, cnt;
	register u_char *C;
	EXEC *head;
	int minlen;
	short asdata, oflg;
	u_char *bfr;
	char *file, *p, *malloc();

	/*
	 * for backward compatibility, allow '-' to specify 'a' flag; no
	 * longer documented in the man page or usage string.
	 */
	asdata = oflg = 0;
	minlen = -1;
	while ((ch = getopt(argc, argv, "-0123456789ao")) != EOF)
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
		case 'o':
			oflg = 1;
			break;
		case '?':
		default:
			fprintf(stderr,
			    "usage: strings [-ao] [-#] [file ... ]\n");
			exit(1);
		}
	argc -= optind;
	argv += optind;

	if (minlen == -1)
		minlen = DEF_LEN;

	if (!(bfr = (u_char *)malloc((u_int)minlen))) {
		fputs("strings: no space.\n", stderr);
		exit(1);
	}
	bfr[minlen] = '\0';
	file = "stdin";
	do {
		if (*argv) {
			if (!freopen(*argv, "r", stdin)) {
				perror(*argv);
				exit(1);
			}
			file = *argv++;
		}
		foff = 0;
		read_len = -1;
		if (asdata)
			head_len = 0;
		else {
			head = (EXEC *)hbfr;
			if ((head_len = read(fileno(stdin), (char *)head, sizeof(EXEC))) == -1) {
				perror(file);
				exit(1);
			}
			if (head_len == sizeof(EXEC) && !N_BADMAG(*head)) {
				foff = N_TXTOFF(*head) + head->a_text;
				if (fseek(stdin, foff, L_SET) == -1) {
					perror(file);
					exit(1);
				}
				read_len = head->a_data;
				head_len = 0;
			}
			else
				hcnt = 0;
		}
		for (cnt = 0; (ch = getch()) != EOF;) {
			if (ISSTR(ch)) {
				if (!cnt)
					C = bfr;
				*C++ = ch;
				if (++cnt < minlen)
					continue;
				if (oflg)
					printf("%07ld %s", foff - minlen,
					    (char *)bfr);
				else
					fputs((char *)bfr, stdout);
				while ((ch = getch()) != EOF && ISSTR(ch))
					putchar((char)ch);
				putchar('\n');
			}
			cnt = 0;
		}
	} while (*argv);
	exit(0);
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
