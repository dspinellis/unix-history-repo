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
static char sccsid[] = "@(#)strings.c	5.3 (Berkeley) 12/3/86";
#endif not lint

#include <sys/types.h>
#include <sys/file.h>
#include <a.out.h>
#include <stdio.h>
#include <ctype.h>

#define DEF_LEN		4		/* default minimum string length */
#define EOS		(char)NULL	/* end of string */
#define ERR		-1		/* general error */
#define ERREXIT		1		/* error exit */
#define NO		0		/* false/no */
#define OK		0		/* ok exit */
#define YES		1		/* true/yes */

#define ISSTR(ch)	(isascii(ch) && (isprint(ch) || ch == '\t'))

typedef struct exec	EXEC;		/* struct exec cast */

static long	foff;			/* offset in the file */
static int	hcnt,			/* head count */
		head_len,		/* length of header */
		read_len;		/* length to read */
static u_char	hbfr[sizeof(EXEC)];	/* buffer for struct exec */

main(argc,argv)
int	argc;
char	**argv;
{
	register int	ch,		/* character */
			cnt;		/* general counter */
	register u_char	*C;		/* bfr pointer */
	EXEC	*head;			/* exec header pointer */
	int	minlen = DEF_LEN;	/* minimum string length */
	short	asdata = NO,		/* look in everything */
		oflg;			/* print octal location */
	u_char	*bfr;			/* collection buffer */
	char	*file,			/* file name for error */
		*malloc();

	/*
	 * for backward compatibility, allow '-' to specify 'a' flag; no
	 * longer documented in the man page or usage string.
	 */
	for (++argv;*argv && **argv ==  '-';++argv) {
		for (cnt = 1;(*argv)[cnt];++cnt)
			switch ((*argv)[cnt]) {
				case 'a':
					asdata = YES;
					break;
				case 'o':
					oflg = YES;
					break;
				default:	/* getopt message compatible */
					if (!isdigit((*argv)[cnt])) {
						fprintf(stderr,"strings: illegal option -- %c\nusage: strings [-ao] [-#] [file ... ]\n",(*argv)[cnt]);
						exit(ERREXIT);
					}
					minlen = atoi(*argv + 1);
					break;
			}
		if (cnt == 1)
			asdata = YES;
	}

	if (!(bfr = (u_char *)malloc((u_int)minlen))) {
		fputs("strings: unable to allocate space.\n",stderr);
		exit(ERREXIT);
	}
	bfr[minlen] = EOS;
	file = "stdin";
	do {
		if (*argv) {
			if (!freopen(*argv,"r",stdin)) {
				perror(*argv);
				exit(ERREXIT);
			}
			file = *argv++;
		}
		foff = 0;
		read_len = ERR;
		if (asdata)
			head_len = 0;
		else {
			head = (EXEC *)hbfr;
			if ((head_len = read(fileno(stdin),(char *)head,sizeof(EXEC))) == ERR) {
				perror(file);
				exit(ERREXIT);
			}
			if (head_len == sizeof(EXEC) && !N_BADMAG(*head)) {
				foff = N_TXTOFF(*head) + head->a_text;
				if (fseek(stdin,foff,L_SET) == ERR) {
					perror(file);
					exit(ERREXIT);
				}
				read_len = head->a_data;
				head_len = 0;
			}
			else
				hcnt = 0;
		}
		for (cnt = 0;(ch = getch()) != EOF;) {
			if (ISSTR(ch)) {
				if (!cnt)
					C = bfr;
				*C++ = ch;
				if (++cnt < minlen)
					continue;
				if (oflg)
					printf("%07ld %s",foff - minlen,bfr);
				else
					fputs((char *)bfr,stdout);
				while ((ch = getch()) != EOF && ISSTR(ch))
					putchar((char)ch);
				putchar('\n');
			}
			cnt = 0;
		}
	} while (*argv);
	exit(OK);
}

/*
 * getch --
 *	get next character from wherever
 */
static
getch()
{
	++foff;
	if (head_len) {
		if (hcnt < head_len)
			return((int)hbfr[hcnt++]);
		head_len = 0;
	}
	if (read_len == ERR || read_len-- > 0)
		return(getchar());
	return(EOF);
}
