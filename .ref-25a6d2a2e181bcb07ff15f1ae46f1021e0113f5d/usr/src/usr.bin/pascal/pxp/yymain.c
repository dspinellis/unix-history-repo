/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)yymain.c	5.3 (Berkeley) %G%";
#endif not lint

/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 November 1978
 *
 *
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 November 1978
 */

#include "whoami.h"
#include "0.h"
#include "yy.h"

int	line = 1;

/*
 * Yymain initializes each of the utility
 * clusters and then starts the processing
 * by calling yyparse.
 */
yymain()
{

	/*
	 * Initialize the scanner
	 */
#ifdef PXP
	if (bracket == 0) {
#endif
		if (getline() == -1) {
			Perror(filename, "No lines in file");
			pexit(NOSTART);
		}
#ifdef PXP
	} else
		yyline = 0;
#endif

#ifdef PI
	magic();

#endif
	/*
	 * Initialize the clusters
	 *
	initstring();
	 */
	inithash();
	inittree();
#ifdef PI
	initnl();
#endif

	/*
	 * Process the input
	 */
	yyparse();
#ifdef PI
	magic2();
#ifdef DEBUG
	dumpnl(0);
#endif
#endif
#ifdef PXP
	prttab();
	if (onefile) {
		extern int outcol;

		if (outcol)
			putchar('\n');
		flush();
		if (eflg) {
			writef(2, "File not rewritten because of errors\n");
			pexit(ERRS);
		}
		signal(1, 1);
		signal(2, 1);
		copyfile();
	}
#endif
	pexit(eflg ? ERRS : AOK);
}

#ifdef PXP
copyfile()
{
	register int c;
	char buf[BUFSIZ];

	if (freopen(stdoutn, "r", stdin) == NULL) {
		perror(stdoutn);
		pexit(ERRS);
	}
	if (freopen(firstname, "w", stdout) == NULL) {
		perror(firstname);
		pexit(ERRS);
	}
	while ((c = getchar()) > 0)
		putchar(c);
	if (ferror(stdout))
		perror(stdout);
}
#endif

static
struct {
	int		magic;
	unsigned	txt_size;
	unsigned	data_size;
	unsigned	bss_size;
	unsigned	syms_size;
	unsigned	entry_point;
	unsigned	tr_size;
	unsigned	dr_size;
} header;

#ifdef PI
magic()
{

    /*
     *	this is the size of /usr/lib/npxheader
     */
#define	HEAD_BYTES	1024
	short		buf[HEAD_BYTES / sizeof ( short )];
	unsigned	*ubuf = buf;
	register int	hf, i;

	hf = open("/usr/lib/npx_header", 0);
	if (hf >= 0 && read(hf, buf, HEAD_BYTES) > sizeof header) {
		header.magic = ubuf[0];
		header.txt_size = ubuf[1];
		header.data_size = ubuf[2];
		header.bss_size = ubuf[3];
		header.syms_size = ubuf[4];
		header.entry_point = ubuf[5];
		header.tr_size = ubuf[6];
		header.dr_size = ubuf[7];
		for (i = 0; i < HEAD_BYTES / sizeof ( short ); i++)
			word(buf[i]);
	}
	close(hf);
	word(0404);
}

magic2()
{
	short i;

	if  (header.magic != 0407)
		panic ( "magic2" );
	pflush();
	lseek(ofil, 0l, 0);
	header.data_size = ( unsigned ) lc - header.txt_size;
	header.data_size -= sizeof header;
	write(ofil, &header, sizeof header);
	lseek(ofil, ( long ) ( HEAD_BYTES - sizeof ( short ) ) , 0);
	i = ( ( unsigned ) lc) - HEAD_BYTES;
	write(ofil, &i, 2);
}
#endif

#ifdef PXP
writef(i, cp)
{

	write(i, cp, strlen(cp));
}
#endif
