/* Copyright (c) 1979 Regents of the University of California */
#
/*
 * pi - Pascal interpreter code translator
 *
 * Charles Haley, Bill Joy UCB
 * Version 1.2 January 1979
 *
 *
 * pxp - Pascal execution profiler
 *
 * Bill Joy UCB
 * Version 1.2 January 1979
 */

#include "0.h"
#include "yy.h"

int	line 1;

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
	extern int fout[];
	register int c;

	close(1);
	if (creat(firstname, 0644) != 1) {
		perror(firstname);
		pexit(ERRS);
	}
	seek(fout[0], 0, 0);
	while ((c = read(fout[0], &fout[3], 512)) > 0) {
		if (write(1, &fout[3], c) != c) {
			perror(firstname);
			pexit(ERRS);
		}
	}
}
#endif

static
struct {
	int magic;
	int txt_size;
	int data_size;
} header;

#ifdef PI
magic()
{
	int buf[512];
	register int hf, i;

	hf = open("/usr/lib/npx_header", 0);
	if (hf >= 0 && read(hf, buf, 1024) > 16) {
		header.magic = buf[0];
		header.txt_size = buf[1];
		header.data_size = buf[2];
		for (i = 0; i < 512; i++)
			word(buf[i]);
	}
	close(hf);
	word(0404);
}

magic2()
{
	int i;

	if (header.magic != 0407)
		return;
	pflush();
	seek(ofil, 0, 0);
	header.data_size = lc - header.txt_size;
	header.data_size =- 16;
	write(ofil, &header, sizeof header);
	seek(ofil, 1022, 0);
	i = ((int) lc) - 1024;
	write(ofil, &i, 2);
}
#endif

#ifdef PXP
writef(i, cp)
{

	write(i, cp, strlen(cp));
}
#endif
