/* Copyright (c) 1979 Regents of the University of California */
#
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

#include "whoami"
#include "0.h"
#include "yy.h"

short	line = 1;

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
			pchr('\n');
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
	lseek(fout[0], 0l, 0);
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

	short		buf[PX_HEAD_BYTES / sizeof ( short )];
	unsigned	*ubuf = buf;
	register int	hf, i;

	hf = open("/usr/lib/px_header", 0);
	if (hf >= 0 && read(hf, buf, PX_HEAD_BYTES) > sizeof header) {
		header.magic = ubuf[0];
		header.txt_size = ubuf[1];
		header.data_size = ubuf[2];
		header.bss_size = ubuf[3];
		header.syms_size = ubuf[4];
		header.entry_point = ubuf[5];
		header.tr_size = ubuf[6];
		header.dr_size = ubuf[7];
		for (i = 0; i < PX_HEAD_BYTES / sizeof ( short ); i++)
			word(buf[i]);
	}
	close(hf);
	word(0404);
}

magic2()
{
	int i;

	if  (header.magic != 0407)
		panic ( "magic2" );
	pflush();
	lseek(ofil, 0l, 0);
	header.data_size = ( unsigned ) lc - header.txt_size;
	header.data_size =- sizeof header;
	write(ofil, &header, sizeof header);
	lseek(ofil, ( long ) ( PX_HEAD_BYTES - sizeof ( int ) ) , 0);
	i = ( ( unsigned ) lc) - PX_HEAD_BYTES;
	write(ofil, &i, sizeof (int));
}
#endif

#ifdef PXP
writef(i, cp)
{

	write(i, cp, strlen(cp));
}
#endif
