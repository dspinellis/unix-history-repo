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
#include "tree.h"
#include "yy.h"

/*
 * Structure describing queued listing lines during the forward move
 * of error recovery.  These lines will be stroed by yyoutline during
 * the forward move and flushed by yyoutfl or yyflush when an
 * error occurs or a program termination.
 */
struct	B {
	int	Bmagic;
	int	Bline;
	int	Bseekp;
	char	*Bfile;
	int	Bseqid;
	struct	B *Bnext;
} *bottled;

/*
 * Filename gives the current input file, lastname is
 * the last filename we printed, and lastid is the seqid of the last line
 * we printed, to help us avoid printing
 * multiple copies of lines.
 */
extern	char *filename;
char	*lastname;
int	lastid;

char	hadsome;
char	holdbl;

/*
 * Print the current line in the input line
 * buffer or, in a forward move of the recovery, queue it for printing.
 */
yyoutline()
{
	register struct B *bp;

	if (Recovery) {
		bp = tree(6, T_BOTTLE, yyline, yylinpt, filename, yyseqid);
		if (bottled != NIL)
			bp->Bnext = bottled->Bnext, bottled->Bnext = bp;
		else
			bp->Bnext = bp;
		bottled = bp;
		return;
	}
	yyoutfl(yyseqid);
	if (yyseqid != lastid)
		yyprline(charbuf, yyline, filename, yyseqid);
}

/*
 * Flush all the bottled output.
 */
yyflush()
{

	yyoutfl(32767);
}

/*
 * Flush the listing to the sequence id toseqid
 */
yyoutfl(toseqid)
	int toseqid;
{
	register struct B *bp;

	bp = bottled;
	if (bp == NIL)
		return;
	bp = bp->Bnext;
	while (bp->Bseqid <= toseqid) {
		yygetline(bp->Bfile, bp->Bseekp, bp->Bline, bp->Bseqid);
		if (bp->Bnext == bp) {
			bottled = NIL;
			break;
		}
		bp = bp->Bnext;
		bottled->Bnext = bp;
	}
}

int	yygetunit -1;
char	*yygetfile;

/*
 * Yysync guarantees that the line associated
 * with the current token was the last line
 * printed for a syntactic error message.
 */
yysync()
{

	yyoutfl(yyeseqid);
	if (lastid != yyeseqid)
		yygetline(yyefile, yyseekp, yyeline, yyeseqid);
}

yySsync()
{

	yyoutfl(OY.Yyeseqid);
}

/*
 * Yygetline gets a line from a file after we have
 * lost it.  The pointer efile gives the name of the file,
 * seekp its offset in the file, and eline its line number.
 * If this routine has been called before the last file
 * it worked on will be open in yygetunit, with the files
 * name being given in yygetfile.  Note that this unit must
 * be opened independently of the unit in use for normal i/o
 * to this file; if it were a dup seeks would seek both files.
 */
yygetline(efile, seekp, eline, eseqid)
	char *efile;
	int seekp, eline, eseqid;
{
	register int cnt;
	register char *bp;
	char buf[CBSIZE + 1];

	if (lastid == eseqid)
		return;
	if (eseqid == yyseqid) {
		bp = charbuf;
		yyprtd++;
	} else {
		bp = buf;
		if (efile != yygetfile) {
			close(yygetunit);
			yygetfile = efile;
			yygetunit = open(yygetfile, 0);
			if (yygetunit < 0)
oops:
				perror(yygetfile), pexit(DIED);
		} 
		if (seek(yygetunit, seekp, 0) < 0)
			goto oops;
		cnt = read(yygetunit, bp, CBSIZE);
		if (cnt < 0)
			goto oops;
		bp[cnt] = 0;
	}
	yyprline(bp, eline, efile, eseqid);
}

yyretrieve()
{

	yygetline(OY.Yyefile, OY.Yyseekp, OY.Yyeline, OY.Yyeseqid);
}

/*
 * Print the line in the character buffer which has
 * line number line.  The buffer may be terminated by a new
 * line character or a null character.  We process
 * form feed directives, lines with only a form feed character, and
 * suppress numbering lines which are empty here.
 */
yyprline(buf, line, file, id)
	register char *buf;
	int line;
	char *file;
	int id;
{

	lastid = id;
	if (buf[0] == '\f' && buf[1] == '\n') {
		printf("\f\n");
		hadsome = 0;
		holdbl = 0;
		return;
	}
	if (holdbl) {
		putchar('\n');
		holdbl = 0;
	}
	if (buf[0] == '\n')
		holdbl = 1;
	else {
		yysetfile(file);
		yyprintf(buf, line);
	}
	hadsome = 1;
}

yyprintf(cp, line)
	register char *cp;
	int line;
{

	printf("%6d  ", line);
	while (*cp != 0 && *cp != '\n')
		putchar(graphic(*cp++));
	putchar('\n');
}

graphic(ch)
	register CHAR ch;
{

	switch (ch) {
		default:
			if (ch >= ' ')
				return (ch);
		case 0177:
			return ('?');
		case '\n':
		case '\t':
			return (ch);
	}
}

extern	int nopflg;

char	printed 1;
/*
 * Set the current file name to be file,
 * printing the name, or a header on a new
 * page if required.
 */
yysetfile(file)
	register char *file;
{

#ifdef PXP
	if (nopflg == 1)
		return;
#endif

	if (lastname == file)
		return;
	if (file == filename && opt('n') && (printed & 02) == 0) {
		printed =| 02;
		header();
	} else
		yyputfn(file);
	lastname = file;
}

/*
 * Put out an include file name
 * if an error occurs but the name has
 * not been printed (or if another name
 * has been printed since it has).
 */
yyputfn(cp)
	register char *cp;
{
	extern int outcol;

	if (cp == lastname && printed)
		return;
	lastname = cp;
	printed = 1;
#ifdef PXP
	if (outcol)
		putchar('\n');
#endif
	printf("%s:\n", cp);
	hadsome = 1;
}
