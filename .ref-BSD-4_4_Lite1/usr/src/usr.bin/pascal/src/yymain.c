/*-
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#ifndef lint
static char sccsid[] = "@(#)yymain.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

#include "whoami.h"
#include "0.h"
#include "tree_ty.h"	/* must be included for yy.h */
#include "yy.h"
#include <a.out.h>
#include "objfmt.h"
#include <signal.h>
#include "config.h"

/*
 * Yymain initializes each of the utility
 * clusters and then starts the processing
 * by calling yyparse.
 */
yymain()
{

#ifdef OBJ
/*
 * initialize symbol table temp files
 */
	startnlfile();
#endif
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
#   ifdef OBJ
	magic();
#   endif OBJ
#endif
	line = 1;
	errpfx = 'E';
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
#   ifdef OBJ

	/*
	 * save outermost block of namelist
	 */
	savenl(NLNIL);

	magic2();
#   endif OBJ
#   ifdef DEBUG
	dumpnl(NLNIL);
#   endif
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
		(void) signal(SIGHUP, SIG_IGN);
		(void) signal(SIGINT, SIG_IGN);
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

	(void) close(1);
	if (creat(firstname, 0644) != 1) {
		perror(firstname);
		pexit(ERRS);
	}
	(void) lseek(fout[0], 0l, 0);
	while ((c = read(fout[0], &fout[3], 512)) > 0) {
		if (write(1, &fout[3], c) != c) {
			perror(firstname);
			pexit(ERRS);
		}
	}
}
#endif


#ifdef PI
#ifdef OBJ

static
struct exec magichdr;

magic()
{

	short		buf[HEADER_BYTES / sizeof ( short )];
	unsigned	*ubuf = (unsigned *) buf;
	register int	hf, i;

	hf = open(px_header,0);
	if (hf >= 0 && read(hf, (char *) buf, HEADER_BYTES) > sizeof(struct exec)) {
		magichdr.a_magic = ubuf[0];
		magichdr.a_text = ubuf[1];
		magichdr.a_data = ubuf[2];
		magichdr.a_bss = ubuf[3];
		magichdr.a_syms = ubuf[4];
		magichdr.a_entry = ubuf[5];
		magichdr.a_trsize = ubuf[6];
		magichdr.a_drsize = ubuf[7];
		for (i = 0; i < HEADER_BYTES / sizeof ( short ); i++)
			word(buf[i]);
	}
	(void) close(hf);
}
#endif OBJ

#ifdef OBJ
magic2()
{
	struct pxhdr pxhd;
	extern long lseek();

	if  (magichdr.a_magic != 0407)
		panic ( "magic2" );
	pflush();
	magichdr.a_data = ( unsigned ) lc - magichdr.a_text;
	magichdr.a_data -= sizeof (struct exec);
	pxhd.objsize = ( ( unsigned ) lc) - HEADER_BYTES;
	pxhd.symtabsize = nlhdrsize();
	magichdr.a_data += pxhd.symtabsize;
	(void) time((long *) (&pxhd.maketime));
	pxhd.magicnum = MAGICNUM;
	(void) lseek(ofil, 0l, 0);
	write(ofil, (char *) (&magichdr), sizeof(struct exec));
	(void) lseek(ofil, ( long ) ( HEADER_BYTES - sizeof ( pxhd ) ) , 0);
	write(ofil, (char *) (&pxhd), sizeof (pxhd));
}
#endif OBJ
#endif

#ifdef PXP
writef(i, cp)
{

	write(i, cp, strlen(cp));
}
#endif
