/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mktab.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * mktab.c
 *
 * Function:	Build nroff terminal tables in a compiler-independent way.
 * Usage:	cc -Itroff mktab.c tabnnn.c -o mktab; mktab > tabnnn
 * Date:	Sat Feb 25 00:10:06 MST 1989
 * Author:	Donn Seeley
 * Remarks:
 *	Traditional nroff terminal table construction works by compiling
 *	a C file into a binary that is read directly into nroff as it runs.
 *	If your C compiler or your relocation format differ from what nroff
 *	expects, you lose.  This program, when linked with a terminal table
 *	object file, fakes up an 'object' file that looks enough like the
 *	traditional one to fool nroff.
 */

#include <stdio.h>
#include "tw.h"

main()
{
	static struct fake_exec {
		int bogus[8];	/* bogus[2] == a_data */
	} fe;
	register int *bip;
	register char **tip;
	register int offset = sizeof t;
	int buf[sizeof t / sizeof (int)];
	char *malloc();
	int twbase = (int *) &t.twinit - &t.bset;

	/*
	 * Copy the integers at the start of the table.
	 */
	bcopy((char *) &t, (char *) buf, twbase * sizeof (int));

	/*
	 * Replace the character pointers in the copy with integer offsets.
	 * This duplicates the effect of relocation offsets.
	 * Take care to count the possibly null control bytes in the codetab
	 *	section.
	 */
	for (bip = &buf[twbase], tip = &t.twinit;
	     tip < &t.codetab[0];
	     ++bip, ++tip)
		if (*tip) {
			*bip = offset;
			offset += strlen(*tip) + 1;
		} else
			*bip = 0;
	for (; tip < &t.zzz; ++bip, ++tip)
		if (*tip) {
			*bip = offset;
			offset += strlen(*tip + 1) + 2;
		} else
			*bip = 0;

	/*
	 * Patch in a fake data segment size field.
	 */
	fe.bogus[2] = offset;

	/*
	 * Dump the header and the table.
	 */
	(void) fwrite((char *) &fe, sizeof fe, 1, stdout);
	(void) fwrite((char *) buf, sizeof t, 1, stdout);

	/*
	 * Dump the strings.
	 */
	for (tip = &t.twinit; tip < &t.codetab[0]; ++tip)
		if (*tip) {
			fputs(*tip, stdout);
			putchar('\0');
		}
	for (tip = &t.codetab[0]; tip < &t.zzz; ++tip)
		if (*tip) {
			putchar(**tip);
			fputs(*tip + 1, stdout);
			putchar('\0');
		}

	return 0;
}
