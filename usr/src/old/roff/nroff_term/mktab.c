/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
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
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mktab.c	5.2 (Berkeley) 4/18/91";
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
