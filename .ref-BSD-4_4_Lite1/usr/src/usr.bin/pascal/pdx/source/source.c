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
static char sccsid[] = "@(#)source.c	8.1 (Berkeley) 6/6/93";
#endif /* not lint */

/*
 * Source file management.
 */

#include "defs.h"
#include "source.h"

/*
 * Seektab is the data structure used for indexing source
 * seek addresses by line number.
 *
 * The constraints are:
 *
 *  we want an array so indexing is fast and easy
 *  we don't want to waste space for small files
 *  we don't want an upper bound on # of lines in a file
 *  we don't know how many lines there are
 *
 * The solution is a sparse array. We have NSLOTS pointers to
 * arrays of NLINESPERSLOT addresses.  To find the source address of
 * a particular line we find the slot, allocate space if necessary,
 * and then find its location within the pointed to array.
 *
 * As a result, there is a limit of NSLOTS*NLINESPERSLOT lines per file
 * but this is plenty high and still fairly inexpensive.
 *
 * This implementation maintains only one source file at any given
 * so as to avoid consuming too much memory.  In an environment where
 * memory is less constrained and one expects to be changing between
 * files often enough, it would be reasonable to have multiple seek tables.
 */

typedef int SEEKADDR;

#define NSLOTS 40
#define NLINESPERSLOT 500

#define slotno(line)	((line)/NLINESPERSLOT)
#define index(line)	((line)%NLINESPERSLOT)
#define slot_alloc()	alloc(NLINESPERSLOT, SEEKADDR)
#define srcaddr(line)	seektab[(line)/NLINESPERSLOT][(line)%NLINESPERSLOT]

LOCAL SEEKADDR *seektab[NSLOTS];

LOCAL FILE *srcfp;

/*
 * check to make sure a source line number is valid
 */

chkline(linenum)
register LINENO linenum;
{
    if (linenum < 1) {
	error("line number must be positive");
    }
    if (linenum > lastlinenum) {
	error("not that many lines");
    }
}

/*
 * print out the given lines from the source
 */

printlines(l1, l2)
LINENO l1, l2;
{
    register int c;
    register LINENO i;
    register FILE *fp;

    chkline(l1);
    chkline(l2);
    if (l2 < l1) {
	error("second line number less than first");
    }
    fp = srcfp;
    fseek(fp, (long) srcaddr(l1), 0);
    for (i = l1; i <= l2; i++) {
	printf("%5d   ", i);
	while ((c = getc(fp)) != '\n') {
	    putchar(c);
	}
	putchar('\n');
    }
}

/*
 * read the source file getting seek pointers for each line
 */

skimsource(file)
char *file;
{
    register int c;
    register SEEKADDR count;
    register FILE *fp;
    register LINENO linenum;
    register SEEKADDR lastaddr;
    register int slot;

    if (file == NIL || file == cursource) {
	return;
    }
    if ((fp = fopen(file, "r")) == NULL) {
	panic("can't open \"%s\"", file);
    }
    if (cursource != NIL) {
	free_seektab();
    }
    cursource = file;
    linenum = 0, count = 0, lastaddr = 0;
    while ((c = getc(fp)) != EOF) {
	count++;
	if (c == '\n') {
	    slot = slotno(++linenum);
	    if (slot >= NSLOTS) {
		panic("skimsource: too many lines");
	    }
	    if (seektab[slot] == NIL) {
		seektab[slot] = slot_alloc();
	    }
	    seektab[slot][index(linenum)] = lastaddr;
	    lastaddr = count;
	}
    }
    lastlinenum = linenum;
    srcfp = fp;
}

/*
 * Erase information and release space in the current seektab.
 * This is in preparation for reading in seek pointers for a
 * new file.  It is possible that seek pointers for all files
 * should be kept around, but the current concern is space.
 */

LOCAL free_seektab()
{
    register int slot;

    for (slot = 0; slot < NSLOTS; slot++) {
	if (seektab[slot] != NIL) {
	    free(seektab[slot]);
	    seektab[slot] = NIL;
	}
    }
}
