/*
 * file.h - definitions for file(1) program
 # @(#)$Header: file.h,v 1.4 87/09/18 10:56:09 ian Exp $
 *
 * Copyright (c) Ian F. Darwin, 1987.
 * Written by Ian F. Darwin.
 *
 * This software is not subject to any license of the American Telephone
 * and Telegraph Company or of the Regents of the University of California.
 *
 * Permission is granted to anyone to use this software for any purpose on
 * any computer system, and to alter it and redistribute it freely, subject
 * to the following restrictions:
 *
 * 1. The author is not responsible for the consequences of use of this
 *    software, no matter how awful, even if they arise from flaws in it.
 *
 * 2. The origin of this software must not be misrepresented, either by
 *    explicit claim or by omission.  Since few users ever read sources,
 *    credits must appear in the documentation.
 *
 * 3. Altered versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.  Since few users
 *    ever read sources, credits must appear in the documentation.
 *
 * 4. This notice may not be removed or altered.
 */

#define HOWMANY	1024		/* how much of the file to look at */
#define MAXMAGIS 250		/* max entries in /etc/magic */
#define MAXDESC	50		/* max leng of text description */
#define MAXstring 32		/* max leng of "string" types */
#define ckfputs(str,fil) {if (fputs(str,fil)==EOF) error(ckfmsg,"");}

struct magic {
	short contflag;		/* 1 if '>0' appears */
	long offset;		/* offset to magic number */
	char reln;		/* relation (0=eq, '>'=gt, etc) */
	char type;		/* int, short, long or string. */
	char vallen;		/* length of string value, if any */
#define 			BYTE	1
#define				SHORT	2
#define				LONG	4
#define				STRING	5
	union VALUETYPE {
		char b;
		short h;
		long l;
		char s[MAXstring];
	} value;		/* either number or string */
	char desc[MAXDESC];	/* description */
};

extern void error(), exit();
