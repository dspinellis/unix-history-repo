/*
 * Ascii magic -- file types that we know based on keywords
 * that can appear anywhere in the file.
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

#include <stdio.h>
#include <ctype.h>
#include "file.h"
#include "names.h"

#ifndef	lint
static char *moduleid = 
	"@(#)$Header: ascmagic.c,v 1.5 87/09/16 14:44:45 ian Exp $";
#endif	/* lint */

char *ckfmsg = "write error on output";

			/* an optimisation over plain strcmp() */
#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

ascmagic(buf)
register char	*buf;
{
	register int i;
	char	*s, *strtok(), *token;
	register struct names *p;
	extern int nbytes;
	short has_escapes = 0;

	/* these are easy, do them first */

	/*
	 * for troff, look for . + letter + letter;
	 * this must be done to disambiguate tar archives' ./file
	 * and other trash from real troff input.
	 */
	if (*buf == '.' && 
		isascii(*(buf+1)) && isalnum(*(buf+1)) &&
		isascii(*(buf+2)) && isalnum(*(buf+2))){
		ckfputs("troff or preprocessor input text", stdout);
		return 1;
	}
	if ((*buf == 'c' || *buf == 'C') && 
	    isascii(*(buf + 1)) && isspace(*(buf + 1))) {
		ckfputs("fortran program text", stdout);
		return 1;
	}

	/* look for tokens from names.h - this is expensive! */
	s = buf;
	while ((token = strtok(s, " \t\n\r\f")) != NULL) {
		s = NULL;	/* make strtok() keep on tokin' */
		for (p = names; p < names + NNAMES; p++) {
			if (STREQ(p->name, token)) {
				ckfputs(types[p->type], stdout);
				return 1;
			}
		}
	}

	switch (is_tar(buf)) {
	case 1:
		ckfputs("tar archive", stdout);
		return 1;
	case 2:
		ckfputs("POSIX tar archive", stdout);
		return 1;
	}

	for (i = 0; i < nbytes; i++) {
		if (!isascii(*(buf+i)))
			return 0;	/* not all ascii */
		if (*(buf+i) == '\033')	/* ascii ESCAPE */
			has_escapes ++;
	}

	/* all else fails, but it is ascii... */
	if (has_escapes){
		ckfputs("ascii text (with escape sequences)", stdout);
		}
	else {
		ckfputs("ascii text", stdout);
		}
	return 1;
}


