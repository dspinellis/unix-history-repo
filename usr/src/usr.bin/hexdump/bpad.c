/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)bpad.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include "hexdump.h"

bpad(fu, pr, bp)
	FU *fu;
	PR *pr;
	u_char *bp;
{
	static char *spec1 = "-0+ #";
	static char *spec2 = ".0123456789";
	enum { NONE, ZEROPAD, NOTSET } fw;
	register char *p1, *p2;
	FU *s_fu;
	PR *cur_pr, *s_pr, *txt_pr;
	int newlinecnt;
	char *txt_p1;

	/*
	 * the format string has no more data to print out.  Go through
	 * the rest of the format string, and, for all non-%s conversions,
	 * replace with %s and remove any conversion flags that don't
	 * apply to %s.  This should replace any output with an equivalent
	 * number of spaces.  At the same time, keep track of any text
	 * that must be printed, i.e. constant text from the user.  Use
	 * this to trim any trailing spaces and tabs, possibly interspersed
	 * by newlines, leaving only the newlines.
	 */
	s_fu = fu;
	cur_pr = pr;
	s_pr = pr = pr->nextpr;
	txt_pr = NULL;
	newlinecnt = 0;
	for (;;) {
		if (fu->flags&F_IGNORE)
			continue;
		for (; pr; pr = pr->nextpr) {
			if (pr->flags != F_TEXT)
				*pr->cchar = 's';
			for (p1 = p2 = pr->fmt; *p1; *p2++ = *p1++)
				switch (*p1) {
				/* remove conversion flags %s can't handle. */
				case '%':
					*p2++ = *p1;
					while (index(spec1, *++p1))
						if (*p1 == '-')
							*p2++ = '-';
					while (index(spec2, *p2++ = *p1++));
					break;
				/* ignore spaces and tabs */
				case ' ': case '\t':
					break;
				/* count newlines */
				case '\n':
					++newlinecnt;
					break;
				/*
				 * anything else has to be printed; keep track
				 * of it, and reset newline counter.
				 */
				default:
					txt_pr = pr;
					txt_p1 = p1 + 1;
					newlinecnt = 0;
					break;
				}
			*p2 = '\0';
		}
		if (!(fu = fu->nextfu))
			break;
		pr = fu->nextpr;
	}
	if (txt_pr) {
		/*
		 * if anything else has to be printed, print out the current
		 * item then the rest of the formats up to the one which has
		 * to be displayed.
		 */
		print(cur_pr, bp);
		*txt_p1 = '\0';
		for (fu = s_fu, pr = s_pr;;) {
			if (fu->flags&F_IGNORE)
				continue;
			for (; pr; pr = pr->nextpr) {
				(void)printf(pr->fmt, "");
				if (pr == txt_pr)
					goto done;
			}
			if (!(fu = fu->nextfu))
				break;
			pr = fu->nextpr;
		}
	} else {
		/*
		 * nothing else has to be printed -- serious special case.
		 * If current item is left justified and zero-pad flag not
		 * set, printf will space pad it.
		 */
		for (p1 = p2 = cur_pr->fmt; *p1; *p2++ = *p1++)
			if (*p1 == '%') {
				*p2++ = *p1++;
				for (fw = NOTSET; *p1; ++p1) {
					while (index(spec1 + 2, *p1))
						*p2++ = *p1++;
					if (*p1 == '-') {
						if (fw == NOTSET)
							fw = NONE;
						else if (fw == ZEROPAD)
							*p2++ = '-';
						continue;
					}
					if (*p1 == '0') {
						if (fw == NONE)
							*p2++ = '-';
						fw = ZEROPAD;
						*p2++ = '0';
						continue;
					}
					break;
				}
				if (fw == NONE)
					while (isdigit(*++p1));
				while (index(spec2, *p1))
					*p2++ = *p1++;
			}
		*p2 = '\0';
	}
	/* print out any trailing newlines */
done:	while(newlinecnt--)
		(void)printf("\n");
}
