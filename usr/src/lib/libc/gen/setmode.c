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

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setmode.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/stat.h>

static mode_t setbits, clrbits, Xbits;

mode_t
getmode(omode)
	mode_t omode;
{
	register mode_t newmode;

	newmode = omode & clrbits;
	newmode |= setbits;
	if (omode & (S_IFDIR|S_IXUSR|S_IXGRP|S_IXOTH))
		newmode |= Xbits;
	return(newmode);
}

#define	STANDARD_BITS	(S_ISUID|S_ISGID|S_IRWXU|S_IRWXG|S_IRWXO)
#define	CLR(a)		{ clrbits |= a; setbits &= ~(a); Xbits &= ~(a); }

setmode(p)
	register char *p;
{
	register int perm, who;
	register char op;
	mode_t mask;
	int permXbits;

	/*
	 * get a copy of the mask for the permissions that are mask
	 * relative.  Flip the bits, we want what's not set.
	 */
	(void)umask(mask = umask(0));
	mask = ~mask;

	setbits = clrbits = Xbits = 0;

	/*
	 * if an absolute number, get it and return; disallow non-octal
	 * digits or illegal bits.
	 */
	if (isdigit(*p)) {
		setbits = (mode_t)strtol(p, (char **)0, 8);
		clrbits = ~(STANDARD_BITS|S_ISTXT);
		Xbits = 0;
		while (*++p)
			if (*p < '0' || *p > '7')
				return(-1);
		if (setbits & clrbits)
			return(-1);
		return(0);
	}

	if (!*p)
		return(-1);
	/*
	 * accumulate bits to add and subtract from each clause of
	 * the symbolic mode
	 */
	for (;;) {
		for (who = 0;; ++p)
			switch (*p) {
			case 'a':
				who |= STANDARD_BITS;
				break;
			case 'u':
				who |= S_ISUID|S_IRWXU;
				break;
			case 'g':
				who |= S_ISGID|S_IRWXG;
				break;
			case 'o':
				who |= S_IRWXO;
				break;
			default:
				goto getop;
			}

getop:		if ((op = *p++) != '+' && op != '-' && op != '=')
			return(-1);

		who &= ~S_ISTXT;
		for (perm = 0;; ++p)
			switch (*p) {
			case 'r':
				perm |= S_IRUSR|S_IRGRP|S_IROTH;
				break;
			case 's':
				/* if only "other" bits ignore set-id */
				if (who & ~S_IRWXO)
					perm |= S_ISUID|S_ISGID;
				break;
			case 't':
				/* if only "other" bits ignore sticky */
				if (who & ~S_IRWXO) {
					who |= S_ISTXT;
					perm |= S_ISTXT;
				}
				break;
			case 'w':
				perm |= S_IWUSR|S_IWGRP|S_IWOTH;
				break;
			case 'X':
				permXbits = S_IXUSR|S_IXGRP|S_IXOTH;
				break;
			case 'x':
				perm |= S_IXUSR|S_IXGRP|S_IXOTH;
				break;
			default:
				goto apply;
			}

apply:		switch(op) {
		case '+':
			/*
			 * If no perm value, skip.  If no who value, use umask
			 * bits.  Don't bother clearing any bits, getmode
			 * clears first, then sets.
			 */
			if (perm || permXbits) {
				if (!who)
					who = mask;
				if (permXbits)
					Xbits |= who & permXbits;
				setbits |= who & perm;
			}
			break;
		case '-':
			/*
			 * If no perm value, skip.  If no who value, use
			 * owner, group, and other.
			 */
			if (perm) {
				if (!who)
					who = S_IRWXU|S_IRWXG|S_IRWXO;
				CLR(who & perm);
			}
			break;
		case '=':
			/*
			 * If no who value, clear all the bits.  Otherwise,
			 * clear the bits specified by who.
			 */
			if (!who) {
				CLR(STANDARD_BITS);
				who = mask;
			} else
				CLR(who);
			if (perm)
				setbits |= who & perm;
			break;
		}

		if (!*p)
			break;
		if (*p != ',')
			goto getop;
		++p;
	}
	clrbits = ~clrbits;
	return(0);
}
