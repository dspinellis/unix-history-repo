/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)setmode.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>

#define	setbits	set[0]
#define	clrbits	set[1]
#define	Xbits	set[2]

mode_t
getmode(set, omode)
	mode_t *set, omode;
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

mode_t *
setmode(p)
	register char *p;
{
	extern int errno;
	register int perm, who;
	register char op;
	mode_t mask, *set;
	int permXbits;
	char *malloc();

	/*
	 * get a copy of the mask for the permissions that are mask
	 * relative.  Flip the bits, we want what's not set.
	 */
	(void)umask(mask = umask(0));
	mask = ~mask;

	if (!(set = (mode_t *)malloc((u_int)(sizeof(mode_t) * 3)))) {
		errno = ENOMEM;
		return(NULL);
	}

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
				return(NULL);
		if (setbits & clrbits)
			return(NULL);
		return(set);
	}

	if (!*p)
		return(NULL);
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
			return(NULL);

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
	return(set);
}
