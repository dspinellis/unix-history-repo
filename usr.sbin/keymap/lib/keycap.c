/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Parts Copyright 1992 by Holger Veit
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
 *
 * This code has been derived from the PRINTCAP source code that is part
 * of 386bsd which underlies the above Copyright. Changes have been made
 * by Holger Veit (-hv-)
 *
 * 29/07/92 -hv- copied and adapted from printcap common_source
 *
 * -hv-: Hey, after gettytab, termcap and printcap this is the fourth time, 
 * that this code has been adapted. Now, we should perhaps think about 
 * a universal capabilities library which unites all four and allows different 
 * data bases to be checked with a unified interface. This must be
 * documented of course in great detail. Any volunteers?
 *
 * "Suddenly, it was so quiet, one could almost hear a byte drop to
 * the floor..."
 *			"1000 methods to get rid of hackers"
 *				by H. Veit, to appear RSN :-)
 */

#ifndef lint
static char sccsid[] = "@(#)keycap.c	1.0 (Contributed to 386bsd) 7/29/92";
#endif /* not lint */

#include <ctype.h>
#include <stdio.h>
#include "pathnames.h"

#ifndef BUFSIZ
#define	BUFSIZ	1024
#endif
#define MAXHOP	32	/* max number of tc= indirections */

/*
 * keycap - routines for dealing with the keyboard capability data base
 *
 * BUG:		Should use a "last" pointer in tbuf, so that searching
 *		for capabilities alphabetically would not be a n**2/2
 *		process when large numbers of capabilities are given.
 * Note:	If we add a last pointer now we will screw up the
 *		tc capability. We really should compile termcap.
 *
 * Essentially all the work here is scanning and decoding escapes
 * in string capabilities.  We don't use stdio because the editor
 * doesn't, and because living w/o it is not hard.
 */

#define KEYCAP

#ifdef KEYCAP
#define tgetent	kgetent
#define tskip	kskip
#define tgetstr	kgetstr
#define tdecode kdecode
#define tgetnum	kgetnum
#define	tgetflag kgetflag
#define tdecode kdecode
#define tnchktc	knchktc
#define	tnamatch knamatch
#define V6
#endif

static	FILE *pfp = NULL;	/* keycap data base file pointer */
static	char *tbuf;
static	int hopcount;		/* detect infinite loops in keycap, init 0 */
static char	*tskip();
char	*tgetstr();
static char	*tdecode();
char	*getenv();

/*
 *
 * -hv- added to understand capability names longer than 2 chars
 *      which is very important if we do not want to crypt the key codes
 */
static char *nmatch(id,cstr)
	char *id,*cstr;
{
	register n = strlen(id);
	register char *c = cstr+n;

	if (strncmp(id,cstr,n)==0 &&
	    (*c==':' || *c=='|' || *c=='=' || *c=='#' || *c=='@')) {
	    	return c;
	}
	return 0;
}

#ifdef NOTUSED
/*
 * Similar to tgetent except it returns the next entry instead of
 * doing a lookup.
 */
getprent(bp)
	register char *bp;
{
	register int c, skip = 0;

	if (pfp == NULL && (pfp = fopen(_PATH_KEYCAP, "r")) == NULL)
		return(-1);
	tbuf = bp;
	for (;;) {
		switch (c = getc(pfp)) {
		case EOF:
			fclose(pfp);
			pfp = NULL;
			return(0);
		case '\n':
			if (bp == tbuf) {
				skip = 0;
				continue;
			}
			if (bp[-1] == '\\') {
				bp--;
				continue;
			}
			*bp = '\0';
			return(1);
		case '#':
			if (bp == tbuf)
				skip++;
		default:
			if (skip)
				continue;
			if (bp >= tbuf+BUFSIZ) {
				write(2, "Keycap entry too long\n", 23);
				*bp = '\0';
				return(1);
			}
			*bp++ = c;
		}
	}
}

endprent()
{
	if (pfp != NULL)
		fclose(pfp);
}
#endif

/*
 * Get an entry for keyboard name in buffer bp,
 * from the keycap file.  Parse is very rudimentary;
 * we just notice escaped newlines.
 *
 */
tgetent(bp, name)
	char *bp, *name;
{

	register char *cp;
	register int c;
	register int i = 0, cnt = 0;
	char ibuf[BUFSIZ];
	char *cp2;
	int tf;

	tbuf = bp;
	tf = 0;

	cp = getenv("KEYCAP");
	/*
	 * KEYCAP can have one of two things in it. It can be the
	 * name of a file to use instead of /usr/share/misc/keycap. In this
	 * case it better start with a "/". Or it can be an entry to
	 * use so we don't have to read the file. In this case it
	 * has to already have the newlines crunched out.
	 */
	if (cp && *cp) {
		if (*cp!='/') {
			cp2 = getenv("KEYBOARD");
			if (cp2==(char *) 0 || strcmp(name,cp2)==0) {
				strcpy(bp,cp);
				return(tnchktc());
			} else {
				tf = open(_PATH_KEYCAP, 0);
			}
		} else
			tf = open(cp, 0);
	}
	if (tf==0)
		tf = open(_PATH_KEYCAP, 0);

	if (tf < 0)
		return (-1);
	for (;;) {
		cp = bp;
		for (;;) {
			if (i == cnt) {
				cnt = read(tf, ibuf, BUFSIZ);
				if (cnt <= 0) {
					close(tf);
					return (0);
				}
				i = 0;
			}
			c = ibuf[i++];
			if (c == '\n') {
				if (cp > bp && cp[-1] == '\\'){
					cp--;
					continue;
				}
				break;
			}
			if (cp >= bp+BUFSIZ) {
				write(2,"Keycap entry too long\n", 22);
				break;
			} else
				*cp++ = c;
		}
		*cp = 0;

		/*
		 * The real work for the match.
		 */
		if (tnamatch(name)) {
			close(tf);
			return(tnchktc());
		}
	}
}

/*
 * tnchktc: check the last entry, see if it's tc=xxx. If so,
 * recursively find xxx and append that entry (minus the names)
 * to take the place of the tc=xxx entry. This allows keycap
 * entries to say "like an HP2621 but doesn't turn on the labels".
 * Note that this works because of the left to right scan.
 */
tnchktc()
{
	register char *p, *q;
	char tcname[16];	/* name of similar keyboard */
	char tcbuf[BUFSIZ];
	char *holdtbuf = tbuf;
	int l;

	p = tbuf + strlen(tbuf) - 2;	/* before the last colon */
	while (*--p != ':')
		if (p<tbuf) {
			write(2, "Bad keycap entry\n", 17);
			return (0);
		}
	p++;
	/* p now points to beginning of last field */
	if (p[0] != 't' || p[1] != 'c')
		return(1);
	strcpy(tcname,p+3);
	q = tcname;
	while (q && *q != ':')
		q++;
	*q = 0;
	if (++hopcount > MAXHOP) {
		write(2, "Infinite tc= loop\n", 18);
		return (0);
	}
	if (tgetent(tcbuf, tcname) != 1)
		return(0);
	for (q=tcbuf; *q != ':'; q++)
		;
	l = p - holdtbuf + strlen(q);
	if (l > BUFSIZ) {
		write(2, "Keycap entry too long\n", 22);
		q[BUFSIZ - (p-tbuf)] = 0;
	}
	strcpy(p, q+1);
	tbuf = holdtbuf;
	return(1);
}

/*
 * Tnamatch deals with name matching.  The first field of the keycap
 * entry is a sequence of names separated by |'s, so we compare
 * against each such name.  The normal : terminator after the last
 * name (before the first field) stops us.
 */
tnamatch(np)
	char *np;
{
	register char *Np, *Bp;

	Bp = tbuf;
	if (*Bp == '#' || *Bp == 0)
		return(0);
	for (;;) {
		for (Np = np; *Np && *Bp == *Np; Bp++, Np++)
			continue;
		if (*Np == 0 && (*Bp == '|' || *Bp == ':' || *Bp == 0))
			return (1);
		while (*Bp && *Bp != ':' && *Bp != '|')
			Bp++;
		if (*Bp == 0 || *Bp == ':')
			return (0);
		Bp++;
	}
}

/*
 * Skip to the next field.  Notice that this is very dumb, not
 * knowing about \: escapes or any such.  If necessary, :'s can be put
 * into the keycap file in octal.
 */
static char *
tskip(bp)
	register char *bp;
{

	while (*bp && *bp != ':')
		bp++;
	if (*bp == ':')
		bp++;
	return (bp);
}

/*
 * Return the (numeric) option id.
 * Numeric options look like
 *	li#80
 * i.e. the option string is separated from the numeric value by
 * a # character.  If the option is not found we return -1.
 * Note that we handle octal numbers beginning with 0.
 */
tgetnum(id)
	char *id;
{
	register int i, base;
	register char *bp = tbuf,*xp;

	for (;;) {
		bp = tskip(bp);
		if (*bp == 0)
			return (-1);
		if ((xp=nmatch(id,bp)) == 0) 
			continue;
		bp = xp;	/* we have an entry */
		if (*bp == '@')
			return(-1);
		if (*bp != '#')
			continue;
		bp++;
		base = 10;
		if (*bp == '0')
			base = 8;
		i = 0;
		while (isdigit(*bp))
			i *= base, i += *bp++ - '0';
		return (i);
	}
}

/*
 * Handle a flag option.
 * Flag options are given "naked", i.e. followed by a : or the end
 * of the buffer.  Return 1 if we find the option, or 0 if it is
 * not given.
 */
tgetflag(id)
	char *id;
{
	register char *bp = tbuf,*xp;

	for (;;) {
		bp = tskip(bp);
		if (!*bp)
			return (0);
		if ((xp=nmatch(id,bp)) != 0) {
			bp = xp;
			if (!*bp || *bp == ':')
				return (1);
			else if (*bp == '@')
				return(0);
		}
	}
	/*NOTREACHED*/
}

/*
 * Get a string valued option.
 * These are given as
 *	cl=^Z
 * Much decoding is done on the strings, and the strings are
 * placed in area, which is a ref parameter which is updated.
 * No checking on area overflow.
 */
char *
tgetstr(id, area)
	char *id, **area;
{
	register char *bp = tbuf,*xp;

	for (;;) {
		bp = tskip(bp);
		if (!*bp)
			return (0);
		if ((xp = nmatch(id,bp)) == 0)
			continue;
		bp = xp;
		if (*bp == '@')
			return(0);
		if (*bp != '=')
			continue;
		bp++;
		return (tdecode(bp, area));
	}
}

/*
 * Tdecode does the grung work to decode the
 * string capability escapes.
 */
static char *
tdecode(str, area)
	register char *str;
	char **area;
{
	register char *cp;
	register int c;
	register char *dp;
	int i;

	cp = *area;
	while ((c = *str++) && c != ':') {
		switch (c) {

		case '^':
			c = *str++ & 037;
			break;

		case '\\':
			dp = "E\033^^\\\\::n\nr\rt\tb\bf\f";
			c = *str++;
nextc:
			if (*dp++ == c) {
				c = *dp++;
				break;
			}
			dp++;
			if (*dp)
				goto nextc;
			if (isdigit(c)) {
				c -= '0', i = 2;
				do
					c <<= 3, c |= *str++ - '0';
				while (--i && isdigit(*str));
			}
			break;
		}
		*cp++ = c;
	}
	*cp++ = 0;
	str = *area;
	*area = cp;
	return (str);
}
