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
static char sccsid[] = "@(#)vis.c	5.2 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <ctype.h>
#include <cencode.h>

#define	isoctal(c)	(((u_char)(c)) >= '0' && ((u_char)(c)) <= '7')

/*
 * vis - visually encode characters
 */
char *
vis(dst, c, flag, nextc)
	register char *dst, c;
	char nextc;
	register int flag;
{
	if (isascii(c) && isgraph(c) ||
	   ((flag & VIS_WHITE) == 0 && 
		(c == ' ' || c == '\n' || (flag & VIS_TAB) == 0 && c == '\t')) ||
	   ((flag & VIS_SAFE) && (c == '\b' || c == '\007' || c == '\r'))) {
		*dst++ = c;
		if (c == '\\' && (flag & VIS_NOSLASH) == 0)
			*dst++ = '\\';
		*dst = '\0';
		return (dst);
	}

	if ((flag & VIS_NOSLASH) == 0)
		*dst++ = '\\';
	if (flag & VIS_CSTYLE) {
		switch(c) {
		case '\n':
			*dst++ = 'n';
			goto done;
		case '\r':
			*dst++ = 'r';
			goto done;
		case '\b':
			*dst++ = 'b';
			goto done;
		case '\007':	/* waiting for ansi compiler */
			*dst++ = 'a';
			goto done;
		case '\v':
			*dst++ = 'v';
			goto done;
		case '\t':
			*dst++ = 't';
			goto done;
		case '\f':
			*dst++ = 'f';
			goto done;
		case ' ':
			*dst++ = 's';
			goto done;
		case '\0':
			*dst++ = '0';
			if ((flag & VIS_NEXTC) == 0 || isoctal(nextc)) {
				*dst++ = '0';
				*dst++ = '0';
			}
			goto done;
		}
	}
	if (((c & 0177) == ' ') || (flag & VIS_OCTAL)) {	
		*dst++ = ((u_char)c >> 6 & 07) + '0';
		*dst++ = ((u_char)c >> 3 & 07) + '0';
		*dst++ = ((u_char)c & 07) + '0';
		goto done;
	}
	if (c & 0200) {
		c &= 0177;
		*dst++ = 'M';
	}
	if (iscntrl(c)) {
		*dst++ = '^';
		if (c == 0177)
			*dst++ = '?';
		else
			*dst++ = c + '@';
	} else {
		*dst++ = '-';
		*dst++ = c;
	}
done:
	*dst = '\0';
	return (dst);
}


/*
 * decode driven by state machine
 */
#define	S_NORMAL	1	/* haven't seen escape char */
#define	S_START		2	/* start decoding special sequence */
#define	S_META		3	/* metachar started (M) */
#define	S_META1		4	/* metachar more, regular char (-) */
#define	S_CTRL		5	/* control char started (^) */
#define	S_OCTAL		6	/* octal number */

/*
 *
 */
cunvis(c, cp, flags)
	char c;
	char *cp;
{
	static int state = S_NORMAL;
	static u_char buildchar;
	static int octal;

	if (flags&UNVIS_END) {
		int ostate = state;
		state = S_NORMAL;
		if (ostate == S_OCTAL) {
			*cp = buildchar;
			return(UNVIS_OK);
		} else if (ostate == S_META1) {
			/* slightly forgiving, if not wrong */
			*cp = ' ' | 0200;
			return(UNVIS_OK);
		} else
			return(ostate == S_NORMAL ? UNVIS_NOCHAR : CDEC_SYNBAD);
	}

	switch (state) {
	case S_NORMAL:
		buildchar = 0;
		if (c == '\\') {
			state = S_START;
			return(UNVIS_NEEDMORE);
		} else if (flags&UNVIS_HAT && c == '^') {
			state = S_CTRL;
			return(UNVIS_NEEDMORE);
		} else {
			*cp = c;
			return(UNVIS_OK);
		}
		break;
	case S_START:
		state = S_NORMAL;
		if (c == '\\') {
			*cp = c;
			return(UNVIS_OK);
		}
		if (isoctal(c)) {
			buildchar = (c-'0');
			octal = 1;
			state = S_OCTAL;
			return(UNVIS_NEEDMORE);
		} 
		switch(c) {
		case 'M':
			buildchar |= 0200;
			state = S_META;
			return(UNVIS_NEEDMORE);
		case '^':
			state = S_CTRL;
			return(UNVIS_NEEDMORE);
		case 'n':
			*cp = '\n';
			return(UNVIS_OK);
		case 'r':
			*cp = '\r';
			return(UNVIS_OK);
		case 'b':
			*cp = '\b';
			return(UNVIS_OK);
		case 'a':
			*cp = '\007';
			return(UNVIS_OK);
		case 'v':
			*cp = '\v';
			return(UNVIS_OK);
		case 't':
			*cp = '\t';
			return(UNVIS_OK);
		case 'f':
			*cp = '\f';
			return(UNVIS_OK);
		case 's':			/* does anyone use this ? */
			*cp = ' ';
			return(UNVIS_OK);
		case 'E':
			*cp = '\033';
			return(UNVIS_OK);
		case '\n':
			return(UNVIS_NOCHAR);	/* hidden newline */
		}
		state = S_NORMAL;
		return(UNVIS_SYNBAD);
	case S_META:
		if (c == '-')
			state = S_META1;
		else if (c == '^')
			state = S_CTRL;
		else {
			state = S_NORMAL;
			return(UNVIS_SYNBAD);
		}
		return(UNVIS_NEEDMORE);
	case S_META1:
		state = S_NORMAL;
		*cp = c | buildchar;
		return(UNVIS_OK);
	case S_CTRL:
		if (c == '?')
			buildchar |= 0177;
		else
			buildchar |= c&037;
		state = S_NORMAL;
		*cp = buildchar;
		return(UNVIS_OK);
	case S_OCTAL:
		if (isoctal(c)) {
			buildchar = (buildchar<<3) + (c-'0');
			if (++octal == 3) {
				state = S_NORMAL;
				*cp = buildchar;
				return(UNVIS_OK);
			} else
				return(UNVIS_NEEDMORE);
		} else {
			state = S_NORMAL;
			*cp = buildchar;
			return(UNVIS_OKPUSH);
		}
	}
}

/*
 * strvis - visually encode characters from src into dst
 *
 *	If len >= 0, encodes exactly len chars from src (including NULL's).
 *	Otherwise, stops before first NULL in src.  In all cases, dst is 
 *	NULL terminated.
 *
 *	Dst must be 4 times the size of src to account for possible
 *	expansion.  The length of dst, not including the trailing NULL,
 *	is returned.
 */
strvis(dst, src, len, flag)
	register char *dst, *src;
	register int len;
{
	char *start = dst;

	for (;;) {
		if (len > 0) { 
			if (len-- == 0)
				break;
		} else if (!*src)
			break;
		dst = vis(dst, *src, flag | VIS_NEXTC, *(src+1));
		src++;
	}

	return (dst - start);
}
