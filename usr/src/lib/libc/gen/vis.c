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
static char sccsid[] = "@(#)vis.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <ctype.h>
#include <cencode.h>

#define	iswhite(c)	((c)==' '||(c)=='\t'||(c)=='\n')
#define	isoctal(c)	(((u_char)(c)) >= '0' && ((u_char)(c)) <= '7')

/*
 * cencode
 */

/*VARARGS2*/
char *
cencode(sc, flags, rachar)
	char sc, rachar;
{
	static char buff[5];
	register char *s = buff;
	register u_char c = sc;

	if (isgraph(c) || (!(flags&CENC_WHITE) && iswhite(c))) {
		if (c == '\\')
			*s++ = '\\';
		*s++ = c;
		goto done;
	}
	*s++ = '\\';
	if (flags&CENC_CSTYLE) {
		switch(c) {
		case '\n':
			*s++ = 'n';
			goto done;
		case '\r':
			*s++ = 'r';
			goto done;
		case '\b':
			*s++ = 'b';
			goto done;
		case '\007':
			*s++ = 'a';
			goto done;
		case '\v':
			*s++ = 'v';
			goto done;
		case '\t':
			*s++ = 't';
			goto done;
		case '\f':
			*s++ = 'f';
			goto done;
		case ' ':
			*s++ = 's';
			goto done;
		case '\0':
			*s++ = '0';
			if (!(flags&CENC_RACHAR) || isoctal(rachar)) {
				*s++ = '0';
				*s++ = '0';
			}
			goto done;
		}
	}
	if ((flags&CENC_GRAPH) && (c&0177) != ' ') {
		if (c & 0200) {
			c &= 0177;
			*s++ = 'M';
		}
		if (iscntrl(c)) {
			*s++ = '^';
			if (c == 0177)
				*s++ = '?';
			else
				*s++ = c + '@';
		} else {
			*s++ = '-';
			*s++ = c;
		}
		goto done;
	}
	if (flags&CENC_OCTAL) {
		if (flags&CENC_RACHAR && !isoctal(rachar))
			(void)sprintf(s, "%o", (int)c);
		else
			(void)sprintf(s, "%03o", (int)c);
		while (*s++)
			;
		goto done;
	}
	/*
	 * Couldn't encode.
	 */
	s--;
	*s = c;
done:
	*s = '\0';
	return(buff);
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
cdecode(c, cp, flags)
	char c;
	char *cp;
{
	static int state = S_NORMAL;
	u_char buildchar;
	int octal;

	if (flags&CDEC_END) {
		int ostate = state;
		state = S_NORMAL;
		if (ostate == S_OCTAL) {
			*cp = buildchar;
			return(CDEC_OK);
		} else if (ostate == S_META1) {
			/* slightly forgiving, if not wrong */
			*cp = ' ' | 0200;
			return(CDEC_OK);
		} else
			return(ostate == S_NORMAL ? CDEC_NOCHAR : CDEC_SYNBAD);
	}

	switch (state) {
	case S_NORMAL:
		buildchar = 0;
		if (c == '\\') {
			state = S_START;
			return(CDEC_NEEDMORE);
		} else if (flags&CDEC_HAT && c == '^') {
			state = S_CTRL;
			return(CDEC_NEEDMORE);
		} else {
			*cp = c;
			return(CDEC_OK);
		}
		break;
	case S_START:
		state = S_NORMAL;
		if (c == '\\') {
			*cp = c;
			return(CDEC_OK);
		}
		if (isoctal(c)) {
			buildchar = (c-'0');
			octal = 1;
			state = S_OCTAL;
			return(CDEC_NEEDMORE);
		} 
		switch(c) {
		case 'M':
			buildchar |= 0200;
			state = S_META;
			return(CDEC_NEEDMORE);
		case '^':
			state = S_CTRL;
			return(CDEC_NEEDMORE);
		case 'n':
			*cp = '\n';
			return(CDEC_OK);
		case 'r':
			*cp = '\r';
			return(CDEC_OK);
		case 'b':
			*cp = '\b';
			return(CDEC_OK);
		case 'a':
			*cp = '\007';
			return(CDEC_OK);
		case 'v':
			*cp = '\v';
			return(CDEC_OK);
		case 't':
			*cp = '\t';
			return(CDEC_OK);
		case 'f':
			*cp = '\f';
			return(CDEC_OK);
		case 's':			/* does anyone use this ? */
			*cp = ' ';
			return(CDEC_OK);
		case 'E':
			*cp = '\033';
			return(CDEC_OK);
		case '\n':
			return(CDEC_NOCHAR);	/* hidden newline */
		}
		state = S_NORMAL;
		return(CDEC_SYNBAD);
	case S_META:
		if (c == '-')
			state = S_META1;
		else if (c == '^')
			state = S_CTRL;
		else {
			state = S_NORMAL;
			return(CDEC_SYNBAD);
		}
		return(CDEC_NEEDMORE);
	case S_META1:
		state = S_NORMAL;
		*cp = c | buildchar;
		return(CDEC_OK);
	case S_CTRL:
		if (c == '?')
			buildchar |= 0177;
		else
			buildchar |= c&037;
		state = S_NORMAL;
		*cp = buildchar;
		return(CDEC_OK);
	case S_OCTAL:
		if (isoctal(c)) {
			buildchar = (buildchar<<3) + (c-'0');
			if (++octal == 3) {
				state = S_NORMAL;
				*cp = buildchar;
				return(CDEC_OK);
			} else
				return(CDEC_NEEDMORE);
		} else {
			state = S_NORMAL;
			*cp = buildchar;
			return(CDEC_OKPUSH);
		}
	}
}
