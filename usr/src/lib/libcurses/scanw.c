/*
 * Copyright (c) 1981 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)scanw.c	5.6 (Berkeley) %G%";
#endif /* not lint */

/*
 * scanw and friends
 *
 */

# include	"curses.ext"
# include	"stdarg.h"

/*
 *	This routine implements a scanf on the standard screen.
 */
scanw(fmt, args)
char	*fmt;
int	args; {

	return _sscans(stdscr, fmt, &args);
}

/*
 *	This routine implements a scanf on the given window.
 */
wscanw(win, fmt, args)
WINDOW	*win;
char	*fmt;
int	args; {

	return _sscans(win, fmt, &args);
}

/*
 *	Internal routine to read from a string, and its data structure.
 */
struct strinfo {
	char	*addr;		/* address */
	int	len;		/* remaining bytes */
};

static int
_winread(cookie, buf, n)
char	*cookie, *buf;
reg int	n; {

	reg struct strinfo *s = (struct strinfo *)cookie;

	if (n > s->len)
		n = s->len;
	bcopy(s->addr, buf, n);
	s->len -= n;
	s->addr += n;
	return n;
}

/*
 *	This routine actually executes the scanf from the window.
 *	SHOULD IMPLEMENT VSSCANF
 */
_sscans(win, fmt)
WINDOW	*win;
char	*fmt; {

	va_list ap;
	int	ret;
	FILE	*f;
	struct	strinfo s;
	char	buf[100];

	if ((f = fropen((char *)&s, _winread)) == NULL)
		return ERR;
	if (wgetstr(win, buf) == ERR) {
		(void) fclose(f);
		return ERR;
	}
	s.addr = buf;
	s.len = strlen(buf);
	va_start(ap, fmt);
	ret = __svfscanf(f, fmt, ap);
	va_end(ap);
	(void) fclose(f);
	return ret;
}
