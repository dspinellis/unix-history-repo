/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Sze-Tyan Wang.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)read.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "extern.h"

/*
 * bytes -- read bytes to an offset from the end and display.
 *
 * This is the function that reads to a byte offset from the end of the input,
 * storing the data in a wrap-around buffer which is then displayed.  If the
 * rflag is set, the data is displayed in lines in reverse order, and this
 * routine has the usual nastiness of trying to find the newlines.  Otherwise,
 * it is displayed from the character closest to the beginning of the input to
 * the end.
 */
void
bytes(fp, off)
	register FILE *fp;
	off_t off;
{
	register int ch, len, tlen;
	register char *ep, *p, *t;
	int wrap;
	char *sp;

	if ((sp = p = malloc(off)) == NULL)
		err(1, "%s", strerror(errno));

	for (wrap = 0, ep = p + off; (ch = getc(fp)) != EOF;) {
		*p = ch;
		if (++p == ep) {
			wrap = 1;
			p = sp;
		}
	}
	if (ferror(fp)) {
		ierr();
		return;
	}

	if (rflag) {
		for (t = p - 1, len = 0; t >= sp; --t, ++len)
			if (*t == '\n' && len) {
				WR(t + 1, len);
				len = 0;
		}
		if (wrap) {
			tlen = len;
			for (t = ep - 1, len = 0; t >= p; --t, ++len)
				if (*t == '\n') {
					if (len) {
						WR(t + 1, len);
						len = 0;
					}
					if (tlen) {
						WR(sp, tlen);
						tlen = 0;
					}
				}
			if (len)
				WR(t + 1, len);
			if (tlen)
				WR(sp, tlen);
		}
	} else {
		if (wrap && (len = ep - p))
			WR(p, len);
		if (len = p - sp)
			WR(sp, len);
	}
}

/*
 * lines -- read lines to an offset from the end and display.
 *
 * This is the function that reads to a line offset from the end of the input,
 * storing the data in an array of buffers which is then displayed.  If the
 * rflag is set, the data is displayed in lines in reverse order, and this
 * routine has the usual nastiness of trying to find the newlines.  Otherwise,
 * it is displayed from the line closest to the beginning of the input to
 * the end.
 */
void
lines(fp, off)
	register FILE *fp;
	off_t off;
{
	struct {
		u_int blen;
		u_int len;
		char *l;
	} *lines;
	register int ch;
	register char *p;
	int blen, cnt, recno, wrap;
	char *sp;

	if ((lines = malloc(off * sizeof(*lines))) == NULL)
		err(1, "%s", strerror(errno));

	sp = NULL;
	blen = cnt = recno = wrap = 0;

	while ((ch = getc(fp)) != EOF) {
		if (++cnt > blen) {
			if ((sp = realloc(sp, blen += 1024)) == NULL)
				err(1, "%s", strerror(errno));
			p = sp + cnt - 1;
		}
		*p++ = ch;
		if (ch == '\n') {
			if (lines[recno].blen < cnt) {
				lines[recno].blen = cnt + 256;
				if ((lines[recno].l = realloc(lines[recno].l,
				    lines[recno].blen)) == NULL)
					err(1, "%s", strerror(errno));
			}
			bcopy(sp, lines[recno].l, lines[recno].len = cnt);
			cnt = 0;
			p = sp;
			if (++recno == off) {
				wrap = 1;
				recno = 0;
			}
		}
	}
	if (ferror(fp)) {
		ierr();
		return;
	}
	if (cnt) {
		lines[recno].l = sp;
		lines[recno].len = cnt;
		if (++recno == off) {
			wrap = 1;
			recno = 0;
		}
	}

	if (rflag) {
		for (cnt = recno - 1; cnt >= 0; --cnt)
			WR(lines[cnt].l, lines[cnt].len);
		if (wrap)
			for (cnt = off - 1; cnt >= recno; --cnt)
				WR(lines[cnt].l, lines[cnt].len);
	} else {
		if (wrap)
			for (cnt = recno; cnt < off; ++cnt)
				WR(lines[cnt].l, lines[cnt].len);
		for (cnt = 0; cnt < recno; ++cnt)
			WR(lines[cnt].l, lines[cnt].len);
	}
}
