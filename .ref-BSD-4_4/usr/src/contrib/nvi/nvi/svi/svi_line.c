/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
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
 */

#ifndef lint
static char sccsid[] = "@(#)svi_line.c	8.1 (Berkeley) 6/9/93";
#endif /* not lint */

#include <sys/types.h>

#include <curses.h>
#include <string.h>

#include "vi.h"
#include "svi_screen.h"

#if DEBUG && 0
#define	TABCH	'-'
#define	TABSTR	"--------------------"
#else
#define	TABSTR	"                    "
#define	TABCH	' '
#endif

/*
 * svi_line --
 *	Update one line on the screen.  One nasty little side effect is
 *	that it returns the screen position for the current character.
 *	Not pretty, but this is the only routine that really knows what's
 *	out there.
 * XXX
 * 	Should cache offset into last line for last screen -- this should
 *	speed up folded lines a lot.  The problem is that if a tab is broken
 *	across the line, it's going to be tricky.  Also, are there really
 *	enough folded lines that this is worthwhile?
 */
int
svi_line(sp, ep, smp, p, len, yp, xp)
	SCR *sp;
	EXF *ep;
	SMAP *smp;
	register char *p;
	size_t len, *xp, *yp;
{
	CHNAME *cname;
	recno_t lno;
	size_t chlen, cols_per_screen, cno_cnt, count_cols, last_count_cols;
	size_t offset_in_char, skip_screens;
	int ch, listset, partial, reverse_video;
	char nbuf[10];

#if DEBUG && 0
	TRACE(sp, "svi_line: row %u: line: %u off: %u\n",
	    smp - HMAP, smp->lno, smp->off);
#endif
	/* Move to the line. */
	MOVE(sp, smp - HMAP, 0);

	/* Get the character map. */
	cname = sp->cname;

	/*
	 * Special case if we're printing the info/mode line.  Skip printing
	 * the leading number, as well as other minor setup.  If painting the
	 * line between two screens, it's always in reverse video.  The only
	 * time this code paints the mode line is when the user is entering
	 * text for a ":" command, so we can put the code here instead of
	 * dealing with the empty line logic below.  This is a kludge, but it's
	 * pretty much confined to this module.
	 *
	 * Set the number of screens to skip until a character is displayed.
	 * Left-right screens are special, because we don't bother building
	 * a buffer to be skipped over.
	 */
	reverse_video = 0;
	if (ISINFOLINE(sp, smp)) {
		if (sp->child != NULL) {
			reverse_video = 1;
			standout();
		}
		listset = 0;
		if (O_ISSET(sp, O_LEFTRIGHT))
			skip_screens = 0;
		else
			skip_screens = smp->off - 1;
	} else {
		listset = O_ISSET(sp, O_LIST);
		skip_screens = smp->off - 1;
	}

	/*
	 * If O_NUMBER is set and this is the first screen of a folding
	 * line or any left-right line, display the line number.  Set
	 * the number of columns for this screen.
	 */
	if (O_ISSET(sp, O_NUMBER) && !ISINFOLINE(sp, smp)) {
		cols_per_screen = sp->cols - O_NUMBER_LENGTH;
		if (skip_screens == 0) {
			(void)snprintf(nbuf,
			    sizeof(nbuf), O_NUMBER_FMT, smp->lno);
			ADDSTR(nbuf);
		}
	} else
		cols_per_screen = sp->cols;

	/*
	 * Get a copy of the line.  Special case non-existent lines and the
	 * first line of an empty file.  In both cases, the cursor position
	 * is 0.
	 */
	if (p == NULL)
		p = file_gline(sp, ep, smp->lno, &len);
	if (p == NULL || len == 0) {
		if (yp != NULL && smp->lno == sp->lno) {
			*yp = smp - HMAP;
			*xp = O_ISSET(sp, O_NUMBER) ? O_NUMBER_LENGTH : 0;
		}
		if (file_lline(sp, ep, &lno))
			return (1);
		if (smp->lno > lno) {
			ADDCH(smp->lno == 1 ?
			    listset && skip_screens == 0 ? '$' : ' ' : '~');
		} else if (p == NULL) {
			GETLINE_ERR(sp, smp->lno);
			return (1);
		} else if (listset && skip_screens == 0)
			ADDCH('$');
		clrtoeol();
		return (0);
	}

	/*
	 * Set the number of characters to skip before reach the cursor
	 * character.  Offset by 1 and use 0 as a flag value.  We may be
	 * called repeatedly with a valid pointer to a cursor position.
	 * Don't fill it in unless it's the right line.
	 */
	cno_cnt = yp == NULL || smp->lno != sp->lno ? 0 : sp->cno + 1;

	/* This is the loop that actually displays characters. */
	for (count_cols = last_count_cols = 0, partial = 0; len; --len) {
		/* Get the next character and figure out its length. */
		if ((ch = *(u_char *)p++) == '\t' && !listset)
			chlen = TAB_OFF(sp, count_cols);
		else
			chlen = cname[ch].len;
		last_count_cols = count_cols;
		count_cols += chlen;

		/*
		 * If skipping screens, see if crossed a screen boundary.  If
		 * so, and this is the last one to skip, start displaying the
		 * characters, assuming there's something to display.
		 */
		if (skip_screens) {
			if (count_cols < cols_per_screen) {
				if (cno_cnt)
					--cno_cnt;
				continue;
			}
			count_cols -= cols_per_screen;
			cols_per_screen = sp->cols;
			if (--skip_screens || !count_cols) {
				if (cno_cnt)
					--cno_cnt;
				continue;
			}
			offset_in_char = chlen - count_cols;
			chlen = count_cols;
		} else
			offset_in_char = 0;

		/*
		 * Only display up to the right-hand column, once we there
		 * we're done.  Set a flag if the entire character wasn't
		 * displayed for use in setting the cursor.
		 */
		if (count_cols >= cols_per_screen) {
			chlen -= count_cols - cols_per_screen;
			if (count_cols > cols_per_screen)
				partial = 1;
			len = 1;		/* XXX 1, not 0, for loop. */
		}

		/*
		 * If the caller wants the cursor value, and this was the
		 * cursor character, set the value.  There are two ways to
		 * put the cursor on a tab -- if it's normal display mode,
		 * it goes on the last "space" of the tab.  If it's input
		 * mode, it goes on the first.  All other characters only
		 * set the cursor if the entire character was displayed,
		 * as the cursor goes on the last "space" of the character.
		 */
		if (cno_cnt && --cno_cnt == 0) {
			*yp = smp - HMAP;
			if (F_ISSET(sp, S_INPUT))
				*xp = last_count_cols;
			else if (!partial) {
				*xp = count_cols - 1;
				if (O_ISSET(sp, O_NUMBER) &&
				    !ISINFOLINE(sp, smp) && smp->off == 1)
					*xp += O_NUMBER_LENGTH;
			}
		}

		/*
		 * Display the character.  If it's a tab and tabs aren't some
		 * ridiculous length, do it fast.  (We do tab expansion here
		 * because curses doesn't have a way to set the tab length.)
		 */
		if (ch == '\t' && !listset) {
			chlen -= offset_in_char;
			if (chlen <= sizeof(TABSTR) - 1) {
				ADDNSTR(TABSTR, chlen);
			} else
				while (chlen--)
					ADDCH(TABCH);
		} else
			ADDNSTR(cname[ch].name + offset_in_char, chlen);
	}

	/*
	 * If not the info/mode line, and O_LIST set, and at the end of
	 * the line, and the line ended on this screen, add a trailing $.
	 */
	if (listset && len == 0 &&
	    skip_screens == 0 && count_cols < cols_per_screen) {
		++count_cols;
		ADDCH('$');
	}

	/* If didn't paint the whole line, clear the rest of it. */
	if (count_cols < cols_per_screen)
		clrtoeol();

	if (reverse_video)
		standend();
	return (0);
}

/*
 * svi_screens --
 *	Return the number of screens required by the line, or,
 *	if a column is specified, by the column within the line.
 */
size_t
svi_screens(sp, ep, lno, cnop)
	SCR *sp;
	EXF *ep;
	recno_t lno;
	size_t *cnop;
{
	size_t cols, len;
	char *p;

	/* Get a copy of the line. */
	if ((p = file_gline(sp, ep, lno, &len)) == NULL || len == 0)
		return (1);

	/* Figure out how many columns the line/column needs. */
	cols = svi_ncols(sp, p, len, cnop);

	/* Leading number if O_NUMBER option set. */
	if (O_ISSET(sp, O_NUMBER))
		cols += O_NUMBER_LENGTH;

	/* Trailing '$' if O_LIST option set. */
	if (O_ISSET(sp, O_LIST) && cnop == NULL)
		cols += sp->cname['$'].len;

	return (cols / sp->cols + (cols % sp->cols ? 1 : 0));
}

/*
 * svi_ncols --
 *	Return the number of columns required by the line, or,
 *	if a column is specified, by the column within the line.
 */
size_t
svi_ncols(sp, p, len, cnop)
	SCR *sp;
	u_char *p;
	size_t len, *cnop;
{
	CHNAME *cname;
	size_t cno_cnt, scno;
	int ch;

	/* Check for column count. */
	if (cnop != NULL)
		cno_cnt = *cnop;

	/* Calculate the columns needed. */
	cname = sp->cname;
	for (scno = 0; len; --len) {
		if ((ch = *(u_char *)p++) == '\t' && !O_ISSET(sp, O_LIST))
			scno += TAB_OFF(sp, scno);
		else
			scno += cname[ch].len;

		if (cnop != NULL) {
			if (cno_cnt == 0)
				break;
			--cno_cnt;
		}
	}

	return (scno);
}
