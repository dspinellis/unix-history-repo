/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

#include "jove.h"
#include "ctype.h"
#include "termcap.h"

#include <varargs.h>
#include <signal.h>
#include <sys/stat.h>

/* Kludge windows gets called by the routines that delete lines from the
   buffer.  If the w->w_line or w->w_top are deleted and this procedure
   is not called, the redisplay routine will barf. */

ChkWindows(line1, line2)
Line	*line1;
register Line	*line2;
{
	register Window	*w = fwind;
	register Line	*lp;

	do {
		for (lp = line1->l_next; lp != line2->l_next; lp = lp->l_next) {
			if (lp == w->w_top)
				w->w_flags |= TOPGONE;
			if (lp == w->w_line)
				w->w_flags |= CURGONE;
		}
		w = w->w_next;
	} while (w != fwind);
}

extern int	RingBell;

redisplay()
{
	register Window	*w = fwind;
	int	lineno,
		done_ID = 0,
		i;
	register struct scrimage	*des_p,
					*phys_p;

	curwind->w_line = curwind->w_bufp->b_dot;
	curwind->w_char = curwind->w_bufp->b_char;

	if (InputPending = charp())
		return;

#ifdef JOB_CONTROL
	if (UpdFreq)
		sighold(SIGALRM);
#endif
	if (RingBell) {
		dobell(1);
		RingBell = 0;
	}
	if (UpdMesg)
		DrawMesg(YES);

	for (lineno = 0, w = fwind; lineno < ILI; w = w->w_next) {
		UpdWindow(w, lineno);
		lineno += w->w_height;
	}

	des_p = DesiredScreen;
	phys_p = PhysScreen;
	for (i = 0; i < ILI; i++, des_p++, phys_p++) {
		if (!done_ID && (des_p->s_id != phys_p->s_id)) {
			DoIDline(i);
			done_ID++;
		}
		if ((des_p->s_flags & (DIRTY | L_MOD)) ||
		    (des_p->s_id != phys_p->s_id) ||
		    (des_p->s_vln != phys_p->s_vln) ||
		    (des_p->s_offset != phys_p->s_offset))
			UpdLine(i);
		if (InputPending)
			goto ret;
	}

	UpdModLine = 0;

	if (Asking) {
		Placur(LI - 1, min(CO - 2, calc_pos(mesgbuf, Asking)));
			/* Nice kludge */
		flusho();
	} else
		GotoDot();
ret:
#ifdef JOB_CONTROL
	if (UpdFreq)
		sigrelse(SIGALRM);
#else
	;	/* yuck */
#endif

}

dobell(n)
{
	while (--n >= 0) {
		if (VisBell && VB)
			putstr(VB);
		else
			putpad("20\007", 1);
	}
	flusho();
}

/* find_pos() returns the position on the line, that c_char represents
   in line. */

find_pos(line, c_char)
Line	*line;
{
	return calc_pos(lcontents(line), c_char);
}

calc_pos(lp, c_char)
register char	*lp;
register int	c_char;
{
	register int	pos = 0;
	register int	c;

	while ((--c_char >= 0) && ((c = *lp++) & 0177) != 0) {
		if (c == '\t')
			pos += (tabstop - (pos % tabstop));
		else if (isctrl(c))
			pos += 2;
		else
			pos++;
 	}
	return pos;
}

int	UpdModLine = 0,
	UpdMesg = 0,
	CanScroll = 0;

DoIDline(start)
{
	register struct scrimage	*des_p = &DesiredScreen[start];
	struct scrimage	*phys_p = &PhysScreen[start];
	register int	i,
			j;

	/* Some changes have been made.  Try for insert or delete lines.
	   If either case has happened, Addlines and/or DelLines will do
	   necessary scrolling, also CONVERTING PhysScreen to account for the
	   physical changes.  The comparison continues from where the
	   insertion/deletion takes place; this doesn't happen very often,
	   usually it happens with more than one window with the same
	   buffer. */

	if (!CanScroll)
		return;		/* We should never have been called! */

	for (i = start; i < ILI; i++, des_p++, phys_p++)
		if (des_p->s_id != phys_p->s_id)
			break;

	for (; i < ILI; i++) {
		for (j = i + 1; j < ILI; j++) {
			des_p = &DesiredScreen[j];
			phys_p = &PhysScreen[j];
			if (des_p->s_id != 0 && des_p->s_id == phys_p->s_id)
				break;
			if (des_p->s_id == PhysScreen[i].s_id) {
				if (des_p->s_id == 0)
					continue;
				if (AddLines(i, j - i)) {
					DoIDline(j);
					return;
				}
				break;
			}
			if ((des_p = &DesiredScreen[i])->s_id == phys_p->s_id) {
				if (des_p->s_id == 0)
					continue;
				if (DelLines(i, j - i)) {
					DoIDline(i);
					return;
				}
				break;
			}
		}
	}
}

/* Make DesiredScreen reflect what the screen should look like when we are done
   with the redisplay.  This deals with horizontal scrolling.  Also makes
   sure the current line of the Window is in the window. */

UpdWindow(w, start)
register Window	*w;
{
	Line	*lp;
	int	i,
		DotIsHere = 0,
		upper,		/* Top of window */
		lower,		/* Bottom of window */
		ntries = 0;	/* # of tries at updating window. */
	register struct scrimage	*des_p,
					*phys_p;
	Buffer	*bp = w->w_bufp;

retry:
	if (w->w_flags & CURGONE) {
		w->w_line = bp->b_dot;
		w->w_char = bp->b_char;
	}
	if (w->w_flags & TOPGONE)
		CentWind(w);	/* Reset topline of screen */
	w->w_flags = 0;
	for (i = w->w_height, lp = w->w_top; --i > 0 && lp != 0; lp = lp->l_next)
		if (lp == w->w_line)
			break;
	if (i == 0 || lp == 0) {	/* Current line not in window */
		ntries++;
		if (ntries == 1) {
			CalcWind(w);
			goto retry;
		} else if (ntries == 2) {
			w->w_top = w->w_line = w->w_bufp->b_first;
			f_mess("ERROR in redisplay: I got hopelessly lost!");
			goto retry;
		} else if (ntries == 3) {
			printf("\rOops, still lost, quitting ...\r\n");
			finish(1);
		}
	}

	upper = start;
	lower = upper + w->w_height - 1;	/* Don't include modeline */
	des_p = &DesiredScreen[upper];
	phys_p = &PhysScreen[upper];
	for (i = upper, lp = w->w_top; lp != 0 && i < lower; i++, des_p++, phys_p++, lp = lp->l_next) {
		des_p->s_window = w;
		des_p->s_lp = lp;
		des_p->s_id = lp->l_dline & ~DIRTY;
		des_p->s_flags = isdirty(lp) ? L_MOD : 0;
		if (w->w_numlines)
			des_p->s_vln = w->w_topnum + (i - upper);
		else
			des_p->s_vln = 0;

		if (lp == w->w_line) {
			int	diff = w->w_numlines ? 8 : 0,
				strt_col = phys_p->s_offset,
				end_col = strt_col + (CO - 2) - diff;

			/* Right now we are displaying from strt_col to
			   end_col of the buffer line.  These are PRINT
			   colums, not actual characters. */
			w->w_dotline = i;
			w->w_dotcol = find_pos(lp, w->w_char);
			/* if the new dotcol is out of range, reselect
			   a horizontal window */
			if (w->w_dotcol < strt_col || w->w_dotcol >= end_col) {
				if (w->w_dotcol < ((CO - 2) - diff))
					strt_col = 0;
				else
					strt_col = w->w_dotcol - (CO / 2);
			}
			w->w_dotcol += diff;
			des_p->s_offset = strt_col;
			DotIsHere++;
		} else
			des_p->s_offset = 0;
	}
	if (!DotIsHere) {
		f_mess("DotNotHere is impossible!");
		finish(1);
	}

	/* Is structure assignment faster than copy each field seperately */
	if (i < lower) {
		static struct scrimage	dirty_plate = { 0, DIRTY, 0, 0, 0, 0 },
					clean_plate = { 0, 0, 0, 0, 0, 0 };

		for (; i < lower; i++, des_p++, phys_p++)
			if (phys_p->s_id != 0)
				*des_p = dirty_plate;
			else
				*des_p = clean_plate;
	}

	des_p->s_window = w;
	des_p->s_flags = 0;
	if (((des_p->s_id = (int) w->w_bufp) != phys_p->s_id) || UpdModLine)
		des_p->s_flags = MODELINE | DIRTY;
}

/* Write whatever is in mesgbuf (maybe we are Asking, or just printed
   a message).  Turns off the UpdateMesg line flag. */

DrawMesg(abortable)
{
	if (charp())
		return;
	i_set(ILI, 0);
	if (swrite(mesgbuf, NIL, abortable)) {
		cl_eol();
		UpdMesg = 0;
	}
	flusho();
}

/* Goto the current position in the current window.  Presumably redisplay()
   has already been called, and curwind->{w_dotline,w_dotcol} have been set
   correctly. */

GotoDot()
{
	if (InputPending)
		return;
	Placur(curwind->w_dotline, curwind->w_dotcol -
				PhysScreen[curwind->w_dotline].s_offset);
	flusho();
}

private
UntilEqual(start)
register int	start;
{
	register struct scrimage	*des_p = &DesiredScreen[start],
					*phys_p = &PhysScreen[start];

	while ((start < ILI) && (des_p->s_id != phys_p->s_id)) {
		des_p++;
		phys_p++;
		start++;
	}

	return start;
}

/* Calls the routine to do the physical changes, and changes PhysScreen to
   reflect those changes. */

AddLines(at, num)
register int	at,
		num;
{
	register  int	i;
	int	bottom = UntilEqual(at + num);

	if (num == 0 || num >= ((bottom - 1) - at))
		return 0;	/* We did nothing */
	v_ins_line(num, at, bottom - 1);

	/* Now change PhysScreen to account for the physical change. */

	for (i = bottom - 1; i - num >= at; i--)
		PhysScreen[i] = PhysScreen[i - num];
	for (i = 0; i < num; i++)
		PhysScreen[at + i].s_id = 0;
	return 1;	/* We did something. */
}

DelLines(at, num)
register int	at,
		num;
{
	register int	i;
	int	bottom = UntilEqual(at + num);

	if (num == 0 || num >= ((bottom - 1) - at))
		return 0;
	v_del_line(num, at, bottom - 1);

	for (i = at; num + i < bottom; i++)
		PhysScreen[i] = PhysScreen[num + i];
	for (i = bottom - num; i < bottom; i++)
		PhysScreen[i].s_id = 0;
	return 1;
}

/* Update line linenum in window w.  Only set PhysScreen to DesiredScreen
   if the swrite or cl_eol works, that is nothing is interupted by 
   characters typed. */ 

UpdLine(linenum)
register int	linenum;
{
	register struct scrimage	*des_p = &DesiredScreen[linenum];
	register Window	*w = des_p->s_window;

	i_set(linenum, 0);
	if (des_p->s_flags & MODELINE)
		ModeLine(w);
	else if (des_p->s_id) {
		des_p->s_lp->l_dline &= ~DIRTY;
		des_p->s_flags &= ~(DIRTY | L_MOD);
#ifdef ID_CHAR
		if (!UseIC && w->w_numlines)
#else
		if (w->w_numlines)
#endif
			(void) swrite(sprint("%6d  ", des_p->s_vln), NIL, YES);

#ifdef ID_CHAR
		if (UseIC) {
			char	outbuf[256],
				*lptr;
			int	fromcol = w->w_numlines ? 8 : 0;

			if (w->w_numlines)
				sprintf(outbuf, "%6d  ", des_p->s_vln);
			lptr = lcontents(des_p->s_lp);
			DeTab(des_p->s_offset, lptr, outbuf + fromcol,
				(sizeof outbuf) - 1 - fromcol,
				des_p->s_window->w_visspace);
			if (IDchar(outbuf, linenum, 0))
				PhysScreen[linenum] = *des_p;
			else if (i_set(linenum, 0), swrite(outbuf, NIL, YES))
				do_cl_eol(linenum);
			else
				PhysScreen[linenum].s_id = -1;
		} else
#endif ID_CHAR
		    if (BufSwrite(linenum))
			do_cl_eol(linenum);
		else
			PhysScreen[linenum].s_id = -1;
	} else if (PhysScreen[linenum].s_id)	/* Not the same ... make sure */
		do_cl_eol(linenum);
}

do_cl_eol(linenum)
register int	linenum;
{
	cl_eol();
	PhysScreen[linenum] = DesiredScreen[linenum];
}

#ifdef ID_CHAR

/* From here to the end of the file is code that tries to utilize the
   insert/delete character feature on some terminals.  It is very confusing
   and not so well written code, AND there is a lot of it.  You may want
   to use the space for something else. */

extern struct screenline	*Screen;
int	IN_INSmode = 0;

int	UseIC;

int	DClen,
	MDClen,
	IClen,
	MIClen,
	IMlen,
	CElen;

disp_opt_init()
{
	DClen = DC ? strlen(DC) : 0;
	MDClen = M_DC ? strlen(M_DC) : 9999;
	IClen = IC ? strlen(IC) : 0;
	MIClen = M_IC ? strlen(M_IC) : 9999;
	IMlen = IM ? strlen(IM) : 0;
	CElen = CE ? strlen(CE) : 0;

	UseIC = (IC || IM || M_IC);
}

INSmode(on)
{
	if (on && !IN_INSmode) {
		putpad(IM, 1);
		IN_INSmode++;
	} else if (!on && IN_INSmode) {
		putpad(EI, 1);
		IN_INSmode = 0;
	}
}

private
DeTab(s_offset, buf, outbuf, limit, visspace)
register char	*buf;
char	*outbuf;
{
	register char	*phys_p = outbuf,
			c;
	register int	pos = 0;
	char		*limitp = &outbuf[limit];

#define OkayOut(ch)	if ((pos++ >= s_offset) && (phys_p < limitp))\
				*phys_p++ = ch;\
			else

	while (c = *buf++) {
		if (c == '\t') {
			int	nchars = (tabstop - (pos % tabstop));

			if (visspace) {
				OkayOut('>');
				--nchars;
			}
			while (--nchars >= 0)
				OkayOut(' ');

		} else if (isctrl(c)) {
			OkayOut('^');
			OkayOut(c == 0177 ? '?' : c + '@');
		} else {
			if (visspace && c == ' ')
				c = '_';
			OkayOut(c);
		}
		if (pos - s_offset >= CO) {
			phys_p = &outbuf[CO - 1];
			*phys_p++ = '!';
			break;
		}			
	}
	*phys_p = 0;
}

/* ID character routines full of special cases and other fun stuff like that.
   It actually works though ... 

  	Returns Non-Zero if you are finished (no differences left). */

private
IDchar(new, lineno, col)
register char	*new;
{
	register int	i;
	int	j,
		oldlen,
		NumSaved;
	register struct screenline	*sline = &Screen[lineno];

	oldlen = sline->s_length - sline->s_line;

	for (i = col; i < oldlen && new[i] != 0; i++)
		if (sline->s_line[i] != new[i])
			break;
	if (new[i] == 0 || i == oldlen)
		return (new[i] == 0 && i == oldlen);

	for (j = i + 1; j < oldlen && new[j]; j++) {
		if (new[j] == sline->s_line[i]) {
			NumSaved = IDcomp(new + j, sline->s_line + i,
					strlen(new)) + NumSimilar(new + i,
						sline->s_line + i, j - i);
			if (OkayInsert(NumSaved, j - i)) {
				InsChar(lineno, i, j - i, new);
				return(IDchar(new, lineno, j));
			}
		}
	}

	for (j = i + 1; j < oldlen && new[i]; j++) {
		if (new[i] == sline->s_line[j]) {
			NumSaved = IDcomp(new + i, sline->s_line + j,
					oldlen - j);
			if (OkayDelete(NumSaved, j - i, new[oldlen] == 0)) {
				DelChar(lineno, i, j - i);
				return(IDchar(new, lineno, j));
			}
		}
	}
	return 0;
}

private
NumSimilar(s, t, n)
register char	*s,
		*t;
{
	register int	num = 0;

	while (n--)
		if (*s++ == *t++)
			num++;
	return num;
}

private
IDcomp(s, t, len)
register char	*s,
		*t;
{
	register int	i;
	int	num = 0,
		nonspace = 0;
	char	c;

	for (i = 0; i < len; i++) {
		if ((c = *s++) != *t++)
			break;
		if (c != ' ')
			nonspace++;
		if (nonspace)
			num++;
	}

	return num;
}

private
OkayDelete(Saved, num, samelength)
{
	/* If the old and the new are the same length, then we don't
	 * have to clear to end of line.  We take that into consideration.
	 */
	return ((Saved + (!samelength ? CElen : 0))
		> min(MDClen, DClen * num));
}

private
OkayInsert(Saved, num)
{
	register int	n = 0;

	if (IC)		/* Per character prefixes */
		n = min(num * IClen, MIClen);

	if (IM && !IN_INSmode) {	
		/* Good terminal.  Fewer characters in this case */
		n += IMlen;
	}

	n += num;	/* The characters themselves */

	return Saved > n;
}

extern int	CapCol;
extern char	*cursend;
extern struct screenline	*Curline;

private
DelChar(lineno, col, num)
{
	register char	*from,
			*to;
	register int	i;
	struct screenline *sp = (&Screen[lineno]);

	Placur(lineno, col);
	if (M_DC && num > 1) {
		char	minibuf[16];

		sprintf(minibuf, M_DC, num);
		putpad(minibuf, num);
	} else {
		for (i = num; --i >= 0; )
			putpad(DC, 1);
	}

	to = sp->s_line + col;
	from = to + num;

	byte_copy(from, to, sp->s_length - from + 1);
	clrline(sp->s_length - num, sp->s_length);
	sp->s_length -= num;
}

private
InsChar(lineno, col, num, new)
char	*new;
{
	register char	*sp1,
			*sp2,	/* To push over the array. */
			*sp3;	/* Last character to push over. */
	int	i;

	i_set(lineno, 0);
	sp2 = Curline->s_length + num;

	if (sp2 >= cursend) {
		i_set(lineno, CO - num - 1);
		cl_eol();
		sp2 = cursend - 1;
	}
	Curline->s_length = sp2;
	sp1 = sp2 - num;
	sp3 = Curline->s_line + col;

	while (sp1 >= sp3)
		*sp2-- = *sp1--;

	new += col;
	byte_copy(new, sp3, num);
	/* The internal screen is correct, and now we have to do
	   the physical stuff. */

	Placur(lineno, col);
	if (IM) {
		if (!IN_INSmode)
			INSmode(1);
	} else if (M_IC && num > 1) {
		char	minibuf[16];

		sprintf(minibuf, M_IC, num);
		putpad(minibuf, num);
	} else if (IC) {
		for (i = 0; i < num; i++)
			putpad(IC, 1);
	}
	for (i = 0; i < num; i++) {
		putchar(new[i]);
		if (IN_INSmode)
			putpad(IP, 1);
	}
	CapCol += num;
}

#endif ID_CHAR

/* chkmail() returns nonzero if there is new mail since the
   last time we checked. */

char	Mailbox[128];	/* initialized in main */
int	MailInt = 60;	/* check no more often than 60 seconds */
#ifdef BIFF
int	BiffChk = NO;	/* whether or not to turn off biff while in JOVE */
#endif

chkmail(force)
{
	time_t	now;
	static time_t	last_chk = 0;
	static int	value = FALSE;
	static off_t	last_size = 0;
	struct stat	stbuf;
	int	last_val;
	extern time_t	time0;

	time(&now);
	if (!force && (now < last_chk + MailInt))
		return value;
	if (stat(Mailbox, &stbuf) < 0)
		return FALSE;
	last_val = value;
	value = ((stbuf.st_mtime > time0) &&
		 (stbuf.st_size > 0) &&
		 (stbuf.st_size > last_size) &&
		 (stbuf.st_mtime + 5 > stbuf.st_atime));
	last_chk = now;
	last_size = stbuf.st_size;
	if (value == TRUE && value != last_val)
		dobell(3);
	return value;
}

/* Print the mode line. */

private char	*mode_p,
		*mend_p;
int	BriteMode = 1;		/* modeline should standout */

private
mode_app(str)
register char	*str;
{
	if (mode_p >= mend_p)
		return;
	while ((mode_p < mend_p) && (*mode_p++ = *str++))
		;
	mode_p--;	/* back over the null */
}

char	ModeFmt[120] = "%3c %[%sJOVE (%M)   Buffer: %b  \"%f\" %]%s%m %((%t)%s%)%e";

ModeLine(w)
register Window	*w;
{
	extern int	i_line;
	int	n,
		ign_some = 0;
	char	line[132],
		*fmt = ModeFmt,
		tmp[16],
		fillc,
		c;
	register Buffer	*thisbuf = w->w_bufp;
	register Buffer *bp;

	mode_p = line;
	mend_p = &line[(sizeof line) - 1];

	if (BriteMode != 0 && SO == 0)
		BriteMode = 0;
	fillc = BriteMode ? ' ' : '-';

	while (c = *fmt++) {
		if (c == '\\')
			c = *fmt++;
		if (c != '%') {
			if (!ign_some)
				*mode_p++ = c;
			continue;
		}
		c = *fmt++;	/* Character after the percent! */
		if (ign_some && c != ')')
			continue;
		n = 1;
		if (c >= '0' && c <= '9') {
			n = 0;
			while (c >= '0' && c <= '9') {
				n = n * 10 + (c - '0');
				c = *fmt++;
			}
		}
		switch (c) {
		case '(':
			if (w->w_next != fwind)	/* Not bottom window. */
				ign_some++;
			break;

		case ')':
			ign_some = 0;
			break;

		case 'c':
			while (--n >= 0)
				*mode_p++ = fillc;
			break;

		case '[':
		case ']':
		    {
		    	char	*strs = (c == '[') ? "[[[[[[[[[[" : "]]]]]]]]]]";

		    	mode_app(strs + 10 - RecDepth);
			break;
		    }
			
		case 's':
			if (mode_p[-1] == ' ')
				continue;
			*mode_p++ = ' ';
			break;

		case 'M':
		    {
		    	static char	*mmodes[] = {
				"Fundamental ",
				"Text ",
				"C ",
#ifdef LISP
				"Lisp ",
#endif
				0
			};

		    	mode_app(mmodes[thisbuf->b_major]);

			if (BufMinorMode(thisbuf, Fill))
				mode_app("Fill ");
			if (BufMinorMode(thisbuf, Abbrev))
				mode_app("Abbrev ");
			if (BufMinorMode(thisbuf, OverWrite))
				mode_app("OvrWt ");
			if (BufMinorMode(thisbuf, Indent))
				mode_app("AI ");
			if (KeyMacro.m_flags & DEFINE)
				mode_app("Def ");
			mode_p--;	/* Back over the extra space. */
			break;
		    }

		case 'b':
			mode_app(thisbuf->b_name);
			break;

		case 'f':
		case 'F':
			if (thisbuf->b_fname == 0)
				mode_app("[No file]");
			else {
				if (c == 'f')
					mode_app(pr_name(thisbuf->b_fname));
				else
					mode_app(basename(thisbuf->b_fname));
			}
			break;


		case 'n':
			for (bp = world, n = 1; bp != 0; bp = bp->b_next, n++)
				if (bp == thisbuf)
					break;

			sprintf(tmp, "%d", n);
			mode_app(tmp);
			break;

		case 'm':
			if (IsModified(w->w_bufp))
				*mode_p++ = '*';
			else
				*mode_p++ = ' ';
			break;

		case 't':
		    {
			char	timestr[12];

		    	mode_app(get_time((time_t *) 0, timestr, 11, 16));
			break;
		    }

#ifdef LOAD_AV
		case 'l':
		    {
			double	theavg;
		    	char	minibuf[10];

		    	get_la(&theavg);
		    	theavg += .005;	/* round to nearest .01 */
		    	sprintf(minibuf, "%d.%02d",
			       (int) theavg,
			       (int)((theavg - (int) theavg) * 100));
		    	mode_app(minibuf);
		    }
		    break;
#endif

		case 'C':	/* check mail here */
			if (chkmail(NO))
				mode_app("[New mail]");
			break;

#ifdef CHDIR
		case 'd':	/* print working directory */
			mode_app(pr_name(pwd()));
			break;
#endif
			
		case 'e':
		    {
			/* 2 space pad pluss padding for magic cookies */
			char	*last_p = &line[CO - 2 - (2 * SG)];

			while (mode_p < last_p)
				*mode_p++ = fillc;

		    	goto outahere;		/* %e means we're done! */
		    }
		}
	}

outahere:
	*mode_p = 0;

	/* Highlight mode line. */
	if (BriteMode) {
#ifdef ID_CHAR
		if (IN_INSmode)
			INSmode(0);
#endif
		putpad(SO, 1);
	}
	if (swrite(line, BriteMode, YES))
		do_cl_eol(i_line);
	if (BriteMode)
		putpad(SE, 1);
}

RedrawDisplay()
{
	Line	*newtop = prev_line((curwind->w_line = curline), exp_p ?
				exp : HALF(curwind));

	if (newtop == curwind->w_top)
		v_clear(FLine(curwind), FLine(curwind) + SIZE(curwind));
	else
		SetTop(curwind, newtop);
}

v_clear(line1, line2)
register int	line1;
{
	register struct scrimage	*phys_p, *des_p;

	phys_p = &PhysScreen[line1];
	des_p = &DesiredScreen[line1];

	while (line1 <= line2) {
		i_set(line1++, 0);
		cl_eol();
		phys_p->s_id = des_p->s_id = 0;
		phys_p++, des_p++;
	}
}

ClAndRedraw()
{
	cl_scr(1);
}

NextPage()
{
	Line	*newline;

	if (Asking)
		return;
	if (exp < 0) {
		exp = -exp;
		PrevPage();
		return;
	}
	if (exp_p)
		UpScroll();
	else {
		if (in_window(curwind, curwind->w_bufp->b_last) != -1) {
			rbell();
			return;
		}
		newline = next_line(curwind->w_top, max(1, SIZE(curwind) - 1));
		SetTop(curwind, curwind->w_line = newline);
		if (curwind->w_bufp == curbuf)
			SetLine(newline);
	}
}

PrevPage()
{
	Line	*newline;

	if (Asking)
		return;
	if (exp < 0) {
		exp = -exp;
		NextPage();
		return;
	}
	if (exp_p)
		DownScroll();
	else {
		newline = prev_line(curwind->w_top, max(1, SIZE(curwind) - 1));
		SetTop(curwind, curwind->w_line = newline);
		if (curwind->w_bufp == curbuf)
			SetLine(newline);
	}
}

UpScroll()
{
	SetTop(curwind, next_line(curwind->w_top, exp));
	if ((curwind->w_bufp == curbuf) &&
	    (in_window(curwind, curline) == -1))
		SetLine(curwind->w_top);
}

DownScroll()
{
	SetTop(curwind, prev_line(curwind->w_top, exp));
	if ((curwind->w_bufp == curbuf) &&
	    (in_window(curwind, curline) == -1))
		SetLine(curwind->w_top);
}

int	VisBell = 0,
	RingBell = 0;	/* So if we have a lot of errors ...
			   ring the bell only ONCE */
rbell()
{
	RingBell++;
}

/* Message prints the null terminated string onto the bottom line of the
   terminal. */

message(str)
char	*str;
{
	if (InJoverc)
		return;
	UpdMesg++;
	errormsg = 0;
	if (str != mesgbuf)
		null_ncpy(mesgbuf, str, (sizeof mesgbuf) - 1);
}

/* End of Window */

Eow()
{
	if (Asking)
		return;
	SetLine(next_line(curwind->w_top, SIZE(curwind) - 1 -
			min(SIZE(curwind) - 1, exp - 1)));
	if (!exp_p)
		Eol();
}

/* Beginning of Window */

Bow()
{
	if (Asking)
		return;
	SetLine(next_line(curwind->w_top, min(SIZE(curwind) - 1, exp - 1)));
}

private int	LineNo,
		last_col,
		DoAutoNL;
private Window	*old_wind;	/* save the window we were in BEFORE
				   before we were called, if UseBuffers
				   is nonzero */

int	UseBuffers = FALSE;
int	TOabort = 0;

/* This initializes the typeout.  If send-typeout-to-buffers is set
   the buffer NAME is created (emptied if it already exists) and output
   goes to the buffer.  Otherwise output is drawn on the screen and
   erased by TOstop() */

TOstart(name, auto_newline)
char	*name;
{
	if (UseBuffers) {
		old_wind = curwind;
		pop_wind(name, YES, B_SCRATCH);
	}
	TOabort = LineNo = last_col = 0;
	DoAutoNL = auto_newline;
}

/* VARARGS1 */

Typeout(fmt, va_alist)
char	*fmt;
va_dcl
{
	if (TOabort)
		return;

	if (!UseBuffers && (LineNo == ILI - 1)) {
		register int	c;

		LineNo = 0;
		last_col = 0;
		f_mess("--more--");
		if ((c = getchar()) != ' ') {
			TOabort++;
			if (c != CTL(G) && c != RUBOUT)
				Ungetc(c);
			return;
		}
		f_mess(NullStr);
	}

	if (fmt) {
		extern int	i_col;
		char	string[132];
		va_list	ap;

		va_start(ap);
		format(string, sizeof string, fmt, ap);
		va_end(ap);
		if (UseBuffers)
			ins_str(string, NO);
		else {
			i_set(LineNo, last_col);
			(void) swrite(string, NIL, YES);
			last_col = i_col;
		}
	}
	if (!UseBuffers) {
		PhysScreen[LineNo].s_id = -1;
		if (fmt == 0 || DoAutoNL != 0) {
			cl_eol();
			flusho();
			LineNo++;
			last_col = 0;
		}
	} else if (fmt == 0 || DoAutoNL != 0)
		ins_str("\n", NO);
}

TOstop()
{
	int	c;

	if (UseBuffers) {
		ToFirst();
		SetWind(old_wind);
	} else {
		if (TOabort)
			return;
		if (last_col != 0)
			Typeout((char *) 0);
		Typeout("----------");
		cl_eol();
		flusho();
		c = getchar();
		if (c != ' ')
			Ungetc(c);
	}
}
