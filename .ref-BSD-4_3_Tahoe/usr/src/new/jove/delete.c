/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

/* Routines to perform all kinds of deletion.  */

#include "jove.h"

/* Assumes that either line1 or line2 is actual the current line, so it can
   put its result into linebuf. */

void
patchup(line1, char1, line2, char2)
Line	*line1,
	*line2;
register int	char1,
		char2;
{
	if (line1 != line2)
		ChkWindows(line1, line2);
	DotTo(line1, char1);
	modify();
	linecopy(linebuf, curchar, lcontents(line2) + char2);

	/* The following is a redisplay optimization. */
	if (line1 != line2 && (char1 == 0 && char2 == 0))
		line1->l_dline = line2->l_dline;

	DFixMarks(line1, char1, line2, char2);
	makedirty(curline);
}

/* Deletes the region by unlinking the lines in the middle,
   and patching things up.  The unlinked lines are still in
   order.  */

Line *
reg_delete(line1, char1, line2, char2)
Line	*line1,
	*line2;
{
	register Line	*retline;

	if ((line1 == line2 && char1 == char2) || line2 == 0)
		complain((char *) 0);
	(void) fixorder(&line1, &char1, &line2, &char2);

	retline = nbufline();	/* New buffer line */

	(void) ltobuf(line1, genbuf);
	if (line1 == line2)
		genbuf[char2] = '\0';

	retline->l_prev = 0;
	retline->l_dline = putline(&genbuf[char1]);
	patchup(line1, char1, line2, char2);

	if (line1 == line2)
		retline->l_next = 0;
	else {
		retline->l_next = line1->l_next;
		(void) ltobuf(line2, genbuf);
		genbuf[char2] = '\0';
		line2->l_dline = putline(genbuf);
		/* Shorten this line */
	}

	if (line1 != line2) {
		line1->l_next = line2->l_next;
		if (line1->l_next)
			line1->l_next->l_prev = line1;
		else
			curbuf->b_last = line1;
		line2->l_next = 0;
	}

	return retline;
}

void
lremove(line1, line2)
register Line	*line1,
		*line2;
{
	Line	*next = line1->l_next;

	if (line1 == line2)
		return;
	line1->l_next = line2->l_next;
	if (line1->l_next)
		line1->l_next->l_prev = line1;
	else
		curbuf->b_last = line1;
	lfreereg(next, line2);	/* Put region at end of free line list. */
}

/* Delete character forward */

void
DelNChar()
{
	del_char(FORWARD, arg_value());
}

/* Delete character backward */

void
DelPChar()
{
	if (MinorMode(OverWrite)) {
		int	count = min(arg_value(), curchar);

		b_char(count);

		/* overwrite with spaces */
		set_arg_value(count);
		LastKeyStruck = ' ';
		SelfInsert();

		b_char(count);
	} else		
		del_char(BACKWARD, arg_value());
}

/* Delete some characters.  If deleting forward then call for_char
   to the final position otherwise call back_char.  Then delete the
   region between the two with patchup(). */

void
del_char(dir, num)
{
	Bufpos	before,
		after;
	int	killp = (abs(num) > 1);

	DOTsave(&before);
	if (dir == FORWARD)
		f_char(num);
	else
		b_char(num);
	if (before.p_line == curline && before.p_char == curchar)
		complain((char *) 0);
	if (killp)
		reg_kill(before.p_line, before.p_char, 1);
	else {
		DOTsave(&after);
		(void) fixorder(&before.p_line, &before.p_char, &after.p_line, &after.p_char);
		patchup(before.p_line, before.p_char, after.p_line, after.p_char);
		lremove(before.p_line, after.p_line);
	}
}

/* This kills a region between point, and line1/char1 and puts it on
   the kill-ring.  If the last command was one of the kill commands,
   the region is appended (prepended if backwards) to the last entry.  */

int	killptr = 0;
Line	*killbuf[NUMKILLS];

void
reg_kill(line2, char2, dot_moved)
Line	*line2;
{
	Line	*nl,
		*line1 = curline;
	int	char1 = curchar;
	int	backwards;

	backwards = !fixorder(&line1, &char1, &line2, &char2);
	/* This is a kludge!  But it possible for commands that don't
	   know which direction they are deleting in (e.g., delete
	   previous word could have been called with a negative argument
	   in which case, it wouldn't know that it really deleted
	   forward. */

	if (!dot_moved)
		backwards = !backwards;

	DotTo(line1, char1);

	nl = reg_delete(line1, char1, line2, char2);

	if (last_cmd != KILLCMD) {
		killptr = ((killptr + 1) % NUMKILLS);
		lfreelist(killbuf[killptr]);
		killbuf[killptr] = nl;
	} else {
		Line	*lastln = lastline(nl);

		if (backwards)
			(void) DoYank(nl, 0, lastln, length(lastln), killbuf[killptr], 0, (Buffer *) 0);
		else {
			Line	*olastln = lastline(killbuf[killptr]);

			(void) DoYank(nl, 0, lastln, length(lastln), olastln, length(olastln), (Buffer *) 0);
		}
	}
	this_cmd = KILLCMD;
}

void
DelReg()
{
	register Mark	*mp = CurMark();

	reg_kill(mp->m_line, mp->m_char, 0);
}

/* Save a region.  A pretend kill. */

void
CopyRegion()
{
	register Line	*nl;
	register Mark	*mp;
	register int	status;

	mp = CurMark();
	if (mp->m_line == curline && mp->m_char == curchar)
		complain((char *) 0);

	killptr = ((killptr + 1) % NUMKILLS);
	if (killbuf[killptr])
		lfreelist(killbuf[killptr]);
	nl = killbuf[killptr] = nbufline();
	SavLine(nl, NullStr);
	nl->l_next = nl->l_prev = 0;

	status = inorder(mp->m_line, mp->m_char, curline, curchar);
	if (status == -1)
		return;

	if (status)
		(void) DoYank(mp->m_line, mp->m_char, curline, curchar,
				nl, 0, (Buffer *) 0);
	else
		(void) DoYank(curline, curchar, mp->m_line, mp->m_char,
				nl, 0, (Buffer *) 0);
}

void
DelWtSpace()
{
	register char	*ep = &linebuf[curchar],
			*sp = &linebuf[curchar];

	while (*ep == ' ' || *ep == '\t')
		ep += 1;
	while (sp > linebuf && *(sp - 1) == ' ' || *(sp - 1) == '\t')
		sp -= 1;
	if (sp != ep) {
		curchar = sp - linebuf;
		DFixMarks(curline, curchar, curline, curchar + (ep - sp));
		strcpy(sp, ep);
		makedirty(curline);
		modify();
	}
}

void
DelBlnkLines()
{
	register Mark	*dot;
	int	all;

	if (!blnkp(&linebuf[curchar]))
		return;
	dot = MakeMark(curline, curchar, M_FLOATER);
	all = !blnkp(linebuf);
	while (blnkp(linebuf) && curline->l_prev)
		SetLine(curline->l_prev);
	all |= (firstp(curline));
	Eol();
	DelWtSpace();
	line_move(FORWARD, 1, NO);
	while (blnkp(linebuf) && !eobp()) {
		DelWtSpace();
		del_char(FORWARD, 1);
	}
	if (!all && !eobp())
		open_lines(1);
	ToMark(dot);
	DelMark(dot);
}

void
DelNWord()
{
	dword(1);
}

void
DelPWord()
{
	dword(0);
}

void
dword(forward)
{
	Bufpos	savedot;

	DOTsave(&savedot);
	if (forward)
		ForWord();
	else
		BackWord();
	reg_kill(savedot.p_line, savedot.p_char, 1);
}
