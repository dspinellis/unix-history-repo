/*************************************************************************
 * This program is copyright (C) 1985, 1986 by Jonathan Payne.  It is    *
 * provided to you without charge for use only on a licensed Unix        *
 * system.  You may copy JOVE provided that this notice is included with *
 * the copy.  You may not sell copies of this program or versions        *
 * modified for use on microcomputer systems, unless the copies are      *
 * included with a Unix system distribution and the source is provided.  *
 *************************************************************************/

int	MarksShouldFloat = 1;

#include "jove.h"

Mark *
MakeMark(line, column, type)
register Line	*line;
{
	register Mark	*newmark = (Mark *) emalloc(sizeof *newmark);

	MarkSet(newmark, line, column);
	newmark->m_next = curbuf->b_marks;
	newmark->m_floater = type;
	curbuf->b_marks = newmark;
	return newmark;
}

DelMark(m)
register Mark	*m;
{
	register Mark	*mp = curbuf->b_marks;

	if (m == mp)
		curbuf->b_marks = m->m_next;
	else {
		while (mp != 0 && mp->m_next != m)
			mp = mp->m_next;
		if (mp == 0)
			complain("Unknown mark!");
		mp->m_next = m->m_next;
	}
	free((char *) m);
}

AllMarkSet(b, line, col)
Buffer	*b;
register Line	*line;
{
	register Mark	*mp;

	for (mp = b->b_marks; mp != 0; mp = mp->m_next)
		MarkSet(mp, line, col);
}

MarkSet(m, line, column)
Mark	*m;
Line	*line;
{
	m->m_line = line;
	m->m_char = column;
}

PopMark()
{
	int	pmark;

	if (curmark == 0)
		return;
	if (curbuf->b_markring[(curbuf->b_themark + 1) % NMARKS] == 0) {
		pmark = curbuf->b_themark;
		do {
			if (--pmark < 0)
				pmark = NMARKS - 1;
		} while (curbuf->b_markring[pmark] != 0);

		curbuf->b_markring[pmark] = MakeMark(curline, curchar, MarksShouldFloat);
		ToMark(curmark);
		DelMark(curmark);
		curmark = 0;
	} else
		PtToMark();

	pmark = curbuf->b_themark - 1;
	if (pmark < 0)
		pmark = NMARKS - 1;
	curbuf->b_themark = pmark;
}

SetMark()
{
	if (exp_p)
		PopMark();
	else
		DoSetMark(curline, curchar);
}

DoSetMark(l, c)
Line	*l;
{
	curbuf->b_themark = (curbuf->b_themark + 1) % NMARKS;
	if (curmark == 0)
		curmark = MakeMark(l, c, MarksShouldFloat);
	else
		MarkSet(curmark, l, c);
	s_mess("[Point pushed]");
}

/* Move point to Mark */

ToMark(m)
Mark	*m;
{
	int	len;

	if (m == 0)
		return;
	DotTo(m->m_line, m->m_char);
	if (curchar > (len = length(curline)))
		curchar = len;
}

Mark *
CurMark()
{
	if (curmark == 0)
		complain("No mark.");
	return curmark;
}

PtToMark()
{
	Line	*mline;
	int	mchar;
	Mark	*m = CurMark();

	mline = curline;
	mchar = curchar;

	ToMark(m);
	MarkSet(m, mline, mchar);
}

/* Fix marks for after a deletion.  For now, even marks that don't
   float will actually float, because we can't allow marks to point
   to non-existant lines. */

DFixMarks(line1, char1, line2, char2)
register Line	*line1,
		*line2;
{
	register Mark	*m;
	Line	*lp = line1;

	if (curbuf->b_marks == 0)
		return;
	while (lp != line2->l_next) {
		for (m = curbuf->b_marks; m != 0; m = m->m_next) {
/*			if (!m->m_floater)
				continue; */
			if (m->m_line == lp)
				m->m_char |= (1 << 15);
		}
		lp = lp->l_next;
	}
	for (m = curbuf->b_marks; m; m = m->m_next) {
/*		if (!m->m_floater)
			continue; */
		if ((m->m_char & (1 << 15)) == 0)
			continue;	/* Not effected */
		m->m_char &= ~(1 << 15);
		if (m->m_line == line1 && m->m_char < char1)
			continue;	/* This mark is not affected */
		if (line1 == line2) {
			if (m->m_char >= char1 && m->m_char <= char2)
				m->m_char = char1;
			else if (m->m_char > char2)
				m->m_char -= (char2 - char1);
			/* Same line move the mark backward */
		} else if (m->m_line == line2) {
			if (m->m_char > char2)
				m->m_char = char1 + (m->m_char - char2);
			else
				m->m_char = char1;
			m->m_line = line1;
		} else {
			m->m_char = char1;
			m->m_line = line1;
		}
	}
}

/* Fix marks after an insertion.  Marks that don't float are ignored
   on insertion, which means PtToMark has to be careful ... */

IFixMarks(line1, char1, line2, char2)
register Line	*line1,
		*line2;
{
	register Mark	*m;

	for (m = curbuf->b_marks; m != 0; m = m->m_next) {
		if (!m->m_floater)
			continue;
		if (m->m_line == line1) {
			if (m->m_char > char1) {
				m->m_line = line2;
				if (line1 == line2)
					m->m_char += (char2 - char1);
				else
					m->m_char = char2 + (m->m_char - char1);
			}
		} 
	}
}
