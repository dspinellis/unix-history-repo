/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "disp.h"

private int	get_indent proto((Line *));
private Line	*tailrule proto((Line *));
private void	DoPara proto((int dir));

/* Thanks to Brian Harvey for this paragraph boundery finding algorithm.
   It's really quite hairy figuring it out.  This deals with paragraphs that
   are seperated by blank lines, lines beginning with a Period (assumed to
   be an nroff command), lines beginning with BackSlash (assumed to be Tex
   commands).  Also handles paragraphs that are separated by lines of
   different indent; and it deals with outdented paragraphs, too.  It's
   really quite nice.  Here's Brian's algorithm.

   Definitions:

   THIS means the line containing the cursor.
   PREV means the line above THIS.
   NEXT means the line below THIS.

   BLANK means empty, empty except for spaces and tabs, starts with a period
   or a backslash, or nonexistent (because the edge of the buffer is
   reached).  ((BH 12/24/85 A line starting with backslash is blank only if
   the following line also starts with backslash.  This is so that \noindent
   is part of a paragraph, but long strings of TeX commands don't get
   rearranged.  It still isn't perfect but it's better.))

   BSBLANK means BLANK or starts with a backslash.  (BH 12/24/85)

   HEAD means the first (nonblank) line of the paragraph containing THIS.
   BODY means all other (nonblank) lines of the paragraph.
   TAIL means the last (nb) line of the paragraph.  (TAIL is part of BODY.)

   HEAD INDENT means the indentation of HEAD.  M-J should preserve this.
   BODY INDENT means the indentation of BODY.  Ditto.

   Subprocedures:

   TAILRULE(BODYLINE)
   If BODYLINE is BLANK, the paragraph has only one line, and there is no
   BODY and therefore no TAIL.  Return.  Otherwise, starting from BODYLINE,
   move down until you find a line that either is BSBLANK or has a different
   indentation from BODYLINE.  The line above that different line is TAIL.
   Return.

   Rules:

   1.  If THIS is BLANK, which command are you doing?  If M-J or M-[, then go
   up to the first non-BLANK line and start over.  (If there is no non-BLANK
   line before THIS, ring the bell.)  If M-], then the first non-BLANK line
   below THIS is HEAD, and the second consecutive non-BSBLANK line (if any) is
   the beginning of BODY.  (If there is no non-BLANK line after THIS, ring
   the bell.)  Do TAILRULE(beginning-of-BODY).  Go to rule A.

   2.  If PREV is BLANK or THIS is BSBLANK, then THIS is HEAD, and NEXT (if
   not BSBLANK) is in BODY.  Do TAILRULE(NEXT).  Go to rule A.

   3.  If NEXT is BSBLANK, then THIS is TAIL, therefore part of BODY.  Go to
   rule 5 to find HEAD.

   4.  If either NEXT or PREV has the same indentation as THIS, then THIS is
   part of BODY.  Do TAILRULE(THIS).  Go to rule 5 to find HEAD.  Otherwise,
   go to rule 6.

   5.  Go up until you find a line that is either BSBLANK or has a different
   indentation from THIS.  If that line is BLANK, the line below it is HEAD;
   If that line is non-BLANK, then call that new line THIS for what follows.
   If THIS is BSBLANK (that is, THIS starts with backslash), THIS is HEAD;
   otherwise, if (the new) PREV has the same indent as THIS, then (the new)
   NEXT is HEAD; if PREV has a different indent from THIS, then THIS is
   HEAD.  Go to rule A.	

   6.  If you got here, then both NEXT and PREV are nonblank and are
   differently indented from THIS.  This is a tricky case and there is no
   guarantee that you're going to win.  The most straightforward thing to do
   is assume that we are not using hanging indentation.  In that case:
   whichever of PREV and THIS is indented further is HEAD.  Do
   TAILRULE(HEAD+1).  Go to rule A.

   6+.  A more complicated variant would be this: if THIS is indented further
   than PREV, we are using regular indentation and rule 6 applies.  If PREV
   is indented further than THIS, look at both NEXT and the line after NEXT.
   If those two lines are indented equally, and more than THIS, then we are
   using hanging indent, THIS is HEAD, and NEXT is the first line of BODY.
   Do TAILRULE(NEXT).  Otherwise, rule 6 applies.

   A.  You now know where HEAD and TAIL are.  The indentation of HEAD is HEAD
   INDENT; the indentation of TAIL is BODY INDENT.

   B.  If you are trying to M-J, you are now ready to do it.

   C.  If you are trying to M-], leave point after the newline that ends
   TAIL.  In other words, leave the cursor at the beginning of the line
   after TAIL.  It is not possible for this to leave point where it started
   unless it was already at the end of the buffer.

   D.  If you are trying to M-[, if the line before HEAD is not BLANK, then
   leave point just before HEAD.  That is, leave the cursor at the beginning
   of HEAD.  If the line before HEAD is BLANK, then leave the cursor at the
   beginning of that line.  If the cursor didn't move, go up to the first
   earlier non-BLANK line and start over.


   End of Algorithm.  I implemented rule 6+ because it seemed nicer.  */

int	RMargin = 78,
	LMargin = 0;
private Line	*para_head,
	*para_tail;
private int	head_indent,
	body_indent;
static int	use_lmargin;

/* some defines for paragraph boundery checking */
#define I_EMPTY		(-1)	/* line "looks" empty (spaces and tabs) */
#define I_PERIOD	(-2)	/* line begins with "." or "\" */
#define I_BUFEDGE	(-3)	/* line is nonexistent (edge of buffer) */

static int	bslash;		/* Nonzero if get_indent finds line starting
				   with backslash */

private int
i_blank(lp)
Line	*lp;
{
	return (get_indent(lp) < 0);
}

private int
i_bsblank(lp)
Line	*lp;
{
	if (i_blank(lp))
		return 1;
	return bslash;
}

private int
get_indent(lp)
register Line	*lp;
{
	Bufpos	save;
	register int	indent;

	bslash = 0;
	if (lp == 0)
		return I_BUFEDGE;
	DOTsave(&save);
	SetLine(lp);
	if (blnkp(linebuf))
		indent = I_EMPTY;
	else if (linebuf[0] == '.')
		indent = I_PERIOD;
	else if (linebuf[0] == '\\') {
		/* BH 12/24/85.  Backslash is BLANK only if next line
		   also starts with Backslash. */
		bslash += 1;
		SetLine(lp->l_next);
		if (linebuf[0] == '\\')
			indent = I_PERIOD;
		else
			indent = 0;
	} else {
		ToIndent();
		indent = calc_pos(linebuf, curchar);
	}
	SetDot(&save);

	return indent;
}

private Line *
tailrule(lp)
register Line	*lp;
{
	int	i;

	i = get_indent(lp);
	if (i < 0)
		return lp;	/* one line paragraph */
	do {
		if ((get_indent(lp->l_next) != i) || bslash)
			/* BH line with backslash is head of next para */
			break;
	} while ((lp = lp->l_next) != 0);
	if (lp == 0)
		complain((char *) 0);
	return lp;
}

/* Finds the beginning, end and indent of the current paragraph, and sets
   the above global variables.  HOW says how to behave when we're between
   paragraphs.  That is, it's either FORWARD or BACKWARD depending on which
   way we're favoring. */

private void
find_para(how)
int	how;
{
	Line	*this,
		*prev,
		*next,
		*head = 0,
		*body = 0,
		*tail = 0;
	int	this_indent;
	Bufpos	orig;		/* remember where we were when we started */

	DOTsave(&orig);
strt:
	this = curline;
	prev = curline->l_prev;
	next = curline->l_next;
	this_indent = get_indent(this);

	if (i_blank(this)) {		/* rule 1 */
		if (how == BACKWARD) {
			while (i_blank(curline))
				if (firstp(curline))
					complain((char *) 0);
				else
					line_move(BACKWARD, 1, NO);
			goto strt;
		} else {
			while (i_blank(curline))
				if (lastp(curline))
					complain((char *) 0);
				else
					line_move(FORWARD, 1, NO);
			head = curline;
			next = curline->l_next;
			if (!i_bsblank(next))
				body = next;
			else
				body = head;
		}
	} else if (i_bsblank(this) || i_blank(prev)) {	/* rule 2 */
		head = this;
		if (!i_bsblank(next))
			body = next;
	} else if (i_bsblank(next)) {	/* rule 3 */
		tail = this;
		body = this;
	} else if ((get_indent(next) == this_indent) ||	/* rule 4 */
		   (get_indent(prev) == this_indent))
		body = this;
	else {		/* rule 6+ */
		if (get_indent(prev) > this_indent) {
			/* hanging indent maybe? */
			if ((next != 0) &&
			    (get_indent(next) == get_indent(next->l_next))) {
				head = this;
				body = next;
			}
		}
		/* Now we handle hanging indent else and the other
		   case of this_indent > get_indent(prev).  That is,
		   if we didn't resolve HEAD in the above if, then
		   we are not a hanging indent. */
		if (head == 0) {	/* still don't know */
			if (this_indent > get_indent(prev))
				head = this;
			else
				head = prev;
			body = head->l_next;
		}
	}
	/* rule 5 -- find the missing parts */
	if (head == 0) {    /* haven't found head of paragraph so do so now */
		Line	*lp;
		int	i;

		lp = this;
		do {
			i = get_indent(lp->l_prev);
			if (i < 0)	/* is blank */
				head = lp;
			else if (bslash)
				head = lp->l_prev;
			else if (i != this_indent) {
				this = lp->l_prev;
				if (get_indent(this->l_prev) == i)
					head = this->l_next;
				else
					head = this;
			}
		} while (head == 0 && (lp = lp->l_prev) != 0);
		if (lp == 0)
			complain((char *) 0);
	}
	if (body == 0)		/* this must be a one line paragraph */
		body = head;
	if (tail == 0)
		tail = tailrule(body);
	if (tail == 0 || head == 0 || body == 0)
		complain("BUG! tail(%d),head(%d),body(%d)!", tail, head, body);
	para_head = head;
	para_tail = tail;
	head_indent = get_indent(head);
	body_indent = get_indent(body);

	SetDot(&orig);
}

void
Justify()
{
	use_lmargin = is_an_arg();
	find_para(BACKWARD);
	DoJustify(para_head, 0, para_tail, length(para_tail), NO,
		  use_lmargin ? LMargin : body_indent);
}

Line *
max_line(l1, l2)
Line	*l1,
	*l2;
{
	if (inorder(l1, 0, l2, 0))
		return l2;
	return l1;
}

Line *
min_line(l1, l2)
Line	*l1,
	*l2;
{
	if (inorder(l1, 0, l2, 0))
		return l1;
	return l2;
}

void
RegJustify()
{
	Mark	*mp = CurMark(),
		*tailmark;
	Line	*l1 = curline,
		*l2 = mp->m_line;
	int	c1 = curchar,
		c2 = mp->m_char;
	Line	*rl1,
		*rl2;

	use_lmargin = is_an_arg();
	(void) fixorder(&l1, &c1, &l2, &c2);
	do {
		DotTo(l1, c1);
		find_para(FORWARD);
		rl1 = max_line(l1, para_head);
		rl2 = min_line(l2, para_tail);
		tailmark = MakeMark(para_tail, 0, M_FLOATER);
		DoJustify(rl1, (rl1 == l1) ? c1 : 0, rl2,
			  (rl2 == l2) ? c2 : length(rl2),
			  NO, use_lmargin ? LMargin : body_indent);
		l1 = tailmark->m_line->l_next;
		DelMark(tailmark);
		c1 = 0;
	} while (l1 != 0 && l2 != rl2);
}

void
do_rfill(ulm)
int	ulm;
{
	Mark	*mp = CurMark();
	Line	*l1 = curline,
		*l2 = mp->m_line;
	int	c1 = curchar,
		c2 = mp->m_char;

	use_lmargin = ulm;
	(void) fixorder(&l1, &c1, &l2, &c2);
	DoJustify(l1, c1, l2, c2, NO, use_lmargin ? LMargin : 0);
}

private void
do_space()
{
	int	c1 = curchar,
		c2 = c1,
		diff,
		nspace;
	char	ch;

	while (c1 > 0 && ((ch = linebuf[c1 - 1]) == ' ' || ch == '\t'))
		c1 -= 1;
	while ((ch = linebuf[c2]) == ' ' || ch == '\t')
		c2 += 1;
	diff = (c2 - c1);
	curchar = c2;

	if (diff == 0)
		return;
	if (c1 > 0) {
		int	topunct = c1 - 1;

		nspace = 1;
		if (diff >= 2) {
			while (strchr("\")]", linebuf[topunct])) {
				if (topunct == 0)
					break;
				topunct -= 1;
			}
			if (strchr("?!.:", linebuf[topunct]))
				nspace = 2;
		}
	} else
		nspace = 0;

	if (diff > nspace)
		del_char(BACKWARD, (diff - nspace), NO);
	else if (diff < nspace)
		insert_c(' ', (nspace - diff));
}

#ifdef MSDOS
/*#pragma loop_opt(off) */
#endif

void
DoJustify(l1, c1, l2, c2, scrunch, indent)
Line	*l1,
	*l2;
int	c1,
	c2,
	scrunch,
	indent;
{
	int	okay_char = -1;
	char	*cp;
	Mark	*savedot = MakeMark(curline, curchar, M_FLOATER),
		*endmark;

	(void) fixorder(&l1, &c1, &l2, &c2);	/* l1/c1 will be before l2/c2 */
	DotTo(l1, c1);
	if (get_indent(l1) >= c1) {
		if (use_lmargin) {
			Bol();
			n_indent(indent + (head_indent - body_indent));
			use_lmargin = 0;	/* turn this off now */
		}
		ToIndent();
	}
	endmark = MakeMark(l2, c2, M_FLOATER);

	for (;;) {
		/* The while loop succeeds at least once, when curchar ==
		   indent.  So we know that okay_char >= indent when we
		   exit the loop. */
		while (calc_pos(linebuf, curchar) < RMargin) {
			if (curline == endmark->m_line && curchar >= endmark->m_char)
				goto outahere;
			okay_char = curchar;
			if (eolp()) {
				/* delete line separator */
				del_char(FORWARD, 1, NO);
				ins_str("  ", NO);
			} else {
				cp = StrIndex(1, linebuf, curchar + 1, ' ');
				if (cp == 0)
					Eol();
				else
					curchar = (cp - linebuf);
			}
			do_space();
		}
		if (okay_char > indent)
			curchar = okay_char;
		if (curline == endmark->m_line && curchar >= endmark->m_char)
			goto outahere;

		/* Can't fit in small margin, so if' we're at the end of
		   the line then we just move to the next line.  Otherwise
		   we divide the line where we are and start over. */
		if (eolp()) {
			Line	*l = curline;

			line_move(FORWARD, 1, NO);
			if (l == curline)	/* didn't actuall go anywhere */
				goto outahere;
		} else {
			DelWtSpace();
			LineInsert(1);
			if (scrunch && TwoBlank()) {
				Eol();
				del_char(FORWARD, 1, NO);
			}
		}
		n_indent(indent);
	}
outahere:
	ToMark(savedot);	/* Back to where we were */
	DelMark(endmark);	/* Free up marks */
	DelMark(savedot);
	this_cmd = last_cmd = 0; /* So everything is under control */
	f_mess("");
}

#ifdef MSDOS
/*#pragma loop_opt() */
#endif

private void
DoPara(dir)
int	dir;
{
	register int	num = arg_value(),
			first_time = TRUE;

	while (--num >= 0) {
tryagain:	find_para(dir);		/* find paragraph bounderies */
		if ((dir == BACKWARD) &&
		    ((!first_time) || ((para_head == curline) && bolp()))) {
			if (bobp())
				complain((char *) 0);
			b_char(1);
			first_time = !first_time;
			goto tryagain;
		}
		SetLine((dir == BACKWARD) ? para_head : para_tail);
		if (dir == BACKWARD && !firstp(curline) &&
		    i_blank(curline->l_prev))
			line_move(BACKWARD, 1, NO);
		else if (dir == FORWARD) {
			if (lastp(curline)) {
				Eol();
				break;
			}
			/* otherwise */
			line_move(FORWARD, 1, NO);
		}
	}
}

void
BackPara()
{
	DoPara(BACKWARD);
}

void
ForPara()
{
	DoPara(FORWARD);
}
