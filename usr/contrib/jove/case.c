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

CapChar()
{
	register int	num,
			restore = 0;
	Bufpos	b;

	DOTsave(&b);

	if (exp < 0) {
		restore++;
		exp = -exp;
		num = exp;
		BackChar();	/* Cap previous EXP chars */
	} else
		num = exp;
		
	exp = 1;	/* So all the commands are done once */

	while (num--) {
		if (upper(&linebuf[curchar])) {
			modify();
			makedirty(curline);
		}
		if (eolp()) {
			if (curline->l_next == 0)
				break;
			SetLine(curline->l_next);
		}
		else
			curchar++;
	}
	if (restore)
		SetDot(&b);
}

CapWord()
{
	register int	num,
			restore = 0;
	Bufpos	b;

	DOTsave(&b);

	if (exp < 0) {
		restore++;
		exp = -exp;
		num = exp;
		BackWord();	/* Cap previous EXP words */
	} else
		num = exp;
		
	exp = 1;	/* So all the commands are done once */

	while (num--) {
		to_word(1);	/* Go to the beginning of the next word. */
		if (eobp())
			break;
		if (upper(&linebuf[curchar])) {
			modify();
			makedirty(curline);
		}
		curchar++;
		while (!eolp() && isword(linebuf[curchar])) {
			if (lower(&linebuf[curchar])) {
				modify();
				makedirty(curline);
			}
			curchar++;
		}
	}
	if (restore)
		SetDot(&b);
}

case_word(up)
{
	Bufpos	before;

	DOTsave(&before);
	ForWord();	/* This'll go backward if negative argument. */
	case_reg(before.p_line, before.p_char, curline, curchar, up);
}

static
upper(c)
register char	*c;
{
	if (islower(*c)) {
		*c -= ' ';
		return 1;
	}
	return 0;
}

static
lower(c)
register char	*c;
{
	if (isupper(*c)) {
		*c += ' ';
		return 1;
	}
	return 0;
}

case_reg(line1, char1, line2, char2, up)
Line	*line1,
	*line2;
int	char1;
{
	(void) fixorder(&line1, &char1, &line2, &char2);
	DotTo(line1, char1);

	exp = 1;
	for (;;) {
		if (curline == line2 && curchar == char2)
			break;
		if (!eolp())
			if ((up) ? upper(&linebuf[curchar]) : lower(&linebuf[curchar])) {
				makedirty(curline);
				modify();
			}
		ForChar();
	}
}

CasRegLower()
{
	CaseReg(0);
}

CasRegUpper()
{
	CaseReg(1);
}

CaseReg(up)
{
	register Mark	*mp = CurMark();
	Bufpos	savedot;

	DOTsave(&savedot);
	case_reg(curline, curchar, mp->m_line, mp->m_char, up);
	SetDot(&savedot);
}

UppWord()
{
	case_word(1);
}

LowWord()
{
	case_word(0);
}
