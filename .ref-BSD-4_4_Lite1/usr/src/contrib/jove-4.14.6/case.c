/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "disp.h"
#include "ctype.h"

private	bool
	lower proto((char *)),
	upper proto((char *));

private void
	CaseReg proto((bool up)),
	case_reg proto((struct line *line1,int char1,struct line *line2,int char2,bool up));

void
CapChar()
{
	register int	num,
			restore = NO;
	Bufpos	b;

	DOTsave(&b);

	num = arg_value();
	if (num < 0) {
		restore = YES;
		num = -num;
		b_char(num);	/* Cap previous EXP chars */
	}
	while (num--) {
		if (upper(&linebuf[curchar])) {
			modify();
			makedirty(curline);
		}
		if (eolp()) {
			if (curline->l_next == NULL)
				break;
			SetLine(curline->l_next);
		} else
			curchar += 1;
	}
	if (restore)
		SetDot(&b);
}

void
CapWord()
{
	register int	num,
			restore = NO;
	Bufpos	b;

	DOTsave(&b);
	num = arg_value();
	if (num < 0) {
		restore = YES;
		num = -num;
		b_word(num);		/* Cap previous EXP words */
	}
	while (num--) {
		to_word(1);	/* Go to the beginning of the next word. */
		if (eobp())
			break;
		if (upper(&linebuf[curchar])) {
			modify();
			makedirty(curline);
		}
		curchar += 1;
		while (!eolp() && jisword(linebuf[curchar])) {
			if (lower(&linebuf[curchar])) {
				modify();
				makedirty(curline);
			}
			curchar += 1;
		}
	}
	if (restore)
		SetDot(&b);
}

private void
case_word(up)
bool	up;
{
	Bufpos	before;

	DOTsave(&before);
	ForWord();	/* this'll go backward if negative argument */
	case_reg(before.p_line, before.p_char, curline, curchar, up);
}

/* Convert *p to upper case.  Return TRUE iff it was changed. */

private bool
upper(p)
register char	*p;
{
	if (jislower(*p & CHARMASK)) {
		*p = CharUpcase(*p & CHARMASK);
		return YES;
	}
	return NO;
}

/* Convert *p to lower case.  Return TRUE iff it was changed. */

private bool
lower(p)
char	*p;
{
	int c = *p & CHARMASK;

	if (jisupper(c)) {
#ifdef	ASCII7
		*p = jtolower(c);
#else	/* !ASCII7 */
#ifdef	IBMPC
		if (c <= 127)
		    c += ' ';
		else {
			switch (c) {
			case 142: c = 132; break;		/* Ae */
			case 153: c = 148; break;		/* Oe */
			case 154: c = 129; break;		/* Ue */
			}
		}
#endif	/* IBMPC */
#ifdef	MAC
		if (c <= 127)
		    c += ' ';
		else {
			int n;

			for(n = 128; ; n++) {
				if (n > 255)
					return NO;
				if ((CharUpcase(n) == c) && jislower(n)) {
					c = n;
					break;
				}
			}
		}
#endif	/* MAC */
		*p = c;
#endif	/* !ASCII7 */
		return YES;
	}
	return NO;
}

#ifndef	ASCII7
char
jtolower(c)
char	c;
{
    if (jislower(c))
	(void) lower(&c);
    return c;
}
#endif	/*!ASCII7*/

private void
case_reg(line1, char1, line2, char2, up)
Line	*line1,
	*line2;
int	char1,
	char2;
bool	up;
{
	(void) fixorder(&line1, &char1, &line2, &char2);
	DotTo(line1, char1);

	for (;;) {
		if (curline == line2 && curchar == char2)
			break;
		if (!eolp())
			if ((up) ? upper(&linebuf[curchar]) : lower(&linebuf[curchar])) {
				makedirty(curline);
				modify();
			}
		f_char(1);
	}
}

void
CasRegLower()
{
	CaseReg(FALSE);
}

void
CasRegUpper()
{
	CaseReg(TRUE);
}

private void
CaseReg(up)
bool	up;
{
	register Mark	*mp = CurMark();
	Bufpos	savedot;

	DOTsave(&savedot);
	case_reg(curline, curchar, mp->m_line, mp->m_char, up);
	SetDot(&savedot);
}

void
UppWord()
{
	case_word(TRUE);
}

void
LowWord()
{
	case_word(FALSE);
}
