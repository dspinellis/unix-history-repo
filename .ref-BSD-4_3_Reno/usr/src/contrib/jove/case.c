/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include "disp.h"
#include "ctype.h"

private	int
# if !(defined(IBMPC) || defined(MAC))
	lower proto((char *)),
# endif
	upper proto((char *));

private void
	CaseReg proto((int up)),
	case_reg proto((struct line *line1,int char1,struct line *line2,int char2,int up)),
	case_word proto((int up));

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
			if (curline->l_next == 0)
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
		while (!eolp() && isword(linebuf[curchar])) {
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
int	up;
{
	Bufpos	before;

	DOTsave(&before);
	ForWord();	/* this'll go backward if negative argument */
	case_reg(before.p_line, before.p_char, curline, curchar, up);
}

private int
upper(c)
register char	*c;
{
	if (islower(*c)) {
#if !defined(ASCII)			/* check for IBM extended character set */
		if (*c <= 127)
#endif /* ASCII */
		*c -= ' ';
#if defined(IBMPC)			/* ... and change Umlaute	*/
		else
		   switch (*c) {
		     case 129: *c = 154; break;		/* ue */
		     case 132: *c = 142; break;		/* ae */
		     case 148: *c = 153; break;		/* oe */
		   }
#endif /* IBMPC */
#if defined(MAC)
		else *c = CaseEquiv[*c];
#endif
		return 1;
	}
	return 0;
}

#if !(defined(IBMPC) || defined(MAC))
private
#endif
int
lower(c)
char	*c;
{
	if (isupper(*c)) {
#if !defined(ASCII)
		if (*c <= 127)
#endif /* ASCII */
		*c += ' ';
#if defined(IBMPC)
		else
		   switch (*c) {
		     case 142: *c = 132; break;		/* Ae */
		     case 153: *c = 148; break;		/* Oe */
		     case 154: *c = 129; break;		/* Ue */
		   }
#endif /* IBMPC */
#if defined(MAC)
		else {
			int n;

			for(n = 128; n < 256; n++) {
				if((CaseEquiv[n] == *c) && islower(n)) {
					*c = n;
					break;
				}
			}
			if(n > 255) return(0);
		}
#endif /* MAC */
		return 1;
	}
	return 0;
}

private void
case_reg(line1, char1, line2, char2, up)	
Line	*line1,
	*line2;
int	char1,
	char2,
	up;
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
	CaseReg(0);
}

void
CasRegUpper()
{
	CaseReg(1);
}

private void
CaseReg(up)
int	up;
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
	case_word(1);
}

void
LowWord()
{
	case_word(0);
}
