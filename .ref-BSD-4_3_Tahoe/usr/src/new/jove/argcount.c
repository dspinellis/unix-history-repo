/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include <ctype.h>

#ifdef MAC
#	undef private
#	define private
#endif

#ifdef	LINT_ARGS
private	void
	gather_numeric_argument(int),
	quad_numeric_arg(void);
#else
private	void
	gather_numeric_argument(),
	quad_numeric_arg();
#endif

#ifdef MAC
#	undef private
#	define private static
#endif

private int	arg_supplied_p,
		arg_count;

int
arg_type()
{
	return arg_supplied_p;
}

void
set_is_an_arg(there_is)
{
	arg_supplied_p = there_is;
}

void
set_arg_value(n)
{
	arg_supplied_p = YES;
	arg_count = n;
}

void
negate_arg_value()
{
	arg_count = -arg_count;
}

void
clr_arg_value()
{
	arg_supplied_p = NO;
	arg_count = 1;
}

/* return whether there is currently a numeric argument */

int
is_an_arg()
{
	return (arg_supplied_p != NO);
}

/* return the numeric argument */

int
arg_value()
{
	return arg_count;
}

/* called by C-U to gather a numeric argument, either C-U's or digits,
   but not both */

void
TimesFour()
{
	quad_numeric_arg();
}

/* This initializes the numeric argument to 1 and starts multiplying
   by 4 (the magic number Stallman came up with).  It is an error to
   invoke quad_numeric_arg() interactively (via TimesFour()), because
   it uses the LastKeyStruck variable to know what character signals
   to multiply again (in the loop). */
private void
quad_numeric_arg()
{
	int	oldc = LastKeyStruck,
		newc,
		narg_count,
		slow;

	slow = 0;
	arg_supplied_p = YES;
	arg_count = 1;
	this_cmd = ARG_CMD;
	do {
		if ((narg_count = arg_count * 4) != 0)
			arg_count = narg_count;
		if (!slow)
			newc = waitchar(&slow);
		else
			newc = getch();
		if (isdigit(newc) || newc == '-') {
		     arg_supplied_p = NO;
		     gather_numeric_argument(newc);
		     return;
		}
		if (slow)
			message(key_strokes);
	} while (newc == oldc);
	Ungetc(newc);
}

private void
gather_numeric_argument(c)
{
	int	sign = 0;
	static int	digited;
	int	slow = 0;

	if (!isdigit(c) && c != '-')
		complain((char *) 0);
	if (arg_supplied_p == NO) {	/* if we just got here */
		arg_count = 0;	/* start over */
		digited = NO;
	} else if (arg_supplied_p == YES_NODIGIT) {
		sign = (arg_count < 0) ? -1 : 1;
		arg_count = 0;
	}

	if (!sign)
		sign = (arg_count < 0) ? -1 : 1;
	if (sign == -1)
		arg_count = -arg_count;
	if (c == '-') {
		sign = -sign;
		goto goread;
	}
	for (;;) {
		if (slow)
			message(key_strokes);
		if (isdigit(c)) {
			arg_count = (arg_count * 10) + (c - '0');
			digited = YES;
		} else {
			if (digited)
				arg_supplied_p = YES;
			else {
				arg_count = 1;
				if (arg_supplied_p == NO)
					arg_supplied_p = YES_NODIGIT;
			}
			arg_count *= sign;
			this_cmd = ARG_CMD;
			Ungetc(c);
			return;
		}
goread:		if (!slow)
			c = waitchar(&slow);
		else {
			add_mess(NullStr);
			c = getch();
		}
	}
}

void
Digit()
{
	gather_numeric_argument(LastKeyStruck);
}

void
Digit0()
{
	gather_numeric_argument('0');
}

void
Digit1()
{
	gather_numeric_argument('1');
}

void
Digit2()
{
	gather_numeric_argument('2');
}

void
Digit3()
{
	gather_numeric_argument('3');
}

void
Digit4()
{
	gather_numeric_argument('4');
}

void
Digit5()
{
	gather_numeric_argument('5');
}

void
Digit6()
{
	gather_numeric_argument('6');
}

void
Digit7()
{
	gather_numeric_argument('7');
}

void
Digit8()
{
	gather_numeric_argument('8');
}

void
Digit9()
{
	gather_numeric_argument('9');
}
