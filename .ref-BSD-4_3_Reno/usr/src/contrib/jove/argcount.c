/***************************************************************************
 * This program is Copyright (C) 1986, 1987, 1988 by Jonathan Payne.  JOVE *
 * is provided to you without charge, and with no warranty.  You may give  *
 * away copies of JOVE, including sources, provided that this notice is    *
 * included in all the files.                                              *
 ***************************************************************************/

#include "jove.h"
#include <ctype.h>

private	void
	gather_numeric_argument proto((int)),
	quad_numeric_arg proto((void));

int	arg_supplied_p,
	arg_count;

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

	slow = NO;
	arg_supplied_p = YES;
	arg_count = 1;
	this_cmd = ARG_CMD;
	do {
		if ((narg_count = arg_count * 4) != 0)
			arg_count = narg_count;
		newc = waitchar(&slow);
		if (isdigit(newc) || newc == '-') {
		     arg_supplied_p = NO;
		     gather_numeric_argument(newc);
		     return;
		}
	} while (newc == oldc);
	Ungetc(newc);
}

private void
gather_numeric_argument(c)
	int	c;
{
	int	sign = 0;
	static int	digited;
	int	slow = NO;

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
goread:		c = waitchar(&slow);
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
