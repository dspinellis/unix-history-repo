/* Copyright (c) 1979 Regents of the University of California */
#include <sgtty.h>
#include <ctype.h>

/*
 * The following array gives the number of tens of milliseconds per
 * character for each speed as returned by gtty.  Thus since 300
 * baud returns a 7, there are 33.3 milliseconds per char at 300 baud.
 */
static
short	tmspc10[] = {
	0, 2000, 1333, 909, 743, 666, 500, 333, 166, 83, 55, 41, 20, 10
};

short	ospeed;
char	PC;

/*
 * Put the character string cp out, with padding.
 * The number of affected lines is affcnt, and the routine
 * used to output one character is outc.
 */
tputs(cp, affcnt, outc)
	register char *cp;
	int affcnt;
	int (*outc)();
{
	register int i = 0;
	register int mspc10;

	if (cp == 0)
		return;

	/*
	 * Convert the number representing the delay.
	 */
	if (isdigit(*cp)) {
		do
			i = i * 10 + *cp++ - '0';
		while (isdigit(*cp));
	}
	i *= 10;
	if (*cp == '.') {
		cp++;
		if (isdigit(*cp))
			i += *cp - '0';
		/*
		 * Only one digit to the right of the decimal point.
		 */
		while (isdigit(*cp))
			cp++;
	}

	/*
	 * If the delay is followed by a `*', then
	 * multiply by the affected lines count.
	 */
	if (*cp == '*')
		cp++, i *= affcnt;

	/*
	 * The guts of the string.
	 */
	while (*cp)
		(*outc)(*cp++);

	/*
	 * If no delay needed, or output speed is
	 * not comprehensible, then don't try to delay.
	 */
	if (i == 0)
		return;
	if (ospeed <= 0 || ospeed >= (sizeof tmspc10 / sizeof tmspc10[0]))
		return;

	/*
	 * Round up by a half a character frame,
	 * and then do the delay.
	 * Too bad there are no user program accessible programmed delays.
	 * Transmitting pad characters slows many
	 * terminals down and also loads the system.
	 */
	mspc10 = tmspc10[ospeed];
	i += mspc10 / 2;
	for (i /= mspc10; i > 0; i--)
		(*outc)(PC);
}
