/* Copyright (c) 1979 Regents of the University of California */
#define	CTRL(c)	('c' & 037)

char	*UP;
char	*BC;

/*
 * Routine to perform cursor addressing.
 * CM is a string containing printf type escapes to allow
 * cursor addressing.  We start out ready to print the destination
 * line, and switch each time we print row or column.
 * The following escapes are defined for substituting row/column:
 *
 *	%d	as in printf
 *	%2	like %2d
 *	%3	like %3d
 *	%.	gives %c hacking special case characters
 *	%+x	like %c but adding x first
 *	%<xy	if value < x add y, else just use valueindexing)
 *	%r	reverses row/column
 *	%i	increments row/column (for one origin indexing)
 *	%%	gives %
 *
 * all other characters are ``self-inserting''.
 */
char *
tgoto(CM, destcol, destline)
	char *CM;
	int destcol, destline;
{
	static char result[16];
	static char added[10];
	char *cp = CM;
	register char *dp = result;
	register int c;
	int oncol = 0;
	register int which;

	if (cp == 0) {
toohard:
		/*
		 * ``We don't do that under BOZO's big top''
		 */
		strcpy(result, "OOPS");
		return (result);
	}
	added[0] = 0;
	while (c = *cp++) {
		if (c != '%') {
			*dp++ = c;
			continue;
		}
		which = oncol ? destcol : destline;
		switch (c = *cp++) {

		case 'n':
			destcol ^= 0140;
			destline ^= 0140;
			continue;

		case 'd':
			if (which < 10)
				goto one;
			if (which < 100)
				goto two;
			/* fall into... */

		case '3':
			*dp++ = (which / 100) | '0';
			which %= 100;
			/* fall into... */

		case '2':
two:	
			*dp++ = which / 10 | '0';
one:
			*dp++ = which % 10 | '0';
			oncol = 1 - oncol;
			continue;

		case '<':
			if (which < *dp++)
				which += *dp++;
			else
				dp++;
			goto casedot;

		case '+':
			which += *cp++;
			/* fall into... */

		case '.':
casedot:
			if (!UP)
				goto toohard;
			if (which == 0 || which == CTRL(d) || which == '\t' || which == '\n') {
				do {
					strcat(added, oncol ? (BC ? BC : "\b") : UP);
					which++;
				} while (which == '\n');
			}
			*dp++ = which;
			/* fall into... */

		case 'r':
			oncol = 1 - oncol;
			continue;

		case 'i':
			destcol++;
			destline++;
			continue;

		case '%':
			*dp++ = c;
			continue;

		default:
			goto toohard;
		}
	}
	strcpy(dp, added);
	return (result);
}
