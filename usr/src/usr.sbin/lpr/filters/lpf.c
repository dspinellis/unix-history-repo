/*	lpf.c	4.4	83/01/05	*/
/*
 * lpf -- Line printer filter: handles underlines for those printers/
 *	  device drivers that won't
 */

#include <stdio.h>
#include <signal.h>

#define	LINELN	132

char	linebuf[LINELN+2];
int	ov;
int	ff;
char	ovbuf[LINELN];

main()
{
	extern char _sobuf[BUFSIZ];

	setbuf(stdout, _sobuf);
	while (getline())
		putline();
	fflush(stdout);
	if (ferror(stdout))
		exit(1);
	exit(0);
}

getline()
{
	register int col, maxcol, c;

	ov = 0;
	for (col = 0; col < LINELN; col++) {
		linebuf[col] = ' ';
		ovbuf[col] = 0;
	}
	col = 0;
	maxcol = 0;
	for (;;) switch (c = getchar()) {

	case EOF:
		return(0);

	default:
		if (c >= ' ') {
			if (col < LINELN) {
				if (linebuf[col] != ' ') {
					ov++;
					ovbuf[col] = c;
				} else
					linebuf[col] = c;
				if (++col > maxcol)
					maxcol = col;
			}
		}
		continue;

	case ' ':
		col++;
		continue;

	case '\t':
		col = (col|07) + 1;
		if (col > maxcol)
			maxcol = col;
		continue;

	case '\r':
		col = 0;
		continue;

	case '\f':
		ff = 1;		/* force form feed */
	case '\n':
		if (maxcol >= LINELN)
			maxcol = LINELN;
		linebuf[maxcol] = 0;
		return(1);

	case '\b':
		if (col > 0)
			col--;
		continue;
	}
}

putline()
{
	register char c, *lp;

	lp = linebuf;
	while (c = *lp++)
		output(c);
	if (ov) {
		putchar('\r');
		lp = ovbuf;
		while (ov) {
			if (c = *lp++) {
				output(c);
				ov--;
			} else
				output(' ');
		}
	}
	putchar('\n');
	if (ff) {
		ff = 0;
		putchar('\f');
	}
	if (ferror(stdout))
		exit(1);
}

output(c)
register char c;
{

	if (c == -1)
		return;
	c &= 0177;
	if (c == 0177)
		putchar('^'), c = '?';
	if (c == 033)
		c = '$';
	if (c < ' ') switch (c) {

	case '\n':
		break;

	case '\f':
	case '\b':
	case '\t':
	case '\r':
		break;

	default:
		putchar('^');
		c |= 0100;
	}
	putchar(c);
}
