/*		lpf.c	4.5	83/02/10
 * 	filter which reads the output of nroff and converts lines
 *	with ^H's to overwritten lines.  Thus this works like 'ul'
 *	but is much better: it can handle more than 2 overwrites
 *	and it is written with some style.
 *	modified by kls to use register references instead of arrays
 *	to try to gain a little speed.
 */
#include <stdio.h>
#include <signal.h>

#define MAXWIDTH  132
#define MAXREP    10

char	buf[MAXREP][MAXWIDTH];
int	maxcol[MAXREP] = {-1};

main()
{
	register FILE *p = stdin, *o = stdout;
	register int i, col;
	register char *cp;
	int done, linedone, maxrep;
	char ch, *limit;

	for (cp = buf[0], limit = buf[MAXREP]; cp < limit; *cp++ = ' ');
	done = 0;
	
	while (!done) {
		col = 0;
		maxrep = 0;
		linedone = 0;
		while (!linedone) {
			switch (ch = getc(p)) {
			case EOF:
				linedone = done = 1;
				ch = '\n';
				break;

			case '\031':
				fflush(stdout);
				kill(getpid(), SIGSTOP);
				break;

			case '\f':
			case '\n':
				linedone = 1;
				break;

			case '\b':
				if (col-- < 0)
					col = 0;
				break;

			case '\r':
				col = 0;
				break;

			case '\t':
				col = (col | 07) + 1;
				break;

			default:
				if (col >= MAXWIDTH)
					break;
				cp = &buf[0][col];
				for (i = 0; i < MAXREP; i++) {
					if (i > maxrep)
						maxrep = i;
					if (*cp == ' ') {
						*cp = ch;
						if (col > maxcol[i])
							maxcol[i] = col;
						break;
					}
					cp += MAXWIDTH;
				}
				col++;
				break;
			}
		}

		/* print out lines */
		for (i = 0; i <= maxrep; i++) {
			for (cp = buf[i], limit = cp+maxcol[i]; cp <= limit;) {
				putc(*cp, o);
				*cp++ = ' ';
			}
			if (i < maxrep)
				putc('\r', o);
			else
				putc(ch, o);
			maxcol[i] = -1;
		}
	}
}
