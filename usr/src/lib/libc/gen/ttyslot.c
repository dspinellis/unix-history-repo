/* @(#)ttyslot.c	4.2 (Berkeley) %G% */

/*
 * Return the number of the slot in the utmp file
 * corresponding to the current user: try for file 0, 1, 2.
 * Definition is the line number in the /etc/ttys file.
 */
#include <sys/file.h>

char	*ttyname();
char	*getttys();
char	*rindex();
static	char	ttys[]	= "/etc/ttys";

#define	NULL	0

ttyslot()
{
	register char *tp, *p;
	register s, tf;

	if ((tp = ttyname(0)) == NULL &&
	    (tp = ttyname(1)) == NULL &&
	    (tp = ttyname(2)) == NULL)
		return(0);
	if ((p = rindex(tp, '/')) == NULL)
		p = tp;
	else
		p++;
	if ((tf = open(ttys, O_RDONLY)) < 0)
		return (0);
	s = 0;
	while (tp = getttys(tf)) {
		s++;
		if (strcmp(p, tp) == 0) {
			close(tf);
			return (s);
		}
	}
	close(tf);
	return (0);
}

#define	BUFSIZ	1024

static char *
getttys(f)
{
	static char buf[BUFSIZ + 1], *next = &buf[BUFSIZ + 1];
	register char *lp;
	char *start;

	for (;;) {
		if (next >= &buf[BUFSIZ]) {
			int n = read(f, buf, BUFSIZ);

			if (n <= 0)
				return (NULL);
			buf[n] = '\0';
			next = &buf[0];
		}
		for (lp = next; *lp && *lp != '\n'; lp++)
			;
		if (*lp == '\n') {
			*lp++ = '\0';
			start = next;
			next = lp;
			return (start + 2);
		}
		lseek(f, next - lp, L_INCR);
		next = lp;
	}
}
