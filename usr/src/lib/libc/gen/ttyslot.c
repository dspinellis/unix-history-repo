/* @(#)ttyslot.c	4.4 (Berkeley) %G% */

/*
 * Return the number of the slot in the utmp file
 * corresponding to the current user: try for file 0, 1, 2.
 * Definition is the line number in the /etc/ttys file.
 */
#include <ttyent.h>

char	*ttyname();
char	*rindex();

#define	NULL	0

ttyslot()
{
	register struct ttyent *ty;
	register char *tp, *p;
	register s;

	if ((tp = ttyname(0)) == NULL &&
	    (tp = ttyname(1)) == NULL &&
	    (tp = ttyname(2)) == NULL)
		return(0);
	if ((p = rindex(tp, '/')) == NULL)
		p = tp;
	else
		p++;
	setttyent();
	s = 0;
	while ((ty = getttyent()) != NULL) {
		s++;
		if (strcmp(ty->ty_name, p) == 0) {
			endttyent();
			return (s);
		}
	}
	endttyent();
	return (0);
}
