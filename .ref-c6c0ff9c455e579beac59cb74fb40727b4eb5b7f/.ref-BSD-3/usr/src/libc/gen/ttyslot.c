/*
 * Return the number of the slot in the utmp file
 * corresponding to the current user: try for file 0, 1, 2.
 * Definition is the line number in the /etc/ttys file.
 */


char	*ttyname();
char	*getttys();
char	*rindex();
static	char	ttys[]	= "/etc/ttys";

#define	NULL	0

ttyslot()
{
	register char *tp, *p;
	register s, tf;

	if ((tp=ttyname(0))==NULL && (tp=ttyname(1))==NULL && (tp=ttyname(2))==NULL)
		return(0);
	if ((p = rindex(tp, '/')) == NULL)
		p = tp;
	else
		p++;
	if ((tf=open(ttys, 0)) < 0)
		return(0);
	s = 0;
	while (tp = getttys(tf)) {
		s++;
		if (strcmp(p, tp)==0) {
			close(tf);
			return(s);
		}
	}
	close(tf);
	return(0);
}

static char *
getttys(f)
{
	static char line[32];
	register char *lp;

	lp = line;
	for (;;) {
		if (read(f, lp, 1) != 1)
			return(NULL);
		if (*lp =='\n') {
			*lp = '\0';
			return(line+2);
		}
		if (lp >= &line[32])
			return(line+2);
		lp++;
	}
}
