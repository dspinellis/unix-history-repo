/*
 * htmp - manipulation routines for "htmp" data base
 * Bill Joy UCB September 24, 1977
 * << get routines >>
 */
struct htmp {
	int	uid;
	char	home[28];
	int	ttytype;
} hentry;

char	htmp[]	"/etc/htmp";

hget(tty)
	char tty;
{
	register int hunit;

	hunit = open(htmp, 0);
	if (hunit < 0)
		goto bad;
	if (seek(hunit, tty * sizeof hentry, 0))
		goto bad;
	if (read(hunit, &hentry, sizeof hentry) != sizeof hentry) {
		hentry.uid = 0;
		hentry.home[0] = '/';
		hentry.home[1] = 0;
		hentry.ttytype = 'un';
	}
	close(hunit);
	return (0);
bad:
	close(hunit);
	return (-1);
}

hgethome()
{

	return (hentry.home);
}

hgetuid()
{

	return(hentry.uid);
}

hgettype()
{

	return(hentry.ttytype);
}

hsgettype()
{
	static char ttype[3];

	ttype[0] = hentry.ttytype & 0377;
	ttype[1] = (hentry.ttytype >> 8) & 0377;
	return (ttype);
}
