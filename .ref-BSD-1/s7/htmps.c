/*
 * htmp - manipulation routines for "htmp" data base
 * Bill Joy UCB September 24, 1977
 * << set routines >>
 */
struct htmp {
	int	uid;
	char	home[28];
	int	ttytype;
} hentry;

char	htmp[];

hput(tty)
	char tty;
{
	register int hunit;

	hunit = open(htmp, 1);
	if (hunit < 0)
		return (-1);
	seek(hunit, tty * sizeof hentry, 0);
	if (write(hunit, &hentry, sizeof hentry) != sizeof hentry)
		goto bad;
	close(hunit);
	return (0);
bad:
	close(hunit);
	return (-1);
}

hsethome(cp)
	char *cp;
{
	register int i;

	for (i = 0; i < 27 && cp[i]; i++)
		hentry.home[i] = cp[i];
	hentry.home[i] = 0;
}

hsetuid(uid)
	int uid;
{

	hentry.uid = uid;
}

hsettype(type)
	int type;
{

	hentry.ttytype = type;
}
