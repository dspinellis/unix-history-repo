/*
 * ttycap - routines for dealing with the teletype capability data base
 * Bill Joy UCB September 25, 1977
 */


tgetmodes(tmvec)
	int *tmvec;
{
	register int i;
	register char *bp;
	extern char *tbuf;

	bp = tskip(tbuf, 1);
	i = 0;
	while (*bp && *bp >= '0' && *bp <= '9')
		i =<< 3, i =| *bp++ - '0';
	tmvec[0] = i;
	if (*bp == ':')
		bp++;
	i = 0;
	while (*bp && *bp >= '0' && *bp <= '9')
		i =<< 3, i =| *bp++ - '0';
	tmvec[1] = i;
}
