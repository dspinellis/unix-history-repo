/* @(#)timezone.c	4.1 (Berkeley) 12/21/80 */
/*
 * The arguments are the number of minutes of time
 * you are westward from Greenwich and whether DST is in effect.
 * It returns a string
 * giving the name of the local timezone.
 *
 * Sorry, I don't know all the names.
 */

static struct zone {
	int	offset;
	char	*stdzone;
	char	*dlzone;
} zonetab[] = {
	4*60, "AST", "ADT",		/* Atlantic */
	5*60, "EST", "EDT",		/* Eastern */
	6*60, "CST", "CDT",		/* Central */
	7*60, "MST", "MDT",		/* Mountain */
	8*60, "PST", "PDT",		/* Pacific */
	0, "GMT", 0,			/* Greenwich */
	-1
};

char *timezone(zone, dst)
{
	register struct zone *zp;
	static char czone[10];
	char *sign;

	for (zp=zonetab; zp->offset!=-1; zp++)
		if (zp->offset==zone) {
			if (dst && zp->dlzone)
				return(zp->dlzone);
			if (!dst && zp->stdzone)
				return(zp->stdzone);
		}
	if (zone<0) {
		zone = -zone;
		sign = "+";
	} else
		sign = "-";
	sprintf(czone, "GMT%s%d:%02d", sign, zone/60, zone%60);
	return(czone);
}
