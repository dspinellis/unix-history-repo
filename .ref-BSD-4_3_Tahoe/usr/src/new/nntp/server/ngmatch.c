#ifndef lint
static	char	*sccsid = "@(#)ngmatch.c	1.2	(Berkeley) 10/15/87";
#endif

#include "common.h"

/*
 * nntpngmatch -- match a list of newsgroup specifiers with a list of
 * given newsgroups.  A pointer to the routine which determines a match is
 * also given.  This allows us to do regular expression handling for RFC
 * 977's NEWNEWS, and more efficient "strncmps" for the access file, which
 * must be checked often.
 * 
 * This is NOT the same routine as ngmatch in the news software.  Pity.
 * 
 *	Parameters:	"nglist" is the list of group specifiers (limited
 *			regexp) to match against.
 *			"ngcount" is the number of groups in nglist.
 *			"matchlist" is the list of newsgroups to match against.
 *			"matchcount" is number of groups in matchlist.
 *
 *	Returns:	1 if the named newsgroup is in the list.
 *			0 otherwise.
 */

ngmatch(func, dflt, ngspec, ngspeccount, matchlist, matchcount)
	int		(*func)();
	int		dflt;
	char		**ngspec;
	int		ngspeccount;
	char		**matchlist;
	int		matchcount;
{
	register int	i, j;
	register int	match;
	register char	*cp;

	if (ngspeccount == 0)
		return (1);

	match = dflt;

	for (i = 0; i < matchcount; ++i) {
		if (cp = index(matchlist[i], '/'))
			*cp = '\0';
		for (j = 0; j < ngspeccount; ++j) {
			if (ngspec[j][0] == '!') {	/* Handle negation */
				if ((*func)(ngspec[j]+1, matchlist[i])) {
					match = 0;
				}
			} else {
				if ((*func)(ngspec[j], matchlist[i])) {
					match = 1;
				}
			}
		}
	}

	return (match);
}


/*
 * restreql -- A small regular expression string equivalnce routine.
 * Thanks and a tip of the hat to Nick Lai, <lai@shadow.berkeley.edu>
 * for this time saving device.
 *
 *	Parameters:	"w" is an asterisk-broadened regexp,
 *			"s" is a non-regexp string.
 *	Returns:	1 if match, 0 otherwise.
 *
 *	Side effects:	None.
 */

restreql(w, s)
	register char *w;
	register char *s;
{

	while (*s && *w) {
		switch (*w) {
			case '*':
				for (w++; *s; s++)
					if (restreql(w, s))
						return 1;
				break;
			default:
				if (*w != *s)
					return 0;
				w++, s++;
				break;
		}
	}
	if (*s)
		return 0;
	while (*w)
		if (*w++ != '*')
			return 0;

	return 1;
}


/*
 * strneql -- see if s1 is equivalent to s2 up to the length of s1.
 * Return 1 if so, 0 otherwise.
 */

strneql(s1, s2)
	register char	*s1;
	register char	*s2;
{
	return (!strncmp(s1, s2, strlen(s1)));
}
