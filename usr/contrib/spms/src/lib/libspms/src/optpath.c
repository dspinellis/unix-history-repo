/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * optpath() condenses a pathname by eliminating adjacent separator
 * characters, and current and parent directory names. If optpath()
 * encounters a parent directory, it backtracks to eliminate the
 * previous directory. If the beginning of the pathname is reached
 * during backtracking, then if the pathname is absolute, the parent
 * directory is purged, otherwise it is shifted to the beginning of
 * pathname. Special care is taken not to clobber a shifted parent
 * by using a guard pointer. Returns pathname.
 */
#include "path.h"

#define absolute_path	(*pathname == _RDIRC)

static char parentdir[] = PARENTDIR;	/* parent directory name */

char *
optpath(pathname)
	register char *pathname;	/* pathname to be optimized */
{
	register char *bp;		/* back pathname pointer */
	register char *fp;		/* forward pathname pointer */
	register char *up;		/* pathname update guard pointer */
	char p1;			/* 1st parent directory character */
	char p2;			/* 2nd parent directory character */

	p1 = parentdir[0];
	p2 = parentdir[1];

	bp = fp = up = pathname;

	/* elimination of initial "./" causes no harmful side-effects */
	if (fp[0] == _CDIRC && fp[1] == _PSC) fp += 2;

	while (*fp != '\0')
		if (fp[0] == _PSC)
			if (fp[1] == _PSC || fp[1] == '\0')
				fp += 1;	/* "//" or trailing `/' */
			else if (fp[1]==_CDIRC && (fp[2]==_PSC || fp[2]=='\0'))
				fp += 2;	/* `.' */
			else if ((fp[1] == p1 && fp[2] == p2) &&
				 (fp[3] == _PSC || fp[3] == '\0'))
				{	/* ".." (don't backtrack over a "..") */
				if (absolute_path || 
				   (bp > up && bp-2 < pathname) ||
				   (bp > up && (bp[-2] != p1 || bp[-1] != p2)))
					{
					while (bp > up && *--bp != _PSC)
						continue;
					}
				else	{
					/* don't clobber separator character */
					if (bp[0] == _PSC) bp++;
					bp[0] = fp[1];
					bp[1] = fp[2];
					bp[2] = fp[3];
					up = bp += 2;
					}
				fp += 3;
				}
			else	{
				*bp++ = *fp++;
				}
		else	{
			*bp++ = *fp++;
			}
	if (bp == pathname && *pathname != '\0')
		*bp++ = (absolute_path) ? _RDIRC : _CDIRC;
	*bp = '\0';
	return(pathname);
}
