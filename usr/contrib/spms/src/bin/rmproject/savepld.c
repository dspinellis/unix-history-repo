/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */
#include <stdio.h>
#include "path.h"
#include "pld.h"
#include "system.h"

#define PMODE 0644			/* file access mode */

extern int FORCE;			/* brute force approach */
static char *tpld = "/tmp/...XXXXXX";	/* temporary project link directory */

/*
 * restorpld() moves a project link directory from /tmp to pathname.
 */
void
restorpld(pathname)
	char *pathname;			/* project link directory pathname */
{
	char *pathcat();		/* pathname concatenation */
	char pldpathname[PATHSIZE];	/* project link directory pathname */
	int fastcopy();			/* fast file copy */

	pathcat(pldpathname, pathname, PLDNAME);
	fastcopy(tpld, pldpathname);
	unlink(tpld);
}



/*
 * savepld() copies a project link directory from pathname to /tmp.
 * Returns 0 if successful, otherwise -1.
 */
savepld(pathname)
	char *pathname;			/* project link directory pathname */
{
	char *mktemp();			/* make temporary file name */
	char *pathcat();		/* pathname concatenation */
	char pldpathname[PATHSIZE];	/* project link directory pathname */

	pathcat(pldpathname, pathname, PLDNAME);
	return(fastcopy(pldpathname, mktemp(tpld)));
}



/*
 * unsavepld() removes the saved project link directory from /tmp.
 */
void
unsavepld()
{
	unlink(tpld);
}



/*
 * fastcopy() copies f1 to f2. Returns 0 if successful, otherwise -1.
 */
fastcopy(f1, f2)
	char *f1;			/* from file */
	char *f2;			/* to file */
{
	register int ifd;		/* input file descriptor */
	register int ofd;		/* output file descriptor */
	register int n;			/* byte count */
	char buf[BUFSIZ];		/* I/O buffer */

	if ((ifd = OPEN(f1, O_RDONLY, PMODE)) == -1)
		if (FORCE)
			return(0);
		else	{
			pperror(f1);
			return(-1);
			}
	if ((ofd = CREATE(f2, O_WRONLY, PMODE)) == -1)
		if (FORCE)
			return(0);
		else	{
			pperror(f2);
			return(-1);
			}
	while ((n = read(ifd, buf, BUFSIZ)) > 0)
		if (write(ofd, buf, n) != n)
			if (FORCE)
				return(0);
			else	{
				pperror("");
				return(-1);
				}
	close(ifd);
	close(ofd);
	return(0);
}
