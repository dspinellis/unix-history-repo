/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pputent() writes buffer pdbp->pbuf to a database. Returns integer NO if
 * write error or buffer exceeded, otherwise YES.
 */ 
#include "stdio.h"
#include "path.h"
#include "pdb.h"
#include "yesno.h"

pputent(pdbp)
	register PDB *pdbp;		/* database stream */
{
	register char *bp;		/* buffer pointer */
	register FILE *fp;		/* file stream */
	register int i;			/* buffer counter */
	char *sprintf();		/* print output to string */

	if ((pdbp->flag&_PERR) != 0)
		return(NO);
	if ((pdbp->flag&_PACCESS) == (_PREAD|_PWRITE))
		fp = pdbp->tfp;
	else if ((pdbp->flag&(_PWRITE|_PAPPEND)) != 0)
		fp = pdbp->fp;
	else	{
		sprintf(pdbp->perr, "%s read access only", pdbp->path);
		pdbp->flag |= _PERR;
		return(NO);
		}
	i = 0;
	bp = pdbp->pbuf;
	pdbp->flag |= _PUPDATE;

	while (i < PBUFSIZE && *bp++ != '\0')
		putc(pdbp->pbuf[i++], fp);
	if (putc('\n', fp) == EOF)
		{
		sprintf(pdbp->perr, "%s write error", pdbp->path);
		pdbp->flag |= _PERR;
		return(NO);
		}
	if (i >= PBUFSIZE)
		{
		sprintf(pdbp->perr, "%s database buffer exceeded", pdbp->path);
		pdbp->flag |= _PERR;
		return(NO);
		}
	return(YES);
}
