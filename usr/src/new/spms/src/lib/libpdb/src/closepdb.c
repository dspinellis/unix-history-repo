/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * closepdb() closes a database Returns 0 normally, 1 if database error.
 */
#include <stdio.h>
#include "path.h"
#include "pdb.h"
#include "system.h"

closepdb(pdbp)
	PDB *pdbp;			/* database description pointer	*/
{
	int errpdb();			/* print database error message */
	int errstat = 0;		/* database error status */

	fclose(pdbp->fp);
	if ((pdbp->flag&_PACCESS) == (_PREAD|_PWRITE))
		{
		fclose(pdbp->tfp);
		if ((pdbp->flag&_PERR) == 0 && (pdbp->flag&_PUPDATE) != 0)
			{
			RENAME(pdbp->tpath, pdbp->path);
			}
		else	{
			unlink(pdbp->tpath);
			}
		}
	else if ((pdbp->flag&(_PWRITE|_PAPPEND)) != 0)
		unlink(pdbp->tpath);
	if ((pdbp->flag&_PERR) != 0)
		{
		errpdb(pdbp);
		errstat = 1;
		}
	free((char *) pdbp);
	return(errstat);
}
