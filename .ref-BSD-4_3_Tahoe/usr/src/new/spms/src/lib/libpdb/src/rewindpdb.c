/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * rewindpdb() rewinds a database unless it has been opened for writing
 * or appending only.
 */
#include <stdio.h>
#include "path.h"
#include "pdb.h"
#include "system.h"

void
rewindpdb(pdbp)
	PDB *pdbp;			/* database stream */
{
	FILE *fopen();			/* open file stream */

	if ((pdbp->flag&_PREAD) == 0 || (pdbp->flag&_PERR) != 0)
		return;
	if ((pdbp->flag&_PACCESS) == (_PREAD|_PWRITE) && (pdbp->flag&_PUPDATE) != 0)
		{
		fclose(pdbp->fp);
		fclose(pdbp->tfp);
		RENAME(pdbp->tpath, pdbp->path);
		pdbp->fp = fopen(pdbp->path, "r");
		pdbp->tfp = fopen(pdbp->tpath, "w");
		}
	else
		rewind(pdbp->fp);
	pdbp->flag &= ~(_PUPDATE|_PEOF);
}
