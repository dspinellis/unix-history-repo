/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * errpdb() prints the error message in pdbp->err, or ERRPDB if pdbp is
 * null. Clears error message and returns 1.
 */
#include <stdio.h>
#include "path.h"
#include "pdb.h"

errpdb(pdbp)
	PDB *pdbp;			/* database stream */
{
	extern char PDBERR[];		/* database error message buffer */
	extern char *PGN;		/* program name */

	if (pdbp == NULL)
		{
		if (*PDBERR != '\0')
			{
			fprintf(stderr, "%s: %s\n", PGN, PDBERR);
			*PDBERR = '\0';
			}
		}
	else	{
		if (*pdbp->perr != '\0')
			{
			fprintf(stderr, "%s: %s\n", PGN, pdbp->perr);
			*pdbp->perr = '\0';
			}
		}
	return(1);
}
