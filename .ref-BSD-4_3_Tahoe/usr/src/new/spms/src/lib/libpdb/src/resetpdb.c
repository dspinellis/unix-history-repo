/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * resetpdb() resets the current database stream CURPDB to pdbp. Also
 * resets buffer pointers.
 */
#include <stdio.h>
#include "path.h"
#include "pdb.h"

extern char *CURPBUF;			/* current database buffer */
extern char *FP;			/* next non-key field */
extern char *KP;			/* next key field */
extern PDB *CURPDB;			/* current database stream */

void
resetpdb(pdbp)
	PDB *pdbp;			/* database stream */
{
	CURPDB = pdbp;
	CURPBUF = FP = KP = pdbp->pbuf;
}
