/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * prmkey() removes each instance of key in a database.
 */
#include <stdio.h>
#include "path.h"
#include "pdb.h"

void
prmkey(key, pdbp)
	char *key;			/* key string */
	PDB *pdbp;			/* database stream */
{
	int pgetent();			/* loasd next entry into buffer */
	int pputent();			/* write buffer to database */
	void pbrmkey();			/* remove key */
	void rewindpdb();		/* rewind database */

	rewindpdb(pdbp);
	while (pgetent(pdbp) != EOF)
		{
		pbrmkey(key);
		pputent(pdbp);
		}
}
