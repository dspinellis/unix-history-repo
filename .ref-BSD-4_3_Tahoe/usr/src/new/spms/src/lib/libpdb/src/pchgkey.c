/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pchgkey() substitutes newkey for every occurrence of oldkey in a database.
 */
#include <stdio.h>
#include "path.h"
#include "pdb.h"

void
pchgkey(oldkey ,newkey, pdbp)
	char *oldkey;			/* old key string */
	char *newkey;			/* new key string */
	PDB *pdbp;			/* database stream */
{
	int pbchgkey();			/* change existing key */
	int pgetent();			/* load next entry into buffer */
	int pputent();			/* write buffer to database */
	void rewindpdb();		/* rewind database */

	rewindpdb(pdbp);
	while (pgetent(pdbp) != EOF)
		{
		pbchgkey(oldkey, newkey);
		pputent(pdbp);
		}
}
