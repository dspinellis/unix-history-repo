/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * prment() removes database entries corresponding to key.
 */
#include <stdio.h>
#include "null.h"
#include "path.h"
#include "pdb.h"

void
prment(key, pdbp)
	char *key;			/* key string */
	PDB *pdbp;			/* database stream */
{
	char *pbfndkey();		/* find key */
	int pgetent();			/* load next entry into buffer */
	int pputent();			/* write buffer to database */
	void rewindpdb();		/* rewind database */

	rewindpdb(pdbp);
	while (pgetent(pdbp) != EOF)
		{
		if (pbfndkey(key) != NULL)
			continue;
		pputent(pdbp);
		}
}
