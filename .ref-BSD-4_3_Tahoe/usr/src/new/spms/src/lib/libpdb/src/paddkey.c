/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * paddkey() adds a newkey to each entry corresponding to key. The entry
 * is not changed if newkey already exists.
 */
#include <stdio.h>
#include "null.h"
#include "path.h"
#include "pdb.h"

void
paddkey(key, newkey, pdbp)
	char *key;			/* key string */
	char *newkey;			/* new key string */
	PDB *pdbp;			/* database stream */
{
	char *pbfndkey();		/* find key */
	int pbaddkey();			/* add key */
	int pgetent();			/* load next entry into buffer */
	int pputent();			/* write buffer to database */
	void rewindpdb();		/* rewind database */

	rewindpdb(pdbp);
	while (pgetent(pdbp) != EOF)
		{
		if (pbfndkey(key) != NULL)
			pbaddkey(newkey);
		pputent(pdbp);
		}
}
