/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * prmstring() removes id=string from each entry corresponding to key.
 */
#include <stdio.h>
#include "null.h"
#include "path.h"
#include "pdb.h"

void
prmstring(key, id, pdbp)
	char *key;			/* key string */
	char *id;			/* string identifier */
	PDB *pdbp;			/* database stream */
{
	char *pbfndkey();		/* find key */
	int pgetent();			/* load next entry into buffer */
	int pputent();			/* write buffer to database */
	void pbrmstring();		/* remove string field */
	void rewindpdb();		/* rewind database */

	rewindpdb(pdbp);
	while (pgetent(pdbp) != EOF)
		{
		if (pbfndkey(key) != NULL)
			pbrmstring(id);
		pputent(pdbp);
		}
}
