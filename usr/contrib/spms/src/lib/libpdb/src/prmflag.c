/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * prmflag() removes flag from each entry corresponding to key.
 */
#include <stdio.h>
#include "null.h"
#include "path.h"
#include "pdb.h"

void
prmflag(key, flag, pdbp)
	char *key;			/* key string */
	char *flag;			/* flag string */
	PDB *pdbp;			/* database stream */
{
	char *pbfndkey();		/* find key */
	int pgetent();			/* load next entry into buffer */
	int pputent();			/* write buffer to database */
	void pbrmflag();		/* remove flag field */
	void rewindpdb();		/* rewind database */

	rewindpdb(pdbp);
	while (pgetent(pdbp) != EOF)
		{
		if (pbfndkey(key) != NULL)
			pbrmflag(flag);
		pputent(pdbp);
		}
}
