/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pputflag() adds flag to each entry corresponding to key. The entry is
 * not changed if flag already exists. If key not found, a new entry
 * is added to the database.
 */
#include <stdio.h>
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "yesno.h"

void
pputflag(key, flag, pdbp)
	char *key;			/* key string */
	char *flag;			/* flag string */
	PDB *pdbp;			/* database stream */
{
	char *pbfndkey();		/* find key */
	int foundkey;			/* found key flag */
	int pbaddflag();		/* add flag field */
	int pbaddkey();			/* add key */
	int pgetent();			/* load next entry into buffer */
	int pputent();			/* write buffer to database */
	void pbclear();			/* clear buffer */
	void rewindpdb();		/* rewind database */

	rewindpdb(pdbp);
	foundkey = NO;
	while (pgetent(pdbp) != EOF)
		{
		if (pbfndkey(key) != NULL)
			{		/* key exists */
			pbaddflag(flag);
			foundkey = YES;
			}
		pputent(pdbp);
		}
	if (foundkey == NO)
		{			/* new entry */
		pbclear();
		pbaddkey(key);
		pbaddflag(flag);
		pputent(pdbp);
		}
}
