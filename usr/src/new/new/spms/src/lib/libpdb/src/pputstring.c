/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pputstring() adds id=string to each entry corresponding to key. If id
 * already exists, the corresponding string is replaced by string. If
 * key not found, a new entry is added to the database.
 */
#include <stdio.h>
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "yesno.h"

void
pputstring(key, id, string, pdbp)
	char *key;			/* key string */
	char *id;			/* string identifier */
	char *string;			/* string argument */
	PDB *pdbp;			/* database stream */
{
	char *pbfndkey();		/* find key */
	int foundkey;			/* found key flag */
	int pbaddkey();			/* add key */
	int pbaddstring();		/* add string field */
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
			pbaddstring(id, string);
			foundkey = YES;
			}
		pputent(pdbp);
		}
	if (foundkey == NO)
		{			/* new entry */
		pbclear();
		pbaddkey(key);
		pbaddstring(id, string);
		pputent(pdbp);
		}
}
