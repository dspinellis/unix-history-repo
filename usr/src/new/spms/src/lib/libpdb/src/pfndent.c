/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pfndent() loads a database entry corresponding to key into the current
 * working buffer CURPBUF. Database must be open for reading only.
 * Returns integer YES if key found, otherwise NO.
 */
#include <stdio.h>
#include "null.h"
#include "path.h"
#include "pdb.h"
#include "yesno.h"

pfndent(key, pdbp)
	char *key;			/* key string */
	PDB *pdbp;			/* database stream */
{
	char *pbfndkey();		/* find key */
	char *sprintf();		/* print output to string */
	int pgetent();			/* load next entry into buffer */
	void rewindpdb();		/* rewind database */

	if ((pdbp->flag&_PERR) != 0)
		return(NO);
	if ((pdbp->flag&(_PWRITE|_PAPPEND)) != 0)
		{
		sprintf(pdbp->perr, "%s must be read access only", pdbp->path);
		pdbp->flag |= _PERR;
		return(NO);
		}
	rewindpdb(pdbp);
	while (pgetent(pdbp) != EOF)
		if (pbfndkey(key) != NULL)
			return(YES);
	return(NO);
}
