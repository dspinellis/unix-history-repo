/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pgetent() loads next database entry into buffer pdbp->pbuf. Also resets
 * buffer pointers and makes pdbp the current working database CURPDB.
 * Returns length of entry or EOF.
 */
#include <stdio.h>
#include "path.h"
#include "pdb.h"

char *CURPBUF;				/* current database buffer */
char *FP;				/* next non-key field */
char *KP;				/* next key field */
PDB *CURPDB;				/* current database stream */

pgetent(pdbp)
	register PDB *pdbp;		/* database stream */
{
	register char *bp;		/* buffer pointer */
	register int c;			/* next character */

	if ((pdbp->flag&_PREAD) == 0 || (pdbp->flag&(_PEOF|_PERR)) != 0)
		return(EOF);
	CURPDB = pdbp;
	CURPBUF = FP = KP = bp = pdbp->pbuf;
	while ((c = getc(pdbp->fp)) != EOF && c != '\n')
		*bp++ = c;
	*bp = '\0';
	if (c == EOF)
		{
		pdbp->flag |= _PEOF;
		return(EOF);
		}
	else
		return(bp - pdbp->pbuf);
}
