/* $Header$ */

/*
 * Author: Peter J. Nicklin
 */

/*
 * pbcpyfield() copies the field pointed to by bp to field. Returns
 * null-terminated field.
 */
#include "pdbuf.h"

char *
pbcpyfield(field, bp)
	register char *field;		/* field string */
	register char *bp;		/* buffer pointer */
{
	char *fieldsave;

	fieldsave = field;
	while (*bp != _PBFS && *bp != '\0')
		*field++ = *bp++;
	*field = '\0';
	return(fieldsave);
}
