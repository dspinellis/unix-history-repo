/* $Header: version.c,v 7.0 86/10/08 15:14:39 lwall Exp $
 *
 * $Log:	version.c,v $
 * Revision 7.0  86/10/08  15:14:39  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#include "patchlevel.h"
#include "INTERN.h"
#include "version.h"

/* Print out the version number. */

void
version()
{
    extern char rcsid[];

    printf("%s\r\nPatch level: %d\r\n", rcsid, PATCHLEVEL);
}
