/* closefds.c - close-up fd:s */
#ifndef	lint
static char ident[] = "@(#)$Id: closefds.c,v 1.3 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

#include "../h/mh.h"
#ifndef	BSD42
#include <stdio.h>
#endif	/* not BSD42 */


void	closefds (i)
register int	i;
{
#ifndef	BSD42
    int     nbits = _NFILE;
#else	/* BSD42 */
    int     nbits = getdtablesize ();
#endif	/* BSD42 */

    for (; i < nbits; i++)
#ifdef	OVERHEAD
	if (i != fd_def && i != fd_ctx)
#endif	/* OVERHEAD */
	    (void) close (i);
}
