/* hosts.c - use MMDF to get hostname information */
#ifndef	lint
static char ident[] = "@(#)$Id: hosts.c,v 1.3 1992/12/15 00:20:22 jromine Exp $";
#endif	/* lint */

/* LINTLIBRARY */

#include "../h/strings.h"
#include "util.h"
#include "mmdf.h"
#include "ch.h"
#ifdef	MMDFII
#include "dm.h"
#endif	/* MMDFII */
#include "../zotnet/mts.h"

/* 
 * We really should be careful with the number of fd's that this routine
 * opens:  the ch_seq ch_table.c module likes to keep 6 (yes, SIX) fds around
 * to speed-up host lookups in the channel table.  Unfortunately, after all
 * of them get opened, six are always open (ch_table may close one to open
 * another).  The bottom line is that if repl calls post, then we get 12
 * (yes, TWELVE) fds open, with only six usable.
 *
 * send will close all fds >= 3 prior to invoking post.  It would be nice
 * if one could control ch_seq's use of fds for table lookups, but such is
 * life.
 *
 */

#ifndef	MMDFII
char   *OfficialName (name)
register char   *name;
{
    register Chan *ch;
    static char buffer[BUFSIZ];

    return ((ch = ch_h2chan (name, buffer)) == (Chan *) (-1) ? NULL
	    : ch == (Chan *) NULL ? LocalName ()
	    : buffer);
}
#else	/* MMDFII */

extern char *invo_name;

extern short ch_yloc;            /* ok to intercept local names        */

static int	inited = 0;

char *OfficialName (name)
register char  *name;
{
    Dmn_route route;
    static char buffer[BUFSIZ];

    if (!inited) {
	mmdf_init (invo_name);
	inited = 1;
    }
    switch (dm_v2route (name, buffer, &route)) {
	case NOTOK: 
	case OK: 
	    return ((ch_yloc && lexequ (name, LocalName ())) ? LocalName ()
		    : NULL);

	default: 
	    return buffer;
    }
}
#endif	/* MMDFII */
