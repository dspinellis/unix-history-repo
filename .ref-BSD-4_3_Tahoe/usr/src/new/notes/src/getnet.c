#include "parms.h"
#include "structs.h"
#include "net.h"

#ifdef	RCSIDENT
static char rcsid[] = "$Header: getnet.c,v 1.7 85/01/18 15:11:46 notes Rel $";
#endif	RCSIDENT

/*
 *	getnet(sys, xmit, reply)
 *	char *sys, **xmit, **reply)
 *
 *	This routine will scan the net.how file, looking for alternate
 *	routes to the system specified. The xmit and reply pointers are
 *	set appropriately.
 *	(null if no entry found).
 *	The routine returns -1 if the file is not there, otherwise
 *	it returns the count of lines matched (0,1,2)
 *
 *	Original Coding:	Ray Essick	April 1982
 */

static char outgoing[CMDLEN];
static char incoming[CMDLEN];				/* hold net command */

getnet (sys, xmit, reply, proto)
char   *sys;
char  **xmit;
char  **reply;
int    *proto;
{

    FILE * nethow;
    char    pathname[256];				/* probably ok */
    char    oneline[512];
    char   *p;
    int     i,
            count;
    char   *tsys;					/* room for slop */
    char   *tdirect;
    char   *thow;
    int     tproto;

    count = 0;						/* lines we have */
    if (xmit != (char **) NULL)
	*xmit = NULL;
    if (reply != (char **) NULL)
	*reply = NULL;
    if (proto != (int *) NULL)
	*proto = 0;					/* default protocol */
    sprintf (pathname, "%s/%s/%s", Mstdir, UTILITY, NETHOW);

    if ((nethow = fopen (pathname, "r")) == NULL)
	return (-1);

    while (fgets (oneline, sizeof oneline, nethow) != NULL && count < 2)
    {
	i = strlen (oneline);				/* get end */
	if (oneline[i - 1] == '\n')			/* not entire line */
	{
	    oneline[i - 1] = '\0';			/* strip newline */
	}
	else
	{
	    fprintf (stderr, "%s: net.how line longer than %d characters",
		    Invokedas, pathname, sizeof oneline);
	    return (-2);				/* line too long */
	}
	if (oneline[0] == '#' || oneline[0] == '\0')
	    continue;					/* comment or empty */

/*
 *	pick the system name
 */
	tsys = p = oneline;				/* pick fields */
	while (*p != ':' && *p)
	    p++;					/* skip to colon */
	if (*p != ':')
	    continue;					/* bad line */
	*p++ = '\0';					/* terminate */
/*
 *	and the direction field 
 */
	tdirect = p;					/* direction */
	while (*p != ':' && *p)
	    p++;					/* skip rest of field */
	if (*p == '\0')
	    continue;					/* bad line */
	*p++ = '\0';					/* terminate */
/*
 *	now the protocol (possibly empty)
 */
	if (sscanf (p, "%d", &tproto) != 1)		/* get protocol */
	    tproto = 0;					/* default */
	while (*p != ':' && *p)
	    p++;					/* skip rest */
	p++;						/* skip  */
/*
 *	skip the empty 4th field to get to the "how to" field
 */
	while (*p != ':' && *p)
	    p++;					/* skip rest of field */
	p++;						/* pointing at "how" */
	thow = p;					/* assign */

/*
 *	now, let's see if it's the one we want.
 */

	if (strcmp (tsys, sys))				/* match? */
	    continue;					/* no */


	if (*tdirect == 'x')				/* transmit */
	{
	    if (xmit != (char **) NULL)			/* want it? */
	    {
		strcpy (outgoing, thow);		/* copy */
		*xmit = outgoing;			/* and point */
	    }
	    if (proto != (int *) NULL)			/* want it? */
		*proto = tproto;			/* give it to him */
	}
	else
	{						/* force reply */
	    if (reply != (char **) NULL)
	    {
		strcpy (incoming, thow);		/* copy */
		*reply = incoming;			/* and point */
	    }
	}
	count++;					/* bump the count */
    }
    fclose (nethow);
    return count;
}
