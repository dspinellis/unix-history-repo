/* isoentity.c - application entity titles -- sequential lookup utilities  */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/acsap/RCS/isoentity.c,v 7.4 91/02/22 09:14:42 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/acsap/RCS/isoentity.c,v 7.4 91/02/22 09:14:42 mrose Interim $
 *
 *
 * $Log:	isoentity.c,v $
 * Revision 7.4  91/02/22  09:14:42  mrose
 * Interim 6.8
 * 
 * Revision 7.3  90/12/23  18:39:15  mrose
 * update
 * 
 * Revision 7.2  90/10/29  18:38:00  mrose
 * updates
 * 
 * Revision 7.1  90/08/08  14:11:40  mrose
 * update
 * 
 * Revision 7.0  89/11/23  21:22:13  mrose
 * Release 6.0
 * 
 */

/*
 *				  NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */


/* LINTLIBRARY */

#include <ctype.h>
#include <stdio.h>
#include "psap.h"
#include "isoaddrs.h"
#include "tailor.h"

/*    DATA */

static char *isoentities = "isoentities";

static FILE *servf = NULL;
static int   stayopen = 0;

static int   mask;
static int   vecp;
static char  buffer[BUFSIZ + 1];
static char *vec[NVEC + NSLACK + 1];

static struct isoentity ies;

/*  */

int	setisoentity (f)
int	f;
{
    if (servf == NULL)
	servf = fopen (isodefile (isoentities, 0), "r");
    else
	rewind (servf);
    stayopen |= f;

    return (servf != NULL);
}


int	endisoentity () {
    if (servf && !stayopen) {
	(void) fclose (servf);
	servf = NULL;
    }

    return 1;
}

/*  */

struct isoentity   *getisoentity () {
    static char  descriptor[BUFSIZ];

    if (servf == NULL
	    && (servf = fopen (isodefile (isoentities, 0), "r")) == NULL)
	return NULL;

    while (_startisoentity (descriptor) == OK)
	if (_stopisoentity (descriptor, (struct isoentity *) NULL) == OK)
	    return (&ies);

    return NULL;
}

/*  */

int	_startisoentity (descriptor)
char   *descriptor;
{
    register char  *cp,
		   *dp;
    char   *ep;

    ep = (dp = buffer) + sizeof buffer;
    while (fgets (dp, ep - dp, servf) != NULL) {
	if (*buffer == '#')
	    continue;
	if (*dp == '\n' && dp == buffer)
	    continue;
	if (cp = index (buffer, '\n')) {
	    *cp-- = NULL;
	    if (*cp == '\\') {
		if ((dp = cp) + 1 >= ep)
		    dp = buffer;
		continue;
	    }
	}

	switch (vecp = str2vecX (buffer, vec, 5 + 1, &mask, NULL, 1)) {
	    case 3:			/* no address */
	    case 4:			/* new-style */
	        break;

	    default:
		continue;
	}

	(void) sprintf (descriptor, "%s-%s", vec[0], vec[1]);

	return OK;
    }

    return DONE;
}

/*  */

int	_stopisoentity (descriptor, iep)
char   *descriptor;
struct isoentity *iep;
{
    register int    i;
    register struct isoentity  *ie = &ies;
    OID		oid = &ie -> ie_identifier;
    struct PSAPaddr *pa = &ie -> ie_addr;
    struct PSAPaddr *pz;
    static unsigned int elements[NELEM + 1];

    bzero ((char *) ie, sizeof *ie);

    if (strcmp (vec[2], "NULL") == 0)
	elements[i = 0] = 0;
    else
	if ((i = str2elem (vec[2], elements)) <= 1)
	    return NOTOK;
    oid -> oid_elements = elements;
    oid -> oid_nelem = i;
    ie -> ie_descriptor = descriptor;

    switch (vecp) {
	case 3:		/* no address */
	    break;

	case 4:		/* new-style */
	    if (pz = str2paddr (vec[3]))
		*pa = *pz;		/* struct copy */
	    break;
    }

    if (iep)
	*iep = *ie;	/* struct copy */

    return OK;
}

/*  */

_printent (ie)
register struct isoentity  *ie;
{
    LLOG (addr_log, LLOG_DEBUG,
	  ("Entity:  %s (%s)", ie -> ie_descriptor,
	   oid2ode (&ie -> ie_identifier)));

    (void) ll_printf (addr_log, "Address: %s\n",
		      paddr2str (&ie -> ie_addr, NULLNA));

    (void) ll_printf (addr_log, "///////\n");

    (void) ll_sync (addr_log);
}
