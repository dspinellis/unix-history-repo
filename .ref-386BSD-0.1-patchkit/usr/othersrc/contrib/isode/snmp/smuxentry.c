/* smuxentry.c - smuxEntry routines */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/smuxentry.c,v 7.2 91/03/09 11:57:27 mrose Exp $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/smuxentry.c,v 7.2 91/03/09 11:57:27 mrose Exp $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	smuxentry.c,v $
 * Revision 7.2  91/03/09  11:57:27  mrose
 * update
 * 
 * Revision 7.1  91/02/22  09:44:04  mrose
 * Interim 6.8
 * 
 * Revision 7.0  90/02/17  10:36:47  mrose
 * *** empty log message ***
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

#include <stdio.h>
#include "smux.h"
#include "tailor.h"

/*    DATA */

static char *smuxEntries = "snmpd.peers";

static FILE *servf = NULL;
static int  stayopen = 0;

static struct smuxEntry    ses;

/*  */

int	setsmuxEntry (f)
int	f;
{
    if (servf == NULL)
	servf = fopen (isodefile (smuxEntries, 0), "r");
    else
	rewind (servf);
    stayopen |= f;

    return (servf != NULL);
}


int	endsmuxEntry () {
    if (servf && !stayopen) {
	(void) fclose (servf);
	servf = NULL;
    }

    return 1;
}

/*  */

struct smuxEntry  *getsmuxEntry () {
    int	    vecp;
    register int i;
    register struct smuxEntry *se = &ses;
    register char  *cp;
    static char buffer[BUFSIZ + 1];
    static char *vec[NVEC + NSLACK + 1];
    static unsigned int elements[NELEM + 1];

    if (servf == NULL
	    && (servf = fopen (isodefile (smuxEntries, 0), "r")) == NULL)
	return NULL;

    bzero ((char *) se, sizeof *se);

    while (fgets (buffer, sizeof buffer, servf) != NULL) {
	if (*buffer == '#')
	    continue;
	if (cp = index (buffer, '\n'))
	    *cp = NULL;
	if ((vecp = str2vec (buffer, vec)) < 3)
	    continue;

	if ((i = str2elem (vec[1], elements)) <= 1)
	    continue;

	se -> se_name = vec[0];
	se -> se_identity.oid_elements = elements;
	se -> se_identity.oid_nelem = i;
	se -> se_password = vec[2];
	se -> se_priority = vecp > 3 ? atoi (vec[3]) : -1;

	return se;
    }

    return NULL;
}

/*  */

struct smuxEntry *getsmuxEntrybyname (name)
char   *name;
{
    register struct smuxEntry *se;

    (void) setsmuxEntry (0);
    while (se = getsmuxEntry ())
	if (strcmp (name, se -> se_name) == 0)
	    break;
    (void) endsmuxEntry ();

    return se;
}

/*  */

struct smuxEntry *getsmuxEntrybyidentity (identity)
OID	identity;
{
    register struct smuxEntry *se;

    (void) setsmuxEntry (0);

/* Compare only the portion of the object identifier that is given in
   the entry read from the smux.peers file, allowing a SMUX sub-agent
   to provide its own version number as a suffix of its identity.  (EJP)
 */
    while (se = getsmuxEntry ())
	if (identity -> oid_nelem >= se -> se_identity.oid_nelem
	        && elem_cmp (identity -> oid_elements,
			     se -> se_identity.oid_nelem,
			     se -> se_identity.oid_elements,
			     se -> se_identity.oid_nelem) == 0)
	    break;

    (void) endsmuxEntry ();

    return se;
}

