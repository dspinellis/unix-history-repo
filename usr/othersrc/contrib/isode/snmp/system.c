/* system.c - MIB realization of the System group */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/system.c,v 7.8 91/02/22 09:44:40 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/system.c,v 7.8 91/02/22 09:44:40 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	system.c,v $
 * Revision 7.8  91/02/22  09:44:40  mrose
 * Interim 6.8
 * 
 * Revision 7.7  91/01/11  15:35:39  mrose
 * sets
 * 
 * Revision 7.6  91/01/07  12:41:16  mrose
 * update
 * 
 * Revision 7.5  90/12/18  10:14:11  mrose
 * update
 * 
 * Revision 7.4  90/10/23  20:37:24  mrose
 * update
 * 
 * Revision 7.3  90/05/14  15:44:29  mrose
 * touch-up
 * 
 * Revision 7.2  90/05/14  13:28:18  mrose
 * system
 * 
 * Revision 7.1  90/05/13  16:18:32  mrose
 * views
 * 
 * Revision 7.0  89/11/23  22:23:33  mrose
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


#include <stdio.h>
#include "mib.h"
#include "tailor.h"

/*  */

static int  o_sysUpTime (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    struct timeval now;
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;
    static   int lastq = -1;
    static   integer diff;

    switch (offset) {
	case type_SNMP_PDUs_get__request:
	    if (oid -> oid_nelem != ot -> ot_name -> oid_nelem + 1
		    || oid -> oid_elements[oid -> oid_nelem - 1] != 0)
		return int_SNMP_error__status_noSuchName;
	    break;

	case type_SNMP_PDUs_get__next__request:
	    if (oid -> oid_nelem == ot -> ot_name -> oid_nelem) {
		OID	new;

		if ((new = oid_extend (oid, 1)) == NULLOID)
		    return NOTOK;
		new -> oid_elements[new -> oid_nelem - 1] = 0;

		if (v -> name)
		    free_SNMP_ObjectName (v -> name);
		v -> name = new;
	    }
	    else
		return NOTOK;
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }

    if (quantum != lastq) {
	lastq = quantum;

	if (gettimeofday (&now, (struct timezone *) 0) == NOTOK) {
	    advise (LLOG_EXCEPTIONS, "failed", "gettimeofday");
	    return generr (offset);
	}

	diff = (now.tv_sec - my_boottime.tv_sec) * 100
	     + ((now.tv_usec - my_boottime.tv_usec) / 10000);
    }

    return o_number (oi, v, (caddr_t) &diff);
}

/*  */

static struct sys_pair {
    char   *s_name;
    char   *s_text;
    IFP	    s_getfnx;
    IFP	    s_setfnx;
}    pairs[] = {
    "sysDescr",    sysDescr,	o_generic,	NULLIFP,
    "sysObjectID", sysObjectID, o_generic,	NULLIFP,
    "sysUpTime",   NULL,	o_sysUpTime,	NULLIFP,
    "sysContact",  NULL,	o_generic,	s_generic,
#define	SYS_NAME	4
    "sysName",     NULL,	o_generic,	s_generic,
    "sysLocation", NULL,	o_generic,	s_generic,
    "sysServices", "72",	o_generic,	NULLIFP,

    NULL
};


init_system () {
    char    buffer[BUFSIZ];
    register OT	    ot;
    register struct sys_pair *sp;

    (void) gethostname (buffer, sizeof buffer);
    pairs[SYS_NAME].s_text = buffer;

    for (sp = pairs; sp -> s_name; sp++)
	if (ot = text2obj (sp -> s_name)) {
	    ot -> ot_getfnx = sp -> s_getfnx;
	    ot -> ot_setfnx = sp -> s_setfnx;

	    if (sp -> s_text)
		if (ot -> ot_syntax)
		    (void) (*ot -> ot_syntax -> os_parse) ((struct qbuf **)
							       &ot -> ot_info,
							   sp -> s_text);
		else
		    advise (LLOG_EXCEPTIONS, NULLCP, "%s: no syntax",
			    sp -> s_name);
	}
	else
	    advise (LLOG_EXCEPTIONS, NULLCP, "%s: unknown object",
		    sp -> s_name);
}
