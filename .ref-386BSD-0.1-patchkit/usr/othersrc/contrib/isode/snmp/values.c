/* values.c - encode values */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/values.c,v 7.8 91/02/22 09:44:57 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/values.c,v 7.8 91/02/22 09:44:57 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	values.c,v $
 * Revision 7.8  91/02/22  09:44:57  mrose
 * Interim 6.8
 * 
 * Revision 7.7  91/01/11  15:35:44  mrose
 * sets
 * 
 * Revision 7.6  90/12/18  10:14:25  mrose
 * update
 * 
 * Revision 7.5  90/10/29  18:39:14  mrose
 * updates
 * 
 * Revision 7.4  90/10/23  20:37:26  mrose
 * update
 * 
 * Revision 7.3  90/07/09  14:49:51  mrose
 * sync
 * 
 * Revision 7.2  90/04/18  08:52:12  mrose
 * oid_normalize
 * 
 * Revision 7.1  90/02/19  15:39:11  mrose
 * one more time
 * 
 * Revision 7.0  90/02/17  10:36:50  mrose
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


#include <stdio.h>
#include "SNMP-types.h"
#include "objects.h"
#include "logger.h"
	    
/*  */

#define	ADVISE	if (o_advise) (*o_advise)

IFP	o_advise = NULLIFP;

/*  */

int	o_generic (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;
    register OS	    os = ot -> ot_syntax;

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
	
    if (os == NULLOS) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP,
		"no syntax defined for object \"%s\"", ot -> ot_text);

	return (offset == type_SNMP_PDUs_get__next__request ? NOTOK
					: int_SNMP_error__status_genErr);
    }
    if (ot -> ot_info == NULL) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP,
		"no value defined for object \"%s\"", ot -> ot_text);

	return (offset == type_SNMP_PDUs_get__next__request ? NOTOK
					: int_SNMP_error__status_noSuchName);
    }

    if (v -> value)
	free_SNMP_ObjectSyntax (v -> value), v -> value = NULL;
    if ((*os -> os_encode) (ot -> ot_info, &v -> value) == NOTOK) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP,
		"encoding error for variable \"%s\"",
		oid2ode (oi -> oi_name));

	return (offset == type_SNMP_PDUs_get__next__request ? NOTOK
					: int_SNMP_error__status_genErr);
    }

    return int_SNMP_error__status_noError;
}
	    
/*  */

int	s_generic (oi, v, offset)
OI	oi;
register struct type_SNMP_VarBind *v;
int	offset;
{
    register OID    oid = oi -> oi_name;
    register OT	    ot = oi -> oi_type;
    register OS	    os = ot -> ot_syntax;

    switch (offset) {
	case type_SNMP_PDUs_set__request:
	case type_SNMP_PDUs_commit:
	case type_SNMP_PDUs_rollback:
	    if (oid -> oid_nelem != ot -> ot_name -> oid_nelem + 1
		    || oid -> oid_elements[oid -> oid_nelem - 1] != 0)
		return int_SNMP_error__status_noSuchName;
	    break;

	default:
	    return int_SNMP_error__status_genErr;
    }
	
    if (os == NULLOS) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP,
		"no syntax defined for object \"%s\"", ot -> ot_text);

	return int_SNMP_error__status_genErr;
    }

    switch (offset) {
	case type_SNMP_PDUs_set__request:
	    if (ot -> ot_save)
		(*os -> os_free) (ot -> ot_save), ot -> ot_save = NULL;
	    if ((*os -> os_decode) (&ot -> ot_save, v -> value) == NOTOK)
		return int_SNMP_error__status_badValue;
	    break;

	case type_SNMP_PDUs_commit:
	    if (ot -> ot_info)
		(*os -> os_free) (ot -> ot_info);
	    ot -> ot_info = ot -> ot_save, ot -> ot_save = NULL;
	    break;

	case type_SNMP_PDUs_rollback:
	    if (ot -> ot_save)
		(*os -> os_free) (ot -> ot_save), ot -> ot_save = NULL;
	    break;
    }

    return int_SNMP_error__status_noError;
}
	    
/*  */

int	o_longword (oi, v, number)
OI	oi;
struct type_SNMP_VarBind *v;
integer	number;				/* actual param: often a constant */
{
    return o_number (oi, v, (caddr_t) &number);
}


int	o_number (oi, v, number)
OI	oi;
register struct type_SNMP_VarBind *v;
caddr_t	number;
{
    int	    result;
    register OT	    ot = oi -> oi_type;
    register OS	    os = ot -> ot_syntax;

    if (os == NULLOS) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP,
		"no syntax defined for object \"%s\"", ot -> ot_text);

	return int_SNMP_error__status_genErr;
    }

    if (v -> value)
	free_SNMP_ObjectSyntax (v -> value), v -> value = NULL;
    result = (*os -> os_encode) (number, &v -> value);

    if (result == NOTOK) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP,
		"encoding error for variable \"%s\"",
		oid2ode (oi -> oi_name));

	return int_SNMP_error__status_genErr;
    }

    return int_SNMP_error__status_noError;
}
	    
/*  */

int	o_string (oi, v, base, len)
OI	oi;
register struct type_SNMP_VarBind *v;
char   *base;
int	len;
{
    int	    result;
    struct qbuf *value;
    register OT	    ot = oi -> oi_type;
    register OS	    os = ot -> ot_syntax;

    if (os == NULLOS) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP,
		"no syntax defined for object \"%s\"", ot -> ot_text);

	return int_SNMP_error__status_genErr;
    }

    if ((value = str2qb (base, len, 1)) == NULL) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP, "out of memory");

	return int_SNMP_error__status_genErr;
    }

    if (v -> value)
	free_SNMP_ObjectSyntax (v -> value), v -> value = NULL;
    result = (*os -> os_encode) (value, &v -> value);
    qb_free (value);

    if (result == NOTOK) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP,
		"encoding error for variable \"%s\"",
		oid2ode (oi -> oi_name));

	return int_SNMP_error__status_genErr;
    }

    return int_SNMP_error__status_noError;
}

/*  */

int	o_qbstring (oi, v, value)
OI	oi;
register struct type_SNMP_VarBind *v;
struct qbuf *value;
{
    int	    result;
    register OT	    ot = oi -> oi_type;
    register OS	    os = ot -> ot_syntax;

    if (os == NULLOS) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP,
		"no syntax defined for object \"%s\"", ot -> ot_text);

	return int_SNMP_error__status_genErr;
    }

    if (v -> value)
	free_SNMP_ObjectSyntax (v -> value), v -> value = NULL;
    result = (*os -> os_encode) (value, &v -> value);

    if (result == NOTOK) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP,
		"encoding error for variable \"%s\"",
		oid2ode (oi -> oi_name));

	return int_SNMP_error__status_genErr;
    }

    return int_SNMP_error__status_noError;
}

/*  */

int	o_specific (oi, v, value)
OI	oi;
register struct type_SNMP_VarBind *v;
caddr_t	value;
{
    int	    result;
    register OT	    ot = oi -> oi_type;
    register OS	    os = ot -> ot_syntax;

    if (os == NULLOS) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP,
		"no syntax defined for object \"%s\"", ot -> ot_text);

	return int_SNMP_error__status_genErr;
    }

    if (v -> value)
	free_SNMP_ObjectSyntax (v -> value), v -> value = NULL;
    result = (*os -> os_encode) (value, &v -> value);

    if (result == NOTOK) {
	ADVISE (LLOG_EXCEPTIONS, NULLCP,
		"encoding error for variable \"%s\"",
		oid2ode (oi -> oi_name));

	return int_SNMP_error__status_genErr;
    }

    return int_SNMP_error__status_noError;
}

/*  */

int	mediaddr2oid (ip, addr, len, islen)
register unsigned int *ip;
register u_char *addr;
int	len,
	islen;
{
    register int   i;

    if (islen)
	*ip++ = len & 0xff;

    for (i = len; i > 0; i--)
	*ip++ = *addr++ & 0xff;

    return (len + (islen ? 1 : 0));
}

/*  */

OID	oid_extend (q, howmuch)
register OID	q;
int	howmuch;
{
    register unsigned int   i,
			   *ip,
			   *jp;
    OID	    oid;

    if (q == NULLOID)
	return NULLOID;
    if ((i = q -> oid_nelem) < 1)
	return NULLOID;
    if ((oid = (OID) malloc (sizeof *oid)) == NULLOID)
	return NULLOID;

    if ((ip = (unsigned int *)
	 	    calloc ((unsigned) (i + howmuch + 1), sizeof *ip))
		== NULL) {
	free ((char *) oid);
	return NULLOID;
    }

    oid -> oid_elements = ip, oid -> oid_nelem = i + howmuch;

    for (i = 0, jp = q -> oid_elements; i < oid -> oid_nelem; i++, jp++)
	*ip++ = *jp;

    return oid;
}

/*  */

OID	oid_normalize (q, howmuch, bigvalue)
register OID	q;
int	howmuch,
	bigvalue;
{
    register int	i;
    register unsigned int   *ip,
			    *jp;
    OID	    oid;

    if ((oid = oid_extend (q, howmuch)) == NULL)
	return NULLOID;

    for (jp = (ip = oid -> oid_elements + q -> oid_nelem) - 1;
	     jp >= oid -> oid_elements;
	     jp--)
	if (*jp > 0) {
	    *jp -= 1;
	    break;
	}
    for (i = howmuch; i > 0; i--)
	*ip++ = (unsigned int) bigvalue;

    return oid;
}
