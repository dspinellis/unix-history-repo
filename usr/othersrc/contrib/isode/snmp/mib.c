/* mib.c - MIB realization */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/mib.c,v 7.10 91/02/22 09:43:34 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/mib.c,v 7.10 91/02/22 09:43:34 mrose Interim $
 *
 * Contributed by NYSERNet Inc.  This work was partially supported by the
 * U.S. Defense Advanced Research Projects Agency and the Rome Air Development
 * Center of the U.S. Air Force Systems Command under contract number
 * F30602-88-C-0016.
 *
 *
 * $Log:	mib.c,v $
 * Revision 7.10  91/02/22  09:43:34  mrose
 * Interim 6.8
 * 
 * Revision 7.9  91/01/11  15:34:30  mrose
 * sets
 * 
 * Revision 7.8  91/01/07  12:40:49  mrose
 * update
 * 
 * Revision 7.7  90/12/18  10:13:42  mrose
 * update
 * 
 * Revision 7.6  90/08/08  14:01:04  mrose
 * stuff
 * 
 * Revision 7.5  90/07/09  14:48:50  mrose
 * sync
 * 
 * Revision 7.4  90/05/13  16:18:11  mrose
 * views
 * 
 * Revision 7.3  90/02/27  18:49:45  mrose
 * unix stuff
 * 
 * Revision 7.2  90/02/17  10:38:19  mrose
 * smux
 * 
 * Revision 7.1  90/01/11  18:34:14  mrose
 * real-sync
 * 
 * Revision 7.0  89/11/23  22:23:12  mrose
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
#ifdef	BSD42
#include <sys/file.h>
#endif
#ifdef	SYS5
#include <fcntl.h>
#endif

/*    DATA */

static	int	kd;


struct nlist nl[] = {
    { "_arptab" },
    { "_arptab_size" },
    { "_icmpstat" },
    { "_ifnet" },
#ifndef	SUNOS41
    { "_ipforwarding" },
#else
    { "_ip_forwarding" },
#endif
    { "_ipstat" },
    { "_rthashsize" },
    { "_rthost" },
    { "_rtnet" },
    { "_tcb" },
    { "_tcpstat" },
    { "_udb" },
    { "_udpstat" },
    { "_rtstat" },
#ifdef	BSD44
    { "_radix_node_head" },
    { "_iso_systype" },
    { "_clnp_stat" },
    { "_esis_stat" },
#endif

    NULL
};


struct	timeval	my_boottime;

OID	nullSpecific = NULLOID;

/*  */

init_mib () {
    register struct nlist *nz;

    if (nlist ("/vmunix", nl) == NOTOK)
	adios ("/vmunix", "unable to nlist");
    for (nz = nl; nz -> n_name; nz++)
	if (nz -> n_value == 0)
	    advise (LLOG_EXCEPTIONS, NULLCP, "\"%s\" not in /vmunix (warning)",
		    nz -> n_name);

    if ((kd = open ("/dev/kmem", O_RDONLY)) == NOTOK)
	adios ("/vmunix", "unable to read");

    if ((nullSpecific = text2oid ("0.0")) == NULLOID)
	adios (NULLCP, "text2oid (\"0.0\") failed!");
}

/*  */

fin_mib () {
    register OT	    ot;

    for (ot = text2obj ("ccitt"); ot; ot = ot -> ot_next)
	if (ot -> ot_status == OT_MANDATORY
	        && ot -> ot_getfnx == o_generic
	        && ot -> ot_info == NULL)
	    advise (LLOG_EXCEPTIONS, NULLCP,
		    "variable \"%s.0\" has no value (warning)", ot -> ot_text);

    if (gettimeofday (&my_boottime, (struct timezone *) 0) == NOTOK) {
	advise (LLOG_EXCEPTIONS, "failed", "gettimeofday");
	bzero ((char *) &my_boottime, sizeof my_boottime);
    }
}

/*  */

set_variable (name, newvalue)
char   *name,
       *newvalue;
{
    caddr_t  value;
    register OT	    ot = text2obj (name);
    register OS	    os;

    if (ot == NULLOT) {
	advise (LLOG_EXCEPTIONS, NULLCP, "unknown object \"%s\"", name);
	return;
    }
    if (ot -> ot_getfnx == NULLIFP) {
	advise (LLOG_EXCEPTIONS, NULLCP, "no getfnx for object \"%s\"",
		ot -> ot_text);
	return;
    }
    if (ot -> ot_getfnx != o_generic) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"non-generic getfnx for object \"%s\"", ot -> ot_text);
	return;
    }
    if ((os = ot -> ot_syntax) == NULLOS) {
	advise (LLOG_EXCEPTIONS, NULLCP, "no syntax defined for object \"%s\"",
		ot -> ot_text);
	return;
    }
    if ((*os -> os_parse) (&value, newvalue) == NOTOK) {
	advise (LLOG_EXCEPTIONS, NULLCP,
		"invalid value for variable \"%s.0\": \"%s\"", 
		ot -> ot_text, newvalue);
	return;
    }
    if (ot -> ot_info) {
	(*os -> os_free) (ot -> ot_info);
	ot -> ot_info = NULL;
    }
    ot -> ot_info = value;
}

/*  */

int	getkmem (n, buffer, cc)
struct nlist *n;
caddr_t	buffer;
int	cc;
{
    if (n -> n_value == 0) {
	advise (LLOG_EXCEPTIONS, NULLCP, "\"%s\" not in /vmunix", n -> n_name);
	return NOTOK;
    }
    if (lseek (kd, (long) n -> n_value, L_SET) == NOTOK) {
	advise (LLOG_EXCEPTIONS, "failed", "lseek of 0x%x for \"%s\" in kmem",
		(long) n -> n_value, n -> n_name);
	return NOTOK;
    }
    if (read (kd, buffer, cc) != cc) {
	advise (LLOG_EXCEPTIONS, "failed", "read of \"%s\" from kmem",
		n -> n_name);
	return NOTOK;
    }

    return OK;
}
