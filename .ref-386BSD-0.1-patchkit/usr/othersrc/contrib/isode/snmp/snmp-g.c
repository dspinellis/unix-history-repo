/* snmp-g.c - SNMP group */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/snmp/RCS/snmp-g.c,v 7.3 91/02/22 09:44:05 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/snmp/RCS/snmp-g.c,v 7.3 91/02/22 09:44:05 mrose Interim $
 *
 *
 * $Log:	snmp-g.c,v $
 * Revision 7.3  91/02/22  09:44:05  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/11  15:35:05  mrose
 * sets
 * 
 * Revision 7.1  90/12/18  10:13:49  mrose
 * update
 * 
 * Revision 7.0  90/12/17  22:07:55  mrose
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
#include "mib.h"
#include "snmp-g.h"

/*    SNMP GROUP */

init_snmp () {
    register OT	    ot;

    bzero ((char *) &snmpstat, sizeof snmpstat);
    snmpstat.s_enableauthentraps = TRAPS_ENABLED;

    if (ot = text2obj ("snmpInPkts"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_inpkts;
    if (ot = text2obj ("snmpOutPkts"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_outpkts;
    if (ot = text2obj ("snmpInBadVersions"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_badversions;
    if (ot = text2obj ("snmpInBadCommunityNames"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_badcommunitynames;
    if (ot = text2obj ("snmpInBadCommunityUses"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_badcommunityuses;
    if (ot = text2obj ("snmpInASNParseErrs"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_asnparseerrs;
    if (ot = text2obj ("snmpInTotalReqVars"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_totalreqvars;
    if (ot = text2obj ("snmpInTotalSetVars"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_totalsetvars;
    if (ot = text2obj ("snmpInGetRequests"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_ingetrequests;
    if (ot = text2obj ("snmpInGetNexts"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_ingetnexts;
    if (ot = text2obj ("snmpInSetRequests"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_insetrequests;
    if (ot = text2obj ("snmpInGetResponses"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_ingetresponses;
    if (ot = text2obj ("snmpInTraps"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_intraps;
    if (ot = text2obj ("snmpOutTooBigs"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_toobigs;
    if (ot = text2obj ("snmpOutNoSuchNames"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_nosuchnames;
    if (ot = text2obj ("snmpOutBadValues"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_badvalues;
    if (ot = text2obj ("snmpOutGenErrs"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_generrs;
    if (ot = text2obj ("snmpOutGetResponses"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_outgetresponses;
    if (ot = text2obj ("snmpOutTraps"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_outtraps;
    if (ot = text2obj ("snmpEnableAuthenTraps"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_setfnx = s_generic,
	ot -> ot_info = (caddr_t) &snmpstat.s_enableauthentraps;

    if (ot = text2obj ("unixNetstat"))
	ot -> ot_getfnx = o_generic,
	ot -> ot_info = (caddr_t) &unix_netstat;
}
