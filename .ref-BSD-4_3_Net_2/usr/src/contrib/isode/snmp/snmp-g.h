/* snmp-g.h - SNMP group */

/* 
 * $Header: /f/osi/snmp/RCS/snmp-g.h,v 7.2 91/02/22 09:44:06 mrose Interim $
 *
 *
 * $Log:	snmp-g.h,v $
 * Revision 7.2  91/02/22  09:44:06  mrose
 * Interim 6.8
 * 
 * Revision 7.1  91/01/11  15:35:07  mrose
 * sets
 * 
 * Revision 7.0  90/12/17  22:07:57  mrose
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


#include "psap.h"


struct snmpstat {
    integer	s_inpkts;
    integer	s_outpkts;
    integer	s_badversions;
    integer	s_badcommunitynames;
    integer	s_badcommunityuses;
    integer	s_asnparseerrs;
    integer	s_totalreqvars;
    integer	s_totalsetvars;
    integer	s_ingetrequests;
    integer	s_ingetnexts;
    integer	s_insetrequests;
    integer	s_ingetresponses;
    integer	s_intraps;
    integer	s_outgetresponses;
    integer	s_outtraps;
    integer	s_toobigs;
    integer	s_nosuchnames;
    integer	s_badvalues;
    integer	s_readonlys;
    integer	s_generrs;
    integer	s_enableauthentraps;
#define	TRAPS_ENABLED	1			/* snmpEnableAuthenTraps */
#define	TRAPS_DISABLED	2			/*   .. */
};

extern struct snmpstat snmpstat;
extern int	unix_netstat;
