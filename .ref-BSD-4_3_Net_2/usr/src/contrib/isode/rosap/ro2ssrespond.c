/* ro2ssrespond.c - responder */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/rosap/RCS/ro2ssrespond.c,v 7.3 91/02/22 09:41:23 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/rosap/RCS/ro2ssrespond.c,v 7.3 91/02/22 09:41:23 mrose Interim $
 *
 * Based on an TCP-based implementation by George Michaelson of University
 * College London.
 *
 *
 * $Log:	ro2ssrespond.c,v $
 * Revision 7.3  91/02/22  09:41:23  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/11/05  13:33:09  mrose
 * update
 * 
 * Revision 7.1  90/07/01  21:05:56  mrose
 * pepsy
 * 
 * Revision 6.0  89/03/18  23:42:19  mrose
 * Release 5.0
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
#include "../acsap/OACS-types.h"
#include "ropkt.h"
#include "tailor.h"

/*    RO-BEGIN.INDICATION */

int	RoInit (vecp, vec, ros, roi)
int	vecp;
char  **vec;
struct RoSAPstart *ros;
struct RoSAPindication *roi;
{
    int     result;
    register struct assocblk   *acb;
    register PE	pe;
    struct SSAPref ref;
    struct SSAPstart    sss;
    register struct SSAPstart  *ss = &sss;
    struct SSAPindication   sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort  *sa = &si -> si_abort;
    struct type_OACS_PConnect	*pconn;

    isodetailor (NULLCP, 0);

    missingP (vec);
    missingP (ros);
    missingP (roi);

    if ((acb = newacblk ()) == NULL)
	return rosaplose (roi, ROS_CONGEST, NULLCP, "out of memory");
    acb -> acb_flags |= ACB_ROS;
    (void) RoSService (acb, roi);

    if (SInit (vecp, vec, ss, si) == NOTOK) {
	(void) ss2roslose (acb, roi, "SInit", sa);
	goto out1;
    }

    acb -> acb_fd = ss -> ss_sd;
    acb -> acb_uabort = SUAbortRequest;

    acb -> acb_requirements = ss -> ss_requirements & SR_BCSUBSET;
    if (acb -> acb_requirements & SR_DUPLEX)
	acb -> acb_requirements = SR_DUPLEX;
    else
	if (acb -> acb_requirements & SR_HALFDUPLEX)
	    acb -> acb_requirements = SR_HALFDUPLEX;
	else {
	    (void) rosaplose (roi, ROS_PROTOCOL, NULLCP,
		    "desired session requirements unavailable");
	    goto out2;
	}

    if (acb -> acb_requirements & SR_HALFDUPLEX) {
	if (((ss -> ss_settings >> ST_DAT_SHIFT) & ST_MASK) == ST_RESP_VALUE) {
	    acb -> acb_settings = ss -> ss_settings;
	    acb -> acb_flags |= ACB_TURN;
	}
	else {
	    acb -> acb_settings = ST_INIT_VALUE << ST_DAT_SHIFT;
	    acb -> acb_flags &= ~ACB_TURN;
	}	    
    }

    if ((pe = ssdu2pe (ss -> ss_data, ss -> ss_cc, NULLCP, &result))
	    == NULLPE) {
	(void) rosaplose (roi, result != PS_ERR_NMEM ? ROS_PROTOCOL
		: ROS_CONGEST, NULLCP, "%s", ps_error (result));
	goto out2;
    }

    SSFREE (ss);

    if (parse_OACS_PConnect (pe, 1, NULLIP, NULLVP, &pconn) == NOTOK) {
	(void) pylose ();
	pe_free (pe);
	goto out2;
    }

    PLOGP (rosap_log,OACS_PConnect, pe, "PConnect", 1);

    bzero ((char *) ros, sizeof *ros);
    ros -> ros_sd = acb -> acb_fd;
    ros -> ros_initiator.roa_addr = ss -> ss_calling;	/* struct copy */
    ros -> ros_port = htons ((u_short) pconn -> pUserData -> applicationProtocol);
    if (pconn -> pUserData -> member_OACS_2 -> offset
	    == type_OACS_ConnectionData_open)
	ros -> ros_data = pe_expunge (pe, pconn -> pUserData
						-> member_OACS_2 -> un.open);
    else
	ros -> ros_data = NULLPE;

    free_OACS_PConnect (pconn);
    return OK;

out2: ;
    bzero ((char *) &ref, sizeof ref);
    (void) SConnResponse (acb -> acb_fd, &ref, NULLSA, SC_CONGEST, 0, 0,
	    SERIAL_NONE, NULLCP, 0, si);
    acb -> acb_fd = NOTOK;

out1: ;
    SSFREE (ss);
    freeacblk (acb);

    return NOTOK;
}

/*    RO-BEGIN.RESPONSE */

int	RoBeginResponse (sd, status, data, roi)
int	sd;
int	status;
PE	data;
struct RoSAPindication *roi;
{
    int	    len,
	    result;
    char   *base;
    PE	pe;
    register struct assocblk   *acb;
    struct SSAPref ref;
    struct SSAPindication sis;
    register struct SSAPindication *si = &sis;
    register struct SSAPabort *sa = &si -> si_abort;
    struct type_OACS_PAccept	paccpt;
    struct type_OACS_DataTransferSyntax	dts;
    struct type_OACS_ConnectionData condata;
    struct member_OACS_4 udata;

    if ((acb = findacblk (sd)) == NULL || (acb -> acb_flags & ACB_CONN))
	return rosaplose (roi, ROS_PARAMETER, NULLCP,
		"invalid association descriptor");
    if (!(acb -> acb_flags & ACB_ROS))
	return rosaplose (roi, ROS_PARAMETER, NULLCP,
		"not an association descriptor for ROS");
    switch (status) {
	case ROS_ACCEPT: 
	    break;

	case ROS_VALIDATE: 
	case ROS_BUSY: 
	    if (data)
		return rosaplose (roi, ROS_PARAMETER, NULLCP,
			"user data not permitted when refusing association");
	    break;

	default: 
	    return rosaplose (roi, ROS_PARAMETER, NULLCP,
		    "bad value for status parameter");
    }
    missingP (roi);

    bzero ((char *) &ref, sizeof ref);	/* ECMA says don't encode this yet */

    base = NULLCP;
    switch (status) {
	case ROS_ACCEPT: 
	    paccpt.member_OACS_3 = &dts;
	    dts.parm = int_OACS_DataTransferSyntax_x409;
	    paccpt.pUserData = &udata;
	    udata.checkpointSize = 0;
	    udata.windowsize = 3;
	    udata.member_OACS_5 = &condata;
	    condata.offset = type_OACS_ConnectionData_open;
	    if (data == NULLPE)
		condata.un.open = pe_alloc (PE_CLASS_UNIV, PE_FORM_PRIM,
					    PE_PRIM_NULL);
	    else
		condata.un.open = data;

	    if (encode_OACS_PAccept (&pe, 1, 0, NULLCP, &paccpt) == NOTOK) {
	no_mem: ;
		(void) rosaplose (roi, ROS_CONGEST, NULLCP, "out of memory");
		goto out1;
	    }
	    status = SC_ACCEPT;
	    break;

	default: 
	    {
	        struct type_OACS_PRefuse prefuse;

	        prefuse.parm = status;
	        if (encode_OACS_PRefuse(&pe, 1, 0, NULLCP, &prefuse) == NOTOK){
		    goto no_mem;
	        }
	    }
	    status = SC_REJECTED;
	    break;
    }

#ifdef	DEBUG
    if (rosap_log -> ll_events & LLOG_PDUS)
	if (status == SC_ACCEPT)
	    pvpdu (rosap_log, print_OACS_PAccept_P, pe, "PAccept", 0);
	else
	    pvpdu (rosap_log, print_OACS_PRefuse_P, pe, "PRefuse", 0);
#endif

    if (pe2ssdu (pe, &base, &len) == NOTOK)
	goto no_mem;

    if (SConnResponse (acb -> acb_fd, &ref, NULLSA, status,
		acb -> acb_requirements, acb -> acb_settings, SERIAL_NONE,
		base, len, si) == NOTOK) {
	acb -> acb_fd = NOTOK;
	(void) ss2roslose (acb, roi, "SConnResponse", sa);
	goto out3;
    }

    if (status == SC_ACCEPT)
	acb -> acb_flags |= ACB_CONN;
    else {
	acb -> acb_fd = NOTOK;
	freeacblk (acb);
    }
    result = OK;

out2: ;
    if (pe) {
	if (data)
	    (void) pe_extract (pe, data);
	pe_free (pe);
    }
    if (base)
	free (base);

    return result;

out1: ;
    (void) SConnResponse (acb -> acb_fd, &ref, NULLSA, SC_CONGEST, 0, 0,
	    SERIAL_NONE, NULLCP, 0, si);
    acb -> acb_fd = NOTOK;
out3: ;
    freeacblk (acb);
    result = NOTOK;
    goto out2;
}
