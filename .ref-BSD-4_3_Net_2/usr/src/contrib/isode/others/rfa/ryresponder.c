/*
 * RFA - Remote File Access
 *
 * Access and Management for a partial file system tree that exists
 * at two sites either as master files or slave files
 *
 * ryresponder.c : responders interface to the ISODE Ry-Library
 *
 * Contributed by Oliver Wenzel, GMD Berlin, 1990
 *
 * $Header: /f/osi/others/rfa/RCS/ryresponder.c,v 7.3 91/02/22 09:28:30 mrose Interim $
 *
 * $Log:	ryresponder.c,v $
 * Revision 7.3  91/02/22  09:28:30  mrose
 * Interim 6.8
 * 
 * Revision 7.2  91/01/14  13:55:07  mrose
 * update
 * 
 * Revision 1.1  91/01/04  16:08:19  ow
 * Initial revision
 * 
 */

#ifndef       lint
static char *rcsid = "$Header: /f/osi/others/rfa/RCS/ryresponder.c,v 7.3 91/02/22 09:28:30 mrose Interim $";
#endif

/*
 *                              NOTICE
 *
 *    Acquisition, use, and distribution of this module and related
 *    materials are subject to the restrictions of a license agreement.
 *    Consult the Preface in the User's Manual for the full terms of
 *    this agreement.
 *
 */

#include <stdio.h>
#include <setjmp.h>
#include <varargs.h>
#include "ryresponder.h"
#include "tsap.h"		/* for listening */
#include "RFA-ops.h"
#include "rfa.h"


int	debug = 0;
IFP	startfnx;
IFP	stopfnx;

static jmp_buf toplevel;

extern int  errno;


int  ros_init (vecp, vec)
int	vecp;
char  **vec;
{
    int	    reply,
	    result,
	    sd;
    struct AcSAPstart   acss;
    register struct AcSAPstart *acs = &acss;
    struct AcSAPindication  acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort   *aca = &aci -> aci_abort;
    register struct PSAPstart *ps = &acs -> acs_start;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;
    PE pe[1];

    if (AcInit (vecp, vec, acs, aci) == NOTOK) {
	acs_advise (aca, "initialization fails");
	return NOTOK;
    }
    advise (LLOG_NOTICE, NULLCP,
		"A-ASSOCIATE.INDICATION: <%d, %s, %s, %s, %d>",
		acs -> acs_sd, oid2ode (acs -> acs_context),
		sprintaei (&acs -> acs_callingtitle),
		sprintaei (&acs -> acs_calledtitle), acs -> acs_ninfo);

    sd = acs -> acs_sd;

    for (vec++; *vec; vec++)
	advise (LLOG_EXCEPTIONS, NULLCP, "unknown argument \"%s\"", *vec);

    reply = startfnx ? (*startfnx) (sd, acs, &pe[0]) : ACS_ACCEPT;

    result = AcAssocResponse (sd, reply, 
		reply != ACS_ACCEPT ? ACS_USER_NOREASON : ACS_USER_NULL,
		NULLOID, NULLAEI, NULLPA, NULLPC, ps -> ps_defctxresult,
		ps -> ps_prequirements, ps -> ps_srequirements, SERIAL_NONE,
		ps -> ps_settings, &ps -> ps_connect, pe,
		reply != ACS_ACCEPT ? 1 : 0, aci);

    ACSFREE (acs);

    if (result == NOTOK) {
	acs_advise (aca, "A-ASSOCIATE.RESPONSE");
	return NOTOK;
    }
    if (reply != ACS_ACCEPT)
	return NOTOK;

    if (RoSetService (sd, RoPService, roi) == NOTOK)
	ros_adios (rop, "set RO/PS fails");

    return sd;
}


int  ros_work (fd)
int	fd;
{
    int	    result;
    caddr_t out;
    struct AcSAPindication  acis;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;

    switch (setjmp (toplevel)) {
	case OK: 
	    break;

	default: 
	    if (stopfnx)
		(*stopfnx) (fd, (struct AcSAPfinish *) 0);
	case DONE:
	    (void) AcUAbortRequest (fd, NULLPEP, 0, &acis);
	    (void) RyLose (fd, roi);
	    return NOTOK;
    }

    switch (result = RyWait (fd, NULLIP, &out, OK, roi)) {
	case NOTOK: 
	    if (rop -> rop_reason == ROS_TIMER)
		break;
	case OK: 
	case DONE: 
	    ros_indication (fd, roi);
	    break;

	default: 
	    adios (NULLCP, "unknown return from RoWaitRequest=%d", result);
    }

    return OK;
}


int ros_indication (sd, roi)
int	sd;
register struct RoSAPindication *roi;
{
    int	    reply,
	    result;

    switch (roi -> roi_type) {
	case ROI_INVOKE: 
	case ROI_RESULT: 
	case ROI_ERROR: 
	    adios (NULLCP, "unexpected indication type=%d", roi -> roi_type);
	    break;

	case ROI_UREJECT: 
	    {
		register struct RoSAPureject   *rou = &roi -> roi_ureject;

		if (rou -> rou_noid)
		    advise (LLOG_EXCEPTIONS, NULLCP, "RO-REJECT-U.INDICATION/%d: %s",
			    sd, RoErrString (rou -> rou_reason));
		else
		    advise (LLOG_EXCEPTIONS, NULLCP,
			    "RO-REJECT-U.INDICATION/%d: %s (id=%d)",
			    sd, RoErrString (rou -> rou_reason),
			    rou -> rou_id);
	    }
	    break;

	case ROI_PREJECT: 
	    {
		register struct RoSAPpreject   *rop = &roi -> roi_preject;

		if (ROS_FATAL (rop -> rop_reason))
		    ros_adios (rop, "RO-REJECT-P.INDICATION");
		ros_advise (rop, "RO-REJECT-P.INDICATION");
	    }
	    break;

	case ROI_FINISH: 
	    {
		register struct AcSAPfinish *acf = &roi -> roi_finish;
		struct AcSAPindication  acis;
		register struct AcSAPabort *aca = &acis.aci_abort;

		advise (LLOG_NOTICE, NULLCP, "A-RELEASE.INDICATION/%d: %d",
			sd, acf -> acf_reason);

		reply = stopfnx ? (*stopfnx) (sd, acf) : ACS_ACCEPT;

		result = AcRelResponse (sd, reply, ACR_NORMAL, NULLPEP, 0,
			    &acis);

		ACFFREE (acf);

		if (result == NOTOK)
		    acs_advise (aca, "A-RELEASE.RESPONSE");
		else
		    if (reply != ACS_ACCEPT)
			break;
		longjmp (toplevel, DONE);
	    }
	/* NOTREACHED */

	default: 
	    adios (NULLCP, "unknown indication type=%d", roi -> roi_type);
    }
}


int  ros_lose (td)
struct TSAPdisconnect *td;
{
    if (td -> td_cc > 0)
	adios (NULLCP, "TNetAccept: [%s] %*.*s",
		TErrString (td -> td_reason), td -> td_cc, td -> td_cc,
		td -> td_data);
    else
	adios (NULLCP, "TNetAccept: [%s]", TErrString (td -> td_reason));
}


void	ros_adios (rop, event)
register struct RoSAPpreject *rop;
char   *event;
{
    ros_advise (rop, event);

    cleanup ();

    longjmp (toplevel, NOTOK);
}


void	ros_advise (rop, event)
register struct RoSAPpreject *rop;
char   *event;
{
    char    buffer[BUFSIZ];

    if (rop -> rop_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s", RoErrString (rop -> rop_reason),
		rop -> rop_cc, rop -> rop_cc, rop -> rop_data);
    else
	(void) sprintf (buffer, "[%s]", RoErrString (rop -> rop_reason));

    advise (LLOG_EXCEPTIONS, NULLCP, "%s: %s", event, buffer);
}


void	acs_advise (aca, event)
register struct AcSAPabort *aca;
char   *event;
{
    char    buffer[BUFSIZ];

    if (aca -> aca_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s",
		AcErrString (aca -> aca_reason),
		aca -> aca_cc, aca -> aca_cc, aca -> aca_data);
    else
	(void) sprintf (buffer, "[%s]", AcErrString (aca -> aca_reason));

    advise (LLOG_EXCEPTIONS, NULLCP, "%s: %s (source %d)", event, buffer,
		aca -> aca_source);
}



/*--------------------------------------------------------------*/
/*  ureject							*/
/*--------------------------------------------------------------*/
int  ureject (sd, reason, rox, roi)
int sd,
    reason;
struct RoSAPinvoke *rox;
struct RoSAPindication *roi;
{
    if (RyDsUReject (sd, rox -> rox_id, reason, ROS_NOPRIO, roi) == NOTOK)
	ros_adios (&roi -> roi_preject, "U-REJECT");

    return OK;
}

