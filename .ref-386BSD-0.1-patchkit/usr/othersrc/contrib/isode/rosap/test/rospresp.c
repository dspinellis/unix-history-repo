/*
 * this is the example use of rosap taken from the manual
 */
#include <stdio.h>
#include "generic.h"
#include <isode/rosap.h>

#define error fprintf

int	ros_indication();

static FILE	*dfp;
main(argc, argv, envp)
int	argc;
char	**argv, **envp;
{
    int	result, sd;
    int	res;

    struct AcSAPstart	acss;
    register struct AcSAPstart	*acs = &acss;
    struct AcSAPindication	acis;
    register struct AcSAPindication	*aci = &acis;
    register struct AcSAPabort	*aca = &aci->aci_abort;
    register struct PSAPstart *ps = &acs->acs_start;
    struct RoSAPindication rois;
    register struct RoSAPindication	*roi = &rois;
    register struct RoSAPpreject	*rop = &roi->roi_preject;
    int	servicetype = RoP;

    if (AcInit(argc, argv, acs, aci) == NOTOK)
	fprintf(stderr, "initialisation fails: %s", AcErrString(aca->aca_reason));

    sd = acs->acs_sd;
    ACSFREE(acs);

    /* would have read command line arguments here */

    if (AcAssocResponse(sd, ACS_ACCEPT, ACS_USER_NULL, NULLOID, NULLAEI,
		&ps->ps_called, NULLPC, ps->ps_defctxresult,
		ps->ps_prequirements, ps->ps_srequirements,
		SERIAL_NONE, ps->ps_settings, &ps->ps_connect,
		NULLPEP, 0, aci) == NOTOK)
	fprintf(stderr, "A-ASSOCIATE.RESPONSE: %s", AcErrString(aca->aca_reason));
    if (RoSetService (sd, RoPService, &rois) == NOTOK)
	error ("RoSetService: %s", RoErrString (rop -> rop_reason));

#ifdef SHOW_BUG
    if (RoSetIndications(sd, ros_indication, roi) == NOTOK)
	fprintf(stderr, "RoSetIndications: %s", RoErrString(rop->rop_reason));
    
     for(;;)
	pause();
#else
    dfp = freopen("/dev/console", "w", stdout);
    fprintf(dfp, "Got to here\n");
    for (;;) {
	switch (res = RoWaitRequest(sd, NOTOK, roi)) {
	case NOTOK:
	    fprintf(stderr,"RoWaitRequest: %s\n", RoErrString(rop->rop_reason));
	    exit(1);

	case OK:
	    fprintf(dfp, "got a request %d\n", res);
	    ros_indication(sd, roi);
	    break;
	
	case DONE:
	    fprintf(dfp, "Done\n");
	    ros_indication(sd, roi);
	    exit(0);	/* should never get to here */
	}
    }
#endif

    fprintf(dfp, "Finished\n");
   
}
/*
 * Request/Reply loop of ROS server. Called when data arrives like a signal
 * routine
 */
static int
ros_indication(sd, roi)
int	sd;
register struct RoSAPindication	*roi;
{

    fprintf(dfp, "ros_indication got called\n");
    switch (roi->roi_type) {
    case ROI_INVOKE:
	ros_invoke(sd, &roi->roi_invoke);
	break;

    case ROI_RESULT:
	ros_result(sd, &roi->roi_result);
	break;

    case ROI_ERROR:
	ros_error(sd, &roi->roi_error);
	break;


    case ROI_UREJECT:
	ros_ureject(sd, &roi->roi_ureject);
	break;

    case ROI_PREJECT:
	ros_preject(sd, &roi->roi_preject);
	break;

    case ROI_FINISH:
	ros_finish(sd, &roi->roi_finish);
	break;

    default:
	fprintf(dfp, "unknown indication type=%d", roi->roi_type);
    }
}

extern int	OP1();



/* OPERATIONS are numbered APDU_OPx, where each is a unique integer.  Further,
   APDU_UNKNOWN is used as a tag different than any valid operation.

   ERRORS are numbered ERROR_xyz, where each is a unique integer.
   ERROR_MISTYPED is used to signal an argument error to an operation.
   Further, ERROR_UNKNOWN is used as a tag to indicate that the operation
   succeeded.

   Finally, note that rox -> rox_args is updated in place by these routines.
   If the routine returns ERROR_UNKNOWN, then rox_args contains the results
   of the operation.  If the routine returns ERROR_MISTYPED, then rox_args is
   untouched.  Otherwise, if the routine returns any other value, then
   rox_args contains the parameters of the error which occurred.  Obviously,
   each routine calls ROXFREE prior to setting rox_args to a new value.
 */

static struct dispatch {
    int     ds_operation;
    IFP     ds_vector;
}       dispatches[] = {
    APDU_OP1,   OP1,
    APDU_ERR,   OP1,
    APDU_URJ,   OP1,

    /* APDU_OPn,   OPn, */

    APDU_UNKNOWN
};


static int  ros_invoke (sd, rox)
int     sd;
register struct RoSAPinvoke *rox;
{
    int     result;
    register struct dispatch   *ds;
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;

    for (ds = dispatches; ds -> ds_operation != APDU_UNKNOWN; ds++)
	if (ds -> ds_operation == rox -> rox_op)
	    break;

    if (ds -> ds_operation == APDU_UNKNOWN) {
	if (RoURejectRequest (sd, &rox -> rox_id, ROS_IP_UNRECOG,
		    ROS_NOPRIO, roi) == NOTOK)
	    error (dfp, "RO-U-REJECT.REQUEST: %s", RoErrString (rop -> rop_reason));
	goto out;
    }

    if (rox -> rox_nolinked == 0) {
	if (RoURejectRequest (sd, &rox -> rox_id, ROS_IP_LINKED,
		    ROS_NOPRIO, roi) == NOTOK)
	    error (dfp, "RO-U-REJECT.REQUEST: %s", RoErrString (rop -> rop_reason));
	goto out;
    }

    switch (result = (*ds -> ds_vector) (rox)) {
	case ERROR_UNKNOWN: 
	    if (RoResultRequest (sd, rox -> rox_id, rox -> rox_op,
			rox -> rox_args, ROS_NOPRIO, roi) == NOTOK)
		error (dfp, "RO-RESULT.REQUEST: %s",
			RoErrString (rop -> rop_reason));
	    break;

	default: 
	    if (RoErrorRequest (sd, rox -> rox_id, result, rox -> rox_args,
			ROS_NOPRIO, roi) == NOTOK)
		error (dfp, "RO-ERROR.REQUEST: %s",
			RoErrString (rop -> rop_reason));
	    break;

	case ERROR_MISTYPED: 
	    if (RoURejectRequest (sd, &rox -> rox_id, ROS_IP_MISTYPED,
			ROS_NOPRIO, roi) == NOTOK)
		error (dfp, "RO-U-REJECT.REQUEST: %s",
			RoErrString (rop -> rop_reason));
	    break;
    }

out: ;
    ROXFREE (rox);
}


static int  ros_result (sd, ror)
int     sd;
register struct RoSAPresult *ror;
{
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;

    if (RoURejectRequest (sd, &ror -> ror_id, ROS_RRP_UNRECOG, ROS_NOPRIO, roi)
	    == NOTOK)
	error (dfp, "RO-U-REJECT.REQUEST: %s", RoErrString (rop -> rop_reason));

    RORFREE (ror);
}


static int  ros_error (sd, roe)
int     sd;
register struct RoSAPerror *roe;
{
    struct RoSAPindication  rois;
    register struct RoSAPindication *roi = &rois;
    register struct RoSAPpreject   *rop = &roi -> roi_preject;

    if (RoURejectRequest (sd, &roe -> roe_id, ROS_REP_UNRECOG, ROS_NOPRIO, roi)
	    == NOTOK)
	error (dfp, "RO-U-REJECT.REQUEST: %s", RoErrString (rop -> rop_reason));

    ROEFREE (roe);
}


static int  ros_ureject (sd, rou)
int     sd;
register struct RoSAPureject *rou;
{
/* handle rejection here... */
}


static int  ros_preject (sd, rop)
int     sd;
register struct RoSAPpreject *rop;
{
    if (ROS_FATAL (rop -> rop_reason))
	error (dfp, "RO-REJECT-P.INDICATION: %s", RoErrString (rop -> rop_reason));

/* handle temporary failure here... */
}

static int  ros_finish (sd, acf)
int     sd;
struct AcSAPfinish *acf;
{
    struct AcSAPindication  acis;
    register struct AcSAPabort *aca = &acis.aci_abort;

    ACFFREE (acf);

    if (AcRelResponse (sd, ACS_ACCEPT, ACR_NORMAL, NULLPEP, 0, &acis) == NOTOK)
	error (dfp, "A-RELEASE.RESPONSE: %s", AcErrString (aca -> aca_reason));

    error (dfp, "association released");

    exit(0);
}

OP1(rox)
register struct RoSAPinvoke *rox;
{
    fprintf(dfp, "Invocation\nid %d", rox->rox_id);
    if (!rox->rox_nolinked)
	fprintf(dfp, " linked to %d", rox->rox_linkid);
    fprintf(dfp, " operation %d\n", rox->rox_op);
    /* print the pe */

    switch (rox->rox_op) {
    case APDU_OP1:
	return (ERROR_UNKNOWN);

    case APDU_ERR:
	return (ERROR_ERROR);

    case APDU_URJ:
	return (ERROR_MISTYPED);

    default:
	fprintf(dfp, "\nunknown operation %d\n", rox->rox_op);
    }
    return (ERROR_ERROR);
}
