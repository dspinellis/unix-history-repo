/*
 * this is the example use of rtsap taken from the manual
 */
#include <stdio.h>
#include <isode/rtsap.h>
#include "support.h"

int	rts_indication();

FILE	*errfp;
main(argc, argv, envp)
int	argc;
char	**argv, **envp;
{
    int	result, sd;
    int	res;

    struct RtSAPstart	rtss;
    register struct RtSAPstart	*rts = &rtss;
    struct RtSAPindication	rtis;
    register struct RtSAPindication	*rti = &rtis;
    register struct AcSAPstart  *acs = &rts->rts_start;
    register struct PSAPstart *ps = &acs->acs_start;
    register struct RtSAPabort *rta = &rti -> rti_abort;

    errfp = freopen("/dev/console", "w", stdout);
    fprintf(errfp, "Got to here\n");

    if (RtInit(argc, argv, rts, rti) == NOTOK)
	fprintf(errfp, "initialisation fails: %s",
	    RtErrString(rta->rta_reason));

    sd = rts->rts_sd;
    RTSFREE(rts);

    /* would have read command line arguments here */

    if (RtOpenResponse(sd, ACS_ACCEPT, NULLOID, NULLAEI,
		 &ps->ps_called, NULLPC, ps->ps_defctxresult,
		NULLPE, rti) == NOTOK)
	fprintf(errfp, "RT-OPEN.RESPONSE: %s",
	    RtErrString(rti->rti_abort.rta_reason));
    
#ifdef DO_ASYNC
    if (RtSetIndications(sd, rts_indication, rti) == NOTOK)
	fprintf(stderr, "RoSetIndications: %s",
	    RtErrString(rti->rti_abort.rta_reason));
    for(;;)
	pause();
#else
    
    oper(sd, SIMP_RCV);

    oper(sd, CPLX_RCV);

    oper(sd, SEND_PLS);

    oper(sd, RCV_GIVE);

    oper(sd, SIMP_SEND);

    oper(sd, CPLX_SEND);

    oper(sd, RCV_PLS);

    oper(sd, SEND_GIVE);

    oper(sd, CPLX_RCV);

    oper(sd, RCV_CLOSE);

#endif
    fprintf(errfp, "Finished\n");
   
}
#if 0
/*
 * Request/Reply loop of ROS server. Called when data arrives like a signal
 * handler routine
 */
static int
rts_indication(sd, rti)
int	sd;
register struct RtSAPindication	*rti;
{

    fprintf(errfp, "rts_indication got called\n");
    switch (rti->roi_type) {
    case ROI_INVOKE:
	rts_invoke(sd, &rti->roi_invoke);
	break;

    case ROI_RESULT:
	rts_result(sd, &rti->roi_result);
	break;

    case ROI_ERROR:
	rts_error(sd, &rti->roi_error);
	break;


    case ROI_UREJECT:
	rts_ureject(sd, &rti->roi_ureject);
	break;

    case ROI_PREJECT:
	rts_preject(sd, &rti->roi_preject);
	break;

    case ROI_FINISH:
	rts_finish(sd, &rti->roi_finish);
	break;

    default:
	fprintf(errfp, "unknown indication type=%d", rti->roi_type);
    }
}

extern int	OP1();



/* OPERATIONS are numbered APDU_OPx, where each is a unique integer.  Further,
   APDU_UNKNOWN is used as a tag different than any valid operation.

   ERRORS are numbered ERROR_xyz, where each is a unique integer.
   ERROR_MISTYPED is used to signal an argument fprintf to an operation.
   Further, ERROR_UNKNOWN is used as a tag to indicate that the operation
   succeeded.

   Finally, note that rox -> rox_args is updated in place by these routines.
   If the routine returns ERROR_UNKNOWN, then rox_args contains the results
   of the operation.  If the routine returns ERROR_MISTYPED, then rox_args is
   untouched.  Otherwise, if the routine returns any other value, then
   rox_args contains the parameters of the fprintf which occurred.  Obviously,
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


static int  rts_invoke (sd, rox)
int     sd;
register struct RoSAPinvoke *rox;
{
    int     result;
    register struct dispatch   *ds;
    struct RoSAPindication  rois;
    register struct RoSAPindication *rti = &rois;
    register struct RoSAPpreject   *rop = &rti -> roi_preject;

    for (ds = dispatches; ds -> ds_operation != APDU_UNKNOWN; ds++)
	if (ds -> ds_operation == rox -> rox_op)
	    break;

    if (ds -> ds_operation == APDU_UNKNOWN) {
	if (RoURejectRequest (sd, &rox -> rox_id, ROS_IP_UNRECOG,
		    ROS_NOPRIO, rti) == NOTOK)
	    fprintf (errfp, "RO-U-REJECT.REQUEST: %s", RoErrString (rop -> rop_reason));
	goto out;
    }

    if (rox -> rox_nolinked == 0) {
	if (RoURejectRequest (sd, &rox -> rox_id, ROS_IP_LINKED,
		    ROS_NOPRIO, rti) == NOTOK)
	    fprintf (errfp, "RO-U-REJECT.REQUEST: %s", RoErrString (rop -> rop_reason));
	goto out;
    }

    switch (result = (*ds -> ds_vector) (rox)) {
	case ERROR_UNKNOWN: 
	    if (RoResultRequest (sd, rox -> rox_id, rox -> rox_op,
			rox -> rox_args, ROS_NOPRIO, rti) == NOTOK)
		fprintf (errfp, "RO-RESULT.REQUEST: %s",
			RoErrString (rop -> rop_reason));
	    break;

	default: 
	    if (RoErrorRequest (sd, rox -> rox_id, result, rox -> rox_args,
			ROS_NOPRIO, rti) == NOTOK)
		fprintf (errfp, "RO-ERROR.REQUEST: %s",
			RoErrString (rop -> rop_reason));
	    break;

	case ERROR_MISTYPED: 
	    if (RoURejectRequest (sd, &rox -> rox_id, ROS_IP_MISTYPED,
			ROS_NOPRIO, rti) == NOTOK)
		fprintf (errfp, "RO-U-REJECT.REQUEST: %s",
			RoErrString (rop -> rop_reason));
	    break;
    }

out: ;
    ROXFREE (rox);
}


static int  rts_result (sd, ror)
int     sd;
register struct RoSAPresult *ror;
{
    struct RoSAPindication  rois;
    register struct RoSAPindication *rti = &rois;
    register struct RoSAPpreject   *rop = &rti -> roi_preject;

    if (RoURejectRequest (sd, &ror -> ror_id, ROS_RRP_UNRECOG, ROS_NOPRIO, rti)
	    == NOTOK)
	fprintf (errfp, "RO-U-REJECT.REQUEST: %s", RoErrString (rop -> rop_reason));

    RORFREE (ror);
}


static int  rts_error (sd, roe)
int     sd;
register struct RoSAPerror *roe;
{
    struct RoSAPindication  rois;
    register struct RoSAPindication *rti = &rois;
    register struct RoSAPpreject   *rop = &rti -> roi_preject;

    if (RoURejectRequest (sd, &roe -> roe_id, ROS_REP_UNRECOG, ROS_NOPRIO, rti)
	    == NOTOK)
	fprintf (errfp, "RO-U-REJECT.REQUEST: %s", RoErrString (rop -> rop_reason));

    ROEFREE (roe);
}


static int  rts_ureject (sd, rou)
int     sd;
register struct RoSAPureject *rou;
{
/* handle rejection here... */
}


static int  rts_preject (sd, rop)
int     sd;
register struct RoSAPpreject *rop;
{
    if (ROS_FATAL (rop -> rop_reason))
	fprintf (errfp, "RO-REJECT-P.INDICATION: %s", RoErrString (rop -> rop_reason));

/* handle temporary failure here... */
}

static int  rts_finish (sd, acf)
int     sd;
struct AcSAPfinish *acf;
{
    struct AcSAPindication  acis;
    register struct AcSAPabort *aca = &acis.aci_abort;

    ACFFREE (acf);

    if (AcRelResponse (sd, ACS_ACCEPT, ACR_NORMAL, NULLPEP, 0, &acis) == NOTOK)
	fprintf (errfp, "A-RELEASE.RESPONSE: %s", AcErrString (aca -> aca_reason));

    fprintf (errfp, "association released");

    exit(0);
}

OP1(rox)
register struct RoSAPinvoke *rox;
{
    fprintf(errfp, "Invocation\nid %d", rox->rox_id);
    if (!rox->rox_nolinked)
	fprintf(errfp, " linked to %d", rox->rox_linkid);
    fprintf(errfp, " operation %d\n", rox->rox_op);
    /* print the pe */

    switch (rox->rox_op) {
    case APDU_OP1:
	return (ERROR_UNKNOWN);

    case APDU_ERR:
	return (ERROR_ERROR);

    case APDU_URJ:
	return (ERROR_MISTYPED);

    default:
	fprintf(errfp, "\nunknown operation %d\n", rox->rox_op);
    }
    return (ERROR_ERROR);
}
#endif
