#include <stdio.h>
#include "generic.h"		/* defines OPERATIONS and ERRORS */
#include <isode/rtsap.h>
#include <isode/rosap.h>

#define error	printf

				/* e.g., "directory" */
static char *myservice = "ROSRTTEST";

				/* e.g., "directory services" */
static char *mycontext = "isode chic read";
static char *mypci = "isode chic read pci";


#define INVOKE	1	/* do a RoInvokeRequest */
#define INTREQ	2	/* do a RoIntrRequest */
#define INVERR	3	/* request an error */
#define INVURJ	4	/* request a user reject */
#define INVPRJ	5	/* request a provider reject */

#define TIMEOUT	30	/* seconds before RtWait times out */

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    sd;
    struct SSAPref sfs;
    register struct SSAPref *sf;
    register struct PSAPaddr *pa;
    struct AcSAPconnect accs;
    register struct AcSAPconnect   *acc = &accs;
    struct AcSAPrelease acrs;
    register struct AcSAPrelease   *acr = &acrs;
    struct AcSAPindication  acis;
    register struct AcSAPindication *aci = &acis;
    register struct AcSAPabort *aca = &aci -> aci_abort;
    struct RoSAPindication  rois;
    register struct RoSAPpreject   *rop = &rois.roi_preject;
    register AEI aei;
    register OID ctx, pci;
    struct PSAPctxlist pcs;
    register struct PSAPctxlist *pc = &pcs;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    struct RtSAPconnect rtcs;
    register struct RtSAPconnect   *rtc = &rtcs;
    register struct RtSAPabort *rta = &rti -> rti_abort;


    if ((aei = str2aei (argv[1], myservice)) == NULLAEI)
	error ("%s-%s: unknown application-entity", argv[1], myservice);
    if ((pa = aei2addr (aei)) == NULLPA)
	error ("address translation failed");
    if ((ctx = ode2oid (mycontext)) == NULLOID)
	error ("%s: unknown object descriptor", mycontext);
    if ((ctx = oid_cpy (ctx)) == NULLOID)
	error ("oid_cpy");
    if ((pci = ode2oid (mypci)) == NULLOID)
	error ("%s: unknown object descriptor", mypci);
    if ((pci = oid_cpy (pci)) == NULLOID)
	error ("oid_cpy");
    pc -> pc_nctx = 1;
    pc -> pc_ctx[0].pc_id = 1;
    pc -> pc_ctx[0].pc_asn = pci;
    pc -> pc_ctx[0].pc_atn = NULLOID;


    if ((sf = addr2ref (PLocalHostName ())) == NULL) {
	sf = &sfs;
	(void) bzero ((char *) sf, sizeof *sf);
    }
    

    fprintf(stderr, "RT-OPEN.REQUEST:\n");
    if (RtOpenRequest (RTS_TWA, RTS_INITIATOR, ctx, NULLAEI, NULLAEI,
	NULLPA, pa, pc, NULLOID, 0, NULLQOS, rtc, rti)
	    == NOTOK)
	fprintf(stderr, "RT-OPEN.REQUEST: %s", RtErrString (rta -> rta_reason));

    if (rtc -> rtc_result != RTS_ACCEPT)
	fprintf(stderr, "association rejected: %s",
	RtErrString (rta->rta_reason));

    sd = rtc -> rtc_sd;
    RTCFREE (rtc);

    if (RoSetService (sd, RoRtService, &rois) == NOTOK)
	error ("RoSetService: %s", RoErrString (rop -> rop_reason));

    invoke (sd, INVOKE);	/* invoke the operations, etc. */
    
    invoke (sd, INTREQ);	/* invoke the operations, etc. */

    invoke (sd, INVERR);	/* invoke the operations, etc. */

    invoke (sd, INVURJ);	/* invoke the operations, etc. */

    invoke (sd, INVPRJ);	/* invoke the operations, etc. */

    /* All this appears to be neccessary because you need the turn to terminate
     * nicely. But we don't have the turn, I presume the responder has kept it
     * from when they replyed to our request
     */

    if (RtPTurnRequest(sd, 0, &rtis) == NOTOK) {
	fprintf(stderr, "SEND_PLS:RT-PLEASE-TURN.REQUEST: %s\n",
	    RtErrString (rtis.rti_abort.rta_reason));
	exit(6);
    }
    if (RtWaitRequest(sd, TIMEOUT, &rtis) == NOTOK) {
	fprintf(stderr, "RtWaitRequest: %s\n",
	    RtErrString (rtis.rti_abort.rta_reason));
	exit(1);
    }
    switch (rtis.rti_type) {
    case RTI_TURN:
	/* Okay we got it */
	break;

    default:
	fprintf(stderr, "unexpected response %d\n", rtis.rti_type);
	exit(6);
    }

    /* Now we can close */
    fprintf (stderr, "RT-CLOSE.REQUEST:\n");
    if (RtCloseRequest (sd, ACF_NORMAL, NULLPE, acr, &rtis) == NOTOK)
	fprintf (stderr, "RT-CLOSE.REQUEST: %s\n",
	    RtErrString (rtis.rti_abort.rta_reason));

    exit (0);
}

/*
 * Test example
 */
invoke(sd, type)
int	sd;
int	type;	/* of invocation */
{
    int	invoke;
    struct RoSAPindication	rind;
    int	res;

    invoke = 1;

    switch (type) {
    case INVOKE:
	res = RoInvokeRequest(sd, APDU_OP1, ROS_SYNC, NULLPE, invoke, NULLIP,
	ROS_NOPRIO, &rind);
	break;

    case INTREQ:
	res = RoIntrRequest(sd, APDU_OP1, NULLPE, invoke, NULLIP, ROS_NOPRIO,
	&rind);
	break;

    case INVERR:
	res = RoInvokeRequest(sd, APDU_ERR, ROS_SYNC, NULLPE, invoke, NULLIP,
	ROS_NOPRIO, &rind);
	break;

    case INVURJ:
	res = RoInvokeRequest(sd, APDU_URJ, ROS_SYNC, NULLPE, invoke, NULLIP,
	ROS_NOPRIO, &rind);
	break;

    case INVPRJ:
	res = RoInvokeRequest(sd, APDU_PRJ, ROS_SYNC, NULLPE, invoke, NULLIP,
	ROS_NOPRIO, &rind);
	break;

    default:
	fprintf(stderr, "invoke called with illegal type %d\n", type);
	exit(1);

    }

    switch (res) {
    case NOTOK:
	if (rind.roi_type == ROI_PREJECT)
	    error("RO-INVOKE.REQUEST: %s\n",
		RoErrString(rind.roi_preject.rop_reason));
	else
	    error("RO-INVOKE.REQUEST:failed: unexpected returned type %d\n",
		rind.roi_type);
	exit(1);
    
    case OK:
	break;

    default:
	error("RO-INVOKE.REQUEST:failed(%d): unexpected returned type %d\n",
	    res, rind.roi_type);
	exit(2);

    }

    switch (rind.roi_type) {
    case ROI_RESULT:
	if (rind.roi_result.ror_id == invoke)
	    printf("Result received\n");
	else
	    printf("Result for wrong request %d\n", rind.roi_result.ror_id);
	break;
    
    case ROI_ERROR:
	if (rind.roi_error.roe_id == invoke)
	    printf("Error received\n");
	else
	    printf("Error for wrong request %d\n", rind.roi_error.roe_id);
	break;

    case ROI_UREJECT:
	if (rind.roi_ureject.rou_id == invoke)
	    printf("User Reject received reason %d\n",
		rind.roi_ureject.rou_reason);
	else
	    printf("User Reject for wrong request %d\n",
		rind.roi_ureject.rou_id);
	break;

    case ROI_PREJECT:
	if (rind.roi_preject.rop_id == invoke)
	    printf("Provider Reject received %s\n",
	    RoErrString(rind.roi_preject.rop_reason));
	else
	    printf("Provider Reject for wrong request %d\n",
		rind.roi_preject.rop_id);
	break;

    default:
	printf("Unexpected reply received %d\n", rind.roi_type);
	break;
    }

}
