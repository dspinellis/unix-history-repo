#include <stdio.h>
#include "generic.h"		/* defines OPERATIONS and ERRORS */
#include <isode/rosap.h>

#define error	printf

				/* e.g., "directory" */
static char *myservice = "ROSPTEST";

				/* e.g., "directory services" */
static char *mycontext = "isode chic read";
static char *mypci = "isode chic read pci";


#define INVOKE	1	/* do a RoInvokeRequest */
#define INTREQ	2	/* do a RoIntrRequest */
#define INVERR	3	/* request an error */
#define INVURJ	4	/* request a user reject */
#define INVPRJ	5	/* request a provider reject */

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
    int	servicetype = RoP;

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
    
    if (AcAssocRequest (ctx, NULLAEI, NULLAEI, NULLPA, pa, pc, NULLOID,
		0, ROS_MYREQUIRE, SERIAL_NONE, 0, sf, NULLPEP, 0, NULLQOS,
		acc, aci)
	    == NOTOK)
	error ("A-ASSOCIATE.REQUEST: %s", AcErrString (aca -> aca_reason));

    if (acc -> acc_result != ACS_ACCEPT)
	error ("association rejected: %s", AcErrString (aca -> aca_reason));

    sd = acc -> acc_sd;

    if (RoSetService (sd, RoPService, &rois) == NOTOK)
	error ("RoSetService: %s", RoErrString (rop -> rop_reason));

    invoke (sd, INVOKE);	/* invoke the operations, etc. */
    
    invoke (sd, INTREQ);	/* invoke the operations, etc. */

    invoke (sd, INVERR);	/* invoke the operations, etc. */

    invoke (sd, INVURJ);	/* invoke the operations, etc. */

    invoke (sd, INVPRJ);	/* invoke the operations, etc. */

    if (AcRelRequest (sd, ACF_NORMAL, NULLPEP, 0, NOTOK, acr, aci) == NOTOK)
	error ("A-RELEASE.REQUEST: %s", AcErrString (aca -> aca_reason));

    if (!acr -> acr_affirmative) {
	(void) AcUAbortRequest (sd, NULLPEP, 0, aci);
	error ("release rejected by peer: %d", acr -> acr_reason);
    }

    ACRFREE (acr);

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
