#include <stdio.h>
#include <ctype.h>
#include "generic.h"		/* defines OPERATIONS and ERRORS */
#include <isode/rtsap.h>
#include <isode/rosap.h>
#include <isode/isoservent.h>

#define error	printf

				/* e.g., "directory" */
static char *myservice = "ROSRTBTEST";

				/* e.g., "directory services" */
static char *mycontext = "isode chic read";
static char *mypci = "isode chic read pci";


#define INVOKE	1	/* do a RoInvokeRequest */
#define INTREQ	2	/* do a RoIntrRequest */
#define INVERR	3	/* request an error */
#define INVURJ	4	/* request a user reject */
#define INVPRJ	5	/* request a provider reject */

#define TIMEOUT	30	/* seconds before RtWait times out */

extern struct isoservent	*getisoserventbyname();
extern PE	mkpelist();

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    sd;
#if 0
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
    register AEI aei;
    register OID ctx, pci;
    struct PSAPctxlist pcs;
    register struct PSAPctxlist *pc = &pcs;
#endif
    struct RtSAPaddr	rtas;
    struct isoservent   *is;
    struct SSAPaddr     *psa;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    struct RtSAPconnect rtcs;
    register struct RtSAPconnect   *rtc = &rtcs;
    register struct RtSAPabort *rta = &rti -> rti_abort;
    struct RoSAPindication  rois;
    register struct RoSAPpreject   *rop = &rois.roi_preject;

    if ((is = getisoserventbyname(myservice, "rtsap")) == NULL) {
        fprintf(stderr, "can't find %s/rosap", myservice);
        exit(1);
    }
    rtas.rta_port = is->is_port;
    if (argc < 2) {
        fprintf(stderr, "Need an arguement of a hostname\n");
        exit(1);
    }
    /* This is an different to the manual which is wrong! 
     */
    if ((is = getisoserventbyname("rts", "ssap")) == NULL) {
        fprintf(stderr, "can't find ssap/rts");
        exit(1);
    }
    if ((psa = is2saddr(argv[1], NULLCP, (struct isoservent *) is)) == NULLSA) {        fprintf(stderr, "Can't compute address to %s\n", argv[1]);
        exit(2);
    }
 
    rtas.rta_addr = *psa; /* struct copy */
 
    fprintf(stderr, "RT-OPEN.REQUEST:\n");
    if (RtBeginRequest (&rtas, RTS_TWA, RTS_INITIATOR, NULLPE, rtc, rti)
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
    if (RtEndRequest (sd, &rtis) == NOTOK)
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
    PE data;

    invoke = 1;

    data = mkpelist(2);
    switch (type) {
    case INVOKE:
	res = RoInvokeRequest(sd, APDU_OP1, ROS_SYNC, data, invoke, NULLIP,
	ROS_NOPRIO, &rind);
	break;

    case INTREQ:
	res = RoIntrRequest(sd, APDU_OP1, data, invoke, NULLIP, ROS_NOPRIO,
	&rind);
	break;

    case INVERR:
	res = RoInvokeRequest(sd, APDU_ERR, ROS_SYNC, data, invoke, NULLIP,
	ROS_NOPRIO, &rind);
	break;

    case INVURJ:
	res = RoInvokeRequest(sd, APDU_URJ, ROS_SYNC, data, invoke, NULLIP,
	ROS_NOPRIO, &rind);
	break;

    case INVPRJ:
	res = RoInvokeRequest(sd, APDU_PRJ, ROS_SYNC, data, invoke, NULLIP,
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
    pe_free(data);

}
/*
 * General routines useful for supporting the tests of rtsap library routines
 */

#define PE_SIZE		3	/* size to build pe's for testing transfer */
#define MKMASK 0x7
#define MKSHIFT 6

extern	PE	mkpe();
/*
 * Generate a randomish list of PElement s for use as ANY or SET  OF ANY ....
 */
PE
mkpelist(i)
int	i;
{
    PE pe, fpe = NULL;

    fpe = pe_alloc(PE_CLASS_PRIV, PE_FORM_CONS, i);
    while (i > 0) {
	pe = mkpe(i);
	pe->pe_next = fpe->pe_cons;
	fpe->pe_cons = pe;
	i--;
    }
    return (fpe);
}

/*
 * generate a randomish PElement
 */
PE
mkpe(i)
{
    int	id, class;
    PE 	pe;

    id = i * i + 1;
    class = PE_CLASS_PRIV;
    switch ((i*i >> MKSHIFT) & MKMASK) {
    case 5:
    case 0:
	pe = flag2prim(i & 0x1, class, id);
	break;
    
    case 6:
    case 1:
	pe = num2prim(i, class, id);
	break;

    case 7:
    case 2:
	pe = str2prim("mkpelist:testdata", 17, class, id);
	break;

    case 3:
	pe = strb2bitstr("\021\0245\375\0124", 4, class, id);
	break;
    
    case 4:
	pe = mkpelist(i - 1);
	break;

    default:
	fprintf(stderr, "mkpe:internal error %d case not handled\n",
	    (i*i >> MKSHIFT) & MKMASK);
	exit(2);
    }

    return (pe);
}
/*
 * Dump a bunch of hex digits printing out those that are printable
 * Print out a given length of octets as hex (with the ASCII characters
 * given if they have any
 */
fpclen(fp, s, len)
register FILE	*fp;
register char	*s;
register int	len;
{
	register int	cnt = 0;

	while (len-- > 0) {
		if (cnt % 8 == 0)
			fprintf(fp, "\n%d:", cnt/8 + 1);
		if (isprint(*s&0x7f))
			fprintf(fp, "\t%02x(%c)", *s&0xff, *s&0x7f);
		else
			fprintf(fp, "\t%02x", *s&0xff);
		s++;
		cnt++;
	}
	fputc('\n', fp);
}

