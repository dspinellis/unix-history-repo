#include <stdio.h>
#include <isode/rtsap.h>

#include "support.h"	/* defines operation values */

				/* e.g., "directory" */
static char *myservice = "RTSTEST";

				/* e.g., "directory services" */
static char *mycontext = "isode chic read";
static char *mypci = "isode chic read pci";

FILE *errfp = stderr;

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    sd;
    struct SSAPref sfs;
    register struct SSAPref *sf;
    register struct PSAPaddr *pa;
    struct RtSAPconnect rtcs;
    register struct RtSAPconnect   *rtc = &rtcs;
    struct AcSAPrelease acrs;
    register struct AcSAPrelease   *acr = &acrs;
    struct RtSAPindication  rtis;
    register struct RtSAPindication *rti = &rtis;
    register struct RtSAPabort *rta = &rti -> rti_abort;
    register AEI aei;
    register OID ctx, pci;
    struct PSAPctxlist pcs;
    register struct PSAPctxlist *pc = &pcs;
    int	wantabort;

    if ((aei = str2aei (argv[1], myservice)) == NULLAEI)
	fprintf(errfp, "%s-%s: unknown application-entity", argv[1], myservice);
    if ((pa = aei2addr (aei)) == NULLPA)
	fprintf(errfp, "address translation failed");
    if ((ctx = ode2oid (mycontext)) == NULLOID)
	fprintf(errfp, "%s: unknown object descriptor", mycontext);
    if ((ctx = oid_cpy (ctx)) == NULLOID)
	fprintf(errfp, "oid_cpy");
    if ((pci = ode2oid (mypci)) == NULLOID)
	fprintf(errfp, "%s: unknown object descriptor", mypci);
    if ((pci = oid_cpy (pci)) == NULLOID)
	fprintf(errfp, "oid_cpy");
    pc -> pc_nctx = 1;
    pc -> pc_ctx[0].pc_id = 1;
    pc -> pc_ctx[0].pc_asn = pci;
    pc -> pc_ctx[0].pc_atn = NULLOID;


    if ((sf = addr2ref (PLocalHostName ())) == NULL) {
	sf = &sfs;
	(void) bzero ((char *) sf, sizeof *sf);
    }

    if (argc > 2)
	wantabort = 1; /* Want to do an abort */
    else
	wantabort = 0;
    
/* read command line arguments here... */

    fprintf(errfp, "RT-OPEN.REQUEST:\n");
    if (RtOpenRequest (RTS_TWA, RTS_INITIATOR, ctx, NULLAEI, NULLAEI,
	NULLPA, pa, pc, NULLOID, 0, NULLQOS, rtc, rti)
	    == NOTOK)
	fprintf(errfp, "RT-OPEN.REQUEST: %s", RtErrString (rta -> rta_reason));

    if (rtc -> rtc_result != RTS_ACCEPT)
	fprintf(errfp, "association rejected: %s",
	RtErrString (rta->rta_reason));

    sd = rtc -> rtc_sd;
    RTCFREE (rtc);

    /*
     * General run through of all the primatives
     * Send using both types of transfer then switch over checking that
     * Please and Give both work. Then receive both types of transfer from
     * the receiving end and then switch over again test that transmitting
     * still works
     */

    oper(sd, SIMP_SEND);

    oper(sd, CPLX_SEND);

    oper(sd, RCV_PLS);

    oper(sd, SEND_GIVE);

    oper(sd, SIMP_RCV);

    oper(sd, CPLX_RCV);

    oper(sd, SEND_PLS);

    oper(sd, RCV_GIVE);

    oper(sd, CPLX_SEND);

    if (wantabort)
	oper(sd, SEND_ABRT);
    else
	oper(sd, SEND_CLOSE);

    exit (0);
}
