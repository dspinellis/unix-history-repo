#include <stdio.h>
#include <ctype.h>
#include <isode/rosap.h>
#include <isode/isoservent.h>
#include "generic.h"		/* defines OPERATIONS and ERRORS */

				/* e.g., "directory" */
static char *myservice = "ROSSTEST";

				/* e.g., "directory services" */
static char *mycontext = "isode chic read";
static char *mypci = "isode chic read pci";


#define INVOKE	1	/* do a RoInvokeRequest */
#define INTREQ	2	/* do a RoIntrRequest */
#define INVERR	3	/* request an error */
#define INVURJ	4	/* request a user reject */
#define INVPRJ	5	/* request a provider reject */

static requirements = SR_NEGOTIATED|SR_HALFDUPLEX;
extern struct isoservent	*getisoserventbyname();
extern struct SSAPaddr		*is2saddr();

extern PE mkpelist();

main (argc, argv, envp)
int	argc;
char  **argv,
      **envp;
{
    int	    sd;
    struct isoservent	*is;
    struct RoSAPindication  rois;
    register struct RoSAPpreject   *rop = &rois.roi_preject;
    struct SSAPaddr	*psa;

    struct RoSAPaddr	roas;
    struct RoSAPconnect	rocs;

    if ((is = getisoserventbyname(myservice, "rosap")) == NULL) {
	fprintf(stderr, "can't find %s/rosap", myservice);
	exit(1);
    }
    roas.roa_port = is->is_port;
    if (argc < 2) {
	fprintf(stderr, "Need an arguement of a hostname\n");
	exit(1);
    }
    /* This is an undescribed strangeness. why look something up if you
     * never use it ??? Do the people who write ISODE documentation never
     * expect anyone to try and read it ?
     */
    if ((is = getisoserventbyname("ros", "ssap")) == NULL) {
	fprintf(stderr, "can't find ssap/ros");
	exit(1);
    }
    if ((psa = is2saddr(argv[1], NULLCP, (struct isoservent *) is)) == NULLSA) {
	fprintf(stderr, "Can't compute address to %s\n", argv[1]);
	exit(2);
    }

    roas.roa_addr = *psa; /* struct copy */

    if (RoBeginRequest(&roas, NULLPE, &rocs, &rois) == NOTOK) {
	fprintf(stderr, "RoBeginRequest:failed:%s\n",
	    RoErrString (rop -> rop_reason));
	exit(1);
    }
    if (rocs.roc_result != ROS_ACCEPT) {
	fprintf(stderr, "Association has been rejected %s\n",
	RoErrString(rocs.roc_result));
	exit(2);
    }
    sd = rocs.roc_sd;

    printf("RoBeginRequest succeeded\n");

    ROCFREE(&rocs);

    if (RoSetService (sd, RoSService, &rois) == NOTOK)
        fprintf(stderr, "RoSetService: %s", RoErrString (rop -> rop_reason));

    printf("RoSetService succeeded\n");

    invoke (sd, INVOKE);	/* invoke the operations, etc. */
    
    invoke (sd, INTREQ);	/* invoke the operations, etc. */

    invoke (sd, INVERR);	/* invoke the operations, etc. */

    invoke (sd, INVURJ);	/* invoke the operations, etc. */

    invoke (sd, INVPRJ);	/* invoke the operations, etc. */

    if (RoEndRequest(sd, 0, &rois) == NOTOK) {
        fprintf(stderr, "RoEndRequest: %s", RoErrString (rop -> rop_reason));
	exit(3);
    }

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
    PE	data;

    invoke = 1;

    printf("invoke %d\n",type);
    data = mkpelist(1);
    /* ROS over Session needs a non null args */
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
	    fprintf(stderr, "RO-INVOKE.REQUEST: %s\n",
		RoErrString(rind.roi_preject.rop_reason));
	else
	    fprintf(stderr, "RO-INVOKE.REQUEST:failed: unexpected returned type %d\n",
		rind.roi_type);
	exit(1);
    
    case OK:
	break;

    default:
	fprintf(stderr, "RO-INVOKE.REQUEST:failed(%d): unexpected returned type %d\n",
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

