#include <stdio.h>
#include <ctype.h>
#include <isode/rtsap.h>

#include "support.h"

/*
 * General routines useful for supporting the tests of rtsap library routines
 */

extern int	fnx_s(), fnx_r();
extern FILE	*errfp;		/* where to send our error messages */

#define TIMEOUT		40	/* seconds to wait for a transfer */

#define PE_SIZE		3	/* size to build pe's for testing transfer */
/*
 * perform the given operation
 */
oper(sd, operation)
int	sd;
int	operation;	/* to be performed */
{
    struct RtSAPindication	rtis;
    struct AcSAPrelease		acr;
    PE	data;

    fprintf(errfp, "oper %d\n", operation);
    switch (operation) {
    case SIMP_SEND:
	data = mkpelist(PE_SIZE);
	if (RtTransferRequest(sd, data, TIMEOUT, &rtis) == NOTOK) {
	    fprintf(errfp, "SIMP_SEND:RT-TRANSFER.REQUEST: %s\n",
		RtErrString (rtis.rti_abort.rta_reason));
	    exit(1);
	}
	pe_free(data);
	break;

    case CPLX_SEND:
	if (RtSetDownTrans(sd, fnx_s, &rtis) == NOTOK) {
	    fprintf(errfp, "CPLX_SEND:RtSetDown: %s\n", /* Can never happen */
		RtErrString (rtis.rti_abort.rta_reason));
	    exit(3);
	}
	if (RtTransferRequest(sd, NULLPE, TIMEOUT, &rtis) == NOTOK) {
	    fprintf(errfp, "CPLX_SEND:failed: %s\n",
		RtErrString (rtis.rti_abort.rta_reason));
	    exit(1);
	}
	fprintf(errfp, "CPLX_SEND:done\n");
	break;

    case CPLX_RCV:
	if (RtSetUpTrans(sd, fnx_r, &rtis) == NOTOK) {
	    fprintf(errfp, "CPLX_RCV:RtSetDown: %s\n", /* Can never happen */
		RtErrString (rtis.rti_abort.rta_reason));
	    exit(3);
	}
	do {
	    if (RtWaitRequest(sd, TIMEOUT, &rtis) == NOTOK) {
		fprintf(errfp, "CPLX_RCV:failed: %s\n",
		    RtErrString (rtis.rti_abort.rta_reason));
		exit(1);
	    }
	    if (rtis.rti_type == RTI_CLOSE || rtis.rti_type == RTI_FINISH
	      || rtis.rti_type == RTI_ABORT) {
		fprintf(errfp, "Unexpected end\n");
		exit(3);
	    }
	
	} while (rtis.rti_type != RTI_TRANSFER);
	fprintf(errfp, "CPLX_RCV:done\n");
	break;

    case SIMP_RCV:
    case RCV_PLS:
    case RCV_GIVE:
    case RCV_ABRT:
    case RCV_FINISH:
    case RCV_CLOSE:
	fprintf(errfp, "RtWaitRequest\n");
	if (RtWaitRequest(sd, TIMEOUT, &rtis) == NOTOK) {
	    fprintf(errfp, "RtWaitRequest: %s\n",
		RtErrString (rtis.rti_abort.rta_reason));
	    exit(1);
	}
	switch (rtis.rti_type) {
	case RTI_TURN:
	    if (operation != RCV_GIVE && operation != RCV_PLS)
		break;
	    if (rtis.rti_turn.rtu_please && operation != RCV_PLS) {
                fprintf(errfp, "Unexpected RT-TURN-GIVE.INDICATION\n");
		exit(4);
	    }
	    if (!rtis.rti_turn.rtu_please && operation == RCV_PLS) {
                fprintf(errfp, "Unexpected RT-TURN-PLEASE.INDICATION\n");
		exit(5);
	    }
	    return;

	case RTI_TRANSFER:
	    if (operation != SIMP_RCV)
		break;
	    data = mkpelist(PE_SIZE);
	    if (pe_cmp(data, rtis.rti_transfer.rtt_data))
		fprintf(errfp, "oper:RTI_TRANSFER: data does not match\n");
	    RTTFREE(&rtis.rti_transfer);
	    pe_free(data);
	    return;

	case RTI_ABORT:
	    if (operation != RCV_ABRT || operation != RCV_CLOSE)
		break;
	    if (rtis.rti_abort.rta_peer)
		fprintf(errfp, "RT-U-ABORT.INDICATION: %s\n",
		    RtErrString (rtis.rti_abort.rta_reason));
	    else
		fprintf(errfp, "RT-P-ABORT.INDICATION: %s\n",
		    RtErrString (rtis.rti_abort.rta_reason));
	    return;

	case RTI_CLOSE:
		break;
		
	case RTI_FINISH:
	    if (operation != RCV_CLOSE)
		break;
	    if (RtCloseResponse(sd, ACR_NORMAL, NULLPE, &rtis) == NOTOK) {
		fprintf(errfp, "RtWaitRequest:RT-CLOSE.RESPONSE: %s\n",
		    RtErrString (rtis.rti_abort.rta_reason));
		exit(9);
	    }
	    return;
	}
	fprintf(errfp, "RtWaitRequest:unexpected indication %d received\n",
	    rtis.rti_type);
	break;

    case SEND_PLS:
	if (RtPTurnRequest(sd, 0, &rtis) == NOTOK) {
	    fprintf(errfp, "SEND_PLS:RT-PLEASE-TURN.REQUEST: %s\n",
		RtErrString (rtis.rti_abort.rta_reason));
	    exit(6);
	}
	break;

    case SEND_GIVE:
	if (RtGTurnRequest(sd, &rtis) == NOTOK) {
	    fprintf(errfp, "SEND_GIVE:RT-GIVE-TURN.REQUEST: %s\n",
		RtErrString (rtis.rti_abort.rta_reason));
	    exit(7);
	}
	break;

    case SEND_CLOSE:
	if (RtCloseRequest (sd, ACF_NORMAL, NULLPE, &acr, &rtis) == NOTOK)
	    fprintf (errfp, "RT-CLOSE.REQUEST: %s\n",
		RtErrString (rtis.rti_abort.rta_reason));
	return;

    case SEND_ABRT:
	data = mkpelist(PE_SIZE);
	if (RtUAbortRequest(sd, data, &rtis) == NOTOK)
	    fprintf (errfp, "RT-CLOSE.REQUEST: %s\n",
		RtErrString (rtis.rti_abort.rta_reason));
	return;

    default:
	    fprintf(errfp, "oper:unknown operation %d\n", operation);

    }
}


#define MKMASK 0x7
#define MKSHIFT 6
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
	fprintf(errfp, "mkpe:internal error %d case not handled\n",
	    (i*i >> MKSHIFT) & MKMASK);
	exit(2);
    }

    return (pe);
}

#define MAXCNT	3	/* How many increments of data to send */
#define DATASIZE	1035	/* Size of maximum increment */
/*
 * function which is called to incrementally send data
 */
fnx_s(sd, base, len, size, ack, ssn, rti)
int	sd;
char	**base;
int	*len, size;
long	ack, ssn;
struct RtSAPindication	*rti;
{
   static int	cnt = MAXCNT;

   if (base == NULLVP) {	/* RT-PLEASE.INDICATION */
       fprintf(errfp, "fnx_s: RT-PLEASE.INDICATION ignored\n");
       return (OK);
   }
   fprintf(errfp, "fnx_s: cnt = %d size = %d ack = %d ssn = %d\n",
       cnt, size, ack, ssn);
   if (cnt > 0) { /* Send some data */
       if (size < 0) {
	   fprintf(errfp, "fnx_s: bad value for size %d error\n", size);
	   return rtsaplose(rti, RTS_TRANSFER, NULLCP,
	       "fnx_s called with bad value for size");
	}
	if (size == 0) {	/* Have to do all at once transfer */
	    cnt = MAXCNT;
	}
	if (DATASIZE < size)
	    size = DATASIZE;
	if ((*base = malloc(size)) == NULL) {
	   fprintf(errfp, "fnx_s: malloc failed on size %d\n", size);
	   return rtsaplose(rti, RTS_TRANSFER, NULLCP, "malloc failed");
	}
	fill(*base, size);
	*len = size;
	cnt--;
    } else {
	cnt = MAXCNT;
	*len = 0;
    }
    return (OK);
}

static char *SSAPact_names[] = {
 "START.INDICATION", "RESUME.INDICATION", "INTERRUPT.INDICATION",
 "INTERRUPT.CONFIRMATION", "DISCARD.INDICATION", "DISCARD.CONFIRMATION",
 "END.INDICATION", "END.CONFIRMATION",
			};

/* Number of entries */
#define NENTRIES(x)	(sizeof (x)/sizeof ((x)[0]))
/*
 * function called by rtsap library to handle the incremental reception
 * of data.
 */
fnx_r(sd, event, addr, rti)
int	sd;
int	event;
caddr_t	addr;
struct	RtSAPindication	*rti;
{
    struct qbuf	*qb;
    struct SSAPactivity	*pa;
    int	len;
    char    *p;
    int		place;

    switch (event) {
    case SI_DATA:
	qb = (struct qbuf *)addr;
	len = qb->qb_len;
       fprintf(errfp, "fnx_r: %d octets of data arrived\n", len);
/* just assume it works - doesn't mention error conditions in the manual */
	p = qb2str(qb);
	if ((place = chk(p, len)) >= 0)
	   fprintf(errfp, "fnx_r: data failed check at octet %d\n", place);
	break;

    case SI_SYNC:
       fprintf(errfp, "fnx_r: S-MINOR-SYNC.INDICATION ignored\n");
       break;

    case SI_ACTIVITY:
       pa = (struct SSAPactivity *) addr;
       if (pa->sv_type >= NENTRIES(SSAPact_names) || pa->sv_type < 0)
	   fprintf(errfp, "fnx_r: S-ACTIVITY of unknown type!\n");
	else
	   fprintf(errfp, "fnx_r: S-ACTIVITY-%s\n", SSAPact_names[pa->sv_type]);
       break;

    case SI_REPORT:
       fprintf(errfp, "fnx_r: S-U-EXCEPTION-REPORT.INDICATION\n");
	break;

    default:
       fprintf(errfp, "fnx_r: unknown event received %d\n", event);
       return rtsaplose(rti, RTS_TRANSFER, NULLCP, "fnx_r:Unknown event");
    }

    return (OK);
}

/*
 * Fill a piece of memory with some data which has a pattern
 */
fill(data, len)
char	*data;
int	len;
{
    while (len-- > 0) {
	*data++ = len & 0xff;
    }
}

/*
 * check that data is filled as per fill routine. Return the place where it
 * fails otherwise -1 if it doesn't
 */
chk(data, len)
char	*data;
int	len;
{
    int	i;

    i = 0;
    while (len-- > 0) {
	if ((*data++ & 0xff) != (len & 0xff))
	    return (i);
	i++;
    }
    return (-1);
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

