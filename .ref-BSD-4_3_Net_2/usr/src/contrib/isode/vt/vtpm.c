/* vtpm.c - VTPM: FSM */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/vt/RCS/vtpm.c,v 7.1 91/02/22 09:48:30 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/vt/RCS/vtpm.c,v 7.1 91/02/22 09:48:30 mrose Interim $
 *
 *
 * $Log:	vtpm.c,v $
 * Revision 7.1  91/02/22  09:48:30  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:31:57  mrose
 * Release 6.0
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


#include "vtpm.h"
#include "eventmsg.h"
#include "sector1.h"

#undef PEPYPARM
#define PEPYPARM int *

int	cmode;
extern int sd;		/*Session descriptor for this association*/
extern int debug;

struct SSAPref sfs;
struct SSAPref *sf;
struct PSAPaddr *pa;
struct AcSAPconnect accs;
struct AcSAPconnect   *acc;
struct AcSAPrelease acrs;
struct AcSAPrelease   *acr;
struct AcSAPindication  acis;
struct AcSAPindication *aci;
struct AcSAPabort *aca;
AEI	aei;
OID	ctx,
	pci;

struct AcSAPstart   acss;
struct AcSAPstart *acs;
struct PSAPstart *ps;
struct PSAPindication pi;
struct PSAPdata	px;
struct PSAPfinish *pf;

/****************************************************************************/
/* GET EVENT - attempt to read a PDU from the presentation connection	    */
/*		   designated by "sd", determine			    */
/*		   which imcoming event has ocurred,			    */
/*		   and process the event with "do_event"		    */
/*									    */
/*		   A non-blocking read is done and OK is returned if there  */
/*		   is nothing to read.					    */
/*									    */
/*  PARAMETERS - 							    */
/*		FD - the presentation ID for the connection to read from    */
/*									    */
/*		PE - a pointer to the presentation element that is read     */
/*		(note that what is passed is a pointer to a pointer to      */
/*		data structure so that the address of the PE		    */
/*		that is read can be returned in this parameter)		    */
/*									    */
/*	RETURNS -  the number of the incoming event associated with reading */
/*		this PE from the network				    */
/****************************************************************************/

int
get_event(dd, pe)
	int	dd;
	PE	*pe;
{
	int	result, event;
	PE	nullpe;

	result = PReadRequest(dd, &px, OK, &pi);
	switch (result) {
	case NOTOK:
		if (debug)
			advise(LLOG_EXCEPTIONS,NULLCP,  "P-READ REQUEST returned NOTOK");
		return(NOTOK);
	case DONE:
		if (pi.pi_type == PI_FINISH) {
			pf = &pi.pi_finish;
			event = RLQ;
			nullpe = NULLPE;
			pe = &nullpe;
		}
		else if(pi.pi_type == PI_SYNC)
		{
			return( pn_ind(dd, &pi.pi_sync)) ;
		}
		else
			adios(NULLCP, "PReadRequest returned DONE, but event unknown (%d)",pi.pi_type);
		break;
	case OK:
		if (px.px_ninfo > 1)
			adios(NULLCP, "read more than one PE from network!\n");
		pe = &(px.px_info[0]);

		/* we are assuming here that you can only get one PDU per P-DATA.
		*/
		PLOG (vt_log, print_VT_PDUs, *pe, NULLCP, 1);
		if ((*pe)->pe_class != PE_CLASS_CONT)
			adios(NULLCP,"read pe of class %d", (*pe)->pe_class);
		switch((*pe)->pe_id) {
		case (ASQ_PDU):
		{
			if (debug)
				advise(LLOG_DEBUG,NULLCP,  "got ASQ_PDU");

			event = ASQ;
		}
		case ASR_PDU:
		{
			if (debug)
				advise(LLOG_DEBUG,NULLCP,  "got ASR_PDU");

			event = ASR;
		}
		case AUQ_PDU:
			if (debug)
				advise(LLOG_DEBUG,NULLCP,  "got AUQ_PDU");
			event = AUQ;
			break;
		case DAQ_PDU:
			if (debug)
				advise(LLOG_DEBUG,NULLCP,  "got DAQ_PDU");
			event = DAQ;
			break;
		case DLQ_PDU:
			if (debug)
				advise(LLOG_DEBUG,NULLCP,  "got DLQ_PDU");

			event = DLQ;
			break;

		case NDQ_PDU:
		{
			if (debug)
				advise(LLOG_DEBUG,NULLCP,  "got NDQ_PDU");

			event = NDQ_tr;	/*See comment below*/

	/*	We're supposed to find out if the NDQ contains an
		update to a triggered control object or not to determine
		what kind of event we have.  Right now we'll assume that
		we do have such an update in all cases.  Note that this may
		be a problem if we use quarantine delivery control in the
		future.
	  
		for each update, find out if the update is for a display object
		or for a control object. if it's a control object get the name 
		of it and find out if it has a trigger
	
	*/
			break;
		}

		case UDQ_PDU:
		{
			event = UDQ;
			break;
		}

		case HDQ_PDU:
		{
			if(debug) advise(LLOG_NOTICE,NULLCP,"Got HDQ");
			event = HDQ;
			break;
		}

		case RLR_PDU:
			event = RLR;
			break;

		default:
			adios(NULLCP,"unknown PDU type %d", (*pe)->pe_id);
		}
	}
	return(do_event(event,*pe));
}



#define SECTORS	6

/* number of states in each sector */

unsigned	max_state[SECTORS] = { 0, 13, 0, 0, 0, 10};

int (*s0[])() =	{
	NULL
};

int (*s1[])() =	{
	s1_01,			/* states in the first sector  */
  	s1_02B,
	s1_02S,
	s1_03B,
	s1_03S,
	s1_10B,
	s1_10N,
	s1_10T,
	s1_50B,
	s1_51Q,
	s1_51R,
	s1_51N,
	s1_51T
};

int (*s2[])() =	{
	NULL
};

int (*s3[])() =	{
	NULL
};

int (*s4[])() =	{
	NULL
};

int (*s5[])() =	{
	s5_400B,
	s5_402B,
	s5_420B,
	s5_422B,
	s5_40N,
	s5_40T,
	s5_42N,
	s5_42T,
	s5_61,
	s5_62
};

int	((**sectors[])()) = {s0, s1, s2, s3, s4, s5};


unsigned	state = 0,
			sector = 1;

do_event(event, pe)
	int	event;
	PE	pe;
{
	if (debug)
		advise(LLOG_DEBUG,NULLCP, 
	       "in do_event, sector is %d, state is %d, event is %d (%s)",
	       sector, state, event,
		       event >= 0
		               && event < sizeof eventname/sizeof eventname[0]
		           ? eventname[event] : "INVALID");
	if (sector >= SECTORS || state >= max_state[sector])
		return(NOTOK);
	return(sectors[sector][state](event, pe));
}

/* ARGSUSED */
pn_ind(dd, psync) /* sync indications */
	int 	dd;
	struct 	PSAPsync *psync;
{
	switch(psync->pn_type)
	{
		case SN_MAJORIND:
			advise(LLOG_DEBUG,NULLCP,  "vt: got SN_MAJORIND");
			break;
		case SN_MAJORCNF:
			advise(LLOG_DEBUG,NULLCP,  "vt: got SN_MAJORCNF");
			break;
		case SN_MINORIND:
			advise(LLOG_DEBUG,NULLCP,  "vt: got SN_MINORIND");
			break;
		case SN_MINORCNF:
			advise(LLOG_DEBUG,NULLCP,  "vt: got SN_MINORCNF");
			break;
		case SN_RESETIND:
/*			advise(LLOG_DEBUG,NULLCP,  "vt: resetind: SN_RESETIND"); */
			if(psync->pn_options != SYNC_RESTART)
			  adios(NULLCP,"resetind: bad options params");
			if(psync->pn_ninfo > 0)
			   return( do_event(BKQ,psync->pn_info[0]));
			  else return( do_event(BKQ,NULLPE));
		case SN_RESETCNF:
/*			advise(LLOG_DEBUG,NULLCP,  "vt: got SN_RESETCNF\n"); */
			if(psync->pn_options != SYNC_RESTART)
			  adios(NULLCP,"resetind: bad options params");
			if(psync->pn_ninfo > 0)
			   return( do_event(BKR,psync->pn_info[0]));
			  else return( do_event(BKR,NULLPE));
		default:
			adios(NULLCP,"received bad sync type");
	 }
	PNFREE(psync);
	return(NOTOK);
}




/*****************************************************************************/
/* P_DATA - send a PDU via PDataRequest					     */
/*									     */
/* RETURNS - OK or exits on error					     */
/*									     */
/*  PARAMETERS - 							     */
/*		PDU - a PE containing the PDU to be sent		     */
/*									     */
/*  CLASSIFICATION - utility function for VTPM (used only in processing	     */
/*			outgoing events that are mapped to P_DATA)	     */
/*									     */	
/*****************************************************************************/

p_data(pdu)
	PE	pdu;
{

	PLOG (vt_log, print_VT_PDUs, pdu, NULLCP, 0);

	if (PDataRequest(sd, &pdu, 1, &pi) != OK)
	    ps_adios (&pi.pi_abort, "P-DATA.REQUEST");
	pe_free(pdu);
	return(OK);
}


/****************************************************************************/
/* P_MAJOR_SYNC.REQUEST - send a PDU via PMajSyncRequest		    */
/*									    */
/*   RETURNS - OK or exits on error					    */
/*									    */
/*   PARAMETERS - 							    */
/*			PDU - a PE containing the PDU to be sent	    */
/*									    */
/*  CLASSIFICATION - utility function for VTPM (used only in processing	    */
/*		outgoing events that are mapped to P_MAJOR_SYNC.REQUEST)    */
/*									    */
/****************************************************************************/

p_maj_sync_req(pdu)
	PE	pdu;
{
	long ssn;

	PLOG (vt_log, print_VT_PDUs, pdu, NULLCP, 0);

	if (PMajSyncRequest(sd, &ssn, &pdu, 1, &pi) != OK)
	    ps_adios (&pi.pi_abort, "P-MAJOR-SYNC.REQUEST");
	return(OK);
}


/****************************************************************************/
/* P_MAJOR_SYNC.RESPONSE - send a PDU via PMajSyncResponse		    */
/*									    */
/*   RETURNS - OK or exits on error					    */
/*									    */
/*  PARAMETERS - 							    */
/*			PDU - a PE containing the PDU to be sent	    */
/*									    */
/*  CLASSIFICATION - utility function for VTPM (used only in processing     */
/*		   outgoing events that are mapped to P_MAJOR_SYNC.RESPONSE)*/
/*									    */
/****************************************************************************/

p_maj_sync_resp(pdu)
	PE	pdu;
{
	PLOG (vt_log, print_VT_PDUs, pdu, NULLCP, 0);

	if (PMajSyncResponse(sd, &pdu, 1, &pi) != OK)
	    ps_adios (&pi.pi_abort, "P-MAJOR-SYNC.RESPONSE");
	return(OK);
}


/***************************************************************************/
/* P_TYPED_DATA  - send a PDU via PTypedRequest				   */
/*									   */
/*  RETURNS - OK or exits on error					   */
/*									   */
/*  PARAMETERS - 							   */
/*			PDU - a PE containing the PDU to be sent	   */
/*									   */
/*  CLASSIFICATION - utility function for VTPM (used only in processing    */
/*			outgoing events that are mapped to P_TYPED_DATA)   */
/*									   */
/***************************************************************************/

p_typed_data(pdu)
	PE	pdu;
{

	PLOG (vt_log, print_VT_PDUs, pdu, NULLCP, 0);

	if (PTypedRequest(sd, &pdu, 1, &pi) != OK)
	    ps_adios (&pi.pi_abort, "P-TYPED-DATA.REQUEST");
	return(OK);
}

/*****************************************************************************/
/* P_RESYNCHRONIZE.REQUEST - send a PDU via PReSyncRequest		     */
/*									     */
/*  RETURNS - OK or exits on error					     */
/*									     */
/*  PARAMETERS -							     */
/*	 PDU - a PE containing the (break) PDU to be sent		     */
/*									     */
/*  CLASSIFICATION - utility function for VTPM (used only in processing	     */
/*			outgoing events that are mapped to P_RESYNC.REQUEST) */
/*****************************************************************************/

p_resync_req(pdu,type)
	PE	pdu;
	int type;
{

long ssn = 0; /* should be made a global at some time */
int settings = ST_INIT_VALUE;

#define VTKP_REQ   0x00 /* setting values, see ssap.h */
#define VTKP_ACC   0x15
#define VTKP_CHO   0x2a

	PLOG (vt_log, print_VT_PDUs, pdu, NULLCP, 0);

	if (PReSyncRequest(sd, type, ssn, settings, &pdu, 1, &pi) != OK)
/*	if (PReSyncRequest(sd, type, 0, 0, (PE *)NULL, 0, &pi) != OK) */
	    ps_adios (&pi.pi_abort, "P-RESYNCHRONIZE.REQUEST");
	return(OK);
}


/****************************************************************************/
/* P_RESYNC.RESPONSE - send a PDU via PReSyncResponse			    */
/*									    */
/*   RETURNS - OK or exits on error					    */
/*									    */
/*   PARAMETERS - 							    */
/*		PDU - a PE containing the PDU to be sent		    */
/*									    */
/*   CLASSIFICATION - utility function for VTPM (used only in processing    */
/*	  	      outgoing events that are mapped to P_RESYNC.RESPONSE) */
/*									    */
/****************************************************************************/

p_resync_resp(pdu)
	PE	pdu;
{

long ssn = 0; /* should be made a global at some time */
int settings = ST_INIT_VALUE;
	PLOG (vt_log, print_VT_PDUs, pdu, NULLCP, 0);

	if (PReSyncResponse(sd, ssn, settings, &pdu, 1, &pi) != OK)
	    ps_adios (&pi.pi_abort, "P-RESYNCHRONIZE.RESPONSE");
	return(OK);
}



/****************************************************************************/
/*	ASR - generate an ASR event. (that is send an ASR PDU which is	    */
/*	      passed in as a parameter as user data on the AcAssocResponse.)*/
/*									    */
/*	PARAMETERS: 	PE - a vt ASR PDU				    */
/*			status (SUCCESS or FAILURE)		            */	
/*									    */
/*	RETURNS:		OK					    */
/****************************************************************************/

asr(pe,status)
	PE	pe;
	int	status;
{

/*	include "pe" as user data on the AcAssocResponse
*/
	struct PSAPctxlist *pl = &ps->ps_ctxlist;
	int s_requirements;
	long isn;
	int reason, i;

	if (debug > 2) {
		for(i=0; i<pl->pc_nctx; i++)
			advise(LLOG_DEBUG,NULLCP," ctx %d: %d %s %d",
				i,pl->pc_ctx[i].pc_id,sprintoid(pl->pc_ctx[i].pc_asn),
				pl->pc_ctx[i].pc_result);

	}
	if (debug) {
		advise(LLOG_DEBUG,NULLCP,  "in asr.\n");
		advise(LLOG_DEBUG,NULLCP,  "about to call AcAssocResp, sd is %d, pe->pe_id is %d\n", sd, pe->pe_id);
	}

	if(status == SUCCESS)
	{
		status = ACS_ACCEPT;
		reason = ACS_USER_NULL;
	}
	else
	{
		status = ACS_PERMANENT;
		reason = ACS_USER_NOREASON;
	}
	s_requirements = SR_DUPLEX | SR_RESYNC | SR_TYPEDATA;
	isn = 0;
	pe -> pe_context = 1;
	if (AcAssocResponse (sd, status, reason, NULLOID, NULLAEI,
		NULLPA,
		&ps->ps_ctxlist,
		ps->ps_defctxresult, ps->ps_prequirements, s_requirements,isn,
		ps->ps_settings, &ps->ps_connect, &pe, 1, aci) == NOTOK)
	    acs_adios (aca, "A-ASSOCIATE.RESPONSE");

	if (debug)
		advise(LLOG_DEBUG,NULLCP,  "sent AcAssociate Response\n");
	return(OK);
}



send_bad_asr(reason)	/*Compose and send ASR with result = failure.  Encode
			  ASR-FailureReason using the reason parameter
			  (0 means no reason).
			*/

int reason;
{

	PE asr_pe;
	ASR_MSG ud;

	bzero ((char *) &ud, sizeof ud);
	if(reason)
	{
		ud.valid_reason = 1;
		ud.reason.type = 1;
		ud.reason.provider_reason = reason;
	}
	else ud.valid_reason = 0;
	ud.result = 0;			/*Failure code*/
	ud.valid_imp = 0;
	ud.valid_coll = 0;
	ud.valid_arg_list = 0;
	ud.version.bitstring = 0x00;
	ud.version.bitcount = 5;
	if(build_ASRPDU_ASRpdu(&asr_pe,1,NULL,NULLCP,(PEPYPARM)&ud) == NOTOK)
	    adios (NULLCP, "ASR build failure (%s)", PY_pepy);
	
	return(asr(asr_pe,FAILURE)); /*Send the PDU thru Association control*/
}


send_rlr(pe)	/*Send RLR (Release Response) PDU to peer.  The RLR is
		  built by vrelrsp().  It is sent by a call to Association
		  Control.
		*/
PE pe;
{
	pe -> pe_context = 1;
	if(AcRelResponse(sd,ACS_ACCEPT,ACR_NORMAL,&pe,1,aci) == NOTOK)
	    acs_adios (&aci->aci_abort, "A-RELEASE.RESPONSE");
}


clear_vte()	/*Clear VT Environment.  */
{

	/*Nothing to do for now since we have no formalized environment
	  and we exit VTP when association ends.
	*/
}


vgvt_ind()	/*Indication to User that peer has given the token*/
{

	/*Don't know how to indicate this to user yet*/
}


vrtq_ind()	/*Indicate to User that peer has requested token*/
{

	/*Don't know how to give indication to user.
	  Synchronous?  Asynch interrupt??? */
}


give_token()	/*Transfer Token to peer.  For VTP, all tokens are given
		  at once so no need to discriminate between them.
		*/
{

	int vt_tokens;
	struct PSAPindication vt_pi;

	vt_tokens = ST_RLS_TOKEN;

	if(PGTokenRequest(sd,vt_tokens,&vt_pi) == NOTOK
	       && vt_pi.pi_abort.pa_reason != PC_OPERATION)
	    ps_adios (&vt_pi.pi_abort, "P-GIVE-TOKENS.REQUEST");
}


request_token()		/*Request Tokens from peer*/
{

	int vt_tokens;
	struct PSAPindication vt_pi;

	vt_tokens = ST_RLS_TOKEN;

	if(PPTokenRequest(sd,vt_tokens,NULLPEP,0,&vt_pi) == NOTOK
	       && vt_pi.pi_abort.pa_reason != PC_OPERATION)
	    ps_adios (&vt_pi.pi_abort, "P-PLEASE-TOKENS.REQUEST");
}

send_all()	/*TEMP -- Should be supplied by Sector 5 actions*/
{
	advise(LLOG_DEBUG,NULLCP,  "send_all dummy routine");
}

/*  */

void  acs_adios (aa, event)
register struct AcSAPabort *aa;
char   *event;
{
    acs_advise (aa, event);

    finalbye ();

    _exit (1);
}


static void  acs_advise (aa, event)
register struct AcSAPabort *aa;
char   *event;
{
    char	buffer[BUFSIZ];

    if (aa -> aca_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s",
			AcErrString (aa -> aca_reason),
			aa -> aca_cc, aa -> aca_cc, aa -> aca_data);
    else
	(void) sprintf (buffer, "[%s]", AcErrString (aa -> aca_reason));

    advise (LLOG_NOTICE,NULLCP,  "%s: %s (source %d)", event, buffer,
	    aa -> aca_source);
}


static void  ps_adios (pab, event)
register struct PSAPabort *pab;
char   *event;
{
    ps_advise (pab, event);

    finalbye ();

    _exit (1);
}


static void  ps_advise (pab, event)
register struct PSAPabort *pab;
char   *event;
{
    char    buffer[BUFSIZ];

    if (pab -> pa_cc > 0)
	(void) sprintf (buffer, "[%s] %*.*s",
		PErrString (pab -> pa_reason),
		pab -> pa_cc, pab -> pa_cc, pab -> pa_data);
    else
	(void) sprintf (buffer, "[%s]", PErrString (pab -> pa_reason));

    advise (LLOG_NOTICE,NULLCP,  "%s: %s%s", event, buffer,
	    pab -> pa_peer ? " (peer initiated)" : "");
}
