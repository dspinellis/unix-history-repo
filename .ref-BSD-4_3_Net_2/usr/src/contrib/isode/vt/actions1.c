/* actions1.c - VTPM: FSM sector 1 actions */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/vt/RCS/actions1.c,v 7.2 91/02/22 09:47:52 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/vt/RCS/actions1.c,v 7.2 91/02/22 09:47:52 mrose Interim $
 *
 *
 * $Log:	actions1.c,v $
 * Revision 7.2  91/02/22  09:47:52  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/10/23  20:44:45  mrose
 * update
 * 
 * Revision 7.0  89/11/23  22:31:26  mrose
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
#include "sector1.h"

/************************************************************************/
/*	This file contains the functions that are executed when the	*/
/*	VT Protocol machine is in a Sector 1 state and a protocol	*/
/*	event occurs.  The state transition matrix is specified in	*/
/*	Table 32 of ISO 9041 (July 1987 version).  The actions which	*/
/*	this collection of functions perform are specified in Table 33	*/
/*	of ISO 9041.							*/
/************************************************************************/

extern int sd;		/*Global Session Descriptor (ISODE) */

#define	invalid_result(str,pe) \
    	adios (NULLCP, "%s: invalid result (%s)", (str), \
	       pe_error ((p) -> pe_errno))

int
a1_0(pe)	/*NDQ-ntr in states 50B or 51Q (Release awaiting peer)*/
		/*Also RTQ in state 51T (Release awaiting peer)*/
PE pe;
{
	/*If quarantined data delivery were supported, we would do:
	/*	++vnt;
	/*	enque(pe);
	/*For now, only supporting simple or no delivery control, so give pe
	/*to application here.
	*/

	vdatind(SEQUENCED,pe);		/*Deliver the NDQ to application*/
	return(OK);
}


int	
a1_1(pe)		/*NDQ-tr in states 50B or 51Q (Release Awaiting Peer)*/
PE pe;
{
	/* Same comment as in a1_0 above */
	vdatind(SEQUENCED,pe);		/*Deliver the NDQ to application*/
	vnt = 0;
	return(OK);
}


int
a1_2(pe)		/*VASSreq from user in state 01 (No Association)*/
PE pe;
{
	dr_pm_st = DEFINED;	/*Draft-VTE param. status = Defined
				/*(SetVTPmS(P) in 9041) */
	/*Set draft-VTE parameter values per profile (SetVTPmV(P) in 9041)*/

	asq(pe);		/*Send the ASQ PDU*/

	vsmd = 0;		/*Very TEMPORARY -- A-Mode only
				  Should be done in VT-user or application call
				*/
	if(vsmd) state = S1_02S;
	else state = S1_02B;
	return(OK);
}


int
a1_3(pe)	/*VASSrsp from user in state 03B (Associate -- Awaiting user)*/
PE pe;
{
	PE p;
	vrsl = -1;
	for(p = first_member(pe); p; p = next_member(pe,p))
		/*Step through elements in sequence*/
	{
		if(PE_ID(p->pe_class,p->pe_id) == PE_ID(PE_CLASS_CONT,2) )
			/*If the result element*/
		{
			if( (vrsl = prim2num(p)) == NOTOK)
			    invalid_result ("a1_3", p);
			break;
		}
	}
	if(vrsl < 0)	/*If no result field*/
		adios(NULLCP, "a1_3: no result field");
	if(vrsl == FAILURE) 
	{
		clear_vte();
		(void) asr(pe,FAILURE);
	}
	else
	{
		/*Set draft-VTE parameters according to list in primitive
		/* (SetVTPmV(L) in 9041)
		/*Set status of draft-VTE parameters above to defined
		/* (SetVTPmSDe(L)
		/*Set current VTE from draft VTE (SetCuVTE) */

		vena = TRUE;	/*Current VTE agreed to*/
		waca = TRUE;
		(void) asr(pe,SUCCESS);
		if(vrsl == SUCCESS)
		{
			sector = 5;
			state = S5_400B;
		}
		else state = S1_10B;
	}
	return(OK);
}


int
a1_4(pe)	/*VASSrsp from user in state 03S (Associate -- Awaiting user)*/
PE pe;
{
	PE p;
	vrsl = -1;
	for(p = first_member(pe); p; p = next_member(pe,p))
		/*Step through elements in sequence*/
	{
		if(PE_ID(p->pe_class,p->pe_id) == PE_ID(PE_CLASS_CONT,2) )
			/*If the result element*/
		{
			if( (vrsl = prim2num(p)) == NOTOK)
			    invalid_result ("a1_4", p);
			break;
		}
	}
	if(vrsl < 0)	/*If no result field*/
		adios(NULLCP,"a1_4: no result field");
	if(vrsl == FAILURE) 
	{
		clear_vte();	/*Discard VTE (DisVTE)*/
		(void) asr(pe,FAILURE);	/*Send the ASR to peer*/
	}
	else
	{
		/*Set draft-VTE parameters according to list in primitive
		/* (SetVTPmV(L) in 9041)
		/*Set status of draft-VTE parameters above to defined
		/* (SetVTPmSDe(L)
		/*Set current VTE from draft VTE (SetCuVTE) */

		vena = TRUE;	/*Current VTE agreed to*/
		(void) asr(pe,SUCCESS);
		if(vrsl == SUCCESS)
		{
			sector = 5;
			if(vtok)state = S5_40T;
			else state = S5_40N;
		}
		else 
		{
			if(vtok) state = S1_10T;
			else state = S1_10N;
		}
	}
	return(OK);
}


/* ARGSUSED */
int
a1_5(pe)		/*RTQ (Token Request) in state 10B
			  (Environment not agreed)*/
PE pe;
{
	give_token();	/*Need a call to lower layer in ISODE to do this*/
	return(OK);
}


/* ARGSUSED */
int
a1_6(pe)	/*VGVTreq from user in 10T*/
PE pe;
{
	give_token();	/*Need a call to lower layer in ISODE to do this*/
	state = S1_10N;
	return(OK);
}


/* ARGSUSED */
int
a1_7(pe)	/*VRELreq from user in state 10B (Env. not agreed)*/
		/*GTQ in 50B*/
PE pe;
{
	if(vtok)
	{
		vt_disconnect();	/*May be only TEMP*/
		state = S1_51Q;
	}
	else
	{
		request_token();
			/*Need call to ISODE to request token*/
		state = S1_50B;

/*Probably need to release the NULL PE for VRELreq that got us here*/

	}
	return(OK);
}


/* ARGSUSED */
int
a1_8(pe)	/*VRELreq in 10T*/
PE pe;
{
	vt_disconnect();	/*May be only TEMP--check function*/
	state = S1_51T;

/*Release NULL PE from VT USER*/

	return(OK);
}

int
a1_9(pe)	/*VRELrsp in 51R & 51N (Release -- Awaiting User)*/
PE pe;
{


/*	vrsl = -1;
/*	for(p = first_member(pe); p; p = next_member(pe,p) )
/*			/*Get Result parameter*/
/*	{
/*		if(PE_ID(p->pe_class,p->pe_id) == PE_ID(PE_CLASS_CONT,0) )
/*		{
/*			if( (vrsl = prim2num(p)) == NOTOK)
/*			    invalid_result ("a1_9", p);
/*			break;
/*		}
/*	}
/*	if(vrsl < 0)
/*		adios(NULLCP,"a1_9: no result field");
/*	
/*	we should look in the pdu and see what the result is, but
/*	since we know our vt-user is an agreeable fellow, we can
/*	assume success
*/

	vrsl = SUCCESS;
	if(vrsl == SUCCESS)
	{
		if(vns > 0)	/*If data left to send*/
		{
			advise(LLOG_NOTICE,NULLCP, "Sending remaining data (a1_9() )");
			send_all(); /*Send remaining data (NDQseq(Vns)-ntr)*/
			vns = 0;
		}
		send_rlr(pe);	/*Send the RLR which User built*/
		clear_vte();	/*Erase the Environment*/
		state = S1_01;
/*		system("reset");	*/
		finalbye ();
		advise(LLOG_NOTICE,NULLCP,"association released by terminal service");
		(void)fflush (stdout);
		exit(0);
	}
	else	/*Result was failure*/
	{
		send_rlr(pe);	/*Send the RLR*/
		if(vena)	/*If agreement on VTE*/
		{
			sector = 5;
			if(vsmd) state = S5_40N;	/*If S-Mode*/
			else state = S5_400B;
		}
		else
		{
			if(vsmd) state = S1_10N;
			else state = S1_10B;
		}
	}
	return(OK);
}


/* ARGSUSED */
int
a1_10(pe)	/*VRQTreq(request token) n state 10N*/
PE pe;
{
	request_token();	/*TEMP -- Need an ISODE call to really do this
				  since there is no VTP PDU*/

/*Probably need to free the NullPE that triggered this*/

	state = S1_10N;		/*Should be here already.  Do this to follow
				  the spec literally*/
	return(OK);
}


/* ARGSUSED */
int
a1_11(pe)	/*VSNEGreq (User Start Negotiation)*/
PE pe;
{

/*MIN not implemented*/
/*Probably need to send back a negative Acknowledgement*/

	return(OK);
}


/* ARGSUSED */
int
a1_12(pe)	/*VSNEGreq*/
PE pe;
{

/*MIN not implemented*/
/*Probably need to send back a Negative Acknowledgement*/

	return(OK);
}


/* ARGSUSED */
int
a1_13(pe)	/*VSWPreq (User Switch profile request)*/
PE pe;
{

	/*Switch Profile not implemented*/
	/*Should probably send back a negative acknowledgement*/

	return(OK);
}


/* ARGSUSED */
int
a1_14(pe)	/*VSWPreq*/
PE pe;
{

	/*Switch Profile not implented*/
	/*Should Probably send back a negative acknowledgement*/

	return(OK);
}


int
a1_15(pe)		/*ASR in state 2B (Assoc. awaiting target) */
PE pe;
{
	PE p;

	vrsl = -1;
	for(p = first_member(pe); p; p = next_member(pe,p) )
	{
		if(PE_ID(p->pe_class,p->pe_id) == PE_ID(PE_CLASS_CONT,2) )
		{
			if( (vrsl = prim2num(p)) == NOTOK)
			    invalid_result ("a1_15", p);
			break;
		}
	}
	if(vrsl < 0)
		adios(NULLCP,"a1_15: no result field");
	if(vrsl == FAILURE)
	{
		clear_vte();
		state = S1_01;
		return(FAILURE);	/*Notify user of ASR (VASScnf)*/
	}
	else
	{

	/*Set draft-VTE param. according to list in primitive or protocol
	/*element (SetVTPmV(L)).
	/*Set status of draft-VTE params. listed in primitive or protocol
	/*element to defined (SetVTPmSDe(L))
	/*Set current-VTE from draft-VTE (SetCuVTE) */

		vena = 1;
		waci = 1;
		if(vrsl == SUCCESS)
		{
			sector = 5;
			state = S5_400B;
		}
		else state = S1_10B;
	}
	return(SUCCESS);	/*Notify user of ASR (VASScnf)*/
}

int
a1_16(pe)		/*ASR in state 2S (Assoc. awaiting target) */
PE pe;
{
	PE p;

	vrsl = -1;
	for(p = first_member(pe); p; p = next_member(pe,p) )
	{
		if(PE_ID(p->pe_class,p->pe_id) == PE_ID(PE_CLASS_CONT,2) )
		{
			if( (vrsl = prim2num(p)) == NOTOK)
			    invalid_result ("a1_16", p);
			break;
		}
	}
	if(vrsl < 0)
		adios(NULLCP,"a1_16: no result field");
	if(vrsl == FAILURE)
	{
		clear_vte();
		state = S1_01;
		return(FAILURE);	/*Notify user of ASR (VASScnf)*/
	}
	else
	{

	/*Set draft-VTE param. according to list in primitive or protocol
	/*element (SetVTPmV(L)).
	/*Set status of draft-VTE params. listed in primitive or protocol
	/*element to defined (SetVTPmSDe(L))
	/*Set current-VTE from draft-VTE (SetCuVTE) */

		vena = 1;
		if(vrsl == SUCCESS)
		{
			sector = 5;
			if(vtok) state = S5_40T;
			else state = S5_40N;
		}
		else 
		{
			if(vtok) state = S1_10T;
			else state = S1_10N;
		}
	}
	return(SUCCESS);	/*Notify user of ASR (VASScnf) */
}


int
a1_17(pe)		/*ASQ in state 01 (No Association)*/
PE pe;
{

	int result;

	result = read_asq(pe);	/*Unpack ASQ*/
	if(result == PROFILE_NG)
	{
		(void)send_bad_asr(PROFILE_NG); /*Send Failure ASR with reason*/
		return(NOTOK);
	}
	if(result == 0)
	{
		(void)send_bad_asr(0);	/*Send failure ASR w/ no reason*/
		return(NOTOK);
	}

	/*SetVTPmS(P)*/
	/*SetVTPmV(P)*/
	dr_pm_st = DEFINED;
	vsmd = 0;
	vtok = 1;			/*For Telnet & transparent profiles*/
	if(vsmd) state = S1_03S;	/*If S-Mode*/
	else state = S1_03B;
	result = vassind(pe);	/*doesn't really use pe but for consistency
				  with version 1*/
	return(result);
}


int
a1_18(pe)	/*UDQ (uncontrolled data) in 51T (Release Awaiting Peer)*/
PE pe;
{
	vdatind(SEQUENCED,pe);	/*Want to do VDATind-h but this is all that's
				  now available*/
	return(OK);
}

	
/* ARGSUSED */
int
a1_19(pe)	/*GTQ in 10N or VRTQreq in 10T*/ 
PE pe;
{
	vtok = TRUE;
	vgvt_ind();	/*VGVTind -- Tell user we have token (as if he cares)*/
	state = S1_10T;;
	return(OK);
}

	
int
a1_20(pe)	/*RLR (Release Response) in 51Q or 51T (Release Awaiting Peer)*/
PE pe;
{

	PE p;

	vrsl = -1;
	for(p = first_member(pe); p; p = next_member(pe,p) )
	{
		if(PE_ID(p->pe_class,p->pe_id) == PE_ID(PE_CLASS_CONT,0) )
			/*If result element*/
		{
			if( (vrsl = prim2num(p)) == NOTOK)
			    invalid_result ("a1_20", p);
			break;
		}
	}
	if(vrsl < 0)	/*if no result field*/
		adios(NULLCP,"a1_20: no result field");
	if(vrsl == SUCCESS)
	{
		/*VRELcnf -- Confirm the release to user -- for now, use the
		  original mechanism (closing TELNET) -- should be changed
		  especially for forms mode*/

		vrelcnf();
		if(vnt > 0)
			/*Should never happen until Quarantined Delivery
			  supported*/
		{
			/*VDATind-n(Vnt)*/
			vnt = 0;
		}
		clear_vte();
		state = S1_01;
	}
	else	/*Release Failed*/
	{
		if(vena)
		{
			sector = 5;
			if(vsmd) state = S5_40T;
			else state = S5_400B;
		}
		else
		{
			if(vsmd) state = S1_10T;
			else state = S1_10B;
		}
	}
	return(OK);
}

			
int
a1_21(pe)	/*DLQ (Deliver Request) in 50B or 51Q (Release Awaiting Peer)*/
PE pe;
{
	if( (vra = prim2flag(pe)) == NOTOK)
		adios(NULLCP,"a1_21: incorrect DLQ");
	if(vra)
	{
		vrsl = FAILURE;
		vrea = COLL_DET;
		/*VRELcnf required in spec but there's really nothing to tell
		  the user*/
	}
	if(vnt > 0) /*Should not happen unless Quarantine Delivery supported*/
	{
		/*VDATind-n(Vnt)*/
		vnt = 0;
	}
	vdelind(pe,vra);	/*Also irrelevant without Quarantine*/
	if(vra)
	{
		sector = 5;
		state = S5_402B;
	}
	return(OK);
}


/* ARGSUSED */
int
a1_22(pe)	/*RLQ (Release Request) in 50B*/
PE pe;
{

	vrsl = FAILURE;
	vrea = COLL_DET;
	/*VRELcnf -- Confirm to user telling of failure due to collision --
	  but user can't do anything now anyway. */

	if(vnt > 0) /*Shouldn't happen without Quarantine Delivery Ctrl*/
	{
		/*VDATind-n(Vnt)*/
		vnt = 0;
	}
	(void)vrelind();	/*Tell user that peer requested release*/
	state = S1_51R;
	return(OK);
}


/* ARGSUSED */
int
a1_23(pe)	/*SNQ (Start negotiation) in 50B*/
PE pe;
{
	/*Switch Negotiation not implemented.
	  Should probably send back negative acknowledgement. */

	return(OK);
}


/* ARGSUSED */
int
a1_24(pe)		/*SPQ (Switch Profile Request) in state 50B*/
PE pe;
{
	/*Profile Switch not implemented.
	  Should probably send back negative acknowledgement. */

	return(OK);
}


/* ARGSUSED */
int
a1_25(pe)	/*RLQ (Release Request) in 10B (Environment not agreed) */
PE pe;
{
	(void)vrelind();
	state = S1_51R;
	return(OK);
}

/* ARGSUSED */
int
a1_26(pe)	/*RLQ (Release Request) in state 10N*/
PE pe;
{
	(void)vrelind();
	state = S1_51R;
	return(OK);

}


/* ARGSUSED */
int
a1_27(pe)	/*RTQ (Request Token) in state 10T*/
PE pe;
{
	vrtq_ind();	/*Tell Application that peer requested token*/

	/*Probably some ISODE call to give token directly instead of telling
	  user */

	return(OK);
}


/* ARGSUSED */
int
a1_28(pe)	/*SNQ (Start Negotiation) in 10N*/
PE pe;
{
	/*MIN not implemented.
	  Need to return NAK. */

	return(OK);
}


/* ARGSUSED */
int
a1_29(pe)	/*SNQ (Start Negotiation) in 10B*/
PE pe;
{
	/*MIN not implemented.
	  Need to send NAK to peer. */

	return(OK);
}


/* ARGSUSED */
int
a1_30(pe)	/*SPQ (Switch Profile Request) in 10B & 10N*/
PE pe;
{
	/*Switch Profile not implemented.
	  Should send NAK to peer. */

	return(OK);
}


/* ARGSUSED */

int
a1_100(pe)	/*APQ (VT-P-ABORT -- Abort from VTPM) in any state*/
PE pe;
{
	state = S1_01;	/*For rigor*/
	adios(NULLCP, "protocol abort -- association terminated");
}


/* ARGSUSED */

int
a1_101(pe)	/*AUQ (VT-U-ABORT -- Abort from VT User) in any state*/
PE pe;
{
	state = S1_01;
	adios(NULLCP,"user abort -- association terminated");
}



/* ARGSUSED */
int
a1_102(pe)	/*VUABreq (Abort by User) in any state*/
PE pe;
{

	PE pe_auq;
	char *reason = "Association Closed by User";

	pe_auq = str2prim(reason,strlen(reason),PE_CLASS_CONT,AUQ_PDU);
	if(pe_auq == NULLPE)
		adios(NULLCP, "a1_102: AUQ build failure (out of memory)");
	if(AcUAbortRequest(sd,&pe_auq,1,aci) == NOTOK)
	    acs_adios (&aci -> aci_abort, "A-ABORT.REQUEST");
	state = S1_01;

	finalbye ();
	exit(1);
}


/* ARGSUSED */
int
a1_103(pe)	/*VTAB (Irrecoverable exception condition) in any state*/
PE pe;
{

	PE pe_apq;

	advise(LLOG_NOTICE,NULLCP,  "Irrecoverable Exception Condition -- Aborting\n");
	pe_apq = num2prim((integer)1,PE_CLASS_CONT,APQ_PDU);
			/*1 is value for Local Error.  0 if for Protocol
			  Error.  Assume 1 for now. */
	if(pe_apq == NULLPE)
		adios(NULLCP,"a1_103: APQ build failure (out of memory)");
	if(AcUAbortRequest(sd,&pe_apq,1,aci) == NOTOK)
	    acs_adios (&aci -> aci_abort, "A-ABORT.REQUEST");

	state = S1_01;	/*For completeness*/

	finalbye ();
	exit(1);
}


/* ARGSUSED */
int
a1_107(pe)		/*Generic Action*/
PE pe;
{
	/*Stay in this state*/
	return(OK);
}

