/* vtuser.c - VT user routines */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/vt/RCS/vtuser.c,v 7.5 91/02/22 09:48:34 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/vt/RCS/vtuser.c,v 7.5 91/02/22 09:48:34 mrose Interim $
 *
 *
 * $Log:	vtuser.c,v $
 * Revision 7.5  91/02/22  09:48:34  mrose
 * Interim 6.8
 * 
 * Revision 7.4  90/12/11  10:53:12  mrose
 * lock-and-load
 * 
 * Revision 7.3  90/10/23  20:44:48  mrose
 * update
 * 
 * Revision 7.2  90/07/09  14:52:10  mrose
 * sync
 * 
 * Revision 7.1  89/11/30  23:51:45  mrose
 * pa2str
 * 
 * Revision 7.0  89/11/23  22:32:00  mrose
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
#include "sector5.h"
#include <sys/ioctl.h>

#undef PTYBUG	/*When testing Break and demon not started from rc.local,
		  this turns off resetting local echo so ioctl to pty
		  will not cause demon to hang up.
		*/

#undef PEPYPARM
#define PEPYPARM int *


extern char peerhost[];
extern struct PSAPaddr ts_bound;
static char *myservice = "terminal";

static char *mycontext = "iso vt";
static char *mypci = "iso vt pci";

static char ascii_go_repertoire[] = {0x1a,0x28,0x42,0x00}; /*ESC 2/8 4/2*/
		/*should be followed by 3 "voids" whatever that is*/
static char full_ascii_repertoire[] = {0x1a,0x28,0x42,/*VOID*/0x1a,0x21,0x40,0x00};
		/*Approximation to GO & CO ASCII sets*/

extern char *isodeversion;
extern char *my_displayobj;
extern int G_Func_Units;	/*Functional Units for this Association*/
extern int vcwa;	/*Collision Winner (TRUE if owned by this peer)*/
extern int vsmd, vtok;
extern int transparent;
extern char na_image;
extern char ni_image;
extern VT_PROFILE vtp_profile;
extern int telnet_profile;
extern int do_break;
extern int debug;

int default_rep_flag = 0;

/*************************************************************************/
/* VASS_REQ - create an ASQ PDU and generate a VASSreq event to 	 */
/*			send it.					 */
/*									 */
/*	PARAMETERS - 							 */
/*									 */
/*		CLASS - class of VTP service (only BASIC supported)	 */
/*									 */
/*		ACC_RI - access rights					 */
/*									 */
/*		PROFILE - designator of the VT profile to request	 */
/*************************************************************************/ 
/* ARGSUSED */
vass_req(class, acc_ri, profile) 
int class, acc_ri;
VT_PROFILE *profile;
{
	PE		a_req;
	ASQ_MSG		ud;
	char		my_version, my_fu;
	OID		p_oid;
	int		i;

	my_version = 0x01;
	if(do_break) my_fu = destBreak;
	else my_fu = 0x00;

	bzero ((char *) &ud, sizeof ud);
	ud.class = class;
	ud.valid_imp = 0;
	ud.coll_winner = ACHOICE;
	ud.valid_coll = 1;
	ud.version.bitstring = my_version;
	ud.version.bitcount = 1;
	if( !strcmp(profile->profile_name,"telnet") )
	{
	    ud.valid_prof = 1;
	    ud.asq_profile.oid_true = 1;
	    if((p_oid = ode2oid(profile->profile_name)) == NULLOID)
		adios(NULLCP,"%s: unknown profile", profile->profile_name);
	    ud.asq_profile.prof_oid = p_oid;

	    ud.asq_profile.num_sp_param = 0;
	    ud.asq_profile.num_cds_objects = 0;
	    ud.asq_profile.num_css_objects = 0;
	    ud.asq_profile.num_dev_objects = 0;
	    ud.asq_profile.del_ctrl.bitcount = 0;

	    ud.asq_profile.num_cds_objects = 1;
	    ud.asq_profile.cds_offer_list[0].obj_name = "D";
	    for(i=0; i<ud.asq_profile.num_cds_objects; i++)
	    {
		ud.asq_profile.cds_offer_list[i].valid_x_dim = 0;
		ud.asq_profile.cds_offer_list[i].valid_y_dim= 0;
		ud.asq_profile.cds_offer_list[i].valid_z_dim = 0;
		ud.asq_profile.cds_offer_list[i].erasure.bitcount = 0;
		ud.asq_profile.cds_offer_list[i].valid_emp_list = 0;
		ud.asq_profile.cds_offer_list[i].valid_fore_color = 0;
		ud.asq_profile.cds_offer_list[i].valid_back_color = 0;
		ud.asq_profile.cds_offer_list[i].access_right.bitcount = 0;
		ud.asq_profile.cds_offer_list[i].valid_rep_list = 0;
	    }
	    if(!vtp_profile.arg_val.tel_arg_list.full_ascii)
	    {
		ud.asq_profile.cds_offer_list[0].valid_rep_list = 1;
		ud.asq_profile.cds_offer_list[0].rep_offer.valid_cap = 1;
		ud.asq_profile.cds_offer_list[0].rep_offer.capability.type = 0;
		ud.asq_profile.cds_offer_list[0].rep_offer.capability.value = 1;
		ud.asq_profile.cds_offer_list[0].rep_offer.num_reps = 1;
		ud.asq_profile.cds_offer_list[0].rep_offer.repertoire[0].rep_type = 2;
		ud.asq_profile.cds_offer_list[0].rep_offer.repertoire[0].valid_font_cap = 0;
		ud.asq_profile.cds_offer_list[0].rep_offer.repertoire[0].num_fonts = 0;
		ud.asq_profile.cds_offer_list[0].rep_offer.repertoire[0].rep_assign = 
			ascii_go_repertoire;
	    }

	    ud.asq_profile.cds_offer_list[0].valid_x_dim = 1;
	    ud.asq_profile.cds_offer_list[0].x_dim.bound_type = 0;
	    ud.asq_profile.cds_offer_list[0].x_dim.addressing.bitcount = 0;
	    ud.asq_profile.cds_offer_list[0].x_dim.absolute.bitcount = 0;
	    ud.asq_profile.cds_offer_list[0].x_dim.window_type = 2;
	    ud.asq_profile.cds_offer_list[0].x_dim.window.type = 0;
	    ud.asq_profile.cds_offer_list[0].x_dim.window.value =
			vtp_profile.arg_val.tel_arg_list.x_window;

	    vtok = 1;
	    vsmd = 0;
	}
	else if( !strcmp(profile->profile_name,"default") )
	{
	    ud.valid_prof = 0;
	    ud.asq_profile.oid_true = 0;
	    ud.asq_profile.num_sp_param = 0;
	    ud.asq_profile.num_cds_objects = 0;
	    ud.asq_profile.num_css_objects = 0;
	    ud.asq_profile.num_dev_objects = 0;
	    vtok = 1;
	    vsmd = 0;
	}
	else
	    adios(NULLCP, "%s: unsupported profile", profile->profile_name);

	ud.func_units.bitstring = my_fu;
	ud.func_units.bitcount = 5;

	if(build_ASQPDU_ASQpdu(&a_req,1,NULL,NULLCP,(PEPYPARM)&ud) == NOTOK)
	    adios(NULLCP, "ASQ build failure (%s)", PY_pepy);

	(void)do_event(VASSreq, a_req);
}


/******************************************************************************/
/*	VASS_RESP - create an ASR PDU and generate a VASSRSP event to send it.*/
/*									      */
/*	PARAMETERS - 							      */
/*									      */
/*		RESULT - SUCCESS or FAILURE				      */
/******************************************************************************/

vass_resp(result)
int result;
{
	PE	a_resp;
	char	my_version, my_fu;
	ASR_MSG ud;
	int i;

	my_version = 0x01;
	if(G_Func_Units & destBreak) do_break = 1;
	else do_break = 0;
	my_fu = G_Func_Units & destBreak; /*VT-Break is only Functional Unit
					    we will accept*/
	bzero ((char *) &ud, sizeof ud);
	ud.valid_reason = 0;
	ud.result = result;
	ud.valid_imp = 0;
	ud.valid_coll = 1;
	if(vcwa == TRUE) ud.coll_winner = ACCEPTOR;
	else ud.coll_winner = INITIATOR;
	ud.version.bitstring = my_version;
	ud.version.bitcount = 1;
	if( !strcmp(vtp_profile.profile_name,"telnet") )
	{
	    ud.valid_arg_list = 1;
	    ud.arg_list.num_css_objects = 0;
	    ud.arg_list.num_dev_objects = 0;
	    ud.arg_list.num_cds_objects = 1;

	    ud.arg_list.cds_val[0].obj_name = "D";
	    for(i=0; i<ud.arg_list.num_cds_objects; i++)
	    {
		ud.arg_list.cds_val[i].dimensions = 0;
		ud.arg_list.cds_val[i].valid_x_dim = 0;
		ud.arg_list.cds_val[i].valid_y_dim = 0;
		ud.arg_list.cds_val[i].valid_z_dim = 0;
		ud.arg_list.cds_val[i].valid_erasure= 0;
		ud.arg_list.cds_val[i].valid_emp_list = 0;
		ud.arg_list.cds_val[i].valid_fore_color = 0;
		ud.arg_list.cds_val[i].valid_back_color = 0;
		ud.arg_list.cds_val[i].valid_access_right = 0;
		ud.arg_list.cds_val[i].valid_rep_list = 0;
		ud.arg_list.cds_val[i].rep_value.repertoire[0].valid_font_cap = 0;
		ud.arg_list.cds_val[i].rep_value.repertoire[0].num_fonts = 0;
	    }
	    if( !default_rep_flag)
	    {
		ud.arg_list.cds_val[0].valid_rep_list = 1;
		ud.arg_list.cds_val[0].rep_value.valid_cap = 1;
		ud.arg_list.cds_val[0].rep_value.capability = 1;
		ud.arg_list.cds_val[0].rep_value.num_reps =  1;
		ud.arg_list.cds_val[0].rep_value.repertoire[0].rep_type = 2;
		ud.arg_list.cds_val[0].rep_value.repertoire[0].rep_assign = 
		    vtp_profile.arg_val.tel_arg_list.full_ascii ?
			full_ascii_repertoire : ascii_go_repertoire;
	    }
	    ud.arg_list.num_sp_param = 0;
	    ud.arg_list.cds_val[0].valid_x_dim = 1;
	    ud.arg_list.cds_val[0].x_dim.bound_type = 0;
	    ud.arg_list.cds_val[0].x_dim.valid_addr = 0;
	    ud.arg_list.cds_val[0].x_dim.valid_abs = 0;
	    ud.arg_list.cds_val[0].x_dim.window_type = 2;
	    ud.arg_list.cds_val[0].x_dim.window =
		vtp_profile.arg_val.tel_arg_list.x_window;

	}
	else if( !strcmp(vtp_profile.profile_name,"default") )
	{
	    ud.valid_arg_list = 0;
	    ud.arg_list.num_sp_param = 0;
	    ud.arg_list.num_cds_objects = 0;
	    ud.arg_list.num_css_objects = 0;
	    ud.arg_list.num_dev_objects = 0;
	}
	else
	    adios (NULLCP, "invalid profile stored");
	ud.func_units.bitstring = my_fu;
	ud.func_units.bitcount = 5;

	if(build_ASRPDU_ASRpdu((PE *)&a_resp,1,NULL,NULLCP,(PEPYPARM)&ud) == NOTOK)
	    advise(LLOG_NOTICE,NULLCP,  "ASR build failure (%s) -- continuing",
		   PY_pepy);
	return(do_event(VASSrsp, a_resp));
}


/*************************************************************************/
/*	"pe" will be to store NDQ contents that could not be mapped to   */
/*	the cbuf because of lack of buffer space.  There is only one,    */
/*	because a new NDQ can be combined with "pe" rather than creating */
/*	a queue of PEs							 */
/*************************************************************************/

PE	pe_buf = NULLPE;


/************************************************************************/
/*  VRELREQ - Generate a VRELREQ to VT State Machine		        */
/* 									*/
/*     PARAMETERS - none						*/
/************************************************************************/

vrelreq()
{
	PE  r_req;

	r_req = NULLPE;
	(void)do_event(VRELreq,r_req);
}

/*************************************************************************/
/*  VRELRSP - create an RLR PDU and send it and generate a VRELRSP-S	 */
/* 									 */
/*     PARAMETERS -							 */
/*									 */
/*           RESULT - success or failure				 */
/*									 */
/*************************************************************************/

vrelrsp(result)
int result;
{

	int offset = 0;
	PE  r_rsp, r_result, r_coll;

	if ((r_rsp = pe_alloc(PE_CLASS_CONT, PE_FORM_CONS, RLR_PDU)) == NULLPE)
	    adios (NULLCP, "RLR build failure (out of memory)");

	if ((r_result = num2prim((integer)result,PE_CLASS_CONT,0)) == NULLPE) 
		adios (NULLCP, "RLR build failure (out of memory)");

	if (seq_add(r_rsp,r_result,offset) == NOTOK)
	    adios (NULLCP, "RLR build failure (%s)", pe_error(r_rsp -> pe_errno));

	if(result == COLL_DET)
	{
		if((r_coll = num2prim((integer)0,PE_CLASS_CONT,2)) == NULLPE)
		    adios (NULLCP, "RLR build failure (out of memory)");
		if (seq_add(r_rsp,r_coll,++offset) == NOTOK)
		    adios (NULLCP, "RLR build failure (%s)",
			   pe_error(r_rsp -> pe_errno));
	}
	if (seq2prim(r_rsp) == NULLPE)
		adios(NULLCP, "RLR encode error, seq2prim: (%s)", PY_pepy);
	(void)do_event(VRELrsp,r_rsp);

	pe_free(r_coll);
	pe_free(r_result);
	pe_free(r_rsp);

}


vrelcnf()
{
	if (debug)
		advise(LLOG_DEBUG, NULLCP,  "Release Confirmed");
}


vrelind()
{
	if (AcFINISHser(sd,pf,aci) == NOTOK)
	    acs_adios (&aci->aci_abort, "A-RELEASE.INDICATION");

	vrelrsp(SUCCESS);
	return(OK);
}



PE	p_ondq = NULLPE;  /* the current "ndq" being prepared for sending*/
PE	p_ovtsdi = NULLPE; 	/* the current "vtsdi"	*/
int	sdi_count = 0;		/* count of "vtsdi"s in current NDQ*/
PE	p_oobjupdt = NULLPE;	/* the current "object_update"	*/
int	obj_count = 0;	/* count of "object_update"s in current "vtsdi"*/
int	updt_count = 0;	/* count of updates in the current "object update"*/
int	cur_emode = NOT_ECHO_NOW; /* echo mode (ECHO_NOW or NOT_ECHO_NOW)*/

/*************************************************************************/
/*	VT_TEXT - Add a text update to the PE that represents 		 */
/*	an NDQ of buffered updates awaiting delivery.			 */
/*									 */
/*	When we do a control object update, the current buffer of	 */
/*	display object updates is terminated and a new one will be	 */
/*	started next time a display object update is queued.		 */
/*	(this is so we can synchronize control updates with		 */
/*	display updates.)						 */
/*									 */
/*	Likewise, whenever we queue a display object update, we		 */
/*	terminate the current sequence of control object updates.	 */
/*									 */
/*	Whenever we change echo mode (ECHO_NOW or NOT_ECHO_NOW)		 */
/*	we have to start a new "vtsdi".					 */
/*	This requires that we terminate the current buffers of		 */
/*	display object and control object updates.			 */
/*									 */
/*	PARAMETERS - 							 */
/*									 */
/*		STR - the character string to be added to the NDQ PDU.	 */
/*		LEN - Number of characters in the string.		 */
/*************************************************************************/

vt_text(str, len)
	char	*str;
	int		len;
{
	TEXT_UPDATE ud;

	if (debug > 6)
	{
	    int i;

	    (void)ll_log(vt_log, LLOG_DEBUG, NULLCP,  ("vt_text sending"));
	    (void)ll_printf (vt_log, "<<");
	    for(i=0; i<len; i++)
		(void)ll_printf (vt_log, "%02x ", *(str+i));
	    (void)ll_printf (vt_log,  ">>\n");
	    (void)ll_sync (vt_log);
	}

	bzero ((char *) &ud, sizeof ud);
	ud.echo_sw = cur_emode;
	ud.type_sw = 0;		/*Display object*/
	ud.updates.do_list.do_name = my_displayobj;
	ud.updates.do_list.do_type = DO_TEXT;		/*Text*/
	ud.updates.do_list.do_cmd.text_ud.text_ptr = str;
	ud.updates.do_list.do_cmd.text_ud.text_count = len;
	send_queue(ud);

	return(OK);
}


send_queue(ud)		/*Build NDQ with update supplied in ud structure*/
TEXT_UPDATE ud;
{

	PE vtsdip;

	if(p_ondq == NULLPE)	/*Nothing waiting to be sent*/
	{
		if(build_NDQPDU_NDQpdu(&p_ondq,1,NULL,NULLCP,(PEPYPARM)&ud) == NOTOK)
		    adios(NULLCP,"NDQ build failure (%s)", PY_pepy);
		p_ondq->pe_context = 1;
	}
	else
	{
		if(build_NDQPDU_VTsdi(&vtsdip,1,NULL,NULLCP,(int *)&ud) == NOTOK)
		    adios(NULLCP,"VTsdi build failure (%s)", PY_pepy);
		vtsdip->pe_context = 1;
		if(seq_add(p_ondq,vtsdip,-1) == NOTOK)
		    adios(NULLCP,"NDQ build failure (%s)",
			  pe_error(p_ondq->pe_errno));
	}
}



/* SETEMODE - set echo mode

	PARAMETERS - 

		MODE - ECHO_NOW or NOT_ECHO_NOW 
*/

setemode(mode)
	int	mode;
{
	if (mode != ECHO_NOW && mode != NOT_ECHO_NOW)
		return(NOTOK);
	if (cur_emode != mode) {
		p_ovtsdi = NULLPE;
		sdi_count = 0;
		p_oobjupdt = NULLPE;
		obj_count = 0;
		cur_emode = mode;
	}
	return(OK);
}


/* this data structure will buffer character output
   that is ready to be read by the application or terminal.
*/

#define CBUFSIZE	10240

struct char_buffer {
	int		max_len, queued;
	unsigned char	*head, *tail;
	unsigned char	buf[CBUFSIZE];
};

struct char_buffer	cbuf = { CBUFSIZE, 0 };

/************************************************************************/
/* GETCH - get a character from the buffer waiting to 			*/
/*			be read by the application			*/
/*									*/
/*	RETURNS - the character, NOTOK if no data, or an error code (<0)*/
/************************************************************************/


int
getch()
{
	int		c;

	if (data_pending() == FALSE) {
		if (!connected)
			return(E_EOF);
		else
			return(WOULDBLOCK);
	}
	c = *cbuf.head;
	if (++cbuf.head >= cbuf.buf + CBUFSIZE)
		cbuf.head = cbuf.buf;
	cbuf.queued--;
	if (debug > 1)
	    advise(LLOG_DEBUG, NULLCP,  "normal return from getch, c is %c,queued is %d", c,cbuf.queued);
	return(c);
}

/* at some point we need to use the async. interface to the network
   so that at any time when data becomes available, we trap to a vtpm
   function to handle it.  This function will examine the type of
   network event and act accordingly.  Each time the function is invoked
   it should completely process the data received and free the PSAPdata
   structure for use in processing the next network event.  
   If it is an expedited 
   data request, for instance, it is treated differently from a P-DATA.  
   If an NDQ is
   received all the data should be read from the PSAPread structure and
   mapped to the cbuf.
   If the cbuf fills up in the process of doing this, the PE containing the
   remaining updates should be put in a queue of pending PEs.
*/


/* This macro does the same thing as PXFREE except it does not free
   the PEs in the px_info array.  We will use this instead of PXFREE
   because we need to free the data PEs one at a time as they are 
   processed.  
   Any unprocessed (pending) PEs are maintained in a queue
   by the VTPM. If some of the PEs received from a PDATArequest are in
   this queue a call to PXFREE would free the
   data and leave dangling references in the queue.

   Note that the vtuser may have at most one unprocessed or partially
   processed PE.  The VTPM can potentially have any number of unprocessed
   PEs in its queue.
*/

#define	PFIN(px) \
{ \
    register int PXI; \
 \
    if ((px) -> px_realinfo) \
		pe_free ((px) -> px_realinfo), (px) -> px_realinfo = NULLPE; \
    else { \
		for (PXI = (px) -> px_ninfo - 1; PXI >= 0; PXI--) \
			if ((px) -> px_info[PXI]) \
				(px) -> px_info[PXI] = NULLPE; \
		(px) -> px_ninfo = 0; \
    } \
}


int
data_pending()
{
	int	result;
	PE	*peptr = NULLPEP;

	if (queued())
		return(TRUE);

		/* something was already in the cbuf
		*/

	if (pe_buf != NULLPE) {

		/* there seems to be something to map
		*/

		map(pe_buf);
		if (queued())
			return(TRUE);
	}
	result = get_event(sd, peptr);

	/* if there was no network event
	*/
	if (result == NOTOK)
		return(FALSE);

	/* get_event may have resulted in data being read and mapped to the
	   cbuf
	*/

	if (queued())
		return(TRUE);

	/* if there is no data left and get_event resulted in the association
	   being released
	*/
	if (!connected) {
		(void)putch(EOF);
		return(TRUE);
	}

	/* there's nothing to read right now, but we're still connected
	*/
	return(FALSE);
}


int
queued()
{
	return(cbuf.queued);
}


/*************************************************************************/
/* 	PUTCH - put a character on the buffer to be read by the		 */
/*			application					 */
/*									 */
/*	RETURNS - OK on success, NOTOK otherwise			 */
/*************************************************************************/
int
putch(c)
	char	c;
{
	if (debug > 1) {
	    advise(LLOG_DEBUG, NULLCP,  "in putch, queued is %d, c is %c", cbuf.queued, c);
	    advise(LLOG_DEBUG, NULLCP,  "cbuf.buf is %d, cbuf.head is %d, cbuf.tail is %d", (int)cbuf.buf, (int)cbuf.head, (int)cbuf.tail);
	    advise(LLOG_DEBUG, NULLCP,  "cbuf.max_len is %d", (int)cbuf.max_len);
	}
	if (cbuf.queued >= CBUFSIZE) {
	    if (debug > 1)
		advise(LLOG_DEBUG, NULLCP,  "***********************\nputch: queued exceeds CBUFSIZE ***************");
	    return(NOTOK);
	}
	if (cbuf.queued <= 0) {
		cbuf.tail = cbuf.head = cbuf.buf;
		cbuf.queued = 0;
	if (debug)
		advise(LLOG_DEBUG, NULLCP,  "tail and head set to %d", (int)cbuf.buf);
	}
	*(cbuf.tail) = c;
	if (++(cbuf.tail) > cbuf.buf + CBUFSIZE)
		cbuf.tail = cbuf.buf;
	cbuf.queued++;
	return(OK);
}
	

/*************************************************************************/
/*	VTSEND - send the updates that have been put into the PE	 */
/*			called "p_ondq".				 */
/*************************************************************************/

vtsend()
{
	if(p_ondq == NULLPE) return;
	vtdata(p_ondq);
	pe_free(p_ondq);
	p_ondq = NULLPE;
	p_ovtsdi = NULLPE; 
	sdi_count = 0; 
	p_oobjupdt = NULLPE; 
	obj_count = 0;
	updt_count = 0;
}

/************************************************************************/
/* VTDATA - generate a VDATREQ event to send an NDQ			*/
/*									*/
/*	PARAMETERS							*/
/*									*/
/*		NDQ - a presentation element containing an NDQ.		*/
/************************************************************************/

vtdata(ndq)
	PE	ndq;
{
	if (ndq == NULLPE)
		return;

	(void)do_event(VDATreq_n,ndq);
}


/************************************************************************/
/* MKDELIVER - create a DLQ. Requests for an acknowlegement are not	*/
/*		allowed at this time.					*/
/************************************************************************/

PE
mkdeliver(ack)
	int	ack;
{
	PE	p_dlq;

	if (ack != FALSE)
		adios(NULLCP, "DLQ PDUs can only be sent without an ACK request");
	if ((p_dlq = bool2prim(ack)) == NULLPE)
	    adios (NULLCP, "DLQ build failure (out of memory)");
	p_dlq->pe_id = DLQ_PDU;
	p_dlq->pe_class = PE_CLASS_CONT;
	p_dlq->pe_context = 1;
	return(p_dlq);
}

/**************************************************************************/
/* VDELREQ - create a deliver request PE and generate a VDELreq		  */
/*			event to send it.				  */
/**************************************************************************/

vdelreq(ack)
	int	ack;
{
	PE	p_dlq;

	if (ack)
		adios(NULLCP, 
			"ACK requests in deliver PDUs not supported at this time");
	p_dlq = mkdeliver(FALSE);

	(void)do_event(VDELreq,p_dlq);
}


/**************************************************************************/
/* VDELIND - we queue up data to go to the terminal when the NDQ	  */
/*			is received, so there's really nothing		  */
/*			to do when we get a VDELIND			  */
/*									  */
/*	PARAMETERS: 							  */
/*			ACK - TRUE or FALSE according to whether 	  */
/*			acknowledgement is requested or not.		  */
/**************************************************************************/

vdelind(del_pe,ack)
	PE	del_pe;
	int	ack;
{
	if (ack) {
		if (debug)
		    advise(LLOG_DEBUG, NULLCP,  "vdelind with ack requested not implemented!");
	}
	pe_free(del_pe);
}

/************************************************************************/
/* VDATIND - On receiving a data indication we will go ahead and	*/
/* map the contents onto the character buffer to go to the terminal	*/
/*									*/
/* PARAMETERS - "type" can be SEQUENCED or NONSEQUENCED			*/
/*				only SEQUENCED is implemented now	*/
/************************************************************************/

vdatind(type, pe)
	int	type;
	PE	pe;
{
	if (type != SEQUENCED)
		adios(NULLCP, "unimplemented NDQ type %d", type);
	map(pe);
}

vhdatind(pe)
PE pe;
{

	advise(LLOG_NOTICE,NULLCP,"vhdatind(): HDQ's not supported\n");
	pe_free(pe);
}

vudatind(pe)
PE pe;
{

	TEXT_UPDATE ud;

	if(unbuild_UDQPDU_UDQpdu(pe,1,NULLIP,NULLVP,(PEPYPARM) &ud) == NOTOK)
	{
		advise(LLOG_NOTICE,NULLCP,"UDQ parse failure\n");
	}
	else
	{
		control_ud((CO_UPDATE *) &(ud.updates.co_list) );
		free( (char *)ud.updates.co_list.co_name);
		pe_free(pe);
	}
}

/*****************************************************************************/
/* Connect_request:							     */
/*									     */
/*	Sends an ASQ, waits for a confirm.				     */
/*									     */
/*	Returns the file descriptor that corresponds to the network socket   */
/*	for the association, or NOTOK if the association failed.	     */
/*									     */
/*	The assumption is made that the acs_sd data element of the AcSAPstart*/
/*	structure is same as the file descriptor for the network socket used */
/*	by the association.						     */
/*****************************************************************************/

con_req()
{
	int	uevent;

	if (debug)
	    advise(LLOG_DEBUG, NULLCP,  "in con_req");

	vass_req(1,WACI_WACA,&vtp_profile);
	if (acc->acc_result != ACS_ACCEPT) {
	    advise(LLOG_NOTICE,NULLCP,  "association rejected: [%s]",
		   AcErrString (acc -> acc_result));
	    state = S1_01;
	    return NOTOK;
	}

	if (debug) {
	    advise(LLOG_DEBUG, NULLCP,  "got associate confirm event, sd is %d", acc->acc_sd);
	    advise(LLOG_DEBUG, NULLCP,  "acc_ninfo is %d", acc->acc_ninfo);
	    advise(LLOG_DEBUG, NULLCP,  "pe_id is %d", acc->acc_info[0]->pe_id);
	    advise(LLOG_DEBUG, NULLCP,  "pe_class is %d", acc->acc_info[0]->pe_class);
	    advise(LLOG_DEBUG, NULLCP,  "pe_form is %d", acc->acc_info[0]->pe_form);
	}

	if (acc->acc_ninfo < 1) 
		adios(NULLCP, "no ASQ PDU sent with the associate confirm");

	sd = acc->acc_sd;
	uevent = do_event(ASR,acc->acc_info[0]);

	if (debug)
	    advise(LLOG_DEBUG, NULLCP, "got user event %d", uevent);

	if(uevent == SUCCESS) return(sd);
	else return(-1);
}



read_asq(pe)	/*Unwrap ASQ PDU.  Use information it contains to fill in
		  some global values (profile_id,G_Func_Units,vcwa).
		  Return 0 if ASQ is improperly formatted or missing a
		  required field.  For now, only the more obvious fields are
		  checked  and only transparent and telnet profiles
		  are handled.  Return PROFILE_NG if profile is not
		  supported.  Return 1 if ASQ is valid.
		*/
PE pe;
{

	int i,n, D;
	ASQ_MSG ud;

	bzero ((char *) &ud, sizeof ud);
	if(unbuild_ASQPDU_ASQpdu(pe,1,NULLIP,NULLVP,(PEPYPARM)&ud) == NOTOK)
	{
	    advise(LLOG_NOTICE,NULLCP,  "ASQ parse failure (%s)", PY_pepy);
	    return(0);
	}


	if(!ud.class)
	{
	    advise(LLOG_DEBUG, NULLCP,  "ASQ without Class");
	    return(0);
	}
	if(ud.valid_coll)
	{
	    if(ud.coll_winner == INITIATOR) vcwa = FALSE;
	    else vcwa = TRUE;
	}
	G_Func_Units = ud.func_units.bitstring & 0x1f;
	if( (!ud.valid_prof) || (!ud.asq_profile.oid_true) ||
		!oid_cmp(ud.asq_profile.prof_oid,ode2oid("default")) )
	{
	    vtp_profile.profile_name = "default";
	    my_displayobj = "DISPLAY-OBJECT-1";
	    telnet_profile = 0;
	    return(1);
	}
	if( !oid_cmp(ud.asq_profile.prof_oid,ode2oid("telnet")) )
	{
	    vtp_profile.profile_name = "telnet";
	    vtp_profile.arg_val.tel_arg_list.full_ascii = 0xff;
	    vtp_profile.arg_val.tel_arg_list.x_window = -1;

	    D = -1;
	    for(n=0; n<ud.asq_profile.num_cds_objects; n++)
	    {
		if( *ud.asq_profile.cds_offer_list[n].obj_name == 'D')
		{
		    D = n;
		    break;
		}
	    }
	    if(D < 0)
	    {
			advise(LLOG_DEBUG, NULLCP,  "ASQ with no D Display Object");
			return(0);
	    }

	    if( !ud.asq_profile.cds_offer_list[D].valid_rep_list )
	    {
		vtp_profile.arg_val.tel_arg_list.full_ascii = 1;
		default_rep_flag = 1;
	    }
	    else	/*Repertoire specified*/
	    {
		if(ud.asq_profile.cds_offer_list[D].rep_offer.num_reps
							> MAXREPS)
		{
		    advise(LLOG_DEBUG, NULLCP,  "ASQ with too many repertoires");
		    return(0);
		}
		for(i=0; i< ud.asq_profile.cds_offer_list[D].rep_offer.num_reps;
				i++)
		{
		    if(ud.asq_profile.cds_offer_list[D].rep_offer.repertoire[i].rep_type != 2)
				continue;
		    if(!strncmp(ud.asq_profile.cds_offer_list[D].rep_offer.repertoire[i].rep_assign,
				ascii_go_repertoire,sizeof(ascii_go_repertoire)))
		    {
			vtp_profile.arg_val.tel_arg_list.full_ascii = 0;
			advise(LLOG_DEBUG, NULLCP,  "Using ASCII GO Repertoire.");
			break;
		    }
		    if(!strncmp(ud.asq_profile.cds_offer_list[D].rep_offer.repertoire[i].rep_assign,
				full_ascii_repertoire,sizeof(full_ascii_repertoire)))
		    {
			vtp_profile.arg_val.tel_arg_list.full_ascii = 1;
			break;
		    }
		}
		if(vtp_profile.arg_val.tel_arg_list.full_ascii < 0) return(0);
	    }
	    transparent = 0;

	    if(ud.asq_profile.cds_offer_list[D].valid_x_dim == 0)
	    {
		advise(LLOG_DEBUG, NULLCP,  "ASQ with no X-Window");
		return(0);
	    }
  	    if(ud.asq_profile.cds_offer_list[D].x_dim.window_type != 2)
				/*If not integer type window field*/
	    {
		advise(LLOG_DEBUG, NULLCP,  "ASQ with invalid X-Window");
		return(0);
	    }
	    if(ud.asq_profile.cds_offer_list[D].x_dim.window.type
					== 0)	/*If single value*/
	    {
		vtp_profile.arg_val.tel_arg_list.x_window =
				ud.asq_profile.cds_offer_list[D].x_dim.window.value;
	    }
	    else if(ud.asq_profile.cds_offer_list[D].x_dim.window.type == 1)
				/*If range*/
	    {
		if((ud.asq_profile.cds_offer_list[D].x_dim.window.min_val
						<= 80) &&
		(ud.asq_profile.cds_offer_list[D].x_dim.window.max_val
						>= 80))
		{
		    vtp_profile.arg_val.tel_arg_list.x_window = 80;
		}
		else
		{
		    vtp_profile.arg_val.tel_arg_list.x_window =
			    ud.asq_profile.cds_offer_list[D].x_dim.window.min_val;
		}

	    }
	    if(vtp_profile.arg_val.tel_arg_list.x_window < 0)
	    {
		advise(LLOG_DEBUG, NULLCP, "ASQ without x-window");
		return(0);
	    }
	    if(vtp_profile.arg_val.tel_arg_list.full_ascii < 0)
	    {
		advise(LLOG_DEBUG, NULLCP,  "Using Default for ASCII repertoire (Full ASCII)");
		vtp_profile.arg_val.tel_arg_list.full_ascii = 1;
	    }
	}
	else
	{
		advise(LLOG_DEBUG, NULLCP,  "Unknown Profile Requested");
		return(PROFILE_NG);
	}

	return(1);
}

vasscnf(pe)	/*Handle ASR received from Acceptor*/
PE pe;
{

	ASR_MSG udr;
	int rep, n;
	int window_flag = 0;
	int rep_flag = 0;

	bzero ((char *) &udr, sizeof udr);
	if(unbuild_ASRPDU_ASRpdu(pe,1,NULLIP,NULLVP,(PEPYPARM)&udr) == NOTOK)
	{
	    advise (LLOG_NOTICE,NULLCP,  "ASR parse failure (%s)", PY_pepy);
	    return(NOTOK);
	}
	if(udr.result != SUCCESS)
	{
		advise(LLOG_NOTICE,NULLCP,  "Association rejected by Peer VT");
		return(NOTOK);
	}
	if(udr.valid_coll) vcwa = udr.coll_winner;
	else 
		advise(LLOG_DEBUG, NULLCP,  "Received ASR with no collision winner");
	if(!strcmp(vtp_profile.profile_name,"transparent"))
	{
	    if(!udr.arg_list.cds_val[0].valid_rep_list) /*No repertoires*/
	    {
		if(strcmp(vtp_profile.arg_val.tr_arg_list.cur_rep,
				TRANSPARENT))
			/*If don't want default for this profile*/
		{
		    advise(LLOG_DEBUG, NULLCP,  "ASR with no repertoire");
		    return(NOTOK);
		}
	    }
	    if(strcmp(vtp_profile.arg_val.tr_arg_list.cur_rep,
			udr.arg_list.cds_val[0].rep_value.repertoire[0].rep_assign))
		/*Only support 1 repertoire in transparent*/
	    {
		advise(LLOG_DEBUG, NULLCP,  "ASR--Invalid repertoire for transparent profile");
		return(NOTOK);
	    }
	}
	else if(!strcmp(vtp_profile.profile_name,"telnet"))
	{
	    if(udr.arg_list.num_sp_param < 1)
	    {
		advise(LLOG_DEBUG, NULLCP,  "ASR without enough Special Arguments");
		return(0);
	    }
	    for(n=0; n<udr.arg_list.num_sp_param; n++)
	    {
		if(udr.arg_list.sp_val[n].param_num == 1)
		{
		    if(udr.arg_list.sp_val[n].param_type == 1)
				/*If integer type*/
		    {
			if(vtp_profile.arg_val.tel_arg_list.x_window !=
				udr.arg_list.sp_val[n].args.int_arg)
			{
			    advise(LLOG_DEBUG, NULLCP,  "ASR with invalid X-Window");
			    return(NOTOK);
			}
			else ++window_flag;
		    }
		}
		else if(udr.arg_list.sp_val[n].param_num == 2)
				/*ASCII Repertoire type*/
		{
		    if(udr.arg_list.sp_val[n].param_type == 0)
				/*If Boolean*/
		    {
			if(vtp_profile.arg_val.tel_arg_list.full_ascii)
				rep = 1;
			else rep = 0;
			if(udr.arg_list.sp_val[n].args.bool_arg != rep)
			{
			    advise(LLOG_DEBUG, NULLCP,  "ASR with invalid Repertoire");
			    return(NOTOK);
			}
			++rep_flag;
		    }
		}
	    }		/*End for loop*/
	    if(!window_flag)
	    {
		advise(LLOG_DEBUG, NULLCP,  "ASR without x-window");
		return(NOTOK);
	    }
	    if(!rep_flag)
	    {
		advise(LLOG_DEBUG, NULLCP,  "ASR with no repertoire");
		return(NOTOK);
	    }
	}
	return(OK);
}


asq(data)
PE data;
{
	int	srequirements;
	struct PSAPctxlist vclist;
	OID vt_asn;
	struct QOStype qos;

	qos.qos_reliability = HIGH_QUALITY;
	qos.qos_sversion = 2;

	if (debug)
	    advise(LLOG_DEBUG, NULLCP,  "in asq");

	acc = &accs;
	acr = &acrs;
	aci = &acis;

/*	 I'm relying on "peerhost" being an external char * that
	has the name of the host we want to connect to
*/
	if ((aei = _str2aei (peerhost, myservice, "iso vt", 1, NULLCP, NULLCP))
	        == NULLAEI)
	    adios (NULLCP, "unable to resolve service: %s", PY_pepy);
	if ((pa = aei2addr (aei)) == NULLPA)
		adios (NULLCP, "address translation failed");

	if ((ctx = ode2oid (mycontext)) == NULLOID)
		adios (NULLCP, "%s: unknown object descriptor", mycontext);
	if ((ctx = oid_cpy (ctx)) == NULLOID)
		adios (NULLCP, "out of memory");
	if ((pci = ode2oid (mypci)) == NULLOID)
		adios (NULLCP, "%s: unknown object descriptor", mypci);
	if ((pci = oid_cpy (pci)) == NULLOID)
		adios (NULLCP, "out of memory");

	if ((sf = addr2ref (PLocalHostName ())) == NULL) {
		sf = &sfs;
		(void) bzero ((char *) sf, sizeof *sf);
	}

	PLOG (vt_log, print_VT_PDUs, data, NULLCP, 0);
	aca = &aci->aci_abort;
	srequirements = SR_DUPLEX | SR_RESYNC | SR_TYPEDATA;
	srequirements &= ~SR_RLS_EXISTS;

	if((vt_asn = oid_cpy (pci)) == NULLOID)
		adios (NULLCP, "out of memory");
	vclist.pc_nctx = 1;
	vclist.pc_ctx[0].pc_id = 1;
	vclist.pc_ctx[0].pc_asn = vt_asn;
	vclist.pc_ctx[0].pc_atn = NULLOID;
	data -> pe_context = 1;

	if (AcAssocRequest (ctx, NULLAEI, aei, NULLPA, pa,
		&vclist, pci,
		0, srequirements, SERIAL_MIN, 0, sf, &data, 1, &qos,
		acc, aci) == NOTOK)
	    acs_adios (aca, "A-ASSOCIATE.REQUEST");

	if (acc -> acc_result != ACS_ACCEPT)
	    return;

	sd = acc->acc_sd;
	ts_bound = acc -> acc_connect.pc_responding;	/* struct copy */
#ifdef	DEBUG
	{
	    register int    i;
	    register struct PSAPconnect *pc = &acc -> acc_connect;
	    register struct PSAPctxlist *pl = &pc -> pc_ctxlist;

	    advise (LLOG_DEBUG, NULLCP,  "context: %s",
		    oid2ode (acc -> acc_context));

	    advise (LLOG_DEBUG, NULLCP,
		    "responding AE title: %s, responding PSAP address: %s",
		    sprintaei (&acc -> acc_respondtitle),
		    paddr2str (&pc -> pc_responding, NULLNA));

	    for (i = 0; i < pl -> pc_nctx; i++)
		advise (LLOG_DEBUG, NULLCP,  "ctx %d: 0x%x 0x%x %d",
			pl -> pc_ctx[i].pc_id, pl -> pc_ctx[i].pc_asn,
			pl -> pc_ctx[i].pc_atn, pl -> pc_ctx[i].pc_result);
	    advise (LLOG_DEBUG, NULLCP,  "default %d", pc -> pc_defctxresult);
	    advise (LLOG_DEBUG, NULLCP,  "p/s requirements 0x%x/0x%x",
		    pc -> pc_prequirements, pc -> pc_srequirements);
	}
#endif
}

vt_disconnect()
{
	if (AcRelRequest (sd, ACF_NORMAL, NULLPEP, 0, NOTOK, acr, aci)
	        == NOTOK)
	    acs_adios (aca, "A-RELEASE.REQUEST");
	if(acr->acr_affirmative) {
	    connected = FALSE;
	    (void) do_event (RLR, acr -> acr_info[0]);
	}

	ACRFREE (acr);

}



#define	ASYNC	0

#define	RMASK \
	"\020\01HALFDUPLEX\02DUPLEX\03EXPEDITED\04MINORSYNC\05MAJORSYNC\06RESYNC\
\07ACTIVITY\010NEGOTIATED\011CAPABILITY\012EXCEPTIONS\013TYPEDATA"

#define	PMASK \
	"\020\01MANAGEMENT\02RESTORATION"

/*    DATA */

long	time ();
char   *ctime ();
int	result;

/*PE	pe;*/ 

/*************************************************************************/
/*    ASS_IND 								 */ 
/*************************************************************************/

ass_ind (argc, argv)
	int	argc;
	char  **argv;
{
    register struct PSAPctxlist *pl;


	aca = &aci->aci_abort;
	ps = &acs->acs_start;
        pl = &ps -> ps_ctxlist;

	if (AcInit (argc, argv, acs, aci) == NOTOK)
		acs_adios (aca, "initialization fails");

    advise (LLOG_NOTICE,NULLCP, 
		"A-ASSOCIATE.INDICATION: <%d, %s, %s, %s, %d>",
		acs -> acs_sd, oid2ode (acs -> acs_context),
		sprintaei (&acs -> acs_callingtitle),
		sprintaei (&acs -> acs_calledtitle), acs -> acs_ninfo);

	advise (LLOG_NOTICE,NULLCP, 
		"PSAP: <%d, %s, %s, %d, %s,",
		ps -> ps_sd,
		paddr2str (&ps -> ps_calling, NULLNA),
		paddr2str (&ps -> ps_called, NULLNA),
		pl -> pc_nctx, sprintb (ps -> ps_prequirements, PMASK));
	advise (LLOG_NOTICE,NULLCP, 
		"  %s, %d, %d>",
		sprintb (ps -> ps_srequirements, RMASK), ps -> ps_isn,
		ps -> ps_ssdusize);

	(void) strcpy (peerhost,
		       na2str (ps -> ps_calling.pa_addr.sa_addr.ta_addrs));

	sd = acs->acs_sd;

/*	ACSFREE(acs);
*/

	PLOG (vt_log, print_VT_PDUs, acs -> acs_info[0], NULLCP, 1);

	return( do_event(ASQ,acs->acs_info[0]) );
}



/* ARGSUSED */

vassind(pe)
	PE	pe;
{ 
	return(vass_resp(SUCCESS));
}


vbrkreq()
{
	PE brk_pe;
	BRcnt brk;
	
	bzero ((char *) &brk, sizeof brk);
	brk.BKQcont.token_val = NOBKTOK;
	brk.BKQcont.ExplPtr.xval = 0;
	brk.BKQcont.ExplPtr.yval = 0;
	brk.BKQcont.ExplPtr.zval = NULLCOORD;
	if ((build_VT_BKQ__pdu(&brk_pe,1,NULL,NULLCP,(PEPYPARM)&brk)) == NOTOK)
	    adios (NULLCP, "BKQ build failed (%s)", PY_pepy);
	brk_pe->pe_context = 1;
	flushbufs();  /* flush local buffers */
	(void)do_event(VBRKreq,brk_pe);
}

vbrkrsp()
{
	PE brk_pe;
	BRcnt brk;
	
	bzero ((char *) &brk, sizeof brk);
	brk.BKRcont.token_val = NOBKTOK;
	brk.BKRcont.ExplPtr.xval = 0;
	brk.BKRcont.ExplPtr.yval = 0;
	brk.BKRcont.ExplPtr.zval = NULLCOORD;
	if ((build_VT_BKR__pdu(&brk_pe,1,NULL,NULLCP,(int *)&brk)) == NOTOK)
	    adios (NULLCP, "BKR build failed (%s)", PY_pepy);
	brk_pe->pe_context = 1;
	(void)do_event(VBRKrsp,brk_pe);
}


/* ARGSUSED */
vbrkind(brk_pe)
PE brk_pe;
{
	flushbufs();
	vtok = 1; /* got tokens from peer */
	advise(LLOG_DEBUG, NULLCP,  "Received VT-BREAK");
	vt_clr_obj();	/*Initialize Control Objects*/
	vbrkrsp();
	if(telnet_profile)
	{
#ifndef PTYBUG
#ifdef BSD44
		ptyecho(0);
#else
		setmode(0,ECHO);	/*Return to Local Echo.  This call is not
			  the same for user (vtp) and server (vtpd) so for
			  now, VT-BREAK can only be requested at user side.*/
#endif
		vt_rem_echo(&na_image);
#endif
		vt_sup_ga(&na_image);
		kill_proc();
	}
	/*Re-Negotiate Remote Echo and Suppress Go Ahead*/
}

/*ARGSUSED */
vbrkcnf(brk_pe)
PE brk_pe;
{
	(void)printf("\r\n[break]\r\n");
	if(telnet_profile)
	{
#ifndef PTYBUG
		vt_rem_echo(&ni_image);
#endif
		vt_sup_ga(&ni_image);
	}
}
	
