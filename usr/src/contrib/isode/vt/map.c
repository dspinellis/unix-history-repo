/* map.c - VT telnet profile mappings */

#ifndef	lint
static char *rcsid = "$Header: /f/osi/vt/RCS/map.c,v 7.2 91/02/22 09:47:58 mrose Interim $";
#endif

/* 
 * $Header: /f/osi/vt/RCS/map.c,v 7.2 91/02/22 09:47:58 mrose Interim $
 *
 *
 * $Log:	map.c,v $
 * Revision 7.2  91/02/22  09:47:58  mrose
 * Interim 6.8
 * 
 * Revision 7.1  90/07/09  14:51:50  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  22:31:31  mrose
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


#define DO_LOCAL_ECHO
#undef PEPYPARM
#define PEPYPARM int *


#include "vtpm.h"
#include "sector1.h"

#include <sys/ioctl.h>
#ifdef BSD44
#include <sys/termios.h>
#endif

extern char erase_char;
extern char erase_line;
#ifdef BSD44
extern struct termios oterm;
#else
extern struct sgttyb ottyb;
extern struct tchars otc;
#endif
extern char intr_char;
extern char ni_image;
extern char na_image;
extern char nego_state;
extern char kb_image;
extern char di_image;
extern char sync_image;
extern char ga_image;
extern int my_right;
extern int cur_emode;
extern char *myhostname;
extern int pty;
extern int transparent;
extern int showoptions;
extern int debug;
extern int telnet_profile;

TEXT_UPDATE *ndq_queue, *deq();		/*Incoming (From Net) NDQ's*/

map(ndq)	/*Parse the given NDQ (could contain several updates).
		  Pass individual updates to appropriate processing
		  routine.
		*/
PE ndq;
{

	TEXT_UPDATE *ud;

	if(unbuild_NDQPDU_NDQpdu(ndq,1,NULLIP,NULLVP,(PEPYPARM)0) == NOTOK)
	{
		advise (LLOG_NOTICE,NULLCP,  "NDQ parse failure (%s)", PY_pepy);
		return;
	}
	while(ud = deq(&ndq_queue) )
	{
		if(ud->type_sw == DISPLAY_OBJ)
		{
			display_ud(&ud->updates.do_list);
			free((char *)ud->updates.do_list.do_name);
		}
		else if(ud->type_sw == CTRL_OBJ)
		{
			control_ud(&ud->updates.co_list);
			free((char *)ud->updates.co_list.co_name);
		}
		else 
			advise(LLOG_NOTICE,NULLCP,  "Invalid Update");
			free((char *)ud);
	}
	pe_free(ndq);
}


display_ud(doptr) 		/*Handle Display Updates*/
DO_UPDATE *doptr;
{

	int i;
	char *pt;
#ifdef BSD44
	struct termios term;
#else
	struct sgttyb ttyb;
#endif

	switch(doptr->do_type) {

	case DO_NEXT_X:
		if(putch('\r') == NOTOK) {
			if(debug)
				advise(LLOG_NOTICE,NULLCP,  "DROPPED CHAR");
			return;
		}

		if(my_right == INITIATOR) {
			if(putch('\n') == NOTOK) /*Current Telnet only gives
									   CR to PTY*/
			{
				advise(LLOG_NOTICE,NULLCP,  "DROPPED CHAR");
				return;
			}
		}
		break;

	case DO_NEXT_Y:
		if(debug)
			advise(LLOG_DEBUG,NULLCP,  "Next Y Array");
		break;

	case DO_PTR_REL:	/*Ignore for TELNET since next update must
						  be erase. */
		break;

	case DO_PTR_ABS:	/*Ignore for TELNET since must have been
						  preceeded by erase line. */
		break;

	case DO_TEXT:
		for(pt = doptr->do_cmd.text_ud.text_ptr, i = 0;
		    i < doptr->do_cmd.text_ud.text_count; ++pt,++i)
		{
			if(putch(*pt) == NOTOK)
			{
				advise(LLOG_NOTICE,NULLCP,  "DROPPED CHAR");
				return;
			}
		}
		free(doptr->do_cmd.text_ud.text_ptr);
		break;

	case DO_RPT_TEXT:
		if(debug)
			advise(LLOG_DEBUG,NULLCP,  "Repeat Text");
		break;

	case DO_ATTR:
		if(debug) 
			advise(LLOG_DEBUG,NULLCP,  "Write Attribute");
		attrib_hdlr(doptr);
		break;

	case DO_ERASE:
		if((doptr->do_cmd.erase.start_erase.ptr_type == 0) &&
		    (doptr->do_cmd.erase.end_erase.ptr_type == 0) )
		{
			if(my_right == ACCEPTOR)
			{
#ifdef BSD44
				if (tcgetattr(pty, &term) == -1)
					perror("ioctl");
				else
					(void)putch(erase_char=term.c_cc[VERASE]);	/* XXX what if _POSIX_VDISABLE */
#else
				if (ioctl(pty,TIOCGETP,(char*)&ttyb) == -1) {
					perror("ioctl");
					adios(NULLCP, "ioctl failed");
				}
				(void)putch(ttyb.sg_erase);
				erase_char = ttyb.sg_erase;
#endif
			}
			else (void)putch(erase_char);
		}
		else if((doptr->do_cmd.erase.start_erase.ptr_type == 3) &&
		    (doptr->do_cmd.erase.end_erase.ptr_type == 6))
		{
			if(my_right == ACCEPTOR)
			{
#ifdef BSD44
				if (tcgetattr(pty, &term) == -1)
					perror("ioctl");
				else
					(void)putch(erase_line=term.c_cc[VKILL]);	/* XXX what if _POSIX_VDISABLE */
#else
				if (ioctl(pty,TIOCGETP,(char*)&ttyb) == -1) {
					perror("ioctl");
					adios(NULLCP, "ioctl failed");
				}
				(void)putch(ttyb.sg_kill);
				erase_line = ttyb.sg_kill;
#endif
			}
			else (void)putch(erase_line);
		}
		break;

	case DO_PREV_X:
		if(debug)
			advise(LLOG_DEBUG,NULLCP,  "Previous X-Array\n");
		break;

	case DO_PREV_Y:
		if(debug)
			advise(LLOG_DEBUG,NULLCP,  "Previous Y-Array\n");
		break;
	}		/*End Switch*/
}


control_ud(coptr)		/*Handle Control Object Updates*/
CO_UPDATE *coptr;
{

	char active = 0;
#ifdef BSD44
	struct termios term;
#else
	struct sgttyb sb;
#endif

	if(!telnet_profile)
	{
		if((my_right == INITIATOR) && (!strcmp(coptr->co_name,"E")))
		/*The Echo Control Object in Default Profile is WACA*/
			def_echo(coptr);
		else
			advise(LLOG_NOTICE,NULLCP,  "Received Invalid CO Update under Default Profile\n");
		return;
	}
	if(coptr->co_type != 1)	/*Only Booleans allowed in TELNET*/
	{
		advise(LLOG_NOTICE,NULLCP,  "Invalid CO Type\n");
		return;
	}
	if(coptr->co_cmd.bool_update.mask_count == 0) active = 0xff;
	else active = *coptr->co_cmd.bool_update.mask;

	if(my_right == INITIATOR)
	{
		if(!strcmp(coptr->co_name,"DI") )
		{
			if(active & AYT_OBJ)
			/*If This CO contains potential update to Are You There bit*/
			{
				if( (di_image & AYT_OBJ) != 
				    (AYT_OBJ & *coptr->co_cmd.bool_update.value))
				/*If this bit was toggled*/
				{
					if(debug)
						advise(LLOG_DEBUG,NULLCP,  "Toggled AYT in DI\n");
					di_image ^= AYT_OBJ;	/*Save the new value*/
				}
			}
			if(active & AO_OBJ)
			/*If potential update to Abort Output bit*/
			{
				if( (di_image & AO_OBJ) !=
				    (AO_OBJ & *coptr->co_cmd.bool_update.value))
				/*Toggled AO bit*/
				{
					if(debug)
						advise(LLOG_DEBUG,NULLCP,  "Toggled AO in DI\n");
					di_image ^= AO_OBJ;	/*Record it*/
				}
			}
			if(active & IP_OBJ)
			/*If potential update to Interrupt Process bit*/
			{
				if( (di_image & IP_OBJ) !=
				    (IP_OBJ & *coptr->co_cmd.bool_update.value))
				/*Toggled AO bit*/
				{
					if(debug)
						advise(LLOG_DEBUG,NULLCP,  "Toggled IP in DI/n");
					di_image ^= IP_OBJ;
				}
			}
			if(active & DM_OBJ)
			{
				if( (di_image & DM_OBJ) !=
				    (DM_OBJ & *coptr->co_cmd.bool_update.value) )

				/*Toggled DM Bit*/
				{
					if(debug)
						advise(LLOG_DEBUG,NULLCP,  "Toggled DM in DI\n");
					di_image ^= DM_OBJ;
				}
			}
			if(active & BRK_OBJ)
			{
				if( (di_image & BRK_OBJ) !=
				    (BRK_OBJ & *coptr->co_cmd.bool_update.value) )
				/*Toggled Break Bit*/
				{
					if(debug)
						advise(LLOG_DEBUG,NULLCP,  "Toggled BRK in DI\n");
					di_image ^= BRK_OBJ;
				}
			}
		}
		else if( !strcmp(coptr->co_name,"NA") )
		{
			if(active & ECHO_OBJ)
			/*Update to Echo Control Object*/
			{
				if(ECHO_OBJ & *coptr->co_cmd.bool_update.value)
				/*Request from Server for Remote Echo*/
				{
					na_image |= ECHO_OBJ;
					if(showoptions)
						(void)printf("Remote Echo Update Received\r\n");
					if(ECHO_OBJ & nego_state) /*If now in Remote Echo*/
					{
						if(ni_image & ECHO_OBJ) /*No request outstatnding*/
						{
							if(showoptions)
								(void)printf("Server Request ignored--Now in Remote echo\r\n");
						}
						else
						{
							if(showoptions)
								(void)printf("Request for Local Echo Denied by Server\r\n");
							ni_image |= ECHO_OBJ;
						}
					}
					else	/*Else Not in Remote Echo*/
					{
						if(ni_image & ECHO_OBJ) /*I Requested Remote Echo*/
						/*This must be confirmation*/
						{
							if(showoptions)
								(void)printf("Server agreed to do Remote Echo\r\n");
						}
						else	/*Request to do Remote Echo*/
						{
							if(showoptions)
								(void)printf("Server Requested Remote Echo\r\n");
							ni_image |= ECHO_OBJ;
							vt_set_nego(ni_image,ECHO_OBJ);/*Respond "WILL"*/
						}
						(void) tmode(1);
						nego_state |= ECHO_OBJ;
						cur_emode = ECHO_NOW;	/*Want Server to Echo*/
					}
				}
				else	/*Request from server for Local Echo*/
				{
					if(showoptions)
						(void)printf("NA--Local Echo\r\n");
					cur_emode = NOT_ECHO_NOW;
					na_image &= ~ECHO_OBJ;
					if(nego_state & ECHO_OBJ) /*If now in Remote Echo*/
					{
						if(ni_image & ECHO_OBJ) /*If no request pending*/
						/*Must be request from sender*/
						{
							if(showoptions)
								(void)printf("Server requested Local Echo -- O.K.\r\n");
							ni_image &= ~ECHO_OBJ;
							vt_set_nego(ni_image,ECHO_OBJ);/*Respond "WILL"*/
						}
						else
						{
							if(showoptions)
								(void)printf("User request for Local Echo Accepted\r\n");
						}
						nego_state &= ~ECHO_OBJ;
						/*			    sb = ottyb;
		/*			    sb.sg_flags |= ECHO|CRMOD|CBREAK;
		/*			    ioctl(fileno(stdin),TIOCSETP,(char*)&sb);
		*/
						(void)tmode(2);
					}
					else	/*Else now in Local Echo*/
					{
						if(ni_image & ECHO_OBJ) /*If requeset pending*/
						/*Must be negative response*/
						{
							ni_image &= ~ECHO_OBJ;
							if(showoptions)
								(void)printf("Request for Remote Echo Denied by Server\r\n");
						}
						else /*Else no request pending*/
						{
							if(showoptions)
								(void)printf("Server Request Ignored--Now in Local Echo\r\n");
						}
					}
				}
			}
			if(active & SUP_GA)
			/*Update to Suppress Go Ahead Control Object*/
			{
				if(SUP_GA & *coptr->co_cmd.bool_update.value)
				{
					if(showoptions)
						(void)printf("Suppress Go Ahead\r\n");
					na_image |= SUP_GA;
					if((ni_image & SUP_GA) == (nego_state & SUP_GA))
					/*If no outstanding request from User*/
					{
						if(!(nego_state & SUP_GA))
						/*If not currently in Suppress Go Ahead*/
						{
							ni_image |= SUP_GA;
							vt_set_nego(ni_image,SUP_GA);/*Reply "Will"*/
						}
					}
					nego_state |= SUP_GA;/*Either here now or entering*/
				}
				else
				{
					if(showoptions)
						(void)printf("Go Ahead\r\n");
					na_image &= ~SUP_GA;
					if( (ni_image & SUP_GA) == (nego_state & SUP_GA) )
					/*Must be request from Server*/
					{
						ni_image |= SUP_GA;
						vt_set_nego(ni_image,SUP_GA);/*Reply "Won't"*/
					}
					else	/*Else response to my request to Suppress*/
					{
						if(showoptions)
							(void)printf("Server refuses to Suppress Go Ahead\r\n");
						ni_image &= ~SUP_GA;	/*Give Up*/
						/*May want to terminate Association here*/
					}
				}
			}
			if(active & DISP_BIN)
			/*Update to WACA Binary Repertoire*/
			{
				if(DISP_BIN & *coptr->co_cmd.bool_update.value)
				{
					if(showoptions)
						(void)printf("WACA requested Binary Repertoire on DI\r\n");
					if((ni_image & DISP_BIN) == (nego_state & DISP_BIN))
					/*No request outstanding from Initiator*/
					{
						if(!(nego_state & DISP_BIN)) /*If not now binary*/
						{
							ni_image |= DISP_BIN;
							vt_set_nego(ni_image,DISP_BIN); /*Send "Will"*/
						}
					}
					nego_state |= DISP_BIN;
					ni_image |= DISP_BIN;
				}
				else
				{
					if(showoptions)
						(void)printf("WACA requested ASCII Repertoire on DI\r\n");
					if((ni_image & DISP_BIN) == (nego_state & DISP_BIN))
					{
						if(nego_state & DISP_BIN) /*If not now ASCII*/
						{
							ni_image &= ~DISP_BIN;
							vt_set_nego(ni_image,DISP_BIN);
						}
					}
					nego_state &= ~DISP_BIN;
					ni_image &= ~DISP_BIN;
				}
			}
			if(active & KBD_BIN)
			/*Update to WACI Binary Repertoire*/
			{
				if(KBD_BIN & *coptr->co_cmd.bool_update.value)
				{
					if(showoptions)
						(void)printf("WACA requested Binary Repertoire on KB\r\n");
					if((ni_image & KBD_BIN) == (nego_state & KBD_BIN))
					/*If no initiator request outstanding*/
					{
						if(!(nego_state & KBD_BIN))/*If not now binary*/
						{
							ni_image |= KBD_BIN;
							vt_set_nego(ni_image,KBD_BIN); /*Reply "Will"*/
							switch_rep(2);
							/*Send Attribute update to use Binary Repertoire*/
						}
					}
					else	/*Else a response to Initiator Request*/
					{
						if(ni_image & KBD_BIN) /*Positive response*/
							switch_rep(2);
					}
					ni_image |= KBD_BIN;
					nego_state |= KBD_BIN;
				}
				else
				{
					if(showoptions)
						(void)printf("Acceptor requested ASCII Repertoire on KB\r\n");
					if((ni_image & KBD_BIN) == (nego_state & KBD_BIN))
					/*Request from Acceptor*/
					{
						if(nego_state & KBD_BIN) /*If not now ASCII*/
						{
							ni_image &= ~KBD_BIN;
							vt_set_nego(ni_image,KBD_BIN); /*Reply "Will"*/
							switch_rep(1);/*Send Attr to ASCII*/
						}
					}
					else	/*Else response to Initiator Request*/
					{
						if( !(ni_image & KBD_BIN))/*Positive response*/
							switch_rep(1);
					}
					ni_image &= ~KBD_BIN;
					nego_state &= ~KBD_BIN;
				}
			}
		}
	}
	else	/*Else Server (Display) side*/
	{
		if(!strcmp(coptr->co_name,"KB") )
		/*Server receives updates to the Keyboard*/
		{
			if(active & AYT_OBJ)
			/*If This CO contains potential update to Are You There bit*/
			{
				if( (kb_image & AYT_OBJ) != 
				    (AYT_OBJ & *coptr->co_cmd.bool_update.value))
				/*If this bit was toggled*/
				{
					if(debug)
						advise(LLOG_DEBUG,NULLCP,  "Toggled AYT in KB");
					kb_image ^= AYT_OBJ;	/*Save the new value*/
					if (vt_text("[associated with terminal service on ", strlen("[associated with terminal service on ")) != OK)
						advise(LLOG_NOTICE,NULLCP,  "vt_text failed");
					if (vt_text(myhostname,strlen(myhostname)) != OK)
						advise(LLOG_NOTICE,NULLCP,  "vt_text failed");
					if (vt_text("]\r\n",3) != OK)
						advise(LLOG_NOTICE,NULLCP,  "vt_text failed");
					vtsend();
				}
			}
			if(active & AO_OBJ)
			/*If potential update to Abort Output bit*/
			{
				if( (kb_image & AO_OBJ) !=
				    (AO_OBJ & *coptr->co_cmd.bool_update.value))
				/*Toggled AO bit*/
				{
					if(debug) 
						advise(LLOG_DEBUG,NULLCP,  "Toggled AO in KB");
					kb_image ^= AO_OBJ;	/*Record it*/
				}
			}
			if(active & IP_OBJ)
			/*If potential update to Interrupt Process bit*/
			{
				if( (kb_image & IP_OBJ) !=
				    (IP_OBJ & *coptr->co_cmd.bool_update.value))
				/*Toggled IP bit*/
				{
					if(debug)
						advise(LLOG_DEBUG,NULLCP,  "Toggled IP in KB");
					kb_image ^= IP_OBJ;
					kill_proc();
				}
			}
			if(active & DM_OBJ)
			{
				if( (kb_image & DM_OBJ) !=
				    (DM_OBJ & *coptr->co_cmd.bool_update.value))
				{
					/*Toggled DM BIt*/
					if(debug)
						advise(LLOG_DEBUG,NULLCP,  "Toggled DM in KB");
					kb_image ^= DM_OBJ;
				}
			}
			if(active & BRK_OBJ)
			/*If potential update to Break Bit*/
			{
				if( (kb_image & BRK_OBJ) !=
				    (BRK_OBJ & *coptr->co_cmd.bool_update.value))
				/*Toggled BREAK bit*/
				{
					if(debug)
						advise(LLOG_DEBUG,NULLCP,  "Toggled BREAK in KB");
					kb_image ^= BRK_OBJ;
					kill_proc();
				}
			}
		}
		else if( !strcmp(coptr->co_name,"NI") )
		{
			if(active & ECHO_OBJ)
			/*Update to Echo Control Object*/
			{
				if(ECHO_OBJ & *coptr->co_cmd.bool_update.value)
				/*Request from User for Remote Echo*/
				{
					ni_image |= ECHO_OBJ;
					if(showoptions)
						(void)printf("Remote Echo Update Received\n");
					if(ECHO_OBJ & nego_state) /*If now in Remote Echo*/
					{
						if(na_image & ECHO_OBJ) /*No request outstatnding*/
							advise(LLOG_NOTICE,NULLCP,  "User Request ignored--Now in Remote echo");
						else /*Must be user's response to a request*/
						{
							if(showoptions)
								(void)printf("Request for Local Echo Denied by User\n");
							na_image |= ECHO_OBJ;
						}
					}
					else	/*Else Not in Remote Echo*/
					{
						if(na_image & ECHO_OBJ) /*I Requested Remote Echo*/
						/*This must be confirmation*/
						{
							if(showoptions)
								(void)printf("User agreed to do Remote Echo\n");
						}
						else	/*Request to do Remote Echo*/
						{
							if(showoptions)
								(void)printf("User Requested Remote Echo--O.K.\n");
							na_image |= ECHO_OBJ;
							vt_set_nego(na_image,ECHO_OBJ);/*Respond "WILL"*/
						}
#ifdef BSD44
						realptyecho(1);
#else
						if (ioctl(pty,TIOCGETP,(char*)&sb) == -1) {
							perror("ioctl");
							adios(NULLCP, "ioctl failed");
						}
						sb.sg_flags |= ECHO;	/*Turn on Echo*/
						if (ioctl(pty,TIOCSETP,(char*)&sb) == -1) {
							perror("ioctl");
							adios(NULLCP, "ioctl failed");
						}
#endif
						nego_state |= ECHO_OBJ;
						cur_emode = NOT_ECHO_NOW;	/*Don't Want user to Echo*/
					}
				}
				else	/*Request from user for Local Echo*/
				{
					if(showoptions)
						(void)printf("NI--Local Echo\n");
					cur_emode = NOT_ECHO_NOW;
					ni_image &= ~ECHO_OBJ;
					if(nego_state & ECHO_OBJ) /*If now in Remote Echo*/
					{
						if(na_image & ECHO_OBJ) /*If no request pending*/
						/*Must be request from user*/
						{

#ifdef DO_LOCAL_ECHO
							if(showoptions)
								(void)printf("User requested Local Echo -- O.K.\n");

							na_image &= ~ECHO_OBJ;
							nego_state &= ~ECHO_OBJ;
#ifdef BSD44
							ptyecho(0);
#else
							setmode(0,ECHO);
#endif
#else
							na_image |= ECHO_OBJ;
							if(showoptions)
								(void)printf("User requested Local Echo -- Denied\n");
#endif

							vt_set_nego(na_image,ECHO_OBJ);	/*Respond "WILL"*/
						}
						else 
						{
							if(showoptions)
								(void)printf("Server request for Local Echo Accepted\n");
							nego_state &= ~ECHO_OBJ;
#ifdef BSD44
							ptyecho(0);
#else
							setmode(0,ECHO);
#endif
						}
					}
					else	/*Else now in Local Echo*/
					{
						if(na_image & ECHO_OBJ) /*If requeset pending*/
						/*Must be negative response*/
						{
							na_image &= ~ECHO_OBJ;
							if(showoptions)
								(void)printf("Request for Remote Echo Denied by User\n");
						}
						else /*Else no request pending*/
						{
							if(showoptions)
								(void)printf("User Request Ignored--Now in Local Echo\n");
						}
					}
				}
			}
			if(active & SUP_GA)
			/*Update to Suppress Go Ahead Control Object*/
			{
				if(SUP_GA & *coptr->co_cmd.bool_update.value)
				{
					if(showoptions)
						(void)printf("Suppress Go Ahead\n");
					ni_image |= SUP_GA;
					if((na_image & SUP_GA) == (nego_state &SUP_GA))
					/*If no request from Acceptor outstanding*/
					{
						if(!(nego_state & SUP_GA))
						/*If not currently in Supress Go Ahead*/
						{
							na_image |= SUP_GA;
							vt_set_nego(na_image,SUP_GA);/*Reply "Will"*/
						}
					}
					nego_state |= SUP_GA; /*Entering or already there*/
				}
				else
				{
					if(showoptions) 
						(void)printf("Don't Suppress Go Ahead\n");
					ni_image &= ~SUP_GA;
					if((na_image & SUP_GA) == (nego_state & SUP_GA))
					/*Must be request from Initiator*/
					{
						na_image |= SUP_GA;
						vt_set_nego(na_image,SUP_GA);/*Reply "Won't"*/
					}
					else /*Else reply to my request*/
					{
						if(showoptions)
							(void)printf("User refuses to Suppress Go Ahead\n");
						na_image &= ~SUP_GA;	/*Give up*/
					}
				}
			}
			if(active & DISP_BIN)
			/*Update to WACI Binary Repertoire*/
			{
				if(DISP_BIN & *coptr->co_cmd.bool_update.value)
				{
					if(showoptions)
						(void)printf("Initiator requested Binary Repertoire on DI\n");
					if((na_image & DISP_BIN) == (nego_state & DISP_BIN))
					/*No request outstanding from Acceptor*/
					{
						if(!(nego_state & DISP_BIN)) /*If not now binary*/
						{
							na_image |= DISP_BIN;
							vt_set_nego(na_image,DISP_BIN); /*Send "Will"*/
							switch_rep(2);
						}
					}
					else	/*Else a response to Acceptor request*/
					{
						if(na_image & KBD_BIN) /*Positive Response*/
							switch_rep(2);
					}
					nego_state |= DISP_BIN;
					na_image |= DISP_BIN;
				}
				else
				{
					if(showoptions)
						(void)printf("Initiator requested ASCII Repertoire on DI\n");
					if((na_image & DISP_BIN) == (nego_state & DISP_BIN))
					{
						if(nego_state & DISP_BIN) /*If not now ASCII*/
						{
							na_image &= ~DISP_BIN;
							vt_set_nego(na_image,DISP_BIN);
							switch_rep(1);
						}
					}
					else
					{
						if(!(na_image & KBD_BIN)) /*Positive Response*/
							switch_rep(1);
					}
					nego_state &= ~DISP_BIN;
					na_image &= ~DISP_BIN;
				}
			}
			if(active & KBD_BIN)
			/*Update to WACI Binary Repertoire*/
			{
				if(KBD_BIN & *coptr->co_cmd.bool_update.value)
				{
					if(showoptions)
						(void)printf("Initiator requested Binary Repertoire on KB\n");
					if((na_image & KBD_BIN) == (nego_state & KBD_BIN))
					/*If no Acceptor request outstanding*/
					{
						if(!(nego_state & KBD_BIN))/*If not now binary*/
						{
							na_image |= KBD_BIN;
							vt_set_nego(na_image,KBD_BIN); /*Reply "Will"*/
						}
					}
					na_image |= KBD_BIN;
					nego_state |= KBD_BIN;
				}
				else
				{
					if(showoptions)
						(void)printf("Initiator requested ASCII Repertoire on KB\n");
					if((na_image & KBD_BIN) == (nego_state & KBD_BIN))
					/*Request from Initator*/
					{
						if(nego_state & KBD_BIN) /*If not now ASCII*/
						{
							na_image &= ~KBD_BIN;
							vt_set_nego(na_image,KBD_BIN); /*Reply "Will"*/
						}
					}
					na_image &= ~KBD_BIN;
					nego_state &= ~KBD_BIN;
				}
			}
		}
	}
	if( !strcmp(coptr->co_name,"SY") ) /*SYNCHRONIZE CO can be written
							by Initiator or Acceptor*/
	{
		if(active & SYNC)
		/*Potential Update to Synch*/
		{
			if( (SYNC & *coptr->co_cmd.bool_update.value) !=
			    (SYNC & sync_image) )
			{
				advise(LLOG_NOTICE,NULLCP,  "Toggled SYNC");
				sync_image ^= SYNC;
			}
		}
	}
	if( !strcmp(coptr->co_name,"GA") )
	{
		if(active & GO_AHEAD)
		/*Potential Update to Go Ahead*/
		{
			if( (GO_AHEAD & *coptr->co_cmd.bool_update.value) !=
			    (GO_AHEAD & ga_image) )
			{
				if(debug) 
					advise(LLOG_DEBUG,NULLCP,  "Toggled Go Ahead");
				ga_image  ^= GO_AHEAD;
			}
		}
	}

}

attrib_hdlr(doptr)	/*Handle Write Attribute Display Object Update*/
DO_UPDATE *doptr;
{


	if(doptr->do_cmd.wrt_attrib.attr_id == 0)
	/*If switching repertoires*/
	{
		if(doptr->do_cmd.wrt_attrib.attr_ext == 2)
		/*If Modal extent*/
		{
			if(doptr->do_cmd.wrt_attrib.attr_val == 1)
			{
			    if(showoptions)
				if(my_right == INITIATOR)
					(void)printf("Switching to ASCII Repertoire\r\n");
			    transparent = 0;
			}
			else if(doptr->do_cmd.wrt_attrib.attr_val == 2)
			{
			    if(showoptions)
				if(my_right == INITIATOR)
					(void)printf("Switching to Transparent profile.\r\n");
			    transparent = 1;
			}
			else (void)printf("Attribute for unavailable repertoire\n");
		}
		else (void)printf("Attribute update with invalid extent (%d)\n",
		    doptr->do_cmd.wrt_attrib.attr_ext);
	}
	else 
		advise(LLOG_NOTICE,NULLCP,  "Attribute Update with invalid I.D. (%d)\n", doptr->do_cmd.wrt_attrib.attr_id);
}

/*    TTY */

#ifdef BSD44
extern struct	termios oterm;

int
tmode(f)
	int f;
{
	static int prevmode = 0;
	struct termios term;
	int onoff, old;

	if (prevmode == f)
		return (f);
	old = prevmode;
	prevmode = f;
	term = oterm;
	switch (f) {
	case 0:
		onoff = 0;
		break;
	case 1:
	case 2:
		onoff = 1;
		if (f == 1)
		{
			term.c_lflag &= ~ECHO;
			term.c_oflag &= ~OPOST;
		}
		else
		{
			term.c_lflag |= ECHO;
			term.c_oflag |= OPOST;
		}
		term.c_lflag &= ~(IEXTEN|ISIG|ICANON);
		break;
	default:
		return old;
	}
	if (tcsetattr(fileno(stdin), TCSAFLUSH, &term) == -1)
		perror("tcsetattr");
	if (ioctl(fileno(stdin), FIONBIO, (char*)&onoff) == -1) {
		perror("ioctl");
	}
	return (old);
}

#else

extern struct	tchars otc;
extern struct	ltchars oltc;
extern struct	sgttyb ottyb;

/* struct	tchars notc =	{ -1, 3, -1, -1, -1, -1 };*/
struct	tchars notc =	{ 
	-1, -1, -1, -1, -1, -1 };
struct	ltchars noltc =	{ 
	-1, -1, -1, -1, -1, -1 };

int
tmode(f)
register int f;
{
	static int prevmode = 0;
	struct tchars *tc;
	struct ltchars *ltc;
	struct sgttyb sb;
	int onoff, old;

	if (prevmode == f)
		return (f);
	old = prevmode;
	prevmode = f;
	sb = ottyb;
	switch (f) {

	case 0:
		onoff = 0;
		tc = &otc;
		ltc = &oltc;
		break;

	case 1:
	case 2:
		if (f == 1)
		{
			sb.sg_flags |= CBREAK;
			sb.sg_flags &= ~(ECHO|CRMOD);
			sb.sg_erase = sb.sg_kill = -1;
		}
		else
		{
			sb.sg_flags &= CBREAK;
			sb.sg_flags |= ECHO|CRMOD;
		}
		tc = &notc;
		notc.t_stopc = otc.t_stopc;
		notc.t_startc = otc.t_startc;
		ltc = &noltc;
		onoff = 1;
		break;

	default:
		return old;
	}
	if (ioctl(fileno(stdin), TIOCSLTC, (char *)ltc) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed");
	}
	if (ioctl(fileno(stdin), TIOCSETC, (char *)tc) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed");
	}
	if (ioctl(fileno(stdin), TIOCSETP, (char *)&sb) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed");
	}
	if (ioctl(fileno(stdin), FIONBIO, (char*)&onoff) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed");
	}
	if (ioctl(fileno(stdout), FIONBIO, (char*)&onoff) == -1) {
		perror("ioctl");
		adios(NULLCP, "ioctl failed");
	}
	return (old);
}
#endif

kill_proc()	/*Terminate current UNIX process using UNIX interrupt char*/
{
#ifdef BSD44
	struct termios term;

	if (tcgetattr(pty, &term) == -1)
		perror("tcgetattr");
	else if (term.c_cc[VINTR] != _POSIX_VDISABLE)
		(void) putch(term.c_cc[VINTR]);
#else
	if(ioctl(pty,TIOCGETC,(char *)&otc) == -1)
	{
		perror("ioctl");
		adios(NULLCP, "ioctl failed");
	}
	(void) putch(otc.t_intrc);
#endif
}

def_echo(coptr)	/*Handle Default Profile Echo Ctrl Object*/
CO_UPDATE *coptr;
{

	char active = 0;

	if(coptr->co_cmd.bool_update.mask_count == 0) active = 0xff;
	else active = *coptr->co_cmd.bool_update.mask;

	if (active & ECHO_OBJ) {
	    if(*coptr->co_cmd.bool_update.value & ECHO_OBJ)
		/*True means do local echo*/
		(void) tmode(2);
	    else
		(void) tmode(1);
	}
} 
#ifdef BSD44
static realptyecho(on)
{
	struct termios term;

	if (tcgetattr(pty, &term) == -1) {
		perror("tcgetattr");
		return;
	}
	if (on)
		term.c_lflag |= ECHO;
	else
		term.c_lflag &= ECHO;
	if (tcsetattr(pty, TCSAFLUSH, &term) == -1) {
		perror("tcsetattr");
		return;
	}
}
#endif
