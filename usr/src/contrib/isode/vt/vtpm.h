/* vtpm.h - VTPM: definitions */

/* 
 * $Header: /f/osi/vt/RCS/vtpm.h,v 7.1 91/02/22 09:48:32 mrose Interim $
 *
 *
 * $Log:	vtpm.h,v $
 * Revision 7.1  91/02/22  09:48:32  mrose
 * Interim 6.8
 * 
 * Revision 7.0  89/11/23  22:31:58  mrose
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


#include <errno.h>
#include <stdio.h>

#include "acsap.h"		/* definitions for AcS-USERs */
#include "logger.h"

/* Make sure this is here in new versions */
/*#include "sector1.h"*/


#define	TRUE			1
#define	FALSE			0

#define	ERR			-1
#define	E_ERROR			-1
#define	E_EOF			-2

#define E_NOEVENT 		1 
#define E_READ			2 

#define INPUT			1
#define OUTPUT			2

#define DEFINED			1
#define UNDEFINED		0

#define BASIC			1

#define WACI_WACA		0
#define WAVAR			1

#define	FAILURE			0
#define	SUCCESS			1
#define	WARNING			2

#define	ECHO_NOW		0
#define	NOT_ECHO_NOW	1

#define DISPLAY			0
#define	CONTROL			2

#define	SEQUENCED		1
#define	NONSEQUENCED	2

#define COLL_DET		1
#define PROFILE_NG		4

#define	TEXT			5

#define WOULDBLOCK		-3

/*  type identifiers  */
#define G_COLLISION_WINNER		1
#define	G_PARM_VLIST			6
#define	G_RESULT3 				11

/*  Incoming Events -- Table 25, ISO 9041           */

#define	VASSreq			1	/*VT-ASSOCIATE request*/
#define	VASSrsp			2	/*VT-ASSOCIATE response*/
#define VBRKreq			3	/*VT-BREAK request*/
#define VBRKrsp			4	/*VT-BREAK response*/
#define	VDACKreq		5	/*VT-ACK-RECEIPT request*/
#define	VDATreq_u		6	/*VT-DATA request urgent priority*/
#define	VDATreq_h		7	/*VT-DATA request high priority*/
#define	VDATreq_n		8	/*VT-DATA request normal*/
#define VDATreq_sqtr		9	/*VT-DATA request trigger control*/
#define VDELreq			10	/*VT-DELIVER request*/
#define	VENEGreq		11	/*VT-END-NEG request*/
#define	VENEGrsp		12	/*VT-END-NEG response*/
#define	VGVTreq			13	/*VT-GIVE-TOKENS request*/
#define VNACCreq		14	/*VT-NEG-ACCEPT (negotiation accept)*/
#define	VNINVreq		15	/*Negotiation Invitaion*/
#define	VNOFFreq		16	/*Negotiation Offer*/
#define	VNREJreq		17	/*Negotiation Reject*/
#define	VRELreq			18	/*VT-RELEASE request*/
#define	VRELrsp			19	/*VT-RELEASE response*/
#define	VRQTreq			20	/*VT-REQUEST-TOKENS request*/
#define	VSNEGreq		21	/*Start Negotiation request*/
#define	VSNEGrsp		22	/*Start Negotiation response*/
#define	VSWPreq			23	/*Switch Profile request*/
#define	VSWPrsp			24	/*Switch Profile response*/
#define	VUABreq			25	/*VT-U-ABORT request*/
#define	APQ			26	/*VT-P-ABORT request*/
#define ASQ			27	/*Associate Request*/
#define	ASR			28	/*Associate Response*/
#define	AUQ			29	/*VT-U-ABORT request*/
#define BKQ			30	/*Break Request*/
#define BKR			31	/*Break Response*/
#define	DAQ			32	/*VT-ACK-RECEIPT*/
#define	DLQ			33	/*VT-DELIVER*/
#define HDQ			34	/*VT-EXPEDITED-DATA*/
#define	ENQ			35	/*End Negotiation Request*/
#define	ENR			36	/*End Negotiation Response*/
#define	GTQ			37	/*VT-GIVE-TOKEN*/
#define	NAQ			38	/*Negotiation Accept Request*/
#define	NDQ_ntr			39	/*VT-DATA -- No Trigger Update*/
#define	NDQ_tr			40	/*VT-DATA -- Trigger Update*/
#define	NJQ			41	/*Negotiation Reject Request*/
#define	NIQ			42	/*Negotiation Invite Request*/
#define	NOQ			43	/*Negotiation Offer Request*/
#define	RLQ			44	/*VT-RELEASE-REQ*/
#define	RLR			45	/*VT-RELEASE-RESP*/
#define	RTQ			46	/*VT-REQUEST-TOKEN*/
#define	SPQ			47	/*VT-SWITCH-PROFILE-REQ*/
#define	SPR			48	/*VT-SWITCH-PROFILE-RESP*/
#define	SNQ			49	/*Start Negotiation Request*/
#define	SNR			50	/*Start Negotiation Response*/
#define	UDQ			51	/*VT-UNCONTROLLED-DATA*/
#define	AUTO			52	/*Internal event not defined by VTP*/
#define	VTAB			53	/*Irrecoverable exception condition*/
#define	PAB			54	/*Failure Indication*/

/*  Outgoing Events -- Table 27, ISO 9041	*/

#define	VACKind			55	/*VT-ACK-RECEIPT Indication*/
#define	VASSind			56	/*VT-ASSOCIATE Indication*/
#define	VASScnf			57	/*VT-ASSOCIATE Confirm*/
#define VBRKind			58	/*VT-BREAK Indication*/
#define VBRKcnf			59	/*VT-BREAK Confirm*/
#define	VDATind_u		60	/*VT-DATA indication -- urgent object*/
#define	VDATind_h		61	/*VT-DATA indication -- high object*/
#define VDATind_n		62	/*VT-DATA indication -- normal object*/
#define VDATind_Vnt		63	/*Sequence of Vnt VT-DATA normal*/
#define	VDELind			64	/*VT-DELIVER indication*/
#define	VENEGind		65	/*VT-END-NEG indication*/
#define	VENEGcnf		66	/*VT-END-NEG confirm*/
#define	VGVTind			67	/*VT-GIVE-TOKENS indication*/
#define	VNINVind		68	/*VT-NEG-INVITE indication*/
#define	VNOFFind		69	/*VT-NEG-OFFER indication*/
#define	VNACCind		70	/*VT-NEG-ACCEPT indication*/
#define	VNREJind		71	/*VT-NEG-REJECT indication*/
#define	VPABind			72	/*VT-P-ABORT indication*/
#define	VRELind			73	/*VT-RELEASE indication*/
#define	VRELcnf			74	/*VT-RELEASE confirm*/
#define	VRQTind			75	/*VT-REQUEST-TOKENS indication*/
#define	VSNEGind		76	/*VT-START-NEG indication*/
#define	VSNEGcnf		77	/*VT-START-NEG confirm*/
#define	VSWPind			78	/*VT-SWITCH-PROFILE indication*/
#define	VSWPcnf			79	/*VT-SWITCH-PROFILE confirm*/
#define	VUABind			80	/*VT-U-ABORT indication*/
#define NDQseq_ntr		81	/*Sequence of NDQ-ntr to xmit updates*/
#define NDQseq_tr		82	/*1 NDQ-tr preceeded by >=1 NDQ-ntr*/
#define NDQ_x_tr		83
#define NDQ_x_ntr		84



/* Sector 1 States */

#define S1_01			0	/*No Association*/
#define S1_02B			1	/*Associate -- Awaiting target*/
#define S1_02S			2	/*Associate -- Awaiting target*/
#define S1_03B			3	/*Associate -- Awaiting User*/
#define S1_03S			4	/*Associate -- Awaiting User*/
#define S1_10B			5	/*Environment Not Agreed*/
#define S1_10N			6
#define S1_10T			7
#define S1_50B			8
#define S1_51Q			9	/*Release -- Awaiting Peer*/
#define S1_51R			10	/*Release -- Awaiting User*/
#define S1_51N			11	/*Release -- Awaiting User*/
#define S1_51T			12	/*Release -- Awaiting Peer*/


/* Sector 5 States */

#define S5_400B			0	/*Data Transfer*/
#define S5_402B			1	/*Data Xfer -- Awaiting Ack from user*/
#define S5_420B			2	/*Data Xfer -- Awaiting Ack from peer*/
#define S5_422B			3	/*Data xfer -- Awaiting Ack (Both)*/
#define S5_40N			4	/*Data Transfer*/
#define S5_40T			5	/*Data Transfer*/
#define S5_42T			6	/*Data Xfer -- Awaiting Ack from peer*/
#define S5_42N			7	/*Data Xfer -- Awaiting Ack from user*/
#define S5_61			8	/*Break Request rcv'd from User*/
#define S5_62			9	/*Break Request rcv'd from Peer*/


#define	INITIATOR		0
#define	ACCEPTOR		1
#define	ACHOICE			2



/*  PDU Types  (Table 4, ISO 9041) */

#define	ASQ_PDU		0
#define	ASR_PDU		1
#define RLR_PDU		2
#define AUQ_PDU		3
#define APQ_PDU		4
#define HDQ_PDU		5
#define NDQ_PDU		6
#define UDQ_PDU		7
#define BKQ_PDU		8
#define BKR_PDU		9
#define DLQ_PDU		10
#define DAQ_PDU		11
#define SPQ_PDU		12
#define SPR_PDU		13
#define SNQ_PDU		14
#define SNR_PDU		15
#define ENQ_PDU		16
#define ENR_PDU		17
#define NIQ_PDU		18
#define NOQ_PDU		19
#define NAQ_PDU		20
#define NJQ_PDU		21

#define PEPYPARM int *

PE	pre, pwe;
PE	mkdeliver(); 
int	fd,
	readfds,
	writefds,
	exfds,
	sd,
	connected,
	netfile;

char	*dprofile, *cprofile;
char	*myname, ttyname();
extern PE	p_ondq;
extern LLog _vt_log, *vt_log;


extern int errno;
extern unsigned	state, sector;

int		vns,
		allpmde, /* all draft VTE parameters defined */
		allpmdu, /* all draft VTE parameters defined or undefined */
		cnw,	 /* collision winner right assigned to this VTPM  */
		dcno,	 /* no delivery control */	
		dcqua,	 /* quarantine delivery control */
		dcsim,	 /* simple delivery control     */
		pmacc,	 /* parameter values acceptable */
		dr_pm_st,/* draft parameter status:
						DEFINED,
						UNDEFINED,
						OFI,  (offered, initiator)
						OFR,  (offered, responder)
						COFI, (counter-offered, initiator)
						COFR, (counter-offered, responder)
						INVI, (invited, initiator)
						INVR  (invited, responder)
				*/
		pracc,
		vtpma,
		vcwa,	 /* whether the collision winner right is owned */
		vena,	 /* agreement on current VTE */
		vnt,	 /* number of VT service data units held for local delivery */
		vns,	 /* number of VT service data units held for transmission */
		vacc,	/*action choice: switch, restore, or not specified*/
		vacp,	/*action proposal: switch, restore, or responder
			  choice*/
		vrea,	/*failure reason: collision detected or profile not
			  supplied*/
		vrjo,	/*rejection option: retain, discard, or responder
			  choice*/
		vrsl,	/*result: succes, failure, or success with warning*/
		vsmd,	/*1 if S-Mode, 0 if A-Mode*/
		vtok,	/*1 if tokens held, 0 otherwise*/
		vtkp,	/*token parameters*/
		waca,	/*WACA right*/
		vra,	/*Boolean recording of acknowledgement request*/
		G_Func_Units,	/*Bit map of Functional Units requested*/
		wavar,		/* boolean, this VTPM has the token */
		waci,		/* boolean, this VTPM is assigned the waci right */
		del_cont;	/* type of delivery control		*/


/* 
	profile is name of the profile. This is also used for the draft profile.
*/
	
int	s1_01(),
	s1_02B(),
	s1_02S(),
	s1_03B(),
	s1_03S(),
	s1_10B(),
	s1_10N(),
	s1_10T(),
	s1_50B(),
	s1_51Q(),
	s1_51R(),
	s1_51N(),
	s1_51T();

int 	s5_400B(),
	s5_402B(),
	s5_420B(),
	s5_422B(),
	s5_40N(),
	s5_40T(),
	s5_42N(),
	s5_42T(),
	s5_61(),
	s5_62();


extern struct SSAPref sfs;
extern struct SSAPref *sf;
extern struct PSAPaddr *pa;
extern struct AcSAPconnect accs;
extern struct AcSAPconnect   *acc;
extern struct AcSAPrelease acrs;
extern struct AcSAPrelease   *acr;
extern struct AcSAPindication  acis;
extern struct AcSAPindication *aci;
extern struct AcSAPabort *aca;
extern AEI	aei;
extern OID	ctx,
		pci;

extern struct AcSAPstart   acss;
extern struct AcSAPstart *acs;
extern struct PSAPstart *ps;
extern struct PSAPindication pi;
extern struct PSAPdata	px;
extern struct PSAPfinish *pf;

void	finalbye ();
void	adios (), advise ();
void	acs_adios (), acs_advise ();
void	ps_adios (), ps_advise ();

int connected;	/*TEMP -- for sector 1 testing only -- will be supplied by VTP*/

char	*malloc();
