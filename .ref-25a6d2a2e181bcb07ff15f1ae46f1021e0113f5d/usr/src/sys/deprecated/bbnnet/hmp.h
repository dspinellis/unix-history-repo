/* 
 * $Header: hmp.h,v 1.1 85/04/08 15:27:33 craig Exp $ 
 * 
 * $Log:	hmp.h,v $
 * Revision 1.1  85/04/08  15:27:33  craig
 * Initial revision
 *  
 * 
*/

/**************************************************************************/
/*  sockaddr_in with additional fields for hmp                            */
/**************************************************************************/

struct sockaddr_hmp {
	short	sin_family;	/* standard INET stuff */
	u_short	sin_port;
	struct	in_addr sin_addr;
	u_char  sih_systype;	/* HMP specific fields */
	u_char	sih_msgtype;
	u_char  sih_options;
	u_char	sih_ctlflgs;
	u_short sih_seqno;
	u_short sih_passwd;
};

#define sih_rseqno   sih_passwd

#define HM_MOREBIT	0x2	/* sih_ctlflgs: remote host has more data */

#define HM_TRAPOPT	0x1	/* sih_options: receive traps */
#define HM_SEQOPT	0x2	/* sih_options: enforce sequencing */
#define HM_NUMOPT	0x3	/* sih_options: use remote seqnum */

#ifdef KERNEL
#define HM_CONNOPTS (HM_NUMOPT)
#define HM_BINDOPTS (HM_TRAPOPT|HM_SEQOPT)
#endif


/**************************************************************************/
/*                       SYSTEM TYPES                                     */
/**************************************************************************/


#define HM_MONHOST	1
#define HM_IMP		2
#define HM_TAC		3
#define HM_GWY		4
#define HM_SIMP		5
#define HM_HOST		6	/* BBN 4.1BSD HOSTS */
#define HM_RESERVED	7
#define HM_TIU		9
#define HM_FEP		10
#define HM_CRHOST	11	/* CRONUS HOST */
#define HM_CRMCS	12	/* CRONUS MON & CTL STA */
#define HM_42HOST	7
#define HM_43HOST	13	/* unapproved # */

/**************************************************************************/
/*                         MESSAGE TYPES                                  */
/**************************************************************************/

#define HM_TRAP		1
#define HM_STATUS	2
#define HM_THRUPUT	3
#define HM_HTM		4
#define HM_PARAM	5
#define HM_ROUTE	6
#define HM_CALLACCT	7

#define HM_POLL		100
#define HM_ERROR	101
#define HM_CTLACK	102

/**************************************************************************/
/*                       ERROR TYPES                                      */
/**************************************************************************/


#define HM_EUNSPEC	1	/* reason unspecified */
#define HM_ERTYPE	2	/* bad r-message type */
#define HM_ERSUB	3	/* bad r-sub type */
#define HM_EUPARAM	4	/* unknown param */
#define HM_EIPARAM	5	/* invalid param */
#define HM_EFORMAT	6	/* invalid paramter/value format */
#define HM_ELOADER	7	/* machine in loader */

/**************************************************************************/
/*                      SPECIAL PORTS                                     */
/**************************************************************************/

#define HM_MAXPORTS 0xff

#define HOST_PORT  1		/* where polls to this host go */

/**************************************************************************/
/*			OPTIONS                                           */
/**************************************************************************/

#define SOI_MONHOST	0x1	/* HMP -- set/get monioring hosts */

#define MAX_MONHOSTS	2	/* how many hosts monitor us at once */

