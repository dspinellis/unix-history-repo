/*
  $Log:	rdp_fsm.c,v $
 * Revision 2.1  84/11/02  10:12:44  walsh
 * Fixed to include RCS comments in checked out source.
 * 
 *
 * description:
 * Transition table for RDP finite state machine.
 * 
 * revision 1.5        
 * date: 84/07/22 19:44:39;  author: walsh;  state: Exp;  lines added/del: 2/1
 * Added a state transition function rdp_closew_rcv() to compensate for
 * socket code's dropping of system priority level for a brief period of time.
 * 
 * revision 1.4        
 * date: 84/07/18 18:50:34;  author: walsh;  state: Exp;  lines added/del: 2/1
 * Added provision for sending of NULL messages.  These are sent on an idle
 * connection to determine that the other side still exists.
 * 
 * revision 1.3        
 * date: 84/07/09 14:17:34;  author: walsh;  state: Exp;  lines added/del: 2/1
 * Added ACK-delay timer to debugging printf arrays.
 * 
 * revision 1.2        
 * date: 84/07/06 09:49:07;  author: root;  state: Exp;  lines added/del: 1/1
 * This version seems to run bug-free.
 * 
 * revision 1.1        
 * date: 84/06/26 14:16:59;  author: walsh;  state: Exp;  
 * Initial revision
 */


#ifdef RDP
#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/kernel.h"
#include "../h/inode.h"
#include "../h/mbuf.h"
#include "../h/socket.h"
#include "../h/socketvar.h"

#include "../net/if.h"
#include "../net/route.h"

#include "../bbnnet/in.h"
#include "../bbnnet/in_var.h"
#include "../bbnnet/net.h"
#include "../bbnnet/in_pcb.h"
#include "../bbnnet/ip.h"
#include "../bbnnet/rdp.h"

#ifdef RDPDEBUG
char *rdpstates[RDP_NSTATES] = 
{
    "RDP_sSAME",
	"RDP_sUNOPENED",
	"RDP_sLISTEN",
	"RDP_sSYNSENT",
	"RDP_sLSYNRCVD",
	"RDP_sSYNRCVD",
	"RDP_sESTAB",
	"RDP_sCLOSEWAIT",
	"RDP_sCLOSED"
} ;

char *rdpinputs[RDP_NINPUTS] = 
{
    "RDP_iCONNECT",
	"RDP_iLISTEN",
	"RDP_iNETR",
	"RDP_iUCLOSE",
	"RDP_iTIMER",
	"RDP_iRCV",
	"RDP_iSEND"
} ;

char *rdptimers[RDP_NTIMERS] = 
{
    "RDP_tCLOSEWAIT",
	"RDP_tRTTL",
	"RDP_tRXMIT",
	"RDP_tACKDELAY",
	"RDP_tNULL"
} ;
#endif

#ifdef KERNEL
extern int	rdp_unop_connect();	/* RDP_sUNOPENED x RDP_iCONNECT */
extern int	rdp_unop_listen();	/* RDP_sUNOPENED x RDP_iLISTEN */
extern int	rdp_unop_netr();	/* RDP_sUNOPENED x RDP_iNETR */
extern int	rdp_unop_close();	/* RDP_sUNOPENED x RDP_iUCLOSE */

extern int	rdp_lis_listen();	/* RDP_sLISTEN x RDP_iLISTEN */
extern int	rdp_lis_netr();	/* RDP_sLISTEN x RDP_iNETR */
extern int	rdp_lis_close();	/* RDP_sLISTEN x RDP_iCLOSE */

extern int	rdp_synsent_netr();	/* RDP_sSYNSENT x RDP_iNETR */
extern int	rdp_synsent_close();	/* RDP_sSYNSENT x RDP_iUCLOSE */
extern int	rdp_synsent_timer();	/* RDP_sSYNSENT x RDP_iTIMER */

extern int	rdp_lsynrcvd_netr();	/* RDP_sLSYNRCVD x RDP_iNETR */
extern int	rdp_lsynrcvd_close();	/* RDP_sLSYNRCVD x RDP_iUCLOSE */
extern int	rdp_lsynrcvd_timer();	/* RDP_sLSYNRCVD x RDP_iTIMER */

extern int	rdp_synrcvd_netr();	/* RDP_sSYNRCVD x RDP_iNETR */
extern int	rdp_synrcvd_close();	/* RDP_sSYNRCVD x RDP_iUCLOSE */
extern int	rdp_synrcvd_timer();	/* RDP_sSYNRCVD x RDP_iTIMER */

extern int	rdp_estab_netr();	/* RDP_sESTAB x RDP_iNETR */
extern int	rdp_estab_close();	/* RDP_sESTAB x RDP_iUCLOSE */
extern int	rdp_estab_timer();	/* RDP_sESTAB x RDP_iTIMER */
extern int	rdp_estab_rcv();	/* RDP_sESTAB x RDP_iRCV */
extern int	rdp_estab_send();	/* RDP_sESTAB x RDP_iSEND */

extern int	rdp_closew_netr();	/* RDP_sCLOSEWAIT x RDP_iNETR */
extern int	rdp_closew_close();	/* RDP_sCLOSEWAIT x RDP_iUCLOSE */
extern int	rdp_closew_timer();	/* RDP_sCLOSEWAIT x RDP_iTIMER */
extern int	rdp_closew_rcv();	/* RDP_sCLOSEWAIT x RDP_iRCV */

#define illegal	0

int (*rdp_action_table[RDP_NSTATES][RDP_NINPUTS])() = 
{
    {
	illegal,		/* to avoid off-by-1 error because SAME is 0 */
	illegal,
	illegal,
	illegal,
	illegal,
	illegal,
	illegal
    }
    ,

    {
	rdp_unop_connect,
	rdp_unop_listen,
	rdp_unop_netr,
	rdp_unop_close,
	illegal,
	illegal,
	illegal
    }
    ,

    {
	illegal,
	rdp_lis_listen,
	rdp_lis_netr,
	rdp_lis_close,
	illegal,
	illegal,
	illegal
    }
    ,

    {
	illegal,
	illegal,
	rdp_synsent_netr,
	rdp_synsent_close,
	rdp_synsent_timer,
	illegal,
	illegal
    }
    ,

    {
	illegal,
	illegal,
	rdp_lsynrcvd_netr,
	rdp_lsynrcvd_close,
	rdp_lsynrcvd_timer,
	illegal,
	illegal
    }
    ,

    {
	illegal,
	illegal,
	rdp_synrcvd_netr,
	rdp_synrcvd_close,
	rdp_synrcvd_timer,
	illegal,
	illegal
    }
    ,

    {
	illegal,
	illegal,
	rdp_estab_netr,
	rdp_estab_close,
	rdp_estab_timer,
	rdp_estab_rcv,
	rdp_estab_send
    }
    ,

    {
	illegal,
	illegal,
	rdp_closew_netr,
	rdp_closew_close,
	rdp_closew_timer,
	rdp_closew_rcv,
	illegal
    }
} ;
#endif
#endif
