/* connection.h - directory internal structures */

/*
 * $Header: /f/osi/h/quipu/RCS/connection.h,v 7.3 91/02/22 09:25:34 mrose Interim $
 *
 *
 * $Log:	connection.h,v $
 * Revision 7.3  91/02/22  09:25:34  mrose
 * Interim 6.8
 * 
 * Revision 7.2  90/10/17  11:46:19  mrose
 * sync
 * 
 * Revision 7.1  90/07/09  14:38:23  mrose
 * sync
 * 
 * Revision 7.0  89/11/23  21:56:28  mrose
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


#ifndef QUIPUCON
#define QUIPUCON

#include "quipu/common.h"
#include "quipu/dsargument.h"
#include "quipu/dsap.h"

#define DSA_PRIO_LOW            1
#define DSA_PRIO_MED            2
#define DSA_PRIO_HIGH           3
#define DSA_MAX_PRIO            3

extern int max_conns;
#define MAX_CONNS		max_conns	/* Maximum concurrent connections */
#define CONNS_RESERVED_DI	2	/* Reserved for Get DSA Info ops */
#define CONNS_RESERVED_X500	10	/* Reserved for normal operations */

/*
*  If the connection definitions are altered bear in mind that 
*  MAX_CONNS - CONNS_RESERVED_FOR_DI - CONNS_RESERVED_FOR_X500
*  must be at least 1 to allow get edb ops to happen, and should
*  probably be at least 2 to allow one get edb operation to block.
*/

/*
*  The following structures form the basis of the connection mesh
*  which is central to the scheduling of the DSA.
*/

/*
*  The di_block structure is used to generate lists of dsa information
*  blocks even when the information is not yet available.
*  There are 3 states which a di_block can be in:
*	DI_ACCESSPOINT:	di_block contains access point only.
*	DI_COMPLETE:	di_block contains entry for dsa.
*	DI_DEFERRED:	di_block is waiting for a dsa entry.
*
*  The links in this structure need to be handled with care!!
*  di_next is used to link the "di_block"s on
*	1) the global list if that is where this block is kept
*	2) the requesting operation if that is where this block is kept
*	3) the requesting task if that is where this block is kept
*
*  di_wake_next is used to link the "di_block"s for the performing
*  operation, so that it is obvious what needs to be alerted.
*
*  di_task points to the task to be alerted from type DI_TASK
*  di_oper points to the operation to be alerted from type DI_OPERATION
*  di_perform points to the operation performing the get dsa info operation
*/
struct di_block
{
	DN	  di_dn;	/* Name of dsa this block refers to */

	char	  di_type;	/* Global list, operation list or task list */
#define DI_GLOBAL	1	/* deferred_dis lists this block */
#define DI_OPERATION	2	/* di_oper lists this block */
#define DI_TASK		3	/* di_task lists this block */

	struct task_act		* di_task;
	struct oper_act		* di_oper;

/*
*  The following are needed to generate chaining arguments from DSAInformation
*  in the case of chaining (DI_OPERATION); and to generate continuation
*  references in the case of referring (DI_TASK). Not present for DI_GLOBAL
*/
	DN			  di_target;
	int			  di_reftype;
	int			  di_rdn_resolved;
	int			  di_aliasedRDNs;

	char	  di_state;	/* How the dsa information is formed */
#define DI_COMPLETE	1	/* di_entry filled out */
#define DI_ACCESSPOINT	2	/* di_accesspoints filled out */
#define DI_DEFERRED	3	/* di_perform still generating di_entry */

	struct access_point	* di_accesspoints;

	Entry			  di_entry;

	struct oper_act		* di_perform;

	struct di_block		* di_wake_next;	/* List of blocks to wake */
	struct di_block		* di_next;	/* List of blocks */
};
#define NULL_DI_BLOCK	((struct di_block *) NULL)

/*
* Operations received over a bound association are represented as a
* linked list of task blocks. The following task block structure 
* contains the invocation (including decoded argument) received,
* representations of the progress of the task and fields to be
* used for generating the response.
*/
struct task_act
{
	int			  tk_prio;
	char			  tk_state;
#define TK_ACTIVE		  1	/* Task ready to have work done */
#define TK_PASSIVE		  2	/* Task waiting for operation */
#define TK_SUSPEND		  3	/* Giving the network a chance */

	struct DSAPinvoke	  tk_dx;
	struct di_block		* tk_dsas;	/* di_blocks for referral */

	struct DSAPindication	  tk_resp;
	struct ds_op_res	* tk_result;
	struct DSError		* tk_error;

	/* Specific additions to provide multi-subtask search implementation */
	struct ds_search_task	* local_st;
	struct ds_search_task	* refer_st;
	struct ds_search_task	* referred_st;

	char			  tk_timed;
	time_t			  tk_timeout;

	struct oper_act		* tk_operlist;
	struct task_act		* tk_next;

	struct connection	* tk_conn;
};
#define NULLTASK ((struct task_act *) NULL)

struct oper_act
{
	int                       on_id;
	char                      on_state;
#define ON_DEFERRED		  1	/* Waiting for DSA INFO */
#define ON_CHAINED		  2	/* Waiting for a response */
#define ON_COMPLETE		  3	/* Waiting to be used by task */
#define ON_ABANDONED		  4	/* Waiting for response - no task */

	char                      on_type;
#define ON_TYPE_X500		  1
#define ON_TYPE_BIND_COMPARE	  2
#define ON_TYPE_GET_DSA_INFO	  3
#define ON_TYPE_GET_EDB		  4
#define ON_TYPE_SUBTASK		  5
#define ON_TYPE_SHADOW		  6

/* Specific to ON_TYPE_X500 */
	struct task_act         *on_task;		/* Task to wake */

/* Specific to ON_TYPE_BIND_COMPARE */
	struct connection	*on_bind_compare;	/* Bind to wake */

/* Specific to ON_TYPE_GET_DSA_INFO */
	struct di_block		* on_wake_list;		/* di_blocks to wake */

/* Specific to ON_TYPE_GET_EDB */
	Entry			  on_getedb_eptr;	/* previous entry */
	char			* on_getedb_ver;	/* previous version */

/* Specific to ON_TYPE_SUBTASK */
	struct ds_search_task	* on_subtask;

	struct di_block		* on_dsas;	/* DSAInfos for chaining */

	struct ds_op_arg        on_req;		/* Argument stuff */
	struct ds_op_arg	*on_arg;

	struct DSAPindication	  on_resp;	/* Response stuff */

	char                    on_relay;       /* if TRUE try Relay DSA */

	struct oper_act         *on_next_conn;
	struct oper_act         *on_next_task;

	struct connection       *on_conn;
};
#define NULLOPER ((struct oper_act *) NULL)

struct conn_start
{
	/* Values stored after call to TNetAccept */
	int			  cs_vecp;
	char 			* cs_vec[4];
	char			* cs_svec[4];

	/* Value extracted from above by conn_init() */
	struct DSAPstart          cs_ds;

	/* Operation carrying out compare for binding */
	struct oper_act 	* cs_bind_compare;

	/* Result or error generated for response */
	struct ds_bind_arg	  cs_res;
	struct ds_bind_error	  cs_err;
};

struct conn_connect
{
	/* Bind argument used in conn_request() */
	struct ds_bind_arg              cc_req;

	struct DSAPconnect		cc_dc;
};

/*
* Conn is the structure used to represent external connections
*/
struct connection
{
	char			  cn_state;
/* State of the connection */
#define CN_INDICATED		  1
#define CN_WAITING		  2
#define CN_CONNECTING1		  3
#define CN_CONNECTING2		  4
#define CN_OPEN			  5
#define CN_FAILED		  6
#define CN_CLOSING		  7
#define CN_OPENING		  8

	char			  cn_ctx; 
/* DS_CTX_* values defined in dsap.h for use by decoders */

	char			  cn_initiator;
	/* TRUE: this DSA is initiator, FALSE: this DSA is responder */

	/* Information used during initialisation of the connection */
        union
        {
		struct conn_start	cn_start_un_start;	/* responder */
		struct conn_connect	cn_start_un_connect;	/* initiator */
	} cn_start_un;
#define cn_start	cn_start_un.cn_start_un_start
#define cn_connect	cn_start_un.cn_start_un_connect

/*
	struct ds_bind_arg	* cn_res;
	struct ds_bind_error		* cn_err;

	struct init_activity	  cn_init_act;
	struct oper_act 	* cn_bind_compare;
*/

        time_t			  cn_last_used;
/* Time at which this connection was last used */

        time_t			  cn_last_release;
/* Time at which this connection release was last attempted */

	DN			  cn_dn;
/* Name of the entity at the far end of the connection */

	struct PSAPaddr           cn_addr;
/* Address of the entity at the far end of the connection */

	int                       cn_ad;
/* Descriptor identifying the association on which the connection is based */

/*
	int			  cn_context_id;
*/
/* Context identifier of context to be used for user-data. */

	int                       cn_op_id;
/* Used to ensure unique invoke id's are used when invoking operations. */

	struct task_act		* cn_tasklist;
/* List of tasks received over this connection. */

	struct oper_act		* cn_operlist;
/* List of operations sent over this connection. */

	struct connection       * cn_next;
/* Rest of list of connections. */

	int 			cn_authen;
/* Takes a value from bind.h -> level to which the association is authenticated */

};
#define NULLCONN ((struct connection *) NULL)

/*
*  Global variables are nasty but useful. Here the external definitions
*  for the most crucial are given:
*	a handle on the connections being processed
*	info describing how this DSA wants the lower layers parameterised
*	a handle on current deferred get dsa info operations
*	a handle on current get_edb operations
*/
extern struct connection	* connlist;     /* Connection blocks */
extern int			  conns_used;	/* No. conns in connlist */
extern struct connection	* connwaitlist; /* Connection blocks to be */
extern struct di_block		* deferred_dis;	/* deferred di_blocks */
extern struct oper_act		* get_edb_ops;	/* GET_EDB operations */
extern struct PSAPaddr		* mydsaaddr;	/* PSAP of this DSA */

#endif
