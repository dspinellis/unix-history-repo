/*      enetdefs.h	Stanford	25 April 1983 */

/* Notes:
 *	- check maxwaiting, etc.
 *	- Xdrops in enState may be meaningless
 */

/*
 *  Ethernet definitions NOT needed for user processes
 *
 **********************************************************************
 * HISTORY
 * 7 October 1985	Jeff Mogul	Stanford
 *	Removed ENMAXOPENS limitation; available minors are now
 *	dynamically allocated to interfaces.
 *	Certain arrays in the enState structure are now indirected
 *	via pointers.
 * 17 October 1984	Jeff Mogul	Stanford
 *	Added RecvCount filed to enOpenDescriptor struct to store count
 *	of packets received on this filter.
 *	Also, made some changes to encourage use of "high-priority"
 *	devices:
 *	- changed ENHIPRIDEV from 12 to 0; allows anyone to use
 *		high priority
 *	- changed ENHIPRI from 128 to 2; no point in having more than
 *		one priority below ENHIPRI, since all would be equivalent.
 *
 * 2 October 1984	Jeff Mogul	Stanford
 *	Added field to enOpenDescriptor struct to store pre-computed
 *	address of short word AFTER last valid word in the filter;
 *	this saves a few instructions in some very busy code.
 *
 * 10 November 1983	Jeffrey Mogul	Stanford
 *	Minor changes to support EIOCDEVP ioctl,
 *	removed references to enUnit[]
 *
 * 25-Apr-83	Jeffrey Mogul	Stanford
 *	Began conversion to 4.2BSD.  This involves removing all
 *	references to the actual hardware.
 *	Most previous history comments removed.
 *	Split off from definitions of interest to users.
 *
 * 10-Aug-82  Mike Accetta (mja) at Carnegie-Mellon University
 *	Added EIOCMBIS and EIOCMBIC definitions, and new ENHOLDSIG mode
 *	bit and ENPRIVMODES defintions (V3.05e). [Last change before
 *	4.2BSD conversion starts.]
 *
 * 22-Feb-80  Rick Rashid (rfr) at Carnegie-Mellon University
 *	Rewritten for multiple simultaneous opens with filters (V1.05).
 *
 * 18-Jan-80  Mike Accetta (mja) at Carnegie-Mellon University
 *      Created (V1.00).
 *
 **********************************************************************
 */

/* tunable constants */
#define	ENHIPRI		2		/* high priority priority */
			/*
			 * setting ENHIPRI to 2 makes sense because all
			 * priorities below ENHIPRI are equivalent; therefore,
			 * we only need one of them.
			 */
#define	ENMAXPRI	255		/* maximum high priority priority */

#define	ENUNIT(dev)	(enUnitMap[minor(dev)]) /* macro */
					/* to extract ethernet unit number */
					/* from device number (used as the */
					/* index into the enState[] table); */
#define	ENINDEX(dev)	(minor(dev)) /* macro to extract logical */
					/* device index from device number */
					/* (used as the index into the */
					/* AllDescriptors table for the */
					/* unit) */

#define ENMAXWAITING	8		/* maximum number of packets */
					/* which can be queued at one time */
					/* for an open enet file (max 256) */
#define ENDEFWAITING	2		/* default number of packets */
					/* which can be queued at one */
					/* time for an open enet file */
#define ENPACKETS	50		/* number of preallocated packets */
					/* available for receiving (these
					/* should be allocated dynamically */
					/* once the system makes */
					/* this more feasible) */

#define ENMINSCAVENGE	4		/* minimum number of packets */
					/* guaranteed to be freed by */
					/* scavenging */

/*
 * Privileged mode bits
 */
#define	ENKERNEL	(0x8000)	/* flag bit to indicate kernel mode */
					/*  requests */

/*
 *  The following definitions are meant to clean up
 *  C language syntax a bit.
 */

typedef	enum {false, true}	boolean;
#define	TRUE	((int)true)
#define	FALSE	((int)false)

#define endcase break;

/*
 * General purpose structure to define circular queues.
 *  Both the queue header and the queue elements have this
 *  structure.
 */

struct Queue
{
    struct Queue * F;
    struct Queue * B;
};

#ifdef notdef
/* Temporary used by queue macros. */
struct Queue * q_tmp;

#define enqueue(head,elt) \
{ (elt)->F = (head);(elt)->B = (head)->B;(head)->B = (elt);((elt)->B)->F = (elt); };

#define remqueue(head,elt) \
    { ((elt)->B)->F = (elt)->F; ((elt)->F)->B = (elt)->B; };

#define dequeue(head) 			  \
    (((q_tmp = (head)->F) == (head)) ? NULL : \
     (((q_tmp->B)->F = q_tmp->F),         \
      ((q_tmp->F)->B = q_tmp->B),	  \
      (q_tmp)))

#define queueempty(head) \
    ((head)->F == (head))

#define initqueue(head)  \
    { (head)->F = (head); (head)->B = (head); };
#endif notdef

/*
 *  The ethernet packet structure.
 *
 */

struct enPacket
{
    struct Queue   enP_Link;		/* queue pointers */
    u_short	  *enP_Data;		/* pointer to start of packet */
    short          enP_ByteCount;	/* # of bytes in Data */
    u_short	   enP_RefCount;	/* # of outstanding references to */
					/* this packet */
    struct mbuf	  *enP_mbuf;		/* first mbuf of packet */
};
#define	enP_F	enP_Link.F
#define	enP_B	enP_Link.B

/*
 *  Ethernet queue header
 */
struct enQueue
{
    struct Queue enQ_Head;	/* queue header and trailer pointers */
    short	 enQ_NumQueued;	/* number of elements in queue */
};
#define	enQ_F	enQ_Head.F
#define	enQ_B	enQ_Head.B

/*
 *  Wait queue header
 */
struct enWaitQueue
{
    struct enPacket *enWQ_Packets[ENMAXWAITING];/* pointers to queued packets */
    char	     enWQ_Head;		/* index into Packets (for dequeue) */
    char	     enWQ_Tail;		/* index into Packets (for enqueue) */
    u_char	     enWQ_NumQueued;	/* current queue size */
    u_char	     enWQ_MaxWaiting;	/* threshold for additions */
};
#define	enNextWaitQueueIndex(idx)					\
	if (++(idx) >= ENMAXWAITING) (idx) = 0
#define	enPrevWaitQueueIndex(idx)					\
	if (--(idx) < 0) (idx) = (ENMAXWAITING-1)

/*
 *  States of receive side of open enet file.
 */
enum enStates {ENRECVIDLE, ENRECVTIMING, ENRECVTIMEDOUT};

struct enOpenDescriptor
{
    struct Queue       enOD_Link;	/* Linked list of OpenDescriptors */
    struct enWaitQueue enOD_Waiting;	/* fixed Queue of waiting packets */
    struct proc       *enOD_SigProc;	/* Process to signal (user mode) */
    short	       enOD_SigPid;	/* Process ID of process to signal */
    short    	       enOD_SigNumb;	/* Signal number for input packet */
					/* notification  */
    long               enOD_Timeout;	/* Length of time to wait for packet */
    struct enfilter    enOD_OpenFilter;	/* Packet filter */
    enum enStates      enOD_RecvState;	/* see enStates enumeration (above) */
    short	       enOD_Flag;	/* option bits */
    
    /* following are for enetselect() */
    short	       enOD_SelColl;	/* true if selects collide */
    struct proc       *enOD_SelProc;	/* process that last selected us */
    
    unsigned long      enOD_RecvCount;	/* number of packets received */

    /* store precomputed address of end of filter */
    unsigned short    *enOD_FiltEnd;	/* addr of short AFTER end of filt */
};

/*
 *  State descriptor for each enet device
 */
struct enState
{
    long	ens_Rcnt;		/* input packets queued since */
					/* system startup */
    long	ens_Xcnt;		/* output packets sent since */
					/* system startup */
    long	ens_Rdrops;		/* input packets dropped since */
					/* system startup */
    long	ens_Xdrops;		/* output packets dropped since */
					/* system startup */
    struct enQueue			/* queue of active open */
		ens_Desq;		/* file descriptors */
    struct endevp	ens_DevParams;	/* device parameters, see enet.h */
};
#define	enRcnt		(enStatep->ens_Rcnt)
#define	enXcnt		(enStatep->ens_Xcnt)
#define	enRdrops	(enStatep->ens_Rdrops)
#define	enXdrops	(enStatep->ens_Xdrops)
#define	enDesq		(enStatep->ens_Desq)
#define	enCurOpens	(enStatep->ens_Desq.enQ_NumQueued)
#define	enDevParams	(enStatep->ens_DevParams)
