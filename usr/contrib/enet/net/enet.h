/*      enet.h	Stanford	25 April 1983 */

/*
 *  Ethernet definitions needed for user processes
 *
 **********************************************************************
 * HISTORY
 * 7 October 1985	Jeff Mogul	Stanford
 *	Added EIOCMFREE ioctl to indicate # of free minor devices;
 *	may or may not be useful.
 * 17 October 1984	Jeff Mogul	Stanford
 *	Added ENF_CAND, ENF_COR, ENF_CNAND, and ENF_CNOR, short-circuit
 *	operators, to make filters run faster.
 *	All evaluate "(*sp++ == *sp++)":
 *	ENF_CAND: returns false immediately if result is false, otherwise
 *		continue
 *	ENF_COR: returns true immediately if result is true, otherwise
 *		continue
 *	ENF_CNAND: returns true immediately if result is false, otherwise
 *		continue
 *	ENF_CNOR: returns false immediately if result is true, otherwise
 *		continue
 *	Also added ENF_NEQ to complement ENF_EQ
 *
 * 10 November 1983	Jeffrey Mogul	Stanford
 *	Slight restructuring for support of 10mb ethers;
 *	added the EIOCDEVP ioctl and associated definitions
 *	and removed the EIOCMTU ioctl (subsumed by EIOCDEVP)
 *
 * 25-Apr-83	Jeffrey Mogul	Stanford
 *	Began conversion to 4.2BSD.  This involves removing all
 *	references to the actual hardware.
 *	Incompatible change: ioctl encodings!
 *	Added EIOCMTU ioctl to get MTU (max packet size).
 *	Most previous history comments removed.
 *	Definitions of interest only to kernel now are in enetdefs.h
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
#ifdef KERNEL
#include "ioctl.h"
#else
#include <sys/ioctl.h>
#endif	KERNEL

#define ENMAXFILTERS	40		/* maximum filter short words */

/*
 *  filter structure for SETF
 */
struct enfilter
{
    u_char  enf_Priority;		/* priority of filter */
    u_char  enf_FilterLen;		/* length of filter command list */
    u_short enf_Filter[ENMAXFILTERS];	/* the filter command list */
};

/*  set/get parameters, set filter ioctl commands  */
#define	EIOCSETP	_IOW(E,100, struct eniocb)
#define	EIOCGETP	_IOR(E,101, struct eniocb)
#define	EIOCSETF	_IOW(E,102, struct enfilter)
#define	EIOCENBS	_IOW(E,103, int)
#define	EIOCINHS	_IO(E,104)
#define	EIOCSETW	_IOW(E,105, u_int)
#define	EIOCFLUSH	_IO(E,106)
#define	EIOCALLOCP	_IO(E,107)
#define	EIOCDEALLOCP	_IO(E,108)
#define	EIOCMBIS	_IOW(E,109, u_short)
#define	EIOCMBIC	_IOW(E,110, u_short)
#define	EIOCDEVP	_IOR(E,111, struct endevp)
#define	EIOCMFREE	_IOR(E,112, int)

/*
 *  Bits in mode word modified by EIOCMBIS and EIOCMBIC.
 */
#define	ENHOLDSIG	(0x0001)	/* don't disable signal after sending */
#define	ENPRIVMODES	(~(ENHOLDSIG))

/*
 *  We now allow specification of up to MAXFILTERS (short) words of a filter
 *  command list to be applied to incoming packets to determine if
 *  those packets should be given to a particular open ethernet file.
 *  
 *  Each open enet file specifies the filter command list via iocontrl.
 *  Each filter command list specifies a sequences of actions which leave a
 *  boolean value on the top of an internal stack.  Each word of the
 *  command list specifies an action from the set {PUSHLIT, PUSHZERO,
 *  PUSHWORD+N} which respectively push the next word of the stack, zero,
 *  or word N of the incoming packet on the stack, and a binary operator
 *  from the set {EQ, LT, LE, GT, GE, AND, OR, XOR} which operates on the
 *  top two elements of the stack and replaces them with its result.  The
 *  special action NOPUSH and the special operator NOP can be used to only
 *  perform the binary operation or to only push a value on the stack.
 *  
 *  If the final value of the filter operation is true, then the packet is
 *  accepted for the open file which specified the filter.
 *  
 */

/*  these must sum to 16!  */
#define	ENF_NBPA	10			/* # bits / action */
#define	ENF_NBPO	6			/* # bits / operator */

/*  binary operators  */
#define ENF_NOP	(0<<ENF_NBPA)
#define	ENF_EQ	(1<<ENF_NBPA)
#define	ENF_LT	(2<<ENF_NBPA)
#define	ENF_LE	(3<<ENF_NBPA)
#define	ENF_GT	(4<<ENF_NBPA)
#define ENF_GE	(5<<ENF_NBPA)
#define	ENF_AND	(6<<ENF_NBPA)
#define	ENF_OR	(7<<ENF_NBPA)
#define	ENF_XOR	(8<<ENF_NBPA)
#define	ENF_COR	(9<<ENF_NBPA)
#define	ENF_CAND	(10<<ENF_NBPA)
#define	ENF_CNOR	(11<<ENF_NBPA)
#define	ENF_CNAND	(12<<ENF_NBPA)
#define	ENF_NEQ		(13<<ENF_NBPA)

/*  stack actions  */
#define	ENF_NOPUSH	0
#define	ENF_PUSHLIT	1	
#define	ENF_PUSHZERO	2
#define	ENF_PUSHWORD	16

/*
 *  parameter buffer structure for GETP and SETP
 */
struct eniocb
{
    u_char en_addr;		/* ethernet address (RO) */
    u_char en_maxfilters;	/* max filter words available (RO) */
    u_char en_maxwaiting;	/* max queued input packets (RO) */
    u_char en_maxpriority;	/* max filter priority for this file (RO) */
    long   en_rtout;		/* receive timeout in (jiffies) (RW) */
};

/*
 * parameter structure for EIOCDEVP (get device parameters)
 */

#define	EN_MAX_ADDR_LEN	8	/* maximum bytes in a hardware address */

struct endevp {
	u_char	end_dev_type;	/* device type, codes below */
	u_char	end_addr_len;	/* length (bytes) of a hardware address */
	u_short	end_hdr_len;	/* length of a hardware packet header */
	u_short	end_MTU;	/* maximum packet size (bytes) */
	u_char	end_addr[EN_MAX_ADDR_LEN];
				/* hardware address for this unit */
	u_char	end_broadaddr[EN_MAX_ADDR_LEN];
				/* hardware-supported broadcast address */
};

/* Ethernet Device Type codes */

#define	ENDT_3MB	3	/* Xerox Experimental Ethernet */
#define	ENDT_BS3MB	1	/* Xerox Experimental Ethernet/byteswapped */
#define	ENDT_10MB	2	/* Xerox-DEC-Intel Standard Ethernet */

#define	ENDT_MIN	1	/* minimum defined device type code */
#define	ENDT_MAX	3	/* maximum defined device type code */
