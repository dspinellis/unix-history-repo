/*
 * Copyright (c) 1986 MICOM-Interlan, Inc., Boxborough Mass
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)npreg.h	7.3 (Berkeley) %G%
 *
 * Merged header file for MICOM-Interlan NP100.
 */

/*
 *	np100.h version 1.3
 *
 *	This version retrieved: 8/18/86 @ 18:58:44
 *	    This delta created: 8/18/86 @ 18:27:32
 */
/*
 * Typedefs for the VAX 
 */

typedef short	sign16;			/* 16 bit signed value */
typedef unsigned short unsign16;	/* 16 bit unsigned value */
typedef unsigned unsign32;		/* 32 bit unsigned value */
typedef long paddr_t;			/* Physical addresses */


/*
 * Tunables
 */


#define NUMCQE		40		/* Number of CQE's per board */

/* Host configuration word in Status Block */

/*
 * To disable the lock/unlock internal function calls clear the 0x8000
 * bit in the host configuration word (HOSTCONF)
 */

#define	HOSTCONF	0x0109	/* See above */
#define	LOWBYTE		1
#define	HIGHBYTE	0
#define BUFFMAPPED	0

/*
 * Memory mapping definintions for PM68DUAL hardware.
 */

#ifdef PM68DUAL
#define	PISHMEM		0x200000
#define PISHMEMSIZE	2
#define PIOFF		0x8000		/* change this to unique mem add. */
#define PIWINDOW	MBUSBUFR + PIOFF
#define WINDOWSIZE	2
#endif
#define	NPMAXXFR	32768		/* Maximum number of bytes / read */ 

/*
 * Define the protocols supported by the NP Driver.
 */

#define NONE		0x00	/* No protocols active for a process */
#define NPMAINT		0x01	/* Maintenance protocol, superusers only */
#define NPNTS		0x02	/* NTS Terminal Server */
#define NPIDP		0x04	/* Direct Datalink Access */
#define NPDLA		0x04	/* Direct Datalink Access */
#define NPXNS		0x06	/* Xerox NS ITP */
#define NPTCP		0x08	/* TCP/IP */
#define NPISO		0x0A	/* ISO */
#define NPCLCONN	0xFF	/* Closed connection, i.e. no protocol */

/*
 * Convert the protocol to a value used in the Device Protocol Mask field
 * of the Shared Memory Status Block.
 */

#define PROTOMASK(x)	( 1 << (x) )

/*
 * Special requests handled by the NP Driver
 */

#define OS_STP		03400	/* Shut down connection on I Board */
#define NPSTOP		3	/* Conversion from above (OS_STP) */
#define NPCHNGP		50	/* Change the protocol on a connection */
#define NPCHNGB		51	/* Change the Board number */

/*
 * Miscellaneous
 */

#define ON		0x8000  /* Used for Command Q's scan and change flag */
#define UBADDRMASK	0x3FFFF /* 18 bit UNIBUS address */
#define INTMASK		0xFFFFFFFC /* Used for address validation */
#define CMDMASK		0xFFFF	/* Mask ioctl cmd field (see ioctl.h) */
#define NPPSADDR	0x324	/* Pointer to addr of on-board panic string */
#define	PANLEN		133		/* length of the panic buffer */

/*
 * Map function code from user to I-Board format
 */

#define FUNCTMAP(x)	(((x) << 6) | 077) /* Maps user function to NP funcs */

/*
 * Round up to a 16 byte boundary
 */

#define ROUND16(x)	(((x) + 15) & (~0x0F)) /* Round to 16 byte boundary */
#define ADDR24		1 /* Used by iomalloc() to specify 24 bit address */

#define NPERRSHIFT	8	/* Used in function ReqDone() */
#define NPOK		0

#define LOWORD(X)	(((ushort *)&(X))[0])
#define HIWORD(X)	(((ushort *)&(X))[1])

/* Everyday flag settings */

#define NPSET		1
#define NPCLEAR		0

/*
 * Command Queue Elements are the primary data structure for passing data
 * between the driver and the device.
 */

struct CQE {

	struct npreq *cqe_reqid;/* Address of asssociated npreq */
	union	{
		unsign32 cqe_Famid;	/* Family ID (Process ID) - wn */
		unsign16 cqe_PrtSig[2];	/* port and signal - tn */
	} u1;
#define	cqe_famid	u1.cqe_Famid
#define	cqe_port	u1.cqe_PrtSig[0]
#define	cqe_signal	u1.cqe_PrtSig[1]
	unsign16 cqe_func;	/* I/O function to be performed */
#ifdef mc68000
	char cqe_prot;		/* Protocol type for I/O request */
	char cqe_lenrpb;	/* Length of the RPB in bytes */
#else
	char cqe_lenrpb;	/* Length of the RPB in bytes */
	char cqe_prot;		/* Protocol type for I/O request */
#endif
	union	{
		unsign16 cqe_ustS[2];	/* Protocol status return */
		unsign32 cqe_ustL;	/* Protocol status return */
	} u2;
#define	cqe_ust0	u2.cqe_ustS[0]
#define	cqe_ust1	u2.cqe_ustS[1]
#define	cqe_usts	u2.cqe_ustL
	unsign16 cqe_devrsv;	/* Reserved for use by device only! */
#ifdef mc68000
	char cqe_char;		/* CQE characteristics */
	char cqe_sts;		/* Status return from device to user */
	char cqe_wind;		/* Buffer mapping window size (page units) */
	char cqe_nbuf;		/* Number of data buffers for I/O */
#else
	char cqe_sts;		/* Status return from device to user */
	char cqe_char;		/* CQE characteristics */
	char cqe_nbuf;		/* Number of data buffers for I/O */
	char cqe_wind;		/* Buffer mapping window size (page units) */
#endif
	unsign16 cqe_bcnt;	/* Total number of bytes in the data buffer */
	union {
		unsign16 cqe_Unused;	/* Unused */
		struct {
			char cqe_Maxbcnt;	/* Maximum size of buffer */
			char cqe_Bflags;	/* Used by the SPI */
		} s;
	} u3;
#define cqe_unused	u3.cqe_Unused
#define cqe_maxbcnt	u3.s.cqe_Maxbcnt
#define cqe_bflags	u3.s.cqe_Bflags
	unsign16 cqe_dma[2];	/* Address of the MULTIBUS data buffer */
	unsign16 rpb1;		/* Word 1 of protocol parameters */
	unsign16 rpb2;		/* Word 2 of protocol parameters */
	unsign16 rpb3;		/* Word 3 of protocol parameters */
	unsign16 rpb4;		/* Word 4 of protocol parameters */
	unsign16 rpb5;		/* Word 5 of protocol parameters */
	unsign16 rpb6;		/* Word 6 of protocol parameters */
	unsign16 rpb7;		/* Word 7 of protocol parameters */
	unsign16 rpb8;		/* Word 8 of protocol parameters */
	unsign16 rpb9;		/* Word 9 of protocol parameters */
	unsign16 rpb10;		/* Word 10 of protocol parameters */
	unsign16 rpb11;		/* Word 11 of protocol parameters */
	unsign16 rpb12;		/* Word 12 of protocol parameters */

};

/*
 * NP Driver Request structure contains information about a request
 * maintained solely by the driver. One per CQE, plus a header.
 */

 struct npreq {

	struct npreq *forw;	/* Forward pointer for active list */
	struct npreq *back;	/* Backward pointer for active list */
	struct npreq *free;	/* Next member on free list */
	struct CQE *element;	/* CQE associated with this request */
	int flags;		/* Always useful */
	int reqcnt;		/* Request count for reqtab */
	int bufoffset;		/* Offset into buffer for turns */
	int	bytecnt;	/* Number of bytes to transfer */
	caddr_t	virtmem;	/* Virtual address of buffer */
	int	mapbase;	/* Address of the mapping register */
	int 	mapsize;	/* Size of mapped area */
	caddr_t	bufaddr;	/* Address of the user buffer */
	struct buf buf;		/* Buf structure needed for mem. mgmt */
	struct proc *procp;	/* Pointer to process of requestor */
	caddr_t user;		/* Structure passed by user from itpuser.h */
	int	(*intr)();	/* Ptr to routine to call at interrupt time */
	int 	int_param;      /* Paramater to be used by above routine */
};

/*
 * Npmaster structure, one per device, is used for boardwise centralization
 * of relevant information including queues, I/O addresses and request pools.
 */

struct npmaster {

	struct npmaster *next; 	/* Linked list of these, NULL terminator */
	struct npspace *shmemp;	/* Shared memory address (driver <-> device) */
	struct uba_device *devp; /* UBA Device for this unit */
	struct NPREG   *iobase;	/* I/O base address for this board */
	struct npreq   *reqtab;	/* Header for pool of CQE requests */
	int	iomapbase;	/* Base index of I/O map reg's allocated */
	int flags;		/* State of the Board */
	int unit;		/* Unit number of this device */
	int vector;		/* Interrupt vector for this unit */
};

struct NPREG {
	unsign16 CSR0;		/* Control Status Register 0 */
	unsign16 CSR1;		/* Control Status Register 1 */
	unsign16 CSR2;		/* Control Status Register 2 */
	unsign16 CSR3;		/* Control Status Register 3 */

};

/*
 * The following structures are used for communicating with the
 * Intelligent Board and are located in Shared Memory.
 */

/*
 * Status Block
 */

struct NpStat{

	unsign16 sb_drw;	/* Device Request Word */
	unsign16 sb_hcw;	/* Host Configuration Word */
	unsign16 sb_dcw;	/* Device Configuration Word */
	unsign16 sb_dpm;	/* Device Protocol Mask */
	unsign16 sb_dcq;	/* Offset to Device CQ */
	unsign16 sb_hcq;	/* Offset to Host CQ */
};

/*
 * Command Queue, two per device. One is owned by the driver and the other
 * is owned by the device.
 */

struct CmdQue {

	unsign16 scanflag;	/* Scan Flag, MSB set if being scanned */
	unsign16 chngflag;	/* Change Flag, MSB set by initiator */
	unsign16 cq_wrap;	/* Offset to last CQE entry +2 */
	unsign16 cq_add;	/* Offset to add a CQE to the queue */
	unsign16 cq_rem;	/* Offset to remove a CQE from the queue */
	unsign16 cq_cqe[NUMCQE]; /* Command Queue Element Offsets */
};

/*
 * Structure of the shared memory area per board. Declared this way to avoid
 * compiler word alignment vagaries when computing offsets.
 */

struct npspace {

	struct NpStat statblock;	/* Status Block */
	struct CmdQue devcq;		/* Device's Command Queue */
	struct CmdQue hostcq;		/* Host's Command Queue */
	struct CQE elements[NUMCQE];	/* Shared Command Queue Elements */
	unsign16 filler[8];		/* Here for 16 byte alignment */
};

/*
 * Structure of array of base addresses of I-Board controllers
 * (See global data definitions in np.c)
 */

struct npbase {
	caddr_t baseaddr;
};

/* State of the NP Driver as kept in NpState */

#define ICPAVAIL	0x01 	/* ICP is waiting for a request */

/* Tells ICP Process that there are no more requests for this board */

#define BRDDONE 1

/* Flags used by the driver (npreq structure) to monitor status of requests */

#define REQDONE 0x01		/* Request completed */
#define IOIFC   0x02		/* Internal Function Code Request */
#define IOERR	0x04		/* Error on Request */
#define NPPEND	0x08		/* Unused at this time */
#define IOABORT 0x10		/* Request aborted by ICP */
#define KERNREQ	0x20		/* Request was from the kernel */
#define WANTREQ 0x40		/* Process is waiting for a npreq structure */
#define NPUIO	0x80		/* Process doing physio */
#define REQALOC 0x100           /* Request has been allocated */
#define REQUSE  0x200           /* Request is in request queue */

/* Service Request Commands from the Intelligent Board */

#define NOREQ	0x00		/* No service requested */
#define NPLOAD  0x01		/* Dump request */
#define NPDUMP	0x02		/* Load request */
#define NPPANIC	0x100		/* Panic request */

/* Definitions of Status returned from the I-Board */

#define NPDONE	0x01		/* Normal completion */
#define NPIFC	0x00		/* Internal Function Code request */
#define NPPERR  0x80		/* Protocol error */
#define NPMERR	0x82		/* Memory allocation failure on I-Board */

/* Definitions of IFC type requests from I-Board */

#define NPLOCK	0x64		/* Lock the process's data area */
#define NPUNLOCK 0xA4		/* Unlock the process */
#define NPREMAP	0x124		/* Window turn */

/* Definition of flags for the Npmaster structure */

#define CSRPEND		0x01		/* CSR0 command pending */
#define PANICREQ	0x02		/* Panic request */
#define DUMPREQ		0x04		/* Dump request */
#define LOADREQ		0x08		/* Load request */
#define BOARDREQ	0x10		/* Any request by the board */
#define BADBOARD	0x20		/* Board disabled */
#define AVAILABLE	0x40		/* Board available */
#define BRDRESET	0x80		/* Board is being reset */
#define	PANIC1	 	0x100		/* Driver wants panic address */
#define	PANIC2		0x200		/* Driver wants panic string */
#define PANIC3		0x400		/* Clear first byte of panic string */
#define LSTCMD          0x800           /* Clear last command during NPIO */
#define SCANNING        0x1000          /* We are scanning for cqe's */

/*
 * Debugging Constants
 */

#define	DEBENTRY	0x0001		/* debug entry points */
#define	DEBMEM		0x0002		/* debug memory */
#define	DEBREQ		0x0004		/* debug requests */
#define	DEBCQE		0x0008		/* debug cqe's */
#define	DEBCQ		0x0010		/* debug cq's */
#define	DEBMAINT	0x0020		/* debug maintainance requests */
#define	DEBINTR		0x0040		/* debug interrupt routines */
#define	DEBINIT		0x0080		/* debug initialization routines */
#define	DEBIFC		0x0100		/* debug Internal function codes */
#define	DEBIOCTL	0x0200		/* debug ioctl calls */
#define	DEBOPEN		0x0400		/* debug open calls */
#define	DEBIO		0x0800		/* debug read & write calls */
#define	DEBCSR		0x1000		/* debug CSR commands */
#define	DEBLOCK		0x2000		/* debug lock / unlock calls */
#define NOBOARD		0x4000		/* debug user/host interface */
#define DEBCANCEL       0x8000          /* debug cancel command */

/*
 *	npreg.h version 1.3
 *
 *	This version retrieved: 8/18/86 @ 18:58:46
 *	    This delta created: 8/18/86 @ 18:27:42
 */

/*
 *			NPREG.H
 *
 * This file contain definitions of specific hardware interest
 * to be used when communicating with the NI1510 Network Processor
 * Board. More complete information can be found in the NI1510
 * Multibus compatible Ethernet Communications Processor Hardware 
 * Specification.
 */

/*
 *	npcmd.h version 1.3
 *
 *	This version retrieved: 8/18/86 @ 18:58:45
 *	    This delta created: 8/18/86 @ 18:27:38
 */
#ifdef KERNEL
#    define IoVOID 0
#else
#    define IoVOID IOC_VOID
#endif

#define NPRESET		(IoVOID|0x01)	/* reset the board */
#define	NPSTART		(IoVOID|0x04)	/* start board execution */
#define	NPGPANIC	(IoVOID|0x05)	/* Get panic message */
#define	NPINIT		(IoVOID|0x06)	/* initialize software on board */
#define NPSTATS 	(IoVOID|0x07)
#define	NPRCSR0		(IoVOID|0x08)	/* read CSR0 */
#define	NPRCSR1		(IoVOID|0x09)	/* read CSR1 */
#define	NPRCSR2		(IoVOID|0x0a)	/* read CSR2 */
#define	NPRCSR3		(IoVOID|0x0b)	/* read CSR3 */
#define	NPWCSR0		(IoVOID|0x0c)	/* write CSR0 */
#define	NPWCSR1		(IoVOID|0x0d)	/* write CSR1 */
#define	NPWCSR2		(IoVOID|0x0e)	/* write CSR2 */
#define	NPWCSR3		(IoVOID|0x0f)	/* write CSR3 */
#define NPPOLL  	(IoVOID|0x10)
#define NPKILL  	(IoVOID|0x11)
#define	NPSETPROT	(IoVOID|0x12)	/* set the protocol to use */
#define	NPSETBOARD	(IoVOID|0x13)	/* set board to use */
#define	NPSETNPDEB	(IoVOID|0x14)	/* set nc debuging level */
#define	NPSETADDR	(IoVOID|0x15)	/* set host address */
#define	NPNETBOOT	(IoVOID|0x16)	/* boot from the network */
#define NPSETLAST       (IoVOID|0x17)   /* set last command flag in NPIO */
#define NPCLRICNT       (IoVOID|0x18)   /* clear interupt count */
#define NPGETICNT       (IoVOID|0x19)   /* get interupt count */
#define NPGETIVEC       (IoVOID|0x1a)   /* get interupt vector */
#define NPMAPMEM        (IoVOID|0x1b)   /* map user memory to shmem */

#define NP_SET          1031            /* set memory mapping */
#define NP_USET         1032            /* unset memory mapping */

struct np_mem {
	long mem_type;
	char *mem_addr;
        long mem_count;
} ;

#define NNPCNN		4	/* Number of connections per board */
#define NPUNIT(a)	((minor(a) >> 4) & 0x0F)
#define NPCONN(a)	((minor(a)) & 0x03)

#define TRUE		1
#define FALSE		0

#define IBOOTADDR	0xF8000l	/* Addr of 80186 Boot ROM */
#define	INETBOOT	0xF8087l
#define IXEQADDR	0x400		/* Where to begin Board image XEQ */
#define DIAGTIME	1200		/* Time for timeout /HZ seconds */

#define	DELAYTIME	1000000L		/* delay count */
#define NPDELAY(N)	{register int n = (N) >> 1; while(--n > 0); }

/* Handy macros for talking to the Board */

#define RESET(x) 	(WCSR3(x->iobase,0xff))
#define CLEARINT(x)	{unsign16 y; y = RCSR2(x->iobase); }
#define INTNI(x)	(WCSR1(x->iobase,0xFF))

/* Command and Status Register (CSR) Definitions */

/*
 * CSR0 is the only direct means for data transfer between the host processor
 * and the 3510. Access is controlled by the 80186 who sets the CSR1 Enable and
 * Ready bits to allow writing here. Writing to this register will always
 * result in an interrupt to the 80186.
 */

/* 
 * Bit definitions for CSR1.
 */

#define NPRFU	0x01		/* Reserved for Future Use */
#define NPHOK	0x02		/* Hardware OK */
#define NPLAN	0x04		/* Logic 0 indicates operational LAN exists */
#define NP_IP	0x08		/* Interrupt pending from this board */
#define NP_IE	0x10		/* Interrupts enabled for this board */
#define NPRDR	0x20		/* Set when 80186 writes data into CSR0 */
#define NPRDY	0x40		/* CSR0 ready to accept data */
#define NPENB	0x80		/* CSR0 available for use by the host */

/*
 * Bit defintions for CSR0 Command Block
 */

#define NPLST	0x20		/* Last Command */
#define NPCMD	0x80		/* Shared Memory Address */
#define NPBGN	0x200		/* Begin Execution in On-Board Memory */
#define NPCBI	0x800		/* Interrupt at completion of Command Block */
#define NPDMP	0x2000		/* Dump 80186 On-Board Memory to Multibus */
#define NPLD	0x8000		/* Load 80186 On-board Memory from Multibus */

/*
 * CSR0 Count definitions. These are the lengths of the Command Blocks for the
 * CSR0 commands above (not counting the Command Word itself).
 */

#define LSTCNT	0
#define CMDCNT	2
#define BGNCNT	2
#define CBICNT	1
#define DMPCNT	5
#define LDCNT	5
#define IOCNT	5

/* Macros for reading and writing CSR's (Control and Status Registers) */

#define	WCSR0(x,y)	((x)->CSR0 = y)
#define	WCSR1(x,y)	((x)->CSR1 = y)
#define	WCSR2(x,y)	((x)->CSR2 = y)
#define	WCSR3(x,y)	((x)->CSR3 = y)

#define	RCSR0(x)	((x)->CSR0)
#define	RCSR1(x)	((x)->CSR1)
#define	RCSR2(x)	((x)->CSR2)
#define	RCSR3(x)	((x)->CSR3)

struct npconn {

	struct npmaster *unit;	/* Unit number (board) of this connection */
	unsign16 protocol;	/* Protocol used on this connection */
	struct buf np_wbuf;	/* write buf structure for raw access */
	struct buf np_rbuf;	/* read buf structure for raw access */
};

/* ICP Board Requests */

#define ICPLOAD  0x02
#define ICPDUMP  0x03
#define ICPPANIC 0x05
#define ICPPOLL  0x10

/*
 *	npdebug.h version 1.3
 *
 *	This version retrieved: 8/18/86 @ 18:58:46
 *	    This delta created: 8/18/86 @ 18:27:39
 */

/*
 * Debugging Constants
 */

#define	DEBENTRY	0x0001		/* debug entry points */
#define	DEBMEM		0x0002		/* debug memory */
#define	DEBREQ		0x0004		/* debug requests */
#define	DEBCQE		0x0008		/* debug cqe's */
#define	DEBCQ		0x0010		/* debug cq's */
#define	DEBMAINT	0x0020		/* debug maintainance requests */
#define	DEBINTR		0x0040		/* debug interrupt routines */
#define	DEBINIT		0x0080		/* debug initialization routines */
#define	DEBIFC		0x0100		/* debug Internal function codes */
#define	DEBIOCTL	0x0200		/* debug ioctl calls */
#define	DEBOPEN		0x0400		/* debug open calls */
#define	DEBIO		0x0800		/* debug read & write calls */
#define	DEBCSR		0x1000		/* debug CSR commands */
#define	DEBLOCK		0x2000		/* debug lock / unlock calls */
