/*************************************************************************/
/*									 */
/*									 */
/*	 ________________________________________________________	 */
/*	/							 \	 */
/*     |	  AAA	       CCCCCCCCCCCCCC	 CCCCCCCCCCCCCC	  |	 */
/*     |	 AAAAA	      CCCCCCCCCCCCCCCC	CCCCCCCCCCCCCCCC  |	 */
/*     |	AAAAAAA	      CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |	 */
/*     |       AAAA AAAA      CCCC		CCCC		  |	 */
/*     |      AAAA   AAAA     CCCC		CCCC		  |	 */
/*     |     AAAA     AAAA    CCCC		CCCC		  |	 */
/*     |    AAAA       AAAA   CCCC		CCCC		  |	 */
/*     |   AAAA	 AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |	 */
/*     |  AAAA	  AAAAAAAAAAA CCCCCCCCCCCCCCCC	CCCCCCCCCCCCCCCC  |	 */
/*     | AAAA	   AAAAAAAAA   CCCCCCCCCCCCCC	 CCCCCCCCCCCCCC	  |	 */
/*	\________________________________________________________/	 */
/*									 */
/*	Copyright (c) 1986 by Advanced Computer Communications		 */
/*	720 Santa Barbara Street, Santa Barbara, California  93101	 */
/*	(805) 963-9431							 */
/*									 */
/*									 */
/*  File:		ddavar.h					 */
/*			When this file is copied to the /sys/vaxif	 */
/*			directory, it is renamed 'if_ddavar.h'.		 */
/*									 */
/*  Project:		UNIX 4.n BSD DDA-X.25 Network Interface Driver	 */
/*			for ACP 5250 and ACP 6250			 */
/*									 */
/*  Function:		This file contains definitions used to control	 */
/*			and track the status of the ACP 5250/6250.	 */
/*			The values for the Set System Parameter Messages */
/*			are located here.  Note that the values are the	 */
/*			same as those for ACP 625 for compatibility.	 */
/*									 */
/*  Revision History at end of file					 */
/*************************************************************************/


#if defined(DDA_PADOPT) || defined(DDA_RAWOPT)
#	define DDA_PAD_OR_RAW
#endif

#define NDDACH		126		/* maximum number of channels */
#define NDDACH_DEFAULT	64		/* default number of channels */

/* path numbers (also called logical channel numbers, or lcns) */
#define DDA_SUPR	0x00		/* supervisory path for control */
#define ALL_CHANS	0xff		/* denotes all LCNs */

/* the following 2 defines (MAXADDRLEN and MINADDRLEN) give the max and min
 * length X25 addresses.  These numbers include 1 byte to hold the length
 * of the address.  Thus the normal value of MAXADDRLEN is 15, for 14-digit
 * X.25 addresses; MAXADDRLEN can never be greater than 16 (15-digit addr).
 */
#define MAXADDRLEN	16		/* max length of an X.25 address */
#define MINADDRLEN	1		/* min length of an X.25 address */

#define TRANSPAC	1		/* network type for TRANSPAC (def=0) */

#define DDAMTU		1006		/* maximum IP msg length */
#define DDA_OQMAX	8		/* max IP msgs on LCN output q */
#define DDA_TIMEOUT	10		/* dda timer interval, in seconds */

/* definitions for the DC_FLAGS byte of the dda_cb structure */
/* bits in dc_flags */
#define DC_OBUSY	0x01
#define	DC_CLIENTS	0x0e
#define	DC_IP		0x00	/* this channel is being used for IP traffic */
#define DC_X29		0x02	/* this channel is being used for X29 traffic */
#define DC_X29W		0x04	/* this channel waiting for x29 connection */
#define DC_RAW		0x08	/* this is a raw X25 channel */
#define DC_IPEND	0x10	/* input pending */

/* bits in hc_sbfc (which is the wrong place) */
#define INVALID_MBUF	0x01		/* Used to clear outstanding I/O */

typedef unsigned char	byte;
typedef int		boolean;

/* X25 LCN state definitions (in dc_state) */
#define LC_DOWN		   0		/* X25 circuit down		*/
#define LC_RESTART	   1		/* X25 circuit restarting	*/
#define LC_IDLE		   2		/* X25 circuit not in use	*/
#define LC_CALL_PENDING	   3		/* X25 circuit call pending	*/
#define LC_DATA_IDLE	   4		/* X25 circuit open		*/
#define LC_CLR_PENDING	   5		/* X25 circuit clear pending	*/

/* Timeout definitions (in dc_timer and dc_out_t) */
#define TMO_OFF		   0			/* timer off		*/
#define TMO_RESTART	 ( 90/DDA_TIMEOUT)	/* restart timeout	*/
#define TMO_CALL_PENDING (180/DDA_TIMEOUT)	/* call timeout		*/
#define TMO_DATA_IDLE	 (600/DDA_TIMEOUT)	/* idle circuit timeout */
#define TMO_CLR_PENDING	 (380/DDA_TIMEOUT)	/* clear timeout	*/

/* Link status codes (third byte of LINE_STATUS message) */
#define LINK_DOWN	0x00		/* Link layer is down		*/
#define LINK_UP		0x01		/* Link layer is up		*/
#define LINK_DISABLED	0x02		/* Link layer is disabled	*/

/* The following parameter modification commands such as BAUD_CNTL,  */
/* or WATCHDOG, are one-byte values containing size and ID followed  */
/* by 0-2 bytes of parameter information.  The number of bytes of    */
/* parameter information is specified in the most significant 2 bits */
/* of the command, the other 6 bits are the ID.	 A 00, 01, or 10     */
/* specify respectively 0, 1, or 2 bytes of parameter information    */
/* follow.							     */ 

/* Line control codes (in body of LINE_CNTL message) */
#define LINK_DISABLE	0x00		/* Disable link layer		*/
#define LINK_ENABLE	0x01		/* Enable link layer		*/
#define LINK_LOOPBACK	0x42		/* Link layer loopback mode	*/
#define	  LOOP_NONE	0x00		/*   Loopback off		*/
#define	  LOOP_EXTERNAL 0x01		/*   Loopback external		*/
#define	  LOOP_INTERNAL 0x03		/*   Loopback internal		*/
#define DTE_DCE_MODE	0x43		/* DTE/DCE Mode Parameter	*/
#define	  DTE		0x00		/*   operate as DTE		*/
#define	  DCE		0x01		/*   operate as DCE		*/
#define DTE_ADDRESS	0x44		/* DTE Address Parameter	*/
#define	  DTE_ADRVAL	0x03		/*   DTE Address value		*/
#define DCE_ADDRESS	0x45		/* DCE Address Parameter	*/
#define	  DCE_ADRVAL	0x01		/*   DCE Address value		*/
#define IFRAME_TIMEO	0x46		/* I-Frame Timeout Parameter	*/
#define	  IFRAME_TOVAL	0x03		/*   I-Frame Timeout value 3s	*/
#define POLL_TIMEO	0x47		/* Poll Timeout Parameter	*/
#define	  POLL_TOVAL	0x03		/*   Poll Timeout value 3s	*/
#define ADM_TIMEO	0x48		/* ADM Timeout Parameter	*/
#define	  ADM_TOVAL	0x03		/*   ADM Timeout value 3s	*/
#define RETRY_LIMIT	0x4a		/* Retry Limit Parameter	*/
#define	  RETRY_VAL	0x14		/*   20 (decimal) retries	*/
#define WATCHDOG	0x4b		/* Watchdog Timeout Parameter	*/
#define	  WATCHDG_VAL	0x03		/*   Watchdog Timeout value 3s	*/
#define BAUD_CNTL	0xa9		/* Baud Rate Parameter		*/
#define CLOCK_CNTL	0x6a		/* Select Clock Source		*/
#define	  EXTERNAL_CLOCK  0x00		/* clock generated externally	*/
#define	  INTERNAL_CLOCK  0x01		/* clock generated internally	*/
#define IDLE_POLL	0x4d		/* Idle Poll Parameter		*/
#define	  IDLE_POLL_ON	0x01		/*   Idle Polling on		*/
#define	  IDLE_POLL_OFF 0x00		/*   Idle Polling off		*/
#define FRAME_WINDOW	0x4e		/* Frame Window Parameter	*/
#define	  FWINDW_VAL	0x07		/*   Frame Window value 7	*/
#define PKT_WINDOW	0x4f		/* Packet Window Parameter	*/
#define	  PWINDW_VAL	0x02		/*   Packet Window value 2	*/
#define PKT_SIZE	0x90		/* Packet Size Parameter	*/
#define	  PVAL_BYTE1	0x08		/*   least significant byte	*/
#define	  PVAL_BYTE2	0x00		/*   most significant byte	*/
#define MAX_PKT_SZ	0xbe		/* Max Packet Size		*/
#define MAX_PKT_WN	0x7c		/* Max Packet Window Size	*/
#define PKT_OPTIONS	0x77		/* Supported 1984 options	*/
#define HIGH_THRESH	0x51		/* High Buffer Threshold Param	*/
#define	  HTRSH_VAL	0x08		/*   High Threshold value	*/
#define LOW_THRESH	0x52		/* Low Buffer Threshold Param	*/
#define	  LTRSH_VAL	0x08		/*   Low Threshold value	*/
#define QUEUED_BUFS	0x53		/* Max Number of Queued Buffers */
#define	  QBUF_VAL	0x08		/*   Queued Buffer value	*/
#define QUEUED_IFRAMES	0x54		/* Max Number Queued I-Frames	*/
#define	  QIFRAME_VAL	0x08		/*   Queued I-Frame value	*/
#define FRAME_SIZE	0x95		/* Maximum Frames Size Parametr */
#define	  FRAME_SIZE1	0x95		/*   least significant byte	*/
#define	  FRAME_SIZE2	0x01		/*   most significant byte	*/
					/*   for value of 405 (decimal) */
#define LCGN		0x56		/* Logical Channel Group Number */
#define	  LCGN_VAL	0x00		/*   lcgn value			*/

#define SVC_LIMIT	0x57		/* Switched Virtual Circuit	*/
#define	  SVC_VAL	0x20		/*   SVC value 32 (decimal)	*/

#define DDAF_OK		0x0001		/* ACP operation flag		*/
#define DDASTAT_OK	0x00		/* ACP system status returned	*/
					/* on interrupt "b"		*/
#define DDASTAT_ERR	0x80
#define DDASTAT_NMC	0x7F

/* flag values for ds->dda_init */
#define INIT_OK		0x10		/* ok to call x25_init()	*/
#define DDA_STANDARD	0x01		/* DDN standard X.25 service	*/
#define DDA_BASIC	0x02		/* DDN basic X.25 service	*/
#define DDA_PDN		0x04		/* Public Data Network X.25	*/
#define DDA_INTCLOCK	0x08		/* internal clocking is set	*/
#define DDA_CLASS_A_B	0x20		/* Standard service/table lookup */
#define DDA_PKTNEG	0x40		/* Packet size negotiation flag */
#define DDA_WNDNEG	0x80		/* Window size negotiation flag */

/* the following offsets are for ddainit_msg, the set systems parameters */
/* message								 */

#define LOOP_OFFSET	6	/* set system parms, loopback	  */
#define DTE_OFFSET	8	/* set system parms, dte/dce mode */
#define BAUD_OFFSET	10	/* set system parms, dte/dce mode */
#define CLOCK_OFFSET	18	/* set system parms, dte/dce mode */
#define DOWN_OFFSET	19	/* set system parms, line down	  */

#define MSG_LENGTH	3	/* offset for message length	  */
#define MSG_OFFSET	4	/* offset for start of parameters */
#define MSGS_BAUD	3	/* msg size for baud rate parms	  */

/* X25 supervisor message codes */
#define CALL		0x00		/* outgoing call		*/
#define RING		0x01		/* incoming call		*/
#define CLEARVC		0x02		/* clear by VCN			*/
#define ANSWER		0x03		/* answer call			*/
#define CLEARLC		0x04		/* clear by LCN			*/
#define RESET		0x20		/* reset LCN			*/
#define RESET_ACK	0x21		/* reset ack			*/
#define INTERRUPT	0x22		/* X25 interrupt		*/
#define READY		0x23		/* flow control ready		*/
#define INTR_ACK	0x24		/* interrupt ack		*/
#define RESTART		0x40		/* X25 restart			*/
#define RSTRT_ACK	0x41		/* restart ack			*/
#define SYS_STATUS	0x42		/* system status msg		*/
#define LINE_CNTL	0x60		/* link control cmnd		*/
#define LINE_STATUS	0x61		/* link status resp		*/
#define SET_BFR_SIZE	0x62		/* set firmware buffer size	*/
#define STATQUERY     0217	      /* statistics query	      */
#define STATRESP      0216	      /* statistics response	      */

/* X25 facilities */
#define X25_FACIL_PKTSIZE 0x42		/* CCITT packet size negotiation*/
#define X25_FACIL_WINSIZE 0x43		/* CCITT window size negotiation*/
#define DDN_FACIL_MARKER 0		/* two of these mark DDN private*/
#define X25_FACIL_DDA	0x04		/* DDA mode facility		*/
#define FAC_DDASTD	0x01		/*   DDA standard mode		*/
#define PKTSIZE_LARGE	12		/* lg2 (4096) for calls		*/
#define WINSIZE_LARGE	7		/* Large (7) window for calls	*/
#define PKTSIZE_DEF	7		/* Default packet size		*/
#define WINSIZE_DEF	2		/* Default window size		*/

/* X25 protocols */
#define X25_PROTO_IP	0xcc		/* X25 IP protocol type code	*/
#define X25_PROTO_X29	0x01		/* X29 over X25 protocol type code */

/* DMESG(unit,value,statement) e.g. DMESG(0,27,printf("error message")) */
/* v should be a constant (no side effects)				*/
/* (v) >> 5 == (v) / 32	 - find which word the bit is in.		*/
/* (v) & 0x1f == (v) % 32 - find the number of the bit to check		*/
/* DMESGSET - set a msg bit to 1					*/
/* DMESGCLR - set a msg bit to 0					*/
/* DMESGTOG - toggle a msg bit						*/
/* DMESGVAL - the value of a message bit. 0 or non-zero			*/
#define DMESGSET(u,v)	(ddamsgs[(u)][(int)((v)>>5)] |=  (1 << ((v) & 0x1f)))
#define DMESGCLR(u,v)	(ddamsgs[(u)][(int)((v)>>5)] &= ~(1 << ((v) & 0x1f)))
#define DMESGTOG(u,v)	(ddamsgs[(u)][(int)((v)>>5)] ^=  (1 << ((v) & 0x1f)))
#define DMESGVAL(u,v)	(ddamsgs[(u)][(int)((v)>>5)] &   (1 << ((v) & 0x1f)))
#define DMESG(u,v,s)	(DMESGVAL(u,v) ? 0 : (s))

#ifdef	VAXVMS				/* always enable debugging under VMS */
#define DDADEBUG
#endif	VAXVMS

#ifdef DDADEBUG
#define	DDADBCH(n, unit) (!DMESGVAL(unit, n + 128)) /* first 128 are !debug*/
#endif

/*   macros to test for call logging -- only use unit 0 space for now */

#define	LOG_BUSY	(!DMESGVAL(0, 256)) /* "all circuits in use" 
					     * and "no circuits available" */
#define	LOG_CALLS	(!DMESGVAL(0, 257)) /* calls and clears */
#define	LOG_ABT		(!DMESGVAL(0, 258)) /* I/O aborts */

/* values in dda_state */
#define S_DISABLED	0	/* link is disabled */
#define S_COMING_UP	1	/* enable issued, waiting for response */
#define S_LINK_UP	2	/* link operational */
#define S_GOING_DOWN	3	/* disable issued, waiting for response */

struct trtab			 /* This stuff should ALSO be in ddareg.h */
{				 /* Address Translation Table IOCTL Data */
    unsigned char  func;
    unsigned char  x25addr[MAXADDRLEN];
    unsigned long  ipaddr;
};

struct ddactl		/* used for -m (message), -q (query), -n (svc_count) */
{
    unsigned char func;
    unsigned char nothing[3];
    int drval;
    char msg[MLEN];
};

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%									%%*/
/*%%	The download structure is the template for communication	%%*/
/*%%	with the ACP7000 when it is running diagnostic roms.  The	%%*/
/*%%	will initially place the SYSGEN_VALID entry in the BIIC GPR0	%%*/
/*%%	when in the probe routine.  The IOCTL mechanism has been	%%*/
/*%%	extended to support a *very* limited download feature which	%%*/
/*%%	enables us to load programs into the 7000 and then execute	%%*/
/*%%	them.  Versabug has been modified to examine GPR0 for the	%%*/
/*%%	SYSGEN_DLOAD value.  GPR1 will contain the physical address of  %%*/
/*%%	our standard sysgen block.  The driver will then place messages	%%*/
/*%%	in the request queue.  These messages will be read by the 7000	%%*/
/*%%	diagnostic firmware and placed into memory as needed.  After	%%*/
/*%%	all code has been loaded,  a final "EXEC" command may be sent	%%*/
/*%%	to the 7000 giving an execution start address.  The 7000 will	%%*/
/*%%	then execute the system code (which should wait for GPR0 to	%%*/
/*%%	contain the standard SYSGEN_VALID pattern before continuing.	%%*/
/*%%									%%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifdef	ACP_BI
#define SYSGEN_VALID	0x12345678	/* normal operation mode */
#define	SYSGEN_DLOAD	0x31415927	/* download operation mode */

#define	DN_LCMD_SETUP	0	/* reset board, alloc buffers, setup shm */
#define	DN_LCMD_FEOP	1	/* send command and data to front end */
#define	DN_LCMD_CLEANUP	2	/* dealloc buffers and restore shm to norm */

#define	DN_TYPE_ID	1	/* identification record */
#define	DN_TYPE_VER	2	/* version record */
#define	DN_TYPE_COPY	3	/* copyright record */
#define	DN_TYPE_DATA	4	/* data record */
#define	DN_TYPE_XFR	5	/* start address transfer record */

struct dda_dnload		/* code download structure */
{
    unsigned char   func;	/* ioctl function code (will be 'L') */
    unsigned char   lcommand;	/* driver load command (setup/op/cleanup) */
    unsigned short  padding;	/* null padding for dest addr */
    unsigned short  len;	/* length of entire record */
    unsigned short  type;	/* type of record passed */
    unsigned int    dest;	/* destination address */
    char           *data;	/* address of data record in user space */
};
#endif	ACP_BI

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 
/*%%								 %%*/ 
/*%% Histogram support declarations and defines.  Slots 0-NDDACH %%*/
/*%% record the amount of time n channels were being used.  The	 %%*/ 
/*%% following define uses of locations that come after the	 %%*/ 
/*%% first NDDACH entries.					 %%*/ 
/*%%								 %%*/ 
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/ 


#define H_LINK_UP	(NDDACH+1)	/* entry that records the time link is*/
					/* up 0-126 */
#define H_START		(NDDACH+2)	/* starting time */
#define H_END		(NDDACH+3)	/* ending time */
#define H_TMO		(NDDACH+4)	/* current value of idle timer */
#define HISTSIZE	(H_TMO+1)	/* size of the histogram table */

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%								 %%*/
/*%%   Information for each device unit is maintained in an	 %%*/
/*%%   array of structures named dda_softc[].  The array is	 %%*/
/*%%   indexed by unit number.	Each entry includes the network	 %%*/
/*%%   interface structure (dda_if) used by the routing code to	 %%*/
/*%%   locate the interface,  an array of Logical Channel	 %%*/
/*%%   control blocks which maintain information about each of	 %%*/
/*%%   the Logical Channels (LCNs) through which X.25 virtual	 %%*/
/*%%   calls are established, a queue of I/O requests pending	 %%*/
/*%%   for the ACP, the UNIBUS interrupt vector for the unit	 %%*/
/*%%   and misc flags.	The Logical Channel Control blocks	 %%*/
/*%%   maintain information about the state of each LCN, a	 %%*/
/*%%   queue of outbound data, Half Duplex Channel (HDX) blocks	 %%*/
/*%%   used for queuing I/O requests to the ACP and an ifuba	 %%*/
/*%%   structure which records the UNIBUS resources being held	 %%*/
/*%%   by the LCN.						 %%*/
/*%%								 %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

struct sioq			 /* Start I/O queue head */
{
    struct hdx_chan *sq_head;	 /* queue head */
    struct hdx_chan *sq_tail;	 /* queue tail */
};

struct hdx_chan			 /* HDX channel block */
{
    struct hdx_chan *hc_next;	/* link to next HDX channel */
    struct mbuf *hc_mbuf;	/* mbuf chain */
    struct mbuf *hc_curr;	/* current mbuf in chain */
    unsigned char *hc_addr;	/* address bits 15-00 */
    unsigned short hc_cnt;	/* byte count */
    unsigned char  hc_func;	/* I/O function */
    unsigned char  hc_sbfc;	/* I/O subfunction */
    unsigned char  hc_chan;	/* HDX channel number */
    unsigned char  hc_inv;	/* place to store various bits */
#ifdef vax11c
    /* VAX C compiler (and GCC running in VAXC native mode) does not pad
	structures to longword boundaries, so we must pad any structures
	that are passed between ACPCONFIG and the driver */
    unsigned char  _vaxcfill[2]; /* pad structure to longword boundary */
#endif
};

union dc_key
{
	struct in_addr key_addr;
	unsigned long key_val;
};

struct dda_cb			 /* Logical Channel control block */
{
#if defined(DDA_PADOPT) && defined(VAXVMS)
    int ptyucb;			/* to store the np unit control blk addr */
    int ttyflags;		/* flags for the VMS x29 driver */
#endif
    int		dc_line;	/* index into tty structure / minor number */
    struct in_addr dc_inaddr;	/* remote Internet address */
    union  dc_key  dc_key;	/* circuit destination key */
    unsigned char  dc_lcn;	/* LCN number */
    unsigned char  dc_state;	/* LCN state */
    unsigned short dc_timer;	/* LCN timer */
    struct ifqueue dc_oq;	/* LCN output queue */
    struct hdx_chan dc_rchan;	/* LCN read HDX channel */
    struct hdx_chan dc_wchan;	/* LCN write HDX channel */
    short  dc_next;		/* LCN next index. Long so padding will work */
    unsigned char dc_wsizeout;	/* negotiated outgoing window size */
    unsigned char dc_wsizein;	/* negotiated ingoing window size */
    unsigned char dc_pktsizeout;/* negotiated outgoing packet size */
    unsigned char dc_pktsizein; /* negotiated ingoing packet size */
    unsigned short dc_flags;	/* misc flags, DC_OBUSY */
    unsigned short dc_out_t;	/* DEBUG output completion timer per lcn */
#ifdef vax11c
    /* VAX C compiler (and GCC running in VAXC native mode) does not pad
	structures to longword boundaries, so we must pad any structures
	that are passed between ACPCONFIG and the driver */
    unsigned char _vaxcfill[2]; /* pad structure to longword boundary */
#endif
};

struct dda_softc		 /* device control structure */
{
    struct ifnet dda_if;	 /* network-visible interface */
    struct dda_cb dda_cb[NDDACH + 1];	/* Logical Channel cntl blks */
    struct sioq dda_sioq;	 /* start I/O queue */
    int	  	   dda_vector;	/* UNIBUS interrupt vector */
    int	    	   dda_mapreg;	 /* UNIBUS mapping register */
    unsigned char  dda_flags;		 /* ACP operational flag (intr b) */
    unsigned char  dda_init;		 /* flag for init, X.25 service */
    unsigned char  dda_state;		 /* state of link (see below) */
    unsigned char  dda_firmrev;	 /* firmware revision level byte */
    struct in_addr dda_ipaddr;	/* local IP address */
    int	    dda_net_id;		/* net type -- for TRANSPAC */
};

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%								 %%*/
/*%%   The dc_flags field in the dda_cb structure is used to	 %%*/
/*%%   indicate that output is busy, DC_OBUSY.	In ddainit(),	 %%*/
/*%%   the flag is set to zero.	 In dda_start(), return if the	 %%*/
/*%%   flag is set DC_OBUSY; otherwise if output isn't active	 %%*/
/*%%   an attempt is made to send another packet.  The packet	 %%*/
/*%%   is dequeued, and the flag is set for DC_BUSY on.	 In	 %%*/
/*%%   both dda_data and dda_supr, if a write completion is	 %%*/
/*%%   indicated, DC_BUSY is turned off before firing up a write %%*/
/*%%   via a call to dda_start().				 %%*/
/*%%								 %%*/
/*%%   In order to modify packet level parameters like packet	 %%*/
/*%%   window, packet level must be restarted.	Doing this	 %%*/
/*%%   reliably requires the driver know four possible states	 %%*/
/*%%   for the FEP: DISABLED (down and not attempting to bring	 %%*/
/*%%   the link up); COMING UP (processed a "-u N" for N != 0,	 %%*/
/*%%   but have not exchanged restarts yet); UP (data transfer	 %%*/
/*%%   state); GOING DOWN (processed "-u 0", but the other end	 %%*/
/*%%   has not yet agreed).  The dda_state variable tracks these %%*/
/*%%   four states.  No other data structure provides this level %%*/
/*%%   of resolution.  In particular, IFF_UP is always on when	 %%*/
/*%%   dda_state is UP, but sometimes on when dda_state is	 %%*/
/*%%   COMING UP or GOING DOWN.					 %%*/
/*%%								 %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#ifdef DDA_MSGQ
#define MSGQSIZE	2048
#define MQHEX		1	/* marks the start of a hex number */
#define MQDEC		2	/* marks the start of a decimal number */
#define MQEND		0	/* marks the end of message queue data */
#define MSGQNAME	"_ddamsgq"	/* the text name of the message q */
#endif


/*

Revision History:

26-Mar-1986: V1.0 - First generated.
		 Clare Russ.
05-Sep-1986: V1.1 - Add definitions for modified ACPCONFIG interface
		 Clare Russ.
27-Oct-1986: V1.2 - Change TMO_DATA_IDLE from 30 to 600 sec.
		 Lars Poulsen and Randy Graves.
30-Jan-1987: V1.3 - Added DDASTAT_NMC
		 Jeff Berkowitz and Stephanie Price
30-Mar-1987: V1.4 - Added new #defines for X.25 facilities
		 Jeff Berkowitz
09-Sep-1987: V2.1 - Moved all major structure and constant
		 declarations from the driver to this file.
		 Added new #defines for X.25 options and facilities
		 Note that you must now edit this file to turn on
		 debugging and logging.
		 Stephanie Price
18-Mar-1988: V3.0 Brad Engstrom
		 Added four fields to the dda_cb structure. These fields
		 are used to track the negotiated packet and window size
		 for both incoming and outgoing directions. This data
		 will be displayed by the -l option of acpconfig.
12-Apr-1988: V3.0 Brad Engstrom
		 Moved all initialized variables to if_dda.c.  This will
		 allow this file to work under BSD or ULTRIX because the
		 ULTRIX C compiler gags on variables declared in header
		 files.
15-Apr-1988: V3.0 Brad Engstrom
		 Added a key field for doing searches for a
		 matching destination.	This is used to see if there is
		 already an open circuit to a particular destination.
15-Apr-1988: V3.0 Brad Engstrom
		 Added a next pointer to the dda_cb structure.	This
		 will allow lcn structures to be linked into free and
		 active lists.	The pointer is actully just an offset to
		 the next element in the list.	This allows programs such
		 as acpconfig to traverse a copy of the structure in
		 memory.
15-Apr-1988: V3.0 Brad Engstrom
		 Got rid of conditional compilation of dc_out_t field
		 in the dda_cb structure.  This will always be included
		 in the structure, but will only be used if DDA_DEBUG is
		 on. This releaves problems of padding size when adding
		 new fields to the dda_cb structure.
22-Apr-1988: V3.0 Brad Engstrom
		 Added new macro DMESG.	 This is used to conditionally
		 print driver console messages.
22-Apr-1988: V3.0 Brad Engstrom
		 Added a new define DDAMAINT_BRD.  If this is defined
		 then it is assumed that the maintenance board is being
		 used so don't do checks for the board id in the probe
		 routine.
10-May-1988: V3.0 Brad Engstrom
		 Added the constant MAXADDRLEN which is the maximum lenth
		 of an X.25 address. Also added	 MINADDRLEN to specify
		 the minimum X25 address length.
26-Oct-1988: V3.1 Charles Carvalho
		Added documentation.
15-Aug-1988: V4.0 Brad Ensgtrom
		added support for X.29 and Programmers Interface
02-Sep-1988: V4.0 Brad Ensgtrom
		added new field hc_inv to the hc structure.  Someone used
		the hc_sbfc field to store the invalid bit.  Now that the
		PI will use the subfunction field the invalid bit must be
		stored someplace else.
09-Jan-1989: V4.1 SAJ -- Merge 4.0, 3.1
		relaxed MAXADDRLEN & MINADDRLEN to allow for transpac
		installed padding changes from TGV for vax11c
17-Feb-1989: Paul Traina
		Merged SAJ's changes for DDA_DEBUG
28-May-1989: Paul Traina
		Changed structure padding for TGV
20-Jun-1989: Paul Traina
		Eliminated old debug logic... call loging is next on the hit
		list.  New driver is not compatible with old acpconfig!
18-Jul-1989: Paul Traina
		Moved dc_key.ttyline out of union, creating dc_line.  This
		is to stop it from getting clobbered on restarts et al.
25-Oct-1989: Paul Traina
		Added download structure for ACP7000 downloading and code
		execution.
*/
