
/*	if_acpvar.h	 	V1.3		05/30/86	*/

/*************************************************************************/
/*                                                                       */
/*                                                                       */
/*       ________________________________________________________        */
/*      /                                                        \       */
/*     |          AAA          CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |      */
/*     |         AAAAA        CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |      */
/*     |        AAAAAAA       CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |      */
/*     |       AAAA AAAA      CCCC              CCCC              |      */
/*     |      AAAA   AAAA     CCCC              CCCC              |      */
/*     |     AAAA     AAAA    CCCC              CCCC              |      */
/*     |    AAAA       AAAA   CCCC              CCCC              |      */
/*     |   AAAA  AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |      */
/*     |  AAAA    AAAAAAAAAAA CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |      */
/*     | AAAA      AAAAAAAAA   CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |      */
/*      \________________________________________________________/       */
/*                                                                       */
/*  	Copyright (c) 1986 by Advanced Computer Communications           */
/*  	720 Santa Barbara Street, Santa Barbara, California  93101       */
/*  	(805) 963-9431                                                   */
/*                                                                       */
/*                                                                       */
/*  File:		if_acpvar.h                                      */
/*                                                                       */
/*  Author:		Arthur Berggreen                                 */
/*                                                                       */
/*  Project:		ACP6100 (UPB with HDLC firmware)                 */
/*          		ACP5100 (QPB with HDLC firmware)                 */
/*                                                                       */
/*  Revision History:                                                    */
/*                                                                       */
/*    21-NOV-1985  Clare Russ:  add fileheader and comments              */
/*         Add definitions for implementatin of new Command Interface    */
/*         (CIF) and Access Path Allocation Protocol (APAP).             */
/*         Note that values for LINK_DISABLE etc, have been updated.     */
/*    22-NOV-1985  Clare Russ:  Modify set system parameters values      */
/*    17-DEC-1985  Clare Russ:  NACPCH is 2 to include all channels:     */
/*         allocation channel, supervisor channel, and data channel.     */
/*    30-MAY-1985  Clare Russ:  Add MPCP host request initiation         */
/*         mailbox subfunction values (0 = data, c3 = supervisory msg)   */
/*                                                                       */
/*************************************************************************/


#define NACPCH		2		/* one ACP data channel    */
#define ACP_ALLOC	0		/* path allocation channel */
#define ACP_SUPR	1		/* supervisor channel      */
#define ACP_DATA	2		/* data channel            */


#define ACPMTU		4096		/* 4096 byte frames! */

#define ACP_OQMAX	8

#define DC_OBUSY	0x01

/* Defines for the Set HDLC System Parameters Command which is used */
/* to modify several operating parameters within the ACP5100/6100   */

#define LINK_DISABLE	0x22		/* disable the HDLC line    */
#define LINK_ENABLE	0x01		/* enable the HDLC line     */

/* The LINK_LOOPBACK, BAUD_CNTL, IDLE_POLL, and CLOCK_CNTL parameter */
/* modification commands are one-byte values containing a size and   */
/* and ID followed by 0-2 bytes of parameter information.  The       */
/* number of bytes of parameter information is specified in the most */
/* significant 2 bits of the command, the other 6 bits are the ID.   */
/* A 00, 01, or 10 specify respectively 0, 1, or 2 bytes of          */
/* parameter information follow.                                     */ 

#define LINK_LOOPBACK	0x42		/* set loopback mode        */
#define  LOOP_NONE	0x00		/*  none (normal operation) */
#define  LOOP_EXTERNAL	0x01		/*  external loopback       */
#define  LOOP_INTERNAL	0x02		/*  internal loopback       */
#define	BAUD_CNTL	0x8c		/* baud rate divisor        */
#define	IDLE_POLL	0x4d		/* poll inactive link       */
#define	CLOCK_CNTL	0x4e		/* select clock source      */

#define LINE_STATUS	0x61
#define LINK_UP		0x01

/*  Host Request Mailbox Completion Status  */

#define ACPIOCOK	0x01	/* success        */
#define ACPIOCABT	0xff	/* aborted        */
#define ACPIOCERR	0xfe	/* error          */
#define ACPIOCOVR	0xfd	/* overrun        */
#define ACPIOCUBE	0xfc	/* xfer count = 0 */

/*  Host Request Mailbox Subfunction  */

#define SBFCN_DATA	0x00	/* data           */
#define SBFCN_SUPR	0xc3	/* supervisory    */

#define ACPF_OK		0x0001	/* ACP operation flag */

#define ACPSTAT_OK	0x00

/* offsets for CIM command/response pairs */

#define	CMD_OFFSET	3	/* CIM header, offset for command */
#define	CID_OFFSET	7	/* Command ID field, set by the   */
				/* generator, is returned with no */
				/* change in the response         */
#define	RSF_OFFSET	11	/* CIM header, offset for RSF     */
#define	DPN_OFFSET	13	/* alloc cmd offset for dpn       */
#define	TYPE_OFFSET	17	/* alloc cmd offset for path type */

#define	LOOP_OFFSET	16	/* set system parms, loopback     */
#define	DTE_OFFSET	18	/* set system parms, dte/dce mode */
#define	BAUD_OFFSET	20	/* set system parms, dte/dce mode */
#define	CLOCK_OFFSET	25	/* set system parms, dte/dce mode */
#define	DOWN_OFFSET	26	/* set system parms, line down    */

#define INIT_OK		0x10	/* send line up CIM to front end  */

/****************************************************************************/
/* Most of the following defines were extracted from front end software     */
/* source include files cim.h, cimdef.h, and params.h on 21-NOV-1985.  This */
/* will ensure that the host and front end are using the same definitions.  */
/****************************************************************************/

/* Parameter Modification Commands for Set HDLC System Paramters command */

#define	DCE_OR_DTE		0x43	/* HDLC: select DCE/DTE mode	*/
#define	DTE_MODE		0x00	/*   specify DTE mode       	*/
#define	DCE_MODE		0x01	/*   specify DCE mode       	*/

#define	DTE_ADDR		0x04	/* HDLC: DTE frame address	*/
#define	DCE_ADDR		0x05	/* HDLC: DCE frame address	*/
#define	IFRAME_T_O		0x06	/* HDLC: T1 after I-frame	*/
#define	POLL_T_O		0x07	/* HDLC: T1 after poll cmd	*/
#define	ADM_T_O		0x08	/* HDLC: T1 for down link	*/
#define	RETRY_LIMIT		0x0a	/* HDLC: N2 counter		*/
#define	WATCHDOG_T_O		0x0b	/* phys: max transmit time	*/
#define	PKT_SIZE		0x10	/* X.25: packet size		*/
#define	PKT_WINDOW		0x11	/* X.25: packet window		*/
#define	PKT_TIMEOUT		0x12	/* X.25: packet timeout		*/
#define UPPER_THRESH		0x13	/* HDLC queue limits: upper	*/
#define LOWER_THRESH		0x14	/*		      lower	*/
#define WHICH_PROTOCOL	0x15	/* protocol above HDLC:		*/
#define	  PRMVAL_HDH		0x01	/* HDH				*/
#define	  PRMVAL_HDLC		0x02	/* none (bare HDLC)		*/
#define	  PRMVAL_X25		0x03	/* X.25 packet level		*/
#define IMP_OR_HOST		0x16	/* HDH: select IMP/host mode	*/
#define PKT_OR_MSG		0x17	/* HDH: select packet/msg mode	*/
#define SEND_TIMER		0x18	/* HDH send timer		*/
#define RESPONSE_TIMER	0x19	/* HDH response timer		*/
#define HLD_TMR		0x1A	/* HDH				*/
#define MISSES		0x1B	/* HDH				*/
#define HITS			0x1C	/* HDH				*/
#define SEND_WINDOW		0x1D	/* HDH				*/
#define MAXIFRM_XMIT		0x1e	/* HDLC: queue to transmitter	*/
#define FRAME_SPACE		0x1f	/* phys: interframe spacing	*/
#define FRAME_SIZE		0x20	/* HDLC: frame size		*/
#define FRAME_WINDOW		0x21	/* HDLC: transmit frame window	*/
#define EXTERNAL_CLOCK		0x00	/* clock generated externally   */
#define INTERNAL_CLOCK		0x01	/* clock generated internally 	*/

#define SUCCESS     	0
#define FAILURE     	1

/*   Path types    */

#define TYPE_DATA       0x01       /* Path type of DATA  */
#define TYPE_CNTL       0x02       /* Path type of CONTROL */


/*   Command Codes used in CIMs                */

                             /* Allocation Facility Commands */
#define CMD_ALLOC       0x22       /* Allocate Path */
#define CMD_DEALLOC     0x24       /* Deallocate Path */

                             /* Allocation Facility Responses */
                             /* Note that the response value  */
                             /* is the command value + 1      */
#define RSP_ALLOC       0x23       /* Allocate Path */
#define RSP_DEALLOC     0x25       /* Deallocate Path */
#define RSP_SSP     	0x63       /* Set Sys Params  */
#define RSP_FLUP        0x65       /* Frame Level Up */
#define RSP_FLDWN       0x67       /* Frame Level Down */

                             /* System Facility Commands */
#define CMD_BFINIT      0x42       /* Buffer Initialization */
#define CMD_BFQRY       0x44       /* Buffer Query */
#define CMD_FACQRY      0x46       /* Facility Query */
#define CMD_MEMRD       0x48       /* Memory Read */
#define CMD_MEMWRT      0x4A       /* Memory Write */

                             /* HDLC Facility Commands */
#define CMD_SSP         0x62       /* Set HDLC Parameters */
#define CMD_FLUP        0x64       /* Frame Level Up */
#define CMD_FLDWN       0x66       /* Frame Level Down */
#define CMD_STAQRY      0x68       /* Statistics Query */
#define CMD_FRMQRY      0x6A       /* Frame Level Status Query */

                             /* DCP Driver Commands */
#define CMD_ERST        0x02       /* Reset Encryption */
#define CMD_DRST        0x04       /* Reset Decryption */
#define CMD_ESTA        0x06       /* Start Encryption */
#define CMD_DSTA        0x08       /* Start Decryption */
#define CMD_ESET        0x0A       /* Set Encryption */
#define CMD_DSET        0x0C       /* Set Decryption */
#define CMD_ESTOP       0x0E       /* Stop Encryption */
#define CMD_DSTOP       0x10       /* Stop Decryption */
#define CMD_DCPERR      0x7E       /* DCP Error Detected */

/*   Facility Codes used in CIMs  */

/*    Symbol        FAC Code     Facility       */

#define FAC_NONE     0x00       /* No Facility assigned   */
#define FAC_ALLOC    0x01       /* Allocation facility */
#define FAC_SYS      0x02       /* System query/response facility */
#define FAC_HDLC     0x03       /* HDLC facility */
#define FAC_DCP      0x04       /* Data Encryption Facility */
#define MAX_FAC         4       /* Maximum facility number  */

             /* RSF Codes used in CIMs  */


/*   RSF Symbol    RSF Code    Response Description */
/*                 (decimal)                        */

#define RSF_SUCC       0        /* Success */
#define RSF_LONG       1        /* Command is too long */
#define RSF_SHORT      2        /* Command is too short */
#define RSF_NOTSUPP    3        /* Request not supported */
#define RSF_DATA_NA    4        /* Buffer contains data-Not allowed */
#define RSF_NOCNTL     5        /* Control Path must be allocated 1st */
#define RSF_NOFAC      6        /* Facility unavailable for allocation */
#define RSF_FACPTH     7        /* Path cannot be allocated for facility */
#define RSF_NOBUF      8        /* No buffers available    */
#define RSF_INVDPN    10        /* Invalid Data Path Number */
#define RSF_INVCMD    11        /* Invalid Command */
#define RSF_INVTYP    12        /* Invalid path type */
#define RSF_INVFAC    13        /* Invalid facility */
#define RSF_DPNOOR    20        /* DPN out of range */
#define RSF_PTHEXC    21        /* Maximum number of paths exceeded */
#define RSF_NOTALLOC  23        /* Path not allocated */
#define RSF_BFROOR    30        /* Buffer size out of range */
#define RSF_SEGBFR    31        /* Segmented Buffers implemented */
#define RSF_BFQEXC    32        /* Too many Buffer queues requested */
#define RSF_INVADR    40        /* Invalid ACP 6100 Address */
#define RSF_INVCNT    41        /* Byte count Invalid */
#define RSF_INVPID    50        /* Parameter ID Unknown */
#define RSF_INVVAL    51        /* Invalid Value */
#define RSF_PTLVAL    52        /* Partial Value */
#define RSF_BDPRTY    61        /* Bad parity for Key; one or more */
                                /* Key bytes had bad (even) parity */
#define RSF_NOSET     62        /* No Set Command given */
#define RSF_INVMOD    63        /* Invalid Mode in Set command */
#define RSF_INACT     64        /* Inactive-Stop issued for operation */
                                /* that's not Started */
#define RSF_ACTIVE    66        /* Active; operation was not Stopped */
#define RSF_ERROR     67        /* Error condition: send Reset(s) */
#define RSF_DCPSTER   70        /* DCP chip error, bad status after */
                                /* Start command */
#define RSF_DCPMODE   71        /* DCP chip error, bad status after */
                                /* mode set */
#define RSF_DCPRESET  73        /* DCP chip error, bad status */
                                /* after software reset */
#define RSF_DCPTRN    74        /* DMAC chip error detected; DMAC */
                                /* malfunction during data transfer */
