/*	@(#)tmscp.h	7.2 (Berkeley) 5/27/88 */

/*
 *	@(#)tmscp.h	1.3	10/21/85
 * Definitions for the Tape Mass Storage Control Protocol
 */

/****************************************************************
 *                                                              *
 *        Licensed from Digital Equipment Corporation           *
 *                       Copyright (c)                          *
 *               Digital Equipment Corporation                  *
 *                   Maynard, Massachusetts                     *
 *                         1985, 1986                           *
 *                    All rights reserved.                      *
 *                                                              *
 *        The Information in this software is subject to change *
 *   without notice and should not be construed as a commitment *
 *   by  Digital  Equipment  Corporation.   Digital   makes  no *
 *   representations about the suitability of this software for *
 *   any purpose.  It is supplied "As Is" without expressed  or *
 *   implied  warranty. 					*
 *								*
 *        If the Regents of the University of California or its *
 *   licensees modify the software in a manner creating  	*
 *   diriviative copyright rights, appropriate copyright  	*
 *   legends may be placed on  the drivative work in addition   *
 *   to that set forth above. 					*
 *								*
 ****************************************************************
 *
 * Modification history: /sys/vax/tmscp.h
 *
 * 18-Oct-85 - afd
 *	Added: defines for tape format (density) flag values.
 *
 * 18-Jul-85 - afd
 *	Added: #define	M_UF_WBKNV	0000100
 *		for write back (which enables cache).
 ************************************************************************/
 
 
/*
 * Control message opcodes
 */
#define	M_OP_ABORT	0001	/* Abort command */
#define	M_OP_GTCMD	0002	/* Get command status command */
#define	M_OP_GTUNT	0003	/* Get unit status command */
#define	M_OP_STCON	0004	/* Set controller characteristics command */
#define	M_OP_AVAIL	0010	/* Available command */
#define	M_OP_ONLIN	0011	/* Online command */
#define	M_OP_STUNT	0012	/* Set unit characteristics command */
#define	M_OP_DTACP	0013	/* Determine access paths command */
#define	M_OP_ACCES	0020	/* Access command */
#define	M_OP_CMPCD	0021	/* Compare controller data command */
#define	M_OP_ERASE	0022	/* Erase command */
#define	M_OP_FLUSH	0023	/* Flush command */
#define M_OP_ERGAP	0026	/* Erase gap command */
#define	M_OP_COMP	0040	/* Compare host data command */
#define	M_OP_READ	0041	/* Read command */
#define	M_OP_WRITE	0042	/* Write command */
#define	M_OP_WRITM	0044	/* Write tape mark command */
#define	M_OP_REPOS	0045	/* Reposition command */
#define	M_OP_AVATN	0100	/* Available attention message */
#define	M_OP_DUPUN	0101	/* Duplicate unit number attention message */
#define	M_OP_ACPTH	0102	/* Access path attention message */
#define	M_OP_END	0200	/* End message flag */
 
 
/*
 * Generic command modifiers
 */
#define	M_MD_COMP	0040000		/* Compare */
#define	M_MD_CLSEX	0020000		/* Clear serious exception */
#define	M_MD_SECOR	0001000		/* Suppress error correction */
#define	M_MD_SEREC	0000400		/* Suppress error recovery */
#define	M_MD_STWRP	0000004		/* Set write protect */
#define	M_MD_ALLCD	0000002		/* All class drivers */
#define	M_MD_NXUNT	0000001		/* Next unit */
 
/*
 * TMSCP command modifiers
 */
#define	M_MD_DLEOT	0000200		/* Delete LEOT */
#define	M_MD_IMMED	0000100		/* Immediate completion */
#define	M_MD_EXCAC	0000040		/* Exclusive access */
#define	M_MD_UNLOD	0000020		/* Unload */
#define	M_MD_REVRS	0000010		/* reverse */
#define	M_MD_OBJCT	0000004		/* object count */
#define	M_MD_REWND	0000002		/* rewind */
 
/*
 * End message flags
 */
#define	M_EF_ERLOG	0040	/* Error log generated */
#define	M_EF_SEREX	0020	/* Serious exception */
#define	M_EF_EOT	0010	/* End of tape encountered */
#define	M_EF_PLS	0004	/* Position lost */
 
 
/*
 * Controller flags
 */
#define	M_CF_ATTN	0200	/* Enable attention messages */
#define	M_CF_MISC	0100	/* Enable miscellaneous error log messages */
#define	M_CF_OTHER	0040	/* Enable other host's error log messages */
#define	M_CF_THIS	0020	/* Enable this host's error log messages */
 
 
/*
 * Unit flags
 */
#define	M_UF_WRTPH	0020000		/* Write protect (hardware) */
#define	M_UF_WRTPS	0010000		/* Write protect (software or volume) */
#define	M_UF_WBKNV	0000100		/* Write back (enables cache) */
#define	M_UF_VSMSU	0000040		/* Variable speed mode suppression */
#define	M_UF_VARSP	0000020		/* Variable speed unit */
#define	M_UF_CMPWR	0000002		/* Compare writes */
#define	M_UF_CMPRD	0000001		/* Compare reads */
 
 
/*
 * Status codes
 */
#define	M_ST_MASK	037		/* Status code mask */
#define	M_ST_SUCC	000		/* Success */
#define	M_ST_ICMD	001		/* Invalid command */
#define	M_ST_ABRTD	002		/* Command aborted */
#define	M_ST_OFFLN	003		/* Unit offline */
#define	M_ST_AVLBL	004		/* Unit available */
#define	M_ST_WRTPR	006		/* Write protected */
#define	M_ST_COMP	007		/* Compare error */
#define	M_ST_DATA	010		/* Data error */
#define	M_ST_HSTBF	011		/* Host buffer access error */
#define	M_ST_CNTLR	012		/* Controller error */
#define	M_ST_DRIVE	013		/* Drive error */
#define	M_ST_FMTER	014		/* Formatter error */
#define	M_ST_BOT	015		/* BOT encountered */
#define	M_ST_TAPEM	016		/* Tape mark encountered */
#define	M_ST_RDTRN	020		/* Record data truncated */
#define	M_ST_PLOST	021		/* Position lost */
#define	M_ST_SEX	022		/* Serious exception */
#define	M_ST_LED	023		/* LEOT detected */
#define	M_ST_DIAG	037		/* Message from an internal diagnostic */
 
/*
 * An MSCP packet
 */
 
struct mscp {
	struct	mscp_header mscp_header;/* device specific header */
	long	mscp_cmdref;		/* command reference number */
	short	mscp_unit;		/* unit number */
	short	mscp_xxx1;		/* unused */
	u_char	mscp_opcode;		/* opcode */
	u_char	mscp_flags;		/* end message flags */
	short	mscp_modifier;		/* modifiers */
	union {
	struct {
		long	Mscp_bytecnt;	/* byte count */
		long	Mscp_buffer;	/* buffer descriptor */
		long	Mscp_mapbase;   /* physical addr of map registers */
		long	Mscp_xxx2;	/* unused */
		long	Mscp_lbn;	/* logical block number */
		long	Mscp_xxx4;	/* unused */
		long	*Mscp_dscptr;	/* pointer to descriptor (software) */
		long	Mscp_sftwds[17];/* software words, padding */
	} mscp_generic;
	struct {
		short	Mscp_version;	/* MSCP version */
		short	Mscp_cntflgs;	/* controller flags */
		short	Mscp_hsttmo;	/* host timeout */
		short	Mscp_usefrac;	/* use fraction */
		quad	Mscp_time;	/* time and date */
		long	Mscp_cntdep;	/* controller dependent parameters */
	} mscp_setcntchar;
	struct {
		short	Mscp_multunt;	/* multi-unit code */
		short	Mscp_unitflgs;	/* unit flags */
		long	Mscp_hostid;	/* host identifier */
		quad	Mscp_unitid;	/* unit identifier */
		long	Mscp_mediaid;	/* media type identifier */
		short	Mscp_format;	/* format (tape density) */
		short	Mscp_speed;	/* tape speed = (ips * bpi) /1000 */
		short	Mscp_fmtmenu;	/* format menu */
		short	Mscp_group;	/* group size */
		short	Mscp_cylinder;	/* cylinder size */
		short	Mscp_xxx3;	/* reserved */
		short	Mscp_rctsize;	/* RCT table size */
		char	Mscp_rbns;	/* RBNs / track */
		char	Mscp_rctcpys;	/* RCT copies */
	} mscp_getunitsts;
	} mscp_un;
	short mscp_fil1;
	short mscp_fil2;
	short mscp_fil3;
};
 
#define mscp_msglen (sizeof (struct mscp) - sizeof(struct mscp_header))
 
/*
 * generic packet
 */
 
#define	mscp_bytecnt	mscp_un.mscp_generic.Mscp_bytecnt
#define	mscp_buffer	mscp_un.mscp_generic.Mscp_buffer
#define	mscp_mapbase	mscp_un.mscp_generic.Mscp_mapbase
#define	mscp_lbn	mscp_un.mscp_generic.Mscp_lbn
#define	mscp_dscptr	mscp_un.mscp_generic.Mscp_dscptr
#define	mscp_sftwds	mscp_un.mscp_generic.Mscp_sftwds
#define	mscp_status	mscp_modifier
 
/*
 * Abort / Get Command Status packet
 */
 
#define	mscp_outref	mscp_bytecnt
 
/*
 * Set Controller Characteristics packet
 */
 
#define	mscp_version	mscp_un.mscp_setcntchar.Mscp_version
#define	mscp_cntflgs	mscp_un.mscp_setcntchar.Mscp_cntflgs
#define	mscp_hsttmo	mscp_un.mscp_setcntchar.Mscp_hsttmo
#define	mscp_usefrac	mscp_un.mscp_setcntchar.Mscp_usefrac
#define	mscp_time	mscp_un.mscp_setcntchar.Mscp_time
#define	mscp_cntdep	mscp_un.mscp_setcntchar.Mscp_cntdep
 
/*
 * Reposition command packet fields
 */
 
#define mscp_reccnt mscp_bytecnt	/* record/object count */
#define mscp_tmkcnt mscp_buffer		/* tape mark count */
 
/*
 * Get Unit Status end packet
 */
 
#define	mscp_multunt	mscp_un.mscp_getunitsts.Mscp_multunt
#define	mscp_unitflgs	mscp_un.mscp_getunitsts.Mscp_unitflgs
#define	mscp_hostid	mscp_un.mscp_getunitsts.Mscp_hostid
#define	mscp_unitid	mscp_un.mscp_getunitsts.Mscp_unitid
#define	mscp_mediaid	mscp_un.mscp_getunitsts.Mscp_mediaid
#define	mscp_format	mscp_un.mscp_getunitsts.Mscp_format /* density:0=high */
#define	mscp_speed	mscp_un.mscp_getunitsts.Mscp_speed  /* (ips*bpi)/1000 */
#define	mscp_fmtmenu	mscp_un.mscp_getunitsts.Mscp_fmtmenu
 
/*
 * Online / Set Unit Characteristics end packet
 */
 
#define	mscp_maxwrt	mscp_dscptr	/* max write byte count */
#define	mscp_noiserec	mscp_cylinder	/* noise record */
 
/*
 * Set Controller Characteristics end packet
 */
 
#define	mscp_cnttmo	mscp_hsttmo	/* controller timeout */
#define	mscp_cntcmdl	mscp_usefrac	/* controller soft & hardware version */
#define	mscp_cntid	mscp_unitid	/* controller id */
 
 
/*
 * Error Log message format codes
 */
#define	M_FM_CNTERR	0	/* Controller error */
#define	M_FM_BUSADDR	1	/* Host memory access error */
#define	M_FM_TAPETRN	5	/* Tape transfer error */
#define	M_FM_STIERR	6	/* STI communication or command failure */
#define	M_FM_STIDEL	7	/* STI drive error log */
#define	M_FM_STIFEL   010	/* STI formatter error log */
 
/*
 * Error Log message flags
 */
#define	M_LF_SUCC	0200	/* Operation successful */
#define	M_LF_CONT	0100	/* Operation continuing */
#define	M_LF_SQNRS	0001	/* Sequence number reset */
 
/*
 * Tape Format Flag Values
 */
#define	M_TF_800	001	/* NRZI 800 bpi */
#define	M_TF_PE		002	/* Phase Encoded 1600 bpi */
#define	M_TF_GCR	004	/* Group Code Recording 6250 bpi */
#define	M_TF_BLK	010	/* Cartridge Block Mode */
 
/*
 * MSCP Error Log packet
 *
 *	NOTE: MSCP packet must be padded to this size.
 */
 
struct mslg {
	struct	mscp_header mslg_header;/* device specific header */
	long	mslg_cmdref;		/* command reference number */
	short	mslg_unit;		/* unit number */
	short	mslg_seqnum;		/* sequence number */
	u_char	mslg_format;		/* format */
	u_char	mslg_flags;		/* error log message flags */
	short	mslg_event;		/* event code */
	quad	mslg_cntid;		/* controller id */
	u_char	mslg_cntsvr;		/* controller software version */
	u_char	mslg_cnthvr;		/* controller hardware version */
	short	mslg_multunt;		/* multi-unit code */
	quad	mslg_unitid;		/* unit id */
	u_char	mslg_unitsvr;		/* unit software version */
	u_char	mslg_unithvr;		/* unit hardware version */
	short	mslg_group;		/* group; retry + level */
	long	mslg_position;		/* position (object count) */
	u_char	mslg_fmtsvr;		/* formatter software version */
	u_char	mslg_fmthvr;		/* formatter hardware version */
	short	mslg_xxx2;		/* unused */
	char	mslg_stiunsucc[62];	/* STI status information */
};
 
#define	mslg_busaddr	mslg_unitid.val[0]
#define	mslg_sdecyl	mslg_group

