/*	mscp.h	81/10/26	1.1	*/
/*
 * Definitions for the Mass Storage Control Protocol
 */


/*
 * Control message opcodes
 */
#define	M_OP_ABORT	0001	/* Abort command */
#define	M_OP_GTCMD	0002	/* Get command status command */
#define	M_OP_GTUNT	0003	/* Get unit status command */
#define	M_OP_STCON	0004	/* Set controller characteristics command */
#define	M_OP_SEREX	0007	/* Serious exception end message */
#define	M_OP_AVAIL	0010	/* Available command */
#define	M_OP_ONLIN	0011	/* Online command */
#define	M_OP_STUNT	0012	/* Set unit characteristics command */
#define	M_OP_DTACP	0013	/* Determine access paths command */
#define	M_OP_ACCES	0020	/* Access command */
#define	M_OP_CMPCD	0021	/* Compare controller data command */
#define	M_OP_ERASE	0022	/* Erase command */
#define	M_OP_FLUSH	0023	/* Flush command */
#define	M_OP_REPLC	0024	/* Replace command */
#define	M_OP_COMP	0040	/* Compare host data command */
#define	M_OP_READ	0041	/* Read command */
#define	M_OP_WRITE	0042	/* Write command */
#define	M_OP_AVATN	0100	/* Available attention message */
#define	M_OP_DUPUN	0101	/* Duplicate unit number attention message */
#define	M_OP_ACPTH	0102	/* Access path attention message */
#define	M_OP_END	0200	/* End message flag */


/*
 * Generic command modifiers
 */
#define	M_MD_EXPRS	0100000		/* Express request */
#define	M_MD_COMP	0040000		/* Compare */
#define	M_MD_CLSEX	0020000		/* Clear serious exception */
#define	M_MD_ERROR	0010000		/* Force error */
#define	M_MD_SCCHH	0004000		/* Suppress caching (high speed) */
#define	M_MD_SCCHL	0002000		/* Suppress caching (low speed) */
#define	M_MD_SECOR	0001000		/* Suppress error correction */
#define	M_MD_SEREC	0000400		/* Suppress error recovery */
#define	M_MD_SSHDW	0000200		/* Suppress shadowing */
#define	M_MD_WBKNV	0000100		/* Write back (non-volatile) */
#define	M_MD_WBKVL	0000040		/* Write back (volatile) */
#define	M_MD_WRSEQ	0000020		/* Write shadow set one unit at a time */

/*
 * AVAILABLE command modifiers
 */
#define	M_MD_ALLCD	0000002		/* All class drivers */
#define	M_MD_SPNDW	0000001		/* Spin down */

/*
 * FLUSH command modifiers
 */
#define	M_MD_FLENU	0000001		/* Flush entire unit */
#define	M_MD_VOLTL	0000002		/* Volatile only */

/*
 * GET UNIT STATUS command modifiers
 */
#define	M_MD_NXUNT	0000001		/* Next unit */

/*
 * ONLINE command modifiers
 */
#define	M_MD_RIP	0000001		/* Allow self destruction */
#define	M_MD_IGNMF	0000002		/* Ignore media format error */

/*
 * ONLINE and SET UNIT CHARACTERISTICS command modifiers
 */
#define	M_MD_ALTHI	0000040		/* Alter host identifier */
#define	M_MD_SHDSP	0000020		/* Shadow unit specified */
#define	M_MD_CLWBL	0000010		/* Clear write-back data lost */
#define	M_MD_STWRP	0000004		/* Set write protect */

/*
 * REPLACE command modifiers
 */
#define	M_MD_PRIMR	0000001		/* Primary replacement block */


/*
 * End message flags
 */
#define	M_EF_BBLKR	0200	/* Bad block reported */
#define	M_EF_BBLKU	0100	/* Bad block unreported */
#define	M_EF_ERLOG	0040	/* Error log generated */
#define	M_EF_SEREX	0020	/* Serious exception */


/*
 * Controller flags
 */
#define	M_CF_ATTN	0200	/* Enable attention messages */
#define	M_CF_MISC	0100	/* Enable miscellaneous error log messages */
#define	M_CF_OTHER	0040	/* Enable other host's error log messages */
#define	M_CF_THIS	0020	/* Enable this host's error log messages */
#define	M_CF_MLTHS	0004	/* Multi-host */
#define	M_CF_SHADW	0002	/* Shadowing */
#define	M_CF_576	0001	/* 576 byte sectors */


/*
 * Unit flags
 */
#define	M_UF_REPLC	0100000		/* Controller initiated bad block replacement */
#define	M_UF_INACT	0040000		/* Inactive shadow set unit */
#define	M_UF_WRTPH	0020000		/* Write protect (hardware) */
#define	M_UF_WRTPS	0010000		/* Write protect (software or volume) */
#define	M_UF_SCCHH	0004000		/* Suppress caching (high speed) */
#define	M_UF_SCCHL	0002000		/* Suppress caching (low speed) */
#define	M_UF_RMVBL	0000200		/* Removable media */
#define	M_UF_WBKNV	0000100		/* Write back (non-volatile) */
#define	M_UF_576	0000004		/* 576 byte sectors */
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
#define	M_ST_MFMTE	005		/* Media format error */
#define	M_ST_WRTPR	006		/* Write protected */
#define	M_ST_COMP	007		/* Compare error */
#define	M_ST_DATA	010		/* Data error */
#define	M_ST_HSTBF	011		/* Host buffer access error */
#define	M_ST_CNTLR	012		/* Controller error */
#define	M_ST_DRIVE	013		/* Drive error */
#define	M_ST_DIAG	037		/* Message from an internal diagnostic */


typedef	short	quad[4];		/* a word-aligned quadword */

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
		long	Mscp_xxx2[2];	/* unused */
		long	Mscp_lbn;	/* logical block number */
		long	Mscp_xxx4;	/* unused */
		long	*Mscp_dscptr;	/* pointer to descriptor (software) */
		long	Mscp_sftwds[4];	/* software words, padding */
	} mscp_generic;
	struct {
		short	Mscp_version;	/* MSCP version */
		short	Mscp_cntflgs;	/* controller flags */
		short	Mscp_hsttmo;	/* host timeout */
		short	Mscp_usefrac;	/* use fraction */
		long	Mscp_time;	/* time and date */
	} mscp_setcntchar;
	struct {
		short	Mscp_multunt;	/* multi-unit code */
		short	Mscp_unitflgs;	/* unit flags */
		long	Mscp_hostid;	/* host identifier */
		quad	Mscp_unitid;	/* unit identifier */
		long	Mscp_mediaid;	/* media type identifier */
		short	Mscp_shdwunt;	/* shadow unit */
		short	Mscp_shdwsts;	/* shadow status */
		short	Mscp_track;	/* track size */
		short	Mscp_group;	/* group size */
		short	Mscp_cylinder;	/* cylinder size */
		short	Mscp_xxx3;	/* reserved */
		short	Mscp_rctsize;	/* RCT table size */
		char	Mscp_rbns;	/* RBNs / track */
		char	Mscp_rctcpys;	/* RCT copies */
	} mscp_getunitsts;
	} mscp_un;
};

/*
 * generic packet
 */

#define	mscp_bytecnt	mscp_un.mscp_generic.Mscp_bytecnt
#define	mscp_buffer	mscp_un.mscp_generic.Mscp_buffer
#define	mscp_lbn	mscp_un.mscp_generic.Mscp_lbn
#define	mscp_dscptr	mscp_un.mscp_generic.Mscp_dscptr
#define	mscp_sftwds	mscp_un.mscp_generic.Mscp_sftwds
#define	mscp_status	mscp_modifier

/*
 * Abort / Get Command Status packet
 */

#define	mscp_outref	mscp_bytecnt

/*
 * Online / Set Unit Characteristics packet
 */

#define	mscp_errlgfl	mscp_lbn
#define	mscp_copyspd	mscp_shdwsts

/*
 * Replace packet
 */

#define	mscp_rbn	mscp_bytecnt

/*
 * Set Controller Characteristics packet
 */

#define	mscp_version	mscp_un.mscp_setcntchar.Mscp_version
#define	mscp_cntflgs	mscp_un.mscp_setcntchar.Mscp_cntflgs
#define	mscp_hsttmo	mscp_un.mscp_setcntchar.Mscp_hsttmo
#define	mscp_usefrac	mscp_un.mscp_setcntchar.Mscp_usefrac
#define	mscp_time	mscp_un.mscp_setcntchar.Mscp_time

/*
 * Get Unit Status end packet
 */

#define	mscp_multunt	mscp_un.mscp_getunitsts.Mscp_multunt
#define	mscp_unitflgs	mscp_un.mscp_getunitsts.Mscp_unitflgs
#define	mscp_hostid	mscp_un.mscp_getunitsts.Mscp_hostid
#define	mscp_unitid	mscp_un.mscp_getunitsts.Mscp_unitid
#define	mscp_mediaid	mscp_un.mscp_getunitsts.Mscp_mediaid
#define	mscp_shdwunt	mscp_un.mscp_getunitsts.Mscp_shdwunt
#define	mscp_shdwsts	mscp_un.mscp_getunitsts.Mscp_shdwsts
#define	mscp_track	mscp_un.mscp_getunitsts.Mscp_track
#define	mscp_group	mscp_un.mscp_getunitsts.Mscp_group
#define	mscp_cylinder	mscp_un.mscp_getunitsts.Mscp_cylinder
#define	mscp_rctsize	mscp_un.mscp_getunitsts.Mscp_rctsize
#define	mscp_rbns	mscp_un.mscp_getunitsts.Mscp_rbns
#define	mscp_rctcpys	mscp_un.mscp_getunitsts.Mscp_rctcpys

/*
 * Online / Set Unit Characteristics end packet
 */

#define	mscp_untsize	mscp_dscptr
#define	mscp_volser	mscp_sftwds[0]

/*
 * Set Controller Characteristics end packet
 */

#define	mscp_cnttmo	mscp_hsttmo
#define	mscp_cntcmdl	mscp_usefrac
#define	mscp_cntid	mscp_unitid


/*
 * Error Log message format codes
 */
#define	M_FM_CNTERR	0	/* Controller error */
#define	M_FM_BUSADDR	1	/* Host memory access error */
#define	M_FM_DISKTRN	2	/* Disk transfer error */
#define	M_FM_SDI	3	/* SDI error */
#define	M_FM_SMLDSK	4	/* Small disk error */

/*
 * Error Log message flags
 */
#define	M_LF_SUCC	0200	/* Operation successful */
#define	M_LF_CONT	0100	/* Operation continuing */
#define	M_LF_SQNRS	0001	/* Sequence number reset */

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
	short	mslg_group;		/* group */
	long	mslg_volser;		/* volume serial number */
	long	mslg_cylinder;		/* cylinder */
	short	mslg_track;		/* track */
	short	mslg_sector;		/* sector */
	long	mslg_lbn;		/* logical block number */
	u_char	mslg_level;		/* level */
	u_char	mslg_retry;		/* retry */
};

#define	mslg_busaddr	mslg_unitid
#define	mslg_sdecyl	mslg_group
