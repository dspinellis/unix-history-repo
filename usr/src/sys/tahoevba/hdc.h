/*
 *  Include file for HCX Disk Controller (HDC).
 *
 *	%W% (Berkeley) %G%
 */

#define	HDC_READ	0
#define	HDC_WRITE	1
#define	HID_HDC		0x01		/* hvme_id for HDC */
#define	HDC_MID		HID_HDC		/* module id code for hdc's */
#define	HDC_MAXBUS	2		/* max# buses */
#define	HDC_DEFBUS	1		/* we only handle bus #1 */
#define	HDC_MAXCTLR	21		/* max# hdc controllers per bus */
#define	HDC_MAXDRIVE	4		/* max# drives per hdc controller */
#define	HDC_UNIT(x)	(minor(x)>>3)	/* the hdc unit number (0-31) */
#define	HDC_PART(x)	(minor(x)&0x07)	/* the hdc partition number (0-7) */
#define	HDC_SPB		2		/* sectors per block for hdc's */
#define	HDC_REMOVABLE	80		/* lowest model# for removable disks */
#define	HDC_PHIO_SIZE	256		/* lword size of physical io buffer */
#define	HDC_VDATA_SIZE	16		/* vendor data size (long words) */
#define	HDC_MAXCHAIN	33		/* maximum number of data chains */
#define	HDC_MAXBC	64*1024		/* maximum byte count per data chain */
#define	HDC_MAXMCBS	32		/* max# mcb's the hdc can handle */
#define	HDC_MAXFLAWS	8000		/* max number of flaws per hdc disk */
					/* io to an hdc register */
#define	HDC_REGISTER(x)	(hc->registers->x)
					/* number of blocks per dump record */
#define	HDC_DUMPSIZE	(HDC_MAXBC/DEV_BSIZE*HDC_MAXCHAIN)

/*
 * The following buf structure defines are used by the hdc handler.  These
 * are required since the handler initiates strategy calls; these calls
 * require more function codes than just read/write, and they like to
 * directly specify the cyl/head/sector.  Note that b_upte and B_NOT1K are
 * never used by the handler.
 */
#define	B_LOCALIO	B_NOT1K
#define	b_hdccommand	b_upte[0]
#define	b_cyl		b_upte[1]
#define	b_head		b_upte[2]
#define	b_sector	b_upte[3]

/*
 * These are the 4 hdc i/o register addresses.
 *
 * Writing to "master_mcb_reg" tells the hdc controller where the master
 * mcb is and initiates hdc operation. The hdc then reads the master mcb
 * and all new mcb's in the active mcb queue.
 *
 * Writing to "module_id_reg" causes the hdc to return the hdc's module id
 * word in the location specified by the address written into the register.
 */
typedef struct {
	u_long	master_mcb_reg,		/* set the master mcb address */
		module_id_reg,		/* returns hdc's module id (hdc_mid) */
		soft_reset_reg,		/* a write here shuts down the hdc */
		hard_reset_reg;		/* send a system reset to the hdc */
} hdc_regs_type;

/*
 * Definition for the module id returned by the hdc when "module_id_reg"
 * is written to. The format is defined by the hdc microcode.
 */
typedef struct {
	u_char	module_id,	/* module id; hdc's return HDC_MID */
		reserved,
		code_rev,	/* micro-code rev#; FF= not loaded */
		fit;		/* FIT test result; FF= no error */
} hdc_mid_type;

/*
 * This structure defines the mcb's. A portion of this structure is
 * used only by the software.  The other portion is set up by software
 * and sent to the hdc firmware to perform an operation; the order
 * of this part of the mcb is determined by the controller firmware.
 *
 * "forw_mcb" and "back_mcb" form a doubly-linked list of mcb's.
 *
 * "context" is the software context word. The hdc firmware copies the
 * the contents of this word to the master mcb whenever the mcb has been
 * completed.  Currently the virtual address of the mcb is saved here.
 *
 * "forw_phaddr" forms a linked list of mcbs.  The addresses are physical
 * since they are used by the hdc firmware.
 *
 * Bits in device control word #1 define the hdc command and
 * control the operation of the hdc.
 *
 * Bits in device control word #2 define the disk sector address
 * for the operation defined in dcw1.
 */
typedef struct {
	long	lwc,			/* long word count & data chain bit */
		ta;			/* transfer address */
} data_chain_type;

#define	LWC_DATA_CHAIN	0x80000000	/* mask for data chain bit in lwc */

typedef struct {
	struct mcb_struct		/* this part used only by software */
		*forw_mcb,		/* pointer to next mcb in chain */
		*back_mcb;		/* pointer to previous mcb in chain */
	struct buf	*buf_ptr;	/* ptr to buf structure for this mcb */
	long	mcb_phaddr;		/* phaddr of hw's part of this mcb */

					/* this part is sent to the hdc hw */
	u_long	forw_phaddr;		/* phys address of next mcb */
	u_int	priority  :  8,		/* device control word #1 */
		interrupt :  1,		/*        "               */
		drive     :  7,		/*        "               */
		command   : 16,		/*        "   (see HCMD_) */
		cyl       : 13,		/* device control word #2 */
		head      :  9,		/*        "               */
		sector    : 10;		/*        "               */
	u_long	reserved[2],
		context;		/* software context word */
					/* data chain and lword count */
	data_chain_type	chain[HDC_MAXCHAIN];
} mcb_type;

					/* defines for the "command"s */
#define	HCMD_STATUS	0x40		/* command: read drive status */
#define	HCMD_READ	0x60		/* command: read data */
#define	HCMD_VENDOR	0x6a		/* command: read vendor data */
#define	HCMD_VERIFY	0x6d		/* command: verify a track */
#define	HCMD_WRITE	0x70		/* command: write data */
#define	HCMD_FORMAT	0x7e		/* command: format a track */
#define	HCMD_CERTIFY	0x7f		/* command: certify a track */
#define	HCMD_WCS	0xd0		/* command: write control store */

/*
 * This structure defines the master mcb - one per hdc controller.
 * The order of this structure is determined by the controller firmware.
 * "R" and "W" indicate read-only and write-only.
 *
 * Bits in the module control long word, "mcl", control the invocation of
 * operations on the hdc.
 *
 * The hdc operates in queued mode or immediate mode.  In queued mode, it
 * grabs new mcb's, prioritizes them, and adds them to its queue; it knows
 * if we've added any mcb's by checking forw_phaddr to see if any are
 * linked off of there.
 *
 * Bits in the master mcb's status word, "mcs", indicate the status
 * of the last-processed mcb.  The MCS_ definitions define these bits.
 * This word is set to zero when the mcb queue is passed to the hdc
 * controller; the hdc controller then sets bits in this word.
 * We cannot modify the mcb queue until the hdc has completed an mcb
 * (the hdc sets the MCS_Q_DONE bit).
 *
 * The "context" word is copied from the context word of the completed
 * mcb.  It is currently the virtual pointer to the completed mcb.
 */
typedef struct {
	u_long	mcl,			/* W  module control lword (MCL_) */
		interrupt,		/* W  interrupt acknowledge word */
		forw_phaddr,		/* W  physical address of first mcb */
		reserve1, reserve2,
		mcs,			/* R  status for last completed mcb */
		cmcb_phaddr,		/* W  physical addr of completed mcb */
		context,		/* R  software context word */
#define	HDC_XSTAT_SIZE	128		/* size of extended status (lwords) */
		xstatus[HDC_XSTAT_SIZE];/* R  xstatus of last mcb */
} master_mcb_type;

					/* definition of master mcb "mcl" */
#define	MCL_QUEUED	0x00000010	/* start queued execution of mcb's */
#define	MCL_IMMEDIATE	0x00000001	/* start immediate xqt of an mcb */

					/* definition of master mcb "mcs" */
#define	MCS_DONE	0x00000080	/* an mcb is done; status is valid */
#define	MCS_FATALERROR	0x00000002	/* a fatal error occurred */
#define	MCS_SOFTERROR	0x00000001	/* a recoverable error occurred */

/*
 * This structure defines the information returned by the hdc controller for
 * a "read drive status" (HCMD_STATUS) command.  The format of this structure
 * is determined by the hdc firmware.  r[1-11] are reserved for future use.
 */
typedef struct {
	u_long	drs,			/* drive status (see DRS_ below) */
		r1, r2, r3;
	u_short	max_cyl,		/* max logical cylinder address */
		max_head,		/* max logical head address */
		r4,
		max_sector,		/* max logical sector address */
		def_cyl,		/* definition track cylinder address */
		def_cyl_count,		/* definition track cylinder count */
		diag_cyl,		/* diagnostic track cylinder address */
		diag_cyl_count,		/* diagnostic track cylinder count */
		max_phys_cyl,		/* max physical cylinder address */
		max_phys_head,		/* max physical head address */
		r5,
		max_phys_sector,	/* max physical sector address */
		r6,
		id,			/* drive id (drive model) */
		r7,
		bytes_per_sec,		/* bytes/sector -vendorflaw conversn */
		r8,
		rpm;			/* disk revolutions per minute */
	u_long	r9, r10, r11;
} drive_stat_type;

					/* defines for drive_stat drs word */
#define	DRS_FAULT	0x00000080	/* drive is reporting a fault */
#define	DRS_RESERVED	0x00000040	/* drive is reserved by other port */
#define	DRS_WRITE_PROT	0x00000020	/* drive is write protected */
#define	DRS_ON_CYLINDER	0x00000002	/* drive heads are not moving now */
#define	DRS_ONLINE	0x00000001	/* drive is available for operation */

#ifdef COMPAT_42
#define	GB_ID		"geometry"
#define	GB_ID_LEN 	sizeof(GB_ID)-1
#define	GB_MAXPART	8
#define	GB_VERSION	1

#define	HDC_DEFPART	GB_MAXPART-1	/* partition# of def and diag cyls */
#define	BPS		512		/* bytes per sector */

/*
 * Geometry Block:
 *
 * The geometry block defines partition offsets and information about the
 * flaw maps on the flaw map track.  It resides on the first sector of the
 * flaw map track.  This structure is also used by vddc disk controllers.
 * In this case, the block resides at sector 0 of the disk.
 *
 * The geometry_sector structure defines the sector containing the geometry
 * block.  This sector is checksumed independent of the geometry information.
 * The fields in these structured which should never be moved are the id and
 * version fields in the geometry_block structure and the checksum field in
 * the geometry_sector structure.  This will provide for easy extensions in
 * the future.
 */

#define	DRIVE_TYPE	flaw_offset	/* For VDDC Geometry Blocks Only */

/* partition Definition structure */
typedef struct {
	long	start,		/* starting 1K block number for partition */
		length;		/* partition size in 1K blocks */
} par_tab;

typedef struct {
	char id[GB_ID_LEN];		/* identifies the geometry block */
	long	version,		/* geometry block version number */
		flaw_offset,		/* flaw map byte offset in partition7 */
		flaw_size,		/* harris flaw map size in bytes */
		flaw_checksum,		/* sum of bytes in harris flaw map */
		unused[3];		/* --- available for use */
	par_tab partition[GB_MAXPART];	/* partition definitions */
} geometry_block;

typedef struct {
	geometry_block	geometry_block;	/* disk geometry */
	char		filler[BPS - sizeof(geometry_block) - sizeof(long)];
	long		checksum;	/* sector checksum */
} geometry_sector;

/*
 * GB_CHECKSUM:
 *
 * This macro computes the checksum for the geometry sector and returns the
 * value.  Input to this macro is a pointer to the geometry_sector.
 */
#define GB_CHECKSUM(_gs_ptr, _checksum) { \
	register u_char *_ptr; \
	register u_long _i, _xsum; \
	_xsum = 0; \
	_ptr = (u_char *)(_gs_ptr); \
	for (_i = 0; _i < (sizeof(geometry_sector) - sizeof(long)); _i++) \
		_xsum += * _ptr++; \
	_checksum = _xsum; \
}
#endif /* COMPAT_42 */

/* hdc controller structure */
typedef struct {
	int		ctlr;		/* controller number (0-15) */
	hdc_regs_type	*registers;	/* base address of hdc io registers */
#ifdef HDC_STANDALONE
	hdc_mid_type	mid;		/* the module id is read to here */
	master_mcb_type master_mcb;	/* the master mcb for this hdc */
	mcb_type	mcb;		/* mcb for this hdc */
#else
	mcb_type	*forw_active,	/* doubly linked list of */
			*back_active,	/* .. active mcb's */
			*forw_free,	/* doubly linked list of */
			*back_free,	/* .. free mcb's */
			*forw_wait,	/* doubly linked list of */
			*back_wait;	/* .. waiting mcb's */
	hdc_mid_type	mid;		/* the module id is read to here */
	long		master_phaddr;	/* physical address of master mcb */
	master_mcb_type master_mcb;	/* the master mcb for this hdc */
	mcb_type	mcbs[HDC_MAXMCBS];/* pool of mcb's for this hdc */
#endif
} hdc_ctlr_type;

/*
 * hdc unit table. It contains information specific to each hdc drive.
 * Some information is obtained from the profile prom and geometry block.
 */
typedef struct {
#ifdef COMPAT_42
	par_tab	partition[GB_MAXPART];	/* partition definitions */
#endif
	struct disklabel dklabel;	/* pack label */
	int		ctlr,		/* the controller number (0-15) */
			slave,		/* the slave number (0-4) */
			unit,		/* the unit number (0-31) */
			id,		/* identifies the disk model */
			spc,		/* sectors per cylinder */
			cylinders,	/* number of logical cylinders */
			heads,		/* number of logical heads */
			sectors,	/* number of logical sectors/track */
			phys_cylinders,	/* number of physical cylinders */
			phys_heads,	/* number of physical heads */
			phys_sectors,	/* number of physical sectors/track */
			def_cyl,	/* logical cylinder of drive def */
			def_cyl_count,	/* number of logical def cylinders */
			diag_cyl,	/* logical cylinder of diag area */
			diag_cyl_count,	/* number of logical diag cylinders */
			rpm,		/* disk rpm */
			bytes_per_sec,	/* bytes/sector -vendorflaw conversn */
			format;		/* TRUE= format program is using dsk */
#ifndef HDC_STANDALONE
	mcb_type	phio_mcb;	/* mcb for handler physical io */
	struct buf	phio_buf;	/* buf for handler physical io */
#endif
					/* data for physical io */
	u_long		phio_data[HDC_PHIO_SIZE];
#ifndef HDC_STANDALONE
	struct buf	raw_buf;	/* buf structure for raw i/o */
#endif
} hdc_unit_type;
