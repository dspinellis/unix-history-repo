/*	cyreg.h	7.1	86/01/05	*/

/* get controller attention and start an operation */
#define CY_ATTENTION(addr) movob(addr, 0xff)	/* also known as: GO */
#define CY_RESET(addr)    CY_ATTENTION(addr+1) /* reset controller */
#define CYUNIT(d)	  (minor(d) & 0xf)

#define MULTIBUS_SHORT(x) (short)((((x)>>8)&0xff) | (((x)<<8)&0xff00))

#define	NORMAL_INTERUPT	0x11
#define	CLEAR_INTERUPT	0x09

#define	T_NOREWIND	0x80


/* Tape Parameter Block definitions */
typedef struct {
	long		cmd;		/* Command */
	short		control;	/* Control */
	short		count;		/* Return count */
	short		size;		/* Buffer size */
	short		rec_over;	/* Records/Overrun */
	char		*data_ptr;	/* Pointer to source/dest */
	short		status;		/* Status */
	short		link_ptr[2];	/* Pointer to next parameter block */
} fmt_tpb;

/* Controller commands */

/* Group. I Control status/commands */
#define	CONFIG	(0x00000000)	/* configure */
#define	SET_PA	(0x08000000)	/* set page */
#define	NO_OP	(0x20000000)	/* no operation */
#define	DRIVE_S	(0x28000000)	/* drive status */
#define	TAPE_AS	(0x74000000)	/* tape assign */
#define	DRIVE_R	(0x90000000)	/* drive reset */

/* Group. II Tape position commands */
#define	REWD_OV	(0x04000000)	/* rewind overlapped */
#define	READ_FO	(0x1C000000)	/* read foreign tape */
#define	REWD_TA	(0x34000000)	/* rewind tape */
#define	OFF_UNL	(0x38000000)	/* off_line and unload */
#define	WRIT_FM	(0x40000000)	/* write filemark */
#define	SERH_FM	(0x44000000)	/* search filemark */
#define	SRFM_FD	(0x44000000)	/* search filemark forward */
#define	SRFM_BK	(0xC4000000)	/* search filemark backward */
#define	SPACE	(0x48000000)	/* skip record */
#define	SP_FORW	(0x48000000)	/* space forward */
#define	SP_BACK	(0xC8000000)	/* space backwords */
#define	ERASE_F	(0x4C000000)	/* erase fixed length */
#define	ERASE_T	(0x50000000)	/* erase to end of tape */
#define	SPAC_FM	(0x70000000)	/* space filemark */
#define	SP_FM_F	(0x70000000)	/* space filemark forward */
#define	SP_FM_B	(0xC9000000)	/* space filemark backward */
#define	SERH_MU	(0x94000000)	/* search multiple filemarks */

/* Group. III Data transfer commands */
#define	READ_BU	(0x10000000)	/* read buffered */
#define	WRIT_BU	(0x14000000)	/* write buffered */
#define	EDIT_BU	(0x18000000)	/* edit buffered */
#define	READ_TA	(0x2C000000)	/* read tape */
#define	WRIT_TA	(0x30000000)	/* write tape */
#define	EDIT_TA	(0x3C000000)	/* edit tape */
#define	READ_ST	(0x60000000)	/* read streaming */
#define	WRIT_ST	(0x64000000)	/* write streaming */

/* Group. IV Special commands */
#define	EXCHANG	(0x0C000000)	/* exchange system and tapemaster RAM */
#define	BLOCK_M	(0x80000000)	/* block move */

/* Group. V Diagnostic commands */
#define	TEST_SH	(0x54000000)	/* short memory test */
#define	TEST_LG	(0x58000000)	/* long memory test */
#define	TEST_CN	(0x5C000000)	/* controller confidence test */
#define	TEST_RW	(0x68000000)	/* test read/write timeing */


/* Control field bit definitions */
#define	CW_UNIT		(0x000c<<8) /* tape select mask, 2 bit field */
#define	CW_MAIL		(0x0010<<8) /* mailbox flag */
#define	CW_INTR		(0x0020<<8) /* interrupt flag */
#define	CW_LINK		(0x0040<<8) /* link flag */
#define	CW_LOCK		(0x0080<<8) /* bus lock flag */
#define	CW_BANK		(0x0100>>8) /* bank select */
#define	CW_REV		(0x0400>>8) /* reverse flag */
#define	CW_SPEED	(0x0800>>8) /* speed/density */
#define	    CW_25ips	0
#define	    CW_100ips	(0x0800>>8)
#define	CW_STREAM  	(0x1000>>8) /* continuous */
#define	CW_WIDTH  	(0x8000>>8) /* width */
#define	    CW_8bits	0
#define	    CW_16bits	(0x8000>>8)


/* Status field bit definitions */
#define	CS_P	(0x0002<<8)	/* Protected, no write ring */
#define	CS_FB	(0x0004<<8)	/* formatter busy */
#define	CS_RDY	(0x0008<<8)	/* drive ready */
#define	CS_EOT	(0x0010<<8)	/* end of tape detected */
#define	CS_LP	(0x0020<<8)	/* tape is at load point */
#define	CS_OL	(0x0040<<8)	/* drive on_line */
#define	CS_FM	(0x0080<<8)	/* Filemark detected */
#define	CS_ERm	(0x1F00>>8)	/* Error value mask */
#define	CS_CR	(0x2000>>8)	/* Controller executed retries */
#define	CS_CC	(0x4000>>8)	/* Command Completed successfully */
#define	CS_CE	(0x8000>>8)	/* Command execution has begun */

#define	CYDS_BITS "\20\6CS_CR\7CS_CC\8CS_CE\12CS_P\13CS_FB\14CS_RDY\15CS_EOT\
\16CS_LP\17CS_OL\20CS_FM"

/* Error value definitions for CS_ERm field */
#define	ER_TIMOUT	(0x01)	/* timed out data busy false */
#define	ER_TIMOUT1	(0x02)	/* data busy false,formatter,ready */
#define	ER_TIMOUT2	(0x03)	/* time out ready busy false */
#define	ER_TIMOUT3	(0x04)	/* time out ready busy true */
#define	ER_TIMOUT4	(0x05)	/* time out data busy true */
#define	ER_NEX		(0x06)	/* time out memory */
#define	ER_BLANK	(0X07)	/* blank tape */
#define	ER_DIAG		(0x08)	/* micro-diagnostic */
#define	ER_EOT		(0x09)	/* EOT forward, BOT rev. */
#define	ER_HARD		(0x0A)	/* retry unsuccessful */
#define	ER_FIFO		(0x0B)	/* FIFO over/under flow */
#define	ER_PARITY	(0x0D)	/* drive to tapemaster parity error */
#define	ER_CHKSUM	(0x0E)	/* prom checksum */
#define	ER_STROBE	(0x0F)	/* time out tape strobe */
#define	ER_NOTRDY	(0x10)	/* tape not ready */
#define	ER_PROT		(0x11)	/* write, no enable ring */
#define	ER_JUMPER	(0x13)	/* missing diagnostic jumper */
#define	ER_LINK		(0x14)	/* bad link, link inappropriate */
#define	ER_FM		(0x15)	/* unexpected filemark */
#define	ER_PARAM	(0x16)	/* bad parameter, byte count ? */
#define	ER_HDWERR	(0x18)	/* unidentified hardware error */
#define	ER_NOSTRM	(0x19)	/* streaming terminated */


/* Channel control block definitions */
typedef struct {
	char	ccw;		/* channel control word */
	char	gate;		/* Tpb access gate */
	short	tpb_ptr[2];	/* points to first tape parameter block */
} fmt_ccb;

#define GATE_OPEN	(char)(0x00)
#define GATE_CLOSED	(char)(0xFF)
#define NORMAL_INTERUP	0x11   



/* System configuration block structrure definitions */
typedef struct {
	char	fixed_value;	/* 0x03 fixed value code */
	char	unused_scb;
	short	ccb_ptr[2];	/* pointer to ->CHANNEL CONTROL BLOCK */
} fmt_scb;


/* System configuration pointer structure definitions */
typedef struct {
	char	bus_size;	/* width of system bus 0=8; 1=16 */
	char	unused_scp;
	short	scb_ptr[2];	/* pointer to ->SYSTEM CONFIGUREATION BLOCK */
} fmt_scp;

#define	_16_BITS	1
#define	_8_BITS		0

