/*	rsp.h	4.2	83/06/01	*/

/*
 * TU58 transfer protocol constants and data structures
 */

/*
 * Structure of a command packet
 */
struct packet {
	u_char	pk_flag;	/* indicates packet type (cmd, data, etc.) */
	u_char	pk_mcount;	/* length of packet (bytes) */
	u_char	pk_op;		/* operation to perform (read, write, etc.) */
	u_char	pk_mod;		/* modifier for op or returned status */
	u_char	pk_unit;	/* unit number */
	u_char	pk_sw;		/* switches */
	u_short	pk_seq;		/* sequence number, always zero */
	u_short	pk_count;	/* requested byte count for read or write */
	u_short	pk_block;	/* block number for read, write, or seek */
	u_short	pk_chksum;	/* checksum, by words with end around carry */
};

/*
 * State information
 */
struct tu {
	u_char	*tu_rbptr;	/* pointer to buffer for read */
	int	tu_rcnt;	/* how much to read */
	u_char	*tu_wbptr;	/* pointer to buffer for write */
	int	tu_wcnt;	/* how much to write */
	int	tu_state;	/* current state of tansfer operation */
	int	tu_flag;	/* read in progress flag */
	char	*tu_addr;	/* real buffer data address */
	int	tu_count;	/* real requested count */
	int	tu_serrs;	/* count of soft errors */
	int	tu_cerrs;	/* count of checksum errors */
	int	tu_herrs;	/* count of hard errors */
	char    tu_dopen[2];	/* drive is open */
};

/*
 * States
 */
#define	TUS_INIT1	0	/* sending nulls */
#define	TUS_INIT2	1	/* sending inits */
#define	TUS_IDLE	2	/* initialized, no transfer in progress */
#define	TUS_SENDH	3	/* sending header */
#define	TUS_SENDD	4	/* sending data */
#define	TUS_SENDC	5	/* sending checksum */
#define	TUS_SENDR	6	/* sending read command packet */
#define	TUS_SENDW	7	/* sending write command packet */
#define	TUS_GETH	8	/* reading header */
#define	TUS_GETD	9	/* reading data */
#define	TUS_GETC	10	/* reading checksum */
#define	TUS_GET		11	/* reading an entire packet */
#define	TUS_WAIT	12	/* waiting for continue */

#define	TUS_NSTATES	13

#define	printstate(state) \
	if ((state) < TUS_NSTATES) \
		printf("%s", tustates[(state)]); \
	else \
		printf("%d", (state));

/*
 * Packet Flags
 */
#define	TUF_DATA	1		/* data packet */
#define	TUF_CMD		2		/* command packet */
#define	TUF_INITF	4		/* initialize */
#define	TUF_CONT	020		/* continue */
#define	TUF_XOFF	023		/* flow control */

/*
 * Op Codes
 */
#define	TUOP_NOOP	0		/* no operation */
#define	TUOP_INIT	1		/* initialize */
#define	TUOP_READ	2		/* read block */
#define	TUOP_WRITE	3		/* write block */
#define	TUOP_SEEK	5		/* seek to block */
#define TUOP_DIAGNOSE	7		/* run micro-diagnostics */
#define	TUOP_END	0100		/* end packet */

/*
 * Mod Flags
 */
#define TUMD_WRV        1               /* write with read verify */

/*
 * Switches
 */
#define	TUSW_MRSP	010		/* use Modified RSP */

