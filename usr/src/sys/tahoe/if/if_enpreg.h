/*	if_enpreg.h	1.1	86/07/20	*/

/*	Copyright (c) 1984 by Communication Machinery Corporation
 *
 *	This file contains material which is proprietary to
 *	Communication Machinery Corporation (CMC) and which
 *	may not be divulged without the written permission
 *	of CMC.
 *
 *	ENP-10 Ram Definition
 *
 *	3/15/85 Jon Phares
 *	Update 7/10/85 S. Holmgren
 *	ENP-10 update 7/21/85 J. Mullen
 *	ENP-20 update 8/11/85 J. Mullen
 *	Mods for CCI TAHOE system 8/14/85 J. Mullen
 * 
 */

#define K		*1024

#define ENPSIZE		(124 K)		/* VME bus space allocated to enp */
#define MINPKTSIZE	60		/* minimum ethernet packet size */

/* Note: paged window (4 K) is identity mapped by ENP kernel to provide
 * 124 K contiguous RAM (as reflected in RAM_SIZE)
 */

#define RAM_WINDOW	(128 K)
#define IOACCESS_WINDOW (512)
#define FIXED_WINDOW	(RAM_WINDOW - IOACCESS_WINDOW)
#define RAMROM_SWAP	(4 K)
#define RAM_SIZE	(FIXED_WINDOW - RAMROM_SWAP)

#define HOST_RAMSIZE	(48 K)
#define ENP_RAMSIZE	(20 K)

/* ...top of 4K local i/o space for ENP */

#ifdef notdef
typedef struct iow10 {
	char	pad1[0x81];
/* written to: causes an interrupt on the host at the vector written
   read from : returns the most significant 8 bits of the slave address */
	char	vector;
	char	pad2[0x1F];
	char	csrarr[0x1E];
	char	pad3[2];
	char	ier;		/* intr. enable reg., 0x80 == enable,0 == off*/
	char	pad4[1];
	char	tir;		/* transmit intr. (Level 4 INP autovector) */
	char	pad5[1];
	char	rir;		/* receive intr. (Level 5 INP autovector) */
	char	pad6[1];
	char	uir;		/* utility intr. (Level 1 INP autovector) */
	char	pad7[7];
	char	mapfirst4k;	/* bit 7 set means ram, clear means rom */
	char	pad8[0x11];
	char	exr;		/* exception register, see bit defines above */
	char	pad9[0xD1F];
	char	hst2enp_interrupt;	/* R or W interrupts ENP */
	char	pad10[0xFF];
	char	hst2enp_reset;	/* R or W resets ENP */
} iow10;
#endif notdef

typedef struct iow20
{
	char	pad0;	
	char	hst2enp_interrupt;
	char	pad1[510];
} iow20;


#ifdef notdef
typedef struct iow30 
{
	char	pad0;
	char	impucsr;
	char 	pad1[0x1d];
	short 	bus2mpu_interrupt;
	short	bs2enp_wsctl;
	short	bs2enp_rsctl;
	short	enp2hst_clear_intr;  /* 0x27 */
	short 	enp_rcv_intr;
	short 	enp_xmit_intr;
	short	hst2enp_interrupt;   /* 0x2d */
	short  	pad2;
	char 	pad3[0xf];
	short	bus_page;	/* Bus page register */	
	char 	pad4[0x1d];
	short	lock_ctrl;
	char 	pad5[0x1d];
	short	duart[0x10];	/* 16 duart regs */
	char 	pad6[0x1f];
	short	bus_window;
} iow30; 
#endif notdef

struct ether_addr
{
	u_char ea_addr[6];
};

#define ETHADDR		struct ether_addr
#define ETHADDR_SIZE	6		/* size of ethernet address	*/

typedef
struct ethlist
{
	int	e_listsize;		/* active addr entries */
	ETHADDR	e_baseaddr;		/* addr lance is working with */
	ETHADDR e_addrs[ 16 ];		/* possible addresses */
} ETHLIST;

typedef
struct enpstat
{
	unsigned long e_xmit_successful;	/* Successful transmissions */
	unsigned long e_mult_retry;		/* multiple retries on xmit */
	unsigned long e_one_retry;		/* single retries */
	unsigned long e_fail_retry;		/* too many retries */
	unsigned long e_deferrals;		/* transmission delayed due
						   to active medium */
	unsigned long e_xmit_buff_err;		/* xmit data chaining failed --
						   "can't happen" */
	unsigned long e_silo_underrun;		/* transmit data fetch failed */
	unsigned long e_late_coll;		/* collision after xmit */
	unsigned long e_lost_carrier;
	unsigned long e_babble;			/* xmit length > 1518 */
	unsigned long e_collision;
	unsigned long e_xmit_mem_err;
	unsigned long e_rcv_successful;		/* good receptions */
	unsigned long e_rcv_missed;		/* no recv buff available */
	unsigned long e_crc_err;		/* checksum failed */
	unsigned long e_frame_err;		/* crc error AND
						   data length != 0 mod 8 */
	unsigned long e_rcv_buff_err;		/* rcv data chain failure --
						   "can't happen" */
	unsigned long e_silo_overrun;		/* receive data store failed */
	unsigned long e_rcv_mem_err;
} ENPSTAT;

typedef struct RING
{
	short	r_rdidx;
	short	r_wrtidx;
	short	r_size;
	short	r_pad;
	int	r_slot[1];
} RING;

typedef struct RING32
{
	short	r_rdidx;
	short	r_wrtidx;
	short	r_size;
	short	r_pad;			/* to make VAXen happy */
	int	r_slot[ 32 ];
} RING32;

/*
 * 	ENP Ram data layout
 *
 *	If you don't put it here - it isn't there
 *
 */

typedef struct enpdevice {
#ifdef notdef
	char	enp_ram_rom[4 K];
#endif notdef
	union {
		char	all_ram[RAM_SIZE];
		struct {
			unsigned int	t_go;
			unsigned int	t_pstart;
		} t;
		struct {
			char	nram[RAM_SIZE - (HOST_RAMSIZE + ENP_RAMSIZE)];
			char	hram[HOST_RAMSIZE];
			char	kram[ENP_RAMSIZE];
		} u_ram;
		struct
		{
			char	pad7[ 0x100 ];	/* starts 0x1100 - 0x2000 */
			short	e_enpstate;	/* 1102 */
			short	e_enpmode;	/* 1104 */
			int	e_enpbase;	/* 1104 */
			int	e_enprun;	/* 1108 */
			unsigned short	e_intrvec;
			unsigned short	e_dummy[3];

			RING32	h_toenp;	/* 110C */
			RING32	h_hostfree;		
			RING32	e_tohost;		
			RING32 	e_enpfree;		

			ENPSTAT	e_stat;
			ETHLIST	e_netaddr;		
		} iface;
	} enp_u;
	iow20	enp_iow;
} ENPDEVICE;

#define	enp_ram		enp_u.all_ram
#define	enp_nram	enp_u.u_ram.nram
#define	enp_hram	enp_u.u_ram.hram
#define	enp_kram	enp_u.u_ram.kram
#define	enp_go		enp_u.t.t_go
#define	enp_prog_start	enp_u.t.t_pstart
#define	enp_intrvec	enp_u.iface.e_intrvec
#define enp_state	enp_u.iface.e_enpstate
#define enp_mode	enp_u.iface.e_enpmode
#define enp_base	enp_u.iface.e_enpbase
#define enp_enprun	enp_u.iface.e_enprun
#define enp_toenp	enp_u.iface.h_toenp
#define enp_hostfree	enp_u.iface.h_hostfree
#define enp_tohost	enp_u.iface.e_tohost
#define enp_enpfree	enp_u.iface.e_enpfree
#define enp_freembuf	enp_u.iface.h_freembuf
#define enp_stat	enp_u.iface.e_stat
#define enp_addr	enp_u.iface.e_netaddr

#define ENPVAL		0xff	/* value to poke in enp_iow.hst2enp_interrupt */
#define RESETVAL	0x00	/* value to poke in enp_iow.enp2hst_clear_intr */

#define INTR_ENP(addr)		addr->enp_iow.hst2enp_interrupt = ENPVAL

#if ENP == 30
#define ACK_ENP_INTR(addr)	addr->enp_iow.enp2hst_clear_intr = RESETVAL
#define IS_ENP_INTR(addr)	(addr->enp_iow.enp2hst_clear_intr&0x80)
# else
#define ACK_ENP_INTR(addr)
#define IS_ENP_INTR(addr)	( 1 )
#endif ENP == 30

#ifdef notdef
#define RESET_ENP(addr)		addr->enp_iow.hst2enp_reset = 01
# else
#define RESET_ENP(addr)
#endif notdef

#ifdef TAHOE
#define ENP_GO( addr,start )	{int v; v = start; \
			enpcopy(&v, &addr->enp_prog_start, sizeof(v) ); \
			v = 0x80800000; \
			enpcopy( &v, &addr->enp_go, sizeof(v) ); }
#else
#define ENP_GO( addr,start,intvec ) { addr->enp_prog_start = (unsigned int)(start); \
				addr->enp_intrvec = (unsigned short) intvec; \
				addr->enp_go = 0x80800000; }
#endif TAHOE

#define SPL_ENP			spl4


/*
 * state bits
 */

#define S_ENPRESET	01		/* enp is in reset state */
#define S_ENPRUN	02		/* enp is in run state */

/*
 * mode bits
 */

#define E_SWAP16		0x1		/* swap two octets within 16 */
#define E_SWAP32		0x2		/* swap 16s within 32 */
#define E_SWAPRD		0x4		/* swap on read */
#define E_SWAPWRT		0x8		/* swap on write */
#define E_DMA			0x10		/* enp does data moving */

#define E_EXAM_LIST		0x80000000	/*enp should examine addrlist */
#define E_ADDR_SUPP		0x40000000	/*enp should use supplied addr*/


/*
 * 	Download ioctl definitions
 */

#define mkioctl(type,value) (0x20000000|('type'<<8)|value)

#define ENPIOGO		mkioctl( S,1 )		/* start the enp */
#define ENPIORESET	mkioctl( S,2 )		/* reset the enp */

/*
 * 	The ENP Data Buffer Structure
 */

typedef struct BCB
{
	struct BCB *b_link;
	short	 b_stat;
	short	 b_len;
	char	*b_addr;
	short	 b_msglen;
	short	 b_reserved;
}BCB;

struct  enp_softc 
{
	struct  arpcom es_ac;           /* common ethernet structures */
	struct	ether_addr es_boardaddr;/* board ethernet address */
}; 

#define es_if           es_ac.ac_if     /* network-visible interface */
#define es_enaddr       es_ac.ac_enaddr /* hardware ethernet address */

#define	ENP_OPEN	1		/* device enpram opened	*/
#define ENP_CLOSE	2		/* device enpram closed	*/
