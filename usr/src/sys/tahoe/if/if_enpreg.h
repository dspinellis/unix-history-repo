/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)if_enpreg.h	7.2 (Berkeley) 5/8/91
 */

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
 */

#define K		*1024

struct ether_addr {
	u_char ea_addr[6];
};

typedef struct ethlist {
	int	e_listsize;		/* active addr entries */
	struct	ether_addr e_baseaddr;	/* addr lance is working with */
	struct	ether_addr e_addrs[16];	/* possible addresses */
} ETHLIST;

typedef struct {
	u_long	e_xmit_successful;	/* Successful transmissions */
	u_long	e_mult_retry;		/* multiple retries on xmit */
	u_long	e_one_retry;		/* single retries */
	u_long	e_fail_retry;		/* too many retries */
	u_long	e_deferrals;		/* xmit delayed 'cuz cable busy */
	u_long	e_xmit_buff_err;	/* xmit data chaining failed --
						   "can't happen" */
	u_long	e_silo_underrun;	/* transmit data fetch failed */
	u_long	e_late_coll;		/* collision after xmit */
	u_long	e_lost_carrier;
	u_long	e_babble;		/* xmit length > 1518 */
	u_long	e_collision;
	u_long	e_xmit_mem_err;
	u_long	e_rcv_successful;	/* good receptions */
	u_long	e_rcv_missed;		/* no recv buff available */
	u_long	e_crc_err;		/* checksum failed */
	u_long	e_frame_err;		/* crc error & data length != 0 mod 8 */
	u_long	e_rcv_buff_err;		/* rcv data chain failure --
						   "can't happen" */
	u_long	e_silo_overrun;		/* receive data store failed */
	u_long	e_rcv_mem_err;
} ENPSTAT;

typedef struct RING {
	short	r_rdidx;
	short	r_wrtidx;
	short	r_size;
	short	r_pad;
	int	r_slot[1];
} RING;

typedef struct RING32 {
	short	r_rdidx;
	short	r_wrtidx;
	short	r_size;
	short	r_pad;			/* to make VAXen happy */
	int	r_slot[32];
} RING32;

/*
 * ENP Ram data layout
 */

/*
 * Note: paged window (4 K) is identity mapped by ENP kernel to provide
 * 124 K contiguous RAM (as reflected in RAM_SIZE)
 */
#define RAM_WINDOW	(128 K)
#define IOACCESS_WINDOW (512)
#define FIXED_WINDOW	(RAM_WINDOW - IOACCESS_WINDOW)
#define RAMROM_SWAP	(4 K)
#define RAM_SIZE	(FIXED_WINDOW - RAMROM_SWAP)

#define HOST_RAMSIZE	(48 K)
#define ENP_RAMSIZE	(20 K)

typedef	struct iow20 {
	char	pad0;	
	char	hst2enp_interrupt;
	char	pad1[510];
} iow20;

struct enpdevice {
#ifdef notdef
	char	enp_ram_rom[4 K];
#endif notdef
	union {
		char	all_ram[RAM_SIZE];
		struct {
			u_int	t_go;
			u_int	t_pstart;
		} t;
		struct {
			char	nram[RAM_SIZE - (HOST_RAMSIZE + ENP_RAMSIZE)];
			char	hram[HOST_RAMSIZE];
			char	kram[ENP_RAMSIZE];
		} u_ram;
		struct {
			char	pad7[0x100];	/* starts 0x1100 - 0x2000 */
			short	e_enpstate;	/* 1102 */
			short	e_enpmode;	/* 1104 */
			int	e_enpbase;	/* 1104 */
			int	e_enprun;	/* 1108 */
			u_short	e_intrvec;
			u_short	e_dummy[3];
			RING32	h_toenp;	/* 110C */
			RING32	h_hostfree;		
			RING32	e_tohost;		
			RING32 	e_enpfree;		
			ENPSTAT	e_stat;
			ETHLIST	e_netaddr;		
		} iface;
	} enp_u;
	iow20	enp_iow;
};

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

#define ENPVAL		0xff	/* enp_iow.hst2enp_interrupt poke value */
#define RESETVAL	0x00	/* enp_iow.enp2hst_clear_intr poke value */

#define INTR_ENP(addr)		(addr->enp_iow.hst2enp_interrupt = ENPVAL)

#if ENP == 30
#define ACK_ENP_INTR(addr)	(addr->enp_iow.enp2hst_clear_intr = RESETVAL)
#define IS_ENP_INTR(addr)	(addr->enp_iow.enp2hst_clear_intr&0x80)
#endif

#ifdef notdef
#define RESET_ENP(addr)		(addr->enp_iow.hst2enp_reset = 01)
#else
#ifdef lint
#define RESET_ENP(addr)		((addr) = (addr))
#else
#define RESET_ENP(addr)
#endif lint
#endif notdef

#ifdef tahoe
#define ENP_GO(addr,start) { \
	int v = start; \
	enpcopy((u_char *)&v, (u_char *)&addr->enp_prog_start, sizeof(v) ); \
	v = 0x80800000; \
	enpcopy((u_char *)&v, (u_char *)&addr->enp_go, sizeof(v) ); \
}
#else
#define ENP_GO(addr,start,intvec ) { \
	addr->enp_prog_start = (u_int)(start); \
	addr->enp_intrvec = (u_short) intvec; \
	addr->enp_go = 0x80800000; \
}
#endif tahoe

/*
 * State bits
 */
#define S_ENPRESET	01		/* enp is in reset state */
#define S_ENPRUN	02		/* enp is in run state */

/*
 * Mode bits
 */
#define E_SWAP16	0x1		/* swap two octets within 16 */
#define E_SWAP32	0x2		/* swap 16s within 32 */
#define E_SWAPRD	0x4		/* swap on read */
#define E_SWAPWRT	0x8		/* swap on write */
#define E_DMA		0x10		/* enp does data moving */

#define E_EXAM_LIST	0x80000000	/* enp should examine addrlist */
#define E_ADDR_SUPP	0x40000000	/* enp should use supplied addr */

/*
 * Download ioctl definitions
 */
#define ENPIOGO		_IO('S',1)		/* start the enp */
#define ENPIORESET	_IO('S',2)		/* reset the enp */

/*
 * The ENP Data Buffer Structure
 */
typedef struct BCB {
	struct	BCB *b_link;
	short	 b_stat;
	short	 b_len;
	u_char	*b_addr;
	short	 b_msglen;
	short	 b_reserved;
} BCB;
