/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Tim L. Tucker.
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
 *	@(#)if_wereg.h	7.1 (Berkeley) 5/9/91
 *
 * PATCHES MAGIC                LEVEL   PATCH THAT GOT US HERE
 * --------------------         -----   ----------------------
 * CURRENT PATCH LEVEL:         1       00100
 * --------------------         -----   ----------------------
 *
 * 20 Sep 92	Barry Lustig		WD8013 16 bit mode -- enable
 *						with "options WD8013".
 */

/*
 * Western Digital 8003 ethernet/starlan adapter 
 */
 
/*
 * Memory Select Register (MSR)
 */
union we_mem_sel {
    struct memory_decode {
        u_char msd_addr:6,		/* Memory decode bits		*/
	       msd_enable:1,		/* Memory (RAM) enable		*/
	       msd_reset:1;		/* Software reset 		*/
    } msd_decode;
#define ms_addr		msd_decode.msd_addr
#define ms_enable	msd_decode.msd_enable
#define ms_reset	msd_decode.msd_reset
    u_char ms_byte;			/* entire byte			*/
};

/* 20 Sep 92*/
/*
 * LA Address Register (LAAR)
 */
union we_laar {
	struct lan_addr_reg {
		u_char addr_l19_b:1,	/* Address Line 19 for enabling    */
					/* 16 bit NIC access to shared RAM */
		unused_b:5,		/* unused (or unknown) bits        */
		lan_16_en_b:1,		/* Enables 16bit shrd RAM for LAN  */
		mem_16_en_b:1;		/* Enables 16bit shrd RAM for host */
	} laar_decode;
#define addr_l19	laar_decode.addr_l19_b
#define lan_16_en	laar_decode.lan_16_en_b
#define mem_16_en	laar_decode.mem_16_en_b
	u_char laar_byte;		/* entire byte                  */
};

/*
 * receive ring discriptor
 *
 * The National Semiconductor DS8390 Network interface controller uses
 * the following receive ring headers.  The way this works is that the
 * memory on the interface card is chopped up into 256 bytes blocks.
 * A contiguous portion of those blocks are marked for receive packets
 * by setting start and end block #'s in the NIC.  For each packet that
 * is put into the receive ring, one of these headers (4 bytes each) is
 * tacked onto the front.
 */
struct we_ring	{
	struct wer_status {		/* received packet status	*/
	    u_char rs_prx:1,		    /* packet received intack	*/
		   rs_crc:1,		    /* crc error		*/
	           rs_fae:1,		    /* frame alignment error	*/
	           rs_fo:1,		    /* fifo overrun		*/
	           rs_mpa:1,		    /* packet received intack	*/
	           rs_phy:1,		    /* packet received intack	*/
	           rs_dis:1,		    /* packet received intack	*/
	           rs_dfr:1;		    /* packet received intack	*/
	} we_rcv_status;		/* received packet status	*/
	u_char	we_next_packet;		/* pointer to next packet	*/
	u_short	we_count;		/* bytes in packet (length + 4)	*/
};

/*
 * Command word definition
 */
union we_command {
    struct command_decode {
	u_char csd_stp:1,		/* STOP!			*/
	       csd_sta:1,		/* START!			*/
               csd_txp:1,		/* Transmit packet		*/
               csd_rd:3,		/* Remote DMA command		*/
               csd_ps:2;		/* Page select			*/
    } csd_decode;
#define cs_stp		csd_decode.csd_stp
#define cs_sta		csd_decode.csd_sta
#define cs_txp		csd_decode.csd_txp
#define cs_rd		csd_decode.csd_rd
#define cs_ps		csd_decode.csd_ps
    u_char cs_byte;			/* entire command byte		*/
};

/*
 * Interrupt status definition
 */
union we_interrupt {
    struct interrupt_decode {
	u_char isd_prx:1,		/* Packet received		*/
	       isd_ptx:1,		/* Packet transmitted		*/
               isd_rxe:1,		/* Receive error		*/
               isd_txe:1,		/* Transmit error		*/
               isd_ovw:1,		/* Overwrite warning		*/
               isd_cnt:1,		/* Counter overflow		*/
               isd_rdc:1,		/* Remote DMA complete		*/
               isd_rst:1;		/* Reset status			*/
    } isd_decode;
#define is_prx		isd_decode.isd_prx
#define is_ptx		isd_decode.isd_ptx
#define is_rxe		isd_decode.isd_rxe
#define is_txe		isd_decode.isd_txe
#define is_ovw		isd_decode.isd_ovw
#define is_cnt		isd_decode.isd_cnt
#define is_rdc		isd_decode.isd_rdc
#define is_rst		isd_decode.isd_rst
    u_char is_byte;			/* entire interrupt byte	*/
};
 
/*
 * Status word definition (transmit)
 */
union wet_status {
    struct tstat {
	u_char tsd_ptx:1,		/* Packet transmitted intack	*/
	       tsd_dfr:1,		/* Non deferred transmition	*/
               tsd_col:1,		/* Transmit Collided		*/
               tsd_abt:1,		/* Transmit Aborted (coll > 16)	*/
               tsd_crs:1,		/* Carrier Sense Lost		*/
               tsd_fu:1,		/* Fifo Underrun		*/
               tsd_chd:1,		/* CD Heartbeat			*/
               tsd_owc:1;		/* Out of Window Collision	*/
    } tsd_decode;
#define ts_ptx		tsd_decode.tsd_ptx
#define ts_dfr		tsd_decode.tsd_dfr
#define ts_col		tsd_decode.tsd_col
#define ts_abt		tsd_decode.tsd_abt
#define ts_crs		tsd_decode.tsd_crs
#define ts_fu		tsd_decode.tsd_fu
#define ts_chd		tsd_decode.tsd_chd
#define ts_owc		tsd_decode.tsd_owc
    u_char ts_byte;			/* entire transmit byte		*/
};

/*
 * General constant definitions
 */

/* Bits in the REGE register */
#define	WD_MICROCHANEL	0x80	/* Microchannel bus (vs. isa) */
#define	WD_LARGERAM	0x40    /* Large RAM */
#define	WD_SOFTCONFIG   0x20	/* Soft config */
#define	WD_REVMASK	0x1e	/* Revision mask */
#define WD_ETHERNET	0x01	/* Ethernet (vs. Starlan) */

#define WD_CHECKSUM	0xFF		/* Checksum byte		*/
#define WD_PAGE_SIZE	256		/* Size of RAM pages in bytes	*/
#define WD_TXBUF_SIZE	6		/* Size of TX buffer in pages	*/
#define WD_ROM_OFFSET	8		/* i/o base offset to ROM	*/
#define WD_IO_PORTS	32		/* # of i/o addresses used	*/
#define WD_NIC_OFFSET	16		/* i/o base offset to NIC	*/

/*
 * Page register offset values
 */
#define WD_P0_COMMAND	0x00		/* Command register 		*/
#define WD_P0_PSTART	0x01		/* Page Start register		*/
#define WD_P0_PSTOP	0x02		/* Page Stop register		*/
#define WD_P0_BNRY	0x03		/* Boundary Pointer		*/
#define WD_P0_TSR	0x04		/* Transmit Status (read-only)	*/
#define WD_P0_TPSR	WD_P0_TSR	/* Transmit Page (write-only)	*/
#define WD_P0_TBCR0	0x05		/* Transmit Byte count, low  WO	*/
#define WD_P0_TBCR1	0x06		/* Transmit Byte count, high WO	*/
#define WD_P0_ISR	0x07		/* Interrupt status register	*/
#define WD_P0_RBCR0	0x0A		/* Remote byte count low     WO	*/
#define WD_P0_RBCR1	0x0B		/* Remote byte count high    WO	*/
#define WD_P0_RSR	0x0C		/* Receive status            RO	*/
#define WD_P0_RCR	WD_P0_RSR	/* Receive configuration     WO */
#define WD_P0_TCR	0x0D		/* Transmit configuration    WO */
#define WD_P0_DCR	0x0E		/* Data configuration	     WO */
#define WD_P0_IMR	0x0F		/* Interrupt masks	     WO	*/
#define WD_P1_COMMAND	0x00		/* Command register 		*/
#define WD_P1_PAR0	0x01		/* Physical address register 0	*/
#define WD_P1_PAR1	0x02		/* Physical address register 1	*/
#define WD_P1_PAR2	0x03		/* Physical address register 2	*/
#define WD_P1_PAR3	0x04		/* Physical address register 3	*/
#define WD_P1_PAR4	0x05		/* Physical address register 4	*/
#define WD_P1_PAR5	0x06		/* Physical address register 5	*/
#define WD_P1_CURR	0x07		/* Current page (receive unit)  */
#define WD_P1_MAR0	0x08		/* Multicast address register 0	*/

/*
 * Configuration constants (receive unit)
 */
#define WD_R_SEP	0x01		/* Save error packets		*/
#define WD_R_AR		0x02		/* Accept Runt packets		*/
#define WD_R_AB		0x04		/* Accept Broadcast packets	*/
#define WD_R_AM		0x08		/* Accept Multicast packets	*/
#define WD_R_PRO	0x10		/* Promiscuous physical		*/
#define WD_R_MON	0x20		/* Monitor mode			*/
#define WD_R_RES1	0x40		/* reserved...			*/
#define WD_R_RES2	0x80		/* reserved...			*/
#define	WD_R_CONFIG	(WD_R_AB)

/*
 * Configuration constants (transmit unit)
 */
#define WD_T_CRC	0x01		/* Inhibit CRC			*/
#define WD_T_LB0	0x02		/* Encoded Loopback Control	*/
#define WD_T_LB1	0x04		/* Encoded Loopback Control	*/
#define WD_T_ATD	0x08		/* Auto Transmit Disable	*/
#define WD_T_OFST	0x10		/* Collision Offset Enable	*/
#define WD_T_RES1	0x20		/* reserved...			*/
#define WD_T_RES2	0x40		/* reserved...			*/
#define WD_T_RES3	0x80		/* reserved...			*/
#define	WD_T_CONFIG	(0)

/*
 * Configuration constants (data unit)
 */
#define WD_D_WTS	0x01		/* Word Transfer Select		*/
#define WD_D_BOS	0x02		/* Byte Order Select		*/
#define WD_D_LAS	0x04		/* Long Address Select		*/
#define WD_D_BMS	0x08		/* Burst Mode Select		*/
#define WD_D_AR		0x10		/* Autoinitialize Remote	*/
#define WD_D_FT0	0x20		/* Fifo Threshold Select	*/
#define WD_D_FT1	0x40		/* Fifo Threshold Select	*/
#define WD_D_RES	0x80		/* reserved...			*/
#define	WD_D_CONFIG	(WD_D_FT1|WD_D_BMS)
#define WD_D_CONFIG16	(WD_D_FT1|WD_D_BMS|WD_D_LAS|WD_D_WTS)	/* 20 Sep 92*/

/*
 * Configuration constants (interrupt mask register)
 */
#define WD_I_PRXE	0x01		/* Packet received enable	*/
#define WD_I_PTXE	0x02		/* Packet transmitted enable	*/
#define WD_I_RXEE	0x04		/* Receive error enable		*/
#define WD_I_TXEE	0x08		/* Transmit error enable	*/
#define WD_I_OVWE	0x10		/* Overwrite warning enable	*/
#define WD_I_CNTE	0x20		/* Counter overflow enable	*/
#define WD_I_RDCE	0x40		/* Dma complete enable		*/
#define WD_I_RES	0x80		/* reserved...			*/
#define WD_I_CONFIG     (WD_I_PRXE|WD_I_PTXE|WD_I_RXEE|WD_I_TXEE)
