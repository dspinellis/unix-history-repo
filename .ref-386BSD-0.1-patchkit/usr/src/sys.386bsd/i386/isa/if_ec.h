/*
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
 * A lot of this was derived from if_wereg.h and 3c503.asm.
 */
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
struct ec_ring	{
	struct ecr_status {		/* received packet status	*/
	    u_char rs_prx:1,		    /* packet received intack	*/
		   rs_crc:1,		    /* crc error		*/
	           rs_fae:1,		    /* frame alignment error	*/
	           rs_fo:1,		    /* fifo overrun		*/
	           rs_mpa:1,		    /* packet received intack	*/
	           rs_phy:1,		    /* packet received intack	*/
	           rs_dis:1,		    /* packet received intack	*/
	           rs_dfr:1;		    /* packet received intack	*/
	} ec_rcv_status;		/* received packet status	*/
	u_char	ec_next_packet;		/* pointer to next packet	*/
	u_short	ec_count;		/* bytes in packet (length + 4)	*/
};

#define EC_PAGE_SIZE 256
#define EC_TXBUF_SIZE 0x06
#define EC_VMEM_OFFSET 0x20
#define EC_RXBUF_OFFSET 0x26
#define EC_RXBUF_END 0x40
#define EC_ROM_OFFSET 8
#define ETHER_ADDR_LEN 6
#define ETHER_MIN_LEN 64
#define ETHER_HDR_SIZE 14
/*
 * Share memory management parameters.
 */
#define XMIT_MTU	0x600
#define SM_TSTART_PG	0x020
#define SM_RSTART_PG	0x026
#define SM_RSTOP_PG	0x040
/*
 * Description of header of each packet in receive area of shared memory.
 */
#define EN_RBUF_STAT	0x0	/* Received frame status. */
#define EN_RBUF_NXT_PG	0x1	/* Page after this frame */
#define EN_RBUF_SIZE_LO	0x2	/* Length of this frame */
#define EN_RBUF_SIZE_HI	0x3	/* Length of this frame */
#define EN_RBUF_NHDR	0x4	/* Length of above header area */
/*
 * E33 Control registers. (base + 40x)
 */
#define E33G		0x0
#define E33G_STARTPG	0x0
#define E33G_STOPPG	0x1
#define E33G_NBURST	0x2
#define E33G_IOBASE	0x3
#define E33G_ROMBASE	0x4
#define E33G_GACFR	0x5
#define E33G_CNTRL	0x6
#define E33G_STATUS	0x7
#define E33G_IDCFR	0x8
#define E33G_DMAAH	0x9
#define E33G_DMAAL	0xa
#define E33G_VP2	0xb
#define E33G_VP1	0xc
#define E33G_VP0	0xd
#define E33G_FIFOH	0xe
#define E33G_FIFOL	0xf
/*
 * Bits in E33G_GACFR register.
 */
#define EGACFR_NORM	0x49
#define EGACFR_IRQOFF	0xc9
/*
 *  Control bits for E33G_CNTRL
 */
#define ECNTRL_RESET    0x01         /* Software reset of ASIC and 8390. */
#define ECNTRL_THIN     0x02         /* Enable thinnet interface. */
#define ECNTRL_SAPROM   0x04         /* Map Address Prom.         */
#define ECNTRL_DBLBFR   0x20         /* FIFO Configuration bit    */
#define ECNTRL_OUTPUT   0x40         /* PC->3c503 direction if set*/
#define ECNTRL_START    0x80         /* Start DMA Logic.          */
/*
 * Bits in E33G status register.
 */
#define	ESTAT_DPRDY	0x80	/* Data port of FIFO ready */
#define ESTAT_UFLW	0x40	/* Tried to read FIFO when it was empty. */
#define ESTAT_OFLW	0x20	/* Tried to write FIFO when it was full */
#define ESTAT_DTC	0x10	/* Terminal count from PC bus DMA Logic */
#define ESTAT_DIP	0x8	/* DMA in progress */
/*
 * 8390 chip registers.
 */
#define EN_CCMD		0x0	/* Chip's command register. */
#define EN0_STARTPG	0x1	/* Starting page of ring buffer. */
#define EN0_STOPPG	0x2	/* Ending page + 1 of ring buffer */
#define EN0_BOUNDARY	0x3	/* Boundary page of ring buffer */
#define EN0_TSR		0x4	/* Transmit status register. */
#define EN0_TPSR	0x4	/* Transmit starting page. */
#define EN0_TCNTLO	0x5	/* Low byte of tx byte count */
#define EN0_TCNTHI	0x6	/* High byte of tx byte count */
#define EN0_ISR		0x7	/* Interrupt status register. */
#define EN0_RSARLO	0x8	/* Remote start address reg 0 */
#define EN0_RSARHI	0x9	/* Remote start address reg 1 */
#define EN0_RCNTLO	0xa	/* Remote byte count reg */
#define EN0_RCNTHI	0xb	/* Remote byte count reg */
#define EN0_RXCR	0xc	/* RX Control reg */
#define EN0_TXCR	0xd	/* TX Control reg */
#define EN0_COUNTER0	0xd	/* Rcv alignment error counter */
#define EN0_DCFG	0xe	/* Data configuration reg */
#define EN0_COUNTER1	0xe	/* rcv CRC error counter */
#define EN0_IMR		0xf	/* Interrupt mask reg */
#define EN0_COUNTER2	0xf	/* rcv missed frame error counter */
#define EN1_PHYS	0x1	/* boards physical enet addr. */
#define EN1_CURPAG	0x7	/* current memory page. */
#define EN1_MULT	0x8	/* multicast filter mask array (8 bytes) */
/*
 * Chip commands in EN_CCMD
 */
#define ENC_STOP	0x1	/* Stop the chip. */
#define ENC_START	0x2	/* Start the chip */
#define ENC_TRANS	0x4	/* Transmit a frame. */
#define ENC_RREAD	0x8	/* Remote read. */
#define ENC_RWRITE	0x10	/* Remote write */
#define ENC_NODMA	0x20	/* No remote DMA used on this card */
#define ENC_PAGE0	0x0	/* Select page 0 of chip regs */
#define ENC_PAGE1	0x40	/* Select page 1 of chip regs */
/*
 * Commands for RX control reg
 */
#define ENRXCR_MON	0x20	/* Monitor mode (no packets rcvd) */
#define ENRXCR_PROMP	0x10	/* Promiscuous phys addresses. */
#define ENRXCR_MULTI	0x8	/* Multicast (if pass filter) */
#define ENRXCR_BCST	0x4	/* Accept broadcasts */
#define ENRXCR_BAD	0x3	/* Accept runts and bad CRC frames */
/*
 * Commands for TX control reg
 */
#define ENTXCR_LOOP	0x2	/* Set loopback mode */
/*
 * bits on the EN0_DCFG config register.
 */
#define ENDCFG_BM8	0x48	/* Set bust mode, 8 deep FIFO */
/*
 * Bits in the EN0_ISR Interrup Status Register
 */
#define ENISR_RX	0x1	/* receiver, no error */
#define ENISR_TX	0x2	/* transmitter, no error */
#define ENISR_RX_ERR	0x4	/* Receiver with error */
#define ENISR_TX_ERR	0x8	/* Transmitter with error */
#define ENISR_OVER	0x10	/* receiver overwrote the ring */
#define ENISR_COUNTERS	0x20	/* Counters need emptying. */
#define ENISR_RDC	0x40	/* Remote DMA complete. */
#define ENISR_RESET	0x80	/* Reset completed */
#define ENISR_ALL	0x3f	/* Interrupts we will enable */
/*
 *  Bits in received packet status byte and EN0_RSR
 */
#define ENPS_RXOK	0x1	/* received a good packet */
/*
 * Bits in TX status reg.
 */
#define ENTSR_PTX	0x1	/* Packet transmitted without error */
#define ENTSR_COLL	0x4	/* Collided at least once */
#define ENTSR_COLL16	0x8	/* Collided 16 times and was dropped */
#define ENTSR_FU	0x20	/* TX FIFO Underrun */

