/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ds8390.h	7.1 (Berkeley) %G%
 */

/*
 * Nominal Semidestructor DS8390 Ethernet Chip
 * Register and bit definitions
 */

/*
 * Page register offset values
 */
#define ds_cmd		0x00		/* Command register: 		*/
#define  DSCM_STOP	 0x01		/*	Stop controller		*/
#define  DSCM_START	 0x02		/*	Start controller	*/
#define  DSCM_TRANS	 0x04		/*	Transmit packet		*/
#define  DSCM_RREAD	 0x08		/*	Remote read 		*/
#define  DSCM_RWRITE	 0x10		/*	Remote write 		*/
#define  DSCM_NODMA	 0x20		/*	No Remote DMA present	*/
#define  DSCM_PG0	 0x00		/*	Select Page 0		*/
#define  DSCM_PG1	 0x40		/*	Select Page 1		*/
#define  DSCM_PG2	 0x80		/*	Select Page 2?		*/

#define ds0_pstart	0x01		/* Page Start register		*/
#define ds0_pstop	0x02		/* Page Stop register		*/
#define ds0_bnry	0x03		/* Boundary Pointer		*/

#define ds0_tsr		0x04		/* Transmit Status (read-only)	*/
#define	 DSTS_PTX	 0x01		/*  Successful packet transmit  */ 
#define	 DSTS_COLL	 0x04		/*  Packet transmit w/ collision*/ 
#define	 DSTS_COLL16	 0x04		/*  Packet had >16 collisions & fail */ 
#define	 DSTS_UND	 0x20		/*  FIFO Underrun on transmission*/ 

#define ds0_tpsr	ds0_tsr		/* Transmit Page (write-only)	*/
#define ds0_tbcr0	0x05		/* Transmit Byte count, low  WO	*/
#define ds0_tbcr1	0x06		/* Transmit Byte count, high WO	*/

#define ds0_isr		0x07		/* Interrupt status register	*/
#define	 DSIS_RX	 0x01		/*  Successful packet reception */ 
#define	 DSIS_TX	 0x02		/*  Successful packet transmission  */ 
#define	 DSIS_RXE	 0x04		/*  Packet reception  w/error   */ 
#define	 DSIS_TXE	 0x08		/*  Packet transmission  w/error*/ 
#define	 DSIS_ROVRN	 0x10		/*  Receiver overrun in the ring*/
#define	 DSIS_CTRS	 0x20		/*  Diagnostic counters need attn */
#define	 DSIS_RDC	 0x40		/*  Remote DMA Complete         */
#define	 DSIS_RESET	 0x80		/*  Reset Complete              */

#define ds0_rsar0	0x08		/* Remote start address low  WO	*/
#define ds0_rsar1	0x09		/* Remote start address high WO	*/
#define ds0_rbcr0	0x0A		/* Remote byte count low     WO	*/
#define ds0_rbcr1	0x0B		/* Remote byte count high    WO	*/

#define ds0_rsr		0x0C		/* Receive status            RO	*/
#define	 DSRS_RPC	 0x01		/*  Received Packet Complete    */

#define ds0_rcr		ds0_rsr		/* Receive configuration     WO */
#define  DSRC_SEP	 0x01		/* Save error packets		*/
#define  DSRC_AR	 0x02		/* Accept Runt packets		*/
#define  DSRC_AB	 0x04		/* Accept Broadcast packets	*/
#define  DSRC_AM	 0x08		/* Accept Multicast packets	*/
#define  DSRC_PRO	 0x10		/* Promiscuous physical		*/
#define  DSRC_MON	 0x20		/* Monitor mode			*/

#define ds0_tcr		0x0D		/* Transmit configuration    WO */
#define  DSTC_CRC	0x01		/* Inhibit CRC			*/
#define  DSTC_LB0	0x02		/* Encoded Loopback Control	*/
#define  DSTC_LB1	0x04		/* Encoded Loopback Control	*/
#define  DSTC_ATD	0x08		/* Auto Transmit Disable	*/
#define  DSTC_OFST	0x10		/* Collision Offset Enable	*/

#define ds0_rcvalctr	ds0_tcr		/* Receive alignment err ctr RO */

#define ds0_dcr		0x0E		/* Data configuration	     WO */
#define  DSDC_WTS	 0x01		/* Word Transfer Select		*/
#define  DSDC_BOS	 0x02		/* Byte Order Select		*/
#define  DSDC_LAS	 0x04		/* Long Address Select		*/
#define  DSDC_BMS	 0x08		/* Burst Mode Select		*/
#define  DSDC_AR	 0x10		/* Autoinitialize Remote	*/
#define  DSDC_FT0	 0x20		/* Fifo Threshold Select	*/
#define  DSDC_FT1	 0x40		/* Fifo Threshold Select	*/

#define ds0_rcvcrcctr	ds0_dcr		/* Receive CRC error counter RO */

#define ds0_imr		0x0F		/* Interrupt mask register   WO	*/
#define  DSIM_PRXE	 0x01		/*  Packet received enable	*/
#define  DSIM_PTXE	 0x02		/*  Packet transmitted enable	*/
#define  DSIM_RXEE	 0x04		/*  Receive error enable	*/
#define  DSIM_TXEE	 0x08		/*  Transmit error enable	*/
#define  DSIM_OVWE	 0x10		/*  Overwrite warning enable	*/
#define  DSIM_CNTE	 0x20		/*  Counter overflow enable	*/
#define  DSIM_RDCE	 0x40		/*  Dma complete enable		*/

#define ds0_rcvfrmctr	ds0_imr		/* Receive Frame error cntr  RO */


#define ds1_par0	ds0_pstart	/* Physical address register 0	*/
				/* Physical address registers 1-4 	*/
#define ds1_par5	ds0_tbcr1	/* Physical address register 5	*/
#define ds1_curr	ds0_isr		/* Current page (receive unit)  */
#define ds1_mar0	ds0_rsar0	/* Multicast address register 0	*/
				/* Multicast address registers 1-6 	*/
#define ds1_mar7	ds0_imr		/* Multicast address register 7	*/
#define ds1_curr	ds0_isr		/* Current page (receive unit)  */

#define DS_PGSIZE	256		/* Size of RAM pages in bytes	*/

/*
 * Packet receive header, 1 per each buffer page used in receive packet
 */
struct prhdr {
	u_char	pr_status;	/* is this a good packet, same as ds0_rsr */
	u_char	pr_nxtpg;	/* next page of packet or next packet */
	u_char	pr_sz0;
	u_char	pr_sz1;
};
