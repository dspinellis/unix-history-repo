/*
 * Copyright (c) 1988, 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
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
 *	@(#)espreg.h	8.2 (Berkeley) 12/14/93
 *
 * from: $Header: espreg.h,v 1.7 92/11/26 02:28:10 torek Exp $ (LBL)
 *
 * Derived from Mary Baker's devSCSIC90.c from the Berkeley
 * Sprite project, which is:
 *
 * Copyright 1988 Regents of the University of California
 * Permission to use, copy, modify, and distribute this
 * software and its documentation for any purpose and without
 * fee is hereby granted, provided that the above copyright
 * notice appear in all copies.  The University of California
 * makes no representations about the suitability of this
 * software for any purpose.  It is provided "as is" without
 * express or implied warranty.
 */

/*
 * Emulex ESP100, ESP100A, and ESP200 registers, as found on the
 * Sun-4c Sbus.
 *
 * The registers are all bytes, and all on longword boundaries.
 * Grody to the max!
 */
struct espreg {
	u_char	esp_tcl;	/* transfer count low (byte 0) (rw) */
	u_char	esp_xxx0[3];
	u_char	esp_tch;	/* transfer count high (byte 1) (rw) */
	u_char	esp_xxx1[3];
	u_char	esp_fifo;	/* fifo data register (rw) */
	u_char	esp_xxx2[3];
	u_char	esp_cmd;	/* command (rw) */
	u_char	esp_xxx3[3];
	u_char	esp_stat;	/* status (ro); scsi id (wo) */
#define	esp_id	esp_stat
	u_char	esp_xxx4[3];
	u_char	esp_intr;	/* interrupt (ro); timeout (wo) */
#define	esp_timeout esp_intr
	u_char	esp_xxx5[3];
	u_char	esp_step;	/* sequence step (ro); sync period (wo) */
#define	esp_syncperiod esp_step
	u_char	esp_xxx6[3];
	u_char	esp_fflags;	/* fifo flags (ro); sync offset (wo) */
#define	esp_syncoff esp_fflags
	u_char	esp_xxx7[3];
	u_char	esp_conf1;	/* configuration #1 (rw) */
	u_char	esp_xxx8[3];
	u_char	esp_ccf;	/* clock conversion factor (wo) */
	u_char	esp_xxx9[3];
	u_char	esp_test;	/* test (do not use) */
	u_char	esp_xxxA[3];
	u_char	esp_conf2;	/* configuration #2 (rw, ESP100A/2xx) */
	u_char	esp_xxxB[3];
	u_char	esp_conf3;	/* configuration #3 (rw, ESP-236) */
	u_char	esp_xxxC[3];
};

/* THE REST OF THESE NAMES COULD STAND TO BE SHORTENED */

/*
 * Bits in esp_cmd.  Note that the cmd register is two levels deep (see
 * Emulex documentation, p. 4-3); our typical usage is to set the command,
 * then set it again with DMA.
 * 
 * Targets will use disconnected and target mode commands; initiators will use
 * disconnected and initiator mode commands. Bit 0x40 indicates disconnected
 * mode, 0x20 target mode, and 0x10 initiator mode.  (However, everyone can
 * use the miscellaneous commands, which have none of those bits set.)
 */
#define	ESPCMD_DMA		0x80	/* flag => do DMA */

/* miscellaneous */
#define	ESPCMD_NOP		0x00	/* do nothing */
#define	ESPCMD_FLUSH_FIFO	0x01	/* flush FIFO */
#define	ESPCMD_RESET_CHIP	0x02	/* reset ESP chip */
#define	ESPCMD_RESET_BUS	0x03	/* reset SCSI bus */
/* NB: fifo flush takes time, may need delay or NOP to allow completion */

/* disconnected */
#define	ESPCMD_RESEL_SEQ	0x40	/* reselect sequence */
#define	ESPCMD_SEL_NATN		0x41	/* select without ATN sequence */
#define	ESPCMD_SEL_ATN		0x42	/* select with ATN sequence */
#define	ESPCMD_SEL_ATNS		0x43	/* select with ATN & stop seq */
#define	ESPCMD_SEL_ENA		0x44	/* enable selection/reselection */
#define	ESPCMD_SEL_DIS		0x45	/* disable selection/reselection */
#define	ESPCMD_SEL_ATN3		0x46	/* select with ATN3 sequence */

/* target state */
#define	ESPCMD_SEND_MSG		0x20	/* send message */
#define	ESPCMD_SEND_STATUS	0x21	/* send status */
#define	ESPCMD_SEND_DATA	0x22	/* send data */
#define	ESPCMD_DIS_SEQ		0x23	/* disconnect sequence */
#define	ESPCMD_TERM_SEQ		0x24	/* terminate sequence */
#define	ESPCMD_TARG_COMP	0x25	/* target command complete sequence */
#define	ESPCMD_DISCONNECT	0x27	/* disconnect */
#define	ESPCMD_RCV_MSG		0x28	/* receive message sequence */
#define	ESPCMD_RCV_CMD		0x29	/* receive command */
#define	ESPCMD_RCV_DATA		0x2a	/* receive data */
#define	ESPCMD_REC_CMD_SEQ	0x2b	/* receive command sequence */
#define	ESPCMD_STOP_DMA		0x04	/* stop DMA (see p. 4-6) */
/*	ESPCMD_TARG_ABORT	0x06	   target abort sequence */

/* initiator state */
#define	ESPCMD_XFER_INFO	0x10	/* transfer information */
#define	ESPCMD_INIT_COMP	0x11	/* initiator command complete seq */
#define	ESPCMD_MSG_ACCEPT	0x12	/* message accepted */
#define	ESPCMD_XFER_PAD		0x18	/* transfer pad (use only w/ DMA) */
#define	ESPCMD_SET_ATN		0x1a	/* set ATN */
#define	ESPCMD_RESET_ATN	0x1b	/* reset ATN */

/*
 * Bits in esp_stat.
 * Bits 3 through 7 are latched until esp_intr is read;
 * bits 0 through 2 (the phase) are not normally latched.
 * The interrupt bit is set even if interrupts are disabled.
 * Hardware or software reset, or reading esp_intr, will
 * clear the interrupt and turn off ESPSTAT_INT.
 */
#ifdef notdef
#define	ESPSTAT_INT		0x80	/* ASC interrupting processor */
#else
#define	ESPSTAT_XXX		0x80	/* rumored unreliable: use dma IP */
#endif
#define	ESPSTAT_GE		0x40	/* gross error */
#define	ESPSTAT_PE		0x20	/* parity error */
#define	ESPSTAT_ERR		0x60	/* pseudo composite */
#define	ESPSTAT_TC		0x10	/* terminal count */
#define	ESPSTAT_VGC		0x08	/* valid group code */
#define	ESPSTAT_MSG		0x04	/* MSG line from SCSI bus */
#define	ESPSTAT_CD		0x02	/* CD line from SCSI bus */
#define	ESPSTAT_IO		0x01	/* IO line from SCSI bus */
#define	ESPSTAT_PHASE		7	/* phase mask */
#define	ESPPHASE_DATA_OUT	0	/* data out */
#define	ESPPHASE_DATA_IN	1	/* data in */
#define	ESPPHASE_CMD		2	/* command */
#define	ESPPHASE_STATUS		3	/* status */
#define	ESPPHASE_MSG_OUT	6	/* message out (w.r.t. initiator) */
#define	ESPPHASE_MSG_IN		7	/* message in */

#ifdef ESP_PHASE_NAMES
/* printed as `... during %s phase' */
char	*espphases[] =
    { "data out", "data in", "command", "status",
      "impossible(4)", "impossible(5)", "message out", "message in" };
#endif

#define	ESPSTAT_BITS	"\20\10INT\7GE\6PE\5TC\4VGC\3MSG\2CD\1IO"

/*
 * Bits in esp_intr.
 */
#define	ESPINTR_SBR	0x80	/* SCSI bus reset detected */
#define	ESPINTR_ILC	0x40	/* illegal command */
#define	ESPINTR_DSC	0x20	/* target disconnected, or timeout */
#define	ESPINTR_SVC	0x10	/* a device wants bus service */
#define	ESPINTR_CMP	0x08	/* function complete */
#define	ESPINTR_RSL	0x04	/* reselected */
#define	ESPINTR_SAT	0x02	/* selected with ATN */
#define	ESPINTR_SEL	0x01	/* selected (no ATN) */

#define	ESPINTR_BITS "\20\10SBR\7ILC\6DSC\5SVC\4CMP\3RSL\2SAT\1SEL"

/*
 * Formula for select/reselect timeout (esp_timeout).
 *	TU = 7682 * CCF * TCP
 *	T / TU = register value
 *	CCF = clock conversion factor
 *	TCP = input clock period (in same units as T)
 *	TU = time unit (i.e., the esp_timeout register counts in TUs)
 *	T = desired timeout
 * (i.e., we want ceil(timeout / (7682*ccf*tcp))).  If timeout is in ms.,
 * and tcp is in MHz, then (ccf * 7682)/tcp gives us 1000*TU, and
 * 1000*timeout/(1000*TU) gives us our result (but remember to round up).
 *
 * N.B.: The register value 0 gives a TU of 256.
 */
#define	ESPTIMO_REGVAL(timo_ms, ccf, mhz) \
	howmany(1000 * (timo_ms), ((ccf) * 7682) / (mhz))

/*
 * Value in esp_step.  These tell us how much of a `sequence' completed,
 * and apply to the following sequenced operations:
 *  [initiator]
 *	select without ATN
 *	select with ATN
 *	select with ATN3
 *	select with ATN and stop
 *  [target]
 *	bus-initiated select with ATN
 *	bus-initiated select
 *	receive command sequence
 *	command complete sequence
 *	disconnect sequence
 *	terminate sequence
 * The actual values are too complicated to define here, except that
 * code 4 always means `everything worked and the command went out'
 * (and is thus typical for everything except ATN-and-stop).
 */
#define	ESPSTEP_MASK		0x07	/* only these bits are valid */
#define	ESPSTEP_DONE		4	/* command went out */

/*
 * Synchronous transfer period (esp_syncperiod, 5 bits).
 * The minimum clocks-per-period is 5 and the max is 35;
 * the default on reset is 5.  Note that a period value of 4
 * actually gives 5 clocks.
 */
#define	ESP_CLOCKS_TO_PERIOD(nclocks) ((nclocks) & 31)

/*
 * Bits in fifo flags (esp_fflags) register.  The FIFO itself
 * is only 16 bytes, so the byte count fits in 5 bits.  Normally
 * a copy of the sequence step register appears in the top 3 bits,
 * but in test mode the chip re-uses one of those for a synchronous
 * offset bit; in any case, they are pretty much worthless.
 *
 * Note that the fifo flags register must not be read while the
 * fifo is changing.
 */
#define	ESP_NFIFO(fflags)	((fflags) & 0x1f)

#define	ESPFFLAGS_TM_SOFFNZ	0x20	/* nonzero sync offset (test mode) */

/*
 * Bits in esp_conf1.
 */
#define	ESPCONF1_SLOW_CABLE	0x80	/* ``slow cable'' mode */
#define	ESPCONF1_REPORT		0x40	/* disable reporting of interrupts
					   from scsi bus reset command */
#define	ESPCONF1_PARTST		0x20	/* parity test mode */
#define	ESPCONF1_PARENB		0x10	/* enable parity */
#define	ESPCONF1_TEST		0x08	/* chip test mode */
#define	ESPCONF1_ID_MASK	0x07	/* SCSI bus ID field */

#define	ESPCONF1_BITS	"\20\10SLOW_CABLE\7REPORT\6PARTST\5PARENB\4TEST"

/*
 * Values for clock conversion factor (esp_ccf).
 */
#define	ESPCCF_FROMMHZ(mhz)	(((mhz) + 4) / 5)
#define	ESPCCF_MIN		2	/* minimum CCF value */

/*
 * Bits in esp_test (for board testing only; can only be used in test mode).
 */
#define	ESPTEST_MBZ		0xf8	/* reserved; must be 0 */
#define	ESPTEST_TRISTATE	0x04	/* all output pins tristated */
#define	ESPTEST_INITIATOR	0x02	/* operate as initiator */
#define	ESPTEST_TARGET		0x01	/* operate as target */

/*
 * Bits in esp_conf2.
 */
#define	ESPCONF2_RSVD		0xe0	/* reserved */
#define	ESPCONF2_TRISTATE_DMA	0x10	/* tristate the DMA REQ pin */
#define	ESPCONF2_SCSI2		0x08	/* enable SCSI 2 (see p. 4-18) */
#define	ESPCONF2_TBPA		0x04	/* enable target bad parity abort */
#define	ESPCONF2_RPE		0x02	/* register parity ena (ESP2xx only) */
#define	ESPCONF2_DPE		0x01	/* DMA parity enable (ESP2xx only) */
