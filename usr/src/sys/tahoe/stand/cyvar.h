/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Computer Consoles Inc.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)cyvar.h	1.3 (Berkeley) %G%
 */

#define TM_ATTENTION(addr,x) 	movob(addr,x)	/* also known as: GO */

#define TM_RESET(addr,x) TM_ATTENTION((addr+1),x) /* reset controller */
#define TM_SHORT(x)	(short)((((x) >> 8) & 0xff) + (((x) << 8) & 0xff00))

#define GATE_OPEN			(char)(0x00)
#define GATE_CLOSED			(char)(0xFF)

#define b_repcnt  b_bcount
#define b_command b_resid

/* Group. I Control status/commands */
#define CONFIG	(0x00000000L)	/* configure */
#define SET_PA	(0x08000000L)	/* set page */
#define NO_OP	(0x20000000L)	/* no operation */
#define DRIVE_S	(0x28000000L)	/* drive status */
#define TAPE_AS	(0x74000000L)	/* tape assign */
#define DRIVE_R	(0x90000000L)	/* drive reset */

/* Group. II Tape position commands */
#define REWD_OV	(0x04000000L)	/* rewind overlapped */
#define READ_FO	(0x1C000000L)	/* read foreign tape */
#define REWD_TA	(0x34000000L)	/* rewind tape */
#define OFF_UNL	(0x38000000L)	/* off_line and unload */
#define WRIT_FM	(0x40000000L)	/* write filemark */
#define SERH_FM	(0x44000000L)	/* search filemark */
#define SRFM_FD	(0x44000000L)	/* search filemark forward */
#define SRFM_BK	(0xC4000000L)	/* search filemark backward */
#define SPACE	(0x48000000L)	/* skip record */
#define SP_FORW	(0x48000000L)	/* space forward */
#define SP_BACK	(0xC8000000L)	/* space backwords */
#define ERASE_F	(0x4C000000L)	/* erase fixed length */
#define ERASE_T	(0x50000000L)	/* erase to end of tape */
#define SPAC_FM	(0x70000000L)	/* space filemark */
#define SERH_MU	(0x94000000L)	/* search multiple filemarks */

/* Group. III Data transfer commands */
#define READ_BU	(0x10000000L)	/* read buffered */
#define WRIT_BU	(0x14000000L)	/* write buffered */
#define EDIT_BU	(0x18000000L)	/* edit buffered */
#define READ_TA	(0x2C000000L)	/* read tape */
#define WRIT_TA	(0x30000000L)	/* write tape */
#define EDIT_TA	(0x3C000000L)	/* edit tape */
#define READ_ST	(0x60000000L)	/* read streaming */
#define WRIT_ST	(0x64000000L)	/* write streaming */

/* Group. IV Special commands */
#define EXCHANG	(0x0C000000L)	/* exchange system and tapemaster RAM */
#define BLOCK_M	(0x80000000L)	/* block move */

/* Group. V Diagnostic commands */
#define TEST_SH	(0x54000000L)	/* short memory test */
#define TEST_LG	(0x58000000L)	/* long memory test */
#define TEST_CN	(0x5C000000L)	/* controller confidence test */
#define TEST_RW	(0x68000000L)	/* test read/write timeing */
/* Control byte[0] bit assignments */
#define CW_TSm	(0x0C)	/* tape select mask, 2 bit field */
#define CW_TSs	(2)	/* tape select shift, 2 bit field <<shift */
#define CW_M	(0x10)	/* mailbox flag */
#define CW_I	(0x20)	/* interrupt flag */
#define CW_L	(0x40)	/* link flag */
#define CW_BL	(0x80)	/* bus lock flag */

/* Control byte[1] bit assignments */
#define CW_BS	(0x01)	/* bank select */
#define CW_R	(0x04)	/* reverse flag */
#define CW_SD	(0x08)	/* speed/density */
#define CW_25ips	(0x00)	/* 25 inches per second speed */
#define CW_100ips	(0x08)	/* 100 inches per second speed */
#define CW_C	(0x10)	/* continuous */
#define CW_W	(0x80)	/* width */
#define CW_8bits	(0x00)	/* width 8 bits */
#define CW_16bits	(0x80)	/* width 16 bits */

/* status byte[0] bit assignements */
#define CS_P	(0x02)	/* Protected, no write ring */
#define CS_FB	(0x04)	/* formatter busy */
#define CS_DR	(0x08)	/* drive ready */
#define CS_EOT	(0x10)	/* end of tape detected */
#define CS_LP	(0x20)	/* tape is at load point */
#define CS_OL	(0x40)	/* drive on_line */
#define CS_FM	(0x80)	/* Filemark detected */

/* status byte[1] bit assignements */
#define CS_ERm	(0x1F)	/* Command (Error) mask */
#define CS_CR	(0x20)	/* Command (Retry) */
#define CS_CC	(0x40)	/* Command (Complete) */
#define CS_CE	(0x80)	/* Command (Entered) */

/* block move control byte[0] bit assignments */
#define BM_SI	(0x01)	/* increment source address */
#define BM_SW	(0x02)	/* source width */
#define BM_DI	(0x04)	/* increment destination address */
#define BM_DW	(0x08)	/* destination width */
#define BM_M	(0x10)	/* mailbox flag */
#define BM_I	(0x20)	/* interrupt flag */
#define BM_L	(0x40)	/* link flag */
#define BM_BL	(0x80)	/* bus lock flag */

/* block move control byte[1] bit assignments */
#define BM_T	(0x01)	/* translate flag */
#define BM_S	(0x02)	/* search flag */
#define BM_NC	(0x04)	/* non_compare flag */
#define BM_TH	(0x08)	/* throttle flag */
#define BM_SL	(0x10)	/* source local flag */
#define BM_DL	(0x20)	/* destination local flag */

/* block move status bit assignments */
#define BS_ERm	(0x1F)	/* Command (Error) mask */
#define BS_HIT	(0x20)	/* found match during search */
#define BS_CC	(0x40)	/* Command (Complete) */
#define BS_CE	(0x80)	/* Command (Entered) */
/* SC_ERROR & BM_ERROR codes */
#define ER_NONE	(0x00)		/* no error */
#define ER_TO1	(0x01)		/* timed out data busy false */
#define ER_TO2	(0x02)		/* data busy false,formatter,ready */
#define ER_TO3	(0x03)		/* time out ready busy false */
#define ER_TO4	(0x04)		/* time out ready busy true */
#define ER_TO5	(0x05)		/* time out data busy true */
#define ER_TO6	(0x06)		/* time out memory */
#define ER_BLAN	(0X07)		/* blank tape */
#define ER_DIAG	(0x08)		/* micro-diagnostic */
#define ER_END	(0x09)		/* EOT forward, BOT rev. */
#define ER_HARD	(0x0A)		/* retry unsuccessful */
#define ER_FIFO	(0x0B)		/* FIFO over/under flow */
/*		(0x0C)		/* Not used */
#define ER_PARI	(0x0D)		/* drive to tapemaster parity error */
#define ER_PSUM	(0x0E)		/* prom checksum */
#define ER_TOF	(0x0F)		/* time out tape strobe */
#define ER_TRN	(0x10)		/* tape not ready */
#define ER_PRO	(0x11)		/* write, no enable ring */
/*		(0x12)		/* Not used */
#define ER_JUMP	(0x13)		/* missing diagnostic jumper */
#define ER_BLIN	(0x14)		/* bad link, link inappropriate */
#define ER_FMAR	(0x15)		/* unexpected filemark */
#define ER_PARA	(0x16)		/* bad parameter, byte count ? */
/*		(0x17)		/* Not used */
#define ER_ER	(0x18)		/* unidentified hardware error */
#define ER_STER	(0x19)		/* streaming terminated */
