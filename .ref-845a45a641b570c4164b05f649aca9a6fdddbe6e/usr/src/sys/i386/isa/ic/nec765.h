/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nec765.h	7.1 (Berkeley) %G%
 */

/*
 * Nec 765 floppy disc controller definitions
 */

/* Main status register */
#define NE7_DAB	0x01	/* Diskette drive A is seeking, thus busy */
#define NE7_DBB	0x02	/* Diskette drive B is seeking, thus busy */
#define NE7_CB	0x10	/* Diskette Controller Busy */
#define NE7_NDM	0x20	/* Diskette Controller in Non Dma Mode */
#define NE7_DIO	0x40	/* Diskette Controller Data register I/O */
#define NE7_RQM	0x80	/* Diskette Controller ReQuest for Master */

/* Status register ST0 */
#define NE7_ST0BITS	"\020\010invalid\007abnormal\006seek complete\005drive check\004drive_rdy\003top head\002unit_sel1\001unit_sel0"

/* Status register ST1 */
#define NE7_ST1BITS	"\020\010end_of_cyl\006bad_crc\005data_overrun\003sec_not_fnd\002write_protect\001no_am"

/* Status register ST2 */
#define NE7_ST2BITS	"\020\007control_mark\006bad_crc\005wrong_cyl\004scan_equal\003scan_not_found\002bad_cyl\001no_dam"

/* Status register ST3 */
#define NE7_ST3BITS	"\020\010fault\007write_protect\006drdy\005tk0\004two_side\003side_sel\002unit_sel1\0012unit_sel0"

/* Commands */
#define NE7CMD_SENSED	2	/*  sense drive - requires unit select byte */
#define NE7CMD_SPECIFY	3	/*  specify drive parameters - requires unit
					parameters byte */
#define NE7CMD_WRITE	5	/*  write - requires eight additional bytes */
#define NE7CMD_READ	6	/*  read - requires eight additional bytes */
#define NE7CMD_RECAL	7	/*  recalibrate drive - requires
					unit select byte */
#define NE7CMD_SENSEI	8	/*  sense controller interrupt status */
#define NE7CMD_SEEK	15	/*  seek drive - requires unit select byte
					and new cyl byte */
