/*
 *	Copyright 1984, 1985 by the Regents of the University of
 *	California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */

/* define orders given to 3270's */
#define	ORDER_SF	0x1d		/* Start Field */
#define	ORDER_SBA	0x11		/* Set Buffer Address (for output) */
#define	ORDER_IC	0x13		/* Insert Cursor (at buffer address) */
#define	ORDER_PT	0x05		/* Program Tab (absurdly complicated) */
#define	ORDER_RA	0x3c		/* Repeat next character to some addr */
#define	ORDER_EUA	0x12		/* Null out every unprotected field
					 * to some address.
					 */
#define	ORDER_YALE	0x2b		/* This is a special YALE order, which
					 * introduces YALE extended orders
					 * (like setting tabs, etc.).
					 */

#define ATTR_MASK		0xc0		/* control bits */
#define ATTR_PROT		0x20		/* protected bit */
#define ATTR_NUMERIC		0x10		/* numeric field */
#define	ATTR_AUTO_SKIP_MASK	0x30		/* mask to check auto skip */
#define	ATTR_AUTO_SKIP_VALUE	0x30		/* value to have auto skip */
#define ATTR_DSPD_MASK		0x0c		/* highlighting, etc. */
#define ATTR_DSPD_DNSPD		0x00		/* display, no select */
#define ATTR_DSPD_DSPD		0x04		/* display, select */
#define ATTR_DSPD_HIGH		0x08		/* highlighted, select */
#define ATTR_DSPD_NONDISPLAY	0x0c		/* non-display, no select */
#define ATTR_MDT		0x01		/* modified data tag */


#define CMD_ERASE_ALL_UNPROTECTED	0x0f
#define CMD_ERASE_WRITE			0x05
#define CMD_WRITE			0x01
#define	CMD_READ_MODIFIED		0x06
#define	CMD_READ_BUFFER			0x02


#define WCC_ALARM	0x04
#define WCC_RESTORE	0x02
#define WCC_RESET_MDT	0x01


#define AID_PA1		0x6c
#define AID_PA2		0x6e
#define AID_PA3		0x6b
#define AID_CLEAR	0x6d

/* Special EBCDIC characters unique to a 3270 */

#define	EBCDIC_DUP	0x1c
#define EBCDIC_FM	0x1e
