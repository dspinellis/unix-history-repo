/* $Header: keymatch.h,v 10.1 86/11/19 10:45:43 jg Exp $ */
/* Copyright 1985 Massachusetts Institute of Technology */

/* keymatch.h - Table to match key codes from RTPC to X default (DEC) codes 
 *
 *	Author:
 *
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *		Providence, RI 02912
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 */

unsigned char keymatch [] =
{
/* IBM key code		X Key Code */
/* 0x00 */		0000,
/* 0x01 */		0001,
/* 0x02 */		0002,
/* 0x03 */		0003,
/* 0x04 */		0004,
/* 0x05 */		0005,
/* 0x06 */		0006,
/* 0x07 */		0126,		/* F1 */
/* 0x08 */		0161,		/* ESC */
/* 0x09 */		0007,
/* 0x0A */		0010,
/* 0x0B */		0011,
/* 0x0C */		0012,
/* 0x0D */		0276,		/* Tab */
/* 0x0E */		0277,		/* ` */
/* 0x0F */		0127,		/* F2 */
/* 0x10 */		0013,
/* 0x11 */		0257,		/* Ctrl */
/* 0x12 */		0256,		/* SHIFT1 -> Shift/SHFT */
/* 0x13 */		0311,		/* ????? */
/* 0x14 */		0260,		/* Caps Lock -> Lock */
/* 0x15 */		0301,		/* q */
/* 0x16 */		0300,		/* 1 *
/* 0x17 */		0130,		/* F3 */
/* 0x18 */		0017,
/* 0x19 */		0261,		/* ALT1 -> Compose Character/SYMBOL */
/* 0x1A */		0303,		/* z */
/* 0x1B */		0307,		/* s */
/* 0x1C */		0302,		/* a */
/* 0x1D */		0306,		/* w */
/* 0x1E */		0305,		/* 2 */
/* 0x1F */		0131,		/* F4 */
/* 0x20 */		0021,
/* 0x21 */		0316,		/* c */
/* 0x22 */		0310,		/* x */
/* 0x23 */		0315,		/* d */
/* 0x24 */		0314,		/* e */
/* 0x25 */		0320,		/* 4 */
/* 0x26 */		0313,		/* 3 */
/* 0x27 */		0132,		/* F5 */
/* 0x28 */		0022,
/* 0x29 */		0324,		/* Space */
/* 0x2A */		0323,		/* v */
/* 0x2B */		0322,		/* f */
/* 0x2C */		0327,		/* t */
/* 0x2D */		0321,		/* r */
/* 0x2E */		0326,		/* 5 */
/* 0x2F */		0144,		/* F6 */
/* 0x30 */		0023,
/* 0x31 */		0336,		/* n */
/* 0x32 */		0331,		/* b */
/* 0x33 */		0335,		/* h */
/* 0x34 */		0330,		/* g */
/* 0x35 */		0334,		/* y */
/* 0x36 */		0333,		/* 6 */
/* 0x37 */		0145,		/* F7 */
/* 0x38 */		0024,
/* 0x39 */		0261,		/* ALT2 -> Compose Character/SYMBOL */
/* 0x3A */		0343,		/* m */
/* 0x3B */		0342,		/* j */
/* 0x3C */		0341,		/* u */
/* 0x3D */		0340,		/* 7 */
/* 0x3E */		0345,		/* 8 */
/* 0x3F */		0146,		/* F8 */
/* 0x40 */		0026,
/* 0x41 */		0350,		/* , */
/* 0x42 */		0347,		/* k */
/* 0x43 */		0346,		/* i */
/* 0x44 */		0353,		/* o */
/* 0x45 */		0357,		/* 0 */
/* 0x46 */		0352,		/* 9 */
/* 0x47 */		0147,		/* F9 */
/* 0x48 */		0027,
/* 0x49 */		0355,		/* . */
/* 0x4A */		0363,		/* / */
/* 0x4B */		0354,		/* l */
/* 0x4C */		0362,		/* ; */
/* 0x4D */		0360,		/* p */
/* 0x4E */		0371,		/* - */
/* 0x4F */		0150,		/* F10 */
/* 0x50 */		0030,
/* 0x51 */		0031,
/* 0x52 */		0373,		/* ' */
/* 0x53 */		0032,
/* 0x54 */		0372,		/* [ */
/* 0x55 */		0365,		/* = */
/* 0x56 */		0164,		/* F11 -> F14 */
/* 0x57 */		0202,		/* Print Screen -> F19 */
/* 0x58 */		0033,
/* 0x59 */		0256,		/* SHIFT2 -> Shift/SHFT */
/* 0x5A */		0275,		/* Return */
/* 0x5B */		0366,		/* ] */
/* 0x5C */		0367,		/* \ */
/* 0x5D */		0035,
/* 0x5E */		0200,		/* F12 -> F17 */
/* 005F */		0203,		/* Scroll Lock -> F20 */
/* 0x60 */		0251,		/* downarrow */
/* 0x61 */		0247,		/* leftarrow */
/* 0x62 */		0201,		/* Pause -> F18 */
/* 0x63 */		0252,		/* uparrow */
/* 0x64 */		0274,		/* delete -> back*/
/* 0x65 */		0175,		/* end -> Do/F16 */
/* 0x66 */		0162,		/* BS  -> F12/BS*/
/* 0x67 */		0213,		/* Insert/E2 */
/* 0x68 */		0037,
/* 0x69 */		0226,		/* R1 */
/* 0x6a */		0250,		/* rightarrow */
/* 0x6b */		0231,		/* R4 */
/* 0x6c */		0235,		/* R7 */
/* 0x6d */		0217,		/* Page Down -> Next Screen */
/* 0x6e */		0215,		/* Home -> Select ???  */
/* 0x6f */		0216,		/* Page Up -> Prev Screen  */
/* 0x70 */		0222,		/* R0 */
/* 0x71 */		0224,		/* R. */
/* 0x72 */		0227,		/* R2 */
/* 0x73 */		0232,		/* R5 */
/* 0x74 */		0233,		/* R6 */
/* 0x75 */		0236,		/* R8 */
/* 0x76 */		0234,		/* Num Lock -> R, ??? */
/* 0x77 */		0241,		/* R/ -> PF1 */
/* 0x78 */		0041,
/* 0x79 */		0225,		/* Enter */
/* 0x7a */		0230,		/* R3 */
/* 0x7b */		0042,
/* 0x7c */		0242,		/* R+ -> PF2 */
/* 0x7d */		0237,		/* R9 */
/* 0x7e */		0243,		/* R* -> PF3*/
/* 0x7f */		0043,
/* 0x80 */		0044,
/* 0x81 */		0045,
/* 0x82 */		0046,
/* 0x83 */		0047,
/* 0x84 */		0240,		/* R- */
};

#define RTPC_CODES 0x84
