#ifndef HPKEYS_H
#define HPKEYS_H
/* $Header: /host/kaukau/disk2/X11R5/R5-hp300/mit/server/ddx/hpbsd/input/RCS/hpkeys.h,v 1.1 1992/09/30 03:14:10 root Exp $ */
/*

Copyright (c) 1986, 1987 by Hewlett-Packard Company
Copyright (c) 1986, 1987 by the Massachusetts Institute of Technology

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

HEWLETT-PACKARD MAKES NO WARRANTY OF ANY KIND WITH REGARD
TO THIS SOFWARE, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED 
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
PURPOSE.  Hewlett-Packard shall not be liable for errors 
contained herein or direct, indirect, special, incidental or 
consequential damages in connection with the furnishing, 
performance, or use of this material.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*/
/***********************************************************************
 *
 * file: hpkeys.h
 *
 * contains key definitions and other static information used by x_hil.c
 *
 */
 
#define	DOWN	0
#define	LEFT	1
#define	RIGHT	2
#define	UP	3

#ifdef __apollo
u_char	cursor_down  		= 0x7b;
u_char	cursor_left  		= 0x61;
u_char	cursor_right 		= 0x63;
u_char	cursor_up    		= 0x49;
u_char	button_1		= 0x44;
u_char	button_2		= 0x45;
u_char	button_3		= 0x46;
u_char	button_4		= 0x47;
u_char	button_5		= 0x5d;
u_char	button_6		= 0x00;
u_char	button_7		= 0x00;
u_char	button_8		= 0x00;
#else
u_char	cursor_down  		= 0x1a;
u_char	cursor_left  		= 0x18;
u_char	cursor_right 		= 0x1c;
u_char	cursor_up    		= 0x12;
u_char	button_1     		= 0x1d;
u_char	button_2     		= 0x19;
u_char	button_3     		= 0x1b;
u_char	button_4     		= 0x1f;
u_char	button_5     		= 0x15;
u_char	button_6     		= 0x00;
u_char	button_7     		= 0x00;
u_char	button_8     		= 0x00;
#endif

u_short	pointer_move       		= 10;
u_short	pointer_mod1_amt    		= 1;
u_short	pointer_mod2_amt    		= 40;
u_short	pointer_mod3_amt    		= 5;
u_char	pointer_amt_mods[3]		= {0xff,0xff,0xff};
u_char	pointer_key_mods[3]		= {0xff,0xff,0xff};
u_char	borrow_mode_mods[3] 		= {0x43,0x5e,0xff};
#ifdef __apollo
u_char	borrow_mode   			= 0x15;
u_char	reset_mods[3] 			= {0x43,0x5e,0xff};
u_char	reset          			= 0x19;
#endif
#if defined(__hpux) || defined(__hp_osf) || defined(hp9000)
u_char	reset_mods[3] 			= {0x06,0x05,0xff};
u_char	reset          			= 0x0f;
#endif
u_char	screen_change_amt		= SCREEN_CHANGE_DEFAULT;
u_char	ptr_button_map[]		=  {0,1,2,3,4,5,6,7,8};
u_char	button_latching			= LATCHING_OFF;
u_char	button_chording			= CHORDING_DEFAULT;
u_char	screen_orientation		= HORIZONTAL;
u_char	screen_row_wrap			= DEFAULT;
u_char	screen_col_wrap			= DEFAULT;
u_int	tablet_xorigin			= 0;
u_int	tablet_yorigin			= 0;
u_int	tablet_width			= 0;
u_int	tablet_height			= 0;
u_char	isotropic_scaling 		= 0;

u_char ascii_to_code [128][7] = {
    {0x0c,0x0a,0x7a,0x7b,0x0b,0x0d,0},		/* cntl - @ */
    {0x0c,0x5a,0x5b,0x0d,0,0,0},		/* cntl - a */
    {0x0c,0x30,0x31,0x0d,0,0,0},		/* cntl - b */
    {0x0c,0x34,0x35,0x0d,0,0,0},		/* cntl - c */
    {0x0c,0x56,0x57,0x0d,0,0,0},		/* cntl - d */
    {0x0c,0x68,0x69,0x0d,0,0,0},		/* cntl - e */
    {0x0c,0x54,0x55,0x0d,0,0,0},		/* cntl - f */
    {0x0c,0x52,0x53,0x0d,0,0,0},		/* cntl - g */
    {0x0c,0x50,0x51,0x0d,0,0,0},		/* cntl - h */
    {0x0c,0xc0,0xc1,0x0d,0,0,0},		/* cntl - i */
    {0x0c,0xd0,0xd1,0x0d,0,0,0},		/* cntl - j */
    {0x0c,0xd2,0xd3,0x0d,0,0,0},		/* cntl - k */
    {0x0c,0xd4,0xd5,0x0d,0,0,0},		/* cntl - l */
    {0x0c,0xe0,0xe1,0x0d,0,0,0},		/* cntl - m */
    {0x0c,0xf0,0xf1,0x0d,0,0,0},		/* cntl - n */
    {0x0c,0xc2,0xc3,0x0d,0,0,0},		/* cntl - o */
    {0x0c,0xc4,0xc5,0x0d,0,0,0},		/* cntl - p */
    {0x0c,0x6c,0x6d,0x0d,0,0,0},		/* cntl - q */
    {0x0c,0x66,0x67,0x0d,0,0,0},		/* cntl - r */
    {0x0c,0x58,0x59,0x0d,0,0,0},		/* cntl - s */
    {0x0c,0x64,0x65,0x0d,0,0,0},		/* cntl - t */
    {0x0c,0x60,0x61,0x0d,0,0,0},		/* cntl - u */
    {0x0c,0x32,0x33,0x0d,0,0,0},		/* cntl - v */
    {0x0c,0x6a,0x6b,0x0d,0,0,0},		/* cntl - w */
    {0x0c,0x36,0x37,0x0d,0,0,0},		/* cntl - x */
    {0x0c,0x62,0x63,0x0d,0,0,0},		/* cntl - y */
    {0x0c,0x38,0x39,0x0d,0,0,0},		/* cntl - z */
    {0x0c,0xc6,0xc7,0x0d,0,0,0},		/* cntl - [ */
    {0x0c,0xca,0xcb,0x0d,0,0,0},		/* cntl - \ */
    {0x0c,0xc8,0xc9,0x0d,0,0,0},		/* cntl - ] */
    {0x0c,0x0a,0x72,0x73,0x0b,0x0d,0},		/* cntl - ^ */
    {0x0c,0x0a,0xb6,0xb7,0x0b,0x0d,0},		/* cntl - _ */
    {0xf2,0xf3,0,0,0,0,0},        		/* space    */
    {0x0a,0x7c,0x7d,0x0b,0,0,0},		/* !        */
    {0x0a,0xd8,0xd9,0x0b,0,0,0},		/* "        */
    {0x0a,0x78,0x79,0x0b,0,0,0},		/* #        */
    {0x0a,0x76,0x77,0x0b,0,0,0},		/* $        */
    {0x0a,0x74,0x75,0x0b,0,0,0},		/* %        */
    {0x0a,0x70,0x71,0x0b,0,0,0},		/* &        */
    {0xd8,0xd9,0,0,0,0,0},        		/* '        */
    {0x0a,0xb2,0xb3,0x0b,0,0,0},		/* (        */
    {0x0a,0xb4,0xb5,0x0b,0,0,0},		/* )        */
    {0x0a,0xb0,0xb1,0x0b,0,0,0},		/* *        */
    {0x0a,0xb8,0xb9,0x0b,0,0,0},		/* +        */
    {0xe2,0xe3,0,0,0,0,0},        		/* ,        */
    {0xb6,0xb7,0,0,0,0,0},        		/* -        */
    {0xe4,0xe5,0,0,0,0,0},        		/* .        */
    {0xe6,0xe7,0,0,0,0,0},        		/* /        */
    {0xb4,0xb5,0,0,0,0,0},        		/* 0        */
    {0x7c,0x7d,0,0,0,0,0},        		/* 1        */
    {0x7a,0x7b,0,0,0,0,0},        		/* 2        */
    {0x78,0x79,0,0,0,0,0},        		/* 3        */
    {0x76,0x77,0,0,0,0,0},        		/* 4        */
    {0x74,0x75,0,0,0,0,0},        		/* 5        */
    {0x72,0x73,0,0,0,0,0},        		/* 6        */
    {0x70,0x71,0,0,0,0,0},        		/* 7        */
    {0xb0,0xb1,0,0,0,0,0},        		/* 8        */
    {0xb2,0xb3,0,0,0,0,0},        		/* 9        */
    {0x0a,0xd6,0xd7,0x0b,0,0,0},		/* :        */
    {0xd6,0xd7,0,0,0,0,0},        		/* ;        */
    {0x0a,0xe2,0xe3,0x0b,0,0,0},		/* <        */
    {0xb8,0xb9,0,0,0,0,0},        		/* =        */
    {0x0a,0xe4,0xe5,0x0b,0,0,0},		/* >        */
    {0x0a,0xe6,0xe7,0x0b,0,0,0},		/* ?        */
    {0x0a,0x7a,0x7b,0x0b,0,0,0},		/* @        */
    {0x0a,0x5a,0x5b,0x0b,0,0,0},		/* A        */
    {0x0a,0x30,0x31,0x0b,0,0,0},		/* B        */
    {0x0a,0x34,0x35,0x0b,0,0,0},		/* C        */
    {0x0a,0x56,0x57,0x0b,0,0,0},		/* D        */
    {0x0a,0x68,0x69,0x0b,0,0,0},		/* E        */
    {0x0a,0x54,0x55,0x0b,0,0,0},		/* F        */
    {0x0a,0x52,0x53,0x0b,0,0,0},		/* G        */
    {0x0a,0x50,0x51,0x0b,0,0,0},		/* H        */
    {0x0a,0xc0,0xc1,0x0b,0,0,0},		/* I        */
    {0x0a,0xd0,0xd1,0x0b,0,0,0},		/* J        */
    {0x0a,0xd2,0xd3,0x0b,0,0,0},		/* K        */
    {0x0a,0xd4,0xd5,0x0b,0,0,0},		/* L        */
    {0x0a,0xe0,0xe1,0x0b,0,0,0},		/* M        */
    {0x0a,0xf0,0xf1,0x0b,0,0,0},		/* N        */
    {0x0a,0xc2,0xc3,0x0b,0,0,0},		/* O        */
    {0x0a,0xc4,0xc5,0x0b,0,0,0},		/* P        */
    {0x0a,0x6c,0x6d,0x0b,0,0,0},		/* Q        */
    {0x0a,0x66,0x67,0x0b,0,0,0},		/* R        */
    {0x0a,0x58,0x59,0x0b,0,0,0},		/* S        */
    {0x0a,0x64,0x65,0x0b,0,0,0},		/* T        */
    {0x0a,0x60,0x61,0x0b,0,0,0},		/* U        */
    {0x0a,0x32,0x33,0x0b,0,0,0},		/* V        */
    {0x0a,0x6a,0x6b,0x0b,0,0,0},		/* W        */
    {0x0a,0x36,0x37,0x0b,0,0,0},		/* X        */
    {0x0a,0x62,0x63,0x0b,0,0,0},		/* Y        */
    {0x0a,0x38,0x39,0x0b,0,0,0},		/* Z        */
    {0xc6,0xc7,0,0,0,0,0},        		/* [        */
    {0xca,0xcb,0,0,0,0,0},        		/* \        */
    {0xc8,0xc9,0,0,0,0,0},        		/* ]        */
    {0x0a,0x72,0x73,0x0b,0,0,0},		/* ^        */
    {0x0a,0xb6,0xb7,0x0b,0,0,0},		/* _        */
    {0x7e,0x7f,0,0,0,0,0},        		/* `        */
    {0x5a,0x5b,0,0,0,0,0},			/* a        */
    {0x30,0x31,0,0,0,0,0},			/* b        */
    {0x34,0x35,0,0,0,0,0},			/* c        */
    {0x56,0x57,0,0,0,0,0},			/* d        */
    {0x68,0x69,0,0,0,0,0},			/* e        */
    {0x54,0x55,0,0,0,0,0},			/* f        */
    {0x52,0x53,0,0,0,0,0},			/* g        */
    {0x50,0x51,0,0,0,0,0},			/* h        */
    {0xc0,0xc1,0,0,0,0,0},			/* i        */
    {0xd0,0xd1,0,0,0,0,0},			/* j        */
    {0xd2,0xd3,0,0,0,0,0},			/* k        */
    {0xd4,0xd5,0,0,0,0,0},			/* l        */
    {0xe0,0xe1,0,0,0,0,0},			/* m        */
    {0xf0,0xf1,0,0,0,0,0},			/* n        */
    {0xc2,0xc3,0,0,0,0,0},			/* o        */
    {0xc4,0xc5,0,0,0,0,0},			/* p        */
    {0x6c,0x6d,0,0,0,0,0},			/* q        */
    {0x66,0x67,0,0,0,0,0},			/* r        */
    {0x58,0x59,0,0,0,0,0},			/* s        */
    {0x64,0x65,0,0,0,0,0},			/* t        */
    {0x60,0x61,0,0,0,0,0},			/* u        */
    {0x32,0x33,0,0,0,0,0},			/* v        */
    {0x6a,0x6b,0,0,0,0,0},			/* w        */
    {0x36,0x37,0,0,0,0,0},			/* x        */
    {0x62,0x63,0,0,0,0,0},			/* y        */
    {0x38,0x39,0,0,0,0,0},			/* z        */
    {0x0a,0xc6,0xc7,0x0b,0,0,0},    		/* {        */
    {0x0a,0xca,0xcb,0x0b,0,0,0},    		/* |        */
    {0x0a,0xc6,0xc9,0x0b,0,0,0},    		/* }        */
    {0x0a,0x7e,0x7f,0x0b,0,0,0},    		/* ~        */
    {0x0a,0x3e,0x3f,0x0b,0,0,0}};   		/* delete   */
#endif
