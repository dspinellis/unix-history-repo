/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Sony Corp. and Kazumasa Utashiro of Software Research Associates, Inc.
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
 * from: $Hdr: mouse.h,v 4.300 91/06/09 06:42:56 root Rel41 $ SONY
 *
 *	@(#)mouse.h	8.1 (Berkeley) 6/11/93
 */

#ifndef __MOUSE__
#define __MOUSE__ 1

/*
 * mouse register structure definition.
 */

/*
 * mouse data (mouse X, Y coordinates and its button status)
 */
struct ms_data {
    int	    md_sw;  /* mouse button */
#define	    MS_BUTNL	0x04
#define	    MS_BUTNM	0x02
#define	    MS_BUTNR	0x01
    int	    md_x;   /* x coordinate */
    int	    md_y;   /* y coordinate */
};

struct ms_coord {
    int	    mc_x;   /* x coordinate */
    int	    mc_y;   /* y coordinate */
};

struct ms_range {
    struct ms_coord	mr_min;	/* upper left */
    struct ms_coord	mr_max;	/* lower right */
};

/*
 * mouse event report
 *	event report is queued when mouse is put in event mode
 *	by using MIOC SETEM ioctl()
 */
struct ms_event {
    struct ms_data	mse_data;	/* mouse X, Y and button status */
    char		mse_trig;	/* trigger that caused this event */
#define	MSE_MOTION	0		    /* mouse movement */
#define MSE_BUTTON	1		    /* mouse buttons */
#define MSE_KEY		2		    /* keyboard keys */
    char		mse_dir;	/* key or button direction */
#define MSE_DOWN	0		    /* down */
#define MSE_UP		1		    /* up */
#define MSE_UNKOWN	2		    /* unkown */
    char		mse_code;	/* key or button code */
#define MSE_BUTNR	0		    /* right button */
#define MSE_BUTNM	1		    /* middle button */
#define MSE_BUTNL	2		    /* left button */
    char		mse_inval;
  /* for key code, see below */
    struct timeval	mse_time;	/* time when this event occurred */
};

struct ms_queue {
    int			mq_head;
    int			mq_tail;
#define MS_MAXREPORT	170
    struct ms_event	mq_queue[MS_MAXREPORT];
    int			dummy[2];
};

/* strct ms_param:
 *	when mouse is moved more than mp_delta, amount that exceeds
 *	the mp_delta is maginified by mp_mag(>0)
 */
struct ms_param {
    int    mp_delta;	/* threshold for maginification */
    int	   mp_mag;	/* magifying factor */
};

/* meaning of ms_eventmask */
#define	    MS_EMEVENT	0x80	/* 1 -> event mode */
#define	    MS_EMKEY	0x40	/* keyboard key changes -> event */

		/* WARNING: use of MS_EMCORD[XY] is no recomended
		 *	MS_EMCORD[XY] will disappear shortly
		 */
#define	    MS_EMCORDY	0x20	/* coordinate y changes -> event */
#define	    MS_EMCORDX	0x10	/* coordinate x changes -> event */

#define	    MS_EMMOTION	0x10	/* coordinates changes -> event */
#define	    MS_EMBUTNL	0x04	/* left button changes -> event */
#define	    MS_EMBUTNM	0x02	/* mid button changes -> event */
#define	    MS_EMBUTNR	0x01	/* right button changes -> event */


/*
 * Key Code
 */

#define KEY_A		41
#define KEY_B		59
#define KEY_C		57
#define KEY_D		43
#define KEY_E		29
#define KEY_F		44
#define KEY_G		45
#define KEY_H		46
#define KEY_I		34
#define KEY_J		47
#define KEY_K		48
#define KEY_L		49
#define KEY_M		61
#define KEY_N		60
#define KEY_O		35
#define KEY_P		36
#define KEY_Q		27
#define KEY_R		30
#define KEY_S		42
#define KEY_T		31
#define KEY_U		33
#define KEY_V		58
#define KEY_W		28
#define KEY_X		56
#define KEY_Y		32
#define KEY_Z		55
#define KEY_0		21
#define KEY_1		12
#define KEY_2		13
#define KEY_3		14
#define KEY_4		15
#define KEY_5		16
#define KEY_6		17
#define KEY_7		18
#define KEY_8		19
#define KEY_9		20
#define KEY_MINUS	22	/* - */
#define KEY_EQUAL	23	/* = */
#define KEY_YEN		24	/* \ */
#define KEY_BRA		37	/* [ */
#define KEY_KET		38	/* ] */
#define KEY_SEMICOL	50	/* ; */
#define KEY_SQUOTE	51	/* ' */
#define KEY_BQUOTE	52	/* ` */
#define KEY_COMMA	62	/* , */
#define KEY_PERIOD	63	/* . */
#define KEY_SLASH	64	/* / */
#define KEY_RO		65	/* 'RO' (katakana) */
#define KEY_ESC		11
#define KEY_TAB		26
#define KEY_BS		25
#define KEY_DEL		39
#define KEY_CR		53	/* carrige return */
#define KEY_SP		70	/* space */
#define KEY_CTRL	40
#define KEY_SHIFTL	54	/* left shift key */
#define KEY_SHIFTR	66	/* right shift key */
#define KEY_ALT		67
#define KEY_CAPS	68
#define KEY_MUHENKAN	69
#define KEY_HENKAN	71
#define KEY_EISUU	72
#define KEY_KANA	73
#define KEY_JIKKOU	74

/* function key */
#define KEY_F1		1
#define KEY_F2		2
#define KEY_F3		3
#define KEY_F4		4
#define KEY_F5		5
#define KEY_F6		6
#define KEY_F7		7
#define KEY_F8		8
#define KEY_F9		9
#define KEY_F10		10

/* numeric keypad */
#define NKEY_0		87
#define NKEY_1		83
#define NKEY_2		84
#define NKEY_3		85
#define NKEY_4		79
#define NKEY_5		80
#define NKEY_6		81
#define NKEY_7		75
#define NKEY_8		76
#define NKEY_9		77
#define NKEY_PERIOD	89
#define NKEY_MINUS	78
#define NKEY_PLUS	82
#define NKEY_COMMA	86
#define NKEY_CR		90
#define NKEY_LEFT	91	/* arrow key */
#define NKEY_RIGHT	93
#define NKEY_UP		88
#define NKEY_DOWN	92
#define OKEY_SPACE	94
#define OKEY_EISUU	95
#define OKEY_SHIFTL	96
#define OKEY_SHIFTR	97
#define OKEY_KANA	98
#define OKEY_EQUAL	99
#define OKEY_ASTERISK	100
#define OKEY_SLASH	101
#define OKEY_TAB	102

/*
 * Mouse I/O contol commands
 */

#include <sys/ioctl.h>

#define	MSIOCGETEM	_IOR('M', 0, int)	/* get current event mask */
#define	MSIOCSETEM	_IOW('M', 1, int)	/* set event mask */
#define	MSIOCSETXY	_IOW('M', 2, struct ms_coord) /* set current x, y */
#define	MSIOCFLUSH	_IO('M', 3)		/* flush event queue */
#define	MSIOCSETPARAM	_IOW('M', 4, struct ms_param) /* set mouse parameter */
#define	MSIOCSETRANGE	_IOW('M', 5, struct ms_range) /* set mouse coordinate range */

#endif /* !__MOUSE__ */
