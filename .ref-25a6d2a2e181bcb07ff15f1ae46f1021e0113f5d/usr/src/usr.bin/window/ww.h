/*	@(#)ww.h	3.53 89/05/11		*/
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)ww.h	3.53 (Berkeley) %G%
 */

#include <sgtty.h>
#include <setjmp.h>
#include <machine/endian.h>
#include "window.h"

struct ww_dim {
	short col;
	short row;
	short ncol;
	short nrow;
};

	/* a coordinate */
struct ww {
		/* general flags and states */
	char ww_state;		/* state of window */
	char ww_oflags;		/* wwopen flags */

		/* information for overlap */

		/* sizes and positions */
	struct ww_dim ww_w;	/* window size and pos */
	struct ww_dim ww_b;	/* buffer size and pos */
	struct ww_dim ww_i;	/* the part inside the screen */
	int ww_pty;		/* pty or socket pair */
	int ww_tty;
	int ww_pid;
	struct ww *ww_next;
	char ww_ttyname[11];
};

	/* state of a tty */
struct ww_tty {
	struct sgttyb ww_sgttyb;
	struct tchars ww_tchars;
	struct ltchars ww_ltchars;
	int ww_lmode;
	int ww_ldisc;
	int ww_fflags;
};

union ww_char {
	short c_w;		/* as a word */
	struct {
#if BYTE_ORDER == LITTLE_ENDIAN || BYTE_ORDER == PDP_ENDIAN
		char C_c;	/* the character part */
		char C_m;	/* the mode part */
#endif
#if BYTE_ORDER == BIG_ENDIAN
		char C_m;	/* the mode part */
		char C_c;	/* the character part */
#endif
	} c_un;
};
#define c_c c_un.C_c
#define c_m c_un.C_m

	/* for display update */
struct ww_update {
	int best_gain;
	int best_col;
	int gain;
};

	/* parts of ww_char */
#define WWC_CMASK	0x00ff
#define WWC_MMASK	0xff00
#define WWC_MSHIFT	8

	/* c_m bits */
#define WWM_REV		0x01	/* reverse video */
#define WWM_BLK		0x02	/* blinking */
#define WWM_UL		0x04	/* underlined */
#define WWM_GRP		0x08	/* graphics */
#define WWM_DIM		0x10	/* half intensity */
#define WWM_USR		0x20	/* user specified mode */
#define WWM_GLS		0x40	/* window only, glass, i.e., transparent */

	/* ww_state values */
#define WWS_INITIAL	0	/* just opened */
#define WWS_HASPROC	1	/* has process on pty */
#define WWS_DEAD	3	/* child died */

	/* ww_state values */
#define WW_INITIAL	0
#define WW_HASPROC	1
#define WW_INCHILD	2
#define WW_DEAD		3
	/* flags for ww_fmap */
#define WWF_U		0x01
#define WWF_R		0x02
#define WWF_D		0x04
#define WWF_L		0x08
#define WWF_MASK	(WWF_U|WWF_R|WWF_D|WWF_L)
#define WWF_LABEL	0x40
#define WWF_TOP		0x80

	/* flags to wwopen() */
#define WWO_PTY		0x01		/* want pty */
#define WWO_SOCKET	0x02		/* want socket pair */
#define WWO_REVERSE	0x04		/* make it all reverse video */
#define WWO_GLASS	0x08		/* make it all glass */
#define WWO_FRAME	0x10		/* this is a frame window */

	/* special ww_index value */
#define WWX_NOBODY	NWW

	/* error codes */
#define WWE_NOERR	0
#define WWE_SYS		1		/* system error */
#define WWE_NOMEM	2		/* out of memory */
#define WWE_TOOMANY	3		/* too many windows */
#define WWE_NOPTY	4		/* no more ptys */
#define WWE_SIZE	5		/* bad window size */
#define WWE_BADTERM	6		/* bad terminal type */
#define WWE_CANTDO	7		/* dumb terminal */

	/* wwtouched[] bits, there used to be more than one */
#define WWU_TOUCHED	0x01		/* touched */

	/* the window structures */
	/* ww_mode values */
#define WW_PTY		0		/* has pty */
#define WW_SOCKET	1		/* has socket pair */
#define WW_NONE		2		/* has nothing */

	/* tty things */
#undef CTRL
#define CTRL(c)		('c'&0x1f)
#define DEL		0x7f
#define ISCTRL(c)	((c) < ' ' || (c) >= DEL)

	/* generally useful variables */
extern struct ww *wwhead, *curwin;
extern struct ww_tty wwoldtty, wwnewtty, wwwintty;
extern int wwnwrite;
extern int wwnwritec;
extern int wwnrow, wwncol;		/* the screen size */

#define wwputchar(c)	wwputc((c), curwin)
#define wwputstr(s)	wwputs((s), curwin)
#define wwsetcursor(r,c) (WRCurRow = (r), WRCurCol = (c))
#define wwflush()	Wrefresh(1)

	/* quicky macros */
struct ww *wwopen();
int wwchild();
