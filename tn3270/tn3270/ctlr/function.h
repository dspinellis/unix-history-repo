/*
 * Copyright (c) 1988 Regents of the University of California.
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
 *	@(#)function.h	4.1 (Berkeley) 12/4/88
 */

/*
 * The following are the various functions which the keyboard can ask
 * the controller to perform.
 *
 * Note that this file (the following entries) are scanned by mkhit.c,
 * and that the format must remain more-or-less consistent
 * [ \t]*TOKEN
 */

enum ctlrfcn {

	undefined = 0,			/* Not yet touched */

	FCN_NULL,	  		/* Illegal sequence */

	FCN_RESET,			/* unlock keyboard */
	FCN_MAKE_SHIFT_LOCK,
	FCN_BREAK_SHIFT_LOCK,

	FCN_MAKE_SHIFT,			/* shift key pressed DOWN */
	FCN_BREAK_SHIFT,		/* shift key released */

	FCN_MAKE_ALT,			/* alt key pressed DOWN */
	FCN_BREAK_ALT,			/* alt key released */

	FCN_MAKE_CTRL,

	FCN_CAPS_LOCK,

	FCN_MONOCASE,			/* DISPLAY in upper case */
	FCN_DVCNL,

	FCN_CHARACTER,			/* Not one of the following, but ... */
	FCN_VERTICAL_BAR,		/* EBCDIC solid vertical bar */
	FCN_CENTSIGN,			/* EBCDIC cent sign */
	FCN_SPACE,			/* EBCDIC space */
	FCN_DP,				/* EBCDIC dup character */
	FCN_FM,				/* EBCDIC field mark */

	FCN_AID,			/* Some AID key */
	FCN_ATTN,
	FCN_CURSEL,			/* Cursor select function (and aid) */
	FCN_TEST,			/* Test function */

	FCN_EINP,			/* erase input (dangerous) */
	FCN_EEOF,
	FCN_DELETE,
	FCN_INSRT,
	FCN_TAB,
	FCN_BTAB,
	FCN_NL,
	FCN_HOME,
	FCN_UP,
	FCN_DOWN,
	FCN_RIGHT,
	FCN_LEFT,
	FCN_LEFT2,
	FCN_RIGHT2,

#if	!defined(PURE3274)
	/*
	 * Local editing functions
	 */
	FCN_SETTAB,			/* set a column tab */
	FCN_DELTAB,
	FCN_COLTAB,
	FCN_COLBAK,
	FCN_INDENT,			/* more margin over one col tab */
	FCN_UNDENT,
	FCN_SETMRG,
	FCN_SETHOM,
	FCN_CLRTAB,
	FCN_ERASE,			/* erase last character */
	FCN_WERASE,
	FCN_FERASE,
	FCN_WORDTAB,			/* tab to start of next word */
	FCN_WORDBACKTAB,
	FCN_WORDEND,			/* find next end of word */
	FCN_FIELDEND,			/* find next end of field */

	/*
	 * APL functions
	 */
	FCN_APLON,			/* start using apl character set */
	FCN_APLOFF,
	FCN_APLEND,

	FCN_PCON,
	FCN_PCOFF,
	FCN_INIT,			/* re-init screen */
	FCN_SYNCH,			/* synch up after line/control error */
	FCN_FLINP,			/* flush input buffer */
	FCN_RESHOW,			/* redraw screen */
	FCN_MASTER_RESET,		/* FLINP, RESET, RESHOW, + more */

	FCN_DISC,			/* suspend application */
	FCN_ESCAPE,			/* enter command mode */

	FCN_ALTK,			/* Dvorak keyboard */

	FCN_XOFF,			/* suspend output to screen */
	FCN_XON,			/* resume output to screen */

	FCN_LPRT			/* print screen on printer */
#endif	/* !defined(PURE3274) */
};
/*
 * The following is the structure which defines what a 3270 keystroke
 * can do.
 */

struct hits {
    unsigned char keynumber;
    struct hit {
	enum ctlrfcn ctlrfcn;
	unsigned char code;	/* AID value or 3270 display code */
    } hit[4];	/* plain, shifted, alted, shiftalted */
};

extern struct hits hits[];

/*
 * Definitions of the shift state (and the left/right shift key position).
 */

#define	SHIFT_RIGHT	0x20	/* Right shift key is down */
#define	SHIFT_LEFT	0x10	/* Left shift key is down */
#define	SHIFT_CONTROL	0x08	/* Control shift state (unused) */
#define	SHIFT_ALT	0x04	/* ALT shift state */
#define	SHIFT_CAPS	0x02	/* Caps lock state */
#define	SHIFT_UPSHIFT	0x01	/* Upshift state */
