/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)termcodes.m4	1.2 (Berkeley) %G%
 */

/* the following allows us to generate our TC values.  it is a bit painful. */

define(TCvalue, 128)
define(TCbump, `define(`TCvalue', eval(TCvalue+1))')
define(TCdefine, ``#define'')
define(TC, `TCdefine define(`TCstring', TC_$1) TCstring TCvalue TCbump `
		'InitialAscii("$1", TCstring) InitialAids(ifelse($2,,0,0x$2))')
define(Is, `TCdefine TC_$1 TCvalue')

#define IsTc(x) (((x)&0xff) >= TC_LOWEST)

/* This lists the codes which are output from termin() */

typedef char TC_Aids_t;

typedef struct {
    char	*tc_name;		/* what the name is */
    char	tc_value;		/* what the value is */
} TC_Ascii_t;

typedef struct {
    char	*tc_name;		/* what the name is */
    char	tc_value;		/* what the value is */
    char	tc_aid;			/* what the AID is */
} TC_AsciiAids_t;

#ifdef DEFINEAIDS
#define InitialAids(x)	x,
#else
#define InitialAids(x)
#endif /* DEFINEAIDS */
#ifdef LETS_SEE_ASCII
#define InitialAscii(x, y) x, y,
#else
#define InitialAscii(x, y)
#endif /* LETS_SEE_ASCII */

#ifdef LETS_SEE_ASCII
#ifdef DEFINEAIDS
static TC_AsciiAids_t TC_AsciiAids[] = {
#else /* so, no aids */
static TC_Ascii_t TC_Ascii[] = {
#endif /* DEFINEAIDS */
#else
#ifdef DEFINEAIDS
static TC_Aids_t TC_Aids[] = {
#endif /* LETS_SEE_ASCII */
#endif

Is(LOWEST)

/* the following are internal to the parser and generator */
/* (and, generally, should not be specified by the user...) */
TC(CALL)			/* INT: call a state sequence */
TC(RETURN)			/* INT: return from TC_CALL */
TC(GOTO)			/* INT: goto next entry */
TC(NULL)			/* Illegal sequence; bell */



Is(LOWEST_USER)			/* lowest code settable by user */

TC(VERTICAL_BAR)
TC(CAPS_LOCK)
TC(MAKE_SHIFT)
TC(BREAK_SHIFT)
TC(DVCNL)
TC(MAKE_ALT)
TC(BREAK_ALT)
TC(SPACE)
TC(LEFT2)
TC(RIGHT2)
TC(MONOCASE)
TC(ATTN)			/* attention generator */
TC(LPRT)			/* local print */
TC(DP)				/* dup character */
TC(FM)				/* field mark character */
TC(CURSEL)			/* cursor select */
TC(CENTSIGN)			/* EBCDIC cent sign */
TC(RESHOW)			/* redisplay the screen */
TC(EINP)			/* erase input */
TC(EEOF)			/* erase end of field */
TC(DELETE)			/* delete character */
TC(INSRT)			/* toggle insert mode */
TC(TAB)				/* field tab */
TC(BTAB)			/* field back tab */
TC(COLTAB)			/* column tab */
TC(COLBAK)			/* column back tab */
TC(INDENT)			/* indent one tab stop */
TC(UNDENT)			/* undent one tab stop */
TC(NL)				/* new line */
TC(HOME)			/* home the cursor */
TC(UP)				/* up cursor */
TC(DOWN)			/* down cursor */
TC(RIGHT)			/* right cursor */
TC(LEFT)			/* left cursor */
TC(SETTAB)			/* set a column tab */
TC(DELTAB)			/* delete a column tab */
TC(SETMRG)			/* set left margin */
TC(SETHOM)			/* set home position */
TC(CLRTAB)			/* clear all column tabs */
TC(APLON)			/* apl on */
TC(APLOFF)			/* apl off */
TC(APLEND)			/* treat input as ascii */
TC(PCON)			/* xon/xoff on */
TC(PCOFF)			/* xon/xoff off */
TC(DISC)			/* disconnect (suspend) */
TC(INIT)			/* new terminal type */
TC(ALTK)			/* alternate keyboard dvorak */
TC(FLINP)			/* flush input */
TC(ERASE)			/* erase last character */
TC(WERASE)			/* erase last word */
TC(FERASE)			/* erase field */
TC(SYNCH)			/* user and us are in synch */
TC(RESET)			/* reset key -unlock keyboard */
TC(MASTER_RESET)		/* master reset key; flush, reset, repaint */
TC(XOFF)			/* please hold output */
TC(XON)				/* please give me output */
TC(ESCAPE)			/* enter telnet command mode */
TC(WORDTAB)			/* Go to first character of next word */
TC(WORDBACKTAB)			/* Go to first character of last word */
TC(WORDEND)			/* Go to last character of this/next word */
TC(FIELDEND)			/* Go to last non-blank of this field */


Is(LOWEST_AID)			/* lowest attn generator */

/* the attention generating keys... */

TC(PA1, 6c)
TC(PA2, 6e)
TC(PA3, 6b)
TC(CLEAR, 6d)
TC(TREQ, f0)
TC(ENTER, 7d)
TC(SELPEN, 7e)	/* Really, only SELPEN with DESIGNATOR = space or null */
TC(PFK1, f1)
TC(PFK2, f2)
TC(PFK3, f3)
TC(PFK4, f4)
TC(PFK5, f5)
TC(PFK6, f6)
TC(PFK7, f7)
TC(PFK8, f8)
TC(PFK9, f9)
TC(PFK10, 7a)
TC(PFK11, 7b)
TC(PFK12, 7c)
TC(PFK13, c1)
TC(PFK14, c2)
TC(PFK15, c3)
TC(PFK16, c4)
TC(PFK17, c5)
TC(PFK18, c6)
TC(PFK19, c7)
TC(PFK20, c8)
TC(PFK21, c9)
TC(PFK22, 4a)
TC(PFK23, 4b)
TC(PFK24, 4c)
TC(PFK25, d1)
TC(PFK26, d2)
TC(PFK27, d3)
TC(PFK28, d4)
TC(PFK29, d5)
TC(PFK30, d6)
TC(PFK31, d7)
TC(PFK32, d8)
TC(PFK33, d9)
TC(PFK34, 5a)
TC(PFK35, 5b)
TC(PFK36, 5c)

Is(HIGHEST_AID)-1			/* highest AID value */
#define IsAid(x)	(((x) >= TC_LOWEST_AID) && ((x) <= TC_HIGHEST_AID))

Is(HIGHEST)-1			/* highest TC value */

#ifdef LETS_SEE_ASCII
};
#else
#ifdef DEFINEAIDS
};
#endif
#endif

#ifdef DEFINEAIDS
#ifdef LETS_SEE_ASCII
#define TCtoAid(x)	TC_AsciiAids[(x)-TC_LOWEST].tc_aid
#else
#define TCtoAid(x)	TC_Aids[x-TC_LOWEST]
#endif
#endif
