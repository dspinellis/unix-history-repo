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
TC(ATTN)			/* attention generator */



Is(LOWEST_USER)			/* lowest code settable by user */

TC(LPRT)			/* local print */
TC(DP)				/* dup character */
TC(FM)				/* field mark character */
TC(CURSEL)			/* cursor select */
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

TC(PA1,6c)				/*  */
TC(PA2,6e)				/*  */
TC(PA3,6b)				/*  */
TC(CLEAR,6d)			/*  */
TC(TREQ,f0)				/*  */
TC(ENTER,7d)			/*  */
TC(PFK1, f1)				/*  */
TC(PFK2, f2)				/*  */
TC(PFK3, f3)				/*  */
TC(PFK4, f4)				/*  */
TC(PFK5, f5)				/*  */
TC(PFK6, f6)				/*  */
TC(PFK7, f7)				/*  */
TC(PFK8, f8)				/*  */
TC(PFK9, f9)				/*  */
TC(PFK10, 7a)			/*  */
TC(PFK11, 7b)			/*  */
TC(PFK12, 7c)			/*  */
TC(PFK13, c1)			/*  */
TC(PFK14, c2)			/*  */
TC(PFK15, c3)			/*  */
TC(PFK16, c4)			/*  */
TC(PFK17, c5)			/*  */
TC(PFK18, c6)			/*  */
TC(PFK19, c7)			/*  */
TC(PFK20, c8)			/*  */
TC(PFK21, c9)			/*  */
TC(PFK22, 4a)			/*  */
TC(PFK23, 4b)			/*  */
TC(PFK24, 4c)			/*  */
TC(PFK25)			/*  */
TC(PFK26)			/*  */
TC(PFK27)			/*  */
TC(PFK28)			/*  */
TC(PFK29)			/*  */
TC(PFK30)			/*  */
TC(PFK31)			/*  */
TC(PFK32)			/*  */
TC(PFK33)			/*  */
TC(PFK34)			/*  */
TC(PFK35)			/*  */
TC(PFK36)			/*  */

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
