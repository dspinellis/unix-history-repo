#ifndef NODIAL
char *dialv = "Dial Command, 5A(046) 2 Nov 92";

/*  C K U D I A	 --  Module for automatic modem dialing. */

/*
  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.

  Original (version 1, 1985) author: Herm Fischer, Encino, CA.
  Contributed to Columbia University in 1985 for inclusion in C-Kermit 4.0.
  Author and maintainer since 1985: Frank da Cruz, Columbia University,
  fdc@columbia.edu.

  Contributions by many others throughout the years, including: Fernando
  Cabral, John Chmielewski, Joe Doupnik, Richard Hill, Larry Jacobs, Eric
  Jones, Tom Kloos, Bob Larson, Peter Mauzey, Joe Orost, Kevin O'Gorman, Kai
  Uwe Rommel, Dan Schullman, Warren Tucker, and others too numerous to list
  here (but see acknowledgements in ckcmai.c).

  This module calls externally defined system-dependent functions for
  communications i/o, as defined in CKCPLM.DOC, the C-Kermit Program Logic
  Manual, and thus should be portable to all systems that implement those
  functions, and where alarm() and signal() work as they do in UNIX.
*/

/*
  To add support for another modem, do the following, all in this module:

  1. Define a modem-type number symbol (n_XXX) for it.

  2. Adjust MAX_MDM to the new number of modem types.

  3. Create a MDMINF structure for it.

  4. Add the address of the MDMINF structure to the ptrtab[] array,
     according to the numerical value of the modem-type number.

  5. Add the user-visible (SET MODEM) name and corresponding modem
     number to the mdmtab[] array, in alphabetical order by modem-name string.

  6. Read through the code and add modem-specific sections as necessary.

  NOTE: The MINIDIAL symbol is used to build this module to include support
  for only a minimum number of standard and/or generally useful modem types,
  namely Hayes, CCITT V.25bis, "Unknown", and None.  When adding support for
  a new modem type, keep it outside of the MINIDIAL sections.
*/

#include "ckcdeb.h"
#ifndef MAC
#include <signal.h>
#endif /* MAC */
#include "ckcasc.h"
#include "ckcker.h"
#include "ckucmd.h"
#include "ckcnet.h"

#ifndef ZILOG
#include <setjmp.h>			/* Longjumps */
#else
#include <setret.h>
#endif /* ZILOG */

#ifdef MAC
#define signal msignal
#define SIGTYP long
#define alarm malarm
#define SIG_IGN 0
#define SIGALRM 1
#define SIGINT 2
SIGTYP (*msignal(int type, SIGTYP (*func)(int)))(int);
#endif /* MAC */

int					/* SET DIAL parameters */
  dialhng = 1,				/* DIAL HANGUP, default is ON */
  dialdpy = 0,				/* DIAL DISPLAY, default is OFF */
  mdmspd  = 1,				/* DIAL SPEED-MATCHING (1 = ON) */
  dialtmo = 0,				/* DIAL TIMEOUT */
  dialksp = 0,				/* DIAL KERMIT-SPOOF */
  dialmnp = 0,				/* DIAL MNP-ENABLE */
#ifdef NOMDMHUP
  dialmhu = 0;				/* DIAL MODEM-HANGUP */
#else
  dialmhu = 1;				/* DIAL MODEM-HANGUP */
#endif /* NOMDMHUP */

char *dialdir = NULL;			/* DIAL DIRECTORY, default none */
char *dialini = NULL;			/* DIAL INIT-STRING, default none */
char *dialcmd = NULL;			/* DIAL DIAL-COMMAND, default none */
char *dialnpr = NULL;			/* DIAL NUMBER-PREFIX, ditto */
FILE * dialfd = NULL;			/* File descriptor of dial directory */

#ifndef MINIDIAL
/*
  Telebit model codes:

  ATI  Model Numbers           Examples
  ---  -------------           --------
  123                          Telebit in "total Hayes-1200" emulation mode
  960                          Telebit in Conventional Command (Hayes) mode
  961  RA12C                   IBM PC internal original Trailblazer
  962  RA12E                   External original Trailblazer
  963  RM12C                   Rackmount original Trailblazer
  964  T18PC                   IBM PC internal Trailblazer-Plus (TB+)
  965  T18SA, T2SAA, T2SAS     External TB+, T1600, T2000, T3000, WB, and later
  966  T18RMM                  Rackmount TB+
  967  T2MC                    IBM PS/2 internal TB+
  968  T1000                   External T1000
  969  ?                       Qblazer
  971  T2500                   External T2500
  972  T2500                   Rackmount T2500
*/

/* Telebit model codes */

#define TB_UNK  0			/* Unknown Telebit model */
#define TB_BLAZ 1			/* Original TrailBlazer */
#define TB_PLUS	2			/* TrailBlazer Plus */
#define TB_1000 3			/* T1000 */
#define TB_1500 4			/* T1500 */
#define TB_1600 5			/* T1600 */
#define TB_2000 6			/* T2000 */
#define TB_2500 7			/* T2500 */
#define TB_3000 8			/* T3000 */
#define TB_QBLA 9			/* Qblazer */
#define TB_WBLA 10			/* WorldBlazer */
#define TB__MAX 10			/* Highest number */

char *tb_name[] = {			/* Array of model names */
    "Unknown",				/* TB_UNK  */
    "TrailBlazer",			/* TB_BLAZ */
    "TrailBlazer-Plus",			/* TB_PLUS */
    "T1000",				/* TB_1000 */
    "T1500",				/* TB_1500 */
    "T1600",				/* TB_1600 */
    "T2000",				/* TB_2000 */
    "T2500",				/* TB_2500 */
    "T3000",				/* TB_3000 */
    "Qblazer",				/* TB_QBLA */
    "WorldBlazer",			/* TB_WBLA */
    ""
};
#endif /* MINIDIAL */

extern int flow, local, mdmtyp, quiet, backgrd, parity, seslog, network;
extern int carrier, duplex;
#ifdef NETCONN
extern int ttnproto;
#endif /* NETCONN */
extern CHAR stchr;
extern long speed;
extern char ttname[], sesfil[];

/*
 * Failure reasons for use with the 'longjmp' exit.
 */
#define F_time		1		/* timeout */
#define F_int		2		/* interrupt */
#define F_modem		3		/* modem-detected failure */
#define F_minit		4		/* cannot initialize modem */

static int mymdmtyp;			/* Local copy of modem type. */
static int n1 = F_time;

_PROTOTYP (static int ddinc, (int) );
_PROTOTYP (int dialhup, (void) );
_PROTOTYP (int getok, (int,int) );
_PROTOTYP (char * getdws, (int) );
_PROTOTYP (static VOID ttslow, (char *s, int millisec) );
_PROTOTYP (VOID xcpy, (char *to, char *from, unsigned len) );
_PROTOTYP (static VOID waitfor, (char *s) );
_PROTOTYP (static VOID dialoc, (char c) );
_PROTOTYP (static int didweget, (char *s, char *r) );
_PROTOTYP (static VOID spdchg, (long s) );
_PROTOTYP (static VOID tbati3, (int n) );

#define MDMINF	struct mdminf

MDMINF		/* structure for modem-specific information */
    {
    int		dial_time;	/* time modem allows for dialing (secs) */
    char	*pause_chars;	/* character(s) to tell modem to pause */
    int		pause_time;	/* time associated with pause chars (secs) */
    char	*wake_str;	/* string to wakeup modem & put in cmd mode */
    int		wake_rate;	/* delay between wake_str characters (msecs) */
    char	*wake_prompt;	/* string prompt after wake_str */
    char	*dmode_str;	/* string to put modem in dialing mode */
    char	*dmode_prompt;	/* string prompt for dialing mode */
    char	*dial_str;	/* dialing string, with "%s" for number */
    int		dial_rate;	/* delay between dialing characters (msecs) */
    int		esc_time;	/* guard time on escape sequence (msecs) */
    char	*esc_str;	/* escape sequence */
    char	*hup_str;	/* hangup string */
    _PROTOTYP( int (*ok_fn), (int,int) ); /* func to read response string */
    };

/*
 * Define symbolic modem numbers.
 *
 * The numbers MUST correspond to the ordering of entries
 * within the ptrtab array, and start at one (1).
 *
 * It is assumed that there are relatively few of these
 * values, and that the high(er) bytes of the value may
 * be used for modem-specific mode information.
 *
 * REMEMBER that only the first eight characters of these
 * names are guaranteed to be unique.
 */

#ifdef MINIDIAL				/* Minimum dialer support */
					/* Only for CCITT, HAYES, and UNK */
#define		n_CCITT		 1
#define		n_HAYES		 2
#define		n_UNKNOWN	 3
#define		MAX_MDM		 3	/* Number of modem types */

#else					/* Full-blown dialer support */

#define		n_ATTDTDM	 1
#define         n_ATTISN         2
#define		n_ATTMODEM	 3
#define		n_CCITT		 4
#define		n_CERMETEK	 5
#define		n_DF03		 6
#define		n_DF100		 7
#define		n_DF200		 8
#define		n_GDC		 9
#define		n_HAYES		10
#define		n_PENRIL	11
#define		n_RACAL		12
#define		n_UNKNOWN	13
#define		n_USROBOT	14
#define		n_VENTEL	15
#define		n_CONCORD	16
#define		n_ATTUPC	17	/* aka UNIX PC and ATT7300 */
#define		n_ROLM          18      /* Rolm CBX DCM */
#define		n_MICROCOM	19
#define         n_HST           20
#define         n_TELEBIT       21      /* Telebits of all kinds */
#define         n_DIGITEL       22	/* Digitel DT-22 (CCITT variant) */
#define		MAX_MDM		22	/* Number of modem types */

#endif /* MINIDIAL */

/*
 * Declare modem "variant" numbers for any of the above for which it is
 * necessary to note various operational modes, using the second byte
 * of a modem number.
 *
 * It is assumed that such modem modes share the same modem-specific
 * information (see MDMINF structure) but may differ in some of the actions
 * that are performed.
 */

/*  Warning - this is starting to get kind of hokey... */

#define DIAL_NV 256
#define n_HAYESNV ( n_HAYES   | DIAL_NV )

#ifndef MINIDIAL
#define DIAL_PEP 512
#define DIAL_V32 1024
#define DIAL_V42 2048
#define DIAL_SLO 4096
#define n_TBPEP   ( n_TELEBIT | DIAL_PEP )
#define n_TB3     ( n_TELEBIT | DIAL_V32 )
#define n_TBNV    ( n_TELEBIT | DIAL_NV )
#define n_TBPNV   ( n_TELEBIT | DIAL_NV | DIAL_PEP )
#define n_TB3NV   ( n_TELEBIT | DIAL_NV | DIAL_V32 )
#define n_TB4     ( n_TELEBIT | DIAL_V42 )
#define n_TBS     ( n_TELEBIT | DIAL_SLO )
#define n_TB4NV   ( n_TELEBIT | DIAL_NV | DIAL_V42 )
#define n_TBSNV   ( n_TELEBIT | DIAL_NV | DIAL_SLO )
#endif /* MINIDIAL */

/*
 * Declare structures containing modem-specific information.
 *
 * REMEMBER that only the first SEVEN characters of these
 * names are guaranteed to be unique.
 */

#ifndef MINIDIAL
static
MDMINF ATTISN =				/* AT&T ISN Network */
    {
    30,					/* Dial time */
    "",					/* Pause characters */
    0,					/* Pause time */
    "\015\015\015\015",			/* Wake string */
    900,				/* Wake rate */
    "DIAL",				/* Wake prompt */
    "",					/* dmode_str */
    "",					/* dmode_prompt */
    "%s\015",				/* dial_str */
    0,					/* dial_rate */
    0,					/* esc_time */
    "",					/* esc_str */
    "",					/* hup_str */
    NULL				/* ok_fn */
    };

static
MDMINF ATTMODEM =	/* information for AT&T switched-network modems */
			/* "Number" following "dial" can include: p's and
			 * t's to indicate pulse or tone (default) dialing,
			 * + for wait for dial tone, , for pause, r for
			 * last number dialed, and, except for 2224B, some
			 * comma-delimited options like o12=y, before number.

 * "Important" options for the modems:
 *
 *	All:		Except for 2224B, enable option 12 for "transparent
 *			data," o12=y.  If a computer port used for both
 *			incoming and outgoing calls is connected to the
 *			modem, disable "enter interactive mode on carriage
 *			return," EICR.  The Kermit "dial" command can
 *			function with EIA leads standard, EIAS.
 *
 *	2212C:		Internal hardware switches at their default
 *			positions (four rockers down away from numbers)
 *			unless EICR is not wanted (rocker down at the 4).
 *			For EIAS, rocker down at the 1.
 *
 *	2224B:		Front-panel switch position 1 must be up (at the 1,
 *			closed).  Disable EICR with position 2 down.
 *			For EIAS, position 4 down.
 *			All switches on the back panel down.
 *
 *	2224CEO:	All front-panel switches down except either 5 or 6.
 *			Enable interactive flow control with o16=y.
 *			Select normal asynchronous mode with o34=0 (zero).
 *			Disable EICR with position 3 up.  For EIAS, 1 up.
 *			Reset the modem after changing switches.
 *
 *	2296A:		If option 00 (zeros) is present, use o00=0.
 *			Enable interactive flow control with o16=y.
 *			Select normal asynchronous mode with o34=0 (zero).
 *                      (available in Microcom Networking version, but
 *                      not necessarily other models of the 2296A).
 *			Enable modem-port flow control (if available) with
 * 			o42=y.  Enable asynchronous operation with o50=y.
 * 			Disable EICR with o69=n.  For EIAS, o66=n, using
 * 			front panel.
 */
    {
    20,			/* dial_time */
    ",",		/* pause_chars */
    2,			/* pause_time */
    "+",		/* wake_str */
    0,			/* wake_rate */
    "",			/* wake_prompt */
    "",			/* dmode_str */
    "",			/* dmode_prompt */
    "at%s\015",		/* dial_str */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL			/* ok_fn */
    };

static
MDMINF ATTDTDM =	/* information for AT&T Digital Terminal Data Module
 *			For dialing: KYBD switch down, others usually up. */
    {
    20,			/* dial_time */
    "",			/* pause_chars */
    0,			/* pause_time */
    "",			/* wake_str */
    0,			/* wake_rate */
    "",			/* wake_prompt */
    "",			/* dmode_str */
    "",			/* dmode_prompt */
    "%s\015",		/* dial_str */		/* not used */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };
#endif /* MINIDIAL */

static
MDMINF CCITT =				/* CCITT V.25bis autodialer */
/*
  According to V.25bis:
  . Even parity is required for giving commands to the modem.
  . Commands might or might not echo.
  . Responses ("Indications") from the modem are terminated by CR and LF.
  . Call setup is accomplished by:
    - DTE raises DTR (V.24 circuit 108)              [ttopen() does this]
    - Modem raises CTS (V.24 circuit 106)            [C-Kermit ignores this]
    - DTE issues a call request command ("CRN")
    - Modem responds with "VAL" ("command accepted")
    - If the call is completed:
        modem responds with "CNX" ("call connected");
        modem turns CTS (106) OFF;
        modem turns DSR (107) ON;
      else:
        modem responds with "CFI <parameter>" ("call failure indication").
  . To clear a call, the DTE turns DTR (108) OFF.
  . There is no mention of the Carrier Detect circuit (109) in the standard.
  . There is no provision for "escaping back" to the modem's command mode.

  It is not known whether there exists in real life a pure V.25bis modem.
  If there is, this code has never been tested on it.  See the Digitel entry.
*/
    {
    40,			/* dial_time -- programmable -- */
    ",:",		/* pause_chars -- "," waits for programmable time */
                        /* ":" waits for dial tone */
    10,			/* pause_time (seconds, just a guess) */
    "",			/* wake_str (none) */
    200,		/* wake_rate (msec) */
    "VAL",		/* wake_prompt */
    "",			/* dmode_str (none) */
    "",			/* dmode_prompt (none) */
    "CRN%s\015",        /* dial_str */
    200,		/* dial_rate (msec) */
    0,			/* No esc_time */
    "",			/* No esc_str  */
    "",			/* No hup_str  */
    NULL		/* No ok_fn    */
    };

#ifndef MINIDIAL	/* Don't include the following if -DMINIDIAL ... */

static
MDMINF CERMETEK =	/* Information for "Cermetek Info-Mate 212 A" modem */
    {
    20,			/* dial_time */
    "BbPpTt",		/* pause_chars */
    0,			/* pause_time */	/** unknown -- DS **/
    "  XY\016R\015",	/* wake_str */
    200,		/* wake_rate */
    "",			/* wake_prompt */
    "",			/* dmode_str */
    NULL,		/* dmode_prompt */
    "\016D '%s'\015",	/* dial_str */
    200,		/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };

static
MDMINF DF03 =		/* information for "DEC DF03-AC" modem */
    {
    27,			/* dial_time */
    "=",		/* pause_chars */	/* wait for second dial tone */
    15,			/* pause_time */
    "\001\002",		/* wake_str */
    0,			/* wake_rate */
    "",			/* wake_prompt */
    "",			/* dmode_str */
    NULL,		/* dmode_prompt */
    "%s",		/* dial_str */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };

static
MDMINF DF100 =		/* information for "DEC DF100-series" modem */
			/*
			 * The telephone "number" can include "P"s and/or "T"s
			 * within it to indicate that subsequent digits are
			 * to be dialed using pulse or tone dialing.  The
			 * modem defaults to pulse dialing.  You may modify
			 * the dial string below to explicitly default all
			 * dialing to pulse or tone, but doing so prevents
			 * the use of phone numbers that you may have stored
			 * in the modem's memory.
			 */
    {
    30,			/* dial_time */
    "=",		/* pause_chars */	/* wait for second dial tone */
    15,			/* pause_time */
    "\001",		/* wake_str */
    0,			/* wake_rate */
    "",			/* wake_prompt */
    "",			/* dmode_str */
    NULL,		/* dmode_prompt */
    "%s#",		/* dial_str */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };

static
MDMINF DF200 =		/* information for "DEC DF200-series" modem */
			/*
			 * The telephone "number" can include "P"s and/or "T"s
			 * within it to indicate that subsequent digits are
			 * to be dialed using pulse or tone dialing.  The
			 * modem defaults to pulse dialing.  You may modify
			 * the dial string below to explicitly default all
			 * dialing to pulse or tone, but doing so prevents
			 * the use of phone numbers that you may have stored
			 * in the modem's memory.
			 */
    {
    30,			/* dial_time */
    "=W",		/* pause_chars */	/* =: second tone; W: 5 secs */
    15,			/* pause_time */	/* worst case */
    "\002",		/* wake_str */		/* allow stored number usage */
    0,			/* wake_rate */
    "",			/* wake_prompt */
    "",			/* dmode_str */
    NULL,		/* dmode_prompt */
    "%s!",		/* dial_str */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };

static
MDMINF DIGITEL =        /* Digitel DT-22 CCITT variant used in Brazil */
/*
  Attempts to adhere strictly to the V.25bis specification do not produce good
  results in real life.  The modem for which this code was developed: (a)
  ignores parity; (b) sometimes terminates responses with LF CR instead of CR
  LF; (c) has a Hayes-like escape sequence; (d) supports a hangup ("HUP")
  command.  Information from Fernando Cabral in Brasilia.
*/
    {
    40,			/* dial_time -- programmable -- */
    ",:",		/* pause_chars -- "," waits for programmable time */
                        /* ":" waits for dial tone */
    10,			/* pause_time (seconds, just a guess) */
    "HUP\015",          /* wake_str (Not Standard CCITT) */
    200,		/* wake_rate (msec) */
    "VAL",		/* wake_prompt */
    "",			/* dmode_str (none) */
    "",			/* dmode_prompt (none) */
    "CRN%s\015",        /* dial_str */
    200,		/* dial_rate (msec) */
    1100,		/* esc_time (Not Standard CCITT) */
    "+++",		/* esc_str  (Not Standard CCITT) */
    "HUP\015",		/* hup_str  (Not Standard CCITT) */
    getok		/* ok_fn */
    };

static
MDMINF GDC =		/* information for "GeneralDataComm 212A/ED" modem */
    {
    32,			/* dial_time */
    "%",		/* pause_chars */
    3,			/* pause_time */
    "\015\015",		/* wake_str */
    500,		/* wake_rate */
    "$",		/* wake_prompt */
    "D\015",		/* dmode_str */
    ":",		/* dmode_prompt */
    "T%s\015",		/* dial_str */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };
#endif /* MINIDIAL */

static
MDMINF HAYES =		/* Information for Hayes and Hayes-like modems */
    {
    35,			/* dial_time */
    ",",		/* pause_chars */
    2,			/* pause_time */
    "ATQ0\015",		/* wake_str */
/*
  Note: Other wake_str's are possible here.  For a Hayes 2400 that is to
  be used for both in and out calls, AT&F&D3 might be best.  For out calls
  only, maybe AT&F&D2.  See Hayes 2400 manual.
*/
    0,			/* wake_rate */
    "",			/* wake_prompt */
    "",			/* dmode_str */
    "",			/* dmode_prompt */
    "ATD%s\015",	/* dial_str, note: user can supply D or T */
    0,			/* dial_rate */
    1100,		/* esc_time */
    "+++",		/* esc_str */
    "ATQ0H0\015",	/* hup_str */
    getok		/* ok_fn */
    };

#ifndef MINIDIAL

static
MDMINF PENRIL =		/* information for "Penril" modem */
    {
    50,			/* dial_time */
    "",			/* pause_chars */	/** unknown -- HF **/
    0,			/* pause_time */
    "\015\015",		/* wake_str */
    300,		/* wake_rate */
    ">",		/* wake_prompt */
    "k\015",		/* dmode_str */
    ":",		/* dmode_prompt */
    "%s\015",		/* dial_str */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };

static
MDMINF RACAL =		/* information for "Racal Vadic" modem, e.g. VA4492E */
    {
    35,			/* dial_time (manual says modem is hardwired to 60) */
    "Kk",		/* pause_chars */
    5,			/* pause_time */
    "\005\015",		/* wake_str, ^E^M */
    50,			/* wake_rate */
    "*",		/* wake_prompt */
    "D\015",		/* dmode_str */
    "?",		/* dmode_prompt */
    "%s\015",		/* dial_str */
    0,			/* dial_rate */
    1100,		/* esc_time */
    "\003\004",		/* esc_str, ^C^D (this actually hangs up) */
    "\005",		/* hup_str, ^E (this goes back to command mode) */
    NULL		/* ok_fn */
    };
#endif /* MINIDIAL */

/*
  The intent of the "unknown" modem is to allow KERMIT to support
  unknown modems by having the user type the entire autodial sequence
  (possibly including control characters, etc.) as the "phone number".
  The protocol and other characteristics of this modem are unknown, with
  some "reasonable" values being chosen for some of them.  The only way to
  detect if a connection is made is to look for carrier.
*/
static
MDMINF UNKNOWN =	/* information for "Unknown" modem */
    {
    30,			/* dial_time */
    "",			/* pause_chars */
    0,			/* pause_time */
    "",			/* wake_str */
    0,			/* wake_rate */
    "",			/* wake_prompt */
    "",			/* dmode_str */
    NULL,		/* dmode_prompt */
    "%s\015",		/* dial_str */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };

#ifndef MINIDIAL

static
MDMINF USROBOT =	/* information for "US Robotics 212A" modem */
    {
    30,			/* dial_time */
    ",",		/* pause_chars */
    2,			/* pause_time */
    "ATS2=01\015",	/* wake_str */
    0,			/* wake_rate */
    "OK\015",		/* wake_prompt */
    "",			/* dmode_str */
    NULL,		/* dmode_prompt */
    "ATTD%s\015",	/* dial_str */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };

#ifdef COMMENT
/* Reportedly this does not work at all. */
static
MDMINF VENTEL =		/* information for "Ventel" modem */
    {
    20,			/* dial_time */
    "%",		/* pause_chars */
    5,			/* pause_time */
    "\015\015\015",	/* wake_str */
    300,		/* wake_rate */
    "$",		/* wake_prompt */
    "",			/* dmode_str */
    NULL,		/* dmode_prompt */
    "<K\015%s\015>",    /* dial_str (was "<K%s\r>") */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };
#else
/* and this does. */
static
MDMINF VENTEL =		/* information for "Ventel" modem */
    {
    20,			/* dial_time */
    "%",		/* pause_chars */
    5,			/* pause_time */
    "\015\015\015",	/* wake_str */
    300,		/* wake_rate */
    "$",		/* wake_prompt */
    "K\015",		/* dmode_str (was "") */
    "Number to call: ",	/* dmode_prompt (was NULL) */
    "%s\015",	        /* dial_str (was "<K%s\r>") */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };
#endif /* COMMENT */

static
MDMINF CONCORD =	/* Info for Condor CDS 220 2400b modem */
    {
    35,			/* dial_time */
    ",",		/* pause_chars */
    2,			/* pause_time */
    "\015\015",		/* wake_str */
    20,			/* wake_rate */
    "CDS >",		/* wake_prompt */
    "",			/* dmode_str */
    NULL,		/* dmode_prompt */
    "<D M%s\015>",	/* dial_str */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };

static
MDMINF ATTUPC = /* dummy information for "ATT7300/Unix PC" internal modem */
    {
    30,			/* dial_time */
    "",			/* pause_chars */
    0,			/* pause_time */
    "",			/* wake_str */
    0,			/* wake_rate */
    "",			/* wake_prompt */
    "",			/* dmode_str */
    NULL,		/* dmode_prompt */
    "%s\015",		/* dial_str */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };

static
MDMINF ROLM =		/* IBM / Siemens / Rolm 8000, 9000, 9751 CBX */
    {
    60,			/* dial_time */
    "",			/* pause_chars */
    0,			/* pause_time */
    "\015\015",		/* wake_str */
    5,			/* wake_rate */
    "MODIFY?",	        /* wake_prompt */
    "",			/* dmode_str */
    "",			/* dmode_prompt */
    "CALL %s\015",	/* dial_str */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };

static
MDMINF MICROCOM =	/* information for "Microcom" modems in native mode */
			/* (long answer only) */
    {
    35,			/* dial_time */
    ",!@",		/* pause_chars (! and @ aren't pure pauses) */
    3,			/* pause_time */
    "\015",		/* wake_str */
    100,		/* wake_rate */
    "!",		/* wake_prompt */
    "",			/* dmode_str */
    NULL,		/* dmode_prompt */
    "d%s\015",		/* dial_str */
    0,			/* dial_rate */
    0,			/* esc_time */
    "",			/* esc_str */
    "",			/* hup_str */
    NULL		/* ok_fn */
    };

static
MDMINF HST =		/* information for Courier HST modem */
    {
    35,			/* dial_time */
    ",",		/* pause_chars */
    2,			/* pause_time */
    "ATQ0S2=43X7&B1&M4\015", /* wake_str */
    0,		        /* wake_rate */
    "OK\015",		/* wake_prompt */
    "",		        /* dmode_str */
    "",		        /* dmode_prompt */
    "ATD%s\015",	/* dial_str */
    0,			/* dial_rate */
    1100,		/* esc_time */
    "+++",		/* esc_str */
    "ATQ0H0\015",	/* hup_str */
    getok		/* ok_fn */
    };

static
MDMINF TELEBIT =	/* information for Telebits */
    {
    60,			/* dial_time */
    ",",		/* pause_chars */
    2,			/* pause_time */
/*
  NOTE: The wake_string MUST contain the I command (model query), and otherwise
  must contain commands that work on ALL Telebit models.  Here we ensure that
  result codes are returned (Q0), and ask for extended result codes (X1), and
  ensure that the escape sequence is +++ and it is enabled.  And also, make
  sure the final character is not a digit (whose echo might be mistaken for a
  result code).  The Ctrl-Q and multiple A's are recommended by Telebit.
*/
    "\021AAAATQ0X1S12=50 S2=43 I\015", /* wake_str. */
    100,		/* wake_rate = 100 msec */
    "OK\015",		/* wake_prompt */
    "",		        /* dmode_str */
    "",		        /* dmode_prompt */
    "ATD%s\015",	/* dial_str, Note: no T or P */
    80,			/* dial_rate */
    1100,		/* esc_time (guard time) */
    "+++",		/* esc_str */
    "ATQ0H0\015",	/* hup_str */
    getok		/* ok_fn */
    };
#endif /* MINIDIAL */

/*
 * Declare table for converting modem numbers to information pointers.
 *
 * The entries MUST be in ascending order by modem number, without any
 * "gaps" in the numbers, and starting from one (1).
 *
 * This table should NOT include entries for the "variant" modem numbers,
 * since they share the same information as the normal value.
 */
static
MDMINF *ptrtab[] = {
#ifdef MINIDIAL
    &CCITT,
    &HAYES,
    &UNKNOWN
#else
    &ATTDTDM,
    &ATTISN,
    &ATTMODEM,
    &CCITT,
    &CERMETEK,
    &DF03,
    &DF100,
    &DF200,
    &GDC,
    &HAYES,
    &PENRIL,
    &RACAL,
    &UNKNOWN,
    &USROBOT,
    &VENTEL,
    &CONCORD,
    &ATTUPC,
    &ROLM,
    &MICROCOM,
    &HST,
    &TELEBIT,
    &DIGITEL
#endif /* MINIDIAL */
};
/*
 * Declare modem names and associated numbers for command parsing,
 * and also for doing number-to-name translation.
 *
 * The entries must be in alphabetical order by modem name.
 */
struct keytab mdmtab[] = {
#ifndef MINIDIAL
    "attdtdm",		n_ATTDTDM,	0,
    "attisn",           n_ATTISN,       0,
    "attmodem",		n_ATTMODEM,	0,
    "att7300",		n_ATTUPC,	0,
#endif /* MINIDIAL */
    "ccitt-v25bis",	n_CCITT,	0,
#ifndef MINIDIAL
    "cermetek",		n_CERMETEK,	0,
    "concord",		n_CONCORD,	0,
    "df03-ac",		n_DF03,		0,
    "df100-series",	n_DF100,	0,
    "df200-series",	n_DF200,	0,
    "digitel-dt22",	n_DIGITEL,	0,
#endif /* MINIDIAL */
    "direct",		0,		CM_INV,	/* Synonym for NONE */
#ifndef MINIDIAL
    "gdc-212a/ed",	n_GDC,		0,
    "gendatacomm",	n_GDC,		CM_INV,	/* Synonym for GDC */
#endif /* MINIDIAL */
    "hayes",		n_HAYES,	0,
#ifndef MINIDIAL
    "hst-courier",      n_HST,          0,
    "microcom",		n_MICROCOM,	0,
#endif /* MINIDIAL */
    "none",             0,              0,
#ifndef MINIDIAL
    "penril",		n_PENRIL,	0,
    "pep-telebit",      n_TBPEP,        0,
    "racalvadic",	n_RACAL,	0,
    "rolm",		n_ROLM,		0,
    "slow-telebit",     n_TBS,		0,
    "telebit",          n_TELEBIT,      0,
#endif /* MINIDIAL */
    "unknown",		n_UNKNOWN,	0,
#ifndef MINIDIAL
    "usrobotics-212a",	n_USROBOT,	0,
    "v32-telebit",      n_TB3,		0,
    "v42-telebit",      n_TB4,		0,
    "ventel",		n_VENTEL,	0
#endif /* MINIDIAL */
};
int nmdm = (sizeof(mdmtab) / sizeof(struct keytab)); /* Number of modems */

#define CONNECTED 1			/* For completion status */
#define FAILED	  2

static
char *F_reason[5] = {			/* Failure reasons for message */
    "Unknown",	"Timeout", "Interrupt", "Modem", "Initialize" };

static int tries = 0;
static int mdmecho = 0;	/* assume modem does not echo */
static int augmdmtyp;	/* "augmented" modem type, to handle modem modes */

static char *p;		/* For command strings & messages */

#define LBUFL 100
#ifdef DYNAMIC
static char *lbuf = NULL;
#else
static char lbuf[LBUFL];
#endif /* DYNAMIC */

#define RBUFL 63
static char rbuf[RBUFL+1];	/* Modem response buffer */

static jmp_buf sjbuf;

static SIGTYP (*savalrm)();	/* For saving alarm handler */
static SIGTYP (*savint)();	/* For saving interrupt handler */

#ifndef MINIDIAL
int tbmodel = 0;		/* Telebit modem model */

char *
gtbmodel() {			/* Function to return name of Telebit model */
    if (tbmodel < 0 || tbmodel > TB__MAX) tbmodel = TB_UNK;
    return(tb_name[tbmodel]);
}

VOID
xcpy(to,from,len)		/* Copy the given number of bytes */
    register char *to, *from;
    register unsigned len; {
	while (len--) *to++ = *from++;
}
#endif /* MINIDIAL */

SIGTYP
dialtime(foo) int foo; {		/* Timer interrupt handler */
    n1 = F_time;			/* Failure reason = timeout */

#ifdef __EMX__
    signal(SIGALRM, SIG_ACK);		/* Needed for OS/2 */
#endif /* __EMX__ */

#ifdef OSK				/* OS-9 */
/*
  We are in an intercept routine but do not perform a F$RTE (done implicitly
  by RTS), so we have to decrement the sigmask as F$RTE does.  Warning:
  longjump only restores the CPU registers, NOT the FPU registers.  So, don't
  use FPU at all or at least don't use common FPU (double or float) register
  variables.
*/
    sigmask(-1);
#endif /* OSK */

    longjmp( sjbuf, F_time );
}

SIGTYP
dialint(foo) int foo; {			/* Keyboard interrupt handler */
    n1 = F_int;
#ifdef __EMX__
    signal(SIGINT, SIG_ACK);		/* Needed for OS/2 */
#endif /* __EMX__ */
#ifdef OSK				/* OS-9, see comment in dialtime() */
    sigmask(-1);
#endif /* OSK */
    longjmp( sjbuf, F_int );
}

/*
  Routine to read a character from communication device, handling TELNET
  protocol negotiations in case we're connected to the modem through a
  TCP/IP TELNET modem server.
*/
static int
ddinc(n) int n; {
    int c;

#ifdef TNCODE
    int done = 0;
    while (!done) {
	c = ttinc(n);
	debug(F000,"ddinc","",c);
	if (c < 0) return(c);
	if (c == IAC && network && ttnproto == NP_TELNET) {
	    switch (tn_doop((CHAR)(c & 0xff),duplex,ttinc)) {
	      case 2: duplex = 0; continue;
	      case 1: duplex = 1;
	      default: continue;
	    }
	} else done = 1;
    }
    return(c & 0xff);
#else
    return(ttinc(n));
#endif /* TNCODE */
}

static VOID
ttslow(s,millisec) char *s; int millisec; { /* Output s-l-o-w-l-y */
    for (; *s; s++) {
	ttoc(*s);
	msleep(millisec);
    }
}

/*
 * Wait for a string of characters.
 *
 * The characters are waited for individually, and other characters may
 * be received "in between".  This merely guarantees that the characters
 * ARE received, and in the order specified.
 */
static VOID
waitfor(s) char *s; {
    CHAR c, x;
    while ( c = *s++ ) {		/* while more characters remain... */
	do {				/* wait for the character */
	    x = ddinc(0) & 0177;
	    debug(F000,"dial waitfor got","",x);
	    if (dialdpy) {
		if (x != LF) conoc(x);
		if (x == CR) conoc(LF);
	    }
	} while ( x != c);
    }
}

static int
didweget(s,r) char *s, *r; {	/* Looks in string s for response r */
    int lr = (int)strlen(r);	/*  0 means not found, 1 means found it */
    int i;
    debug(F110,"didweget",r,0);
    debug(F110," in",s,0);
    for (i = (int)strlen(s)-lr; i >= 0; i--)
	if ( s[i] == r[0] ) if ( !strncmp(s+i,r,lr) ) return( 1 );
    return( 0 );
}


/* R E S E T -- Reset alarms, etc. on exit. */

static VOID
dreset() {
    alarm(0);
    signal(SIGALRM,savalrm);		/* restore alarm handler */
    signal(SIGINT,savint);		/* restore interrupt handler */
}

/*
  Call this routine when the modem reports that it has connected at a certain
  speed, giving that speed as the argument.  If the connection speed is not
  the same as Kermit's current communication speed, AND the modem interface
  speed is not locked (i.e. DIAL SPEED-MATCHING is not ON), then change the
  device speed to the one given.
*/
static VOID
#ifdef CK_ANSIC
spdchg(long s)
#else
spdchg(s) long s;
#endif /* CK_ANSIC */
/* spdchg */ {
    int s2;
    if (!mdmspd)			/* If modem interface speed locked, */
      return;				/*  don't do this. */
    if (speed != s) {			/* Speeds differ? */
	s2 = s / 10L;			/* Convert to cps expressed as int */
	if (ttsspd(s2) < 0) {		/* Change speed. */
	    printf(" Warning: speed change to %ld failed.\r\n",s);
	} else {
	    printf(" Speed changed to %ld.\r\n",s);
	    speed = s;			/* Update global speed variable */
	}
    }
}

/*
  Display all characters received from modem dialer through this routine,
  for consistent handling of carriage returns and linefeeds.
*/
static VOID
#ifdef CK_ANSIC
dialoc(char c)
#else
dialoc(c) char c;
#endif /* CK_ANSIC */
{ /* dialoc */			/* Dial Output Character */
    if (dialdpy) {
	if (c != LF) conoc(c);		/* Don't echo LF */
	if (c == CR) conoc(LF);		/* Echo CR as CRLF */
    }
}

#ifndef MINIDIAL
/*
  tbati3() -- Routine to find out Telebit model when ATI reports "965"
  or "971". This routine sends another query, ATI3, to get further info
  to narrow down the model number.  Argument is ATI response as integer.
  Result: sets tbmodel variable to Telebit model.
*/
static VOID
tbati3(n) int n; {
    int status;
    ttflui();				/* Flush input buffer */
    ttslow("ATI3\015",100);		/* Send ATI3<CR> */
    status = getok(5,0);		/* Get OK response, nonstrict */
    if (status < 1) {			/* ERROR or timeout */
	tbmodel = TB_UNK;
	debug(F111,"tbati3 fails",rbuf,status);
	return;
    }
    debug(F110,"tbati3 rbuf",rbuf,0);

/* Got a good response, check the model info */

    if (n == 965) {			/* "965" - various models. */
	if (didweget(rbuf,"T1600")) {
	    tbmodel = TB_1600;			/* T1600 */
	} else if (didweget(rbuf,"T3000")) {
	    tbmodel = TB_3000;			/* T3000 */
	} else if (didweget(rbuf,"World")) {
	    tbmodel = TB_WBLA;			/* WorldBlazer */
	} else if (didweget(rbuf,"Version B") || /* TrailBlazer-Plus models */
		   didweget(rbuf,"TBSA") ||
		   didweget(rbuf,"TBRM") ||
		   didweget(rbuf,"DC")) { 	/* Ven-Tel EC18K */
	    tbmodel = TB_PLUS;
	} else tbmodel = TB_UNK;		/* Others: Unknown */

    } else if (n == 971) {		/* "971" could be T1500 or T1600. */
	if (didweget(rbuf,"T1500"))
	  tbmodel = TB_1500;
	else tbmodel = TB_2500;
    }					/* Other, don't change tbmodel. */
}
#endif /* MINIDIAL */


/*  C K D I A L	 --  Dial up the remote system */

/* Returns 1 if call completed, 0 otherwise */

static int waitct, mdmwait, mdmstat = 0;

int
ckdial(telnbr) char *telnbr; {

    char c, c2;
    char errmsg[50], *erp;		/* for error messages */
    char fullnum[100];			/* for full (prefixed) phone number */
    MDMINF *pmdminf;			/* pointer to modem-specific info */
    int x, m, n = F_time;
    char *s, *pc, *ws;

    long conspd;
    char *cptr;

    mymdmtyp = mdmtyp;			/* Make local copy of modem type */

#ifndef MINIDIAL
    tbmodel = TB_UNK;			/* Initialize Telebit model */
#endif /* MINIDIAL */

    if (mymdmtyp < 1) {
	if (network)
	  printf("Please SET HOST first, and then SET MODEM\n");
	else
	  printf("Sorry, you must SET MODEM first\n");
	return(0);
    }
    if (!local) {
	printf("Sorry, you must SET LINE or SET HOST first\n");
	return(0);
    }
    if (!network && speed < 0L) {
	printf("Sorry, you must SET SPEED first\n");
	return(0);
    }
    debug(F110,"dial number",telnbr,0);
    debug(F110,"dial prefix",(dialnpr ? dialnpr : ""), 0);

/* Add prefix to phone number */

    if (dialnpr && *dialnpr) {
	sprintf(fullnum,"%s%s",(dialnpr ? dialnpr : ""),telnbr);
	telnbr = fullnum;
    }
    debug(F110,"prefixed number", telnbr, 0);

#ifdef DYNAMIC
    if (!(lbuf = malloc(LBUFL+1))) {    /* Allocate input line buffer */
	printf("Sorry, DIAL memory buffer can't be allocated\n");
	return(0);
    }
#endif /* DYNAMIC */

    if (ttopen(ttname,&local,mymdmtyp,0) < 0) { /* Open, no carrier wait */
	erp = errmsg;
	sprintf(erp,"Sorry, can't open %s",ttname);
	perror(errmsg);
	return(0);
    }

/* Condition console terminal and communication line */

    /* Place line into "clocal" dialing state, */
    /* important mainly for System V UNIX.     */

    if (ttpkt(speed,FLO_DIAL,parity) < 0) {
	ttclos(0);			/* If ttpkt fails do all this... */
	if (ttopen(ttname,&local,mymdmtyp,0) < 0) {
	    erp = errmsg;
	    sprintf(erp,"Sorry, can't reopen %s",ttname);
	    perror(errmsg);
	    return(0);
	}				/* And try again. */
	if (ttpkt(speed,FLO_DIAL,parity) < 0) {
	    printf("Sorry, Can't condition communication line\n");
	    printf("Try 'set line %s' again\n",ttname);
	    return(0);
	}
    }
    msleep(500);

    pmdminf = ptrtab[ (mymdmtyp & 0xff) -1 ]; /* set pointer to modem info */
    augmdmtyp = mymdmtyp;		/* initialize "augmented" modem type */
    mymdmtyp &= 0xff;			/* major modem type */

    /* Interdigit waits for tone dial */

    if (dialtmo < 1) {			/* Automatic computation. */
	waitct = 1 * (int)strlen(telnbr) ; /* Compute worst case dial time */
	waitct += pmdminf->dial_time;	/* dialtone + completion wait times */
	for (s = telnbr; *s; s++) {	/* add in pause characters time */
	    for (p=pmdminf->pause_chars; *p; p++)
	      if (*s == *p) {
		  waitct += pmdminf->pause_time;
		  break;
	      }
	}
#ifndef MINIDIAL
	if (augmdmtyp == n_TBPEP || augmdmtyp == n_TBPNV) {
	    waitct += 30;	/* Longer connect wait for PEP call */
	}
#endif /* MINIDIAL */
    } else {				/* User-specified timeout */
	waitct = dialtmo;
    }
/*
  waitct is our alarm() timer.
  mdmwait is how long we tell the modem to wait for carrier.
  We set mdmwait to be 1 second less than waitct, to increase the
  chance that we get a response from the modem before timing out.
*/
    if (waitct < 0) waitct = 0;
    mdmwait = (waitct > 5) ? waitct - 5 : waitct;

    for (m = 0; m < nmdm; m++) {	/* Look up modem type. */
	if (mdmtab[m].kwval == mymdmtyp) {
	    break;
	}
    }
    if (!quiet && !backgrd) {		/* Print information messages. */
	printf(" Dialing: %s\n", telnbr);
	if (network) {
	    printf(" Via modem server: %s, modem-dialer: %s\n",
		   ttname, (m >= nmdm ? "(unknown)" : mdmtab[m].kwd) );
	} else {
	    printf(" Device: %s, modem-dialer: %s",
		   ttname, (m >= nmdm ? "(unknown)" : mdmtab[m].kwd) );
	    if (speed > -1L)
	      printf(", speed: %ld\n", speed);
	    else
	      printf(", speed: (unknown)\n", speed);
	}
	printf(" Dial timeout: %d seconds\n",waitct);
	printf(
#ifdef MAC
	       " Type Command-. to cancel dialing\n"
#else
#ifdef UNIX
	       " To cancel: type your interrupt character (normally Ctrl-C)\n"
#else
	       " To cancel: type Ctrl-C\n"
#endif /* UNIX */
#endif /* MAC */
	       );
    }
    debug(F111,"ckdial",ttname,(int) (speed / 10L));
    debug(F101,"ckdial timeout","",waitct);

/* Hang up the modem (in case it wasn't "on hook") */
/* But only if SET DIAL HANGUP ON... */

    if (dialhup() < 0) {
	debug(F100,"ckdial dialhup failed","",0);
#ifndef MINIDIAL
	if (mymdmtyp == n_TELEBIT)	/* Telebit might need a BREAK */
	  ttsndb();			/*  first. */
#endif /* MINIDIAL */
	if (dialhng) {			/* If it failed, */
	    ttclos(0);			/* close and reopen the device. */
            if (ttopen(ttname,&local,mymdmtyp,0) < 0) {
		printf("Sorry, Can't hang up communication device\n");
		printf("Try 'set line %s' again\n",ttname);
		return(0);
	    }
	}
    }
#ifndef MINIDIAL
    if (augmdmtyp == n_ROLM)		/* Don't start talking to Rolm */
      msleep(500);			/* too soon... */
#endif /* MINIDIAL */

/*
  Establish jump vector, or handle "failure" jumps.
  longjmp() sets global failure reason, n1.
*/
    if (setjmp(sjbuf)) {		/* if a "failure jump" was taken... */
	n = n1;
	alarm(0);			/* disable timeouts */
	if (setjmp(sjbuf)) {		/* failure while handling failure */
	    printf ("%s failure while handling failure.\n", F_reason[n1]);
	} else {			/* first (i.e., non-nested) failure */
	    signal(SIGALRM, dialtime);	/* be sure to catch signals */
#ifdef MAC
	    signal(SIGINT, dialint);
#else /* MAC */
#ifdef OSK
	    if (signal(SIGINT, SIG_IGN) != (SIGTYP (*)()) SIG_IGN)
		signal(SIGINT, dialint);
#else
	    if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, dialint);
#endif /* OS9 */
#endif /* MAC */
	    alarm(10);			/* Be sure to exit this section */
	}
	switch (n) {			/* Type of failure */
	    case F_time: {		/* Timed out */
		printf ("DIAL TIMEOUT interval expired.\n");
		if (mymdmtyp == n_HAYES
#ifndef MINIDIAL
		    || mymdmtyp == n_TELEBIT || mymdmtyp == n_HST
#endif /* MINIDIAL */
		    )
		  ttoc('\015');		/* Send CR to interrupt dialing */
  		  /* some hayes modems don't fail with BUSY on busy lines */
		debug(F110,"dial","timeout",0);
		break;
	    }
	    case F_int: {		/* Dialing interrupted */
		printf ("Dialing interrupted.\n");
		debug(F110,"dial","interrupted",0);
		if (mymdmtyp == n_HAYES
#ifndef MINIDIAL
		    || mymdmtyp == n_TELEBIT || mymdmtyp == n_HST
#endif /* MINIDIAL */
		    )
		  ttoc('\015');		/* Send CR to interrupt dialing */
		break;
	    }
	    case F_modem: {		/* Modem detected a failure */
		printf ("Call failed (\"");
		for (pc = lbuf; *pc; pc++)
		  if (isprint(*pc))
		    putchar(*pc);	/* Display printable reason */
		printf ("\").\n");
		debug(F110,"dial",lbuf,0);
		break;
	    }
	    case F_minit: {		/* Cannot initialize modem */
		printf ("Can't initialize modem.\n");
		debug(F110,"dial","modem init",0);
		break;
	    }
	}
	dreset();			/* reset alarms, etc. */
#ifdef DYNAMIC
	if (lbuf) free(lbuf);
#endif /* DYNAMIC */
	return(0);			/* exit with failure code */
    }

/* Set timer and interrupt handlers. */

    alarm(0);				/* No alarms yet. */
#ifdef MAC
    savint = signal(SIGINT, dialint);	/* And terminal interrupt handler. */
#else /* MAC */
#ifdef OSK
    if ((savint = signal(SIGINT,SIG_IGN)) != (SIGTYP (*)()) SIG_IGN)
      signal(SIGINT,dialint);
#else
    if ((savint = signal(SIGINT,SIG_IGN)) != SIG_IGN )
      signal(SIGINT,dialint);
#endif /* OS-9 */
#endif /* MAC */

/* Put modem in command mode. */

switch (augmdmtyp) {			/* Send the wakeup string */

#ifdef ATT7300
	case n_ATTUPC: {
/*
  For ATT7300/Unix PC's with their special internal modem.  Timeout
  and user interrupts are enabled during dialing.  attdial() is in
  file ckutio.c.  - jrd
*/
_PROTOTYP( int attdial, (char *, long, char *) );
            savalrm = signal(SIGALRM,dialtime); /* Set alarm handler. */
	    alarm(waitct);			/* do alarm properly */
	    if (attdial(ttname,speed,telnbr)) { /* dial internal modem */
		dreset();			/* reset alarms, etc. */
		printf(" Call failed.\r\n");
		dialhup();	        	/* Hangup the call */
#ifdef DYNAMIC
		if (lbuf) free(lbuf);
#endif /* DYNAMIC */
		return(0);			/* return failure */
	    }
	    dreset();				/* reset alarms, etc. */
	    ttpkt(speed,FLO_DIAX,parity);	/* cancel dialing ioctl */
	    if (!quiet && !backgrd) {
		if (dialdpy) printf("\n");
		printf(" Call complete.\07\r\n");
	    }
#ifdef DYNAMIC
            if (lbuf) free(lbuf);
#endif /* DYNAMIC */
	    return(1);	 /* no conversation with modem to complete dialing */
	}
#endif /* ATT7300 */

/*
  For Hayes modem command language, figure out if modem is giving verbose
  or digit result codes.
*/
    case n_HAYES:
    case n_HAYESNV:
#ifndef MINIDIAL
    case n_HST:
	if (augmdmtyp == n_HST)
	  m = 255;
	else
#endif /* MINIDIAL */
	  m = 60;			/* Maximum value for S7 */
	ws = dialini ? dialini : HAYES.wake_str;
	for (tries = 4; tries > 0; tries--) {
	    ttslow(ws,pmdminf->wake_rate);
	    mdmstat = getok(4,1);
	    if (mdmstat > 0) break;
	    sleep(1);			/* Wait before retrying */
	}
	if (mdmstat > 0) { 		/* Initialized OK. */
	    char hbuf[10];
/*
  Now tell the modem about our dial timeout.  For Hayes 1200, the maximum
  is 60 seconds.  For Hayes 2400 (the manual says) it is 30 seconds, but I'm
  not sure I believe this (I don't have one handy to see for myself).
  If you give the modem a value larger than its maximum, it sets the timeout
  to its maximum.  The manual does not say what happens when the value is 0,
  but experimentation shows that it allows the modem to wait forever, in
  which case Kermit will time out at the desired time.
*/
/*
  Note by Kai Uwe Rommel:
  This is not acceptable for general use of the hayes modem type with other
  compatible modems. Most other modems allow a range of 1..255 while 0 is
  invalid on several modems. Let it be the user's responsibility to make
  sure a valid value is used. Higher values are desirable for users with
  rotary dialing and with high speed modems, where protocol negotiation takes
  quite a long time.
*/
#ifdef COMMENT
	    if (mdmwait > m)		/* If larger than maximum, */
	      mdmwait = 0;		/* make it infinite. */
#else
	    if (mdmwait > 255)		/* If larger than maximum, */
	      mdmwait = 255;		/* make it maximum. */
#endif /* COMMENT */

	    sprintf(hbuf,"ATS7=%d%c",mdmwait,13); /* S7 = Carrier wait time */
	    ttslow(hbuf,pmdminf->wake_rate); /* Set it. */
	    mdmstat = getok(4,1);	/* Get response from modem */
	    debug(F101,"S7 mdmstat","",mdmstat);
	    break;			/* Errors here are not fatal */
	}
	/* modem-initialization failure */
	n1 = (mdmstat == 0) ? F_minit : F_modem;
	longjmp( sjbuf, n1 );

#ifndef MINIDIAL

/*
  Telebit modems fall into two basic groups: old and new.  The functions and
  command sets are different between the two groups, and also vary by specific
  models within each group, and even by firmware ROM revision number.  Read
  ckcker.bwr for details.

  Commands used by C-Kermit include:

    Old       New            Meaning
    -------   --------       ----------------------------------------
    Q0        Q0             Enable result codes.
    X1        X1             Extended result codes.
    X1        X1             Extended result codes + BUSY, NO DIALTONE, etc.
    I         I              Model number inquiry.
    I3        I3             Additional model information inquiry.
    S12=50    S12=50         Escape sequence guard time (1 sec).
    S2=43     S2=43          Escape character is '+'.
    S7=xx     S7=xx          DIAL TIMEOUT, calculated or SET by user.
    S48=0     S48=0          7-bit data (Kermit's PARITY is not NONE).
    S48=1     S48=1          8-bit data (Kermit's PARITY is NONE).
    S50=0     S50=0          Automatic speed & protocol determination.
    S50=3     S50=3          2400/1200/300 bps.
    S50=6     S50=6          V.32 (9600 bps).
    S50=255   S50=255        PEP mode.
    S110=1    S190=1 S191=7  Allow compression in PEP mode.
    S51=?     S51=?          DTE interface speed (left alone by Kermit).
    S54=3     S61=0 S63=0    Pass BREAK signal (always).
    S58=2     S58=2          RTS/CTS flow control if Kermit's FLOW is RTS/CTS.
    S58=?     S58=?          S58 unchanged if Kermit's FLOW is not RTS/CTS.
    S68=255   S68=255        Use flow control specified by S58 (always).
    S95=0     S180=0         MNP disabled (SET DIAL MNP-ENABLE OFF)
    S95=2     S180=3         MNP, fallback to direct (also as V.42 fallback)
    S97=1     S180=2         Enable V.42 (LAPM) error correction
    S98=3                    Enable compression in both directions
    S106=1                   V.42bis compression enable

For Kermit Spoof (same commands for all models that support it):

    S111=0                   No Kermit spoofing
    S111=10                  Kermit with no parity
    S111=11                  Kermit with odd parity
    S111=12                  Kermit with even parity
    S111=13                  Kermit with mark parity
    S111=14                  Kermit with space parity
    S112=??                  Kermit's start-of-packet character (stchr).
*/
    case n_TELEBIT:			/* Telebits... */
    case n_TBPEP:
    case n_TB3:
    case n_TB4:
    case n_TBS:
    case n_TBNV:
    case n_TBPNV:
    case n_TB3NV:
    case n_TB4NV:
    case n_TBSNV: {
	int S111;			/* Telebit Kermit spoof register */
	char tbcmdbuf[64];		/* Telebit modem command buffer */
	char *mnpstr = "";		/* Pointer to MNP-enable string */
	char *dprstr = "";		/* Pointer to dial protocol string */
/*
  If user defined a DIAL INIT-STRING, send that now, otherwise send built-in
  Telebit string.  Try up to 4 times to get OK or 0 response from modem.
  NOTE: The default init string *must* be independent of Telebit model.
*/
	ws = dialini ? dialini : TELEBIT.wake_str;
	debug(F110,"ckdial telebit init string",ws,0);
	for (tries = 4; tries > 0; tries--) {
	    ttsndb();			/* Begin by sending BREAK */
	    ttslow(ws,pmdminf->wake_rate); /* Send wakeup string */
	    mdmstat = getok(5,0);	/* Get modem's response */
	    if (mdmstat) break;		/* If response OK, done */
	    msleep(300);		/* Otherwise, sleep 1/3 second */
	    dialhup();			/* Hang up */
	    ttflui();			/* Flush input buffer and try again */
	}
	if (mdmstat < 1) {		/* If we didn't get a response */
	    n1 = F_minit;		/* fail, reason = can't initialize */
	    longjmp( sjbuf, F_minit );
	}
	if (!dialini) {			/* If using built-in init strings... */
/*
  Try to get the model number.  It should be in the getok() response buffer,
  rbuf[], because the Telebit init string asks for it with the "I" command.
  If the model number is 965, we have to make another query to narrow it down.
*/
	    if (didweget(rbuf,"962") ||	/* Check model number */
		didweget(rbuf,"961") ||
		didweget(rbuf,"963")) {
		tbmodel = TB_BLAZ;	/* Trailblazer */
	    } else if (didweget(rbuf,"972")) {
		tbmodel = TB_2500;	/* T2500 */
	    } else if (didweget(rbuf,"968")) {
		tbmodel = TB_1000;	/* T1000 */
	    } else if (didweget(rbuf,"966") ||
		       didweget(rbuf,"967") ||
		       didweget(rbuf,"964")) {
		tbmodel = TB_PLUS;	/* Trailblazer-Plus */
	    } else if (didweget(rbuf,"969")) {
		tbmodel = TB_QBLA;	/* Qblazer */
	    } else if (didweget(rbuf,"965")) { /* Most new models */
		tbati3(965);		/* Go find out */
	    } else if (didweget(rbuf,"971")) { /* T1500 or T2500 */
		tbati3(971);		/* Go find out */
	    } else if (didweget(rbuf,"123") || didweget(rbuf,"960")) {
		tbmodel = TB_UNK;	/* Telebit in Hayes mode */
	    }
	    debug(F111,"Telebit model",tb_name[tbmodel],tbmodel);
	    if (dialdpy)
	      printf("Telebit model: %s\n",tb_name[tbmodel]);
	    ttflui();
/*
  Dial timeout.  S7 is set to the DIAL TIMEOUT value, or else to 255 if the
  dial timeout is greater than 255, which is the maximum value for Telebits.
  S7 can't be set to 0 on Telebits -- it gives an error.
*/
	    if (mdmwait > 255)		/* If dial timeout too big */
	      mdmwait = 255;		/* make it as big as possible. */
/*
  Flow control.  If C-Kermit's FLOW-CONTROL is RTS/CTS, then we set this on
  the modem too.  Unfortunately, many versions of UNIX only allow RTS/CTS
  to be set outside of Kermit (e.g. by selecting a special device name).
  In that case, Kermit doesn't know that it should set RTS/CTS on the modem,
  in which case the user can add the appropriate Telebit command with
  SET DIAL DIAL-COMMAND.
*/
	    if (flow == FLO_RTSC) {	/* RTS/CTS active in Kermit */
		sprintf(tbcmdbuf,
			"ATS7=%d S48=%d S50=0 S58=2 S68=255\015",
			mdmwait, parity ? 0 : 1);
	    } else
	      sprintf(tbcmdbuf,		/* Otherwise, don't touch modem's */
		      "ATS7=%d S48=%d S50=0 S68=255\015", /* flow control */
		      mdmwait, parity ? 0 : 1);		  /* setting. */
	    s = tbcmdbuf;
	    debug(F110,"ckdial Telebit init step 2",s,0);
	    for (tries = 4; tries > 0; tries--) {
		ttslow(s,pmdminf->wake_rate);
		mdmstat = getok(5,1);
		if (mdmstat) break;
		msleep(500);
		ttflui();
	    }
	    if (mdmstat < 1) {
		n1 = F_minit;
		longjmp( sjbuf, F_minit ); /* Failed. */
	    }
/*
  Model-dependent items, but constant per model.
*/
	    switch(tbmodel) {
	      case TB_BLAZ:
	      case TB_PLUS: 		/* TrailBlazer-Plus */
	      case TB_1000:		/* T1000 */
	      case TB_2000:		/* T2000 */
	      case TB_2500:		/* T2500 */
#ifdef COMMENT
/* Code from edit 183 told modem to follow RS-232 wrt CD and DTR */
		/* DTR, CD, follow RS-232, pass BREAK */
		sprintf(tbcmdbuf,"ATS52=1 S53=4 S54=3\015");
#else
/* But everybody agreed we should not touch modem's CD and DTR settings. */
		/* Just pass BREAK */
		sprintf(tbcmdbuf,"ATS54=3\015");
#endif /* COMMENT */
		break;
	      case TB_1600:		/* T1600 */
	      case TB_3000:		/* T3000 */
	      case TB_WBLA:		/* WorldBlazer */
	      case TB_QBLA:		/* Qblazer */
#ifdef COMMENT
/* Code from edit 183 */
		/* Follow RS-232, No CONNECT suffix, pass BREAK */
		sprintf(tbcmdbuf,"AT&C1&D2&Q0 S59=0 S61=0 S63=0\015");
#else
/* Everybody agrees we should not touch modem's CD and DTR settings. */
/* Also no more &Q0, no more S59=0 (doesn't matter, so don't touch). */
/* So this section now deals only with treatment of BREAK. */
/* Here we also raise the result code from X1 to X2, which allows */
/* the T1600, T3000, and WB to supply NO DIALTONE, BUSY, RRING, and DIALING. */
/* X2 means something else on the other models. */
		/* Transmit BREAK in sequence, raise result code. */
		sprintf(tbcmdbuf,"ATX2 S61=0 S63=0\015");
#endif /* COMMENT */
		break;
	      default:			/* Others, do nothing */
		tbcmdbuf[0] = NUL;
		break;
	    }
	    s = tbcmdbuf;
	    if (*s) {
		debug(F110,"ckdial Telebit init step 3",s,0);
		for (tries = 4; tries > 0; tries--) {
		    ttslow(s,pmdminf->wake_rate);
		    mdmstat = getok(5,1);
		    if (mdmstat) break;
		    msleep(500);
		    ttflui();
		}
		if (mdmstat < 1) {
		    n1 = F_minit;
		    longjmp( sjbuf, F_minit ); /* Failed. */
		}
	    } else debug(F100,"ckdial Telebit init step 3 skipped","",0);

/* Error correction, MNP or V.42 */

	    if (augmdmtyp & DIAL_V42) { /* User wants V.42 */
		switch (tbmodel) {	/* which implies fallback to MNP. */
		  case TB_PLUS:		/* BC7.00 and up firmware */
		  case TB_2000:		/* now really the same as TB+ ? */
		  case TB_2500:		/* LAPM+compress->MNP->direct */
		    mnpstr = "S50=0 S95=2 S97=1 S98=3 S106=1";
		    break;
		  case TB_1600:
		  case TB_3000:
		  case TB_WBLA:
		  case TB_QBLA:
#ifdef COMMENT
		    mnpstr = "S180=2 S181=0"; /* V.42, fallback = lock speed */
#else
/* Better not to mess with S181, let it be used however user has it set. */
/* S180=2 allows fallback to MNP, S180=1 disallows fallback to MNP. */
		    mnpstr = "S180=2";	/* V.42 */
#endif /* COMMENT */
		    break;
		  default:
		    if (dialdpy)
		      printf("V.42 not supported by this Telebit model\n");
		}
	    } else {			/* Handle DIAL MNP-ENABLE setting */
		switch (tbmodel) {
		  case TB_BLAZ:		/* TrailBlazer */
		  case TB_PLUS:		/* TrailBlazer-Plus */
		  case TB_1000:		/* T1000 */
		  case TB_2000:		/* T2000 */
		  case TB_2500:		/* T2500 */
		    mnpstr = dialmnp ? "S95=2" : "S95=0"; /* ON, OFF */
		    break;
		  case TB_1600:		/* T1600 */
		  case TB_3000:		/* T3000 */
		  case TB_WBLA:		/* WorldBlazer */
		  case TB_QBLA:		/* Qblazer */
		    mnpstr = dialmnp ? "S180=3" : "S180=0"; /* ON, OFF */
		    /* (Leave S181 fallback method alone) */
		    break;
		  default:
		    mnpstr = "";
		}
	    }

/* Dialing protocol */

	    dprstr = "";	/* Initialize dialing protocol string */
	    p = "";		/* and message string */
	    switch (augmdmtyp) {
	      case n_TELEBIT:		/* Start at highest and work down */
              case n_TBNV:
       		p = "standard";
		switch (tbmodel) {	/* First group starts with PEP */
		  case TB_BLAZ:		/* TrailBlazer */
		  case TB_PLUS:		/* TrailBlazer-Plus */
		  case TB_1000:		/* T1000 */
		  case TB_2000:		/* T2000 */
		  case TB_2500:		/* T2500 */
		    dprstr = "S50=0 S110=1"; /* PEP, compression allowed. */
		    break;
		  case TB_WBLA:		/* WorldBlazer has PEP */
		    dprstr = "S50=0 S190=1 S191=7"; /* PEP, */
		    break;			    /* compression allowed. */
		  case TB_1600:		/* T1600 doesn't have PEP */
		  case TB_3000:		/* T3000 doesn't */
		  case TB_QBLA:		/* Qblazer doesn't*/
		  default:
		    dprstr = "S50=0";	/* No PEP available */
		    break;
		}
		break;

	      case n_TBS:		/* Telebit up to 2400 Baud */
	      case n_TBSNV:		/* i.e. "Slow mode". */
		p = "300/1200/2400 Baud"; /* Leave S90 alone assuming it is */
		dprstr = "S50=3";	  /* already set for V.22 vs 212A */
		break;
	      case n_TB3:		/* Telebit V.32 */
	      case n_TB3NV:
		if (tbmodel == TB_3000 || tbmodel == TB_1600 ||
		    tbmodel == TB_2500 || tbmodel == TB_WBLA) {
		    p = "V.32";
		    /* Note: we don't touch S51 (interface speed) here. */
		    /* We're already talking to the modem, and the modem */
		    /* SHOULD be able to make a V.32 call no matter what */
		    /* its interface speed is.  (In practice, however, */
		    /* that is not always true.) */
		    dprstr = "S50=6";
		} else if (dialdpy)
		  printf("V.32 not supported by this Telebit model.\n");
		break;
	      case n_TBPEP:		/* Force PEP Protocol */
              case n_TBPNV:
		if (tbmodel != TB_1600 && /* Models that don't support PEP */
		    tbmodel != TB_3000 &&
		    tbmodel != TB_QBLA) {
		    p = "PEP";
		    if (tbmodel == TB_WBLA) /* WorldBlazer */
		      dprstr = "S50=255 S190=1 S191=7";
		    else if (tbmodel != TB_1000)
		      dprstr = "S50=255 S110=1"; /* TrailBlazer, etc. */
		    else dprstr = "S50=255"; /* T1000, no compression */
		} else if (dialdpy)
		  printf("PEP not supported by this Telebit model.\n");
		break;
	    }

/* Kermit Spoof */

	    if (dialksp) {
		p = "Kermit Spoof";
		switch (parity) {	/* S111 value depends on parity */
		  case 'e': S111 = 12; break;
		  case 'm': S111 = 13; break;
		  case 'o': S111 = 11; break;
		  case 's': S111 = 14; break;
		  case 0:
		  default:  S111 = 10; break;

		}
#ifdef COMMENT
/*
  This code forced the use of PEP mode if DIAL KERMIT-SPOOF was ON, which
  prevented successful connection if the other modem did not support PEP.
*/
		if (tbmodel == TB_BLAZ || /* Must force PEP on old models */
		    tbmodel == TB_PLUS ||
		    tbmodel == TB_1000 ||
		    tbmodel == TB_2000 ||
		    tbmodel == TB_2500)
		  sprintf(tbcmdbuf,"AT%s %s S50=255 S111=%d S112=%d\015",
			  mnpstr,dprstr,S111,stchr);
		/* PEP is not required on T1500, T1600, T3000, and WB */
		/* (but MNP is, but that is user's responsibility).   */
		else
#endif /* COMMENT */
		  if (tbmodel != TB_QBLA)
		  sprintf(tbcmdbuf,"AT%s %s S111=%d S112=%d\015",
			  mnpstr,dprstr,S111,stchr);
		else {			/* Qblazer has no Kermit spoof */
		    sprintf(tbcmdbuf,"AT%s %s\015", mnpstr,dprstr);
		    p = "No Kermit Spoof";
		    if (dialdpy)
		      printf("Kermit Spoof not supported by Qblazer\n");
		}
	    } else {			/* No Kermit spoof */
		p = "No Kermit Spoof";
		sprintf(tbcmdbuf,"AT%s %s %s\015",
			mnpstr, dprstr,
			(tbmodel == TB_QBLA) ? "" : "S111=0 S112=0");
	    }
	    s = tbcmdbuf;
	    debug(F111,"ckdial Telebit config",p,speed);
	    debug(F110,"ckdial Telebit init step 4",s,0);
	    if (*s) {
		for (tries = 4; tries > 0; tries--) {
		    ttslow(s,pmdminf->wake_rate);
		    mdmstat = getok(5,1);
		    if (mdmstat) break;
		    msleep(500);
		    ttflui();
		}
		debug(F101,"ckdial telebit init mdmstat","",mdmstat);
		if (mdmstat < 1) {
		    n1 = F_minit;
		    longjmp(sjbuf, F_minit);
		}
	    }
	}
	/* Done with Telebit protocols, remove bits from modem type */
	/* Except nonverbal bit */
	augmdmtyp &= ~(DIAL_PEP|DIAL_V32|DIAL_V42|DIAL_SLO);
	debug(F101,"ckdial Telebit augmdmtyp","",augmdmtyp);
	break;
    }

    case n_MICROCOM:			/* Interdigit waits for tone dial */
        {
	    jmp_buf savejmp;
	    alarm(0);
	    savalrm = signal(SIGALRM,dialtime); /* Set alarm handler. */
	    xcpy((char *)savejmp, (char *)sjbuf, sizeof savejmp);
	    if (setjmp(sjbuf)) {
	    	/* try the autobaud sequence */
		xcpy((char *)sjbuf, (char *)savejmp, sizeof savejmp);
		alarm(5);
	        ttslow("44445", MICROCOM.wake_rate);
		waitfor(MICROCOM.wake_str);
	    } else {
		alarm(2);
		ws = dialini ? dialini : MICROCOM.wake_str;
		ttslow(ws, MICROCOM.wake_rate);
	    	waitfor(ws);
#ifdef COMMENT
/*
  Kermit spoof for Microcom modems.  Untested.
  This is reportedly the way to do it for QX/3296c, QX/4232hs, QX/4232bis,
  and QX/9624c.  The effect on other models is unknown.
*/
		ws = dialksp ? "APM1\015" : "APM0\015";	/* Kermit spoof */
		ttslow(ws, MICROCOM.wake_rate);
	    	waitfor(ws);
#endif /* COMMENT */
		alarm(0);
		signal(SIGALRM,savalrm); /* Set alarm handler. */
		xcpy((char *)sjbuf, (char *)savejmp, sizeof savejmp);
	    }
	}
	break;

    case n_ATTDTDM:		/* DTDM requires BREAK to wake up */
	ttsndb();		/* Send BREAK */
	break;			/* ttsndb() defined in ckutio.c */

#endif /* MINIDIAL */

    default:			/* Place modem into command mode */
	ws = dialini ? dialini : pmdminf->wake_str;
	if (ws && (int)strlen(ws) > 0) {
	    debug(F111,"ckdial default, wake string", ws, pmdminf->wake_rate);
	    ttslow(ws, pmdminf->wake_rate);
	} else debug(F100,"ckdial no wake_str","",0);
	if (pmdminf->wake_prompt && (int)strlen(pmdminf->wake_prompt) > 0) {
	    debug(F110,"ckdial default, waiting for wake_prompt",
		  pmdminf->wake_prompt,0);
	    savalrm = signal(SIGALRM,dialtime);
	    alarm(10);
	    waitfor(pmdminf->wake_prompt);
	} else debug(F100,"ckdial no wake_prompt","",0);
	break;
    }
    alarm(0);				/* Turn off alarm */
    signal(SIGALRM,savalrm);		/* Restore alarm handler. */
    debug(F100,"ckdial got wake prompt","",0);
    msleep(500);			/* Allow settling time */

#ifndef MINIDIAL

/* Enable/disable MNP (Telebit already done above) */

    switch (augmdmtyp) {
      case n_HST:
	if (dialmnp)
	  ttslow("AT&K2\015",pmdminf->wake_rate);
	else
	  ttslow("AT&K0\015",pmdminf->wake_rate);
	getok(5,1);			/* Get response */
	break;

     /* Add others here ... */

      default:
	break;
    }
#endif /* MINIDIAL */

/* Put modem into dialing mode, if the modem requires it. */

    if (pmdminf->dmode_str && *(pmdminf->dmode_str)) {
	ttslow(pmdminf->dmode_str, pmdminf->dial_rate);
	savalrm = signal(SIGALRM,dialtime);
	alarm(10);
	if (pmdminf->dmode_prompt) { /* Wait for prompt, if any expected */
	    waitfor(pmdminf->dmode_prompt);
	    msleep(300);
	}
	alarm(0);		 /* Turn off alarm on dialing prompts */
	signal(SIGALRM,savalrm); /* Restore alarm */
	ttflui();		 /* Clear out stuff from waking modem up */
    }

/* Dial the number.  First form the dialing string. */

    sprintf(lbuf, dialcmd ? dialcmd : pmdminf->dial_str, telnbr);
    debug(F110,"dialing",lbuf,0);
    ttslow(lbuf,pmdminf->dial_rate);	/* Send the dialing string */

    savalrm = signal(SIGALRM,dialtime);	/* Time to allow for connecting */
    x = alarm(waitct);			/* This much time... */
    debug(F101,"ckdial old alarm","",x);
    debug(F101,"ckdial waitct","",waitct);

#ifndef MINIDIAL
    switch (augmdmtyp) {
    case n_RACAL: 	   /* Acknowledge printout of dialing string */
      sleep(3);
      ttflui();
      ttoc('\015');
      break;
    case n_VENTEL:
      waitfor("\012\012"); /* Ignore the first two strings */
      break;
    default:
      break;
    }
#endif /* MINIDIAL */

/* Check for connection */

    mdmstat = 0;			/* No status yet */
    strcpy(lbuf,"No Connection");	/* Default reason for failure */
    debug(F101,"dial awaiting response, augmdmtyp","",augmdmtyp);
    while (mdmstat == 0) {
	switch (augmdmtyp) {
	  default:
	    for (n = -1; n < LBUFL-1; ) { /* Accumulate response */
		c2 = ddinc(0);		/* Read a character, blocking */
		if (c2 == 0 || c2 == -1) /* Ignore NULs and errors */
		  continue;		/* (Timeout will handle errors) */
		else			/* Real character, keep it */
		  lbuf[++n] = c2 & 0177;
		dialoc(lbuf[n]);	/* Maybe echo it  */
                if (augmdmtyp == n_CCITT) { /* V.25 bis dialing... */
/*
  This assumes that V.25bis indications are all at least 3 characters long
  and are terminated by either CRLF or LFCR.
*/
		    if (n < 3) continue;
		    if ((lbuf[n] == CR) && (lbuf[n-1] == LF)) break;
		    if ((lbuf[n] == LF) && (lbuf[n-1] == CR)) break;
		}
#ifndef MINIDIAL
		  else if (augmdmtyp == n_DIGITEL) {
		    if ((lbuf[n] == CR) && (lbuf[n-1] == LF) ||
			(lbuf[n] == LF) && (lbuf[n-1] == CR))
		      break;
		    else
		      continue;
		}
#endif /* MINIDIAL */
		  else {		/* All others, break on CR or LF */
		    if ( lbuf[n] == CR || lbuf[n] == LF ) break;
		}
	    }
	    lbuf[++n] = '\0';		/* Terminate response from modem */
	    debug(F111,"dial modem response",lbuf,n);
	    if (n) {			/* If one or more characters present */
		switch (augmdmtyp) {	/* check for modem response message. */
#ifndef MINIDIAL
		  case n_ATTMODEM:
		    /* Careful - "Connected" / "Not Connected" */
		    if (didweget(lbuf,"Busy") ||
			didweget(lbuf,"Not connected") ||
			didweget(lbuf,"Not Connected") ||
			didweget(lbuf,"No dial tone") ||
			didweget(lbuf,"No Dial Tone") ||
			didweget(lbuf,"No answer") ||
			didweget(lbuf,"No Answer"))
			mdmstat = FAILED;
		    else if (didweget(lbuf,"Answered") ||
			didweget(lbuf,"Connected"))
			mdmstat = CONNECTED;
		    break;

		  case n_ATTISN:
		    if (didweget(lbuf,"ANSWERED"))
			mdmstat = CONNECTED;
		    else if (didweget(lbuf,"BUSY") ||
			didweget(lbuf,"DISCONNECT") ||
			didweget(lbuf,"NO ANSWER") ||
			didweget(lbuf,"WRONG ADDRESS"))
			mdmstat = FAILED;
		    break;

		  case n_ATTDTDM:
		    if (didweget(lbuf,"ANSWERED"))
			mdmstat = CONNECTED;
		    else if (didweget(lbuf,"BUSY") ||
			didweget(lbuf,"CHECK OPTIONS") ||
			didweget(lbuf,"DISCONNECTED") ||
			didweget(lbuf,"DENIED"))
			mdmstat = FAILED;
#ifdef DEBUG
#ifdef ATT6300
		    else if (deblog && didweget(lbuf,"~~"))
			mdmstat = CONNECTED;
#endif /* ATT6300 */
#endif /* DEBUG */
		    break;
#endif /* MINIDIAL */

		  case n_CCITT:			/* CCITT V.25bis */
#ifndef MINIDIAL
		  case n_DIGITEL:		/* or Digitel variant */
#endif /* MINIDIAL */
		    if (didweget(lbuf,"VAL")) {	/* Dial command confirmation */
#ifndef MINIDIAL
			if (augmdmtyp == n_CCITT)
#endif /* MINIDIAL */
			  continue;		/* Go back and read more */
#ifndef MINIDIAL
/* Digitel doesn't give an explicit connect confirmation message */
			else {
			    int n;
			    for (n = -1; n < LBUFL-1; ) {
				lbuf[++n] = c2 = ddinc(0) & 0177;
				dialoc(lbuf[n]);
				if ((lbuf[n] == CR) && (lbuf[n-1] == LF) ||
				    (lbuf[n] == LF) && (lbuf[n-1] == CR))
				  break;
			    }
			    mdmstat = CONNECTED; /* Assume we're connected */
			    if (dialdpy && carrier != CAR_OFF) {
				sleep(1); 	/* Wait a second */
				n = ttgmdm();	/* Try to read modem signals */
				if ((n > -1) && (n & BM_DCD == 0))
				  printf("Warning: No Carrier\n");
			    }
		      }
#endif /* MINIDIAL */
		    } else if (didweget(lbuf,"CNX")) { /* Connected */
			mdmstat = CONNECTED;
                    } else if (didweget(lbuf, "INV")) {
			mdmstat = FAILED;	/* Command error */
			strcpy(lbuf,"INV");
		    } else if (didweget(lbuf,"CFI")) { /* Call Failure */
#ifdef COMMENT
/*
  V.25 bis says that the failure reason comes on the same line, so
  we don't need to read any more characters here.
*/
			for (n = 0; n < LBUFL-1; n++) { /* Read reason */
			    lbuf[n] = c2 = (ddinc(0) & 0177);
                            if (c2 == LF) /* Modem answers LF CR */
			      continue;
			    dialoc(lbuf[n]);
			    if (lbuf[n] == CR || lbuf[n] == LF) break;
			}
#endif /* COMMENT */
			if (didweget(lbuf,"AB")) /* Interpret reason code */
			  strcpy(lbuf,"AB: Timed out");
			else if (didweget(lbuf,"CB"))
			  strcpy(lbuf,"CB: Local DCE Busy");
			else if (didweget(lbuf,"ET"))
			  strcpy(lbuf,"ET: Busy");
			else if (didweget(lbuf, "NS"))
			  strcpy(lbuf,"NS: Number not stored");
			else if (didweget(lbuf,"NT"))
			  strcpy(lbuf,"NT: No answer");
			else if (didweget(lbuf,"RT"))
			  strcpy(lbuf,"RT: Ring tone");
			else if (didweget(lbuf,"PV"))
			  strcpy(lbuf,"PV: Parameter value error");
			else if (didweget(lbuf,"PS"))
			  strcpy(lbuf,"PS: Parameter syntax error");
			else if (didweget(lbuf,"MS"))
			  strcpy(lbuf,"MS: Message syntax error");
			else if (didweget(lbuf,"CU"))
			  strcpy(lbuf,"CU: Command unknown");
			else if (didweget(lbuf,"FC"))
			  strcpy(lbuf,"FC: Forbidden call");
			mdmstat = FAILED;
		    } else if (didweget(lbuf,"INC")) { /* Incoming Call */
			strcpy(lbuf,"INC: Incoming call");
			mdmstat = FAILED;
		    } else if (didweget(lbuf,"DLC")) { /* Delayed Call */
			strcpy(lbuf,"DLC: Delayed call");
			mdmstat = FAILED;
		    } else		/* Response was probably an echo. */
#ifndef MINIDIAL
		      if (augmdmtyp == n_CCITT)
#endif /* MINIDIAL */
			continue;
#ifndef MINIDIAL
		      else	/* Digitel: If no error, then connect. */
			mdmstat = CONNECTED;
#endif /* MINIDIAL */
		    break;

#ifndef MINIDIAL
		  case n_CERMETEK:
		    if (didweget(lbuf,"\016A")) {
			mdmstat = CONNECTED;
			ttslow("\016U 1\015",200); /* make transparent*/
		    }
		    break;

		  case n_DF100:	     /* DF100 has short response codes */
		    if (strcmp(lbuf,"A") == 0)
			mdmstat = CONNECTED; /* Attached */
		    else if (strcmp(lbuf,"N") == 0 || /* No Ans or Dialtone */
			     strcmp(lbuf,"E") == 0 || /* Error */
			     strcmp(lbuf,"R") == 0) { /* Ready */
			mdmstat = FAILED;
			break;
		    }
		    /* otherwise fall thru... */

		  case n_DF200:
		    if (didweget(lbuf,"Attached"))
			mdmstat = CONNECTED;
		    /*
		     * The DF100 will respond with "Attached" even if DTR
		     * and/or carrier are not present.	Another reason to
		     * (also) wait for carrier?
		     */
		    else if (didweget(lbuf,"Busy") ||
			didweget(lbuf,"Disconnected") ||
			didweget(lbuf,"Error") ||
			didweget(lbuf,"No answer") ||
			didweget(lbuf,"No dial tone") ||
			didweget(lbuf,"Speed:"))
			mdmstat = FAILED;
		    /*
		     * It appears that the "Speed:..." response comes after an
		     * "Attached" response, so this is never seen.  HOWEVER,
		     * it would be very handy to detect this and temporarily
		     * reset the speed, since it's a nuisance otherwise.
		     * If we wait for some more input from the modem, how do
		     * we know if it's from the remote host or the modem?
		     * Carrier reportedly doesn't get set until after the
		     * "Speed:..." response (if any) is sent.  Another reason
		     * to (also) wait for carrier.
		     */
		    break;

		  case n_GDC:
		    if (didweget(lbuf,"ON LINE"))
			mdmstat = CONNECTED;
		    else if (didweget(lbuf,"NO CONNECT"))
			mdmstat = FAILED;
		    break;

		  case n_USROBOT:
		  case n_HST:
		  case n_TELEBIT:
#endif /* MINIDIAL */
		  case n_HAYES:
		    if (mdmspd && !network) {
			s = lbuf;
			while (*s != '\0' && *s != 'C') s++;
			cptr = (*s == 'C') ? s : NULL;
			conspd = 0L;
			if ((cptr != NULL) && !strncmp(cptr,"CONNECT ",8)) {
			    if ((int)strlen(cptr) < 9) /* Just CONNECT, */
			      conspd = 300L;      /* use 300 bps */
			    else if (isdigit(*(cptr+8))) /* not CONNECT FAST */
			      conspd = atol(cptr + 8); /* CONNECT nnnn */
			    if (conspd != speed) {
				if ((conspd / 10L) > 0) {
				    if (ttsspd((int) (conspd / 10L)) < 0) {
				       printf(" Can't change speed to %lu\r\n",
					       conspd);
				    } else {
					speed = conspd;
					mdmstat = CONNECTED;
					if ( !quiet && !backgrd )
					  printf(" Speed changed to %lu\r\n",
						 conspd);
				    }
				}
			    } /* Expanded to handle any conceivable speed */
			}
		    }
#ifndef MINIDIAL
		    if (mymdmtyp == n_TELEBIT) {
			if (didweget(lbuf,"CONNECT FAST/KERM")) {
			    mdmstat = CONNECTED;
			    if (!quiet && !backgrd) printf("FAST/KERM ");
			    break;
			}
 		    }
#endif /* MINIDIAL */
  		    if (didweget(lbuf,"RRING") ||
			didweget(lbuf,"RINGING") ||
			didweget(lbuf,"DIALING")) {
			mdmstat = 0;
		    } else if (didweget(lbuf,"CONNECT")) {
			mdmstat = CONNECTED;
		    } else if (didweget(lbuf,"NO CARRIER") ||
			didweget(lbuf,"NO DIALTONE") ||
			didweget(lbuf,"NO DIAL TONE") ||
			didweget(lbuf,"BUSY") ||
			didweget(lbuf,"NO ANSWER") ||
			didweget(lbuf,"VOICE") ||
			didweget(lbuf,"RING") ||
			didweget(lbuf,"ERROR")) {
			mdmstat = FAILED;
		    }
		    break;
#ifndef MINIDIAL
		  case n_PENRIL:
		    if (didweget(lbuf,"OK"))
			mdmstat = CONNECTED;
		    else if (didweget(lbuf,"BUSY") ||
			didweget(lbuf,"NO RING"))
			mdmstat = FAILED;
		    break;
		  case n_RACAL:
		    if (didweget(lbuf,"ON LINE"))
			mdmstat = CONNECTED;
		    else if (didweget(lbuf,"FAILED CALL"))
			mdmstat = FAILED;
		    break;
		  case n_ROLM:
		    if (didweget(lbuf,"CALLING"))
			mdmstat = 0;
		    else if (didweget(lbuf,"COMPLETE"))
			mdmstat = CONNECTED;
		    else if (didweget(lbuf,"FAILED") ||
			didweget(lbuf,"NOT AVAILABLE") ||
			didweget(lbuf,"LACKS PERMISSION") ||
			didweget(lbuf,"NOT A DATALINE") ||
			didweget(lbuf,"INVALID GROUP NAME") ||
			didweget(lbuf,"BUSY") ||
			didweget(lbuf,"ABANDONDED") ||
			didweget(lbuf,"DOES NOT ANSWER") ||
			didweget(lbuf,"INVALID DATA LINE NUMBER"))
			mdmstat = FAILED;
		    break;
		  case n_VENTEL:
		    if (didweget(lbuf,"ONLINE!") ||
			didweget(lbuf,"Online!"))
			mdmstat = CONNECTED;
		    else if (didweget(lbuf,"BUSY") ||
			didweget(lbuf,"DEAD PHONE") ||
			didweget(lbuf,"Busy"))
			mdmstat = FAILED;
		    break;
		  case n_CONCORD:
		    if (didweget(lbuf,"INITIATING"))
			mdmstat = CONNECTED;
		    else if (didweget(lbuf,"BUSY") ||
			didweget(lbuf,"CALL FAILED"))
			mdmstat = FAILED;
		    break;
		  case n_MICROCOM:
		    /* "RINGBACK" means phone line ringing, continue */
		    if (didweget(lbuf,"NO CONNECT") ||
			didweget(lbuf,"BUSY") ||
			didweget(lbuf,"NO DIALTONE") ||
			didweget(lbuf,"COMMAND ERROR") ||
			didweget(lbuf,"IN USE"))
			mdmstat = FAILED;
		    else if (didweget(lbuf,"CONNECT"))
			mdmstat = CONNECTED;
			/* trailing speed ignored */
		    break;
#endif /* MINIDIAL */
		}
	    }
	    break;

#ifndef MINIDIAL
	case n_DF03:			/* because response lacks CR or NL */
	    c = ddinc(0) & 0177;
	    dialoc(c);
	    debug(F000,"dial df03 got","",c);
	    if ( c == 'A' ) mdmstat = CONNECTED;
	    if ( c == 'B' ) mdmstat = FAILED;
	    break;

	case n_TBNV:			/* Hayeslike modems in digit */
	case n_TB3NV			/* response mode... */:
	case n_TBPNV:
	case n_TB4NV:
	case n_TBSNV:
#endif /* MINIDIAL */
	case n_HAYESNV:

/*
  The method for reading Hayes numeric result codes has been totally
  redone as of 5A(174) to account for all of the following.  Not all have
  been tested, and others probably need to be added.

  Hayes numeric result codes (Hayes 1200 and higher):
     0 = OK
     1 = CONNECT at 300 bps (or 1200 bps on Hayes 1200 with basic code set)
     2 = RING
     3 = NO CARRIER
     4 = ERROR (in command line)
     5 = CONNECT 1200 (extended code set)
  Hayes 2400 and higher:
     6 = NO DIALTONE
     7 = BUSY
     8 = NO ANSWER
     9 = (there is no 9)
    10 = CONNECT 2400
  Reportedly, the codes for Hayes V.32 modems are:
    1x = CONNECT <suffix>
    5x = CONNECT 1200 <suffix>
    9x = CONNECT 2400 <suffix>
   11x = CONNECT 4800 <suffix>
   12x = CONNECT 9600 <suffix>
  Where:
    x:   suffix:
    R  = RELIABLE
    RC = RELIABLE COMPRESSED
    L  = LAPM
    LC = LAPM COMPRESSED
  And for Telebits, all the above, except no suffix in numeric mode, plus:
    11 = CONNECT 4800
    12 = CONNECT 9600
    13 = CONNECT 14400
    14 = CONNECT 19200
    15 = CONNECT 38400
    16 = CONNECT 57600
    20 = CONNECT 300/REL  (= MNP)
    22 = CONNECT 1200/REL (= MNP)
    23 = CONNECT 2400/REL (= MNP)
    46 = CONNECT 7512  (i.e. 75/1200)
    47 = CONNECT 1275  (i.e. 1200/75)
    48 = CONNECT 7200
    49 = CONNECT 12000
    50 = CONNECT FAST (not on T1600/3000)
    52 = RRING
    53 = DIALING
    54 = NO PROMPTTONE
    61 = CONNECT FAST/KERM (Kermit spoof)
    70 = CONNECT FAST/COMP (PEP + compression)
    71 = CONNECT FAST/KERM/COMP (PEP + compression + Kermit spoof)
*/
#define NBUFL 8
	    {				/* Nonverbal response code handler */
	    char nbuf[NBUFL+1];		/* Response buffer */
	    int i, j;			/* Buffer pointers */
	    debug(F101,"RESPONSE mdmecho","",mdmecho);
	    if (mdmecho) {		/* Sponge up dialing string echo. */
		while (1) {
		    c = ddinc(0) & 0x7f;
		    debug(F000,"SPONGE","",c);
		    dialoc(c);
		    if (c == CR) break;
		}
	    }
	    while (mdmstat == 0) {	/* Read response */
		for (i = 0; i < NBUFL; i++) /* Clear the buffer */
		  nbuf[i] = '\0';
		i = 0;			/* Reset the buffer pointer. */
		c = ddinc(0) & 0177;	/* Get first digit of response. */
					/* using an untimed, blocking read. */
		debug(F000,"RESPONSE-A","",c);
		dialoc(c);		/* Echo it if requested. */
		if (!isdigit(c))	/* If not a digit, keep looking. */
		  continue;
		nbuf[i++] = c;		/* Got first digit, save it. */
		while (c != CR && i < 8) { /* Now read characters up to CR */
		    x = ddinc(0) & 0177; /* Get a character. */
		    c = (char) x;	/* Got it OK. */
		    debug(F000,"RESPONSE-C","",c);
		    if (c != CR)	/* If it's not a carriage return, */
		      nbuf[i++] = c;	/*  save it. */
		    dialoc(c);		/* Echo it. */
		}
		nbuf[i] = '\0';		/* Done, terminate the buffer. */
		debug(F111,"dial hayesnv lbuf",lbuf,n);
		debug(F111,"dial hayesnv got",nbuf,i);
		/*
		  Separate any non-numeric suffix from the numeric result code
		  with a null.
		*/
		for (j = i-1; (j > -1) && !isdigit(nbuf[j]); j--)
		  nbuf[j+1] = nbuf[j];
		j++;
		nbuf[j++] = '\0';
		debug(F110,"dial hayesnv numeric",nbuf,0);
		debug(F111,"dial hayesnv suffix ",nbuf+j,j);
		if ((int)strlen(nbuf) > 3) /* Probably phone number echoing. */
		  continue;
		/*
		  Now read and interpret the results...
		*/
		i = atoi(nbuf);		/* Convert to integer */
		switch (i) {
		  case 1:		/* CONNECT */
		    mdmstat = CONNECTED; /* Could be any speed */
		    break;
		  case 2:		/* RING */
		    if (dialdpy) printf("\r\n Local phone is ringing!\r\n");
		    mdmstat = FAILED;
		    break;
		  case 3:		/* NO CARRIER */
		    if (dialdpy) printf("\r\n No Carrier.\r\n");
		    mdmstat = FAILED;
		    break;
		  case 4:		/* ERROR */
		    if (dialdpy) printf("\r\n Modem Command Error.\r\n");
		    mdmstat = FAILED;
		    break;
		  case 5:		/* CONNECT 1200 */
		    spdchg(1200L);	/* Change speed if necessary. */
		    mdmstat = CONNECTED;
		    break;
		  case 6:		/* NO DIALTONE */
		    if (dialdpy) printf("\r\n No Dialtone.\r\n");
		    mdmstat = FAILED;
		    break;
		  case 7:		/* BUSY */
		    if (dialdpy) printf("\r\n Busy.\r\n");
		    mdmstat = FAILED;
		    break;
		  case 8:		/* NO ANSWER */
		    if (dialdpy) printf("\r\n No Answer.\r\n");
		    mdmstat = FAILED;
		    break;
		  case 9:		/* CONNECT 2400 */
		  case 10:
		    spdchg(2400L);	/* Change speed if necessary. */
		    mdmstat = CONNECTED;
		    break;
		  case 11:		/* CONNECT 4800 */
		    spdchg(4800L);
		    mdmstat = CONNECTED;
		    break;
		  case 12:		/* CONNECT 9600 */
		    spdchg(9600L);
		    mdmstat = CONNECTED;
		    break;
		  case 13:		/* CONNECT 14400 */
		    spdchg(14400L);
		    mdmstat = CONNECTED;
		    break;
		  case 14:
		    spdchg(19200L);	/* CONNECT 19200 */
		    mdmstat = CONNECTED;
		    break;
		  case 15:		/* CONNECT 34800 */
		    spdchg(38400L);
		    mdmstat = CONNECTED;
		    break;
		  case 16:		/* CONNECT 57600 */
		    spdchg(57600L);
		    mdmstat = CONNECTED;
		    break;
		  case 20:		/* CONNECT 300/REL */
		    spdchg(300L);
		    mdmstat = CONNECTED;
		    break;
		  case 22:		/* CONNECT 1200/REL */
		    spdchg(1200L);
		    mdmstat = CONNECTED;
		    break;
		  case 23:		/* CONNECT 2400/REL */
		    spdchg(2400L);
		    mdmstat = CONNECTED;
		    break;
		  case 46:		/* CONNECT 7512 */
		    spdchg(8880L);	/* 75/1200 split speed */
		    mdmstat = CONNECTED; /* (special C-Kermit code) */
		    break;
		  case 47:		/* CONNECT 1200/75 */
		    mdmstat = CONNECTED; /* Speed not supported by Kermit */
		    printf("CONNECT 1200/75 - Not support by C-Kermit\r\n");
		    break;
		  case 48:		/* CONNECT 7200 */
		    spdchg(7200L);
		    mdmstat = CONNECTED;
		    break;
		  case 49:		/* CONNECT 12000 */
		    spdchg(12000L);
		    mdmstat = CONNECTED;
		    break;
#ifndef MINIDIAL
		  case 50:		/* CONNECT FAST */
		    if (mymdmtyp == n_TELEBIT) /* Early models only */
		      mdmstat = CONNECTED;
		    break;
		  case 52:		/* RRING */
		    if (mymdmtyp == n_TELEBIT)
		      if (dialdpy) printf(" Ringing...\r\n");
		    break;
		  case 53:		/* DIALING */
		    if (mymdmtyp == n_TELEBIT)
		      if (dialdpy) printf(" Dialing...\r\n");
		    break;
		  case 54:		/* NO PROMPTTONE */
		    if (mymdmtyp == n_TELEBIT) {
			if (dialdpy) printf("\r\n No Prompttone.\r\n");
			mdmstat = FAILED;
		    }
		    break;
		  case 61:		/* Various Telebit PEP modes */
		  case 62:
		  case 63:
		  case 70:
		  case 71:
		  case 72:
		  case 73:
		    if (mymdmtyp == n_TELEBIT) /* Early models only */
		      mdmstat = CONNECTED;
		    break;
#endif /* MINIDIAL */
		  default:
		    break;
		}
	    }
	    if (mdmstat == CONNECTED && nbuf[j] != '\0') {
		if (dialdpy) {
		    printf("\r\n");
		    if (nbuf[j] == 'R') printf("RELIABLE");
		    if (nbuf[j] == 'L') printf("LAPM");
		    if (nbuf[j+1] == 'C') printf(" COMPRESSED");
		    printf("\r\n");
		}
	    }
	}
        break;

	case n_UNKNOWN: {
	        int x, y = waitct;
		mdmstat = FAILED;	/* Assume failure. */
		while (y-- > -1) {
		    x = ttchk();
		    if (x > 0) {
			if (x > LBUFL) x = LBUFL;
			x = ttxin(x,(CHAR *)lbuf);
			if ((x > 0) && dialdpy) conol(lbuf);
		    }
		    x = ttgmdm();	/* Try to read modem signals */
		    if (x < 0) break;	/* Can't, fail. */
		    if (x & BM_DCD) {	/* Got signals OK.  Carrier present? */
			mdmstat = CONNECTED; /* Yes, done. */
			break;
		    }			/* No, keep waiting. */
		    sleep(1);
		}
	        break;
  	    }
	}				/* switch (augmdmtyp) */
    }					/* while (mdmstat == 0) */
    x = alarm(0);			/* Turn off alarm. */
    debug(F101,"ckdial alarm off","",x);
    if ( mdmstat != CONNECTED ) {	/* Modem-detected failure */
	n1 = F_modem;
	longjmp( sjbuf, F_modem );	/* Exit (with reason in lbuf) */
    }
    msleep(1000);			/* In case DTR blinks  */
    alarm(3);				/* Precaution in case of trouble */
    debug(F110,"dial","succeeded",0);
#ifndef MINIDIAL
    if (augmdmtyp != n_ROLM)		/* Rolm has wierd modem signaling */
#endif /* MINIDIAL */
      ttpkt(speed,FLO_DIAX,parity);	/* Cancel dialing state ioctl */
    dreset();				/* Reset alarms, etc. */
    if (!quiet && !backgrd)
      printf (" Call complete.\07\n");
#ifdef DYNAMIC
    if (lbuf) free(lbuf);
#endif /* DYNAMIC */
    return(1);				/* Return successfully */
}

/*
  getok() - wait up to n seconds for OK (0) or ERROR (4) response from modem.
  Use with Hayeslike or CCITT modems for reading the reply to a nondialing
  command.

  Second argument says whether to be strict about numeric result codes, i.e.
  to require they be preceded by CR or else be the first character in the
  response, e.g. to prevent the ATH0<CR> echo from looking like a valid
  response.  Strict == 0 is needed for ATI on Telebit, which can return the
  model number concatenated with the numeric response code, e.g. "9620"
  ("962" is the model number, "0" is the response code).  getok() Returns:

   0 if it timed out,
   1 if it succeeded,
  -1 on modem command, i/o, or other error.
*/
static jmp_buf okbuf;			/* Jump-buf for getok(). */

SIGTYP
oktimo(foo) int foo; {			/* Alarm handler for getok(). */
#ifdef OSK				/* OS-9, see comment in dialtime(). */
    sigmask(-1);
#endif /* OSK */
    longjmp(okbuf,1);
}

int
getok(n, strict) int n, strict; {
    CHAR c;
    int i, x, status, oldalarm;
    SIGTYP (*saval)();			/* For saving alarm handler locally */

    mdmecho = 0;			/* Assume no echoing of commands */
    saval = signal(SIGALRM,oktimo);	/* Set response timer, */
    oldalarm = alarm(n);		/* saving old one. */
    if (setjmp(okbuf)) {		/* Timed out. */
	alarm(oldalarm);		/* Restore old alarm */
 	if (saval) signal(SIGALRM,saval); /* and alarm handler */
	debug(F100,"getok timeout","",0);
	ttflui();			/* Flush input buffer */
	return(0);			/* and return timeout indication */

    } else if (augmdmtyp == n_CCITT	/* CCITT, easy... */
#ifndef MINIDIAL
	       || augmdmtyp == n_DIGITEL /* Digitel, ditto. */
#endif /* MINIDIAL */
	       ) {
	waitfor("VAL");
        return(1);

    } else {				/* Hayes & friends, start here... */
	status = 0;			/* No status yet. */
	for (x = 0; x < RBUFL; x++)	/* Initialize response buffer */
	  rbuf[x] = SP;			/*  to all spaces */
	rbuf[RBUFL] = NUL;		/* and terminate with NUL. */
	while (status == 0) {		/* While no status... */
	    x = ddinc(0);		/* Read a character */
	    if (x < 0) {		/* I/O error */
		alarm(oldalarm);	/* Turn off alarm */
		if (saval) signal(SIGALRM,saval); /* and restore handler. */
		return(-1);		/* Return error code. */
	    }
	    debug(F101,"getok ddinc","",x); /* Got a character. */
	    c = x & 0x7f;		/* Get low order 7 bits */
	    if (!c)			/* Don't deposit NULs */
	      continue;			/* or else didweget() won't work */
	    if (dialdpy) conoc((char)c); /* Echo it if requested */
	    for (i = 0; i < RBUFL-1; i++) /* Rotate buffer */
	      rbuf[i] = rbuf[i+1];
	    rbuf[RBUFL-1] = c;		/* Deposit character at end */
	    debug(F000,"getok:",rbuf,(int) c); /* Log it */
	    switch (c) {		/* Interpret it. */
	      case CR:			/* Got a carriage return. */
		switch(rbuf[RBUFL-2]) {	/* Look at character before it. */
		  case '0':		/* 0 = OK numeric response */
		    if (!strict ||
			rbuf[RBUFL-3] == CR || rbuf[RBUFL-3] == SP) {
			augmdmtyp |= DIAL_NV; /* OR in the "nonverbal" bit. */
			status = 1;	/* Good response */
		    }
		    break;
		  case '4':		/* 4 = ERROR numeric response */
		    if (!strict ||
			rbuf[RBUFL-3] == CR || rbuf[RBUFL-3] == SP) {
			augmdmtyp |= DIAL_NV; /* OR in the nonverbal bit. */
			status = -1;	/* Bad command */
		    }
		    break;
		}
		if (dialdpy && (augmdmtyp & DIAL_NV)) /* If numeric results, */
		  conoc(LF);		  /* echo a linefeed too. */
		break;
	      case LF:			/* Got a linefeed. */
		/*
		  Note use of explicit octal codes in the string for
		  CR and LF.  We want real CR and LF here, not whatever
		  the compiler happens to define \r and \n as...
		*/
		if (!strcmp(rbuf+RBUFL-4,"OK\015\012")) /* Good response */
		  status = 1;
		else if (!strcmp(rbuf+RBUFL-7,"ERROR\015\012"))	/* Error */
		  status = -1;
		augmdmtyp &= ~(DIAL_NV); /* Turn off the nonverbal bit */
		break;
	      /* Check whether modem echoes its commands... */
	      case 't':			/* Got little t */
		if (!strcmp(rbuf+RBUFL-3,"\015at") || /* See if it's "at" */
		    !strcmp(rbuf+RBUFL-3," at"))
		    mdmecho = 1;
		debug(F111,"MDMECHO-t",rbuf+RBUFL-2,mdmecho);
		break;
	      case 'T':			/* Got Big T */
		if (!strcmp(rbuf+RBUFL-3,"\015AT") ||	/* See if it's "AT" */
		    !strcmp(rbuf+RBUFL-3," AT"))
		    mdmecho = 1;
		debug(F111,"MDMECHO-T",rbuf+RBUFL-3,mdmecho);
		break;
	      default:			/* Other characters, accumulate. */
		status = 0;
		break;
	    }
	}
	debug(F101,"getok returns","",status);
	alarm(oldalarm);		/* Restore previous alarm */
	if (saval) signal(SIGALRM,saval); /* and handler */
	ttflui();			/* Flush input buffer */
	return(status);			/* Return status */
    }
}

/* Maybe hang up the phone, depending on various SET DIAL parameters. */

int
dialhup() {
    int x = 0;
    if (dialhng) {			/* DIAL HANGUP ON? */
	x = mdmhup();			/* Try modem-specific method first */
	debug(F101,"dialhup mdmhup","",x);
	if (x > 0) {			/* If it worked, */
	    if (dialdpy)
	      printf(" Modem hangup OK\r\n"); /* fine. */
	} else if (network) {		/* If we're telnetted to */
	    if (dialdpy)		/* a modem server, just print a msg */
	      printf(" Warning: modem hangup failed\r\n"); /* don't hangup! */
	    return(0);
	} else {			/* Otherwise */
	    x = tthang();		/* Tell the OS to turn off DTR. */
	    if (dialdpy) {		/* DIAL DISPLAY ON? */
		if (x > 0)		/* Yes, tell results from tthang() */
		  printf(" Hangup OK\r\n");
		else if (x == 0)
		  printf(" Hangup skipped\r\n");
		else
		  perror(" Hangup error");
	    }
	}
    } else if (dialdpy) printf(" Hangup skipped\r\n"); /* DIAL HANGUP OFF */
    return(x);
}

/*
  M D M H U P  --

  Sends escape sequence to modem, then sends its hangup command.  Returns:
   0: If modem type is 0 (direct serial connection),
      or if modem type is < 0 (network connection),
      or if no action taken because DIAL MODEM-HANGUP is OFF)
        or because no hangup string for current modem type,
      or C-Kermit is in remote mode,
      or if action taken but there was no positive response from modem;
   1: Success: modem is in command state and acknowledged the hangup command;
  -1: On modem command error.
*/
int
mdmhup() {
#ifdef MDMHUP
    MDMINF *p;				/* Modem info structure pointer */
    int m, x = 0;

    if (dialmhu == 0 || local == 0)	/* If DIAL MODEM-HANGUP is OFF, */
      return(0);			/*  or not in local mode, fail. */

#ifdef OS2
/*
  In OS/2, if CARRIER is OFF, and there is indeed no carrier signal, any
  attempt to do i/o at this point can hang the program.  This might be true
  for other operating systems too.
*/
    if (!network) {			/* Not a network connection */
	m = ttgmdm();			/* Get modem signals */
	if ((m > -1) && (m & BM_DCD == 0)) /* Check for carrier */
	  return(0);			/* No carrier, skip the rest */
    }
#endif /* OS2 */

    m = mdmtyp & 0xff;			/* Get basic modem type (no bits!). */
    if ((m < 1) || (m > MAX_MDM))	/* If modem type not in range, */
      return(0);			/*  fail. */
    p = ptrtab[m-1];			/* Get modem info pointer */
    if (!(p->hup_str) || !*(p->hup_str)) { /* No hangup string? */
	debug(F100,"mdmhup no hup_str","",0); /* No, */
	return(0);			/*  fail. */
    } else {
	debug(F110,"mdmhup hup_str",p->hup_str,0); /* Yes. */
	if (p->esc_str && *(p->esc_str)) { /* Have escape sequence? */
	    debug(F110,"mdmhup esc_str",p->esc_str,0);
	    debug(F101,"mdmhup esc_time","",p->esc_time);
	    if (ttpkt(speed,FLO_DIAL,parity) < 0) /* Condition line */
	      return(-1);		/*  for dialing. */
	    if (p->esc_time)		/* If we have a guard time */
	      msleep(p->esc_time);	/* Pause for guard time */
#ifdef NETCONN
	    /* Send modem's escape sequence */
	    if (network) {		/* Must catch errors here. */
		if (ttol((CHAR *)(p->esc_str),(int)strlen(p->esc_str)) < 0)
		  return(-1);
	    } else {
		ttslow(p->esc_str,p->wake_rate); /* Send escape sequence */
	    }
#else
	    ttslow(p->esc_str,p->wake_rate); /* Send escape sequence */
#endif /* NETCONN */
	    if (p->esc_time)		/* Pause for guard time again */
	      msleep(p->esc_time);
	    msleep(500);		/* Wait half a sec for echoes. */
	    ttflui();			/* Flush response or echo, if any */
	}
#ifdef NETCONN
	    /* Send modem's hangup command */
	    if (network) {		/* Must catch errors here. */
		if (ttol((CHAR *)(p->hup_str),(int)strlen(p->hup_str)) < 0)
		  return(-1);
	    } else {
		ttslow(p->hup_str,p->wake_rate);
	    }
#else
	ttslow(p->hup_str,p->wake_rate); /* Now Send hangup string */
#endif /* NETCONN */
	if (p->ok_fn) {			/* Look for OK response */
	    x = (*(p->ok_fn))(3,1);	/* Give it 3 seconds, be strict. */
	    debug(F101,"mdmhup hangup response","",x);
	} else {			/* No OK function, */
	    x = 1;			/* so assume it worked */
	    debug(F101,"mdmhup no ok_fn","",x);
	}
	return(x);			/* Return OK function's return code. */
    }
#else					/* MDMHUP not defined. */
    return(0);				/* Always fail. */
#endif /* MDMHUP */
}

char *					/* Let external routines ask */
getdws(mdmtyp) int mdmtyp; {		/* about dial init-string. */
    MDMINF * p;
    if ((mdmtyp & 0xff) < 1 || (mdmtyp & 0xff) > MAX_MDM)
      return("");
    p = ptrtab[(mdmtyp & 0xff) -1];
    return(dialini ? dialini : p->wake_str);
}

char *
getdcs(mdmtyp) int mdmtyp; {		/* Same deal for dial-command */
    MDMINF * p;
    if ((mdmtyp & 0xff) < 1 || (mdmtyp & 0xff) > MAX_MDM)
      return("");
    p = ptrtab[(mdmtyp & 0xff) -1];
    return(dialcmd ? dialcmd : p->dial_str);
}

#else /* NODIAL */

char *dialv = "Dial Command Disabled";

int					/* To allow NODIAL versions to */
mdmhup() {				/* call mdmhup(), so calls to  */
    return(0);				/* mdmhup() need not be within */
}					/* #ifndef NODIAL conditionals */

#endif /* NODIAL */
