#ifndef NODIAL
char *dialv = "Dial Command, 5A(028) 23 Dec 91";

/*  C K U D I A	 --  Dialing program for connection to remote system */

/*
  Original author: Herm Fischer (HFISCHER@USC-ECLB).
  Contributed to Columbia University for inclusion in C-Kermit.
  Copyright (C) 1985, Herman Fischer, 16400 Ventura Blvd, Encino CA 91436, and
  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.

 Os9/68k and Microcom modem support by Bob Larson (Blarson@ecla.usc.edu)
 Additional code by many others over the years.

 ------

 This module should work under all versions of Unix.  It calls externally
 defined system-depended functions for i/o, but depends upon the existence
 of various modem control functions.

 This module, and the supporting routines in the ckutio.c module, assume
 that the computer and modem properly utilize the following data communi-
 cations signals (that means one should prepare the modem to use, not
 circumvent, these signals):

     Data Terminal Ready (DTR):  This signal is asserted by the computer
     when Kermit is about to ask the modem to dial a call, and is
     removed when Kermit wishes to have the modem hang up a call.
     The signal is asserted both while Kermit is asking the modem
     to dial a specific number, and after connection, while Kermit
     is in a data exchange mode.

     Carrier Detect (CD):  This signal must be asserted by the modem when
     a carrier is detected from a remote modem on a communications
     circuit.  It must be removed by the modem when the circuit
     disconnects or is hung up.	 (Carrier Detect is ignored while
     Kermit is asking the modem to dial the call, because there is
     no consistant usage of this signal during the dialing phase
     among different modem manufacturers.)
*/

/*
 * Modifications:
 *
 *	21-Jul-85	Fixed failure returns hanging on no carrier signal
 *			Requires tthang change too (ckutio.c revision)
 *							-- Herm Fischer
 *
 *	28-Jun-85	Fixed bug with defaulting the modem-failure message
 *			in lbuf.
 *							-- Dan Schullman
 *
 *	27-Jun-85	Merged in code from Joe Orost at Berkeley for
 *			supporting the US Robotics modem, which included
 *			changing the single characters in MDMINF into
 *			multi-character strings and modifying waitfor.
 *							-- Dan Schullman
 *
 *	26-Jun-85	Allow interrupts to be used to abort dialing,
 *			and ring the bell when a connection is made.
 *			Reorganized some of the failure paths to use the
 *			same code, and now close the line on failures.
 *			Allow use of stored numbers with the DF100 and
 *			DF200 modems.  Handlers now declared after the
 *			call to setjmp.
 *							-- Dan Schullman
 *
 *	24-May-85	DF03, DF100-series, DF200-series, and "unknown" modem
 *			support added.	Also restructured the various data
 *			tables, fixed some bugs related to missing data and
 *			missing case labels, and modified the failure message
 *			to display the "reason" given by the modem.
 *							-- Dan Schullman
 *	16-Mar-87	Support for the ATT7300 UNIX PC internal modem was
 *			added.
 *							-- Richard E. Hill
 *
 *	21-Feb-88	Os9/68k and microcom modem support.
 *							-- Bob Larson
 *
 *	14-Mar-88	Rewrite code for ATT7300 (here and in ckutio.c)
 *			Avoids dial(3c) with it's LCK files, hangs up line
 *			correctly, enables user interrupts and timeouts,
 *			turns on/off the system getty() login procedure.
 *			Correct Hayes command sequence at little.
 *			Procedures: attdial, atthang, ongetty, offgetty.
 *			Parts adapted from work of Richard E. Hill and
 *			Kevin O'Gorman.
 *							-- Joe R. Doupnik
 *
 *      13-Jan-89       Added IBM/Siemens/Rolm CBX dialing support.
 *                      - F. da Cruz
 *
 *	29-Aug-89	Added support for AT&T 2212C, 2224B, 2224CEO, and
 *			2296A switched-network modems in AT&T mode, and
 *			for the AT&T Digital Terminal Data Module (DTDM).
 *							-- Eric F. Jones
 *
 *       4-Jun-90       Added delay on "ANSWERED" for AT&T DTDM
 *       7-Jun-90       Added support for AT&T ISN Network
 *                      - John L. Chmielewski, AT&T
 *
 *       March 1991     Support for Telebit modems, Kermit spoofing.
 *                      Warren Tucker, Larry Jacobs, Frank da Cruz.
 */

/*
 * To add support for another modem, do the following:
 *
 *	Define a modem number symbol (n_XXX) for it, keeping the list
 *	in alphabetical and numerical order, and renumbering the values
 *	as necessary.  Make sure the n_XXX symbol is unique within 7 chars.
 *
 *	Create a MDMINF structure for it, again keeping the list alphabetical
 *	for sanity's sake.
 *
 *	Add the address of the MDMINF structure to the ptrtab array, again
 *	in alphabetical and numerical order.
 *
 *	Add the "user visible" modem name and corresponding modem number to
 *	the mdmtab array, again in alphabetical order.
 *
 *	Read through the code and add modem-specific sections as necessary.
 */

/*
 * The intent of the "unknown" modem is hopefully to allow KERMIT to support
 * unknown modems by having the user type the entire autodial sequence
 * (possibly including control characters, etc.) as the "phone number".
 * The only reason that the CONNECT command cannot be used to do this is
 * that a remote line cannot normally be opened unless carrier is present.
 *
 * The protocol and other characteristics of this modem are unknown, with
 * some "reasonable" values being chosen for some of them.  The only way to
 * detect if a connection is made is to look for carrier present.
 */
#include "ckcdeb.h"
#ifndef MAC
#include <signal.h>
#endif /* MAC */
#include "ckcasc.h"
#include "ckcker.h"
#include "ckucmd.h"

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

extern int flow, local, mdmtyp, quiet, parity, seslog, network, dialhng,
  dialdpy, mdmspd, dialtmo, dialksp, dialmnp;
extern char *dialini;
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

#define		n_ATTDTDM	 1
#define         n_ATTISN         2
#define		n_ATTMODEM	 3
#define		n_CERMETEK	 4
#define		n_DF03		 5
#define		n_DF100		 6
#define		n_DF200		 7
#define		n_GDC		 8
#define		n_HAYES		 9
#define		n_PENRIL	10
#define		n_RACAL		11
#define		n_UNKNOWN	12
#define		n_USROBOT	13
#define		n_VENTEL	14
#define		n_CONCORD	15
#define		n_ATTUPC	16	/* aka Unix PC and ATT7300 */
#define		n_ROLM          17      /* Rolm CBX DCM */
#define		n_MICROCOM	18
#define         n_HST           19
#define         n_TELEBIT       20      /* Telebit Trailblazer */
#define MAX_MDM 20
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
#define DIAL_PEP 512
#define DIAL_V32 1024
#define DIAL_V42 2048
#define DIAL_SLO 4096

#define n_HAYESNV ( n_HAYES   | DIAL_NV )
#define n_TBPEP   ( n_TELEBIT | DIAL_PEP )
#define n_TB3     ( n_TELEBIT | DIAL_V32 )
#define n_TBNV    ( n_TELEBIT | DIAL_NV )
#define n_TBPNV   ( n_TELEBIT | DIAL_NV | DIAL_PEP )
#define n_TB3NV   ( n_TELEBIT | DIAL_NV | DIAL_V32 )
#define n_TB4     ( n_TELEBIT | DIAL_V42 )
#define n_TBS     ( n_TELEBIT | DIAL_SLO )
#define n_TB4NV   ( n_TELEBIT | DIAL_NV | DIAL_V42 )
#define n_TBSNV   ( n_TELEBIT | DIAL_NV | DIAL_SLO )

/*
 * Declare structures containing modem-specific information.
 *
 * REMEMBER that only the first SEVEN characters of these
 * names are guaranteed to be unique.
 */

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
    0					/* dial_rate */
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
    0			/* dial_rate */
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
    0			/* dial_rate */
    };

static
MDMINF CERMETEK =	/* information for "Cermetek Info-Mate 212 A" modem */
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
    200			/* dial_rate */
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
    0			/* dial_rate */
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
    0			/* dial_rate */
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
    0			/* dial_rate */
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
    0			/* dial_rate */
    };

static
MDMINF HAYES =		/* information for "Hayes" modem */
    {
    35,			/* dial_time */
    ",",		/* pause_chars */
    2,			/* pause_time */
    "AT\015",		/* wake_str */
/*
  Note: Other wake_str's are possible here.  For Hayes 2400 that is to
  be used for both in and out calls, AT&F&D3 might be best.  For out calls
  only, maybe AT&F&D2.  See Hayes 2400 manual.
*/
    0,			/* wake_rate */
    "",			/* wake_prompt */
    "",			/* dmode_str */
    "",			/* dmode_prompt */
    "ATD%s\015",	/* dial_str, note: user can supply D or T */
    0			/* dial_rate */
    };

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
    0			/* dial_rate */
    };

static
MDMINF RACAL =		/* information for "Racal Vadic" modem */
    {
    35,			/* dial_time */
    "Kk",		/* pause_chars */
    5,			/* pause_time */
    "\005\015",		/* wake_str */
    50,			/* wake_rate */
    "*",		/* wake_prompt */
    "D\015",		/* dmode_str */
    "?",		/* dmode_prompt */
    "%s\015",		/* dial_str */
    0			/* dial_rate */
    };

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
    0			/* dial_rate */
    };

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
    0			/* dial_rate */
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
    0			/* dial_rate */
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
    0			/* dial_rate */
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
    0			/* dial_rate */
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
    0			/* dial_rate */
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
    0			/* dial_rate */
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
    0			/* dial_rate */
    };

static
MDMINF HST =		/* information for Courier HST modem */
    {
    35,			/* dial_time */
    ",",		/* pause_chars */
    2,			/* pause_time */
    "ATE1Q0S2=43X7&B1&M4\015", /* wake_str */
    0,		        /* wake_rate */
    "OK\015",		/* wake_prompt */
    "",		        /* dmode_str */
    "",		        /* dmode_prompt */
    "ATD%s\015",	/* dial_str */
    0			/* dial_rate */
    };

static
MDMINF TELEBIT =	/* information for Telebits */
    {
    60,			/* dial_time */
    ",",		/* pause_chars */
    2,			/* pause_time */
    "AAAAATF1M1Q4X3S2=43\015", /* wake_str */
    10,			/* wake_rate */
    "OK\015",		/* wake_prompt */
    "",		        /* dmode_str */
    "",		        /* dmode_prompt */
    "ATD%s\015",	/* dial_str */
    80			/* dial_rate */
    };

/*
 * Declare table for converting modem numbers to information pointers.
 *
 * The entries MUST be in ascending order by modem number, without any
 * "gaps" in the numbers, and starting from one (1).
 *
 * This table should NOT include entries for the "variant" modem numbers,
 * since it is assumed that they share the same information as the normal
 * value.
 */
static
MDMINF *ptrtab[] =
    {
    &ATTDTDM,
    &ATTISN,
    &ATTMODEM,
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
    &ATTUPC,			/* ATT7300 internal modem, jrd*/
    &ROLM,			/* Rolm CBX, fdc */
    &MICROCOM,
    &HST,
    &TELEBIT
    };

/*
 * Declare modem names and associated numbers for command parsing,
 * and also for doing number-to-name translation.
 *
 * The entries MUST be in alphabetical order by modem name.
 */
struct keytab mdmtab[] =
    {
    "attdtdm",		n_ATTDTDM,	0,
    "attisn",           n_ATTISN,       0,
    "attmodem",		n_ATTMODEM,	0,
    "att7300",		n_ATTUPC,	0,
    "cermetek",		n_CERMETEK,	0,
    "concord",		n_CONCORD,	0,
    "df03-ac",		n_DF03,		0,
    "df100-series",	n_DF100,	0,
    "df200-series",	n_DF200,	0,
    "direct",		0,		0,
    "gendatacomm",	n_GDC,		0,
    "hayes",		n_HAYES,	0,
    "hst-courier",      n_HST,          0,
    "microcom",		n_MICROCOM,	0,
    "none",             0,              0,
    "penril",		n_PENRIL,	0,
    "pep-telebit",      n_TBPEP,        0,
    "racalvadic",	n_RACAL,	0,
    "rolm",		n_ROLM,		0,
    "slow-telebit",     n_TBS,		0,
    "telebit",          n_TELEBIT,      0,
    "unknown",		n_UNKNOWN,	0,
    "usrobotics-212a",	n_USROBOT,	0,
    "v32-telebit",      n_TB3,		0,
    "v42-telebit",      n_TB4,		0,
    "ventel",		n_VENTEL,	0
    };

int nmdm = (sizeof(mdmtab) / sizeof(struct keytab));	/* number of modems */

_PROTOTYP (int dialhup, (void) );
_PROTOTYP (int getok, (int) );
_PROTOTYP (char * getdws, (int) );
_PROTOTYP (static VOID ttslow, (char *s, int millisec) );
_PROTOTYP (VOID xcpy, (char *to, char *from, unsigned len) );
_PROTOTYP (static VOID waitfor, (char *s) );
_PROTOTYP (static VOID dialoc, (char c) );
_PROTOTYP (static int didweget, (char *s, char *r) );
_PROTOTYP (static VOID spdchg, (long s) );

#define DIALING 4		/* for ttpkt parameter */
#define CONNECT 5

#define CONNECTED 1		/* for completion status */
#define FAILED	  2

static
char *F_reason[5] = {		/* failure reasons for message */
    "Unknown",	"Timeout", "Interrupt", "Modem", "Initialize" };

static int tries = 0;
static int mdmecho = 0;	/* assume modem does not echo */
static int augmdmtyp;	/* "augmented" modem type, to handle modem modes */

static char *p;		/* For command strings & messages */

#define LBUFL 100
#ifdef DYNAMIC
static char *lbuf;
#else
static char lbuf[LBUFL];
#endif /* DYNAMIC */

static jmp_buf sjbuf;

static SIGTYP (*savalrm)();	/* for saving alarm handler */
static SIGTYP (*savint)();	/* for saving interrupt handler */

VOID
xcpy(to,from,len)		/* Copy the given number of bytes */
  register char *to, *from;
  register unsigned len; {
    while (len--) *to++ = *from++;
}

SIGTYP
dialtime(foo) int foo; {		/* timer interrupt handler */
    n1 = F_time;
    longjmp( sjbuf, F_time );
}

SIGTYP
dialint(foo) int foo; {			/* user-interrupt handler */
    n1 = F_int;
    longjmp( sjbuf, F_int );
}

static VOID
ttslow(s,millisec) char *s; int millisec; {  /* output s-l-o-w-l-y */
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
	    x = ttinc(0) & 0177;
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
reset() {
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
spdchg(s) long s; {
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


/*  C K D I A L	 --  Dial up the remote system */

/* Returns 1 if call completed, 0 otherwise */

int
ckdial(telnbr) char *telnbr; {

    char c, c2;
    int waitct, status = 0;
    char errmsg[50], *erp;
    MDMINF *pmdminf;	/* pointer to modem-specific info */
    int x, y, m, n;
    char *s, *pc, *ws;

    long conspd;
    char *cptr;

    mymdmtyp = mdmtyp;			/* Make local copy of modem type */

    if (network) {
	printf("Sorry, 'dial' not available on network connection\n");
	return(0);
    }
    if (mymdmtyp < 1) {
	printf("Sorry, you must 'set modem' first\n");
	return(0);
    }
    if (!local) {
	printf("Sorry, you must 'set line' first\n");
	return(0);
    }
    if (speed < 0L) {
	printf("Sorry, you must 'set speed' first\n");
	return(0);
    }
    debug(F110,"dial string",telnbr,0);

    /* Carrier no-wait can be invalidated by ckutio fun and games, jrd */
    if (ttopen(ttname,&local,mymdmtyp,0) < 0) { /* Open, no carrier wait */
	erp = errmsg;
	sprintf(erp,"Sorry, can't open %s",ttname);
	perror(errmsg);
	return(0);
    }
    pmdminf = ptrtab[(mymdmtyp & 0xff) -1 ]; /* set pointer to modem info */
    augmdmtyp = mymdmtyp;		/* initialize "augmented" modem type */
    mymdmtyp &= 0xff;			/* major modem type */

    /* interdigit waits for tone dial */

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
	if (augmdmtyp == n_TBPEP || augmdmtyp == n_TBPNV) {
	    waitct += 30;	/* Longer connect wait for PEP call */
	}
    } else {				/* User-specified timeout */
	waitct = dialtmo;
    }
    for (m = 0; m < nmdm; m++) {	/* Look up modem type. */
	if (mdmtab[m].kwval == mymdmtyp) {
	    break;
	}
    }
    printf(" Dialing %s\n",telnbr);
    printf(" Device=%s, modem=%s, speed=%ld\n",
	   ttname,
	   (m >= nmdm ? "unk" : mdmtab[m].kwd),
	   speed);

    printf(" The timeout for completing the call is %d seconds.\n",waitct);
    printf(
#ifdef MAC
" Type Command-. to cancel the dialing.\n");
#else
" Type your interrupt character (normally ^C) to cancel the dialing.\n");
#endif /* MAC */
    debug(F111,"ckdial",ttname,(int) (speed / 10L));
    debug(F101,"ckdial timeout","",waitct);

/* Hang up the modem (in case it wasn't "on hook") */
/* But only if SET DIAL HANGUP ON... */

    if (dialhup() < 0) {
	if (dialhng) {
	    ttclos(0);
            if (ttopen(ttname,&local,mymdmtyp,0) < 0) {
		printf("Sorry, Can't hang up tty line\n");
		printf("Try 'set line %s' again\n",ttname);
		return(0);
	    }
	}
    }
    if (augmdmtyp == n_ROLM) sleep(1);

/* Condition console terminal and communication line */

    /* Place line into "clocal" dialing state */

    if (ttpkt(speed,DIALING,parity) < 0) {
	dialhup();			/* If ttpkt fails do all this... */
	ttclos(0);
	if (ttopen(ttname,&local,mymdmtyp,0) < 0) {
	    erp = errmsg;
	    sprintf(erp,"Sorry, can't reopen %s",ttname);
	    perror(errmsg);
	    return(0);
	}				/* And try again. */
	if (ttpkt(speed,DIALING,parity) < 0) {
	    printf("Sorry, Can't condition communication line\n");
	    printf("Try 'set line %s' again\n",ttname);
	    return(0);
	}
    }
#ifdef DYNAMIC
    if (!(lbuf = malloc(LBUFL+1))) {    /* Allocate input line buffer */
	printf("Sorry, memory buffer can't be allocated\n");
	return(0);
    }
#endif /* DYNAMIC */
    sleep(2);				/* Let it finish waking up. */

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
	    if (signal(SIGINT, SIG_IGN) != SIG_IGN)
		signal(SIGINT, dialint);
#endif /* MAC */
	    alarm(10);			/* be sure to exit this section */
	}
	switch (n) {			/* type of failure */
	    case F_time: {		/* timed out */
		printf ("No connection made within the allotted time.\n");
		if (mymdmtyp == n_HAYES || mymdmtyp == n_TELEBIT)
		  ttoc('\015');		/* Send CR to interrupt dialing */
  		  /* some hayes modems don't fail with BUSY on busy lines */
		debug(F110,"dial","timeout",0);
		break;
	    }
	    case F_int: {		/* dialing interrupted */
		printf ("Dialing interrupted.\n");
		debug(F110,"dial","interrupted",0);
		if (mymdmtyp == n_HAYES || mymdmtyp == n_TELEBIT)
		  ttoc('\015');		/* Send CR to interrupt dialing */
		break;
	    }
	    case F_modem: {		/* modem detected a failure */
		printf ("Failed (\"");
		for (pc = lbuf; *pc; pc++)
		  if (isprint(*pc))
		    putchar(*pc);	/* display printable reason */
		printf ("\").\n");
		debug(F110,"dial",lbuf,0);
		break;
	    }
	    case F_minit: {		/* cannot initialize modem */
		printf ("Can't initialize modem.\n");
		debug(F110,"dial","modem init",0);
		break;
	    }
	}
	reset();			/* reset alarms, etc. */
	dialhup();			/* try to hang up phone. */
#ifdef DYNAMIC
	if (lbuf) free(lbuf);
#endif /* DYNAMIC */
	return(0);			/* exit with failure code */
    }

/* Set timer and interrupt handlers. */

    savalrm = signal(SIGALRM,dialtime); /* Set alarm handler */
#ifdef MAC
    savint = signal(SIGINT, dialint);
#else /* MAC */
    if ((savint = signal(SIGINT,SIG_IGN)) != SIG_IGN )
      signal(SIGINT,dialint);		/* Set int handler if not ignored */
#endif /* MAC */
#ifdef COMMENT
    x = 10;				/* Give modem 10 seconds to wake up */
    debug(F101,"ckdial wakeup timout","",x);
    x = alarm(x);
    debug(F101,"ckdial previous alarm","",x);
#endif /* COMMENT */

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
	    alarm(waitct);			/* do alarm properly */
	    if (attdial(ttname,speed,telnbr)) { /* dial internal modem */
		reset();			/* reset alarms, etc. */
		if (! quiet) printf(" Call did not complete.");
		dialhup();	        	/* Hangup the call */
#ifdef DYNAMIC
		if (lbuf) free(lbuf);
#endif /* DYNAMIC */
		return(0);			/* return failure */
	    }
	    reset();				/* reset alarms, etc. */
	    ttpkt(speed,CONNECT,parity); /* cancel dialing ioctl */
	    if (! quiet) {
		if (dialdpy) printf("\n");
		printf(" Call completed.\07\r\n");
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
	ws = dialini ? dialini : HAYES.wake_str;
	for (tries = 4; tries > 0; tries--) {
	    ttslow(ws,pmdminf->wake_rate);
	    status = getok(4);
	    if (status > 0) break;
	    sleep(1);			/* Wait before retrying */
	}
	if (status > 0) break;		/* Initialized OK. */
	/* modem-initialization failure */
	n1 = (status == 0) ? F_minit : F_modem;
	longjmp( sjbuf, n1 );

/*
    Telebit support: Trailblazer, T1000, T2500.
    C-Kermit global variables 'dialksp', 'flow', 'dialini', 'parity',
    and local variable 'augmdmtyp' modify our behavior:

    modulation- and confguration-independent initialization:
    S11         DTMF timing (left alone)
    S45         remote access (left alone)
    S48=1       all 8 bits are significant, 0 only 7 bits
    S51=255     set serial port baud rate automatically (no typeahead)
    S52=2       go on hook when DTR drops and reset to NV-RAM
    S53         DCD/DSR control (left alone)
    S54=2       pass BREAK signal to remote modem immediately
    S55=0       respond to command escape sequence <sec>+++<sec>
    depending upon the value of 'flow':
    ==FLO_NONE  S58=0   DTE uses no flow control
    ==FLO_HARD  S58=2   DTE uses hardware (CTS/RTS) flow control
    ==FLO_XONX  S58=3   DTE uses XON/XOFF flow control
    S66         lock interface speed: depends on value of mdmspd variable:
                mdmspd == 0 sets S66=1 (lock speed, use flow control)
                mdmspd != 0 sets S66=0 (change speed, no flow control)
    S68=255     DCE uses whatever flow control DTE uses (specified in S58)
    S95=0       MNP: depends on value of dialmnp variable:
                dialmnp == 0 sets S95=0 (no MNP)
                dialmnp != 0 sets S95=2 (auto MNP)
    S110=1      use data compression if remote modem agrees

    SET MODEM TELEBIT
    "traditional" configuration (PEP, V.22, V.22bis, 212A, 103):
    S50=0       use automatic configuration to answering modem

    SET MODEM PEP-TELEBIT
    PEP mode with SET DIAL KERMIT-SPOOF ON
    S50=255     use PEP + lock DCE speed to DTE speed
    S111=10     Kermit with no parity
    S111=11     Kermit with odd parity
    S111=12     Kermit with even parity
    S111=13     Kermit with mark parity
    S111=14     Kermit with space parity
    S112=??     set to Kermit's start-of-packet character, stchr

    SET MODEM PEP-TELEBIT with SET DIAL KERMIT-SPOOF OFF
    S50=255     use PEP + lock DCE speed to DTE speed
    S111=0      use normal PEP inter-modem protocol

    SET MODEM V32-TELEBIT  (doesn't work with Trailblazer or T1000)
    S50=6       use V.32

    SET MODEM V42-TELEBIT
    S50=0       auto speed select
    S95=1	MNP reliable mode
    S96=1	enable MNP data compression    
    S97=1	enable LAP-M
    S98=3	compression in both directions
    S106=1	V.42 detection enable

    SET MODEM SLOW-TELEBIT
    S94=1	connect to any speed up to maximum specified by S50
    S50=3	connect to Bell 103/212a, V.21, V.22 and V.22bis
*/
    case n_TELEBIT:			/* Telebits... */
    case n_TBNV:
    case n_TBPEP:
    case n_TB3:
    case n_TB4:
    case n_TBS:
    case n_TBPNV:
    case n_TB3NV:
    case n_TB4NV:
    case n_TBSNV: {
	int S58,S66,S95,S111,S112;	/* Some Telebit register names */
	char t25buf[64];		/* Modem command buffer */
/*
  If user defined a DIAL INIT-STRING, send that now, otherwise send built-in
  Telebit string.  Try up to 4 times to get OK or 0 response from modem.
*/
	ws = dialini ? dialini : TELEBIT.wake_str;
	debug(F110,"ckdial telebit init string",ws,0);
	for (tries = 4; tries > 0; tries--) {
	    ttslow(ws,pmdminf->wake_rate);
	    status = getok(5);
	    if (status) break;
	    dialhup();			/* Hang up */
	    ttflui();			/* Flush input buffer */
	}
	if (status < 1) {
	    n1 = F_minit;
	    longjmp( sjbuf, F_minit ); /* Failed. */
	}
	if (!dialini) {			/* If using built-in init strings */
	    /* Step 2 */
	    sprintf(t25buf,
		    "ATS48=%d S51=255 S52=2 S54=2\015",
		    parity ? 0 : 1);
	    s = t25buf;
	    debug(F110,"ckdial Telebit init step 2",s,0);
	    for (tries = 4; tries > 0; tries--) {
		ttslow(s,pmdminf->wake_rate);
		status = getok(5);
		if (status) break;
		msleep(500);
		ttflui();
	    }
	    if (status < 1) {
		n1 = F_minit;
		longjmp( sjbuf, F_minit ); /* Failed. */
	    }
	    switch (flow) {		/* Flow control */
	      case FLO_HARD: S58 = 2; break; /* RTX/CTS  (hardware) */
	      default:	     S58 = 0; break; /* NONE (or other) */
	    }		
	    if (dialmnp) {		/* MNP negotiation */
		S95 = 2;		/* Enabled (auto) */
	    } else {
		S95 = 0;		/* Disabled */
	    }
	    S66 = mdmspd ? 0 : 1;	/* Interface speed locked? */
	    msleep(500);
	    sprintf(t25buf,
		    "ATS58=%d S55=0 S66=%d S68=255 S95=%d\015",S58,S66,S95);
	    s = t25buf;
	    debug(F110,"ckdial Telebit init step 3",s,0);
	    for (tries = 4; tries > 0; tries--) {
		ttslow(s,pmdminf->wake_rate);
		status = getok(5);
		if (status) break;
		msleep(500);
		ttflui();
	    }
	    if (status < 1) {
		n1 = F_minit;
		longjmp( sjbuf, F_minit ); /* Failed. */
	    }
	    p = "";
	    s = "";
	    switch (augmdmtyp) {	/* we will auto-config */
	      case n_TELEBIT:		/* 103,212,v22bis modulation */
              case n_TBNV:
       		p = "standard";		/* Automatic speed determination */
		s = "ATS50=0 S110=0\015";
		break;
	      case n_TBPEP:		/* PEP Protocol */
              case n_TBPNV:
		if (dialksp) {		/* SET DIAL KERMIT-SPOOF ON? */
		    switch (parity) {
		      case 'e': S111 = 12; break;
		      case 'm': S111 = 13; break;
		      case 'o': S111 = 11; break;
		      case 's': S111 = 14; break;
		      default:
		      case 0:   S111 = 10; break;
		    }
		    S112 = stchr;	/* Start-of-packet character */
		    sprintf(t25buf,
			    "ATS50=255 S110=1 S111=%d S112=%d\015",S111,S112);
		    p = "PEP/Kermit";
		    s = t25buf;
		} else {		/* SET DIAL KERMIT-SPOOF OFF */
		    p = "PEP/No-Kermit";
		    s = "ATS50=255 S110=1\015"; /* Accept any protocol */
		}
		debug(F101,"ckdial telebit pep waitct","",waitct);
		break;

	      case n_TB3:			/* Telebit V.32 */
	      case n_TB3NV:
		p = "V.32";
		s = "ATS50=6\015";
		break;

	      case n_TB4:			/* Telebit V.42 */
	      case n_TB4NV:
		p = "V.42";
		s = "ATS50=0 S95=1 S96=1 S97=1 S98=3 S106=1\015";
		break;
		
	      case n_TBS:			/* Telebit up to 2400 Baud */
	      case n_TBSNV:
		p = "300/1200/2400 Baud";
		s = "ATS94=1 S50=3\015";
		break;
	    }
	    debug(F111,"ckdial Telebit config",p,speed);
	    debug(F110,"ckdial Telebit init step 4",s,0);
	    if (*s) {
		for (tries = 4; tries > 0; tries--) {
		    ttslow(s,pmdminf->wake_rate);
		    status = getok(5);
		    if (status) break;
		    msleep(500);
		    ttflui();
		}
		debug(F101,"ckdial telebit init status","",status);
		if (status < 1) {
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
      
    case n_MICROCOM:		/* interdigit waits for tone dial */
        {
	    jmp_buf savejmp;
	    alarm(0);
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
		alarm(0);
		xcpy((char *)sjbuf, (char *)savejmp, sizeof savejmp);
	    }
	}
	break;
    case n_ATTDTDM:		/* DTDM requires BREAK to wake up */
	ttsndb();		/* Send BREAK */
	break;			/* ttsndb() defined in ckutio.c */

    default:			/* place modem into command mode */
        ws = dialini ? dialini : pmdminf->wake_str;
	debug(F111,"ckdial default, wake string",ws,
	      pmdminf->wake_rate);
	ttslow(ws, pmdminf->wake_rate);
	debug(F110,"ckdial default, waiting for wake_prompt",
	      pmdminf->wake_prompt,0);
	savalrm = signal(SIGALRM,dialtime);
	alarm(10);
	waitfor(pmdminf->wake_prompt);
	break;
    }
    alarm(0);			/* turn off alarm */
    if (augmdmtyp != n_TELEBIT)
      debug(F100,"ckdial got wake prompt","",0);
    msleep(500);		/* give things settling time */

/* Enable/disable MNP (Telebit already done above) */

    switch (augmdmtyp) {
      case n_HST:	
	if (dialmnp)
	  ttslow("AT&K2\015",pmdminf->wake_rate);
	else
	  ttslow("AT&K0\015",pmdminf->wake_rate);
	getok(5);			/* Get response */
	break;
      default:
	break;
    }

/* Dial the number */
				/* put modem into dialing mode */
    ttslow(pmdminf->dmode_str, pmdminf->dial_rate);
    savalrm = signal(SIGALRM,dialtime);
    alarm(10);
    if (pmdminf->dmode_prompt) {	/* wait for prompt, if any expected */
	waitfor(pmdminf->dmode_prompt);
	msleep(300);
    }
    alarm(0);			/* turn off alarm on dialing prompts */
    ttflui();			/* clear out stuff from waking modem up */

    sprintf(lbuf, pmdminf->dial_str, telnbr); /* form dialing string */
    debug(F110,"dialing",lbuf,0);
    ttslow(lbuf,pmdminf->dial_rate);	/* send dialing string */

    savalrm = signal(SIGALRM,dialtime);	/* Time to allow for connecting */
    x = alarm(waitct);			/* This much time... */
    debug(F101,"ckdial old alarm","",x);
    debug(F101,"ckdial waitct","",waitct);
    
    switch (augmdmtyp) {
    case n_RACAL: /* acknowledge printout of dialing string */
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

/* Check for connection */

/*
 * I believe we also need to look for carrier in order to determine if a
 * connection has been made.  In fact, for many we may only want to look for
 * the "failure" responses in order to short-circuit the timeout, and let
 * carrier be the determination of whether a connection has been made. -- DS
 */
    status = 0;
    strcpy(lbuf,"No Connection");	/* default failure reason */
    debug(F101,"dial awaiting response, augmdmtyp","",augmdmtyp);
    while (status == 0) {
	switch (augmdmtyp) {
	  default:
	    for (n = 0; n < LBUFL-1; n++) { /* accumulate response */
		lbuf[n] = c2 = (ttinc(0) & 0177);
		dialoc(c2);
		if ( lbuf[n] == CR || lbuf[n] == LF ) break;
	    }
	    lbuf[n] = '\0';		/* Terminate response from modem */
	    debug(F111,"dial modem response",lbuf,n);
	    if (n) {			/* If one or more characters present */
		switch (augmdmtyp) {	/* check for modem response message. */
		  case n_ATTMODEM:
		    /* Careful - "Connected" / "Not Connected" */
		    if (didweget(lbuf,"Busy") ||
			didweget(lbuf,"Not connected") ||
			didweget(lbuf,"Not Connected") ||
			didweget(lbuf,"No dial tone") ||
			didweget(lbuf,"No Dial Tone") ||
			didweget(lbuf,"No answer") ||
			didweget(lbuf,"No Answer"))
			status = FAILED;
		    else if (didweget(lbuf,"Answered") ||
			didweget(lbuf,"Connected"))
			status = CONNECTED;
		    break;

		  case n_ATTISN:
		    if (didweget(lbuf,"ANSWERED"))
			status = CONNECTED;
		    else if (didweget(lbuf,"BUSY") ||
			didweget(lbuf,"DISCONNECT") ||
			didweget(lbuf,"NO ANSWER") ||
			didweget(lbuf,"WRONG ADDRESS"))
			status = FAILED;
		    break;

		  case n_ATTDTDM:
		    if (didweget(lbuf,"ANSWERED"))
			status = CONNECTED;
		    else if (didweget(lbuf,"BUSY") ||
			didweget(lbuf,"CHECK OPTIONS") ||
			didweget(lbuf,"DISCONNECTED") ||
			didweget(lbuf,"DENIED"))
			status = FAILED;
#ifdef DEBUG
#ifdef ATT6300
		    else if (deblog && didweget(lbuf,"~~"))
			status = CONNECTED;
#endif /* ATT6300 */
#endif /* DEBUG */
		    break;

		  case n_CERMETEK:
		    if (didweget(lbuf,"\016A")) {
			status = CONNECTED;
			ttslow("\016U 1\015",200); /* make transparent*/
		    }
		    break;

		  case n_DF100:	     /* DF100 has short response codes */
		    if (strcmp(lbuf,"A") == 0)
			status = CONNECTED; /* Attached */
		    else if (strcmp(lbuf,"N") == 0 || /* No Ans or Dialtone */
			     strcmp(lbuf,"E") == 0 || /* Error */
			     strcmp(lbuf,"R") == 0) { /* Ready */
			status = FAILED;
			break;
		    }
		    /* otherwise fall thru... */

		  case n_DF200:
		    if (didweget(lbuf,"Attached"))
			status = CONNECTED;
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
			status = FAILED;
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
			status = CONNECTED;
		    else if (didweget(lbuf,"NO CONNECT"))
			status = FAILED;
		    break;

		  case n_HAYES:
		  case n_USROBOT:
		  case n_HST:
		  case n_TELEBIT:
		    if (mdmspd) {
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
					status = CONNECTED;
					if ( !quiet )
					  printf(" Speed changed to %lu\r\n",
						 conspd);
				    }
				}
			    } /* Expanded to handle any conceivable speed */
			}
		    }
		    if (mymdmtyp == n_TELEBIT) {
			if (didweget(lbuf,"CONNECT FAST/KERM")) {
			    status = CONNECTED;
			    if (!quiet) printf("FAST/KERM ");
			    break;
			}
 		    }
  		    if (didweget(lbuf,"RRING") || didweget(lbuf,"RINGING")) { 
			status = 0;
		    } else if (didweget(lbuf,"CONNECT")) {
			status = CONNECTED;
		    } else if (didweget(lbuf,"NO CARRIER") ||
			didweget(lbuf,"NO DIALTONE") ||
			didweget(lbuf,"NO DIAL TONE") ||
			didweget(lbuf,"BUSY") ||
			didweget(lbuf,"NO ANSWER") ||
			didweget(lbuf,"VOICE") ||
			didweget(lbuf,"RING") ||
			didweget(lbuf,"ERROR")) {
			status = FAILED;
		    }
		    break;
		  case n_PENRIL:
		    if (didweget(lbuf,"OK"))
			status = CONNECTED;
		    else if (didweget(lbuf,"BUSY") ||
			didweget(lbuf,"NO RING"))
			status = FAILED;
		    break;
		  case n_RACAL:
		    if (didweget(lbuf,"ON LINE"))
			status = CONNECTED;
		    else if (didweget(lbuf,"FAILED CALL"))
			status = FAILED;
		    break;
		  case n_ROLM:
		    if (didweget(lbuf,"CALLING"))
			status = 0;
		    else if (didweget(lbuf,"COMPLETE"))
			status = CONNECTED;
		    else if (didweget(lbuf,"FAILED") ||
			didweget(lbuf,"NOT AVAILABLE") ||
			didweget(lbuf,"LACKS PERMISSION") ||
			didweget(lbuf,"NOT A DATALINE") ||
			didweget(lbuf,"BUSY") ||
			didweget(lbuf,"ABANDONDED") ||
			didweget(lbuf,"DOES NOT ANSWER"))
			status = FAILED;
		    /*
                      Early versions of the Rolm 9751 CBX software do not
                      give a CALL COMPLETE indication when dialing an
                      outpool number, but they do seem to return a long
                      string of DELs at that point.  (this doesn't really
                      work...)
		    else if (didweget(lbuf,"\177\177\177"))
			status = CONNECTED;
		    */
		    break;
		  case n_VENTEL:
		    if (didweget(lbuf,"ONLINE!") ||
			didweget(lbuf,"Online!"))
			status = CONNECTED;
		    else if (didweget(lbuf,"BUSY") ||
			didweget(lbuf,"DEAD PHONE") ||
			didweget(lbuf,"Busy"))
			status = FAILED;
		    break;
		  case n_CONCORD:
		    if (didweget(lbuf,"INITIATING"))
			status = CONNECTED;
		    else if (didweget(lbuf,"BUSY") ||
			didweget(lbuf,"CALL FAILED"))
			status = FAILED;
		    break;
		  case n_MICROCOM:
		    /* "RINGBACK" means phone line ringing, continue */
		    if (didweget(lbuf,"NO CONNECT") ||
			didweget(lbuf,"BUSY") ||
			didweget(lbuf,"NO DIALTONE") ||
			didweget(lbuf,"COMMAND ERROR") ||
			didweget(lbuf,"IN USE"))
			status = FAILED;
		    else if (didweget(lbuf,"CONNECT"))
			status = CONNECTED;
			/* trailing speed ignored */
		    break;
		}
	    }
	    break;

	case n_DF03:			/* because response lacks CR or NL */
	    c = ttinc(0) & 0177;
	    dialoc(c);
	    debug(F000,"dial df03 got","",c);
	    if ( c == 'A' ) status = CONNECTED;
	    if ( c == 'B' ) status = FAILED;
	    break;

	case n_HAYESNV:			/* Hayeslike modems in digit */
	case n_TBNV:			/* response mode... */
	case n_TB3NV:
	case n_TBPNV:
	case n_TB4NV:
	case n_TBSNV:
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
  And for Telebits:
    50 = CONNECT FAST
    52 = RRING
    What else?
*/
	    {				/* Nonverbal response code handler */
	    char nbuf[8];		/* Response buffer */
	    int i, j;			/* Buffer pointers */
	    if (mdmecho) {		/* Sponge up dialing string echo. */
		while ((c = (ttinc(0) & 0177)) != CR)
		  dialoc(c);
	    }
	    while (status == 0) {	/* Read response */
		for (i = 0; i < 8; i++)	/* Clear the buffer */
		  nbuf[i] = '\0';
		i = 0;			/* Reset the buffer pointer. */
		c = ttinc(0) & 0177;	/* Get first digit of response. */
					/* using an untimed, blocking read. */
		dialoc(c);		/* Echo it if requested. */
		if (!isdigit(c))	/* If not a digit, keep looking. */
		  continue;
		nbuf[i++] = c;		/* Got a digit, save it. */
		while (i < 8) {		/* Keep reading till we time out */
		    y = alarm(0) - 1;	/* Save current alarm */
		    x = ttinc(1);	/* Read char with 1-sec timeout */
		    if (y > 0) alarm(y); /* Restore alarm */
		    if (x > 0) {	/* If we didn't time out, */
			c = (char) x;	/*  we got this character. */
			if (c != CR)	/* Save it, */
			  nbuf[i++] = c; /* if it's not a carriage return. */
			dialoc(c);	/* Echo it. */
		    } else break;	/* Otherwise assume end of response. */
		}
		nbuf[i] = '\0';		/* Terminate the buffer. */
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
		    status = CONNECTED;	/* Could be any speed */
		    break;
		  case 2:		/* RING */
		    if (dialdpy) printf("\r\n Local phone is ringing!\r\n");
		    status = FAILED;
		    break;
		  case 3:		/* NO CARRIER */
		    if (dialdpy) printf("\r\n No Carrier.\r\n");
		    status = FAILED;
		    break;
		  case 4:		/* ERROR */
		    if (dialdpy) printf("\r\n Modem Command Error.\r\n");
		    status = FAILED;
		    break;
		  case 5:		/* CONNECT 1200 */
		    spdchg(1200L);	/* Change speed if necessary. */
		    status = CONNECTED;
		    break;
		  case 6:		/* NO DIALTONE */
		    if (dialdpy) printf("\r\n No Dialtone.\r\n");
		    status = FAILED;
		    break;
		  case 7:		/* BUSY */
		    if (dialdpy) printf("\r\n Busy.\r\n");
		    status = FAILED;
		    break;
		  case 8:		/* NO ANSWER */
		    if (dialdpy) printf("\r\n No Answer.\r\n");
		    status = FAILED;
		    break;
		  case 9:		/* CONNECT 2400 */
		  case 10:
		    spdchg(2400L);	/* Change speed if necessary. */
		    status = CONNECTED;
		    break;
		  case 11:		/* CONNECT 4800 */
		    spdchg(4800L);
		    status = CONNECTED;
		    break;
		  case 12:		/* CONNECT 9600 */
		    spdchg(9600L);
		    status = CONNECTED;
		    break;
		  case 52:		/* RRING */
		    debug(F101,"dial hayesnv 52 mymdmtyp","",mymdmtyp);
		    debug(F101,"dial hayesnv 52 n_TELEBIT","",n_TELEBIT);
		    if (mymdmtyp == n_TELEBIT)
		      if (dialdpy) printf(" Ringing...\r\n");
		    break;
		  case 50:		/* CONNECT FAST */
		    if (mymdmtyp == n_TELEBIT)
		      status = CONNECTED; /* CONNECT FAST */
		    break;
		  default:
		    break;
		}
	    }
	    if (status == CONNECTED && nbuf[j] != '\0') {
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
		while (y-- > -1) {
		    x = ttchk();
		    if (x > 0) {
			if (x > LBUFL) x = LBUFL;
			x = ttxin(x,(CHAR *)lbuf);
			if ((x > 0) && dialdpy) conol(lbuf);
		    }
		    x = ttgmdm();	/* Try to read modem signals */
		    if (x < 0) break;	/* Can't, pretend we suceeded */
		    if (x & BM_DCD)	/* Got signals OK.  Carrier present? */
		      break;		/* Yes, done. */
		    sleep(1);
		}
	        status = CONNECTED;
	        break;
  	    }
	}				/* switch (augmdmtyp) */
    }					/* while status == 0 */
    x = alarm(0);			/* turn off alarm on connecting */
    debug(F101,"ckdial alarm off","",x);
    if ( status != CONNECTED ) {		/* modem-detected failure */
	n1 = F_modem;
	longjmp( sjbuf, F_modem );	/* exit (with reason in lbuf) */
    }
    msleep(1000);			/* in case DTR blinks  */
    alarm(3);				/* precaution in case of trouble */
    debug(F110,"dial","succeeded",0);
    if (augmdmtyp != n_ROLM)		/* Rolm has wierd modem signaling */
      ttpkt(speed,CONNECT,parity);	/* cancel dialing state ioctl */
    reset();				/* reset alarms, etc. */
    if (!quiet)
      printf (" Call completed.\07\n");
#ifdef DYNAMIC
    if (lbuf) free(lbuf);
#endif /* DYNAMIC */
    return(1);				/* return, and presumably connect */
}

/*
  getok - wait up to n seconds for "OK<CRLF>" or "<CR>0" from modem.
  Use with Hayeslike modems for reading reply to a nondialing command.
  Does its own internal timeout handling, but resets alarms to 0.
  Returns 0 if it timed out, 1 if it succeeded, -1 on modem command error.
*/
static jmp_buf okbuf;			/* Jump-buf for getok(). */

SIGTYP
oktimo(foo) int foo; {			/* Alarm handler for getok(). */
    longjmp(okbuf,1);
}

int
getok(n) int n; {
    CHAR c;
    int i, x, status;
    char rbuf[8];			/* response buffer */
    SIGTYP (*saval)();			/* for saving alarm handler locally */

    saval = signal(SIGALRM,oktimo);	/* set timer */
    alarm(n);
    if (setjmp(okbuf)) {		/* handle timeout */
	alarm(0);
	if (saval) signal(SIGALRM,saval);
	debug(F100,"getok timed out","",0);
	ttflui();
	return(0);
    } else {				/* not timed out... */
	status = 0;
	rbuf[7] = '\0';			/* Terminate response buffer */
	while (status == 0) {
	    x = ttinc(0);		/* Read a character */
	    if (x < 0) return(-1);	/* i/o error */
	    debug(F101,"getok ttinc returns","",x);
	    c = x & 0x7f;		/* Get low order 7 bits */
	    debug(F101,"getok converts this to char","",c);
	    if (dialdpy) conoc((char)c); /* Echo it if requested */
	    for (i = 0; i < 6; i++)	/* Rotate buffer */
	      rbuf[i] = rbuf[i+1];
	    rbuf[6] = c;		/* Deposit character */
	    debug(F000,"getok",rbuf,(int) c); /* Log it */
	    switch (c) {		/* Interpret it. */
	      case CR:			/* Got carriage return. */
		if (augmdmtyp & DIAL_NV)  /* If using digit result codes */
		  conoc(LF);		  /* echo a linefeed too. */
		break;
	      case LF:			/* Got linefeed. */
		/*
		  Note use of explicit octal codes in the string for
		  CR and LF.  We want real CR and LF here, not whatever
		  the compiler happens to define \r and \n as...
		*/
		if (!strcmp(rbuf+3,"OK\015\012"))
		  status = 1;
		else if (!strcmp(rbuf,"ERROR\015\012"))
		  status = -1;
		augmdmtyp &= ~(DIAL_NV); /* Turn off the nonverbal bit */
		break;
	      /* Check whether modem echoes its commands... */
	      case 't':			/* Got little t */
		if (!strcmp(rbuf+4,"\015at")) mdmecho = 1;
		break;
	      case 'T':			/* Got Big T */
		if (!strcmp(rbuf+4,"\015AT")) mdmecho = 1;
		break;
	      case '0':			/* Got 0: Numeric result code? */
		if (rbuf[5] == CR) {	/* Yes if previous char was CR. */
		    augmdmtyp |= DIAL_NV; /* OR in the "nonverbal" bit. */
		    status = 1;
		}
		break;
	      case '4':			/* Command line error */
		if (rbuf[5] == CR)	/* if previous char was CR */
		  status = -1;
		break;
	      default:			/* Other characters, accumulate. */
		status = 0;
		break;
	    }
	}
	debug(F101,"getok returns","",status);
	alarm(0);
	if (saval) signal(SIGALRM,saval);
	ttflui();
	return(status);
    }
}

/* Maybe hang up the phone, depending on various SET DIAL parameters. */

int
dialhup() {
    int x = 0;
    if (dialhng) {			/* DIAL HANGUP ON? */
	x = tthang();
	if (dialdpy) {			/* DIAL DISPLAY ON? */
	    if (x > 0)			/* Yes, tell results from tthang() */
	      printf(" Hangup ok\r\n");
	    else if (x == 0)
	      printf(" Hangup skipped\r\n");
	    else
	      perror(" Hangup error");
	}
    } else if (dialdpy) printf(" No hangup\r\n"); /* DIAL HANGUP OFF */
    return(x);
}

char *					/* Let external routines ask */
getdws(mdmtyp) int mdmtyp; {		/* about dial wake string. */
    MDMINF * pmdminf;
    if (mdmtyp < 1 || mdmtyp > MAX_MDM) return("");
    pmdminf = ptrtab[(mdmtyp & 0xff) -1 ];
    return(dialini ? dialini : pmdminf->wake_str);
}

#else
char *dialv = "Dial Command Disabled";
#endif /* NODIAL */
