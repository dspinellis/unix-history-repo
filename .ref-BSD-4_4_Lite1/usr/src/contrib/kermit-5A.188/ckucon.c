char *connv = "CONNECT Command for UNIX, 5A(047) 23 Nov 92";

/*  C K U C O N  --  Dumb terminal connection to remote system, for UNIX  */
/*
  Author: Frank da Cruz (fdc@columbia.edu, FDCCU@CUVMA.BITNET),
  Columbia University Center for Computing Activities.
  First released January 1985.
  Copyright (C) 1985, 1992, Trustees of Columbia University in the City of New
  York.  Permission is granted to any individual or institution to use this
  software as long as it is not sold for profit.  This copyright notice must be
  retained.  This software may not be included in commercial products without
  written permission of Columbia University.
*/

#include "ckcdeb.h"			/* Common things first */

#ifdef NEXT
#undef NSIG
#include <sys/wait.h>			/* For wait() */
#endif /* NEXT */

#include <signal.h>			/* Signals */
#include <errno.h>			/* Error numbers */

#ifdef ZILOG				/* Longjumps */
#include <setret.h>
#else
#include <setjmp.h>
#endif /* ZILOG */

/* Kermit-specific includes */

#include "ckcasc.h"			/* ASCII characters */
#include "ckcker.h"			/* Kermit things */
#include "ckucmd.h"			/* For xxesc() prototype */
#include "ckcnet.h"			/* Network symbols */
#ifndef NOCSETS
#include "ckcxla.h"			/* Character set translation */
#endif /* NOCSETS */

/* Internal function prototypes */

_PROTOTYP( VOID doesc, (char) );
_PROTOTYP( VOID logchar, (char) );
_PROTOTYP( int hconne, (void) );
_PROTOTYP( VOID shomdm, (void) );

#ifndef SIGUSR1				/* User-defined signals */
#define SIGUSR1 30
#endif /* SIGUSR1 */

#ifndef SIGUSR2
#define SIGUSR2 31
#endif /* SIGUSR2 */

/* External variables */

extern int local, escape, duplex, parity, flow, seslog, sessft, debses,
 mdmtyp, ttnproto, cmask, cmdmsk, network, nettype, deblog, sosi, tnlm,
 xitsta, what, ttyfd, quiet, backgrd, pflag, tt_crd, tn_nlm;
extern long speed;
extern char ttname[], sesfil[], myhost[], *ccntab[];

#ifndef NOSETKEY			/* Keyboard mapping */
extern KEY *keymap;			/* Single-character key map */
extern MACRO *macrotab;			/* Key macro pointer table */
static MACRO kmptr = NULL;		/* Pointer to current key macro */
#endif /* NOSETKEY */

/* Global variables local to this module */

static int quitnow = 0,			/* <esc-char>Q was typed */
  dohangup = 0,				/* <esc-char>H was typed */
  sjval = 0,				/* Setjump return value */
  goterr = 0,				/* I/O error flag */
#ifndef SUNX25
  active = 0,				/* Lower fork active flag */
#endif /* SUNX25 */
  inshift = 0,				/* SO/SI shift states */
  outshift = 0;

static char kbuf[10], *kbp;		/* Keyboard buffer & pointer */
static PID_T parent_id = (PID_T)0;	/* Process id of keyboard fork */

static char *ibp;			/* Input buffer pointer */
static int ibc = 0;			/* Input buffer count */
#ifdef pdp11
#define IBUFL 1536			/* Input buffer length */
#else
#define IBUFL 4096
#endif /* pdp11 */

static char *obp;			/* Output buffer pointer */
static int obc = 0;			/* Output buffer count */
#define OBUFL 1024			/* Output buffer length */

#define TMPLEN 200			/* Temporary message buffer length */
#ifdef DYNAMIC
static char *ibuf = NULL, *obuf = NULL, *temp = NULL; /* Buffers */
#else
static char ibuf[IBUFL], obuf[OBUFL], temp[TMPLEN];
#endif /* DYNAMIC */

/* SunLink X.25 items */

#ifdef SUNX25
static char *p;				/* General purpose pointer */
char x25ibuf[MAXIX25];			/* Input buffer */
char x25obuf[MAXOX25];			/* Output buffer */
int active = 0;				/* Lower fork active flag */
int ibufl;				/* Length of input buffer */
int obufl;				/* Length of output buffer */
unsigned char tosend = 0;
int linkid, lcn;
static int dox25clr = 0;
CHAR padparms[MAXPADPARMS+1];
static int padpipe[2];			/* Pipe descriptor to pass PAD parms */
#endif /* SUNX25 */

/* Character-set items */

#ifndef NOCSETS
#ifdef CK_ANSIC /* ANSI C prototypes... */
extern CHAR (*xls[MAXTCSETS+1][MAXFCSETS+1])(CHAR); /* Character set */
extern CHAR (*xlr[MAXTCSETS+1][MAXFCSETS+1])(CHAR); /* translation functions */
static CHAR (*sxo)(CHAR);	/* Local translation functions */
static CHAR (*rxo)(CHAR);	/* for output (sending) terminal chars */
static CHAR (*sxi)(CHAR);	/* and for input (receiving) terminal chars. */
static CHAR (*rxi)(CHAR);
#else /* Not ANSI C... */
extern CHAR (*xls[MAXTCSETS+1][MAXFCSETS+1])();	/* Character set */
extern CHAR (*xlr[MAXTCSETS+1][MAXFCSETS+1])();	/* translation functions. */
static CHAR (*sxo)();		/* Local translation functions */
static CHAR (*rxo)();		/* for output (sending) terminal chars */
static CHAR (*sxi)();		/* and for input (receiving) terminal chars. */
static CHAR (*rxi)();
#endif /* CK_ANSIC */
extern int language;		/* Current language. */
static int langsv;		/* For remembering language setting. */
extern struct csinfo fcsinfo[]; /* File character set info. */
extern int tcsr, tcsl;		/* Terminal character sets, remote & local. */
static int tcs;			/* Intermediate ("transfer") character set. */

#ifndef NOESCSEQ
/*
  As of edit 178, the CONNECT command will skip past ANSI escape sequences
  to avoid translating the characters within them.  This allows the CONNECT
  command to work correctly when connected to a remote host that uses a
  7-bit ISO 646 national character set, in which characters like '[' would
  normally be translated into accented characters, ruining the terminal's
  interpretation (and generation) of escape sequences.

  Escape sequences of non-ANSI/ISO-compliant terminals are not handled.
*/
#ifndef SKIPESC
#define SKIPESC
#endif /* SKIPESC */
/*
  States for the escape-sequence recognizer.
*/
#define ES_NORMAL 0			/* Normal, not in escape sequence */
#define ES_GOTESC 1			/* Current character is ESC */
#define ES_ESCSEQ 2			/* Inside an escape sequence */
#define ES_GOTCSI 3			/* Inside a control sequence */
#define ES_STRING 4			/* Inside DCS,OSC,PM, or APC string */
#define ES_TERMIN 5			/* 1st char of string terminator */

static int
  skipesc = 0,				/* Skip over ANSI escape sequences */
  inesc = ES_NORMAL;			/* State of sequence recognizer */
/*
  ANSI escape sequence handling.  Only the 7-bit form is treated, because
  translation is not a problem in the 8-bit environment, in which all GL
  characters are ASCII and no translation takes place.  So we don't check
  for the 8-bit single-character versions of CSI, DCS, OSC, APC, or ST.
  Here is the ANSI sequence recognizer state table, followed by the code
  that implements it.

  Definitions:
    CAN = Cancel                       01/08         Ctrl-X
    SUB = Substitute                   01/10         Ctrl-Z
    DCS = Device Control Sequence      01/11 05/00   ESC P
    CSI = Control Sequence Introducer  01/11 05/11   ESC [
    ST  = String Terminator            01/11 05/12   ESC \
    OSC = Operating System Command     01/11 05/13   ESC ]
    PM  = Privacy Message              01/11 05/14   ESC ^
    APC = Application Program Command  01/11 05/15   ESC _

  ANSI escape sequence recognizer:

    State    Input  New State  ; Commentary

    NORMAL   (start)           ; Start in NORMAL state

    (any)    CAN    NORMAL     ; ^X cancels
    (any)    SUB    NORMAL     ; ^Z cancels

    NORMAL   ESC    GOTESC     ; Begin escape sequence
    NORMAL   other             ; NORMAL control or graphic character

    GOTESC   ESC               ; Start again
    GOTESC   [      GOTCSI     ; CSI
    GOTESC   P      STRING     ; DCS introducer, consume through ST
    GOTESC   ]      STRING     ; OSC introducer, consume through ST
    GOTESC   ^      STRING     ; PM  introducer, consume through ST
    GOTESC   _      STRING     ; APC introducer, consume through ST
    GOTESC   0..~   NORMAL     ; 03/00 through 17/14 = Final character
    GOTESC   other  ESCSEQ     ; Intermediate or ignored control character

    ESCSEQ   ESC    GOTESC     ; Start again
    ESCSEQ   0..~   NORMAL     ; 03/00 through 17/14 = Final character
    ESCSEQ   other             ; Intermediate or ignored control character

    GOTCSI   ESC    GOTESC     ; Start again
    GOTCSI   @..~   NORMAL     ; 04/00 through 17/14 = Final character
    GOTCSI   other             ; Intermediate char or ignored control char

    STRING   ESC    TERMIN     ; Maybe have ST
    STRING   other             ; Consume all else

    TERMIN   \      NORMAL     ; End of string
    TERMIN   other  STRING     ; Still in string
*/
/*
  chkaes() -- Check ANSI Escape Sequence.
  Call with EACH character in input stream.
  Sets global inesc variable according to escape sequence state.
*/
VOID
#ifdef CK_ANSIC
chkaes(char c)
#else
chkaes(c) char c;
#endif /* CK_ANSIC */
/* chkaes */ {

    if (c == CAN || c == SUB)		/* CAN and SUB cancel any sequence */
      inesc = ES_NORMAL;
    else				/* Otherwise */
      switch (inesc) {			/* enter state switcher */

	case ES_NORMAL:			/* NORMAL state */
	  if (c == ESC)			/* Got an ESC */
	    inesc = ES_GOTESC;		/* Change state to GOTESC */
	  break;			/* Otherwise stay in NORMAL state */

	case ES_GOTESC:			/* GOTESC state */
	  if (c == '[')			/* Left bracket after ESC is CSI */
	    inesc = ES_GOTCSI;		/* Change to GOTCSI state */
	  else if (c > 057 && c < 0177)	/* Final character '0' thru '~' */
	    inesc = ES_NORMAL;		/* Back to normal */
	  else if (c == 'P' || (c > 0134 && c < 0140)) /* P, [, ^, or _ */
	    inesc = ES_STRING;		/* Switch to STRING-absorption state */
	  else if (c != ESC)		/* ESC in an escape sequence... */
	    inesc = ES_ESCSEQ;		/* starts a new escape sequence */
	  break;			/* Intermediate or ignored ctrl char */

	case ES_ESCSEQ:			/* ESCSEQ -- in an escape sequence */
	  if (c > 057 && c < 0177)	/* Final character '0' thru '~' */
	    inesc = ES_NORMAL;		/* Return to NORMAL state. */
	  else if (c == ESC)		/* ESC ... */
	    inesc = ES_GOTESC;		/* starts a new escape sequence */
	  break;			/* Intermediate or ignored ctrl char */

	case ES_GOTCSI:			/* GOTCSI -- In a control sequence */
	  if (c > 077 && c < 0177)	/* Final character '@' thru '~' */
	    inesc = ES_NORMAL;		/* Return to NORMAL. */
	  else if (c == ESC)		/* ESC ... */
	    inesc = ES_GOTESC;		/* starts over. */
	  break;			/* Intermediate or ignored ctrl char */

	case ES_STRING:			/* Inside a string */
	  if (c == ESC)			/* ESC may be 1st char of terminator */
	    inesc = ES_TERMIN;		/* Go see. */
	  break;			/* Absorb all other characters. */

	case ES_TERMIN:			/* May have a string terminator */
	  if (c == '\\')		/* which must be backslash */
	    inesc = ES_NORMAL;		/* If so, back to NORMAL */
	  else				/* Otherwise */
	    inesc = ES_STRING;		/* Back to string absorption. */
      }
}
#endif /* NOESCSEQ */
#endif /* NOCSETS */

/* Connect state parent/child communication signal handlers */

static jmp_buf con_env;		 /* Environment pointer for connect errors */
/*
  Note: Some C compilers (e.g. Cray UNICOS) interpret the ANSI C standard
  about setjmp() in a way that disallows constructions like:

    if ((var = setjmp(env)) == 0) ...

  which prevents the value returned by longjmp() from being used at all.
  So the following handlers set a global variable instead.
*/
static
SIGTYP
conn_int(foo) int foo; {		/* Modem read failure handler, */
    signal(SIGUSR1,SIG_IGN);		/* Disarm the interrupt */
    sjval = 1;				/* Set global variable */
    longjmp(con_env,sjval);		/* Notifies parent process to stop */
}

static
SIGTYP
mode_chg(foo) int foo; {
    signal(SIGUSR2,mode_chg);		/* Re-arm the signal immediately. */

#ifdef SUNX25				/* X.25 read new params from pipe */
    if (nettype == NET_SX25) {
        read(padpipe[0],padparms,MAXPADPARMS);
        debug(F100,"pad_chg","",0);
/*
  NOTE: we can probably skip this longjmp, just as we do in the "else" case.
  But I don't (yet) have any way to test this.
*/
	sjval = 2;			/* Set global variable. */
	longjmp(con_env,sjval);
	debug(F100,"mode_chg X.25","",0);
    } else {
#endif /* SUNX25 */
	duplex = 1 - duplex;		/* Toggle duplex mode. */
	debug(F101,"mode_chg duplex","",duplex);
#ifdef SUNX25
    }
#endif /* SUNX25 */
}

/*  C K C P U T C  --  C-Kermit CONNECT Put Character to Screen  */
/*
  Output is buffered to avoid slow screen writes on fast connections.
*/
int
ckcputf() {				/* Dump the output buffer */
    int x = 0;
    if (obc > 0)			/* If we have any characters, */
      x = conxo(obc,obuf);		/* dump them, */
    obp = obuf;				/* reset the pointer */
    obc = 0;				/* and the counter. */
    return(x);				/* Return conxo's return code */
}

int
ckcputc(c) int c; {
    int x;

    *obp++ = c & 0xff;			/* Deposit the character */
    obc++;				/* Count it */
    if (ibc == 0 ||			/* If input buffer about empty */
	obc == OBUFL) {			/* or output buffer full */
	debug(F101,"CKCPUTC obc","",obc);
	x = conxo(obc,obuf);		/* dump the buffer, */
	obp = obuf;			/* reset the pointer */
	obc = 0;			/* and the counter. */
	return(x);			/* Return conxo's return code */
    } else return(0);
}

/*  C K C G E T C  --  C-Kermit CONNECT Get Character  */
/*
  Buffered read from communication device.
  Returns the next character, refilling the buffer if necessary.
  On error, returns ttinc's return code (see ttinc() description).
  Dummy argument for compatible calling conventions with ttinc().
*/
int
ckcgetc(dummy) int dummy; {
    int c, n;

    debug(F101,"CKCGETC 1 ibc","",ibc); /* Log */
    if (ibc < 1) {			/* Need to refill buffer? */
	ibc = 0;			/* Yes, reset count */
	ibp = ibuf;			/* and buffer pointer */
	debug(F100,"CKCGETC 1 calling ttinc(0)","",0); /* Log */
	c = ttinc(0);			/* Read one character, blocking */
	debug(F101,"CKCGETC 1 ttinc(0)","",c); /* Log */
	if (c < 0) {			/* If error, return error code */
	    return(c);
	} else {			/* Otherwise, got one character */
	    *ibp++ = c;			/* Advance buffer pointer */
	    ibc++;			/* and count. */
	}

	/* Now quickly read any more that might have arrived */

	if ((n = ttchk()) > 0) {	/* Any more waiting? */
	    if (n > (IBUFL - ibc))	/* Get them all at once. */
	      n = IBUFL - ibc;		/* Don't overflow buffer */
	    if ((n = ttxin(n,(CHAR *)ibp)) > 0) {
		ibp += n;		/* Advance pointer */
		ibc += n;		/* and counter */
	    } else return(-1);
	}
	debug(F101,"CKCGETC 2 ibc","",ibc); /* Log how many */
	ibp = ibuf;
    }
    c = *ibp++ & 0xff;			/* Get next character from buffer */
    ibc--;				/* Reduce buffer count */
    return(c);				/* Return the character */
}

/*  C O N E C T  --  Perform terminal connection  */

int
conect() {
    PID_T pid;			/* Process id of child (modem reader) */
    int	n;			/* General purpose counter */

    int c;			/* c is a character, but must be signed 
				   integer to pass thru -1, which is the
				   modem disconnection signal, and is
				   different from the character 0377 */
    int c2;			/* A copy of c */
    int csave;			/* Another copy of c */
    int tx;			/* tn_doop() return code */
#ifdef SUNX25
    int i;			/* Worker for X.25 code*/
#endif /* SUNX25 */

#ifdef DYNAMIC
    if (!(ibuf = malloc(IBUFL+1))) {    /* Allocate input line buffer */
	printf("Sorry, CONNECT input buffer can't be allocated\n");
	return(0);
    }
    if (!(obuf = malloc(OBUFL+1))) {    /* Allocate input line buffer */
	printf("Sorry, CONNECT output buffer can't be allocated\n");
	free(ibuf);
	return(0);
    }
    if (!(temp = malloc(TMPLEN+1))) {    /* Allocate temporary buffer */
	printf("Sorry, CONNECT temporary buffer can't be allocated\n");
	free(obuf);
	return(0);
    }
#endif /* DYNAMIC */

    if (!local) {
#ifdef NETCONN
	printf("Sorry, you must SET LINE or SET HOST first\n");
#else
	printf("Sorry, you must SET LINE first\n");
#endif /* NETCONN */
	return(0);
    }
    if (speed < 0L && network == 0) {
	printf("Sorry, you must SET SPEED first\n");
	return(0);
    }
#ifdef TCPSOCKET
    if (network && (nettype != NET_TCPB)
#ifdef SUNX25
        && (nettype != NET_SX25)
#endif /* SUNX25 */
    ) {
	printf("Sorry, network type not supported\n");
	return(0);
    }
#endif /* TCPSOCKET */

    if (ttyfd < 0) {			/* If communication device not open */
	debug(F111,"ckucon opening",ttname,0); /* Open it now */
	if (ttopen(ttname,
		   &local,
		   network ? -nettype : mdmtyp,
		   0
		   ) < 0) {
	    sprintf(temp,"Sorry, can't open %s",ttname);
	    perror(temp);
	    debug(F110,"ckucon open failure",temp,0);
	    return(0);
	}
    }
    dohangup = 0;			/* Hangup not requested yet */
#ifdef SUNX25
    dox25clr = 0;			/* X.25 clear not requested yet */
#endif /* SUNX25 */

    if (!quiet) {
#ifdef NETCONN
	if (network) {
	    printf("Connecting to host %s",ttname);
#ifdef SUNX25
	    if (nettype == NET_SX25)
	      printf(", Link ID %d, LCN %d",linkid,lcn);
#endif /* SUNX25 */
	} else {
#endif /* NETCONN */
	    printf("Connecting to %s",ttname);
	    if (speed > -1L) printf(", speed %ld",speed);
#ifdef NETCONN
	}
#endif /* NETCONN */
	printf(".\r\nThe escape character is Ctrl-%c (ASCII %d, %s)\r\n",
	       ctl(escape), escape, (escape == 127 ? "DEL" : ccntab[escape]));
	printf("Type the escape character followed by C to get back,\r\n");
	printf("or followed by ? to see other options.\r\n");
	if (seslog) {
	    printf("(Session logged to %s, ",sesfil);
	    printf("%s)\r\n", sessft ? "binary" : "text");
	}
	if (debses) printf("Debugging Display...)\r\n");
	fflush(stdout);
    }

/* Condition console terminal and communication line */	    

    if (conbin((char)escape) < 0) {
	printf("Sorry, can't condition console terminal\n");
	return(0);
    }
    debug(F101,"connect cmask","",cmask);
    debug(F101,"connect cmdmsk","",cmdmsk);
    goterr = 0;
    if ((n = ttvt(speed,flow)) < 0) {	/* Enter "virtual terminal" mode */
	debug(F101,"CONNECT ttvt","",n);
	goterr = 1;			/* Error recovery... */
	tthang();			/* Hang up and close the device. */
	ttclos(0);
	if (ttopen(ttname,		/* Open it again... */
		   &local,
		   network ? -nettype : mdmtyp,
		   0
		   ) < 0) {
	    sprintf(temp,"Sorry, can't reopen %s",ttname);
	    perror(temp);
	    return(0);
	}
	if (ttvt(speed,flow) < 0) {	/* Try virtual terminal mode again. */
	    conres();			/* Failure this time is fatal. */
	    printf("Sorry, Can't condition communication line\n");
	    return(0);
	}
#ifdef NETCONN
	if (network && ttnproto == NP_TELNET)
	  tn_ini();			/* Just in case ttopen didn't... */
#endif /* NETCONN */
    }
    debug(F101,"connect ttvt ok, escape","",escape);

#ifndef NOCSETS
/* Set up character set translations */

    tcs = gettcs(tcsr,tcsl);		/* Get intermediate set. */

    if (tcsr == tcsl) {			/* Remote and local sets the same? */
	sxo = rxo = NULL;		/* If so, no translation. */
	sxi = rxi = NULL;
    } else {				/* Otherwise, set up */
	sxo = xls[tcs][tcsl];		/* translation function */
	rxo = xlr[tcs][tcsr];		/* pointers for output functions */
	sxi = xls[tcs][tcsr];		/* and for input functions. */
	rxi = xlr[tcs][tcsl];
    }
/*
  This is to prevent use of zmstuff() and zdstuff() by translation functions.
  They only work with disk i/o, not with communication i/o.  Luckily Russian
  translation functions don't do any stuffing...
*/
    langsv = language;
#ifndef NOCYRIL
    if (language != L_RUSSIAN)
#endif /* NOCYRIL */
      language = L_USASCII;

#ifdef SKIPESC
/*
  We need to activate the "skip escape sequence" feature when:
  (a) translation is elected, and
  (b) the local and/or remote set is a 7-bit set other than US ASCII.
*/
    skipesc = (tcs != TC_TRANSP) &&	/* Not transparent */
      (fcsinfo[tcsl].size == 128 || fcsinfo[tcsr].size == 128) && /* 7 bits */
	(fcsinfo[tcsl].code != FC_USASCII); /* But not ASCII */
    inesc = ES_NORMAL;			/* Initial state of recognizer */
#ifdef COMMENT
    debug(F101,"tcs","",tcs);
    debug(F101,"tcsl","",tcsl);
    debug(F101,"tcsr","",tcsr);
    debug(F101,"fcsinfo[tcsl].size","",fcsinfo[tcsl].size);
    debug(F101,"fcsinfo[tcsr].size","",fcsinfo[tcsr].size);
#endif /* COMMENT */
    debug(F101,"skipesc","",skipesc);
#endif /* SKIPESC */
#endif /* NOCSETS */

/*
  This is a label we jump back to when the lower fork sensed the need
  to change modes.  As of 5A(178), this is used only by X.25 code
  (perhaps unnecessarily? -- The X.25 code needs a lot of testing and
  cleaning up...)
*/
newfork:
    debug(F100,"CONNECT newfork","",0);
    parent_id = getpid();		/* Get parent id for signalling */
    signal(SIGUSR1,SIG_IGN);		/* Don't kill myself */
#ifdef SUNX25
    pipe(padpipe);                      /* Create pipe to pass PAD parms */
#endif /* SUNX25 */
    pid = fork();			/* All ok, make a fork */
    if (pid == (PID_T) -1) {		/* Can't create it. */
	conres();			/* Reset the console. */
	perror("Can't create keyboard fork");
	if (!quiet) {
	printf("\r\nCommunications disconnect (Back at %s)\r\n",
	       *myhost ?
	       myhost :
#ifdef UNIX
	       "local UNIX system"
#else
	       "local system"
#endif /* UNIX */
	       );
	}
	printf("\n");
	what = W_NOTHING;		/* So console modes are set right. */
#ifndef NOCSETS
	language = langsv;		/* Restore language */
#endif /* NOCSETS */
	parent_id = (PID_T) 0;		/* Clean up */
#ifdef DYNAMIC
	if (temp) free(temp);		/* Free allocated memory */
	if (ibuf) free(ibuf);
	if (obuf) free(obuf);
#endif /* DYNAMIC */
	return(1);
    }
    if (pid) {				/* Top fork reads, sends keystrokes */
	what = W_CONNECT;		/* Keep track of what we're doing */
	active = 1;
	debug(F101,"CONNECT keyboard fork duplex","",duplex);

	/* Catch communication errors or mode changes in lower fork */

	if (setjmp(con_env) == 0) {	/* Normal entry... */
	    sjval = 0;			/* Initialize setjmp return code. */
	    signal(SIGUSR1,conn_int);	/* Routine for child process exit. */
	    signal(SIGUSR2,mode_chg);	/* Routine for mode change. */
#ifdef SUNX25
	    if (network && nettype == NET_SX25) {
		obufl = 0;
		bzero (x25obuf,sizeof(x25obuf)) ;
	    }
#endif /* SUNX25 */

/*
  Here is the big loop that gets characters from the keyboard and sends them
  out the communication device.  There are two components to the communication
  path: the connection from the keyboard to C-Kermit, and from C-Kermit to
  the remote computer.  The treatment of the 8th bit of keyboard characters 
  is governed by SET COMMAND BYTESIZE (cmdmsk).  The treatment of the 8th bit
  of characters sent to the remote is governed by SET TERMINAL BYTESIZE
  (cmask).   This distinction was introduced in edit 5A(164).
*/
	    while (active) {
#ifndef NOSETKEY
		if (kmptr) {		/* Have current macro? */
		    debug(F100,"kmptr non NULL","",0);
		    if ((c = (CHAR) *kmptr++) == NUL) { /* Get char from it */
			kmptr = NULL;	/* If no more chars,  */
			debug(F100,"macro empty, continuing","",0);
			continue;	/* reset pointer and continue */
		    }
		    debug(F000,"char from macro","",c);
		} else			/* No macro... */
#endif /* NOSETKEY */
		  c = congks(0);	/* Read from keyboard */

		debug(F111,"** KEYB",dbchr(c),c);

                if (c == -1) {		/* If read() got an error... */
#ifdef COMMENT
/*
 This seems to cause problems.  If read() returns -1, the signal has already
 been delivered, and nothing will wake up the pause().
*/
		    pause();		/* Wait for transmitter to finish. */
#else
#ifdef A986
/*
  On Altos machines with Xenix 3.0, pressing DEL in connect mode brings us
  here (reason unknown).  The console line discipline at this point has
  intr = ^C.  The communications tty has intr = DEL but we get here after
  pressing DEL on the keyboard, even when the remote system has been set not
  to echo.  With A986 defined, we stay in the read loop and beep only if the
  offending character is not DEL.
*/
		    if ((c & 127) != 127) conoc(BEL);
#else
		    conoc(BEL);		/* Beep */
		    active = 0;		/* and terminate the read loop */
		    continue;
#endif /* A986 */
#endif /* COMMENT */
		}
		c &= cmdmsk;		/* Do any requested masking */
#ifndef NOSETKEY
/*
  Note: kmptr is NULL if we got character c from the keyboard, and it is
  not NULL if it came from a macro.  In the latter case, we must avoid
  expanding it again.
*/
		if (!kmptr && macrotab[c]) { /* Macro definition for c? */
		    kmptr = macrotab[c];     /* Yes, set up macro pointer */
		    continue;		     /* and restart the loop, */
		} else c = keymap[c];	     /* else use single-char keymap */
#endif /* NOSETKEY */
		if (
#ifndef NOSETKEY
		    !kmptr &&
#endif /* NOSETKEY */
		    ((c & 0x7f) == escape)) { /* Escape character? */
		    debug(F000,"connect got escape","",c);
		    c = congks(0) & 0177; /* Got esc, get its arg */
		    /* No key mapping here */
		    doesc((char) c);	/* Now process it */

		} else {		/* It's not the escape character */
		    csave = c;		/* Save it before translation */
					/* for local echoing. */
#ifndef NOCSETS
#ifndef SKIPESC
		    /* Translate character sets */
		    if (sxo) c = (*sxo)(c); /* From local to intermediate. */
		    if (rxo) c = (*rxo)(c); /* From intermediate to remote. */
#else
		    if (inesc == ES_NORMAL) { /* If not inside escape seq.. */
			/* Translate character sets */
			if (sxo) c = (*sxo)((char)c); /* Local-intermediate */
			if (rxo) c = (*rxo)((char)c); /* Intermediate-remote */
		    }
		    if (skipesc) chkaes((char)c); /* Check escape seq status */
#endif /* SKIPESC */
#endif /* NOCSETS */
/*
 If Shift-In/Shift-Out is selected and we have a 7-bit connection,
 handle shifting here.
*/
		    if (sosi) {		     /* Shift-In/Out selected? */
			if (cmask == 0177) { /* In 7-bit environment? */
			    if (c & 0200) {          /* 8-bit character? */
				if (outshift == 0) { /* If not shifted, */
				    ttoc(dopar(SO)); /* shift. */
				    outshift = 1;
				}
			    } else {
				if (outshift == 1) { /* 7-bit character */
				    ttoc(dopar(SI)); /* If shifted, */
				    outshift = 0;    /* unshift. */
				}
			    }
			}
			if (c == SO) outshift = 1;   /* User typed SO */
			if (c == SI) outshift = 0;   /* User typed SI */
		    }
		    c &= cmask;		/* Apply Kermit-to-host mask now. */
#ifdef SUNX25
                    if (network && nettype == NET_SX25) {
                        if (padparms[PAD_ECHO]) {
                            if (debses)
			      conol(dbchr(c)) ;
                            else
			      if ((c != padparms[PAD_CHAR_DELETE_CHAR])   &&
				  (c != padparms[PAD_BUFFER_DELETE_CHAR]) &&
				  (c != padparms[PAD_BUFFER_DISPLAY_CHAR]))
                                conoc(c) ;
                            if (seslog) logchar(c);
                        }
			if (c == CR && (padparms[PAD_LF_AFTER_CR] == 4 ||
					padparms[PAD_LF_AFTER_CR] == 5)) {
                            if (debses)
			      conol(dbchr(LF)) ;
                            else
			      conoc(LF) ;
                            if (seslog) logchar(LF);
                        }
                        if (c == padparms[PAD_BREAK_CHARACTER])
			  breakact();
                        else if (padparms[PAD_DATA_FORWARD_TIMEOUT]) {
                            tosend = 1;
                            x25obuf [obufl++] = c;
                        } else if (((c == padparms[PAD_CHAR_DELETE_CHAR])  ||
				    (c == padparms[PAD_BUFFER_DELETE_CHAR]) ||
				    (c == padparms[PAD_BUFFER_DISPLAY_CHAR])) 
				   && (padparms[PAD_EDITING]))
			  if (c == padparms[PAD_CHAR_DELETE_CHAR])
			    if (obufl > 0) {
				conol("\b \b"); obufl--;
			    } else {}
			  else if (c == padparms[PAD_BUFFER_DELETE_CHAR]) {
			      conol ("\r\nPAD Buffer Deleted\r\n");
			      obufl = 0;
			  }
			  else if (c == padparms[PAD_BUFFER_DISPLAY_CHAR]) {
			      conol("\r\n");
			      conol(x25obuf);
			      conol("\r\n");
			  } else {} 
                        else {
                            x25obuf [obufl++] = c;
                            if (obufl == MAXOX25) tosend = 1;
                            else if (c == CR) tosend = 1;
                        }
                        if (tosend) 
			  if (ttol(x25obuf,obufl) < 0) {
			      perror ("\r\nCan't send characters");
			      active = 0;
			  } else {
			      bzero (x25obuf,sizeof(x25obuf));
			      obufl = 0;
			      tosend = 0;
			  } else {};
                    } else {
#endif /* SUNX25 */ 

/* If we have a CR, handle CR/CRLF mapping... */

		    if (c == '\015') {
			if (tnlm	/* TERMINAL NEWLINE ON */
#ifdef TNCODE				/* And for TELNET... */
			|| (network && ttnproto == NP_TELNET)
#endif /* TNCODE */
			) {
			    ttoc(dopar('\015'));	/* Send CR */
			    if (duplex) conoc('\015');	/* Maybe echo CR */
#ifdef TNCODE
			    if (network && !tn_nlm && ttnproto == NP_TELNET)
			      c = '\0';		/* Stuff NUL */
			    else
#endif /* TNCODE */
			      c = '\012';	/* Stuff LF */
			    csave = c;
			}
		    }			/* Now process the LF or NUL... */
#ifdef TNCODE
/* If user types the 0xff character (TELNET IAC), it must be doubled. */
		    else
		      if (c == IAC && network && ttnproto == NP_TELNET) {
					/* Send one copy now */
			ttoc((char)IAC); /* and the other one just below. */
		    }
#endif /* TNCODE */

		    /* Send the character */

		    if (ttoc((char)dopar((CHAR) c)) > -1) {
		    	if (duplex) {	/* If half duplex, must echo */
			    if (debses)
			      conol(dbchr(csave)); /* the original char */
			    else	           /* not the translated one */
			      conoc((char)csave);
			    if (seslog) { /* And maybe log it too */
				c2 = csave;
				if (sessft == 0 && csave == '\r')
				  c2 = '\n';
				logchar((char)c2);
			    }
			}
    	    	    } else {
			perror("\r\nCan't send character");
			active = 0;
		    }
#ifdef SUNX25
		} 
#endif /* SUNX25 */
		}
	    }
	}				/* Come here on death of child */
	debug(F100,"CONNECT killing port fork","",0);
	kill(pid,9);			/* Done, kill inferior fork. */
	wait((WAIT_T *)0);		/* Wait till gone. */
	if (sjval == 1) {		/* Read error on comm device */
	    dohangup = 1;
#ifdef NETCONN
	    if (network) {
		ttclos(0);
#ifdef SUNX25
		if (nettype == NET_SX25) initpad();
#endif /* SUNX25 */
	    }
#endif /* NETCONN */
	}
	if (sjval == 2)			/* If it was a mode change, go back */
	  goto newfork;			/* and coordinate with other fork. */
	conres();			/* Reset the console. */
	if (quitnow) doexit(GOOD_EXIT,xitsta); /* Exit now if requested. */
	if (dohangup > 0) {		/* If hangup requested, do that. */
#ifndef NODIAL
	    if (dohangup > 1)		/* User asked for it */
	      if (mdmhup() < 1)		/* Maybe hang up via modem */
#endif /* NODIAL */
	      tthang();			/* And make sure we don't hang up */
	    dohangup = 0;		/* again unless requested again. */
	}
#ifdef SUNX25
	if (dox25clr) {			/* If X.25 clear requested */
	    x25clear();			/* do that. */
	    initpad();
	    dox25clr = 0;		/* But only once. */
	}
#endif /* SUNX25 */
	if (!quiet)
	  printf("(Back at %s)", *myhost ? myhost : "local UNIX system");
	printf("\n");
	what = W_NOTHING;		/* So console modes set right. */
#ifndef NOCSETS
	language = langsv;		/* Restore language */
#endif /* NOCSETS */
	parent_id = (PID_T) 0;
#ifdef DYNAMIC
	if (temp) free(temp);		/* Free allocated memory */
	if (ibuf) free(ibuf);
	if (obuf) free(obuf);
#endif /* DYNAMIC */
	return(1);

    } else {				/* Inferior reads, prints port input */

#ifndef OXOS
	if (priv_can()) {		/* Cancel all privs */
	    printf("?setuid error - fatal\n");
	    doexit(BAD_EXIT,-1);
	}
#endif /* OXOS */
	signal(SIGINT, SIG_IGN);	/* In case these haven't been */
	signal(SIGQUIT, SIG_IGN);	/* inherited from above... */

	inshift = outshift = 0;		/* Initial SO/SI shift state. */
	sleep(1);			/* Wait for parent's handler setup.  */
	ibp = ibuf;			/* Initialize input buffering */
	ibc = 0;			/* And output buffering. */
	obp = obuf;
	obc = 0;
	debug(F100,"CONNECT starting port fork","",0);
	while (1) {			/* Fresh read, wait for a character. */
#ifdef SUNX25
	    if (network && (nettype == NET_SX25)) {
		bzero(x25ibuf,sizeof(x25ibuf)) ;
		if ((ibufl = ttxin(MAXIX25,x25ibuf)) < 0) {
		    if (ibufl == -2) {  /* PAD parms changes */
			write(padpipe[1],padparms,MAXPADPARMS);
			kill(parent_id,SIGUSR2);
		    } else {
			if (!quiet)
			  printf("\r\nCommunications disconnect ");
			kill(parent_id,SIGUSR1);
		    }
		    pause();
		}
		if (debses) {		/* Debugging output */
		    p = x25ibuf ;
                        while (ibufl--) { c = *p++; conol(dbchr(c)); }
		} else {
		    if (sosi
#ifndef NOCSETS
			|| tcsl != tcsr
#endif /* NOCSETS */
			) { /* Character at a time */
			for (i = 1; i < ibufl; i++) {
			    c = x25ibuf[i] & cmask;
			    if (sosi) { /* Handle SI/SO */
				if (c == SO) {
				    inshift = 1;
				    continue;
				} else if (c == SI) {
				    inshift = 0;
				    continue;
				}
				if (inshift)
				  c |= 0200;
			    }
#ifndef NOCSETS
#ifndef SKIPESC
		    /* Translate character sets */
		    if (sxo) c = (*sxo)(c); /* From local to intermediate. */
		    if (rxo) c = (*rxo)(c); /* From intermediate to remote. */

#else /* Skipping escape sequences... */

		    if (inesc == ES_NORMAL) { /* If not inside escape seq.. */
			/* Translate character sets */
			if (sxo) c = (*sxo)(c); /* Local to intermediate. */
			if (rxo) c = (*rxo)(c); /* Intermediate to remote. */
		    }
		    if (skipesc) chkaes(c); /* Check escape sequence status */
#endif /* SKIPESC */
#endif /* NOCSETS */
			    c &= cmdmsk; /* Apply command mask. */
			    conoc(c);    /* Output to screen */
			    logchar(c);  /* and session log */
			}
		    } else {		 /* All at once */
			for (i = 1; i < ibufl; i++)
			  x25ibuf[i] &= (cmask & cmdmsk);
			conxo(ibufl,x25ibuf);
			if (seslog) zsoutx(ZSFILE,x25ibuf,ibufl);
		    }
		}
		continue;

	    } else {			/* Not X.25... */
#endif /* SUNX25 */
/*
  Get the next communication line character from our internal buffer.
  If the buffer is empty, refill it.
*/
		c = ckcgetc(0);		/* Get next character */
		/* debug(F101,"CONNECT c","",c); */
		if (c < 0) {
		    if (!quiet) {	/* Failed... */
			printf("\r\nCommunications disconnect ");
			if ( c == -3
#ifdef UTEK
/* This happens on UTEK if there's no carrier */
			    && errno != EWOULDBLOCK
#endif /* UTEK */
			    )
			  perror("\r\nCan't read character");
		    }
		    tthang();		/* Hang up the connection */
		    kill(parent_id,SIGUSR1); /* Notify parent */
		    for (;;) pause();	/* and wait to be killed */
		}
		debug(F111,"** PORT",dbchr(c),c);
#ifdef TNCODE
		/* Handle TELNET negotiations here */	
		if (c == IAC && network && ttnproto == NP_TELNET) {
		    ckcputf();		/* Dump output buffer */
		    if ((tx = tn_doop((CHAR)(c & 0xff),duplex,ckcgetc)) == 0) {
			continue;
		    } else if (tx == -1) { /* I/O error */
			if (!quiet)
			  printf("\r\nCommunications disconnect ");
			kill(parent_id,SIGUSR1); /* Notify parent. */
			for (;;) pause(); /* Wait to be killed. */
		    } else if ((tx == 1) && (!duplex)) { /* ECHO change */
			kill(parent_id,SIGUSR2); /* Tell the parent fork */
			duplex = 1;
		    } else if ((tx == 2) && (duplex)) { /* ECHO change */
			kill(parent_id,SIGUSR2); 
			duplex = 0;
		    } else if (tx == 3) { /* Quoted IAC */
			c = 255;
		    } else continue;	/* Negotiation OK, get next char. */
		}
#endif /* TNCODE */
		if (debses) {		/* Output character to screen */
		    char *s;		/* Debugging display... */
		    s = dbchr(c);
		    while (*s)
		      ckcputc(*s++);
		} else {		/* Regular display ... */
		    c &= cmask;		/* Apply Kermit-to-remote mask */
		    if (sosi) {		/* Handle SI/SO */
			if (c == SO) {	/* Shift Out */
			    inshift = 1;
			    continue;
			} else if (c == SI) { /* Shift In */
			    inshift = 0;
			    continue;
			}
			if (inshift) c |= 0200; 
		    }
#ifndef NOCSETS
#ifndef SKIPESC
		    /* Translate character sets */
		    if (sxi) c = (*sxi)((CHAR)c);
		    if (rxi) c = (*rxi)((CHAR)c);
#else
		    if (inesc == ES_NORMAL) {
			/* Translate character sets */
			if (sxi) c = (*sxi)((CHAR)c);
			if (rxi) c = (*rxi)((CHAR)c);
		    }
		    /* Adjust escape sequence status */
		    if (skipesc) chkaes((char)c);
#endif /* SKIPESC */
#endif /* NOCSETS */
		    c &= cmdmsk;	/* Apply command mask. */
		    if (c == CR && tt_crd) { /* SET TERM CR-DISPLAY CRLF ? */
			ckcputc(c);	     /* Yes, output CR */
			if (seslog) logchar((char)c);
			c = LF;		     /* and insert a linefeed */
		    }
		    ckcputc(c);		/* Output character to screen */
		    if (seslog) logchar((char)c); /* Do session log */
		}
#ifdef SUNX25
	    }   
#endif /* SUNX25 */	
	}
    }
}


/*  H C O N N E  --  Give help message for connect.  */

int
hconne() {
    int c;
    static char *hlpmsg[] = {
"\r\n  ? for this message",
"\r\n  0 (zero) to send a null",
"\r\n  B to send a BREAK",
#ifdef CK_LBRK
"\r\n  L to send a Long BREAK",
#endif /* CK_LBRK */
#ifdef NETCONN
"\r\n  I to send a network interrupt packet",
#ifdef TCPSOCKET
"\r\n  A to send Are You There?",
#endif /* TCPSOCKET */
#ifdef SUNX25
"\r\n  R to reset X.25 virtual circuit",
#endif /* SUNX25 */
#endif /* NETCONN */
"\r\n  H to hangup and close the connection",
"\r\n  Q to hangup and quit Kermit",
"\r\n  S for status",
"\r\n  ! to push to local shell",
"\r\n  Z to suspend",
"\r\n  \\ backslash code:",
"\r\n    \\nnn  decimal character code",
"\r\n    \\Onnn octal character code",
"\r\n    \\Xhh  hexadecimal character code",
"\r\n    terminate with carriage return.",
"\r\n Type the escape character again to send the escape character, or",
"\r\n press the space-bar to resume the CONNECT command.\r\n\r\n",
"" };
    conol("\r\nPress C to return to ");
    conol(*myhost ? myhost : "the C-Kermit prompt");
    conol(", or:");
    conola(hlpmsg);			/* Print the help message. */
    conol("Command>");			/* Prompt for command. */
    c = congks(0) & 0177;		/* Get character, strip any parity. */
    /* No key mapping or translation here */
    if (c != CMDQ)
      conoll("");
    return(c);				/* Return it. */
}


/*  D O E S C  --  Process an escape character argument  */

VOID
#ifdef CK_ANSIC
doesc(char c)
#else
doesc(c) char c;
#endif /* CK_ANSIC */
/* doesc */ {
    CHAR d;
  
    while (1) {
	if (c == escape) {		/* Send escape character */
	    d = dopar((CHAR) c); ttoc((char) d); return;
    	} else				/* Or else look it up below. */
	    if (isupper(c)) c = tolower(c);

	switch(c) {

	case 'c':			/* Escape back to prompt */
	case '\03':
	    active = 0; conol("\r\n"); return;

	case 'b':			/* Send a BREAK signal */
	case '\02':
	    ttsndb(); return;

#ifdef NETCONN
	case 'i':			/* Send Interrupt */
	case '\011':
#ifdef TCPSOCKET
#ifndef IP
#define IP 244
#endif /* IP */
	    if (network && ttnproto == NP_TELNET) { /* TELNET */
		temp[0] = IAC;		/* I Am a Command */
		temp[1] = IP;		/* Interrupt Process */
		temp[2] = NUL;
		ttol((CHAR *)temp,2);
	    } else 
#endif /* TCPSOCKET */
#ifdef SUNX25
            if (network && (nettype == NET_SX25)) { /* X.25 */
		(VOID) x25intr(0);	            /* X.25 interrupt packet */
		conol("\r\n");
	    } else
#endif /* SUNX25 */
	      conoc(BEL);
	    return;

#ifdef TCPSOCKET
	case 'a':			/* "Are You There?" */
	case '\01':
#ifndef AYT
#define AYT 246
#endif /* AYT */
	    if (network && ttnproto == NP_TELNET) {
		temp[0] = IAC;		/* I Am a Command */
		temp[1] = AYT;		/* Are You There? */
		temp[2] = NUL;
		ttol((CHAR *)temp,2);
	    } else conoc(BEL);
	    return;
#endif /* TCPSOCKET */
#endif /* NETCONN */

#ifdef CK_LBRK
	case 'l':			/* Send a Long BREAK signal */
	    ttsndlb(); return;
#endif /* CK_LBRK */

	case 'h':			/* Hangup */
	case '\010':
#ifdef SUNX25
            if (network && (nettype == NET_SX25)) dox25clr = 1;
            else
#endif /* SUNX25 */
	    dohangup = 2; active = 0; conol("\r\nHanging up "); return;

#ifdef SUNX25
        case 'r':                       /* Reset the X.25 virtual circuit */
        case '\022':
            if (network && (nettype == NET_SX25)) (VOID) x25reset();
            conol("\r\n"); return;
#endif /* SUNX25 */
 
	case 'q':			/* Quit */
	    quitnow = 1; active = 0; conol("\r\n"); return;

	case 's':			/* Status */
	    sprintf(temp,
		    "\r\nConnected %s %s", network ? "to" : "through", ttname);
	    conol(temp);
#ifdef SUNX25
            if (network && (nettype == NET_SX25)) {
                sprintf(temp,", Link ID %d, LCN %d",linkid,lcn); conol(temp);
	    }
#endif /* SUNX25 */
	    if (speed >= 0L) {
		sprintf(temp,", speed %ld", speed);
		conoll(temp);
	    } else conoll("");
	    sprintf(temp,
		    "Terminal bytesize: %d, Command bytesize: %d, Parity: ",
		    (cmask  == 0177) ? 7 : 8,
		    (cmdmsk == 0177) ? 7 : 8 );
	    conol(temp);

	    switch (parity) {
	      case  0:  conoll("none");  break;
	      case 'e': conoll("even");  break;
	      case 'o': conoll("odd");   break;
	      case 's': conoll("space"); break;
	      case 'm': conoll("mark");  break;
	    }
	    sprintf(temp,"Terminal echo: %s", duplex ? "local" : "remote");
	    conoll(temp);
	    if (seslog) {
		conol("Logging to: "); conoll(sesfil);
            }
	    if (!network) shomdm();
	    return;

	case '?':			/* Help */
	    c = hconne(); continue;

	case '0':			/* Send a null */
	    c = '\0'; d = dopar((CHAR) c); ttoc((char) d); return;

#ifndef NOPUSH
	case 'z': case '\032':		/* Suspend */
	    stptrap(0); return;

	case '@':			/* Start inferior command processor */
	case '!':
	    conres();			/* Put console back to normal */
	    zshcmd("");			/* Fork a shell. */
	    if (conbin((char)escape) < 0) {
		printf("Error resuming CONNECT session\n");
		active = 0;
	    }
	    return;
#endif /* NOPUSH */

	case SP:			/* Space, ignore */
	    return;

	default:			/* Other */
	    if (c == CMDQ) {		/* Backslash escape */
		int x;
		kbp = kbuf;
		*kbp++ = c;
		while (((c = (congks(0) & cmdmsk)) != '\r') && (c != '\n'))
		  *kbp++ = c;
		*kbp = NUL; kbp = kbuf;
		x = xxesc(&kbp);	/* Interpret it */
		if (x >= 0) {		/* No key mapping here */
		    c = dopar((CHAR) x);
		    ttoc((char) c);
		    return;
		} else {		/* Invalid backslash code. */
		    conoc(BEL);
		    return;
		}
	    }
	    conoc(BEL); return; 	/* Invalid esc arg, beep */
    	}
    }
}

VOID
#ifdef CK_ANSIC
logchar(char c)
#else
logchar(c) char c;
#endif /* CK_ANSIC */
/* logchar */ {			/* Log character c to session log */
    if (seslog) 
      if ((sessft != 0) ||
	  (c != '\r' &&
	   c != '\0' &&
	   c != XON &&
	   c != XOFF))
	if (zchout(ZSFILE,c) < 0) {
	    conoll("");
	    conoll("ERROR WRITING SESSION LOG, LOG CLOSED!");
	    seslog = 0;
	}
}
