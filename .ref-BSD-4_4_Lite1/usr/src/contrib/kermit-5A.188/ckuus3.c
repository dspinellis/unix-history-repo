#ifndef NOICP

/*  C K U U S 3 --  "User Interface" for Unix Kermit, part 3  */
 
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
 
/*  SET command (but much material has been split off into ckuus7.c). */
 
/*
  Kermit-specific includes.
  Definitions here supersede those from system include files.
*/
#include "ckcdeb.h"			/* Debugging & compiler things */
#include "ckcasc.h"			/* ASCII character symbols */
#include "ckcker.h"			/* Kermit application definitions */
#include "ckcxla.h"			/* Character set translation */
#include "ckcnet.h"			/* Network symbols */
#include "ckuusr.h"			/* User interface symbols */
 
/* Variables */

extern int size, spsiz, spmax, urpsiz, srvtim, 
  local, server, success,
  flow, binary, delay, parity, escape, what, srvdis,
  turn, duplex, backgrd,
  turnch, bctr, mdmtyp, keep, maxtry, unkcs, network,
  ebqflg, quiet, swcapr, nettype,
  wslotr, lscapr, lscapu,
  carrier, debses,
  cdtimo, nlangs, bgset, pflag, msgflg, dblchar,
  cmdmsk, spsizr, wildxpand, suspend, 
  techo, terror;

extern char *ccntab[];

#ifndef NOSCRIPT
extern int secho;			/* Whether SCRIPT cmd should echo */
#endif /* NOSCRIPT */

#ifndef NOSPL
#ifdef DCMDBUF
extern int *count;
#else
extern int count[];
#endif /* DCMDBUF */
extern int mecho, merror;		/* Macro echo, error */
extern int incase;			/* INPUT case */
#endif /* NOSPL */

extern int bigsbsiz, bigrbsiz;		/* Packet buffers */

extern long speed;			/* Terminal speed */

extern CHAR sstate;			/* Protocol start state */
extern char ttname[];			/* Communication device name */
#ifndef MAC
#ifndef NOSETKEY
extern KEY *keymap;			/* Character map for SET KEY (1:1)  */
extern MACRO *macrotab;			/* Macro map for SET KEY (1:string) */
#ifdef OS2
int wideresult;                         /* for SET KEY, wide OS/2 scan codes */
#endif /* OS2 */
#endif /* NOSETKEY */
#endif /* MAC */

#ifndef NOCSETS
/* system-independent character sets, defined in ckcxla.[ch] */
extern struct csinfo tcsinfo[];
extern struct langinfo langs[];

/* Other character-set related variables */
extern int tcharset, tslevel, language;
#endif /* NOCSETS */ 

/* Declarations from cmd package */
 
#ifdef DCMDBUF
extern char *cmdbuf;			/* Command buffer */
extern char *line;
#else
extern char cmdbuf[];			/* Command buffer */
extern char line[];			/* Character buffer for anything */
#endif /* DCMDBUF */

/* From main ckuser module... */
 
extern char tmpbuf[], *tp, *lp;		/* Temporary buffer */

extern int tlevel;			/* Take Command file level */

#ifndef NOSPL
extern int cmdlvl;			/* Overall command level */
#endif /* NOSPL */

#ifdef UNIX
extern int sessft;			/* Session-log file type */
#endif /* UNIX */


#ifdef VMS
int vms_msgs = 1;			/* SET MESSAGES */
#endif /* VMS */

/* Keyword tables for SET commands */
 
struct keytab chktab[] = {
    "1", 1, 0,
    "2", 2, 0,
    "3", 3, 0,
    "blank-free-2", 4, 0
};

/* For SET MODEM */

struct keytab autotab[] = {
    "changes-speed", 1, 0,
    "matches-speed", 0, 0
};

/* For SET CARRIER */

struct keytab crrtab[] = {
    "auto", CAR_AUT, 0,
    "off",  CAR_OFF, 0,
    "on",   CAR_ON, 0
};
int ncrr = 3;

/* For SET DEBUG */

struct keytab dbgtab[] = {
    "off",     0,  0,
    "on",      1,  0,
    "session", 2,  0
};
int ndbg = 3;

/* Transmission speeds */

/* Note, the values are encoded in cps rather than bps because 19200 and */
/* 38400 are too big for some ints.  All but 75bps are multiples of ten. */
/* Result of lookup in this table must be multiplied by 10 to get actual */
/* speed in bps.  If this number is 70, it must be changed to 75. */
/* If it is 888, this means 75/1200 split speed.  134.5 (IBM 2741) is not */
/* supported, and split speed is not possible in AT&T UNIX. */ 

/* The values are generic, rather than specific to UNIX.  We can't use */
/* B75, B1200, B9600, etc, because non-UNIX versions of C-Kermit will not */
/* necessarily have these symbols defined. */

/* Like all other keytabs, this one must be in "alphabetical" order, */
/* rather than numeric order. */ 

struct keytab spdtab[] = {
    "0",      0,  CM_INV,
    "110",   11,  0,
#ifdef OS2
#ifdef __32BIT__
 "115200",11520,  0,
#endif /* __32BIT__ */
#endif /* OS2 */
  "1200",   120,  0,
#ifdef OS2
  "14400", 1440,  0,
#endif /* OS2 */
  "150",     15,  0,
  "19200", 1920,  0,
  "200",     20,  0,
  "2400",   240,  0,
  "300",     30,  0,
  "3600",   360,  0,
  "38400", 3840,  0,
  "4800",   480,  0,
  "50",       5,  0,
#ifdef OS2
  "57600", 5760,  0,  
#endif /* OS2 */
  "600",     60,  0,
#ifdef OS2
  "7200",   720,  0,
#endif /* OS2 */
  "75",       7,  0,
#ifdef ANYBSD
  "75/1200",888,  0,		/* Special code "888" for split speed */
#endif /* ANYBSD */
#ifdef OS2
#ifdef __32BIT__
  "76800", 7680,  0,
#endif /* __32BIT__ */
#endif /* OS2 */
  "9600",   960,  0
};
int nspd = (sizeof(spdtab) / sizeof(struct keytab)); /* how many speeds */

#ifndef NOCSETS
extern struct keytab lngtab[];		/* Languages for SET LANGUAGE */
extern int nlng;
#endif /* NOCSETS */

/* Duplex keyword table */
 
struct keytab dpxtab[] = {
    "full", 	 0, 0,
    "half",      1, 0
};
 
/* SET FILE parameters */

struct keytab filtab[] = {
    "bytesize",         XYFILS, 0,
#ifndef NOCSETS
    "character-set",    XYFILC, 0,
#endif /* NOCSETS */
    "collision",        XYFILX, 0,
    "display",          XYFILD, 0,
    "incomplete",       XYFILI, 0,
#ifdef VMS
    "label",            XYFILL, 0,
#endif /* VMS */
    "names",            XYFILN, 0,
#ifdef VMS
    "record-length",    XYFILR, 0,
#endif /* VMS */
    "type",             XYFILT, 0,
    "warning",          XYFILW, CM_INV
};
int nfilp = (sizeof(filtab) / sizeof(struct keytab));

/* Flow Control */

struct keytab flotab[] = {		/* SET FLOW-CONTROL keyword table */
#ifdef CK_DTRCD
    "dtr/cd",   FLO_DTRC, 0,
#endif /* CK_DTRCD */
#ifdef CK_DTRCTS
    "dtr/cts",FLO_DTRT, 0,
#endif /* CK_DTRCTS */
    "keep",     FLO_KEEP, 0,
    "none",     FLO_NONE, 0,
#ifdef CK_RTSCTS
    "rts/cts",  FLO_RTSC, 0,
#endif /* CK_RTSCTS */
    "xon/xoff", FLO_XONX, 0
};
int nflo = (sizeof(flotab) / sizeof(struct keytab));

/*  Handshake characters  */
 
struct keytab hshtab[] = {
    "bell", 007, 0,
    "code", 998, 0,
    "cr",   015, 0,
    "esc",  033, 0,
    "lf",   012, 0,
    "none", 999, 0,			/* (can't use negative numbers) */
    "xoff", 023, 0,
    "xon",  021, 0
};
int nhsh = (sizeof(hshtab) / sizeof(struct keytab));
 
struct keytab fttab[] = {		/* File types */
#ifdef VMS
    "b",         XYFT_B, CM_INV|CM_ABR,
#endif /* VMS */
    "binary",    XYFT_B, 0,
#ifdef VMS
    "block",     XYFT_I, CM_INV,
    "image",     XYFT_I, 0,
    "labeled",   XYFT_L, 0,
#endif /* VMS */
    "text",      XYFT_T, 0
};
int nfttyp = (sizeof(fttab) / sizeof(struct keytab));
 
#ifndef NODIAL
extern struct keytab mdmtab[] ;		/* Modem types (in module ckudia.c) */
extern int nmdm;			/* Number of them */
#ifndef MINIDIAL
extern int tbmodel;			/* Telebit model ID */
#endif /* MINIDIAL */
#endif /* NODIAL */
 
#ifdef UNIX
struct keytab wildtab[] = {		/* SET WILDCARD-EXPANSION */
    "kermit",  0, 0,
    "shell",   1, 0
};
#endif /* UNIX */

#ifdef NETCONN
extern struct keytab netcmd[];
extern int nnets;
#endif /* NETCONN */

#ifdef SUNX25
struct keytab x25tab[] = {
    "call-user-data",    XYUDAT, 0,
    "closed-user-group", XYCLOS, 0,
    "reverse-charge",    XYREVC, 0  
};
int nx25 = (sizeof(x25tab) / sizeof(struct keytab));

struct keytab padx3tab[] = {
    "break-action",         PAD_BREAK_ACTION,           0,
    "break-character",      PAD_BREAK_CHARACTER,        0,
    "character-delete",     PAD_CHAR_DELETE_CHAR,       0,
    "cr-padding",           PAD_PADDING_AFTER_CR,       0,
    "discard-output",       PAD_SUPPRESSION_OF_DATA,    0,
    "echo",                 PAD_ECHO,                   0,
    "editing",              PAD_EDITING,                0,
    "escape",               PAD_ESCAPE,                 0,
    "forward",              PAD_DATA_FORWARD_CHAR,      0,
    "lf-padding",           PAD_PADDING_AFTER_LF,       0,
    "lf-insert",            PAD_LF_AFTER_CR,            0,
    "line-delete",          PAD_BUFFER_DELETE_CHAR,     0,
    "line-display",         PAD_BUFFER_DISPLAY_CHAR,    0,
    "line-fold",            PAD_LINE_FOLDING,           0,
    "pad-flow-control",     PAD_FLOW_CONTROL_BY_PAD,    0,
    "service-signals",      PAD_SUPPRESSION_OF_SIGNALS, 0,
    "timeout",              PAD_DATA_FORWARD_TIMEOUT,   0,
/* Speed is read-only */
    "transmission-rate",    PAD_LINE_SPEED,             0,
    "user-flow-control",    PAD_FLOW_CONTROL_BY_USER,   0
};
int npadx3 = (sizeof(padx3tab) / sizeof(struct keytab));
#endif /* SUNX25 */

/* Parity keyword table */
 
struct keytab partab[] = {
    "even",    'e', 0,
    "mark",    'm', 0,
    "none",     0 , 0,
    "odd",     'o', 0,
    "space",   's', 0
};
int npar = (sizeof(partab) / sizeof(struct keytab));
 
/* On/Off table */
 
struct keytab onoff[] = {
    "off",       0, 0,
    "on",        1, 0
};
 
struct keytab rltab[] = {
    "local",     1, 0,
    "off",       0, CM_INV,
    "on",        1, CM_INV,
    "remote",    0, 0
};
int nrlt = (sizeof(rltab) / sizeof(struct keytab));

/* Incomplete File Disposition table */
static
struct keytab ifdtab[] = {
    "discard",   0, 0,
    "keep",      1, 0
};

/* SET TAKE parameters table */
static
struct keytab taktab[] = {
    "echo",  0, 0,
    "error", 1, 0,
    "off",   2, CM_INV,			/* For compatibility */
    "on",    3, CM_INV			/* with MS-DOS Kermit... */
};

/* SET MACRO parameters table */
static
struct keytab smactab[] = {
    "echo",  0, 0,
    "error", 1, 0
};

#ifndef NOSCRIPT
static
struct keytab scrtab[] = {
    "echo",  0, 0
};
#endif /* NOSCRIPT */

/* Bytesize table */
struct keytab byttab[] = {
    "bytesize",  0, 0
};
int nbytt = 1;

#ifndef NOSERVER
/* Server parameters table */
struct keytab srvtab[] = {
    "display", XYSERD, 0,
    "timeout", XYSERT, 0
};
#endif /* NOSERVER */

/* SET TRANSFER/XFER table */

struct keytab tstab[] = {
#ifndef NOCSETS
    "character-set", 1,   0,
#endif /* NOCSETS */
    "locking-shift", 2,   0
};
int nts = (sizeof(tstab) / sizeof(struct keytab));

#ifndef NOCSETS
/* SET TRANSFER CHARACTER-SET table */

extern struct keytab tcstab[];
extern int ntcs;
#endif /* NOCSETS */

/* SET TRANSFER LOCKING-SHIFT table */
struct keytab lstab[] = {
    "forced", 2,   0,
    "off",    0,   0,
    "on",     1,   0
};
int nls = (sizeof(lstab) / sizeof(struct keytab));

/* SET TELNET table */
#ifdef TNCODE
extern int tn_duplex, tn_nlm;
extern char *tn_term;
struct keytab tntab[] = {
    "echo",          CK_TN_EC,   0,
    "newline-mode",  CK_TN_NL,   0,
    "terminal-type", CK_TN_TT,   0
};
int ntn = (sizeof(tntab) / sizeof(struct keytab));
#endif /* TNCODE */

struct keytab ftrtab[] = {		/* Feature table */
#ifndef NOCSETS				/* 0 = we have it, 1 = we don't */
"character-sets",	0, 0,
#else
"character-sets",	1, 0,
#endif /* NOCSETS */
#ifndef NOCYRIL
"cyrillic",		0, 0,
#else
"cyrillic",		1, 0,
#endif /* NOCYRIL */

#ifndef NODEBUG
"debug",		0, 0,
#else
"debug",		1, 0,
#endif /* NODEBUG */

#ifndef NODIAL
"dial",			0, 0,
#else
"dial",			1, 0,
#endif /* NODIAL */

#ifdef DYNAMIC
"dynamic-memory",       0, 0,
#else
"dynamic-memory",       1, 0,
#endif /* DYNAMIC */

#ifdef CK_CURSES
"fullscreen-display",	0, 0,
#else
"fullscreen-display",	1, 0,
#endif /* CK_CURSES */
#ifndef NOHELP
"help",			0, 0,
#else
"help",			1, 0,
#endif /* NOHELP */
#ifndef NOSPL
"if-command",		0, 0,
#else
"if-command",		1, 0,
#endif /* NOSPL */
#ifndef NOJC
#ifdef UNIX
"job-control",		0, 0,
#else
"job-control",		1, 0,
#endif /* UNIX */
#else
"job-control",		1, 0,
#endif /* NOJC */
#ifdef KANJI
"kanji",		0, 0,
#else
"kanji",		1, 0,
#endif /* KANJI */
#ifndef NOCSETS
"latin1",		0, 0,
#else
"latin1",		1, 0,
#endif /* NOCSETS */
#ifdef LATIN2
"latin2",		0, 0,
#else
"latin2",		1, 0,
#endif /* LATIN2 */
#ifdef NETCONN
"network",		0, 0,
#else
"network",		1, 0,
#endif /* NETCONN */
#ifndef NOPUSH
"push",			0, 0,
#else
"push",			1, 0,
#endif /* PUSH */
#ifndef NOSCRIPT
"script-command",	0, 0,
#else
"script-command",	1, 0,
#endif /* NOSCRIPT */
#ifndef NOSERVER
"server-mode",		0, 0,
#else
"server-mode",		1, 0,
#endif /* NOSERVER */
#ifndef NOSHOW
"show-command",		0, 0,
#else
"show-command",		1, 0,
#endif /* NOSHOW */
#ifndef NOXMIT
"transmit",		0, 0,
#else
"transmit",		1, 0
#endif /* NOXMIT */
};
int nftr = (sizeof(ftrtab) / sizeof(struct keytab));

int					/* CHECK command */
dochk() {
    int x, y;
    if ((y = cmkey(ftrtab,nftr,"","",xxstring)) < 0) return(y);
    if ((x = cmcfm()) < 0) return(x);
    if (msgflg)				/* If at top level... */
      printf(" %svailable\n", y ? "Not a" : "A"); /* Print a message */
    else if (y && !backgrd)		/* Or if not available and in */
      printf(" CHECK: feature not available\n"); /* command file or macro */
    return(success = 1 - y);
}

#ifndef MAC
#ifndef NOSETKEY
int
set_key() {				/* SET KEY */
    int y;
    int kc;				/* Key code */
    char *bind;				/* Key binding */

    if ((y = cmnum("numeric key code","",10,&kc,xxstring)) < 0)
      return(y);
    if (kc < 0 || kc >= KMSIZE) {
	printf("?key code must be between 0 and %d\n", KMSIZE - 1);
	return(-9);
    }
#ifdef OS2
    wideresult = -1;
#endif /* OS2 */
    if ((y = cmtxt("key definition","",&bind,xxstring)) < 0)
      return(y);
    if (macrotab[kc]) {			/* Possibly free old macro from key */
	free(macrotab[kc]);
	macrotab[kc] = NULL;
    }
    switch (strlen(bind)) {		/* Action depends on length */
#ifdef OS2
      case 0:				/* Reset to default binding */
        keymap[kc] = wideresult == -1 ? kc : 0;   /* or bind to NUL */
	break;
      case 1:				/* Single character */
	keymap[kc] = wideresult == -1 ? (CHAR) *bind : wideresult;
	break;
#else /* Not OS/2 */
      case 0:				/* Reset to default binding */
	keymap[kc] = kc;
	break;
      case 1:				/* Single character */
  	keymap[kc] = (CHAR) *bind;
  	break;
#endif /* OS2 */

      default:				/* Character string */
	keymap[kc] = kc;
	macrotab[kc] = (MACRO) malloc(strlen(bind)+1);
	if (macrotab[kc])
	  strcpy((char *) macrotab[kc], bind);
	break;
    }
    return(1);
}
#endif /* NOSETKEY */
#endif /* MAC */

/*  D O P R M  --  Set a parameter.  */
/*
 Returns:
  -2: illegal input
  -1: reparse needed
   0: success
*/
int
doprm(xx,rmsflg) int xx, rmsflg; {
    int i, x, y = 0, z;
    long zz;
    char *s;
 
switch (xx) {
 
#ifdef SUNX25				/* SET X25 ... */
case XYX25:
    return(setx25());

case XYPAD:				/* SET PAD ... */
    return(setpadp());
#endif /* SUNX25 */

case XYEOL:	/* These have all been moved to set send/receive... */
case XYLEN: 	/* Let the user know what to do. */
case XYMARK:
case XYNPAD:
case XYPADC:
case XYTIMO:
    printf("...Use SET SEND or SET RECEIVE instead.\n");
    printf("Type HELP SET SEND or HELP SET RECEIVE for more info.\n");
    return(success = 0);

case XYATTR:				/* File Attribute packets */
    return(setat(rmsflg));

case XYIFD:				/* Incomplete file disposition */
    if ((y = cmkey(ifdtab,2,"","discard",xxstring)) < 0) return(y);
    if ((x = cmcfm()) < 0) return(x);
    if (rmsflg) {
	sstate = setgen('S', "310", y ? "1" : "0", "");
	return((int) sstate);
    } else {
	keep = y;
	return(success = 1);
    }
 
#ifndef NOSPL
case XYINPU:				/* SET INPUT */
    return(setinp());
#endif /* NOSPL */

#ifdef NETCONN
case XYNET:				/* SET NETWORK */
    if ((z = cmkey(netcmd,nnets,"","tcp/ip",xxstring)) < 0)
      return(z);
    if ((x = cmcfm()) < 0) return(x);
    nettype = z;
    if (
	(nettype != NET_DEC) &&
	(nettype != NET_SX25) &&
        (nettype != NET_TCPB)) {
	printf("?Network type not supported\n");
	return(success = 0);
    } else {
	return(success = 1);
    }
#endif /* NETCONN */

case XYHOST:				/* SET HOST or SET LINE */
case XYLINE:
    return(setlin(xx,1));
 
#ifndef MAC
#ifndef NOSETKEY
case XYKEY:				/* SET KEY */
    return(set_key());
#endif /* NOSETKEY */
#endif /* MAC */

#ifndef NOCSETS
case XYLANG: 				/* Language */
    if ((y = cmkey(lngtab,nlng,"","none",xxstring)) < 0) /* language code */
      return(y);
    if ((x = cmcfm()) < 0) return(x);	/* And confirmation of command */

    /* Look up language and get associated character sets */
    for (i = 0; (i < nlangs) && (langs[i].id != y); i++) ;
    if (i >= nlangs) {
	printf("?internal error, sorry\n");
	return(success = 0);
    }
    language = i;			/* All good, set the language, */
    return(success = 1);
#endif /* NOCSETS */

#ifndef MAC
case XYBACK:				/* BACKGROUND */
    if ((z = cmkey(onoff,2,"","",xxstring)) < 0) return(z);
    if ((y = cmcfm()) < 0) return(y);
    bgset = z;
    success = 1;
    bgchk();
    return(success);
#endif /* MAC */

case XYQUIE:				/* QUIET */
    return(success = seton(&quiet));

case XYBUF: {				/* BUFFERS */
#ifdef DYNAMIC
    int sb, rb;
    if ((y = cmnum("send buffer size","",10,&sb,xxstring)) < 0) {
	if (y == -3) printf("?two numbers required\n");
	return(y);
    }
    if (sb < 80) {
	printf("?too small");
	return(-2);
    }
    if ((y = cmnum("receive buffer size","",10,&rb,xxstring)) < 0)  {
	if (y == -3) printf("?receive buffer size required\n");
	return(y);
    }
    if (rb < 80) {
	printf("?too small");
	return(-2);
    }
    if ((y = cmcfm()) < 0) return(y);
    if ((y = inibufs(sb,rb)) < 0) return(y);
    y = adjpkl(urpsiz,wslotr,bigrbsiz); /* Maybe adjust packet sizes */
    if (y != urpsiz) urpsiz = y;
    y = adjpkl(spsiz,wslotr,bigsbsiz);
    if (y != spsiz) spsiz = spmax = spsizr = y;
    return(success = 1);
#else
    printf("?Sorry, not available\n");
    return(success = 0);
#endif /* DYNAMIC */
}

case XYCHKT:				/* BLOCK-CHECK */
    if ((x = cmkey(chktab,4,"","1",xxstring)) < 0) return(x);
    if ((y = cmcfm()) < 0) return(y);
    bctr = x;			     /* Set locally too, even if REMOTE SET */
    if (rmsflg) {
	if (x == 4) {
	    tmpbuf[0] = 'B';
	    tmpbuf[1] = '\0';
	} else sprintf(tmpbuf,"%d",x);
	sstate = setgen('S', "400", tmpbuf, "");
	return((int) sstate);
    } else {
	return(success = 1);
    }
 
#ifndef MAC
/*
  The Mac has no carrier...
*/
case XYCARR:				/* CARRIER */
    if ((y = cmkey(crrtab,ncrr,"","auto",xxstring)) < 0) return(y);
    if (y == CAR_ON) {
	x = cmnum("Carrier wait timeout, seconds","0",10,&z,xxstring);
	if (x < 0) return(z);
    }
    if ((x = cmcfm()) < 0) return(x);
    carrier = ttscarr(y);
    cdtimo = z;
    return(success = 1);
#endif /* MAC */

#ifdef TNCODE
case XYTEL:				/* TELNET */
    if ((z = cmkey(tntab,ntn,"parameter for TELNET negotiations", "",
		   xxstring)) < 0) return(z);
    switch (z) {
      case CK_TN_EC:			/* ECHO */
	if ((x = cmkey(rltab,nrlt,
		       "initial TELNET echoing state","local",xxstring)) < 0)
	  return(x);
	if ((y = cmcfm()) < 0) return(y);
	tn_duplex = x;
	return(success = 1);

      case CK_TN_TT:			/* TERMINAL TYPE */
	if ((y = cmtxt("terminal type for TELNET connections","",
		       &s,xxstring)) < 0)
	  return(y);
	if (tn_term) free(tn_term);	/* Free any previous storage */
	if (s == NULL || *s == NUL) {	/* If none given */
	    tn_term = NULL;		/* remove the override string */
	    return(success = 1);
	} else if (tn_term = malloc(strlen(s)+1)) { /* Make storage for new */
	    strcpy(tn_term,s);		/* Copy string into new storage */
	    return(success = 1);
	} else return(success = 0);

      case CK_TN_NL:			/* NEWLINE-MODE */
	return(success = seton(&tn_nlm));

      default:
	return(-2);
    }
#endif /* TNCODE */

default:
    break;
}

switch (xx) {
#ifndef NOSPL
case XYCOUN:				/* SET COUNT */
    x = cmnum("Positive number","0",10,&z,xxstring);
    if (x < 0) return(x);
    if ((x = cmcfm()) < 0) return(x);
    if (z < 0) {
	printf("?A positive number, please\n");
	return(0);
    }
    debug(F101,"XYCOUN: z","",z);
    return(success = setnum(&count[cmdlvl],z,0,10000));
#endif /* NOSPL */

#ifndef NOSPL
case XYCASE:
    return(success = seton(&incase));
#endif /* NOSPL */

case XYCMD:				/* COMMAND ... */
    if ((y = cmkey(byttab,nbytt,"","bytesize",xxstring)) < 0) return(y);
    if ((y = cmnum("bytesize for command characters, 7 or 8","7",10,&x,
		   xxstring)) < 0)
	  return(y);
    if (x != 7 && x != 8) {
	printf("\n?The choices are 7 and 8\n");
	return(success = 0);
    }
    if ((y = cmcfm()) < 0) return(y);
    if (x == 7) cmdmsk = 0177;
    else if (x == 8) cmdmsk = 0377;
    return(success = 1);
    
case XYDFLT:				/* SET DEFAULT = CD */
    return(success = docd());

case XYDEBU:				/* SET DEBUG { on, off, session } */
    if ((y = cmkey(dbgtab,ndbg,"","",xxstring)) < 0) return(y);
    if ((x = cmcfm()) < 0) return(x);
    switch (y) {
      case 0:				/* 0 = all debugging off. */
	debses = 0;
#ifdef DEBUG
	if (deblog) doclslog(LOGD);
#endif /* DEBUG */
        return(success = 1);

      case 1:				/* 1 = log debugging to debug.log */
#ifdef DEBUG
	deblog = debopn("debug.log", 0);
	return(success = deblog ? 1 : 0);
#else
	printf("?Sorry, debug log feature not enabled\n");
	return(success = 0);
#endif /* DEBUG */

      case 2:				/* 2 = session. */
	return(success = debses = 1);
    }

case XYDELA:				/* SET DELAY */
    y = cmnum("Number of seconds before starting to send","5",10,&x,xxstring);
    if (x < 0) x = 0;
    return(success = setnum(&delay,x,y,999));

default:
    break;
}

switch (xx) {
 
#ifndef NODIAL
case XYDIAL:				/* SET DIAL */
    return(setdial());
#endif /* NODIAL */

#ifdef COMMENT				/* Unused at present */
case XYDOUB:
    if ((x = cmfld("Character to double","none",&s,xxstring)) < 0) {
	if (x == -3) {
	    dblchar = -1;
	    if (msgflg) printf("Doubling Off\n");
	    return(success = 1);
	} else return(x);
    }
    strcpy(line,s);
    lp = line;
    if ((x = cmcfm()) < 0) return(x);
    if (!xxstrcmp(lp,"none",4)) {
	dblchar = -1;
	if (msgflg) printf("Doubling Off\n");
	return(success = 1);
    }
    if ((int)strlen(lp) != 1) return(-2);
    dblchar = *lp & 0xFF;
    if (msgflg) printf("Doubled: %d\n",dblchar);
    return(success = 1);
#endif /* COMMENT */

case XYDUPL:				/* SET DUPLEX */
    if ((y = cmkey(dpxtab,2,"","full",xxstring)) < 0) return(y);
    if ((x = cmcfm()) < 0) return(x);
    duplex = y;
    return(success = 1);
 
case XYLCLE:				/* LOCAL-ECHO (= DUPLEX) */
    return(success = seton(&duplex));

case XYESC:				/* SET ESCAPE */
    sprintf(tmpbuf,"%d",DFESC);
    y = cmnum("Decimal ASCII code for CONNECT-mode escape character",
	      tmpbuf, 10,&x,xxstring);
    success = setcc(&escape,x,y);
#ifdef COMMENT
/* This is what SHOW ESCAPE is for. */
    if (success && msgflg)
      printf(" CONNECT-mode escape character: %d (Ctrl-%c, %s)\n",
	     escape,ctl(escape),(escape == 127 ? "DEL" : ccntab[escape]));
#endif /* COMMENT */
    return(success);

default:
    break;
}

switch (xx) {
case XYFILE:				/* SET FILE */
    return(setfil(rmsflg));

case XYFLOW:				/* FLOW-CONTROL */
/*
  Note: flotab[] keyword table (defined above) only includes the legal 
  flow-control options for each implementation, controlled by symbols
  defined in ckcdeb.h.
*/
    if ((y = cmkey(flotab,nflo,"","xon/xoff",xxstring)) < 0) return(y);
    if ((x = cmcfm()) < 0) return(x);
    flow = y;
    debug(F101,"set flow","",flow);
    return(success = 1);
 
case XYHAND:				/* HANDSHAKE */
    if ((y = cmkey(hshtab,nhsh,"","none",xxstring)) < 0) return(y);
    if (y == 998) {
	if ((x = cmnum("ASCII value","",10,&y,xxstring)) < 0)
	  return(x);
	if ((y < 1) || ((y > 31) && (y != 127))) {
	    printf("?Character must be in ASCII control range\n");
	    return(-9);
	}
    }
    if ((x = cmcfm()) < 0) return(x);
    turn = (y > 0127) ? 0 : 1 ;
    turnch = y;
    return(success = 1);
 
#ifndef NOSPL
case XYMACR:				/* SET MACRO */
    if ((y = cmkey(smactab,2,"","",xxstring)) < 0) return(y);
    switch (y) {
      case 0: return(success = seton(&mecho));
      case 1: return(success = seton(&merror));
      default: return(-2);
    }
#endif /* NOSPL */

#ifndef NODIAL
case XYMODM:				/* SET MODEM */
    if ((x = cmkey(mdmtab,nmdm,"type of modem","none", xxstring)) < 0)
	return(x);
    if ((z = cmcfm()) < 0) return(z);
    mdmtyp = x;
#ifndef MINIDIAL
    tbmodel = 0;          /* If it's a Telebit, we don't know the model yet */
#endif /* MINIDIAL */
    return(success = 1);
#endif /* NODIAL */

  case XYMSGS:
#ifdef VMS
    if ((z = cmkey(onoff,2,"","",xxstring)) < 0) return(z);
    if ((y = cmcfm()) < 0) return(y);
    vms_msgs = z;
    printf("Sorry, SET MESSAGES not implemented yet\n");
    return(success = 0);
#endif /* VMS */
default:
    break;
}

switch (xx) {
	
case XYPARI:				/* PARITY */
    if ((y = cmkey(partab,npar,"","none",xxstring)) < 0) return(y);
    if ((x = cmcfm()) < 0) return(x);
 
/* If parity not none, then we also want 8th-bit prefixing */
 
    if (parity = y) ebqflg = 1; else ebqflg = 0;
    return(success = 1);
 
#ifndef NOFRILLS
case XYPROM:				/* SET PROMPT */
/*
  Note: xxstring not invoked here.  Instead, it is invoked every time the
  prompt is issued.  This allows the prompt string to contain variables
  that can change, like \v(dir), \v(time), etc.
*/
#ifdef MAC
    if ((x = cmtxt("Program's command prompt","Mac-Kermit>",&s,NULL)) < 0)
#else
    if ((x = cmtxt("Program's command prompt","C-Kermit>",&s,NULL)) < 0)
#endif /* MAC */
      return(x);
    if (*s == '{') {			/* Remove enclosing braces, if any */
	x = (int)strlen(s);
	if (s[x-1] == '}') {
	    s[x-1] = NUL;
	    s++;
	}
    }
#ifdef COMMENT
/*
  Let's not do this any more -- we don't do it anywhere else.
*/
    else if (*s == '"') {		/* For compatibility with pre-5A */
	x = (int)strlen(s);
	if (s[x-1] == '"') {
	    s[x-1] = NUL;
	    s++;
	}
    }
#endif /* COMMENT */
    cmsetp(s);				/* Set the prompt */
    return(success = 1);
#endif /* NOFRILLS */
 
case XYRETR:				/* RETRY: per-packet retry limit */
    y = cmnum("Maximum retries per packet","10",10,&x,xxstring);
    if (x < 0) x = 0;
    if ((x = setnum(&maxtry,x,y,999)) < 0) return(x);
    if (maxtry <= wslotr) {
	printf("?Retry limit must be greater than window size\n");
	return(success = 0);
    }
    sprintf(tmpbuf,"%d",maxtry);
    if (rmsflg) {
	sstate = setgen('S', "403", tmpbuf, "");
	return((int) sstate);
    } else return(success = x);
 
#ifndef NOSERVER
case XYSERV:				/* SET SERVER items */
    if ((y = cmkey(srvtab,2,"","",xxstring)) < 0) return(y);
    switch (y) {
      case XYSERT:
	tp = tmpbuf;
        sprintf(tp,"%d",DSRVTIM);
	if ((y = cmnum("interval for server NAKs, 0 = none",tp,10,&x,
		       xxstring)) < 0)
	  return(y);
	if (x < 0) {
	    printf("\n?Specify a positive number, or 0 for no server NAKs\n");
	    return(0);
	}
	if ((y = cmcfm()) < 0) return(y);
	sprintf(tp,"%d",x);
	if (rmsflg) {
	    sstate = setgen('S', "404", tp, "");
	    return((int) sstate);
	} else {
	    srvtim = x;			/* Set the server timeout variable */
	    return(success = 1);
	}
      case XYSERD:			/* SERVER DISPLAY */
	return(success = seton(&srvdis)); /* ON or OFF... */
      default:
	return(-2);
    }
#endif /* NOSERVER */

#ifdef UNIX
#ifndef NOJC
case XYSUSP:				/* SET SUSPEND */
    seton(&suspend);			/* on or off... */
    return(success = 1);
#endif /* NOJC */
#endif /* UNIX */

case XYTAKE:				/* SET TAKE */
    if ((y = cmkey(taktab,4,"","",xxstring)) < 0) return(y);
    switch (y) {
      case 0: return(success = seton(&techo));
      case 1: return(success = seton(&terror));
      case 2: techo = 0; return(success = 1); /* For compatibility with */
      case 3: techo = 1; return(success = 1); /* MS-DOS Kermit */
      default: return(-2);
    }

#ifndef NOSCRIPT
case XYSCRI:				/* SET SCRIPT */
    if ((y = cmkey(scrtab,1,"","echo",xxstring)) < 0) return(y);
    switch (y) {
      case 0: return(success = seton(&secho));
      default: return(-2);
    }
#endif /* NOSCRIPT */

default:
    break;
}

switch (xx) {
case XYTERM:				/* SET TERMINAL */
    return(settrm());

default:
    break;
}

switch (xx) {

/* SET SEND/RECEIVE protocol parameters. */
 
case XYRECV:
case XYSEND:
    return(setsr(xx,rmsflg));
 
#ifdef UNIX
case XYSESS:				/* SESSION-LOG */
    if ((x = cmkey(fttab,2,"type of file","text",xxstring)) < 0)
      return(x);
    if ((y = cmcfm()) < 0) return(y);
    sessft = x;
    return(success = 1);
#endif /* UNIX */

case XYSPEE:				/* SET SPEED */
    if (network) {
	printf("\n?Speed cannot be set for network connections\n");
	return(success = 0);
    }
    lp = line;
    sprintf(lp,"Transmission rate for %s in bits per second",ttname);

    if ((x = cmkey(spdtab,nspd,line,"",xxstring)) < 0) {
	if (x == -3) printf("?value required\n");
	return(x);
    }
    if ((y = cmcfm()) < 0) return(y);
    if (!local) {
	printf("?Sorry, you must SET LINE first\n");
	return(success = 0);
    }
    zz = (long) x * 10L;
    if (zz == 70) zz = 75;		/* (see spdtab[] def) */
    if (ttsspd(x) < 0)  {		/* Call ttsspd with cps, not bps! */
	printf("?Unsupported line speed - %ld\n",zz);
	return(success = 0);
    } else {
	speed = zz;
	if (pflag &&
#ifndef NOSPL
	    cmdlvl == 0
#else
	    tlevel < 0
#endif /* NOSPL */
	    ) {
	    if (speed == 8880)
	      printf("%s, 75/1200 bps\n",ttname);
	    else
	      printf("%s, %ld bps\n",ttname,speed);
	}
	return(success = 1);
    }
 
  case XYXFER:				/* SET TRANSFER */
    if ((y = cmkey(tstab,nts,"","character-set",xxstring)) < 0) return(y);
#ifndef NOCSETS
    if (y == 1) {			/* character-set */
	if ((y = cmkey(tcstab,ntcs,"","transparent",xxstring)) < 0) return(y);
	if ((x = cmcfm()) < 0) return(x);
	if (rmsflg) {
	    sstate = setgen('S', "405", tcsinfo[y].designator, "");
	    return((int) sstate);
	} else {
	    tslevel = (y == TC_TRANSP) ? 0 : 1; /* transfer syntax level */
	    tcharset = y;		/* transfer character set */
	    return(success = 1);
	}
    } else
#endif /* NOCSETS */
      if (y == 2) {			/* LOCKING-SHIFT options */
	  if ((y = cmkey(lstab,nls,"","on",xxstring)) < 0)
	    return(y);
	  if ((x = cmcfm()) < 0) return(x);
	  lscapr = (y == 1) ? 1 : 0;	/* ON: requested = 1 */
	  lscapu = (y == 2) ? 2 : 0;	/* FORCED:  used = 1 */
	  return(success = 1);
      } else return(-2);

#ifndef NOXMIT
  case XYXMIT:				/* SET TRANSMIT */
    return(setxmit());
#endif /* NOXMIT */

#ifndef NOCSETS
  case XYUNCS:				/* UNKNOWN-CHARACTER-SET */
    if ((y = cmkey(ifdtab,2,"","discard",xxstring)) < 0) return(y);
    if ((x = cmcfm()) < 0) return(x);
    unkcs = y;
    return(success = 1);
#endif /* NOCSETS */

#ifdef UNIX
  case XYWILD:				/* WILDCARD-EXPANSION */
    if ((y = cmkey(wildtab,2,"who expands wildcards","kermit",xxstring)) < 0)
      return(y);
    if ((x = cmcfm()) < 0) return(x);
    wildxpand = y;
    return(success = 1);
#endif /* UNIX */

  case XYWIND:				/* WINDOW-SLOTS */
    y = cmnum("Number of sliding-window slots, 1 to 31","1",10,&x,xxstring);
    y = setnum(&z,x,y,31);
    if (y < 0) return(y);
    if (z < 1) z = 1;
#ifdef COMMENT
    /* This is unreasonable */
    if (maxtry < z) {
	printf("?Window slots must be less than retry limit\n");
	return(success = 0);
    }
#endif /* COMMENT */
    if (rmsflg) {			/* Set remote window size */
	tp = tmpbuf;
	sprintf(tp,"%d",z);
	sstate = setgen('S', "406", tp, "");
	return((int) sstate);
    }
    wslotr = z;				/* Set local window size */
    swcapr = (wslotr > 1) ? 1 : 0;	/* Set window bit in capas word? */
    if (wslotr > 1) {			/* Window size > 1? */
	y = adjpkl(urpsiz,wslotr,bigrbsiz); /* Maybe adjust packet size */
	if (y != urpsiz) {		/* Did it change? */
	    urpsiz = y;
	    if (msgflg)
	    printf(
" Adjusting receive packet-length to %d for %d window slots\n",
		   urpsiz, wslotr);
	}
    }
    return(success = 1);

default:
    if ((x = cmcfm()) < 0) return(x);
    printf("Not working yet - %s\n",cmdbuf);
    return(success = 0);
    }
}
#endif /* NOICP */
