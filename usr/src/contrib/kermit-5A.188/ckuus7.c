#ifndef NOICP

/*  C K U U S 7 --  "User Interface" for Unix Kermit, part 7  */
 
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
 
/*
  This file created from parts of ckuus3.c, which became to big for
  Mark Williams Coherent compiler to handle.
*/

/*
  Definitions here supersede those from system include files.
*/
#include "ckcdeb.h"			/* Debugging & compiler things */
#include "ckcasc.h"			/* ASCII character symbols */
#include "ckcker.h"			/* Kermit application definitions */
#include "ckcxla.h"			/* Character set translation */
#include "ckcnet.h"			/* Network symbols */
#include "ckuusr.h"			/* User interface symbols */

static int x, y = 0, z;
static char *s;

static int mdmsav = -1;			/* Save modem type around network */
static int oldplex = -1;		/* Duplex holder around network */

extern int success, nfilp, fmask, fncnv, frecl, nfttyp, binary, warn, msgflg;
extern int cmask, maxrps, wslotr, bigsbsiz, bigrbsiz, urpsiz, rpsiz, spsiz;
extern int spsizr, spsizf, maxsps, spmax, pflag, bctr, npad, timef, timint;
extern int pkttim, rtimo, local, nfils, displa, atcapr, nettype, escape;
extern int mdmtyp, duplex, dfloc, network, cdtimo, fncact, mypadn;
extern int tnlm, sosi, tlevel, lf_opts, backgrd, flow, fdispla;
extern int
  atenci, atenco, atdati, atdato, atleni, atleno, atblki, atblko,
  attypi, attypo, atsidi, atsido, atsysi, atsyso, atdisi, atdiso; 

extern long speed;

extern CHAR sstate, eol, seol, stchr, mystch, mypadc, padch;

extern char *cmarg, *cmarg2, *dftty;

extern char tmpbuf[], *tp, *lp;		/* Temporary buffer & pointers */
#ifndef NOFRILLS
extern char optbuf[];			/* Buffer for MAIL or PRINT options */
extern int rprintf;			/* REMOTE PRINT flag */
#endif /* NOFRILLS */
extern char ttname[];

extern struct keytab onoff[], filtab[], fttab[], rltab[];
extern int nrlt;

#ifdef DCMDBUF
extern char *atxbuf;			/* Atom buffer */
extern char *cmdbuf;			/* Command buffer */
extern char *line;			/* Character buffer for anything */
#ifdef CK_CURSES
/* This needs cleaning up... */
#ifdef UNIX
extern char *trmbuf;			/* Termcap buffer */
#endif /* UNIX */
#ifdef OS2
extern char *trmbuf;			/* Termcap buffer */
#endif /* OS2 */
#ifdef OSK
extern char *trmbuf;			/* Termcap buffer */
#endif /* OSK */
#ifdef AMIGA
extern char *trmbuf;			/* Termcap buffer */
#endif /* AMIGA */
#endif /* CK_CURSES */
#else
extern char atxbuf[];			/* Atom buffer */
extern char cmdbuf[];			/* Command buffer */
extern char line[];			/* Character buffer for anything */
#ifdef CK_CURSES
#ifdef UNIX
extern char trmbuf[];			/* Termcap buffer */
#endif /* UNIX */
#endif /* CK_CURSES */
#endif /* DCMDBUF */

#ifndef NOCSETS
extern struct keytab fcstab[];		/* For 'set file character-set' */
extern struct keytab ttcstab[];
extern int nfilc, fcharset, ntermc, tcsr, tcsl;
#endif /* NOCSETS */

#ifndef NOSPL
extern int cmdlvl;			/* Overall command level */
#endif /* NOSPL */

#ifdef TNCODE
extern int tn_init;
#endif /* TNCODE */

#ifdef SUNX25
extern int revcall, closgr, cudata, nx25, npadx3;
extern char udata[MAXCUDATA];
extern CHAR padparms[MAXPADPARMS+1];
extern struct keytab x25tab[], padx3tab[];
#endif /* SUNX25 */

#ifdef CK_CURSES
#ifndef VMS
_PROTOTYP(int tgetent,(char *, char *));
#endif /* VMS */
#endif /* CK_CURSES */

#ifndef NODIAL
extern int dialhng, dialtmo, dialksp, dialdpy, dialmnp, dialmhu;
extern int mdmspd;
extern char *dialini, *dialdir, *dialcmd, *dialnpr;
extern FILE * dialfd;


struct keytab dialtab[] = {
    "dial-command", XYDDIA, 0,
    "directory", XYDDIR, 0,
    "display", XYDDPY, 0,
    "hangup",  XYDHUP, 0,
    "init-string", XYDINI, 0,
    "kermit-spoof", XYDKSP, 0,
    "mnp-enable", XYDMNP, 0,
#ifdef MDMHUP
    "modem-hangup", XYDMHU, 0,
#endif /* MDMHUP */
    "prefix", XYDNPR, 0,
    "speed-matching", XYDSPD, 0,
    "timeout", XYDTMO, 0
};
int ndial = (sizeof(dialtab) / sizeof(struct keytab));
#endif /* NODIAL */

#ifndef NOXMIT
/* set transmit */
#define XMITF 0
#define XMITL 1
#define XMITP 2
#define XMITE 3
#define XMITX 4
#define XMITS 5
#define XMITW 6

#ifndef NOXMIT
#define XMBUFL 50
extern int xmitf, xmitl, xmitp, xmitx, xmits, xmitw;
char xmitbuf[XMBUFL+1] = { NUL };	/* TRANSMIT eof string */
#endif /* NOXMIT */

struct keytab xmitab[] = {
    "echo",     XMITX, 0,
    "eof",      XMITE, 0,
    "fill",     XMITF, 0,
    "linefeed", XMITL, 0,
    "locking-shift", XMITS, 0,
    "pause",    XMITW, 0,
    "prompt",   XMITP, 0
};
int nxmit = (sizeof(xmitab) / sizeof(struct keytab));
#endif /* NOXMIT */

/* For SET FILE COLLISION */
/* Some of the following may be possible for some C-Kermit implementations */
/* but not others.  Those that are not possible for your implementation */
/* should be ifdef'd out. */

struct keytab colxtab[] = { /* SET FILE COLLISION options */
    "append",    XYFX_A, 0,  /* append to old file */
#ifdef COMMENT
    "ask",       XYFX_Q, 0,  /* ask what to do (not implemented) */
#endif
    "backup",    XYFX_B, 0,  /* rename old file */
    "discard",   XYFX_D, 0,  /* don't accept new file */
    "no-supersede", XYFX_D, CM_INV, /* ditto (MSK compatibility) */
    "overwrite", XYFX_X, 0,  /* overwrite the old file == file warning off */
    "rename",    XYFX_R, 0,  /* rename the incoming file == file warning on */
    "update",    XYFX_U, 0,  /* replace if newer */
};
int ncolx = (sizeof(colxtab) / sizeof(struct keytab));

static struct keytab rfiltab[] = {	/* for REMOTE SET FILE */
    "collision",     XYFILX, 0,
    "record-length", XYFILR, 0,
    "type",          XYFILT, 0
};
int nrfilp = (sizeof(rfiltab) / sizeof(struct keytab));

struct keytab fntab[] = {   		/* File naming */
    "converted", 1, 0,
    "literal",   0, 0
};

/* Terminal parameters table */
struct keytab trmtab[] = {
#ifdef OS2
    "arrow-keys",    XYTARR, 0,
#endif /* OS2 */
    "bytesize",      XYTBYT, 0,
#ifndef NOCSETS
    "character-set", XYTCS,  0,
#endif /* NOCSETS */
#ifdef OS2
    "color",         XYTCOL, 0,
#endif /* OS2 */
    "cr-display",    XYTCRD, 0,
#ifdef OS2
    "cursor",        XYTCUR, 0,
#endif /* OS2 */
    "echo",          XYTEC,  0,
#ifdef OS2
    "keypad-mode",   XYTKPD, 0,
#endif /* OS2 */
    "locking-shift", XYTSO,  0,
    "newline-mode",  XYTNL,  0
#ifdef OS2
,   "type",          XYTTYP, 0,
    "wrap",          XYTWRP, 0
#endif /* OS2 */
};
int ntrm = (sizeof(trmtab) / sizeof(struct keytab));

struct keytab crdtab[] = {		/* Carriage-return display */
    "crlf",        1, 0,
    "normal",      0, 0
};
extern int tt_crd;			/* Carriage-return display variable */

#ifdef OS2
/*
  OS/2 serial communication devices.
*/
struct keytab os2devtab[] = {
    "1",    1, CM_INV,			/* Invisible synonyms, like */
    "2",    2, CM_INV,			/* "set port 1" */
    "3",    3, CM_INV,
    "4",    4, CM_INV,
    "5",    5, CM_INV,
    "6",    6, CM_INV,
    "7",    7, CM_INV,
    "8",    8, CM_INV,
    "com1", 1, 0,			/* Real device names */
    "com2", 2, 0,
    "com3", 3, 0,
    "com4", 4, 0,
    "com5", 5, 0,
    "com6", 6, 0,
    "com7", 7, 0,
    "com8", 7, 0
};
int nos2dev = (sizeof(os2devtab) / sizeof(struct keytab));

/*
  Terminal parameters that can be set by SET commands.
  Used by the ck?con.c terminal emulator code.  
  For now, only use for OS/2.  Should add these for Macintosh.
*/
int tt_arrow = 1;			/* Arrow key mode */
int tt_keypad = 0;			/* Keypad mode */
int tt_wrap = 1;			/* Autowrap */
int tt_type = TT_VT102;			/* Terminal type */
int tt_cursor = 0;			/* Cursor type */
#endif /* OS2 */

#ifdef OS2
struct keytab akmtab[] = {		/* Arrow key mode */
    "application", 1, 0,
    "cursor",      0, 0
};
struct keytab kpmtab[] = {		/* Keypad mode */
    "application", 0, 0,
    "numeric",     1, 0
};

struct keytab ttycoltab[] = {		/* Items to be colored */
    "help",        4, 0,		/* Help screen */
    "normal",      0, 0,		/* Normal screen text */
    "reverse",     1, 0,		/* Reverse video */
    "status",      3, 0,		/* Status line */
    "underlined",  2, 0			/* Underlined text */
};
int ncolors = 5;

struct keytab ttyclrtab[] = {		/* Colors */
    "black",       0, 0,
    "blue",        1, 0,
    "brown",       6, 0,
    "cyan",        3, 0,
    "dgray",       8, 0,
    "green",       2, 0,
    "lblue",       9, 0,
    "lcyan",      11, 0,
    "lgray",       7, 0,
    "lgreen",     10, 0,
    "lmagenta",   13, 0,
    "lred",       12, 0,
    "magenta",     5, 0,
    "red",         4, 0,
    "white",      15, 0,
    "yellow",     14, 0
};
int nclrs = 16;

struct keytab ttycurtab[] = {
    "full",        2, 0,
    "half",        1, 0,
    "underline",   0, 0
};
int ncursors = 3;
#endif /* OS2 */

#ifdef OS2
struct keytab ttyptab[] = {
#ifdef COMMENT
/*
  Not supported yet...
  The idea is to let the console driver handle the escape sequences.
*/
    "none",    TT_NONE,  0,
#endif /* COMMENT */
#ifdef OS2PM
    "tek4014", TT_TEK40, 0,
#endif /* OS2PM */
    "vt102",   TT_VT102, 0,
    "vt52",    TT_VT52,  0
};
int nttyp = 2;
#endif /* OS2 */

/* #ifdef VMS */
struct keytab fbtab[] = {		/* Binary record types for VMS */
    "fixed",     XYFT_B, 0,		/* Fixed is normal for binary */
    "undefined", XYFT_U, 0		/* Undefined if they ask for it */
};
int nfbtyp = (sizeof(fbtab) / sizeof(struct keytab));
/* #endif */

#ifdef VMS
struct keytab lbltab[] = {		/* Labeled File info */
    "acl",         LBL_ACL, 0,
    "backup-date", LBL_BCK, 0,
    "name",        LBL_NAM, 0,
    "owner",       LBL_OWN, 0,
    "path",        LBL_PTH, 0
};
int nlblp = (sizeof(lbltab) / sizeof(struct keytab));
#endif /* VMS */

struct keytab fdtab[] = {		/* SET FILE DISPLAY options */
#ifdef MAC				/* Macintosh */
    "fullscreen", XYFD_R, 0,		/* Full-screen but not curses */
    "none",   XYFD_N, 0,
    "off",    XYFD_N, CM_INV,
    "on",     XYFD_R, CM_INV,
    "quiet",  XYFD_N, CM_INV

#else					/* Not Mac */
    "crt", XYFD_S, 0,			/* CRT display */
#ifdef CK_CURSES
#ifdef COMMENT
    "curses",     XYFD_C, CM_INV,	/* Full-screen, curses */
#endif /* COMMENT */
    "fullscreen", XYFD_C, 0,		/* Full-screen, whatever the method */
#endif /* CK_CURSES */
    "none",   XYFD_N, 0,		/* No display */
    "off",    XYFD_N, CM_INV,		/* Ditto */
    "on",     XYFD_R, CM_INV,		/* On = Serial */
    "quiet",  XYFD_N, CM_INV,		/* No display */
    "serial", XYFD_R, 0			/* Serial */
#endif /* MAC */
};
int nfdtab = (sizeof(fdtab) / sizeof(struct keytab));

struct keytab rsrtab[] = {		/* For REMOTE SET RECEIVE */
    "packet-length", XYLEN, 0,
    "timeout", XYTIMO, 0
};
int nrsrtab = (sizeof(rsrtab) / sizeof(struct keytab));

/* Send/Receive Parameters */
 
struct keytab srtab[] = {
    "end-of-packet", XYEOL, 0,
    "packet-length", XYLEN, 0,
    "pad-character", XYPADC, 0,
    "padding", XYNPAD, 0,
    "start-of-packet", XYMARK, 0,
    "timeout", XYTIMO, 0
};
int nsrtab = (sizeof(srtab) / sizeof(struct keytab));

/* REMOTE SET */

struct keytab rmstab[] = {
    "attributes",  XYATTR, 0,
    "block-check", XYCHKT, 0,
    "file",        XYFILE, 0,
    "incomplete",  XYIFD,  0,
    "receive",     XYRECV, 0,
    "retry",       XYRETR, 0,
    "server",      XYSERV, 0,
    "transfer",    XYXFER, 0,
    "window",      XYWIND, 0
};
int nrms = (sizeof(rmstab) / sizeof(struct keytab));

struct keytab attrtab[] = {
    "all",           AT_XALL, 0,
#ifdef COMMENT
    "blocksize",     AT_BLKS, 0,	/* not used */
#endif /* COMMENT */
    "character-set", AT_ENCO, 0,
    "date",          AT_DATE, 0,
    "disposition",   AT_DISP, 0,
    "encoding",      AT_ENCO, CM_INV,
    "length",        AT_LENK, 0,
    "off",           AT_ALLN, 0,
    "on",            AT_ALLY, 0,
#ifdef COMMENT
    "os-specific",   AT_SYSP, 0,	/* not used by UNIX or VMS */
#endif /* COMMENT */
    "system-id",     AT_SYSI, 0,
    "type",          AT_FTYP, 0,
};
int natr = (sizeof(attrtab) / sizeof(struct keytab)); /* how many attributes */

#ifndef NOSPL
extern int indef, intime, incase, inecho, insilence;
struct keytab inptab [] = {		/* SET INPUT parameters */
    "case",            IN_CAS, 0,
    "default-timeout", IN_DEF, CM_INV,
    "echo",            IN_ECH, 0,
    "silence",         IN_SIL, 0,
    "timeout-action",  IN_TIM, 0
};
int ninp = (sizeof(inptab) / sizeof(struct keytab));

struct keytab intimt[] = {		/* SET INPUT TIMEOUT parameters */
    "proceed", 0, 0,			/* 0 = proceed */
    "quit",    1, 0			/* 1 = quit */
};

struct keytab incast[] = {		/* SET INPUT CASE parameters */
    "ignore",  0, 0,			/* 0 = ignore */
    "observe", 1, 0			/* 1 = observe */
};
#endif /* NOSPL */

/* The following routines broken out of doprm() to give compilers a break. */

/*  S E T O N  --  Parse on/off (default on), set parameter to result  */
 
int
seton(prm) int *prm; {
    int x, y;
    if ((y = cmkey(onoff,2,"","on",xxstring)) < 0) return(y);
    if ((x = cmcfm()) < 0) return(x);
    *prm = y;
    return(1);
}
 
/*  S E T N U M  --  Set parameter to result of cmnum() parse.  */
/*
 Call with pointer to integer variable to be set,
   x = number from cnum parse, y = return code from cmnum,
   max = maximum value to accept, -1 if no maximum.
 Returns -9 on failure, after printing a message, or 1 on success.
*/
int
setnum(prm,x,y,max) int x, y, *prm, max; {
    extern int cmflgs;
    debug(F101,"setnum","",y);
    if (y == -3) {
	printf("\n?Value required\n");
	return(-9);
    }
    if (y == -2) {
	printf("%s?Not a number: %s\n",cmflgs == 1 ? "" : "\n", atxbuf);
	return(-9);
    }
    if (y < 0) return(y);
    if (max > -1 && x > max) {
	printf("?Sorry, %d is the maximum\n",max);
	return(-9);
    }
    if ((y = cmcfm()) < 0) return(y);
    *prm = x;
    return(1);
}
 
/*  S E T C C  --  Set parameter to an ASCII control character value.  */
 
int
setcc(prm,x,y) int x, y, *prm; {
    if (y == -3) {
	printf("\n?Value required\n");
	return(-3);
    }
    if (y < 0) return(y);
    if ((x > 037) && (x != 0177)) {
	printf("\n?Not in ASCII control range - %d\n",x);
	return(-2);
    }
    if ((y = cmcfm()) < 0) return(y);
    *prm = x;
    return(1);
}

#ifndef NODIAL
_PROTOTYP(static int dialstr,(char **, char *));
/*
  Parse a DIAL-related string, stripping enclosing braces, if any.
*/
static int
dialstr(p,msg) char **p; char *msg; {
    int x;
    if ((x = cmtxt(msg, "", &s, xxstring)) < 0)
      return(x);
    if (*s == '{') {			/* Strip enclosing braces, if any */
	x = strlen(s);
	if (s[x-1] == '}') {
	    s[x-1] = NUL;
	    s++;
	}
    }
    if (*p) {				/* Free any previous string. */
	free(*p);
	*p = (char *) 0;
    }	
    if ((x = strlen(s)) > 0) {
	*p = malloc(x + 1);		/* Allocate space for it */
	strcpy(*p,s);			/* and make a safe copy. */
    } else *p = (char *) 0;
    return(success = 1);
}

int					/* Set DIAL command options */
setdial() {
    if ((y = cmkey(dialtab,ndial,"","",xxstring)) < 0) return(y);
    switch (y) {
      case XYDHUP:			/* DIAL HANGUP */
	return(success = seton(&dialhng));
      case XYDINI:			/* DIAL INIT-STRING */
	return(dialstr(&dialini,"Modem dialer initialization string"));
      case XYDNPR:			/* DIAL NUMBER-PREFIX */
	return(dialstr(&dialnpr,"Modem dialer telephone number prefix"));
      case XYDDIA:			/* DIAL DIAL-COMMAND */
	x = cmtxt("Dialing command for modem,\n\
 include \"%s\" to stand for phone number,\n\
 for example, \"set dial dial-command ATDT%s\\13\"",
		  "",
		  &s,
		  xxstring);
	if (x < 0 && x != -3)		/* Handle parse errors */
	  return(x);
	y = x = strlen(s);		/* Get length of text */
	if (x > 0 && *s == '{') {	/* Strip enclosing braces, */
	    if (s[x-1] == '}') {	/* if any. */
		s[x-1] = NUL;
		s++;
		y -= 2;
	    }
	}
	if (y > 0) {			/* If there is any text (left), */
	    for (x = 0; x < y; x++) {	/* make sure they included "%s" */
		if (s[x] != '%') continue;
		if (s[x+1] == 's') break;
	    }
	    if (x == y) {
		printf(
"?Dial-command must contain \"%cs\" for phone number.\n",'%');
		return(-9);
	    }
	}
	if (dialcmd) {			/* Free any previous string. */
	    free(dialcmd);
	    dialcmd = (char *) 0;
	}	
	if (y > 0) {
	    dialcmd = malloc(y + 1);	/* Allocate space for it */
	    strcpy(dialcmd,s);		/* and make a safe copy. */
	}
	return(success = 1);
      case XYDKSP:			/* DIAL KERMIT-SPOOF */
	return(success = seton(&dialksp));
      case XYDTMO:			/* DIAL TIMEOUT */
	y = cmnum("Seconds to wait for call completion","0",10,&x,xxstring);
	return(success = setnum(&dialtmo,x,y,10000));
      case XYDDPY:			/* DIAL DISPLAY */
	return(success = seton(&dialdpy));
      case XYDSPD:			/* DIAL SPEED-MATCHING */
					/* used to be speed-changing */
	if ((y = seton(&mdmspd)) < 0) return(y);
#ifdef COMMENT
	mdmspd = 1 - mdmspd;		/* so here we reverse the meaning */
#endif /* COMMENT */
	return(success = 1);
      case XYDMNP:			/* DIAL MNP-ENABLE */
	return(success = seton(&dialmnp));
#ifdef MDMHUP
      case XYDMHU:			/* DIAL MODEM-HANGUP */
	return(success = seton(&dialmhu));
#endif /* MDMHUP */
      case XYDDIR:			/* DIAL DIRECTORY */
	if ((y = cmifi("Name of dialing directory file","",&s,&y,
		       xxstring)) < 0) {
	    if (y == -3) {
		if (dialdir) free(dialdir);	/* Free any previous storage */
		dialdir = NULL;
		return(success = 1);
	    } else return(y);
	}
	if (y) {
	    printf("?Wildcards not allowed\n");
	    return(-9);
	}
	strcpy(line,s);			/* Make safe copy of dial string */
	if ((y = cmcfm()) < 0) return(y); /* Confirm the command */
	if (dialdir) free(dialdir);	/* Free previous filename storage */
	if (dialfd) {			/* Close previous file, if any */
	    fclose(dialfd);
	    dialfd = NULL;
	}
	s = line;			/* Point back to dial string */
	if (s == NULL || *s == NUL) {	/* If no name given */
	    dialdir = NULL;		/* remove the name string */
	    return(success = 1);
	} else if ((dialdir = malloc(strlen(s)+1)) == NULL) {
	    return(success = 0);	/* Can't malloc storage for name */
	} else {			/* Have storage for name */
	    strcpy(dialdir,s);		/* Copy string into new storage */
	    if ((dialfd = fopen(dialdir,"r")) == NULL) { /* Open the file */
		perror(dialdir);	/* Can't, print message saying why */
		success = 0;		/* We didn't succeed */
		return(-9);		/* Fail, message already printed */
	    }
	    return(success = 1);	/* Everything is OK */
	}

      default:
	printf("?Unexpected SET DIAL parameter\n");
	return(-2);
    }
}
#endif /* NODIAL */

int
setfil(rmsflg) int rmsflg; {
    if (rmsflg) {
	if ((y = cmkey(rfiltab,nrfilp,"Remote file parameter","",
		       xxstring)) < 0) {
	    if (y == -3) {
		printf("?Remote file parameter required\n");
		return(-9);
	    } else return(y);
	}
    } else {
	if ((y = cmkey(filtab,nfilp,"File parameter","",xxstring)) < 0)
	  return(y);
    }
    switch (y) {
#ifdef COMMENT				/* Not needed */
      case XYFILB:			/* Blocksize */
	sprintf(tmpbuf,"%d",DBLKSIZ);
	if ((y = cmnum("file block size",tmpbuf,10,&z,xxstring)) < 0)
	  return(y);
	if ((x = cmcfm()) < 0) return(x);
	if (rmsflg) {
	    sprintf(tmpbuf,"%d",z);
	    sstate = setgen('S', "311", tmpbuf, "");
	    return((int) sstate);
	} else {
	    fblksiz = z;
	    return(success = 1);
	}
#endif /* COMMENT */

      case XYFILS:			/* Byte size */
	if ((y = cmnum("file byte size (7 or 8)","8",10,&z,xxstring)) < 0)
	  return(y);
	if (z != 7 && z != 8) {
	    printf("\n?The choices are 7 and 8\n");
	    return(0);
	}
	if ((y = cmcfm()) < 0) return(y);
	if (z == 7) fmask = 0177;
	else if (z == 8) fmask = 0377;
	return(success = 1);

#ifndef NOCSETS
      case XYFILC:			/* Character set */
	if ((x = cmkey(fcstab,nfilc,"local file code","ascii", xxstring)) < 0)
	  return(x);
	if ((z = cmcfm()) < 0) return(z);
	fcharset = x;
	return(success = 1);
#endif /* NOCSETS */

      case XYFILD:			/* Display */
	if ((x = cmkey(fdtab,nfdtab,"file transfer display style","",
		       xxstring)) < 0)
	  return(x);
	if ((z = cmcfm()) < 0) return(z);
#ifdef CK_CURSES
	if (x == XYFD_C) {
#ifdef VMS
	    lp = line;
#else
	    lp = trmbuf;
#endif /* VMS */
#ifndef MYCURSES
	    s = getenv("TERM");
	    if (tgetent(lp,s) < 1) {
#ifdef VMS
		printf("Sorry, terminal type not supported: %s\n",s);
#else
		printf("Sorry, terminal type unknown: %s\n",s);
#endif /* VMS */
		return(success = 0);
	    }
#endif /* MYCURSES */
	    line[0] = '\0';
	}
#endif /* CK_CURSES */
	fdispla = x;			/* It's OK. */
	return(success = 1);

      case XYFILN:			/* Names */
	if ((x = cmkey(fntab,2,"how to handle filenames","converted",
		       xxstring)) < 0)
	  return(x);
	if ((z = cmcfm()) < 0) return(z);
	fncnv = x;
	return(success = 1);

      case XYFILR:			/* Record length */
	sprintf(tmpbuf,"%d",DLRECL);
	if ((y = cmnum("file record length",tmpbuf,10,&z,xxstring)) < 0)
	  return(y);
	if ((x = cmcfm()) < 0) return(x);
	if (rmsflg) {
	    sprintf(tmpbuf,"%d",z);
	    sstate = setgen('S', "312", tmpbuf, "");
	    return((int) sstate);
	} else {
	    frecl = z;
	    return(success = 1);
	}

#ifdef COMMENT
      case XYFILO:			/* Organization */
	if ((x = cmkey(forgtab,nforg,"file organization","sequential",
		       xxstring)) < 0)
	  return(x);
	if ((y = cmcfm()) < 0) return(y);
	if (rmsflg) {
	    sprintf(tmpbuf,"%d",x);
	    sstate = setgen('S', "314", tmpbuf, "");
	    return((int) sstate);
	} else {
	    forg = x;
	    return(success = 1);
	}	
#endif /* COMMENT */

#ifdef COMMENT				/* Not needed */
      case XYFILF:			/* Format */
	if ((x = cmkey(frectab,nfrec,"file record format","stream",
		       xxstring)) < 0)
	  return(x);
	if ((y = cmcfm()) < 0) return(y);
	if (rmsflg) {
	    sprintf(tmpbuf,"%d",x);
	    sstate = setgen('S', "313", tmpbuf, "");
	    return((int) sstate);
	} else {
	    frecfm = x;
	    return(success = 1);
	}
#endif /* COMMENT */

#ifdef COMMENT
      case XYFILP:			/* Printer carriage control */
	if ((x = cmkey(fcctab,nfcc,"file carriage control","newline",
		       xxstring)) < 0)
	  return(x);
	if ((y = cmcfm()) < 0) return(y);
	if (rmsflg) {
	    sprintf(tmpbuf,"%d",x);
	    sstate = setgen('S', "315", tmpbuf, "");
	    return((int) sstate);
	} else {
	    fcctrl = x;
	    return(success = 1);
	}	
#endif /* COMMENT */

      case XYFILT:			/* Type */
	if ((x = cmkey(fttab,nfttyp,"type of file","text",xxstring)) < 0)
	  return(x);
#ifdef COMMENT
	if ((y = cmnum("file byte size (7 or 8)","8",10,&z,xxstring)) < 0)
	  return(y);
	if (z != 7 && z != 8) {
	    printf("\n?The choices are 7 and 8\n");
	    return(0);
	}
#endif /* COMMENT */

#ifdef VMS
        /* Allow VMS users to choose record format for binary files */
        if ((x == XYFT_B) && (rmsflg == 0)) {
	    if ((x = cmkey(fbtab,nfbtyp,"VMS record format","fixed",
			   xxstring)) < 0)
	      return(x);
	}
#endif /* VMS */
	if ((y = cmcfm()) < 0) return(y);
	if (rmsflg) {
	    sstate = setgen('S', "300", x ? "1" : "0", "");
	    return((int) sstate);
	} else {
	    binary = x;
#ifdef COMMENT
	    if (z == 7) fmask = 0177;
	    else if (z == 8) fmask = 0377;
#endif /* COMMENT */
	    return(success = 1);
	}

      case XYFILX:			/* Collision Action */
	if ((x = cmkey(colxtab,ncolx,"Filename collision action","backup",
		       xxstring)) < 0)
	  return(x);
	if ((y = cmcfm()) < 0) return(y);
	fncact = x;
	if (rmsflg) {
	    sprintf(tmpbuf,"%d",fncact);
	    sstate = setgen('S', "302", tmpbuf, "");
	    return((int) sstate);
	} else {
	    if (fncact == XYFX_R) warn = 1; /* SET FILE WARNING implications */
	    if (fncact == XYFX_X) warn = 0; /* ... */
	    return(success = 1);
	}

      case XYFILW:			/* Warning/Write-Protect */
	seton(&warn);
	if (warn)
	  fncact = XYFX_R;
	else
	  fncact = XYFX_X;
	return(success = 1);

#ifdef VMS
      case XYFILL:			/* LABELED FILE parameters */
	if ((x = cmkey(lbltab,nlblp,"VMS labeled file feature","",
		       xxstring)) < 0)
	  return(x);
	if ((success = seton(&y)) < 0)
	  return(success);
	if (y)				/* Set or reset the selected bit */
	  lf_opts |= x;			/* in the options bitmask. */
	else
	  lf_opts &= ~x;
	return(success);
#endif /* VMS */

      case XYFILI:			/* INCOMPLETE */
	return(doprm(XYIFD,rmsflg));

      default:
	printf("?unexpected file parameter\n");
	return(-2);
    }
}

int
settrm() {
    if ((y = cmkey(trmtab,ntrm,"","",xxstring)) < 0) return(y);
#ifdef MAC
    printf("\n?Sorry, not implemented yet.  Please use the Settings menu.\n");
    return(-9);
#else
    switch (y) {
      case XYTBYT:			/* SET TERMINAL BYTESIZE */
	if ((y = cmnum("bytesize for terminal connection","8",10,&x,
		       xxstring)) < 0)
	  return(y);
	if (x != 7 && x != 8) {
	    printf("\n?The choices are 7 and 8\n");
	    return(success = 0);
	}
	if ((y = cmcfm()) < 0) return(y);
	if (x == 7) cmask = 0177;
	else if (x == 8) cmask = 0377;
        return(success = 1);

      case XYTSO:			/* SET TERMINAL LOCKING-SHIFT */
	return(success = seton(&sosi));

      case XYTNL:			/* SET TERMINAL NEWLINE-MODE */
	return(success = seton(&tnlm)); 

#ifdef OS2
      case XYTCOL:			/* SET TERMINAL COLOR */
	if ((x = cmkey(ttycoltab,ncolors,"","normal",xxstring)) < 0) {
	    return(x);
        } else {
	    extern int colornormal, colorreverse, colorunderline,
	      colorstatus, colorhelp, scrninitialised;
	    int fg, bg;
	    if ((fg = cmkey(ttyclrtab,nclrs,
			    "foreground color","black",xxstring)) < 0)
	      return(fg);
	    if ((bg = cmkey(ttyclrtab,nclrs,
			    "background color","cyan",xxstring)) < 0)
	      return(bg);
	    if ((y = cmcfm()) < 0)
	      return(y);
	    switch (x) {
	      case 0:
		colornormal = fg | bg << 4;
		break;
	      case 1:
		colorreverse = fg | bg << 4;
		break;
	      case 2:
		colorunderline = fg | bg << 4;
		break;
	      case 3:
		colorstatus = fg | bg << 4;
		break;
	      case 4:
		colorhelp = fg | bg << 4;
		break;
	      default:
		printf("%s - invalid\n",cmdbuf);
		return(-9);
		break;
	    }
	    scrninitialised = 0;
        }
	return(success = 1);

      case XYTCUR:			/* SET TERMINAL CURSOR */
	if ((x = cmkey(ttycurtab,ncursors,"","underline",xxstring)) < 0)
	  return(x);
	if ((y = cmcfm()) < 0) return(y);
        tt_cursor = x;
	return(success = 1);

      case XYTTYP:			/* SET TERMINAL TYPE */
	if ((x = cmkey(ttyptab,nttyp,"","vt102",xxstring)) < 0) return(x);
	if ((y = cmcfm()) < 0) return(y);
	tt_type = x;
	return(success = 1);

      case XYTARR:			/* SET TERMINAL ARROW-KEYS */
	if ((x = cmkey(akmtab,2,"","",xxstring)) < 0) return(x);
	if ((y = cmcfm()) < 0) return(y);
	tt_arrow = x;			/* 0 = application, 1 = cursor */
	return(success = 1);

      case XYTKPD:			/* SET TERMINAL KEYPAD-MODE */
	if ((x = cmkey(kpmtab,2,"","",xxstring)) < 0) return(x);
	if ((y = cmcfm()) < 0) return(y);
	tt_keypad = x;			/* 0 = application, 1 = numeric */
	return(success = 1);

      case XYTWRP:			/* SET TERMINAL WRAP */
	seton(&tt_wrap);
	return(success = 1);	
#endif /* OS2 */

#ifndef NOCSETS
      case XYTCS:			/* SET TERMINAL CHARACTER-SET */
	/* set terminal character-set <remote> <local> */
	if ((x = cmkey(ttcstab,ntermc,
		       "remote terminal character-set","",xxstring)) < 0) 
	  return(x);
	if (x == FC_TRANSP) {		/* TRANSPARENT? */
	    if ((x = cmcfm()) < 0) return(x); /* Confirm the command */
	    tcsr = tcsl = FC_USASCII;	/* Make them both the same */
	    return(success = 1);
	}

/* Not transparent, so get local set to translate it into */

	s = "";				/* Make current file char set */
	for (y = 0; y <= nfilc; y++)	/* be the default... */
	  if (fcstab[y].kwval == fcharset) {
	      s = fcstab[y].kwd;
	      break;
	    }
	if ((y = cmkey(fcstab,nfilc,
		       "local character-set",s,xxstring)) < 0)
	  return(y);
	if ((z = cmcfm()) < 0) return(z); /* Confirm the command */
	tcsr = x;			/* Remote character set */
	tcsl = y;			/* Local character set */
	return(success = 1);
#endif /* NOCSETS */

      case XYTEC:			/* SET TERMINAL ECHO */
	if ((x = cmkey(rltab,nrlt,"which side echos during CONNECT",
		       "remote", xxstring)) < 0) return(x);
	if ((y = cmcfm()) < 0) return(y);
	duplex = x;
	return(success = 1); 

      case XYTCRD:			/* SET TERMINAL CR-DISPLAY */
	if ((x = cmkey(crdtab,2,"", "normal", xxstring)) < 0) return(x);
	if ((y = cmcfm()) < 0) return(y);
	tt_crd = x;
	return(success = 1); 

      default:				/* Shouldn't get here. */
	return(-2);
    }    
#endif /* MAC */
}

int					/* SET SEND/RECEIVE */
setsr(xx, rmsflg) int xx; int rmsflg; {
    if (xx == XYRECV)
    	strcpy(line,"Parameter for inbound packets");
    else
    	strcpy(line,"Parameter for outbound packets");
 
    if (rmsflg) {
	if ((y = cmkey(rsrtab,nrsrtab,line,"",xxstring)) < 0) {
	    if (y == -3) {
		printf("?Remote receive parameter required\n");
		return(-9);
	    } else return(y);
	}
    } else {
	if ((y = cmkey(srtab,nsrtab,line,"",xxstring)) < 0) return(y);
    }
    switch (y) {
 
      case XYEOL:
	y = cmnum("Decimal ASCII code for packet terminator","13",10,&x,
		  xxstring);
	if ((y = setcc(&z,x,y)) < 0) return(y);
	if (xx == XYRECV) eol = z; else seol = z;
	return(success = y);
 
      case XYLEN:
	y = cmnum("Maximum number of characters in a packet","90",10,&x,
		  xxstring);
	if (xx == XYRECV) {		/* Receive... */
	    if ((y = setnum(&z,x,y,maxrps)) < 0)
	      return(y);
	    if (z < 10) {
		printf("Sorry, 10 is the minimum\n");
		return(-9);
	    }
	    if (rmsflg) {
		tp = tmpbuf;
		sprintf(tp,"%d",z);
		sstate = setgen('S', "401", tp, "");
		return((int) sstate);
	    } else {
		if (z > MAXRP) z = MAXRP;
		y = adjpkl(z,wslotr,bigrbsiz);
		if (y != z) {
		    urpsiz = y;
		    if (
#ifndef NOSPL
			cmdlvl == 0
#else
			tlevel < 0
#endif /* NOSPL */
			)
		      if (msgflg) printf(
" Adjusting receive packet-length to %d for %d window slots\n",
			 y, wslotr);
		}
		urpsiz = y;
		rpsiz =  (y > 94) ? 94 : y;
	    }
	} else {			/* Send... */
	    if ((y = setnum(&z,x,y,maxsps)) < 0)
	      return(y);
	    if (z < 10) {
		printf("Sorry, 10 is the minimum\n");
		return(-9);
	    }
	    if (z > MAXSP) z = MAXSP;
	    spsiz = z;			/* Set it */
	    y = adjpkl(spsiz,wslotr,bigsbsiz);
	    if (y != spsiz &&
#ifndef NOSPL
		cmdlvl == 0
#else
		tlevel < 0
#endif /* NOSPL */
		)
	      if (msgflg)
		printf("Adjusting packet size to %d for %d window slots\n",
		     y,wslotr);
	    spsiz = spmax = spsizr = y;	/* Set it and flag that it was set */
	    spsizf = 1;			/* to allow overriding Send-Init. */
	}
	if (pflag &&
#ifndef NOSPL
	    cmdlvl == 0
#else
	    tlevel < 0
#endif /* NOSPL */
	    ) {
	    if (z > 94 && msgflg) {
		printf("Extended-length packets requested.\n");
		if (bctr < 2 && z > 200) printf("\
Remember to SET BLOCK 2 or 3 for long packets.\n");
	    }
	    if (speed <= 0L) speed = ttgspd();
#ifdef COMMENT
/*
  Kermit does this now itself.
*/
	    if (speed <= 0L && z > 200 && msgflg) {
		printf("\
Make sure your timeout interval is long enough for %d-byte packets.\n",z);
	    }
#endif /* COMMENT */
	}
	return(success = y);

      case XYMARK:
	y = cmnum("Code for packet-start character","1",10,&x,xxstring);
#ifdef UNIX
/*
  Printable start-of-packet works for UNIX and VMS only!
*/
	if ((y = setnum(&z,x,y,126)) < 0) return(y);
#else
#ifdef VMS
	if ((y = setnum(&z,x,y,126)) < 0) return(y);
#else
	if ((y = setcc(&z,x,y)) < 0) return(y);
#endif /* VMS */
#endif /* UNIX */
	if (xx == XYRECV) stchr = z; else mystch = z;
	return(success = y);


      case XYNPAD:			/* PADDING */
	y = cmnum("How many padding characters for inbound packets","0",10,&x,
		  xxstring);
	if ((y = setnum(&z,x,y,94)) < 0) return(y);
	if (xx == XYRECV) mypadn = z; else npad = z;
	return(success = y);
 
      case XYPADC:			/* PAD-CHARACTER */
	y = cmnum("Decimal ASCII code for packet padding character","0",10,&x,
		  xxstring);
	if ((y = setcc(&z,x,y)) < 0) return(y);
	if (xx == XYRECV) mypadc = z; else padch = z;
	return(success = y);
 
      case XYTIMO:			/* TIMEOUT */
	if (xx == XYRECV) {
	    char buf[16];		/* Construct default */
	    sprintf(buf,"%d",URTIME);
	    y = cmnum("Packet timeout interval",buf,10,&x,xxstring);
	    if ((y = setnum(&z,x,y,94)) < 0) return(y);

	    if (rmsflg) {		/* REMOTE SET RECEIVE TIMEOUT */
		tp = tmpbuf;		/*   Tell Kermit server what */
		sprintf(tp,"%d",z);	/*   timeout to ask me to use. */
		sstate = setgen('S', "402", tp, "");
		return((int) sstate);
	    } else {			/* SET RECEIVE TIMEOUT */
		pkttim = z;		/*   Value to put in my negotiation */
	    }				/*   packet for other Kermit to use */

	} else {			/* SET SEND TIMEOUT */
	    y = cmnum("Packet timeout interval","",10,&x,xxstring);
	    if (y == -3) {		/* They cancelled a previous */
		x = DMYTIM;		/* SET SEND command, so restore */
		y = 0;			/* the default */
		timef = 0;		/* and turn off the override flag */
	    } else {			/* They gave a number */
		timef = 1;		/* so turn on the override flag */
	    }
	    if ((y = setnum(&z,x,y,94)) < 0)
	      return(y);
	    timint = rtimo = z;		/* Override value for me to use */
	}
	return(success = 1);

      default:
	return(-2);
    }					/* End of SET SEND/RECEIVE... */
}

#ifndef NOXMIT
int
setxmit() {
    if ((y = cmkey(xmitab,nxmit,"","",xxstring)) < 0) return(y);
    switch (y) {
      case XMITE:			/* EOF */
	y = cmtxt("Characters to send at end of file,\n\
 Use backslash codes for control characters","",&s,xxstring);
	if (y < 0) return(y);
	if ((int)strlen(s) > XMBUFL) {
	    printf("?Too many characters, %d maximum\n",XMBUFL);
	    return(-2);
	}
	strcpy(xmitbuf,s);
	return(success = 1);

      case XMITF:			/* Fill */
	y = cmnum("Numeric code for blank-line fill character","0",10,&x,
		  xxstring);
	if ((y = setnum(&z,x,y,127)) < 0) return(y);
	xmitf = z;
	return(success = 1);
      case XMITL:			/* Linefeed */
        return(success = seton(&xmitl));
      case XMITS:			/* Locking-Shift */
        return(success = seton(&xmits));
      case XMITP:			/* Prompt */
	y = cmnum("Numeric code for host's prompt character, 0 for none",
		  "10",10,&x,xxstring);
	if ((y = setnum(&z,x,y,127)) < 0) return(y);
	xmitp = z;
	return(success = 1);
      case XMITX:			/* Echo */
        return(success = seton(&xmitx));
      case XMITW:			/* Pause */
	y = cmnum("Number of milliseconds to pause between binary characters\n\
or text lines during transmission","0",10,&x,xxstring);
	if ((y = setnum(&z,x,y,1000)) < 0) return(y);
	xmitw = z;
	return(success = 1);
      default:
	return(-2);
    }
}
#endif /* NOXMIT */

/*  D O R M T  --  Do a remote command  */
 
#ifdef ATTSV
#ifndef aegis
#ifndef datageneral
#define CK_NEED_SIG
#endif /* datageneral */
#endif /* aegis */
#endif /* ATTSV */

VOID rmsg() {
    if (pflag)
      printf(
#ifdef CK_NEEDSIG

       " Type your escape character, %s, followed by X or E to cancel.\n",
       dbchr(escape)
#else
       " Press the X or E key to cancel.\n"
#endif /* CK_NEEDSIG */
      );
}

int
dormt(xx) int xx; {
    int x, y, retcode;
    char *s, sbuf[50], *s2;

 
    if (xx < 0) return(xx);

    if (xx == XZSET) {			/* REMOTE SET */
	if ((y = cmkey(rmstab,nrms,"","",xxstring)) < 0) {
	    if (y == -3) {
		printf("?Parameter name required\n");
		return(-9);
	    } else return(y);
	}
	return(doprm(y,1));
    }

    switch (xx) {			/* Others... */
 
case XZCWD:				/* CWD */
    if ((x = cmtxt("Remote directory name","",&s,xxstring)) < 0) return(x);
    debug(F111,"XZCWD: ",s,x);
    *sbuf = NUL;
    s2 = sbuf;

/* The following is commented out, because there is practically no */
/* computer in the world that requires a password for directory changing. */
/* (The DEC-20 was the only one, and they're mostly all gone.) */
#ifdef DIRPWDPR
    if (*s != NUL) {			/* If directory name given, */
					/* get password on separate line. */
        if (tlevel > -1) {		/* From take file... */
 
	    if (fgets(sbuf,50,tfile[tlevel]) == NULL)
	    	fatal("take file ends prematurely in 'remote cwd'");
	    debug(F110," pswd from take file",s2,0);
	    for (x = (int)strlen(sbuf);
	     	 x > 0 && (sbuf[x-1] == NL || sbuf[x-1] == CR);
		 x--)
		sbuf[x-1] = '\0';
 
        } else {			/* From terminal... */
 
	    printf(" Password: "); 		/* get a password */
#ifdef OS2
	    while (((x = isatty(0) ? coninc(0) :
		     getchar()) != NL) && (x != CR)) {     /* with no echo */
#else
	    while (((x = getchar()) != NL) && (x != CR)) { /* with no echo */
#endif /* OS2 */
	    	if ((x &= 0177) == '?') {
	    	    printf("? Password of remote directory\n Password: ");
		    s2 = sbuf;
		    *sbuf = NUL;
	    	}
	    	else if (x == ESC)	/* Mini command line editor... */
	    	    putchar(BEL);
		else if (x == BS || x == 0177)
		    s2--;
		else if (x == 025) {	/* Ctrl-U */
		    s2 = sbuf;
		    *sbuf = NUL;
		}
	    	else
		    *s2++ = x;
            }
	    *s2 = NUL;
	    putchar('\n');
        }
        s2 = sbuf;
    } else s2 = "";
#endif /* DIRPWDPR */

    debug(F110," password",s2,0);
    sstate = setgen('C',s,s2,"");
    retcode = 0;
    break;

case XZDEL:				/* Delete */
    if ((x = cmtxt("Name of remote file(s) to delete","",&s,xxstring)) < 0) {
	if (x == -3) {
	    printf("?Name of remote file(s) required\n");
	    return(-9);
	} else return(x);
    }
    if (local) ttflui();		/* If local, flush tty input buffer */
    retcode = sstate = rfilop(s,'E');
    break;
 
case XZDIR:				/* Directory */
    if ((x = cmtxt("Remote directory or file specification","",&s,
		   xxstring)) < 0)
    	return(x);
    if (local) ttflui();		/* If local, flush tty input buffer */
    rmsg();
    retcode = sstate = setgen('D',s,"","");
    break;
 
case XZHLP:				/* Help */
    if ((x = cmcfm()) < 0) return(x);
    sstate = setgen('H',"","","");
    retcode = 0;
    break; 

#ifndef NOPUSH
case XZHOS:				/* Host */
    if ((x = cmtxt("Command for remote system","",&cmarg,xxstring)) < 0)
      return(x);
    if ((int)strlen(cmarg) < 1)  {
	if (x == -3) {
	    printf("?Remote host command required\n");
	    return(-9);
	} else return(x);
    }
    rmsg();
    retcode = sstate = 'c';
    break; 
#endif /* NOPUSH */

#ifndef NOFRILLS
case XZKER:
    if ((x = cmtxt("Command for remote Kermit","",&cmarg,xxstring)) < 0)
      return(x);
    if ((int)strlen(cmarg) < 1)  {
	if (x == -3) {
	    printf("?Remote Kermit command required\n");
	    return(-9);
	} else return(x);
    }
    retcode = sstate = 'k';
    rmsg();
    break; 

case XZLGI: {				/* Login */
    char *p1, *p2, *p3;
    if ((x = cmfld("User ID","",&s,xxstring)) < 0) return(x);
    if ((p1 = malloc((int)strlen(s) + 1)) == NULL) {
	printf("Internal error: malloc\n");
	return(-2);
    } else strcpy(p1,s);
    if ((x = cmfld("Password","",&s,xxstring)) < 0) return(x);
    if ((p2 = malloc((int)strlen(s) + 1)) == NULL) {
	printf("Internal error: malloc\n");
	return(-2);
    } else strcpy(p2,s);
    if ((x = cmtxt("Account or carriage return","",
		   &s,xxstring)) < 0 && x != -3)
	return(x);
    if ((p3 = malloc((int)strlen(s) + 1)) == NULL) {
	printf("Internal error: malloc\n");
	return(-2);
    } else strcpy(p3,s);
    sstate = setgen('I',p1,p2,p3);
    if (p3) free(p3);
    if (p2) free(p2);
    if (p1) free(p1);
    retcode = 0;
    break; 
}

case XZLGO:				/* Logout */
    if ((x = cmcfm()) < 0) return(x);
    sstate = setgen('I',"","","");
    retcode = 0;
    break; 

case XZPRI:				/* Print */
    if (!atdiso || !atcapr) {		/* Disposition attribute off? */
	printf("?Disposition Attribute is Off\n");
	return(-2);
    }
    cmarg = "";
    cmarg2 = "";
    if ((x = cmifi("Local file(s) to print on remote printer","",&s,&y,
		   xxstring)) < 0) {
	if (x == -3) {
	    printf("?Name of local file(s) required\n");
	    return(-9);
	}
	return(x);
    }
    strcpy(line,s);			/* Make a safe copy of filename */
    *optbuf = NUL;			/* Wipe out any old options */
    if ((x = cmtxt("Options for remote print command","",&s,xxstring)) < 0)
      return(x);
    strcpy(optbuf,s);			/* Make a safe copy of options */
    if ((int)strlen(optbuf) > 94) {	/* Make sure this is legal */
	printf("?Option string too long\n");
	return(-9);
    }
    nfils = -1;				/* Expand file list internally */
    cmarg = line;			/* Point to file list. */
    rprintf = 1;			/* REMOTE PRINT modifier for SEND */
    sstate = 's';			/* Set start state to SEND */
    if (local) displa = 1;
    retcode = 0;
    break;
#endif /* NOFRILLS */
	
case XZSPA:				/* Space */
    if ((x = cmtxt("Confirm, or remote directory name","",&s,xxstring)) < 0)
      return(x);
    retcode = sstate = setgen('U',s,"","");
    break;
    
#ifndef NOFRILLS
case XZTYP:				/* Type */
    if ((x = cmtxt("Remote file specification","",&s,xxstring)) < 0)
      return(x);
    if ((int)strlen(s) < 1) {
	printf("?Remote filename required\n");
        return(-9);	
    }
    rmsg();
    retcode = sstate = rfilop(s,'T');
    break;
#endif /* NOFRILLS */
 
#ifndef NOFRILLS
case XZWHO:
    if ((x = cmtxt("Remote user name, or carriage return","",&s,xxstring)) < 0)
    	return(x);
    retcode = sstate = setgen('W',s,"","");
    break;
#endif /* NOFRILLS */
 
default:
        if ((x = cmcfm()) < 0) return(x);
        printf("?Not implemented - %s\n",cmdbuf);
        return(-2);
    }
    if (local) ttflui();		/* If local, flush tty input buffer */
    return(retcode);
}
 
 
/*  R F I L O P  --  Remote File Operation  */
 
CHAR
#ifdef CK_ANSIC
rfilop(char * s, char t)
#else
rfilop(s,t) char *s, t; 
#endif /* CK_ANSIC */
/* rfilop */ {
    if (*s == NUL) {
	printf("?File specification required\n");
	return((CHAR) 0);
    }
    debug(F111,"rfilop",s,t);
    return(setgen(t,s,"",""));
}

#ifdef SUNX25
int
setx25() {
    if ((y = cmkey(x25tab,nx25,"X.25 call options","",xxstring)) < 0)
      return(y);
    switch (y) {
      case XYUDAT:
	if ((z = cmkey(onoff,2,"X.25 call user data","",xxstring))
	    < 0) return(z);
	if (z == 0) {
	    if ((z = cmcfm()) < 0) return(z);
	    cudata = 0;             /* disable call user data */
	    return (success = 1);
	}
	if ((x = cmtxt("X.25 call user data string","",&s,xxstring)) < 0)
	  return(x);
	if ((int)strlen(s) == 0) {
	    return (-3);
	} else if ((int)strlen(s) > MAXCUDATA) {
	    printf("?The length must be > 0 and <= %d\n",MAXCUDATA);
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	strcpy(udata,s);
	cudata = 1;			/* X.25 call user data specified */
	return (success = 1);
      case XYCLOS:
	if ((z = cmkey(onoff,2,"X.25 closed user group call","",xxstring))
	    < 0) return(z);
	if (z == 0) {
	    if ((z = cmcfm()) < 0) return(z);
	    closgr = -1;		/* disable closed user group */
	    return (success = 1);
	}
	if ((y = cmnum("0 <= cug index >= 99","",10,&x,xxstring)) < 0)
	  return(y);
	if (x < 0 || x > 99) {
	    printf("?The choices are 0 <= cug index >= 99\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	closgr = x;			/* closed user group selected */
	return (success = 1);

      case XYREVC:
	if((z = cmkey(onoff,2,"X.25 reverse charge call","",xxstring)) < 0)
	  return(z);
	if ((x = cmcfm()) < 0) return(x);
	revcall = z;
	return (success = 1);
    }
}

int
setpadp() {
    if ((y = cmkey(padx3tab,npadx3,"PAD X.3 parameter name","",xxstring)) < 0)
      return(y);
    x = y;
    switch (x) {
      case PAD_BREAK_CHARACTER:
	if ((y = cmnum("PAD break character value","",10,&z,xxstring)) < 0)
	  return(y);
	if ((y = cmcfm()) < 0) return(y);
	break;
      case PAD_ESCAPE:
	if ((y = cmnum("PAD escape","",10,&z,xxstring)) < 0) return(y);
	if (z != 0 && z != 1) {
	    printf("?The choices are 0 or 1\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;
      case PAD_ECHO:
 	if ((y = cmnum("PAD echo","",10,&z,xxstring)) < 0) return(y);
	if (z != 0 && z != 1) {
	    printf("?The choices are 0 or 1\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;
      case PAD_DATA_FORWARD_CHAR:
 	if ((y = cmnum("PAD data forward char","",10,&z,xxstring)) < 0)
	  return(y);
	if (z != 0 && z != 2) {
	    printf("?The choices are 0 or 2\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;
      case PAD_DATA_FORWARD_TIMEOUT:
 	if ((y = cmnum("PAD data forward timeout","",10,&z,xxstring)) < 0)
	    return(y);
	if (z < 0 || z > 255) {
	    printf("?The choices are 0 or 1 <= timeout <= 255\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;
      case PAD_FLOW_CONTROL_BY_PAD:
 	if ((y = cmnum("PAD pad flow control","",10,&z,xxstring)) < 0)
	  return(y);
	if (z != 0 && z != 1) {
	    printf("?The choices are 0 or 1\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;
      case PAD_SUPPRESSION_OF_SIGNALS:
 	if ((y = cmnum("PAD service","",10,&z,xxstring)) < 0) return(y);
	if (z != 0 && z != 1) {
	    printf("?The choices are 0 or 1\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;

      case PAD_BREAK_ACTION:
 	if ((y = cmnum("PAD break action","",10,&z,xxstring)) < 0) return(y);
	if (z != 0 && z != 1 && z != 2 && z != 5 && z != 8 && z != 21) {
	    printf("?The choices are 0, 1, 2, 5, 8 or 21\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;

      case PAD_SUPPRESSION_OF_DATA:
 	if ((y = cmnum("PAD data delivery","",10,&z,xxstring)) < 0) return(y);
	if (z != 0 && z != 1) {
	    printf("?The choices are 0 or 1\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;

      case PAD_PADDING_AFTER_CR:
 	if ((y = cmnum("PAD crpad","",10,&z,xxstring)) < 0) return(y);
	if (z < 0 || z > 7) {
	    printf("?The choices are 0 or 1 <= crpad <= 7\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;

      case PAD_LINE_FOLDING:
 	if ((y = cmnum("PAD linefold","",10,&z,xxstring)) < 0) return(y);
	if (z < 0 || z > 255) {
	    printf("?The choices are 0 or 1 <= linefold <= 255\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;

      case PAD_LINE_SPEED:
 	if ((y = cmnum("PAD baudrate","",10,&z,xxstring)) < 0) return(y);
	if (z < 0 || z > 18) {
	    printf("?The choices are 0 <= baudrate <= 18\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;

      case PAD_FLOW_CONTROL_BY_USER:
 	if ((y = cmnum("PAD terminal flow control","",10,&z,xxstring)) < 0)
	    return(y);
	if (z != 0 && z != 1) {
	    printf("?The choices are 0 or 1\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;

      case PAD_LF_AFTER_CR:
 	if ((y = cmnum("PAD crpad","",10,&z,xxstring)) < 0) return(y);
	if (z < 0 || z == 3 || z > 7) {
	    printf("?The choices are 0, 1, 2, 4, 5, 6 or 7\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;

      case PAD_PADDING_AFTER_LF:
 	if ((y = cmnum("PAD lfpad","",10,&z,xxstring)) < 0) return(y);
	if (z < 0 || z > 7) {
	    printf("?The choices are 0 or 1 <= lfpad <= 7\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;

      case PAD_EDITING:
 	if ((y = cmnum("PAD edit control","",10,&z,xxstring)) < 0) return(y);
	if (z != 0 && z != 1) {
	    printf("?The choices are 0 or 1\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;

      case PAD_CHAR_DELETE_CHAR:
 	if ((y = cmnum("PAD char delete char","",10,&z,xxstring)) < 0)
	    return(y);
	if (z < 0 || z > 127) {
	    printf("?The choices are 0 or 1 <= chardelete <= 127\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;

      case PAD_BUFFER_DELETE_CHAR:
 	if ((y = cmnum("PAD buffer delete char","",10,&z,xxstring)) < 0)
	    return(y);
	if (z < 0 || z > 127) {
	    printf("?The choices are 0 or 1 <= bufferdelte <= 127\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;

      case PAD_BUFFER_DISPLAY_CHAR:
 	if ((y = cmnum("PAD display line char","",10,&z,xxstring)) < 0)
	    return(y);
	if (z < 0 || z > 127) {
	    printf("?The choices are 0 or 1 <= displayline <= 127\n");
	    return(-2);
	}
	if ((y = cmcfm()) < 0) return(y);
	break;
    }
    padparms[x] = z;
    return(success = 1);
}
#endif /* SUNX25 */ 

int
setat(rmsflg) int rmsflg; {
    int xx;
    if ((y = cmkey(attrtab,natr,"File Attribute packets","",xxstring)) < 0)
      return(y);    
    if (y == AT_XALL) {			/* ATTRIBUTES ALL ON or ALL OFF */
	if ((z = seton(&xx)) < 0) return(z);
	if (rmsflg) {
	    printf("Sorry, command not available\n");
	    return(-9);
	} else {
	    atenci = xx;		/* Encoding in */
	    atenco = xx;		/* Encoding out */
	    atdati = xx;		/* Date in */
	    atdato = xx;		/* Date out */
	    atdisi = xx;		/* Disposition in/out */
	    atdiso = xx;
	    atleni = xx;		/* Length in/out (both kinds) */
	    atleno = xx;
	    atblki = xx;		/* Blocksize in/out */
	    atblko = xx;
	    attypi = xx;		/* File type in/out */
	    attypo = xx;
	    atsidi = xx;		/* System ID in/out */
	    atsido = xx;
	    atsysi = xx;		/* System-dependent params in/out */
	    atsyso = xx;
	}
	return(z);
    } else if (y == AT_ALLY || y == AT_ALLN) { /* ATTRIBUTES ON or OFF */
	if ((x = cmcfm()) < 0) return(x);
	atcapr = (y == AT_ALLY) ? 1 : 0;
	if (rmsflg) {
	    sstate = setgen('S', "132", atcapr ? "1" : "0", "");
	    return((int) sstate);
	} else return(success = 1);
    }
    /* Otherwise, it's an individual attribute that wants turning off/on */

    if ((z = cmkey(onoff,2,"","",xxstring)) < 0) return(z);
    if ((x = cmcfm()) < 0) return(x);

/* There are better ways to do this... */
/* The real problem is that we're not separating the in and out cases */
/* and so we have to arbitrarily pick the "in" case, i.e tell the remote */
/* server to ignore incoming attributes of the specified type, rather */
/* than telling it not to send them.  The protocol does not (yet) define */
/* codes for "in-and-out-at-the-same-time". */

    switch(y) {
      case AT_DISP:
	if (rmsflg) {
	    sstate = setgen('S', "142", z ? "1" : "0", "");
	    return((int) sstate);
	}
	atdisi = atdiso = z; break;
      case AT_ENCO:
	if (rmsflg) {
	    sstate = setgen('S', "141", z ? "1" : "0", "");
	    return((int) sstate);
	}
	atenci = atenco = z; break;
      case AT_DATE:
	if (rmsflg) {
	    sstate = setgen('S', "135", z ? "1" : "0", "");
	    return((int) sstate);
	}
	atdati = atdato = z; break;
      case AT_LENB:
      case AT_LENK:
	if (rmsflg) {
	    sstate = setgen('S', "133", z ? "1" : "0", "");
	    return((int) sstate);
	}
	atleni = atleno = z; break;
      case AT_BLKS:
	if (rmsflg) {
	    sstate = setgen('S', "139", z ? "1" : "0", "");
	    return((int) sstate);
	}
	atblki = atblko = z; break;
      case AT_FTYP:
	if (rmsflg) {
	    sstate = setgen('S', "134", z ? "1" : "0", "");
	    return((int) sstate);
	}
	attypi = attypo = z; break;
      case AT_SYSI:
	if (rmsflg) {
	    sstate = setgen('S', "145", z ? "1" : "0", "");
	    return((int) sstate);
	}
	atsidi = atsido = z; break;
      case AT_SYSP:
	if (rmsflg) {
	    sstate = setgen('S', "147", z ? "1" : "0", "");
	    return((int) sstate);
	}
	atsysi = atsyso = z; break;
      default:
	printf("?Not available\n");
	return(-2);
    }
    return(1);
}

#ifndef NOSPL
int
setinp() {
    if ((y = cmkey(inptab,ninp,"","",xxstring)) < 0) return(y);
    switch (y) {
      case IN_DEF:			/* SET INPUT DEFAULT-TIMEOUT */
	z = cmnum("Positive number","",10,&x,xxstring);
	return(success = setnum(&indef,x,z,94));
      case IN_TIM:			/* SET INPUT TIMEOUT-ACTION */
	if ((z = cmkey(intimt,2,"","",xxstring)) < 0) return(z);
	if ((x = cmcfm()) < 0) return(x);
	intime = z;
	return(success = 1);
      case IN_CAS:			/* SET INPUT CASE */
	if ((z = cmkey(incast,2,"","",xxstring)) < 0) return(z);
	if ((x = cmcfm()) < 0) return(x);
	incase = z;
	return(success = 1);
      case IN_ECH:			/* SET INPUT ECHO */
	return(success = seton(&inecho));
      case IN_SIL:			/* SET INPUT SILENCE */
	z = cmnum("Positive number","",10,&x,xxstring);
	return(success = setnum(&insilence,x,z,-1));
    }
    return(0);
}
#endif /* NOSPL */

/*
  setlin -- parse name of and then open a communication device.
  Call with:
    xx == XXLINE for a serial (tty) line, XXHOST for a network host,
    zz == 0 means if user doesn't give a device name, continue current
            active connection (if any);
    zz != 0 means if user doesn't give a device name, then close the
            current connection and restore the default communication device.
*/
int
setlin(xx, zz) int xx, zz; {
    if (xx == XYHOST) {			/* SET HOST <hostname> */
#ifndef NETCONN
        printf("?Network connections not supported\n");
	return(-9);
#else
	if (
	    (nettype != NET_DEC) &&
	    (nettype != NET_SX25) &&
            (nettype != NET_TCPB)) {
	    printf("?Network type not supported\n");
	    return(-9);
	  }
	if (nettype != NET_TCPB) {	/* Not a TCP/IP connection */
					/* Just get a text string */
	    if ((x = cmtxt( zz ? 
   "Network host name,\n or carriage return to close an open connection" :
   "Network host name,\n or carriage return to resume an open connection",
			   "",&s,xxstring)) < 0)
	      return(x);

	} else {			/* TCP/IP connection... */

	    /* Parse for host and service separately. */

	    if ((x = cmfld( zz ?
   "IP host name or number,\n or carriage return to close an open connection" :
   "IP host name or number,\n or carriage return to resume an open connection",
			   "",&s,xxstring)) < 0) {
		if (x != -3)		/* Parse error */
		  return(x);		/* return it */
		else if (!zz)		/* No hostname given */
		  return(1);		/* and none required, */
	    }				/* continue current connection. */
	    if (*s) {			/* If they gave a host name... */
		strcpy(line,s);		/* make a copy */
		/* Check for "host:service" */
		for ( ; (*s != '\0') && (*s != ':'); s++) ;

		/* If no service given, let them type one now. */

		if (!*s) {
		    if ((x = cmfld(
    "TCP service name or number,\n or carriage return for telnet (23)",
				   "23",&s,xxstring)) < 0 && x != -3)
		      return(x);
		    if (*s) {		/* If they gave a service, */
			strcat(line,":"); /* concatenate it to the hostname */
			strcat(line,s);	/* separated by a colon, because */
		    }			/* this is how ttopen() wants it. */
		}
		if ((x = cmcfm()) < 0) return(x); /* Confirm the command */
		s = line;
	    }
	}

	/* New connection wanted. */

	ttflui();			/* Clear away buffered up junk */
	ttclos(0);			/* Close old connection, if any */
	if (oldplex > -1)		/* Restore duplex setting. */
	  duplex = oldplex;
	if (*s) {			/* They gave a hostname */
	    x = 1;			/* Network connection always local */
	    mdmsav = mdmtyp;		/* Remember old modem type */
	    mdmtyp = -nettype;		/* Special code for network */
	    if (nettype == NET_TCPB) {	/* For TCP/IP telnet connections */
		oldplex = duplex;	/* Remember previous duplex */
		duplex = 0;		/* Set full duplex and let */
	  }				/* negotiations change if necessary. */
	} else {			/* They just said "set host" */
	    if (network && msgflg)
	      printf(" Closing connection\n");
	    s = dftty;			/* So go back to normal */
	    x = dfloc;			/* default tty, location, */
	    network = 0;		/* No more network connection. */
	    duplex = oldplex;		/* Restore old duplex setting. */
	    if (mdmtyp < 0) {		/* Switching from net to async? */
		if (mdmsav > -1)	/* Restore modem type from last */
		  mdmtyp = mdmsav;	/* SET MODEM command, if any. */
		else
		  mdmtyp = 0;
	    }
	}
#endif /* NETCONN */
    }

/* Serial tty device, possibly modem, connection... */

    if (xx == XYLINE) {			/* SET LINE */
#ifdef OS2				/* or SET PORT */
	if ((x = cmkey(os2devtab,nos2dev,"serial communication device name",
		       "com1", xxstring)) < 0) {
	    debug(F101,"OS2 SET PORT x","",x);
	    if (x != -9) return(x);
	    s = atxbuf;			/* Doesn't match keyword */
	    if (*s == '_') s++;		/* Secret notation for file handle */
	    else return(-9);
	} else if (x >= 1 && x <= 8) {	/* Regular COM port */
	    s = os2devtab[x+7].kwd;	/* Use its real name */
	} else s = atxbuf;		/* Something else, believe it */
#else
	if ((x = cmtxt("Communication device name",dftty,&s,xxstring)) < 0)
	  return(x);
#endif /* OS2 */

	if (local) ttflui();		/* Clear away buffered up junk */
	ttclos(0);			/* Close old line, if any was open */
	if (*s) {			/* They gave a device name */
	    x = -1;			/* Let ttopen decide about it */
	} else {			/* They just said "set line" */
	    s = dftty;			/* so go back to normal tty */
	    x = dfloc;			/* and mode. */
	}
	if (mdmtyp < 0) {		/* Switching from net to async? */
	    if (mdmsav > -1)		/* Restore modem type from last */
	      mdmtyp = mdmsav;		/* SET MODEM command, if any. */
	    else
	      mdmtyp = 0;
	    mdmsav = -1;
	}
	if (oldplex > -1) {		/* Restore previous duplex setting. */
	    duplex = oldplex;
	    oldplex = -1;
	}
	network = 0;			/* No more network. */
    }
#ifdef COMMENT
/*
  The following is removed, not so much because it's a horrible hack, but
  because it only works if the SET FLOW command was given *before* the SET
  LINE command, whereas normally these commands can be given in any order.
*/
#ifdef NEXT
/*
  This is a horrible hack, but it's nice for users.  On the NeXT, you select
  RTS/CTS hardware flow control not by system calls, but by referring to the
  device with a different name.  If the user has asked for RTS/CTS flow
  control on a NeXT, but used the non-RTS/CTS device name in the SET LINE
  command, we make the appropriate substitute here.  I wonder how much bigger
  this section of code will grow as the years go by... 
*/
    if ((flow == FLO_RTSC) &&		/* RTS/CTS flow control selected */
	strcmp(s,dftty)) {		/*  ...on external port? */
	y = strlen(s);			/* Yes, insert "f" as next-to-last */
	if (s[y-2] != 'f') {		/* character in device name if not */
	    strcpy(line,s);		/* already there... */
	    line[y] = line[y-1];	/* So /dev/cua => /dev/cufa, etc. */
	    line[y-1] = 'f';
	    line[y+1] = '\0';
	    s = line;
	}
    }
#endif /* NEXT */
#endif /* COMMENT */

    if ((y = ttopen(s,&x,mdmtyp,cdtimo)) < 0 ) { /* Open the new line */
	if (y == -2) {
	    printf("?Timed out, no carrier.\n");
	    printf("Try SET CARRIER OFF and SET LINE again, or else\n");
	    printf("SET MODEM, SET LINE, and then DIAL.\n");
	} else if (y == -3) {
	    printf("Sorry, access to lock denied: %s\n",s);
	} else if (y == -4) {
	    printf("Sorry, access to device denied: %s\n",s);
	} else if (y == -5) {
#ifdef VMS
	    printf("Sorry, device is in use or otherwise unavailable: %s\n",s);
#else
	    printf("Sorry, device is in use: %s\n",s);
#endif /* VMS */
        } else {			/* Other error. */
#ifndef VMS
	    if (errno) {
		tp = tmpbuf;
		sprintf(tp,"Sorry, can't open connection: %s",s);
		perror(tp);
	    } else
#endif /* VMS */
	      printf("Sorry, can't open connection: %s\n",s);
	}    
	local = dfloc;			/* Go back to normal */
#ifndef MAC
	strcpy(ttname,dftty);		/* Restore default tty name */
#endif /* MAC */
	speed = ttgspd();
	network = 0;			/* No network connection active */
	return(success = 0);		/* Return failure */
    }
    if (x > -1) local = x;		/* Opened ok, set local/remote. */
    network = (mdmtyp < 0);		/* Remember connection type. */
#ifdef TNCODE
    if (network) tn_init = 0;		/* Say telnet not init'd yet. */
#endif /* TNCODE */
    strcpy(ttname,s);			/* Copy name into real place. */
    speed = ttgspd();			/* Get the current speed. */
    debug(F111,"set line ",ttname,local);
#ifdef NETCONN
#ifdef SUNX25
    if (nettype == NET_SX25) duplex = 1; /* Duplex half */
#endif /* SUNX25 */
#endif /* NETCONN */
    return(success = 1);
}
#endif /* NOICP */
