char *userv = "User Interface 5A(092), 23 Nov 92";

/*  C K U U S R --  "User Interface" for Unix Kermit (Part 1)  */

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
  NOTE: Because of the massive additions in functionality, and therefore
  the increase in the number of commands, much code was moved from here to
  the two new modules, ckuus4.c and ckuus5.c.  This module now contains only
  the top-level command keyword table, the SET command keyword table, and
  the top-level interactive command parser/dispatcher.  ckuus3.c contains the
  rest of the SET and REMOTE command parsers; ckuus2.c contains the help
  command parser and help text strings, and ckuus4.c and ckuus5.c contain
  miscellaneous pieces that logically belong in the ckuusr.c file but had to
  be moved because of size problems with some C compilers / linkers.
  Later...  as the other modules became too large, a ckuus6.c was created.
  Still later...  ckuus7.c.
  Also: ckuusy.c contains the UNIX-style command-line interface;
  ckuusx.c contains routines needed by both the command-line interface and
  the interactive command parser.
*/

/*
 The ckuus*.c modules depend on the existence of C library features like fopen,
 fgets, feof, (f)printf, argv/argc, etc.  Other functions that are likely to
 vary among Unix implementations -- like setting terminal modes or interrupts
 -- are invoked via calls to functions that are defined in the system-
 dependent modules, ck?[ft]io.c.  The command line parser processes any
 arguments found on the command line, as passed to main() via argv/argc.  The
 interactive parser uses the facilities of the cmd package (developed for this
 program, but usable by any program).  Any command parser may be substituted
 for this one.  The only requirements for the Kermit command parser are these:

1. Set parameters via global variables like duplex, speed, ttname, etc.  See
   ckmain.c for the declarations and descriptions of these variables.

2. If a command can be executed without the use of Kermit protocol, then
   execute the command directly and set the variable sstate to 0. Examples
   include 'set' commands, local directory listings, the 'connect' command.

3. If a command requires the Kermit protocol, set the following variables:

    sstate                             string data
      'x' (enter server mode)            (none)
      'r' (send a 'get' command)         cmarg, cmarg2
      'v' (enter receive mode)           cmarg2
      'g' (send a generic command)       cmarg
      's' (send files)                   nfils, cmarg & cmarg2 OR cmlist
      'c' (send a remote host command)   cmarg

    cmlist is an array of pointers to strings.
    cmarg, cmarg2 are pointers to strings.
    nfils is an integer.

    cmarg can be a filename string (possibly wild), or
       a pointer to a prefabricated generic command string, or
       a pointer to a host command string.
    cmarg2 is the name to send a single file under, or
       the name under which to store an incoming file; must not be wild.
       If it's the name for receiving, a null value means to store the
       file under the name it arrives with.
    cmlist is a list of nonwild filenames, such as passed via argv.
    nfils is an integer, interpreted as follows:
      -1: filespec (possibly wild) in cmarg, must be expanded internally.
       0: send from stdin (standard input).
      >0: number of files to send, from cmlist.

 The screen() function is used to update the screen during file transfer.
 The tlog() function writes to a transaction log.
 The debug() function writes to a debugging log.
 The intmsg() and chkint() functions provide the user i/o for interrupting
   file transfers.
*/

#ifndef NOICP
/* Includes */
 
#include "ckcdeb.h"
#include "ckcasc.h"
#include "ckcker.h"
#include "ckuusr.h"
#include "ckcxla.h"
#include "ckcnet.h"			/* Network symbols */
 
#ifdef datageneral
#include <packets:common.h>
#define fgets(stringbuf,max,fd) dg_fgets(stringbuf,max,fd)
#endif /* datageneral */

/* External Kermit Variables, see ckmain.c for description. */
 
extern int size, local, sndsrc, xitsta, server, displa, binary, msgflg, 
  escape, duplex, nfils, quiet, tlevel, pflag, zincnt, atcapr, atdiso, verwho;
 
extern long vernum;
extern char *versio;
extern char *ckxsys, *cmarg, *cmarg2, **cmlist;
#ifndef NOHELP
extern char *introtxt[];
#endif /* NOHELP */
extern char *PWDCMD, *WHOCMD, *TYPCMD;
extern char ttname[];
#ifndef NOFRILLS
extern int rmailf;			/* MAIL command items */
extern char optbuf[];
#endif /* NOFRILLS */
extern CHAR sstate;

#ifdef NETCONN
extern int network;			/* Have active network connection */
#endif /* NETCONN */

#ifndef NOMSEND				/* Multiple SEND */
extern char *msfiles[];
#endif /* NOMSEND */
extern char fspec[];			/* Most recent filespec */

#ifndef NOCSETS
extern int nfilc;
extern struct keytab fcstab[];
#endif /* NOCSETS */

int rcflag = 0;				/* Pointer to home directory string */
int repars,				/* Reparse needed */
    techo = 0,				/* Take echo */
    terror = 0;				/* Take error action, 1 = quit */
#ifndef NOSCRIPT
int secho = 1;
#endif /* NOSCRIPT */

#ifndef NOXMIT
/* Variables for TRANSMIT command */

int xmitx = 1;			/* Whether to echo during TRANSMIT */
int xmitf = 0;			/* Character to fill empty lines */
int xmitl = 0;			/* 0 = Don't send linefeed too */
int xmitp = LF;			/* Host line prompt */
int xmits = 0;			/* Use shift-in/shift-out, 0 = no */
int xmitw = 0;			/* Milliseconds to pause during TRANSMIT */
#endif /* NOXMIT */

/* Declarations from ck?fio.c module */
 
extern char *SPACMD, *SPACM2;		/* SPACE commands */
 
/* Command-oriented items */

#ifdef DCMDBUF
extern char *cmdbuf;			/* Command buffers */
extern char *atmbuf;
extern char *line;			/* Character buffer for anything */
extern int *ifcmd;
#else
extern char cmdbuf[];			/* Command buffers */
extern char atmbuf[];
extern char line[];			/* Character buffer for anything */
extern int ifcmd[];
#endif /* DCMDBUF */

char *lp;				/* Pointer to line buffer */

#ifndef NOSPL
extern char inpbuf[];			/* Buffer for INPUT and REINPUT */
char *inpbp = inpbuf;			/* And pointer to same */
extern char lblbuf[];			/* Buffer for labels */
#endif /* NOSPL */

char psave[80] = { NUL };		/* For saving & restoring prompt */

extern char tmpbuf[];			/* Temporary buffer */
 
extern int success;			/* Command success/failure flag */

#ifndef NOSPL
int					/* SET INPUT parameters. */
  indef = 5,				/* 5 seconds default timeout */
  intime = 0,				/* 0 = proceed */
  incase = 0,				/* 0 = ignore */
  inecho = 1,				/* 1 = echo on */
  insilence = 0;			/* 0 = no silence constraint */
 
int maclvl = -1;			/* Macro nesting level */
int mecho = 0;				/* Macro echo, 0 = don't */
int merror = 0;				/* Macro error action */
char varnam[6];				/* For variable names */
extern int macargc[];			/* ARGC from macro invocation */

extern char *m_arg[MACLEVEL][NARGS];	/* Stack of macro arguments */
 
extern char **a_ptr[];			/* Array pointers */
extern int a_dim[];			/* Array dimensions */

#ifdef DCMDBUF
extern struct cmdptr *cmdstk;		/* The command stack itself */
#else
extern struct cmdptr cmdstk[];		/* The command stack itself */
#endif /* DCMDBUF */
extern int cmdlvl;			/* Current position in command stack */
#endif /* NOSPL */

static int x, y, z = 0;			/* Local workers */
static char *s;

#define xsystem(s) zsyscmd(s)

/* Top-Level Interactive Command Keyword Table */
 
struct keytab cmdtab[] = {
#ifndef NOPUSH
    "!",	   XXSHE, CM_INV,	/* shell escape */
#endif /* NOPUSH */
    "#",    	   XXCOM, CM_INV,	/* comment */
#ifndef NOSPL
    ":",           XXLBL, CM_INV,	/* label */
#endif /* NOSPL */
#ifndef NOPUSH
    "@",           XXSHE, CM_INV,	/* DCL escape */
#endif /* NOPUSH */
#ifndef NOSPL
    "asg",         XXASS, CM_INV,       /* invisible synonym for assign */
    "ask",         XXASK, 0,		/* ask */
    "askq",        XXASKQ,0,            /* ask quietly */
    "assign",      XXASS, 0,            /* assign */
#endif /* NOSPL */
#ifndef NOFRILLS
    "bug",         XXBUG, 0,		/* bug report instructions */
#endif /* NOFRILLS */
    "bye",         XXBYE, 0,		/* bye to remote server */
    "c",           XXCON, CM_INV|CM_ABR, /* invisible synonym for connect */
#ifndef NOFRILLS
    "cat",         XXTYP, CM_INV,	/* display a local file */
#endif /* NOFRILLS */
    "cd",          XXCWD, 0,		/* change directory */
    "check",       XXCHK, 0,		/* check for a feature */
#ifndef NOFRILLS
    "clear",       XXCLE, 0,		/* clear input buffer */
#endif /* NOFRILLS */
    "close",	   XXCLO, 0,		/* close a log file */
#ifdef NOFRILLS
    "comment",     XXCOM, CM_INV,	/* comment */
#else
    "comment",     XXCOM, 0,		/* comment */
#endif /* NOFRILLS */
    "connect",     XXCON, 0,		/* connect to remote system */
    "cwd",	   XXCWD, CM_INV,	/* invisisble synonym for cd */
#ifndef NOSPL
    "dcl",         XXDCL, CM_INV,	/* declare an array */
    "declare",     XXDCL, 0,		/* declare an array */
    "decrement",   XXDEC, 0,		/* decrement a numeric variable */
    "define",      XXDEF, 0,		/* define a macro */
#endif /* NOSPL */
#ifndef NOFRILLS
    "delete",      XXDEL, 0,		/* delete a file */
#endif /* NOFRILLS */
#ifndef NODIAL
    "dial",	   XXDIAL,0,		/* dial a phone number */
#endif /* NODIAL */
#ifndef MAC
    "directory",   XXDIR, 0,		/* directory of files */
#endif /* MAC */
#ifndef NOFRILLS
    "disable",     XXDIS, 0,		/* disable server function */
#endif /* NOFRILLS */
#ifndef NOSPL
    "do",          XXDO,  0,		/* execute a macro */
#endif /* NOSPL */
#ifndef NOFRILLS
    "e-packet",    XXERR, CM_INV,	/* Send an Error packet */
#endif /* NOFRILLS */
    "echo",        XXECH, 0,		/* echo argument */
#ifndef NOSPL
    "else",        XXELS, CM_INV,	/* ELSE part of IF statement */
#endif /* NOSPL */
#ifndef NOFRILLS
    "enable",      XXENA, 0,		/* ENABLE a server function */
#endif /* NOFRILLS */
#ifndef NOSPL
    "end",         XXEND, 0,		/* END command file or macro */
#endif /* NOSPL */
    "ex",          XXEXI, CM_INV|CM_ABR, /* Let "ex" still be EXIT */
    "exit",	   XXEXI, 0,		/* exit the program */
    "extproc",     XXCOM, CM_INV,       /* dummy command */
    "finish",      XXFIN, 0,		/* FINISH */
#ifndef NOSPL
    "for",         XXFOR, 0,		/* FOR loop */
#endif /* NOSPL */
#ifndef NOFRILLS
#ifndef MAC
    "fot",	   XXDIR, CM_INV,	/* "fot" = "dir" (for Chris) */
#endif /* MAC */
#endif /* NOFRILLS */
    "g",           XXGET, CM_INV|CM_ABR, /* Invisible abbreviation for GET */
#ifndef NOSPL
    "ge",          XXGET, CM_INV|CM_ABR, /* Ditto */
#endif /* NOSPL */
    "get",         XXGET, 0,		/* GET */
#ifndef NOSPL
#ifndef NOFRILLS
    "getok",       XXGOK, 0,		/* GETOK (ask for Yes/No) */
#endif /* NOFRILLS */
#endif /* NOSPL */
#ifndef NOSPL
    "goto",        XXGOTO,0,		/* goto label in take file or macro */
#endif /* NOSPL */
    "hangup",      XXHAN, 0,		/* hangup dialed phone connection */
    "help",	   XXHLP, 0,		/* display help text */
#ifndef NOSPL
    "i",           XXINP, CM_INV|CM_ABR, /* invisible synonym for INPUT */
    "if",          XXIF,  0,		/* if (condition) command */
    "in",          XXINP, CM_INV|CM_ABR, /* invisible synonym for INPUT */
    "increment",   XXINC, 0,		/* increment a numeric variable */
    "input",       XXINP, 0,		/* input string from comm line */
#endif /* NOSPL */
#ifndef NOHELP
     "introduction", XXINT, 0,		/* Print introductory text */
#endif /* NOHELP */
#ifndef NOFRILLS
    "l",           XXLOG, CM_INV|CM_ABR,/* invisible synonym for log */
#endif /* NOFRILLS */
    "log",  	   XXLOG, 0,		/* open a log file */
#ifndef NOFRILLS
#ifndef MAC
    "ls",          XXDIR, CM_INV,	/* invisible synonym for directory */
#endif /* MAC */
    "mail",        XXMAI, 0,		/* mail file to user */
    "man",         XXHLP, CM_INV,       /* Synonym for help */
#endif /* NOFRILLS */
#ifndef NOMSEND
    "mget",        XXGET, CM_INV,	/* MGET = GET */
#endif /* NOMSEND */
#ifndef NOSPL
    "mpause",      XXMSL, CM_INV,	/* Millisecond sleep */
#endif /* NOSPL */
#ifndef NOMSEND
    "ms",          XXMSE, CM_INV|CM_ABR,
    "msend",       XXMSE, 0,		/* Multiple SEND */
#endif /* NOMSEND */
#ifndef NOSPL
    "msleep",      XXMSL, 0,		/* Millisecond sleep */
#endif /* NOSPL */
#ifndef NOMSEND
    "mput",        XXMSE, CM_INV,	/* MPUT = MSEND */
#endif /* NOMSEND */
#ifndef NOFRILLS
    "mv",          XXREN, CM_INV,	/* rename a local file */
#endif /* NOFRILLS */
#ifndef NOSPL
    "o",           XXOUT, CM_INV|CM_ABR,/* invisible synonym for OUTPUT */
    "open",        XXOPE, 0,		/* open file for reading or writing */
    "output",      XXOUT, 0,		/* output string to comm line */
#endif /* NOSPL */
#ifdef SUNX25
    "pad",         XXPAD, 0,            /* PAD commands */
#endif /* SUNX25 */
#ifndef NOSPL
    "pause",       XXPAU, 0,		/* sleep for specified interval */
#ifdef TCPSOCKET
    "ping",        XXPNG, 0,		/* PING (for TCP/IP) */
#endif /* TCPSOCKET */
    "pop",         XXEND, CM_INV,	/* allow POP as synonym for END */
#endif /* NOSPL */
#ifndef NOFRILLS
    "print",       XXPRI, 0,		/* PRINT */
#ifndef NOPUSH
    "pu",          XXSHE, CM_INV,	/* PU = PUSH */
    "push",        XXSHE, 0,		/* PUSH command (like RUN, !) */
#endif /* NOPUSH */
    "put",	   XXSEN, CM_INV,	/* PUT = SEND */
#ifndef MAC
    "pwd",         XXPWD, 0,            /* print working directory */
#endif /* MAC */
#endif /* NOFRILLS */
    "quit",	   XXQUI, 0,		/* quit from program = exit */
    "r",           XXREC, CM_INV,	/* invisible synonym for receive */
#ifndef NOSPL
    "read",        XXREA, 0,            /* read */
#endif /* NOSPL */
    "receive",	   XXREC, 0,		/* receive files */
#ifndef NODIAL
    "redial",      XXRED, 0,		/* redial */
#endif /* NODIAL */
#ifndef NOSPL
    "reinput",     XXREI, 0,            /* reinput */
#endif /* NOSPL */
    "remote",	   XXREM, 0,		/* send generic command to server */
#ifndef NOFRILLS
    "rename",      XXREN, 0,		/* rename a local file */
    "replay",      XXTYP, CM_INV,	/* replay (for now, just type) */
#endif /* NOFRILLS */
#ifndef NOSPL
    "return",      XXRET, 0,		/* return from function */
#endif /* NOSPL */
#ifndef NOFRILLS
    "rm",          XXDEL, CM_INV,	/* invisible synonym for delete */
#endif /* NOFRILLS */
#ifndef NOPUSH
    "run",         XXSHE, 0,		/* run a program or command */
#endif /* NOPUSH */
    "s",           XXSEN, CM_INV|CM_ABR, /* invisible synonym for send */
#ifndef NOSCRIPT
    "script",	   XXLOGI,0,		/* execute a uucp-style script */
#endif /* NOSCRIPT */
    "send",	   XXSEN, 0,		/* send files */
#ifndef NOSERVER
    "server",	   XXSER, 0,		/* be a server */
#endif /* NOSERVER */
    "set",	   XXSET, 0,		/* set parameters */
#ifndef NOSHOW
    "show", 	   XXSHO, 0,		/* show parameters */
#endif /* NOSHOW */
#ifndef NOSPL
#ifndef NOFRILLS
    "sleep",       XXPAU, CM_INV,	/* sleep for specified interval */
#endif /* NOFRILLS */
#endif /* NOSPL */
#ifndef MAC
#ifndef NOFRILLS
    "sp",          XXSPA, CM_INV|CM_ABR,
    "spa",         XXSPA, CM_INV|CM_ABR,
#endif /* NOFRILLS */
    "space",       XXSPA, 0,		/* show available disk space */
#endif /* MAC */
#ifndef NOFRILLS
#ifndef NOPUSH
    "spawn",       XXSHE, CM_INV,	/* synonym for PUSH, RUN */
#endif /* NOPUSH */
#endif /* NOFRILLS */
    "statistics",  XXSTA, 0,		/* display file transfer stats */
#ifndef NOSPL
    "stop",        XXSTO, 0,		/* stop all take files */
#endif /* NOSPL */
#ifndef NOJC
    "suspend",     XXSUS, 0,		/* Suspend */
#endif /* NOJC */
    "take",	   XXTAK, 0,		/* take commands from file */
#ifndef NOFRILLS
#ifdef NETCONN
    "telnet",      XXTEL, 0,		/* telnet */
#endif /* NETCONN */
    "test",        XXTES, CM_INV,	/* (for testing) */
#endif /* NOFRILLS */
#ifndef NOCSETS
    "translate",   XXXLA, 0,		/* translate local file char sets */
#endif
#ifndef NOXMIT
    "transmit",    XXTRA, 0,		/* raw upload file */
#endif /* NOXMIT */
#ifndef NOFRILLS
    "type",        XXTYP, 0,		/* display a local file */
#endif /* NOFRILLS */
    "version",     XXVER, 0		/* version number display */
#ifndef NOSPL
,   "wait",        XXWAI, 0		/* wait (like pause) */
,   "while",       XXWHI, 0		/* while */
#endif /* NOSPL */
#ifndef MAC
#ifndef NOFRILLS
,   "who",         XXWHO, 0		/* who */
#endif /* NOFRILLS */
#endif /* MAC */
#ifndef NOSPL
,   "write",       XXWRI, 0		/* write */
,   "xif",         XXIFX, 0		/* Extended IF */
#endif /* NOSPL */
#ifndef NOCSETS
,   "xlate",       XXXLA, CM_INV	/* translate local file char sets */
#endif
#ifndef NOXMIT
,   "xmit",        XXTRA, CM_INV	/* raw upload file */
#endif /* NOXMIT */
,   "z",           XXSUS, CM_INV	/* Suspend */
#ifndef NOSPL
,   "_assign",     XXASX, CM_INV	/* used internally by FOR, etc */
,   "_define",     XXDFX, CM_INV	/* used internally by FOR, etc */
,   "_getargs",    XXGTA, CM_INV        /* used internally by FOR, etc */
,   "_putargs",    XXPTA, CM_INV        /* used internally by FOR, etc */
#endif /* NOSPL */
};
int ncmd = (sizeof(cmdtab) / sizeof(struct keytab));

char toktab[] = {
#ifndef NOPUSH
    '!',				/* Shell escape */
#endif /* NOPUSH */
    '#',				/* Comment */
    ';',				/* Comment */
#ifndef NOSPL
    ':',				/* Label */
#endif /* NOSPL */
#ifndef NOPUSH
    '@',				/* DCL escape */
#endif /* NOPUSH */
    '\0'				/* End of this string */
};

#ifndef NOSPL
#ifndef NOFRILLS
struct keytab yesno[] = {
    "no",    0, 0,
    "ok",    1, 0,
    "yes",   1, 0
};
int nyesno = (sizeof(yesno) / sizeof(struct keytab));
#endif /* NOFRILLS */
#endif /* NOSPL */

/* Parameter keyword table */
 
struct keytab prmtab[] = {
    "attributes",       XYATTR,  0,
    "b",		XYBACK,  CM_INV|CM_ABR,
    "ba",		XYBACK,  CM_INV|CM_ABR,
    "background",       XYBACK,  0,
    "baud",	        XYSPEE,  CM_INV,
    "block-check",  	XYCHKT,  0,
#ifdef DYNAMIC
    "buffers",          XYBUF,   0,
#endif /* DYNAMIC */
#ifndef MAC
    "carrier",          XYCARR,  0,
#endif /* MAC */
#ifndef NOSPL
    "case",             XYCASE,  0,
#endif /* NOSPL */
    "command",          XYCMD,   0,
#ifndef NOSPL
    "count",            XYCOUN,  0,
#endif /* NOSPL */
    "d",		XYDELA,  CM_INV|CM_ABR,
    "de",		XYDELA,  CM_INV|CM_ABR,
    "debug",            XYDEBU,  CM_INV,
#ifdef VMS
    "default",          XYDFLT,  0,
#else
#ifndef MAC
    "default",          XYDFLT,  CM_INV,
#endif /* MAC */
#endif /* VMS */
    "delay",	    	XYDELA,  0,
#ifndef NODIAL
    "dial",             XYDIAL,  0,
#endif /* NODIAL */
    "duplex",	    	XYDUPL,  0,
    "escape-character", XYESC,   0,
    "file", 	  	XYFILE,  0,
    "flow-control", 	XYFLOW,  0,
    "handshake",    	XYHAND,  0,
#ifdef NETCONN
    "host",             XYHOST,  0,
#endif /* NETCONN */
    "incomplete",   	XYIFD,   CM_INV,
#ifndef NOSPL
    "i",		XYINPU,  CM_INV|CM_ABR,
    "in",		XYINPU,  CM_INV|CM_ABR,
    "input",            XYINPU,  0,
#ifndef MAC
#ifndef NOSETKEY
    "key",		XYKEY,   0,
#endif /* NOSETKEY */
#endif /* MAC */
#endif /* NOSPL */
    "l",                XYLINE,  CM_INV|CM_ABR,
#ifndef NOCSETS
    "language",         XYLANG,  0,
#endif /* NOCSETS */
    "line",             XYLINE,  0,
    "local-echo",	XYLCLE,  CM_INV,
#ifndef NOSPL
    "macro",            XYMACR,  0,
#endif /* NOSPL */
#ifdef COMMENT
#ifdef VMS
    "messages",         XYMSGS,  0,
#endif /* VMS */
#endif /* COMMENT */
#ifndef NODIAL
    "modem-dialer",	XYMODM,	 0,
#endif
#ifdef NETCONN
    "network",          XYNET,   0,
#endif /* NETCONN */
#ifdef SUNX25
    "pad",              XYPAD,   0,
#endif /* SUNX25 */
    "parity",	    	XYPARI,  0,
    "port",             XYLINE,  CM_INV,
#ifndef NOFRILLS
    "prompt",	    	XYPROM,  0,
#endif /* NOFRILLS */
    "quiet",		XYQUIE,  0,
    "receive",          XYRECV,  0,
    "retry-limit",      XYRETR,  0,
#ifndef NOSCRIPT
    "script",		XYSCRI,  0,
#endif /* NOSCRIPT */
    "send",             XYSEND,  0,
#ifndef NOSERVER
    "server",           XYSERV,  0,
#endif /* NOSERVER */
#ifdef UNIX
    "session-log",      XYSESS,  0,
#endif /* UNIX */
    "speed",	        XYSPEE,  0,
#ifndef NOJC
    "suspend",          XYSUSP,  0,
#endif /* NOJC */
    "take",             XYTAKE,  0,
#ifdef TNCODE
    "telnet",           XYTEL,   0,
#endif /* TNCODE */
    "terminal",         XYTERM,  0,
    "transfer",         XYXFER,  0,
#ifndef NOXMIT
    "transmit",         XYXMIT,  0,
#endif /* NOXMIT */
#ifndef NOCSETS
    "unknown-char-set", XYUNCS,  0,
#endif /* NOCSETS */
    "window-size",      XYWIND,  0
#ifdef UNIX
,   "wildcard-expansion", XYWILD, 0
#endif /* UNIX */
#ifdef SUNX25
,   "x.25",             XYX25,   0,
    "x25",              XYX25,   CM_INV
#endif /* SUNX25 */
#ifndef NOCSETS
,   "xfer",             XYXFER,  CM_INV
#endif /* NOCSETS */
#ifndef NOXMIT
,   "xmit",             XYXMIT,  CM_INV
#endif /* NOXMIT */
};
int nprm = (sizeof(prmtab) / sizeof(struct keytab)); /* How many parameters */
 
/* Table of networks */
#ifdef NETCONN
struct keytab netcmd[] = {
#ifdef DECNET
    "decnet",        NET_DEC,  0,
#endif /* DECNET */
#ifdef NPIPE
    "named-pipe",    NET_PIPE, 0,
#endif /* NPIPE */
#ifdef TCPSOCKET
    "tcp/ip",        NET_TCPB, 0
#endif /* TCPSOCKET */
#ifdef SUNX25
,   "x",            NET_SX25, CM_INV|CM_ABR,
    "x.25",         NET_SX25, 0,
    "x25",          NET_SX25, CM_INV
#endif /* SUNX25 */
};
int nnets = (sizeof(netcmd) / sizeof(struct keytab)); /* How many networks */
#endif /* NETCONN */

/* Remote Command Table */
 
struct keytab remcmd[] = {
    "cd",        XZCWD, 0,
    "cwd",       XZCWD, CM_INV,
    "delete",    XZDEL, 0,
    "directory", XZDIR, 0,
    "help",      XZHLP, 0,
#ifndef NOPUSH
    "host",      XZHOS, 0,
#endif /* NOPUSH */
#ifndef NOFRILLS
    "kermit",    XZKER, 0,
    "login",     XZLGI, 0,
    "logout",    XZLGO, 0,
    "print",     XZPRI, 0,
#endif /* NOFRILLS */
    "set",       XZSET, 0,
    "space",	 XZSPA, 0
#ifndef NOFRILLS
,   "type", 	 XZTYP, 0,
    "who",  	 XZWHO, 0
#endif /* NOFRILLS */
};
int nrmt = (sizeof(remcmd) / sizeof(struct keytab));
 
struct keytab logtab[] = {
#ifdef DEBUG
    "debugging",    LOGD, 0,
#endif /* DEBUG */
    "packets",	    LOGP, 0,
    "session",      LOGS, 0
#ifdef TLOG
,   "transactions", LOGT, 0
#endif /* TLOG */
};
int nlog = (sizeof(logtab) / sizeof(struct keytab));
 
struct keytab writab[] = {
#ifndef NOSPL
    "append-file",     LOGW, CM_INV,
#endif /* NOSPL */
    "debug-log",       LOGD, 0,
    "error",           LOGE, 0,
#ifndef NOSPL
    "file",            LOGW, 0,
#endif /* NOSPL */
    "packet-log",      LOGP, 0,
    "screen",          LOGX, 0,
    "session-log",     LOGS, 0,
    "sys$output",      LOGX, CM_INV,
    "transaction-log", LOGT, 0
};
int nwri = (sizeof(writab) / sizeof(struct keytab));

#define CLR_DEV  1
#define CLR_INP  2

static struct keytab clrtab[] = {	/* Keywords for CLEAR command */
#ifndef NOSPL
    "both",          CLR_DEV|CLR_INP, 0,
#endif /* NOSPL */
    "device-buffer", CLR_DEV,         0,
#ifndef NOSPL
    "input-buffer",  CLR_INP,         0
#endif /* NOSPL */
};
int nclear = (sizeof(clrtab) / sizeof(struct keytab));

struct keytab clstab[] = {		/* Keywords for CLOSE command */
#ifndef NOSPL
    "append-file",     LOGW, CM_INV,
#endif /* NOSPL */
#ifdef DEBUG
    "debug-log",       LOGD, 0,
#endif /* DEBUG */
    "packet-log",      LOGP, 0,
#ifndef NOSPL
    "read-file",       LOGR, 0,
#endif /* NOSPL */
    "session-log",     LOGS, 0
#ifdef TLOG
,   "transaction-log", LOGT, 0
#endif /* TLOG */
#ifndef NOSPL
,   "write-file",      LOGW, 0
#endif /* NOSPL */
};
int ncls = (sizeof(clstab) / sizeof(struct keytab));

/* SHOW command arguments */
 
struct keytab shotab[] = {
#ifndef NOSPL
    "arguments", SHARG, 0,
    "arrays", SHARR, 0,
#endif /* NOSPL */
    "attributes", SHATT, 0,
    "character-sets", SHCSE, 0,
    "communications", SHCOM, 0,
#ifndef NOSPL
    "count", SHCOU, 0,
#endif /* NOSPL */
#ifdef VMS
    "default", SHDFLT, 0,
#else
    "default", SHDFLT, CM_INV,
#endif /* VMS */
#ifndef NODIAL
    "dial", SHDIA, 0,
#endif /* NODIAL */
    "escape", SHESC, 0,
    "features", SHFEA, 0,
    "file", SHFIL, 0,
#ifndef NOSPL
    "functions", SHFUN, 0,
    "globals", SHVAR, 0,
#endif /* NOSPL */
#ifndef NOSETKEY
    "key", SHKEY, 0,
#endif /* NOSETKEY */
#ifdef VMS
    "labeled-file-info", SHLBL, 0,
#endif /* VMS */
#ifndef NOCSETS
    "languages", SHLNG, 0,
#endif /* NOCSETS */
#ifndef NOSPL
    "macros", SHMAC, 0,
#endif /* NOSPL */
    "modem-signals", SHMOD, 0,
    "network", SHNET, 0,
#ifdef SUNX25
    "pad", SHPAD, 0,
#endif /* SUNX25 */
    "parameters", SHPAR, CM_INV,
    "protocol", SHPRO, 0,
#ifndef NOSPL
    "scripts", SHSCR, 0,
#endif /* NOSPL */
#ifndef NOSERVER
    "server", SHSER, 0,
#endif /* NOSERVER */
    "status", SHSTA, 0,
#ifdef MAC
    "stack", SHSTK, 0,			/* debugging */
#endif /* MAC */
    "terminal", SHTER, 0
#ifndef NOXMIT
,   "transmit", SHXMI, 0
#endif /* NOXMIT */
#ifndef NOSPL
,   "variables", SHBUI, 0
#endif /* NOSPL */
#ifndef NOFRILLS
,   "versions", SHVER, 0
#endif /* NOFRILLS */
#ifndef NOXMIT
,   "xmit", SHXMI, CM_INV
#endif /* NOXMIT */
};
int nsho = (sizeof(shotab) / sizeof(struct keytab));

#ifdef SUNX25
struct keytab padtab[] = {              /* PAD commands */
    "clear",      XYPADL, 0,
    "interrupt",  XYPADI, 0,
    "reset",      XYPADR, 0,
    "status",     XYPADS, 0
};
int npadc = (sizeof(padtab) / sizeof(struct keytab));
#endif /* SUNX25 */

struct keytab enatab[] = {		/* ENABLE commands */
    "all",        EN_ALL,  0,
#ifndef datageneral
    "bye",        EN_BYE,  0,
#endif /* datageneral */
    "cd",         EN_CWD,  0,
    "cwd",        EN_CWD,  CM_INV,
    "delete",     EN_DEL,  0,
    "directory",  EN_DIR,  0,
    "finish",     EN_FIN,  0,
    "get",        EN_GET,  0,
    "host",       EN_HOS,  0,
    "send",       EN_SEN,  0,
    "set",        EN_SET,  0,
    "space",      EN_SPA,  0,
    "type",       EN_TYP,  0,
    "who",        EN_WHO,  0
};
int nena = (sizeof(enatab) / sizeof(struct keytab));

#ifndef NOSPL
#ifdef COMMENT
struct mtab mactab[MAC_MAX] = {		/* Preinitialized macro table */
    NULL, NULL, 0
};
#else
struct mtab *mactab;			/* Dynamically allocated macro table */
#endif /* COMMENT */
int nmac = 0;

struct keytab mackey[MAC_MAX];		/* Macro names as command keywords */
#endif /* NOSPL */

/* Forward declarations of functions */

_PROTOTYP (int doask,   ( int  ) );
_PROTOTYP (int dodef,   ( int  ) );
_PROTOTYP (int dodel,   ( void ) );
_PROTOTYP (int dodial,  ( int  ) );
_PROTOTYP (int dodir,   ( void ) );
_PROTOTYP (int doelse,  ( void ) );
_PROTOTYP (int dofor,   ( void ) );
_PROTOTYP (int dogta,   ( int  ) );
_PROTOTYP (int doincr,  ( int  ) );
_PROTOTYP (int dopaus,  ( int  ) );
_PROTOTYP (int doping,  ( void ) );
_PROTOTYP (int dorenam, ( void ) );

#ifdef TCPSOCKET
int
doping() {
    char *p;
    int x;

    if (network)			/* If we have a current connection */
      strcpy(line,ttname);		/* get the host name */
    else *line = '\0';			/* as default host to be pinged. */
    for (p = line; *p; p++)		/* Remove ":service" from end. */
      if (*p == ':') { *p = '\0'; break; }
    if ((x = cmtxt("IP host name or number", line, &s, xxstring)) < 0)
      return(x);
/* Construct PING command */
#ifdef VMS
#ifdef MULTINET				/* TGV MultiNet */
    sprintf(line,"multinet ping %s /num=1",s);
#else
    sprintf(line,"ping %s 56 1",s);	/* Other VMS TCP/IP's */
#endif /* MULTINET */
#else					/* Not VMS */
    sprintf(line,"ping %s",s);
#endif /* VMS */
    conres();				/* Make console normal  */
#ifdef DEC_TCPIP
    printf("\n");			/* Prevent prompt-stomping */
#endif /* DEC_TCPIP */
    x = zshcmd(line);
    concb((char)escape);
    return(success = 1);		/* We don't know the status */
}
#endif /* TCPSOCKET */

/*  D O C M D  --  Do a command  */
 
/*
 Returns:
   -2: user typed an illegal command
   -1: reparse needed
    0: parse was successful (even tho command may have failed).
*/ 
int
docmd(cx) int cx; {

    debug(F101,"docmd entry, cx","",cx);

/*
  Massive switch() broken up into many smaller ones, for the benefit of
  compilers that run out of space when trying to handle large switch
  statements.
*/
    switch (cx) {
      case -4:			/* EOF */
#ifdef OSK
	if (msgflg)  printf("\n");
#else
	if (msgflg)  printf("\r\n");
#endif /* OSK */
	  doexit(GOOD_EXIT,xitsta);
      case -3:				/* Null command */
	return(0);
      case -9:				/* Like -2, but errmsg already done */
      case -1:				/* Reparse needed */
	return(cx);
      case -6:				/* Special */
      case -2:				/* Error, maybe */
#ifndef NOSPL
/*
  Maybe they typed a macro name.  Let's look it up and see.
*/
	if (cx == -6)			/* If they typed CR */
	  strcat(cmdbuf,"\015");	/*  add it back to command buffer. */
	if (ifcmd[cmdlvl] == 2)		/* Watch out for IF commands. */
	  ifcmd[cmdlvl]--;
	repars = 1;			/* Force reparse */
	cmres();
	cx = XXDO;			/* Try DO command */
#else
	return(cx);
#endif /* NOSPL */
      default:
	break;
    }
 
#ifndef NOSPL
/* Copy macro args from/to two levels up, used internally by _floop et al. */
    if (cx == XXGTA || cx == XXPTA) {	/* _GETARGS, _PUTARGS */
	int x;
	debug(F101,"docmd XXGTA","",XXGTA);
	debug(F101,"docmd cx","",cx);
	debug(F101,"docmd XXGTA maclvl","",maclvl);
	x = dogta(cx);
	debug(F101,"docmd dogta returns","",x);
	debug(F101,"docmd dogta maclvl","",maclvl);
	return(x);
    }
#endif /* NOSPL */

#ifndef NOSPL
/* ASK, ASKQ, READ */
    if (cx == XXASK || cx == XXASKQ || cx == XXREA) {
	return(doask(cx));
    }
#endif /* NOSPL */

#ifndef NOFRILLS
    if (cx == XXBUG) {			/* BUG */
	if ((x = cmcfm()) < 0) return(x);
	return(dobug());
    }
#endif /* NOFRILLS */

    if (cx == XXBYE) {			/* BYE */
	if ((x = cmcfm()) < 0) return(x);
	sstate = setgen('L',"","","");
	if (local) ttflui();		/* If local, flush tty input buffer */
	return(0);
    } 

#ifndef NOFRILLS
    if (cx == XXCLE) {			/* CLEAR */
	if ((x = cmkey(clrtab,nclear,"buffer(s) to clear",
#ifdef NOSPL
		  "device-buffer"
#else
		  "both"
#endif /* NOSPL */
		  ,xxstring)) < 0) return(x);
	if ((y = cmcfm()) < 0) return(y);

	/* Clear device input buffer if requested */
	y = (x & CLR_DEV) ? ttflui() : 0;
#ifndef NOSPL
	/* Clear INPUT command buffer if requested */
	if (x & CLR_INP) {
	    for (x = 0; x < INPBUFSIZ; x++)
	      inpbuf[x] = 0;
	    inpbp = inpbuf;
	}
#endif /* NOSPL */
	return(success = (y == 0));
    }
#endif /* NOFRILLS */

    if (cx == XXCOM) {			/* COMMENT */
	if ((x = cmtxt("Text of comment line","",&s,NULL)) < 0)
	  return(x);
	/* Don't change SUCCESS flag for this one */
	return(0);
    } 

    if (cx == XXCON) {			/* CONNECT */
	if ((x = cmcfm()) < 0)
	  return(x);
	return(success = doconect());
    }

    if (cx == XXCWD)			/* CWD */
      return(success = docd());

    if (cx == XXCHK)			/* CHECK */
      return(success = dochk());

    if (cx == XXCLO) {			/* CLOSE */
	x = cmkey(clstab,ncls,"Which log or file to close","",xxstring);
	if (x == -3) {
	    printf("?You must say which file or log\n");
	    return(-9);
	}
	if (x < 0) return(x);
	if ((y = cmcfm()) < 0) return(y);
	y = doclslog(x);
	success = (y == 1);
	return(success);
    }

#ifndef NOSPL
    if (cx == XXDEC || cx == XXINC)	/* DECREMENT, INCREMENT */
      return(doincr(cx));
#endif /* NOSPL */

#ifndef NOSPL
    if (cx == XXDEF || cx == XXASS || cx == XXASX || cx == XXDFX)
      return(dodef(cx));		/* DEFINE, ASSIGN */
#endif /* NOSPL */

#ifndef NOSPL    
    if (cx == XXDCL) {			/* DECLARE an array */
	if ((y = cmfld("Array name","",&s,NULL)) < 0) {
	    if (y == -3) {
		printf("?Array name required\n");
		return(-9);
	    } else return(y);
	}
	if ((y = arraynam(s,&x,&z)) < 0) return(y);
	if ((y = cmcfm()) < 0) return(y);
	if (dclarray((char)x,z) < 0) {
	    printf("?Declare failed\n");
	    return(success = 0);
	}
	return(success = 1);
    }
#endif /* NOSPL */


#ifndef NODIAL
    if (cx == XXRED || cx == XXDIAL)	/* DIAL or REDIAL */
      return(dodial(cx));
#endif /* NODIAL */
 
#ifndef NOFRILLS
    if (cx == XXDEL)			/* DELETE */
      return(dodel());
#endif /* NOFRILLS */

#ifndef MAC
    if (cx == XXDIR)			/* DIRECTORY */
      return(dodir());
#endif /* MAC */
 
#ifndef NOSPL
    if (cx == XXELS)			/* ELSE */
      return(doelse());
#endif /* NOSPL */

#ifndef NOFRILLS
    if (cx == XXENA || cx == XXDIS) {	/* ENABLE, DISABLE */
	s = (cx == XXENA) ?
	  "Server function to enable" :
	    "Server function to disable";

	if ((x = cmkey(enatab,nena,s,"",xxstring)) < 0) {
	    if (x == -3) {
		printf("?Name of server function required\n");
		return(-9);
	    } else return(x);
	}
	if ((y = cmcfm()) < 0) return(y);
	return(doenable(cx,x));
    }
#endif /* NOFRILLS */

#ifndef NOSPL
    if (cx == XXRET) {			/* RETURN */
	if (cmdlvl == 0) {		/* At top level, nothing happens... */
	    if ((x = cmcfm()) < 0)
	      return(x);
	    return(success = 1);
	} else if (cmdstk[cmdlvl].src == CMD_TF) { /* In TAKE file, like POP */
	    if ((x = cmtxt("optional return value","",&s,NULL)) < 0)
	      return(x);		/* Allow trailing text, but ignore. */
	    if ((x = cmcfm()) < 0)
	      return(x);
	    popclvl();			/* pop command level */
	    return(success = 1);	/* always succeeds */
	} else if (cmdstk[cmdlvl].src == CMD_MD) { /* Within macro */  
	    if ((x = cmtxt("optional return value","",&s,NULL)) < 0)
	      return(x);
	    return(doreturn(s));	/* Trailing text is return value. */
	} else return(-2);
    }
#endif /* NOSPL */

#ifndef NOSPL
    if (cx == XXDO) {			/* DO (a macro) */
	if (nmac == 0) {
	    printf("\n?No macros defined\n");
	    return(-2);
	}
	for (y = 0; y < nmac; y++) {	/* copy the macro table */
	    mackey[y].kwd = mactab[y].kwd; /* into a regular keyword table */
	    mackey[y].kwval = y;	/* with value = pointer to macro tbl */
	    mackey[y].flgs = mactab[y].flgs;
	}
	/* parse name as keyword */
	if ((x = cmkey(mackey,nmac,"macro","",xxstring)) < 0) {
	    if (x == -3) {
		printf("?Macro name required\n");
		return(-9);
	    } else return(x);
	}
	if ((y = cmtxt("optional arguments","",&s,xxstring)) < 0)
	  return(y);			/* get args */
	return(dodo(x,s) < 1 ? (success = 0) : 1);
    }
#endif /* NOSPL */

    if (cx == XXECH) {			/* ECHO */
	if ((x = cmtxt("Material to be echoed","",&s,xxstring)) < 0)
	  return(x);
	if (*s == '{') {		/* Strip enclosing braces */
	    x = (int)strlen(s);
	    if (s[x-1] == '}') {
		s[x-1] = NUL;
		s++;
	    }
	}
	printf("%s\n",s);
	return(1);			/* Always succeeds */
    }

#ifndef NOSPL
    if (cx == XXOPE)			/* OPEN */
      return(doopen());
#endif /* NOSPL */

#ifndef NOSPL
    if (cx == XXOUT) {			/* OUTPUT */
	if ((x = cmtxt("Text to be output","",&s,NULL)) < 0)
	  return(x);
	debug(F110,"OUTPUT 1",s,0);
	if (*s == '{') {		/* Strip enclosing braces, */
	    x = (int)strlen(s);		/* if any. */
	    if (s[x-1] == '}') {
		s[x-1] = NUL;
		s++;
	    }
	}
	debug(F110,"OUTPUT 2",s,0);
	for (x = 0, y = 0; s[x]; x++, y++) { /* Convert \B, \L to \\B, \\L */
	    if (x > 0 &&
		(s[x] == 'B' || s[x] == 'b' || s[x] == 'L' || s[x] == 'l'))
	      if ((x == 1 && s[x-1] == CMDQ) ||
		  (x > 1 && s[x-1] == CMDQ && s[x-2] != CMDQ))
		line[y++] = CMDQ;
	    line[y] = s[x];
	}
	line[y++] = '\0';		/* Now expand variables, etc. */
	debug(F110,"OUTPUT 3",line,0);
	s = line+y+1;
	x = LINBUFSIZ - strlen(line) - 1;
	debug(F101,"OUTPUT size","",x);
	if (xxstring(line,&s,&x) < 0)
	  return(success = 0);
	s = line+y+1;
	debug(F110,"OUTPUT 4",s,0);
	return(success = dooutput(s));
    }
#endif /* NOSPL */

#ifdef SUNX25
    if (cx == XXPAD) {			/* PAD commands */
	x = cmkey(padtab,npadc,"PAD command","",xxstring);
	if (x == -3) {
	    printf("?You must specify a PAD command to execute\n");
	    return(-2);
	}
	if (x < 0) return(x);
    
	switch (x) {
	  case XYPADL: 
	    if (x25stat() < 0)
	      printf("Sorry, you must 'set network' & 'set host' first\r\n");
	    else {
		x25clear();
		initpad();
	    }
	    break;
	  case XYPADS:
	    if (x25stat() < 0)
	      printf("Not connected\r\n");
	    else {
		extern int linkid, lcn;
		conol("Connected thru ");
		conol(ttname);
		printf(", Link id %d, Logical channel number %d\r\n",
		       linkid,lcn);
	    }
	    break;
	  case XYPADR:
	    if (x25stat() < 0)
	      printf("Sorry, you must 'set network' & 'set host' first\r\n");
	    else
	      x25reset(0,0);
	    break;
	  case XYPADI:
	    if (x25stat() < 0)
	      printf("Sorry, you must 'set network' & 'set host' first\r\n");
	    else 
	      x25intr(0);
	}
	return(0);
}
#endif /* SUNX25 */

#ifndef NOSPL
    if (cx == XXPAU || cx == XXWAI || cx == XXMSL) /* PAUSE, WAIT, etc */
      return(dopaus(cx));
#endif /* NOSPL */

#ifndef NOFRILLS
    if (cx == XXPRI) {
	if ((x = cmifi("File to print","",&s,&y,xxstring)) < 0) {
	    if (x == -3) {
		printf("?A file specification is required\n");
		return(-9);
	    } else return(x);
	}
	if (y != 0) {
	    printf("?Wildcards not allowed\n");
	    return(-9);
	}
	strcpy(line,s);
	if ((x = cmtxt("Local print command options, or carriage return","",&s,
		       xxstring)) < 0) return(x);
	return(success = (zprint(s,line) == 0) ? 1 : 0);
    }

#ifdef TCPSOCKET
    if (cx == XXPNG) 			/* PING an IP host */
      return(doping());
#endif /* TCPSOCKET */

#ifndef MAC
    if (cx == XXPWD) {			/* PWD */
	if ((x = cmcfm()) < 0) return(x);
	xsystem(PWDCMD);
	return(success = 1);		/* blind faith */
    }
#endif /* MAC */
#endif /* NOFRILLS */

    if (cx == XXQUI || cx == XXEXI) {	/* EXIT, QUIT */
	if ((y = cmnum("exit status code","",10,&x,xxstring)) < 0) {
	    if (y == -3)
	      x = xitsta;
	    else return(y);
	}
	if ((y = cmcfm()) < 0) return(y);
#ifdef VMS
	doexit(GOOD_EXIT,x);
#else
#ifdef OSK
/* Returning any codes here makes the OS-9 shell print an error message. */
	doexit(GOOD_EXIT,-1);
#else
#ifdef datageneral
        doexit(GOOD_EXIT,x);
#else
	doexit(x,-1);
#endif /* datageneral */
#endif /* OSK */
#endif /* VMS */
    }

#ifndef NOFRILLS
    if (cx == XXERR) {			/* ERROR */
	if ((x = cmcfm()) < 0) return(x);
	ttflui();
	sstate = 'a';
	return(0);
    }
#endif /* NOFRILLS */

    if (cx == XXFIN) {			/* FINISH */
	if ((x = cmcfm()) < 0) return(x);
	sstate = setgen('F',"","","");
	if (local) ttflui();		/* If local, flush tty input buffer */
	return(0);
    }

#ifndef NOSPL
    if (cx == XXFOR)			/* FOR loop */
      return(dofor());
#endif /* NOSPL */

    if (cx == XXGET) {			/* GET */
	x = cmtxt("Name of remote file(s), or carriage return","",&cmarg,
		  xxstring);
#ifndef NOFRILLS
	if ((x == -2) || (x == -1)) return(x);
#else
	if (x < 0) return(x);
#endif /* NOFRILLS */
	if (*cmarg == '{') {		/* Strip any enclosing braces */
	    x = (int)strlen(cmarg);	/* This allows preservation of */
	    if (cmarg[x-1] == '}') {	/* leading and/or trailing */
		cmarg[x-1] = NUL;	/* spaces. */
		cmarg++;
	    }
	}
	x = doget();
#ifdef MAC
	if (sstate == 'r')
	    scrcreate();
#endif /* MAC */
	return(x);
    }

#ifndef NOSPL
#ifndef NOFRILLS
    if (cx == XXGOK) {			/* GETOK */
	return(success = doask(cx));
    }
#endif /* NOFRILLS */
#endif /* NOSPL */

    if (cx == XXHLP) {			/* HELP */
#ifdef NOHELP
	return(dohlp(XXHLP));        
#else
	x = cmkey2(cmdtab,ncmd,"C-Kermit command","help",toktab,xxstring);
	debug(F101,"HELP command x","",x);
	if (x == -5) {
	    y = chktok(toktab);
	    debug(F101,"top-level cmkey token","",y);
	    ungword();
	    switch (y) {
#ifndef NOPUSH
	      case '!': x = XXSHE; break;
#endif /* NOPUSH */
	      case '#': x = XXCOM; break;
	      case ';': x = XXCOM; break;
#ifndef NOSPL
	      case ':': x = XXLBL; break;
#endif /* NOSPL */
	      case '&': x = XXECH; break;
	      default:
		printf("\n?Invalid - %s\n",cmdbuf);
		x = -2;
	    }
	}
	return(dohlp(x));
#endif /* NOHELP */
    }
 
#ifndef NOHELP
    if (cx == XXINT)			/* INTRO */
      return(hmsga(introtxt));
#endif /* NOHELP */

    if (cx == XXHAN) {			/* HANGUP */
	if ((x = cmcfm()) < 0) return(x);
#ifndef NODIAL
	if ((x = mdmhup()) < 1)
#endif /* NODIAL */
	  x = (tthang() > -1);
	return(success = x);
    }

#ifndef NOSPL
    if (cx == XXGOTO) {			/* GOTO */
/* Note, here we don't set SUCCESS/FAILURE flag */
	if ((y = cmfld("label","",&s,xxstring)) < 0) {
	    if (y == -3) {
		printf("?Label name required\n");
		return(-9);
	    } else return(y);
	}
	strcpy(lblbuf,s);
	if ((x = cmcfm()) < 0) return(x);
	s = lblbuf;
	return(dogoto(s));
    }
#endif /* NOSPL */

#ifndef NOSPL
/* IF, Extended IF, WHILE */
    if (cx == XXIF || cx == XXIFX || cx == XXWHI) {
	return(doif(cx));
    }
#endif /* NOSPL */

#ifndef NOSPL
    if (cx == XXINP || cx == XXREI) {	/* INPUT and REINPUT */
	y = cmnum("seconds to wait for input","1",10,&x,xxstring);
	if (y < 0) {
	    return(y);
	}
	if (x <= 0) x = 1;
	if ((y = cmtxt("Material to be input","",&s,xxstring)) < 0)
	  return(y);
#ifdef COMMENT
/*
  Now it's ok -- null argument means wait for any character.
*/
	if (*s == '\0') {
	    printf("?Text required\n");
	    return(-9);
	}
#endif /* COMMENT */
	if (*s == '{') {
	    y = (int)strlen(s);
	    if (s[y-1] == '}') {
		s[y-1] = NUL;
		s++;
	    }
	}
	if (cx == XXINP) {		/* INPUT */
	    debug(F110,"calling doinput",s,0);
	    success = doinput(x,s);	/* Go try to input the search string */
	} else {			/* REINPUT */
	    debug(F110,"xxrei line",s,0);
	    success = doreinp(x,s);
	}
	if (intime && !success) {	/* TIMEOUT-ACTION = QUIT? */
	    popclvl();			/* If so, pop command level. */
	    if (pflag && cmdlvl == 0) {
		if (cx == XXINP) printf("?Input timed out\n");
		if (cx == XXREI) printf("?Reinput failed\n");
	    }
	}
	return(success);		/* Return do(re)input's return code */
    }
#endif /* NOSPL */

#ifndef NOSPL
    if (cx == XXLBL) {			/* LABEL */
	if ((x = cmfld("label","",&s,xxstring)) < 0) {
	    if (x == -3) {
		printf("?Label name required\n");
		return(-9);
	    } else return(x);
	}
	if ((x = cmcfm()) < 0) return(x);
	return(0);
    }
#endif /* NOSPL */

    if (cx == XXLOG) {			/* LOG */
	x = cmkey(logtab,nlog,"What to log","",xxstring);
	if (x == -3) {
	    printf("?Type of log required\n");
	    return(-9);
	}
	if (x < 0) return(x);
	x = dolog(x);
	if (x < 0)
	  return(x);
	else
	  return(success = x);
    }
 
#ifndef NOSCRIPT
    if (cx == XXLOGI) {			/* UUCP-style script */
	if ((x = cmtxt("expect-send expect-send ...","",&s,xxstring)) < 0)
	  return(x);
#ifdef VMS
	conres();			/* For Ctrl-C to work... */
#endif /* VMS */
	return(success = dologin(s));	/* Return 1=completed, 0=failed */
    }
#endif /* NOSCRIPT */
 
    if (cx == XXREC) {			/* RECEIVE */
	cmarg2 = "";
	x = cmofi("Name under which to store the file, or CR","",&s,
		  xxstring);
	if ((x == -1) || (x == -2)) return(x);
	if ((x = cmcfm()) < 0) return(x);
	strcpy(line,s);
	cmarg2 = line;
	debug(F111,"cmofi cmarg2",cmarg2,x);
	sstate = 'v';
#ifdef MAC
	scrcreate();
#endif /* MAC */
	if (local) displa = 1;
	return(0);
    }
 
    if (cx == XXREM) {			/* REMOTE */
	x = cmkey(remcmd,nrmt,"Remote Kermit server command","",xxstring);
	if (x == -3) {
	    printf("?You must specify a command for the remote server\n");
	    return(-9);
	}
	return(dormt(x));
    }

#ifndef NOFRILLS
    if (cx == XXREN)			/* RENAME */
      return(dorenam());
#endif /* NOFRILLS */

    if (cx == XXSEN || cx == XXMAI) {	/* SEND, MAIL */
	cmarg = cmarg2 = "";
	if ((x = cmifi("File(s) to send","",&s,&y,xxstring)) < 0) {
	    if (x == -3) {
		printf("?A file specification is required\n");
		return(-9);
	    } else return(x);
	}
	nfils = -1;			/* Files come from internal list. */
	strcpy(line,s);			/* Save copy of string just parsed. */
	strncpy(fspec,s,FSPECL);	/* and here for \v(filespec) */
	if (cx == XXSEN) {		/* SEND command */
	    debug(F101,"Send: wild","",y);
	    if (y == 0) {
		if ((x = cmtxt("Name to send it with","",&cmarg2,
			       xxstring)) < 0)
		  return(x);
	    } else {
		if ((x = cmcfm()) < 0) return(x);
	    }
	    cmarg = line;		/* File to send */
	    debug(F110,"Sending:",cmarg,0);
	    if (*cmarg2 != '\0') debug(F110," as:",cmarg2,0);
	} else {			/* MAIL */
#ifndef NOFRILLS
	    if (!atdiso || !atcapr) {	/* Disposition attribute off? */
		printf("?Disposition Attribute is Off\n");
		return(-2);
	    }
	    debug(F101,"Mail: wild","",y);
	    *optbuf = NUL;		/* Wipe out any old options */
	    if ((x = cmtxt("Address to mail to","",&s,xxstring)) < 0)
	      return(x);
	    if ((int)strlen(s) == 0) {
		printf("?Address required\n");
		return(-9);
	    }
	    strcpy(optbuf,s);
	    if ((int)strlen(optbuf) > 94) { /* Ensure legal size */
		printf("?Option string too long\n");
		return(-2);
	    }
	    cmarg = line;		/* File to send */
	    debug(F110,"Mailing:",cmarg,0);
	    debug(F110,"To:",optbuf,0);
	    rmailf = 1;			/* MAIL modifier flag for SEND */
#else
	    printf("?Sorry, MAIL feature not configured.\n");
	    return(-2);
#endif /* NOFRILLS */
	}
	sstate = 's';			/* Set start state to SEND */
#ifdef MAC
	scrcreate();
#endif /* MAC */
	if (local) {			/* If in local mode, */
	    displa = 1;			/* turn on file transfer display */
#ifdef COMMENT
/* Redundant -- this is done later in sipkt() */
	    ttflui();			/* and flush tty input buffer. */
#endif /* COMMENT */
	}
	return(0);
    }
 
#ifndef NOMSEND
    if (cx == XXMSE) {			/* MSEND command */
	nfils = 0;			/* Like getting a list of */
	lp = line;			/* files on the command line */
	while (1) {
	    char *p;
	    if ((x = cmifi("Names of files to send, separated by spaces","",
			   &s,&y,xxstring)) < 0) {
		if (x == -3) {
		    if (nfils <= 0) {
			printf("?A file specification is required\n");
			return(-9);
		    } else break;
		}
		return(x);
	    }
	    msfiles[nfils++] = lp;	/* Got one, count it, point to it, */
	    p = lp;			/* remember pointer, */
	    while (*lp++ = *s++) ;	/* and copy it into buffer */
	    debug(F111,"msfiles",msfiles[nfils-1],nfils-1);
	    if (nfils == 1) *fspec = NUL; /* Take care of \v(filespec) */
	    if (((int)strlen(fspec) + (int)strlen(p) + 1) < FSPECL) {
		strcat(fspec,p);
		strcat(fspec," ");
	    }
	}
	cmlist = msfiles;		/* Point cmlist to pointer array */
	cmarg2 = "";			/* No internal expansion list (yet) */
	sndsrc = nfils;			/* Filenames come from cmlist */
	sstate = 's';			/* Set start state to SEND */
#ifdef MAC
	scrcreate();
#endif /* MAC */
	if (local) {			/* If in local mode, */
	    displa = 1;			/* turn on file transfer display */
	    ttflui();			/* and flush tty input buffer. */
	}
	return(0);
    }
#endif /* NOMSEND */

#ifndef NOSERVER
    if (cx == XXSER) {			/* SERVER */
	if ((x = cmcfm()) < 0) return(x);
	sstate = 'x';
#ifdef MAC
	scrcreate();
#endif /* MAC */
	if (local) displa = 1;
#ifdef AMIGA
	reqoff();			/* No DOS requestors while server */
#endif /* AMIGA */
    return(0);
    }
#endif /* NOSERVER */

    if (cx == XXSET) {			/* SET command */
	x = cmkey(prmtab,nprm,"Parameter","",xxstring);
	if (x == -3) {
	    printf("?You must specify a parameter to set\n");
	    return(-9);
	}
	if (x < 0) return(x);
	/* have to set success separately for each item in doprm()... */
	/* actually not really, could have just had doprm return 0 or 1 */
	/* and set success here... */
	y = doprm(x,0);
	if (y == -3) {
	    printf("?More fields required\n");
	    return(-9);
	} else return(y);
    }

#ifndef NOPUSH
    if (cx == XXSHE) {			/* SHELL (system) command */
	if (cmtxt("System command to execute","",&s,xxstring) < 0)
	  return(-1);
	conres();			/* Make console normal  */
	x = zshcmd(s);
	concb((char)escape);
	return(success = x);
    }
#endif /* NOPUSH */

#ifndef NOSHOW
    if (cx == XXSHO) {			/* SHOW */
	x = cmkey(shotab,nsho,"","parameters",xxstring);
	if (x < 0) return(x);
	return(doshow(x));
    }
#endif /* NOSHOW */
 
#ifndef MAC
    if (cx == XXSPA) {			/* SPACE */
#ifdef datageneral
	/* AOS/VS can take an argument after its "space" command. */
	if ((x = cmtxt("Confirm, or local directory name","",&s,xxstring)) < 0)
	  return(x);
	if (*s == NUL) xsystem(SPACMD);
	else {
	    sprintf(line,"space %s",s);
	    xsystem(line);
	}
#else
#ifdef OS2
	if ((x = cmtxt("Press Enter for current disk,\n\
 or specify a disk letter like A:","",&s,xxstring)) < 0)
	  return(x);
	if (*s == NUL) {		/* Current disk */
	    printf(" Free space: %ldK\n", zdskspace(0)/1024L);
	} else {
	    int drive = toupper(*s);
	    printf(" Drive %c: %ldK free\n", drive, 
		   zdskspace(drive - 'A' + 1) / 1024L);
	}
#else
#ifdef UNIX
#ifdef COMMENT
	if ((x = cmtxt("Confirm for current disk,\n\
 or specify a disk device or directory","",&s,xxstring)) < 0)
	  return(x);
#else
	x = cmdir("Confirm for current disk,\n\
 or specify a disk device or directory","",&s,xxstring);
	if (x == -3)
	  s = "";
	else if (x < 0)
	  return(x);
	if ((x = cmcfm()) < 0) return(x);
#endif /* COMMENT */
	if (*s == NUL) {		/* Current disk */
	    xsystem(SPACMD);
	} else {			/* Specified disk */
	    sprintf(line,"%s %s",SPACM2,s);
	    xsystem(line);
	}
#else
	if ((x = cmcfm()) < 0) return(x);
	xsystem(SPACMD);
#endif /* UNIX */
#endif /* OS2 */
#endif /* datageneral */
	return(success = 1);		/* Pretend it worked */
    }
#endif /* MAC */
 
    if (cx == XXSTA) {			/* STATISTICS */
	if ((x = cmcfm()) < 0) return(x);
	return(success = dostat());
    }

    if (cx == XXSTO || cx == XXEND) {	/* STOP, END, or POP */
	if ((y = cmnum("exit status code","0",10,&x,xxstring)) < 0)
	  return(y);
	if ((y = cmtxt("Message to print","",&s,xxstring)) < 0)
	  return(y);
	if (*s == '{') {		/* Strip any enclosing braces */
	    x = (int)strlen(s);
	    if (s[x-1] == '}') {
		s[x-1] = NUL;
		s++;
	    }
	}
	if (*s) printf("%s\n",s);
	if (cx == XXSTO) dostop(); else popclvl(); 
	return(success = (x == 0));
    }

    if (cx == XXSUS) {			/* SUSPEND */
	if ((y = cmcfm()) < 0) return(y);
#ifdef NOJC
	printf("Sorry, this version of Kermit cannot be suspended\n");
#else
	stptrap(0);
#endif /* NOJC */
	return(0);
    }

    if (cx == XXTAK) {			/* TAKE */
	if (tlevel > MAXTAKE-1) {
	    printf("?Take files nested too deeply\n");
	    return(-2);
	}
	if ((y = cmifi("C-Kermit command file","",&s,&x,xxstring)) < 0) { 
	    if (y == -3) {
		printf("?A file name is required\n");
		return(-9);
	    } else return(y);
	}
	if (x != 0) {
	    printf("?Wildcards not allowed in command file name\n");
	    return(-9);
	}
	strcpy(line,s);
	if ((y = cmcfm()) < 0) return(y);
	return(success = dotake(line));
    }
 
#ifdef NETCONN
    if (cx == XXTEL) {			/* TELNET */
	if ((y = setlin(XYHOST,0)) < 0) return(y);
	return (success = (y == 0) ? 0 : doconect());
    }
#endif /* NETCONN */

#ifndef NOXMIT
    if (cx == XXTRA) {			/* TRANSMIT */
	if ((x = cmifi("File to transmit","",&s,&y,xxstring)) < 0) {
	    if (x == -3) {
		printf("?Name of an existing file\n");
		return(-9);
	    } else return(x);
	}
	if (y != 0) {
	    printf("?Only a single file may be transmitted\n");
	    return(-2);
	}
	strcpy(line,s);			/* Save copy of string just parsed. */
	if ((y = cmcfm()) < 0) return(y); /* Confirm the command */
	debug(F111,"calling transmit",line,xmitp);
	return(success = transmit(line,(char)xmitp)); /* Do the command */
    }
#endif /* NOXMIT */

#ifndef NOFRILLS
    if (cx == XXTYP) {			/* TYPE */
#ifndef MAC
	char *tc;
#endif /* MAC */
	if ((x = cmifi("File to type","",&s,&y,xxstring)) < 0) {
	    if (x == -3) {
		printf("?Name of an existing file\n");
		return(-9);
	    } else return(x);
	}
	if (y != 0) {
	    printf("?A single file please\n");
	    return(-2);
	}
#ifndef MAC
	if (!(tc = getenv("CK_TYPE"))) tc = TYPCMD;
	sprintf(line,"%s %s",tc,s);
	if ((y = cmcfm()) < 0) return(y); /* Confirm the command */
	xsystem(line);
	return(success = 1);
#else
	strcpy(line,s);
	if ((y = cmcfm()) < 0) return(y); /* Confirm the command */
	return(success = dotype(line));
#endif /* MAC */
    }
#endif /* NOFRILLS */

#ifndef NOFRILLS
    if (cx == XXTES) {			/* TEST */
	/* Fill this in with whatever is being tested... */
	if ((y = cmcfm()) < 0) return(y); /* Confirm the command */

#ifndef NOSPL
#ifdef COMMENT
	{ int d, i, j;			/* Dump all arrays */
	  char c, **p;
	  for (i = 0; i < 27; i++) {
	      p = a_ptr[i];
	      d = a_dim[i];
	      c = (i == 0) ? 64 : i + 96;
	      if (d && p) {
		  fprintf(stderr,"&%c[%d]\n",c,d);
		  for (j = 0; j <= d; j++) {
		      if (p[j]) {
			  fprintf(stderr,"  &%c[%2d] = [%s]\n",c,j,p[j]);
		      }
		  }	  
	      }
	  }
      }
#else /* Not COMMENT */
	printf("cmdlvl = %d, tlevel = %d, maclvl = %d\n",cmdlvl,tlevel,maclvl);
	if (maclvl < 0) {
	    printf("%s\n",
	     "Call me from inside a macro and I'll dump the argument stack");
	    return(0);
	}
	printf("Macro level: %d, ARGC = %d\n     ",maclvl,macargc[maclvl]);
	for (y = 0; y < 10; y++) printf("%7d",y);
	for (x = 0; x <= maclvl; x++) {
	    printf("\n%2d:  ",x);
	    for (y = 0; y < 10; y++) {
		s = m_arg[x][y];
		printf("%7s",s ? s : "(none)");
	    }
	}
	printf("\n");
#endif /* COMMENT */
#endif /* NOSPL */
	return(0);
    }
#endif /* NOFRILLS */

#ifndef NOCSETS
    if (cx == XXXLA) {	   /* TRANSLATE <ifn> from-cs to-cs <ofn> */
	int incs, outcs;
	if ((x = cmifi("File to translate","",&s,&y,xxstring)) < 0) {
	    if (x == -3) {
		printf("?Name of an existing file\n");
		return(-9);
	    } else return(x);
	}
	if (y != 0) {
	    printf("?A single file please\n");
	    return(-2);
	}
	strcpy(line,s);			/* Save copy of string just parsed. */

	if ((incs = cmkey(fcstab,nfilc,"from character-set","",xxstring)) < 0)
	  return(incs);
	if ((outcs = cmkey(fcstab,nfilc,"to character-set","",xxstring)) < 0)
	  return(outcs);
	if ((x = cmofi("output file",CTTNAM,&s,xxstring)) < 0) return(x);
	strncpy(tmpbuf,s,50);
	if ((y = cmcfm()) < 0) return(y); /* Confirm the command */
	return(success = xlate(line,tmpbuf,incs,outcs)); /* Execute it */
    }
#endif /* NOCSETS */

    if (cx == XXVER) {			/* VERSION */
	if ((y = cmcfm()) < 0) return(y);
	printf("%s,%s\n Numeric: %ld",versio,ckxsys,vernum);
	if (verwho) printf("-%d\n",verwho); else printf("\n");
	return(success = 1);
    }

#ifndef MAC
#ifndef NOFRILLS
    if (cx == XXWHO) {			/* WHO */
	char *wc;
#ifdef datageneral
        xsystem(WHOCMD);
#else
	if ((y = cmtxt("user name","",&s,xxstring)) < 0) return(y);
	if (!(wc = getenv("CK_WHO"))) wc = WHOCMD;
	sprintf(line,"%s %s",wc,s);
	xsystem(line);
#endif /* datageneral */
	return(success = 1);
    }
#endif /* NOFRILLS */
#endif /* MAC */

#ifndef NOFRILLS
    if (cx == XXWRI) {			/* WRITE */
	if ((x = cmkey(writab,nwri,"to file or log","",xxstring)) < 0) {
	    if (x == -3) printf("?Write to what?\n");
	    return(x);
	}
	if ((y = cmtxt("text","",&s,xxstring)) < 0) return(y);
	if (*s == '{') {		/* Strip enclosing braces */
	    y = (int)strlen(s);
	    if (s[y-1] == '}') {
		s[y-1] = NUL;
		s++;
	    }
	}
	switch (x) {
	  case LOGD: y = ZDFILE; break;
	  case LOGP: y = ZPFILE; break;
	  case LOGS: y = ZSFILE; break;
	  case LOGT: y = ZTFILE; break;
#ifndef NOSPL
	  case LOGW: y = ZWFILE; break;
#endif /* NOSPL */
	  case LOGX:
	  case LOGE:

#ifndef MAC
	    if (x == LOGE) fprintf(stderr,"%s",s);
	    else
#endif /* MAC */
	      printf("%s",s);
	    if (
#ifndef NOSPL
		cmdlvl == 0
#else
		tlevel == -1
#endif /* NOSPL */
		)
#ifndef MAC
	      if (x == LOGE) fprintf(stderr,"\n");
	      else
#endif /* MAC */
		printf("\n");
	    return(success = 1);
	  default: return(-2);
	}
	if ((x = zsout(y,s)) < 0)
	  printf("?File or log not open\n");
	return(success = (x == 0) ? 1 : 0);
    }
#endif /* NOFRILLS */

    debug(F101,"docmd unk arg","",cx);
    return(-2);				/* None of the above. */
} /* end of docmnd() */

#endif /* NOICP */
