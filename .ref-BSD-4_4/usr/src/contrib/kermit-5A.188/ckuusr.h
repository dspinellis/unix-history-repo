/*  C K U U S R . H  --  Symbol definitions for C-Kermit ckuus*.c modules  */
 
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
#ifndef CKUUSR_H
#define CKUUSR_H

#include "ckucmd.h"			/* Get symbols from command package */
 
/* Sizes of things */

#define FSPECL 300			/* Max length for MSEND/GET string */
#define VNAML 20			/* Max length for variable name */
#define FORDEPTH 10			/* Maximum depth of nested FOR loops */
#define GVARS 126			/* Highest global var allowed */
#define MAXTAKE 30			/* Maximum nesting of TAKE files */
#define MACLEVEL 50			/* Maximum nesting for macros */
#define NARGS 10			/* Max number of macro arguments */
#define LINBUFSIZ CMDBL+10		/* Size of line[] buffer */
#define INPBUFSIZ 257			/* Size of INPUT buffer */
#define CMDSTKL ( MACLEVEL + MAXTAKE + 2) /* Command stack depth */
#define MAC_MAX 256			/* Maximum number of macros */
#define MSENDMAX 100			/* Number of filespecs for MSEND */

struct cmdptr {				/* Command stack structure */
    int src;				/* Command Source */
    int lvl;				/* Current TAKE or DO level */
};

struct mtab {				/* Macro table, like keyword table */
    char *kwd;				/* But with pointers for vals */
    char *mval;				/* instead of ints. */
    short flgs;
};

/* Name of C-Kermit program initialization file. */

#ifdef vms
#define KERMRC "CKERMIT.INI"
#else
#ifdef OS2
#define KERMRC "ckermit.ini"
#else
#ifdef UNIX
#define KERMRC ".kermrc"
#else
#ifdef OSK
#define KERMRC ".kermrc"
#else
#define KERMRC "CKERMIT.INI"
#endif /* OSK */
#endif /* UNIX */
#endif /* OS2 */
#endif /* vms */

/* Includes */

#ifdef MINIX
/* why? */
#include <sys/types.h>
#endif /* MINIX */

/* Symbols for command source */

#define CMD_KB 0			/* KeyBoard or standard input */
#define CMD_TF 1			/* TAKE command File */
#define CMD_MD 2			/* Macro Definition */

/* Top Level Commands */
/* Values associated with top-level commands must be 0 or greater. */
 
#define XXBYE   0	/* BYE */
#define XXCLE   1	/* CLEAR */
#define XXCLO   2	/* CLOSE */
#define XXCON   3	/* CONNECT */
#define XXCPY   4	/* COPY */
#define XXCWD   5	/* CWD (Change Working Directory) */
#define XXDEF	6	/* DEFINE (a command macro) */
#define XXDEL   7	/* (Local) DELETE */
#define XXDIR   8	/* (Local) DIRECTORY */
#define XXDIS   9	/* DISABLE <-- changed from DISCONNECT! */
#define XXECH  10	/* ECHO */
#define XXEXI  11	/* EXIT */
#define XXFIN  12	/* FINISH */
#define XXGET  13	/* GET */
#define XXHLP  14	/* HELP */
#define XXINP  15	/* INPUT */
#define XXLOC  16	/* LOCAL */
#define XXLOG  17	/* LOG */
#define XXMAI  18	/* MAIL */
#define XXMOU  19	/* (Local) MOUNT */
#define XXMSG  20	/* (Local) MESSAGE */
#define XXOUT  21	/* OUTPUT */
#define XXPAU  22	/* PAUSE */
#define XXPRI  23	/* (Local) PRINT */
#define XXQUI  24	/* QUIT */
#define XXREC  25	/* RECEIVE */
#define XXREM  26	/* REMOTE */
#define XXREN  27	/* (Local) RENAME */
#define XXSEN  28	/* SEND */
#define XXSER  29   	/* SERVER */
#define XXSET  30	/* SET */
#define XXSHE  31	/* Command for SHELL */
#define XXSHO  32	/* SHOW */
#define XXSPA  33	/* (Local) SPACE */
#define XXSTA  34	/* STATISTICS */
#define XXSUB  35	/* (Local) SUBMIT */
#define XXTAK  36	/* TAKE */
#define XXTRA  37	/* TRANSMIT */
#define XXTYP  38	/* (Local) TYPE */
#define XXWHO  39	/* (Local) WHO */
#define XXDIAL 40	/* (Local) DIAL */
#define XXLOGI 41	/* (Local) SCRIPT */
#define XXCOM  42	/* Comment */
#define XXHAN  43       /* HANGUP */
#define XXXLA  44	/* TRANSLATE */
#define XXIF   45	/* IF */
#define XXLBL  46       /* label */
#define XXGOTO 47	/* GOTO */
#define XXEND  48       /* END */
#define XXSTO  49       /* STOP */
#define XXDO   50       /* DO */
#define XXPWD  51       /* PWD */
#define XXTES  52       /* TEST */
#define XXASK  53       /* ASK */
#define XXASKQ 54       /* ASKQ */
#define XXASS  55       /* ASSIGN */
#define XXREI  56       /* REINPUT */
#define XXINC  57       /* INCREMENT */
#define XXDEC  59       /* DECREMENT */
#define XXELS  60       /* ELSE */
#define XXEXE  61	/* EXECUTE */
#define XXWAI  62	/* WAIT */
#define XXVER  63       /* VERSION */
#define XXENA  64       /* ENABLE */
#define XXWRI  65       /* WRITE */
#define XXCLS  66       /* CLS (clear screen) */
#define XXRET  67	/* RETURN */
#define XXOPE  68       /* OPEN */
#define XXREA  69	/* READ */
#define XXON   70       /* ON */
#define XXDCL  71       /* DECLARE */
#define XXBEG  72       /* BEGIN (not used) */
#define XXFOR  72       /* FOR */
#define XXWHI  73       /* WHILE */
#define XXIFX  74       /* Extended IF */
#define XXCMS  75       /* SEND from command output (not yet) */
#define XXCMR  76       /* RECEIVE to a command's input (not yet) */
#define XXCMG  77       /* GET to a command's input (not yet) */
#define XXSUS  78       /* SUSPEND */
#define XXERR  79       /* ERROR */
#define XXMSE  80       /* MSEND */
#define XXBUG  81       /* BUG */
#define XXPAD  82       /* PAD (as in X.25 PAD) SUNX25 */
#define XXRED  83       /* REDIAL */
#define XXGTA  84	/* _getargs (invisible internal) */
#define XXPTA  85	/* _putargs (invisible internal) */
#define XXGOK  86       /* GETOK - Ask for YES/NO */
#define XXTEL  87	/* TELNET */
#define XXASX  88	/* _ASSIGN (evaluates var name) */
#define XXDFX  89	/* _DEFINE (evaluates var name) */
#define XXPNG  90	/* PING (for TCP/IP) */
#define XXINT  91       /* INTRODUCTION */
#define XXCHK  92	/* CHECK (a feature) */
#define XXMSL  93       /* MSLEEP, MPAUSE (millisecond sleep) */

/* IF conditions */

#define  XXIFCO 0       /* IF COUNT */
#define  XXIFER 1       /* IF ERRORLEVEL */
#define  XXIFEX 2       /* IF EXIST */
#define  XXIFFA 3       /* IF FAILURE */
#define  XXIFSU 4       /* IF SUCCESS */
#define  XXIFNO 5       /* IF NOT */
#define  XXIFDE 6       /* IF DEFINED */
#define  XXIFEQ 7	/* IF EQUAL (strings) */
#define  XXIFAE 8       /* IF = (numbers) */
#define  XXIFLT 9       /* IF < (numbers) */
#define  XXIFGT 10      /* IF > (numbers) */
#define  XXIFLL 11      /* IF Lexically Less Than (strings) */
#define  XXIFLG 12      /* IF Lexically Greater Than (strings) */
#define  XXIFEO 13      /* IF EOF (READ file) */
#define  XXIFBG 14      /* IF BACKGROUND */
#define  XXIFNU 15	/* IF NUMERIC */
#define  XXIFFG 16      /* IF FOREGROUND */

/* SET parameters */
 
#define XYBREA  0	/* BREAK simulation */
#define XYCHKT  1	/* Block check type */
#define XYDEBU  2	/* Debugging */
#define XYDELA  3	/* Delay */
#define XYDUPL  4	/* Duplex */
#define XYEOL   5	/* End-Of-Line (packet terminator) */
#define XYESC   6	/* Escape character */
#define XYFILE  7	/* File Parameters */
			/* (this space available) */
#define XYFLOW  9	/* Flow Control */
#define XYHAND 10	/* Handshake */
#define XYIFD  11	/* Incomplete File Disposition */
#define XYIMAG 12	/* "Image Mode" */
#define XYINPU 13	/* INPUT command parameters */
#define XYLEN  14	/* Maximum packet length to send */
#define XYLINE 15	/* Communication line to use */
#define XYLOG  16	/* Log file */
#define XYMARK 17	/* Start of Packet mark */
#define XYNPAD 18	/* Amount of padding */
#define XYPADC 19	/* Pad character */
#define XYPARI 20	/* Parity */
#define XYPAUS 21	/* Interpacket pause */
#define XYPROM 22	/* Program prompt string */
#define XYQBIN 23	/* 8th-bit prefix */
#define XYQCTL 24	/* Control character prefix */
#define XYREPT 25	/* Repeat count prefix */
#define XYRETR 26	/* Retry limit */
#define XYSPEE 27	/* Line speed (baud rate) */
#define XYTACH 28	/* Character to be doubled */
#define XYTIMO 29	/* Timeout interval */
#define XYMODM 30	/* Modem type */
#define XYSEND 31	/* SEND parameters, used with some of the above */
#define XYRECV 32   	/* RECEIVE parameters, ditto */
#define XYTERM 33	/* Terminal parameters */
#define   XYTBYT 0      /*  Terminal Bytesize (7 or 8) */
#define   XYTTYP 1      /*  Terminal Type */
#define     TT_NONE  0	/*    NONE */
#define     TT_VT52  1	/*    DEC VT-52  */
#define     TT_VT100 2	/*    DEC VT-100 */
#define     TT_VT102 3	/*    DEC VT-102 */
#define     TT_VT220 4	/*    DEC VT-220 */
#define     TT_VT320 5	/*    DEC VT-320 */
#define     TT_TEK40 6	/*    Tektronix 401x */
#define   XYTCS  2      /*  Terminal Character Set */
#define   XYTSO  3	/*  Terminal Shift-In/Shift-Out */
#define   XYTNL  4      /*  Terminal newline mode */
#define   XYTCOL 5      /*  Terminal colors */
#define   XYTEC  6	/*  Terminal echo = duplex = local-echo */
#define   XYTCUR 7	/*  Terminal cursor */
#define   XYTARR 8	/*  Terminal arrow-key mode */
#define   XYTKPD 9      /*  Terminal keypad mode */
#define   XYTWRP 10     /*  Terminal wrap */
#define   XYTCRD 11	/*  Terminal CR-display */
#define XYATTR 34       /* Attribute packets */
#define XYSERV 35	/* Server parameters */
#define   XYSERT 0      /*  Server timeout   */
#define   XYSERD 1	/*  Server display   */
#define XYWIND 36       /* Window size */
#define XYXFER 37       /* Transfer */
#define XYLANG 38       /* Language */
#define XYCOUN 39       /* Count */
#define XYTAKE 40       /* Take */ 
#define XYUNCS 41       /* Unknown-character-set */
#define XYKEY  42       /* Key */
#define XYMACR 43       /* Macro */
#define XYHOST 44       /* Hostname on network */
#define XYNET  45       /* Type of Network */
#define XYCARR 46	/* Carrier */
#define XYXMIT 47       /* Transmit */
#define XYDIAL 48       /* Dial options */
#define  XYDHUP  0	/*   Dial Hangup */
#define  XYDINI  1      /*   Dial Initialization string */
#define  XYDKSP  2      /*   Dial Kermit-Spoof */
#define  XYDTMO  3      /*   Dial Timeout */
#define  XYDDPY  4      /*   Dial Display */
#define  XYDSPD  5      /*   Dial Speed matching */
#define  XYDMNP  6	/*   Dial MNP negotiation enabled */
#define  XYDV32  7	/*   Dial V.32 mode enabled */
#define  XYDV42  8	/*   Dial V.42 mode enabled */
#define  XYDV42B 9	/*   Dial V.42bis mode enabled */
#define  XYDDIR 10	/*   Dial directory */
#define  XYDDIA 11	/*   Dial dial-command */
#define  XYDMHU 12	/*   Dial modem-hangup */
#define  XYDNPR 13      /*   Dial number-prefix */
#define XYSESS 49       /* SET SESSION options */
#define XYBUF  50       /* Buffer length */
#define XYBACK 51	/* Background */
#define XYDFLT 52       /* Default */
#define XYDOUB 53	/* Double */
#define XYCMD  54       /* Command */
#define XYCASE 55       /* Case */
#define XYCOMP 56       /* Compression */
#define XYX25  57       /* X.25 parameter (SUNX25) */
#define XYPAD  58       /* X.3 PAD parameter (SUNX25) */
#define XYWILD 59       /* Wildcard expansion method */
#define XYSUSP 60       /* Suspend */
#define XYMAIL 61	/* Mail-Command */
#define XYPRIN 62	/* Print-Command */
#define XYQUIE 63	/* Quiet */
#define XYLCLE 64	/* Local-echo */
#define XYSCRI 65	/* SCRIPT command paramaters */
#define XYMSGS 66       /* MESSAGEs ON/OFF */
#define XYTEL  67       /* TELNET parameters */
#define  CK_TN_EC 0	/*  TELNET ECHO */
#define  CK_TN_TT 1	/*  TELNET TERMINAL-TYPE */
#define  CK_TN_NL 2     /*  TELNET NEWLINE-MODE */

/* #ifdef SUNX25 */
/* PAD command parameters */

#define XYPADL 0        /* clear virtual call */
#define XYPADS 1        /* status of virtual call */
#define XYPADR 2        /* reset of virtual call */
#define XYPADI 3        /* send an interrupt packet */

/* Used with XYX25... */
#define XYUDAT 0       /* X.25 call user data */
#define XYCLOS 1       /* X.25 closed user group call */
#define XYREVC 2       /* X.25 reverse charge call */
/* #endif */ /* SUNX25 */

/* SHOW command symbols */

#define SHPAR 0				/* Parameters */
#define SHVER 1				/* Versions */
#define SHCOM 2				/* Communications */
#define SHPRO 3				/* Protocol */
#define SHFIL 4				/* File */
#define SHLNG 5				/* Language */
#define SHCOU 6				/* Count */
#define SHMAC 7				/* Macros */
#define SHKEY 8				/* Key */
#define SHSCR 9				/* Scripts */
#define SHSPD 10			/* Speed */
#define SHSTA 11			/* Status */
#define SHSER 12			/* Server */
#define SHXMI 13			/* Transmit */
#define SHATT 14			/* Attributes */
#define SHMOD 15			/* Modem */
#define SHDFLT 16			/* Default (as in VMS) */
#define SHVAR 17			/* Show global variables */
#define SHARG 18			/* Show macro arguments */
#define SHARR 19			/* Show arrays */
#define SHBUI 20			/* Show builtin variables */
#define SHFUN 21			/* Show functions */
#define SHPAD 22			/* Show (X.25) PAD */
#define SHTER 23			/* Show terminal settings */
#define SHESC 24			/* Show escape character */
#define SHDIA 25			/* Show DIAL parameters */
#define SHNET 26			/* Show network parameters */
#define SHLBL 27			/* Show VMS labeled file parameters */
#define SHSTK 28			/* Show stack, MAC debugging */
#define SHCSE 29			/* Show character sets */
#define SHFEA 30			/* Show features */

/* REMOTE command symbols */
 
#define XZCPY  0	/* Copy */
#define XZCWD  1	/* Change Working Directory */
#define XZDEL  2	/* Delete */
#define XZDIR  3	/* Directory */
#define XZHLP  4	/* Help */
#define XZHOS  5	/* Host */
#define XZKER  6	/* Kermit */
#define XZLGI  7	/* Login */
#define XZLGO  8	/* Logout */
#define XZMAI  9	/* Mail <-- wrong, this should be top-level */
#define XZMOU 10	/* Mount */
#define XZMSG 11	/* Message */
#define XZPRI 12	/* Print */
#define XZREN 13	/* Rename */
#define XZSET 14	/* Set */
#define XZSPA 15	/* Space */
#define XZSUB 16	/* Submit */
#define XZTYP 17	/* Type */
#define XZWHO 18	/* Who */
 
/* SET INPUT command parameters */

#define IN_DEF  0			/* Default timeout */
#define IN_TIM  1			/* Timeout action */
#define IN_CAS  2			/* Case (matching) */
#define IN_ECH  3			/* Echo */
#define IN_SIL  4			/* Silence */

/* ENABLE/DISABLE command parameters */

#define EN_ALL  0			/* All */
#define EN_CWD  1			/* CD/CWD */
#define EN_DIR  2			/* Directory */
#define EN_FIN  3			/* FINISH */
#define EN_GET  4			/* Get */
#define EN_HOS  5			/* Host command */
#define EN_KER  6			/* Kermit command */
#define EN_LOG  7			/* Login */
#define EN_SEN  8			/* Send */
#define EN_SET  9			/* Set */
#define EN_SPA 10			/* Space */
#define EN_TYP 11			/* Type */
#define EN_WHO 12			/* Who/Finger */
#define EN_DEL 13			/* Delete */
#define EN_BYE 14			/* BYE (as opposed to FINISH) */

/* Symbols for logs */
 
#define LOGD 0	    	/* Debugging */
#define LOGP 1          /* Packets */
#define LOGS 2          /* Session */
#define LOGT 3          /* Transaction */
#define LOGX 4          /* Screen */
#define LOGR 5		/* The "OPEN read file */
#define LOGW 6          /* The "OPEN" write/append file */
#define LOGE 7		/* Error (e.g. stderr) */

/* Symbols for builtin variables */

#define VN_ARGC 0			/* ARGC */
#define VN_COUN 1			/* COUNT */
#define VN_DATE 2			/* DATE */
#define VN_DIRE 3			/* DIRECTORY */
#define VN_ERRO 4			/* ERRORLEVEL */
#define VN_TIME 5			/* TIME */
#define VN_VERS 6			/* VERSION */
#define VN_IBUF 7			/* INPUT buffer */
#define VN_SUCC 8			/* SUCCESS flag */
#define VN_LINE 9			/* LINE */
#define VN_ARGS 10			/* Program command-line arg count */
#define VN_SYST 11			/* System type */
#define VN_SYSV 12			/* System version */
#define VN_RET  13			/* RETURN value */
#define VN_FILE 14			/* Most recent filespec */
#define VN_NDAT 15			/* Numeric date yyyy/mm/dd */
#define VN_HOME 16			/* Home directory */
#define VN_SPEE 17			/* Transmission speed */
#define VN_HOST 18			/* Host name */
#define VN_TTYF 19			/* TTY file descriptor (UNIX only) */
#define VN_PROG 20			/* Program name */
#define VN_NTIM 21			/* NTIME */
#define VN_FFC  22			/* Characters in last file xferred */
#define VN_TFC  23			/* Chars in last file group xferred */
#define VN_CPU  24			/* CPU type */
#define VN_CMDL 25			/* Command level */
#define VN_DAY  26                      /* Day of week, string */
#define VN_NDAY 27                      /* Day of week, numeric */
#define VN_LCL  28			/* Local (vs) remote mode */
#define VN_CMDS 29			/* Command source */
#define VN_CMDF 30			/* Command file name */
#define VN_MAC  31			/* Macro name */
#define VN_EXIT 32			/* Exit status */
#define VN_ICHR 33			/* INPUT character */
#define VN_ICNT 34			/* INPUT count */

/* Symbols for builtin functions */

#define FNARGS 6			/* Maximum number of function args */

#define FN_IND 0			/* Index (of string 1 in string 2) */
#define FN_LEN 1			/* Length (of string) */
#define FN_LIT 2			/* Literal (don't expand the string) */
#define FN_LOW 3			/* Lower (convert to lowercase) */
#define FN_MAX 4			/* Max (maximum) */
#define FN_MIN 5			/* Min (minimum) */
#define FN_MOD 6			/* Mod (modulus) */
#define FN_EVA 7			/* Eval (evaluate arith expression) */
#define FN_SUB 8			/* Substr (substring) */
#define FN_UPP 9			/* Upper (convert to uppercase) */
#define FN_REV 10			/* Reverse (a string) */
#define FN_REP 11			/* Repeat (a string) */
#define FN_EXE 12			/* Execute (a macro) */
#define FN_VAL 13			/* Return value (of a macro) */
#define FN_LPA 14			/* LPAD (left pad) */
#define FN_RPA 15			/* RPAD (right pad) */
#define FN_DEF 16			/* Definition of a macro, unexpanded */
#define FN_CON 17			/* Contents of a variable, ditto */
#define FN_FIL 18                       /* File list */
#define FN_FC  19			/* File count */
#define FN_CHR 20			/* Character (like BASIC CHR$()) */
#define FN_RIG 21			/* Right (like BASIC RIGHT$()) */
#define FN_COD 22			/* Code value of character */

/* ANSI-style prototypes for user interface functions */

_PROTOTYP( int parser, ( int ) );
_PROTOTYP( int xxstring, (char *, char **, int *) );
_PROTOTYP( int yystring, (char *, char **) );
_PROTOTYP( int xxstrcmp, (char *, char *, int) );
_PROTOTYP( int xxout, (char) );
_PROTOTYP( int getncm, (char *, int) );
_PROTOTYP( int getnct, (char *, int) );
_PROTOTYP( VOID bgchk, (void) );
_PROTOTYP( char * fneval, (char *, char * [], int ) );
_PROTOTYP( char * nvlook, (char *) );
_PROTOTYP( char * arrayval, (int, int) );
_PROTOTYP( int arraynam, (char *, int *, int *) );
_PROTOTYP( char * bldlen, (char *, char *) );
_PROTOTYP( int chkarray, (int, int) );
_PROTOTYP( int dclarray, (char, int) );
_PROTOTYP( int parsevar, (char *, int *, int *) );
_PROTOTYP( int macini, (void) );
_PROTOTYP( VOID initmac, (void) );
_PROTOTYP( int delmac, (char *) );
_PROTOTYP( int addmac, (char *, char *) );
_PROTOTYP( int addmmac, (char *, char *[]) );
_PROTOTYP( int dobug, (void) );
_PROTOTYP( int docd, (void) );
_PROTOTYP( int doclslog, (int) );
_PROTOTYP( int docmd, (int) );
_PROTOTYP( int doconect, (void) );
_PROTOTYP( int dodo, (int, char *) );
_PROTOTYP( int doenable, (int, int) );
_PROTOTYP( int doget, (void) );
_PROTOTYP( int dogoto, (char *) );
_PROTOTYP( int dohlp, (int) );
_PROTOTYP( int dohrmt, (int) );
_PROTOTYP( int doif, (int) );
_PROTOTYP( int doinput, (int, char *) );
_PROTOTYP( int doreinp, (int, char *) );
_PROTOTYP( int dolog, (int) );
_PROTOTYP( int dologin, (char *) );
_PROTOTYP( int doopen, (void) );
_PROTOTYP( int dooutput, (char *) );
_PROTOTYP( int doprm, (int, int) );
_PROTOTYP( int doreturn, (char *) );
_PROTOTYP( int dormt, (int) );
_PROTOTYP( int doshow, (int) );
_PROTOTYP( int doshodial, (void) );
_PROTOTYP( int dostat, (void) );
_PROTOTYP( int dostop, (void) );
_PROTOTYP( int dotype, (char *) );
_PROTOTYP( int transmit, (char *, char) );
_PROTOTYP( int xlate, (char *, char *, int, int) );
_PROTOTYP( int litcmd, (char **, char **) );
_PROTOTYP( int incvar, (char *, int, int, int *) );
_PROTOTYP( int ckdial, (char *) );
_PROTOTYP( char * getdws, (int) );
_PROTOTYP( char * getdcs, (int) );
_PROTOTYP( int hmsg, (char *) );
_PROTOTYP( int hmsga, (char * []) );
_PROTOTYP( int mlook, (struct mtab [], char *, int) );
_PROTOTYP( int mxlook, (struct mtab [], char *, int) );
_PROTOTYP( VOID prtopt, (char *) );
_PROTOTYP( CHAR rfilop, (char *, char) );
_PROTOTYP( int setcc, (int *, int, int) );
_PROTOTYP( int setnum, (int *, int, int, int) );
_PROTOTYP( int seton, (int *) );
_PROTOTYP( VOID shmdmlin, (void) );
_PROTOTYP( int shoatt, (void) );
_PROTOTYP( VOID shocharset, (void) );
_PROTOTYP( int shomac, (char *, char *) );
_PROTOTYP( VOID shopar, (void) );
_PROTOTYP( VOID shoparc, (void) );
_PROTOTYP( VOID shoparc, (void) );
_PROTOTYP( VOID shoparf, (void) );
_PROTOTYP( VOID shoparp, (void) );
#ifndef NOCSETS
_PROTOTYP( VOID shoparl, (void) );
#endif /* NOCSETS */
_PROTOTYP( VOID shodial, (void) );
_PROTOTYP( VOID shomdm, (void) );
_PROTOTYP( VOID shonet, (void) );
_PROTOTYP( VOID shover, (void) );
_PROTOTYP( int pktopn, (char *,int) );
_PROTOTYP( int traopn, (char *,int) );
_PROTOTYP( int sesopn, (char *,int) );
_PROTOTYP( int debopn, (char *,int) );
_PROTOTYP( char * parnam, (char) );
_PROTOTYP( int popclvl, (void) );
_PROTOTYP( int varval, (char *, int *) );
_PROTOTYP( char * evala, (char *) );
_PROTOTYP( int setat, (int) );
_PROTOTYP( int setinp, (void) );
_PROTOTYP( int setlin, (int, int) );
_PROTOTYP( int setdial, (void) );
_PROTOTYP( int setfil, (int) );
_PROTOTYP( int settrm, (void) );
_PROTOTYP( int setsr, (int, int) );
_PROTOTYP( int setxmit, (void) );
_PROTOTYP( int set_key, (void) );
_PROTOTYP( int dochk, (void) );
_PROTOTYP( char *ludial, (char *, FILE *) );
_PROTOTYP( VOID xwords, (char *, int, char *[]) );
_PROTOTYP( VOID shotcs, (int, int) );
_PROTOTYP( char *hhmmss, (long x) );
#endif /* CKUUSR_H */

/* End of ckuusr.h */
