/* ckcker.h -- Symbol and macro definitions for C-Kermit */

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

#ifndef CKCKER_H
#define CKCKER_H

#ifdef NOICP				/* No Interactive Command Parser */
#ifndef NOSPL				/* implies... */
#define NOSPL				/* No Script Programming Language */
#endif /* NOSPL */
#endif /* NOICP */

/* Codes for what we are doing now */

#define W_NOTHING  0			/* Nothing */
#define W_SEND     2			/* SENDing or MAILing */
#define W_RECV     4			/* RECEIVEing or GETting */
#define W_REMO     8			/* Doing a REMOTE command */
#define W_CONNECT 16			/* CONNECT mode */
#define W_COMMAND 32			/* Command mode */

/* Terminal types */
#define VT100     0			/* Also for VT52 mode */
#define TEKTRONIX 1

/* Normal packet and window size */

#define MAXPACK 94			/* Maximum unextended packet size */
					/* Can't be more than 94! */
#define MAXWS 31			/* Maximum window size */
					/* Can't be more than 31! */

/* Maximum long packet size for sending packets */
/* Override these from cc command line via -DMAXSP=nnn */

#ifdef DYNAMIC
#ifndef MAXSP
#define MAXSP 9024
#endif /* MAXSP */
#else  /* not DYNAMIC */
#ifndef MAXSP
#define MAXSP 2048
#endif /* MAXSP */
#endif /* DYNAMIC */

/* Maximum long packet size for receiving packets */
/* Override these from cc command line via -DMAXRP=nnn */

#ifdef DYNAMIC
#ifndef MAXRP 
#define MAXRP 9024
#endif /* MAXRP */
#else  /* not DYNAMIC */
#ifndef MAXRP 
#define MAXRP 2048
#endif /* MAXRP */
#endif /* DYNAMIC */

#ifdef COMMENT				/* Restriction removed in edit 185 */
#ifdef VMS				/* Dynamic or not, */
#undef MAXSP				/* VMS seems to have an intrinsic */
#define MAXSP 1920			/* limit of about 1920. */
#undef MAXRP
#define MAXRP 1920
#endif /* VMS */
#endif /* COMMENT */

/* Default sizes for windowed packet buffers */
/* Override these from cc command line via -DSBSIZ=nnn, -DRBSIZ=nnn */

#ifdef DYNAMIC
#ifndef SBSIZ 
#define SBSIZ 9050
#endif /* SBSIZ */
#ifndef RBSIZ 
#define RBSIZ 9050
#endif /* RBSIZ */
#else  /* not DYNAMIC */
#ifndef SBSIZ
#define SBSIZ (MAXPACK * (MAXWS + 1))
#endif /* SBSIZ */
#ifndef RBSIZ
#define RBSIZ (MAXPACK * (MAXWS + 1))
#endif /* RBSIZ */
#endif /* DYNAMIC */

#ifdef COMMENT				/* Restriction removed in edit 185 */
#ifdef VMS				/* VMS again... */
#undef SBSIZ
#define SBSIZ 1930
#undef RBSIZ
#define RBSIZ 1930
#endif /* VMS */
#endif /* COMMENT */

/* Kermit parameters and defaults */

#define CTLQ	   '#'			/* Control char prefix I will use */
#define MYEBQ	   '&'			/* 8th-Bit prefix char I will use */
#define MYRPTQ	   '~'			/* Repeat count prefix I will use */

#define MAXTRY	    10			/* Times to retry a packet */
#define MYPADN	    0			/* How many padding chars I need */
#define MYPADC	    '\0'		/* Which padding character I need */

#define DMYTIM	    7			/* Default timeout interval to use. */
#define URTIME	    10			/* Timeout interval to use on me. */
#define DSRVTIM     0			/* Default server cmd wait timeout. */

#define DEFTRN	    0			/* Default line turnaround handshake */
#define DEFPAR	    0			/* Default parity */
#define MYEOL	    CR			/* Incoming packet terminator. */

#define DRPSIZ	    90			/* Default incoming packet size. */
#define DSPSIZ	    90			/* Default outbound packet size. */

#define DDELAY      5			/* Default delay. */
#define DSPEED	    9600		/* Default line speed. */

#ifdef OS2				/* Default CONNECT-mode */
#define DFESC 29			/* escape character */
#else
#ifdef NEXT				/* Ctrl-] for PC and NeXT */
#define DFESC 29
#else
#ifdef GEMDOS				/* And Atari ST */
#define DFESC 29
#else
#define DFESC 28			/* Ctrl-backslash for others */
#endif /* GEMDOS */
#endif /* NEXT */
#endif /* OS2 */

#ifdef NOPUSH				/* NOPUSH implies NOJC */
#ifndef NOJC				/* (no job control) */
#define NOJC
#endif /* NOJC */
#endif /* NOPUSH */

#ifdef UNIX				/* Default for SET SUSPEND */
#ifdef NOJC				/* UNIX but job control disabled */
#define DFSUSP      0
#else					/* UNIX, job control enabled. */
#define DFSUSP      1
#endif /* NOJC */
#else
#define DFSUSP      0
#endif /* UNIX */

/* Files */

#define ZCTERM      0	    	/* Console terminal */
#define ZSTDIO      1		/* Standard input/output */
#define ZIFILE	    2		/* Current input file (SEND, etc) (in) */
#define ZOFILE      3	    	/* Current output file (RECEIVE, GET) (out) */
#define ZDFILE      4	    	/* Current debugging log file (out) */
#define ZTFILE      5	    	/* Current transaction log file (out) */
#define ZPFILE      6	    	/* Current packet log file (out) */
#define ZSFILE      7		/* Current session log file (out) */
#define ZSYSFN	    8		/* Input/Output from a system function */
#define ZRFILE      9           /* Local file for READ (in) */
#define ZWFILE     10           /* Local file for WRITE (out) */
#define ZNFILS     11	    	/* How many defined file numbers */

/*
 Buffered file i/o is used to avoid gratuitous function calls while encoding a
 packet.  The previous way involved 2 nested function calls for EACH character
 of the file.  This way, we only do 2 calls per K of data.  This reduces
 packet encoding time to 1% of its former cost.  Originally added by Paul
 Placeway.
*/
#ifdef VMS		/* In VMS, allow for longest possible RMS record */
#ifdef DYNAMIC
#define INBUFSIZE 32768	/* File input buffer size */
#define OBUFSIZE 32768 	/* File output buffer size */
#else
#define INBUFSIZE 4096	/* File input buffer size */
#define OBUFSIZE 4096 	/* File output buffer size */
#endif /* DYNAMIC */
#else  /* Not VMS */	/* For all others, just use a 1K buffer */
#define INBUFSIZE 1024
#define OBUFSIZE 1024
#endif /* VMS */

/* get the next char; sorta like a getc() macro */
#define zminchar() (((--zincnt)>=0) ? ((int)(*zinptr++) & 0377) : zinfill())

/* stuff a character into the input buffer */
#define zmstuff(c) zinptr--, *zinptr = c, zincnt++

/* put a character to a file, like putchar() macro */
#define zmchout(c) \
((*zoutptr++=(char)(c)),(((++zoutcnt)>=OBUFSIZE)?zoutdump():0))

/* Screen functions */

#define SCR_FN 1    	/* filename */
#define SCR_AN 2    	/* as-name */
#define SCR_FS 3 	/* file-size */
#define SCR_XD 4    	/* x-packet data */
#define SCR_ST 5      	/* File status: */
#define   ST_OK   0   	/*  Transferred OK */
#define   ST_DISC 1 	/*  Discarded */
#define   ST_INT  2     /*  Interrupted */
#define   ST_SKIP 3 	/*  Skipped */
#define   ST_ERR  4 	/*  Fatal Error */
#define   ST_REFU 5     /*  Refused (use Attribute codes for reason) */
#define   ST_INC  6	/* Incompletely received */
#define SCR_PN 6    	/* packet number */
#define SCR_PT 7    	/* packet type or pseudotype */
#define SCR_TC 8    	/* transaction complete */
#define SCR_EM 9    	/* error message */
#define SCR_WM 10   	/* warning message */
#define SCR_TU 11	/* arbitrary undelimited text */
#define SCR_TN 12   	/* arbitrary new text, delimited at beginning */
#define SCR_TZ 13   	/* arbitrary text, delimited at end */
#define SCR_QE 14	/* quantity equals (e.g. "foo: 7") */
#define SCR_CW 15	/* close screen window */

/* Macros */

#define tochar(ch)  (((ch) + SP ) & 0xFF )	/* Number to character */
#define xunchar(ch) (((ch) - SP ) & 0xFF )	/* Character to number */
#define ctl(ch)     (((ch) ^ 64 ) & 0xFF )	/* Controllify/Uncontrollify */
#define unpar(ch)   (((ch) & 127) & 0xFF )	/* Clear parity bit */

/* Symbols for File Attributes */

#define AT_XALL  0			/* All of them */
#define AT_ALLY  1			/* All of them on (Yes) */
#define AT_ALLN  2			/* All of them off (no) */
#define AT_LENK  3			/* Length in K */
#define AT_FTYP  4			/* File Type */
#define AT_DATE  5			/* Creation date */
#define AT_CREA  6			/* Creator */
#define AT_ACCT  7			/* Account */
#define AT_AREA  8			/* Area */
#define AT_PSWD  9			/* Password for area */
#define AT_BLKS 10			/* Blocksize */
#define AT_ACCE 11			/* Access */
#define AT_ENCO 12			/* Encoding */
#define AT_DISP 13			/* Disposition */
#define AT_LPRO 14			/* Local Protection */
#define AT_GPRO 15			/* Generic Protection */
#define AT_SYSI 16			/* System ID */
#define AT_RECF 17			/* Record Format */
#define AT_SYSP 18			/* System-Dependent Parameters */
#define AT_LENB 19			/* Length in Bytes */

/* Kermit packet information structure */

struct pktinfo {			/* Packet information structure */
    CHAR *bf_adr;			/*  buffer address */
    int   bf_len;			/*  buffer length */
    CHAR *pk_adr;			/* Packet address within buffer */
    int   pk_len;			/*  length of data within buffer */
    int   pk_typ;			/*  packet type */
    int   pk_seq;			/*  packet sequence number */
    int   pk_flg;			/*  ack'd bit */
    int   pk_rtr;			/*  retransmission count */
};

/* File-related symbols and structures */

#define   XYFILN 0  	/*  Naming  */
#define   XYFILT 1  	/*  Type    */
#define     XYFT_T 0    /*    Text  */
#define     XYFT_B 1    /*    Binary */
#define     XYFT_I 2    /*    Image or Block (VMS) */
#define     XYFT_L 3	/*    Labeled (tagged binary) (VMS) */
#define     XYFT_U 4    /*    Binary Undefined (VMS) */
#define   XYFILW 2      /*  Warning */
#define   XYFILD 3      /*  Display */
#define     XYFD_N 0    /*    None, Off */
#define     XYFD_R 1    /*    Regular, Dots */
#define     XYFD_C 2    /*    Cursor-positioning (e.g. with curses) */
#define     XYFD_S 3    /*    Simple counter */
#define   XYFILC 4      /*  Character set */
#define   XYFILF 5      /*  Record Format */
#define     XYFF_S  0   /*    Stream */
#define     XYFF_V  1   /*    Variable */
#define     XYFF_VB 2   /*    Variable with RCW's */
#define     XYFF_F  3   /*    Fixed length */
#define     XYFF_U  4   /*    Undefined */
#define   XYFILR 6      /*  Record length */
#define   XYFILO 7      /*  Organization */
#define     XYFO_S 0    /*    Sequential */
#define     XYFO_I 1    /*    Indexed */
#define     XYFO_R 2    /*    Relative */
#define   XYFILP 8      /*  Printer carriage control */
#define     XYFP_N 0    /*    Newline (imbedded control characters) */
#define     XYFP_F 1    /*    FORTRAN (space, 1, +, etc, in column 1 */
#define     XYFP_P 2    /*    Special printer carriage controls */
#define     XYFP_X 4    /*    None */
#define   XYFILX 9      /*  Collision Action */
#define     XYFX_A 3    /*    Append */
#define     XYFX_Q 5    /*    Ask */
#define     XYFX_B 2    /*    Backup */
#define     XYFX_D 4    /*    Discard */
#define     XYFX_R 0    /*    Rename */
#define     XYFX_X 1    /*    Replace */
#define     XYFX_U 6    /*    Update */
#define   XYFILB 10     /*  Blocksize */
#define   XYFILZ 11     /*  Disposition */
#define     XYFZ_N 0    /*    New, Create */
#define     XYFZ_A 1    /*    New, append if file exists, else create */
#define     XYFZ_O 2    /*    Old, file must exist */
#define     XYFZ_X 3    /*    Output to pipe/process */
#define     XYFZ_Y 4    /*    Input from pipe/process */
#define   XYFILS 12     /*  File Byte Size */
#define   XYFILL 13     /*  File Label (VMS) */
#define   XYFILI 14     /*  File Incomplete */

/* ANSI-style forward declarations for protocol-related functions. */

_PROTOTYP( int input, (void) );
_PROTOTYP( int inibufs, (int, int) );
_PROTOTYP( int makebuf, (int, int, CHAR [], struct pktinfo *) );
_PROTOTYP( int mksbuf, (int) );
_PROTOTYP( int mkrbuf, (int) );
_PROTOTYP( int spack, (char, int, int, CHAR *) );
_PROTOTYP( VOID proto, (void) );
_PROTOTYP( int rpack, (void) );
_PROTOTYP( int ack1, (CHAR *) );
_PROTOTYP( int ackn, (int) );
_PROTOTYP( int ackns, (int, CHAR *) );
_PROTOTYP( int nack, (int) );
_PROTOTYP( int resend, (int) );
_PROTOTYP( int errpkt, (CHAR *) );
_PROTOTYP( VOID logpkt, (char, int, CHAR *) );
_PROTOTYP( CHAR dopar, (CHAR) );
_PROTOTYP( int chk1, (CHAR *) );
_PROTOTYP( unsigned int chk2, (CHAR *) );
_PROTOTYP( unsigned int chk3, (CHAR *) );
_PROTOTYP( int sipkt, (char) );
_PROTOTYP( int sinit, (void) );
_PROTOTYP( VOID rinit, (CHAR *) );
_PROTOTYP( int spar, (CHAR *) );
_PROTOTYP( int rcvfil, (char *) );
_PROTOTYP( CHAR * rpar, (void) );
_PROTOTYP( CHAR * rpar, (void) );
_PROTOTYP( int gnfile, (void) );
_PROTOTYP( int getsbuf, (int) );
_PROTOTYP( int getrbuf, (void) );
_PROTOTYP( int freesbuf, (int) );
_PROTOTYP( int freerbuf, (int) );
_PROTOTYP( int dumpsbuf, (void) );
_PROTOTYP( int dumprbuf, (void) );
_PROTOTYP( VOID freerpkt, (int) );
_PROTOTYP( int chkwin, (int, int, int) );
_PROTOTYP( int rsattr, (CHAR *) );
_PROTOTYP( char *getreason, (char *) );
_PROTOTYP( int scmd, (char, CHAR *) );
_PROTOTYP( int encstr, (CHAR *) );
_PROTOTYP( int decode, (CHAR *, int (*)(char), int) );
_PROTOTYP( int fnparse, (char *) );
_PROTOTYP( int syscmd, (char *, char *) );
_PROTOTYP( int cwd, (char *) );
_PROTOTYP( VOID screen, (int, char, long, char *) );
_PROTOTYP( int remset, (char *) );
_PROTOTYP( int initattr, (struct zattr *) );
_PROTOTYP( int gattr, (CHAR *, struct zattr *) );
_PROTOTYP( int adebu, (char *, struct zattr *) );
_PROTOTYP( int canned, (CHAR *) );
_PROTOTYP( int opent, (struct zattr *) );
_PROTOTYP( int opena, (char *, struct zattr *) );
_PROTOTYP( int openi, (char *) );
_PROTOTYP( int openo, (char *, struct zattr *, struct filinfo *) );
_PROTOTYP( int reof, (char *, struct zattr *) );
_PROTOTYP( VOID reot, (void) );
_PROTOTYP( int sfile, (int) );
_PROTOTYP( int sattr, (int) );
_PROTOTYP( int sdata, (void) );
_PROTOTYP( int seof, (CHAR *) );
_PROTOTYP( int sxeof, (CHAR *) );
_PROTOTYP( int seot, (void) );
_PROTOTYP( int window, (int) );
_PROTOTYP( int errmsg, (char *) );
_PROTOTYP( int clsif, (void) );
_PROTOTYP( int clsof, (int) );
_PROTOTYP( CHAR setgen, (char, char *, char *, char *) );
_PROTOTYP( int getpkt, (int, int) );
_PROTOTYP( int putsrv, (char) );
_PROTOTYP( int puttrm, (char) );
_PROTOTYP( int putfil, (char) );
_PROTOTYP( VOID zdstuff, (CHAR) );
_PROTOTYP( int tinit, (void) );
_PROTOTYP( VOID pktinit, (void) );
_PROTOTYP( VOID rinit, (CHAR *) );
_PROTOTYP( VOID resetc, (void) );
_PROTOTYP( VOID xsinit, (void) );
_PROTOTYP( int adjpkl, (int,int,int) );
_PROTOTYP( int chktimo, (int,int) );
_PROTOTYP( int nxtpkt, (void) );
_PROTOTYP( int ack, (void) );
_PROTOTYP( int ackns, (int, CHAR *) );
_PROTOTYP( int ackn, (int) );
_PROTOTYP( int ack1, (CHAR *) );
_PROTOTYP( int nack, (int) );
_PROTOTYP( VOID rcalcpsz, (void) );
_PROTOTYP( int resend, (int) );
_PROTOTYP( int errpkt, (CHAR *) );
_PROTOTYP( VOID srinit, (void) );
_PROTOTYP( VOID tstats, (void) );
_PROTOTYP( VOID fstats, (void) );
_PROTOTYP( VOID intmsg, (long) );
_PROTOTYP( VOID ermsg, (char *) );
_PROTOTYP( int chkint, (void) );
_PROTOTYP( VOID sdebu, (int) );
_PROTOTYP( VOID rdebu, (CHAR *, int) );
_PROTOTYP( char * dbchr, ( int ) );
#ifdef COMMENT
_PROTOTYP( SIGTYP stptrap, (int, int) );
_PROTOTYP( SIGTYP trap, (int, int) );
#else
_PROTOTYP( SIGTYP stptrap, (int) );
_PROTOTYP( SIGTYP trap, (int) );
#endif /* COMMENT */

/* User interface functions needed by main program, etc. */

_PROTOTYP( VOID prescan, (void) );
_PROTOTYP( VOID setint, (void) );
_PROTOTYP( VOID cmdini, (void) );
_PROTOTYP( int dotake, (char *) );
_PROTOTYP( int cmdlin, (void) );
_PROTOTYP( int conect, (void) );
_PROTOTYP( int ckcgetc, (int) );
_PROTOTYP( int ckcputc, (int) );
_PROTOTYP (int mdmhup, (void) );
_PROTOTYP( VOID herald, (void) );
_PROTOTYP( VOID fixcmd, (void) );
_PROTOTYP( int doarg, (char) );
_PROTOTYP( VOID usage, (void) );
_PROTOTYP( VOID doclean, (void) );
_PROTOTYP( int sndhlp, (void) );
_PROTOTYP( VOID ckhost, (char *, int) );
_PROTOTYP( int gettcs, (int, int) );

#ifdef KANJI
_PROTOTYP( int zkanji, (int (*)(void)) ); /* Kanji function prototypes */
_PROTOTYP( int zkanjf, (void) );
_PROTOTYP( int zkanjz, (void) );
_PROTOTYP( int xkanjz, (int (*)(char) ) );
_PROTOTYP( int xkanji, (int, int (*)(char) ) );
#endif /* KANJI */

#endif /* CKCKER_H */

/* End of ckcker.h */
