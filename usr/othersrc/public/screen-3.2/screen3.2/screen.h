/* Copyright (c) 1991
 *      Juergen Weigert (jnweiger@immd4.informatik.uni-erlangen.de)
 *      Michael Schroeder (mlschroe@immd4.informatik.uni-erlangen.de)
 * Copyright (c) 1987 Oliver Laumann
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 1, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program (see the file COPYING); if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Noteworthy contributors to screen's design and implementation:
 *	Wayne Davison (davison@borland.com)
 *	Patrick Wolfe (pat@kai.com, kailand!pat)
 *	Bart Schaefer (schaefer@cse.ogi.edu)
 *	Nathan Glasser (nathan@brokaw.lcs.mit.edu)
 *	Larry W. Virden (lwv27%cas.BITNET@CUNYVM.CUNY.Edu)
 *	Howard Chu (hyc@hanauma.jpl.nasa.gov)
 *	Tim MacKenzie (tym@dibbler.cs.monash.edu.au)
 *	Markku Jarvinen (mta@{cc,cs,ee}.tut.fi)
 *	Marc Boucher (marc@CAM.ORG)
 *
 ****************************************************************
 * $Id: screen.h,v 1.2 92/02/03 02:28:13 jnweiger Exp $ FAU
 */

/* screen.h now includes enough to satisfy its own references.
 * only config.h is still needed.
 */

#include <stdio.h>
#include <errno.h>

#ifdef ISC
# ifdef ENAMETOOLONG
#  undef ENAMETOOLONG
# endif
# ifdef ENOTEMPTY
#  undef ENOTEMPTY
# endif
# include <net/errno.h>
#endif

#if defined(pyr)
extern int errno;
#endif

#ifdef sun
# define getpgrp __getpgrp
# define exit __exit
#endif

#ifdef POSIX
#include <unistd.h>
# if defined(__STDC__)
#  include <stdlib.h>
# endif
#endif

#ifdef sun
# undef getpgrp
# undef exit
#endif

#ifdef POSIX
# include <termios.h>
# ifdef hpux
#  include <bsdtty.h>
# endif
#else
# ifdef TERMIO
#  include <termio.h>
# else
#  include <sgtty.h>
# endif /* TERMIO */
#endif /* POSIX */

#if defined(BSD) || defined(sequent) || defined(pyr)
# include <strings.h>
#else /* BSD */
# ifdef SVR4
#  define strlen ___strlen___
#  include <string.h>
#  undef strlen
   extern size_t strlen(const char *);
# else /* SVR4 */
#  include <string.h>
# endif /* SVR4 */
#endif /* BSD */

#if (defined(TIOCGWINSZ) || defined(TIOCSWINSZ)) && defined(M_UNIX)
# include <sys/stream.h>
# include <sys/ptem.h>
#endif

#ifdef SUIDROOT
#  ifdef LOCKPTY
#	 undef LOCKPTY
#  endif
#endif

#ifndef UTMPOK
#  ifdef USRLIMIT
#	 undef USRLIMIT
#  endif
#endif

#ifndef LOGINDEFAULT
#  define LOGINDEFAULT 0
#endif

#if defined(LOADAV_3DOUBLES) || defined(LOADAV_3LONGS) ||\
    defined(LOADAV_4LONGS) || defined(LOADAV_NEXT)
#  define LOADAV
#endif

#ifndef FSCALE
# if defined(MIPS) || defined(SVR4)
#  define FSCALE 256            /* MIPS doesn't, and... */
# else
#  ifdef sgi
#   define FSCALE 1024.0
#  else
#   define FSCALE 1000.0 	/* Sequent doesn't define FSCALE...grrrr */
#  endif
# endif	
#endif

#ifndef F_OK
#define F_OK 0
#endif
#ifndef X_OK
#define X_OK 1
#endif
#ifndef W_OK
#define W_OK 2
#endif
#ifndef R_OK
#define R_OK 4
#endif

#ifndef MAXPATH
# define MAXPATH 1024
#endif

#ifndef SIG_T_DEFINED
# ifdef SIGVOID
#  if defined(ultrix)
#   define sig_t void
#  else /* nice compilers: */
    typedef void sig_t;
#  endif
# else
   typedef int sig_t; /* (* sig_t) */
# endif
#endif /* SIG_T_DEFINED */

#if defined(SVR4) || (defined(SYSV) && defined(ISC)) || defined(__386BSD__)
# define SIGTYPE       void
# define SIGPROTOARG   (int)
# define SIGDEFARG     int sigsig
# define SIGARG        0
#else
# define SIGPROTOARG   (void)
# define SIGDEFARG
# define SIGARG
#endif

#if (!defined(SYSV) && !defined(POSIX) && !defined(apollo)) || defined(sysV68) 
# ifndef PID_T_DEFINED
typedef int pid_t;
# endif /* PID_T_DEFINED */
#endif

#if defined(M_XENIX)
typedef int pid_t;
typedef int gid_t;
typedef int uid_t;
#endif

#if defined(UTMPOK) && defined(_SEQUENT_)
# define GETUTENT
#endif

#ifdef GETUTENT
  typedef char *slot_t;
#else
  typedef int slot_t;
#endif

#if !defined(BSD) && !defined(sequent) && !defined(NeXT)
# define index strchr
# define rindex strrchr
#endif

#ifdef SYSV /* jw. */
# define bzero(poi,len) memset(poi,0,len)
# define killpg(pgrp,sig) kill( -(pgrp), sig)
#endif

#if defined(_POSIX_SOURCE) && defined(ISC)
# ifndef O_NDELAY
#  define O_NDELAY O_NONBLOCK
# endif
#endif

/* here comes my own Free: jw. */
#define Free(a) {if ((a) == 0) abort(); else free((void *)(a)); (a)=0;}

#define Ctrl(c) ((c)&037)

/* modes for markroutine 
 */
#define PLAIN 0
#define TRICKY 1
#define CRAZY 2 /* should rather be TAXY or MAHEM though... */

/*typedef long off_t; */	/* Someone might need this */

enum state_t 
{
  LIT,				/* Literal input */
  ESC,				/* Start of escape sequence */
  ASTR,				/* Start of control string */
  STRESC,			/* ESC seen in control string */
  CSI,				/* Reading arguments in "CSI Pn ; Pn ; ... ; XXX" */
  PRIN,				/* Printer mode */
  PRINESC,			/* ESC seen in printer mode */
  PRINCSI,			/* CSI seen in printer mode */
  PRIN4			/* CSI 4 seen in printer mode */
};

enum string_t 
{
  NONE,
  DCS,				/* Device control string */
  OSC,				/* Operating system command */
  APC,				/* Application program command */
  PM,				/* Privacy message */
  AKA				/* a.k.a. for current screen */
};

#define MAXSTR		256
#define MAXARGS 	64
#define MSGWAIT 	5
#define MSGMINWAIT 	1

/* 
 * 4 <= IOSIZE <=1000
 * you may try to vary this value. Use low values if your (VMS) system
 * tends to choke when pasting. Use high values if you want to test
 * how many characters your pty's can buffer.
 */
#define IOSIZE		80

/*
 * if a nasty user really wants to try a history of 2000 lines on all 10
 * windows, he will allocate 5 MegaBytes of memory, which is quite enough.
 */
#define MAXHISTHEIGHT 3000
#define DEFAULTHISTHEIGHT 100

struct win 
{
  int wpid; /* process, that is connected to the other end of ptyfd */
  int ptyfd;	/* usually the master side of our pty pair */
  int ttyflag;	/* 1 if ptyfd is connected to a user specified tty. */
  int aflag;
  char outbuf[IOSIZE];
  int outlen;
  int autoaka, akapos;
  char cmd[MAXSTR];
  char tty[MAXSTR];
  int args[MAXARGS];
  int NumArgs;
  slot_t slot;
  char **image;
  char **attr;
  char **font;
  int LocalCharset;
  int charsets[4];
  int ss;
  int active;
  int x, y;
  char LocalAttr;
  int saved;
  int Saved_x, Saved_y;
  char SavedLocalAttr;
  int SavedLocalCharset;
  int SavedCharsets[4];
  int top, bot;
  int wrap;
  int origin;
  int insert;
  int keypad;
  int width, height;	/* width AND height, as we have now resized wins. jw.*/
  int histheight;       /* all histbases are malloced with width * histheight */
  int histidx;          /* 0= < histidx < histheight; where we insert lines */
  char **ihist; 	/* the history buffer  image */
  char **ahist; 	/* attributes */
  char **fhist; 	/* fonts */
  enum state_t state;
  enum string_t StringType;
  char string[MAXSTR];
  char *stringp;
  char *tabs;
  int vbwait;            
  int bell;
  int flow;
  int WinLink;
  FILE *logfp;
  int monitor;
  int cursor_invisible;
  int norefresh;	/* we dont redisplay when switching to that win */
};

/*
 * Definitions for flow
 *   000  -(-)
 *   001  +(-)
 *   010  -(+)
 *   011  +(+)
 *   100  -(a)
 *   111  +(a)
 */
#define FLOW_NOW	(1<<0)
#define FLOW_AUTO	(1<<1)
#define FLOW_AUTOFLAG	(1<<2)

/*
 * Parameters for the Detach() routine
 */
#define D_DETACH	0
#define D_STOP		1
#define D_REMOTE	2
#define D_POWER 	3
#define D_REMOTE_POWER	4
#define D_LOCK		5

/*
 * Here are the messages the attacher sends to the backend
 */
#define MSG_CREATE	0
#define MSG_ERROR	1
#define MSG_ATTACH	2
#define MSG_CONT	3
#define MSG_DETACH	4
#define MSG_POW_DETACH	5
#define MSG_WINCH	6
#define MSG_HANGUP	7

struct msg
{
  int type;
  union
    {
      struct
	{
	  int lflag;
	  int aflag;
	  int flowflag;
	  int hheight;  /* size of scrollback buffer */
	  int nargs;
	  char line[MAXPATH];
	  char dir[MAXPATH];
	  char screenterm[20]; /* is screen really "screen" ? */
	}
      create;
      struct
	{
	  int apid;
	  int adaptflag; /* do we wish to adapt window size? */
	  int lines, columns;
	  char tty[MAXPATH];
	  char password[20];
	  char envterm[MAXPATH];
	}
      attach;
      struct 
	{
	  char password[20];
	  int dpid;
	  char tty[MAXPATH];
	}
      detach;
      char message[MAXPATH * 2];
    } m;
};

/*
 * And the signals the attacher receives from the backend
 */
#define SIG_BYE		SIGHUP
#define SIG_POWER_BYE	SIGUSR1
#define SIG_LOCK	SIGUSR2
#define SIG_STOP	SIGTSTP
#define SIG_PW_OK	SIGUSR1
#define SIG_PW_FAIL	SIG_BYE


struct mode
{
#ifdef POSIX
  struct termios tio;
# ifdef hpux
  struct ltchars m_ltchars;
# endif
#else
# ifdef TERMIO
  struct termio tio;
# else
  struct sgttyb m_ttyb;
  struct tchars m_tchars;
  struct ltchars m_ltchars;
  int m_ldisc;
  int m_lmode;
# endif				/* TERMIO */
#endif				/* POSIX */
};

#define BELL		7
#define VBELLWAIT	1 /* No. of seconds a vbell will be displayed */

#define BELL_OFF	0 /* No bell has occurred in the window */
#define BELL_ON 	1 /* A bell has occurred, but user not yet notified */
#define BELL_DONE	2 /* A bell has occured, user has been notified */
#define BELL_VISUAL     3 /* A bell has occured in fore win, notify him visually */

#define MON_OFF 	0 /* Monitoring is off in the window */
#define MON_ON		1 /* No activity has occurred in the window */
#define MON_FOUND	2 /* Activity has occured, but user not yet notified */
#define MON_DONE	3 /* Activity has occured, user has been notified */

#define DUMP_TERMCAP	0 /* WriteFile() options */
#define DUMP_HARDCOPY	1
#define DUMP_EXCHANGE	2

#undef MAXWIN20

#ifdef MAXWIN20
#define MAXWIN	20
#else
#define MAXWIN	10
#endif

/* the key definitions are used in screen.c and help.c */
/* keep this list synchronus with the names given in fileio.c */
enum keytype
{
  KEY_IGNORE, /* Keep these first 2 at the start */
  KEY_SCREEN,
  KEY_0,  KEY_1,  KEY_2,  KEY_3,  KEY_4,
  KEY_5,  KEY_6,  KEY_7,  KEY_8,  KEY_9,
#ifdef MAXWIN20
  KEY_10, KEY_11, KEY_12, KEY_13, KEY_14,
  KEY_15, KEY_16, KEY_17, KEY_18, KEY_19,
#endif
  KEY_AKA,
  KEY_CLEAR,
  KEY_COLON,
  KEY_COPY,
  KEY_DETACH,
  KEY_FLOW,
  KEY_HARDCOPY,
  KEY_HELP,
  KEY_HISTNEXT,
  KEY_HISTORY,
  KEY_INFO,
  KEY_KILL,
  KEY_LASTMSG,
  KEY_LICENSE,
  KEY_LOCK,
  KEY_LOGTOGGLE,
  KEY_LOGIN,
  KEY_MONITOR,
  KEY_NEXT,
  KEY_OTHER,
  KEY_PASTE,
  KEY_POW_DETACH,
  KEY_PREV,
  KEY_QUIT,
  KEY_READ_BUFFER,
  KEY_REDISPLAY,
  KEY_REMOVE_BUFFERS,
  KEY_RESET,
  KEY_SET,
  KEY_SHELL,
  KEY_SUSPEND,
  KEY_TERMCAP,
  KEY_TIME,
  KEY_VBELL,
  KEY_VERSION,
  KEY_WIDTH,
  KEY_WINDOWS,
  KEY_WRAP,
  KEY_WRITE_BUFFER,
  KEY_XOFF,
  KEY_XON,
  KEY_EXTEND,
  KEY_X_WINDOWS,
  KEY_BONUSWINDOW,
  KEY_CREATE,
};

struct key 
{
  enum keytype type;
  char **args;
};

#ifdef NETHACK
#	define Msg_nomem Msg(0, "You feel stupid.")
#else
#	define Msg_nomem Msg(0, "Out of memory.")
#endif

#ifdef DEBUG
#	define debug(x) {fprintf(dfp,x);fflush(dfp);}
#	define debug1(x,a) {fprintf(dfp,x,a);fflush(dfp);}
#	define debug2(x,a,b) {fprintf(dfp,x,a,b);fflush(dfp);}
#	define debug3(x,a,b,c) {fprintf(dfp,x,a,b,c);fflush(dfp);}
	extern FILE *dfp;
#else
#	define debug(x) {}
#	define debug1(x,a) {}
#	define debug2(x,a,b) {}
#	define debug3(x,a,b,c) {}
#endif

#if defined(__STDC__)
# ifndef __P
#  define __P(a) a
# endif
#else
# ifndef __P
#  define __P(a) ()
# endif
# define const
#endif

#ifdef hpux
# define setreuid(ruid, euid) setresuid(ruid, euid, -1)
# define setregid(rgid, egid) setresgid(rgid, egid, -1)
#endif

#ifdef UTMPOK
# ifdef SVR4
#  include <utmpx.h>
#  define UTMPFILE     UTMPX_FILE
#  define utmp         utmpx
#  define getutent     getutxent
#  define getutid      getutxid
#  define getutline    getutxline
#  define pututline    pututxline
#  define setutent     setutxent
#  define endutent     endutxent
# else /* SVR4 */
#  include <utmp.h>
# endif /* SVR4 */
#endif

#ifndef UTMPFILE
# ifdef UTMP_FILE
#  define UTMPFILE     UTMP_FILE
# else
#  ifdef BSDI
#   define UTMPFILE    "/var/run/utmp"
#  else
#   define UTMPFILE     "/etc/utmp"
#  endif /* BSDI */
# endif
#endif

#if !defined(SYSV) || defined(sun) || defined(RENO) || defined(xelos)
#ifndef __386BSD__
# define BSDWAIT
#endif
#endif
