/*
 *      DEFS.H
 *      UTREE global definitions.
 *      3.01-um klin, Tue Jun  4 14:16:55 1991
 *              klin, Tue Oct 15 14:01:57 1991, Handling of symlinks changed
 *              klin, Sat Oct 26 15:27:40 1991, Some additions
 *      3.02-um klin, Fri Nov  1 10:43:31 1991, Screen layout changed
 *              klin, Sun Nov 24 19:30:43 1991, Cd to current directory before
 *                                              executing some commands
 *      3.03-um klin, Sat Jan 11 19:53:57 1992, Generic list glist added
 *                                              List definitions to list.h
 *                                              Line editor extended
 *              klin, Sat Feb 15 19:18:06 1992, Display attributes added
 *                                              Partinioning of directory and
 *                                              file windows on tree screen
 *                                              changed
 *              klin, Sun Feb 23 18:16:24 1992, Key handlings and key bindings
 *                                              changed
 *            e klin, Sat Apr 11 11:05:54 1992, Use colors for video attributes
 *
 *      Copyright (c) 1991/92 by Peter Klingebiel & UNIX Magazin Muenchen.
 *      For copying and distribution information see the file COPYRIGHT.
 */
#if     defined(_MAIN_) && !defined(lint)
static char sccsid_defs[] = "@(#) utree 3.03e-um (klin) Apr 11 1992 defs.h";
#endif  /* _MAIN_ && !lint */

/*
 *      Global and local include files.
 */

#include "conf.h"               /* Configurable definitions             */
#include "cmds.h"               /* System commands definitions          */

#include <stdio.h>
#include <signal.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <setjmp.h>
#ifdef  BSD
# include <sgtty.h>
# include <sys/dir.h>
#else   /* SYSV */
# include <sys/sysmacros.h>
# include <termio.h>
# ifdef NODIRENT
#  include <sys/dir.h>
# else  /* !NODIRENT */
#  include <dirent.h>
# endif /* NODIRENT */
#endif  /* BSD */
#include <pwd.h>
#include <grp.h>
#include <time.h>
#include <varargs.h>

#ifdef  NULL                    /* Reset the standard NULL definition   */
# undef NULL
#endif  /* NULL */
#define NULL    ((char *) 0)    /* The string NIL pointer               */

#ifdef  NOWINCHG
# undef SIGWINCH
# undef TIOCGWINSZ
#endif  /* NOWINCHG */

/*
 *      Definitions for utree which should not be changed.
 */

/* Buffer size definitions to avoid too often alloc() and free()        */
#define NBUFSZ  1024            /* Filename buffer size                 */
#define NFILES  64              /* Filename vector size (= NBUFSZ/16)   */
#define NBUFINC 512             /* Filename buffer increment            */
#define NFILINC 32              /* Filename vector incr (= NBUFINC/16)  */

/* Other global definitions                                             */
#ifndef MAXPATHLEN
# define NAMELEN 256            /* Max length of pathnames              */
#else   /* !MAXPATHLEN */
# define NAMELEN MAXPATHLEN
#endif  /* MAXPATHLEN */
#define FILELEN 256             /* Max input length of filenames        */
#define INPLEN  256             /* Often used input buffer length       */
#define PATLEN  256             /* Search pattern length                */
#define FNAMSZ  16              /* Max filename length on screen        */
#define FWINSZ  20              /* Filename width in window             */
#define USERCMD 10              /* # of user defined commands           */
#define MININD  3               /* Minimal tree indention               */
#define MAXIND  9               /* Maximal tree indention               */
#define MINFIL  1               /* Minimal number of file lines         */
#define MAXFIL  9               /* Maximal number of file lines         */
#define DEFFIL  3               /* Default number of file lines         */
#define VMODE0  0               /* Minimal video mode: no attributes    */
#define VMODE1  1               /* Middle video mode: reverse+underline */
#define VMODE2  2               /* Maximal video mode: all attributes   */
#define HLEVEL  9999            /* Huge number for tree level           */
#define EXECLEN (2*NAMELEN)     /* Command buffer length                */
#define HLMIN   6               /* Minimal size of history list         */
#define HLMAX   99              /* Maximal size of history list         */
#define HLDEF   22              /* Default size of history list         */

/* Return values for many functions                                     */
#define RV_OK   IN_OK           /* Work is done         (See keys.h)    */
#define RV_NUL  IN_NUL          /* No input             (See keys.h)    */
#define RV_INT  IN_INT          /* Break                (See keys.h)    */
#define RV_END  IN_EOF          /* EOF                  (See keys.h)    */
#define RV_HLP  IN_HLP          /* Help requested       (See keys.h)    */
#define RV_NXT  IN_NXT          /* Next requested       (See keys.h)    */
#define RV_PRV  IN_PRV          /* Prev requested       (See keys.h)    */
#define RV_SIZ  IN_SIZ          /* Screen size changed  (See keys.h)    */
#define RV_DIR  -11             /* Change directory                     */
#define RV_RET  -12             /* Return from file menu                */
#define RV_ERR  -13             /* Memory error                         */

/*
 *      Generic list type for scrolling in lists.
 *      Is a component in many other list types.
 */
typedef struct _glist {
  char *string;                 /*  General used string                 */
  struct _glist *prev;          /*  Previous element in list            */
  struct _glist *next;          /*  Next element in list                */
} glist;

#define GNULL   ((glist *) 0)   /* The glist NIL pointer                */

/*
 *      Access to items of glist record is done with macros
 *      to hide this record and for abbreviation.
 */

#define GNAME(p)        ((p)->string)
#define GPREV(p)        ((p)->prev)
#define GNEXT(p)        ((p)->next)

#include "list.h"               /* Directory/file list definitions      */
#include "hist.h"               /* History list definitons              */
#include "vars.h"               /* Variable/command definitions         */
#include "help.h"               /* Help page definitions                */
#include "term.h"               /* Terminal/screen definitions          */
#include "keys.h"               /* Key definitions                      */

/*
 *      Screen update flags.
 */

#define SF_TREE  0x0001         /* Update tree/file window              */
#define SF_LIST  0x0002         /* Update current list entry            */
#define SF_LAST  0x0004         /* Update last list entry               */
#define SF_SEPL  0x0008         /* Update separator line                */
#define SF_HELP  0x0010         /* Update help line                     */
#define SF_ECHO  0x0020         /* Update echo line                     */
#define SF_MOVE  0x0040         /* Update cursor position               */
#define SF_FILE  0x0080         /* Update file window on tree screen    */
#define SF_PBAR  0x0100         /* Update position bar                  */
#define SF_FULL  0xffff         /* Full update                          */

/*
 *      Display attribute flags.
 */
#define DA_NONE         0       /* No attributes                        */
#define DA_NORMAL       0       /* Reset to normal state                */
#define DA_REVERSE      1       /* Reverse                              */
#define DA_BOLD         2       /* Highlight                            */
#define DA_HALF         3       /* Hide                                 */
#define DA_ERROR        4       /* Error                                */
#define DA_MARK         5       /* Special mark in menu line            */
#define DA_BOLDREV      6       /* Reverse highlight                    */
#define DA_HALFREV      7       /* Reverse hide                         */
#define DA_BLINKREV     8       /* Reverse blink                        */

/*
 *      Some other useful definitions and macros.
 */

#define GLOBL
#define LOCAL   static
#define EXTRN   extern
#ifndef SIGNL
# define SIGNL  int
#endif  /* SIGNL */
#ifdef  lint                    /* Don't disturb lint !                 */
# define VOID   int
#else   /* !lint */
# define VOID   void
#endif  /* lint */

#define EQU(s1, s2)     (strcmp(s1, s2) == 0)
#define CMP(s1, s2)     (strcmp(s1, s2))

/* Get type of file pointed to by struct stat s                         */
#define STFMT(s)        ((s)->st_mode & S_IFMT)

/* Check if file f is a directory                                       */
#define ISDIR(f, s)     ((*statfun)(f, &s) == 0 && STFMT(&s) == S_IFDIR)

/* Check if file f is a block special file                              */
#define ISBLK(f, s)     ((*statfun)(f, &s) == 0 && STFMT(&s) == S_IFBLK)

/* Line is not empty and not a comment                                  */
#define VALID(c)        ( !(c=='\0' || c=='\n' || c=='#' || c==' ' || c=='\t'))

/* Can we change to directory p?                                        */
#define CANCD(p)        (access(p, 01) == 0)

/* Can we scroll a window and are we allowed to do this?                */
#ifdef  USEANSICOLORS
# define CANSCROLL      (scrollcap && VARSET(V_TS) && (!colorcap || !usecolors))
#else   /* !USEANSICOLORS */
# define CANSCROLL      (scrollcap && VARSET(V_TS))
#endif  /* USEANSICOLORS */

/*
 *      External system variables.
 */

EXTRN int errno;                /* System error code number             */

/*
 *      Global variables declared in main.c.
 */

#ifndef _MAIN_
EXTRN char *prgname, *home;
EXTRN int helpline, echoline;
EXTRN int firstdline, lastdline, ndlines;
EXTRN int firstfline, lastfline, nflines, maxnflines;
EXTRN int firstline, lastline;
EXTRN int indent, maxindent;
EXTRN int videomode, videoattr, graphattr;
EXTRN int maxlevel;
EXTRN int filecount, dircount;
EXTRN int treeflag, fileflag;
EXTRN int buildflag, writeflag;
EXTRN int sortbytime;
EXTRN int hiddendirs;
EXTRN int sizechange;
#ifdef  USEANSICOLORS
EXTRN int usecolors;
#endif
EXTRN dlist *droot, *cdlist, *cwlist;
EXTRN int fperpage, fperline;
EXTRN char rootdir[];
EXTRN char fpattern[], gpattern[], tpattern[];
EXTRN char utreemenu[], ufilemenu[];
EXTRN int (*statfun)();
# ifdef BSD
EXTRN jmp_buf winchjump;
EXTRN int atread;
# endif /* BSD */
EXTRN char *who, *hitkey;
#endif  /* !_MAIN_ */

/*
 *      Declaration of library and internal utree functions.
 */

#ifdef  S_IFLNK
EXTRN int stat(), lstat();
#else   /* ! S_IFLNK */
# define lstat  stat
EXTRN int stat();
#endif  /* S_IFLNK */

#ifdef  BSD
# define getcwd(p, n)    getwd(p)
# define strchr(s, c)    index(s, c)
# define strrchr(s, c)   rindex(s, c)
# define memset(s, c, n) bzero(s, n)
EXTRN char *index(), *rindex();
EXTRN DIR *opendir();
EXTRN struct direct *readdir();
#else   /* SYSV */
# ifdef NODIRENT
#  define DIR           FILE
#  define opendir(n)    fopen(n, "r")
#  define closedir(p)   fclose(p)
#  define rewinddir(p)  rewind(p)
# else  /* !NODIRENT */
EXTRN DIR *opendir();
EXTRN struct dirent *readdir();
# endif /* NODIRENT */
EXTRN char *strchr(), *strrchr();
#endif  /* BSD */
EXTRN char *strcpy(), *getenv(), *fgets(), *ctime();
EXTRN time_t time();

EXTRN dlist *newdlist();
EXTRN char *ualloc(), *basename(), *pathname(), *fileaccess(), *strsav();
