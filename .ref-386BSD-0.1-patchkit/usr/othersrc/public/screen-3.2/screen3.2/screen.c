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
 */

#ifndef lint
  static char rcs_id[] = "$Id: screen.c,v 1.2 92/02/03 02:28:05 jnweiger Exp $ FAU";
#endif


#include <sys/param.h>
/* #include <signal.h> */
#include <ctype.h>
#include <pwd.h>
#include <fcntl.h>
#ifdef sgi
# include <sys/sysmacros.h>
#endif /* sgi */
#if !defined(sun) && !defined(B43) && !defined(ISC)
# include <time.h>
#endif
/*
 * Gee!! We should reverse that #if! 
 */
#if defined(sun) || defined(_AIX) || defined(sysV68) || defined(MIPS) || defined(GOULD_NP1) || defined(B43) || defined(ISC) || defined(apollo) || defined(BSDI) || defined(sgi)
# include <sys/time.h>
#endif
#if defined(M_XENIX) || defined(M_UNIX)
#include <sys/select.h> /* for timeval */
#endif
#include <sys/types.h>
#ifdef ISC
# include <sys/bsdtypes.h>
#endif
#if !defined(sysV68) && !defined(M_XENIX)
# include <sys/wait.h>
#endif
#include <sys/stat.h>
#ifndef sgi
# include <sys/file.h>
#endif /* sgi */
#ifndef sun
# include <sys/ioctl.h>
#endif /* sun */

#include <signal.h>

#include "config.h"

#ifdef SHADOWPW
# include <shadow.h>
#endif /* SHADOWPW */

#ifdef SVR4
# include <sys/stropts.h>
#endif

#ifdef SYSV
# include <sys/utsname.h>
#endif

#if defined(_SEQUENT_) 
/* for the FD.. stuff */
# include <sys/select.h>
#endif 

#if defined(sequent) || defined(SVR4)
# include <sys/resource.h>
#endif /* sequent || SVR4 */

#ifdef ISC
# include <sys/tty.h>
# include <sys/sioctl.h>
# include <sys/pty.h>
#endif

#include "screen.h"

#include "patchlevel.h"

#if defined(xelos) || defined(sysV68) || defined(M_XENIX)
 struct passwd *getpwuid __P((uid_t));
 struct passwd *getpwnam __P((char *));
#endif

#ifdef USEVARARGS
# if defined(__STDC__)
#  include <stdarg.h>
# else
#  include <varargs.h>
# endif
#endif

#ifdef DEBUG
FILE *dfp;
#endif


#ifdef COPY_PASTE
extern char *copybuffer;	/* def in mark.c jw. */
extern copylen;
#endif /* COPY_PASTE */

extern char *blank, *null, Term[], screenterm[], **environ, *Termcap;
int force_vt = 1, assume_LP = 0;
extern int in_ovl;
extern int ovl_blockfore;
extern void (*ovl_process)();
extern int help_page;
extern int screenwidth, screenheight;
extern char display_tty[];
extern int default_width, default_height;
extern int Z0width, Z1width;
extern int ISO2022;
extern int status, HS;
extern char *Z0, *WS, *LastMsg;
extern time_t TimeDisplayed;
int BellDisplayed;
int VBellWait, MsgWait, MsgMinWait;

/* tputs uses that: jw */
extern short ospeed;

extern int flow, default_flow, wrap, visual_bell, default_monitor;
extern int errno;
extern sys_nerr;
extern char *sys_errlist[];
extern char mark_key_tab[];

#if defined(TIOCSWINSZ) || defined(TIOCGWINSZ)
extern struct winsize glwz;
#endif

static char *MakeWinMsg __P((char *, int));
static void MakeNewEnv __P((void));
static int Attach __P((int));
static void Attacher __P((void));
static void SigHandler __P((void));
static SIGTYPE AttacherSigInt __P(SIGPROTOARG);
static SIGTYPE SigChld __P(SIGPROTOARG);
static SIGTYPE SigInt __P(SIGPROTOARG);
static SIGTYPE CoreDump __P((int));
static void DoWait __P((void));
static SIGTYPE Finit __P((int));
static void InitKeytab __P((void));
static void SetForeWindow __P((int));
static int NextWindow __P((void));
static int PreviousWindow __P((void));
static int MoreWindows __P((void));
static void FreeWindow __P((struct win *));
static void execvpe __P((char *, char **, char **));
static void LogToggle __P((void));
static void ShowWindows __P((void));
static void ShowTime __P((void));
static void ShowInfo __P((void));
static int OpenPTY __P((void));
#ifdef PASSWORD
static void trysend __P((int, struct msg *, char *));
#endif
#if defined(SIGWINCH) && defined(TIOCGWINSZ)
static SIGTYPE SigAttWinch __P(SIGPROTOARG);
#endif
static void fgtty __P((void));
static void freetty __P((void));
static void brktty __P((void));

#if defined(LOCK)
static SIGTYPE DoLock __P(SIGPROTOARG);
static void LockTerminal __P((void));
#endif

#ifdef COPY_PASTE
static pastelen;
static char *pastebuffer;
#endif
#ifdef PASSWORD
extern char Password[];
#endif

static struct passwd *ppp;

/* used for opening a new pty-pair: */
static char PtyName[32], TtyName[32];

/* used for the attacher's tty: */
static char *attach_tty;

char *ShellProg;
char *ShellArgs[2];
static char inbuf[MAXWIN][IOSIZE];
static inlen[MAXWIN];
static inbuf_ct;
static ESCseen;
static GotSignal;

static char DefaultShell[] = "/bin/sh";
static char DefaultPath[] = ":/usr/ucb:/bin:/usr/bin";

#ifdef hpux
char PtyProto[] = "/dev/ptym/ptyXY";
char TtyProto[] = "/dev/pty/ttyXY";
#else
# if !(defined(sequent) || defined(_SEQUENT_) || defined(SVR4))
static char PtyProto[] = "/dev/ptyXY";
static char TtyProto[] = "/dev/ttyXY";
# endif
#endif /* hpux */
int TtyMode = 0622;
#ifdef SOCKDIR
char *SockDir = SOCKDIR;
#else
char *SockDir = ".iscreen";
#endif
extern char SockPath[], *SockNamePtr, *SockName;
int ServerSocket = -1;
static char **NewEnv;

char *RcFileName = NULL;
char Esc = Ctrl('a');
char MetaEsc = 'a';
char *home;

int HasWindow;
char *LoginName;
char *BellString;
char *VisualBellString;
char *ActivityString;
char *BufferFile;
char *PowDetachString;
int auto_detach = 1;
int iflag, rflag, dflag, lsflag, quietflag, wipeflag;
int adaptflag, loginflag = -1, allflag;
static intrc, startc, stopc;
char HostName[MAXSTR];
int Detached, Suspended;
int DeadlyMsg = 1;
int AttacherPid;	/* Non-Zero in child if we have an attacher */
int MasterPid;
int real_uid, real_gid, eff_uid, eff_gid;
int default_histheight;
int default_startup;
int slowpaste;

#if defined(BSDJOBS) && !(defined(POSIX) || defined(SYSV))
int DevTty = -1;
#endif

#ifdef NETHACK
int nethackflag = 0;
#endif

struct mode OldMode, NewMode;

struct win *fore = NULL;
int WinList = -1;
int ForeNum;
struct win *wtab[MAXWIN];

struct key ktab[256];

#ifndef FD_SET
typedef struct fd_set
{
  int fd_bits[1];
}      fd_set;
# define FD_ZERO(fd) ((fd)->fd_bits[0] = 0)
# define FD_SET(b, fd) ((fd)->fd_bits[0] |= 1 << (b))
# define FD_ISSET(b, fd) ((fd)->fd_bits[0] & 1 << (b))
# define FD_SETSIZE 32
#endif


#ifndef WTERMSIG
# ifndef BSDWAIT /* if wait is NOT a union: */
#  define WTERMSIG(status) (status & 0177)
# else
#  define WTERMSIG(status) status.w_T.w_Termsig 
# endif
#endif

#ifndef WIFCORESIG
# ifndef BSDWAIT /* if wait is NOT a union: */
#  define WIFCORESIG(status) (status & 0200)
# else
#  define WIFCORESIG(status) status.w_T.w_Coredump
# endif
#endif

#ifndef WEXITSTATUS
# ifndef BSDWAIT /* if wait is NOT a union: */
#  define WEXITSTATUS(status) ((status >> 8) & 0377)
# else
#  define WEXITSTATUS(status) status.w_T.w_Retcode
# endif
#endif

char *shellaka = NULL;

/*
 * Do this last
 */
#include "extern.h"

/*
 * XXX: Missing system header files.
 */
#ifdef USEVARARGS
# ifndef VPRNT_DECLARED
int vsprintf __P((char *, char *, va_list));
# endif /* VPRNT_DECLARED */
#endif
int select __P((int, fd_set *, fd_set *, fd_set *, struct timeval *));

static void
brktty()
{
#ifdef POSIX
  setsid();		/* will break terminal affiliation */
# ifdef BSD
  ioctl(0, TIOCSCTTY, 0);
# endif /* BSD */
#else
# ifdef SYSV
  setpgrp();		/* will break terminal affiliation */
# else
#  ifdef BSDJOBS
  if (DevTty)
    if (ioctl(DevTty, TIOCNOTTY, (char *) 0) != 0)
      debug2("brktty: ioctl(DevTty=%d, TIOCNOTTY, 0) = %d\n", DevTty, errno);
#  endif
# endif
#endif
}

static void
freetty()
{
  brktty();
#if defined(BSDJOBS) && !(defined(POSIX) || defined(SYSV))
  if (DevTty >= 0)
    {
      close(DevTty);
      DevTty = -1;
    }
#endif
  close(0);
  close(1);
  close(2);
  debug("did freetty\n");
}

static void
fgtty()
{
#ifdef BSDJOBS
  int mypid;

  mypid = getpid();

# ifdef BSDI
  setsid();
  ioctl(0, TIOCSCTTY, 0);
# endif /* BSDI */

# ifdef POSIX
  if (tcsetpgrp(0, mypid))
    {
      debug1("fgtty: tcsetpgrp: %d\n", errno);
      /* error is likely to have side-effects -- better to warn our user */
      SendErrorMsg("fgtty: Could not set process group id in tty");
    }
# else
  if (ioctl(0, TIOCSPGRP, &mypid) != 0)
    debug1("fgtty: TIOSETPGRP: %d\n", errno);
  /* posix setsid() in brktty() from freetty() already made us leader */
  if (setpgrp(0, mypid))
    debug1("fgtty: setpgrp: %d\n", errno);
# endif /* POSIX */
#endif /* BSDJOBS */
}

#ifdef hpux
/*
 * hpux has berkeley signal semantics if we use sigvector,
 * but not, if we use signal, so we define our own signal() routine.
 * (jw)
 */
void (*signal(sig, func)) ()
int sig;
void (*func) ();
{
  struct sigvec osv, sv;

  sv.sv_handler = func;
  sv.sv_mask = sigmask(sig);
  sv.sv_flags = SV_BSDSIG;
  if (sigvector(sig, &sv, &osv) < 0)
    return (BADSIG);
  return (osv.sv_handler);
}
#endif	/* hpux */

#ifndef USEBCOPY
void bcopy(s1, s2, len)
register char *s1, *s2;
register int len;
{
  if (s1 < s2 && s2 < s1 + len)
    {
      s1 += len;
      s2 += len;
      while (len-- > 0)
	*--s2 = *--s1;
    }
  else
    while (len-- > 0)
      *s2++ = *s1++;
}
#endif	/* USEBCOPY */

void bclear(p, n)
int n;
char *p;
{
  bcopy(blank, p, n);
}

static void
closeallfiles()
{
  int f;
#ifdef SVR4
  struct rlimit rl;
  
  if ((getrlimit(RLIMIT_NOFILE, &rl) == 0) && rl.rlim_max != RLIM_INFINITY)
    f = rl.rlim_max;
  else
#endif /* SVR4 */
#if defined(SYSV) && !defined(ISC)
  f = NOFILE;
#else /* SYSV && !ISC */
  f = getdtablesize();
#endif /* SYSV && !ISC */
  while (--f > 2)
    close(f);
}
  
static int InterruptPlease = 0;

void main(ac, av)
int ac;
char **av;
{
  register int n, len;
  register struct win *p;
  char *ap, *aka = NULL;
  char *av0;
  char socknamebuf[2 * MAXSTR];
  int s = 0;
  fd_set r, w, e;
  int mflag = 0;
  struct timeval tv;
  int nsel;
  char buf[IOSIZE], *bufp, *myname = (ac == 0) ? "screen" : av[0];
  struct stat st;
  int buflen, tmp;
#ifdef _MODE_T			/* (jw) */
  mode_t oumask;
#else
  int oumask;
#endif
#ifdef SYSV
  struct utsname utsnam;
#endif

  /*
   *  First, close all unused descriptors
   *  (otherwise, we might have problems with the select() call)
   */
  closeallfiles();
#ifdef DEBUG
  (void) mkdir("/tmp/debug", 0777);
  if ((dfp = fopen("/tmp/debug/screen.front", "w")) == NULL)
    dfp = stderr;
  else
    (void) chmod("/tmp/debug/screen.front", 0666);
#endif
  debug1("-- screen debug started %s\n", *av);
#ifdef POSIX
  debug("POSIX\n");
#endif
#ifdef TERMIO
  debug("TERMIO\n");
#endif
#ifdef SYSV
  debug("SYSV\n");
#endif
#ifdef NAMEDPIPE
  debug("NAMEDPIPE\n");
#endif
#if defined(SIGWINCH) && defined(TIOCGWINSZ)
  debug("Window changing enabled\n");
#endif
#ifdef NOREUID
  debug("NOREUID\n");
#endif
#ifdef hpux
  debug("hpux\n");
#endif
#ifdef USEBCOPY
  debug("USEBCOPY\n");
#endif
#ifdef UTMPOK
  debug("UTMPOK\n");
#endif
#ifdef LOADAV
  debug("LOADAV\n");
#endif
#ifdef NETHACK
  debug("NETHACK\n");
#endif
#ifdef TERMINFO
  debug("TERMINFO\n");
#endif
#ifdef NAME_MAX
  debug1("NAME_MAX = %d\n", NAME_MAX);
#endif

  BellString = SaveStr("Bell in window %");
  VisualBellString = SaveStr("   Wuff,  Wuff!!  ");
  ActivityString = SaveStr("Activity in window %");
  BufferFile = SaveStr("/tmp/screen-exchange");
  PowDetachString = 0;
  default_histheight = DEFAULTHISTHEIGHT;
  default_startup = (ac > 1) ? 0 : 1;
  adaptflag = 0;
  slowpaste = 0;
  VBellWait = VBELLWAIT;
  MsgWait = MSGWAIT;
  MsgMinWait = MSGMINWAIT;
  CompileKeys((char *)NULL, mark_key_tab);

  av0 = *av;
  while (ac > 0)
    {
      ap = *++av;
      if (--ac > 0 && *ap == '-')
	{
	  switch (ap[1])
	    {
	    case 'a':
	      allflag = 1;
	      break;
	    case 'A':
	      adaptflag = 1;
	      break;
	    case 'c':
	      if (ap[2])
		RcFileName = ap + 2;
	      else
		{
		  if (--ac == 0)
		    exit_with_usage(myname);
		  RcFileName = *++av;
		}
	      break;
	    case 'e':
	      if (ap[2])
		ap += 2;
	      else
		{
		  if (--ac == 0)
		    exit_with_usage(myname);
		  ap = *++av;
		}
	      if (!ParseEscape(ap))
		Msg(0, "Two characters are required with -e option.");
	      break;
	    case 'f':
	      switch (ap[2])
		{
		case 'n':
		case '0':
		  default_flow = FLOW_NOW * 0;
		  break;
		case 'y':
		case '1':
		case '\0':
		  default_flow = FLOW_NOW * 1;
		  break;
		case 'a':
		  default_flow = FLOW_AUTOFLAG;
		  break;
		default:
		  exit_with_usage(myname);
		}
	      break;
            case 'h':
	      if (ap[2])
		default_histheight = atoi(ap + 2);
	      else
		{
		  if (--ac == 0)
		    exit_with_usage(myname);
		  default_histheight = atoi(*++av);
		}
	      if (default_histheight < 0)
		default_histheight = 0;
	      break;
	    case 'i':
	      iflag = 1;
	      break;
	    case 't': /* title is a synonym for AkA */
	    case 'k':
	      if (ap[2])
		aka = ap + 2;
	      else
		{
		  if (--ac == 0)
		    exit_with_usage(myname);
		  aka = *++av;
		}
	      break;
	    case 'l':
	      switch (ap[2])
		{
		case 'n':
		case '0':
		  loginflag = 0;
		  break;
		case 'y':
		case '1':
		case '\0':
		  loginflag = 1;
		  break;
		case 's':
		case 'i':
		  lsflag = 1;
		  break;
		default:
		  exit_with_usage(myname);
		}
	      break;
	    case 'w':
	      lsflag = 1;
	      wipeflag = 1;
	      break;
	    case 'L':
	      assume_LP = 1;
	      break;
	    case 'm':
	      mflag = 1;
	      break;
	    case 'O':
	      force_vt = 0;
	      break;
	    case 'T':
              if (ap[2])
		{
		  if (strlen(ap+2) < 20)
                    strcpy(screenterm, ap + 2);
		}
              else
                {
                  if (--ac == 0)
                    exit_with_usage(myname);
		  if (strlen(*++av) < 20)
                    strcpy(screenterm, *av);
                }
              break;
	    case 'q':
	      quietflag = 1;
	      break;
	    case 'r':
	    case 'R':
	      if (ap[2])
		{
		  SockName = ap + 2;
		  if (ac != 1)
		    exit_with_usage(myname);
		}
	      else if (ac > 1 && *av[1] != '-')
		{
		  SockName = *++av;
		  ac--;
		}
	      rflag = (ap[1] == 'r') ? 1 : 2;
	      break;
#ifdef REMOTE_DETACH
	    case 'd':
	      dflag = 1;
	      /* FALLTHRU */
	    case 'D':
	      if (!dflag)
		dflag = 2;
	      if (ap[2])
		SockName = ap + 2;
	      if (ac == 2)
		{
		  if (*av[1] != '-')
		    {
		      SockName = *++av;
		      ac--;
		    }
		}
	      break;
#endif
	    case 's':
	      if (ap[2])
		ShellProg = ap + 2;
	      else
		{
		  if (--ac == 0)
		    exit_with_usage(myname);
		  ShellProg = *++av;
		}
	      break;
	    default:
	      exit_with_usage(myname);
	    }
	}
      else
	break;
    }
  real_uid = getuid();
  real_gid = getgid();
  eff_uid = geteuid();
  eff_gid = getegid();
  if (eff_uid != real_uid)
    {		
      /* if running with s-bit, we must install a special signal
       * handler routine that resets the s-bit, so that we get a
       * core file anyway.
       */
      signal(SIGBUS, CoreDump);
      signal(SIGSEGV, CoreDump);
    }
  if (!ShellProg && (ShellProg = getenv("SHELL")) == 0)
    ShellProg = DefaultShell;
  ShellArgs[0] = ShellProg;
#ifdef NETHACK
  nethackflag = (getenv("NETHACKOPTIONS") != NULL);
#endif
  home = getenv("HOME");	/* may or may not return a result. jw. */
  if ((LoginName = getlogin()) && LoginName[0] != '\0')
    {
      if ((ppp = getpwnam(LoginName)) != (struct passwd *) 0)
	if (ppp->pw_uid != real_uid)
	  ppp = (struct passwd *) 0;
    }
  if (ppp == 0)
    {
      if ((ppp = getpwuid(real_uid)) == 0)
        {
#ifdef NETHACK
          if (nethackflag)
	    Msg(0, "An alarm sounds through the dungeon...\nWarning, the kops are coming.");
	  else
#endif
	  Msg(0, "getpwuid() can't identify your account!");
	  exit(1);
        }
      LoginName = ppp->pw_name;
    }
  if (home == 0 || *home == '\0')
    home = ppp->pw_dir;
  if (strlen(LoginName) > 20)
    Msg(0, "LoginName too long - sorry.");
  if (strlen(home) > MAXPATH - 25)
    Msg(0, "$HOME too long - sorry.");
#ifdef PASSWORD
  strcpy(Password, ppp->pw_passwd);
#endif

  /* ttyname implies isatty */
  if (!(attach_tty = ttyname(0)))
    {
#ifdef NETHACK
      if (nethackflag)
	Msg(0, "You must play from a terminal.");
      else
#endif
      Msg(0, "Must be connected to a terminal.");
      exit(1);
    }
  if (strlen(attach_tty) >= MAXPATH)
    Msg(0, "TtyName too long - sorry.");
  if ((n = secopen(attach_tty, O_RDWR, 0)) < 0)
    Msg(0, "Cannot open '%s' - please check.", attach_tty);
  close(n);
    
  debug1("attach_tty is %s\n", attach_tty);
  
#ifdef _MODE_T
  oumask = umask(0);		/* well, unsigned never fails? jw. */
#else
  if ((oumask = umask(0)) == -1)
    Msg(errno, "Cannot change umask to zero");
#endif
  if ((SockDir = getenv("ISCREENDIR")) == NULL)
    SockDir = getenv("SCREENDIR");
  if (SockDir && strlen(SockDir) >= MAXPATH - 1)
    Msg(0, "ridiculous long $(I)SCREENDIR - try again.");
#ifndef SOCKDIR
  if (SockDir == 0)
    {
      sprintf(SockPath, "%s/.iscreen", home);
      SockDir = SockPath;
    }
#endif
  if (SockDir)
    {
      if (access(SockDir, F_OK))
	{
	  if (UserContext() > 0)
	    {
	      if (mkdir(SockDir, 0700))
		UserReturn(0);
	      UserReturn(1);
	    }
	  if (UserStatus() <= 0)
	    Msg(0, "Cannot make directory '%s'", SockDir);
	}
      if (SockDir != SockPath)
        strcpy(SockPath, SockDir);
    }
#ifdef SOCKDIR
  else
    {
      SockDir = SOCKDIR;
      if (stat(SockDir, &st))
	{
	  if (mkdir(SockDir, eff_uid ? 0777 : 0755) == -1)
	    Msg(errno, "Cannot make directory '%s'", SockDir);
	}
      else
	{
          n = eff_uid ? 0777 : 0755;
	  if ((st.st_mode & 0777) != n)
	    Msg(0, "Directory '%s' must have mode %03o.", SockDir, n);
	}
      sprintf(SockPath, "%s/S-%s", SockDir, LoginName);
      if (access(SockPath, F_OK))
	{
	  if (mkdir(SockPath, 0700) == -1)
	    Msg(errno, "Cannot make directory '%s'", SockPath);
	  (void) chown(SockPath, real_uid, real_gid);
	}
    }
#endif
  if (stat(SockPath, &st) == -1)
    {
      Msg(errno, "Cannot access %s", SockPath);
    }
  else
    {
#ifdef _POSIX_SOURCE
      if (S_ISDIR(st.st_mode) == 0)
#else
      if ((st.st_mode & S_IFMT) != S_IFDIR)
#endif
	Msg(0, "%s is not a directory.", SockPath);
      if (st.st_uid != real_uid)
	Msg(0, "You are not the owner of %s.", SockPath);
      if ((st.st_mode & 0777) != 0700)
	Msg(0, "Directory %s must have mode 700.", SockPath);
    }
  strcat(SockPath, "/");
  SockNamePtr = SockPath + strlen(SockPath);
  (void) umask(oumask);
#if defined(SYSV) && !defined(ISC)
  if (uname(&utsnam) == -1)
    Msg(0, "uname() failed, errno = %d", errno);
  else
    {
      strncpy(HostName, utsnam.nodename, MAXSTR);
      HostName[(sizeof(utsnam.nodename) <= MAXSTR) ? 
               sizeof(utsnam.nodename) : MAXSTR] = '\0';
    }
#else
  (void) gethostname(HostName, MAXSTR);
#endif
  HostName[MAXSTR - 1] = '\0';
  if ((ap = index(HostName, '.')) != NULL)
    *ap = '\0';
  GetTTY(0, &OldMode);
#ifdef POSIX
  ospeed = (short) cfgetospeed(&OldMode.tio);
#else
# ifndef TERMIO
  ospeed = (short) OldMode.m_ttyb.sg_ospeed;
# endif
#endif
  debug1("...setting extern short ospeed = %d\n", ospeed);

  if (lsflag)
    {
      int i;
      i = FindSocket(0, (int *)NULL);
      /* MakeClientSocket appended the last (Sock)Name there: */
      *SockNamePtr = '\0';
      if (i == 0)
	{
#ifdef NETHACK
          if (nethackflag)
	    Msg(0, "This room is empty (%s)\n", SockPath);
          else
#endif /* NETHACK */
          Msg(0, "No Sockets found in %s\n", SockPath);
        }
      else
        Msg(0, "%d Socket%s in %s.\n", i, i > 1 ? "s" : "", SockPath);
        /* NOTREACHED */
    }
  if (rflag)
    {
      debug("screen -r: - is there anybody out there?\n");
#ifdef SHADOWPW
      setspent();  /* open shadow file while we are still root */
#endif /* SHADOWPW */
      if (Attach(MSG_ATTACH))
	{
	  Attacher();
	  /* NOTREACHED */
	}
      debug("screen -r: backend not responding -- still crying\n");
    }
  else if (dflag)
    {
      (void) Attach(MSG_DETACH);
      DeadlyMsg = 0;
      Msg(0, "[%s %sdetached.]\n", SockName, (dflag > 1 ? "power " : ""));
      eexit(0);
      /* NOTREACHED */
    }
  if (!mflag && (SockName = getenv("STY")) != 0 && *SockName != '\0')
    {
      setuid(real_uid);
      setgid(real_gid);
      s = MakeClientSocket(1, SockName);
      if (ac == 0)
	{
	  ac = 1;
	  av = ShellArgs;
	}
      av[ac] = aka;
      SendCreateMsg(s, ac, av, allflag, default_flow, loginflag, default_histheight,
		    screenterm);
      close(s);
      exit(0);
    }
#if defined(BSDJOBS) && !(defined(POSIX) || defined(SYSV))
  if ((DevTty = open("/dev/tty", O_RDWR | O_NDELAY)) == -1)
    Msg(errno, "/dev/tty");
#endif
  switch (MasterPid = fork())
    {
    case -1:
      Msg(errno, "fork");
      /* NOTREACHED */
    case 0:
      break;
    default:
      sprintf(socknamebuf, "%d.%s.%s", MasterPid, stripdev(attach_tty),
	      HostName);
      for (ap = socknamebuf; *ap; ap++)
	if (*ap == '/')
	  *ap = '-';
      SockName = socknamebuf;
#ifdef SHADOWPW
      setspent();  /* open shadow file while we are still root */
#endif /* SHADOWPW */
      Attacher();
      /* NOTREACHED */
    }
#ifdef DEBUG
  if (dfp != stderr)
    fclose(dfp);
  if ((dfp = fopen("/tmp/debug/screen.back", "w")) == NULL)
    dfp = stderr;
  else
    (void) chmod("/tmp/debug/screen.back", 0666);
#endif
  debug("-- screen.back debug started\n");
  ap = av0 + strlen(av0) - 1;
  while (ap >= av0)
    {
      if (!strncmp("screen", ap, 6))
	{
	  strncpy(ap, "SCREEN", 6); /* name this process "SCREEN-BACKEND" */
	  break;
	}
      ap--;
    }
  if (ap < av0)
    *av0 = 'S';

  AttacherPid = getppid();
  sprintf(socknamebuf, "%d.%s.%s", getpid(), stripdev(attach_tty), HostName);
  for (ap = socknamebuf; *ap; ap++)
    if (*ap == '/')
      *ap = '-';
  SockName = socknamebuf;
  ServerSocket = s = MakeServerSocket();
#ifdef ETCSCREENRC
  if ((ap = getenv("SYSSCREENRC")) == NULL)
    StartRc(ETCSCREENRC);
  else
    StartRc(ap);
#endif
  StartRc(RcFileName);
  InitTermcap();
  InitTerm(0);
  MakeNewEnv();
  strcpy(display_tty, attach_tty);
#ifdef UTMPOK
# ifdef apollo
  ReInitUtmp();
# else
  InitUtmp();
# endif /* apollo */
#endif
#ifdef LOADAV
# ifdef NeXT
  InitNeXTLoadAvg(); /* NeXT load average */
# else
  InitKmem();
# endif /* !NeXT */
#endif /* LOADAV */
  signal(SIGHUP, SigHup);
  signal(SIGINT, Finit);
  signal(SIGQUIT, Finit);
  signal(SIGTERM, Finit);
#ifdef BSDJOBS
  signal(SIGTTIN, SIG_IGN);
  signal(SIGTTOU, SIG_IGN);
#endif
  InitKeytab();
#ifdef ETCSCREENRC
  if ((ap = getenv("SYSSCREENRC")) == NULL)
    FinishRc(ETCSCREENRC);
  else
    FinishRc(ap);
#endif
  FinishRc(RcFileName);

  /* Note: SetMode must be called _after_ FinishRc (flow is set there).
   */
  SetMode(&OldMode, &NewMode);
  SetTTY(0, &NewMode);
  if (loginflag == -1)
      loginflag = LOGINDEFAULT;
  if (ac == 0)
    {
      ac = 1;
      av = ShellArgs;
      if (!aka)
	aka = shellaka;
    }
  if (!HasWindow)
    {
      debug("We open one default window, as screenrc did not specify one.\n");
      if (MakeWindow(aka, av, allflag, default_flow, 0, (char *)0, loginflag, -1, (char *)0) == -1)
	{
	  Finit(1);
	  /* NOTREACHED */
	}
    }
  if (default_startup)
    display_copyright();
#ifdef SYSV
  signal(SIGCLD, SigChld);
#else
  signal(SIGCHLD, SigChld);
#endif
  signal(SIGINT, SigInt);
  tv.tv_usec = 0;
  if (rflag == 2)
    {
#ifdef NETHACK
      if (nethackflag)
        Msg(0, "I can't seem to find a... Hey, wait a minute!  Here comes a screen now.");
      else
#endif
      Msg(0, "New screen...");
      rflag = 0;
    }
  brktty();
  for (;;)
    {
      /*
       * check to see if message line should be removed
       */
      if (status)
	{
	  int time_left;

	  debug("checking status...\n");
	  time_left = TimeDisplayed + (BellDisplayed ? VBellWait : MsgWait) - time((time_t *)0);
	  if (time_left > 0)
	    {
	      tv.tv_sec = time_left;
	      debug(" not yet.\n");
	    }
	  else
	    {
	      debug(" removing now.\n");
	      RemoveStatus();
	    }
	}
      /*
       * check for I/O on all available I/O descriptors
       */
      FD_ZERO(&r);
      FD_ZERO(&w);
      FD_ZERO(&e);
      if (inbuf_ct > 0)
	for (n = 0; n < MAXWIN; n++)
#ifdef COPY_PASTE		/* wrong here? jw. */
	  if (inlen[n] > 0 || (pastelen > 0 && n == ForeNum))
#else
	  if (inlen[n] > 0)
#endif
	    FD_SET(wtab[n]->ptyfd, &w);
      if (!Detached)
	FD_SET(0, &r);
      for (n = WinList; n != -1; n = p->WinLink)
	{
	  p = wtab[n];
	  if (p->active && status && !BellDisplayed && !HS)
	    continue;
	  if (p->outlen > 0)
	    continue;
	  if (in_ovl && ovl_blockfore && n == ForeNum)
	    continue;
	  FD_SET(p->ptyfd, &r);
	}
      FD_SET(s, &r);
      (void) fflush(stdout);
      if (GotSignal && !status)
	{
	  SigHandler();
	  continue;
	}
      if ((nsel = select(FD_SETSIZE, &r, &w, &e, (status) ? &tv : (struct timeval *) 0)) < 0)
	{
	  debug1("Bad select - errno %d\n", errno);
	  if (errno != EINTR)
	    {
	      perror("select");
	      Finit(1);
	    }
	  else
	    {
	      errno = 0;
	      if ((!GotSignal || status) && !InterruptPlease)
	        continue;
	    }
	}
      if (InterruptPlease)
	{
	  char buf[1];

	  debug("Backend received interrupt\n");
	  *buf = intrc;
	  write(wtab[ForeNum]->ptyfd, buf, 1);
	  debug1("Backend wrote interrupt to %d\n", ForeNum);
	  InterruptPlease = 0;

	  continue;
	}
      if (GotSignal && !status)
	{
	  SigHandler();
	  continue;
	}
      /* Process a client connect attempt and message */
      if (nsel && FD_ISSET(s, &r))
	{
          nsel--;
	  if (!HS)
	    RemoveStatus();
	  if (in_ovl)
	    {
	      SetOvlCurr();
	      (*ovl_process)(0, 0); /* We have to abort first!! */
	      CheckScreenSize(1); /* Change fore */
	      DeadlyMsg = 0;
#ifdef NETHACK
              if (nethackflag)
	        Msg(0, "KAABLAMM!!!  You triggered a land mine!");
              else
#endif
	      Msg(0, "Aborted because of window change or message.");
	    }
	  else
	    CheckScreenSize(1); /* Change fore */
	  ReceiveMsg(s);
	  continue;
	}
      /*
       * Write the stored user input to the window descriptors first.
       * We do not want to choke, if he types fast.
       */
      if (nsel && inbuf_ct > 0)
	{
	  for (n = 0; n < MAXWIN ; n++)
	    {
	      if (inlen[n] <= 0)
		continue;
	      tmp = wtab[n]->ptyfd;
              if (FD_ISSET(tmp, &w))
                {
		  if ((len = write(tmp, inbuf[n], inlen[n])) > 0)
		    {
		      if ((inlen[n] -= len) == 0)
		      inbuf_ct--;
		      bcopy(inbuf[n] + len, inbuf[n], inlen[n]);
		    }
		  if (--nsel == 0)
		    break;
		}
	    }
	}
      /* Read, process, and store the user input */
      if (nsel && FD_ISSET(0, &r))
	{
          nsel--;
	  if (!HS)
	    RemoveStatus();
	  if (ESCseen)
	    {
	      buf[0] = Esc;
	      buflen = read(0, buf + 1, IOSIZE - 1) + 1;
	      ESCseen = 0;
	    }
	  else
	    buflen = read(0, buf, IOSIZE);
	  if (buflen < 0)
	    {
	      debug1("Read error: %d - SigHup()ing!\n", errno);
	      SigHup(SIGARG);
	      continue;
	    }
	  if (buflen == 0)
	    {
	      debug("Found EOF - SigHup()ing!\n");
	      SigHup(SIGARG);
	      continue;
	    }
	  bufp = buf;
          if (in_ovl)
	    {
	      SetOvlCurr();
	      (*ovl_process)(&bufp, &buflen);
	    }
	  while (buflen > 0)
	    {
	      n = ForeNum;
	      len = inlen[n];
	      bufp = ProcessInput(bufp, &buflen, inbuf[n], &inlen[n],
				  sizeof *inbuf);
	      if (inlen[n] > 0 && len == 0)
		inbuf_ct++;
	    }
	  if (inbuf_ct > 0)
	    continue;
	}
      if (GotSignal && !status)
	{
	  SigHandler();
	  continue;
	}
#ifdef COPY_PASTE
      /* Write the copybuffer contents first, if any. jw. */
      if (pastelen > 0)
	{
	  n = ForeNum;
	  debug1("writing pastebuffer (%d)\n", pastelen);
	  tmp = wtab[n]->ptyfd;
	  if (			/* FD_ISSET(tmp, &w) && */
	      (len = write(tmp, pastebuffer,
			   pastelen > IOSIZE ? IOSIZE : pastelen)) > 0)
	    {
	      pastebuffer += len;
	      pastelen -= len;
	      debug1("%d bytes pasted\n", len);
	      if (slowpaste > 0)
		{
		  struct timeval t;

                  debug1("slowpaste %d\n", slowpaste);
		  t.tv_usec = (long) (slowpaste * 1000);
		  t.tv_sec = 0;
		  select(0, (fd_set *)0, (fd_set *)0, (fd_set *)0, &t);
		}
	      else
	        continue;
	    }
	  /* 
	   * We could not paste? Let's see if the pty did echo the lot.
	   * Then continue by processing some pty output.
	   */
	}
#endif
      if (GotSignal && !status)
	{
	  SigHandler();
	  continue;
	}
      /* Read and process the output from the window descriptors */
      for (n = WinList; n != -1; n = p->WinLink)
	{
	  p = wtab[n];
	  if (in_ovl && ovl_blockfore && n == ForeNum)
	    continue;
	  if (p->outlen)
	    WriteString(p, p->outbuf, p->outlen);
	  else if (nsel && FD_ISSET(p->ptyfd, &r))
	    {
	      nsel--;
	      if ((len = read(p->ptyfd, buf, IOSIZE)) == -1)
		{
#ifdef EWOULDBLOCK
		  if (errno == EWOULDBLOCK)
		    len = 0;
#endif
		}
#if defined(TIOCPKT) && !defined(sgi)
	      if (buf[0])
		{
		  debug1("PAKET %x\n", buf[0]);
		  if (buf[0] & TIOCPKT_NOSTOP)
		    {
		      NewAutoFlow(p, 0);
		    }
		  if (buf[0] & TIOCPKT_DOSTOP)
		    {
		      NewAutoFlow(p, 1);
		    }
		}
	      if (len > 1)
		WriteString(p, buf + 1, len - 1);
#else /* TIOCPKT && !sgi */
	      if (len > 0)
		WriteString(p, buf, len);
#endif /* TIOCPKT && !sgi */
	    }
	  if (p->bell == BELL_ON)
	    {
	      p->bell = BELL_DONE;
	      Msg(0, MakeWinMsg(BellString, n));
	      if (p->monitor == MON_FOUND)
		p->monitor = MON_DONE;
	    }
	  else if (p->bell == BELL_VISUAL)
	    {
	      if (!BellDisplayed)
		{
		  p->bell = BELL_DONE;
		  Msg(0, VisualBellString);
		  BellDisplayed = 1;
		}
	    }
	  else if (p->monitor == MON_FOUND)
	    {
	      p->monitor = MON_DONE;
	      Msg(0, MakeWinMsg(ActivityString, n));
	    }
	}
      if (GotSignal && !status)
	SigHandler();
#ifdef DEBUG
      if (nsel)
	debug1("Left over nsel: %d\n", nsel);
#endif
    }
  /* NOTREACHED */
}

static void SigHandler()
{
  struct stat st;
  while (GotSignal)
    {
      GotSignal = 0;
      DoWait();
#ifdef SYSV
      signal(SIGCLD, SigChld);
#endif
    }
  if (stat(SockPath, &st) == -1)
    {
      debug1("SigHandler: Yuck! cannot stat '%s'\n", SockPath);
      if (!RecoverSocket())
	{
	  debug("SCREEN cannot recover from corrupt Socket, bye\n");
	  Finit(1);
	}
      else
	debug1("'%s' reconstructed\n", SockPath);
    }
  else
    debug2("SigHandler: stat '%s' o.k. (%03o)\n", SockPath, st.st_mode);
}

#ifdef DEBUG
int FEpanic;

SIGTYPE FEChld(SIGDEFARG)
{
  FEpanic=1;
#ifndef SIGVOID
  return((SIGTYPE) 0);
#endif
}
#endif

static SIGTYPE SigChld(SIGDEFARG)
{
  debug("SigChld()\n");
  GotSignal = 1;
#ifndef SIGVOID
  return((SIGTYPE) 0);
#endif
}

SIGTYPE SigHup(SIGDEFARG)
{
  debug("SigHup()\n");
  if (auto_detach)
    Detach(D_DETACH);
  else
    Finit(0);
#ifndef SIGVOID
  return((SIGTYPE) 0);
#endif
}

/*
 * the frontend's Interrupt handler
 * we forward SIGINT to the backend
 */
static SIGTYPE 
AttacherSigInt(SIGDEFARG)
{
  Kill(MasterPid, SIGINT);
  signal(SIGINT, AttacherSigInt);
# ifndef SIGVOID
  return (SIGTYPE) 0;
# endif
}


/* 
 * the backend's Interrupt handler
 * we cannot insert the intrc directly, as we never know
 * if fore and ForeNum are valid.
 */
static SIGTYPE SigInt(SIGDEFARG)
{
#if HAZARDOUS
  char buf[1];

  debug("SigInt()\n");
  *buf = (char) intrc;
  inlen[ForeNum] = 0;
  if (fore && !in_ovl)
    write(fore->ptyfd, buf, 1);
#else
  debug("SigInt() careful\n");
  InterruptPlease = 1;
  signal(SIGINT, SigInt);
#endif
#ifndef SIGVOID
  return((SIGTYPE) 0);
#endif
}

static SIGTYPE CoreDump(sig)
int sig;
{
  setgid(getgid());
  setuid(getuid());
  unlink("core");
  fprintf(stderr, "\r\n[screen caught signal %d.%s]\r\n", sig,
#ifdef SHADOWPW
          ""
#else /* SHADOWPW */
          " (core dumped)"
#endif /* SHADOWPW */
          );
  fflush(stderr);
  Kill(AttacherPid, SIG_BYE);
#ifdef SHADOWPW
  eexit(sig);
#else /* SHADOWPW */
  abort();
#endif /* SHADOWPW */
#ifndef SIGVOID
  return((SIGTYPE) 0);
#endif
}

static void DoWait()
{
  register int n, next, pid;
#ifdef BSDWAIT
  union wait wstat;
#else
  int wstat;
#endif

#ifdef BSDJOBS
# ifndef BSDWAIT
  while ((pid = waitpid(-1, &wstat, WNOHANG | WUNTRACED)) > 0)
# else
  while ((pid = wait3(&wstat, WNOHANG | WUNTRACED, (struct rusage *) 0)) > 0)
# endif
#else	/* BSDJOBS */
  while ((pid = wait(&wstat)) < 0)
    if (errno != EINTR)
      break;
  if (pid >= 0)
#endif	/* BSDJOBS */
    {
      for (n = WinList; n != -1; n = next)
	{
	  next = wtab[n]->WinLink;
	  if (pid == wtab[n]->wpid)
	    {
#ifdef BSDJOBS
	      if (WIFSTOPPED(wstat))
		{
# ifdef NETHACK	
                  if (nethackflag)
		    Msg(0, "You regain consciousness.");
		  else
# endif /* NETHACK */
		  Msg(0, "Child has been stopped, restarting.");
		  debug1("WIFSTOPPED: %d SIGCONT\n", wtab[n]->wpid);
		  if (killpg(wtab[n]->wpid, SIGCONT))
		    kill(wtab[n]->wpid, SIGCONT);
		}
	      else
#endif
		KillWindow(n);
	    }
	}
    }
}

void KillWindow(n)
int n;
{
  register int i;
  /*
   * Remove window from linked list.
   */
  if (n == WinList)	/* WinList = ForeNum */
    {
      RemoveStatus();
      WinList = fore->WinLink;
      fore = 0;
    }
  else
    {
      i = WinList;
      while (wtab[i]->WinLink != n)
	i = wtab[i]->WinLink;
      wtab[i]->WinLink = wtab[n]->WinLink;
    }
  FreeWindow(wtab[n]);
  wtab[n] = 0;
  if (inlen[n] > 0)
    {
      inlen[n] = 0;
      inbuf_ct--;
    }
  /*
   * If the foreground window disappeared check the head of the linked list
   * of windows for the most recently used window. If no window is alive at
   * all, exit.
   */
  if (WinList == -1)
    Finit(0);
  if (!fore)
    SwitchWindow(WinList);
}

static SIGTYPE Finit(i)
int i;
{
  register int n, next;

#ifdef SYSV
  signal(SIGCLD, SIG_IGN);
#else
  signal(SIGCHLD, SIG_IGN);
#endif
  signal(SIGHUP, SIG_IGN);
  debug1("Finit(%d);\n", i);
  for (n = WinList; n != -1; n = next)
    {
      next = wtab[n]->WinLink;
      FreeWindow(wtab[n]);
    }
  FinitTerm();
  SetTTY(0, &OldMode);
#ifdef UTMPOK
  RestoreLoginSlot();
#endif
  printf("\n[screen is terminating]\n");
  freetty();
  if (ServerSocket != -1)
    {
      debug1("we unlink(%s)\n", SockPath);
      (void) unlink(SockPath);
    }
  Kill(AttacherPid, SIG_BYE);
  exit(i);
#ifndef SIGVOID
  return((SIGTYPE) 0);
#endif
}

void
eexit(e)
int e;
{
  if (ServerSocket != -1)
    {
      debug1("we unlink(%s)\n", SockPath);
      (void) unlink(SockPath);
    }
  exit(e);
}

static void InitKeytab()
{
  register unsigned int i;

  for (i = 0; i < sizeof(ktab)/sizeof(*ktab); i++)
    ktab[i].type = KEY_IGNORE;

  ktab['h'].type = ktab[Ctrl('h')].type = KEY_HARDCOPY;
#ifdef BSDJOBS
  ktab['z'].type = ktab[Ctrl('z')].type = KEY_SUSPEND;
#endif
  ktab['c'].type = ktab[Ctrl('c')].type = KEY_SHELL;
  ktab[' '].type = ktab[Ctrl(' ')].type =
    ktab['n'].type = ktab[Ctrl('n')].type = KEY_NEXT;
  ktab['-'].type = ktab['p'].type = ktab[Ctrl('p')].type = KEY_PREV;
  ktab['k'].type = ktab[Ctrl('k')].type = KEY_KILL;
  ktab['l'].type = ktab[Ctrl('l')].type = KEY_REDISPLAY;
  ktab['w'].type = ktab[Ctrl('w')].type = KEY_WINDOWS;
  ktab['v'].type = ktab[Ctrl('v')].type = KEY_VERSION;
  ktab['q'].type = ktab[Ctrl('q')].type = KEY_XON;
  ktab['s'].type = ktab[Ctrl('s')].type = KEY_XOFF;
  ktab['t'].type = ktab[Ctrl('t')].type = KEY_TIME;
  ktab['i'].type = ktab[Ctrl('i')].type = KEY_INFO;
  ktab['m'].type = ktab[Ctrl('m')].type = KEY_LASTMSG;
  ktab['A'].type = KEY_AKA, ktab['A'].args = NULL;
  ktab['L'].type = KEY_LOGIN;
  ktab[','].type = KEY_LICENSE;
  ktab['W'].type = KEY_WIDTH;
  ktab['.'].type = KEY_TERMCAP;
  ktab[Ctrl('\\')].type = KEY_QUIT;
  ktab['d'].type = ktab[Ctrl('d')].type = KEY_DETACH;
  ktab['r'].type = ktab[Ctrl('r')].type = KEY_WRAP;
  ktab['f'].type = ktab[Ctrl('f')].type = KEY_FLOW;
  ktab['C'].type = KEY_CLEAR;
  ktab['Z'].type = KEY_RESET;
  ktab['H'].type = KEY_LOGTOGGLE;
  if (Esc != MetaEsc)
    ktab[Esc].type = KEY_OTHER;
  else
    ktab[Esc].type = KEY_IGNORE;
  ktab['M'].type = KEY_MONITOR;
  ktab['?'].type = KEY_HELP;
  for (i = 0; i <= 9; i++)
    ktab['0' + i].type = (enum keytype) (i + (int)KEY_0);
  ktab[Ctrl('G')].type = KEY_VBELL;
  ktab[':'].type = KEY_COLON;
#ifdef COPY_PASTE
  ktab['['].type = ktab[Ctrl('[')].type = KEY_COPY;
  ktab[']'].type = ktab[Ctrl(']')].type = KEY_PASTE;
  ktab['{'].type = KEY_HISTORY;
  ktab['}'].type = KEY_HISTNEXT;
  ktab['>'].type = KEY_WRITE_BUFFER;
  ktab['<'].type = KEY_READ_BUFFER;
  ktab['='].type = KEY_REMOVE_BUFFERS;
#endif
#ifdef POW_DETACH
  ktab['D'].type = KEY_POW_DETACH;
#endif
#ifdef LOCK
  ktab['x'].type = ktab[Ctrl('x')].type = KEY_LOCK;
#endif
}

/*
 * this is a braindamaged hack: if (obuf == NULL) then we provided
 * a key_type as a second char in ibuf. not a key.
 */
char *ProcessInput(ibuf, pilen, obuf, polen, obuf_size)
char *ibuf, *obuf;
register int *pilen, *polen, obuf_size;
{
  register int n;
  register enum keytype k;
  register char *s, *p;
  char buf[2];
  int newwidth;

  if (!obuf)
    obuf_size = 0;

  for (s = ibuf, p = obuf + *polen; *pilen > 0; --*pilen, s++)
    {
      if (*s == Esc)
	{
	  debug2("'%c %c ", s[0], s[1]);
	  debug2("%c %c' ", s[2], s[3]);
	  if (*pilen > 1)
	    {
	      --*pilen;
	      s++;
#if defined(GOULD_NP1)
	      k = (obuf)?(ktab[*s].type):(enum keytype)(int)(*s);
#else
	      k = (obuf)?(ktab[*s].type):(enum keytype)(*s);
#endif
	      debug2("Processinput C-A %02x '%c' ", k, k);
	      debug1("%s\n", (obuf)?"std":"NOOBUF");
	      if (*s == MetaEsc)
		{
		  if (*polen < obuf_size)
		    {
		      *p++ = Esc;
		      ++*polen;
		    }
		}
	      else if ((int)k >= (int)KEY_0 && (int)k <= (int)KEY_9)
		SwitchWindow((int)k - (int)KEY_0);
	      else
		switch (k)
		  {
		  case KEY_TERMCAP:
		    WriteFile(DUMP_TERMCAP);
		    break;
		  case KEY_HARDCOPY:
		    WriteFile(DUMP_HARDCOPY);
		    break;
		  case KEY_LOGTOGGLE:
		    LogToggle();
		    break;
#ifdef BSDJOBS
		  case KEY_SUSPEND:
		    *pilen = 0;
		    Detach(D_STOP);
		    break;
#endif
		  case KEY_SHELL:
		    debug("calling MakeWindow with shell\n");
		    MakeWindow(shellaka, ShellArgs, allflag, default_flow,
			       0, (char *) 0, loginflag, -1, (char *)0);
		    break;
		  case KEY_NEXT:
		    if (MoreWindows())
		      SwitchWindow(NextWindow());
		    break;
		  case KEY_PREV:
		    if (MoreWindows())
		      SwitchWindow(PreviousWindow());
		    break;
		  case KEY_KILL:
		    KillWindow(n = ForeNum);
#ifdef NETHACK
      		    if (nethackflag)
		      Msg(0, "You destroy poor window %d", n);
#endif
		    break;
		  case KEY_QUIT:
		    Finit(0);
		    /* NOTREACHED */
		  case KEY_DETACH:
		    *pilen = 0;
		    Detach(D_DETACH);
		    break;
#ifdef POW_DETACH
		  case KEY_POW_DETACH:
		    *pilen = 0;
		    if (obuf)
		      {
			buf[0] = *s;
			buf[1] = '\0';
			Msg(0, buf);
			read(0, buf, 1);
			if (*buf != *s)
			  {
			    write(1, "\007", 1);
			    RemoveStatus();
#ifdef NETHACK
			    if (nethackflag)
			       Msg(0, "The blast of disintegration whizzes by you!");
#endif
			    break;
			  }
		      }
		    Detach(D_POWER); /* detach and kill Attacher's
				      * parent	 */
		    break;
#endif
		  case KEY_REDISPLAY:
		    Activate(0);
		    break;
		  case KEY_WINDOWS:
		    ShowWindows();
		    break;
		  case KEY_VERSION:
		    Msg(0, "screen %d.%.2d.%.2d%s (%s) %s", REV, VERS,
		        PATCHLEVEL, STATE, ORIGIN, DATE);
		    break;
		  case KEY_TIME:
		    ShowTime();
		    break;
		  case KEY_INFO:
		    ShowInfo();
		    break;
		  case KEY_OTHER:
		    if (MoreWindows())
		      SwitchWindow(fore->WinLink);
		    break;
		  case KEY_XON:
		    if (*polen < obuf_size)
		      {
			*p++ = Ctrl('q');
			++*polen;
		      }
		    break;
		  case KEY_XOFF:
		    if (*polen < obuf_size)
		      {
			*p++ = Ctrl('s');
			++*polen;
		      }
		    break;
#ifdef LOCK
		  case KEY_LOCK:
		    Detach(D_LOCK); /* do it micha's way */
		    break;
#endif
		  case KEY_WIDTH:
		    if (Z0 || WS)
		      {
			if (fore->width == Z0width)
			  newwidth = Z1width;
			else if (fore->width == Z1width)
			  newwidth = Z0width;
			else if (fore->width > (Z0width+Z1width)/2)
			  newwidth = Z0width;
			else
			  newwidth = Z1width;
			ChangeWindowSize(fore, newwidth, fore->height);
			Activate(fore->norefresh);
		      }
		    else
		      Msg(0, "Your termcap does not specify how to change the terminal's width.");
		    break;
		  case KEY_LOGIN:
		    SlotToggle(0);
		    break;
		  case KEY_AKA:
		    if (!ktab[*s].args)
		      InputAKA();
		    else
		      strncpy(fore->cmd + fore->akapos, ktab[*s].args[0], 20);
		    break;
		  case KEY_COLON:
		    InputColon();
		    break;
		  case KEY_LASTMSG:
		    Msg(0, "%s", LastMsg);
		    break;
		  case KEY_SET:
		    DoSet(ktab[*s].args);
		    break;
		  case KEY_SCREEN:
		    debug3("KEY_SCREEN DoSc(, ktab[%d].args(='%s','%s')...)\n",
			   *s, ktab[*s].args[0], ktab[*s].args[1]);
		    DoScreen("key", ktab[*s].args);
		    break;
		  case KEY_CREATE:
		    debug2("KEY_CREATE MaWi(0, ktab[%d].args(='%s')...)\n",
			   *s, ktab[*s].args);
		    MakeWindow((char *) 0, ktab[*s].args, allflag, default_flow, 0, (char *) 0, loginflag, -1, (char *)0);
		    break;
		  case KEY_WRAP:
		    fore->wrap = !fore->wrap;
		    Msg(0, "%cwrap", fore->wrap ? '+' : '-');
		    break;
		  case KEY_FLOW:
		    if (fore->flow & FLOW_AUTOFLAG)
		      fore->flow = (fore->flow & FLOW_AUTO) | FLOW_NOW;
		    else if (fore->flow & FLOW_NOW)
		      fore->flow &= ~FLOW_NOW;
		    else
		      fore->flow = fore->flow ? FLOW_AUTOFLAG|FLOW_AUTO|FLOW_NOW : FLOW_AUTOFLAG;
		    SetFlow(fore->flow & FLOW_NOW);
		    Msg(0, "%cflow%s", (fore->flow & FLOW_NOW) ? '+' : '-',
			(fore->flow & FLOW_AUTOFLAG) ? "(auto)" : "");
		    break;
		  case KEY_CLEAR:
		    if (fore->state == LIT)
		      WriteString(fore, "\033[H\033[J", 6);
		    break;
		  case KEY_RESET:
		    if (fore->state == LIT)
		      WriteString(fore, "\033c", 2);
		    break;
		  case KEY_MONITOR:
		    if (fore->monitor == MON_OFF)
		      {
			fore->monitor = MON_ON;
			Msg(0,
			    "Window %d is now being monitored for all activity.",
			    ForeNum);
		      }
		    else
		      {
			fore->monitor = MON_OFF;
			Msg(0,
			    "Window %d is no longer being monitored for activity.",
			    ForeNum);
		      }
		    break;
		  case KEY_HELP:
		    display_help();
		    break;
		  case KEY_LICENSE:
		    display_copyright();
		    break;
#ifdef COPY_PASTE
		  case KEY_COPY:
		    (void) MarkRoutine(PLAIN);
		    break;
		  case KEY_HISTNEXT:
		    if (MarkRoutine(CRAZY))
		      if (copybuffer != NULL)
		        {
		  	  pastelen = copylen;
		  	  pastebuffer = copybuffer;
			  debug("histnext\n");
		        }
		    break;
		  case KEY_HISTORY:
		    if (MarkRoutine(TRICKY))
		      if (copybuffer != NULL)
			{
			  pastelen = copylen;
			  pastebuffer = copybuffer;
			  debug1("history new copylen: %d\n", pastelen);
			}
		    break;
		  case KEY_PASTE:
		    if (copybuffer == NULL)
		      {
#ifdef NETHACK
      			if (nethackflag)
			  Msg(0, "Nothing happens.");
      			else
#endif
			Msg(0, "empty buffer");
			copylen = 0;
			break;
		      }
		    pastelen = copylen;
		    pastebuffer = copybuffer;
		    break;
		  case KEY_WRITE_BUFFER:
		    if (copybuffer == NULL)
		      {
#ifdef NETHACK
      			if (nethackflag)
			  Msg(0, "Nothing happens.");
      			else
#endif
			Msg(0, "empty buffer");
			copylen = 0;
			break;
		      }
		    WriteFile(DUMP_EXCHANGE);
		    break;
		  case KEY_READ_BUFFER:
		    ReadFile();
		    break;
		  case KEY_REMOVE_BUFFERS:
		    KillBuffers();
		    break;
#endif				/* COPY_PASTE */
		  case KEY_VBELL:
		    if (visual_bell)
		      {
			visual_bell = 0;
			Msg(0, "switched to audible bell");
		      }
		    else
		      {
			visual_bell = 1;
			Msg(0, "switched to visual bell");
		      }
		    break;
		   default:
		    break;
		  }
	    }
	  else
	    ESCseen = 1;
	  --*pilen;
	  s++;
	  break;
	}
      else if (*polen < obuf_size)
	{
	  *p++ = *s;
	  ++*polen;
	}
    }
  return (s);
}

/* Send a terminal report as if it were typed. */ 
void
Report(wp, fmt, n1, n2)
struct win *wp;
char *fmt;
int n1, n2;
{
  register int n, len;
  char rbuf[40];

  sprintf(rbuf, fmt, n1, n2);
  len = strlen(rbuf);

  for (n = 0; n < MAXWIN; n++)
    {
      if (wp == wtab[n])
	{
	  if ((unsigned)(inlen[n] + len) <= sizeof *inbuf)
	    {
	      bcopy(rbuf, inbuf[n] + inlen[n], len);
	      if (inlen[n] == 0)
		inbuf_ct++;
	      inlen[n] += len;
	    }
	  break;
	}
    }/* for */
}

void
SwitchWindow(n)
int n;
{
  debug1("SwitchWindow %d\n", n);
  if (!wtab[n])
    {
      ShowWindows();
      return;
    }
  if (wtab[n] == fore)
    {
      Msg(0, "This IS window %d.", n);
      return;
    }
  SetForeWindow(n);
  if (!Detached && !in_ovl)
    Activate(fore->norefresh);
}

static void SetForeWindow(n)
int n;
{
  /*
   * If we come from another window, make it inactive.
   */
  if (fore)
    fore->active = 0;
  ForeNum = n;
  fore = wtab[n];
  if (!Detached && !in_ovl)
    fore->active = 1;
  /*
   * Place the window at the head of the most-recently-used list.
   */
  if ((n = WinList) != ForeNum)
    {
      /*
       * we had a bug here. we sometimes ran into n = -1; and crashed.
       * (this is not the perfect fix. "if (...) break;" inserted. jw.)
       */
      while (wtab[n]->WinLink != ForeNum)
	{
	  if (wtab[n]->WinLink == -1)
	    break;
	  n = wtab[n]->WinLink;
	}
      wtab[n]->WinLink = fore->WinLink;
      fore->WinLink = WinList;
      WinList = ForeNum;
    }
}

static int NextWindow()
{
  register struct win **pp;

  for (pp = wtab + ForeNum + 1; pp != wtab + ForeNum; ++pp)
    {
      if (pp == wtab + MAXWIN)
	pp = wtab;
      if (*pp)
	break;
    }
  return pp - wtab;
}

static int PreviousWindow()
{
  register struct win **pp;

  for (pp = wtab + ForeNum - 1; pp != wtab + ForeNum; --pp)
    {
      if (pp < wtab)
	pp = wtab + MAXWIN - 1;
      if (*pp)
	break;
    }
  return pp - wtab;
}

static int MoreWindows()
{
  if (fore->WinLink != -1)
    return 1;
#ifdef NETHACK
  if (nethackflag)
    Msg(0, "You cannot escape from window %d!", ForeNum);
  else
#endif
  Msg(0, "No other window.");
  return 0;
}

static void FreeWindow(wp)
struct win *wp;
{
#ifdef UTMPOK
  RemoveUtmp(wp);
#endif
#ifdef SUIDROOT
  (void) chmod(wp->tty, 0666);
  (void) chown(wp->tty, 0, 0);
#endif
  close(wp->ptyfd);
  if (wp->logfp != NULL)
    fclose(wp->logfp);
  ChangeWindowSize(wp, 0, 0);
  Free(wp);
}

int
MakeWindow(prog, args, aflag, flowflag, StartAt, dir, lflag, histheight, term)
char *prog, **args, *dir;
int aflag, flowflag, StartAt, lflag, histheight;
char *term; /* if term is nonzero we assume it "vt100" or the like.. */
{
  register struct win **pp, *p;
  register int n, f;
  int tf, tlflag;
  char ebuf[10];
#ifndef TIOCSWINSZ
  char libuf[20], cobuf[20];
#endif
  char tebuf[25];

  pp = wtab + StartAt;
  do
    {
      if (*pp == 0)
	break;
      if (++pp == wtab + MAXWIN)
	pp = wtab;
    } while (pp != wtab + StartAt);
  if (*pp)
    {
      Msg(0, "No more windows.");
      return -1;
    }

   if (((tlflag = lflag) == -1) && ((tlflag = loginflag) == -1))
	tlflag = LOGINDEFAULT;

#ifdef USRLIMIT
  /*
   * Count current number of users, if logging windows in.
   */
  if (tlflag == 1 && CountUsers() >= USRLIMIT)
    {
      Msg(0, "User limit reached.  Window will not be logged in.");
      tlflag = 0;
    }
#endif
  n = pp - wtab;
  debug1("Makewin creating %d\n", n);
  if ((f = OpenPTY()) == -1)
    {
      Msg(0, "No more PTYs.");
      return -1;
    }
#ifdef SYSV
  (void) fcntl(f, F_SETFL, O_NDELAY);
#else
  (void) fcntl(f, F_SETFL, FNDELAY);
#endif
#ifdef TIOCPKT
    {
# ifdef sgi
      /*
       * on IRIX 3.3, regardless of stream head's read mode (RNORM/RMSGN/RMSGD)
       * we loose data in TIOCPKT mode if our buffer is too small (IOSIZE)
       * to hold the whole packet at first read().
       * (Marc Boucher)
       */
      int flag = 0;
# else /* sgi */
      int flag = 1;
# endif /* sgi */

      if (ioctl(f, TIOCPKT, &flag))
	{
	  Msg(errno, "TIOCPKT ioctl");
	  close(f);
	  return -1;
	}
    }
#endif
  if ((p = (struct win *) malloc(sizeof(struct win))) == 0)
    {
      close(f);
      Msg_nomem;
      return -1;
    }
  bzero((char *) p, (int) sizeof(struct win));
  p->ptyfd = f;
  p->aflag = aflag;
  if (flowflag < 0)
    flowflag = default_flow;
  p->flow = flowflag | ((flowflag & FLOW_AUTOFLAG) ? (FLOW_AUTO|FLOW_NOW) : FLOW_AUTO);
  if (!prog)
    prog = Filename(args[0]);
  strncpy(p->cmd, prog, MAXSTR - 1);
  if ((prog = rindex(p->cmd, '|')) != NULL)
    {
      *prog++ = '\0';
      prog += strlen(prog);
      p->akapos = prog - p->cmd;
      p->autoaka = 0;
    }
  else
    p->akapos = 0;
  p->monitor = default_monitor;
  p->norefresh = 0;
  strncpy(p->tty, TtyName, MAXSTR - 1);
#ifdef SUIDROOT
  (void) chown(TtyName, real_uid, real_gid);
# ifdef UTMPOK
  (void) chmod(TtyName, tlflag ? TtyMode : (TtyMode & ~022));
# else
  (void) chmod(TtyName, TtyMode);
# endif
#endif

  if (histheight < 0)
    histheight = default_histheight;
  if (ChangeWindowSize(p, default_width, default_height))
    {
      FreeWindow(p);
      return -1;
    }
  ChangeScrollback(p, histheight, default_width);
  ResetScreen(p);
  debug("forking...\n");
  switch (p->wpid = fork())
    {
    case -1:
      Msg(errno, "fork");
      FreeWindow(p);
      return -1;
    case 0:
      signal(SIGHUP, SIG_DFL);
      signal(SIGINT, SIG_DFL);
      signal(SIGQUIT, SIG_DFL);
      signal(SIGTERM, SIG_DFL);
#ifdef BSDJOBS
      signal(SIGTTIN, SIG_DFL);
      signal(SIGTTOU, SIG_DFL);
#endif
      setuid(real_uid);
      setgid(real_gid);
      if (dir && chdir(dir) == -1)
	{
	  SendErrorMsg("Cannot chdir to %s: %s", dir, sys_errlist[errno]);
	  eexit(1);
	}

      freetty();
      if ((tf = open(TtyName, O_RDWR)) == -1)
	{
	  SendErrorMsg("Cannot open %s: %s", TtyName, sys_errlist[errno]);
	  eexit(1);
	}
#ifdef SVR4
      if (ioctl(tf, I_PUSH, "ptem"))
	{
	  SendErrorMsg("Cannot I_PUSH ptem %s %s", TtyName, sys_errlist[errno]);
	  eexit(1);
	}
      if (ioctl(tf, I_PUSH, "ldterm"))
	{
	  SendErrorMsg("Cannot I_PUSH ldterm %s %s", TtyName, sys_errlist[errno]);
	  eexit(1);
	}
      if (ioctl(tf, I_PUSH, "ttcompat"))
	{
	  SendErrorMsg("Cannot I_PUSH ttcompat %s %s", TtyName, sys_errlist[errno]);
	  eexit(1);
	}
#endif
      (void) dup2(tf, 0);
      (void) dup2(tf, 1);
      (void) dup2(tf, 2);
#ifdef DEBUG
      dfp = stderr;
#endif
      closeallfiles();
      fgtty();
#ifdef TIOCSWINSZ
      glwz.ws_col = p->width;
      glwz.ws_row = p->height;
      (void) ioctl(0, TIOCSWINSZ, &glwz);
#else
      sprintf(libuf, "LINES=%d", p->height);
      sprintf(cobuf, "COLUMNS=%d", p->width);
      NewEnv[4] = libuf;
      NewEnv[5] = cobuf;
#endif
      SetTTY(0, &OldMode);
      if (aflag)
        NewEnv[2] = MakeTermcap(1);
      else
        NewEnv[2] = Termcap;
      if (term && *term && strcmp(screenterm, term) &&
	  (strlen(term) < 20))
	{
          char *s1, *s2, tl;

	  sprintf(tebuf, "TERM=%s", term);
	  debug2("Makewindow %d with %s\n", n, tebuf);
          tl = strlen(term);
	  NewEnv[1] = tebuf;
          if (s1 = index(Termcap, '|'))
	    {
	      if (s2 = index(++s1, '|'))
		{
		  if (strlen(Termcap) - (s2 - s1) + tl < 1024)
		    {
		      bcopy(s2, s1 + tl, strlen(s2) + 1);
		      bcopy(term, s1, tl);
		    }
		}
            }
	}
      sprintf(ebuf, "WINDOW=%d", n);
      NewEnv[3] = ebuf;

      execvpe(*args, args, NewEnv);
      SendErrorMsg("Cannot exec %s: %s", *args, sys_errlist[errno]);
      exit(1);
    } /* end fork switch */
  /*
   * Place the newly created window at the head of the most-recently-used list.
   */
  *pp = p;
  p->WinLink = WinList;
  WinList = n;
  HasWindow = 1;
#ifdef UTMPOK
  debug1("MakeWindow will %slog in.\n", tlflag?"":"not ");
  if (tlflag == 1)
    SetUtmp(p, n);
  else
    p->slot = (slot_t) -1;
#endif
  SetForeWindow(n);
  Activate(0);
  return n;
}

static void execvpe(prog, args, env)
char *prog, **args, **env;
{
  register char *path, *p;
  char buf[1024];
  char *shargs[MAXARGS + 1];
  register int i, eaccess = 0;

  if (prog[0] == '/')
    path = "";
  else if ((path = getenv("PATH")) == 0)
    path = DefaultPath;
  do
    {
      p = buf;
      while (*path && *path != ':')
	*p++ = *path++;
      if (p > buf)
	*p++ = '/';
      strcpy(p, prog);
      if (*path)
	++path;
      execve(buf, args, env);
      switch (errno)
	{
	case ENOEXEC:
	  shargs[0] = DefaultShell;
	  shargs[1] = buf;
	  for (i = 1; (shargs[i + 1] = args[i]) != NULL; ++i)
	    ;
	  execve(DefaultShell, shargs, env);
	  return;
	case EACCES:
	  eaccess = 1;
	  break;
	case ENOMEM:
	case E2BIG:
	case ETXTBSY:
	  return;
	}
    } while (*path);
  if (eaccess)
    errno = EACCES;
}


static void LogToggle()
{
  char buf[1024];

  sprintf(buf, "screenlog.%d", ForeNum);
  if (fore->logfp != NULL)
    {
      Msg(0, "Logfile \"%s\" closed.", buf);
      fclose(fore->logfp);
      fore->logfp = NULL;
      return;
    }
  if ((fore->logfp = secfopen(buf, "a")) == NULL)
    {
      Msg(errno, "Error opening logfile \"%s\"", buf);
      return;
    }
  Msg(0, "%s logfile \"%s\"", ftell(fore->logfp) ? "Appending to" : "Creating", buf);
}

#ifdef NOREUID
static int UserPID;
static SIGTYPE (*Usersigcld)__P(SIGPROTOARG);
#endif
static int UserSTAT;

int UserContext()
{
#ifdef NOREUID
  if (eff_uid == real_uid)
    return(1);
# ifdef SYSV
  Usersigcld = signal(SIGCLD, SIG_DFL);
# else
  Usersigcld = signal(SIGCHLD, SIG_DFL);
# endif
  debug("UserContext: forking.\n");
  switch (UserPID = fork())
    {
    case -1:
      Msg(errno, "fork");
      return -1;
    case 0:
      signal(SIGHUP, SIG_DFL);
      signal(SIGINT, SIG_IGN);
      signal(SIGQUIT, SIG_DFL);
      signal(SIGTERM, SIG_DFL);
# ifdef BSDJOBS
      signal(SIGTTIN, SIG_DFL);
      signal(SIGTTOU, SIG_DFL);
# endif
      setuid(real_uid);
      setgid(real_gid);
      return 1;
    default:
      return 0;
    }
#else
  setreuid(eff_uid, real_uid);
  setregid(eff_gid, real_gid);
  return 1;
#endif
}

void
UserReturn(val)
int val;
{
#if defined(NOREUID)
  if (eff_uid == real_uid)
    UserSTAT = val;
  else
    exit(val);
#else
  setreuid(real_uid, eff_uid);
  setregid(real_gid, eff_gid);
  UserSTAT = val;
#endif
}

int UserStatus()
{
#ifdef NOREUID
  int i;
# ifdef BSDWAIT
  union wait wstat;
# else
  int wstat;
# endif

  if (eff_uid == real_uid)
    return UserSTAT;
  if (UserPID < 0)
    return -1;
  while ((errno = 0, i = wait(&wstat)) != UserPID)
    if (i < 0 && errno != EINTR)
      break;
# ifdef SYSV
  (void) signal(SIGCLD, Usersigcld);
# else
  (void) signal(SIGCHLD, Usersigcld);
# endif
  if (i == -1)
    return -1;
  return (WEXITSTATUS(wstat));
#else
  return UserSTAT;
#endif
}

static void ShowWindows()
{
  char buf[1024];
  register char *s;
  register struct win **pp, *p;
  register int i, OtherNum = fore->WinLink;
  register char *cmd;

  for (i = 0, s = buf, pp = wtab; pp < wtab + MAXWIN; ++i, ++pp)
    {
      if ((p = *pp) == 0)
	continue;

      if (p->akapos)
	{
	  if (*(p->cmd + p->akapos) && *(p->cmd + p->akapos - 1) != ':')
	    cmd = p->cmd + p->akapos;
	  else
	    cmd = p->cmd + strlen(p->cmd) + 1;
	}
      else
	cmd = p->cmd;
      if (s - buf + 5 + strlen(cmd) > fore->width - 1)
	break;
      if (s > buf)
	{
	  *s++ = ' ';
	  *s++ = ' ';
	}
      *s++ = i + '0';
      if (i == ForeNum)
	*s++ = '*';
      else if (i == OtherNum)
	*s++ = '-';
      if (p->monitor == MON_DONE)
	*s++ = '@';
      if (p->bell == BELL_DONE)
	*s++ = '!';
#ifdef UTMPOK
      if (p->slot != (slot_t) 0 && p->slot != (slot_t) -1)
	*s++ = '$';
#endif
      if (p->logfp != NULL)
	{
	  strcpy(s, "(L)");
	  s += 3;
	}
      *s++ = ' ';
      strcpy(s, cmd);
      s += strlen(s);
      if (i == ForeNum)
	{
	  /* 
	   * this is usually done by Activate(), but when looking
	   * on your current window, you may get annoyed, as there is still
	   * that temporal '!' and '@' displayed.
	   * So we remove that after displaying it once.
	   */
	  p->bell = BELL_OFF;
	  if (p->monitor != MON_OFF)
	    p->monitor = MON_ON;
	}
    }
  *s++ = ' ';
  *s = '\0';
  Msg(0, "%s", buf);
}

#ifdef LOADAV_3LONGS
extern long loadav[3];
#else
# ifdef LOADAV_4LONGS
extern long loadav[4];
# else
#  ifdef LOADAV_NEXT
extern float loadav;
#  else
extern double loadav[3];
#  endif
# endif
#endif
extern avenrun;

static void ShowTime()
{
  char buf[512];
#ifdef LOADAV
  char *p;
#endif
  struct tm *tp;
  time_t now;

  (void) time(&now);
  tp = localtime(&now);
  sprintf(buf, "%2d:%02.2d:%02.2d %s", tp->tm_hour, tp->tm_min, tp->tm_sec,
	  HostName);
#ifdef LOADAV
  if (avenrun && GetAvenrun())
    {
      p = buf + strlen(buf);
# ifdef LOADAV_3LONGS
      sprintf(p, " %2.2f %2.2f %2.2f", (double) loadav[0] / FSCALE,
	      (double) loadav[1] / FSCALE, (double) loadav[2] / FSCALE);
# else
#  ifdef LOADAV_4LONGS
      sprintf(p, " %2.2f %2.2f %2.2f %2.2f", (double) loadav[0] / 100,
	      (double) loadav[1] / 100, (double) loadav[2] / 100,
	      (double) loadav[3] / 100);
#  else
#   ifdef LOADAV_NEXT
      sprintf(p, " %2.2f", loadav);
#   else
#    ifdef apollo
      sprintf(p, " %2.2f %2.2f %2.2f", loadav[0]/65536.0, loadav[1]/65536.0,
	      loadav[2]/65536.0);
#    else
      sprintf(p, " %2.2f %2.2f %2.2f", loadav[0], loadav[1], loadav[2]);
#    endif /* apollo */
#   endif /* LOADAV_NEXT */
#  endif /* LOADAV_4LONGS */
# endif /* LOADAV_3LONGS */
    }
#endif /* LOADAV */
  Msg(0, "%s", buf);
}

static void ShowInfo()
{
  char buf[512], *p;
  register struct win *wp = fore;
  register int i;

  sprintf(buf, "(%d,%d)/(%d,%d)+%d %c%sflow %cins %corg %cwrap %capp %clog %cmon %cr",
	  wp->x + 1, wp->y + 1, wp->width, wp->height,
	  wp->histheight,
	  (wp->flow & FLOW_NOW) ? '+' : '-',
	  (wp->flow & FLOW_AUTOFLAG) ? "" : ((wp->flow & FLOW_AUTO) ? "(+)" : "(-)"),
	  wp->insert ? '+' : '-', wp->origin ? '+' : '-',
	  wp->wrap ? '+' : '-', wp->keypad ? '+' : '-',
	  (wp->logfp != NULL) ? '+' : '-',
	  (wp->monitor != MON_OFF) ? '+' : '-',
	  wp->norefresh ? '-' : '+');
  if (ISO2022)
    {
      p = buf + strlen(buf);
      sprintf(p, " G%1d [", wp->LocalCharset);
      for (i = 0; i < 4; i++)
	p[i + 5] = wp->charsets[i] ? wp->charsets[i] : 'B';
      p[9] = ']';
      p[10] = '\0';
    }
  Msg(0, "%s", buf);
}

#if defined(sequent) || defined(_SEQUENT_) || defined(SVR4)

static int OpenPTY()
{
  char *m, *s;
  register int f;
# ifdef SVR4
  char *ptsname();
  SIGTYPE (*sigcld)();

  if ((f = open("/dev/ptmx", O_RDWR)) == -1)
    return(-1);

  /*
   * SIGCLD set to SIG_DFL for grantpt() because it fork()s and
   * exec()s pt_chmod
   */
  sigcld = signal(SIGCLD, SIG_DFL);
       
  if ((m = ptsname(f)) == NULL || unlockpt(f) || grantpt(f))
    {
      signal(SIGCLD, sigcld);
      close(f);
      return(-1);
    } 
  signal(SIGCLD, sigcld);
  strncpy(TtyName, m, sizeof TtyName);
# else /* SVR4 */
  if ((f = getpseudotty(&s, &m)) < 0)
    return(-1);
  strncpy(PtyName, m, sizeof PtyName);
  strncpy(TtyName, s, sizeof TtyName);
# endif /* SVR4 */
# ifdef POSIX
  tcflush(f, TCIOFLUSH);
# else
  (void) ioctl(f, TIOCFLUSH, (char *) 0);
# endif
# ifdef LOCKPTY
  (void) ioctl(f, TIOCEXCL, (char *) 0);
# endif
  return (f);
}

#else /* defined(sequent) || defined(_SEQUENT_) || defined(SVR4) */
# ifdef MIPS

static int OpenPTY()
{
  register char *p, *l, *d;
  register f, tf;
  register my_minor;
  struct stat buf;
   
  strcpy(PtyName, PtyProto);
  for (p = PtyName; *p != 'X'; ++p)
    ;
  for (l = "zyxwvutsrqp"; *p = *l; ++l)
    {
      for (d = "0123456789abcdef"; p[1] = *d; ++d)
	{
	  if ((f = open(PtyName, O_RDWR)) != -1)
	    {
	      fstat(f, &buf);
	      my_minor = minor(buf.st_rdev);
	      sprintf(TtyName, "/dev/ttyq%d", my_minor);
	      if ((tf = open(TtyName, O_RDWR)) != -1)
		{
		  close(tf);
#ifdef LOCKPTY
		  (void) ioctl(f, TIOCEXCL, (char *)0);
#endif
		  return f;
		}
	      close(f);
	    }
	}
    }
  return -1;
}

# else  /* MIPS */
#  ifdef sgi

static int OpenPTY()
{
  register f;
  register my_minor;
  struct stat buf;
   
  strcpy(PtyName, "/dev/ptc");
  f = open(PtyName, O_RDWR|O_NDELAY);
  if (f >= 0)
    {
      if (fstat(f, &buf) < 0)
	{
	  close(f);
	  return -1;
	}
      my_minor = minor(buf.st_rdev);
      sprintf(TtyName, "/dev/ttyq%d", my_minor);
    }
  return f;
}

#  else /* sgi */
#   ifdef _AIX /* RS6000 */

static int OpenPTY()
{
  register int i, f, tf;

  for (i = 0; i < 256; i++)
    {
      sprintf(PtyName, "/dev/ptc/%d", i);
      if ((f = open(PtyName, O_RDWR)) != -1)
	{
	  sprintf(TtyName, "/dev/pts/%d", i);
	  if ((tf = open(TtyName, O_RDWR)) != -1)
	    {
	      close(tf);
#ifdef LOCKPTY
	      (void) ioctl(f, TIOCEXCL, (char *) 0);
#endif
	      return f;
	    }
	  close(f);
	}
    }
  return -1;
}

#   else /* _AIX, RS6000 */

static int OpenPTY()
{
  register char *p, *q, *l, *d;
  register int f, tf;

#    if !defined(hpux)
  debug("Hello, You are none of: sequent, _SEQUENT_, SVR4, MIPS, sgi, AIX\n");
  debug("       This OpenPTY() is for hpux, ... and for you?\n");
#    endif
  strcpy(PtyName, PtyProto);
  strcpy(TtyName, TtyProto);
  for (p = PtyName; *p != 'X'; ++p)
    ;
  for (q = TtyName; *q != 'X'; ++q)
    ;
#ifdef sequent
  /* why ask for sequent in #else (not sequent) section? jw. */
  for (l = "p"; (*p = *l) != '\0'; ++l)
    {				/* } */
      for (d = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"; (p[1] = *d) != '\0'; ++d)
	{			/* } */
#else
# ifdef hpux
  for (l = "pqrstuvw"; (*p = *l) != '\0'; ++l)
# else
  for (l = "qpr"; (*p = *l) != '\0'; ++l)
#endif
    {
      for (d = "0123456789abcdef"; (p[1] = *d) != '\0'; ++d)
	{
#endif
	  if ((f = open(PtyName, O_RDWR)) != -1)
	    {
	      q[0] = *l;
	      q[1] = *d;
	      if ((tf = open(TtyName, O_RDWR)) != -1)
		{
		  /* close tf, thus we also get rid of an unwanted
		   * controlling terminal! 
		   */
		  close(tf);
#ifdef LOCKPTY
		  (void) ioctl(f, TIOCEXCL, (char *) 0);
#endif
		  return f;
		}
	      close(f);
	    }
	}
    }
  return -1;
}

#   endif /* _AIX, RS6000 */
#  endif /* sgi */
# endif /* MIPS */
#endif

void 
SetTTY(fd, mp)
int fd;
struct mode *mp;
{
  errno = 0;
#ifdef POSIX
  tcsetattr(fd, TCSADRAIN, &mp->tio);
# ifdef hpux
  ioctl(fd, TIOCSLTC, &mp->m_ltchars);
# endif
#else
# ifdef TERMIO
  ioctl(fd, TCSETA, &mp->tio);
# else
  /* ioctl(fd, TIOCSETP, &mp->m_ttyb); */
  ioctl(fd, TIOCSETC, &mp->m_tchars);
  ioctl(fd, TIOCSLTC, &mp->m_ltchars);
  ioctl(fd, TIOCLSET, &mp->m_lmode);
  ioctl(fd, TIOCSETD, &mp->m_ldisc);
  ioctl(fd, TIOCSETP, &mp->m_ttyb);
# endif
#endif
  if (errno)
    Msg(0, "SetTTY: ioctl failed");
}

void
GetTTY(fd, mp)
int fd;
struct mode *mp;
{
  errno = 0;
#ifdef POSIX
  tcgetattr(fd, &mp->tio);
# ifdef hpux
  ioctl(fd, TIOCGLTC, &mp->m_ltchars);
# endif
#else
# ifdef TERMIO
  ioctl(fd, TCGETA, &mp->tio);
# else
  ioctl(fd, TIOCGETP, &mp->m_ttyb);
  ioctl(fd, TIOCGETC, &mp->m_tchars);
  ioctl(fd, TIOCGLTC, &mp->m_ltchars);
  ioctl(fd, TIOCLGET, &mp->m_lmode);
  ioctl(fd, TIOCGETD, &mp->m_ldisc);
# endif
#endif
  if (errno)
    Msg(0, "GetTTY: ioctl failed");
}

void
SetMode(op, np)
struct mode *op, *np;
{
  *np = *op;

#if defined(TERMIO) || defined(POSIX)
  np->tio.c_iflag &= ~ICRNL;
# ifdef ONLCR
  np->tio.c_oflag &= ~ONLCR;
# endif
  np->tio.c_lflag &= ~(ICANON | ECHO);

  /*
   * Unfortunately, the master process never will get SIGINT if the real
   * terminal is different from the one on which it was originaly started
   * (process group membership has not been restored or the new tty could not
   * be made controlling again). In my solution, it is the attacher who
   * receives SIGINT (because it is always correctly associated with the real
   * tty) and forwards it to the master [kill(MasterPid, SIGINT)]. 
   * Marc Boucher (marc@CAM.ORG)
   */
  np->tio.c_lflag |= ISIG;
  /* 
   * careful, careful catche monkey..
   * never set VMIN and VTIME to zero, if you want blocking io.
   */
  np->tio.c_cc[VMIN] = 1;
  np->tio.c_cc[VTIME] = 0;
#ifdef VSTART
  startc = op->tio.c_cc[VSTART];
#endif
#ifdef VSTOP
  stopc = op->tio.c_cc[VSTOP];
#endif
  if (iflag)
    intrc = op->tio.c_cc[VINTR];
  else
    intrc = np->tio.c_cc[VINTR] = 0377;
  np->tio.c_cc[VQUIT] = 0377;
  if (flow == 0)
    {
      np->tio.c_cc[VINTR] = 0377;
#ifdef VSTART
      np->tio.c_cc[VSTART] = 0377;
#endif
#ifdef VSTOP
      np->tio.c_cc[VSTOP] = 0377;
#endif
      np->tio.c_iflag &= ~IXON;
    }
#ifdef VDISCARD
  np->tio.c_cc[VDISCARD] = 0377;
#endif
#ifdef VSUSP
  np->tio.c_cc[VSUSP] = 0377;
#endif
# ifdef hpux
  np->m_ltchars.t_suspc = 0377;
  np->m_ltchars.t_dsuspc = 0377;
  np->m_ltchars.t_flushc = 0377;
  np->m_ltchars.t_lnextc = 0377;
# else
#  ifdef VDSUSP
  np->tio.c_cc[VDSUSP] = 0377;
#  endif
# endif
#else
  startc = op->m_tchars.t_startc;
  stopc = op->m_tchars.t_stopc;
  if (iflag)
    intrc = op->m_tchars.t_intrc;
  else
    intrc = np->m_tchars.t_intrc = -1;
  np->m_ttyb.sg_flags &= ~(CRMOD | ECHO);
  np->m_ttyb.sg_flags |= CBREAK;
  np->m_tchars.t_quitc = -1;
  if (flow == 0)
    {
      np->m_tchars.t_intrc = -1;
      np->m_tchars.t_startc = -1;
      np->m_tchars.t_stopc = -1;
    }
  np->m_ltchars.t_suspc = -1;
  np->m_ltchars.t_dsuspc = -1;
  np->m_ltchars.t_flushc = -1;
  np->m_ltchars.t_lnextc = -1;
#endif				/* defined(TERMIO) || defined(POSIX) */
}

void
SetFlow(on)
int on;
{
  if (flow == on)
    return;
#if defined(TERMIO) || defined(POSIX)
  if (on)
    {
      NewMode.tio.c_cc[VINTR] = intrc;
#ifdef VSTART
      NewMode.tio.c_cc[VSTART] = startc;
#endif
#ifdef VSTOP
      NewMode.tio.c_cc[VSTOP] = stopc;
#endif
      NewMode.tio.c_iflag |= IXON;
    }
  else
    {
      NewMode.tio.c_cc[VINTR] = 0377;
#ifdef VSTART
      NewMode.tio.c_cc[VSTART] = 0377;
#endif
#ifdef VSTOP
      NewMode.tio.c_cc[VSTOP] = 0377;
#endif
      NewMode.tio.c_iflag &= ~IXON;
    }
# ifdef POSIX
  if (tcsetattr(0, TCSADRAIN, &NewMode.tio))
# else
  if (ioctl(0, TCSETA, &NewMode.tio) != 0)
# endif
    debug1("SetFlow: ioctl errno %d\n", errno);
#else
  if (on)
    {
      NewMode.m_tchars.t_intrc = intrc;
      NewMode.m_tchars.t_startc = startc;
      NewMode.m_tchars.t_stopc = stopc;
    }
  else
    {
      NewMode.m_tchars.t_intrc = -1;
      NewMode.m_tchars.t_startc = -1;
      NewMode.m_tchars.t_stopc = -1;
    }
  if (ioctl(0, TIOCSETC, &NewMode.m_tchars) != 0)
    debug1("SetFlow: ioctl errno %d\n", errno);
#endif				/* defined(TERMIO) || defined(POSIX) */
  flow = on;
}

/* we return 1 if we could attach one, or 0 if none */
static int Attach(how)
int how;
{
  int lasts;
  struct msg m;
  struct stat st;
  char *s;

  if (how == MSG_WINCH)
    {
      bzero((char *) &m, sizeof(m));
      m.type = how;
      if ((lasts = MakeClientSocket(0, SockName)) >= 0)
	{
          write(lasts, &m, sizeof(m));
          close(lasts);
	}
      return 0;
    }

  if (how == MSG_CONT)
    {
      if ((lasts = MakeClientSocket(0, SockName)) < 0)
        {
          printf("Sorry, cannot contact session \"%s\" again\r\n",
                 SockName);
          sleep(2);
          how = MSG_ATTACH;
        }
    }
    
  if (how != MSG_CONT)
    {
      switch (FindSocket(how, &lasts))
        {
        case 0:
          if (rflag == 2)
	    return 0;
          if (quietflag)
	    eexit(10);
          if (SockName && *SockName)
            Msg(0, "There is no screen to be %sed matching %s.", 
	        dflag ? "detach" : "resum", SockName);
          else
            Msg(0, "There is no screen to be %sed.",
                dflag ? "detach" : "resum");
          /* NOTREACHED */
        case 1:
          break;
        default:
          Msg(0, "Type \"screen [-d] -r [pid.]tty.host\" to resume one of them.");
          /* NOTREACHED */
        }
      /*
       * Go in UserContext. Advantage is, you can kill your attacher
       * when things go wrong. Any disadvantages? jw.
       */
      setuid(real_uid);
      setgid(real_gid);

      SockName = SockNamePtr;
      MasterPid = 0;
      while (*SockName)
        {
          if (*SockName > '9' || *SockName < '0')
	    break;
          MasterPid = 10 * MasterPid + *SockName - '0';
          SockName++;
        }
      SockName = SockNamePtr;
      debug1("Attach decided, it is '%s'\n", SockPath);
      debug1("Attach found MasterPid == %d\n", MasterPid);
      if (stat(SockPath, &st) == -1)
        Msg(errno, "stat %s", SockPath);
      if ((st.st_mode & 0700) != (dflag ? 0700 : 0600))
        Msg(0, "That screen is %sdetached.", dflag ? "already " : "not ");
#ifdef REMOTE_DETACH
      if (dflag &&
          (how == MSG_ATTACH || how == MSG_DETACH || how == MSG_POW_DETACH))
        {
          strcpy(m.m.detach.tty, attach_tty);
          debug1("attach_tty is %s\n", attach_tty);
          m.m.detach.dpid = getpid();
# ifdef POW_DETACH
          if (dflag == 2)
	    m.type = MSG_POW_DETACH;
          else
# endif
	    m.type = MSG_DETACH;
          if (write(lasts, (char *) &m, sizeof(m)) != sizeof(m))
    	    Msg(errno, "write");
          close(lasts);
          if (how != MSG_ATTACH)
    	    return 0;	/* we detached it. jw. */
          sleep(1);	/* we dont want to overrun our poor backend. jw. */
          if ((lasts = MakeClientSocket(0, SockName)) == -1)
            Msg(0, "Cannot contact screen again. Shit.");
        }
#endif
    }
  m.type = how;
  strcpy(m.m.attach.tty, attach_tty);
  debug1("attach_tty is %s\n", attach_tty);
  s = getenv("TERM");
  if (s)
    {
      if (strlen(s) >= MAXPATH - 5)
	Msg(0, "$TERM too long - sorry.");
      sprintf(m.m.attach.envterm, "TERM=%s", s);
    }
  else
    *m.m.attach.envterm = '\0';
  debug1("attach: sending %d bytes... ", sizeof m);

  m.m.attach.apid = getpid();
  m.m.attach.adaptflag = adaptflag;
  m.m.attach.lines = m.m.attach.columns = 0;
  if (s = getenv("LINES"))
    m.m.attach.lines = atoi(s);
  if (s = getenv("COLUMNS"))
    m.m.attach.columns = atoi(s);

#ifdef PASSWORD
  if (how == MSG_ATTACH || how == MSG_CONT)
    trysend(lasts, &m, m.m.attach.password);
  else
#endif
    {
      if (write(lasts, (char *) &m, sizeof(m)) != sizeof(m))
	Msg(errno, "write");
      close(lasts);
    }
  debug1("Attach(%d): sent\n", m.type);
  Suspended = 0;
  rflag = 0;
  return 1;
}


#ifdef PASSWORD

static trysendstat;

static SIGTYPE trysendok(SIGDEFARG)
{
  trysendstat = 1;
}

static SIGTYPE trysendfail(SIGDEFARG)
{
  trysendstat = -1;
# ifdef SYSV
  signal(SIG_PW_FAIL, trysendfail);
# endif /* SYSV */
}

static char screenpw[9];

static void trysend(fd, m, pwto)
int fd;
struct msg *m;
char *pwto;
{
  char *npw = NULL;
  SIGTYPE (*sighup)();
  SIGTYPE (*sigusr1)();
  int tries;

  sigusr1 = signal(SIG_PW_OK, trysendok);
  sighup = signal(SIG_PW_FAIL, trysendfail);
  for (tries = 0; ; )
    {
      strcpy(pwto, screenpw);
      trysendstat = 0;
      if (write(fd, (char *) m, sizeof(*m)) != sizeof(*m))
	Msg(errno, "write");
      close(fd);
      while (trysendstat == 0)
	pause();
      if (trysendstat > 0)
	{
	  signal(SIG_PW_OK, sigusr1);
	  signal(SIG_PW_FAIL, sighup);
	  return;
	}
      if (++tries > 1 || (npw = getpass("Screen Password:")) == 0 || *npw == 0)
	Msg(0, "Password incorrect");
      strncpy(screenpw, npw, 8);
      if ((fd = MakeClientSocket(0, SockName)) == -1)
	Msg(0, "Cannot contact screen again. Shit.");
    }
}
#endif /* PASSWORD */


/*
 * Unfortunatelly this is also the SIGHUP handler, so we have to
 * check, if the backend is already detached.
 */

static SIGTYPE AttacherFinit(SIGDEFARG)
{
  struct stat statb;
  struct msg m;
  int s;

  debug("AttacherFinit();\n");
  signal(SIGHUP, SIG_IGN);
  /* Check if signal comes from backend */
  if (SockName)
    {
      strcpy(SockNamePtr, SockName);
      if (stat(SockPath, &statb) == 0 && (statb.st_mode & 0777) != 0600)
	{
	  debug("Detaching backend!\n");
	  bzero((char *) &m, sizeof(m));
	  strcpy(m.m.detach.tty, attach_tty);
          debug1("attach_tty is %s\n", attach_tty);
	  m.m.detach.dpid = getpid();
	  m.type = MSG_HANGUP;
	  if ((s = MakeClientSocket(0, SockName)) >= 0)
	    {
	      write(s, &m, sizeof(m));
	      close(s);
	    }
	}
    }
  exit(0);
#ifndef SIGVOID
  return((SIGTYPE) 0);
#endif
}

#ifdef POW_DETACH
static SIGTYPE AttacherFinitBye(SIGDEFARG)
{
  int ppid;
  debug("AttacherFintBye()\n");
  freetty();
  setuid(real_uid);
  setgid(real_gid);
  /* we don't want to disturb init (even if we were root), eh? jw */
  if ((ppid = getppid()) > 1)
    Kill(ppid, SIGHUP);		/* carefully say good bye. jw. */
  exit(0);
#ifndef SIGVOID
  return((SIGTYPE) 0);
#endif
}
#endif

static SuspendPlease;

static SIGTYPE SigStop(SIGDEFARG)
{
  debug("SigStop()\n");
  SuspendPlease = 1;
#ifndef SIGVOID
  return((SIGTYPE) 0);
#endif
}

#ifdef LOCK
static LockPlease;

static SIGTYPE DoLock(SIGDEFARG)
{
  debug("DoLock()\n");
  LockPlease = 1;
# ifdef SYSV
  signal(SIG_LOCK, DoLock);
# endif
# ifndef SIGVOID
  return((SIGTYPE) 0);
# endif
}
#endif

#if defined(SIGWINCH) && defined(TIOCGWINSZ)
static SigWinchPlease;

static SIGTYPE SigAttWinch(SIGDEFARG)
{
  debug("SigAttWinch()\n");
  SigWinchPlease = 1;
# ifndef SIGVOID
  return((SIGTYPE) 0);
# endif
}
#endif

static void Attacher()
{
  /*
   * permanent in UserContext. Advantage is, you can kill your attacher
   * when things go wrong. Any disadvantages? jw.
   */
  setuid(real_uid);	/* XXX: already done in Attach() */
  setgid(real_gid);	/* XXX: already done in Attach() */

  signal(SIGHUP, AttacherFinit);
  signal(SIG_BYE, AttacherFinit);
#ifdef POW_DETACH
  signal(SIG_POWER_BYE, AttacherFinitBye);
#endif
#ifdef LOCK
  signal(SIG_LOCK, DoLock);
#endif
  signal(SIGINT, AttacherSigInt);
#ifdef BSDJOBS
  signal(SIG_STOP, SigStop);
#endif
#if defined(SIGWINCH) && defined(TIOCGWINSZ)
  signal(SIGWINCH, SigAttWinch);
#endif
#ifdef DEBUG
# ifdef SYSV
  signal(SIGCLD, FEChld);
# else
  signal(SIGCHLD, FEChld);
# endif
#endif
  debug("attacher: going for a nap.\n");
  dflag = 0;
  while (1)
    {
      pause();
      debug("attacher: huh! a signal!\n");
#ifdef DEBUG
      if (FEpanic)
        {
	  printf("\n\rSuddenly the Dungeon collapses!! - You die...\n\r");
	  SetTTY(0, &OldMode);
	  eexit(1);
        }
#endif
#ifdef BSDJOBS
      if (SuspendPlease)
	{
	  SuspendPlease = 0;
	  signal(SIGTSTP, SIG_DFL);
	  debug("attacher: killing myself SIGTSTP\n");
	  kill(getpid(), SIGTSTP);

	  debug1("attacher: continuing from stop(%d)\n", Suspended);
	  signal(SIG_STOP, SigStop);
	  (void) Attach(MSG_CONT);
	}
#endif
#ifdef LOCK
      if (LockPlease)
	{
	  LockPlease = 0;
	  LockTerminal();
# ifdef SYSV
	  signal(SIG_LOCK, DoLock);
# endif
	  (void) Attach(MSG_CONT);
	}
#endif	/* LOCK */
#if defined(SIGWINCH) && defined(TIOCGWINSZ)
      if (SigWinchPlease)
	{
	  SigWinchPlease = 0;
# ifdef SYSV
	  signal(SIGWINCH, SigAttWinch);
# endif
	  (void) Attach(MSG_WINCH);
	}
#endif	/* SIGWINCH */
    }
}

#ifdef LOCK

/* ADDED by Rainer Pruy 10/15/87 */
/* POLISHED by mls. 03/10/91 */

static char LockEnd[] = "Welcome back to screen !!\n";

static void LockTerminal()
{
  char *prg;
  int sig, pid;
  SIGTYPE (*sigs[NSIG])__P(SIGPROTOARG);

  for (sig = 1; sig < NSIG; sig++)
    {
      sigs[sig] = signal(sig, SIG_IGN);
    }
  SetTTY(0, &OldMode);
  printf("\n");

  prg = getenv("LOCKPRG");
  if (prg && strcmp(prg, "builtin") && !access(prg, X_OK))
    {
# ifdef SYSV
      signal(SIGCLD, SIG_DFL);
# else /* SYSV */
      signal(SIGCHLD, SIG_DFL);
# endif /* SYSV */
      debug1("lockterminal: '%s' seems executable, execl it!\n", prg);
      if ((pid = fork()) == 0)
        {
          /* Child */
          setuid(real_uid);	/* this should be done already */
          setgid(real_gid);
          closeallfiles();	/* important: /etc/shadow may be open */
          execl(prg, "SCREEN-LOCK", NULL);
          exit(errno);
        }
      if (pid == -1)
        {
#ifdef NETHACK
          if (nethackflag)
            Msg(errno, "Cannot fork terminal - lock failed");
          else
#endif
          Msg(errno, "Cannot lock terminal - fork failed");
        }
      else
        {
#ifdef BSDWAIT
          union wait wstat;
#else
          int wstat;
#endif
          int wret;

#ifdef hpux
          signal(SIGCLD, SIG_DFL);
#endif
          errno = 0;
          while (((wret = wait((int *) &wstat)) != pid) ||
	         ((wret == -1) && (errno == EINTR))
	         )
	    errno = 0;
    
          if (errno)
	    {
	      perror("Lock");
	      sleep(2);
	    }
	  else if (WTERMSIG(wstat) != 0)
	    {
	      fprintf(stderr, "Lock: %s: Killed by signal: %d%s\n", prg,
		      WTERMSIG(wstat), WIFCORESIG(wstat) ? " (Core dumped)" : "");
	      sleep(2);
	    }
	  else if (WEXITSTATUS(wstat))
	    {
	      debug2("Lock: %s: return code %d\n", prg, WEXITSTATUS(wstat));
	    }
          else
	    printf(LockEnd);
        }
    }
  else
    {
      if (prg)
	{
          debug1("lockterminal: '%s' seems NOT executable, we use our builtin\n", prg);
	}
      else
	{
	  debug("lockterminal: using buitin.\n");
	}
      screen_builtin_lck();
    }
  /* reset signals */
  for (sig = 1; sig < NSIG; sig++)
    {
      if (sigs[sig] != (SIGTYPE(*) ()) - 1)
	signal(sig, sigs[sig]);
    }
}				/* LockTerminal */

/* -- original copyright by Luigi Cannelloni 1985 (luigi@faui70.UUCP) -- */
void
screen_builtin_lck()
{
  char fullname[100], *cp1, message[BUFSIZ];
  char c, *pass, mypass[9];
#ifdef SHADOWPW
  struct spwd *sss = NULL;
#endif
  int t;

#ifdef undef
  /* get password entry */
  if ((ppp = getpwuid(real_uid)) == NULL)
    {
      fprintf(stderr, "screen_builtin_lck: No passwd entry.\007\n");
      sleep(2);
      return;
    }
  if (!isatty(0))
    {
      fprintf(stderr, "screen_builtin_lck: Not a tty.\007\n");
      sleep(2);
      return;
    }
#endif
  pass = ppp->pw_passwd;
#ifdef SHADOWPW
realpw:
#endif
  for (t = 0; t < 13; t++)
    {
      c = pass[t];
      if (!(c == '.' || c == '/' ||
            (c >= '0' && c <= '9') || 
            (c >= 'a' && c <= 'z') || 
            (c >= 'A' && c <= 'Z'))) 
        break;
    }
  if (t < 13)
    {
      debug("builtin_lock: ppp->pw_passwd bad, has it a shadow?\n");
#ifdef SHADOWPW
      setspent(); /* rewind shadow file */
      if ((sss == NULL) && (sss = getspnam(ppp->pw_name)))
        {
          pass = sss->sp_pwdp;
          goto realpw;
        }
#endif /* SHADOWPW */
      if (pass = getpass("Key:   "))
        {
          strncpy(mypass, pass, 8);
          mypass[8] = 0;
          if (*mypass == 0)
            return;
          if (pass = getpass("Again: "))
            {
              if (strcmp(mypass, pass))
                {
                  fprintf(stderr, "Passwords don't match.\007\n");
                  sleep(2);
                  return;
                }
            }
        }
      if (pass == 0)
        {
          fprintf(stderr, "Getpass error.\007\n");
          sleep(2);
          return;
        }
      pass = 0;
    }

  debug("screen_builtin_lck looking in gcos field\n");
  strcpy(fullname, ppp->pw_gecos);
  if ((cp1 = index(fullname, ',')) != NULL)
    *cp1 = '\0';
  if ((cp1 = index(fullname, '&')) != NULL)
    {
      sprintf(cp1, "%s", ppp->pw_name);
      *cp1 = islower(*cp1) ? toupper(*cp1) : *cp1;
    }

  sprintf(message, "Screen used by %s <%s>.\nPassword:\007",
          fullname, ppp->pw_name);

  /* loop here to wait for correct password */
  for (;;)
    {
      debug("screen_builtin_lck awaiting password\n");
      if ((cp1 = getpass(message)) == NULL)
        {
          AttacherFinit(SIGARG);
          /* NOTREACHED */
        }
      if (pass)
        {
          if (!strcmp(crypt(cp1, pass), pass))
            break;
        }
      else
        {
          if (!strcmp(cp1, mypass))
            break;
        }
      debug("screen_builtin_lck: NO!!!!!\n");
    }
  debug("password ok.\n");
}

#endif	/* LOCK */

/*
 * Detach now has the following modes:
 *	D_DETACH	SIG_BYE		detach backend and exit attacher
 *	D_STOP		SIG_STOP	stop attacher (and detach backend)
 *	D_REMOTE	SIG_BYE		remote detach -- reattach to new attacher
 *	D_POWER 	SIG_POWER_BYE 	power detach -- attacher kills his parent
 *	D_REMOTE_POWER	SIG_POWER_BYE	remote power detach -- both
 *	D_LOCK		SIG_LOCK	lock the attacher
 * (jw)
 * we always remove our utmp slots. (even when "lock" or "stop")
 * Note: Take extra care here, we may be called by unterrupt!
 */
void
Detach(mode)
int mode;
{
  int sign = 0;
#ifdef UTMPOK
  register int n;
#endif

  if (Detached)
    return;
  debug1("Detach(%d)\n", mode);
  if (fore && status)
    RemoveStatus();
  signal(SIGHUP, SIG_IGN);
  SetTTY(0, &OldMode);
  FinitTerm();
  switch (mode)
    {
    case D_DETACH:
      printf("\n[detached]\n");
      sign = SIG_BYE;
      break;
#ifdef BSDJOBS
    case D_STOP:
      (void) fflush(stdout);
      sign = SIG_STOP;
      break;
#endif
#ifdef REMOTE_DETACH
    case D_REMOTE:
      printf("\n[remote detached]\n");
      sign = SIG_BYE;
      break;
#endif
#ifdef POW_DETACH
    case D_POWER:
      printf("\n[power detached]\n");
      if (PowDetachString) 
        printf("%s\n", PowDetachString);
      sign = SIG_POWER_BYE;
      break;
#ifdef REMOTE_DETACH
    case D_REMOTE_POWER:
      printf("\n[remote power detached]\n");
      if (PowDetachString) 
        printf("%s\n", PowDetachString);
      sign = SIG_POWER_BYE;
      break;
#endif
#endif
    case D_LOCK:
      ClearDisplay();
      sign = SIG_LOCK;
      /* tell attacher to lock terminal with a lockprg. */
      break;
    }
#ifdef UTMPOK
  for (n = WinList; n != -1; n = wtab[n]->WinLink)
    if (wtab[n]->slot != (slot_t) -1)
      {
	RemoveUtmp(wtab[n]);
        /*
	 * Set the slot to 0 to get the window
         * logged in again.
	 */
	wtab[n]->slot = (slot_t) 0;
      }
  RestoreLoginSlot();
#endif
  freetty();
  (void) chmod(SockPath, /* S_IFSOCK | */ 0600); /* Flag detached-ness */
    /*
     * tell father to father what to do. We do that after we
     * freed the tty, thus getty feels more comfortable on hpux
     * if it was a power detach.
     */
  Kill(AttacherPid, sign);
  debug2("Detach: Signal %d to Attacher(%d)!\n", sign, AttacherPid);
  if (mode != D_LOCK && mode != D_STOP)
    AttacherPid = 0;

  Detached = 1;
  Suspended = (mode == D_STOP) ? 1 : 0;
  if (fore)
    fore->active = 0;
  debug("Detach returns, we are successfully detached.\n");
}

void
Kill(pid, sig)
int pid, sig;
{
  if (pid < 2)
    return;
  (void) kill(pid, sig);
}

static int IsSymbol(e, s)
register char *e, *s;
{
  register char *p;
  register int n;

  for (p = e; *p && *p != '='; ++p)
    ;
  if (*p)
    {
      *p = '\0';
      n = strcmp(e, s);
      *p = '=';
      return n == 0;
    }
  return 0;
}

static void MakeNewEnv()
{
  register char **op, **np;
  static char buf[MAXSTR];

  for (op = environ; *op; ++op)
    ;
  NewEnv = np = (char **) malloc((unsigned) (op - environ + 6 + 1) * sizeof(char **));
  if (!NewEnv)
    Msg_nomem;
  if (strlen(SockName) > MAXSTR - 5)
    SockName = "?";
  sprintf(buf, "STY=%s", SockName);
  *np++ = buf;	                /* NewEnv[0] */
  *np++ = Term;	                /* NewEnv[1] */
#ifdef TIOCSWINSZ
  np += 2;	/* room for TERMCAP and WINDOW */
#else
  np += 4;	/* room for TERMCAP WINDOW LINES COLUMNS */
#endif

  for (op = environ; *op; ++op)
    {
      if (!IsSymbol(*op, "TERM") && !IsSymbol(*op, "TERMCAP")
	  && !IsSymbol(*op, "STY") && !IsSymbol(*op, "WINDOW")
	  && !IsSymbol(*op, "SCREENCAP")
#ifndef TIOCGWINSZ
	  && !IsSymbol(*op, "LINES") && !IsSymbol(*op, "COLUMNS")
#endif
	  )
	*np++ = *op;
    }
  *np = 0;
}

void
#ifdef USEVARARGS
/*VARARGS2*/
# if defined(__STDC__)
Msg(int err, char *fmt, ...)
# else
Msg(err, fmt, va_alist)
int err;
char *fmt;
va_dcl
# endif
{
  static va_list ap = 0;
#else
/*VARARGS2*/
Msg(err, fmt, p1, p2, p3, p4, p5, p6)
int err;
char *fmt;
unsigned long p1, p2, p3, p4, p5, p6;
{
#endif
  char buf[MAXPATH*2];
  char *p = buf;

  if (Detached)
    return;
#ifdef USEVARARGS
# if defined(__STDC__)
  va_start(ap, fmt);
# else
  va_start(ap);
# endif
  (void) vsprintf(p, fmt, ap);
  va_end(ap);
#else
  sprintf(p, fmt, p1, p2, p3, p4, p5, p6);
#endif
  if (err)
    {
      p += strlen(p);
      if (err > 0 && err < sys_nerr)
	sprintf(p, ": %s", sys_errlist[err]);
      else
	sprintf(p, ": Error %d", err);
    }
  if (HasWindow)
    {
      debug1("Msg('%s');\n", p);
      MakeStatus(buf);
    }
  else
    {
      printf("%s\r\n", buf);
      if (DeadlyMsg)
	{
          debug1("Msg('%s') screen is not up, exiting..\n", buf);
          Kill(AttacherPid, SIG_BYE);
          eexit(1);
	}
      else
	debug1("Harmless; Msg('%s');\n", buf);
    }
  DeadlyMsg = 1;
}

char *Filename(s)
char *s;
{
  register char *p;

  if (s == NULL) 
    return s;
  p = s + strlen(s) - 1;
  while (p >= s && *p != '/')
    --p;
  return ++p;
}

/*
 * '^' is allowed as an escape mechanism for control characters. jw.
 */
static char *MakeWinMsg(s, n)
register char *s;
int n;
{
  static char buf[MAXSTR];
  register char *p = buf;
  register int ctrl;

  ctrl = 0;
  for (; *s && p < buf + MAXSTR - 1; s++, p++)
    if (ctrl)
      {
        ctrl = 0;
        if (*s == '^' || *s < 64)
          *p = *s;
        else 
          *p = *s - 64;
      }
    else
      {
        switch (*s)
          {
          case '%':
    	    *p = n + '0';
	    break;
          case '~':
	    *p = BELL;
	    break;
	  case '^':
	    ctrl = 1;
	    *p-- = '^';
	    break;
          default:
	    *p = *s;
	    break;
          }
      }
  *p = '\0';
  return buf;
}

