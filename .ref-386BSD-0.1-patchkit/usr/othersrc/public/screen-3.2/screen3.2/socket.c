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
  static char rcs_id[] = "$Id: socket.c,v 1.2 92/02/03 02:28:17 jnweiger Exp $ FAU";
#endif

#include "config.h"
#if defined(MIPS) || defined(GOULD_NP1) || defined(B43)
extern int errno;
#endif
#include <sys/types.h>
#include <sys/stat.h>
#ifndef sgi
# include <sys/file.h>
#endif
#ifndef NAMEDPIPE
#include <sys/socket.h>
#endif
#include <fcntl.h>
#ifndef NAMEDPIPE
#include <sys/un.h>
#endif
#include <signal.h>
#ifndef M_XENIX
#include <sys/time.h>
#endif /* M_XENIX */
#ifdef DIRENT
# include <sys/param.h>
# include <dirent.h>
#else
# include <sys/dir.h>
# define dirent direct
#endif

#include "screen.h"

#ifdef USEVARARGS
# if defined(__STDC__)
#  include <stdarg.h>
# else
#  include <varargs.h>
# endif
#endif

#include "extern.h"

#if defined(_SEQUENT_) && !defined(NAMEDPIPE)
# define connect sconnect	/* _SEQUENT_ has braindamaged connect */
#endif

extern char *RcFileName, *extra_incap, *extra_outcap;
extern WinList, Detached, ServerSocket, real_uid, real_gid, eff_uid, eff_gid;
#ifdef BSDJOBS
extern int Suspended;
#endif
extern AttacherPid, dflag, rflag, lsflag, quietflag, wipeflag;
extern char HostName[];
extern struct mode OldMode, NewMode;
extern char display_tty[];
extern struct win *wtab[], *fore;
#ifdef NETHACK
extern nethackflag;
#endif

#ifdef PASSWORD
extern int CheckPassword;
extern char Password[];
#endif
#if defined(BSDJOBS) && !(defined(POSIX) || defined(SYSV))
extern DevTty;
#endif
extern char *getenv();

char SockPath[MAXPATH];
char *SockNamePtr, *SockName;

char *strdup(str)
const char *str;
{
  char *ret;

  if ((ret = (char *) malloc(strlen(str) + 1)) == 0)
    {
      Msg_nomem;
      return (0);
    }
  (void) strcpy(ret, str);
  return (ret);
}

int RecoverSocket()
{
  int s = 0, d;

  (void) unlink(SockPath);
  s = MakeServerSocket();
  if (s != ServerSocket)
    {
      debug2("Oh, Serversocket was %d, now %d, let's dup!\n", ServerSocket, s);
      d = dup2(s, ServerSocket);
      close(s);
      if (d != ServerSocket)
	{
	  debug2("Hmm, dup2() failed, Serversocket was %d, now %d, bye\n",
		 ServerSocket, d);
	  return 0;
	}
    }
  if (Detached)
    (void) chmod(SockPath, /* S_IFSOCK | */ 0600); /* Flag detached-ness */
  return 1;
}

/*
 * Socket mode 700 means we are Attached. 600 is detached.
 * We return how many sockets we found. If it was exactly one, we come
 * back with a SockPath set to it and open it in a fd pointed to by fdp.
 * If fdp == 0 we simply produce a list if all sockets.
 */
int FindSocket(how, fdp)
int how;
int *fdp;
{
  register int s, lasts = 0, found = 0, deadcount = 0, wipecount = 0;
  register int l = 0;
  register DIR *dirp;
  register struct dirent *dp;
  register char *Name;
  struct stat st;
  struct foundsock
    {
      char *name;
      int mode;
    } foundsock[100];	/* 100 is hopefully enough. */
  int foundsockcount = 0;

  /* User may or may not give us a (prefix) SockName. We want to search. */
  debug("FindSocket:\n");
  if (SockName)
    {
      debug1("We want to match '%s'\n", SockName);
      l = strlen(SockName);
#ifdef NAME_MAX
      if (l > NAME_MAX)
	l = NAME_MAX;
#endif
    }

#ifdef NFS_HACK
  setreuid(eff_uid, real_uid);
#endif
  debug1("FindSock searching... '%s'\n", SockPath);
  /*
   * this is a hack: SockName may point to Filename(Sockpath)...
   */
  found = *SockNamePtr;
  *SockNamePtr = '\0';
  if ((dirp = opendir(SockPath)) == NULL)
    {
      Msg(0, "Cannot opendir %s", SockPath);
      /* NOTREACHED */
    }
  *SockNamePtr = found;
  found = 0;
  while ((dp = readdir(dirp)) != NULL)
    {
      Name = dp->d_name;
      /* 
       * there may be a file ".termcap" here. 
       * Ignore it just like "." and "..". 
       */
      if (Name[0] == '.')
	continue;
      debug2("Attach found: '%s', needed '%s'\n", Name, SockName);
      if (SockName && l)
	{
	  register char *n = Name;

	  /*
	   * The SockNames "hf", "ttyhf", "1", "12345.tty", "12345.ttyhf.medusa"
	   * all match the Name "12345.ttyhf.medusa".
	   */

	  if ((*SockName <= '0' || *SockName > '9') && (*n > '0' && *n <= '9'))
	    {
	      while (*++n)
		if (*n == '.')
		  {
		    n++;
		    break;
		  }
	      if (strncmp("tty", SockName, 3) && !strncmp("tty", n, 3))
		n += 3;
	    }
	  if (strncmp(n, SockName, l))
	    {
	      debug3("strncmp('%s', '%s', %d)\n", n, SockName, l);
	      continue;
	    }
	}
      /*
       * ATTENTION! MakeClientSocket adds SockName to SockPath! 
       * Anyway, we need it earlier.
       */
      strcpy(SockNamePtr, Name);
      if (stat(SockPath, &st))
	continue;
      if (st.st_uid != real_uid)
	continue;
      foundsock[foundsockcount].name = strdup(Name);
      foundsock[foundsockcount].mode = s = st.st_mode & 0777;
      debug2("FindSocket: %s has mode %04o...\n", Name, s);
      if (s == 0700 || s == 0600)
	{
	  /*
	   * We try to connect through the socket. If successfull, 
	   * thats o.k. Otherwise we record that mode as -1.
	   * MakeClientSocket() must be careful not to block forever.
	   */
	  if ((s = MakeClientSocket(0, Name)) == -1)
	    { 
	      foundsock[foundsockcount].mode = -1;
	      deadcount++;
	    }
	  else
	    close(s);
	}
      if (++foundsockcount >= 100)
	break;
    }
  closedir(dirp);
#ifdef NFS_HACK
  setreuid(real_uid, eff_uid);
#endif

  if (wipeflag)
    {
      for (s = 0; s < foundsockcount; s++)
	{
	  if (foundsock[s].mode == -1)
	    {
              strcpy(SockNamePtr, foundsock[s].name);
	      debug1("wiping '%d'\n", SockPath);
	      if (unlink(SockPath) == 0)
	        {
		  foundsock[s].mode = -2;
	          wipecount++;
		}
	    }
	}
    }
  for (s = 0; s < foundsockcount; s++)
    if ((foundsock[s].mode) == (dflag ? 0700 : 0600)) 
      {
	found++;
	lasts = s;
      }
  if (quietflag && (lsflag || (found != 1 && rflag != 2)))
    eexit(10 + found);
  debug2("attach: found=%d, foundsockcount=%d\n", found, foundsockcount);
  if (found == 1 && lsflag == 0)
    {
      if ((lasts = MakeClientSocket(0, SockName = foundsock[lasts].name)) == -1)
        found = 0;
    }
  else if (!quietflag && foundsockcount > 0)
    {
      switch (found)
        {
        case 0:
          if (lsflag)
	    {
#ifdef NETHACK
              if (nethackflag)
	        printf("Your inventory:\n");
	      else
#endif
	      printf((foundsockcount > 1) ?
	             "There are screens on:\n" : "There is a screen on:\n");
	    }
          else
	    {
#ifdef NETHACK
              if (nethackflag)
	        printf("Nothing fitting exists in the game:\n");
	      else
#endif
	      printf((foundsockcount > 1) ?
	             "There are screens on:\n" : "There is a screen on:\n");
	    }
          break;
        case 1:
#ifdef NETHACK
          if (nethackflag)
            printf((foundsockcount > 1) ?
                   "Prove thyself worthy or perish:\n" : 
                   "You see here a good looking screen:\n");
          else
#endif
          printf((foundsockcount > 1) ? 
                 "There are several screens on:\n" :
                 "There is a possible screen on:\n");
          break;
        default:
#ifdef NETHACK
          if (nethackflag)
            printf((foundsockcount > 1) ? 
                   "You may whish for a screen, what do you want?\n" : 
                   "You see here a screen:\n");
          else
#endif
          printf((foundsockcount > 1) ?
                 "There are several screens on:\n" : "There is a screen on:\n");
          break;
        }
      for (s = 0; s < foundsockcount; s++)
	{
	  switch (foundsock[s].mode)
	    {
	    case 0700:
	      printf("\t%s\t(Attached)\n", foundsock[s].name);
	      break;
	    case 0600:
	      printf("\t%s\t(Detached)\n", foundsock[s].name);
	      break;
	    case -1:
#if defined(__STDC__) || defined(_AIX)
	      printf("\t%s\t(Dead ??\?)\n", foundsock[s].name);
#else
	      printf("\t%s\t(Dead ???)\n", foundsock[s].name);
#endif
	      break;
	    case -2:
	      printf("\t%s\t(Removed)\n", foundsock[s].name);
	      break;
	    default:
	      printf("\t%s\t(Wrong mode)\n", foundsock[s].name);
	      break;
	    }
	}
    }
  if (deadcount && !quietflag)
    {
      if (wipeflag)
        {
#ifdef NETHACK
          if (nethackflag)
            printf("You hear%s distant explosion%s.\n",
	       (deadcount > 1) ? "" : " a", (deadcount > 1) ? "s" : "");
          else
#endif
          printf("%d Socket%s wiped out.\n", deadcount, (deadcount > 1)?"s":"");
	}
      else
	{
#ifdef NETHACK
          if (nethackflag)
            printf("The dead screen%s touch%s you. Try 'screen -wipe'.\n",
	       (deadcount > 1) ? "s" : "", (deadcount > 1) ? "" : "es");
          else
#endif
          printf("Remove dead Sockets with 'screen -wipe'.\n");
	}
    }

  for (s = 0; s < foundsockcount; s++)
    Free(foundsock[s].name);
  if (found == 1 && fdp)
    *fdp = lasts;
  if (fdp)
    return found;
  return foundsockcount - wipecount;
}

#ifdef NAMEDPIPE

int
MakeServerSocket()
{
  register int s;
  struct stat st;

  strcpy(SockNamePtr, SockName);
#ifdef NAME_MAX
  if (strlen(SockNamePtr) > NAME_MAX)
    {
      debug2("MakeClientSocket: '%s' truncated to %d chars\n",
	     SockNamePtr, NAME_MAX);
      SockNamePtr[NAME_MAX] = '\0';
    }
#endif

  if ((s = open(SockPath, O_WRONLY | O_NDELAY)) >= 0)
    {
      debug("huii, my fifo already exists??\n");
      if (quietflag)
	{
	  Kill(AttacherPid, SIG_BYE);
	  eexit(11);
	}
      printf("There is already a screen running on %s.\n",
	     Filename(SockPath));
      if (stat(SockPath, &st) == -1)
	Msg(errno, "stat");
      if (st.st_uid != real_uid)
	Msg(0, "Unfortunatelly you are not its owner.");
      if ((st.st_mode & 0700) == 0600)
	Msg(0, "To resume it, use \"screen -r\"");
      else
	Msg(0, "It is not detached.");
      /* NOTREACHED */
    }
  (void) unlink(SockPath);
  if (UserContext() > 0)
    {
#if defined(_POSIX_SOURCE) && defined(ISC)
      if (mknod(SockPath, 0010700, 0))
#else
      if (mknod(SockPath, S_IFIFO | S_IEXEC | S_IWRITE | S_IREAD, 0))
#endif
	UserReturn(0);
      UserReturn(1);
    }
  if (UserStatus() <= 0)
    Msg(0, "mknod fifo %s failed", SockPath);
  /*
   * MUST be RDWR because otherwise we will get EOF's if
   * nobody has opened the pipe for writing
   */
  if ((s = secopen(SockPath, O_RDWR | O_NDELAY, 0)) < 0)
    Msg(errno, "open fifo %s", SockPath);
  return s;
}


int
MakeClientSocket(err, name)
int err;
char *name;
{
  register int s = 0;

  strcpy(SockNamePtr, name);
#ifdef NAME_MAX
  if (strlen(SockNamePtr) > NAME_MAX)
    {
      debug2("MakeClientSocket: '%s' truncated to %d chars\n",
	     SockNamePtr, NAME_MAX);
      SockNamePtr[NAME_MAX] = '\0';
    }
#endif
  
  if ((s = secopen(SockPath, O_WRONLY | O_NDELAY, 0)) >= 0)
    {
      (void) fcntl(s, F_SETFL, 0);
      return s;
    }
  if (err)
    {
      Msg(errno, "open: %s (but continuing...)", SockPath);
      debug1("MakeClientSocket() open %s failed\n", SockPath);
    }
  else
    {
      debug1("MakeClientSocket() open %s failed\n", SockPath);
    }
  return -1;
}

#else	/* NAMEDPIPE */

int
MakeServerSocket()
{
  register int s;
  struct sockaddr_un a;
  struct stat st;

  if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
    Msg(errno, "socket");
  a.sun_family = AF_UNIX;
  strcpy(SockNamePtr, SockName);
#ifdef NAME_MAX
  if (strlen(SockNamePtr) > NAME_MAX)
    {
      debug2("MakeServerSocket: '%s' truncated to %d chars\n",
	     SockNamePtr, NAME_MAX);
      SockNamePtr[NAME_MAX] = '\0';
    }
#endif

  strcpy(a.sun_path, SockPath);
  if (connect(s, (struct sockaddr *) & a, strlen(SockPath) + 2) != -1)
    {
      debug("oooooh! socket already is alive!\n");
      if (quietflag)
	{ 
	  Kill(AttacherPid, SIG_BYE);
	  /* 
	   * oh, well. nobody receives that return code. papa 
	   * dies by signal.
	   */
	  eexit(11);
	}
      printf("There is already a screen running on %s.\n",
	     Filename(SockPath));
      if (stat(SockPath, &st) == -1)
	Msg(errno, "stat");
      if (st.st_uid != real_uid)
	Msg(0, "Unfortunatelly you are not its owner.");
      if ((st.st_mode & 0700) == 0600)
	Msg(0, "To resume it, use \"screen -r\"");
      else
	Msg(0, "It is not detached.");
      /* NOTREACHED */
    }
  (void) unlink(SockPath);
#ifndef NOREUID
  setreuid(eff_uid, real_uid);
  setregid(eff_gid, real_gid);
#endif
  if (bind(s, (struct sockaddr *) & a, strlen(SockPath) + 2) == -1)
    Msg(errno, "bind");
  (void) chmod(SockPath, /* S_IFSOCK | */ 0700);
#ifdef NOREUID
  chown(SockPath, real_uid, real_gid);
#else
  setreuid(real_uid, eff_uid);
  setregid(real_gid, eff_gid);
#endif
  if (listen(s, 5) == -1)
    Msg(errno, "listen");
#ifdef F_SETOWN
  fcntl(s, F_SETOWN, getpid());
  debug1("Serversocket owned by %d\n", fcntl(s, F_GETOWN, 0));
#endif /* F_SETOWN */
  return s;
}

int
MakeClientSocket(err, name)
int err;
char *name;
{
  register int s;
  struct sockaddr_un a;

  if ((s = socket(AF_UNIX, SOCK_STREAM, 0)) == -1)
    Msg(errno, "socket");
  a.sun_family = AF_UNIX;
  strcpy(SockNamePtr, name);
#ifdef NAME_MAX
  if (strlen(SockNamePtr) > NAME_MAX)
    {
      debug2("MakeClientSocket: '%s' truncated to %d chars\n",
	     SockNamePtr, NAME_MAX);
      SockNamePtr[NAME_MAX] = '\0';
    }
#endif

  strcpy(a.sun_path, SockPath);
#ifndef NOREUID
  setreuid(eff_uid, real_uid);
  setregid(eff_gid, real_gid);
#else
  if (access(SockPath, W_OK))
    {
      if (err)
	Msg(errno, "%s", SockPath);
      else
	debug2("MakeClientSocket: access(%s): %d.\n", SockPath, errno);
      close(s);
      return -1;
    }
#endif
  if (connect(s, (struct sockaddr *) & a, strlen(SockPath) + 2) == -1)
    {
      if (err)
	Msg(errno, "%s: connect", SockPath);
      else
	debug("MakeClientSocket: connect failed.\n");
      close(s);
      s = -1;
    }
#ifndef NOREUID
  setreuid(real_uid, eff_uid);
  setregid(real_gid, eff_gid);
#endif
  return s;
}
#endif


void
SendCreateMsg(s, ac, av, aflag, flowflag, lflag, histheight, sterm)
int s, ac, aflag, flowflag, lflag, histheight;
char **av;
char *sterm;
{
  struct msg m;
  register char *p;
  register int len, n;

  debug1("SendCreateMsg() to '%s'\n", SockPath);
  m.type = MSG_CREATE;
  p = m.m.create.line;
  for (n = 0; ac > 0 && n < MAXARGS - 1; ++av, --ac, ++n)
    {
      len = strlen(*av) + 1;
      if (p + len >= m.m.create.line + MAXPATH - 1)
	break;
      strcpy(p, *av);
      p += len;
    }
  if (!ac && *av && p + strlen(*av) + 1 < m.m.create.line + MAXPATH)
    strcpy(p, *av);
  else
    *p = '\0';
  m.m.create.nargs = n;
  m.m.create.aflag = aflag;
  m.m.create.flowflag = flowflag;
  m.m.create.lflag = lflag;
  m.m.create.hheight = histheight;
#ifdef SYSV
  if (getcwd(m.m.create.dir, sizeof(m.m.create.dir)) == 0)
#else
    if (getwd(m.m.create.dir) == 0)
#endif
      Msg(0, "%s", m.m.create.dir);
  strncpy(m.m.create.screenterm, sterm, 19);
  m.m.create.screenterm[19] = '\0';
  debug1("SendCreateMsg writing '%s'\n", m.m.create.line);
  if (write(s, (char *) &m, sizeof m) != sizeof m)
    Msg(errno, "write");
}

void
#ifdef USEVARARGS
/*VARARGS1*/
# if defined(__STDC__)
SendErrorMsg(char *fmt, ...)
# else
SendErrorMsg(fmt, va_alist)
char *fmt;
va_dcl
#endif
{ /* } */
  static va_list ap = 0;
#else
/*VARARGS1*/
SendErrorMsg(fmt, p1, p2, p3, p4, p5, p6)
char *fmt;
unsigned long p1, p2, p3, p4, p5, p6;
{
#endif
  register int s;
  struct msg m;

  s = MakeClientSocket(1, SockName);
  debug1("SendErrorMsg() to '%s'\n", SockPath);
  m.type = MSG_ERROR;
#ifdef USEVARARGS
# if defined(__STDC__)
  va_start(ap, fmt);
# else
  va_start(ap);
# endif
  (void) vsprintf(m.m.message, fmt, ap);
  va_end(ap);
#else
  sprintf(m.m.message, fmt, p1, p2, p3, p4, p5, p6);
#endif
  debug1("SendErrorMsg writing '%s'\n", m.m.message);
  (void) write(s, (char *) &m, sizeof m);
  close(s);
  sleep(2);
}

#ifdef PASSWORD
static int CheckPasswd(pwd, pid, tty)
int pid;
char *pwd, *tty;
{
  if (CheckPassword && 
      strcmp(crypt(pwd, (strlen(Password) > 1) ? Password : "JW"),
	     Password))
    {
      if (*pwd)
	{
#ifdef NETHACK
          if (nethackflag)
	    Msg(0, "'%s' tries to explode in the sky, but fails. (%s)", tty, pwd);
          else
#endif
	  Msg(0, "Illegal reattach attempt from terminal %s, \"%s\"", tty, pwd);
	}
      debug1("CheckPass() wrong password kill(%d, SIG_PW_FAIL)\n", pid);
      Kill(pid, SIG_PW_FAIL);
      return 0;
    }
  debug1("CheckPass() from %d happy\n", pid);
  Kill(pid, SIG_PW_OK);
  return 1;
}
#endif	/* PASSWORD */

static void ExecCreate(mp)
struct msg *mp;
{
  char *args[MAXARGS];
  register int n;
  register char **pp = args, *p = mp->m.create.line;

  for (n = mp->m.create.nargs; n > 0; --n)
    {
      *pp++ = p;
      p += strlen(p) + 1;
    }
  *pp = 0;
  if (!*p)
    p = 0;
  MakeWindow(p, args, mp->m.create.aflag, mp->m.create.flowflag,
		      0, mp->m.create.dir, mp->m.create.lflag,
		      mp->m.create.hheight, mp->m.create.screenterm);
}

static int
CheckPid(pid)
int pid;
{
  if (pid < 2)
    return(-1);
  if (eff_uid == real_uid)
    return kill(pid, 0);
  if (UserContext() == 1)
    {
      UserReturn(kill(pid, 0));
    }
  return UserStatus();
}

void
ReceiveMsg(s)
int s;
{
  int left, len, i;
  static struct msg m;
  char *p;
  static char lbuf[20], cbuf[20]; /* static for later putenv()'s */
#ifdef NAMEDPIPE
  /*
   * we may be called if there are no pending messages, so we will have to
   * block on first read.
   */
  debug("Ha, there was someone knocking on my fifo??\n");
  if (fcntl(s, F_SETFL, 0) == -1)
    {
      Msg(errno, "fcntl no O_NDELAY");
      exit(1);
    }
  p = (char *) &m;
  left = sizeof(m);
  while (left > 0 && (len = read(s, p, left)) > 0)
    {
      /*		if (p == (char *)&m)
       *		{	if (fcntl(s, F_SETFL, O_NDELAY) == -1)
       *			{	Msg(errno, "fcntl O_NDELAY !");
       *				return;
       *			}
       *		}
       */
      p += len;
      left -= len;
    }
# ifdef DEBUG
  if (len == 0)
    debug("ReceiveMsg: Yucc! Got an EOF !!\n");
# endif
#else
  register int ns;
  struct sockaddr_un a;

  len = sizeof(struct sockaddr_un);
  debug("Ha, there was someone knocking on my socket??\n");
  if ((ns = accept(s, (struct sockaddr *) & a, &len)) == -1)
    {
      Msg(errno, "accept");
      return;
    }
  p = (char *) &m;
  left = sizeof m;
  while (left > 0 && (len = read(ns, p, left)) > 0)
    {
      p += len;
      left -= len;
    }
  close(ns);
#endif				/* NAMEDPIPE */

  if (len == -1)
    Msg(errno, "read");
  if (left > 0)
    return;
  debug1("RecMsg: type %d !, ", m.type);
  switch (m.type)
    {
    case MSG_WINCH:
      /* Already processed in screen.c */
#ifdef notdef
      CheckScreenSize(1); /* Change fore */
#endif
      break;
    case MSG_CREATE:
      if (!Detached)
	ExecCreate(&m);
      break;
    case MSG_CONT:
      debug3("RecMsg: apid=%d,was %d, Detached=%d\n", m.m.attach.apid,
             AttacherPid, Detached);

#ifdef SECURE_MSG_CONT
      if (m.m.attach.apid != AttacherPid || !Detached)
	/*
	 * now we realize, that there is something strange happening.
	 * First, there was some idiot who said "CONT" and thought
	 * "ATTACH". This is, when we have (AttacherPid == 0). Harmless.
	 * Let him in. Second, we are waiting for a certain AttacherPid,
	 * and not for "any" attacher. This is when (AttacherPid != 0).
	 * But Darwin's rule says "survival of the fittest", thus: Look
	 * weather "our" AttacherPid is still alive and well, and let the
	 * new one only in, if the old Attacher has been killed. jw.
	 */
	if (AttacherPid != 0 && kill(AttacherPid, 0) == 0)
	  break;		/* Intruder Alert */
	else
#endif				/* SECURE_MSG_CONT */
	  AttacherPid = 0;	/* we dont want to kill ourselves
				 * later. jw. */
      /* FALLTHROUGH */
    case MSG_ATTACH:
      if (CheckPid(m.m.attach.apid))
	{
	  debug1("Attach attempt with bad pid(%d)\n", m.m.attach.apid);
	  Msg(0, "Attach attempt with bad pid(%d) !", m.m.attach.apid);
          break;
	}
#ifdef PASSWORD
      if (!CheckPasswd(m.m.attach.password, m.m.attach.apid, m.m.attach.tty))
	{
	  debug3("RcvMsg:Checkpass(%s,%d,%s) failed\n",
		 m.m.attach.password, m.m.attach.apid, m.m.attach.tty);
	  break;
	}
#endif				/* PASSWORD */
      if (!Detached)
	{
	  debug("RecMsg: hey, why you disturb, we are not detached. hangup!\n");
	  Kill(m.m.attach.apid, SIG_BYE);
	  Msg(0, "Attach msg ignored: We are not detached.");
	  break;
	}
      if ((i = secopen(m.m.attach.tty, O_RDWR | O_NDELAY, 0)) < 0)
	{
	  debug1("ALERT: Cannot open %s!\n", m.m.attach.tty);
#ifdef NETHACK
          if (nethackflag)
	    Msg(errno, 
	        "You can't open (%s). Perhaps there's a Monster behind it.",
	        m.m.attach.tty);
          else
#endif
	  Msg(errno, "Attach: Could not open %s", m.m.attach.tty);
	  Kill(m.m.attach.apid, SIG_BYE);
	  break;
	}
      errno = 0;
      if (i)
	{
	  debug("PANIC: open did not return fd 0!\n");
	  close(0);
	  dup(i);
	  close(i);
	  close(1);
	  close(2);
	}
      debug2("RecMsg: apid %d is o.k. and we just opened '%s'\n", m.m.attach.apid, m.m.attach.tty);

#ifdef hpux_and_it_still_does_not_work
      {
	int pgrp, zero = 0;

	if (ioctl(0, TIOCGPGRP, &pgrp) == -1)
	  debug1("tcgetpgrp: %d\n", errno);	/* save old pgrp from tty */

	if (ioctl(0, TIOCSPGRP, &zero) == -1)
	  debug1("tcsetpgrp: %d\n", errno);	/* detach tty from proc grp */
#ifdef hpux
	setpgrp();
#else
	if (setpgrp(0, getpid()) == -1)
	  debug1("setpgrp: %d\n", errno);	/* make me proc group leader */
#endif

	close(0);	/* reopen tty, now we should get it as our tty */
	(void)secopen(m.m.attach.tty, O_RDWR | O_NDELAY, 0);	/* */

#ifdef hpux
	setpgrp2(0, pgrp);
#else
	setpgrp(0, pgrp);
#endif
	ioctl(0, TIOCSPGRP, &pgrp);
      }
#endif  /* hpux_and_it_still_does_not_work */

      (void) dup(0);
      (void) dup(0);
      GetTTY(0, &OldMode);
#if defined(BSDJOBS) && !(defined(POSIX) || defined(SYSV))
      if ((DevTty = open("/dev/tty", O_RDWR | O_NDELAY)) == -1)
	{
	  debug1("DevTty not opened: %d.\n", errno);
	  Msg(errno, "/dev/tty");
	}
#endif
      signal(SIGHUP, SigHup);
#ifdef BSDJOBS
      if (Suspended && m.type == MSG_ATTACH)
	if (kill(AttacherPid, SIG_BYE) == 0)
	  kill(AttacherPid, SIGCONT);
#endif

      AttacherPid = m.m.attach.apid;
      strcpy(display_tty, m.m.attach.tty);
#ifdef UTMPOK
      /*
       * we set the Utmp slots again, if we were detached normally
       * and if we were detached by ^Z.
       */
      ReInitUtmp();
      for (i = WinList; i != -1; i = wtab[i]->WinLink)
	if (wtab[i]->slot != (slot_t) -1)
	  SetUtmp(wtab[i], i);
#endif
      (void) chmod(SockPath, /* S_IFSOCK | */ 0700);
      Detached = 0;
#ifdef BSDJOBS
      Suspended = 0;
#endif
      /*
       * we like to have always the valid termcap, even if we are
       * reattached from a different tty type. thus attacher had to
       * tell us, what TERM we have. The attacher has correct env
       * setting. we (the backend may be wrong).
       */
      if (*m.m.attach.envterm)
	{
#if defined(pyr) || defined(xelos) || defined(sequent)
	  /*
	   * Kludge for systems with braindamaged termcap routines,
	   * which evaluate $TERMCAP, regardless weather it describes
	   * the correct terminal type or not.
	   */
	  if (strcmp(getenv("TERM"), m.m.attach.envterm + 5))
	    {
	      debug("a different terminal, so unsetenv(TERMCAP)");
	      unsetenv("TERMCAP");
	    }
#endif
	
#if !defined(sequent) && !defined(MIPS)
	  putenv(m.m.attach.envterm);
#else
	  setenv("TERM", m.m.attach.envterm + 5, 1);
#endif
	}
#if !defined(sequent) && !defined(MIPS)
      sprintf(lbuf, "LINES=%d", m.m.attach.lines);
      if (m.m.attach.lines > 0 || getenv("LINES"))
        putenv(lbuf);
      sprintf(cbuf, "COLUMNS=%d", m.m.attach.columns);
      if (m.m.attach.columns > 0 || getenv("COLUMNS"))
        putenv(cbuf);
#else
      sprintf(lbuf, "%d", m.m.attach.lines);
      if (m.m.attach.lines > 0 || getenv("LINES"))
        setenv("LINES", lbuf, 1);
      sprintf(cbuf, "%d", m.m.attach.columns);
      if (m.m.attach.columns > 0 || getenv("COLUMNS"))
        setenv("COLUMNS", cbuf, 1);
#endif
      
      /*
       * We reboot our Terminal Emulator. Forget all we knew about
       * the old terminal, reread the termcap entries in .screenrc
       * (and nothing more from .screenrc is read. Mainly because
       * I did not check, weather a full reinit is save. jw) 
       * and /etc/screenrc, and initialise anew.
       */
      if (extra_outcap)
	Free(extra_outcap);
      if (extra_incap)
	Free(extra_incap);
#ifdef ETCSCREENRC
      if ((p = getenv("SYSSCREENRC")) == NULL)
	StartRc(ETCSCREENRC);
      else
	StartRc(p);
#endif
      StartRc(RcFileName);
      InitTermcap();
      InitTerm(m.m.attach.adaptflag);
      SetMode(&OldMode, &NewMode);
      SetTTY(0, &NewMode);
      fore->active = 1;
      Activate(0);
      debug("activated...\n");
      break;
    case MSG_ERROR:
      Msg(0, "%s", m.m.message);
      break;
    case MSG_HANGUP:
#ifdef REMOTE_DETACH
    case MSG_DETACH:
# ifdef POW_DETACH
    case MSG_POW_DETACH:
# endif				/* POW_DETACH */
#endif
#ifdef REMOTE_DETACH
# ifdef POW_DETACH
      if (m.type == MSG_POW_DETACH)
	Detach(D_REMOTE_POWER);
      else
# endif				/* POW_DETACH */
      if (m.type == MSG_DETACH)
	Detach(D_REMOTE);
      else
#endif
      SigHup(SIGARG);
      break;
    default:
      Msg(0, "Invalid message (type %d).", m.type);
    }
}

#if defined(_SEQUENT_) && !defined(NAMEDPIPE)
#undef connect

sconnect(s, sapp, len)
int s, len;
struct sockaddr *sapp;
{
  register struct sockaddr_un *sap;
  struct stat st;
  int x;

  sap = (struct sockaddr_un *)sapp;
  if (stat(sap->sun_path,&st))
    return -1;
  chmod(sap->sun_path, 0);
  x = connect(s, (struct sockaddr *) sap, len);
  chmod(sap->sun_path, st.st_mode);
  return x;
}
#endif
