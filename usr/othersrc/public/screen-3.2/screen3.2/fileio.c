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
  static char rcs_id[] = "$Id: fileio.c,v 1.2 92/02/03 02:27:42 jnweiger Exp $ FAU";
#endif

#if defined(pyr) || defined(MIPS) || defined(GOULD_NP1) || defined(B43)
extern int errno;
#endif
#include <sys/types.h>
#ifndef sgi
# include <sys/file.h>
#endif /* sgi */
#include <sys/stat.h>
#include <fcntl.h>

#ifdef BSDI
# include <sys/signal.h>
#endif /* BSDI */

#include "config.h"
#include "screen.h"
#include "extern.h"

#ifdef _SEQUENT_
# define UTHOST		/* _SEQUENT_ has got ut_find_host() */
#endif

#ifndef GETUTENT
# ifdef GETTTYENT
#  include <ttyent.h>
# else
struct ttyent
{
  char *ty_name;
};
static char *tt, *ttnext;
static char ttys[] = "/etc/ttys";
# endif
#endif

#ifdef LOADAV
# ifndef NeXT
#  include <nlist.h>

static char KmemName[] = "/dev/kmem";
#  if defined(_SEQUENT_) || defined(MIPS) || defined(SVR4) || defined(ISC) || defined (sgi)
static char UnixName[] = "/unix";
#  else
#   ifdef sequent
static char UnixName[] = "/dynix";
#   else
#    ifdef hpux
static char UnixName[] = "/hp-ux";
#    else
#     ifdef xelos
static char UnixName[] = "/xelos";
#     else
static char UnixName[] = "/vmunix";
#     endif /* xelos */
#    endif /* hpux */
#   endif /* sequent */
#  endif /* _SEQUENT_ ... */

#  ifdef alliant
static char AvenrunSym[] = "_Loadavg";
#  else
#   if defined(hpux) || defined(_SEQUENT_) || defined(SVR4) || defined(ISC) || defined(sgi)
static char AvenrunSym[] = "avenrun";
#   else
static char AvenrunSym[] = "_avenrun";
#   endif
#  endif /* alliant */
static struct nlist nl[2];
int avenrun;
static kmemf;
#  ifdef LOADAV_3LONGS
long loadav[3];
#  else
#   ifdef LOADAV_4LONGS
long loadav[4];
#   else
double loadav[3];
#   endif
#  endif
# else /* NeXT */
#  include <mach.h>
kern_return_t error;
host_t host;
unsigned int info_count;
struct processor_set_basic_info info;
processor_set_t default_set;
float loadav;
int avenrun;
# endif /* NeXT */
#endif /* LOADAV */

#if defined(UTMPOK) && defined(GETUTENT) && !defined(SVR4)
# if defined(hpux) /* cruel hpux release 8.0 */
#  define pututline _pututline
# endif /* hpux */
extern struct utmp *getutline(), *pututline();
# if defined(_SEQUENT_)
extern struct utmp *ut_add_user(), *ut_delete_user();
extern char *ut_find_host();
# endif
#endif
#ifdef NETHACK
extern nethackflag;
#endif
int hardcopy_append = 0;
int all_norefresh = 0;

extern char *RcFileName, *home, *extra_incap, *extra_outcap;
extern char *BellString, *ActivityString, *ShellProg, *ShellArgs[];
extern char *BufferFile, *PowDetachString, *VisualBellString;
extern int VBellWait, MsgWait, MsgMinWait;
extern struct key ktab[];
extern char Esc, MetaEsc;
extern char *shellaka, SockPath[], *SockNamePtr, *LoginName;
extern int loginflag, allflag, TtyMode, auto_detach;
extern int iflag, rflag, dflag;
extern int default_flow, wrap;
extern HS, termcapHS, use_hardstatus, visual_bell, default_monitor;
extern int default_histheight;
extern int default_startup;
extern int slowpaste;
extern DeadlyMsg, HasWindow;
extern ForeNum, screenwidth, screenheight;
extern char display_tty[];
extern struct win *fore;
extern char screenterm[];
extern int join_with_cr;
extern struct mode OldMode, NewMode;
extern int HasWindow;
extern char mark_key_tab[];
extern int real_uid, eff_uid;
extern int real_gid, eff_gid;

#ifdef PASSWORD
int CheckPassword;
char Password[20];
#endif

#ifdef COPY_PASTE
extern char *copybuffer;
extern copylen;
#endif

static char *CatExtra __P((char *, char *));
static char **SaveArgs __P((int, char **));
static int Parse __P((char *, char *[]));
static char *ParseChar __P((char *, char *));
static void ParseNum __P((int, char *[], int*));
static void ParseOnOff __P((int, char *[], int*));
static void ParseSaveStr __P((int, char *[], char **, char *));
static int IsNum __P((char *, int));
static int IsNumColon __P((char *, int, char *, int));
static slot_t TtyNameSlot __P((char *));

#if !defined(GETTTYENT) && !defined(GETUTENT)
static void setttyent __P((void));
static struct ttyent *getttyent __P((void));
#endif

/*
 * XXX: system
 */
extern time_t time __P((time_t *));
#if !defined(BSDI) && !defined(SVR4)
extern char *getpass __P((char *));
#endif /* !BSDI && !SVR4 */
#if defined(LOADAV) && !defined(NeXT) && !defined(NLIST_DECLARED)
extern int nlist __P((char *, struct nlist *));
#endif

char *KeyNames[] = 
{
  "screen",
  "select0", "select1", "select2", "select3", "select4",
  "select5", "select6", "select7", "select8", "select9",
  "aka", "clear", "colon", "copy", "detach", "flow",
  "hardcopy", "help", "histnext", "history", "info", "kill", "lastmsg",
  "license",
  "lockscreen", "log", "login", "monitor", "next", "other", "paste",
  "pow_detach", "prev", "quit", "readbuf", "redisplay", "removebuf",
  "reset", "set", "shell", "suspend", "termcap", "time", "vbell",
  "version", "width", "windows", "wrap", "writebuf", "xoff", "xon",
  0,
};


/* Must be in alpha order !!! */

char *RCNames[] =
{
  "activity", "all", "autodetach", "bell", "bind", "bufferfile", "chdir",
  "crlf", "echo", "escape", "flow", "hardcopy_append", "hardstatus", "login", 
  "markkeys", "mode", "monitor", "msgminwait", "msgwait", "nethack", "password",
  "pow_detach_msg", "redraw", "refresh", "screen", "scrollback", "shell", 
  "shellaka", "sleep", "slowpaste", "startup_message", "term", "termcap",
  "terminfo", "vbell", "vbell_msg", "vbellwait", "visualbell",
  "visualbell_msg", "wrap",
};

enum RCcases
{
  RC_ACTIVITY,
  RC_ALL,
  RC_AUTODETACH,
  RC_BELL,
  RC_BIND,
  RC_BUFFERFILE,
  RC_CHDIR,
  RC_CRLF,
  RC_ECHO,
  RC_ESCAPE,
  RC_FLOW,
  RC_HARDCOPY_APP,
  RC_HARDSTATUS,
  RC_LOGIN,
  RC_MARKKEYS,
  RC_MODE,
  RC_MONITOR,
  RC_MSGMINWAIT,
  RC_MSGWAIT,
  RC_NETHACK,
  RC_PASSWORD,
  RC_POW_DETACH_MSG,
  RC_REDRAW,
  RC_REFRESH,
  RC_SCREEN,
  RC_SCROLLBACK,
  RC_SHELL,
  RC_SHELLAKA,
  RC_SLEEP,
  RC_SLOWPASTE,
  RC_STARTUP_MESSAGE,
  RC_TERM,
  RC_TERMCAP,
  RC_TERMINFO,
  RC_VBELL,
  RC_VBELL_MSG,
  RC_VBELLWAIT,
  RC_VISUALBELL,
  RC_VISUALBELL_MSG,
  RC_WRAP,
  RC_RCEND
};

#ifdef UTMPOK
static utmp, utmpf;
static char UtmpName[] = UTMPFILE;
# ifdef MIPS
  static utmpfappend;
# endif
#endif

static FILE *fp = NULL;
static char *rc_name;

char *SaveStr(str)
register char *str;
{
  register char *cp;

  if ((cp = malloc(strlen(str) + 1)) == NULL)
    Msg_nomem;
  else
    strcpy(cp, str);
  return cp;
}

static char *CatExtra(str1, str2)
register char *str1, *str2;
{
  register char *cp;
  register int len1, len2, add_colon;

  len1 = strlen(str1);
  if (len1 == 0)
    return(str2);
  add_colon = (str1[len1 - 1] != ':');
  if (str2)
    {
      len2 = strlen(str2);
      if ((cp = realloc(str2, (unsigned) len1 + len2 + add_colon + 1)) == NULL)
	Msg_nomem;
      bcopy(cp, cp + len1 + add_colon, len2 + 1);
    }
  else
    {
      if (len1 == 0)
	return 0;
      if ((cp = malloc((unsigned) len1 + add_colon + 1)) == NULL)
	Msg_nomem;
      cp[len1 + add_colon] = '\0'; 
    }
  bcopy(str1, cp, len1);
  if (add_colon)
    cp[len1] = ':';

  return cp;
}

static char *findrcfile(rcfile)
char *rcfile;
{
  static char buf[256];
  char *rc, *p;

  if (rcfile)
    {
      rc = SaveStr(rcfile);
      debug1("findrcfile: you specified '%s'\n", rcfile);
    }
  else
    {
      debug("findrcfile: you specified nothing...\n");
      if ((p = getenv("ISCREENRC")) != NULL && *p != '\0')
	{
	  debug1("  ... but $ISCREENRC has: '%s'\n", p);
	  rc = SaveStr(p);
	}
      else if ((p = getenv("SCREENRC")) != NULL && *p != '\0')
	{
	  debug1("  ... but $SCREENRC has: '%s'\n", p);
	  rc = SaveStr(p);
	}
      else
	{
	  debug("  ...nothing in $SCREENRC, defaulting $HOME/.screenrc\n");
	  if (strlen(home) > 244)
	    Msg(0, "Rc: home too large");
	  sprintf(buf, "%s/.iscreenrc", home);
          if (access(buf, R_OK))
	    sprintf(buf, "%s/.screenrc", home);
	  rc = SaveStr(buf);
	}
    }
  return rc;
}

/*
 * this will be called twice:
 * 1) rcfilename = "/etc/screenrc"
 * 2) rcfilename = RcFileName
 */
void
StartRc(rcfilename)
char *rcfilename;
{
  register int argc, len;
  register char *p, *cp;
  char buf[256];
  char *args[MAXARGS], *t;

  rc_name = findrcfile(rcfilename);

  if ((fp = secfopen(rc_name, "r")) == NULL)
    {
      if (RcFileName && strcmp(RcFileName, rc_name) == 0)
	{
          /*
           * User explicitly gave us that name,
           * this is the only case, where we get angry, if we can't read
           * the file.
           */
	  debug3("StartRc: '%s','%s', '%s'\n", RcFileName, rc_name, rcfilename);
          Msg(0, "Unable to open \"%s\".", rc_name);
	  /* NOTREACHED */
	}
      debug1("StartRc: '%s' no good. ignored\n", rc_name);
      Free(rc_name);
      rc_name = "";
      return;
    }
  if ((t = getenv("TERM")) == NULL)
    Msg(0, "No TERM in environment.");
  debug1("startrc got termcp:%s\n", t);
  while (fgets(buf, sizeof buf, fp) != NULL)
    {
      if ((p = rindex(buf, '\n')) != NULL)
	*p = '\0';
      if ((argc = Parse(buf, args)) == 0)
	continue;
      if (strcmp(args[0], "echo") == 0)
	{
	  if (argc < 2 || (argc == 3 && strcmp(args[1], "-n")) || argc > 3)
	    {
	      DeadlyMsg = 0;
	      Msg(0, "%s: 'echo [-n] \"string\"' expected.", rc_name);
	    }
	  else
	    {
	      printf((argc == 3) ? "%s" : "%s\r\n", args[argc - 1]);
	    }
	}
      else if (strcmp(args[0], "sleep") == 0)
	{
	  if (argc != 2)
	    {
	      DeadlyMsg = 0;
	      Msg(0, "%s: sleep: one numeric argument expected.", rc_name);
	    }
	  else
	    sleep(atoi(args[1]));
	}
#ifdef TERMINFO
      else if (strcmp(args[0], "terminfo") == 0)
#else
      else if (strcmp(args[0], "termcap") == 0)
#endif
	{
	  if (argc < 3 || argc > 4)
	    Msg(0, "%s: %s: incorrect number of arguments.", rc_name, args[0]);
	  for (p = args[1]; p && *p; p = cp)
	    {
	      if ((cp = index(p, '|')) != 0)
		*cp++ = '\0';
	      len = strlen(p);
	      if (p[len - 1] == '*')
		{
		  if (!(len - 1) || !strncmp(p, t, len - 1))
		    break;
		}
	      else if (!strcmp(p, t))
		break;
	    }
	  if (!(p && *p))
	    continue;
	  extra_incap = CatExtra(args[2], extra_incap);
	  if (argc == 4)
	    extra_outcap = CatExtra(args[3], extra_outcap);
	}
    }
  fclose(fp);
  Free(rc_name);
  rc_name = "";
}

static char *
ParseChar(p, cp)
char *p, *cp;
{
  if (*p == '^')
    {
      if (*++p == '?')
        *cp = '\177';
      else if (*p >= '@')
        *cp = Ctrl(*p);
      else
        return 0;
      ++p;
    }
  else if (*p == '\\' && *++p <= '7' && *p >= '0')
    {
      *cp = 0;
      do
        *cp = *cp * 8 + *p - '0';
      while (*++p <= '7' && *p >= '0');
    }
  else
    *cp = *p++;
  return p;
}

/*
 * CompileKeys must be called before Markroutine is first used.
 * to initialise the keys with defaults, call CompileKeys(NULL, mark_key_tab);
 *
 * s is an ascii string in a termcap-like syntax. It looks like
 *   "j=u:k=d:l=r:h=l: =.:" and so on...
 * this example rebinds the cursormovement to the keys u (up), d (down),
 * l (left), r (right). placing a mark will now be done with ".".
 */
int CompileKeys(s, array)
char *s, *array;
{
  int i;
  unsigned char key, value;

  if (!s || !*s)
    {
      for (i = 0; i < 256; i++)
        array[i] = i;
      return 0;
    }
  while (*s)
    {
      s = ParseChar(s, (char *) &key);
      if (*s != '=')
	return -1;
      do 
	{
          s = ParseChar(++s, (char *) &value);
	  array[value] = key;
	}
      while (*s == '=');
      if (!*s) 
	break;
      if (*s++ != ':')
	return -1;
    }
  return 0;
}

static char **SaveArgs(argc, argv)
register int argc;
register char **argv;
{
  register char **ap, **pp;

  if ((pp = ap = (char **) malloc((unsigned) (argc + 1) * sizeof(char **))) == 0)
    Msg_nomem;
#ifdef notdef
  debug("saveargs:\n"); 
#endif
  while (argc--)
    {
      debug1(" '%s'", *argv);
      *pp++ = SaveStr(*argv++);
    }
  debug("\n");
  *pp = 0;
  return ap;
}

void
FinishRc(rcfilename)
char *rcfilename;
{
  /* in FinishRc screen is not yet open, thus Msg() is deadly here.
   */
  char buf[256];

  rc_name = findrcfile(rcfilename);

  if ((fp = secfopen(rc_name, "r")) == NULL)
    {
      if (RcFileName && strcmp(RcFileName, rc_name) == 0)
	{
    	  /*
 	   * User explicitly gave us that name, 
	   * this is the only case, where we get angry, if we can't read
	   * the file.
	   */
  	  debug3("FinishRc:'%s','%s','%s'\n", RcFileName, rc_name, rcfilename);
          Msg(0, "Unable to open \"%s\".", rc_name);
	  /* NOTREACHED */
	}
      debug1("FinishRc: '%s' no good. ignored\n", rc_name);
      Free(rc_name);
      rc_name = "";
      return;
    }

  debug("finishrc is going...\n");
  while (fgets(buf, sizeof buf, fp) != NULL)
    {
      RcLine(buf);
    }
  (void) fclose(fp);
  Free(rc_name);
  rc_name = "";
}

/*
 * this is a KEY_SET pressed
 */
void
DoSet(argv)
char **argv;
{
  char *p;
  static char buf[256];

  p = buf;
  debug("DoSet\n");
  if (!argv || !*argv || !**argv)
    {
      debug("empty DoSet\n");
      sprintf(buf, "set ");
      RcLine(buf);
      return;
    }
  sprintf(p, "set"); p+=3;
  while(*argv && (strlen(buf) + strlen(*argv) < 255))
    {
      sprintf(p, " %s", *argv++);
      p += strlen(p);
    }
  RcLine(buf);
}

/*
 *	"$HOST blafoo"   	-> "localhost blafoo"
 *	"${HOST}blafoo"	  	-> "localhostblafoo"
 *	"\$HOST blafoo" 	-> "$HOST blafoo"
 *	"\\$HOST blafoo"	-> "\localhost blafoo"
 *	"'$HOST ${HOST}'"	-> "'$HOST ${HOST}'" 
 *	"'\$HOST'"       	-> "'\$HOST'"
 *	"\'$HOST' $HOST"   	-> "'localhost' $HOST"
 */
static char *expand_env_vars(ss)
char *ss;
{
  static char ebuf[2048];
  register int esize = 2047, quofl = 0;
  register char *e = ebuf;
  register char *s = ss;
  register char *v;

  while (*s && *s != '\n' && esize > 0)
    {
      if (*s == '\'')
	quofl ^= 1;
      if (*s == '$' && !quofl)
	{
	  char *p, c;

	  p = ++s;
	  if (*s == '{')
	    {
	      p = ++s;
	      while (*p != '}')
	        if (*p++ == '\0')
	          return ss;
	    }
	  else
	    {
	      while (*p != ' ' && *p != '\0' && *p != '\n')
		p++;
	    }
	  c = *p;
	  debug1("exp: c='%c'\n", c);
	  *p = '\0';
	  if (v = getenv(s)) 
	    {
	      debug2("exp: $'%s'='%s'\n", s, v);
	      while (*v && esize-- > 0)
	        *e++ = *v++;
	    }
	  else 
	    debug1("exp: '%s' not env\n", s);
	  if ((*p = c) == '}')
	    p++;
	  s = p;
	}
      else
	{
	  if (s[0] == '\\' && !quofl)
	    if (s[1] == '$' || (s[1] == '\\' && s[2] == '$') ||
	        s[1] == '\'' || (s[1] == '\\' && s[2] == '\''))
	      s++;
	  *e++ = *s++;
	  esize--;
	}
    }
  if (esize <= 0)
    Msg(0, "expand_env_vars: buffer overflow\n");
  *e = '\0';
  return ebuf;
}

void
RcLine(ubuf)
char *ubuf;
{
  char *args[MAXARGS];
  register char *buf, *p, **pp, **ap;
  register int argc, setflag;
  int q, qq;
  char key;
  int low, high, mid, x;

  buf = expand_env_vars(ubuf); 

  ap = args;

  if ((p = rindex(buf, '\n')) != NULL)
    *p = '\0';
  if (strncmp("set ", buf, 4) == 0)
    {
      buf += 4;
      setflag = 1;
      debug1("RcLine: '%s' is a set command\n", buf);
    }
  else if (strncmp("se ", buf, 3) == 0)
    {
      buf += 3;
      setflag = 1;
      debug1("RcLine: '%s' is a se command\n", buf);
    }
  else
    {
      setflag = 0;
      debug1("RcLine: '%s'\n", buf);
    }
  if ((argc = Parse(buf, ap)) == 0)
    {
      if (setflag)
	{
	  DeadlyMsg = 0;
	  Msg(0, "%s: set what?\n", rc_name);
	}
      return;
    }

  low = 0;
  high = (int)RC_RCEND - 1;
  while (low <= high)
    {
      mid = (low + high) / 2;
      x = strcmp(ap[0], RCNames[mid]);
      if (x < 0)
        high = mid - 1;
      else if (x > 0)
        low = mid + 1;
      else
        break;
    }
  if (low > high)
    mid = (int)RC_RCEND;
  switch ((enum RCcases) mid)
    {
    case RC_ESCAPE:
      if (argc != 2 || !ParseEscape(ap[1]))
	{
	  DeadlyMsg = 0; 
	  Msg(0, "%s: two characters required after escape.", rc_name);
	  return;
	}
      if (Esc != MetaEsc)
	ktab[Esc].type = KEY_OTHER;
      else
	ktab[Esc].type = KEY_IGNORE;
      return;
    case RC_CHDIR:
      if (setflag)
	break;
      p = argc < 2 ? home : ap[1];
      if (chdir(p) == -1)
	{
	  DeadlyMsg = 0; 
	  Msg(errno, "%s", p);
	}
      return;
    case RC_SHELL:
      ParseSaveStr(argc, ap, &ShellProg, "shell");
      ShellArgs[0] = ShellProg;
      return;
    case RC_SHELLAKA:
      ParseSaveStr(argc, ap, &shellaka, "shellaka");
      return;
    case RC_SCREEN:
      if (setflag)
	break;
      DoScreen(rc_name, ap + 1);
      return;
    case RC_SLEEP:
    case RC_TERMCAP:
    case RC_TERMINFO:
      return;			/* Already handled */
    case RC_TERM:
      {
        char *tmp = NULL;

        ParseSaveStr(argc, ap, &tmp, "term");
        if (!tmp)
          return;
	if (strlen(tmp) >= 20)
	  {
	    DeadlyMsg = 0;
            Msg(0,"%s: term: argument too long ( < 20)", rc_name);
            Free(tmp);
	    return;
          }
        strcpy(screenterm, args[1]);
	Free(tmp);
        debug1("screenterm set to %s\n", screenterm);
        MakeTermcap(0);
        return;	
      }
    case RC_ECHO:
      if (HasWindow && *rc_name == '\0')
	{
	  /*
	   * user typed ^A:echo... well, echo isn't FinishRc's job,
	   * but as he wanted to test us, we show good will
	   */
	  DeadlyMsg = 0;
	  if (argc == 2 || (argc == 3 && !strcmp(ap[1], "-n")))
	    Msg(0, "%s", ap[argc - 1]);
	  else
 	    Msg(0, "%s: 'echo [-n] \"string\"' expected.", rc_name);
	}
      return;
    case RC_BELL:
      ParseSaveStr(argc, ap, &BellString, "bell");
      return;
    case RC_BUFFERFILE:
      ParseSaveStr(argc, ap, &BufferFile, "bufferfile");
      return;
    case RC_ACTIVITY:
      ParseSaveStr(argc, ap, &ActivityString, "activity");
      return;
    case RC_POW_DETACH_MSG:
      ParseSaveStr(argc, ap, &PowDetachString, "pow_detach");
      return;
    case RC_LOGIN:
#ifdef UTMPOK
      q = loginflag;
      ParseOnOff(argc, ap, &loginflag);
      if (fore && setflag)
	{
	  SlotToggle(loginflag?(1):(-1));
	  loginflag = q;
	}
#endif
      return;
    case RC_FLOW:
      if (argc == 3 && ap[2][0] == 'i')
	{
	  iflag = 1;
	  argc--;
	}
      if (argc == 2 && ap[1][0] == 'a')
	default_flow = FLOW_AUTOFLAG;
      else
	ParseOnOff(argc, ap, &default_flow);
      return;
    case RC_WRAP:
      ParseOnOff(argc, ap, &wrap);
      return;
    case RC_HARDSTATUS:
      ParseOnOff(argc, ap, &use_hardstatus);
      if (use_hardstatus)
	HS = termcapHS;
      else
	HS = 0;
      return;
    case RC_MONITOR:
	{
	  int f; 

	  ParseOnOff(argc, ap, &f);
	  if (fore && setflag)
	    fore->monitor = (f == 0) ? MON_OFF : MON_ON;
	  else
	    default_monitor = (f == 0) ? MON_OFF : MON_ON;
	}
      return;
    case RC_REDRAW:
    case RC_REFRESH:
	{
	  int r;

	  ParseOnOff(argc, ap, &r);
	  if (fore && setflag)
	    fore->norefresh = (r) ? 0 : 1;
	  else
	    {
	      all_norefresh = (r) ? 0 : 1;
	      if (all_norefresh)
	        Msg(0, "No refresh on window change!\n");
	      else
	        Msg(0, "Window specific refresh\n");
	    }
	}
      return;
    case RC_VBELL:
    case RC_VISUALBELL:
      ParseOnOff(argc, ap, &visual_bell);
      return;
    case RC_VBELLWAIT:
      ParseNum(argc, ap, &VBellWait);
      if (fore && rc_name[0] == '\0')
        Msg(0, "vbellwait set to %d seconds", VBellWait);
      return;
    case RC_MSGWAIT:
      ParseNum(argc, ap, &MsgWait);
      if (fore && rc_name[0] == '\0')
        Msg(0, "msgwait set to %d seconds", MsgWait);
      return;
    case RC_MSGMINWAIT:
      ParseNum(argc, ap, &MsgMinWait);
      if (fore && rc_name[0] == '\0')
        Msg(0, "msgminwait set to %d seconds", MsgMinWait);
      return;
    case RC_SCROLLBACK:
      if (fore && setflag)
	{
	  int i;

	  ParseNum(argc, ap, &i);
	  ChangeScrollback(fore, i, fore->width);
	  if (fore && rc_name[0] == '\0')
	    Msg(0, "scrollback set to %d", fore->histheight);
	}
      else
	ParseNum(argc, ap, &default_histheight);
      return;
    case RC_SLOWPASTE:
      ParseNum(argc, ap, &slowpaste);
      if (fore && rc_name[0] == '\0')
	Msg(0, "slowpaste set to %d milliseconds", slowpaste);
      return;
    case RC_MARKKEYS:
      {
        char *tmp = NULL;

        ParseSaveStr(argc, ap, &tmp, "markkeys");
        if (CompileKeys(ap[1], mark_key_tab))
	  {
	    DeadlyMsg = 0;
	    Msg(0, "%s: markkeys: syntax error.", rc_name);
	    Free(tmp);
	    return;
	  }
        debug1("markkeys %s\n", ap[1]);
        Free(tmp);
        return;
      }
#ifdef NETHACK
    case RC_NETHACK:
      ParseOnOff(argc, ap, &nethackflag);
      return;
#endif
    case RC_HARDCOPY_APP:
      ParseOnOff(argc, ap, &hardcopy_append);
      return;
    case RC_VBELL_MSG:
    case RC_VISUALBELL_MSG:
      ParseSaveStr(argc, ap, &VisualBellString, "vbell_msg");
      debug1(" new vbellstr '%s'\n", VisualBellString);
      return;
    case RC_MODE:
      if (argc != 2)
	{
	  DeadlyMsg = 0; 
	  Msg(0, "%s: mode: one argument required.", rc_name);
	  return;
	}
      if (!IsNum(ap[1], 7))
	{
	  DeadlyMsg = 0; 
	  Msg(0, "%s: mode: octal number expected.", rc_name);
	  return;
	}
      (void) sscanf(ap[1], "%o", &TtyMode);
      return;
    case RC_CRLF:
      ParseOnOff(argc, ap, &join_with_cr);
      return;
    case RC_AUTODETACH:
      ParseOnOff(argc, ap, &auto_detach);
      return;
    case RC_STARTUP_MESSAGE:
      ParseOnOff(argc, ap, &default_startup);
      return;
#ifdef PASSWORD
    case RC_PASSWORD:
      CheckPassword = 1;
      if (argc >= 2)
	{
	  strncpy(Password, ap[1], sizeof Password);
	  if (!strcmp(Password, "none"))
	    CheckPassword = 0;
	}
      else
	{
	  char *mstr = 0;
	  int msleep = 0, st;
          char salt[2];

#ifdef POSIX
	  if (HasWindow)
	    {
	      Msg(0, "Cannot ask for password on POSIX systems");
	      return;
	    }
#endif
	  /* there is a clear screen sequence in the buffer. */
	  fflush(stdout);
	  if (HasWindow)
	    {
              ClearDisplay();
	      SetTTY(0, &OldMode);
	    }
	  strncpy(Password, getpass("New screen password:"),
		  sizeof(Password));
	  if (strcmp(Password, getpass("Retype new password:")))
	    {
#ifdef NETHACK
              if (nethackflag)
	        mstr = "[ Passwords don't match - your armor crumbles away ]";
	      else
#endif
	      mstr = "[ Passwords don't match - checking turned off ]";
	      msleep = 1;
	      CheckPassword = 0;
	    }
	  if (Password[0] == '\0')
	    {
	      CheckPassword = 0;
	      mstr = "[ No password - no secure ]";
	      msleep = 1;
	    }
	  for (st=0; st<2; st++)
            salt[st] = 'A' + (int)((time(0) >> 6*st) % 26);
	  strncpy(Password, crypt(Password, salt), sizeof(Password));
	  if (CheckPassword)
	    {
#ifdef COPY_PASTE
	      if (copybuffer)

		Free(copybuffer);
	      copylen = strlen(Password);
	      if ((copybuffer = (char *) malloc(copylen+1)) == NULL)
		{
		  Msg_nomem;
		  return;
		}
	      strcpy(copybuffer, Password);
	      mstr = "[ Password moved into copybuffer ]";
	      msleep = 1;
#else				/* COPY_PASTE */
	      mstr = "[ Crypted password is \"%s\" ]";
	      msleep = 5;
#endif				/* COPY_PASTE */
	    }
          if (HasWindow)
	    {
	      SetTTY(0, &NewMode);
	      Activate(0); /* Redraw */
	      if (mstr)
	        {
	          Msg(0, mstr, Password);
	        }
	    }
          else
	    {
	      if (mstr)
	        {
	          printf(mstr, Password);
	          putchar('\n');
	          sleep(msleep);
	        }
              ClearDisplay();
	    }
	}
      debug1("finishrc: our password is: --%s%-- \n", Password);
      return;
#endif				/* PASSWORD */
    case RC_ALL:
      if (!setflag || !HasWindow || *rc_name)
        break;
      display_help();
      return;
    case RC_BIND:
      if (setflag)
	break;
      p = ap[1];
      if (argc < 2 || *p == '\0')
	{
	  DeadlyMsg = 0; 
	  Msg(0, "%s: key expected after bind.", rc_name);
	  return;
	}
      if ((p = ParseChar(p, &key)) == NULL || *p)
	{
	  DeadlyMsg = 0; 
	  Msg(0, "%s: bind: character, ^x, or (octal) \\032 expected.",
	      rc_name);
	  return;
	}
      if (ktab[key].type != KEY_IGNORE)
	{
	  ktab[key].type = KEY_IGNORE;
	  if ((pp = ktab[key].args) != NULL)
	    {
	      for (; *pp; pp++)
		Free(*pp);
	      Free(ktab[key].args);
	    }
	}
      if (argc > 2)
	{
	  for (pp = KeyNames; *pp; ++pp)
	    if (strcmp(ap[2], *pp) == 0)
	      break;
	  if (*pp)
	    {
	      ktab[key].type = (enum keytype) (pp - KeyNames + 1);
	      if (argc > 3)
		{
		  ktab[key].args = SaveArgs(argc - 3, ap + 3);
		}
	      else
		ktab[key].args = NULL;
	    }
	  else
	    {
	      ktab[key].type = KEY_CREATE;
	      ktab[key].args = SaveArgs(argc - 2, ap + 2);
	    }
	}
      return;
    case RC_RCEND:
    default:
	{
	  char ibuf[3];
	  /*
	   * now we are user-friendly: 
	   * if anyone typed a key name like "help" or "next" ...
	   * we did not match anything above. so look in the KeyNames table.
	   */
	  debug1("--ap[0] %s\n", ap[0]);
	  for (pp = KeyNames; *pp; ++pp)
	    if (strcmp(ap[0], *pp) == 0)
		break;
	  if (*pp == 0)
	    break;

	  ibuf[0] = Esc;
	  ibuf[1] = pp - KeyNames +1;
	  debug1("RcLine: it was a keyname: '%s'\n", *pp);
	  q = 2; qq = 0;
	  if (HasWindow)
	    ProcessInput(ibuf, &q, (char *)0, &qq, 0);
	  else
	    {
	      DeadlyMsg = 0; 
	      Msg(0, "%s: Key '%s' has no effect while no window open...\n",
	          rc_name, ap[0]);
	    }
	}
      return;
    }
  DeadlyMsg = 0; 
  Msg(0, "%s: unknown %skeyword \"%s\"", rc_name, 
      setflag?"'set' ":"", ap[0]);
}

static int 
Parse(buf, args)
char *buf, **args;
{
  register char *p = buf, **ap = args;
  register int delim, argc;

  argc = 0;
  for (;;)
    {
      while (*p && (*p == ' ' || *p == '\t'))
	++p;
      if (*p == '\0' || *p == '#')
	{
	  *p = '\0';
	  return argc;
	}
      if (argc > MAXARGS - 1)
	Msg(0, "%s: too many tokens.", rc_name);
      delim = 0;
      if (*p == '"' || *p == '\'')
	delim = *p++;
      argc++;
      *ap = p;
      *++ap = 0;
      while (*p && !(delim ? *p == delim : (*p == ' ' || *p == '\t')))
	++p;
      if (*p == '\0')
	{
	  if (delim)
	    {
	      DeadlyMsg = 0;
	      Msg(0, "%s: Missing quote.", rc_name);
	      return 0;
	}
	  return argc;
	}
      *p++ = '\0';
    }
}

int 
ParseEscape(p)
char *p;
{
  if ((p = ParseChar(p, &Esc)) == NULL ||
      (p = ParseChar(p, &MetaEsc)) == NULL || *p)
    return 0;
  return 1;
}

static void
ParseNum(argc, ap, var)
int argc;
char *ap[];
int *var;
{
  int i;
  char *p;

  if (argc == 2 && ap[1][0] != '\0')
    {
      i = 0; 
      p = ap[1];
      while (*p)
	{
	  if (*p >= '0' && *p <= '9')
	    i = 10 * i + (*p - '0');
	  else
	    {
	      DeadlyMsg = 0;
	      Msg(0, "%s: %s: invalid argument. Give numeric argument",
		  rc_name, ap[0]);
	      return;
	    }    
	  p++;
	}
    }
  else
    {
      DeadlyMsg = 0;
      Msg(0, "%s: %s: invalid argument. Give one argument",
          rc_name, ap[0]);
      return;
    }
  debug1("ParseNum got %d\n", i);
  *var = i;
}

static void
ParseSaveStr(argc, ap, var, title)
int argc;
char *ap[];
char **var;
char *title;
{
  if (argc != 2)
    {
      DeadlyMsg = 0;
      Msg(0, "%s: %s: one argument required.", rc_name, title);
      return;
    }
  if (*var)
    Free(*var);
  *var = SaveStr(ap[1]);
  return;
}
 
static void
ParseOnOff(argc, ap, var)
int argc;
char *ap[];
int *var;
{
  register int num = -1;

  if (argc == 2 && ap[1][0] == 'o')
    {
      if (ap[1][1] == 'f')
	num = 0;
      else if (ap[1][1] == 'n')
	num = 1;
    }
  if (num < 0)
    {
      DeadlyMsg = 0;
      Msg(0, "%s: %s: invalid argument. Give 'on' or 'off'", rc_name, ap[0]);
      return;
    }
  *var = num;
}


static int IsNum(s, base)
register char *s;
register int base;
{
  for (base += '0'; *s; ++s)
    if (*s < '0' || *s > base)
      return 0;
  return 1;
}

static int IsNumColon(s, base, p, psize)
int base, psize;
char *s, *p;
{
  char *q;
  if ((q = rindex(s, ':')) != NULL)
    {
      strncpy(p, q + 1, psize - 1);
      p[psize - 1] = '\0';
      *q = '\0';
    }
  else
    *p = '\0';
  return IsNum(s, base);
}

void
SlotToggle(how)
int how;
/*
 * how = 0	real toggle mode
 * how > 0	do try to set a utmp slot.
 * how < 0	try to withdraw a utmp slot
 *
 * slot = -1    window not logged in.
 * slot = 0     window not logged in, but should be logged in. 
 *              (unable to write utmp, or detached).
 */
{
  debug1("SlotToggle %d\n", how);
  if (how == 0)
    how = (fore->slot == (slot_t) -1)?(1):(-1);
    /* 
     * slot 0 or active -> we try to log out.
     * slot -1          -> we try to log in.
     */
#ifdef UTMPOK
  if (how > 0)
    {
      debug(" try to log in\n");
      if ((fore->slot == (slot_t) -1) || (fore->slot == (slot_t) 0))
	{
#ifdef USRLIMIT
          if (CountUsers() >= USRLIMIT)
            Msg(0, "User limit reached.");
          else
#endif
            {
              if (SetUtmp(fore, ForeNum) == 0)
                Msg(0, "This window is now logged in.");
              else
                Msg(0, "This window should now be logged in.");
            }
	}
      else
	Msg(0, "This window is already logged in.");
    }
  else if (how < 0)
    {
      debug(" try to log out\n");
      if (fore->slot == (slot_t) -1)
	Msg(0, "This window is already logged out\n");
      else if (fore->slot == (slot_t) 0)
	{
	  debug("What a relief! In fact, it was not logged in\n");
	  Msg(0, "This window is not logged in.");
	  fore->slot = (slot_t) -1;
	}
      else
	{
	  RemoveUtmp(fore);
	  if (fore->slot != (slot_t) -1)
	    Msg(0, "What? Cannot remove Utmp slot?");
	  else
	    Msg(0, "This window is no longer logged in.");
	}
    }
#else	/* !UTMPOK */
  Msg(0, "Unable to modify %s.\n", UTMPFILE);
#endif
}

void
DoScreen(fn, av)
char *fn, **av;
{
  register int flowflag, num, lflag = loginflag, aflag = 0;
  register char *aka = NULL;
  register int histheight = default_histheight;
  char buf[20];
  char termbuf[25];
  char *termp;
  char *args[2];

  flowflag = default_flow;
  termbuf[0] = '\0';
  termp = NULL;
  while (av && *av && av[0][0] == '-')
    {
      switch (av[0][1])
	{
	case 'f':
	  switch (av[0][2])
	    {
	    case 'n':
	    case '0':
	      flowflag = FLOW_NOW * 0;
	      break;
	    case 'y':
	    case '1':
	    case '\0':
	      flowflag = FLOW_NOW * 1;
	      break;
	    case 'a':
	      flowflag = FLOW_AUTOFLAG;
	      break;
	    default:
	      break;
	    }
	  break;
	case 'k':
	case 't':
	  if (av[0][2])
	    aka = &av[0][2];
	  else if (*++av)
	    aka = *av;
	  else
	    --av;
	  break;
	case 'T':
	  if (av[0][2])
	    termp = &av[0][2];
	  else if (*++av)
	    termp = *av;
	  else
	    --av;
	  break;
	case 'h':
	  if (av[0][2])
	    histheight = atoi(av[0] + 2);
	  else if (*++av)
	    histheight = atoi(*av);
	  else 
	    --av;
	  break;
	case 'l':
	  switch (av[0][2])
	    {
	    case 'n':
	    case '0':
	      lflag = 0;
	      break;
	    case 'y':
	    case '1':
	    case '\0':
	      lflag = 1;
	      break;
	    default:
	      break;
	    }
	  break;
	case 'a':
	  aflag = 1;
	  break;
	default:
	  Msg(0, "%s: screen: invalid option -%c.", fn, av[0][1]);
	  break;
	}
      ++av;
    }
  num = 0;
  if (av && *av && IsNumColon(*av, 10, buf, sizeof(buf)))
    {
      if (*buf != '\0')
	aka = buf;
      num = atoi(*av);
      if (num < 0 || num > MAXWIN - 1)
	{
	  Msg(0, "%s: illegal screen number %d.", fn, num);
	  num = 0;
	}
      ++av;
    }
  if (!av || !*av)
    {
      av = args;
      av[0] = ShellProg;
      av[1] = NULL;
      if (!aka)
	aka = shellaka;
    }
  MakeWindow(aka, av, aflag, flowflag, num, (char *) 0, lflag, histheight, termp);
}

void
WriteFile(dump)
int dump;
{
  /* dump==0:	create .termcap,
   * dump==1:	hardcopy,
   * #ifdef COPY_PASTE
   * dump==2:	BUFFERFILE
   * #endif COPY_PASTE 
   */
  register int i, j, k;
  register char *p;
  register FILE *f;
  char fn[1024];
  char *mode = "w";

  switch (dump)
    {
    case DUMP_TERMCAP:
      i = SockNamePtr - SockPath;
      strncpy(fn, SockPath, i);
      strcpy(fn + i, ".termcap");
      break;
    case DUMP_HARDCOPY:
      sprintf(fn, "hardcopy.%d", ForeNum);
      if (hardcopy_append && !access(fn, W_OK))
	mode = "a";
      break;
    case DUMP_EXCHANGE:
      sprintf(fn, "%s", BufferFile);
      umask(0);
      break;
    }

  debug2("WriteFile(%d) %s\n", dump, fn);
  if (UserContext() > 0)
    {
      debug("Writefile: usercontext\n");
      if ((f = fopen(fn, mode)) == NULL)
	{
	  debug2("WriteFile: fopen(%s,\"%s\") failed\n", fn, mode);
	  UserReturn(0);
	}
      else
	{
	  switch (dump)
	    {
	    case DUMP_HARDCOPY:
	      if (*mode == 'a')
		{
		  putc('>', f);
		  for (j = screenwidth - 2; j > 0; j--)
		    putc('=', f);
		  fputs("<\n", f);
		}
	      for (i = 0; i < screenheight; ++i)
		{
		  p = fore->image[i];
		  for (k = screenwidth - 1; k >= 0 && p[k] == ' '; --k)
		    ;
		  for (j = 0; j <= k; ++j)
		    putc(p[j], f);
		  putc('\n', f);
		}
	      break;
	    case DUMP_TERMCAP:
	      if ((p = index(MakeTermcap(fore->aflag), '=')) != NULL)
		{
		  fputs(++p, f);
		  putc('\n', f);
		}
	      break;
#ifdef COPY_PASTE
	    case DUMP_EXCHANGE:
	      p = copybuffer;
	      for (i = 0; i < copylen; i++)
		putc(*p++, f);
	      break;
#endif
	    }
	  (void) fclose(f);
	  UserReturn(1);
	}
    }
  if (UserStatus() <= 0)
    Msg(0, "Cannot open \"%s\"", fn);
  else
    {
      switch (dump)
	{
	case DUMP_TERMCAP:
	  Msg(0, "Termcap entry written to \"%s\".", fn);
	  break;
	case DUMP_HARDCOPY:
	  Msg(0, "Screen image %s to \"%s\".",
	      (*mode == 'a') ? "appended" : "written", fn);
	  break;
#ifdef COPY_PASTE
	case DUMP_EXCHANGE:
	  Msg(0, "Copybuffer written to \"%s\".", fn);
#endif
	}
    }
}

#ifdef COPY_PASTE

void
ReadFile()
{
  int i, l, size;
  char fn[1024], c;
  struct stat stb;

  sprintf(fn, "%s", BufferFile);
  debug1("ReadFile(%s)\n", fn);
  if ((i = secopen(fn, O_RDONLY, 0)) < 0)
    {
      Msg(errno, "no %s -- no slurp", fn);
      return;
    }
  if (fstat(i, &stb))
    {
      Msg(errno, "no good %s -- no slurp", fn);
      close(i);
      return;
    }
  size = stb.st_size;
  if (copybuffer)
    Free(copybuffer);
  copylen = 0;
  if ((copybuffer = malloc(size)) == NULL)
    {
      close(i);
      Msg_nomem;
      return;
    }
  errno = 0;
  if ((l = read(i, copybuffer, size)) != size)
    {
      copylen = (l > 0) ? l : 0;
#ifdef NETHACK
      if (nethackflag)
        Msg(errno, "You choke on your food: %d bytes", copylen);
      else
#endif
      Msg(errno, "Got only %d bytes from %s", copylen, fn);
      close(i);
      return;
    }
  copylen = l;
  if (read(i, &c, 1) > 0)
    Msg(0, "Slurped only %d characters into buffer - try again", copylen);
  else
    Msg(0, "Slurped %d characters into buffer", copylen);
  close(i);
  return;
}

void
KillBuffers()
{
  char fn[1024];
  sprintf(fn, "%s", BufferFile);
  errno = 0;
  if (access(fn, W_OK) == -1)
    {
      Msg(errno, "%s not removed", fn);
      return;
    }
  else
    {
      unlink(fn);
      Msg(errno, "%s removed", fn);
    }
}
#endif	/* COPY_PASTE */

#ifdef USRLIMIT
CountUsers()
{
#ifdef GETUTENT
  struct utmp *ut, *getutent();
#else
  struct utmp utmpbuf;
#endif
  int UserCount;

  debug1("CountUsers() - utmp=%d\n",utmp);
  if (!utmp)
    return(0);
  UserCount = 0;
#ifdef GETUTENT
  setutent();
  while (ut = getutent())
    if (ut->ut_type == USER_PROCESS)
      UserCount++;
#else
  (void) lseek(utmpf, (off_t) 0, 0);
  while (read(utmpf, &utmpbuf, sizeof(struct utmp)) > 0)
    {
      if (utmpbuf.ut_name[0] != '\0')
       UserCount++;
    }
#endif
  return(UserCount);
}
#endif

#ifdef UTMPOK

static slot_t loginslot;
static struct utmp utmp_logintty;
#ifdef _SEQUENT_
static char loginhost[100+1];
#endif

void
InitUtmp()
{
  debug("InitUtmp testing...\n");
  if ((utmpf = open(UtmpName, O_RDWR)) == -1)
    {
      if (errno != EACCES)
	Msg(errno, UtmpName);
      debug("InitUtmp failed.\n");
      utmp = 0;
      return;
    }
#ifdef GETUTENT
  close(utmpf);
  utmpf= -1;
#endif
#ifdef MIPS
  if ((utmpfappend = open(UtmpName, O_APPEND)) == -1) 
    {
      if (errno != EACCES)
	Msg(errno, UtmpName);
      return;
    }
#endif
  utmp = 1;
#ifndef apollo
  ReInitUtmp();
#endif
}

void
ReInitUtmp()
{
#ifndef apollo
  if (!utmp)
    {
      debug("Reinitutmp: utmp == 0\n");
      return;
    }
#endif
  debug("(Re)InitUtmp: removing your logintty\n");
  loginslot = TtyNameSlot(display_tty);
  if (loginslot!=(slot_t)0 && loginslot!=(slot_t)-1)
    {
#ifdef _SEQUENT_
      if (p=ut_find_host(loginslot))
        strncpy(loginhost, p, 100);
#endif
      RemoveLoginSlot(loginslot, &utmp_logintty);
    }
  debug1(" slot %d zapped\n", loginslot);
}

void
RestoreLoginSlot()
{
  debug("RestoreLoginSlot()\n");
#ifdef apollo
  InitUtmp();
#endif
  if (utmp && loginslot!=(slot_t)0 && loginslot!=(slot_t)-1)
    {
#ifdef GETUTENT
# ifdef _SEQUENT_
      int fail;
      debug1(" logging you in again (slot %s)\n", loginslot);
/*
 * We have problems if we add the console and use ut_add_user()
 * because the id will be 'scon' instead of 'co'. So we
 * restore it with pututline(). The reason why we don't use
 * pututline all the time is that we want to set the host field.
 * Unfortunatelly this can only be done with ut_add_user().
 */
      if (*loginhost)
        {
          fail = (ut_add_user(LoginName, loginslot, utmp_logintty.ut_pid,
                              *loginhost?loginhost:(char *)0) == 0);
        }
      else
        {
          setutent();
          fail = (pututline(&utmp_logintty) == 0);
        }
      if (fail)
# else	/* _SEQUENT_ */
      debug1(" logging you in again (slot %s)\n", loginslot);
      setutent();
      if (pututline(&utmp_logintty)==0)
# endif	/* _SEQUENT */
#else	/* GETUTENT */
      debug1(" logging you in again (slot %d)\n", loginslot);
# ifdef sequent
      /* call sequent undocumented routine to count logins and add utmp entry if possible */
      if (add_utmp(loginslot, &utmp_logintty) == -1)
# else
      (void) lseek(utmpf, (off_t) (loginslot * sizeof(struct utmp)), 0);
      if (write(utmpf, (char *) &utmp_logintty, sizeof(struct utmp))
	  != sizeof(struct utmp))
# endif /* sequent */
#endif	/* GETUTENT */
        {
#ifdef NETHACK
          if (nethackflag)
            Msg(errno, "%s is too hard to dig in.", UTMPFILE);
	  else
#endif
          Msg(errno,"Could not write %s.", UTMPFILE);
        }
    }
#ifdef apollo
  close(utmpf);
#endif
  loginslot = (slot_t) 0;
}

void
RemoveLoginSlot(slot, up)
slot_t slot;
struct utmp *up;
{
#ifdef GETUTENT
  struct utmp *uu;
#endif
  struct utmp u;
#ifdef apollo
  struct utmp *uq;
#endif

#ifdef GETUTENT
  debug2("RemoveLoginSlot(%s, %08x)\n", (slot == (slot_t) 0 ||
         slot == (slot_t) -1 ) ? "no slot" : slot, up);
#else
  debug2("RemoveLoginSlot(%d, %08x)\n", slot, up);
#endif
#ifdef apollo
  InitUtmp();
  bzero((char *)up, sizeof(struct utmp));
  uq = (struct utmp *)malloc(sizeof(struct utmp));
  bzero((char *)uq, sizeof(struct utmp));
#endif /* apollo */
  if (!utmp)
    return;
  if (slot != (slot_t) 0 && slot != (slot_t) -1)
    {
      bzero((char *) &u, sizeof u);
#ifdef GETUTENT
      setutent();
      strncpy(u.ut_line, slot, sizeof(u.ut_line));
      if ((uu = getutline(&u)) == 0)
        {
	  DeadlyMsg = 0;
          Msg(0, "Utmp slot not found -> not removed");
          return;
        }
      *up= *uu;
# ifdef _SEQUENT_
      if (ut_delete_user(slot, uu->ut_pid, 0, 0) == 0)
# else
      uu->ut_type = DEAD_PROCESS;
      uu->ut_exit.e_termination = 0;
      uu->ut_exit.e_exit= 0;
      if (pututline(uu) == 0)
# endif
#else
      (void) lseek(utmpf, (off_t) (slot * sizeof u), 0);
      if (read(utmpf, (char *) up, sizeof u) != sizeof u)
	{
	  DeadlyMsg = 0;
	  Msg(errno, "cannot read %s ???", UTMPFILE);
	  sleep(1);
	}
      (void) lseek(utmpf, (off_t) (slot * sizeof u), 0);
# ifdef apollo
      bcopy((char *)up, (char *)uq, sizeof(struct utmp));
      bzero(uq->ut_name, sizeof(uq->ut_name));
      bzero(uq->ut_host, sizeof(uq->ut_host));
      if (write(utmpf, (char *)uq, sizeof(struct utmp)) != sizeof(struct utmp))
# else
      if (write(utmpf, (char *) &u, sizeof u) != sizeof u)
# endif /* apollo */
#endif
        {
#ifdef NETHACK
          if (nethackflag)
	    {
	      DeadlyMsg = 0;
              Msg(errno, "%s is too hard to dig in.", UTMPFILE); 
	    }
          else
#endif
	    {
	      DeadlyMsg = 0;
              Msg(errno, "Could not write %s.", UTMPFILE);
	    }
        }
    }
  else 
    {
      debug1("There is no utmp-slot to be removed(%d)\n", slot);
    }
#ifdef apollo
  close(utmpf);
  free(uq);
#endif
}

char *
stripdev(nam)
char *nam;
{
#ifdef apollo
  char *p;

  if (nam == NULL)
    return NULL;
  if (p = strstr(nam,"/dev/"))
    return p + 5;
#else
  if (nam == NULL)
    return NULL;
  if (strncmp(nam, "/dev/", 5) == 0)
    return nam + 5;
#endif
  return nam;
}

static slot_t TtyNameSlot(nam)
char *nam;
{
  char *name;
  register slot_t slot;
#ifndef GETUTENT
  register struct ttyent *tp;
#endif
#ifdef apollo
  struct utmp *up;
#endif

  debug1("TtyNameSlot(%s)\n", nam);
#ifdef apollo
  InitUtmp();
#endif
  if (!utmp || nam == NULL)
    return (slot_t)0;
  name = stripdev(nam);
#ifdef GETUTENT
  slot = name;
#else
# ifdef apollo
  slot = 0;
  up = (struct utmp *)malloc(sizeof(struct utmp));
  while (1)
    {
      if ((read(utmpf, (char *)up, sizeof(struct utmp)) ==
	   sizeof(struct utmp)) && (strcmp(up->ut_line, name)))
	slot++;
      else
	break;
    }
  close(utmpf);
  free(up);
# else /* !apollo */
  slot = 1;
  setttyent();
  while ((tp = getttyent()) != NULL && strcmp(name, tp->ty_name) != 0)
    {
      debug2("'%s' %d, ", tp->ty_name, slot);
      ++slot;
    }
  debug("\n");
#  ifdef MIPS
  if (tp == NULL)
    {
      slot = CreateUtmp(name);
    }
#  endif /* MIPS */
# endif /* apollo */
#endif /* GETUTENT */
  return slot;
}

int
SetUtmp(wi, displaynumber)
struct win *wi;
int displaynumber;
{
  register char *p;
  register slot_t slot;
  char *line;
  struct utmp u;
#ifdef UTHOST
# ifdef _SEQUENT_
  char host[100+5];
# else
  char host[sizeof(utmp_logintty.ut_host)+5];
# endif
#endif

  wi->slot = (slot_t) 0;
  if (!utmp)
    return -1;
  if ((slot = TtyNameSlot(wi->tty)) == (slot_t) NULL)
    {
      debug1("SetUtmp failed (tty %s).\n",wi->tty);
      return -1;
    }
  debug2("SetUtmp %d will get slot %d...\n", displaynumber, (int)slot);
#ifdef apollo
  InitUtmp();
#endif

#ifdef UTHOST
  host[sizeof(host)-5] = '\0';
# ifdef _SEQUENT_
  strncpy(host, loginhost, sizeof(host) - 5);
# else
  strncpy(host, utmp_logintty.ut_host, sizeof(host) - 5);
# endif
  if (loginslot != (slot_t)0 && loginslot != (slot_t)-1 && host[0] != '\0')
    {
      /*
       * we want to set our ut_host field to something like
       * ":ttyhf:s.0" or
       * "faui45:s.0" or
       * "132.199.81.4:s.0" (even this may hurt..), but not
       * "faui45.informati"......:s.0
       */
      for (p = host; *p; p++)
	{
	  if ((*p < '0' || *p > '9') && (*p != '.'))
	    break;
	}
      if (*p)
	{
	  for (p = host; *p; p++)
	    {
	      if (*p == '.')
		{
		  *p = '\0';
		  break;
		}
	    }
	}
    }
  else
    {
      strncpy(host + 1, stripdev(display_tty), sizeof(host) - 6);
      host[0] = ':';
    }
  debug1("rlogin hostname: '%s'\n", host);
  sprintf(host + strlen(host), ":S.%c", '0' + displaynumber);
  debug1("rlogin hostname: '%s'\n", host);
#endif /* UTHOST */

  line = stripdev(wi->tty);
  bzero((char *) &u, sizeof u);

#ifdef GETUTENT
# ifdef _SEQUENT_
  if (ut_add_user(LoginName, slot, wi->wpid, host)==0)
# else
  strncpy(u.ut_user, LoginName, sizeof(u.ut_user));
  strncpy(u.ut_id, line + strlen(line) - 2, sizeof(u.ut_id));
  strncpy(u.ut_line, line, sizeof(u.ut_line));
  u.ut_pid = wi->wpid;
  u.ut_type = USER_PROCESS;
#  ifdef SVR4
    (void) time(&u.ut_tv.tv_sec);
    u.ut_tv.tv_usec=0;
#  else
    (void) time(&u.ut_time);
#  endif /* SVR4 */
#  ifdef UTHOST
  strncpy(u.ut_host, host, sizeof(u.ut_host));
#  endif /* UTHOST */
  if (pututline(&u) == 0)
# endif /* _SEQUENT_ */
#else	/* GETUTENT */
  strncpy(u.ut_line, line, sizeof(u.ut_line));
  strncpy(u.ut_name, LoginName, sizeof(u.ut_name));
# ifdef UTHOST
  strncpy(u.ut_host, host, sizeof(u.ut_host));
# endif	/* UTHOST */
# ifdef MIPS
  u.ut_type = 7; /* USER_PROCESS */
  strncpy(u.ut_id, line + 3, 4);
# endif /* MIPS */
  (void) time(&u.ut_time);
# ifdef sequent
/* call sequent undocumented routine to count logins and add utmp entry if possible */
  if (add_utmp(slot, &u) == -1)
# else
  (void) lseek(utmpf, (off_t) (slot * sizeof u), 0);
  if (write(utmpf, (char *) &u, sizeof u) != sizeof u)
# endif /* sequent */
#endif	/* GETUTENT */

    {
#ifdef NETHACK
      if (nethackflag)
        Msg(errno, "%s is too hard to dig in.", UTMPFILE);
      else
#endif
      Msg(errno,"Could not write %s.", UTMPFILE);
#ifdef apollo
      close(utmpf);
#endif
      return -1;
    }
  debug("SetUtmp successful\n");
  wi->slot = slot;
#ifdef apollo
  close(utmpf);
#endif
  return 0;
}

#ifdef MIPS

#define GETTTYENT
static int ttyfd = 0;

static void setttyent()
{
  if (ttyfd)
    close(ttyfd);
  ttyfd = open(UtmpName, O_RDONLY);
}

static struct ttyent *getttyent()
{
  static struct utmp u;
  static struct ttyent t;

  if (!ttyfd)
    return NULL;
  
  if (read(ttyfd, &u, sizeof u)) 
    {
      t.ty_name = u.ut_line;
      return &t;
    }
  return NULL;
}

CreateUtmp(name)
char *name;
{
  int slot;
  struct utmp u;

  strncpy(u.ut_line, name, 8);
  strncpy(u.ut_name, LoginName, 8);
  u.ut_type = 7; /* USER_PROCESS */
  strncpy(u.ut_id, name+3, 4);
  (void) time(&u.ut_time);
  slot = (lseek(utmpfappend, 0, 2) + 1) / sizeof u;
  (void) write(utmpfappend, (char *)&u, sizeof u);
  close(utmpfappend);
  if ((utmpfappend = open(UtmpName, O_APPEND)) == -1) 
    {
      if (errno != EACCES)
        Msg(errno, UtmpName);
      return;
    }
  return slot;
}
#endif /* MIPS */

/*
 * if slot could be removed or was 0,  wi->slot = -1;
 * else not changed.
 */
int
RemoveUtmp(wi)
struct win *wi;
{
#ifdef GETUTENT
  struct utmp *uu;
#endif
#ifdef apollo
  struct utmp *up;
#endif
  struct utmp u;
  slot_t slot;

  slot = wi->slot;
#ifdef GETUTENT
  debug1("RemoveUtmp(%s)\n", (slot == (slot_t) 0) ?
         "no slot (0)":((slot == (slot_t) -1) ? "no slot (-1)" : slot));
#else
  debug1("RemoveUtmp(wi.slot: %d)\n", slot);
#endif
#ifdef apollo
  InitUtmp();
  up = (struct utmp *)malloc(sizeof(struct utmp));
  bzero((char *)up, sizeof(struct utmp));
#endif /* apollo */
  if (!utmp)
    return -1;
  if (slot == (slot_t) 0 || slot == (slot_t) -1)
    {
      debug1("There is no utmp-slot to be removed(%d)\n", slot);
      wi->slot = (slot_t) -1;
      return 0;
    }
  bzero((char *) &u, sizeof u);
#ifdef GETUTENT
  setutent();
  strncpy(u.ut_line, slot, sizeof(u.ut_line));
  if ((uu = getutline(&u)) == 0)
    {
      Msg(0, "Utmp slot not found -> not removed");
      return -1;
    }
# ifdef _SEQUENT_
  if (ut_delete_user(slot, uu->ut_pid, 0, 0) == 0)
# else
  uu->ut_type = DEAD_PROCESS;
  uu->ut_exit.e_termination = 0;
  uu->ut_exit.e_exit= 0;
  if (pututline(uu) == 0)
# endif
#else	/* GETUTENT */
  (void) lseek(utmpf, (off_t) (slot * sizeof u), 0);
# ifdef apollo
  if (read(utmpf, (char *) up, sizeof u) != sizeof u)
    {
      DeadlyMsg = 0;
      Msg(errno, "cannot read %s?", UTMPFILE);
      sleep(1);
    }
  (void) lseek(utmpf, (off_t) (slot * sizeof u), 0);
  bzero(up->ut_name, sizeof(u.ut_name));
  bzero(up->ut_host, sizeof(u.ut_host));
  if (write(utmpf, (char *)up, sizeof u) != sizeof u)
# else
  if (write(utmpf, (char *) &u, sizeof u) != sizeof u)
# endif /* apollo */
#endif
    {
#ifdef NETHACK
      if (nethackflag)
        Msg(errno, "%s is too hard to dig in.", UTMPFILE);
      else
#endif
      Msg(errno,"Could not write %s.", UTMPFILE);
#ifdef apollo
      close(utmpf);
      free(up);
#endif
      return -1;
    }
  debug("RemoveUtmp successfull\n");
  wi->slot = (slot_t) -1;
#ifdef apollo
  close(utmpf);
  free(up);
#endif
  return 0;
}

#endif	/* UTMPOK */

#if !defined(GETTTYENT) && !defined(GETUTENT)

static void setttyent()
{
  struct stat s;
  register int f;
  register char *p, *ep;

  if (ttnext)
    {
      ttnext = tt;
      return;
    }
  if ((f = open(ttys, O_RDONLY)) == -1 || fstat(f, &s) == -1)
    Msg(errno, ttys);
  if ((tt = malloc((unsigned) s.st_size + 1)) == 0)
    Msg_nomem;
  if (read(f, tt, s.st_size) != s.st_size)
    Msg(errno, ttys);
  close(f);
  for (p = tt, ep = p + s.st_size; p < ep; ++p)
    if (*p == '\n')
      *p = '\0';
  *p = '\0';
  ttnext = tt;
}

static struct ttyent *getttyent()
{
  static struct ttyent t;

  if (*ttnext == '\0')
    return NULL;
  t.ty_name = ttnext + 2;
  ttnext += strlen(ttnext) + 1;
  return &t;
}

#endif	/* GETTTYENT */

#ifdef LOADAV
# ifdef LOADAV_NEXT
void
InitNeXTLoadAvg()
{
  error = processor_set_default(host_self(), &default_set);
  if (error != KERN_SUCCESS)
    mach_error("Error calling processor_set_default", error);
  else
    avenrun = 1;
}

int
GetAvenrun()
{
  info_count = PROCESSOR_SET_BASIC_INFO_COUNT;
  error = processor_set_info(default_set, PROCESSOR_SET_BASIC_INFO, &host,
			     (processor_set_info_t)&info, &info_count);
  if (error != KERN_SUCCESS)
    {
      mach_error("Error calling processor_set_info", error);
      return 0;
    }
  else
    {
      loadav = (float)info.load_average / LOAD_SCALE;
      return 1;
    }
}

# else /* need kmem for load avg */

void
InitKmem()
{
  debug("Init Kmem...\n");
#  ifndef apollo
  if ((kmemf = open(KmemName, O_RDONLY)) == -1)
    return;
  debug("Kmem opened\n");
  nl[0].n_name = AvenrunSym;
  debug2("Searching in %s for %s\n", UnixName, nl[0].n_name);
  nlist(UnixName, nl);
  if (/* nl[0].n_type == 0 || */ nl[0].n_value == 0)
    {
      close(kmemf);
      return;
    }
#   ifdef sgi
  nl[0].n_value &= ~(1 << 31); /* clear upper bit */
#   endif /* sgi */
  debug("AvenrunSym found!!\n");
#  endif /* apollo */
  avenrun = 1;
}

int
GetAvenrun()
{
# ifdef apollo
  int load[3];
  register int i;

  proc1_$get_loadav(load);
  for (i = 0; i < 3; i++)
    loadav[i] = (double)load[i] / 65536.0;
# else
  if (lseek(kmemf, (off_t) nl[0].n_value, 0) == (off_t) - 1)
    return 0;
  if (read(kmemf, (char *) loadav, sizeof loadav) != sizeof loadav)
    return 0;
# endif /* apollo */

  return 1;
}

# endif /* !NeXT, need kmem for load avg */
#endif	/* LOADAV */

/*
 * (Almost) secure open and fopen... mlschroe.
 */

FILE *
secfopen(name, mode)
char *name;
char *mode;
{
  FILE *fi;
#ifdef NOREUID
  int flags, fd;
#endif

  debug2("secfopen(%s, %s)\n", name, mode);
  if (eff_uid == real_uid)
    return(fopen(name, mode));
#ifndef NOREUID
  setreuid(eff_uid, real_uid);
  setregid(eff_gid, real_gid);
  fi = fopen(name, mode);
  setreuid(real_uid, eff_uid);
  setregid(real_gid, eff_gid);
#else
  if (mode[0] && mode[1] == '+')
    flags = O_RDWR;
  else
    flags = (mode[0] == 'r') ? O_RDONLY : O_WRONLY;
  if (mode[0] == 'w')
    flags |= O_CREAT | O_TRUNC;
  else if (mode[0] == 'a')
    flags |= O_CREAT | O_APPEND;
  else if (mode[0] != 'r')
    {
      errno = EINVAL;
      return(0);
    }
  if ((fd = secopen(name, flags, 0666)) < 0)
    return(0);
  if ((fi = fdopen(fd, mode)) == 0)
    {
      close(fd);
      return(0);
    }
#endif
  return(fi);
}


int
secopen(name, flags, mode)
char *name;
int flags;
int mode;
{
  int fd;
#ifdef NOREUID
  int q;
  struct stat stb;
#endif

  debug3("secopen(%s, 0x%x, 0%03o)\n", name, flags, mode);
  if (eff_uid == real_uid)
    return(open(name, flags, mode));
#ifndef NOREUID
  setreuid(eff_uid, real_uid);
  setregid(eff_gid, real_gid);
  fd = open(name, flags, mode);
  setreuid(real_uid, eff_uid);
  setregid(real_gid, eff_gid);
#else
  /* Truncation/creation is done in UserContext */
  if ((flags & O_TRUNC) || ((flags & O_CREAT) && access(name, F_OK)))
    {
      if (UserContext() > 0)
	{
          if ((fd = open(name, flags, mode)) >= 0)
	    {
	      close(fd);
	      UserReturn(0);
            }
	  if (errno == 0)
	    errno = EACCES;
	  UserReturn(errno);
	}
      if (q = UserStatus())
	{
	  if (q > 0)
	    errno = q;
          return(-1);
	}
    }
  if (access(name, F_OK))
    return(-1);
  if ((fd = open(name, flags & ~(O_TRUNC | O_CREAT), 0)) < 0)
    return(-1);
  debug("open successful\n");
  if (fstat(fd, &stb))
    {
      close(fd);
      return(-1);
    }
  debug("fstat successful\n");
  if (stb.st_uid != real_uid)
    {
      switch (flags & (O_RDONLY | O_WRONLY | O_RDWR))
        {
	case O_RDONLY:
	  q = 0004;
	  break;
	case O_WRONLY:
	  q = 0002;
	  break;
	default:
	  q = 0006;
	  break;
        }
      if ((stb.st_mode & q) != q)
	{
          debug("secopen: permission denied\n");
	  close(fd);
	  errno = EACCES;
	  return(-1);
	}
    }
#endif
  debug1("secopen ok - returning %d\n", fd);
  return(fd);
}

#ifdef BUGGYGETLOGIN
char *
getlogin()
{
  char *tty;
#ifdef utmp
# undef utmp
#endif
  struct utmp u;
  static char retbuf[sizeof(u.ut_user)+1];
  int fd;

  for (fd = 0; fd <= 2 && (tty = ttyname(fd)) == NULL; fd++)
    ;
  if ((tty == NULL) || ((fd = open(UTMP_FILE, O_RDONLY)) < 0))
    return NULL;
  tty = stripdev(tty);
  retbuf[0] = '\0';
  while (read(fd, &u, sizeof(struct utmp)) == sizeof(struct utmp))
    {
      if (!strncmp(tty, u.ut_line, sizeof(u.ut_line)))
	{
	  strncpy(retbuf, u.ut_user, sizeof(u.ut_user));
	  retbuf[sizeof(u.ut_user)] = '\0';
	  if (u.ut_type == USER_PROCESS)
	    break;
	}
    }
  close(fd);

  return *retbuf ? retbuf : NULL;
}
#endif
