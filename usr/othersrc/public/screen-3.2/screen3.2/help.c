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
  static char rcs_id[] = "$Id: help.c,v 1.2 92/02/03 02:28:33 jnweiger Exp $ FAU";
#endif

#include "config.h"
#include <stdio.h>
#include <sys/types.h>

#ifdef BSDI
# include <sys/signal.h>
#endif /* BSDI */

#include "screen.h"
#include "ansi.h"
#include "extern.h"
#include "patchlevel.h"

int help_page = 0;
int command_search, command_bindings = 0;
extern char Esc, MetaEsc;
extern char *KeyNames[];
extern struct key ktab[];
extern int screenwidth, screenheight;
extern char *blank, *null, *CE;
extern struct win *fore;

static void centerline __P((char *));
static void HelpRedisplayLine __P((int, int, int, int));
static void process_help_input __P((char **, int *));
static void AbortHelp __P((void));
static void add_key_to_buf __P((char *, int));

void
exit_with_usage(myname)
char *myname;
{
  printf("Use: %s [-opts] [cmd [args]]\n", myname);
  printf(" or: %s -r [host.tty]\n\nOptions:\n", myname);
  printf("-a           Force all capabilities into each window's termcap\n");
  printf("-A -[r|R]    Adapt all windows to the new display width & height\n");
  printf("-c file      Read configuration file instead of .screenrc\n");
#ifdef REMOTE_DETACH
  printf("-d (-r)      Detach the elsewhere running screen (and reattach here)\n");
  printf("-D (-r)      Detach and logout remote (and reattach here)\n");
#endif
  printf("-e xy        Change command characters\n");
  printf("-f           Flow control on, -fn = off, -fa = auto\n");
  printf("-h lines     Set the size of the scrollback history buffer\n");
  printf("-i           Interrupt output sooner when flow control is on\n");
  printf("-l           Login mode on (update %s), -ln = off\n", UTMPFILE);
  printf("-list        or -ls. Do nothing, just list our SockDir\n");
  printf("-L           Terminal's last character can be safely updated\n");
  printf("-O           Choose optimal output rather than exact vt100 emulation\n");
  printf("-q           Quiet startup. Sets $status if unsuccessful.\n");
  printf("-r           Reattach to a detached screen process\n");
  printf("-R           Reattach if possible, otherwise start a new session\n");
  printf("-s shell     Shell to execute rather than $SHELL\n");
  printf("-T term      Use term as $TERM for windows, rather than \"screen\"\n");
  printf("-t title     Set command's a.k.a. (window title)\n");
  printf("-wipe        Do nothing, just clean up SockDir\n");
  exit(1);
}

/* Esc-char is not consumed. All others are. Esc-char, space, and return end */
static void
process_help_input(ppbuf, plen)
char **ppbuf;
int *plen;
{
  int done = 0;

  if (ppbuf == 0)
    {
      AbortHelp();
      return;
    }
  while (!done && *plen > 0)
    {
      switch (**ppbuf)
	{
	case ' ':
	  if (display_help() == 0)
            break;
	  /* FALLTHROUGH */
	case '\r':
	case '\n':
	  done = 1;
	  break;
	default:
	  if (**ppbuf == Esc)
	    {
	      done = 1;
	      continue;
	    }
	  break;
	}
      ++*ppbuf;
      --*plen;
    }
  if (done)
    AbortHelp();
}

static void
AbortHelp()
{
  help_page = 0;
  ExitOverlayPage();
  Activate(0);
}

static int maxrow, grow, numcols, numrows, num_names;
static int numskip, numpages;

int
display_help()
{
  int col, crow, n, key = 0;
  enum keytype typ = ktab[0].type;
  char buf[256], Esc_buf[5], cbuf[256];

  if (!help_page++)
    {
      if (screenwidth < 26 || screenheight < 6)
        {
	  Msg(0, "Window size too small for help page");
	  help_page = 0;
	  return -1;
        }
      InitOverlayPage(process_help_input, HelpRedisplayLine, (int (*)())0, 0);

      command_bindings = 0;
      for (key = 0; key < 256; key++)
        if ((typ = ktab[key].type) == KEY_CREATE
	    || typ == KEY_SCREEN
	    || typ == KEY_SET
	    || (typ == KEY_AKA && ktab[key].args))
	  command_bindings++;
      debug1("help: command_bindings counted: %d\n",command_bindings);
      for (n = 0; KeyNames[n] != NULL; n++)
	; /* we dont know "sizeof * KeyNames" */
      num_names = n - 1;
      debug1("help: we find %d named keys (+1).\n", num_names);
      command_search = 0;

      numcols = screenwidth/26;
      if (numcols == 0)
        numcols = 1;
      numrows = (num_names + numcols -1) / numcols;
      debug1("Numrows: %d\n", numrows);
      numskip = screenheight-5 - (2 + numrows);
      while (numskip < 0)
	numskip += screenheight-5;
      numskip %= screenheight-5;
      debug1("Numskip: %d\n", numskip);
      if (numskip > screenheight/3 || numskip > command_bindings)
	numskip = 1;
      maxrow = 2 + numrows + numskip + command_bindings;
      grow = 0;

      numpages = (maxrow + screenheight-6) / (screenheight-5);
    }

  if (grow >= maxrow)
    { 
      return(-1);
    }

  /* Clear the help screen */
  ClearDisplay();
  
  sprintf(cbuf,"Screen key bindings, page %d of %d.", help_page, numpages);
  centerline(cbuf);
  printf("\n");
  crow = 2;

  *Esc_buf = '\0';
  add_key_to_buf(Esc_buf, Esc);
  Esc_buf[strlen(Esc_buf) - 1] = '\0';

  for (; crow < screenheight - 3; crow++)
    {
      if (grow < 1)
        {
   	  *buf = '\0';
          add_key_to_buf(buf, MetaEsc);
          buf[strlen(buf) - 1] = '\0';
          sprintf(cbuf,"Command key:  %s   Literal %s:  %s", Esc_buf, Esc_buf, buf);
          centerline(cbuf);
	  grow++;
        }
      else if (grow >= 2 && grow-2 < numrows)
	{
	  for (col = 0; col < numcols && (n = numrows * col + (grow-2)) < num_names; col++)
	    {
	      debug1("help: searching key %d\n", n);
	      buf[0] = '\0';
	      for (key = 0; key < 128; key++)
		if (ktab[key].type == (enum keytype) (n + 2)
		    && ((enum keytype) (n + 2) != KEY_AKA || !ktab[key].args) )
		  add_key_to_buf(buf, key);
	      buf[14] = '\0';
	      /*
	       * Format is up to 10 chars of name, 1 spaces, 14 chars of key
	       * bindings, and a space.
	       */
	      printf("%-10.10s %-14.14s ", KeyNames[n + 1], buf);
	    }
	  printf("\r\n");
          grow++;
        }
      else if (grow-2-numrows >= numskip 
               && grow-2-numrows-numskip < command_bindings)
        {
          char **pp, *cp;

	  while (command_search < 128
		 && (typ = ktab[command_search].type) != KEY_CREATE
		 && typ != KEY_SCREEN
		 && typ != KEY_SET
		 && (typ != KEY_AKA || !ktab[command_search].args))
	    command_search++;
	  buf[0] = '\0';
	  add_key_to_buf(buf, command_search);
	  printf("%-4s", buf);
	  col = 4;
	  if (typ != KEY_CREATE)
	    {
	      col += strlen(KeyNames[(int)typ - 1]) + 1;
	      printf("%s ", KeyNames[(int)typ - 1]);
	    }
	  pp = ktab[command_search++].args;
	  while (pp && (cp = *pp) != NULL)
	    {
	      if (!*cp || (index(cp, ' ') != NULL))
		{
		  if (index(cp, '\'') != NULL)
		    *buf = '"';
		  else
		    *buf = '\'';
		  sprintf(buf + 1, "%s%c", cp, *buf);
		  cp = buf;
		}
	      if ((col += strlen(cp) + 1) >= screenwidth)
		{
		  col = screenwidth - (col - (strlen(cp) + 1)) - 2;
		  if (col >= 0)
		    {
		      n = cp[col];
		      cp[col] = '\0';
		      printf("%s$", *pp);
		      cp[col] = (char) n;
	  	    }
	          break;
	        }
	      printf("%s%c", cp, (screenwidth - col != 1 || !pp[1]) ? ' ' : '$');
	      pp++;
	    }
	  printf("\r\n");
	  grow++;
	}
      else
	{
          putchar('\n');
	  grow++;
	}
    }
  printf("\n");
  sprintf(cbuf,"[Press Space %s Return to end; %s to begin a command.]",
	 grow < maxrow ? "for next page;" : "or", Esc_buf);
  centerline(cbuf);
  fflush(stdout);
  SetLastPos(0, screenheight-1);
  return(0);
}

static void
add_key_to_buf(buf, key)
char *buf;
int key;
{
  debug1("help: key found: %c\n", key);
  switch (key)
    {
    case ' ':
      strcat(buf, "sp ");
      break;
    case 0x7f:
      strcat(buf, "^? ");
      break;
    default:
      if (key < ' ')
	sprintf(buf + strlen(buf), "^%c ", (key | 0x40));
      else
	sprintf(buf + strlen(buf), "%c ", key);
      break;
    }
}

static void
centerline(str)
char *str;
{
  int l;
  l = (screenwidth - 1 + strlen(str)) / 2;
  if (l > screenwidth - 1)
    l = screenwidth - 1;
  printf("%*.*s\r\n", l, l, str);
}

static void
HelpRedisplayLine(y, xs, xe, isblank)
int y, xs, xe, isblank;
{
  if (isblank)
    return;
  if (CE)
    {
      GotoPos(xs, y);
      PutStr(CE);
      return;
    }
  DisplayLine(null, null, null, blank, null, null, y, xs, xe);
}

/*
 * here all the copyright stuff 
 */


static char version[40];

static char cpmsg[] = "\
\n\
iScreen version %v\n\
\n\
Copyright (c) 1991\n\
    Juergen Weigert (jnweiger@immd4.informatik.uni-erlangen.de)\n\
    Michael Schroeder (mlschroe@immd4.informatik.uni-erlangen.de)\n\
Copyright (c) 1987 Oliver Laumann\n\
\n\
This program is free software; you can redistribute it and/or \
modify it under the terms of the GNU General Public License as published \
by the Free Software Foundation; either version 1, or (at your option) \
any later version.\n\
\n\
This program is distributed in the hope that it will be useful, \
but WITHOUT ANY WARRANTY; without even the implied warranty of \
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the \
GNU General Public License for more details.\n\
\n\
You should have received a copy of the GNU General Public License \
along with this program (see the file COPYING); if not, write to the \
Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.\n";


static void process_copyright_input __P((char **, int *));
static void AbortCopyright __P((void));
static void copypage __P((void));

static char *cps, *savedcps;

static void
process_copyright_input(ppbuf, plen)
char **ppbuf;
int *plen;
{
  int done = 0;

  if (ppbuf == 0)
    {
      AbortCopyright();
      return;
    }
  while (!done && *plen > 0)
    {
      switch (**ppbuf)
	{
	case ' ':
          if (*cps)
	    {
	      copypage();
	      break;
	    }
	  /* FALLTHROUGH */
	case '\r':
	case '\n':
	  AbortCopyright();
	  done = 1;
	  break;
	default:
	  break;
	}
      ++*ppbuf;
      --*plen;
    }
}

static void
AbortCopyright()
{
  ExitOverlayPage();
  Activate(0);
}

void
display_copyright()
{
  if (screenwidth < 10 || screenheight < 5)
    {
      Msg(0, "Window size too small for copyright page");
      return;
    }
  InitOverlayPage(process_copyright_input, HelpRedisplayLine, (int (*)())0, 0);
  sprintf(version, "%d.%.2d.%.2d%s (%s) %s", REV, VERS, PATCHLEVEL, STATE, ORIGIN, DATE);
  cps = cpmsg;
  savedcps = 0;
  copypage();
}


static void
copypage()
{
  char *ws;
  int x, y, l;
  char cbuf[80];

  ClearDisplay();
  x = y = 0;
  while(*cps)
    {
      ws = cps;
      while (*cps == ' ')
	cps++;
      if (strncmp(cps, "%v", 2) == 0)
	{
	  savedcps = cps + 2;
	  ws = cps = version;
	}
      while (*cps && *cps != ' ' && *cps != '\n')
	cps++;
      l = cps - ws;
      cps = ws;
      if (l > screenwidth - 1)
	l = screenwidth - 1;
      if (x && x + l >= screenwidth - 2)
	{
	  printf("\r\n");
	  x = 0;
	  if (++y > screenheight - 4)
            break;
	}
      if (x)
	{
	  putchar(' ');
	  x++;
	}
      if (l)
        printf("%*.*s", l, l, ws);
      x += l;
      cps += l;
      if (*cps == 0 && savedcps)
	{
	  cps = savedcps;
	  savedcps = 0;
	}
      if (*cps == '\n')
	{
	  printf("\r\n");
	  x = 0;
	  if (++y > screenheight - 4)
            break;
	}
      if (*cps == ' ' || *cps == '\n')
	cps++;
    }
  while (y++ < screenheight - 2)
    printf("\r\n");
  sprintf(cbuf,"[Press Space %s Return to end.]",
	 *cps ? "for next page;" : "or");
  centerline(cbuf);
  fflush(stdout);
  SetLastPos(0, screenheight-1);
}
  
