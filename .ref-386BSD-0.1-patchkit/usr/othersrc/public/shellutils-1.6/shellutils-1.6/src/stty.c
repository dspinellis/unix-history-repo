/* stty -- change and print terminal line settings
   Copyright (C) 1990, 1991 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Usage: stty [-ag] [--all] [--save] [setting...]

   Options:
   -a, --all	Write all current settings to stdout in human-readable form.
   -g, --save	Write all current settings to stdout in stty-readable form.

   If no args are given, write to stdout the baud rate and settings that
   have been changed from their defaults.  Mode reading and changes
   are done on stdin.

   David MacKenzie <djm@ai.mit.edu> */

#include <stdio.h>
#include <sys/types.h>
#include <termios.h>
#ifdef _AIX
#include <sys/ioctl.h>		/* Needed to get window size. */
#endif
#ifdef WINSIZE_IN_PTEM
#include <sys/stream.h>
#include <sys/ptem.h>
#endif
#include <getopt.h>
#ifdef __STDC__
#include <stdarg.h>
#define VA_START(args, lastarg) va_start(args, lastarg)
#else
#include <varargs.h>
#define VA_START(args, lastarg) va_start(args)
#endif
#include "system.h"

#if defined(GWINSZ_BROKEN)	/* Such as for SCO UNIX 3.2.2. */
#undef TIOCGWINSZ
#endif

#ifndef _POSIX_VDISABLE
#define _POSIX_VDISABLE ((unsigned char) 0)
#endif

#define	Control(c) ((c) & 0x1f)
/* Canonical values for control characters. */
#ifndef CINTR
#define	CINTR Control ('c')
#endif
#ifndef CQUIT
#define	CQUIT 28
#endif
#ifndef CERASE
#define	CERASE 127
#endif
#ifndef CKILL
#define	CKILL Control ('u')
#endif
#ifndef CEOF
#define	CEOF Control ('d')
#endif
#ifndef CEOL
#define	CEOL _POSIX_VDISABLE
#endif
#ifndef CSTART
#define	CSTART Control ('q')
#endif
#ifndef CSTOP
#define	CSTOP Control ('s')
#endif
#ifndef CSUSP
#define	CSUSP Control ('z')
#endif
#if defined(VEOL2) && !defined(CEOL2)
#define	CEOL2 _POSIX_VDISABLE
#endif
#if defined(VSWTCH) && !defined(CSWTCH)
#define	CSWTCH _POSIX_VDISABLE
#endif
#if defined(VDSUSP) && !defined (CDSUSP)
#define	CDSUSP Control ('y')
#endif
#if defined(VREPRINT) && !defined(CRPRNT)
#define	CRPRNT Control ('r')
#endif
#if defined(VWERASE) && !defined(CWERASE)
#define	CWERASE Control ('w')
#endif
#if defined(VLNEXT) && !defined(CLNEXT)
#define	CLNEXT Control ('v')
#endif

char *visible ();
unsigned long baud_to_value ();
int recover_mode ();
int screen_columns ();
int set_mode ();
int string_to_baud ();
long integer_arg ();
tcflag_t *mode_type_flag ();
void display_all ();
void display_changed ();
void display_recoverable ();
void display_settings ();
void display_speed ();
void display_window_size ();
void error ();
void sane_mode ();
void set_control_char ();
void set_speed ();
void set_window_size ();

enum speed_setting
{
  input_speed, output_speed, both_speeds
};

enum output_type
{
  changed, all, recoverable
};

enum mode_type
{
  control, input, output, local, combination
};

/* Flags for `struct mode_info'. */
#define SANE_SET 1		/* Set in `sane' mode. */
#define SANE_UNSET 2		/* Unset in `sane' mode. */
#define REV 4			/* Can be turned off by prepending `-'. */
#define OMIT 8			/* Don't display value. */

struct mode_info
{
  char *name;
  enum mode_type type;
  char flags;
  unsigned long bits;
  unsigned long mask;
};

struct mode_info mode_info[] =
{
  {"parenb", control, REV, PARENB, 0},
  {"parodd", control, REV, PARODD, 0},
  {"cs5", control, 0, CS5, CSIZE},
  {"cs6", control, 0, CS6, CSIZE},
  {"cs7", control, 0, CS7, CSIZE},
  {"cs8", control, 0, CS8, CSIZE},
  {"hupcl", control, REV, HUPCL, 0},
  {"hup", control, REV | OMIT, HUPCL, 0},
  {"cstopb", control, REV, CSTOPB, 0},
  {"cread", control, SANE_SET | REV, CREAD, 0},
  {"clocal", control, REV, CLOCAL, 0},
#ifdef CRTSCTS
  {"crtscts", control, REV, CRTSCTS, 0},
#endif

  {"ignbrk", input, SANE_UNSET | REV, IGNBRK, 0},
  {"brkint", input, SANE_SET | REV, BRKINT, 0},
  {"ignpar", input, REV, IGNPAR, 0},
  {"parmrk", input, REV, PARMRK, 0},
  {"inpck", input, REV, INPCK, 0},
  {"istrip", input, REV, ISTRIP, 0},
  {"inlcr", input, SANE_UNSET | REV, INLCR, 0},
  {"igncr", input, SANE_UNSET | REV, IGNCR, 0},
  {"icrnl", input, SANE_SET | REV, ICRNL, 0},
  {"ixon", input, REV, IXON, 0},
  {"ixoff", input, SANE_UNSET | REV, IXOFF, 0},
  {"tandem", input, REV | OMIT, IXOFF, 0},
#ifdef IUCLC
  {"iuclc", input, SANE_UNSET | REV, IUCLC, 0},
#endif
#ifdef IXANY
  {"ixany", input, SANE_UNSET | REV, IXANY, 0},
#endif
#ifdef IMAXBEL
  {"imaxbel", input, SANE_SET | REV, IMAXBEL, 0},
#endif

  {"opost", output, SANE_SET | REV, OPOST, 0},
#ifdef OLCUC
  {"olcuc", output, SANE_UNSET | REV, OLCUC, 0},
#endif
#ifdef OCRNL
  {"ocrnl", output, SANE_UNSET | REV, OCRNL, 0},
#endif
#ifdef ONLCR
  {"onlcr", output, SANE_SET | REV, ONLCR, 0},
#endif
#ifdef ONOCR
  {"onocr", output, SANE_UNSET | REV, ONOCR, 0},
#endif
#ifdef ONLRET
  {"onlret", output, SANE_UNSET | REV, ONLRET, 0},
#endif
#ifdef OFILL
  {"ofill", output, SANE_UNSET | REV, OFILL, 0},
#endif
#ifdef OFDEL
  {"ofdel", output, SANE_UNSET | REV, OFDEL, 0},
#endif
#ifdef NLDLY
  {"nl1", output, SANE_UNSET, NL1, NLDLY},
  {"nl0", output, SANE_SET, NL0, NLDLY},
#endif
#ifdef CRDLY
  {"cr3", output, SANE_UNSET, CR3, CRDLY},
  {"cr2", output, SANE_UNSET, CR2, CRDLY},
  {"cr1", output, SANE_UNSET, CR1, CRDLY},
  {"cr0", output, SANE_SET, CR0, CRDLY},
#endif
#ifdef TABDLY
  {"tab3", output, SANE_UNSET, TAB3, TABDLY},
  {"tab2", output, SANE_UNSET, TAB2, TABDLY},
  {"tab1", output, SANE_UNSET, TAB1, TABDLY},
  {"tab0", output, SANE_SET, TAB0, TABDLY},
#endif
#ifdef BSDLY
  {"bs1", output, SANE_UNSET, BS1, BSDLY},
  {"bs0", output, SANE_SET, BS0, BSDLY},
#endif
#ifdef VTDLY
  {"vt1", output, SANE_UNSET, VT1, VTDLY},
  {"vt0", output, SANE_SET, VT0, VTDLY},
#endif
#ifdef FFDLY
  {"ff1", output, SANE_UNSET, FF1, FFDLY},
  {"ff0", output, SANE_SET, FF0, FFDLY},
#endif

  {"isig", local, SANE_SET | REV, ISIG, 0},
  {"icanon", local, SANE_SET | REV, ICANON, 0},
#ifdef IEXTEN
  {"iexten", local, SANE_SET | REV, IEXTEN, 0},
#endif
  {"echo", local, SANE_SET | REV, ECHO, 0},
  {"echoe", local, SANE_SET | REV, ECHOE, 0},
  {"crterase", local, REV | OMIT, ECHOE, 0},
  {"echok", local, SANE_SET | REV, ECHOK, 0},
  {"echonl", local, SANE_UNSET | REV, ECHONL, 0},
  {"noflsh", local, SANE_UNSET | REV, NOFLSH, 0},
#ifdef XCASE
  {"xcase", local, SANE_UNSET | REV, XCASE, 0},
#endif
#ifdef TOSTOP
  {"tostop", local, SANE_UNSET | REV, TOSTOP, 0},
#endif
#ifdef ECHOPRT
  {"echoprt", local, SANE_UNSET | REV, ECHOPRT, 0},
  {"prterase", local, REV | OMIT, ECHOPRT, 0},
#endif
#ifdef ECHOCTL
  {"echoctl", local, SANE_SET | REV, ECHOCTL, 0},
  {"ctlecho", local, REV | OMIT, ECHOCTL, 0},
#endif
#ifdef ECHOKE
  {"echoke", local, SANE_SET | REV, ECHOKE, 0},
  {"crtkill", local, REV | OMIT, ECHOKE, 0},
#endif

  {"evenp", combination, REV | OMIT, 0, 0},
  {"parity", combination, REV | OMIT, 0, 0},
  {"oddp", combination, REV | OMIT, 0, 0},
  {"nl", combination, REV | OMIT, 0, 0},
  {"ek", combination, OMIT, 0, 0},
  {"sane", combination, OMIT, 0, 0},
  {"cooked", combination, REV | OMIT, 0, 0},
  {"raw", combination, REV | OMIT, 0, 0},
  {"pass8", combination, REV | OMIT, 0, 0},
  {"litout", combination, REV | OMIT, 0, 0},
  {"cbreak", combination, REV | OMIT, 0, 0},
#ifdef IXANY
  {"decctlq", combination, REV | OMIT, 0, 0},
#endif
#ifdef TABDLY
  {"tabs", combination, REV | OMIT, 0, 0},
#endif
#if defined(XCASE) && defined(IUCLC) && defined(OLCUC)
  {"lcase", combination, REV | OMIT, 0, 0},
  {"LCASE", combination, REV | OMIT, 0, 0},
#endif
#if defined(ECHOCTL) && defined(ECHOKE)
  {"crt", combination, OMIT, 0, 0},
  {"dec", combination, OMIT, 0, 0},
#endif

  {NULL, control, 0, 0, 0}
};

struct control_info
{
  char *name;
  unsigned char saneval;
  int offset;
};

/* Control characters. */

struct control_info control_info[] =
{
  {"intr", CINTR, VINTR},
  {"quit", CQUIT, VQUIT},
  {"erase", CERASE, VERASE},
  {"kill", CKILL, VKILL},
  {"eof", CEOF, VEOF},
  {"eol", CEOL, VEOL},
#ifdef VEOL2
  {"eol2", CEOL2, VEOL2},
#endif
#ifdef VSWTCH
  {"swtch", CSWTCH, VSWTCH},
#endif
  {"start", CSTART, VSTART},
  {"stop", CSTOP, VSTOP},
  {"susp", CSUSP, VSUSP},
#ifdef VDSUSP
  {"dsusp", CDSUSP, VDSUSP},
#endif
#ifdef VREPRINT
  {"rprnt", CRPRNT, VREPRINT},
#endif
#ifdef VWERASE
  {"werase", CWERASE, VWERASE},
#endif
#ifdef VLNEXT
  {"lnext", CLNEXT, VLNEXT},
#endif

  /* These must be last because of the display routines. */
  {"min", 0, VMIN},
  {"time", 0, VTIME},
  {NULL, 0, 0}
};

/* The width of the screen, for output wrapping. */
int max_col;

/* Current position, to know when to wrap. */
int current_col;

struct option longopts[] =
{
  {"all", 0, NULL, 'a'},
  {"save", 0, NULL, 'g'},
  {NULL, 0, NULL, 0}
};

/* The name this program was run with. */
char *program_name;

/* Print format string MESSAGE and optional args.
   Wrap to next line first if it won't fit.
   Print a space first unless MESSAGE will start a new line. */

/* VARARGS */
void
#ifdef __STDC__
wrapf (char *message, ...)
#else
wrapf (message, va_alist)
     char *message;
     va_dcl
#endif
{
  va_list args;
  char buf[1024];		/* Plenty long for our needs. */
  int buflen;

  VA_START (args, message);
  vsprintf (buf, message, args);
  va_end (args);
  buflen = strlen (buf);
  if (current_col + buflen >= max_col)
    {
      putchar ('\n');
      current_col = 0;
    }
  if (current_col > 0)
    {
      putchar (' ');
      current_col++;
    }
  current_col += buflen;
  VA_START (args, message);
  vprintf (message, args);
  va_end (args);
}

void
main (argc, argv)
     int argc;
     char **argv;
{
  struct termios mode;
  enum output_type output_type = changed;
  int optc;

  program_name = argv[0];
  opterr = 0;

  while ((optc = getopt_long (argc, argv, "ag", longopts, (int *) 0)) != EOF)
    {
      if (optc == 'a')
	output_type = all;
      else if (optc == 'g')
	output_type = recoverable;
      else
	break;
    }

  if (tcgetattr (0, &mode))
    error (1, errno, "standard input");

  max_col = screen_columns ();
  current_col = 0;

  if (optind == argc)
    {
      if (optc == '?')
	error (1, 0, "invalid argument `%s'", argv[--optind]);
      display_settings (output_type, &mode);
      exit (0);
    }

  while (optind < argc)
    {
      int match_found = 0;
      int reversed = 0;
      int i;

      if (argv[optind][0] == '-')
	{
	  ++argv[optind];
	  reversed = 1;
	}
      for (i = 0; mode_info[i].name != NULL; ++i)
	{
	  if (!strcmp (argv[optind], mode_info[i].name))
	    {
	      match_found = set_mode (&mode_info[i], reversed, &mode);
	      break;
	    }
	}
      if (match_found == 0 && reversed)
	error (1, 0, "invalid argument `%s'", --argv[optind]);
      if (match_found == 0)
	{
	  for (i = 0; control_info[i].name != NULL; ++i)
	    {
	      if (!strcmp (argv[optind], control_info[i].name))
		{
		  if (optind == argc - 1)
		    error (1, 0, "missing argument to `%s'", argv[optind]);
		  match_found = 1;
		  ++optind;
		  set_control_char (&control_info[i], argv[optind], &mode);
		  break;
		}
	    }
	}
      if (match_found == 0)
	{
	  if (!strcmp (argv[optind], "ispeed"))
	    {
	      if (optind == argc - 1)
		error (1, 0, "missing argument to `%s'", argv[optind]);
	      ++optind;
	      set_speed (input_speed, argv[optind], &mode);
	    }
	  else if (!strcmp (argv[optind], "ospeed"))
	    {
	      if (optind == argc - 1)
		error (1, 0, "missing argument to `%s'", argv[optind]);
	      ++optind;
	      set_speed (output_speed, argv[optind], &mode);
	    }
#ifdef TIOCGWINSZ
	  else if (!strcmp (argv[optind], "rows"))
	    {
	      if (optind == argc - 1)
		error (1, 0, "missing argument to `%s'", argv[optind]);
	      ++optind;
	      set_window_size ((int) integer_arg (argv[optind]), -1);
	    }
	  else if (!strcmp (argv[optind], "cols")
		   || !strcmp (argv[optind], "columns"))
	    {
	      if (optind == argc - 1)
		error (1, 0, "missing argument to `%s'", argv[optind]);
	      ++optind;
	      set_window_size (-1, (int) integer_arg (argv[optind]));
	    }
	  else if (!strcmp (argv[optind], "size"))
	    display_window_size (0);
#endif
#ifndef C_LINE_MISSING
	  else if (!strcmp (argv[optind], "line"))
	    {
	      if (optind == argc - 1)
		error (1, 0, "missing argument to `%s'", argv[optind]);
	      ++optind;
	      mode.c_line = integer_arg (argv[optind]);
	    }
#endif
	  else if (!strcmp (argv[optind], "speed"))
	    display_speed (&mode, 0);
	  else if (string_to_baud (argv[optind]) != -1)
	    set_speed (both_speeds, argv[optind], &mode);
	  else if (recover_mode (argv[optind], &mode) == 0)
	    error (1, 0, "invalid argument `%s'", argv[optind]);
	}
      optind++;
    }

  if (tcsetattr (0, TCSADRAIN, &mode))
    error (1, errno, "standard input");

  exit (0);
}

/* Return 0 if not applied because not reversible; otherwise return 1. */

int
set_mode (info, reversed, mode)
     struct mode_info *info;
     int reversed;
     struct termios *mode;
{
  tcflag_t *bitsp;

  if (reversed && (info->flags & REV) == 0)
    return 0;

  bitsp = mode_type_flag (info->type, mode);

  if (bitsp == NULL)
    {
      /* Combination mode. */
      if (!strcmp (info->name, "evenp") || !strcmp (info->name, "parity"))
	{
	  if (reversed)
	    mode->c_cflag = (mode->c_cflag & ~PARENB & ~CSIZE) | CS8;
	  else
	    mode->c_cflag = (mode->c_cflag & ~PARODD & ~CSIZE) | PARENB | CS7;
	}
      else if (!strcmp (info->name, "oddp"))
	{
	  if (reversed)
	    mode->c_cflag = (mode->c_cflag & ~PARENB & ~CSIZE) | CS8;
	  else
	    mode->c_cflag = (mode->c_cflag & ~CSIZE) | CS7 | PARODD | PARENB;
	}
      else if (!strcmp (info->name, "nl"))
	{
	  if (reversed)
	    mode->c_iflag = mode->c_iflag & ~(ICRNL | INLCR | IGNCR);
	  else
	    mode->c_iflag = mode->c_iflag | ICRNL;
	}
      else if (!strcmp (info->name, "ek"))
	{
	  mode->c_cc[VERASE] = CERASE;
	  mode->c_cc[VKILL] = CKILL;
	}
      else if (!strcmp (info->name, "sane"))
	sane_mode (mode);
      else if (!strcmp (info->name, "cbreak"))
	{
	  if (reversed)
	    mode->c_lflag |= ICANON;
	  else
	    mode->c_lflag &= ~ICANON;
	}
      else if (!strcmp (info->name, "pass8"))
	{
	  if (reversed)
	    {
	      mode->c_cflag = (mode->c_cflag & ~CSIZE) | CS7 | PARENB;
	      mode->c_iflag |= ISTRIP;
	    }
	  else
	    {
	      mode->c_cflag = (mode->c_cflag & ~PARENB & ~CSIZE) | CS8;
	      mode->c_iflag &= ~ISTRIP;
	    }
	}
      else if (!strcmp (info->name, "litout"))
	{
	  if (reversed)
	    {
	      mode->c_cflag = (mode->c_cflag & ~CSIZE) | CS7 | PARENB;
	      mode->c_iflag |= ISTRIP;
	      mode->c_oflag |= OPOST;
	    }
	  else
	    {
	      mode->c_cflag = (mode->c_cflag & ~PARENB & ~CSIZE) | CS8;
	      mode->c_iflag &= ~ISTRIP;
	      mode->c_oflag &= ~OPOST;
	    }
	}
      else if (!strcmp (info->name, "raw") || !strcmp (info->name, "cooked"))
	{
	  if ((info->name[0] == 'r' && reversed)
	      || (info->name[0] == 'c' && !reversed))
	    {
	      /* Cooked mode. */
	      mode->c_iflag |= BRKINT | IGNPAR | ISTRIP | ICRNL | IXON;
	      mode->c_oflag |= OPOST;
	      mode->c_lflag |= ISIG | ICANON;
#if VMIN == VEOF
	      mode->c_cc[VEOF] = CEOF;
#endif
#if VTIME == VEOL
	      mode->c_cc[VEOL] = CEOL;
#endif
	    }
	  else
	    {
	      /* Raw mode. */
	      mode->c_iflag = 0;
	      mode->c_oflag &= ~OPOST;
	      mode->c_lflag &= ~(ISIG | ICANON
#ifdef XCASE
				 | XCASE
#endif
				 );
	      mode->c_cc[VMIN] = 1;
	      mode->c_cc[VTIME] = 0;
	    }
	}
#ifdef IXANY
      else if (!strcmp (info->name, "decctlq"))
	{
	  if (reversed)
	    mode->c_iflag |= IXANY;
	  else
	    mode->c_iflag &= ~IXANY;
	}
#endif
#ifdef TABDLY
      else if (!strcmp (info->name, "tabs"))
	{
	  if (reversed)
	    mode->c_oflag = (mode->c_oflag & ~TABDLY) | TAB3;
	  else
	    mode->c_oflag = (mode->c_oflag & ~TABDLY) | TAB0;
	}
#endif
#if defined(XCASE) && defined(IUCLC) && defined(OLCUC)
      else if (!strcmp (info->name, "lcase")
	       || !strcmp (info->name, "LCASE"))
	{
	  if (reversed)
	    {
	      mode->c_lflag &= ~XCASE;
	      mode->c_iflag &= ~IUCLC;
	      mode->c_oflag &= ~OLCUC;
	    }
	  else
	    {
	      mode->c_lflag |= XCASE;
	      mode->c_iflag |= IUCLC;
	      mode->c_oflag |= OLCUC;
	    }
	}
#endif
#if defined(ECHOCTL) && defined(ECHOKE)
      else if (!strcmp (info->name, "crt"))
	mode->c_lflag |= ECHOE | ECHOCTL | ECHOKE;
      else if (!strcmp (info->name, "dec"))
	{
	  mode->c_cc[VINTR] = 3; /* ^C */
	  mode->c_cc[VERASE] = 127; /* DEL */
	  mode->c_cc[VKILL] = 21; /* ^U */
	  mode->c_lflag |= ECHOE | ECHOCTL | ECHOKE;
#ifdef IXANY
	  mode->c_iflag &= ~IXANY;
#endif
	}
#endif
    }
  else if (reversed)
    *bitsp = *bitsp & ~info->mask & ~info->bits;
  else
    *bitsp = (*bitsp & ~info->mask) | info->bits;

  return 1;
}

void
set_control_char (info, arg, mode)
     struct control_info *info;
     char *arg;
     struct termios *mode;
{
  unsigned char value;

  if (!strcmp (info->name, "min") || !strcmp (info->name, "time"))
    value = integer_arg (arg);
  else if (arg[0] == '\0' || arg[1] == '\0')
    value = arg[0];
  else if (!strcmp (arg, "^-") || !strcmp (arg, "undef"))
    value = _POSIX_VDISABLE;
  else if (arg[0] == '^' && arg[1] != '\0')	/* Ignore any trailing junk. */
    {
      if (arg[1] == '?')
	value = 127;
      else
	value = arg[1] & ~0140;	/* Non-letters get weird results. */
    }
  else
    value = integer_arg (arg);
  mode->c_cc[info->offset] = value;
}

void
set_speed (type, arg, mode)
     enum speed_setting type;
     char *arg;
     struct termios *mode;
{
  int baud;

  baud = string_to_baud (arg);
  if (type == input_speed || type == both_speeds)
    cfsetispeed (mode, baud);
  if (type == output_speed || type == both_speeds)
    cfsetospeed (mode, baud);
}

#ifdef TIOCGWINSZ
void
set_window_size (rows, cols)
     int rows, cols;
{
  struct winsize win;

  if (ioctl (0, TIOCGWINSZ, (char *) &win))
    error (1, errno, "standard input");
  if (rows >= 0)
    win.ws_row = rows;
  if (cols >= 0)
    win.ws_col = cols;
  if (ioctl (0, TIOCSWINSZ, (char *) &win))
    error (1, errno, "standard input");
}

void
display_window_size (fancy)
     int fancy;
{
  struct winsize win;

  if (ioctl (0, TIOCGWINSZ, (char *) &win))
    error (1, errno, "standard input");
  wrapf (fancy ? "rows %d; columns %d;" : "%d %d\n", win.ws_row, win.ws_col);
  if (!fancy)
    current_col = 0;
}
#endif

int
screen_columns ()
{
#ifdef TIOCGWINSZ
  struct winsize win;

  if (ioctl (0, TIOCGWINSZ, (char *) &win))
    error (1, errno, "standard input");
  if (win.ws_col > 0)
    return win.ws_col;
#endif
  if (getenv ("COLUMNS"))
    return atoi (getenv ("COLUMNS"));
  return 80;
}

tcflag_t *
mode_type_flag (type, mode)
     enum mode_type type;
     struct termios *mode;
{
  switch (type)
    {
    case control:
      return &mode->c_cflag;

    case input:
      return &mode->c_iflag;

    case output:
      return &mode->c_oflag;

    case local:
      return &mode->c_lflag;

    case combination:
      return NULL;
    }
}

void
display_settings (output_type, mode)
     enum output_type output_type;
     struct termios *mode;
{
  switch (output_type)
    {
    case changed:
      display_changed (mode);
      break;

    case all:
      display_all (mode);
      break;

    case recoverable:
      display_recoverable (mode);
      break;
    }
}

void
display_changed (mode)
     struct termios *mode;
{
  int i;
  int empty_line;
  tcflag_t *bitsp;
  unsigned long mask;
  enum mode_type prev_type = control;

  display_speed (mode, 1);
#ifndef C_LINE_MISSING
  wrapf ("line = %d;", mode->c_line);
#endif
  putchar ('\n');
  current_col = 0;

  empty_line = 1;
  for (i = 0; strcmp (control_info[i].name, "min"); ++i)
    {
      if (mode->c_cc[control_info[i].offset] == control_info[i].saneval)
	continue;
      empty_line = 0;
      wrapf ("%s = %s;", control_info[i].name,
	     visible (mode->c_cc[control_info[i].offset]));
    }
  if ((mode->c_lflag & ICANON) == 0)
    {
      wrapf ("min = %d; time = %d;\n", (int) mode->c_cc[VMIN],
	     (int) mode->c_cc[VTIME]);
    }
  else if (empty_line == 0)
    putchar ('\n');
  current_col = 0;

  empty_line = 1;
  for (i = 0; mode_info[i].name != NULL; ++i)
    {
      if (mode_info[i].flags & OMIT)
	continue;
      if (mode_info[i].type != prev_type)
	{
	  if (empty_line == 0)
	    {
	      putchar ('\n');
	      current_col = 0;
	      empty_line = 1;
	    }
	  prev_type = mode_info[i].type;
	}

      bitsp = mode_type_flag (mode_info[i].type, mode);
      mask = mode_info[i].mask ? mode_info[i].mask : mode_info[i].bits;
      if ((*bitsp & mask) == mode_info[i].bits)
	{
	  if (mode_info[i].flags & SANE_UNSET)
	    {
	      wrapf ("%s", mode_info[i].name);
	      empty_line = 0;
	    }
	}
      else if ((mode_info[i].flags & (SANE_SET | REV)) == (SANE_SET | REV))
	{
	  wrapf ("-%s", mode_info[i].name);
	  empty_line = 0;
	}
    }
  if (empty_line == 0)
    putchar ('\n');
  current_col = 0;
}

void
display_all (mode)
     struct termios *mode;
{
  int i;
  tcflag_t *bitsp;
  unsigned long mask;
  enum mode_type prev_type = control;

  display_speed (mode, 1);
#ifdef TIOCGWINSZ
  display_window_size (1);
#endif
#ifndef C_LINE_MISSING
  wrapf ("line = %d;", mode->c_line);
#endif
  putchar ('\n');
  current_col = 0;

  for (i = 0; strcmp (control_info[i].name, "min"); ++i)
    {
      wrapf ("%s = %s;", control_info[i].name,
	     visible (mode->c_cc[control_info[i].offset]));
    }
  wrapf ("min = %d; time = %d;\n", mode->c_cc[VMIN], mode->c_cc[VTIME]);
  current_col = 0;

  for (i = 0; mode_info[i].name != NULL; ++i)
    {
      if (mode_info[i].flags & OMIT)
	continue;
      if (mode_info[i].type != prev_type)
	{
	  putchar ('\n');
	  current_col = 0;
	  prev_type = mode_info[i].type;
	}

      bitsp = mode_type_flag (mode_info[i].type, mode);
      mask = mode_info[i].mask ? mode_info[i].mask : mode_info[i].bits;
      if ((*bitsp & mask) == mode_info[i].bits)
	wrapf ("%s", mode_info[i].name);
      else if (mode_info[i].flags & REV)
	wrapf ("-%s", mode_info[i].name);
    }
  putchar ('\n');
  current_col = 0;
}

void
display_speed (mode, fancy)
     struct termios *mode;
     int fancy;
{
  if (cfgetispeed (mode) == 0 || cfgetispeed (mode) == cfgetospeed (mode))
    wrapf (fancy ? "speed %lu baud;" : "%lu\n",
	   baud_to_value (cfgetospeed (mode)));
  else
    wrapf (fancy ? "ispeed %lu baud; ospeed %lu baud;" : "%lu %lu\n",
	   baud_to_value (cfgetispeed (mode)),
	   baud_to_value (cfgetospeed (mode)));
  if (!fancy)
    current_col = 0;
}

void
display_recoverable (mode)
     struct termios *mode;
{
  int i;

  printf ("%lx:%lx:%lx:%lx", mode->c_iflag, mode->c_oflag,
	  mode->c_cflag, mode->c_lflag);
  for (i = 0; i < NCCS; ++i)
    printf (":%x", (unsigned int) mode->c_cc[i]);
  putchar ('\n');
}

int
recover_mode (arg, mode)
     char *arg;
     struct termios *mode;
{
  int i, n;
  unsigned int chr;

  if (sscanf (arg, "%lx:%lx:%lx:%lx%n", &mode->c_iflag, &mode->c_oflag,
	      &mode->c_cflag, &mode->c_lflag, &n) != 4)
    return 0;
  arg += n;
  for (i = 0; i < NCCS; ++i)
    {
      if (sscanf (arg, ":%x%n", &chr, &n) != 1)
	return 0;
      mode->c_cc[i] = chr;
      arg += n;
    }
  return 1;
}

struct speed_map
{
  char *string;			/* ASCII representation. */
  speed_t speed;		/* Internal form. */
  unsigned long value;		/* Numeric value. */
};

struct speed_map speeds[] =
{
  {"0", B0, 0},
  {"50", B50, 50},
  {"75", B75, 75},
  {"110", B110, 110},
  {"134", B134, 134},
  {"134.5", B134, 134},
  {"150", B150, 150},
  {"200", B200, 200},
  {"300", B300, 300},
  {"600", B600, 600},
  {"1200", B1200, 1200},
  {"1800", B1800, 1800},
  {"2400", B2400, 2400},
  {"4800", B4800, 4800},
  {"9600", B9600, 9600},
  {"19200", B19200, 19200},
  {"38400", B38400, 38400},
  {"exta", B19200, 19200},
  {"extb", B38400, 38400},
#ifdef B57600
  {"57600", B57600, 57600},
#endif
#ifdef B115200
  {"115200", B115200, 115200},
#endif
  {NULL, 0, 0}
};

int
string_to_baud (arg)
     char *arg;
{
  int i;

  for (i = 0; speeds[i].string != NULL; ++i)
    if (!strcmp (arg, speeds[i].string))
      return speeds[i].speed;
  return -1;
}

unsigned long
baud_to_value (speed)
     speed_t speed;
{
  int i;

  for (i = 0; speeds[i].string != NULL; ++i)
    if (speed == speeds[i].speed)
      return speeds[i].value;
  return 0;
}

void
sane_mode (mode)
     struct termios *mode;
{
  int i;
  tcflag_t *bitsp;

  for (i = 0; control_info[i].name; ++i)
    {
#if VMIN == VEOF
      if (!strcmp (control_info[i].name, "min"))
	break;
#endif
      mode->c_cc[control_info[i].offset] = control_info[i].saneval;
    }

  for (i = 0; mode_info[i].name != NULL; ++i)
    {
      if (mode_info[i].flags & SANE_SET)
	{
	  bitsp = mode_type_flag (mode_info[i].type, mode);
	  *bitsp = (*bitsp & ~mode_info[i].mask) | mode_info[i].bits;
	}
      else if (mode_info[i].flags & SANE_UNSET)
	{
	  bitsp = mode_type_flag (mode_info[i].type, mode);
	  *bitsp = *bitsp & ~mode_info[i].mask & ~mode_info[i].bits;
	}
    }
}

/* Return a string that is the printable representation of character CH.  */
/* Adapted from `cat' by Torbjorn Granlund.  */

char *
visible (ch)
     unsigned char ch;
{
  static char buf[10];
  char *bpout = buf;

  if (ch == _POSIX_VDISABLE)
    return "<undef>";

  if (ch >= 32)
    {
      if (ch < 127)
	*bpout++ = ch;
      else if (ch == 127)
	{
	  *bpout++ = '^';
	  *bpout++ = '?';
	}
      else
	{
	  *bpout++ = 'M',
	    *bpout++ = '-';
	  if (ch >= 128 + 32)
	    {
	      if (ch < 128 + 127)
		*bpout++ = ch - 128;
	      else
		{
		  *bpout++ = '^';
		  *bpout++ = '?';
		}
	    }
	  else
	    {
	      *bpout++ = '^';
	      *bpout++ = ch - 128 + 64;
	    }
	}
    }
  else
    {
      *bpout++ = '^';
      *bpout++ = ch + 64;
    }
  *bpout = '\0';
  return buf;
}

/* Parse string S as an integer, using decimal radix by default,
   but allowing octal and hex numbers as in C.  */
/* From `od' by Richard Stallman.  */

long
integer_arg (s)
     char *s;
{
  long value;
  int radix = 10;
  char *p = s;
  int c;

  if (*p != '0')
    radix = 10;
  else if (*++p == 'x')
    {
      radix = 16;
      p++;
    }
  else
    radix = 8;

  value = 0;
  while (((c = *p++) >= '0' && c <= '9')
	 || (radix == 16 && (c & ~40) >= 'A' && (c & ~40) <= 'Z'))
    {
      value *= radix;
      if (c >= '0' && c <= '9')
	value += c - '0';
      else
	value += (c & ~40) - 'A';
    }

  if (c == 'b')
    value *= 512;
  else if (c == 'B')
    value *= 1024;
  else
    p--;

  if (*p)
    error (1, 0, "invalid integer argument `%s'", s);
  return value;
}
