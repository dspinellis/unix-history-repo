/* Keyboard input; editor command loop.
   Copyright (C) 1985, 1986, 1987, 1988, 1990 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/*** For version 19, can simplify this by making interrupt_input 1 on VMS.  */

/* Allow config.h to undefine symbols found here.  */
#include <signal.h>

#include "config.h"
#include <stdio.h>
#undef NULL
#include "termchar.h"
#include "termopts.h"
#include "termhooks.h"
#include "lisp.h"
#include "macros.h"
#include "window.h"
#include "commands.h"
#include "buffer.h"
#include <setjmp.h>
#include <errno.h>

extern int errno;

/* Get FIONREAD, if it is available.  */
#ifdef USG
#include <termio.h>
#include <fcntl.h>
#else /* not USG */
#ifndef VMS
#include <sys/ioctl.h>
#endif /* not VMS */
#endif /* not USG */

/* Allow m- file to inhibit use of FIONREAD.  */
#ifdef BROKEN_FIONREAD
#undef FIONREAD
#endif

/* Make all keyboard buffers much bigger when using X windows.  */
#ifdef HAVE_X_WINDOWS
#define BUFFER_SIZE_FACTOR 16
#else
#define BUFFER_SIZE_FACTOR 1
#endif

/* Following definition copied from eval.c */

struct backtrace
  {
    struct backtrace *next;
    Lisp_Object *function;
    Lisp_Object *args;	/* Points to vector of args. */
    int nargs;		/* length of vector */
	       /* if nargs is UNEVALLED, args points to slot holding list of unevalled args */
    char evalargs;
  };

/* Non-nil disable property on a command means
 do not execute it; call disabled-command-hook's value instead. */
Lisp_Object Qdisabled, Vdisabled_command_hook;

int recent_keys_index;	/* Index for storing next element into recent_keys */
int total_keys;		/* Total number of elements stored into recent_keys */
char recent_keys[100];	/* Holds last 100 keystrokes */

/* Buffer holding the key that invoked the current command.  */
char *this_command_keys;
int this_command_key_count;	/* Size in use.  */
int this_command_keys_size;	/* Size allocated.  */

extern struct backtrace *backtrace_list;

static jmp_buf getcjmp;	/* for longjmp to where kbd input is being done. */

int waiting_for_input;	/* True while doing kbd input */

/* True while displaying for echoing.   Delays C-g throwing. */
static int echoing;

int immediate_quit;	/* Nonzero means C-G should cause immediate error-signal. */

int help_char;		/* Character to recognize as the help char.  */

Lisp_Object Vhelp_form;	/* Form to execute when help char is typed.  */

/* Character that causes a quit.  Normally C-g.  */

int quit_char;

extern Lisp_Object global_map;

/* Current depth in recursive edits.  */

int command_loop_level;

/* Last input character read as a command.  */

int last_command_char;

/* Last input character read for any purpose.  */

int last_input_char;

/* If not -1, a character to be read as the next command input */

int unread_command_char;

/* Char to use as prefix when a meta character is typed in.
 This is bound on entry to minibuffer in case Esc is changed there.  */

int meta_prefix_char;

/* Total number of times read_command_char has returned.  */

int num_input_chars;

/* Auto-save automatically when this many characters have been typed
   since the last time.  */

static int auto_save_interval;

/* Value of num_input_chars as of last auto save.  */

int last_auto_save;

/* Last command executed by the editor command loop, not counting
   commands that set the prefix argument.  */

Lisp_Object last_command;

/* The command being executed by the command loop.
   Commands may set this, and the value set will be copied into last_command
   instead of the actual command.  */
Lisp_Object this_command;

Lisp_Object Qself_insert_command;
Lisp_Object Qforward_char;
Lisp_Object Qbackward_char;

/* read_key_sequence stores here the command definition of the
   key sequence that it reads.  */
Lisp_Object read_key_sequence_cmd;

/* Form to evaluate (if non-nil) when Emacs is started */
Lisp_Object Vtop_level;

/* User-supplied string to translate input characters through */
Lisp_Object Vkeyboard_translate_table;

FILE *dribble;			/* File in which we write all commands we read */

/* Nonzero if input is available */
int input_pending;

/* Nonzero if should obey 0200 bit in input chars as "Meta" */
int meta_key;

extern char *pending_malloc_warning;

/* Buffer for pre-read keyboard input */
unsigned char kbd_buffer [256 * BUFFER_SIZE_FACTOR];

/* Number of characters available in kbd_buffer.  */
int kbd_count;

/* Pointer to next available character in kbd_buffer.  */
unsigned char *kbd_ptr;

/* Address (if not 0) of word to zero out
 if a SIGIO interrupt happens */
long *input_available_clear_word;

/* Nonzero means use SIGIO interrupts; zero means use CBREAK mode.
   Default is 1 if INTERRUPT_INPUT is defined.  */

int interrupt_input;

/* Nonzero while interrupts are temporarily deferred during redisplay.  */

int interrupts_deferred;

/* nonzero means use ^S/^Q for flow control.  */

int flow_control;

#ifndef BSD4_1
#define sigfree() sigsetmask (0)
#define sigholdx(sig) sigsetmask (1 << ((sig) - 1))
#define sigblockx(sig) sigblock (1 << ((sig) - 1))
#define sigunblockx(sig) sigblock (0)
#define sigpausex(sig) sigpause (0)
#endif /* not BSD4_1 */

#ifdef BSD4_1
#define SIGIO SIGTINT
/* sigfree and sigholdx are in sysdep.c */
#define sigblockx(sig) sighold (sig)
#define sigunblockx(sig) sigrelse (sig)
#define sigpausex(sig) sigpause (sig)
#endif /* BSD4_1 */

#ifndef sigmask
#define sigmask(no) (1L << ((no) - 1))
#endif

/* We are unable to use interrupts if FIONREAD is not available,
   so flush SIGIO so we won't try. */
#ifndef FIONREAD
#ifdef SIGIO
#undef SIGIO
#endif
#endif

/* If we support X Windows, and won't get an interrupt when input
   arrives from the server, poll periodically so we can detect C-g.  */
#ifdef HAVE_X_WINDOWS
#ifndef SIGIO
#define POLL_FOR_INPUT
#endif
#endif

/* Function for init_keyboard to call with no args (if nonzero).  */
void (*keyboard_init_hook) ();

static void read_avail_input ();
static void get_input_pending ();

/* Non-zero tells input_available_signal to call read_socket_hook
   even if FIONREAD returns zero.  */
static int force_input;

static int echo_keystrokes;	/* > 0 if we are to echo keystrokes */

/* Nonzero means echo each character as typed.  */
static int immediate_echo;

#define	min(a,b)	((a)<(b)?(a):(b))
#define	max(a,b)	((a)>(b)?(a):(b))

static char echobuf[100];
static char *echoptr;

/* Install the string STR as the beginning of the string of echoing,
   so that it serves as a prompt for the next character.
   Also start echoing.  */

echo_prompt (str)
     char *str;
{
  int len = strlen (str);
  if (len > sizeof echobuf - 4)
    len = sizeof echobuf - 4;
  bcopy (str, echobuf, len + 1);
  echoptr = echobuf + len;

  echo ();
}

/* Add the character C to the echo string,
   if echoing is going on.  */

echo_char (c)
     int c;
{
  extern char *push_key_description ();

  if (immediate_echo)
    {
      char *ptr = echoptr;

      if (ptr - echobuf > sizeof echobuf - 6)
	return;

      ptr = push_key_description (c, ptr);
      *ptr++ = ' ';
      if (echoptr == echobuf && c == help_char)
	{
	  strcpy (ptr, "(Type ? for further options) ");
	  ptr += strlen (ptr);
	}

      *ptr = 0;
      echoptr = ptr;

      echo ();
    }
}

/* Temporarily add a dash to the end of the echo string,
   so that it serves as a mini-prompt for the very next character.  */

echo_dash ()
{
  if (!immediate_echo && echoptr == echobuf)
    return;

  /* Put a dash at the end of the buffer temporarily,
     but make it go away when the next character is added.  */
  echoptr[0] = '-';
  echoptr[1] = 0;

  echo ();
}

/* Display the current echo string, and begin echoing if not already
   doing so.  */

echo ()
{
  if (!immediate_echo)
    {
      int i;
      immediate_echo = 1;

      for (i = 0; i < this_command_key_count; i++)
	echo_char (this_command_keys[i]);
      echo_dash ();
    }

  echoing = 1;
  message1 (echobuf);
  echoing = 0;

  if (waiting_for_input && !NULL (Vquit_flag))
    quit_throw_to_read_command_char ();
}

/* Turn off echoing, for the start of a new command.  */

cancel_echoing ()
{
  immediate_echo = 0;
  echoptr = echobuf;
}

/* When an auto-save happens, record the "time", and don't do again soon.  */
record_auto_save ()
{
  last_auto_save = num_input_chars;
}

Lisp_Object recursive_edit_unwind (), command_loop ();

DEFUN ("recursive-edit", Frecursive_edit, Srecursive_edit, 0, 0, "",
  "Invoke the editor command loop recursively.\n\
Do (throw 'exit nil) within the command loop to make this function return,\n\
or (throw 'exit t) to make this function signal an error.\n\
This function is called by the editor initialization\n\
to begin editing.")
  ()
{
  int count = specpdl_ptr - specpdl;

  command_loop_level++;
  update_mode_lines = 1;

  record_unwind_protect (recursive_edit_unwind,
			 (current_buffer != XBUFFER (XWINDOW (selected_window)->buffer)
			  ? Fcurrent_buffer ()
			  : Qnil));

  recursive_edit_1 ();

  unbind_to (count);
  return Qnil;
}

Lisp_Object
recursive_edit_1 ()
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object val;

  if (command_loop_level > 0)
    {
      specbind (Qstandard_output, Qt);
      specbind (Qstandard_input, Qt);
    }

  val = command_loop ();
  if (EQ (val, Qt))
    Fsignal (Qquit, Qnil);

  unbind_to (count);
  return Qnil;
}

Lisp_Object
recursive_edit_unwind (buffer)
     Lisp_Object buffer;
{
  if (!NULL (buffer))
    Fset_buffer (buffer);
  command_loop_level--;
  update_mode_lines = 1;
  return Qnil;
}

Lisp_Object
cmd_error (data)
     Lisp_Object data;
{
  Lisp_Object errmsg, tail, errname, file_error;
  struct gcpro gcpro1;
  int i;

  Vquit_flag = Qnil;
  Vinhibit_quit = Qt;
  Vstandard_output = Qt;
  Vstandard_input = Qt;
  Vexecuting_macro = Qnil;
  echo_area_contents = 0;

  Fdiscard_input ();
  bell ();

  errname = Fcar (data);

  if (EQ (errname, Qerror))
    {
      data = Fcdr (data);
      if (!CONSP (data)) data = Qnil;
      errmsg = Fcar (data);
      file_error = Qnil;
    }
  else
    {
      errmsg = Fget (errname, Qerror_message);
      file_error = Fmemq (Qfile_error,
			  Fget (errname, Qerror_conditions));
    }

  /* Print an error message including the data items.
     This is done by printing it into a scratch buffer
     and then making a copy of the text in the buffer. */
  
  if (!CONSP (data)) data = Qnil;
  tail = Fcdr (data);
  GCPRO1 (tail);

  /* For file-error, make error message by concatenating
     all the data items.  They are all strings.  */
  if (!NULL (file_error))
    errmsg = XCONS (tail)->car, tail = XCONS (tail)->cdr;

  if (XTYPE (errmsg) == Lisp_String)
    Fprinc (errmsg, Qt);
  else
    write_string_1 ("peculiar error", -1, Qt);

  for (i = 0; CONSP (tail); tail = Fcdr (tail), i++)
    {
      write_string_1 (i ? ", " : ": ", 2, Qt);
      if (!NULL (file_error))
	Fprinc (Fcar (tail), Qt);
      else
	Fprin1 (Fcar (tail), Qt);
    }
  UNGCPRO;

  /* In -batch mode, force out the error message and newlines after it
     and then die.  */
  if (noninteractive)
    {
      message ("");
      Fkill_emacs (make_number (-1));
    }

  Vquit_flag = Qnil;

  Vinhibit_quit = Qnil;
  return make_number (0);
}

Lisp_Object command_loop_1 ();
Lisp_Object command_loop_2 ();
Lisp_Object cmd_error ();
Lisp_Object top_level_1 ();

/* Entry to editor-command-loop.
   This level has the catches for exiting/returning to editor command loop.
   It returns nil to exit recursive edit, t to abort it.  */

Lisp_Object
command_loop ()
{
  if (command_loop_level > 0 || minibuf_level > 0)
    {
      return internal_catch (Qexit, command_loop_2, Qnil);
    }
  else
    while (1)
      {
	internal_catch (Qtop_level, top_level_1, Qnil);
	internal_catch (Qtop_level, command_loop_2, Qnil);
	/* End of file in -batch run causes exit here.  */
	if (noninteractive)
	  Fkill_emacs (Qt);
      }
}

/* Here we catch errors in execution of commands within the
   editing loop, and reenter the editing loop.
   When there is an error, cmd_error runs and returns a non-nil
   value to us.  A value of nil means that cmd_loop_1 itself
   returned due to end of file (or end of kbd macro).  */

Lisp_Object
command_loop_2 ()
{
  register Lisp_Object val;
  do
    val = internal_condition_case (command_loop_1, Qerror, cmd_error);
  while (!NULL (val));
  return Qnil;
}

Lisp_Object
top_level_2 ()
{
  return Feval (Vtop_level);
}

Lisp_Object
top_level_1 ()
{
  /* On entry to the outer level, run the startup file */
  if (!NULL (Vtop_level))
    internal_condition_case (top_level_2, Qerror, cmd_error);
  else if (!NULL (Vpurify_flag))
    message ("Bare impure Emacs (standard Lisp code not loaded)");
  else
    message ("Bare Emacs (standard Lisp code not loaded)");
  return Qnil;
}

DEFUN ("top-level", Ftop_level, Stop_level, 0, 0, "",
  "Exit all recursive editing levels.")
  ()
{
  Fthrow (Qtop_level, Qnil);
}

DEFUN ("exit-recursive-edit", Fexit_recursive_edit, Sexit_recursive_edit, 0, 0, "",
  "Exit from the innermost recursive edit or minibuffer.")
  ()
{
  if (command_loop_level > 0 || minibuf_level > 0)
    Fthrow (Qexit, Qnil);
  error ("No recursive edit is in progress");
}

DEFUN ("abort-recursive-edit", Fabort_recursive_edit, Sabort_recursive_edit, 0, 0, "",
  "Abort the command that requested this recursive edit or minibuffer input.")
  ()
{
  if (command_loop_level > 0 || minibuf_level > 0)
    Fthrow (Qexit, Qt);
  error ("No recursive edit is in progress");
}

/* This is the actual command reading loop,
 sans error-handling encapsulation */

Lisp_Object Fcommand_execute ();

Lisp_Object
command_loop_1 ()
{
  Lisp_Object cmd;
  int lose;
  int nonundocount;
  char keybuf[30];
  int i;
  int no_redisplay;
  int no_direct;

  Vprefix_arg = Qnil;
  waiting_for_input = 0;
  cancel_echoing ();
  last_command = Qt;
  nonundocount = 0;
  no_redisplay = 0;
  this_command_key_count = 0;
  
  while (1)
    {
      /* Install chars successfully executed in kbd macro */
      if (defining_kbd_macro && NULL (Vprefix_arg))
	finalize_kbd_macro_chars ();

      /* Make sure current window's buffer is selected.  */

      if (XBUFFER (XWINDOW (selected_window)->buffer) != current_buffer)
	set_buffer_internal (XBUFFER (XWINDOW (selected_window)->buffer));

      /* Display any malloc warning that just came out.
	 Use while because displaying one warning can cause another.  */
      while (pending_malloc_warning)
	display_malloc_warning ();

      no_direct = 0;

      /* If minibuffer on and echo area in use,
	 wait 2 sec and redraw minibufer.  */

      if (minibuf_level && echo_area_contents)
	{
	  int count = specpdl_ptr - specpdl;
	  specbind (Qinhibit_quit, Qt);
	  Fsit_for (make_number (2), Qnil);
	  unbind_to (count);

	  echo_area_contents = 0;
	  no_direct = 1;
	  if (!NULL (Vquit_flag))
	    {
	      Vquit_flag = Qnil;
	      unread_command_char = quit_char;
	    }
	}

      i = 0;
#if 0
      /* If prev. command was directly displayed, we don't need
	 redisplay.  Try shortcut for reading single-char key sequence.  */
      if (no_redisplay)
	i = fast_read_one_key (keybuf);
#endif /* 0 */
      /* Shortcut not applicable or found a prefix key.
	 Take full precautions and read key sequence the hard way.  */
      if (i == 0)
	{
#ifdef C_ALLOCA
	  alloca (0);		/* Cause a garbage collection now */
				/* Since we can free the most stuff here.  */
#endif /* C_ALLOCA */

	  /* Read next key sequence; i gets its length.  */

	  i = read_key_sequence (keybuf, sizeof keybuf, 0,
				 no_redisplay && buffer_shared <= 1);
	}

      /* Now we have read a key sequence of length I,
	 or else I is 0 and we found end of file.  */

      if (i == 0)		/* End of file -- happens only in */
	return Qnil;		/* a kbd macro, at the end */

      last_command_char = keybuf[i - 1];
      
      cmd = read_key_sequence_cmd;
      if (!NULL (Vexecuting_macro))
	{
	  if (!NULL (Vquit_flag))
	    {
	      Vexecuting_macro = Qt;
	      QUIT;		/* Make some noise. */
				/* Will return since macro now empty. */
	    }
	}

      /* Do redisplay processing after this command except in special
	 cases identified below that set no_redisplay to 1.  */
      no_redisplay = 0;

      /* Execute the command.  */

      if (NULL (cmd))
	{
	  /* nil means key is undefined.  */
	  bell ();
	  defining_kbd_macro = 0;
	  update_mode_lines++;
	  Vprefix_arg = Qnil;
	}
      else
	{
	  this_command = cmd;
	  if (NULL (Vprefix_arg) && ! no_direct)
	    {
	      if (EQ (cmd, Qforward_char) && point < ZV)
		{
		  lose = FETCH_CHAR (point);
		  SET_PT (point + 1);
		  if (lose >= ' ' && lose < 0177
		      && (XFASTINT (XWINDOW (selected_window)->last_modified)
			  >= MODIFF)
		      && (XFASTINT (XWINDOW (selected_window)->last_point)
			  == point)
		      && !windows_or_buffers_changed
		      && EQ (current_buffer->selective_display, Qnil)
		      && !detect_input_pending ()
		      && NULL (Vexecuting_macro))
		    no_redisplay = direct_output_forward_char (1);
		  goto directly_done;
		}
	      else if (EQ (cmd, Qbackward_char) && point > BEGV)
		{
		  SET_PT (point - 1);
		  lose = FETCH_CHAR (point);
		  if (lose >= ' ' && lose < 0177
		      && (XFASTINT (XWINDOW (selected_window)->last_modified)
			  >= MODIFF)
		      && (XFASTINT (XWINDOW (selected_window)->last_point)
			  == point)
		      && !windows_or_buffers_changed
		      && EQ (current_buffer->selective_display, Qnil)
		      && !detect_input_pending ()
		      && NULL (Vexecuting_macro))
		    no_redisplay = direct_output_forward_char (-1);
		  goto directly_done;
		}
	      else if (EQ (cmd, Qself_insert_command))
		{
		  if (NULL (Vexecuting_macro) &&
		      !EQ (minibuf_window, selected_window))
		    {
		      if (!nonundocount || nonundocount >= 20)
			{
			  Fundo_boundary ();
			  nonundocount = 0;
			}
		      nonundocount++;
		    }
		  lose = (XFASTINT (XWINDOW (selected_window)->last_modified)
			  < MODIFF)
		    || (XFASTINT (XWINDOW (selected_window)->last_point)
			  != point)
		    || MODIFF <= current_buffer->save_modified
		    || windows_or_buffers_changed
		    || !EQ (current_buffer->selective_display, Qnil)
		    || detect_input_pending ()
		    || !NULL (Vexecuting_macro);
		  if (self_insert_internal (last_command_char, 0))
		    {
		      lose = 1;
		      nonundocount = 0;
		    }
		  if (!lose
		      && (point == ZV || FETCH_CHAR (point) == '\n')
		      && last_command_char >= ' '
		      && last_command_char < 0177)
		    no_redisplay
		      = direct_output_for_insert (last_command_char);
		  goto directly_done;
		}
	    }

	  /* Here for a command that isn't executed directly */

	  nonundocount = 0;
	  if (NULL (Vprefix_arg))
	    Fundo_boundary ();
	  Fcommand_execute (cmd, Qnil);

	directly_done: ;
	}

      if (NULL (Vprefix_arg))
	{
	  last_command = this_command;
	  this_command_key_count = 0;
	  cancel_echoing ();
	}
    }
}

/* Input of single characters from keyboard */

Lisp_Object print_help ();

int echo_flag;
int echo_now;

/* Alarm interrupt calls this and requests echoing at earliest safe time. */
request_echo ()
{
  int old_errno = errno;

  /* Note: no need to reestablish handler on USG systems
     because it is established, if approriate, each time an alarm is requested.  */
#ifdef subprocesses
#ifdef BSD4_1
  extern int select_alarmed;
  if (select_alarmed == 0)
    {
      select_alarmed = 1;
      sigrelse (SIGALRM);
      return;
    }
#endif
#endif

#ifdef BSD4_1
  sigisheld (SIGALRM);
#endif

  if (echo_now)
    echo ();
  else
    echo_flag = 1;

#ifdef BSD4_1
  sigunhold (SIGALRM);
#endif

  errno = old_errno;
}

/* Nonzero means polling for input is temporarily suppresed.  */
int poll_suppress_count;

/* Number of seconds between polling for input.  */
int polling_period;

#ifdef POLL_FOR_INPUT
int polling_for_input;

/* Handle an alarm once each second and read pending input
   so as to handle a C-g if it comces in.  */

input_poll_signal ()
{
  int junk;

  if (!waiting_for_input)
    read_avail_input (&junk);
  signal (SIGALRM, input_poll_signal);
  alarm (polling_period);
}

#endif

/* Begin signals to poll for input, if they are appropriate.
   This function is called unconditionally from various places.  */

start_polling ()
{
#ifdef POLL_FOR_INPUT
  if (read_socket_hook)
    {
      poll_suppress_count--;
      if (poll_suppress_count == 0)
	{
	  signal (SIGALRM, input_poll_signal);
	  polling_for_input = 1;
	  alarm (polling_period);
	}
    }
#endif
}

/* Turn off polling.  */

stop_polling ()
{
#ifdef POLL_FOR_INPUT
  if (read_socket_hook)
    {
      if (poll_suppress_count == 0)
	{
	  polling_for_input = 0;
	  alarm (0);
	}
      poll_suppress_count++;
    }
#endif
}

/* read a character from the keyboard; call the redisplay if needed */
/* commandflag 0 means do not do auto-saving, but do do redisplay.
   -1 means do not do redisplay, but do do autosaving.
   1 means do both.  */

read_command_char (commandflag)
     int commandflag;
{
  register int c;
  int alarmtime;
  int count;
  Lisp_Object tem;
  jmp_buf save_jump;
  extern request_echo ();

  if (unread_command_char >= 0)
    {
      c = unread_command_char;
      unread_command_char = -1;
      if (this_command_key_count == 0)
	goto reread_first;
      goto reread;
    }

  if (!NULL (Vexecuting_macro))
    {
      if (XTYPE (Vexecuting_macro) != Lisp_String
	  || XSTRING (Vexecuting_macro)->size <= executing_macro_index)
	return -1;
      QUIT;
      c = XSTRING (Vexecuting_macro)->data[executing_macro_index++];
      goto from_macro;
    }

  /* Save outer setjmp data, in case called recursively.  */
  bcopy (getcjmp, save_jump, sizeof getcjmp);

  stop_polling ();

  if (commandflag >= 0 && !detect_input_pending ())
    redisplay ();

  if (commandflag != 0
      && auto_save_interval > 0
      && num_input_chars - last_auto_save > max (auto_save_interval, 20)
      && !detect_input_pending ())
    Fdo_auto_save (Qnil);

  if (_setjmp (getcjmp))
    {
      c = quit_char;
      waiting_for_input = 0;
      input_available_clear_word = 0;

      goto non_reread;
    }

  /* Message turns off echoing unless more keystrokes turn it on again. */
  if (echo_area_contents && *echo_area_contents && echo_area_contents != echobuf)
    cancel_echoing ();
  else
    /* If already echoing, continue, and prompt.  */
    echo_dash ();

  /* If in middle of key sequence and minibuffer not active,
     start echoing if enough time elapses.  */
  if (minibuf_level == 0 && !immediate_echo && this_command_key_count > 0
      && echo_keystrokes > 0
      && (echo_area_contents == 0 || *echo_area_contents == 0))
    {
      /* Else start echoing if user waits more than `alarmtime' seconds. */
      /* This interrupt either calls echo right away
	 or sets echo_flag, which causes echo to be called
	 by set_waiting_for_input's next invocation.  */
      signal (SIGALRM, request_echo);
      echo_flag = 0;
      echo_now = 0;
      alarmtime = echo_keystrokes;
      alarm ((unsigned) alarmtime);
    }

  c = kbd_buffer_read_command_char ();

 non_reread:

  bcopy (save_jump, getcjmp, sizeof getcjmp);

  /* Cancel alarm if it was set and has not already gone off. */
  if (alarmtime > 0) alarm (0);

  echo_area_contents = 0;

  if (c < 0) return -1;

  c &= meta_key ? 0377 : 0177;

  if (XTYPE (Vkeyboard_translate_table) == Lisp_String
      && XSTRING (Vkeyboard_translate_table)->size > c)
    c = XSTRING (Vkeyboard_translate_table)->data[c];

  total_keys++;
  recent_keys[recent_keys_index] = c;
  recent_keys_index = (recent_keys_index + 1) % sizeof recent_keys;

  if (dribble)
    {
      putc (c, dribble);
      fflush (dribble);
    }

  store_kbd_macro_char (c);

  start_polling ();

 from_macro:
 reread_first:  /* Rereading a char and it is the first in a command.  */

  echo_char (c);

  /* Record this character as part of the current key.  */
  if (this_command_key_count == this_command_keys_size)
    {
      this_command_keys_size *= 2;
      this_command_keys
	= (char *) xrealloc (this_command_keys, this_command_keys_size);
    }
  this_command_keys[this_command_key_count++] = c;

  /* Rereading in the middle of a command.  */
 reread:

  last_input_char = c;

  num_input_chars++;

  /* Process the help character specially if enabled */
  if (c == help_char && !NULL (Vhelp_form))
    {
      count = specpdl_ptr - specpdl;

      record_unwind_protect (Fset_window_configuration,
			     Fcurrent_window_configuration ());

      tem = Feval (Vhelp_form);
      if (XTYPE (tem) == Lisp_String)
	internal_with_output_to_temp_buffer ("*Help*", print_help, tem);

      cancel_echoing ();
      c = read_command_char (0);
      /* Remove the help from the screen */
      unbind_to (count);
      redisplay ();
      if (c == 040)
	{
	  cancel_echoing ();
	  c = read_command_char (0);
	}
    }

  return c;
}

Lisp_Object
print_help (object)
     Lisp_Object object;
{
  Fprinc (object, Qnil);
  return Qnil;
}

/* Low level keyboard input.
 Read characters into kbd_buffer
 from which they are obtained by kbd_buffer_read_command_char.  */

/* Set this for debugging, to have a way to get out */
int stop_character;

/* Store a character obtained at interrupt level into kbd_buffer, fifo */
kbd_buffer_store_char (c)
     register int c;
{
  c &= 0377;

  if (c == quit_char
      || ((c == (0200 | quit_char)) && !meta_key))
    {
      interrupt_signal ();
      return;
    }

  if (c && c == stop_character)
    {
      sys_suspend ();
      return;
    }

  if (kbd_ptr != kbd_buffer)
    {
      bcopy (kbd_ptr, kbd_buffer, kbd_count);
      kbd_ptr = kbd_buffer;
    }

  if (kbd_count < sizeof kbd_buffer)
    {
      kbd_buffer[kbd_count++] = c;
    }
}

kbd_buffer_read_command_char ()
{
  register int c;

  if (noninteractive)
    {
      c = getchar ();
      return c;
    }

  /* Either ordinary input buffer or C-g buffered means we can return.  */
  while (!kbd_count)
    {
      if (!NULL (Vquit_flag))
	quit_throw_to_read_command_char ();

      /* One way or another, wait until input is available; then, if
	 interrupt handlers have not read it, read it now.  */

#ifdef VMS
      wait_for_kbd_input ();
#else
/* Note SIGIO has been undef'd if FIONREAD is missing.  */
#ifdef SIGIO
      gobble_input ();
#endif /* SIGIO */
      if (!kbd_count)
	{
#ifdef subprocesses
	  wait_reading_process_input (0, -1, 1);
#else
/* Note SIGIO has been undef'd if FIONREAD is missing.  */
#ifdef SIGIO
	  if (interrupt_input)
	    {
	      sigblockx (SIGIO);
	      set_waiting_for_input (0);
	      while (!kbd_count)
		sigpausex (SIGIO);
	      clear_waiting_for_input ();
	      sigunblockx (SIGIO);
	    }
#else
	  interrupt_input = 0;
#endif /* not SIGIO */
#endif /* subprocesses */

	  if (!interrupt_input && !kbd_count)
	    {
	      read_avail_input (0);
	    }
	}
#endif /* not VMS */
    }

  input_pending = --kbd_count > 0;
  c = *kbd_ptr;			/* *kbd_ptr++ would have a timing error. */
  kbd_ptr++;			/* See kbd_buffer_store_char. */
  return (c & (meta_key ? 0377 : 0177)); /* Clean up if sign was extended. */
}

/* Force an attempt to read input regardless of what FIONREAD says.  */

force_input_read ()
{
  force_input = 1;
  detect_input_pending ();
  force_input = 0;
}

/* Store into *addr the number of terminal input chars available.
   Equivalent to ioctl (0, FIONREAD, addr) but works
   even if FIONREAD does not exist.  */

static void
get_input_pending (addr)
     int *addr;
{
#ifdef VMS
  /* On VMS, we always have something in the buffer
     if any input is available.  */
  /*** It might be simpler to make interrupt_input 1 on VMS ***/
  *addr = kbd_count | !NULL (Vquit_flag);
#else
  /* First of all, have we already counted some input?  */
  *addr = kbd_count | !NULL (Vquit_flag);
  /* If input is being read as it arrives, and we have none, there is none.  */
  if (*addr > 0 || (interrupt_input && ! interrupts_deferred && ! force_input))
    return;
#ifdef FIONREAD
  if (! force_input)
    {
      /* If we can count the input without reading it, do so.  */
      if (ioctl (0, FIONREAD, addr) < 0)
	*addr = 0;
      if (*addr == 0 || read_socket_hook == 0)
	return;
      /* If the input consists of window-events, not all of them
	 are necessarily kbd chars.  So process all the input
	 and see how many kbd chars we got.  */
    }
#endif
#ifdef SIGIO
  {
    /* It seems there is a timing error such that a SIGIO can be handled here
       and cause kbd_count to become nonzero even though raising of SIGIO
       has already been turned off.  */
    int mask = sigblock (sigmask (SIGIO));
    if (kbd_count == 0)
      read_avail_input (*addr);
    sigsetmask (mask);
  }
#else
  /* If we can't count the input, read it (if any) and see what we got.  */
  read_avail_input (*addr);
#endif
  *addr = kbd_count | !NULL (Vquit_flag);
#endif
}

/* Read pending any input out of the system and into Emacs.  */

/* This function is temporary in Emacs 18.  It is used only
   with X windows.  X windows always turns on interrupt input
   if possible, so this function has nothing to do except
   on systems that don't have SIGIO.  And they also don't have FIONREAD.  */
void
consume_available_input ()
{
#ifdef SIGIO
  if (!interrupt_input || interrupts_deferred)
#endif
    read_avail_input (0);
}

/* Read any terminal input already buffered up by the system
   into the kbd_buffer, assuming the buffer is currently empty.
   Never waits.

   If NREAD is nonzero, assume it contains # chars of raw data waiting.
   If it is zero, we determine that datum.

   Input gets into the kbd_buffer either through this function
   (at main program level) or at interrupt level if input
   is interrupt-driven.  */

static void
read_avail_input (nread)
     int nread;
{
  /* This function is not used on VMS.  */
#ifndef VMS
  char buf[256 * BUFFER_SIZE_FACTOR];
  register int i;

#ifdef FIONREAD
  if (! force_input)
    {
      if (nread == 0)
	get_input_pending (&nread);
      if (nread == 0)
	return;
    }
  if (nread > sizeof buf)
    nread = sizeof buf;

  /* Read what is waiting.  */
  if (read_socket_hook)
    nread = (*read_socket_hook) (0, buf, nread);
  else
    nread = read (0, buf, nread);

#else /* no FIONREAD */
#ifdef USG
  fcntl (fileno (stdin), F_SETFL, O_NDELAY);
  if (read_socket_hook)
    {
      nread = (*read_socket_hook) (0, buf, sizeof buf);
    }
  else
    {
      nread = read (fileno (stdin), buf, sizeof buf);
    }
#ifdef EBADSLT
  if (nread == -1 && (errno == EAGAIN || errno == EBADSLT))
#else
  if (nread == -1 && errno == EAGAIN)
#endif
    nread = 0;
  fcntl (fileno (stdin), F_SETFL, 0);
#else /* not USG */
  you lose
#endif /* not USG */
#endif /* no FIONREAD */

  /* Scan the chars for C-g and store them in kbd_buffer.  */
  if (kbd_count == 0)
    kbd_ptr = kbd_buffer;

  for (i = 0; i < nread; i++)
    {
      kbd_buffer_store_char (buf[i]);
      /* Don't look at input that follows a C-g too closely.
	 This reduces lossage due to autorepeat on C-g.  */
      if (buf[i] == quit_char)
	break;
    }
#endif /* not VMS */
}

#ifdef SIGIO   /* for entire page */
/* Note SIGIO has been undef'd if FIONREAD is missing.  */

/* If using interrupt input and some input chars snuck into the
   buffer before we enabled interrupts, fake an interrupt for them.  */

gobble_input ()
{
  int nread;
  if (interrupt_input)
    {
      if (ioctl (0, FIONREAD, &nread) < 0)
	nread = 0;
      if (nread)
	{
	  sigholdx (SIGIO);
	  input_available_signal (SIGIO);
	  sigfree ();
	}
    }
}

input_available_signal (signo)
     int signo;
{
  unsigned char buf[256 * BUFFER_SIZE_FACTOR];
  int nread;
  register int i;
  /* Must preserve main program's value of errno.  */
  int old_errno = errno;
#ifdef BSD4_1
  extern int select_alarmed;
#endif

#ifdef USG
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (signo, input_available_signal);
#endif /* USG */

#ifdef BSD4_1
  sigisheld (SIGIO);
#endif

  if (input_available_clear_word)
    *input_available_clear_word = 0;

  while (1)
    {
      if (ioctl (0, FIONREAD, &nread) < 0)
	/* Formerly simply exited the loop, but that sometimes led to
	   a failure of Emacs to terminate.
	   SIGHUP seems appropriate if we can't reach the terminal.  */
	kill (getpid (), SIGHUP);
      if (nread <= 0)
	break;
#ifdef BSD4_1
      select_alarmed = 1;  /* Force the select emulator back to life */
#endif
      if (read_socket_hook)
	{
	  nread = (*read_socket_hook) (0, buf, sizeof buf);
	  if (!nread)
	    continue;
	}
      else
	{
	  if (nread > sizeof buf)
	    nread = sizeof buf;
	  nread = read (0, buf, nread);
	}

      for (i = 0; i < nread; i++)
	{
	  kbd_buffer_store_char (buf[i]);
	  /* Don't look at input that follows a C-g too closely.
	     This reduces lossage due to autorepeat on C-g.  */
	  if (buf[i] == quit_char)
	    break;
	}
    }
#ifdef BSD4_1
  sigfree ();
#endif
  errno = old_errno;
}
#endif /* SIGIO */

#if 0
/* This is turned off because it didn't produce much speedup.  */

/* Read a single-char key sequence.  Do not redisplay.
   Return 1 if successful, or 0 if what follows is not
   a single-char key.  (In that case, a char has been unread.)
   This is used instead of read_key_sequence as an optimization
   just after a direct-updating command is done, since at such
   times we know that no redisplay is required.  */

int
fast_read_one_key (keybuf)
     char *keybuf;
{
  register Lisp_Object map;
  register int c;
  register Lisp_Object tem;

  keys_prompt = 0;
  /* Read a character, and do not redisplay.  */
  c = read_command_char (-1);
  Vquit_flag = Qnil;

  /* Assume until further notice that we are unlucky
     and will return zero, so this char will be
     reread by read_key_sequence.  */

  unread_command_char = c;

  if (c < 0 || c >= 0200)
    return 0;

  map = current_buffer->keymap;
  if (!EQ (map, Qnil))
    {
      tem = get_keyelt (access_keymap (map, c));
      if (!EQ (tem, Qnil))
	return 0;
    }

  XSET (map, Lisp_Vector, global_map);
  tem = !NULL (map)
    ? get_keyelt (access_keymap (map, c))
      : Qnil;

  read_key_sequence_cmd = tem;

  /* trace symbols to their function definitions */

  while (XTYPE (tem) == Lisp_Symbol && !NULL (tem)
	 && !EQ (tem, Qunbound))
    tem = XSYMBOL (tem)->function;

  /* Is the definition a prefix character?  */

  if (XTYPE (tem) == Lisp_Vector ||
      (CONSP (tem) && EQ (XCONS (tem)->car, Qkeymap)))
    return 0;

  unread_command_char = -1;
  keybuf[0] = c;
  return 1;
}

#endif /* 0 */

/* Read a sequence of keys that ends with a non prefix character,
 and store them in KEYBUF, a buffer of size BUFSIZE.
 Prompt with PROMPT.  Echo starting immediately unless `prompt' is 0.
 Return the length of the key sequence stored.
 NODISPLAY nonzero means don't do redisplay before the first character
 (just for speedup).  */

int
read_key_sequence (keybuf, bufsize, prompt, nodisplay)
     char *keybuf;
     int bufsize;
     unsigned char *prompt;
     int nodisplay;
{
  register int i;
  Lisp_Object nextlocal, nextglobal;
  register int c, nextc;
  Lisp_Object local, global;

  if (FROM_KBD)
    {
      if (prompt)
	echo_prompt (prompt);
      else if (cursor_in_echo_area)
	echo_dash ();
    }

  nextc = read_command_char (nodisplay ? -1 : !prompt);
  nextlocal = current_buffer->keymap;
  XSET (nextglobal, Lisp_Vector, global_map);

  i = 0;
  while (!NULL (nextlocal) || !NULL (nextglobal))
    {
      if (i == bufsize)
	error ("key sequence too long");

      if (nextc >= 0)
	{
	  c = nextc;
	  nextc = -1;
	}
      else
	c = read_command_char (!prompt);
      Vquit_flag = Qnil;
      nodisplay = 0;

      if (c < 0)
	return 0;
      if (c >= 0200)
	{
	  nextc = c & 0177;
	  c = meta_prefix_char;
	}

      keybuf[i] = c;

      global = !NULL (nextglobal)
	? get_keyelt (access_keymap (nextglobal, c))
	  : Qnil;

      local = !NULL (nextlocal)
	? get_keyelt (access_keymap (nextlocal, c))
	  : Qnil;

      /* If C is not defined in either keymap
	 and it is an uppercase letter, try corresponding lowercase.  */

      if (NULL (global) && NULL (local) && UPPERCASEP (c))
	{
	  global = !NULL (nextglobal)
	    ? get_keyelt (access_keymap (nextglobal, DOWNCASE (c)))
	      : Qnil;

	  local = !NULL (nextlocal)
	    ? get_keyelt (access_keymap (nextlocal, DOWNCASE (c)))
	      : Qnil;

	  /* If that has worked better that the original char,
	     downcase it permanently.  */

	  if (!NULL (global) || !NULL (local))
	    {
	      keybuf[i] = c = DOWNCASE (c);
	    }
	}

      i++;

      nextlocal = Qnil;
      nextglobal = Qnil;

      read_key_sequence_cmd = !NULL (local) ? local : global;

      /* trace symbols to their function definitions */

      while (XTYPE (global) == Lisp_Symbol && !NULL (global)
	     && !EQ (global, Qunbound))
	global = XSYMBOL (global)->function;
      while (XTYPE (local) == Lisp_Symbol && !NULL (local)
	     && !EQ (local, Qunbound))
	local = XSYMBOL (local)->function;

      /* Are the definitions prefix characters? */

      if (XTYPE (local) == Lisp_Vector ||
	  (CONSP (local) && EQ (XCONS (local)->car, Qkeymap))
	  ||
	  /* If nextc is set, we are processing a prefix char
	     that represents a meta-bit.
	     Let a global prefix definition override a local non-prefix.
	     This is for minibuffers that redefine Escape for completion.
	     A real Escape gets completion, but Meta bits get ESC-prefix.  */
	  ((NULL (local) || nextc >= 0)
	   && (XTYPE (global) == Lisp_Vector ||
	       (CONSP (global) && EQ (XCONS (global)->car, Qkeymap)))))
	{
	  if (XTYPE (local) == Lisp_Vector ||
	      (CONSP (local) && EQ (XCONS (local)->car, Qkeymap)))
	    nextlocal = local;
	  else
	    nextlocal = Qnil;

	  if (XTYPE (global) == Lisp_Vector ||
	      (CONSP (global) && EQ (XCONS (global)->car, Qkeymap)))
	    nextglobal = global;
	  else
	    nextglobal = Qnil;
	}
    }

  return i;
}

DEFUN ("read-key-sequence", Fread_key_sequence, Sread_key_sequence, 1, 1, 0,
  "Read a sequence of keystrokes and return as a string.\n\
The sequence is sufficient to specify a non-prefix command\n\
starting from the current local and global keymaps.\n\
A C-g typed while in this function is treated like\n\
any other character, and quit-flag is not set.\n\
One arg, PROMPT, a prompt string or  nil, meaning do not prompt specially.")
  (prompt)
     Lisp_Object prompt;
{
  char keybuf[30];
  register int i;

  if (!NULL (prompt))
    CHECK_STRING (prompt, 0);
  QUIT;

  this_command_key_count = 0;
  i = read_key_sequence (keybuf, sizeof keybuf,
			 (NULL (prompt)) ? 0 : XSTRING (prompt)->data,
			 0);
  return make_string (keybuf, i);
}

DEFUN ("command-execute", Fcommand_execute, Scommand_execute, 1, 2, 0,
 "Execute CMD as an editor command.\n\
CMD must be a symbol that satisfies the `commandp' predicate.\n\
Optional second arg RECORD-FLAG non-nil\n\
means unconditionally put this command in the command-history.\n\
Otherwise, this is done only if an arg is read using the minibuffer.")
     (cmd, record)
     Lisp_Object cmd, record;
{
  register Lisp_Object final;
  register Lisp_Object tem;
  Lisp_Object prefixarg;
  struct backtrace backtrace;
  extern int debug_on_next_call;

  prefixarg = Vprefix_arg, Vprefix_arg = Qnil;
  Vcurrent_prefix_arg = prefixarg;
  debug_on_next_call = 0;

  if (XTYPE (cmd) == Lisp_Symbol)
    {
      tem = Fget (cmd, Qdisabled);
      if (!NULL (tem))
	return call0 (Vdisabled_command_hook);
    }

  while (1)
    {
      final = cmd;
      while (XTYPE (final) == Lisp_Symbol)
	{
	  if (EQ (Qunbound, XSYMBOL (final)->function))
	    Fsymbol_function (final);    /* Get an error! */
	  final = XSYMBOL (final)->function;
	}

      if (CONSP (final) && (tem = Fcar (final), EQ (tem, Qautoload)))
	do_autoload (final, cmd);
      else
	break;
    }

  if (CONSP (final) || XTYPE (final) == Lisp_Subr)
    {
      backtrace.next = backtrace_list;
      backtrace_list = &backtrace;
      backtrace.function = &Qcall_interactively;
      backtrace.args = &cmd;
      backtrace.nargs = 1;
      backtrace.evalargs = 0;

      tem = Fcall_interactively (cmd, record);

      backtrace_list = backtrace.next;
      return tem;
    }
  if (XTYPE (final) == Lisp_String)
    {
      return Fexecute_kbd_macro (final, prefixarg);
    }
  return Qnil;
}

DEFUN ("execute-extended-command", Fexecute_extended_command, Sexecute_extended_command,
  1, 1, "P",
  "Read function name, then read its arguments and call it.")
  (prefixarg)
     Lisp_Object prefixarg;
{
  Lisp_Object function;
  char buf[40];
  Lisp_Object saved_keys;
  struct gcpro gcpro1;

  saved_keys = Fthis_command_keys ();
  GCPRO1 (saved_keys);

  buf[0] = 0;

  if (EQ (prefixarg, Qminus))
    strcpy (buf, "- ");
  else if (CONSP (prefixarg) && XINT (XCONS (prefixarg)->car) == 4)
    strcpy (buf, "C-u ");
  else if (CONSP (prefixarg) && XTYPE (XCONS (prefixarg)->car) == Lisp_Int)
    sprintf (buf, "%d ", XINT (XCONS (prefixarg)->car));
  else if (XTYPE (prefixarg) == Lisp_Int)
    sprintf (buf, "%d ", XINT (prefixarg));

  /* This isn't strictly correct if execute-extended-command
     is bound to anything else */
  strcat (buf, "M-x ");

  function = Fcompleting_read (build_string (buf), Vobarray, Qcommandp, Qt, Qnil);

  saved_keys = concat2 (saved_keys, function);
  if (this_command_keys_size < XSTRING (function)->size)
    {
      this_command_keys_size += XSTRING (function)->size;
      this_command_keys = (char *) xrealloc (this_command_keys,
					      this_command_keys_size);
    }
  bcopy (XSTRING (function)->data, this_command_keys,
	 XSTRING (function)->size + 1);
  this_command_key_count = XSTRING (saved_keys)->size;

  UNGCPRO;

  function = Fintern (function, Vobarray);
  Vprefix_arg = prefixarg;
  this_command = function;

  return Fcommand_execute (function, Qt);
}

detect_input_pending ()
{
  if (!input_pending)
    get_input_pending (&input_pending);

  return input_pending;
}

DEFUN ("input-pending-p", Finput_pending_p, Sinput_pending_p, 0, 0, 0,
  "T if command input is currently available with no waiting.\n\
Actually, the value is NIL only if we can be sure that no input is available.")
  ()
{
  if (unread_command_char >= 0) return Qt;

  return detect_input_pending () ? Qt : Qnil;
}

DEFUN ("recent-keys", Frecent_keys, Srecent_keys, 0, 0, 0,
  "Return string of last 100 chars read from terminal.")
  ()
{
  Lisp_Object val;
  if (total_keys < sizeof recent_keys)
    return make_string (recent_keys, total_keys);

  val = make_string (recent_keys, sizeof recent_keys);
  bcopy (recent_keys + recent_keys_index,
	 XSTRING (val)->data,
	 sizeof recent_keys - recent_keys_index);
  bcopy (recent_keys,
	 XSTRING (val)->data + sizeof recent_keys - recent_keys_index,
	 recent_keys_index);
  return val;
}

DEFUN ("this-command-keys", Fthis_command_keys, Sthis_command_keys, 0, 0, 0,
  "Return string of the keystrokes that invoked this command.")
  ()
{
  return make_string (this_command_keys, this_command_key_count);
}

DEFUN ("recursion-depth", Frecursion_depth, Srecursion_depth, 0, 0, 0,
  "Return the current depth in recursive edits.")
  ()
{
  Lisp_Object temp;
  XFASTINT (temp) = command_loop_level + minibuf_level;
  return temp;
}

DEFUN ("open-dribble-file", Fopen_dribble_file, Sopen_dribble_file, 1, 1,
  "FOpen dribble file: ",
  "Start writing all keyboard characters to FILE.")
  (file)
     Lisp_Object file;
{
  file = Fexpand_file_name (file, Qnil);
  dribble = fopen (XSTRING (file)->data, "w");
  return Qnil;
}

DEFUN ("discard-input", Fdiscard_input, Sdiscard_input, 0, 0, 0,
  "Discard the contents of the terminal input buffer.\n\
Also flush any kbd macro definition in progress.")
  ()
{
  defining_kbd_macro = 0;
  update_mode_lines++;

  unread_command_char = -1;
  discard_tty_input ();

  kbd_count = 0;
  input_pending = 0;

  return Qnil;
}

DEFUN ("suspend-emacs", Fsuspend_emacs, Ssuspend_emacs, 0, 1, "",
  "Stop Emacs and return to superior process.  You can resume.\n\
If optional arg STUFFSTRING is non-nil, its characters are stuffed\n\
to be read as terminal input by Emacs's superior shell.\n\
Before suspending, if `suspend-hook' is bound and value is non-nil\n\
call the value as a function of no args.  Don't suspend if it returns non-nil.\n\
Otherwise, suspend normally and after resumption call\n\
`suspend-resume-hook' if that is bound and non-nil.")
  (stuffstring)
     Lisp_Object stuffstring;
{
  register Lisp_Object tem;
  int count = specpdl_ptr - specpdl;
  int old_height, old_width;
  int width, height;
  struct gcpro gcpro1;
  extern init_sys_modes ();

  if (!NULL (stuffstring))
    CHECK_STRING (stuffstring, 0);
  GCPRO1 (stuffstring);

  /* Call value of suspend-hook
     if it is bound and value is non-nil.  */
  tem = intern ("suspend-hook");
  tem = XSYMBOL (tem)->value;
  if (! EQ (tem, Qunbound) && ! EQ (tem, Qnil))
    {
      tem = call0 (tem);
      if (!EQ (tem, Qnil)) return Qnil;
    }

  get_screen_size (&old_width, &old_height);
  reset_sys_modes ();
  /* sys_suspend can get an error if it tries to fork a subshell
     and the system resources aren't available for that.  */
  record_unwind_protect (init_sys_modes, 0);
  stuff_buffered_input (stuffstring);
  sys_suspend ();
  unbind_to (count);

  /* Check if terminal/window size has changed.
     Note that this is not useful when we are running directly
     with a window system; but suspend should be disabled in that case.  */
  get_screen_size (&width, &height);
  if (width != old_width || height != old_height)
    change_screen_size (height, width, 0);

  /* Call value of suspend-resume-hook
     if it is bound and value is non-nil.  */
  tem = intern ("suspend-resume-hook");
  tem = XSYMBOL (tem)->value;
  if (! EQ (tem, Qunbound) && ! EQ (tem, Qnil))
    call0 (tem);
  UNGCPRO;
  return Qnil;
}

/* If STUFFSTRING is a string, stuff its contents as pending terminal input.
   Then in any case stuff anthing Emacs has read ahead and not used.  */

stuff_buffered_input (stuffstring)
     Lisp_Object stuffstring;
{
  register unsigned char *p;

/* stuff_char works only in BSD, versions 4.2 and up.  */
#ifdef BSD
#ifndef BSD4_1
  if (XTYPE (stuffstring) == Lisp_String)
    {
      register int count;

      p = XSTRING (stuffstring)->data;
      count = XSTRING (stuffstring)->size;
      while (count-- > 0)
	stuff_char (*p++);
      stuff_char ('\n');
    }
  /* Anything we have read ahead, put back for the shell to read.  */
  while (kbd_count)
    {
      stuff_char (*kbd_ptr++);
      kbd_count--;
    }
  input_pending = 0;
#endif
#endif /* BSD and not BSD4_1 */
}

set_waiting_for_input (word_to_clear)
     long *word_to_clear;
{
  input_available_clear_word = word_to_clear;

  /* Tell interrupt_signal to throw back to read_command_char,  */
  waiting_for_input = 1;

  /* If interrupt_signal was called before and buffered a C-g,
     make it run again now, to avoid timing error.  */
  if (!NULL (Vquit_flag))
    quit_throw_to_read_command_char ();

  /* Tell alarm signal to echo right away */
  echo_now = 1;

  /* If alarm has gone off already, echo now.  */
  if (echo_flag)
    {
      echo ();
      echo_flag = 0;
    }
}

clear_waiting_for_input ()
{
  /* Tell interrupt_signal not to throw back to read_command_char,  */
  waiting_for_input = 0;
  echo_now = 0;
  input_available_clear_word = 0;
}

/* This routine is called at interrupt level in response to C-G.
 If interrupt_input, this is the handler for SIGINT.
 Otherwise, it is called from kbd_buffer_store_char,
 in handling SIGIO or SIGTINT.

 If `waiting_for_input' is non zero, then unless `echoing' is nonzero,
 immediately throw back to read_command_char.

 Otherwise it sets the Lisp variable  quit-flag  not-nil.
 This causes  eval  to throw, when it gets a chance.
 If  quit-flag  is already non-nil, it stops the job right away.  */

interrupt_signal ()
{
  char c;
  /* Must preserve main program's value of errno.  */
  int old_errno = errno;
  extern Lisp_Object Vwindow_system;

#ifdef USG
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (SIGINT, interrupt_signal);
  signal (SIGQUIT, interrupt_signal);
#endif /* USG */

  cancel_echoing ();

  if (!NULL (Vquit_flag) && NULL (Vwindow_system))
    {
      fflush (stdout);
      reset_sys_modes ();
      sigfree ();
#ifdef SIGTSTP			/* Support possible in later USG versions */
/*
 * On systems which can suspend the current process and return to the original
 * shell, this command causes the user to end up back at the shell.
 * The "Auto-save" and "Abort" questions are not asked until
 * the user elects to return to emacs, at which point he can save the current
 * job and either dump core or continue.
 */
      sys_suspend ();
#else
#ifdef VMS
      if (sys_suspend () == -1)
	{
	  printf ("Not running as a subprocess;\n");
	  printf ("you can continue or abort.\n");
	}
#else /* not VMS */
      /* Perhaps should really fork an inferior shell?
	 But that would not provide any way to get back
	 to the original shell, ever.  */
      printf ("No support for stopping a process on this operating system;\n");
      printf ("you can continue or abort.\n");
#endif /* not VMS */
#endif /* not SIGTSTP */
      printf ("Auto-save? (y or n) ");
      fflush (stdout);
      if (((c = getchar ()) & ~040) == 'Y')
	Fdo_auto_save (Qnil);
      while (c != '\n') c = getchar ();
#ifdef VMS
      printf ("Abort (and enter debugger)? (y or n) ");
#else /* not VMS */
      printf ("Abort (and dump core)? (y or n) ");
#endif /* not VMS */
      fflush (stdout);
      if (((c = getchar ()) & ~040) == 'Y')
	abort ();
      while (c != '\n') c = getchar ();
      printf ("Continuing...\n");
      fflush (stdout);
      init_sys_modes ();
    }
  else
    {
      /* If executing a function that wants to be interrupted out of
	     and the user has not deferred quitting by binding `inhibit-quit'
	     then quit right away.  */
      if (immediate_quit && NULL (Vinhibit_quit))
	{
	  immediate_quit = 0;
          sigfree ();
	  Fsignal (Qquit, Qnil);
	}
      else
	/* Else request quit when it's safe */
	Vquit_flag = Qt;
    }

  if (waiting_for_input && !echoing)
    quit_throw_to_read_command_char ();

  errno = old_errno;
}

/* Handle a C-g by making read_command_char return C-g.  */

quit_throw_to_read_command_char ()
{
  quit_error_check ();
  sigfree ();
  /* Prevent another signal from doing this before we finish.  */
  waiting_for_input = 0;
  input_pending = 0;
  unread_command_char = -1;
#ifdef POLL_FOR_INPUT
  if (poll_suppress_count != 1)
    abort ();
#endif
  _longjmp (getcjmp, 1);
}

DEFUN ("set-input-mode", Fset_input_mode, Sset_input_mode, 2, 3, 0,
  "Set mode of reading keyboard input.\n\
First arg non-nil means use input interrupts; nil means use CBREAK mode.\n\
Second arg non-nil means use ^S/^Q flow control for output to terminal\n\
 (no effect except in CBREAK mode).\n\
Optional third arg non-nil specifies character to use for quitting.\n\n\
Note that the arguments will change incompatibly in version 19.")
  (interrupt, flow, quit)
     Lisp_Object interrupt, flow, quit;
{
  reset_sys_modes ();
#ifdef SIGIO
/* Note SIGIO has been undef'd if FIONREAD is missing.  */
#ifdef NO_SOCK_SIGIO
  if (read_socket_hook)
    interrupt_input = 0;	/* No interrupts if reading from a socket.  */
  else
#endif /* NO_SOCK_SIGIO */
    interrupt_input = !NULL (interrupt);
#else /* not SIGIO */
  interrupt_input = 0;
#endif /* not SIGIO */
  flow_control = !NULL (flow);
  if (!NULL (quit))
    {
      CHECK_NUMBER (quit, 2);
      quit_char = XINT (quit);
      /* Don't let this value be out of range.  */
      quit_char &= (meta_key ? 0377 : 0177);
    }
  init_sys_modes ();
  return Qnil;
}

init_keyboard ()
{
  this_command_keys_size = 40;
  this_command_keys = (char *) xmalloc (40);

  command_loop_level = -1;	/* Correct, before outermost invocation.  */
  quit_char = Ctl ('G');
  immediate_quit = 0;
  unread_command_char = -1;
  recent_keys_index = 0;
  total_keys = 0;
  kbd_count = 0;
  kbd_ptr = kbd_buffer;
  input_pending = 0;
  force_input = 0;
  if (!noninteractive)
    {
      signal (SIGINT, interrupt_signal);
#ifdef USG
      /* On USG systems, C-g is set up for both SIGINT and SIGQUIT
	 and we can't tell which one it will give us.  */
      signal (SIGQUIT, interrupt_signal);
#endif /* USG */
/* Note SIGIO has been undef'd if FIONREAD is missing.  */
#ifdef SIGIO
      signal (SIGIO, input_available_signal);
#endif SIGIO
    }

/* Use interrupt input by default, if it works and noninterrupt input
   has deficiencies.  */

#ifdef INTERRUPT_INPUT
  interrupt_input = 1;
#else
  interrupt_input = 0;
#endif

  sigfree ();
  dribble = 0;

  if (keyboard_init_hook)
    (*keyboard_init_hook) ();

  poll_suppress_count = 1;
#ifdef POLL_FOR_INPUT
  start_polling ();
#endif
}

syms_of_keyboard ()
{
  Qself_insert_command = intern ("self-insert-command");
  staticpro (&Qself_insert_command);

  Qforward_char = intern ("forward-char");
  staticpro (&Qforward_char);

  Qbackward_char = intern ("backward-char");
  staticpro (&Qbackward_char);

  Qtop_level = intern ("top-level");
  staticpro (&Qtop_level);

  Qdisabled = intern ("disabled");
  staticpro (&Qdisabled);

  defsubr (&Sread_key_sequence);
  defsubr (&Srecursive_edit);
  defsubr (&Sinput_pending_p);
  defsubr (&Scommand_execute);
  defsubr (&Srecent_keys);
  defsubr (&Sthis_command_keys);
  defsubr (&Ssuspend_emacs);
  defsubr (&Sabort_recursive_edit);
  defsubr (&Sexit_recursive_edit);
  defsubr (&Srecursion_depth);
  defsubr (&Stop_level);
  defsubr (&Sdiscard_input);
  defsubr (&Sopen_dribble_file);
  defsubr (&Sset_input_mode);
  defsubr (&Sexecute_extended_command);

  DEFVAR_LISP ("disabled-command-hook", &Vdisabled_command_hook,
    "Value is called instead of any command that is disabled\n\
\(has a non-nil  disabled  property).");

  DEFVAR_BOOL ("meta-flag", &meta_key,
    "*Non-nil means treat 0200 bit in terminal input as Meta bit.");

  DEFVAR_INT ("last-command-char", &last_command_char,
    "Last terminal input character that was part of a command, as an integer.");

  DEFVAR_INT ("last-input-char", &last_input_char,
    "Last terminal input character, as an integer.");

  DEFVAR_INT ("unread-command-char", &unread_command_char,
    "Character to be read as next input from command input stream, or -1 if none.");

  DEFVAR_INT ("meta-prefix-char", &meta_prefix_char,
    "Meta-prefix character code.  Meta-foo as command input\n\
turns into this character followed by foo.");
  meta_prefix_char = 033;

  DEFVAR_LISP ("last-command", &last_command,
    "The last command executed.  Normally a symbol with a function definition,\n\
but can be whatever was found in the keymap, or whatever the variable\n\
`this-command' was set to by that command.");
  last_command = Qnil;

  DEFVAR_LISP ("this-command", &this_command,
    "The command now being executed.\n\
The command can set this variable; whatever is put here\n\
will be in  last-command  during the following command.");
  this_command = Qnil;

  DEFVAR_INT ("auto-save-interval", &auto_save_interval,
    "*Number of keyboard input characters between auto-saves.\n\
Zero means disable autosaving.");
  auto_save_interval = 300;

  DEFVAR_INT ("echo-keystrokes", &echo_keystrokes,
    "*Nonzero means echo unfinished commands after this many seconds of pause.");
  echo_keystrokes = 1;

  DEFVAR_INT ("polling-period", &polling_period,
    "*Interval between polling for input during Lisp execution.\n\
The reason for polling is to make C-g work to stop a running program.\n\
Polling is needed only when using X windows and SIGIO does not work.\n\
Polling is automatically disabled in all other cases.");
  polling_period = 2;

  DEFVAR_INT ("help-char", &help_char,
    "Character to recognize as meaning Help.\n\
When it is read, do (eval help-form), and display result if it's a string.\n\
If help-form's value is nil, this char can be read normally.");
  help_char = Ctl ('H');

  DEFVAR_LISP ("help-form", &Vhelp_form,
    "Form to execute when character help-char is read.\n\
If the form returns a string, that string is displayed.\n\
If help-form is nil, the help char is not recognized.");
  Vhelp_form = Qnil;
  
  DEFVAR_LISP ("top-level", &Vtop_level,
    "Form to evaluate when Emacs starts up.\n\
Useful to set before you dump a modified Emacs.");
  Vtop_level = Qnil;

  DEFVAR_LISP ("keyboard-translate-table", &Vkeyboard_translate_table,
    "String used as translate table for keyboard input, or nil.\n\
Each character is looked up in this string and the contents used instead.\n\
If string is of length N, character codes N and up are untranslated."); 
  Vkeyboard_translate_table = Qnil;
}

keys_of_keyboard ()
{
  ndefkey (Vglobal_map, Ctl ('Z'), "suspend-emacs");
  ndefkey (Vctl_x_map, Ctl ('Z'), "suspend-emacs");
  ndefkey (Vesc_map, Ctl ('C'), "exit-recursive-edit");
  ndefkey (Vglobal_map, Ctl (']'), "abort-recursive-edit");
  ndefkey (Vesc_map, 'x', "execute-extended-command");
}
