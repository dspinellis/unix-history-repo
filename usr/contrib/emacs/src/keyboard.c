/* Keyboard input; editor command loop.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

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

/* Get FIONREAD, if it is available.  */
#ifdef USG
#include <termio.h>
#include <fcntl.h>
#else /* not USG */
#include <sys/ioctl.h>
#endif /* not USG */

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

extern struct backtrace *backtrace_list;

static jmp_buf getcjmp;	/* for longjmp to where kbd input is being done. */

int waiting_for_input;	/* True while doing kbd input */

static int echoing;	/* True while inside EchoKeys.  Delays C-g throwing. */

int immediate_quit;	/* Nonzero means C-G should cause immediate error-signal. */

int help_char;		/* Character to recognize as the help char.  */

Lisp_Object Vhelp_form;	/* Form to execute when help char is typed.  */

extern struct Lisp_Vector *CurrentGlobalMap;

/* Total number of times get_char has returned.  */

int num_input_chars;

/* Last input character read as a command.  */

int last_command_char;

/* Last input character read for any purpose.  */

int last_input_char;

/* If not -1, a character to be read as the next command input */

int unread_command_char;

/* Char to use as prefix when a meta character is typed in.
 This is bound on entry to minibuffer in case Esc is changed there.  */

int meta_prefix_char;

static auto_save_interval;	/* The number of keystrokes between
				   auto-saves. */
static Keystrokes;		/* The number of keystrokes since the last
				   auto-save. */

Lisp_Object last_command;	/* Previous command, represented by a Lisp object.
				   Does not include prefix commands and arg setting commands */

Lisp_Object this_command;	/* If a command sets this,
				   the value goes into previous-command for the next command. */

Lisp_Object Qself_insert_command;
Lisp_Object Qforward_char;
Lisp_Object Qbackward_char;

/* read_key_sequence stores here the command definition of the
 key sequence that it reads */
Lisp_Object read_key_sequence_cmd;

/* Form to evaluate (if non-nil) when Emacs is started */
Lisp_Object Vtop_level;

/* User-supplied string to translate input characters through */
Lisp_Object Vkeyboard_translate_table;

FILE *dribble;			/* File in which we write all commands we read */

/* Nonzero if input is available */
int input_pending;

/* Nonzero if should obey 0200 bit in input chars as "Meta" */
int MetaFlag;

/* Buffer for pre-read keyboard input */
unsigned char kbd_buffer [256];

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

static char KeyBuf[10];		/* Buffer for keys from get_char () */
static NextK;			/* Next index into KeyBuf */
static echo_keystrokes;		/* True iff we are to echo keystrokes */
static Echo1;			/* Stuff for final echo */
unsigned char *keys_prompt;	/* String to display in front of echoed keystrokes, or 0 */

#define	min(a,b)	((a)<(b)?(a):(b))

static char echobuf[100];

EchoThem (notfinal)
     register notfinal;
{
  char *p;
  int i;

  extern char *push_key_description ();

  if (!(keys_prompt || (echo_keystrokes && NextK)))
    /* (!(echo_keystrokes && (NextK || keys_prompt)) */
    return;

  echoing = 1;
  p = echobuf;
  if (keys_prompt)
    {
      strcpy (p, keys_prompt);
      p += strlen (p);
    }
  for (i = 0; i < NextK; i++)
    {
      p = push_key_description (KeyBuf[i], p);
      *p++ = ' ';
      if (i == 0 && KeyBuf[0] == help_char)
	{
	  strcpy (p, "(Type ? for further options) ");
	  p += strlen (p);
	}
    }
  if (notfinal && NextK
      && !(NextK == 1 && KeyBuf[0] == help_char))
    p[-1] = '-';
  *p = 0;
  minibuf_message = echobuf;

  if (notfinal)
    Echo1++;		/* set echoed-flag */
  if (notfinal >= 0)
    DoDsp (0);

  echoing = 0;

  if (waiting_for_input && !NULL (Vquit_flag))
    quit_throw_to_get_char ();
}

Lisp_Object recursive_edit_unwind (), command_loop ();
Lisp_Object cmd_error ();
Lisp_Object top_level_1 ();

DEFUN ("recursive-edit", Frecursive_edit, Srecursive_edit, 0, 0, "",
  "Invoke the editor command loop recursively.\n\
Do (throw 'exit nil) within the command loop to make this function return,\n\
or (throw 'exit t) to make this function signal an error.\n\
This function is called by the editor initialization\n\
to begin editing.")
  ()
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object val;

  RecurseDepth++;
  RedoModes++;

  record_unwind_protect (recursive_edit_unwind,
			 (RecurseDepth &&
			  bf_cur != XBUFFER (XWINDOW (selected_window)->buffer))
			 ? Fcurrent_buffer ()
			 : Qnil);

  if (!RecurseDepth)
    internal_condition_case (top_level_1, Qerror, cmd_error);

  while (1)
    {
      val = internal_condition_case (command_loop, Qerror, cmd_error);
      /* Value is number if returned due to cmd_error.
	 In that case, just loop around.  */
      if (NULL (val))
	break;
      if (EQ (val, Qt))
	Fsignal (Qquit, Qnil);
    }

  unbind_to (count);
  return Qnil;
}

Lisp_Object
recursive_edit_unwind (buffer)
     Lisp_Object buffer;
{
  if (!NULL (buffer))
    Fset_buffer (buffer);
  RecurseDepth--;
  RedoModes++;
  return Qnil;
}

Lisp_Object
cmd_error (data)
     Lisp_Object data;
{
  Lisp_Object errmsg, tail, errname, file_error;
  int i;

  Vquit_flag = Qnil;
  Vinhibit_quit = Qt;
  Vstandard_input = Qt;
  Vstandard_output = Qt;
  Vexecuting_macro = Qnil;
  minibuf_message = 0;

  Fdiscard_input ();
  Ding ();

  errname = Fcar (data);

  if (EQ (errname, Qerror))
    {
      data = Fcdr (data);
      if (!LISTP (data)) data = Qnil;
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
  
  if (!LISTP (data)) data = Qnil;
  tail = Fcdr (data);

  /* For file-error, make error message by concatenating
     all the data items.  They are all strings.  */
  if (!NULL (file_error))
    errmsg = XCONS (tail)->car, tail = XCONS (tail)->cdr;

  if (XTYPE (errmsg) == Lisp_String)
    Fprinc (errmsg, Qt);
  else
    write_string_1 ("peculiar error", -1, Qt);

  for (i = 0; LISTP (tail); tail = Fcdr (tail), i++)
    {
      write_string_1 (i ? ", " : ": ", 2, Qt);
      if (!NULL (file_error))
	Fprinc (Fcar (tail), Qt);
      else
	Fprin1 (Fcar (tail), Qt);
    }

  /* In -batch mode, force out the error message and newlines after it */
  if (noninteractive)
    message ("");

  Vquit_flag = Qnil;

  Vinhibit_quit = Qnil;
  return make_number (0);
}

void
debugger (sig, data)
     Lisp_Object sig, data;
{
  Fterpri (Qnil);
  Fprinc (Fget (sig, Qerror_message), Qnil);
  putchar (' ');
  Fprin1 (data, Qnil);
  Fterpri (Qnil);
  abort ();
}

Lisp_Object command_loop_1 ();

/* Entry to editor-command-loop.
   This level has the catches for exiting/returning to editor command loop.
   It returns nil to exit recursive edit, t to abort it.  */

Lisp_Object
command_loop ()
{
  if (RecurseDepth)
    {
      return internal_catch (Qexit, command_loop_1, Qnil);
    }
  else
    {
      while (1)
	{
	  internal_catch (Qtop_level, command_loop_1, Qnil);
	  top_level_1 ();
	}
    }
}

Lisp_Object
top_level_1 ()
{
  /* On entry to the outer level, run the startup file */
  if (!NULL (Vtop_level))
    Feval (Vtop_level);
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
  if (RecurseDepth)
    Fthrow (Qexit, Qnil);
  error ("No recursive edit is in progress");
}

DEFUN ("abort-recursive-edit", Fabort_recursive_edit, Sabort_recursive_edit, 0, 0, "",
  "Abort the command that requested this recursive edit or minibuffer input.")
  ()
{
  if (RecurseDepth)
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

  Vprefix_arg = Qnil;
  waiting_for_input = 0;
  Echo1 = 0;
  NextK = 0;
  Vstandard_input = Qt;
  Vstandard_output = Qt;
  last_command = Qt;
  nonundocount = 0;
  
  while (1)
    {
#ifdef C_ALLOCA
      alloca (0);		/* Cause a garbage collection now */
				/* Since we can free the most stuff here.  */
#endif /* C_ALLOCA */

      /* Install chars successfully executed in kbd macro */

      if (defining_kbd_macro)
	finalize_kbd_macro_chars ();

      /* Make sure current window's buffer is selected.  */

      if (XBUFFER (XWINDOW (selected_window)->buffer) != bf_cur)
	SetBfp (XBUFFER (XWINDOW (selected_window)->buffer));

      /* If minibuffer on and echo area in use,
	 wait 2 sec and redraw minibufer.  */

      if (MinibufDepth && minibuf_message)
	{
	  Fsit_for (make_number (2));
	  minibuf_message = 0;
	  if (!NULL (Vquit_flag))
	    {
	      Vquit_flag = Qnil;
	      unread_command_char = Ctl ('g');
	    }
	}

      /* Read next key sequence; i gets its length.  */

      i = read_key_sequence (keybuf, sizeof keybuf, 0);
      if (!i)			/* End of file -- happens only in */
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

      if (NULL (cmd))
	{
	  Ding ();
	  defining_kbd_macro = 0;
	  RedoModes++;
	  Vprefix_arg = Qnil;
	}
      else
	{
	  this_command = cmd;
	  if (NULL (Vprefix_arg))
	    {
	      if (EQ (cmd, Qforward_char) && point <= NumCharacters)
		{
		  lose = CharAt (point);
		  SetPoint (point + 1);
		  if (lose >= ' ' && lose < 0177
		      && (XFASTINT (XWINDOW (selected_window)->last_modified)
			  >= bf_modified)
		      && (XFASTINT (XWINDOW (selected_window)->last_point)
			  == point)
		      && !windows_or_buffers_changed
		      && !detect_input_pending ()
		      && NULL (Vexecuting_macro))
		    direct_output_forward_char (1);
		  goto directly_done;
		}
	      else if (EQ (cmd, Qbackward_char) && point > FirstCharacter)
		{
		  SetPoint (point - 1);
		  lose = CharAt (point);
		  if (lose >= ' ' && lose < 0177
		      && (XFASTINT (XWINDOW (selected_window)->last_modified)
			  >= bf_modified)
		      && (XFASTINT (XWINDOW (selected_window)->last_point)
			  == point)
		      && !windows_or_buffers_changed
		      && !detect_input_pending ()
		      && NULL (Vexecuting_macro))
		    direct_output_forward_char (-1);
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
			  < bf_modified)
		    || (XFASTINT (XWINDOW (selected_window)->last_point)
			  != point)
		    || bf_modified <= bf_cur->save_modified
		    || windows_or_buffers_changed
		    || detect_input_pending ()
		    || !NULL (Vexecuting_macro);
		  if (SelfInsert (last_command_char))
		    {
		      lose = 1;
		      nonundocount = 0;
		    }
		  if (!lose
		      && (point == NumCharacters + 1 || CharAt (point) == '\n')
		      && last_command_char >= ' '
		      && last_command_char < 0177)
		    direct_output_for_insert (last_command_char);
		  goto directly_done;
		}
	    }

	  /* Here for a command that isn't executed directly */

	  nonundocount = 0;
	  if (NULL (Vprefix_arg) && NULL (Vexecuting_macro) &&
	      !EQ (minibuf_window, selected_window))
	    Fundo_boundary ();
	  Fcommand_execute (cmd, Qnil);

	directly_done: ;
	}

      if (NULL (Vprefix_arg))
	{
	  last_command = this_command;
	  NextK = 0;
	  Echo1 = 0;
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
  /* Note: no need to reestablish handler on USG systems
     because it is established, if approriate, each time an alarm is requested.  */
#if defined(BSD4_1) && defined(subprocesses)
  extern int select_alarmed;
  if (select_alarmed == 0)
    {
      select_alarmed = 1;
      sigrelse (SIGALRM);
      return;
    }
#endif
#ifdef BSD4_1
  sigisheld (SIGALRM);
#endif

  if (echo_now)
    EchoThem (1);
  else
    echo_flag = 1;

#ifdef BSD4_1
  sigunhold (SIGALRM);
#endif
}

/* read a character from the keyboard; call the redisplay if needed */
/* commandflag nonzero means auto-saving may be considered */
get_char (commandflag)
     int commandflag;
{
  register int c;
  register alarmtime = echo_keystrokes;
  int count;
  Lisp_Object tem;
  extern request_echo ();

  if ((c = unread_command_char) >= 0)
    {
      unread_command_char = -1;
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

  if (!input_pending && !detect_input_pending ())
    {
      DoDsp (0);
      if (auto_save_interval > 0 && commandflag
	  && Keystrokes > auto_save_interval
	  && Keystrokes > 20)
	{
	  Fdo_auto_save (Qnil);
	  Keystrokes = 0;
	}
    }

  Keystrokes++;

  if (_setjmp (getcjmp))
    {
      c = Ctl('g');
      waiting_for_input = 0;
      input_available_clear_word = 0;

      goto non_reread;
    }

  /* If echoing already, we will echo without delay, so need no interrupt */
  /* If no keys accumulated yet, need no interrupt. */
  /* If minibuffer active, do not echo keystrokes. */
  if (Echo1 || !NextK || MinibufDepth)
    alarmtime = 0;
    /* Message turns off echoing unless more keystrokes turn it on again. */
  if (minibuf_message && minibuf_message != echobuf)
    alarmtime = 0, Echo1 = 0, NextK = 0;

  if (Echo1)		/* If already echoing, put a dash at the end now */
    EchoThem (1);

  /* Else start echoing if user waits more than `alarmtime' seconds. */
  if (alarmtime > 0)
    {
      /* This interrupt either calls EchoThem right away
	 or sets echo_flag, which causes EchoThem to be called
	 by set_waiting_for_input's next invocation.  */
      signal (SIGALRM, request_echo);
      echo_flag = 0;
      echo_now = 0;
      alarm ((unsigned) alarmtime);
    }

#ifdef DEBUG
  /* This should be impossible, but I suspect it happens.  */
  if (getcjmp[0] == 0)
    abort ();
#endif /* DEBUG */

  c = kbd_buffer_get_char ();

 non_reread:

#ifdef DEBUG
  /* Cause immediate crash if anyone tries to throw back to this frame
     beyond here.  */
  getcjmp[0] = 0;
#endif /* DEBUG */

  /* Cancel alarm if it was set and has not already gone off. */
  if (alarmtime > 0) alarm (0);

  minibuf_message = 0;

  if (c < 0) return -1;

  c &= MetaFlag ? 0377 : 0177;

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

 from_macro:
  if (NextK < sizeof KeyBuf)
    KeyBuf[NextK++] = c;

  /* If already echoing, echo right away. */
  if (Echo1)
    EchoThem (0);

 reread:
  /* If the first character of a command is being reread,
     store it in case a pause follows and it must be echoed later.
     This has no effect on a non-reread character
     since NextK is not zero here for them.  */
  if (NextK == 0)
    KeyBuf[NextK++] = c;

  last_input_char = c;

  num_input_chars++;

  /* Process the help character specially if enabled */
  if (c == help_char && !NULL (Vhelp_form))
    {
      count = specpdl_ptr - specpdl;

      record_unwind_protect (save_window_restore, save_window_save ());

      tem = Feval (Vhelp_form);
      if (XTYPE (tem) == Lisp_String)
	internal_with_output_to_temp_buffer ("*Help*", print_help, tem);

      NextK = 0;
      c = get_char (0);
      /* Remove the help from the screen */
      unbind_to (count);
      DoDsp (0);
      if (c == 040)
	{
	  NextK = 0;
	  c = get_char (0);
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
 from which they are obtained by kbd_buffer_get_char.  */

kbd_buffer_get_char ()
{
  register int c;
  int nread;

  if (noninteractive)
    {
      c = getchar ();
      if (c < 0)		/* In batch mode, die at input eof */
	Fkill_emacs (Qt);
      return c;
    }

  /* Either ordinary input buffer or C-g buffered means we can return.  */
  while (!kbd_count)
    {
      if (!NULL (Vquit_flag))
	quit_throw_to_get_char ();

#ifdef SIGIO
      gobble_input ();
#endif /* SIGIO */
      if (!kbd_count)
	{
#ifdef subprocesses
	  wait_reading_process_input (0, -1, 1);
#else
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
	      read_avail_input ();
	    }
	}
    }

  input_pending = --kbd_count > 0;
  c = *kbd_ptr;			/* *kbd_ptr++ would have a timing error. */
  kbd_ptr++;			/* See kbd_buffer_store_char. */
  return c & 0377;		/* Clean up if sign was extended. */
}

/* Store into *addr the number of terminal input chars available.
   Equivalent to ioctl (0, FIONREAD, addr) but works
   even if FIONREAD does not exist.  */

get_input_pending (addr)
     int *addr;
{
#ifdef FIONREAD
  if (ioctl (0, FIONREAD, addr) < 0)
    *addr = 0;
#else /* no FIONREAD */
  read_avail_input ();
  *addr = kbd_count;
#endif /* no FIONREAD */
}

/* Read any terminal input already buffered up by the system
   into the kbd_buffer, assuming the buffer is currently empty.  */

read_avail_input ()
{
#ifdef FIONREAD
  int nread;
  get_input_pending (&nread);
  if (nread > sizeof kbd_buffer)
    nread = sizeof kbd_buffer;
  if (!nread)
    nread = 1;
  set_waiting_for_input (0);
  kbd_count = read (0, kbd_buffer, nread);
  clear_waiting_for_input ();
  kbd_ptr = kbd_buffer;
#else /* no FIONREAD */
#ifdef USG
  /* Assume this is only done when the buffer is empty.
     It's stupid to call this function without checking kbd_count.  */
  if (kbd_count)
    abort ();

  fcntl (fileno (stdin), F_SETFL, O_NDELAY);
  kbd_ptr = kbd_buffer;
  kbd_count = read (fileno (stdin), kbd_buffer, sizeof kbd_buffer);
  fcntl (fileno (stdin), F_SETFL, 0);
#else /* not USG */
  you lose
#endif /* not USG */
#endif /* no FIONREAD */
}

#ifdef SIGIO   /* for entire page */

gobble_input ()
{
  int nread;
  if (interrupt_input)
    {
      get_input_pending (&nread);
      if (nread)
	{
	  sigholdx (SIGIO);
	  input_available_signal (SIGIO);
	  sigfree ();
	}
    }
}

/* Set this for debugging, to have a way to get out */
int stop_character;

/* Store a character obtained at interrupt level into kbd_buffer, fifo */
kbd_buffer_store_char (c)
     int c;
{
  if (!MetaFlag)
    c &= 0177;
  else
    c &= 0377;

  if (c == 07)
    {
      interrupt_signal ();
      return;
    }

  if (c && c == stop_character)
    {
#ifdef SIGTSTP			/* Support possible in later USG versions */
      kill (getpid (), SIGTSTP);
#else
      fake_suspend ();
#endif /* not SIGTSTP */
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

input_available_signal (signo)
     int signo;
{
  unsigned char buf[64];
  int nread;
  register int i;
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
      get_input_pending (&nread);
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
	  if (buf[i] == Ctl('G'))
	    break;
	}
    }
#ifdef BSD4_1
  sigfree ();
#endif
}
#endif /* SIGIO */

/* Read a sequence of keys that ends with a non prefix character,
 and store them in keybuf, a buffer of size bufsize.
 Prompt with `prompt'.  Echo starting immediately unless `prompt' is 0.
 Return the length of the key sequence stored.
*/

int
read_key_sequence (keybuf, bufsize, prompt)
     char *keybuf;
     unsigned char *prompt;
{
  register int i;
  Lisp_Object nextlocal, nextglobal;
  register int c, nextc;
  Lisp_Object local, global;

  keys_prompt = prompt;

  if (prompt)
    NextK = 0;
  if (prompt && INTERACTIVE)
    EchoThem (1);

  nextlocal = bf_cur->keymap;
  XSET (nextglobal, Lisp_Vector, CurrentGlobalMap);

  i = 0;
  nextc = -1;
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
	c = get_char (!prompt);
      Vquit_flag = Qnil;

      if (c < 0)
	return 0;
      if (c >= 0200)
	{
	  nextc = c & 0177;
	  c = meta_prefix_char;
	}

      keybuf[i++] = c;

      global = !NULL (nextglobal)
	? get_keyelt (access_keymap (nextglobal, c))
	  : Qnil;

      local = !NULL (nextlocal)
	? get_keyelt (access_keymap (nextlocal, c))
	  : Qnil;

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
	  (LISTP (local) && EQ (XCONS (local)->car, Qkeymap))
	  ||
	  /* If nextc is set, we are processing a prefix char
	     that represents a meta-bit.
	     Let a global prefix definition override a local non-prefix.
	     This is for minibuffers that redefine Escape for completion.
	     A real Escape gets completion, but Meta bits get ESC-prefix.  */
	  ((NULL (local) || nextc >= 0)
	   && (XTYPE (global) == Lisp_Vector ||
	       (LISTP (global) && EQ (XCONS (global)->car, Qkeymap)))))
	{
	  if (XTYPE (local) == Lisp_Vector ||
	      (LISTP (local) && EQ (XCONS (local)->car, Qkeymap)))
	    nextlocal = local;
	  else
	    nextlocal = Qnil;

	  if (XTYPE (global) == Lisp_Vector ||
	      (LISTP (global) && EQ (XCONS (global)->car, Qkeymap)))
	    nextglobal = global;
	  else
	    nextglobal = Qnil;
	}
    }

  keys_prompt = 0;
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
  i = read_key_sequence (keybuf, sizeof keybuf,
			 (NULL (prompt)) ? 0 : XSTRING (prompt)->data);
  return make_string (keybuf, i);
}

DEFUN ("command-execute", Fcommand_execute, Scommand_execute, 1, 2, 0,
 "Execute CMD as an editor command.\n\
CMD must be a symbol with a function definition;\n\
also, it must satisfy the commandp predicate.")
     (cmd, record)
     Lisp_Object cmd, record;
{
  Lisp_Object final;
  Lisp_Object tem;
  Lisp_Object prefixarg;
  struct backtrace backtrace;

  prefixarg = Vprefix_arg, Vprefix_arg = Qnil;
  Vcurrent_prefix_arg = prefixarg;

  if (XTYPE (cmd) == Lisp_Symbol)
    {
      tem = Fget (cmd, Qdisabled);
      if (!NULL (tem))
	return Fapply (Vdisabled_command_hook, Qnil);
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

      if (LISTP (final) && (tem = Fcar (final), EQ (tem, Qautoload)))
	do_autoload (final, cmd);
      else
	break;
    }

  if (LISTP (final) || XTYPE (final) == Lisp_Subr)
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

detect_input_pending ()
{
  if (!input_pending)
    {
      if (kbd_count)
	input_pending = kbd_count;
      else
	get_input_pending (&input_pending);
    }
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
  return make_string (KeyBuf, NextK);
}

DEFSIMPLE ("recursion-depth", Frecursion_depth, Srecursion_depth,
	   "Return the current depth in recursive edits.",
	   Lisp_Int, XSETINT, RecurseDepth)

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
  RedoModes++;

  unread_command_char = -1;
  discard_tty_input ();

  kbd_count = 0;
  input_pending = 0;

  return Qnil;
}

DEFUN ("suspend-emacs", Fsuspend_emacs, Ssuspend_emacs, 0, 1, "",
  "Stop Emacs and return to superior process.  You can resume.\n\
If optional arg STUFFSTRING is non-nil, its characters are stuffed\n\
to be read as terminal input by Emacs's superior shell.")
  (stuffstring)
     Lisp_Object stuffstring;
{
#ifdef SIGTSTP			/* Support possible in later USG versions */
  if (!NULL (stuffstring))
    CHECK_STRING (stuffstring, 0);

  reset_sys_modes ();
  stuff_buffered_input (stuffstring);
  kill (0, SIGTSTP);
  init_sys_modes ();
#else
  fake_suspend ();
#endif
  return Qnil;
}

#ifndef SIGTSTP
/* On a system where suspending is not implemented,
   instead fork a subshell and let it talk directly to the terminal
   while we wait.  */

fake_suspend ()
{
  int pid = fork ();
  int spid;
  int status;
  char *sh;
  int (*interrupt) ();
  int (*quit) ();
  int (*term) ();
  unsigned char *temp;
  int i;

  if (pid == -1)
    error ("Can't spawn subshell");
  if (pid == 0)
    {
      sh = (char *) getenv ("SHELL");
      if (sh == 0)
	sh = "sh";
      /* Use our buffer's default directory for the subshell.  */
      if (XTYPE (bf_cur->directory) == Lisp_String)
	{
	  temp = (unsigned char *) alloca (XSTRING (bf_cur->directory)->size + 2);
	  bcopy (XSTRING (bf_cur->directory)->data, temp,
		 XSTRING (bf_cur->directory)->size);
	  i = XSTRING (bf_cur->directory)->size;
	  if (temp[i - 1] != '/') temp[i++] = '/';
	  temp[i] = 0;
	  chdir (temp);
	}
      execlp (sh, sh, 0);
      write (1, "Can't execute subshell", 22);
      _exit (1);
    }
  interrupt = signal (SIGINT, SIG_IGN);
  quit = signal (SIGQUIT, SIG_IGN);
  term = signal (SIGTERM, SIG_IGN);
  reset_sys_modes ();
  wait_for_termination (pid);
  init_sys_modes ();
  signal (SIGINT, interrupt);
  signal (SIGQUIT, quit);
  signal (SIGTERM, term);
}
#endif /* no SIGTSTP */

/* If STUFFSTRING is a string, stuff its contents as pending terminal input.
   Then in any case stuff anthing Emacs has read ahead and not used.  */

stuff_buffered_input (stuffstring)
     Lisp_Object stuffstring;
{
  register unsigned char *p;
  register int count;

/* stuff_char works only in BSD, versions 4.2 and up.  */
#if defined (BSD) && ~defined (BSD4_1)
  if (XTYPE (stuffstring) == Lisp_String)
    {
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
#endif /* BSD and not BSD4_1 */
}

set_waiting_for_input (word_to_clear)
     long *word_to_clear;
{
  input_available_clear_word = word_to_clear;

  /* Tell interrupt_signal to throw back to get_char,  */
  waiting_for_input = 1;

  /* If interrupt_signal was called before and buffered a C-g,
     make it run again now, to avoid timing error.  */
  if (!NULL (Vquit_flag))
    quit_throw_to_get_char ();

  /* Tell alarm signal to echo right away */
  echo_now = 1;

  /* If alarm has gone off already, echo now.  */
  if (echo_flag)
    {
      EchoThem (1);
      echo_flag = 0;
    }
}

clear_waiting_for_input ()
{
  /* Tell interrupt_signal not to throw back to get_char,  */
  waiting_for_input = 0;
  echo_now = 0;
  input_available_clear_word = 0;
}

/* This routine is called at interrupt level in response to C-G.
 If interrupt_input, this is the handler for SIGINT.
 Otherwise, it is called from kbd_buffer_store_char,
 in handling SIGIO or SIGTINT.

 If `waiting_for_input' is non zero, then unless `echoing' is nonzero,
 immediately throw back to get_char.

 Otherwise it sets the Lisp variable  quit-flag  not-nil.
 This causes  eval  to throw, when it gets a chance.
 If  quit-flag  is already non-nil, it stops the job right away.  */

interrupt_signal ()
{
  char c;

#ifdef USG
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (SIGINT, interrupt_signal);
  signal (SIGQUIT, interrupt_signal);
#endif /* USG */

  Echo1 = 0;

  if (!NULL (Vquit_flag))
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
      kill (0, SIGTSTP);
#else
      /* Perhaps should really fork an inferior shell?
	 But that would not provide any way to get back
	 to the original shell, ever.  */
      printf ("No support for stopping a process in this Unix version;\n");
      printf ("you can continue or abort.\n");
#endif /* not SIGTSTP */
      printf ("Auto-save? (y or n) ");
      fflush (stdout);
      if (((c = getchar ()) & ~040) == 'Y')
	Fdo_auto_save (Qnil);
      while (c != '\n') c = getchar ();
      printf ("Abort (and dump core)? (y or n) ");
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
    quit_throw_to_get_char ();
}

/* Handle a C-g by making get_char return C-g.  */

quit_throw_to_get_char ()
{
  sigfree ();
  /* Prevent another signal from doing this before we finish.  */
  waiting_for_input = 0;
  input_pending = 0;
  unread_command_char = -1;
  _longjmp (getcjmp, 1);
}

DEFUN ("set-input-mode", Fset_input_mode, Sset_input_mode, 2, 2, 0,
  "Set mode of reading keyboard input.\n\
First arg non-nil means use input interrupts; nil means use CBREAK mode.\n\
Second arg non-nil means use ^S/^Q flow control\n\
 (no effect except in CBREAK mode).")
  (interrupt, flow)
     Lisp_Object interrupt, flow;
{
  reset_sys_modes ();
#ifdef SIGIO
  interrupt_input = !NULL (interrupt);
#else /* not SIGIO */
  interrupt_input = 0;
#endif /* not SIGIO */
  flow_control = !NULL (flow);
  init_sys_modes ();
  return Qnil;
}

init_keyboard ()
{
  RecurseDepth = -1;	/* Correct, before outermost invocation of editor loop */
  keys_prompt = 0;
  immediate_quit = 0;
  unread_command_char = -1;
  recent_keys_index = 0;
  total_keys = 0;
  kbd_count = 0;
  kbd_ptr = kbd_buffer;
  input_pending = 0;
  if (!noninteractive)
    {
      signal (SIGINT, interrupt_signal);
#ifdef USG
      /* On USG systems, C-g is set up for both SIGINT and SIGQUIT
	 and we can't tell which one it will give us.  */
      signal (SIGQUIT, interrupt_signal);
#endif /* USG */
    }
#ifdef SIGIO
  signal (SIGIO, input_available_signal);
#endif SIGIO

/* Use interrupt input by default, if it works and noninterrupt input
   has deficiencies.  */

#ifdef INTERRUPT_INPUT
  interrupt_input = 1;
#else
  interrupt_input = 0;
#endif

  sigfree ();
  dribble = 0;
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

  DefLispVar ("disabled-command-hook", &Vdisabled_command_hook,
    "Value is called instead of any command that is disabled\n\
\(has a non-nil  disabled  property).");

  DefBoolVar ("meta-flag", &MetaFlag,
    "*Non-nil means treat 0200 bit in terminal input as Meta bit.");

  DefIntVar ("last-command-char", &last_command_char,
    "Last terminal input character that was part of a command, as an integer.");

  DefIntVar ("last-input-char", &last_input_char,
    "Last terminal input character, as an integer.");

  DefIntVar ("unread-command-char", &unread_command_char,
    "Character to be read as next input from command input stream, or -1 if none.");

  DefIntVar ("meta-prefix-char", &meta_prefix_char,
    "Meta-prefix character code.  Meta-foo as command input\n\
turns into this character followed by foo.");
  meta_prefix_char = 033;

  DefLispVar ("last-command", &last_command,
    "The last command executed.  Normally a symbol with a function definition,\n\
but can be whatever was found in the keymap, or whatever that command left in this-command.");
  last_command = Qnil;

  DefLispVar ("this-command", &this_command,
    "The command now being executed.\n\
The command can set this variable; whatever is put here\n\
will be in  last-command  during the following command.");
  this_command = Qnil;

  DefIntVar ("auto-save-interval", &auto_save_interval,
    "*Number of keyboard input characters between auto-saves.");
  auto_save_interval = 300;

  DefIntVar ("echo-keystrokes", &echo_keystrokes,
    "*Nonzero means echo unfinished commands after this many seconds of pause.");
  echo_keystrokes = 1;

  DefIntVar ("help-char", &help_char,
    "Character to recognize as meaning Help.\n\
When it is read, do (eval help-form), and display result if it's a string.\n\
If help-form's value is nil, this char can be read normally.");
  help_char = Ctl ('H');

  DefLispVar ("help-form", &Vhelp_form,
    "Form to execute when character help-char is read.\n\
If the form returns a string, that string is displayed.\n\
If help-form is nil, the help char is not recognized.");
  Vhelp_form = Qnil;
  
  DefLispVar ("top-level", &Vtop_level,
    "Form to evaluate when Emacs starts up.\n\
Useful to set before you dump a modified Emacs.");
  Vtop_level = Qnil;

  DefLispVar ("keyboard-translate-table", &Vkeyboard_translate_table,
    "String used as translate table for keyboard input, or nil.\n\
Each character is looked up in this string and the contents used instead.\n\
If string is of length N, character codes N and up are untranslated."); 
  Vkeyboard_translate_table = Qnil;
}

keys_of_keyboard ()
{
  defkey (GlobalMap, Ctl ('Z'), "suspend-emacs");
  defkey (CtlXmap, Ctl ('Z'), "suspend-emacs");
  defkey (ESCmap, Ctl ('C'), "exit-recursive-edit");
  defkey (GlobalMap, Ctl (']'), "abort-recursive-edit");
}
