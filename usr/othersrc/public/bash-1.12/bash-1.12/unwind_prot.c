/* I can't stand it anymore!  Please can't we just write the
   whole Unix system in lisp or something? */

/* Copyright (C) 1987,1989 Free Software Foundation, Inc.

This file is part of GNU Bash, the Bourne Again SHell.

Bash is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 1, or (at your option) any later
version.

Bash is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with Bash; see the file COPYING.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

/* **************************************************************** */
/*								    */
/*		      Unwind Protection Scheme for Bash		    */
/*								    */
/* **************************************************************** */
#include <signal.h>
#include "config.h"
#include "general.h"
#include "unwind_prot.h"

/* If CLEANUP is null, then ARG contains a tag to throw back to. */
typedef struct _uwp {
  struct _uwp *next;
  Function *cleanup;
  char *arg;
} UNWIND_ELT;

static void
  unwind_frame_discard_internal (), unwind_frame_run_internal (),
  add_unwind_protect_internal (), remove_unwind_protect_internal (),
  run_unwind_protects_internal ();

static UNWIND_ELT *unwind_protect_list = (UNWIND_ELT *)NULL;

/* Run a function without interrupts. */
void
without_interrupts (function, arg1, arg2)
     VFunction *function;
     char *arg1, *arg2;
{
#if defined (_POSIX_VERSION)
  sigset_t set, oset;

  sigemptyset (&set);
  sigemptyset (&oset);

  sigaddset (&set, SIGINT);
  sigprocmask (SIG_BLOCK, &set, &oset);
#else
#  if defined (USG)
  SigHandler *old_int;

  old_int = (SigHandler *)signal (SIGINT, SIG_IGN);
#  else
  int oldmask = sigblock (SIGINT);
#  endif
#endif

  (*function)(arg1, arg2);

#if defined (_POSIX_VERSION)
  sigprocmask (SIG_SETMASK, &oset, (sigset_t *)NULL);
#else
#  if defined (USG)
  signal (SIGINT, old_int);
#  else
  sigsetmask (oldmask);
#  endif
#endif
}

/* Start the beginning of a region. */
void
begin_unwind_frame (tag)
     char *tag;
{
  add_unwind_protect ((Function *)NULL, tag);
}

/* Discard the unwind protects back to TAG. */
void
discard_unwind_frame (tag)
     char *tag;
{
  without_interrupts (unwind_frame_discard_internal, tag, (char *)NULL);
}

/* Run the unwind protects back to TAG. */
void
run_unwind_frame (tag)
     char *tag;
{
  without_interrupts (unwind_frame_run_internal, tag, (char *)NULL);
}

/* Add the function CLEANUP with ARG to the list of unwindable things. */
void
add_unwind_protect (cleanup, arg)
     Function *cleanup;
     char *arg;
{
  without_interrupts (add_unwind_protect_internal, (char *)cleanup, arg);
}

/* Remove the top unwind protect from the list. */
void
remove_unwind_protect ()
{
  without_interrupts
    (remove_unwind_protect_internal, (char *)NULL, (char *)NULL);
}

/* Run the list of cleanup functions in unwind_protect_list. */
void
run_unwind_protects ()
{
  without_interrupts
    (run_unwind_protects_internal, (char *)NULL, (char *)NULL);
}

/* **************************************************************** */
/*								    */
/*                        The Actual Functions                 	    */
/*								    */
/* **************************************************************** */

static void
add_unwind_protect_internal (cleanup, arg)
     Function *cleanup;
     char *arg;
{
  UNWIND_ELT *elt;

  elt = (UNWIND_ELT *)xmalloc (sizeof (UNWIND_ELT));
  elt->cleanup = cleanup;
  elt->arg = arg;
  elt->next = unwind_protect_list;
  unwind_protect_list = elt;
}

static void
remove_unwind_protect_internal ()
{
  UNWIND_ELT *elt = unwind_protect_list;

  if (elt)
    {
      unwind_protect_list = unwind_protect_list->next;
      free (elt);
    }
}

static void
run_unwind_protects_internal ()
{
  UNWIND_ELT *t, *elt = unwind_protect_list;

  while (elt)
   {
      /* This function can be run at strange times, like when unwinding
	the entire world of unwind protects.  Thus, we may come across
	 an element which is simply a label for a catch frame.  Don't call
	 the non-existant function. */
      if (elt->cleanup)
	(*(elt->cleanup)) (elt->arg);

      t = elt;
      elt = elt->next;
      free (t);
    }
  unwind_protect_list = elt;
}

static void
unwind_frame_discard_internal (tag)
     char *tag;
{
  UNWIND_ELT *elt;

  while (elt = unwind_protect_list)
    {
      unwind_protect_list = unwind_protect_list->next;
      if (!elt->cleanup && (STREQ (elt->arg, tag)))
	{
	  free (elt);
	  break;
	}
      else
	free (elt);
    }
}

static void
unwind_frame_run_internal (tag)
     char *tag;
{
  UNWIND_ELT *elt;

  while (elt = unwind_protect_list)
    {
      unwind_protect_list = elt->next;

      /* If tag, then compare. */
      if (!elt->cleanup)
	{
	  if (strcmp (elt->arg, tag) == 0)
	    {
	      free (elt);
	      break;
	    }
	  free (elt);
	  continue;
	}
      else
	{
	  (*(elt->cleanup)) (elt->arg);
	  free (elt);
	}
    }
}

/* Structure describing a saved variable and the value to restore it to. */
typedef struct {
  int *variable;
  char *desired_setting;
  int size;
} SAVED_VAR;

/* Restore the value of a variable, based on the contents of SV.  If
   sv->size is greater than sizeof (char *), sv->desired_setting points to
   a block of memory SIZE bytes long holding the value, rather than the
   value itself.  This block of memory is copied back into the variable. */
static void
restore_variable (sv)
     SAVED_VAR *sv;
{

  /* I wrote this switch statement not realizing how silly some compilers
     can be.  Since we expect both cases to be the same size, it really
     makes no difference (today), but it irks me that I cannot express the
     thought clearly. */
  switch (sv->size)
    {
    /* case sizeof (char *): */
    case sizeof (int):
      *(sv->variable) = (int)sv->desired_setting;
      break;

    default:
      bcopy (sv->desired_setting, (char *)sv->variable, sv->size);
      free (sv->desired_setting);
    }

  free (sv);
}

/* Save the value of a variable so it will be restored when unwind-protects
   are run.  VAR is a pointer to the variable.  VALUE is the value to be
   saved.  SIZE is the size in bytes of VALUE.  If SIZE is bigger than what
   can be saved in a char *, memory will be allocated and the value saved
   into that using bcopy (). */
void
unwind_protect_var (var, value, size)
     int *var;
     char *value;
     int size;
{
  SAVED_VAR *s = (SAVED_VAR *)xmalloc (sizeof (SAVED_VAR));

  s->variable = var;
  if (size > sizeof (char *))
    {
      s->desired_setting = (char *)xmalloc (size);
      bcopy (value, s->desired_setting, size);
    }
  else
    s->desired_setting = value;
  s->size = size;
  add_unwind_protect ((Function *)restore_variable, (char *)s);
}
