/* Definitions for asynchronous process control in GNU Emacs.
   Copyright (C) 1985, 1990 Free Software Foundation, Inc.

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


/*
 * Structure records pertinent information about open channels.
 * There is one channel associated with each process.
 */

struct Lisp_Process
  {
    int size;
    struct Lisp_Vector *v_next;
    /* Descriptor by which we read from this process */
    Lisp_Object infd;
    /* Descriptor by which we write to this process */
    Lisp_Object outfd;
    /* Name of this process */
    Lisp_Object name;
    /* List of command arguments that this process was run with */
    Lisp_Object command;
    /* (funcall FILTER PROC STRING)  (if FILTER is non-nil)
       to dispose of a bunch of chars from the process all at once */
    Lisp_Object filter;
    /* (funcall SENTINEL PROCESS) when process state changes */
    Lisp_Object sentinel;
    /* Buffer that output is going to */
    Lisp_Object buffer;
    /* Number of this process */
    Lisp_Object pid;
    /* Non-nil if this is really a command channel */
    Lisp_Object command_channel_p;
    /* Non-nil if this is really a child process */
    Lisp_Object childp;
    /* Marker set to end of last buffer-inserted output from this process */
    Lisp_Object mark;
    /* Non-nil means kill silently if Emacs is exited.  */
    Lisp_Object kill_without_query;
    /* Record the process status in the raw form in which it comes from `wait'.
       This is to avoid consing in a signal handler.  */
    Lisp_Object raw_status_low;
    Lisp_Object raw_status_high;
    /* Symbol indicating status of process.
       This may be a symbol: run, open, or closed.
       Or it may be a list, whose car is stop, exit or signal
       and whose cdr is a pair (EXIT_CODE . COREDUMP_FLAG)
       or (SIGNAL_NUMBER . COREDUMP_FLAG).  */
    Lisp_Object status;
    /* Non-nil if communicating through a pty.  */
    Lisp_Object pty_flag;
    /* Event-count of last event in which this process changed status.  */
    Lisp_Object tick;
    /* Event-count of last such event reported.  */
    Lisp_Object update_tick;
};

#define ChannelMask(n) (1<<(n))
