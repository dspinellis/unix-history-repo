/* Definitions for asynchronous process control in GNU Emacs.
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
    /* The state of the process, expressed by flags
	(RUNNING, STOPPED, etc.)  */
    Lisp_Object flags;
    /* # of signal that the process got, that stopped it,
       or code it exited with.  */
    Lisp_Object reason;
    /* Marker set to end of last buffer-inserted output from this process */
    Lisp_Object mark;
    /* Non-nil means kill silently if Emacs is exited.  */
    Lisp_Object kill_without_query;
};

/* Process status, found in the flags component */
#define PROC_STATUS 3

/* Values of process status */
#define RUNNING	0	/* process is running */
#define STOPPED	1	/* process has stopped */
#define EXITED	2	/* process has exited */
#define SIGNALED 3	/* process was signalled */

/* Additional bits in the flags component */
#define CHANGED 4	/* This bit is set when state changes;
			   cleared when a change message has been given */
#define COREDUMPED 8	/* This bit is set if core was dumped */


#define ChannelMask(n) (1<<(n))
