/* Data structures associated with breakpoints in GDB.
   Copyright (C) 1992 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#if !defined (BREAKPOINT_H)
#define BREAKPOINT_H 1

#include "frame.h"
#include "value.h"

/* This is the maximum number of bytes a breakpoint instruction can take.
   Feel free to increase it.  It's just used in a few places to size
   arrays that should be independent of the target architecture.  */

#define	BREAKPOINT_MAX	16

/* The follow stuff is an abstract data type "bpstat" ("breakpoint status").
   This provides the ability to determine whether we have stopped at a
   breakpoint, and what we should do about it.  */

typedef struct bpstat *bpstat;

/* Interface:  */
/* Clear a bpstat so that it says we are not at any breakpoint.
   Also free any storage that is part of a bpstat.  */
extern void bpstat_clear PARAMS ((bpstat *));

/* Return a copy of a bpstat.  Like "bs1 = bs2" but all storage that
   is part of the bpstat is copied as well.  */
extern bpstat bpstat_copy PARAMS ((bpstat));

/* Get a bpstat associated with having just stopped at address *PC
   and frame address FRAME_ADDRESS.  Update *PC to point at the
   breakpoint (if we hit a breakpoint).  */
/* FIXME:  prototypes uses equivalence between FRAME_ADDR and CORE_ADDR */
extern bpstat bpstat_stop_status PARAMS ((CORE_ADDR *, CORE_ADDR));

/* Nonzero if we should print the frame.  */
#define bpstat_should_print(bs) ((bs) != NULL && (bs)->print)

/* Nonzero if we should stop.  */
#define bpstat_stop(bs) ((bs) != NULL && (bs)->stop)

/* Find the bpstat associated with a breakpoint.  NULL otherwise. */
bpstat bpstat_find_breakpoint(/* bpstat, breakpoint */);

/* Nonzero if we hit a momentary breakpoint.  */
#define bpstat_momentary_breakpoint(bs) ((bs) != NULL && (bs)->momentary)

/* Nonzero if a signal that we got in wait() was due to circumstances
   explained by the BS.  */
/* Currently that is true iff we have hit a breakpoint.  */
#define bpstat_explains_signal(bs) ((bs) != NULL)

/* Nonzero if we should step constantly (e.g. watchpoints on machines
   without hardware support).  This isn't related to a specific bpstat,
   just to things like whether watchpoints are set.  */
extern int bpstat_should_step PARAMS ((void));

/* Print a message indicating what happened.  Returns nonzero to
   say that only the source line should be printed after this (zero
   return means print the frame as well as the source line).  */
extern int bpstat_print PARAMS ((bpstat));

/* Return the breakpoint number of the first breakpoint we are stopped
   at.  *BSP upon return is a bpstat which points to the remaining
   breakpoints stopped at (but which is not guaranteed to be good for
   anything but further calls to bpstat_num).
   Return 0 if passed a bpstat which does not indicate any breakpoints.  */
extern int bpstat_num PARAMS ((bpstat *));

/* Perform actions associated with having stopped at *BSP.  */
extern void bpstat_do_actions PARAMS ((bpstat *));

/* Modify BS so that the actions will not be performed.  */
extern void bpstat_clear_actions PARAMS ((bpstat));

/* Implementation:  */
struct bpstat
{
  /* Linked list because there can be two breakpoints at the
     same place, and a bpstat reflects the fact that both have been hit.  */
  bpstat next;
  /* Breakpoint that we are at.  */
  struct breakpoint *breakpoint_at;
  /* Commands left to be done.  */
  struct command_line *commands;
  /* Old value associated with a watchpoint.  */
  value old_val;
  /* Nonzero if we should print the frame.  Only significant for the first
     bpstat in the chain.  */
  char print;
  /* Nonzero if we should stop.  Only significant for the first bpstat in
     the chain.  */
  char stop;
  /* Nonzero if we hit a momentary breakpoint.  Only significant for the
     first bpstat in the chain.  */
  char momentary;
};

/* Type of breakpoint. */
/* FIXME In the future, we should fold all other breakpoint-like things into
   here.  This includes:

   1) single-step (for machines where we have to simulate single stepping),
   2) step-resume (for 'next'ing over subroutine calls),
   3) call-dummy (the breakpoint at the end of a subroutine stub that gdb
      uses to call functions in the target).
*/

enum bptype {
  bp_breakpoint,		/* Normal breakpoint */
  bp_until,			/* used by until command */
  bp_finish,			/* used by finish command */
  bp_watchpoint,		/* Watchpoint */
  bp_longjmp,			/* secret breakpoint to find longjmp() */
  bp_longjmp_resume		/* secret breakpoint to escape longjmp() */
};

/* States of enablement of breakpoint. */

enum enable { disabled, enabled};

/* Disposition of breakpoint.  Ie: what to do after hitting it. */

enum bpdisp {
  delete,			/* Delete it */
  disable,			/* Disable it */
  donttouch			/* Leave it alone */
};

/* Note that the ->silent field is not currently used by any commands
   (though the code is in there if it was to be, and set_raw_breakpoint
   does set it to 0).  I implemented it because I thought it would be
   useful for a hack I had to put in; I'm going to leave it in because
   I can see how there might be times when it would indeed be useful */

/* This is for a breakpoint or a watchpoint.  */

struct breakpoint
{
  struct breakpoint *next;
  /* Type of breakpoint. */
  enum bptype type;
  /* Zero means disabled; remember the info but don't break here.  */
  enum enable enable;
  /* What to do with this breakpoint after we hit it. */
  enum bpdisp disposition;
  /* Number assigned to distinguish breakpoints.  */
  int number;
  /* Address to break at, or NULL if not a breakpoint.  */
  CORE_ADDR address;
  /* Line number of this address.  Redundant.  Only matters if address
     is non-NULL.  */
  int line_number;
  /* Symtab of file of this address.  Redundant.  Only matters if address
     is non-NULL.  */
  struct symtab *symtab;
  /* Non-zero means a silent breakpoint (don't print frame info
     if we stop here). */
  unsigned char silent;
  /* Number of stops at this breakpoint that should
     be continued automatically before really stopping.  */
  int ignore_count;
  /* "Real" contents of byte where breakpoint has been inserted.
     Valid only when breakpoints are in the program.  Under the complete
     control of the target insert_breakpoint and remove_breakpoint routines.
     No other code should assume anything about the value(s) here.  */
  char shadow_contents[BREAKPOINT_MAX];
  /* Nonzero if this breakpoint is now inserted.  Only matters if address
     is non-NULL.  */
  char inserted;
  /* Nonzero if this is not the first breakpoint in the list
     for the given address.  Only matters if address is non-NULL.  */
  char duplicate;
  /* Chain of command lines to execute when this breakpoint is hit.  */
  struct command_line *commands;
  /* Stack depth (address of frame).  If nonzero, break only if fp
     equals this.  */
  FRAME_ADDR frame;
  /* Conditional.  Break only if this expression's value is nonzero.  */
  struct expression *cond;

  /* String we used to set the breakpoint (malloc'd).  Only matters if
     address is non-NULL.  */
  char *addr_string;
  /* String form of the breakpoint condition (malloc'd), or NULL if there
     is no condition.  */
  char *cond_string;

  /* The expression we are watching, or NULL if not a watchpoint.  */
  struct expression *exp;
  /* The largest block within which it is valid, or NULL if it is
     valid anywhere (e.g. consists just of global symbols).  */
  struct block *exp_valid_block;
  /* Value of the watchpoint the last time we checked it.  */
  value val;
};

/* Prototypes for breakpoint-related functions.  */

#ifdef __STDC__		/* Forward declarations for prototypes */
struct frame_info;
#endif

extern int
breakpoint_here_p PARAMS ((CORE_ADDR));

extern void
until_break_command PARAMS ((char *, int));

extern void
breakpoint_re_set PARAMS ((void));

extern void
clear_momentary_breakpoints PARAMS ((void));

/* FIXME:  Prototype uses equivalence of "struct frame_info *" and FRAME */
extern struct breakpoint *
set_momentary_breakpoint PARAMS ((struct symtab_and_line,
				  struct frame_info *,
				  enum bptype));

extern void
set_ignore_count PARAMS ((int, int, int));

extern void
set_default_breakpoint PARAMS ((int, CORE_ADDR, struct symtab *, int));

extern void
mark_breakpoints_out PARAMS ((void));

extern void
delete_breakpoint PARAMS ((struct breakpoint *));

extern void
breakpoint_auto_delete PARAMS ((bpstat));

extern void
breakpoint_clear_ignore_counts PARAMS ((void));

extern void
break_command PARAMS ((char *, int));

extern int
insert_breakpoints PARAMS ((void));

extern int
remove_breakpoints PARAMS ((void));

extern void
enable_longjmp_breakpoint PARAMS ((void));

extern void
disable_longjmp_breakpoint PARAMS ((void));

extern void
set_longjmp_resume_breakpoint PARAMS ((CORE_ADDR, FRAME));
 
/* The following are for displays, which aren't really breakpoints, but
   here is as good a place as any for them.  */

extern void
disable_current_display PARAMS ((void));

extern void
do_displays PARAMS ((void));

extern void
disable_display PARAMS ((int));

extern void
clear_displays PARAMS ((void));

#endif /* !defined (BREAKPOINT_H) */
