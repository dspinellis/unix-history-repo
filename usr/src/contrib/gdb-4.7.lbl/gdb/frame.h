/* Definitions for dealing with stack frames, for GDB, the GNU debugger.
   Copyright 1986, 1989, 1991, 1992 Free Software Foundation, Inc.

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

#if !defined (FRAME_H)
#define FRAME_H 1

/* FRAME is the type of the identifier of a specific stack frame.  It
   is a pointer to the frame cache item corresponding to this frame.
   Please note that frame id's are *not* constant over calls to the
   inferior.  Use frame addresses, which are.
  
   FRAME_ADDR is the type of the address of a specific frame.  I
   cannot imagine a case in which this would not be CORE_ADDR, so
   maybe it's silly to give it it's own type.  Life's rough.
  
   FRAME_FP is a macro which converts from a frame identifier into a
   frame_address.
  
   FRAME_INFO_ID is a macro which "converts" from a frame info pointer
   to a frame id.  This is here in case I or someone else decides to
   change the FRAME type again.
  
   This file and blockframe.c are the only places which are allowed to
   use the equivalence between FRAME and struct frame_info *.  EXCEPTION:
   value.h uses CORE_ADDR instead of FRAME_ADDR because the compiler
   will accept that in the absence of this file.
   FIXME:  Prototypes in other files make use of the equivalence between
           "FRAME" and "struct frame_info *" and the equivalence between
	   CORE_ADDR and FRAME_ADDR.  */

typedef struct frame_info *FRAME;
typedef CORE_ADDR	FRAME_ADDR;
#define FRAME_FP(fr)	((fr)->frame)
#define FRAME_INFO_ID(f)	(f)

/* Caching structure for stack frames.  This is also the structure
   used for extended info about stack frames.  May add more to this
   structure as it becomes necessary.
  
   Note that the first entry in the cache will always refer to the
   innermost executing frame.  This value is set in wait_for_inferior.  */

struct frame_info
  {
    /* Nominal address of the frame described.  */
    FRAME_ADDR frame;
    /* Address at which execution is occurring in this frame.
       For the innermost frame, it's the current pc.
       For other frames, it is a pc saved in the next frame.  */
    CORE_ADDR pc;
    /* The frame called by the frame we are describing, or 0.
       This may be set even if there isn't a frame called by the one
       we are describing (.->next == 0); in that case it is simply the
       bottom of this frame */
    FRAME_ADDR next_frame;
    /* Anything extra for this structure that may have been defined
       in the machine depedent files. */
#ifdef EXTRA_FRAME_INFO
    EXTRA_FRAME_INFO
#endif
    /* Pointers to the next and previous frame_info's in this stack.  */
    FRAME next, prev;
  };

/* Describe the saved registers of a frame.  */

struct frame_saved_regs
  {
    /* For each register, address of where it was saved on entry to the frame,
       or zero if it was not saved on entry to this frame.  */
    CORE_ADDR regs[NUM_REGS];
  };

/* Define a default FRAME_CHAIN_VALID, in the form that is suitable for most
   targets.  If FRAME_CHAIN_VALID returns zero it means that the given frame
   is the outermost one and has no caller.

   If a particular target needs a different definition, then it can override
   the definition here by providing one in the tm file. */

#if !defined (FRAME_CHAIN_VALID)

#if defined (FRAME_CHAIN_VALID_ALTERNATE)

/* Use the alternate method of avoiding running up off the end of the frame
   chain or following frames back into the startup code.  See the comments
   in objfiles.h. */
   
#define FRAME_CHAIN_VALID(chain, thisframe)	\
  ((chain) != 0					\
   && !inside_main_func ((thisframe) -> pc)	\
   && !inside_entry_func ((thisframe) -> pc))

#else

#define FRAME_CHAIN_VALID(chain, thisframe)	\
  ((chain) != 0					\
   && !inside_entry_file (FRAME_SAVED_PC (thisframe)))

#endif	/* FRAME_CHAIN_VALID_ALTERNATE */

#endif	/* FRAME_CHAIN_VALID */

/* If we encounter a request to use base register addressing of variables
   on a machine for which gdb has not been configured to support such
   access, report the failure to support this access mode. */

#if !defined (FRAME_GET_BASEREG_VALUE)

#define FRAME_GET_BASEREG_VALUE(frame, regno) \
  (error ("Missing valid method for finding contents of base register."),0)

#endif

/* The stack frame that the user has specified for commands to act on.
   Note that one cannot assume this is the address of valid data.  */

extern FRAME selected_frame;

/* Level of the selected frame:
   0 for innermost, 1 for its caller, ...
   or -1 for frame specified by address with no defined level.  */

extern int selected_frame_level;

extern struct frame_info *
get_frame_info PARAMS ((FRAME));

extern struct frame_info *
get_prev_frame_info PARAMS ((FRAME));

extern FRAME
create_new_frame PARAMS ((FRAME_ADDR, CORE_ADDR));

extern void
flush_cached_frames PARAMS ((void));

extern void
reinit_frame_cache PARAMS ((void));

extern void
get_frame_saved_regs PARAMS ((struct frame_info *, struct frame_saved_regs *));

extern void
set_current_frame PARAMS ((FRAME));

extern FRAME
get_prev_frame PARAMS ((FRAME));

extern FRAME
get_current_frame PARAMS ((void));

extern FRAME
get_next_frame PARAMS ((FRAME));

extern struct block *
get_frame_block PARAMS ((FRAME));

extern struct block *
get_current_block PARAMS ((void));

extern struct block *
get_selected_block PARAMS ((void));

extern struct symbol *
get_frame_function PARAMS ((FRAME));

extern CORE_ADDR
get_frame_pc PARAMS ((FRAME));

extern CORE_ADDR
get_pc_function_start PARAMS ((CORE_ADDR));

extern struct block *
block_for_pc PARAMS ((CORE_ADDR));

extern int
frameless_look_for_prologue PARAMS ((FRAME));

extern void
print_frame_args PARAMS ((struct symbol *, struct frame_info *, int, FILE *));

extern FRAME
find_relative_frame PARAMS ((FRAME, int*));

extern void
print_stack_frame PARAMS ((FRAME, int, int));

extern void
select_frame PARAMS ((FRAME, int));

extern void
record_selected_frame PARAMS ((FRAME_ADDR *, int *));

extern void
print_frame_info PARAMS ((struct frame_info *, int, int, int));

extern CORE_ADDR
find_saved_register PARAMS ((FRAME, int));

#endif /* !defined (FRAME_H)  */
