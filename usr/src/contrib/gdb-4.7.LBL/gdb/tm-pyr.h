/* Definitions to make GDB run on a Pyramid under OSx 4.0 (4.2bsd).
   Copyright (C) 1988, 1989, 1991 Free Software Foundation, Inc.

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

#define TARGET_BYTE_ORDER BIG_ENDIAN

/* Traditional Unix virtual address spaces have thre regions: text,
   data and stack.  The text, initialised data, and uninitialised data
   are represented in separate segments of the a.out file.
   When a process dumps core, the data and stack regions are written
   to a core file.  This gives a debugger enough information to
   reconstruct (and debug) the virtual address space at the time of
   the coredump.
   Pyramids have an distinct fourth region of the virtual address
   space, in which the contents of the windowed registers are stacked
   in fixed-size frames.  Pyramid refer to this region as the control
   stack.  Each call (or trap) automatically allocates a new register
   frame; each return deallocates the current frame and restores the
   windowed registers to their values before the call.

   When dumping core, the control stack is written to a core files as
   a third segment. The core-handling functions need to know to deal
   with it. */ 
/* Tell core.c there is an extra segment.  */
#define REG_STACK_SEGMENT

/* Floating point is IEEE compatible on most Pyramid hardware
   (Older processors do not have IEEE NaNs).  */
#define IEEE_FLOAT

/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */

#define NAMES_HAVE_UNDERSCORE

/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 0

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  */

/* FIXME -- do we want to skip insns to allocate the local frame?
   If so, what do they look like?
   This is becoming harder, since tege@sics.SE wants to change
   gcc to not output a prologue when no frame is needed.   */
#define SKIP_PROLOGUE(pc)  do {} while (0)


/* Immediately after a function call, return the saved pc.
   Can't always go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

#define SAVED_PC_AFTER_CALL(frame) FRAME_SAVED_PC(frame)

/* Address of end of stack space.  */
/* This seems to be right for the 90x comp.vuw.ac.nz.
   The correct value at any site may be a function of the configured
   maximum control stack depth.  If so, I don't know where the
   control-stack depth is configured, so I can't #include it here. */ 
#define STACK_END_ADDR (0xc00cc000)

/* Register window stack (Control stack) stack definitions
    - Address of beginning of control stack.
    - size of control stack frame
   (Note that since crts0 is usually the first function called,
    main()'s control stack is one frame (0x80 bytes) beyond this value.  */

#define CONTROL_STACK_ADDR (0xc00cd000)

/* Bytes in a register window -- 16 parameter regs, 16 local regs
   for each call, is 32 regs * 4 bytes */

#define CONTROL_STACK_FRAME_SIZE (32*4)

/* FIXME.  On a pyr, Data Stack grows downward; control stack goes upwards. 
   Which direction should we use for INNER_THAN, PC_INNER_THAN ?? */

#define INNER_THAN <
#define PC_INNER_THAN >

/* Stack has strict alignment.  */

#define STACK_ALIGN(ADDR) (((ADDR)+3)&-4)

/* Sequence of bytes for breakpoint instruction.  */

#define BREAKPOINT {0xf0, 00, 00, 00}

/* Amount PC must be decremented by after a breakpoint.
   This is often the number of bytes in BREAKPOINT
   but not always.  */

#define DECR_PC_AFTER_BREAK 0

/* Nonzero if instruction at PC is a return instruction. 
   On a pyr, this is either "ret" or "retd".
   It would be friendly to check that any "retd" always had an
   argument of 0, since anything else is invalid. */

#define ABOUT_TO_RETURN(pc) \
(((read_memory_integer (pc, 2) & 0x3ff0) == 0x3090) || \
 ((read_memory_integer (pc, 2) & 0x0ff0) == 0x00a0))

/* Return 1 if P points to an invalid floating point value.
   LEN is the length in bytes -- not relevant on the Vax.  */
/* FIXME -- this is ok for a vax, bad for big-endian ieee format.
   I would use the definition for a Sun; but it is no better! */

#define INVALID_FLOAT(p, len) ((*(short *) p & 0xff80) == 0x8000)

/* Say how long (ordinary) registers are.  */

#define REGISTER_TYPE long

/* Number of machine registers */
/* pyramids have 64, plus one for the PSW; plus perhaps one more for the
   kernel stack pointer (ksp) and control-stack pointer (CSP) */

#define NUM_REGS 67

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */

#define REGISTER_NAMES \
{"gr0", "gr1", "gr2", "gr3", "gr4", "gr5", "gr6", "gr7", \
 "gr8", "gr9", "gr10", "gr11", "logpsw", "cfp", "sp", "pc", \
 "pr0", "pr1", "pr2", "pr3", "pr4", "pr5", "pr6", "pr7", \
 "pr8", "pr9", "pr10", "pr11", "pr12", "pr13", "pr14", "pr15", \
 "lr0", "lr1", "lr2", "lr3", "lr4", "lr5", "lr6", "lr7", \
 "lr8", "lr9", "lr10", "lr11", "lr12", "lr13", "lr14", "lr15", \
 "tr0", "tr1", "tr2", "tr3", "tr4", "tr5", "tr6", "tr7", \
 "tr8", "tr9", "tr10", "tr11", "tr12", "tr13", "tr14", "tr15", \
  "psw", "ksp", "csp"}

/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */

/* pseudo-registers: */
#define PS_REGNUM 64		/* Contains processor status */
#define PSW_REGNUM 64		/* Contains current psw, whatever it is.*/
#define CSP_REGNUM 65		/* address of this control stack frame*/
#define KSP_REGNUM 66		/* Contains process's Kernel Stack Pointer */

#define CFP_REGNUM 13		/* Current data-stack frame ptr */
#define TR0_REGNUM 48		/* After function call, contains
				   function result */

/* Registers interesting to the machine-independent part of gdb*/

#define FP_REGNUM CSP_REGNUM	/* Contains address of executing (control)
				   stack frame */
#define SP_REGNUM 14		/* Contains address of top of stack -??*/
#define PC_REGNUM 15		/* Contains program counter */

/* Define DO_REGISTERS_INFO() to do machine-specific formatting
   of register dumps. */

#define DO_REGISTERS_INFO(_regnum, fp) pyr_do_registers_info(_regnum, fp)

/* need this so we can find the global registers: they never get saved. */
extern unsigned int global_reg_offset;
extern unsigned int last_frame_offset;

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */
#define REGISTER_BYTES (NUM_REGS*4)

/* the Pyramid has register windows.  */

#define HAVE_REGISTER_WINDOWS

/* Is this register part of the register window system?  A yes answer
   implies that 1) The name of this register will not be the same in
   other frames, and 2) This register is automatically "saved" (out
   registers shifting into ins counts) upon subroutine calls and thus
   there is no need to search more than one stack frame for it. */

#define REGISTER_IN_WINDOW_P(regnum)	\
  ((regnum) >= 16 && (regnum) < 64)

/* Index within `registers' of the first byte of the space for
   register N.  */

#define REGISTER_BYTE(N) ((N) * 4)

/* Number of bytes of storage in the actual machine representation
   for register N.  On the Pyramid, all regs are 4 bytes.  */

#define REGISTER_RAW_SIZE(N) 4

/* Number of bytes of storage in the program's representation
   for register N.  On the Pyramid, all regs are 4 bytes.  */

#define REGISTER_VIRTUAL_SIZE(N) 4

/* Largest value REGISTER_RAW_SIZE can have.  */

#define MAX_REGISTER_RAW_SIZE 4

/* Largest value REGISTER_VIRTUAL_SIZE can have.  */

#define MAX_REGISTER_VIRTUAL_SIZE 4

/* Nonzero if register N requires conversion
   from raw format to virtual format.  */

#define REGISTER_CONVERTIBLE(N) 0

/* Convert data from raw format for register REGNUM
   to virtual format for register REGNUM.  */

#define REGISTER_CONVERT_TO_VIRTUAL(REGNUM,FROM,TO)	\
  bcopy ((FROM), (TO), 4);

/* Convert data from virtual format for register REGNUM
   to raw format for register REGNUM.  */

#define REGISTER_CONVERT_TO_RAW(REGNUM,FROM,TO)	\
  bcopy ((FROM), (TO), 4);

/* Return the GDB type object for the "standard" data type
   of data in register N.  */

#define REGISTER_VIRTUAL_TYPE(N) builtin_type_int

/* FIXME: It seems impossible for both EXTRACT_RETURN_VALUE and
   STORE_RETURN_VALUE to be correct. */

/* Store the address of the place in which to copy the structure the
   subroutine will return.  This is called from call_function. */

/****FIXME****/
#define STORE_STRUCT_RETURN(ADDR, SP) \
  { write_register (TR0_REGNUM, (ADDR)); }

/* Extract from an array REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF.  */

/* Note that on a register-windowing machine (eg, Pyr, SPARC), this is
   where the value is found after the function call -- ie, it should
   correspond to GNU CC's FUNCTION_VALUE rather than FUNCTION_OUTGOING_VALUE.*/

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
  bcopy (((int *)(REGBUF))+TR0_REGNUM, VALBUF, TYPE_LENGTH (TYPE))

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format.  */
/* on pyrs, values are returned in */

#define STORE_RETURN_VALUE(TYPE,VALBUF) \
  write_register_bytes (REGISTER_BYTE(TR0_REGNUM), VALBUF, TYPE_LENGTH (TYPE))

/* Extract from an array REGBUF containing the (raw) register state
   the address in which a function should return its structure value,
   as a CORE_ADDR (or an expression that can be used as one).  */
/* FIXME */
#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) \
  ( ((int *)(REGBUF)) [TR0_REGNUM])


/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

#define EXTRA_FRAME_INFO \
	FRAME_ADDR bottom;	\
	CORE_ADDR frame_cfp;	\
	CORE_ADDR frame_window_addr;

#define INIT_EXTRA_FRAME_INFO(fromleaf, fci)  \
do {								\
  (fci)->frame_window_addr = (fci)->frame;			\
  (fci)->bottom =						\
	  ((fci)->next ?					\
	   ((fci)->frame == (fci)->next_frame ?			\
	    (fci)->next->bottom : (fci)->next->frame) :		\
	   read_register (SP_REGNUM));				\
  (fci)->frame_cfp =						\
	  read_register (CFP_REGNUM);				\
  /***fprintf (stderr,						\
	   "[[creating new frame for %0x,pc=%0x,csp=%0x]]\n",	\
	   (fci)->frame, (fci)->pc,(fci)->frame_cfp);*/		\
} while (0);

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer. */

/* In the case of the pyr, the frame's nominal address is the address
   of parameter register 0.  The previous frame is found 32 words up.   */

#define FRAME_CHAIN(thisframe)	\
  ( (thisframe) -> frame - CONTROL_STACK_FRAME_SIZE)

 /*((thisframe) >= CONTROL_STACK_ADDR))*/

/* Define other aspects of the stack frame.  */

/* A macro that tells us whether the function invocation represented
   by FI does not have a frame on the stack associated with it.  If it
   does not, FRAMELESS is set to 1, else 0.

   I do not understand what this means on a Pyramid, where functions
   *always* have a control-stack frame, but may or may not have a
   frame on the data stack.  Since GBD uses the value of the
   control stack pointer as its "address" of a frame, FRAMELESS
   is always 1, so does not need to be defined.  */


/* Where is the PC for a specific frame */

#define FRAME_SAVED_PC(fi) \
  ((CORE_ADDR) (read_memory_integer ( (fi) -> frame + 60, 4)))

/* There may be bugs in FRAME_ARGS_ADDRESS and FRAME_LOCALS_ADDRESS;
   or there may be bugs in accessing the registers that break
   their definitions.
   Having the macros expand into functions makes them easier to debug.
   When the bug is finally located, the inline macro defintions can
   be un-#if 0ed, and frame_args_addr and frame_locals_address can
   be deleted from pyr-dep.c */ 

/* If the argument is on the stack, it will be here.  */
#define FRAME_ARGS_ADDRESS(fi) \
  frame_args_addr(fi)

#define FRAME_LOCALS_ADDRESS(fi) \
  frame_locals_address(fi)

/* The following definitions doesn't seem to work.
   I don't understand why. */
#if 0
#define FRAME_ARGS_ADDRESS(fi) \
   /*(FRAME_FP(fi) + (13*4))*/ (read_register (CFP_REGNUM))

#define FRAME_LOCALS_ADDRESS(fi) \
  ((fi)->frame +(16*4))

#endif /* 0 */

/* Return number of args passed to a frame.
   Can return -1, meaning no way to tell.  */

#define FRAME_NUM_ARGS(val, fi)  (val = -1)

/* Return number of bytes at start of arglist that are not really args.  */

#define FRAME_ARGS_SKIP 0

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.

   Note that on register window machines, we are currently making the
   assumption that window registers are being saved somewhere in the
   frame in which they are being used.  If they are stored in an
   inferior frame, find_saved_register will break.

   On pyrs, frames of window registers are stored contiguously on a
   separate stack.  All window registers are always stored.
   The pc and psw (gr15 and gr14)  are also always saved: the call
   insn saves them in pr15 and pr14 of the new frame (tr15,tr14 of the
   old frame).  
   The data-stack frame pointer (CFP) is only saved in functions which
   allocate a (data)stack frame (with "adsf").  We detect them by
   looking at the first insn of the procedure. 

   Other non-window registers (gr0-gr11) are never saved.  Pyramid's C
   compiler and gcc currently ignore them, so it's not an issue.   */ 

#define FRAME_FIND_SAVED_REGS(fi_p, frame_saved_regs) \
{  register int regnum;							\
  register CORE_ADDR pc;						\
  register CORE_ADDR fn_start_pc;					\
  register int first_insn;						\
  register CORE_ADDR prev_cf_addr;					\
  register int window_ptr;						\
  FRAME fid = FRAME_INFO_ID (fi_p);					\
  if (!fid) fatal ("Bad frame info struct in FRAME_FIND_SAVED_REGS");	\
  bzero (&(frame_saved_regs), sizeof (frame_saved_regs));		\
									\
  window_ptr = prev_cf_addr = FRAME_FP(fi_p);				\
									\
  for (regnum = 16 ; regnum < 64; regnum++,window_ptr+=4)		\
  {									\
    (frame_saved_regs).regs[regnum] = window_ptr;			\
  }									\
									\
  /* In each window, psw, and pc are "saved" in tr14,tr15. */		\
  /*** psw is sometimes saved in gr12 (so sez <sys/pcb.h>) */		\
  (frame_saved_regs).regs[PS_REGNUM] = FRAME_FP(fi_p) + (14*4);  	\
									\
/*(frame_saved_regs).regs[PC_REGNUM] = (frame_saved_regs).regs[31];*/	\
  (frame_saved_regs).regs[PC_REGNUM] = FRAME_FP(fi_p) + ((15+32)*4);	\
									\
  /* Functions that allocate a frame save sp *where*? */		\
/*first_insn = read_memory_integer (get_pc_function_start ((fi_p)->pc),4); */ \
									\
  fn_start_pc = (get_pc_function_start ((fi_p)->pc));			\
  first_insn = read_memory_integer(fn_start_pc, 4);			\
									\
  if (0x08 == ((first_insn >> 20) &0x0ff)) {				\
    /* NB: because WINDOW_REGISTER_P(cfp) is false, a saved cfp		\
       in this frame is only visible in this frame's callers.		\
       That means the cfp we mark saved is my caller's cfp, ie pr13.	\
       I don't understand why we don't have to do that for pc, too.  */	\
									\
    (frame_saved_regs).regs[CFP_REGNUM] = FRAME_FP(fi_p)+(13*4);	\
									\
    (frame_saved_regs).regs[SP_REGNUM] =				\
	  read_memory_integer (FRAME_FP(fi_p)+((13+32)*4),4);		\
  }									\
									\
/*									\
 *(frame_saved_regs).regs[CFP_REGNUM] = (frame_saved_regs).regs[61];	\
 * (frame_saved_regs).regs[SP_REGNUM] =					\
 *	  read_memory_integer (FRAME_FP(fi_p)+((13+32)*4),4);		\
 */									\
									\
  (frame_saved_regs).regs[CSP_REGNUM] = prev_cf_addr;			\
}

/* Things needed for making the inferior call functions.  */
#if 0
/* These are all lies.  These macro definitions are appropriate for a
    SPARC. On a pyramid, pushing a dummy frame will
   surely involve writing the control stack pointer,
   then saving the pc.  This requires a privileged instruction.
   Maybe one day Pyramid can be persuaded to add a syscall to do this.
   Until then, we are out of luck. */

/* Push an empty stack frame, to record the current PC, etc.  */

#define PUSH_DUMMY_FRAME \
{ register CORE_ADDR sp = read_register (SP_REGNUM);\
  register int regnum;				    \
  sp = push_word (sp, 0); /* arglist */		    \
  for (regnum = 11; regnum >= 0; regnum--)	    \
    sp = push_word (sp, read_register (regnum));    \
  sp = push_word (sp, read_register (PC_REGNUM));   \
  sp = push_word (sp, read_register (FP_REGNUM));   \
/*  sp = push_word (sp, read_register (AP_REGNUM));*/   \
  sp = push_word (sp, (read_register (PS_REGNUM) & 0xffef)   \
		      + 0x2fff0000);		    \
  sp = push_word (sp, 0); 			    \
  write_register (SP_REGNUM, sp);		    \
  write_register (FP_REGNUM, sp);		    \
/*  write_register (AP_REGNUM, sp + 17 * sizeof (int));*/ }

/* Discard from the stack the innermost frame, restoring all registers.  */

#define POP_FRAME  \
{ register CORE_ADDR fp = read_register (FP_REGNUM);		 \
  register int regnum;						 \
  register int regmask = read_memory_integer (fp + 4, 4);	 \
  write_register (PS_REGNUM, 					 \
		  (regmask & 0xffff)				 \
		  | (read_register (PS_REGNUM) & 0xffff0000));	 \
  write_register (PC_REGNUM, read_memory_integer (fp + 16, 4));  \
  write_register (FP_REGNUM, read_memory_integer (fp + 12, 4));  \
/*  write_register (AP_REGNUM, read_memory_integer (fp + 8, 4));*/   \
  fp += 16;							 \
  for (regnum = 0; regnum < 12; regnum++)			 \
    if (regmask & (0x10000 << regnum))				 \
      write_register (regnum, read_memory_integer (fp += 4, 4)); \
  fp = fp + 4 + ((regmask >> 30) & 3);				 \
  if (regmask & 0x20000000)					 \
    { regnum = read_memory_integer (fp, 4);			 \
      fp += (regnum + 1) * 4; }					 \
  write_register (SP_REGNUM, fp);				 \
  set_current_frame (read_register (FP_REGNUM)); }

/* This sequence of words is the instructions
     calls #69, @#32323232
     bpt
   Note this is 8 bytes.  */

#define CALL_DUMMY {0x329f69fb, 0x03323232}

#define CALL_DUMMY_START_OFFSET 0  /* Start execution at beginning of dummy */

/* Insert the specified number of args and function address
   into a call sequence of the above form stored at DUMMYNAME.  */

#define FIX_CALL_DUMMY(dummyname, pc, fun, nargs, args, type, gcc_p)   \
{ *((char *) dummyname + 1) = nargs;		\
  *(int *)((char *) dummyname + 3) = fun; }
#endif /* 0 */

#define POP_FRAME \
  { error ("The return command is not supported on this machine."); }
