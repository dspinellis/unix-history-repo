/* Target machine definitions for GDB on a Sequent Symmetry under dynix 3.0,
   with Weitek 1167 and i387 support.
   Copyright 1986, 1987, 1989, 1991, 1992 Free Software Foundation, Inc.
   Symmetry version by Jay Vosburgh (uunet!sequent!fubar).

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

/* I don't know if this will work for cross-debugging, even if you do get
   a copy of the right include file.  */
#include <machine/reg.h>

#define TARGET_BYTE_ORDER LITTLE_ENDIAN

/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */

#define NAMES_HAVE_UNDERSCORE

/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 0

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  From m-i386.h */

#define SKIP_PROLOGUE(frompc)   {(frompc) = i386_skip_prologue((frompc));}

extern int
i386_skip_prologue PARAMS ((int));

/* Immediately after a function call, return the saved pc.
   Can't always go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

#define SAVED_PC_AFTER_CALL(frame) \
  read_memory_integer(read_register(SP_REGNUM), 4)

/* I don't know the real values for these.  */
#define TARGET_UPAGES UPAGES
#define TARGET_NBPG NBPG

/* Address of end of stack space.  */

#define STACK_END_ADDR (0x40000000 - (TARGET_UPAGES * TARGET_NBPG))

/* Stack grows downward.  */

#define INNER_THAN <

/* Sequence of bytes for breakpoint instruction.  */

#define BREAKPOINT {0xcc}

/* Amount PC must be decremented by after a breakpoint.
   This is often the number of bytes in BREAKPOINT
   but not always.  */

#define DECR_PC_AFTER_BREAK 0

/* Nonzero if instruction at PC is a return instruction.  */
/* For Symmetry, this is really the 'leave' instruction, which */
/* is right before the ret */

#define ABOUT_TO_RETURN(pc) (read_memory_integer (pc, 1) == 0xc9)

/* Return 1 if P points to an invalid floating point value.
*/

#define INVALID_FLOAT(p, len) (0)

#if 0
 --- this code can't be used unless we know we are running native,
     since it uses host specific ptrace calls.
/* code for 80387 fpu.  Functions are from i386-dep.c, copied into
 * symm-dep.c.
 */
#define FLOAT_INFO { i386_float_info(); }
#endif

/* Say how long (ordinary) registers are.  */

#define REGISTER_TYPE long

/* Number of machine registers */
#define NUM_REGS 49

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */

/* Symmetry registers are in this weird order to match the register
   numbers in the symbol table entries.  If you change the order,
   things will probably break mysteriously for no apparent reason.
   Also note that the st(0)...st(7) 387 registers are represented as
   st0...st7.  */

#define REGISTER_NAMES { "eax", "edx", "ecx", "st0", "st1", \
			     "ebx", "esi", "edi", "st2", "st3", \
			     "st4", "st5", "st6", "st7", "esp", \
			     "ebp", "eip", "eflags", "fp1", "fp2", \
			     "fp3", "fp4", "fp5", "fp6", "fp7", \
			     "fp8", "fp9", "fp10", "fp11", "fp12", \
			     "fp13", "fp14", "fp15", "fp16", "fp17", \
			     "fp18", "fp19", "fp20", "fp21", "fp22", \
			     "fp23", "fp24", "fp25", "fp26", "fp27", \
			     "fp28", "fp29", "fp30", "fp31" }

/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */

#define FP1_REGNUM 18		/* first 1167 register */
#define SP_REGNUM 14		/* Contains address of top of stack */
#define FP_REGNUM 15		/* Contains address of executing stack frame */
#define PC_REGNUM 16		/* Contains program counter */
#define PS_REGNUM 17		/* Contains processor status */

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */
/* 10 i386 registers, 8 i387 registers, and 31 Weitek 1167 registers */
#define REGISTER_BYTES ((10 * 4) + (8 * 10) + (31 * 4))

/* Index within `registers' of the first byte of the space for
   register N.  */

#define REGISTER_BYTE(N) 		\
((N < 3) ? (N * 4) :			\
(N < 5) ? (((N - 2) * 10) + 2) :	\
(N < 8) ? (((N - 5) * 4) + 32) :	\
(N < 14) ? (((N - 8) * 10) + 44) :	\
    (((N - 14) * 4) + 104))

/* Number of bytes of storage in the actual machine representation
 * for register N.  All registers are 4 bytes, except 387 st(0) - st(7),
 * which are 80 bits each. 
 */

#define REGISTER_RAW_SIZE(N) \
((N < 3) ? 4 :	\
(N < 5) ? 10 :	\
(N < 8) ? 4 :	\
(N < 14) ? 10 :	\
    4)

/* Number of bytes of storage in the program's representation
   for register N.  On the vax, all regs are 4 bytes.  */

#define REGISTER_VIRTUAL_SIZE(N) 4

/* Largest value REGISTER_RAW_SIZE can have.  */

#define MAX_REGISTER_RAW_SIZE 10

/* Largest value REGISTER_VIRTUAL_SIZE can have.  */

#define MAX_REGISTER_VIRTUAL_SIZE 4

/* Nonzero if register N requires conversion
   from raw format to virtual format.  */

#define REGISTER_CONVERTIBLE(N) \
((N < 3) ? 0 : \
(N < 5) ? 1  : \
(N < 8) ? 0  : \
(N < 14) ? 1 : \
    0)

/* Convert data from raw format for register REGNUM
   to virtual format for register REGNUM.  */

#define REGISTER_CONVERT_TO_VIRTUAL(REGNUM,FROM,TO)	\
((REGNUM < 3) ? bcopy ((FROM), (TO), 4) : \
(REGNUM < 5) ? i387_to_double((FROM), (TO)) : \
(REGNUM < 8) ? bcopy ((FROM), (TO), 4) : \
(REGNUM < 14) ? i387_to_double((FROM), (TO)) : \
    bcopy ((FROM), (TO), 4))

extern void
i387_to_double PARAMS ((char *, char *));

/* Convert data from virtual format for register REGNUM
   to raw format for register REGNUM.  */

#define REGISTER_CONVERT_TO_RAW(REGNUM,FROM,TO)	\
((REGNUM < 3) ? bcopy ((FROM), (TO), 4) : \
(REGNUM < 5) ? double_to_i387((FROM), (TO)) : \
(REGNUM < 8) ? bcopy ((FROM), (TO), 4) : \
(REGNUM < 14) ? double_to_i387((FROM), (TO)) : \
    bcopy ((FROM), (TO), 4))

extern void
double_to_i387 PARAMS ((char *, char *));

/* Return the GDB type object for the "standard" data type
   of data in register N.  */

#define REGISTER_VIRTUAL_TYPE(N) \
((N < 3) ? builtin_type_int : \
(N < 5) ? builtin_type_double : \
(N < 8) ? builtin_type_int : \
(N < 14) ? builtin_type_double : \
    builtin_type_int)

/* from m-i386.h */
/* Store the address of the place in which to copy the structure the
   subroutine will return.  This is called from call_function. */

#define STORE_STRUCT_RETURN(ADDR, SP) \
  { (SP) -= sizeof (ADDR);		\
    write_memory ((SP), &(ADDR), sizeof (ADDR)); \
    write_register(0, (ADDR)); }

/* Extract from an array REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF.  */

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
  symmetry_extract_return_value(TYPE, REGBUF, VALBUF)

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format.  */

#define STORE_RETURN_VALUE(TYPE,VALBUF) \
  write_register_bytes (0, VALBUF, TYPE_LENGTH (TYPE))

/* Extract from an array REGBUF containing the (raw) register state
   the address in which a function should return its structure value,
   as a CORE_ADDR (or an expression that can be used as one).  */

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) (*(int *)(REGBUF))


/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer.

   However, if FRAME_CHAIN_VALID returns zero,
   it means the given frame is the outermost one and has no caller.  */

/* On Symmetry, %ebp points to caller's %ebp, and the return address
   is right on top of that.  */

#define FRAME_CHAIN(thisframe)  \
  (!inside_entry_file ((thisframe)->pc) ? \
   read_memory_integer((thisframe)->frame, 4) :\
   0)

#define FRAME_CHAIN_VALID(chain, thisframe) \
  (chain != 0)

/* Define other aspects of the stack frame.  */

/* A macro that tells us whether the function invocation represented
   by FI does not have a frame on the stack associated with it.  If it
   does not, FRAMELESS is set to 1, else 0.  */
#define FRAMELESS_FUNCTION_INVOCATION(FI, FRAMELESS) \
  (FRAMELESS) = frameless_look_for_prologue(FI)

#define FRAME_SAVED_PC(fi) (read_memory_integer((fi)->frame + 4, 4))

#define FRAME_ARGS_ADDRESS(fi) ((fi)->frame)

#define FRAME_LOCALS_ADDRESS(fi) ((fi)->frame)

/* Return number of args passed to a frame.
   Can return -1, meaning no way to tell.
  
   The weirdness in the "addl $imm8" case is due to gcc sometimes
   issuing "addl $-int" after function call returns; this would
   produce ridiculously huge arg counts.  */

#define FRAME_NUM_ARGS(numargs, fi)  \
{ \
  int op = read_memory_integer(FRAME_SAVED_PC((fi)), 4); \
  int narg; \
  if ((op & 0xff) == 0x59) /* 0x59  'popl %ecx' */ \
    { \
      numargs = 1; \
    } \
  else if ((op & 0xffff) == 0xc483) /* 0xc483 'addl $imm8' */ \
    { \
      narg = ((op >> 16) & 0xff); \
      numargs = (narg >= 128) ? -1 : narg / 4; \
    } \
  else if ((op & 0xffff) == 0xc481) /* 0xc481 'addl $imm32' */ \
    { \
      narg = read_memory_integer(FRAME_SAVED_PC((fi))+2,4); \
      numargs = (narg < 0) ? -1 : narg / 4; \
    } \
  else \
    { \
      numargs = -1; \
    } \
}

/* Return number of bytes at start of arglist that are not really args.  */

#define FRAME_ARGS_SKIP 8

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.  */

#define FRAME_FIND_SAVED_REGS(frame_info, frame_saved_regs) \
{ i386_frame_find_saved_regs ((frame_info), &(frame_saved_regs)); }

#ifdef __STDC__		/* Forward decl's for prototypes */
struct frame_info;
struct frame_saved_regs;
#endif

extern void
i386_frame_find_saved_regs PARAMS ((struct frame_info *,
				    struct frame_saved_regs *));


/* Things needed for making the inferior call functions.  */

#define PUSH_DUMMY_FRAME \
{  CORE_ADDR sp = read_register (SP_REGNUM); \
  int regnum; \
  sp = push_word (sp, read_register (PC_REGNUM)); \
  sp = push_word (sp, read_register (FP_REGNUM)); \
  write_register (FP_REGNUM, sp); \
  for (regnum = 0; regnum < NUM_REGS; regnum++) \
    sp = push_word (sp, read_register (regnum)); \
  write_register (SP_REGNUM, sp); \
}

#define POP_FRAME  \
{ \
  FRAME frame = get_current_frame (); \
  CORE_ADDR fp; \
  int regnum; \
  struct frame_saved_regs fsr; \
  struct frame_info *fi; \
  fi = get_frame_info (frame); \
  fp = fi->frame; \
  get_frame_saved_regs (fi, &fsr); \
  for (regnum = 0; regnum < NUM_REGS; regnum++) { \
      CORE_ADDR adr; \
      adr = fsr.regs[regnum]; \
      if (adr) \
	write_register (regnum, read_memory_integer (adr, 4)); \
  } \
  write_register (FP_REGNUM, read_memory_integer (fp, 4)); \
  write_register (PC_REGNUM, read_memory_integer (fp + 4, 4)); \
  write_register (SP_REGNUM, fp + 8); \
  flush_cached_frames (); \
  set_current_frame ( create_new_frame (read_register (FP_REGNUM), \
					read_pc ())); \
}

/* from i386-dep.c, worked better than my original... */
/* This sequence of words is the instructions
 * call (32-bit offset)
 * int 3
 * This is 6 bytes.
 */

#define CALL_DUMMY { 0x223344e8, 0xcc11 }

#define CALL_DUMMY_LENGTH 8

#define CALL_DUMMY_START_OFFSET 0  /* Start execution at beginning of dummy */

/* Insert the specified number of args and function address
   into a call sequence of the above form stored at DUMMYNAME.  */

#define FIX_CALL_DUMMY(dummyname, pc, fun, nargs, args, type, gcc_p)   \
{ \
	int from, to, delta, loc; \
	loc = (int)(read_register (SP_REGNUM) - CALL_DUMMY_LENGTH); \
	from = loc + 5; \
	to = (int)(fun); \
	delta = to - from; \
	*(int *)((char *)(dummyname) + 1) = delta; \
}

extern void
print_387_control_word PARAMS ((unsigned int));

extern void
print_387_status_word PARAMS ((unsigned int));
