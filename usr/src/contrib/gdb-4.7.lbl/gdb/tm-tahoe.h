/* Definitions to make GDB target for a tahoe running 4.3-Reno.
   Copyright 1986, 1987, 1989, 1991, 1992 Free Software Foundation, Inc.

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

/*
 * Ported by the State University of New York at Buffalo by the Distributed
 * Computer Systems Lab, Department of Computer Science, 1991.
 */

#define TARGET_BYTE_ORDER BIG_ENDIAN
#define BITS_BIG_ENDIAN 0

/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */

#define NAMES_HAVE_UNDERSCORE

/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 2

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  */

#define SKIP_PROLOGUE(pc)	\
{ register int op = (unsigned char) read_memory_integer (pc, 1);  \
  if (op == 0x11) pc += 2;  /* skip brb */			  \
  if (op == 0x13) pc += 3;  /* skip brw */			  \
  if (op == 0x2c &&						  \
      ((unsigned char) read_memory_integer (pc+2, 1)) == 0x5e)	  \
    pc += 3;  /* skip subl2 */					  \
  if (op == 0xe9 &&						  \
      ((unsigned char) read_memory_integer (pc+1, 1)) == 0xae &&  \
      ((unsigned char) read_memory_integer(pc+3, 1)) == 0x5e)	  \
     pc += 4;  /* skip movab */					  \
  if (op == 0xe9 &&						  \
      ((unsigned char) read_memory_integer (pc+1, 1)) == 0xce &&  \
      ((unsigned char) read_memory_integer(pc+4, 1)) == 0x5e)	  \
    pc += 5;  /* skip movab */					  \
  if (op == 0xe9 &&						  \
      ((unsigned char) read_memory_integer (pc+1, 1)) == 0xee &&  \
      ((unsigned char) read_memory_integer(pc+6, 1)) == 0x5e)	  \
    pc += 7;  /* skip movab */					  \
}

/* Immediately after a function call, return the saved pc.
   Can't always go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

#define SAVED_PC_AFTER_CALL(frame) FRAME_SAVED_PC(frame)

/* Wrong for cross-debugging.  I don't know the real values.  */
#include <machine/param.h>
#define TARGET_UPAGES UPAGES
#define TARGET_NBPG NBPG

/* Address of end of stack space.  */

#define STACK_END_ADDR (0xc0000000 - (TARGET_UPAGES * TARGET_NBPG))

/* On BSD, sigtramp is in the u area.  Can't check the exact
   addresses because for cross-debugging we don't have target include
   files around.  This should be close enough.  */
#define IN_SIGTRAMP(pc, name) ((pc) >= STACK_END_ADDR && (pc < 0xc0000000))

/* Stack grows downward.  */

#define INNER_THAN <

/* Sequence of bytes for breakpoint instruction.  */

#define BREAKPOINT {0x30}

/* Amount PC must be decremented by after a breakpoint.
   This is often the number of bytes in BREAKPOINT
   but not always.  */

#define DECR_PC_AFTER_BREAK 0

/* Nonzero if instruction at PC is a return instruction.  */

#define ABOUT_TO_RETURN(pc) (read_memory_integer (pc, 1) == 0x40)

/* Return 1 if P points to an invalid floating point value.
   LEN is the length in bytes -- not relevant on the Tahoe. */

#define INVALID_FLOAT(p, len) ((*(short *) p & 0xff80) == 0x8000)

/* Say how long (ordinary) registers are.  */

#define REGISTER_TYPE long

/* Number of machine registers */

#define NUM_REGS 19

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */

#define REGISTER_NAMES {"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9", "r10", "r11", "r12", "fp", "sp", "pc", "ps", "al", "ah"}

#define FP_REGNUM 13		/* Contains address of executing stack frame */
#define SP_REGNUM 14		/* Contains address of top of stack */
#define PC_REGNUM 15		/* Contains program counter */
#define PS_REGNUM 16		/* Contains processor status */

#define AL_REGNUM 17		/* Contains accumulator */
#define AH_REGNUM 18

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */

#define REGISTER_BYTES (19*4)

/* Index within `registers' of the first byte of the space for
   register N.  */

#define REGISTER_BYTE(N) ((N) * 4)

/* Number of bytes of storage in the actual machine representation
   for register N.  On the tahoe, all regs are 4 bytes.  */

#define REGISTER_RAW_SIZE(N) 4

/* Number of bytes of storage in the program's representation
   for register N.  On the tahoe, all regs are 4 bytes.  */

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

/* Store the address of the place in which to copy the structure the
   subroutine will return.  This is called from call_function. */

#define STORE_STRUCT_RETURN(ADDR, SP) \
  { write_register (1, (ADDR)); }

/* Extract from an array REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF.  */

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
  bcopy (REGBUF, VALBUF, TYPE_LENGTH (TYPE))

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format.  */

#define STORE_RETURN_VALUE(TYPE,VALBUF) \
  write_register_bytes (0, VALBUF, TYPE_LENGTH (TYPE))

/* Extract from an array REGBUF containing the (raw) register state
   the address in which a function should return its structure value,
   as a CORE_ADDR (or an expression that can be used as one).  */

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) (*(int *)(REGBUF))

/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).

   FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer. */

/* In the case of the Tahoe, the frame's nominal address is the FP value,
   and it points to the old FP */

#define FRAME_CHAIN(thisframe)  \
  (!inside_entry_file ((thisframe)->pc) ? \
   read_memory_integer ((thisframe)->frame, 4) :\
   0)

/* Define other aspects of the stack frame.  */

/* Saved PC */

#define FRAME_SAVED_PC(FRAME) (read_memory_integer ((FRAME)->frame - 8, 4))

/* In most of GDB, getting the args address is too important to
   just say "I don't know". */

#define FRAME_ARGS_ADDRESS(fi) ((fi)->frame)

/* Address to use as an anchor for finding local variables */

#define FRAME_LOCALS_ADDRESS(fi) ((fi)->frame)

/* Return number of args passed to a frame.
   Can return -1, meaning no way to tell.  */

#define FRAME_NUM_ARGS(numargs, fi)  \
{ numargs = ((0xffff & read_memory_integer(((fi)->frame-4),4)) - 4) >> 2; }

/* Return number of bytes at start of arglist that are not really args.  */

#define FRAME_ARGS_SKIP 0

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.  */

#define FRAME_FIND_SAVED_REGS(frame_info, frame_saved_regs) \
{ register int regnum;     \
  register int rmask = read_memory_integer ((frame_info)->frame-4, 4) >> 16;\
  register CORE_ADDR next_addr;     \
  bzero (&frame_saved_regs, sizeof frame_saved_regs);     \
  next_addr = (frame_info)->frame - 8;     \
  for (regnum = 12; regnum >= 0; regnum--, rmask <<= 1)  \
    (frame_saved_regs).regs[regnum] = (rmask & 0x1000) ? (next_addr -= 4) : 0;\
  (frame_saved_regs).regs[SP_REGNUM] = (frame_info)->frame + 4;  \
  (frame_saved_regs).regs[PC_REGNUM] = (frame_info)->frame - 8;  \
  (frame_saved_regs).regs[FP_REGNUM] = (frame_info)->frame;      \
}

/* Things needed for making the inferior call functions.  */

/* Push an empty stack frame, to record the current PC, etc.  */

#define PUSH_DUMMY_FRAME \
{ register CORE_ADDR sp = read_register (SP_REGNUM);	\
  register int regnum;					\
printf("PUSH_DUMMY_FRAME\n");				\
  sp = push_word (sp, read_register (FP_REGNUM));	\
  write_register (FP_REGNUM, sp);			\
  sp = push_word (sp, 0x1fff0004);   /*SAVE MASK*/	\
  sp = push_word (sp, read_register (PC_REGNUM));	\
  for (regnum = 12; regnum >= 0; regnum--)		\
    sp = push_word (sp, read_register (regnum));	\
  write_register (SP_REGNUM, sp);			\
}

/* Discard from the stack the innermost frame, restoring all registers.  */

#define POP_FRAME  \
{ register CORE_ADDR fp = read_register (FP_REGNUM);			\
  register int regnum;							\
  register int regmask = read_memory_integer (fp-4, 4);			\
printf("POP_FRAME\n");							\
  regmask >>= 16;                                               	\
  write_register (SP_REGNUM, fp+4);	                           	\
  write_register (PC_REGNUM, read_memory_integer(fp-8, 4));	  	\
  write_register (FP_REGNUM, read_memory_integer(fp, 4));  		\
  fp -= 8;								\
  for (regnum = 12; regnum >= 0; regnum--, regmask <<= 1)		\
    if (regmask & 0x1000)                                            	\
      write_register (regnum, read_memory_integer (fp-=4, 4));		\
  flush_cached_frames ();						\
  set_current_frame (create_new_frame (read_register (FP_REGNUM),	\
					read_pc ())); }

/* This sequence of words is the instructions
     calls #69, @#32323232
     bpt
   Note this is 8 bytes.  */

#define CALL_DUMMY {0xbf699f32, 0x32323230}

/* Start execution at beginning of dummy */

#define CALL_DUMMY_START_OFFSET 0

/* Insert the specified number of args and function address
   into a call sequence of the above form stored at DUMMYNAME.  */

#define FIX_CALL_DUMMY(dummyname, pc, fun, nargs, args, valtype, using_gcc) \
{ int temp = (int) fun;				\
  *((char *) dummyname + 1) = nargs;		\
  bcopy(&temp,(char *)dummyname+3,4); }

