/* Definitions to make GDB run on an encore under umax 4.2
   Copyright (C) 1987, 1989 Free Software Foundation, Inc.

This file is part of GDB.

GDB is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GDB is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GDB; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#ifndef ns16000
#define ns16000
#endif

#define HAVE_WAIT_STRUCT

/* Define the bit, byte, and word ordering of the machine.  */
/* #define BITS_BIG_ENDIAN */
/* #define BYTES_BIG_ENDIAN */
/* #define WORDS_BIG_ENDIAN */

/* Encore's modifications to ptrace format */

#define UMAX_PTRACE

/* Encore's modifications to core-file format */

#define UMAX_CORE

/* Do implement the attach and detach commands.  */

#define ATTACH_DETACH

/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */

#define NAMES_HAVE_UNDERSCORE

/* Exec files and symbol tables are in COFF format */

#define COFF_FORMAT

/* Doesn't have siginterupt.  */
#define NO_SIGINTERRUPT

/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 0

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  */

#define SKIP_PROLOGUE(pc)   				\
{ register unsigned char op = read_memory_integer (pc, 1);	\
  if (op == 0x82) { op = read_memory_integer (pc+2,1);  \
  		    if ((op & 0x80) == 0) pc += 3;	\
		    else if ((op & 0xc0) == 0x80) pc += 4;	\
		    else pc += 6;			\
		   }					\
}

/* Immediately after a function call, return the saved pc.
   Can't always go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

#define SAVED_PC_AFTER_CALL(frame) \
	read_memory_integer (read_register (SP_REGNUM), 4)

/* Address of end of stack space.  */

#define STACK_END_ADDR (0xfffff000)

/* Stack grows downward.  */

#define INNER_THAN <

/* Sequence of bytes for breakpoint instruction.  */

#define BREAKPOINT {0xf2}

/* Amount PC must be decremented by after a breakpoint.
   This is often the number of bytes in BREAKPOINT
   but not always.  */

#define DECR_PC_AFTER_BREAK 0

/* Nonzero if instruction at PC is a return instruction.  */

#define ABOUT_TO_RETURN(pc) (read_memory_integer (pc, 1) == 0x12)

#ifndef NaN
#include <nan.h>
#endif NaN

/* Return 1 if P points to an invalid floating point value.  */

#define INVALID_FLOAT(p, s) \
	 ((s == sizeof (float))?	\
		NaF (*(float *) p) :	\
		NaD (*(double *) p))

/* Largest integer type */
#define LONGEST long

/* Name of the builtin type for the LONGEST type above. */
#define BUILTIN_TYPE_LONGEST builtin_type_long

/* Say how long (ordinary) registers are.  */

#define REGISTER_TYPE long

/* Number of machine registers */

#define NUM_REGS		25

#define NUM_GENERAL_REGS	8

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */

#define REGISTER_NAMES {"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",	\
 			"f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",	\
			"sp", "fp", "pc", "ps",				\
 			"fsr",						\
			"l0", "l1", "l2", "l3", "xx",			\
 			}

/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */

#define FP0_REGNUM 8		/* Floating point register 0 */
#define SP_REGNUM 16		/* Contains address of top of stack */
#define AP_REGNUM FP_REGNUM
#define FP_REGNUM 17		/* Contains address of executing stack frame */
#define PC_REGNUM 18		/* Contains program counter */
#define PS_REGNUM 19		/* Contains processor status */
#define FPS_REGNUM 20		/* Floating point status register */
#define LP0_REGNUM 21		/* Double register 0 (same as FP0) */

/* called from register_addr() -- blockend not used for now */
#define REGISTER_U_ADDR(addr, blockend, regno) \
{ 									\
  switch (regno) {							\
  case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7:	\
	addr = PU_R0 - (regno * sizeof (int)); break;			\
  case SP_REGNUM:							\
  	addr = PU_SP; break;						\
  case PC_REGNUM:							\
  	addr = PU_PC; break;						\
  case FP_REGNUM:							\
  	addr = PU_FP; break;						\
  case PS_REGNUM:							\
  	addr = PU_PSL; break;						\
  case FPS_REGNUM:							\
  	addr = PU_FSR; break;						\
  case FP0_REGNUM + 0: case FP0_REGNUM + 1:				\
  case FP0_REGNUM + 2: case FP0_REGNUM + 3: 				\
  case FP0_REGNUM + 4: case FP0_REGNUM + 5: 				\
  case FP0_REGNUM + 6: case FP0_REGNUM + 7: 				\
  	addr = PU_F0 + (regno - FP0_REGNUM) * sizeof (float); break;	\
  case LP0_REGNUM + 0: case LP0_REGNUM + 1:				\
  case LP0_REGNUM + 2: case LP0_REGNUM + 3:				\
  	addr = PU_F0 + (regno - LP0_REGNUM) * sizeof (double); break;	\
  default:								\
  	printf ("bad argument to REGISTER_U_ADDR %d\n", regno);		\
	abort ();							\
  }									\
}

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */
#define REGISTER_BYTES ((NUM_REGS - 4) * sizeof (int) + 4 * sizeof (double))

/* Index within `registers' of the first byte of the space for
   register N.  */

#define REGISTER_BYTE(N) ((N) >= LP0_REGNUM ? \
	LP0_REGNUM * 4 + ((N) - LP0_REGNUM) * 8 : (N) * 4)

/* Number of bytes of storage in the actual machine representation
   for register N.  On the 32000, all regs are 4 bytes
   except for the doubled floating registers. */

#define REGISTER_RAW_SIZE(N) ((N) >= LP0_REGNUM ? 8 : 4)

/* Number of bytes of storage in the program's representation
   for register N.  On the 32000, all regs are 4 bytes
   except for the doubled floating registers. */

#define REGISTER_VIRTUAL_SIZE(N) ((N) >= LP0_REGNUM ? 8 : 4)

/* Largest value REGISTER_RAW_SIZE can have.  */

#define MAX_REGISTER_RAW_SIZE 8

/* Largest value REGISTER_VIRTUAL_SIZE can have.  */

#define MAX_REGISTER_VIRTUAL_SIZE 8

/* Nonzero if register N requires conversion
   from raw format to virtual format.  */

#define REGISTER_CONVERTIBLE(N) 0

/* Convert data from raw format for register REGNUM
   to virtual format for register REGNUM.  */

#define REGISTER_CONVERT_TO_VIRTUAL(REGNUM,FROM,TO)	\
  bcopy ((FROM), (TO), REGISTER_VIRTUAL_SIZE(REGNUM));

/* Convert data from virtual format for register REGNUM
   to raw format for register REGNUM.  */

#define REGISTER_CONVERT_TO_RAW(REGNUM,FROM,TO)	\
  bcopy ((FROM), (TO), REGISTER_VIRTUAL_SIZE(REGNUM));

/* Return the GDB type object for the "standard" data type
   of data in register N.  */

#define REGISTER_VIRTUAL_TYPE(N) \
	(((N) < FP0_REGNUM) ?				\
		builtin_type_int :			\
		((N) < FP0_REGNUM + 8) ?		\
			builtin_type_float :		\
			((N) < LP0_REGNUM) ?		\
				builtin_type_int :	\
				builtin_type_double)

/* Store the address of the place in which to copy the structure the
   subroutine will return.  This is called from call_function.

   On this machine this is a no-op, because gcc isn't used on it
   yet.  So this calling convention is not used. */

#define STORE_STRUCT_RETURN(ADDR, SP)

/* Extract from an array REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF.  */

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
  bcopy (REGBUF+REGISTER_BYTE (TYPE_CODE (TYPE) == TYPE_CODE_FLT ? FP0_REGNUM : 0), VALBUF, TYPE_LENGTH (TYPE))

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format.  */

#define STORE_RETURN_VALUE(TYPE,VALBUF) \
  write_register_bytes (REGISTER_BYTE (TYPE_CODE (TYPE) == TYPE_CODE_FLT ? FP0_REGNUM : 0), VALBUF, TYPE_LENGTH (TYPE))

/* Extract from an array REGBUF containing the (raw) register state
   the address in which a function should return its structure value,
   as a CORE_ADDR (or an expression that can be used as one).  */

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) (*(int *)(REGBUF))

/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer.

   FRAME_CHAIN_COMBINE takes the chain pointer and the frame's nominal address
   and produces the nominal address of the caller frame.

   However, if FRAME_CHAIN_VALID returns zero,
   it means the given frame is the outermost one and has no caller.
   In that case, FRAME_CHAIN_COMBINE is not used.  */

/* In the case of the ns32000 series, the frame's nominal address is the FP
   value, and at that address is saved previous FP value as a 4-byte word.  */

#define FRAME_CHAIN(thisframe)  \
  (outside_startup_file ((thisframe)->pc) ? \
   read_memory_integer ((thisframe)->frame, 4) :\
   0)

#define FRAME_CHAIN_VALID(chain, thisframe) \
  (chain != 0 && (outside_startup_file (FRAME_SAVED_PC (thisframe))))

#define FRAME_CHAIN_COMBINE(chain, thisframe) (chain)

/* Define other aspects of the stack frame.  */

#define FRAME_SAVED_PC(FRAME) (read_memory_integer ((FRAME)->frame + 4, 4))

/* Compute base of arguments. */

#define FRAME_ARGS_ADDRESS(fi)	\
  ((ns32k_get_enter_addr ((fi)->pc) > 1) ? \
	((fi)->frame) : (read_register (SP_REGNUM) - 4))

#define FRAME_LOCALS_ADDRESS(fi) ((fi)->frame)

/* Get the address of the enter opcode for this function, if it is active.
   Returns positive address > 1 if pc is between enter/exit,
   1 if pc before enter or after exit, 0 otherwise. */

#ifndef CORE_ADDR
#include "defs.h"   /* Make sure CORE_ADDR is defined.  */
#endif

extern CORE_ADDR ns32k_get_enter_addr ();

/* Return number of args passed to a frame.
   Can return -1, meaning no way to tell.
   Encore's C compiler often reuses same area on stack for args,
   so this will often not work properly.  If the arg names
   are known, it's likely most of them will be printed. */

#define FRAME_NUM_ARGS(numargs, fi)			\
{ CORE_ADDR	pc;					\
  CORE_ADDR	enter_addr;				\
  unsigned int	insn;					\
  unsigned int	addr_mode;				\
  int width;						\
							\
  numargs = -1;						\
  enter_addr = ns32k_get_enter_addr ((fi)->pc);		\
  if (enter_addr > 0)					\
    {							\
      pc = (enter_addr == 1) ?				\
	SAVED_PC_AFTER_CALL (fi) :			\
	FRAME_SAVED_PC (fi);				\
      insn = read_memory_integer (pc,2);		\
      addr_mode = (insn >> 11) & 0x1f;			\
      insn = insn & 0x7ff;				\
      if ((insn & 0x7fc) == 0x57c &&			\
		addr_mode == 0x14) /* immediate */	\
	{						\
	  if (insn == 0x57c) /* adjspb */		\
  		width = 1;				\
	  else if (insn == 0x57d) /* adjspw */		\
  		width = 2;				\
	  else if (insn == 0x57f) /* adjspd */		\
  		width = 4;				\
	  numargs = read_memory_integer (pc+2,width);	\
	  if (width > 1)				\
	    flip_bytes (&numargs, width);		\
	  numargs = - sign_extend (numargs, width*8) / 4;\
	}						\
    }							\
}

/* Return number of bytes at start of arglist that are not really args.  */

#define FRAME_ARGS_SKIP 8

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.  */

#define FRAME_FIND_SAVED_REGS(frame_info, frame_saved_regs)	\
{ 								\
  register int	regmask, regnum;				\
  int		localcount;					\
  register CORE_ADDR	enter_addr;				\
  register CORE_ADDR	next_addr;				\
								\
  bzero (&(frame_saved_regs), sizeof (frame_saved_regs));	\
  enter_addr = ns32k_get_enter_addr ((frame_info)->pc);		\
  if (enter_addr > 1)						\
    {								\
      regmask = read_memory_integer (enter_addr+1, 1) & 0xff;	\
      localcount = ns32k_localcount (enter_addr);		\
      next_addr = (frame_info)->frame + localcount;		\
      for (regnum = 0; regnum < 8; regnum++, regmask >>= 1)	\
	(frame_saved_regs).regs[regnum] = (regmask & 1) ?	\
					  (next_addr -= 4) : 0;	\
      (frame_saved_regs).regs[SP_REGNUM] = (frame_info)->frame + 4;\
      (frame_saved_regs).regs[PC_REGNUM] = (frame_info)->frame + 4;\
      (frame_saved_regs).regs[FP_REGNUM] =			\
		  (read_memory_integer ((frame_info)->frame, 4));\
    }								\
  else if (enter_addr == 1)					\
    {								\
      CORE_ADDR sp = read_register (SP_REGNUM);			\
      (frame_saved_regs).regs[PC_REGNUM] = sp;			\
      (frame_saved_regs).regs[SP_REGNUM] = sp + 4;		\
    }								\
}

/* Compensate for lack of `vprintf' function.  */
#ifndef HAVE_VPRINTF
#define vprintf(format, ap) _doprnt (format, ap, stdout)
#endif /* not HAVE_VPRINTF */

/* Things needed for making the inferior call functions.  */

/* Push an empty stack frame, to record the current PC, etc.  */

#define PUSH_DUMMY_FRAME \
{ register CORE_ADDR sp = read_register (SP_REGNUM);\
  register int regnum;				    \
  sp = push_word (sp, read_register (PC_REGNUM));   \
  sp = push_word (sp, read_register (FP_REGNUM));   \
  write_register (FP_REGNUM, sp);		    \
  for (regnum = 0; regnum < 8; regnum++)  \
    sp = push_word (sp, read_register (regnum));    \
  write_register (SP_REGNUM, sp);  			\
}

/* Discard from the stack the innermost frame, restoring all registers.  */

#define POP_FRAME  \
{ register FRAME frame = get_current_frame ();			 \
  register CORE_ADDR fp;					 \
  register int regnum;						 \
  struct frame_saved_regs fsr;					 \
  struct frame_info *fi;						 \
  fi = get_frame_info (frame);					 \
  fp = fi->frame;						 \
  get_frame_saved_regs (fi, &fsr);				 \
  for (regnum = 0; regnum < 8; regnum++)			 \
    if (fsr.regs[regnum])					 \
      write_register (regnum, read_memory_integer (fsr.regs[regnum], 4)); \
  write_register (FP_REGNUM, read_memory_integer (fp, 4));	 \
  write_register (PC_REGNUM, read_memory_integer (fp + 4, 4));   \
  write_register (SP_REGNUM, fp + 8);				 \
  flush_cached_frames ();					 \
  set_current_frame (create_new_frame (read_register (FP_REGNUM),\
				       read_pc ())); }

/* This sequence of words is the instructions
     enter	0xff,0		82 ff 00
     jsr	@0x00010203	7f ae c0 01 02 03
     adjspd	0x69696969	7f a5 01 02 03 04
     bpt			f2
   Note this is 16 bytes.  */

#define CALL_DUMMY { 0x7f00ff82, 0x0201c0ae, 0x01a57f03, 0xf2040302 }

#define CALL_DUMMY_START_OFFSET	3
#define CALL_DUMMY_LENGTH	16
#define CALL_DUMMY_ADDR		5
#define CALL_DUMMY_NARGS	11

/* Insert the specified number of args and function address
   into a call sequence of the above form stored at DUMMYNAME.  */

#define FIX_CALL_DUMMY(dummyname, pc, fun, nargs, type)   		\
{								\
	int	flipped;					\
	flipped = fun | 0xc0000000;				\
	flip_bytes (&flipped, 4);				\
	*((int *) (((char *) dummyname)+CALL_DUMMY_ADDR)) = flipped;	\
	flipped = - nargs * 4;					\
	flip_bytes (&flipped, 4);				\
	*((int *) (((char *) dummyname)+CALL_DUMMY_NARGS)) = flipped;	\
}
