/* Definitions to make GDB run on a Sequent Symmetry under dynix 3.0,
   with Weitek 1167 and i387 support.
   Copyright (C) 1986, 1987, 1989 Free Software Foundation, Inc.

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

/* Symmetry version by Jay Vosburgh (uunet!sequent!fubar) */

#include <machine/reg.h>

#define SYMMETRY

/* This machine doesn't have the siginterrupt call.  */
#define NO_SIGINTERRUPT

#define HAVE_WAIT_STRUCT

/* Define the bit, byte, and word ordering of the machine.  */
/* #define BITS_BIG_ENDIAN */
/* #define BYTES_BIG_ENDIAN */
/* #define WORDS_BIG_ENDIAN */

/* Define SFILE_FN_FLAGGED if the source file is flagged with an N_FN
   symbol instead of an N_TEXT symbol.  */

#define OFILE_FN_FLAGGED

/* Get rid of any system-imposed stack limit if possible.  */

#define SET_STACK_LIMIT_HUGE

/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */

#define NAMES_HAVE_UNDERSCORE

/* Debugger information will be in DBX format.  */

#define READ_DBX_FORMAT

/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 0

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  From m-i386.h */

#define SKIP_PROLOGUE(frompc)   {(frompc) = i386_skip_prologue((frompc));}

/* Immediately after a function call, return the saved pc.
   Can't always go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

#define SAVED_PC_AFTER_CALL(frame) \
  read_memory_integer(read_register(SP_REGNUM), 4)

/* This is the amount to subtract from u.u_ar0
   to get the offset in the core file of the register values.  */

#define KERNEL_U_ADDR (0x80000000 - (UPAGES * NBPG))

/* Address of end of stack space.  */

#define STACK_END_ADDR (0x40000000 - (UPAGES * NBPG))

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

/* code for 80387 fpu.  Functions are from i386-dep.c, copied into
 * symm-dep.c.
 */
#define FLOAT_INFO { i386_float_info(); }

/* largest int type */
#define LONGEST long

#define BUILTIN_TYPE_LONGEST builtin_type_long


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

/* The magic numbers below are offsets into u_ar0 in the user struct.
 * They live in <machine/reg.h>.  Gdb calls this macro with blockend
 * holding u.u_ar0 - KERNEL_U_ADDR.  Only the registers listed are
 * saved in the u area (along with a few others that aren't useful
 * here.  See <machine/reg.h>).
 */

#define REGISTER_U_ADDR(addr, blockend, regno) \
{ struct user foo;	/* needed for finding fpu regs */ \
switch (regno) { \
    case 0: \
      addr = blockend + EAX * sizeof(int); break; \
  case 1: \
      addr = blockend + EDX * sizeof(int); break; \
  case 2: \
      addr = blockend + ECX * sizeof(int); break; \
  case 3:			/* st(0) */ \
      addr = blockend - \
	  ((int)&foo.u_fpusave.fpu_stack[0][0] - (int)&foo); \
      break; \
  case 4:			/* st(1) */ \
      addr = blockend - \
	  ((int) &foo.u_fpusave.fpu_stack[1][0] - (int)&foo); \
      break; \
  case 5: \
      addr = blockend + EBX * sizeof(int); break; \
  case 6: \
      addr = blockend + ESI * sizeof(int); break; \
  case 7: \
      addr = blockend + EDI * sizeof(int); break; \
  case 8:			/* st(2) */ \
      addr = blockend - \
	  ((int) &foo.u_fpusave.fpu_stack[2][0] - (int)&foo); \
      break; \
  case 9:			/* st(3) */ \
      addr = blockend - \
	  ((int) &foo.u_fpusave.fpu_stack[3][0] - (int)&foo); \
      break; \
  case 10:			/* st(4) */ \
      addr = blockend - \
	  ((int) &foo.u_fpusave.fpu_stack[4][0] - (int)&foo); \
      break; \
  case 11:			/* st(5) */ \
      addr = blockend - \
	  ((int) &foo.u_fpusave.fpu_stack[5][0] - (int)&foo); \
      break; \
  case 12:			/* st(6) */ \
      addr = blockend - \
	  ((int) &foo.u_fpusave.fpu_stack[6][0] - (int)&foo); \
      break; \
  case 13:			/* st(7) */ \
      addr = blockend - \
	  ((int) &foo.u_fpusave.fpu_stack[7][0] - (int)&foo); \
      break; \
  case 14: \
      addr = blockend + ESP * sizeof(int); break; \
  case 15: \
      addr = blockend + EBP * sizeof(int); break; \
  case 16: \
      addr = blockend + EIP * sizeof(int); break; \
  case 17: \
      addr = blockend + FLAGS * sizeof(int); break; \
  case 18:			/* fp1 */ \
  case 19:			/* fp2 */ \
  case 20:			/* fp3 */ \
  case 21:			/* fp4 */ \
  case 22:			/* fp5 */ \
  case 23:			/* fp6 */ \
  case 24:			/* fp7 */ \
  case 25:			/* fp8 */ \
  case 26:			/* fp9 */ \
  case 27:			/* fp10 */ \
  case 28:			/* fp11 */ \
  case 29:			/* fp12 */ \
  case 30:			/* fp13 */ \
  case 31:			/* fp14 */ \
  case 32:			/* fp15 */ \
  case 33:			/* fp16 */ \
  case 34:			/* fp17 */ \
  case 35:			/* fp18 */ \
  case 36:			/* fp19 */ \
  case 37:			/* fp20 */ \
  case 38:			/* fp21 */ \
  case 39:			/* fp22 */ \
  case 40:			/* fp23 */ \
  case 41:			/* fp24 */ \
  case 42:			/* fp25 */ \
  case 43:			/* fp26 */ \
  case 44:			/* fp27 */ \
  case 45:			/* fp28 */ \
  case 46:			/* fp29 */ \
  case 47:			/* fp30 */ \
  case 48:			/* fp31 */ \
     addr = blockend - \
	 ((int) &foo.u_fpasave.fpa_regs[(regno)-18] - (int)&foo); \
  } \
}

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

/* Convert data from virtual format for register REGNUM
   to raw format for register REGNUM.  */

#define REGISTER_CONVERT_TO_RAW(REGNUM,FROM,TO)	\
((REGNUM < 3) ? bcopy ((FROM), (TO), 4) : \
(REGNUM < 5) ? double_to_i387((FROM), (TO)) : \
(REGNUM < 8) ? bcopy ((FROM), (TO), 4) : \
(REGNUM < 14) ? double_to_i387((FROM), (TO)) : \
    bcopy ((FROM), (TO), 4))

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

/* Compensate for lack of `vprintf' function.  */
#ifndef HAVE_VPRINTF
#define vprintf(format, ap) _doprnt (format, ap, stdout)
#endif /* not HAVE_VPRINTF */

/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer.

   FRAME_CHAIN_COMBINE takes the chain pointer and the frame's nominal address
   and produces the nominal address of the caller frame.

   However, if FRAME_CHAIN_VALID returns zero,
   it means the given frame is the outermost one and has no caller.
   In that case, FRAME_CHAIN_COMBINE is not used.  */

/* On Symmetry, %ebp points to caller's %ebp, and the return address
   is right on top of that.
*/

#define FRAME_CHAIN(thisframe)  \
  (outside_startup_file ((thisframe)->pc) ? \
   read_memory_integer((thisframe)->frame, 4) :\
   0)

#define FRAME_CHAIN_VALID(chain, thisframe) \
  (chain != 0)

#define FRAME_CHAIN_COMBINE(chain, thisframe) (chain)

/* Define other aspects of the stack frame.  */

/* A macro that tells us whether the function invocation represented
   by FI does not have a frame on the stack associated with it.  If it
   does not, FRAMELESS is set to 1, else 0.  */
#define FRAMELESS_FUNCTION_INVOCATION(FI, FRAMELESS) \
  FRAMELESS_LOOK_FOR_PROLOGUE(FI, FRAMELESS)

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

#define FIX_CALL_DUMMY(dummyname, pc, fun, nargs, type)   \
{ \
	int from, to, delta, loc; \
	loc = (int)(read_register (SP_REGNUM) - CALL_DUMMY_LENGTH); \
	from = loc + 5; \
	to = (int)(fun); \
	delta = to - from; \
	*(int *)((char *)(dummyname) + 1) = delta; \
}

/* Interface definitions for kernel debugger KDB.  */
/* This doesn't work... */
/* Map machine fault codes into signal numbers.
   First subtract 0, divide by 4, then index in a table.
   Faults for which the entry in this table is 0
   are not handled by KDB; the program's own trap handler
   gets to handle then.  */

#define FAULT_CODE_ORIGIN 0
#define FAULT_CODE_UNITS 4
#define FAULT_TABLE    \
{ 0, SIGKILL, SIGSEGV, 0, 0, 0, 0, 0, \
  0, 0, SIGTRAP, SIGTRAP, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0}

/* Start running with a stack stretching from BEG to END.
   BEG and END should be symbols meaningful to the assembler.
   This is used only for kdb.  */

#define INIT_STACK(beg, end)  \
{ asm (".globl end");         \
  asm ("movl $ end, %esp");      \
  asm ("movl %ebp, $0"); }

/* Push the frame pointer register on the stack.  */
#define PUSH_FRAME_PTR        \
  asm ("pushl %ebp");

/* Copy the top-of-stack to the frame pointer register.  */
#define POP_FRAME_PTR  \
  asm ("movl (%esp), %ebp");

/* After KDB is entered by a fault, push all registers
   that GDB thinks about (all NUM_REGS of them),
   so that they appear in order of ascending GDB register number.
   The fault code will be on the stack beyond the last register.  */

#define PUSH_REGISTERS        \
{ asm("pushad"); }
/*
{ asm("pushl %eax"); \
  asm("pushl %edx"); \
  asm("pushl %ecx"); \
  asm("pushl %st(0)"); \
  asm("pushl %st(1)"); \
  asm("pushl %ebx"); \
  asm("pushl %esi"); \
  asm("pushl %edi"); \
  asm("pushl %st(2)"); \
  asm("pushl %st(3)"); \
  asm("pushl %st(4)"); \
  asm("pushl %st(5)"); \
  asm("pushl %st(6)"); \
  asm("pushl %st(7)"); \
  asm("pushl %esp"); \
  asm("pushl %ebp"); \
  asm("pushl %eip"); \
  asm("pushl %eflags"); \
  asm("pushl %fp1"); \
  asm("pushl %fp2"); \
  asm("pushl %fp3"); \
  asm("pushl %fp4"); \
  asm("pushl %fp5"); \
  asm("pushl %fp6"); \
  asm("pushl %fp7"); \
  asm("pushl %fp8"); \ 
  asm("pushl %fp9"); \
  asm("pushl %fp10"); \
  asm("pushl %fp11"); \
  asm("pushl %fp12"); \
  asm("pushl %fp13"); \
  asm("pushl %fp14"); \
  asm("pushl %fp15"); \
  asm("pushl %fp16"); \
  asm("pushl %fp17"); \
  asm("pushl %fp18"); \
  asm("pushl %fp19"); \
  asm("pushl %fp20"); \
  asm("pushl %fp21"); \
  asm("pushl %fp22"); \ 
  asm("pushl %fp23"); \
  asm("pushl %fp24"); \
  asm("pushl %fp25"); \
  asm("pushl %fp26"); \
  asm("pushl %fp27"); \
  asm("pushl %fp28"); \
  asm("pushl %fp29"); \
  asm("pushl %fp30"); \
  asm("pushl %fp31"); \
}
*/
/* Assuming the registers (including processor status) have been
   pushed on the stack in order of ascending GDB register number,
   restore them and return to the address in the saved PC register.  */

#define POP_REGISTERS      \
{ asm ("popad"); }
