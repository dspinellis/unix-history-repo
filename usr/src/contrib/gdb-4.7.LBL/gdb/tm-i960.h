/* Parameters for target machine Intel 960, for GDB, the GNU debugger.
   Copyright (C) 1990-1991 Free Software Foundation, Inc.
   Contributed by Intel Corporation.
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

/* Definitions to target GDB to any i960.  */

#ifndef I80960
#define I80960
#endif

/* Hook for the SYMBOL_CLASS of a parameter when decoding DBX symbol
   information.  In the i960, parameters can be stored as locals or as
   args, depending on the type of the debug record.

   From empirical observation, gcc960 uses N_LSYM to indicate
   arguments passed in registers and then copied immediately
   to the frame, and N_PSYM to indicate arguments passed in a
   g14-relative argument block.  */

#define	DBX_PARM_SYMBOL_CLASS(type) ((type == N_LSYM)? LOC_LOCAL_ARG: LOC_ARG)

/* Byte order is configurable, but this machine runs little-endian.  */
#define	TARGET_BYTE_ORDER	LITTLE_ENDIAN

/* We have IEEE floating point, if we have any float at all.  */

#define IEEE_FLOAT

/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */

#define NAMES_HAVE_UNDERSCORE


/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 0

/* Advance ip across any function entry prologue instructions
   to reach some "real" code.  */

#define SKIP_PROLOGUE(ip)	{ ip = skip_prologue (ip); }
extern CORE_ADDR skip_prologue ();

/* Immediately after a function call, return the saved ip.
   Can't always go through the frames for this because on some machines
   the new frame is not set up until the new function
   executes some instructions.  */

#define SAVED_PC_AFTER_CALL(frame) (saved_pc_after_call (frame))
extern CORE_ADDR saved_pc_after_call ();

/* Stack grows upward */

#define INNER_THAN >

/* Nonzero if instruction at ip is a return instruction.  */

#define ABOUT_TO_RETURN(ip) (read_memory_integer(ip,4) == 0x0a000000)

/* Return 1 if P points to an invalid floating point value.
   LEN is the length in bytes.  */

#define INVALID_FLOAT(p, len) (0)

/* How long (ordinary) registers are */

#define REGISTER_TYPE long

/* Number of machine registers */
#define NUM_REGS 40

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */

#define REGISTER_NAMES { \
	/*  0 */ "pfp", "sp",  "rip", "r3",  "r4",  "r5",  "r6",  "r7", \
	/*  8 */ "r8",  "r9",  "r10", "r11", "r12", "r13", "r14", "r15",\
	/* 16 */ "g0",  "g1",  "g2",  "g3",  "g4",  "g5",  "g6",  "g7", \
	/* 24 */ "g8",  "g9",  "g10", "g11", "g12", "g13", "g14", "fp", \
	/* 32 */ "pcw", "ac",  "tc",  "ip",  "fp0", "fp1", "fp2", "fp3",\
}

/* Register numbers of various important registers (used to index
   into arrays of register names and register values).  */

#define R0_REGNUM   0	/* First local register		*/
#define SP_REGNUM   1	/* Contains address of top of stack */
#define RIP_REGNUM  2	/* Return instruction pointer (local r2) */
#define R15_REGNUM 15	/* Last local register		*/
#define G0_REGNUM  16	/* First global register	*/
#define G13_REGNUM 29	/* g13 - holds struct return address */
#define G14_REGNUM 30	/* g14 - ptr to arg block / leafproc return address */
#define FP_REGNUM  31	/* Contains address of executing stack frame */
#define	PCW_REGNUM 32	/* process control word */
#define	ACW_REGNUM 33	/* arithmetic control word */
#define	TCW_REGNUM 34	/* trace control word */
#define IP_REGNUM  35	/* instruction pointer */
#define FP0_REGNUM 36	/* First floating point register */

/* Some registers have more than one name */

#define PC_REGNUM  IP_REGNUM	/* GDB refers to ip as the Program Counter */
#define PFP_REGNUM R0_REGNUM	/* Previous frame pointer	*/

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */
#define REGISTER_BYTES ((36*4) + (4*10))

/* Index within `registers' of the first byte of the space for register N.  */

#define REGISTER_BYTE(N) ( (N) < FP0_REGNUM ? \
				(4*(N)) : ((10*(N)) - (6*FP0_REGNUM)) )

/* The i960 has register windows, sort of.  */

#define HAVE_REGISTER_WINDOWS

/* Is this register part of the register window system?  A yes answer
   implies that 1) The name of this register will not be the same in
   other frames, and 2) This register is automatically "saved" upon
   subroutine calls and thus there is no need to search more than one
   stack frame for it.
   
   On the i960, in fact, the name of this register in another frame is
   "mud" -- there is no overlap between the windows.  Each window is
   simply saved into the stack (true for our purposes, after having been
   flushed; normally they reside on-chip and are restored from on-chip
   without ever going to memory).  */

#define REGISTER_IN_WINDOW_P(regnum)	((regnum) <= R15_REGNUM)

/* Number of bytes of storage in the actual machine representation
   for register N.  On the i960, all regs are 4 bytes except for floating
   point, which are 10.  NINDY only sends us 8 byte values for these,
   which is a pain, but VxWorks handles this correctly, so we must.  */

#define REGISTER_RAW_SIZE(N)		( (N) < FP0_REGNUM ? 4 : 10 )

/* Number of bytes of storage in the program's representation for register N. */

#define REGISTER_VIRTUAL_SIZE(N)	( (N) < FP0_REGNUM ? 4 : 8 )

/* Largest value REGISTER_RAW_SIZE can have.  */

#define MAX_REGISTER_RAW_SIZE 10

/* Largest value REGISTER_VIRTUAL_SIZE can have.  */

#define MAX_REGISTER_VIRTUAL_SIZE 8

/* Nonzero if register N requires conversion from raw format to virtual
   format.  */

#define REGISTER_CONVERTIBLE(N) ((N) >= FP0_REGNUM)

/* Convert data from raw format for register REGNUM
   to virtual format for register REGNUM.  */

extern struct ext_format ext_format_i960;

#define REGISTER_CONVERT_TO_VIRTUAL(REGNUM,FROM,TO)     \
{ \
  if ((REGNUM) >= FP0_REGNUM)   \
    ieee_extended_to_double (&ext_format_i960, (FROM), (double *)(TO));     \
  else                                  \
    bcopy ((FROM), (TO), 4);	\
}

/* Convert data from virtual format for register REGNUM
   to raw format for register REGNUM.  */

#define REGISTER_CONVERT_TO_RAW(REGNUM,FROM,TO) \
{ \
  if ((REGNUM) >= FP0_REGNUM)   \
    double_to_ieee_extended (&ext_format_i960, (double *)(FROM), (TO));     \
  else                                  \
    bcopy ((FROM), (TO), 4); 	\
}


/* Return the GDB type object for the "standard" data type
   of data in register N.  */

#define REGISTER_VIRTUAL_TYPE(N) ((N) < FP0_REGNUM ? \
					builtin_type_int : builtin_type_double)

/* Macros for understanding function return values... */

/* Does the specified function use the "struct returning" convention
   or the "value returning" convention?  The "value returning" convention
   almost invariably returns the entire value in registers.  The
   "struct returning" convention often returns the entire value in
   memory, and passes a pointer (out of or into the function) saying
   where the value (is or should go).

   Since this sometimes depends on whether it was compiled with GCC,
   this is also an argument.  This is used in call_function to build a
   stack, and in value_being_returned to print return values.

   On i960, a structure is returned in registers g0-g3, if it will fit.
   If it's more than 16 bytes long, g13 pointed to it on entry.  */

#define USE_STRUCT_CONVENTION(gcc_p, type) (TYPE_LENGTH (type) > 16)

/* Extract from an array REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF.  This is only called if USE_STRUCT_CONVENTION for this
   type is 0.

   On the i960 we just take as many bytes as we need from G0 through G3.  */

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
	bcopy(REGBUF+REGISTER_BYTE(G0_REGNUM), VALBUF, TYPE_LENGTH (TYPE))

/* If USE_STRUCT_CONVENTION produces a 1, 
   extract from an array REGBUF containing the (raw) register state
   the address in which a function should return its structure value,
   as a CORE_ADDR (or an expression that can be used as one).

   Address of where to put structure was passed in in global
   register g13 on entry.  God knows what's in g13 now.  The
   (..., 0) below is to make it appear to return a value, though
   actually all it does is call error().  */

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) \
   (error("Don't know where large structure is returned on i960"), 0)

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format, for "value returning" functions.
  
   For 'return' command:  not (yet) implemented for i960.  */

#define STORE_RETURN_VALUE(TYPE,VALBUF) \
    error ("Returning values from functions is not implemented in i960 gdb")

/* Store the address of the place in which to copy the structure the
   subroutine will return.  This is called from call_function. */

#define STORE_STRUCT_RETURN(ADDR, SP) \
    error ("Returning values from functions is not implemented in i960 gdb")

/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer.

   However, if FRAME_CHAIN_VALID returns zero,
   it means the given frame is the outermost one and has no caller.  */

/* We cache information about saved registers in the frame structure,
   to save us from having to re-scan function prologues every time
   a register in a non-current frame is accessed.  */

#define EXTRA_FRAME_INFO 	\
	struct frame_saved_regs *fsr;	\
	CORE_ADDR arg_pointer;

/* Zero the frame_saved_regs pointer when the frame is initialized,
   so that FRAME_FIND_SAVED_REGS () will know to allocate and
   initialize a frame_saved_regs struct the first time it is called.
   Set the arg_pointer to -1, which is not valid; 0 and other values
   indicate real, cached values.  */

#define INIT_EXTRA_FRAME_INFO(fromleaf, fi) \
	((fi)->fsr = 0, (fi)->arg_pointer = -1)

/* On the i960, we get the chain pointer by reading the PFP saved
   on the stack and clearing the status bits.  */

#define FRAME_CHAIN(thisframe) \
  (read_memory_integer (FRAME_FP(thisframe), 4) & ~0xf)

/* FRAME_CHAIN_VALID returns zero if the given frame is the outermost one
   and has no caller.

   On the i960, each various target system type must define FRAME_CHAIN_VALID,
   since it differs between NINDY and VxWorks, the two currently supported
   targets types.  We leave it undefined here.  */


/* A macro that tells us whether the function invocation represented
   by FI does not have a frame on the stack associated with it.  If it
   does not, FRAMELESS is set to 1, else 0.  */

#define FRAMELESS_FUNCTION_INVOCATION(FI, FRAMELESS) \
  { (FRAMELESS) = (leafproc_return ((FI)->pc) != 0); }

/* Note that in the i960 architecture the return pointer is saved in the
   *caller's* stack frame.
  
   Make sure to zero low-order bits because of bug in 960CA A-step part
   (instruction addresses should always be word-aligned anyway).  */

#define FRAME_SAVED_PC(frame) \
			((read_memory_integer(FRAME_CHAIN(frame)+8,4)) & ~3)

/* On the i960, FRAME_ARGS_ADDRESS should return the value of
   g14 as passed into the frame, if known.  We need a function for this.
   We cache this value in the frame info if we've already looked it up.  */

#define FRAME_ARGS_ADDRESS(fi) 	\
  (((fi)->arg_pointer != -1)? (fi)->arg_pointer: frame_args_address (fi, 0))
extern CORE_ADDR frame_args_address ();		/* i960-tdep.c */

/* This is the same except it should return 0 when
   it does not really know where the args are, rather than guessing.
   This value is not cached since it is only used infrequently.  */

#define	FRAME_ARGS_ADDRESS_CORRECT(fi)	(frame_args_address (fi, 1))

#define FRAME_LOCALS_ADDRESS(fi)	(fi)->frame

/* Set NUMARGS to the number of args passed to a frame.
   Can return -1, meaning no way to tell.  */

#define FRAME_NUM_ARGS(numargs, fi)	(numargs = -1)

/* Return number of bytes at start of arglist that are not really args.  */

#define FRAME_ARGS_SKIP 0

/* Produce the positions of the saved registers in a stack frame.  */

#define FRAME_FIND_SAVED_REGS(frame_info_addr, sr) \
	frame_find_saved_regs (frame_info_addr, &sr)
extern void frame_find_saved_regs();		/* See i960-tdep.c */


/* Print status when we get a random unexpected signal.  We have more
   kinds of signals than Unix does... */

#define	PRINT_RANDOM_SIGNAL(stop_signal) print_fault (stop_signal)

/* Things needed for making calls to functions in the inferior process */

/* Push an empty stack frame, to record the current ip, etc.
  
   Not (yet?) implemented for i960.  */

#define PUSH_DUMMY_FRAME	\
error("Function calls into the inferior process are not supported on the i960")

/* Discard from the stack the innermost frame, restoring all registers.  */

#define POP_FRAME \
	pop_frame ()


/* This sequence of words is the instructions
  
  	callx 0x00000000
  	fmark
 */

/* #define CALL_DUMMY { 0x86003000, 0x00000000, 0x66003e00 } */

/* #define CALL_DUMMY_START_OFFSET 0 *//* Start execution at beginning of dummy */

/* Indicate that we don't support calling inferior child functions.  */

#undef CALL_DUMMY

/* Insert the specified number of args and function address
   into a call sequence of the above form stored at 'dummyname'.
  
   Ignore arg count on i960.  */

/* #define FIX_CALL_DUMMY(dummyname, fun, nargs) *(((int *)dummyname)+1) = fun */

#undef FIX_CALL_DUMMY


/* Interface definitions for kernel debugger KDB */
/* (Not relevant to i960.) */
