/* Definitions to make GDB target for an ARM under RISCiX (4.3bsd).
   Copyright (C) 1986, 1987, 1989, 1991 Free Software Foundation, Inc.

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

#define TARGET_BYTE_ORDER LITTLE_ENDIAN

/* IEEE format floating point */

#define IEEE_FLOAT

/* I provide my own xfer_core_file to cope with shared libraries */

#define XFER_CORE_FILE

/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */

#define NAMES_HAVE_UNDERSCORE

/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 0

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  */

#define SKIP_PROLOGUE(pc) pc = skip_prologue(pc)

/* Immediately after a function call, return the saved pc.
   Can't always go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

#define SAVED_PC_AFTER_CALL(frame) (read_register (LR_REGNUM) & 0x03fffffc)

/* I don't know the real values for these.  */
#define TARGET_UPAGES UPAGES
#define TARGET_NBPG NBPG

/* Address of end of stack space.  */

#define STACK_END_ADDR (0x01000000 - (TARGET_UPAGES * TARGET_NBPG))

/* Stack grows downward.  */

#define INNER_THAN <

/* Sequence of bytes for breakpoint instruction.  */

#define BREAKPOINT {0x00,0x00,0x18,0xef} /* BKPT_SWI from <sys/ptrace.h> */

/* Amount PC must be decremented by after a breakpoint.
   This is often the number of bytes in BREAKPOINT
   but not always.  */

#define DECR_PC_AFTER_BREAK 0

/* Nonzero if instruction at PC is a return instruction.  */

#define ABOUT_TO_RETURN(pc) \
      ((read_memory_integer(pc, 4) & 0x0fffffff == 0x01b0f00e) || \
       (read_memory_integer(pc, 4) & 0x0ffff800 == 0x09eba800))

/* Return 1 if P points to an invalid floating point value.
   LEN is the length in bytes.  */

#define INVALID_FLOAT(p, len) 0

/* code to execute to print interesting information about the
 * floating point processor (if any)
 * No need to define if there is nothing to do.
 */
#define FLOAT_INFO { arm_float_info (); }

/* Say how long (ordinary) registers are.  */

#define REGISTER_TYPE long

/* Number of machine registers */

/* Note: I make a fake copy of the pc in register 25 (calling it ps) so
   that I can clear the status bits from pc (register 15) */

#define NUM_REGS 26

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */

#define REGISTER_NAMES \
      { "a1", "a2", "a3", "a4",					\
	"v1", "v2", "v3", "v4", "v5", "v6",			\
        "sl", "fp", "ip", "sp", "lr", "pc",			\
        "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "fps", "ps" }

/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */

#define AP_REGNUM 11
#define FP_REGNUM 11		/* Contains address of executing stack frame */
#define SP_REGNUM 13		/* Contains address of top of stack */
#define LR_REGNUM 14		/* address to return to from a function call */
#define PC_REGNUM 15		/* Contains program counter */
#define F0_REGNUM 16		/* first floating point register */
#define FPS_REGNUM 24		/* floating point status register */
#define PS_REGNUM 25		/* Contains processor status */


/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */
#define REGISTER_BYTES (16*4 + 12*8 + 4 + 4)

/* Index within `registers' of the first byte of the space for
   register N.  */

#define REGISTER_BYTE(N) (((N) < F0_REGNUM) ? (N)*4 : \
			  (((N) < PS_REGNUM) ? 16*4 + ((N) - 16)*12 : \
			   16*4 + 8*12 + ((N) - FPS_REGNUM) * 4))

/* Number of bytes of storage in the actual machine representation
   for register N.  On the vax, all regs are 4 bytes.  */

#define REGISTER_RAW_SIZE(N) (((N) < F0_REGNUM || (N) >= FPS_REGNUM) ? 4 : 12)

/* Number of bytes of storage in the program's representation
   for register N.  On the vax, all regs are 4 bytes.  */

#define REGISTER_VIRTUAL_SIZE(N) (((N) < F0_REGNUM || (N) >= FPS_REGNUM) ? 4 : 8)

/* Largest value REGISTER_RAW_SIZE can have.  */

#define MAX_REGISTER_RAW_SIZE 12

/* Largest value REGISTER_VIRTUAL_SIZE can have.  */

#define MAX_REGISTER_VIRTUAL_SIZE 8

/* Nonzero if register N requires conversion
   from raw format to virtual format.  */

#define REGISTER_CONVERTIBLE(N) ((unsigned)(N) - F0_REGNUM < 8)

/* Convert data from raw format for register REGNUM
   to virtual format for register REGNUM.  */

#define REGISTER_CONVERT_TO_VIRTUAL(REGNUM,FROM,TO)	\
  if (REGISTER_CONVERTIBLE(REGNUM))					\
      convert_from_extended((FROM), (TO));				\
  else									\
      bcopy ((FROM), (TO), 4);

/* Convert data from virtual format for register REGNUM
   to raw format for register REGNUM.  */

#define REGISTER_CONVERT_TO_RAW(REGNUM,FROM,TO)	\
  if (REGISTER_CONVERTIBLE(REGNUM)) 			\
    convert_to_extended((FROM), (TO)); 		\
  else							\
    bcopy ((FROM), (TO), 4);

/* Return the GDB type object for the "standard" data type
   of data in register N.  */

#define REGISTER_VIRTUAL_TYPE(N) \
 (((unsigned)(N) - F0_REGNUM) < 8 ? builtin_type_double : builtin_type_int)

/* The system C compiler uses a similar structure return convention to gcc */

#define USE_STRUCT_CONVENTION(gcc_p, type) (TYPE_LENGTH (type) > 4)

/* Store the address of the place in which to copy the structure the
   subroutine will return.  This is called from call_function. */

#define STORE_STRUCT_RETURN(ADDR, SP) \
  { write_register (0, (ADDR)); }

/* Extract from an array REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF.  */

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
  if (TYPE_CODE (TYPE) == TYPE_CODE_FLT)				\
    convert_from_extended(REGBUF + REGISTER_BYTE (F0_REGNUM), VALBUF);	\
  else									\
    bcopy (REGBUF, VALBUF, TYPE_LENGTH (TYPE))

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format.  */

#define STORE_RETURN_VALUE(TYPE,VALBUF) \
  if (TYPE_CODE (TYPE) == TYPE_CODE_FLT) {				\
    char _buf[MAX_REGISTER_RAW_SIZE];					\
    convert_to_extended(VALBUF, _buf);					\
    write_register_bytes (REGISTER_BYTE (F0_REGNUM), _buf, MAX_REGISTER_RAW_SIZE); \
  } else								\
    write_register_bytes (0, VALBUF, TYPE_LENGTH (TYPE))

/* Extract from an array REGBUF containing the (raw) register state
   the address in which a function should return its structure value,
   as a CORE_ADDR (or an expression that can be used as one).  */

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) (*(int *)(REGBUF))

/* Specify that for the native compiler variables for a particular
   lexical context are listed after the beginning LBRAC instead of
   before in the executables list of symbols.  */
#define VARIABLES_INSIDE_BLOCK(desc, gcc_p) (!(gcc_p))


/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer.

   However, if FRAME_CHAIN_VALID returns zero,
   it means the given frame is the outermost one and has no caller.  */

/* In the case of the ARM, the frame's nominal address is the FP value,
   and 12 bytes before comes the saved previous FP value as a 4-byte word.  */

#define FRAME_CHAIN(thisframe)  \
  ((thisframe)->pc >= first_object_file_end ? \
   read_memory_integer ((thisframe)->frame - 12, 4) :\
   0)

#define FRAME_CHAIN_VALID(chain, thisframe) \
  (chain != 0 && (FRAME_SAVED_PC (thisframe) >= first_object_file_end))

/* Define other aspects of the stack frame.  */

/* A macro that tells us whether the function invocation represented
   by FI does not have a frame on the stack associated with it.  If it
   does not, FRAMELESS is set to 1, else 0.  */
#define FRAMELESS_FUNCTION_INVOCATION(FI, FRAMELESS) \
{							\
  CORE_ADDR func_start, after_prologue;			\
  func_start = (get_pc_function_start ((FI)->pc) +	\
		FUNCTION_START_OFFSET);			\
  after_prologue = func_start;				\
  SKIP_PROLOGUE (after_prologue);			\
  (FRAMELESS) = (after_prologue == func_start);		\
}

/* Saved Pc.  */

#define FRAME_SAVED_PC(FRAME) \
  (read_memory_integer ((FRAME)->frame - 4, 4) & 0x03fffffc)

#define FRAME_ARGS_ADDRESS(fi) (fi->frame)

#define FRAME_LOCALS_ADDRESS(fi) ((fi)->frame)

/* Return number of args passed to a frame.
   Can return -1, meaning no way to tell.  */

#define FRAME_NUM_ARGS(numargs, fi) (numargs = -1)

/* Return number of bytes at start of arglist that are not really args.  */

#define FRAME_ARGS_SKIP 0

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.  */

#define FRAME_FIND_SAVED_REGS(frame_info, frame_saved_regs) \
{							      			\
    register int regnum;							\
    register int frame;								\
    register int next_addr;							\
    register int return_data_save;						\
    register int saved_register_mask;						\
    bzero (&frame_saved_regs, sizeof frame_saved_regs);				\
    frame = (frame_info)->frame;						\
    return_data_save = read_memory_integer(frame, 4) & 0x03fffffc - 12;		\
    saved_register_mask =							\
	read_memory_integer(return_data_save, 4);				\
    next_addr = frame - 12;							\
    for (regnum = 4; regnum < 10; regnum++)					\
	if (saved_register_mask & (1<<regnum)) {				\
	    next_addr -= 4;							\
	    (frame_saved_regs).regs[regnum] = next_addr;			\
	}									\
    if (read_memory_integer(return_data_save + 4, 4) == 0xed6d7103) {		\
	next_addr -= 12;							\
	(frame_saved_regs).regs[F0_REGNUM + 7] = next_addr;			\
    }										\
    if (read_memory_integer(return_data_save + 8, 4) == 0xed6d6103) {		\
	next_addr -= 12;							\
	(frame_saved_regs).regs[F0_REGNUM + 6] = next_addr;			\
    }										\
    if (read_memory_integer(return_data_save + 12, 4) == 0xed6d5103) {		\
	next_addr -= 12;							\
	(frame_saved_regs).regs[F0_REGNUM + 5] = next_addr;			\
    }										\
    if (read_memory_integer(return_data_save + 16, 4) == 0xed6d4103) {		\
	next_addr -= 12;							\
	(frame_saved_regs).regs[F0_REGNUM + 4] = next_addr;			\
    }										\
    (frame_saved_regs).regs[SP_REGNUM] = next_addr;				\
    (frame_saved_regs).regs[PC_REGNUM] = frame - 4;				\
    (frame_saved_regs).regs[PS_REGNUM] = frame - 4;				\
    (frame_saved_regs).regs[FP_REGNUM] = frame - 12;				\
}

/* Things needed for making the inferior call functions.  */

/* Push an empty stack frame, to record the current PC, etc.  */

#define PUSH_DUMMY_FRAME \
{								\
    register CORE_ADDR sp = read_register (SP_REGNUM);		\
    register int regnum;					\
    /* opcode for ldmdb fp,{v1-v6,fp,ip,lr,pc}^ */		\
    sp = push_word(sp, 0xe92dbf0); /* dummy return_data_save ins */ \
    /* push a pointer to the dummy instruction minus 12 */	\
    sp = push_word(sp, read_register (SP_REGNUM) - 16);		\
    sp = push_word(sp, read_register (PS_REGNUM));		\
    sp = push_word(sp, read_register (SP_REGNUM));		\
    sp = push_word(sp, read_register (FP_REGNUM));		\
    for (regnum = 9; regnum >= 4; regnum --)			\
	sp = push_word(sp, read_register (regnum));		\
    write_register (FP_REGNUM, read_register (SP_REGNUM) - 8);	\
    write_register (SP_REGNUM, sp); }

/* Discard from the stack the innermost frame, restoring all registers.  */

#define POP_FRAME \
{									\
    register CORE_ADDR fp = read_register (FP_REGNUM);			\
    register unsigned long return_data_save =				\
	read_memory_integer ( (read_memory_integer (fp, 4) &		\
			       0x03fffffc)  - 12, 4);			\
    register int regnum;						\
    write_register (PS_REGNUM, read_memory_integer (fp - 4, 4));	\
    write_register (PC_REGNUM, read_register (PS_REGNUM) & 0x03fffffc);	\
    write_register (SP_REGNUM, read_memory_integer (fp - 8, 4));	\
    write_register (FP_REGNUM, read_memory_integer (fp - 12, 4));	\
    fp -= 12;								\
    for (regnum = 9; regnum >= 4; regnum--)				\
	if (return_data_save & (1<<regnum)) {				\
	    fp -= 4;							\
	    write_register (regnum, read_memory_integer(fp, 4));	\
	}								\
    flush_cached_frames ();						\
    set_current_frame (create_new_frame (read_register (FP_REGNUM),	\
					 read_pc ()));			\
}

/* This sequence of words is the instructions

     ldmia	sp!,{a1-a4}
     mov 	lk,pc
     bl		*+8
     swi	bkpt_swi

   Note this is 16 bytes.  */

#define CALL_DUMMY {0xe8bd000f, 0xe1a0e00f, 0xeb000000, 0xef180000}

#define CALL_DUMMY_START_OFFSET 0  /* Start execution at beginning of dummy */

/* Insert the specified number of args and function address
   into a call sequence of the above form stored at DUMMYNAME.  */

#define FIX_CALL_DUMMY(dummyname, pc, fun, nargs, args, type, gcc_p) \
{										\
    register enum type_code code = TYPE_CODE (type);				\
    register nargs_in_registers, struct_return = 0;				\
    /* fix the load-arguments mask to move the first 4 or less arguments	\
       into a1-a4 but make sure the structure return address in a1 is		\
       not disturbed if the function is returning a structure */		\
    if ((code == TYPE_CODE_STRUCT ||						\
	 code == TYPE_CODE_UNION ||						\
	 code == TYPE_CODE_ARRAY) &&						\
	TYPE_LENGTH (type) > 4) {						\
	nargs_in_registers = min(nargs + 1, 4);					\
	struct_return = 1;							\
    } else									\
	nargs_in_registers = min(nargs, 4);					\
    *(char *) dummyname = (1 << nargs_in_registers) - 1 - struct_return;	\
    *(int *)((char *) dummyname + 8) =						\
	(((fun - (pc + 16)) / 4) & 0x00ffffff) | 0xeb000000; }
