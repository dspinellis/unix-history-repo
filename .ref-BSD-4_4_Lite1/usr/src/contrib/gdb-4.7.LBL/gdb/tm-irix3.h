/* Target machine description for SGI Iris under Irix, for GDB.
   Copyright 1990, 1991, 1992 Free Software Foundation, Inc.

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

#include "coff/sym.h"			/* Needed for PDR below. */
#include "coff/symconst.h"

#define TARGET_BYTE_ORDER BIG_ENDIAN

/* Floating point is IEEE compliant */
#define IEEE_FLOAT

/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */

/*#define NAMES_HAVE_UNDERSCORE*/

/* SGI's assembler doesn't grok dollar signs in identifiers.
   So we use dots instead.  This item must be coordinated with G++. */
#undef CPLUS_MARKER
#define CPLUS_MARKER '.'

/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 0

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  */

#define SKIP_PROLOGUE(pc)	pc = mips_skip_prologue(pc)

/* Immediately after a function call, return the saved pc.
   Can't always go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

#define SAVED_PC_AFTER_CALL(frame)	read_register(RA_REGNUM)

/* Are we currently handling a signal */

#define IN_SIGTRAMP(pc, name)	in_sigtramp(pc, name)

/* Address of end of stack space.  */

#define STACK_END_ADDR (0x7ffff000)

/* Stack grows downward.  */

#define INNER_THAN <

#define BREAKPOINT {0, 0x5, 0, 0xd}

/* Amount PC must be decremented by after a breakpoint.
   This is often the number of bytes in BREAKPOINT
   but not always.  */

#define DECR_PC_AFTER_BREAK 0

/* Nonzero if instruction at PC is a return instruction. "j ra" on mips. */

#define ABOUT_TO_RETURN(pc) (read_memory_integer (pc, 4) == 0x3e00008)

/* Return 1 if P points to an invalid floating point value. */

#define INVALID_FLOAT(p,l)	isa_NAN(p,l)

/* Say how long (all) registers are.  */

#define REGISTER_TYPE long

/* Number of machine registers */

#define NUM_REGS 71

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */

#define REGISTER_NAMES 	\
    {	"zero",	"at",	"v0",	"v1",	"a0",	"a1",	"a2",	"a3", \
	"t0",	"t1",	"t2",	"t3",	"t4",	"t5",	"t6",	"t7", \
	"s0",	"s1",	"s2",	"s3",	"s4",	"s5",	"s6",	"s7", \
	"t8",	"t9",	"k0",	"k1",	"gp",	"sp",	"fp",	"ra", \
	"f0",   "f1",   "f2",   "f3",   "f4",   "f5",   "f6",   "f7", \
	"f8",   "f9",   "f10",  "f11",  "f12",  "f13",  "f14",  "f15", \
	"f16",  "f17",  "f18",  "f19",  "f20",  "f21",  "f22",  "f23",\
	"f24",  "f25",  "f26",  "f27",  "f28",  "f29",  "f30",  "f31",\
	"pc",	"cause", "bad",	"hi",	"lo",	"fsr",  "fir" \
    }

/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */

#define A0_REGNUM 4		/* Loc of first arg during a subr call */
#define SP_REGNUM 29		/* Contains address of top of stack */
#define FP_REGNUM 30		/* Pseudo register that contains true address of executing stack frame */
#define RA_REGNUM 31		/* Contains return address value */
#define FP0_REGNUM 32		/* Floating point register 0 (single float) */
#define PC_REGNUM 64		/* Contains program counter */
#define PS_REGNUM 65		/* Contains processor status */
#define HI_REGNUM 67		/* Multiple/divide temp */
#define LO_REGNUM 68		/* ... */
#define FCRCS_REGNUM 69		/* FP control/status */
#define FCRIR_REGNUM 70		/* FP implementation/revision */

/* Define DO_REGISTERS_INFO() to do machine-specific formatting
   of register dumps. */

#define DO_REGISTERS_INFO(_regnum, fp) mips_do_registers_info(_regnum, fp)

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */
#define REGISTER_BYTES (NUM_REGS*4)

/* Index within `registers' of the first byte of the space for
   register N.  */

#define REGISTER_BYTE(N) ((N) * 4)

/* Number of bytes of storage in the actual machine representation
   for register N.  On mips, all regs are 4 bytes.  */

#define REGISTER_RAW_SIZE(N) 4

/* Number of bytes of storage in the program's representation
   for register N.  On mips, all regs are 4 bytes.  */

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

#define STORE_STRUCT_RETURN(addr, sp) \
  { sp = push_word(sp, addr);}

/* Extract from an array REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF.  XXX floats */

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
  bcopy (REGBUF+REGISTER_BYTE (TYPE_CODE (TYPE) == TYPE_CODE_FLT ? FP0_REGNUM : 2), VALBUF, TYPE_LENGTH (TYPE))

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format.  */

#define STORE_RETURN_VALUE(TYPE,VALBUF) \
  write_register_bytes (REGISTER_BYTE (TYPE_CODE (TYPE) == TYPE_CODE_FLT ? FP0_REGNUM : 2), VALBUF, TYPE_LENGTH (TYPE))

/* Extract from an array REGBUF containing the (raw) register state
   the address in which a function should return its structure value,
   as a CORE_ADDR (or an expression that can be used as one).  */

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) (*(int *)(REGBUF+16))

/* Structures are returned by ref in extra arg0 */
#define USE_STRUCT_CONVENTION(gcc_p, type)	1


/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer. */

#define FRAME_CHAIN(thisframe) (FRAME_ADDR)mips_frame_chain(thisframe)

/* Define other aspects of the stack frame.  */


/* A macro that tells us whether the function invocation represented
   by FI does not have a frame on the stack associated with it.  If it
   does not, FRAMELESS is set to 1, else 0.  */
/* We handle this differently for mips, and maybe we should not */

#define FRAMELESS_FUNCTION_INVOCATION(FI, FRAMELESS)  {(FRAMELESS) = 0;}

/* Saved Pc.  */

#define FRAME_SAVED_PC(FRAME)	(mips_frame_saved_pc(FRAME))

#define FRAME_ARGS_ADDRESS(fi)	(fi)->frame

#define FRAME_LOCALS_ADDRESS(fi) (fi)->frame

/* Return number of args passed to a frame.
   Can return -1, meaning no way to tell.  */

#define FRAME_NUM_ARGS(num, fi)	(num = mips_frame_num_args(fi))

/* Return number of bytes at start of arglist that are not really args.  */

#define FRAME_ARGS_SKIP 0

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.  */

#define FRAME_FIND_SAVED_REGS(frame_info, frame_saved_regs) ( \
  (frame_saved_regs) = *(frame_info)->saved_regs, \
  (frame_saved_regs).regs[SP_REGNUM] = (frame_info)->frame)


/* Things needed for making the inferior call functions.  */

/* Stack has strict alignment. However, use PUSH_ARGUMENTS
   to take care of it. */
/*#define STACK_ALIGN(addr)	(((addr)+3)&~3)*/

#define PUSH_ARGUMENTS(nargs, args, sp, struct_return, struct_addr) \
    sp = mips_push_arguments(nargs, args, sp, struct_return, struct_addr)

/* Push an empty stack frame, to record the current PC, etc.  */

#define PUSH_DUMMY_FRAME 	mips_push_dummy_frame()

/* Discard from the stack the innermost frame, restoring all registers.  */

#define POP_FRAME		mips_pop_frame()

#define MK_OP(op,rs,rt,offset) (((op)<<26)|((rs)<<21)|((rt)<<16)|(offset))
#define CALL_DUMMY_SIZE (16*4)
#define Dest_Reg 2
#define CALL_DUMMY {\
 MK_OP(0,RA_REGNUM,0,8),	/* jr $ra # Fake ABOUT_TO_RETURN ...*/\
 0,				/* nop 	  #  ... to stop raw backtrace*/\
 0x27bd0000,			/* addu	sp,?0 # Pseudo prologue */\
/* Start here: */\
 MK_OP(061,SP_REGNUM,12,0),	/* lwc1 $f12,0(sp) # Reload first 4 args*/\
 MK_OP(061,SP_REGNUM,13,4),	/* lwc1 $f13,4(sp) */\
 MK_OP(061,SP_REGNUM,14,8),	/* lwc1 $f14,8(sp) */\
 MK_OP(061,SP_REGNUM,15,12),	/* lwc1 $f15,12(sp) */\
 MK_OP(043,SP_REGNUM,4,0),	/* lw $r4,0(sp) # Re-load FP regs*/\
 MK_OP(043,SP_REGNUM,5,4),	/* lw $r5,4(sp) */\
 MK_OP(043,SP_REGNUM,6,8),	/* lw $r6,8(sp) */\
 MK_OP(043,SP_REGNUM,7,12),	/* lw $r7,12(sp) */\
 (017<<26)| (Dest_Reg << 16),	/* lui $r31,<target upper 16 bits>*/\
 MK_OP(13,Dest_Reg,Dest_Reg,0),	/* ori $r31,$r31,<lower 16 bits>*/ \
 (Dest_Reg<<21) | (31<<11) | 9,	/* jalr $r31 */\
 MK_OP(043,SP_REGNUM,7,12),	/* lw $r7,12(sp) */\
 0x5000d,			/* bpt */\
}

#define CALL_DUMMY_START_OFFSET 12

/* Insert the specified number of args and function address
   into a call sequence of the above form stored at DUMMYNAME.  */

#define FIX_CALL_DUMMY(dummyname, start_sp, fun, nargs,	args, rettype, gcc_p)\
  (((int*)dummyname)[11] |= (((unsigned long)(fun)) >> 16), \
   ((int*)dummyname)[12] |= (unsigned short)(fun))

/* Specific information about a procedure.
   This overlays the MIPS's PDR records, 
   mipsread.c (ab)uses this to save memory */

typedef struct mips_extra_func_info {
	long	numargs;	/* number of args to procedure (was iopt) */
	PDR	pdr;		/* Procedure descriptor record */
} *mips_extra_func_info_t;

#define EXTRA_FRAME_INFO \
  mips_extra_func_info_t proc_desc; \
  int num_args;\
  struct frame_saved_regs *saved_regs;

#define INIT_EXTRA_FRAME_INFO(fromleaf, fci) init_extra_frame_info(fci)

/* Special symbol found in blocks associated with routines.  We can hang
   mips_extra_func_info_t's off of this.  */

#define MIPS_EFI_SYMBOL_NAME "__GDB_EFI_INFO__"
