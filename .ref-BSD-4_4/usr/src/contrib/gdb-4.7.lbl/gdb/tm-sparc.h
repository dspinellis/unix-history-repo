/* Target machine sub-parameters for SPARC, for GDB, the GNU debugger.
   This is included by other tm-*.h files to define SPARC cpu-related info.
   Copyright 1986, 1987, 1989, 1991, 1992 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@mcc.com)

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

/* Floating point is IEEE compatible.  */
#define IEEE_FLOAT

/* When passing a structure to a function, Sun cc passes the address
   in a register, not the structure itself.  It (under SunOS4) creates
   two symbols, so we get a LOC_ARG saying the address is on the stack
   (a lie, and a serious one since we don't know which register to
   use), and a LOC_REGISTER saying that the struct is in a register
   (sort of a lie, but fixable with REG_STRUCT_HAS_ADDR).  Gcc version
   two (as of 1.92) behaves like sun cc.  REG_STRUCT_HAS_ADDR is smart
   enough to distinguish between Sun cc, gcc version 1 and gcc version 2.

   This still doesn't work if the argument is not one passed in a
   register (i.e. it's the 7th or later argument).  */
#define REG_STRUCT_HAS_ADDR(gcc_p) (gcc_p != 1)
#define STRUCT_ARG_SYM_GARBAGE(gcc_p) (gcc_p != 1)

/* If Pcc says that a parameter is a short, it's a short.  This is
   because the parameter does get passed in in a register as an int,
   but pcc puts it onto the stack frame as a short (not nailing
   whatever else might be there.  I'm not sure that I consider this
   swift.  Sigh.)

   No, don't do this.  The problem here is that pcc says that the
   argument is in the upper half of the word reserved on the stack,
   but puts it in the lower half.  */
/* #define BELIEVE_PCC_PROMOTION 1 */
/* OK, I've added code to dbxread.c to deal with this case.  */
#define BELIEVE_PCC_PROMOTION_TYPE

/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 0

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  SKIP_PROLOGUE_FRAMELESS_P advances
   the PC past some of the prologue, but stops as soon as it
   knows that the function has a frame.  Its result is equal
   to its input PC if the function is frameless, unequal otherwise.  */

#define SKIP_PROLOGUE(pc) \
  { pc = skip_prologue (pc, 0); }
#define SKIP_PROLOGUE_FRAMELESS_P(pc) \
  { pc = skip_prologue (pc, 1); }
extern CORE_ADDR skip_prologue ();

/* Immediately after a function call, return the saved pc.
   Can't go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

/* On the Sun 4 under SunOS, the compile will leave a fake insn which
   encodes the structure size being returned.  If we detect such
   a fake insn, step past it.  */

#define PC_ADJUST(pc) sparc_pc_adjust(pc)
extern CORE_ADDR sparc_pc_adjust();

#define SAVED_PC_AFTER_CALL(frame) PC_ADJUST (read_register (RP_REGNUM))

/* Stack grows downward.  */

#define INNER_THAN <

/* Stack has strict alignment.  */

#define STACK_ALIGN(ADDR) (((ADDR)+7)&-8)

/* Sequence of bytes for breakpoint instruction.  */

#define BREAKPOINT {0x91, 0xd0, 0x20, 0x01}

/* Amount PC must be decremented by after a breakpoint.
   This is often the number of bytes in BREAKPOINT
   but not always.  */

#define DECR_PC_AFTER_BREAK 0

/* Nonzero if instruction at PC is a return instruction.  */
/* For SPARC, this is either a "jmpl %o7+8,%g0" or "jmpl %i7+8,%g0".

   Note: this does not work for functions returning structures under SunOS.  */
#define ABOUT_TO_RETURN(pc) \
  ((read_memory_integer (pc, 4)|0x00040000) == 0x81c7e008)

/* Return 1 if P points to an invalid floating point value.  */

#define INVALID_FLOAT(p, len) 0   /* Just a first guess; not checked */

/* Say how long (ordinary) registers are.  */

#define REGISTER_TYPE long

/* Number of machine registers */

#define NUM_REGS 72

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */

#define REGISTER_NAMES  \
{ "g0", "g1", "g2", "g3", "g4", "g5", "g6", "g7",	\
  "o0", "o1", "o2", "o3", "o4", "o5", "sp", "o7",	\
  "l0", "l1", "l2", "l3", "l4", "l5", "l6", "l7",	\
  "i0", "i1", "i2", "i3", "i4", "i5", "fp", "i7",	\
								\
  "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7",	\
  "f8", "f9", "f10", "f11", "f12", "f13", "f14", "f15",	\
  "f16", "f17", "f18", "f19", "f20", "f21", "f22", "f23",	\
  "f24", "f25", "f26", "f27", "f28", "f29", "f30", "f31",	\
                                                                \
  "y", "psr", "wim", "tbr", "pc", "npc", "fpsr", "cpsr" }

/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */

#define	G0_REGNUM 0             /* %g0 */
#define	G1_REGNUM 1		/* %g1 */
#define O0_REGNUM 8		/* %o0 */
#define	SP_REGNUM 14		/* Contains address of top of stack, \
				   which is also the bottom of the frame.  */
#define	RP_REGNUM 15		/* Contains return address value, *before* \
				   any windows get switched.  */
#define	O7_REGNUM 15		/* Last local reg not saved on stack frame */
#define	L0_REGNUM 16		/* First local reg that's saved on stack frame
				   rather than in machine registers */
#define	I0_REGNUM 24		/* %i0 */
#define	FP_REGNUM 30		/* Contains address of executing stack frame */
#define	I7_REGNUM 31		/* Last local reg saved on stack frame */
#define	FP0_REGNUM 32		/* Floating point register 0 */
#define	Y_REGNUM 64		/* Temp register for multiplication, etc.  */
#define	PS_REGNUM 65		/* Contains processor status */
#define	WIM_REGNUM 66		/* Window Invalid Mask (not really supported) */
#define	TBR_REGNUM 67		/* Trap Base Register (not really supported) */
#define	PC_REGNUM 68		/* Contains program counter */
#define	NPC_REGNUM 69           /* Contains next PC */
#define	FPS_REGNUM 70		/* Floating point status register */
#define	CPS_REGNUM 71		/* Coprocessor status register */

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */
#define REGISTER_BYTES (32*4+32*4+8*4)

/* Index within `registers' of the first byte of the space for
   register N.  */
/* ?? */
#define REGISTER_BYTE(N)  ((N)*4)

/* The SPARC processor has register windows.  */

#define HAVE_REGISTER_WINDOWS

/* Is this register part of the register window system?  A yes answer
   implies that 1) The name of this register will not be the same in
   other frames, and 2) This register is automatically "saved" (out
   registers shifting into ins counts) upon subroutine calls and thus
   there is no need to search more than one stack frame for it. */

#define REGISTER_IN_WINDOW_P(regnum)	\
  ((regnum) >= 8 && (regnum) < 32)

/* Number of bytes of storage in the actual machine representation
   for register N.  */

/* On the SPARC, all regs are 4 bytes.  */

#define REGISTER_RAW_SIZE(N) (4)

/* Number of bytes of storage in the program's representation
   for register N.  */

/* On the SPARC, all regs are 4 bytes.  */

#define REGISTER_VIRTUAL_SIZE(N) (4)

/* Largest value REGISTER_RAW_SIZE can have.  */

#define MAX_REGISTER_RAW_SIZE 8

/* Largest value REGISTER_VIRTUAL_SIZE can have.  */

#define MAX_REGISTER_VIRTUAL_SIZE 8

/* Nonzero if register N requires conversion
   from raw format to virtual format.  */

#define REGISTER_CONVERTIBLE(N) (0)

/* Convert data from raw format for register REGNUM
   to virtual format for register REGNUM.  */

#define REGISTER_CONVERT_TO_VIRTUAL(REGNUM,FROM,TO) \
{ memcpy ((TO), (FROM), 4); }

/* Convert data from virtual format for register REGNUM
   to raw format for register REGNUM.  */

#define REGISTER_CONVERT_TO_RAW(REGNUM,FROM,TO)	\
{ memcpy ((TO), (FROM), 4); }

/* Return the GDB type object for the "standard" data type
   of data in register N.  */

#define REGISTER_VIRTUAL_TYPE(N) \
 ((N) < 32 ? builtin_type_int : (N) < 64 ? builtin_type_float : \
  builtin_type_int)

/* Writing to %g0 is a noop (not an error or exception or anything like
   that, however).  */

#define CANNOT_STORE_REGISTER(regno) ((regno) == G0_REGNUM)

/* Store the address of the place in which to copy the structure the
   subroutine will return.  This is called from call_function. */

#define STORE_STRUCT_RETURN(ADDR, SP) \
  { target_write_memory ((SP)+(16*4), (char *)&(ADDR), 4); }

/* Extract from an array REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF.  */

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF)	      \
  {      	       	       	       	       	       	       	           \
    if (TYPE_CODE (TYPE) == TYPE_CODE_FLT)		       		   \
      {							       		   \
	memcpy ((VALBUF), ((int *)(REGBUF))+FP0_REGNUM, TYPE_LENGTH(TYPE));\
      }							       		   \
    else						       		   \
      memcpy ((VALBUF),						   	   \
	      (char *)(REGBUF) + 4 * 8 +				   \
	      (TYPE_LENGTH(TYPE) >= 4 ? 0 : 4 - TYPE_LENGTH(TYPE)),	   \
	      TYPE_LENGTH(TYPE));					   \
  }

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format.  */
/* On sparc, values are returned in register %o0.  */
#define STORE_RETURN_VALUE(TYPE,VALBUF) \
  {    	       	       	       	       	       	       	       	       	     \
    if (TYPE_CODE (TYPE) == TYPE_CODE_FLT)				     \
      /* Floating-point values are returned in the register pair */          \
      /* formed by %f0 and %f1 (doubles are, anyway).  */                    \
      write_register_bytes (REGISTER_BYTE (FP0_REGNUM), (VALBUF),	     \
			    TYPE_LENGTH (TYPE));			     \
    else								     \
      /* Other values are returned in register %o0.  */                      \
      write_register_bytes (REGISTER_BYTE (O0_REGNUM), (VALBUF),	     \
			    TYPE_LENGTH (TYPE));  \
  }

/* Extract from an array REGBUF containing the (raw) register state
   the address in which a function should return its structure value,
   as a CORE_ADDR (or an expression that can be used as one).  */

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) \
  (sparc_extract_struct_value_address (REGBUF))

extern CORE_ADDR
sparc_extract_struct_value_address PARAMS ((char [REGISTER_BYTES]));


/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer. */

/* In the case of the Sun 4, the frame-chain's nominal address
   is held in the frame pointer register.

   On the Sun4, the frame (in %fp) is %sp for the previous frame.
   From the previous frame's %sp, we can find the previous frame's
   %fp: it is in the save area just above the previous frame's %sp.

   If we are setting up an arbitrary frame, we'll need to know where
   it ends.  Hence the following.  This part of the frame cache
   structure should be checked before it is assumed that this frame's
   bottom is in the stack pointer.

   If there isn't a frame below this one, the bottom of this frame is
   in the stack pointer.

   If there is a frame below this one, and the frame pointers are
   identical, it's a leaf frame and the bottoms are the same also.

   Otherwise the bottom of this frame is the top of the next frame.  */

#define EXTRA_FRAME_INFO	FRAME_ADDR bottom;
#define INIT_EXTRA_FRAME_INFO(fromleaf, fci)  \
  (fci)->bottom =					\
   ((fci)->next ?					\
    ((fci)->frame == (fci)->next_frame ?		\
     (fci)->next->bottom : (fci)->next->frame) :	\
    read_register (SP_REGNUM));

#define FRAME_CHAIN(thisframe) (sparc_frame_chain (thisframe))
CORE_ADDR sparc_frame_chain ();

/* Define other aspects of the stack frame.  */

/* A macro that tells us whether the function invocation represented
   by FI does not have a frame on the stack associated with it.  If it
   does not, FRAMELESS is set to 1, else 0.  */
#define FRAMELESS_FUNCTION_INVOCATION(FI, FRAMELESS) \
  (FRAMELESS) = frameless_look_for_prologue(FI)

/* Where is the PC for a specific frame */

#define FRAME_SAVED_PC(FRAME) frame_saved_pc (FRAME)
CORE_ADDR frame_saved_pc ();

/* If the argument is on the stack, it will be here.  */
#define FRAME_ARGS_ADDRESS(fi) ((fi)->frame)

#define FRAME_STRUCT_ARGS_ADDRESS(fi) ((fi)->frame)

#define FRAME_LOCALS_ADDRESS(fi) ((fi)->frame)

/* Set VAL to the number of args passed to frame described by FI.
   Can set VAL to -1, meaning no way to tell.  */

/* We can't tell how many args there are
   now that the C compiler delays popping them.  */
#define FRAME_NUM_ARGS(val,fi) (val = -1)

/* Return number of bytes at start of arglist that are not really args.  */

#define FRAME_ARGS_SKIP 68

/*
 * The LBL version of gdb uses a modified approach for calling functions
 * in the inferior.  Vanilla gdb saves the inferior's cpu on the inferior's
 * stack.  This is silly since it easier to save it in the debugger.
 */
#define NEW_CALL_FUNCTION
#define FRAME_FIND_SAVED_REGS(fi, sr) \
	frame_find_saved_regs(fi, &(sr))
#define POP_FRAME sparc_pop_frame()

/* Sparc has no reliable single step ptrace call */

#define NO_SINGLE_STEP 1
extern void single_step ();

/* We need two arguments (in general) to the "info frame" command.
   Note that the definition of this macro implies that there exists a
   function "setup_arbitrary_frame" in sparc-tdep.c */

#define FRAME_SPECIFICATION_DYADIC

/* To print every pair of float registers as a double, we use this hook.  */

#define	PRINT_REGISTER_HOOK(regno)	\
  if (((regno) >= FP0_REGNUM)		\
   && ((regno) <  FP0_REGNUM + 32)	\
   && (0 == (regno & 1))) {		\
    char doublereg[8];		/* two float regs */	\
    if (!read_relative_register_raw_bytes (i  , doublereg  )	\
     && !read_relative_register_raw_bytes (i+1, doublereg+4)) {	\
      printf("\t");			\
      print_floating (doublereg, builtin_type_double, stdout);	\
    }					\
  }

/* Optimization for storing registers to the inferior.  The hook
   DO_DEFERRED_STORES
   actually executes any deferred stores.  It is called any time
   we are going to proceed the child, or read its registers.
   The hook CLEAR_DEFERRED_STORES is called when we want to throw
   away the inferior process, e.g. when it dies or we kill it.
   FIXME, this does not handle remote debugging cleanly.  */

extern int deferred_stores;
#define	DO_DEFERRED_STORES	\
  if (deferred_stores)		\
    target_store_registers (-2);
#define	CLEAR_DEFERRED_STORES	\
  deferred_stores = 0;

/*
 * Where to find function arguments that don't have dbx symbols.
 * (i.e., this allows us to see function args in a backtrace when
 * no debugging symbols are present)
 */
#define NAMELESS_ARG(fi, n, v) \
	target_read_memory(32 + (fi)->bottom + 4 * n, (char *)&v, 4)

