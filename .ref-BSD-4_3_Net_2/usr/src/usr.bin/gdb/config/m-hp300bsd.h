/*-
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1991 by Donn Seeley at UUNET Technologies, Inc.
 * Modified 1990 by Van Jacobson at Lawrence Berkeley Laboratory.
 *
 *	@(#)m-hp300bsd.h	6.8 (Berkeley) 5/8/91
 */

/* Parameters for execution on a Hewlett-Packard 9000/300, running bsd.
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

/* Describe the endian nature of this machine.  */
#define BITS_BIG_ENDIAN
#define BYTES_BIG_ENDIAN
#define WORDS_BIG_ENDIAN

/*
 * Configuration file for HP9000/300 series machine running
 * University of Utah's 4.3bsd port.  This is NOT for HP-UX.
 * Problems to hpbsd-bugs@cs.utah.edu
 */

#ifndef hp300
#define hp300
#endif

/* Library stuff: POSIX tty (not supported yet), V7 tty (sigh), vprintf.  */

#define	HAVE_TERMIOS	1
#define	USE_OLD_TTY	1
#define	HAVE_VPRINTF	1

/* We support local and remote kernel debugging.  */

#define	KERNELDEBUG	1

/* Watch out for NaNs */

#define IEEE_FLOAT

/* Get rid of any system-imposed stack limit if possible.  */

#define SET_STACK_LIMIT_HUGE

/* Define this if the C compiler puts an underscore at the front
   of external names before giving them to the linker.  */

#define NAMES_HAVE_UNDERSCORE

/* Debugger information will be in DBX format.  */

#define READ_DBX_FORMAT

/* Don't implement the attach and detach commands.  */

/* #define ATTACH_DETACH */

/* Offset from address of function to start of its code.
   Zero on most machines.  */

#define FUNCTION_START_OFFSET 0

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  */

#define SKIP_PROLOGUE(pc)   \
{ register int op = read_memory_integer (pc, 2);	\
  if (op == 0047126)				\
    pc += 4;   /* Skip link #word */			\
  else if (op == 0044016)			\
    pc += 6;   /* Skip link #long */			\
}

/* Immediately after a function call, return the saved pc.
   Can't go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions.  */

#define SAVED_PC_AFTER_CALL(frame) \
read_memory_integer (read_register (SP_REGNUM), 4)

/* This is the amount to subtract from u.u_ar0
   to get the offset in the core file of the register values.  */

#ifndef NEWVM
#define KERNEL_U_ADDR 0x00816000
#define STACK_END_ADDR 0xfff00000
#else
#include <machine/vmparam.h>
#define KERNEL_U_ADDR USRSTACK
#define	SZ_SIGTRAMP 20
#define STACK_END_ADDR (USRSTACK - SZ_SIGTRAMP)
#endif

/* Same as offsetof macro from stddef.h (which 4.3BSD doesn't have).  */
#define my_offsetof(TYPE, MEMBER) ((unsigned long) &((TYPE *)0)->MEMBER)

#ifndef NEWVM
/* On the HP300, sigtramp is in the u area.  Gak!  User struct is not
   mapped to the same virtual address in user/kernel address space
   (hence STACK_END_ADDR as opposed to KERNEL_U_ADDR).  */
#define IN_SIGTRAMP(pc, name) \
  ((pc) >= STACK_END_ADDR + my_offsetof (struct user, u_pcb.pcb_sigc[0])   \
   && (pc) < STACK_END_ADDR + my_offsetof (struct user, u_pcb.pcb_sigc[12]) \
   )
#else
#define IN_SIGTRAMP(pc, name) ((pc) >= STACK_END_ADDR && (pc) < USRSTACK)
#endif

/* Stack grows downward.  */

#define INNER_THAN <

/* Sequence of bytes for breakpoint instruction.  */

#define BREAKPOINT {0x4e, 0x42}

/* Amount PC must be decremented by after a breakpoint.
   This is often the number of bytes in BREAKPOINT
   but not always.  */

#define DECR_PC_AFTER_BREAK 2

/* Nonzero if instruction at PC is a return instruction.  */

/* Allow any of the return instructions, including a trapv and a return
   from interupt.  */
#define ABOUT_TO_RETURN(pc) ((read_memory_integer (pc, 2) & ~3) == 0x4e74)

/* Return 1 if P points to an invalid floating point value.  */

#define INVALID_FLOAT(p, len) 0   /* Just a first guess; not checked */

/* Largest integer type */
#define LONGEST long

/* Name of the builtin type for the LONGEST type above. */
#define BUILTIN_TYPE_LONGEST builtin_type_long

/* Say how long (ordinary) registers are.  */

#define REGISTER_TYPE long

/* Number of machine registers */

#define NUM_REGS 29

/* Initializer for an array of names of registers.
   There should be NUM_REGS strings in this initializer.  */

#define REGISTER_NAMES  \
 {"d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7", \
  "a0", "a1", "a2", "a3", "a4", "a5", "a6", "sp", \
  "ps", "pc",  \
  "fp0", "fp1", "fp2", "fp3", "fp4", "fp5", "fp6", "fp7", \
  "fpcontrol", "fpstatus", "fpiaddr" }

/* Register numbers of various important registers.
   Note that some of these values are "real" register numbers,
   and correspond to the general registers of the machine,
   and some are "phony" register numbers which are too large
   to be actual register numbers as far as the user is concerned
   but do serve to get the desired values when passed to read_register.  */

#define FP_REGNUM 14		/* Contains address of executing stack frame */
#define SP_REGNUM 15		/* Contains address of top of stack */
#define PS_REGNUM 16		/* Contains processor status */
#define PC_REGNUM 17		/* Contains program counter */
#define FP0_REGNUM 18		/* Floating point register 0 */
#define FPC_REGNUM 26		/* 68881 control register */
#define FPS_REGNUM 27		/* 68881 status register */

/* Total amount of space needed to store our copies of the machine's
   register state, the array `registers'.  */
#define REGISTER_BYTES (16*4+8*12+8+12)

/* Index within `registers' of the first byte of the space for
   register N.  */

#define REGISTER_BYTE(N)  \
 ((N) >= FPC_REGNUM ? (((N) - FPC_REGNUM) * 4) + 168	\
  : (N) >= FP0_REGNUM ? (((N) - FP0_REGNUM) * 12) + 72	\
  : (N) * 4)

/* Number of bytes of storage in the actual machine representation
   for register N.  On the 68000, all regs are 4 bytes
   except the floating point regs which are 12 bytes.  */
/* Note that the unsigned cast here forces the result of the
   subtractiion to very high positive values if N < FP0_REGNUM */

#define REGISTER_RAW_SIZE(N) (((unsigned)(N) - FP0_REGNUM) < 8 ? 12 : 4)

/* Number of bytes of storage in the program's representation
   for register N.  On the 68000, all regs are 4 bytes
   except the floating point regs which are 8-byte doubles.  */

#define REGISTER_VIRTUAL_SIZE(N) (((unsigned)(N) - FP0_REGNUM) < 8 ? 8 : 4)

/* Largest value REGISTER_RAW_SIZE can have.  */

#define MAX_REGISTER_RAW_SIZE 12

/* Largest value REGISTER_VIRTUAL_SIZE can have.  */

#define MAX_REGISTER_VIRTUAL_SIZE 8

/* Nonzero if register N requires conversion
   from raw format to virtual format.  */

#define REGISTER_CONVERTIBLE(N) (((unsigned)(N) - FP0_REGNUM) < 8)

/* Convert data from raw format for register REGNUM
   to virtual format for register REGNUM.  */

#define REGISTER_CONVERT_TO_VIRTUAL(REGNUM,FROM,TO)	\
{ if ((REGNUM) >= FP0_REGNUM && (REGNUM) < FPC_REGNUM)	\
    convert_from_68881 ((FROM), (TO));	\
  else					\
    bcopy ((FROM), (TO), 4); }

/* Convert data from virtual format for register REGNUM
   to raw format for register REGNUM.  */

#define REGISTER_CONVERT_TO_RAW(REGNUM,FROM,TO)	\
{ if ((REGNUM) >= FP0_REGNUM && (REGNUM) < FPC_REGNUM)	\
    convert_to_68881 ((FROM), (TO));	\
  else					\
    bcopy ((FROM), (TO), 4); }

/* Return the GDB type object for the "standard" data type
   of data in register N.  */

#define REGISTER_VIRTUAL_TYPE(N) \
 (((unsigned)(N) - FP0_REGNUM) < 8 ? builtin_type_double : builtin_type_int)

/* Store the address of the place in which to copy the structure the
   subroutine will return.  This is called from call_function. */

#define STORE_STRUCT_RETURN(ADDR, SP) \
  { write_register (9, (ADDR)); }

/* Extract from an array REGBUF containing the (raw) register state
   a function return value of type TYPE, and copy that, in virtual format,
   into VALBUF.  */

#define EXTRACT_RETURN_VALUE(TYPE,REGBUF,VALBUF) \
  bcopy ((char *)(REGBUF) + 						\
	    (TYPE_LENGTH (TYPE) < 4  ?  4 - TYPE_LENGTH (TYPE)  :  0),	\
	 VALBUF, TYPE_LENGTH (TYPE))

/* Write into appropriate registers a function return value
   of type TYPE, given in virtual format.  */

#define STORE_RETURN_VALUE(TYPE,VALBUF) \
  write_register_bytes (0, VALBUF, TYPE_LENGTH (TYPE))

/* Extract from an array REGBUF containing the (raw) register state
   the address in which a function should return its structure value,
   as a CORE_ADDR (or an expression that can be used as one).  */

#define EXTRACT_STRUCT_VALUE_ADDRESS(REGBUF) (*(int *)(REGBUF))

/* This is a piece of magic that is given a register number REGNO
   and as BLOCKEND the address in the system of the end of the user structure
   and stores in ADDR the address in the kernel or core dump
   of that register.  */

#define REGISTER_U_ADDR(addr, blockend, regno)				\
{									\
  if (regno < PS_REGNUM)						\
    addr = (int) &((struct frame *)(blockend))->f_regs[regno];		\
  else if (regno == PS_REGNUM)						\
    addr = (int) &((struct frame *)(blockend))->f_stackadj;		\
  else if (regno == PC_REGNUM)						\
    addr = (int) &((struct frame *)(blockend))->f_pc;			\
  else if (regno < FPC_REGNUM)						\
    addr = (int)							\
      &((struct user *)0)->u_pcb.pcb_fpregs.fpf_regs[((regno)-FP0_REGNUM)*3];\
  else if (regno == FPC_REGNUM)						\
    addr = (int) &((struct user *)0)->u_pcb.pcb_fpregs.fpf_fpcr;	\
  else if (regno == FPS_REGNUM)						\
    addr = (int) &((struct user *)0)->u_pcb.pcb_fpregs.fpf_fpsr;	\
  else									\
    addr = (int) &((struct user *)0)->u_pcb.pcb_fpregs.fpf_fpiar;	\
}

/* Describe the pointer in each stack frame to the previous stack frame
   (its caller).  */

/* FRAME_CHAIN takes a frame's nominal address
   and produces the frame's chain-pointer.

   FRAME_CHAIN_COMBINE takes the chain pointer and the frame's nominal address
   and produces the nominal address of the caller frame.

   However, if FRAME_CHAIN_VALID returns zero,
   it means the given frame is the outermost one and has no caller.
   In that case, FRAME_CHAIN_COMBINE is not used.  */

/* In the case of the Sun, the frame's nominal address
   is the address of a 4-byte word containing the calling frame's address.  */

#define FRAME_CHAIN(thisframe)  \
  (outside_startup_file ((thisframe)->pc) ? \
   read_memory_integer ((thisframe)->frame, 4) :\
   0)

#ifdef KERNELDEBUG
#ifdef NEWVM
#define KERNSTACK_TOP ((unsigned long)KERNEL_U_ADDR + UPAGES*NBPG)
#else
#define KERNSTACK_TOP (KERNEL_U_ADDR + 0x3000) /* UPAGES * NBPG */
#endif
extern int kernel_debugging;
#define FRAME_CHAIN_VALID(chain, thisframe) \
	(chain != 0 && \
	 !kernel_debugging ? outside_startup_file(FRAME_SAVED_PC(thisframe)) :\
	 (chain >= read_register(SP_REGNUM) && chain < KERNSTACK_TOP))
#else
#define FRAME_CHAIN_VALID(chain, thisframe) \
  (chain != 0 && (outside_startup_file (FRAME_SAVED_PC (thisframe))))
#endif

#define FRAME_CHAIN_COMBINE(chain, thisframe) (chain)

/* Define other aspects of the stack frame.  */

/* A macro that tells us whether the function invocation represented
   by FI does not have a frame on the stack associated with it.  If it
   does not, FRAMELESS is set to 1, else 0.  */
#define FRAMELESS_FUNCTION_INVOCATION(FI, FRAMELESS) \
  FRAMELESS_LOOK_FOR_PROLOGUE(FI, FRAMELESS)

#define FRAME_SAVED_PC(FRAME) (read_memory_integer ((FRAME)->frame + 4, 4))

#define FRAME_ARGS_ADDRESS(fi) ((fi)->frame)

#define FRAME_LOCALS_ADDRESS(fi) ((fi)->frame)

/* Set VAL to the number of args passed to frame described by FI.
   Can set VAL to -1, meaning no way to tell.  */

/* Note that we may not be able to tell how many args there are
   now that the C compiler delays popping them.  */

#define FRAME_NUM_ARGS(val, fi)  \
{ register CORE_ADDR pc = FRAME_SAVED_PC (fi);			\
  register int insn = 0177777 & read_memory_integer (pc, 2);	\
  if (insn == 0047757 || insn == 0157374)  /* lea W(sp),sp or addaw #W,sp */ \
    val = read_memory_integer (pc + 2, 2);			\
  else if ((insn & 0170777) == 0050217 /* addql #N, sp */	\
	   || (insn & 0170777) == 0050117)  /* addqw */		\
    { val = (insn >> 9) & 7; if (val == 0) val = 8; }		\
  else if (insn == 0157774) /* addal #WW, sp */			\
    val = read_memory_integer (pc + 2, 4);			\
  else								\
    val = -1;							\
  val >>= 2; }

/* Return number of bytes at start of arglist that are not really args.  */

#define FRAME_ARGS_SKIP 8

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.  */

#define FRAME_FIND_SAVED_REGS(frame_info, frame_saved_regs)		\
{ register int regnum;							\
  register int regmask;							\
  register CORE_ADDR next_addr;						\
  register CORE_ADDR pc;						\
  int nextinsn;								\
  bzero (&frame_saved_regs, sizeof frame_saved_regs);			\
  if ((frame_info)->next						\
      && (frame_info)->next->frame < (frame_info)->pc 			\
      && (frame_info)->pc < (frame_info)->frame)			\
    { goto lose; }							\
  else   								\
    { pc = get_pc_function_start ((frame_info)->pc); 			\
      /* Verify we have a link a6 instruction next;			\
	 if not we lose.  If we win, find the address above the saved   \
	 regs using the amount of storage from the link instruction.  */\
      if (044016 == read_memory_integer (pc, 2))			\
	next_addr = (frame_info)->frame + read_memory_integer (pc += 2, 4), pc+=4; \
      else if (047126 == read_memory_integer (pc, 2))			\
	next_addr = (frame_info)->frame + read_memory_integer (pc += 2, 2), pc+=2; \
      else goto lose;							\
      /* If have an addal #-n, sp next, adjust next_addr.  */		\
      if ((0177777 & read_memory_integer (pc, 2)) == 0157774)		\
	next_addr += read_memory_integer (pc += 2, 4), pc += 4;		\
    }									\
  /* next should be a moveml to (sp) or -(sp) or a movl r,-(sp) */	\
  regmask = read_memory_integer (pc + 2, 2);				\
  /* But before that can come an fmovem.  Check for it.  */		\
  nextinsn = 0xffff & read_memory_integer (pc, 2);			\
  if (0xf227 == nextinsn						\
      && (regmask & 0xff00) == 0xe000)					\
    { pc += 4; /* Regmask's low bit is for register fp7, the first pushed */ \
      for (regnum = FP0_REGNUM + 7; regnum >= FP0_REGNUM; regnum--, regmask >>= 1)		\
	if (regmask & 1)						\
          (frame_saved_regs).regs[regnum] = (next_addr -= 12);		\
      regmask = read_memory_integer (pc + 2, 2); }			\
  if (0044327 == read_memory_integer (pc, 2))				\
    { pc += 4; /* Regmask's low bit is for register 0, the first written */ \
      for (regnum = 0; regnum < 16; regnum++, regmask >>= 1)		\
	if (regmask & 1)						\
          (frame_saved_regs).regs[regnum] = (next_addr += 4) - 4; }	\
  else if (0044347 == read_memory_integer (pc, 2))			\
    { pc += 4; /* Regmask's low bit is for register 15, the first pushed */ \
      for (regnum = 15; regnum >= 0; regnum--, regmask >>= 1)		\
	if (regmask & 1)						\
          (frame_saved_regs).regs[regnum] = (next_addr -= 4); }		\
  else if (0x2f00 == (0xfff0 & read_memory_integer (pc, 2)))		\
    { regnum = 0xf & read_memory_integer (pc, 2); pc += 2;		\
      (frame_saved_regs).regs[regnum] = (next_addr -= 4); }		\
  /* fmovemx to index of sp may follow.  */				\
  regmask = read_memory_integer (pc + 2, 2);				\
  nextinsn = 0xffff & read_memory_integer (pc, 2);			\
  if (0xf236 == nextinsn						\
      && (regmask & 0xff00) == 0xf000)					\
    { pc += 10; /* Regmask's low bit is for register fp0, the first written */ \
      for (regnum = FP0_REGNUM + 7; regnum >= FP0_REGNUM; regnum--, regmask >>= 1)		\
	if (regmask & 1)						\
          (frame_saved_regs).regs[regnum] = (next_addr += 12) - 12;	\
      regmask = read_memory_integer (pc + 2, 2); }			\
  /* clrw -(sp); movw ccr,-(sp) may follow.  */				\
  if (0x426742e7 == read_memory_integer (pc, 4))			\
    (frame_saved_regs).regs[PS_REGNUM] = (next_addr -= 4);		\
  lose: ;								\
  (frame_saved_regs).regs[SP_REGNUM] = (frame_info)->frame + 8;		\
  (frame_saved_regs).regs[FP_REGNUM] = (frame_info)->frame;		\
  (frame_saved_regs).regs[PC_REGNUM] = (frame_info)->frame + 4;		\
}


/* Discard from the stack the innermost frame, 
   restoring all saved registers.  */

#define POP_FRAME  \
{ register FRAME frame = get_current_frame ();			 \
  register CORE_ADDR fp;					 \
  register int regnum;						 \
  struct frame_saved_regs fsr;					 \
  struct frame_info *fi;					 \
  char raw_buffer[12];						 \
  fi = get_frame_info (frame);					 \
  fp = fi->frame;						 \
  get_frame_saved_regs (fi, &fsr);				 \
  for (regnum = FP0_REGNUM + 7; regnum >= FP0_REGNUM; regnum--)	 \
    if (fsr.regs[regnum])					 \
      { read_memory (fsr.regs[regnum], raw_buffer, 12);		 \
        write_register_bytes (REGISTER_BYTE (regnum), raw_buffer, 12); }\
  for (regnum = FP_REGNUM - 1; regnum >= 0; regnum--)		 \
    if (fsr.regs[regnum])					 \
      write_register (regnum, read_memory_integer (fsr.regs[regnum], 4)); \
  if (fsr.regs[PS_REGNUM])					 \
    write_register (PS_REGNUM, read_memory_integer (fsr.regs[PS_REGNUM], 4)); \
  write_register (FP_REGNUM, read_memory_integer (fp, 4));	 \
  write_register (PC_REGNUM, read_memory_integer (fp + 4, 4));   \
  write_register (SP_REGNUM, fp + 8);				 \
  flush_cached_frames ();					 \
  set_current_frame (create_new_frame (read_register (FP_REGNUM),\
					read_pc ())); }

#define NEW_CALL_FUNCTION

/* Interface definitions for kernel debugger KDB.  */

/* Map machine fault codes into signal numbers.
   First subtract 0, divide by 4, then index in a table.
   Faults for which the entry in this table is 0
   are not handled by KDB; the program's own trap handler
   gets to handle then.  */

#define FAULT_CODE_ORIGIN 0
#define FAULT_CODE_UNITS 4
#define FAULT_TABLE    \
{ 0, 0, 0, 0, SIGTRAP, 0, 0, 0, \
  0, SIGTRAP, 0, 0, 0, 0, 0, SIGKILL, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  SIGILL }

/* Start running with a stack stretching from BEG to END.
   BEG and END should be symbols meaningful to the assembler.
   This is used only for kdb.  */

#define INIT_STACK(beg, end)  \
{ asm (".globl end");         \
  asm ("movel #end, sp");      \
  asm ("movel #0,a6"); }

/* Push the frame pointer register on the stack.  */
#define PUSH_FRAME_PTR        \
  asm ("movel a6,sp@-");

/* Copy the top-of-stack to the frame pointer register.  */
#define POP_FRAME_PTR  \
  asm ("movl sp@,a6");

/* After KDB is entered by a fault, push all registers
   that GDB thinks about (all NUM_REGS of them),
   so that they appear in order of ascending GDB register number.
   The fault code will be on the stack beyond the last register.  */

#define PUSH_REGISTERS        \
{ asm ("clrw -(sp)");	      \
  asm ("pea sp@(10)");	      \
  asm ("movem #0xfffe,sp@-"); }

/* Assuming the registers (including processor status) have been
   pushed on the stack in order of ascending GDB register number,
   restore them and return to the address in the saved PC register.  */

#define POP_REGISTERS          \
{ asm ("subil #8,sp@(28)");     \
  asm ("movem sp@,#0xffff"); \
  asm ("rte"); }
