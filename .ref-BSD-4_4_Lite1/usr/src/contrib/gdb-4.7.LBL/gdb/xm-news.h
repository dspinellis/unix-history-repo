/* Parameters for execution on a Sony/NEWS, for GDB, the GNU debugger.
   Copyright 1987, 1989, 1992 Free Software Foundation, Inc.

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

#define HOST_BYTE_ORDER BIG_ENDIAN

#define HAVE_WAIT_STRUCT

/* Get rid of any system-imposed stack limit if possible.  */

#define SET_STACK_LIMIT_HUGE

/* We can't use "isatty" or "fileno" on this machine.  This isn't good,
   but it will have to do.  */
#define ISATTY(FP)	((FP) == stdin || (FP) == stdout)

/* THis is the amount to subtract from u.u_ar0
   to get the offset in the core file of the register values. */

#define KERNEL_U_ADDR UADDR

/* The offsets in this macro are from /usr/include/machine/reg.h */

#define REGISTER_U_ADDR(addr, blockend, regno)		\
{   static char offsets[] = { \
	/*d0-d7:*/1,2,3,4,5,6,7,8, \
	/*a0-a6:*/9,10,11,12,13,14,15, /*sp:*/-4, /*ps:*/0, /*pc:*/-1, \
	/*fp0-fp7:*/19,22,25,28,31,34,37,40, /*fpc:*/16,17,18 }; \
	addr = blockend + 4 * offsets[regno]; \
}

/* NewsOS 3 apparently dies on large alloca's -- roland@ai.mit.edu.  */
#define	BROKEN_LARGE_ALLOCA


/* Interface definitions for kernel debugger KDB.  */

/* Use GNU assembler instead of standard assembler */
#define USE_GAS

/* Motorola assembly format */
#ifndef USE_GAS
#define MOTOROLA
#endif

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

#ifdef MOTOROLA
#define INIT_STACK(beg, end)  \
{ asm (".globl end");         \
  asm ("move.l $ end, sp");      \
  asm ("clr.l fp"); }
#else
#define INIT_STACK(beg, end)  \
{ asm (".globl end");         \
  asm ("movel $ end, sp");      \
  asm ("clrl fp"); }
#endif

/* Push the frame pointer register on the stack.  */
#ifdef MOTOROLA
#define PUSH_FRAME_PTR        \
  asm ("move.l fp, -(sp)");
#else
#define PUSH_FRAME_PTR        \
  asm ("movel fp, -(sp)");
#endif

/* Copy the top-of-stack to the frame pointer register.  */
#ifdef MOTOROLA
#define POP_FRAME_PTR  \
  asm ("move.l (sp), fp");
#else
#define POP_FRAME_PTR  \
  asm ("movl (sp), fp");
#endif

/* After KDB is entered by a fault, push all registers
   that GDB thinks about (all NUM_REGS of them),
   so that they appear in order of ascending GDB register number.
   The fault code will be on the stack beyond the last register.  */

#ifdef MOTOROLA
#define PUSH_REGISTERS        \
{ asm ("clr.w -(sp)");	      \
  asm ("pea (10,sp)");	      \
  asm ("movem $ 0xfffe,-(sp)"); }
#else
#define PUSH_REGISTERS        \
{ asm ("clrw -(sp)");	      \
  asm ("pea 10(sp)");	      \
  asm ("movem $ 0xfffe,-(sp)"); }
#endif

/* Assuming the registers (including processor status) have been
   pushed on the stack in order of ascending GDB register number,
   restore them and return to the address in the saved PC register.  */

#ifdef MOTOROLA
#define POP_REGISTERS          \
{ asm ("subi.l $8,28(sp)");     \
  asm ("movem (sp),$ 0xffff"); \
  asm ("rte"); }
#else
#define POP_REGISTERS          \
{ asm ("subil $8,28(sp)");     \
  asm ("movem (sp),$ 0xffff"); \
  asm ("rte"); }
#endif
