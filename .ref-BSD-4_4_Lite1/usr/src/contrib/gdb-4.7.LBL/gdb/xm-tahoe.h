/* Definitions to make GDB hosted on a tahoe running 4.3-Reno
   Copyright 1986, 1987, 1989, 1991, 1992 Free Software Foundation, Inc.
   Contributed by the State University of New York at Buffalo, by the
   Distributed Computer Systems Lab, Department of Computer Science, 1991.

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

/* Make sure the system include files define BIG_ENDIAN, UINT_MAX, const,
   etc, rather than GDB's files.  */
#include <stdio.h>
#include <sys/param.h>

/* Host is big-endian */

#define	HOST_BYTE_ORDER	BIG_ENDIAN

/* Get rid of any system-imposed stack limit if possible.  */

#define SET_STACK_LIMIT_HUGE

/* This is the amount to subtract from u.u_ar0
   to get the offset in the core file of the register values.  */

#define KERNEL_U_ADDR (0xc0000000 - (TARGET_UPAGES * TARGET_NBPG))

#define REGISTER_U_ADDR(addr, blockend, regno)		\
{ addr = blockend - 100 + regno * 4;			\
  if (regno == PC_REGNUM) addr = blockend - 8;		\
  if (regno == PS_REGNUM) addr = blockend - 4;		\
  if (regno == FP_REGNUM) addr = blockend - 40;	        \
  if (regno == SP_REGNUM) addr = blockend - 36;         \
  if (regno == AL_REGNUM) addr = blockend - 20;       \
  if (regno == AH_REGNUM) addr = blockend - 24;}

/* Interface definitions for kernel debugger KDB.  */

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
  asm ("movl $ end, sp");      \
  asm ("clrl fp"); }

/* Push the frame pointer register on the stack.  */

#define PUSH_FRAME_PTR        \
  asm ("pushl fp");

/* Copy the top-of-stack to the frame pointer register.  */

#define POP_FRAME_PTR  \
  asm ("movl (sp), fp");

/* After KDB is entered by a fault, push all registers
   that GDB thinks about (all NUM_REGS of them),
   so that they appear in order of ascending GDB register number.
   The fault code will be on the stack beyond the last register.  */

#define PUSH_REGISTERS        \
{ asm ("pushl 8(sp)");        \
  asm ("pushl 8(sp)");        \
  asm ("pushal 0x41(sp)");    \
  asm ("pushl r0" );       \
  asm ("pushl r1" );       \
  asm ("pushl r2" );       \
  asm ("pushl r3" );       \
  asm ("pushl r4" );       \
  asm ("pushl r5" );       \
  asm ("pushl r6" );       \
  asm ("pushl r7" );       \
  asm ("pushl r8" );       \
  asm ("pushl r9" );       \
  asm ("pushl r10" );       \
  asm ("pushl r11" );       \
  asm ("pushl r12" );       \
  asm ("pushl fp" );       \
  asm ("pushl sp" );       \
  asm ("pushl pc" );       \
  asm ("pushl ps" );       \
  asm ("pushl aclo" );       \
  asm ("pushl achi" );       \
}

/* Assuming the registers (including processor status) have been
   pushed on the stack in order of ascending GDB register number,
   restore them and return to the address in the saved PC register.  */

#define POP_REGISTERS      \
{                          \
  asm ("movl (sp)+, achi");   \
  asm ("movl (sp)+, aclo");   \
  asm ("movl (sp)+, ps");   \
  asm ("movl (sp)+, pc");   \
  asm ("movl (sp)+, sp");   \
  asm ("movl (sp)+, fp");   \
  asm ("movl (sp)+, r12");   \
  asm ("movl (sp)+, r11");   \
  asm ("movl (sp)+, r10");   \
  asm ("movl (sp)+, r9");   \
  asm ("movl (sp)+, r8");   \
  asm ("movl (sp)+, r7");   \
  asm ("movl (sp)+, r6");   \
  asm ("movl (sp)+, r5");   \
  asm ("movl (sp)+, r4");   \
  asm ("movl (sp)+, r3");   \
  asm ("movl (sp)+, r2");   \
  asm ("movl (sp)+, r1");   \
  asm ("movl (sp)+, r0");   \
  asm ("subl2 $8,(sp)");   \
  asm ("movl (sp),sp");    \
  asm ("rei"); }
