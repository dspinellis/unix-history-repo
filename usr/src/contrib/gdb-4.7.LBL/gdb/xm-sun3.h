/* Parameters for execution on a Sun, for GDB, the GNU debugger.
   Copyright (C) 1986, 1987, 1989 Free Software Foundation, Inc.

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

/* Get rid of any system-imposed stack limit if possible.  */

#define SET_STACK_LIMIT_HUGE

/* Enable use of alternate code for Sun's format of core dump file.  */

#define NEW_SUN_CORE

/* We have to grab the regs since we store all regs at once.  */

#define CHILD_PREPARE_TO_STORE() \
	read_register_bytes (0, (char *)NULL, REGISTER_BYTES)

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
