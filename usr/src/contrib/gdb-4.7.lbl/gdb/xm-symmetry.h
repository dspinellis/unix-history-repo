/* Definitions to make GDB run on a Sequent Symmetry under dynix 3.0,
   with Weitek 1167 and i387 support.
   Copyright 1986, 1987, 1989, 1992  Free Software Foundation, Inc.

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

/* Symmetry version by Jay Vosburgh (uunet!sequent!fubar) */

/* This machine doesn't have the siginterrupt call.  */
#define NO_SIGINTERRUPT

#define HAVE_WAIT_STRUCT

/* XPT_DEBUG doesn't work yet under Dynix 3.0.12, but UNDEBUG does... */
/* #define PTRACE_ATTACH XPT_DEBUG
#define PTRACE_DETACH XPT_UNDEBUG
#define ATTACH_DETACH  */

#define HOST_BYTE_ORDER LITTLE_ENDIAN

/* Get rid of any system-imposed stack limit if possible.  */

#define SET_STACK_LIMIT_HUGE

/* This is the amount to subtract from u.u_ar0
   to get the offset in the core file of the register values.  */

#define KERNEL_U_ADDR (0x80000000 - (UPAGES * NBPG))

/* The magic numbers below are offsets into u_ar0 in the user struct.
   They live in <machine/reg.h>.  Gdb calls this macro with blockend
   holding u.u_ar0 - KERNEL_U_ADDR.  Only the registers listed are
   saved in the u area (along with a few others that aren't useful
   here.  See <machine/reg.h>).  */

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

/* Override copies of {fetch,store}_inferior_registers in infptrace.c.  */

#define FETCH_INFERIOR_REGISTERS

/* We must fetch all the regs before storing, since we store all at once.  */

#define CHILD_PREPARE_TO_STORE() read_register_bytes (0, NULL, REGISTER_BYTES)

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
