/* Parameters for execution on an HP 9000 model 320, for GDB, the GNU debugger.
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

/* HP/UX is USG, but it does have <ptrace.h>  */
#include <sys/ptrace.h>

#define HOST_BYTE_ORDER BIG_ENDIAN

/* Define this to indicate problems with traps after continuing.  */
#define HP_OS_BUG

/* fetch_inferior_registers is in hp300hpux-dep.c.  */
#define FETCH_INFERIOR_REGISTERS

/* Set flag to indicate whether HP's assembler is in use. */
#ifdef __GNUC__
#ifdef __HPUX_ASM__
#define HPUX_ASM
#endif
#else /* not GNU C.  */
#define HPUX_ASM
#endif /* not GNU C.  */

/* Define this for versions of hp-ux older than 6.0 */
/* #define HPUX_VERSION_5 */

/* define USG if you are using sys5 /usr/include's */
#undef USG	/* In case it was defined in the Makefile for cplus-dem.c */
#define USG

/* The mem functions are in <string.h>.  */
#undef MEM_FNS_DECLARED
#define MEM_FNS_DECLARED 1

#define HAVE_TERMIO

/* Get rid of any system-imposed stack limit if possible.  */
/* The hp9k320.h doesn't seem to have this feature.  */
/* #define SET_STACK_LIMIT_HUGE */
/* So we'll just have to avoid big alloca's.  */
#define BROKEN_LARGE_ALLOCA

/* This is the amount to subtract from u.u_ar0
   to get the offset in the core file of the register values.  */

#ifdef HPUX_VERSION_5
#define KERNEL_U_ADDR 0x00979000
#else /* Not HPUX version 5.  */
/* Use HPUX-style nlist() to get kernel_u_addr.  */
#define KERNEL_U_ADDR_HPUX
#endif /* Not HPUX version 5.  */

#define REGISTER_ADDR(u_ar0, regno)					\
  (unsigned int)							\
  (((regno) < PS_REGNUM)						\
   ? (&((struct exception_stack *) (u_ar0))->e_regs[(regno + R0)])	\
   : (((regno) == PS_REGNUM)						\
      ? ((int *) (&((struct exception_stack *) (u_ar0))->e_PS))		\
      : (&((struct exception_stack *) (u_ar0))->e_PC)))

#define FP_REGISTER_ADDR(u, regno)					\
  (((char *)								\
    (((regno) < FPC_REGNUM)						\
     ? (&u.u_pcb.pcb_mc68881[FMC68881_R0 + (((regno) - FP0_REGNUM) * 3)]) \
     : (&u.u_pcb.pcb_mc68881[FMC68881_C + ((regno) - FPC_REGNUM)])))	\
   - ((char *) (& u)))

/* Do implement the attach and detach commands.  */

#define ATTACH_DETACH

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

#ifndef HPUX_ASM

/* Start running with a stack stretching from BEG to END.
   BEG and END should be symbols meaningful to the assembler.
   This is used only for kdb.  */

#define INIT_STACK(beg, end)  \
{ asm (".globl end");         \
  asm ("movel $ end, sp");      \
  asm ("clrl fp"); }

/* Push the frame pointer register on the stack.  */
#define PUSH_FRAME_PTR        \
  asm ("movel fp, -(sp)");

/* Copy the top-of-stack to the frame pointer register.  */
#define POP_FRAME_PTR  \
  asm ("movl (sp), fp");

/* After KDB is entered by a fault, push all registers
   that GDB thinks about (all NUM_REGS of them),
   so that they appear in order of ascending GDB register number.
   The fault code will be on the stack beyond the last register.  */

#define PUSH_REGISTERS        \
{ asm ("clrw -(sp)");	      \
  asm ("pea 10(sp)");	      \
  asm ("movem $ 0xfffe,-(sp)"); }

/* Assuming the registers (including processor status) have been
   pushed on the stack in order of ascending GDB register number,
   restore them and return to the address in the saved PC register.  */

#define POP_REGISTERS          \
{ asm ("subil $8,28(sp)");     \
  asm ("movem (sp),$ 0xffff"); \
  asm ("rte"); }

#else /* HPUX_ASM */

/* Start running with a stack stretching from BEG to END.
   BEG and END should be symbols meaningful to the assembler.
   This is used only for kdb.  */

#define INIT_STACK(beg, end)						\
{ asm ("global end");							\
  asm ("mov.l &end,%sp");						\
  asm ("clr.l %a6"); }

/* Push the frame pointer register on the stack.  */
#define PUSH_FRAME_PTR							\
  asm ("mov.l %fp,-(%sp)");

/* Copy the top-of-stack to the frame pointer register.  */
#define POP_FRAME_PTR							\
  asm ("mov.l (%sp),%fp");

/* After KDB is entered by a fault, push all registers
   that GDB thinks about (all NUM_REGS of them),
   so that they appear in order of ascending GDB register number.
   The fault code will be on the stack beyond the last register.  */

#define PUSH_REGISTERS							\
{ asm ("clr.w -(%sp)");							\
  asm ("pea 10(%sp)");							\
  asm ("movm.l &0xfffe,-(%sp)"); }

/* Assuming the registers (including processor status) have been
   pushed on the stack in order of ascending GDB register number,
   restore them and return to the address in the saved PC register.  */

#define POP_REGISTERS							\
{ asm ("subi.l &8,28(%sp)");						\
  asm ("mov.m (%sp),&0xffff");						\
  asm ("rte"); }

#endif /* HPUX_ASM */
