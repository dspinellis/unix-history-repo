/* Definitions to make GDB run on an Altos 3068 (m68k running SVR2)
   Copyright (C) 1987,1989 Free Software Foundation, Inc.

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

/* The altos support would make a good base for a port to other USGR2 systems
   (like the 3b1 and the Convergent miniframe).  */

/* This is only needed in one file, but it's cleaner to put it here than
   putting in more #ifdef's.  */
#include <sys/page.h>
#include <sys/net.h>

#define USG

#define HAVE_TERMIO

#define CBREAK XTABS	/* It takes all kinds... */

#ifndef R_OK
#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0
#endif

/* Get sys/wait.h ie. from a Sun and edit it a little (mc68000 to m68k) */
/* Why bother?  */
#if 0
#define HAVE_WAIT_STRUCT
#endif

/* This is the amount to subtract from u.u_ar0
   to get the offset in the core file of the register values. */

#define KERNEL_U_ADDR 0x1fbf000

#define REGISTER_U_ADDR(addr, blockend, regno)		\
{	if (regno <= SP_REGNUM) \
	  addr = blockend + regno * 4; \
	else if (regno == PS_REGNUM) \
	  addr = blockend + regno * 4 + 4; \
	else if (regno == PC_REGNUM) \
	  addr = blockend + regno * 4 + 2; \
}

#define REGISTER_ADDR(u_ar0, regno)					\
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


#ifndef __GNUC__
#undef USE_GAS
#define ALTOS_AS
#else
#define USE_GAS
#endif

/* Motorola assembly format */
#if !defined(USE_GAS) && !defined(ALTOS)
#define MOTOROLA
#endif

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

#ifdef MOTOROLA
#define INIT_STACK(beg, end)  \
{ asm (".globl end");         \
  asm ("move.l $ end, sp");      \
  asm ("clr.l fp"); }
#else
#ifdef ALTOS_AS
#define INIT_STACK(beg, end)  \
{ asm ("global end");         \
  asm ("mov.l &end,%sp");      \
  asm ("clr.l %fp"); }
#else
#define INIT_STACK(beg, end)  \
{ asm (".globl end");         \
  asm ("movel $ end, sp");      \
  asm ("clrl fp"); }
#endif
#endif

/* Push the frame pointer register on the stack.  */
#ifdef MOTOROLA
#define PUSH_FRAME_PTR        \
  asm ("move.l fp, -(sp)");
#else
#ifdef ALTOS_AS
#define PUSH_FRAME_PTR        \
  asm ("mov.l %fp, -(%sp)");
#else
#define PUSH_FRAME_PTR        \
  asm ("movel fp, -(sp)");
#endif
#endif

/* Copy the top-of-stack to the frame pointer register.  */
#ifdef MOTOROLA
#define POP_FRAME_PTR  \
  asm ("move.l (sp), fp");
#else
#ifdef ALTOS_AS
#define POP_FRAME_PTR  \
  asm ("mov.l (%sp), %fp");
#else
#define POP_FRAME_PTR  \
  asm ("movl (sp), fp");
#endif
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
#ifdef ALTOS_AS
#define PUSH_REGISTERS        \
{ asm ("clr.w -(%sp)");	      \
  asm ("pea (10,%sp)");	      \
  asm ("movm.l &0xfffe,-(%sp)"); }
#else
#define PUSH_REGISTERS        \
{ asm ("clrw -(sp)");	      \
  asm ("pea 10(sp)");	      \
  asm ("movem $ 0xfffe,-(sp)"); }
#endif
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
#ifdef ALTOS_AS
#define POP_REGISTERS          \
{ asm ("sub.l &8,28(%sp)");     \
  asm ("movem (%sp),&0xffff"); \
  asm ("rte"); }
#else
#define POP_REGISTERS          \
{ asm ("subil $8,28(sp)");     \
  asm ("movem (sp),$ 0xffff"); \
  asm ("rte"); }
#endif
#endif
