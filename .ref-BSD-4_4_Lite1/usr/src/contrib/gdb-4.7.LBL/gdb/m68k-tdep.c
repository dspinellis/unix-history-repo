/* Target dependent code for the Motorola 68000 series.
   Copyright (C) 1990, 1992 Free Software Foundation, Inc.

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

#include "defs.h"
#include "ieee-float.h"
#include "frame.h"
#include "symtab.h"

const struct ext_format ext_format_68881 = {
/* tot sbyte smask expbyte manbyte */
   12, 0,    0x80, 0,1,	   4,8		/* mc68881 */
};


/* Things needed for making the inferior call functions.
   It seems like every m68k based machine has almost identical definitions
   in the individual machine's configuration files.  Most other cpu types
   (mips, i386, etc) have routines in their *-tdep.c files to handle this
   for most configurations.  The m68k family should be able to do this as
   well.  These macros can still be overridden when necessary.  */

/* Push an empty stack frame, to record the current PC, etc.  */

void
m68k_push_dummy_frame ()
{
  register CORE_ADDR sp = read_register (SP_REGNUM);
  register int regnum;
  char raw_buffer[12];

  sp = push_word (sp, read_register (PC_REGNUM));
  sp = push_word (sp, read_register (FP_REGNUM));
  write_register (FP_REGNUM, sp);
#if defined (HAVE_68881)
  for (regnum = FP0_REGNUM + 7; regnum >= FP0_REGNUM; regnum--)
    {
      read_register_bytes (REGISTER_BYTE (regnum), raw_buffer, 12);
      sp = push_bytes (sp, raw_buffer, 12);
    }
#endif
  for (regnum = FP_REGNUM - 1; regnum >= 0; regnum--)
    {
      sp = push_word (sp, read_register (regnum));
    }
  sp = push_word (sp, read_register (PS_REGNUM));
  write_register (SP_REGNUM, sp);
}

/* Discard from the stack the innermost frame,
   restoring all saved registers.  */

void
m68k_pop_frame ()
{
  register FRAME frame = get_current_frame ();
  register CORE_ADDR fp;
  register int regnum;
  struct frame_saved_regs fsr;
  struct frame_info *fi;
  char raw_buffer[12];

  fi = get_frame_info (frame);
  fp = fi -> frame;
  get_frame_saved_regs (fi, &fsr);
#if defined (HAVE_68881)
  for (regnum = FP0_REGNUM + 7 ; regnum >= FP0_REGNUM ; regnum--)
    {
      if (fsr.regs[regnum])
	{
	  read_memory (fsr.regs[regnum], raw_buffer, 12);
	  write_register_bytes (REGISTER_BYTE (regnum), raw_buffer, 12);
	}
    }
#endif
  for (regnum = FP_REGNUM - 1 ; regnum >= 0 ; regnum--)
    {
      if (fsr.regs[regnum])
	{
	  write_register (regnum, read_memory_integer (fsr.regs[regnum], 4));
	}
    }
  if (fsr.regs[PS_REGNUM])
    {
      write_register (PS_REGNUM, read_memory_integer (fsr.regs[PS_REGNUM], 4));
    }
  write_register (FP_REGNUM, read_memory_integer (fp, 4));
  write_register (PC_REGNUM, read_memory_integer (fp + 4, 4));
  write_register (SP_REGNUM, fp + 8);
  flush_cached_frames ();
  set_current_frame (create_new_frame (read_register (FP_REGNUM),
				       read_pc ()));
}


/* Given an ip value corresponding to the start of a function,
   return the ip of the first instruction after the function 
   prologue.  This is the generic m68k support.  Machines which
   require something different can override the SKIP_PROLOGUE
   macro to point elsewhere.

   Some instructions which typically may appear in a function
   prologue include:

   A link instruction, word form:

	link.w	%a6,&0			4e56  XXXX

   A link instruction, long form:

	link.l  %fp,&F%1		480e  XXXX  XXXX

   A movm instruction to preserve integer regs:

	movm.l  &M%1,(4,%sp)		48ef  XXXX  XXXX

   A fmovm instruction to preserve float regs:

	fmovm   &FPM%1,(FPO%1,%sp)	f237  XXXX  XXXX  XXXX  XXXX

   Some profiling setup code (FIXME, not recognized yet):

	lea.l   (.L3,%pc),%a1		43fb  XXXX  XXXX  XXXX
	bsr     _mcount			61ff  XXXX  XXXX

  */

#define P_LINK_L	0x480e
#define P_LINK_W	0x4e56
#define P_MOV_L		0x207c
#define P_JSR		0x4eb9
#define P_BSR		0x61ff
#define P_LEA_L		0x43fb
#define P_MOVM_L	0x48ef
#define P_FMOVM		0xf237
#define P_TRAP		0x4e40

CORE_ADDR
m68k_skip_prologue (ip)
CORE_ADDR ip;
{
  register CORE_ADDR limit;
  struct symtab_and_line sal;
  register int op;

  /* Find out if there is a known limit for the extent of the prologue.
     If so, ensure we don't go past it.  If not, assume "infinity". */

  sal = find_pc_line (ip, 0);
  limit = (sal.end) ? sal.end : (CORE_ADDR) ~0;

  while (ip < limit)
    {
      op = read_memory_integer (ip, 2);
      op &= 0xFFFF;
      
      if (op == P_LINK_W)
	{
	  ip += 4;	/* Skip link.w */
	}
      else if (op == P_LINK_L)
	{
	  ip += 6;	/* Skip link.l */
	}
      else if (op == P_MOVM_L)
	{
	  ip += 6;	/* Skip movm.l */
	}
      else if (op == P_FMOVM)
	{
	  ip += 10;	/* Skip fmovm */
	}
      else
	{
	  break;	/* Found unknown code, bail out. */
	}
    }
  return (ip);
}

#ifdef USE_PROC_FS	/* Target dependent support for /proc */

#include <sys/procfs.h>

/*  The /proc interface divides the target machine's register set up into
    two different sets, the general register set (gregset) and the floating
    point register set (fpregset).  For each set, there is an ioctl to get
    the current register set and another ioctl to set the current values.

    The actual structure passed through the ioctl interface is, of course,
    naturally machine dependent, and is different for each set of registers.
    For the m68k for example, the general register set is typically defined
    by:

	typedef int gregset_t[18];

	#define	R_D0	0
	...
	#define	R_PS	17

    and the floating point set by:

    	typedef	struct fpregset {
	  int	f_pcr;
	  int	f_psr;
	  int	f_fpiaddr;
	  int	f_fpregs[8][3];		(8 regs, 96 bits each)
	} fpregset_t;

    These routines provide the packing and unpacking of gregset_t and
    fpregset_t formatted data.

 */


/*  Given a pointer to a general register set in /proc format (gregset_t *),
    unpack the register contents and supply them as gdb's idea of the current
    register values. */

void
supply_gregset (gregsetp)
gregset_t *gregsetp;
{
  register int regi;
  register greg_t *regp = (greg_t *) gregsetp;

  for (regi = 0 ; regi < R_PC ; regi++)
    {
      supply_register (regi, (char *) (regp + regi));
    }
  supply_register (PS_REGNUM, (char *) (regp + R_PS));
  supply_register (PC_REGNUM, (char *) (regp + R_PC));
}

void
fill_gregset (gregsetp, regno)
gregset_t *gregsetp;
int regno;
{
  register int regi;
  register greg_t *regp = (greg_t *) gregsetp;
  extern char registers[];

  for (regi = 0 ; regi < R_PC ; regi++)
    {
      if ((regno == -1) || (regno == regi))
	{
	  *(regp + regi) = *(int *) &registers[REGISTER_BYTE (regi)];
	}
    }
  if ((regno == -1) || (regno == PS_REGNUM))
    {
      *(regp + R_PS) = *(int *) &registers[REGISTER_BYTE (PS_REGNUM)];
    }
  if ((regno == -1) || (regno == PC_REGNUM))
    {
      *(regp + R_PC) = *(int *) &registers[REGISTER_BYTE (PC_REGNUM)];
    }
}

#if defined (FP0_REGNUM)

/*  Given a pointer to a floating point register set in /proc format
    (fpregset_t *), unpack the register contents and supply them as gdb's
    idea of the current floating point register values. */

void 
supply_fpregset (fpregsetp)
fpregset_t *fpregsetp;
{
  register int regi;
  char *from;
  
  for (regi = FP0_REGNUM ; regi < FPC_REGNUM ; regi++)
    {
      from = (char *) &(fpregsetp -> f_fpregs[regi-FP0_REGNUM][0]);
      supply_register (regi, from);
    }
  supply_register (FPC_REGNUM, (char *) &(fpregsetp -> f_pcr));
  supply_register (FPS_REGNUM, (char *) &(fpregsetp -> f_psr));
  supply_register (FPI_REGNUM, (char *) &(fpregsetp -> f_fpiaddr));
}

/*  Given a pointer to a floating point register set in /proc format
    (fpregset_t *), update the register specified by REGNO from gdb's idea
    of the current floating point register set.  If REGNO is -1, update
    them all. */

void
fill_fpregset (fpregsetp, regno)
fpregset_t *fpregsetp;
int regno;
{
  int regi;
  char *to;
  char *from;
  extern char registers[];

  for (regi = FP0_REGNUM ; regi < FPC_REGNUM ; regi++)
    {
      if ((regno == -1) || (regno == regi))
	{
	  from = (char *) &registers[REGISTER_BYTE (regi)];
	  to = (char *) &(fpregsetp -> f_fpregs[regi-FP0_REGNUM][0]);
	  bcopy (from, to, REGISTER_RAW_SIZE (regi));
	}
    }
  if ((regno == -1) || (regno == FPC_REGNUM))
    {
      fpregsetp -> f_pcr = *(int *) &registers[REGISTER_BYTE (FPC_REGNUM)];
    }
  if ((regno == -1) || (regno == FPS_REGNUM))
    {
      fpregsetp -> f_psr = *(int *) &registers[REGISTER_BYTE (FPS_REGNUM)];
    }
  if ((regno == -1) || (regno == FPI_REGNUM))
    {
      fpregsetp -> f_fpiaddr = *(int *) &registers[REGISTER_BYTE (FPI_REGNUM)];
    }
}

#endif	/* defined (FP0_REGNUM) */

#endif  /* USE_PROC_FS */

#ifdef GET_LONGJMP_TARGET
/* Figure out where the longjmp will land.  Slurp the args out of the stack.
   We expect the first arg to be a pointer to the jmp_buf structure from which
   we extract the pc (JB_PC) that we will land at.  The pc is copied into PC.
   This routine returns true on success. */

int
get_longjmp_target(pc)
     CORE_ADDR *pc;
{
  CORE_ADDR sp, jb_addr;

  sp = read_register(SP_REGNUM);

  if (target_read_memory(sp + SP_ARG0, /* Offset of first arg on stack */
			 &jb_addr,
			 sizeof(CORE_ADDR)))
    return 0;


  SWAP_TARGET_AND_HOST(&jb_addr, sizeof(CORE_ADDR));

  if (target_read_memory(jb_addr + JB_PC * JB_ELEMENT_SIZE, pc,
			 sizeof(CORE_ADDR)))
    return 0;

  SWAP_TARGET_AND_HOST(pc, sizeof(CORE_ADDR));

  return 1;
}
#endif /* GET_LONGJMP_TARGET */

/* Immediately after a function call, return the saved pc before the frame
   is setup.  For sun3's, we check for the common case of being inside of a
   system call, and if so, we know that Sun pushes the call # on the stack
   prior to doing the trap. */

CORE_ADDR
m68k_saved_pc_after_call(frame)
     struct frame_info *frame;
{
#ifdef GDB_TARGET_IS_SUN3
  int op;

  op = read_memory_integer (frame->pc, 2);
  op &= 0xFFFF;

  if (op == P_TRAP)
    return read_memory_integer (read_register (SP_REGNUM) + 4, 4);
  else
#endif /* GDB_TARGET_IS_SUN3 */
    return read_memory_integer (read_register (SP_REGNUM), 4);
}

FRAME_ADDR
m68k_frame_chain(p)
	FRAME p;
{
#ifdef KERNELDEBUG
	extern int kernel_debugging;
	/* XXX check for last kernel frame here */
#else
	if (inside_entry_file(p->pc))
		return (0);
#endif
	return (read_memory_integer(p->frame, 4));
}
