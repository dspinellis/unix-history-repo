/* IBM RS/6000 native-dependent code for GDB, the GNU debugger.
   Copyright 1986, 1987, 1989, 1991, 1992 Free Software Foundation, Inc.

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
#include "inferior.h"
#include "target.h"
#include "nm.h"

#include <sys/ptrace.h>
#include <sys/reg.h>

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <fcntl.h>

#include <a.out.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/core.h>

extern int errno;

static void
exec_one_dummy_insn PARAMS ((void));

/* Conversion from gdb-to-system special purpose register numbers.. */

static int special_regs[] = {
  IAR,				/* PC_REGNUM	*/
  MSR,				/* PS_REGNUM	*/
  CR,				/* CR_REGNUM	*/
  LR,				/* LR_REGNUM	*/
  CTR,				/* CTR_REGNUM	*/
  XER,				/* XER_REGNUM   */
  MQ				/* MQ_REGNUM	*/
};

void
fetch_inferior_registers (regno)
  int regno;
{
  int ii;
  extern char registers[];

  if (regno < 0) {			/* for all registers */

    /* read 32 general purpose registers. */

    for (ii=0; ii < 32; ++ii)
      *(int*)&registers[REGISTER_BYTE (ii)] = 
	ptrace (PT_READ_GPR, inferior_pid, (PTRACE_ARG3_TYPE) ii, 0, 0);

    /* read general purpose floating point registers. */

    for (ii=0; ii < 32; ++ii)
      ptrace (PT_READ_FPR, inferior_pid, 
	(PTRACE_ARG3_TYPE) &registers [REGISTER_BYTE (FP0_REGNUM+ii)],
	      FPR0+ii, 0);

    /* read special registers. */
    for (ii=0; ii <= LAST_SP_REGNUM-FIRST_SP_REGNUM; ++ii)
      *(int*)&registers[REGISTER_BYTE (FIRST_SP_REGNUM+ii)] = 
	ptrace (PT_READ_GPR, inferior_pid, (PTRACE_ARG3_TYPE) special_regs[ii],
		0, 0);

    registers_fetched ();
    return;
  }

  /* else an individual register is addressed. */

  else if (regno < FP0_REGNUM) {		/* a GPR */
    *(int*)&registers[REGISTER_BYTE (regno)] =
	ptrace (PT_READ_GPR, inferior_pid, (PTRACE_ARG3_TYPE) regno, 0, 0);
  }
  else if (regno <= FPLAST_REGNUM) {		/* a FPR */
    ptrace (PT_READ_FPR, inferior_pid,
	(PTRACE_ARG3_TYPE) &registers [REGISTER_BYTE (regno)],
	    (regno-FP0_REGNUM+FPR0), 0);
  }
  else if (regno <= LAST_SP_REGNUM) {		/* a special register */
    *(int*)&registers[REGISTER_BYTE (regno)] =
	ptrace (PT_READ_GPR, inferior_pid,
		(PTRACE_ARG3_TYPE) special_regs[regno-FIRST_SP_REGNUM], 0, 0);
  }
  else
    fprintf (stderr, "gdb error: register no %d not implemented.\n", regno);

  register_valid [regno] = 1;
}

/* Store our register values back into the inferior.
   If REGNO is -1, do this for all registers.
   Otherwise, REGNO specifies which register (so we can save time).  */

void
store_inferior_registers (regno)
     int regno;
{
  extern char registers[];

  errno = 0;

  if (regno == -1) {			/* for all registers..	*/
      int ii;

       /* execute one dummy instruction (which is a breakpoint) in inferior
          process. So give kernel a chance to do internal house keeping.
	  Otherwise the following ptrace(2) calls will mess up user stack
	  since kernel will get confused about the bottom of the stack (%sp) */

       exec_one_dummy_insn ();

      /* write general purpose registers first! */
      for ( ii=GPR0; ii<=GPR31; ++ii) {
	ptrace (PT_WRITE_GPR, inferior_pid, (PTRACE_ARG3_TYPE) ii,
		*(int*)&registers[REGISTER_BYTE (ii)], 0);
	if ( errno ) { 
	  perror ("ptrace write_gpr"); errno = 0;
	}
      }

      /* write floating point registers now. */
      for ( ii=0; ii < 32; ++ii) {
	ptrace (PT_WRITE_FPR, inferior_pid, 
		  (PTRACE_ARG3_TYPE) &registers[REGISTER_BYTE (FP0_REGNUM+ii)],
		FPR0+ii, 0);
        if ( errno ) {
	  perror ("ptrace write_fpr"); errno = 0;
        }
      }

      /* write special registers. */
      for (ii=0; ii <= LAST_SP_REGNUM-FIRST_SP_REGNUM; ++ii) {
        ptrace (PT_WRITE_GPR, inferior_pid,
		(PTRACE_ARG3_TYPE) special_regs[ii],
		*(int*)&registers[REGISTER_BYTE (FIRST_SP_REGNUM+ii)], 0);
	if ( errno ) {
	  perror ("ptrace write_gpr"); errno = 0;
	}
      }
  }

  /* else, a specific register number is given... */

  else if (regno < FP0_REGNUM) {		/* a GPR */

    ptrace (PT_WRITE_GPR, inferior_pid, (PTRACE_ARG3_TYPE) regno,
		*(int*)&registers[REGISTER_BYTE (regno)], 0);
  }

  else if (regno <= FPLAST_REGNUM) {		/* a FPR */
    ptrace (PT_WRITE_FPR, inferior_pid, 
	    (PTRACE_ARG3_TYPE) &registers[REGISTER_BYTE (regno)],
	    regno-FP0_REGNUM+FPR0, 0);
  }

  else if (regno <= LAST_SP_REGNUM) {		/* a special register */

    ptrace (PT_WRITE_GPR, inferior_pid,
	    (PTRACE_ARG3_TYPE) special_regs [regno-FIRST_SP_REGNUM],
	    *(int*)&registers[REGISTER_BYTE (regno)], 0);
  }

  else
    fprintf (stderr, "Gdb error: register no %d not implemented.\n", regno);

  if ( errno ) {
    perror ("ptrace write");  errno = 0;
  }
}

/* Execute one dummy breakpoint instruction.  This way we give the kernel
   a chance to do some housekeeping and update inferior's internal data,
   including u_area. */
static void
exec_one_dummy_insn ()
{
#define	DUMMY_INSN_ADDR	(TEXT_SEGMENT_BASE)+0x200

  unsigned long shadow;
  unsigned int status, pid;

  /* We plant one dummy breakpoint into DUMMY_INSN_ADDR address. We assume that
     this address will never be executed again by the real code. */

  target_insert_breakpoint (DUMMY_INSN_ADDR, &shadow);

  errno = 0;
  ptrace (PT_CONTINUE, inferior_pid, (PTRACE_ARG3_TYPE) DUMMY_INSN_ADDR, 0, 0);
  if (errno)
    perror ("pt_continue");

  do {
    pid = wait (&status);
  } while (pid != inferior_pid);
    
  target_remove_breakpoint (DUMMY_INSN_ADDR, &shadow);
}

void
fetch_core_registers (core_reg_sect, core_reg_size, which, reg_addr)
     char *core_reg_sect;
     unsigned core_reg_size;
     int which;
     unsigned int reg_addr;	/* Unused in this version */
{
  /* fetch GPRs and special registers from the first register section
     in core bfd. */
  if (which == 0) {

    /* copy GPRs first. */
    bcopy (core_reg_sect, registers, 32 * 4);

    /* gdb's internal register template and bfd's register section layout
       should share a common include file. FIXMEmgo */
    /* then comes special registes. They are supposed to be in the same
       order in gdb template and bfd `.reg' section. */
    core_reg_sect += (32 * 4);
    bcopy (core_reg_sect, &registers [REGISTER_BYTE (FIRST_SP_REGNUM)],
    			(LAST_SP_REGNUM - FIRST_SP_REGNUM + 1) * 4);
  }

  /* fetch floating point registers from register section 2 in core bfd. */
  else if (which == 2)
    bcopy (core_reg_sect, &registers [REGISTER_BYTE (FP0_REGNUM)], 32 * 8);

  else
    fprintf (stderr, "Gdb error: unknown parameter to fetch_core_registers().\n");
}
