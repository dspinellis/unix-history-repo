/* HP/UX interface for HP 300's, for GDB when running under Unix.
   Copyright (C) 1986, 1987, 1989, 1991 Free Software Foundation, Inc.
   
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
#include "frame.h"
#include "inferior.h"

/* Defining this means some system include files define some extra stuff.  */
#define WOPR
#include <sys/param.h>
#include <sys/dir.h>
#include <signal.h>
#include <sys/user.h>
#include <sys/ioctl.h>
#include <fcntl.h>

#include <sys/ptrace.h>
#include <sys/reg.h>
#include <sys/trap.h>

#include "gdbcore.h"

#include <sys/file.h>
#include <sys/stat.h>

#define INFERIOR_AR0(u)							\
  ((ptrace								\
    (PT_RUAREA, inferior_pid,						\
     (PTRACE_ARG3_TYPE) ((char *) &u.u_ar0 - (char *) &u), 0))		\
   - KERNEL_U_ADDR)

static void
fetch_inferior_register (regno, regaddr)
     register int regno;
     register unsigned int regaddr;
{
#ifndef HPUX_VERSION_5
  if (regno == PS_REGNUM)
    {
      union { int i; short s[2]; } ps_val;
      int regval;
      
      ps_val.i = (ptrace (PT_RUAREA, inferior_pid, (PTRACE_ARG3_TYPE) regaddr,
			  0));
      regval = ps_val.s[0];
      supply_register (regno, &regval);
    }
  else
#endif /* not HPUX_VERSION_5 */
    {
      char buf[MAX_REGISTER_RAW_SIZE];
      register int i;
      
      for (i = 0; i < REGISTER_RAW_SIZE (regno); i += sizeof (int))
	{
	  *(int *) &buf[i] = ptrace (PT_RUAREA, inferior_pid,
				     (PTRACE_ARG3_TYPE) regaddr, 0);
	  regaddr += sizeof (int);
	}
      supply_register (regno, buf);
    }
  return;
}

static void
store_inferior_register_1 (regno, regaddr, value)
     int regno;
     unsigned int regaddr;
     int value;
{
  errno = 0;
  ptrace (PT_WUAREA, inferior_pid, (PTRACE_ARG3_TYPE) regaddr, value);
#if 0
  /* HP-UX randomly sets errno to non-zero for regno == 25.
     However, the value is correctly written, so ignore errno. */
  if (errno != 0)
    {
      char string_buf[64];
      
      sprintf (string_buf, "writing register number %d", regno);
      perror_with_name (string_buf);
    }
#endif
  return;
}

static void
store_inferior_register (regno, regaddr)
     register int regno;
     register unsigned int regaddr;
{
#ifndef HPUX_VERSION_5
  if (regno == PS_REGNUM)
    {
      union { int i; short s[2]; } ps_val;
      
      ps_val.i = (ptrace (PT_RUAREA, inferior_pid, (PTRACE_ARG3_TYPE) regaddr,
			  0));
      ps_val.s[0] = (read_register (regno));
      store_inferior_register_1 (regno, regaddr, ps_val.i);
    }
  else
#endif /* not HPUX_VERSION_5 */
    {
      char buf[MAX_REGISTER_RAW_SIZE];
      register int i;
      extern char registers[];
      
      for (i = 0; i < REGISTER_RAW_SIZE (regno); i += sizeof (int))
	{
	  store_inferior_register_1
	    (regno, regaddr,
	     (*(int *) &registers[(REGISTER_BYTE (regno)) + i]));
	  regaddr += sizeof (int);
	}
    }
  return;
}

void
fetch_inferior_registers (regno)
     int regno;
{
  struct user u;
  register unsigned int ar0_offset;
  
  ar0_offset = (INFERIOR_AR0 (u));
  if (regno == -1)
    {
      for (regno = 0; (regno < FP0_REGNUM); regno++)
	fetch_inferior_register (regno, (REGISTER_ADDR (ar0_offset, regno)));
      for (; (regno < NUM_REGS); regno++)
	fetch_inferior_register (regno, (FP_REGISTER_ADDR (u, regno)));
    }
  else
    fetch_inferior_register (regno,
			     (regno < FP0_REGNUM
			      ? REGISTER_ADDR (ar0_offset, regno)
			      : FP_REGISTER_ADDR (u, regno)));
}

/* Store our register values back into the inferior.
   If REGNO is -1, do this for all registers.
   Otherwise, REGNO specifies which register (so we can save time).  */

void
store_inferior_registers (regno)
     register int regno;
{
  struct user u;
  register unsigned int ar0_offset;
  extern char registers[];

  if (regno >= FP0_REGNUM)
    {
      store_inferior_register (regno, (FP_REGISTER_ADDR (u, regno)));
      return;
    }
  
  ar0_offset = (INFERIOR_AR0 (u));
  if (regno >= 0)
    {
      store_inferior_register (regno, (REGISTER_ADDR (ar0_offset, regno)));
      return;
    }

  for (regno = 0; (regno < FP0_REGNUM); regno++)
    store_inferior_register (regno, (REGISTER_ADDR (ar0_offset, regno)));
  for (; (regno < NUM_REGS); regno++)
    store_inferior_register (regno, (FP_REGISTER_ADDR (u, regno)));
  return;
}


/* Take the register values out of a core file and store
   them where `read_register' will find them.  */

#ifdef HPUX_VERSION_5
#define e_PS e_regs[PS]
#define e_PC e_regs[PC]
#endif /* HPUX_VERSION_5 */

void
fetch_core_registers (core_reg_sect, core_reg_size, which, reg_addr)
     char *core_reg_sect;
     int core_reg_size;
     int which;
     unsigned int reg_addr;	/* Unused in this version */
{
  int val, regno;
  struct user u;
  struct exception_stack *pes = (struct exception_stack *) core_reg_sect;
#define es (*pes)
  char *buf;

  if (which == 0) {
    if (core_reg_size < 
		  ((char *) &es.e_offset - (char *) &es.e_regs[R0]))
	  error ("Not enough registers in core file");
    for (regno = 0; (regno < PS_REGNUM); regno++)
      supply_register (regno, &es.e_regs[regno + R0]);
    val = es.e_PS;
    supply_register (regno++, &val);
    supply_register (regno++, &es.e_PC);

  } else if (which == 2) {

    /* FIXME: This may not work if the float regs and control regs are
       discontinuous.  */
    for (regno = FP0_REGNUM, buf = core_reg_sect;
	 (regno < NUM_REGS);
	 buf += REGISTER_RAW_SIZE (regno), regno++)
      {
	supply_register (regno, buf);
      }
  }
}
