/* Intel 386 native support.
   Copyright (C) 1988, 1989, 1991, 1992 Free Software Foundation, Inc.

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
#include "language.h"
#include "gdbcore.h"

#ifdef USG
#include <sys/types.h>
#endif

#include <sys/param.h>
#include <sys/dir.h>
#include <signal.h>
#include <sys/user.h>
#include <sys/ioctl.h>
#include <fcntl.h>

#include <sys/file.h>
#include <sys/stat.h>

#include <sys/reg.h>
#include "ieee-float.h"

#include "target.h"

extern struct ext_format ext_format_i387;

/* this table must line up with REGISTER_NAMES in tm-i386v.h */
/* symbols like 'EAX' come from <sys/reg.h> */
static int regmap[] = 
{
  EAX, ECX, EDX, EBX,
  UESP, EBP, ESI, EDI,
  EIP, EFL, CS, SS,
  DS, ES, FS, GS,
};

/* blockend is the value of u.u_ar0, and points to the
 * place where GS is stored
 */

int
i386_register_u_addr (blockend, regnum)
     int blockend;
     int regnum;
{
#if 0
  /* this will be needed if fp registers are reinstated */
  /* for now, you can look at them with 'info float'
   * sys5 wont let you change them with ptrace anyway
   */
  if (regnum >= FP0_REGNUM && regnum <= FP7_REGNUM) 
    {
      int ubase, fpstate;
      struct user u;
      ubase = blockend + 4 * (SS + 1) - KSTKSZ;
      fpstate = ubase + ((char *)&u.u_fpstate - (char *)&u);
      return (fpstate + 0x1c + 10 * (regnum - FP0_REGNUM));
    } 
  else
#endif
    return (blockend + 4 * regmap[regnum]);
  
}

#if 0
/* using FLOAT_INFO as is would be a problem.  FLOAT_INFO is called
   via a command xxx and eventually calls ptrace without ever having
   traversed the target vector.  This would be terribly impolite
   behaviour for a sun4 hosted remote gdb.

   A fix might be to move this code into the "info registers" command.
   rich@cygnus.com 15 Sept 92. */
i386_float_info ()
{
  struct user u; /* just for address computations */
  int i;
  /* fpstate defined in <sys/user.h> */
  struct fpstate *fpstatep;
  char buf[sizeof (struct fpstate) + 2 * sizeof (int)];
  unsigned int uaddr;
  char fpvalid = 0;
  unsigned int rounded_addr;
  unsigned int rounded_size;
  extern int corechan;
  int skip;
  
  uaddr = (char *)&u.u_fpvalid - (char *)&u;
  if (target_has_execution)
    {
      unsigned int data;
      unsigned int mask;
      
      rounded_addr = uaddr & -sizeof (int);
      data = ptrace (3, inferior_pid, (PTRACE_ARG3_TYPE) rounded_addr, 0);
      mask = 0xff << ((uaddr - rounded_addr) * 8);
      
      fpvalid = ((data & mask) != 0);
    } 
#if 0
  else 
    {
      if (lseek (corechan, uaddr, 0) < 0)
	perror ("seek on core file");
      if (myread (corechan, &fpvalid, 1) < 0) 
	perror ("read on core file");
      
    }
#endif	/* no core support yet */
  
  if (fpvalid == 0) 
    {
      printf ("no floating point status saved\n");
      return;
    }
  
  uaddr = (char *)&U_FPSTATE(u) - (char *)&u;
  if (target_has_execution)
    {
      int *ip;
      
      rounded_addr = uaddr & -sizeof (int);
      rounded_size = (((uaddr + sizeof (struct fpstate)) - uaddr) +
		      sizeof (int) - 1) / sizeof (int);
      skip = uaddr - rounded_addr;
      
      ip = (int *)buf;
      for (i = 0; i < rounded_size; i++) 
	{
	  *ip++ = ptrace (3, inferior_pid, (PTRACE_ARG3_TYPE) rounded_addr, 0);
	  rounded_addr += sizeof (int);
	}
    } 
#if 0
  else 
    {
      if (lseek (corechan, uaddr, 0) < 0)
	perror_with_name ("seek on core file");
      if (myread (corechan, buf, sizeof (struct fpstate)) < 0) 
	perror_with_name ("read from core file");
      skip = 0;
    }
#endif	/* 0 */ 

  fpstatep = (struct fpstate *)(buf + skip);
  print_387_status (fpstatep->status, (struct env387 *)fpstatep->state);
}

#endif /* never */
