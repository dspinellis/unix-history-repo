/* Native-dependent code for BSD Unix running on i386's, for GDB.
   Copyright 1988, 1989, 1991, 1992 Free Software Foundation, Inc.

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
#include <machine/reg.h>

/* this table must line up with REGISTER_NAMES in tm-i386.h */
/* symbols like 'tEAX' come from <machine/reg.h> */
static int tregmap[] = 
{
  tEAX, tECX, tEDX, tEBX,
  tESP, tEBP, tESI, tEDI,
  tEIP, tEFLAGS, tCS, tSS
};
static int sregmap[] = 
{
  sEAX, sECX, sEDX, sEBX,
  sESP, sEBP, sESI, sEDI,
  sEIP, sEFLAGS, sCS, sSS
};

/* blockend is the value of u.u_ar0, and points to the
   place where ES is stored.  */

int
i386_register_u_addr (blockend, regnum)
     int blockend;
     int regnum;
{
  /* The following condition is a kludge to get at the proper register map
     depending upon the state of pcb_flag.
     The proper condition would be
     if (u.u_pcb.pcb_flag & FM_TRAP)
     but that would require a ptrace call here and wouldn't work
     for corefiles.  */

  if (blockend < 0x1fcc)
    return (blockend + 4 * tregmap[regnum]);
  else
    return (blockend + 4 * sregmap[regnum]);
}

/*XXX!!!*/
/*	BSDI $Id: i386bsd-xdep.c,v 1.2 1992/09/02 17:43:24 trent Exp $	*/

/*
 * GDB support code for BSD/386.
 */

#include <sys/param.h>
#include <sys/ptrace.h>
#include <sys/user.h>
#include <machine/frame.h>
#include <machine/reg.h>

#include "defs.h"
#include "inferior.h"

#if !defined (offsetof)
#define offsetof(TYPE, MEMBER) ((unsigned long) &((TYPE *)0)->MEMBER)
#endif

static int trapmap[] = {
	tEAX, tECX, tEDX, tEBX, tESP, tEBP, tESI, tEDI,
	tEIP, tEFLAGS, tCS, tSS, tDS, tES, tES, tES
};

static int syscmap[] = {
	sEAX, sECX, sEDX, sEBX, sESP, sEBP, sESI, sEDI,
	sEIP, sEFLAGS, sCS, sSS, sCS, sCS, sCS, sCS
};

static int *regmap;
static unsigned long *reg_offset;

/*
 * There aren't that many registers -- might as well fetch them all,
 * and avoid repeatedly paying for those pcb and kproc reads.
 */
void
fetch_inferior_registers(int regno)
{
	int i;
	int val;

	if (ptrace(PT_READ_U, inferior_pid, 
	    (caddr_t)offsetof(struct user, u_pcb.pcb_flags), 0) & FM_TRAP)
		regmap = trapmap;
	else
		regmap = syscmap;

	reg_offset = (unsigned long *)
	    (ptrace(PT_READ_U, inferior_pid, (caddr_t)
	     offsetof(struct user, u_kproc.kp_proc.p_regs), 0) -
	     VM_MAXUSER_ADDRESS);
	for (i = 0; i < NUM_REGS; ++i) {
		val = ptrace(PT_READ_U, inferior_pid,
		    (caddr_t)&reg_offset[regmap[i]], 0);
		supply_register(i, (char *)&val);
	}

	/* FP registers? */
}

/*
 * Should we bother to defer any stores?
 */
void
store_inferior_registers(int regno)
{
	int i;

	if (regno >= 0) {
		ptrace(PT_WRITE_U, inferior_pid, (caddr_t)
		    &reg_offset[regmap[regno]],
		    *(int *)&registers[REGISTER_BYTE(regno)]);
		return;
	}

	for (i = 0; i < NUM_REGS; ++i)
		ptrace(PT_WRITE_U, inferior_pid, (caddr_t)
		    &reg_offset[regmap[i]],
		    *(int *)&registers[REGISTER_BYTE(i)]);
}

void
fetch_core_registers(char *upage, unsigned size, int which, unsigned reg_addr)
{
	struct user *up;
	unsigned long *core_regs;
	int i;

	if (which)
		return;

	up = (struct user *)upage;
	core_regs = (unsigned long *)(upage +
	    ((unsigned long)up->u_kproc.kp_proc.p_regs - VM_MAXUSER_ADDRESS));
	regmap = up->u_pcb.pcb_flags & FM_TRAP ? trapmap : syscmap;
	for (i = 0; i < NUM_REGS; ++i)
		supply_register(i, (char *)&core_regs[regmap[i]]);
}
