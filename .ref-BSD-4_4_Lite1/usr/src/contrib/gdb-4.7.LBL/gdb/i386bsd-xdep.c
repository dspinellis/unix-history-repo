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
