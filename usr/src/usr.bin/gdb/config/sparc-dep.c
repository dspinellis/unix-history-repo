/*-
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1991 by Donn Seeley at UUNET Technologies, Inc.
 * Modified 1990 by Van Jacobson at Lawrence Berkeley Laboratory.
 */

#ifndef lint
static char sccsid[] = "@(#)sparc-dep.c	6.4 (Berkeley) 5/8/91";
#endif /* not lint */

/* Machine-dependent code which would otherwise be in inflow.c and core.c,
   for GDB, the GNU debugger.
   Copyright (C) 1986, 1987, 1989 Free Software Foundation, Inc.
   This code is for the sparc cpu.

This file is part of GDB.

GDB is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GDB is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GDB; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include <string.h>
#include "defs.h"
#include "param.h"
#include "frame.h"
#include "inferior.h"
#include "obstack.h"
#include "value.h"

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <fcntl.h>

#include <sys/ptrace.h>
#include <machine/reg.h>

#include <a.out.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/core.h>

#ifdef KERNELDEBUG
#include <kvm.h>
#include <fcntl.h>

extern int kernel_debugging;
kvm_t *kd;

static struct proc *cur_proc;
CORE_ADDR intstack_top;
CORE_ADDR intstack_bottom;

CORE_ADDR kernstack_top;
CORE_ADDR kernstack_bottom;

#endif

extern int errno;
extern int attach_flag;

/* This function simply calls ptrace with the given arguments.  
   It exists so that all calls to ptrace are isolated in this 
   machine-dependent file. */
int
call_ptrace (request, pid, arg3, arg4)
     int request, pid, arg3, arg4;
{
  return ptrace (request, pid, arg3, arg4);
}

void
kill_inferior ()
{
	if (remote_debugging) {
#ifdef KERNELDEBUG
		if (kernel_debugging) {
			/*
			 * It's a very, very bad idea to go away leaving
			 * breakpoints in a remote kernel or to leave it
			 * stopped at a breakpoint. 
			 */
			clear_breakpoints();
		}
#endif
		remote_close(0);
		inferior_died();
	} else if (inferior_pid != 0) {
		ptrace(8, inferior_pid, 0, 0);
		wait(0);
		inferior_died();
	}
}

/* This is used when GDB is exiting.  It gives less chance of error.*/

void
kill_inferior_fast ()
{
	if (remote_debugging) {
#ifdef KERNELDEBUG
		if (kernel_debugging)
			clear_breakpoints();
#endif
		remote_close(0);
	} else if (inferior_pid != 0) {
		ptrace(8, inferior_pid, 0, 0);
		wait(0);
	}
}

/*
 * Simulate single-step ptrace call for sun4.  Code written by Gary
 * Beihl (beihl@mcc.com); modified by Steven McCanne (mccanne@ee.lbl.gov).
 */

union sparcinsn {
	u_long code;
	struct {
		u_int op:2;
		u_int a:1;
		u_int cond:4;
		u_int op2:3;
		u_int disp22:22;
	} b;
};

/*
 * Return the address, other than npc, that could be executed next.
 * If only the possibility is npc, return 0.
 * (There is only one such "other" possible address.)
 */
CORE_ADDR
annulled_dest(insn, pc, npc)
	union sparcinsn insn;
	CORE_ADDR pc, npc;
{
	long int offset;

	if (insn.b.op == 0 && insn.b.a &&
	    (insn.b.op2 == 2 || insn.b.op2 == 6 || insn.b.op2 == 7)) {
		offset = 4 * ((int) (insn.b.disp22 << 10) >> 10);
		if (insn.b.cond == 8)
			return pc + offset;
		else
			return npc + 4;
	}
	return 0;
}

/* 
 * Duplicated from breakpoint.c because (at least for now) this is a
 * machine dependent routine.
 */
static char break_insn[] = BREAKPOINT;

/* From infrun.c */
extern int stop_after_trap, stop_after_attach;

u_long target0_addr;
u_long target0_shadow;
u_long target1_addr;
u_long target1_shadow;

/*
 * Non-zero if we just simulated a single-step ptrace call.  This is
 * needed because we cannot remove the breakpoints in the inferior
 * process until after the `wait' in `wait_for_inferior'. 
 * Used for sun4.
 */
int one_stepped;

void
single_step(signal)
	int signal;
{
	CORE_ADDR pc, target0, target1;
	union sparcinsn insn;
	
	pc = read_register(PC_REGNUM);
	(void)read_memory(pc, &insn.code, 4);

	if (!one_stepped) {
		/*
		 * This is a hack to special case call instructions.
		 * If we are stepping over subroutines, find each call
		 * and trap on return, rather than single step until
		 * wait_for_inferior() discovers that we hit a new routine.
		 * The reason is that stepping over functions in a remote 
		 * kernel can have bad results when the function being 
		 * stepped over is used by the kernel in between traps.
		 * (i.e., a trap instruction gets poked into the function
		 * being stepped over).
		 */
		if (step_over_calls > 0 &&
		    ((insn.code & 0xc0000000) == 0x40000000 || 
		     (insn.code & 0xfff80000) == 0x9fc00000)) {
			target0 = PC_ADJUST(pc);
			target1 = 0;
		} else {
			target0 = read_register(NPC_REGNUM);
			target1 = annulled_dest(insn, pc, target0);
		}
		target0_addr = target0;
		read_memory(target0, &target0_shadow, 4);
		write_memory(target0, break_insn, 4);

		target1_addr = target1;
		if (target1) {
			read_memory(target1, &target1_shadow, 4);
			write_memory(target1, break_insn, 4);
		}
		/*
		 * Resume the inferior.
		 */
		if (remote_debugging)
			remote_resume(0, 0);
		else
			ptrace(7, inferior_pid, 1, signal);
		one_stepped = 1;
	} else {
		/* Remove breakpoints */
		write_memory(target0_addr, &target0_shadow, 4);
		if (target1_addr)
			write_memory(target1_addr, &target1_shadow, 4);
		one_stepped = 0;
	}
}

/* Resume execution of the inferior process.
   If STEP is nonzero, single-step it.
   If SIGNAL is nonzero, give it that signal.  */

void
resume (step, signal)
     int step;
     int signal;
{
	errno = 0;
	if (remote_debugging) {
		/* invalidate the kernel stack limits */
		cur_proc = 0;
		remote_resume(step, signal);
	} else    {
		/* Sparc doesn't have single step on ptrace */
		if (step)
			single_step(signal);
		else
			ptrace(7, inferior_pid, 1, signal);
		if (errno)
			perror_with_name ("ptrace");
	}
}

#ifdef ATTACH_DETACH

/* Start debugging the process whose number is PID.  */

int
attach (pid)
     int pid;
{
  errno = 0;
  ptrace (PTRACE_ATTACH, pid, 0, 0);
  if (errno)
    perror_with_name ("ptrace");
  attach_flag = 1;
  return pid;
}

/* Stop debugging the process whose number is PID
   and continue it with signal number SIGNAL.
   SIGNAL = 0 means just continue it.  */

void
detach (signal)
     int signal;
{
  errno = 0;
  ptrace (PTRACE_DETACH, inferior_pid, 1, signal);
  if (errno)
    perror_with_name ("ptrace");
  attach_flag = 0;
}
#endif /* ATTACH_DETACH */

void
fetch_inferior_registers ()
{
	struct regs inferior_registers;
	struct fp_status inferior_fp_registers;
	extern char registers[];
	int cwp;
	struct rwindow local_and_ins;
	
	if (remote_debugging) {
		remote_fetch_registers(registers);
		return;
	}
	ptrace (PTRACE_GETREGS, inferior_pid, &inferior_registers);
	ptrace (PTRACE_GETFPREGS, inferior_pid, &inferior_fp_registers);
	
	registers[REGISTER_BYTE (0)] = 0;
	bcopy (&inferior_registers.r_g1, &registers[REGISTER_BYTE (1)], 15 * 4);
	bcopy (&inferior_fp_registers, &registers[REGISTER_BYTE (FP0_REGNUM)],
	       sizeof inferior_fp_registers.fpu_fr);
	*(int *)&registers[REGISTER_BYTE (PS_REGNUM)] = inferior_registers.r_ps; 
	*(int *)&registers[REGISTER_BYTE (PC_REGNUM)] = inferior_registers.r_pc;
	*(int *)&registers[REGISTER_BYTE (NPC_REGNUM)] = inferior_registers.r_npc;
	*(int *)&registers[REGISTER_BYTE (Y_REGNUM)] = inferior_registers.r_y;
	/*      *(int *)&registers[REGISTER_BYTE (RP_REGNUM)] =
		inferior_registers.r_o7 + 8;
		bcopy (&inferior_fp_registers.Fpu_fsr,
		&registers[REGISTER_BYTE (FPS_REGNUM)],
		sizeof (FPU_FSR_TYPE)); */
	
	read_inferior_memory (inferior_registers.r_sp,
			      &registers[REGISTER_BYTE (16)],
			      16*4);
}

/* Store our register values back into the inferior.
   If REGNO is -1, do this for all registers.
   Otherwise, REGNO specifies which register (so we can save time).  */

void
store_inferior_registers (regno)
     int regno;
{
  struct regs inferior_registers;
  struct fp_status inferior_fp_registers;
  extern char registers[];

  if (remote_debugging)
    remote_store_registers (registers);
  else
    {
      int in_regs = 1, in_fpregs = 1, in_fparegs, in_cpregs = 1;
  
      if (regno >= 0)
	if (FP0_REGNUM <= regno && regno <= FP0_REGNUM + 32)
	  in_regs = 0;
	else
	  in_fpregs = 0;

      if (in_regs)
	{
	  bcopy (&registers[REGISTER_BYTE (1)],
		 &inferior_registers.r_g1, 15 * 4);

	  inferior_registers.r_ps =
	    *(int *)&registers[REGISTER_BYTE (PS_REGNUM)];
	  inferior_registers.r_pc =
	    *(int *)&registers[REGISTER_BYTE (PC_REGNUM)];
	  inferior_registers.r_npc =
	    *(int *)&registers[REGISTER_BYTE (NPC_REGNUM)];
	  inferior_registers.r_y =
	    *(int *)&registers[REGISTER_BYTE (Y_REGNUM)];

	  write_inferior_memory (*(int *)&registers[REGISTER_BYTE (SP_REGNUM)],
				 &registers[REGISTER_BYTE (16)],
				 16*4);
	}
      if (in_fpregs)
	{
	  bcopy (&registers[REGISTER_BYTE (FP0_REGNUM)],
		 &inferior_fp_registers,
		 sizeof inferior_fp_registers.fpu_fr);

  /*      bcopy (&registers[REGISTER_BYTE (FPS_REGNUM)],
	         &inferior_fp_registers.Fpu_fsr,
		 sizeof (FPU_FSR_TYPE));
  ****/
	}

      if (in_regs)
	ptrace (PTRACE_SETREGS, inferior_pid, &inferior_registers);
      if (in_fpregs)
	ptrace (PTRACE_SETFPREGS, inferior_pid, &inferior_fp_registers);
    }
}

/* NOTE! I tried using PTRACE_READDATA, etc., to read and write memory
   in the NEW_SUN_PTRACE case.
   It ought to be straightforward.  But it appears that writing did
   not write the data that I specified.  I cannot understand where
   it got the data that it actually did write.  */

/* Copy LEN bytes from inferior's memory starting at MEMADDR
   to debugger memory starting at MYADDR. 
   On failure (cannot read from inferior, usually because address is out
   of bounds) returns the value of errno. */

int
read_inferior_memory (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  register int i;
  /* Round starting address down to longword boundary.  */
  register CORE_ADDR addr = memaddr & - sizeof (int);
  /* Round ending address up; get number of longwords that makes.  */
  register int count
    = (((memaddr + len) - addr) + sizeof (int) - 1) / sizeof (int);
  /* Allocate buffer of that many longwords.  */
  register int *buffer = (int *) alloca (count * sizeof (int));
  extern int errno;

  if (remote_debugging)
	  return (remote_read_inferior_memory(memaddr, myaddr, len));
  /* Read all the longwords */
  errno = 0;
  for (i = 0; i < count && errno == 0; i++, addr += sizeof (int))
	buffer[i] = ptrace (1, inferior_pid, addr, 0);
  /* Copy appropriate bytes out of the buffer.  */
  bcopy ((char *) buffer + (memaddr & (sizeof (int) - 1)), myaddr, len);
  return errno;
}

/* Copy LEN bytes of data from debugger memory at MYADDR
   to inferior's memory at MEMADDR.
   On failure (cannot write the inferior)
   returns the value of errno.  */

int
write_inferior_memory (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  register int i;
  /* Round starting address down to longword boundary.  */
  register CORE_ADDR addr = memaddr & - sizeof (int);
  /* Round ending address up; get number of longwords that makes.  */
  register int count
    = (((memaddr + len) - addr) + sizeof (int) - 1) / sizeof (int);
  /* Allocate buffer of that many longwords.  */
  register int *buffer = (int *) alloca (count * sizeof (int));
  extern int errno;

  /* Fill start and end extra bytes of buffer with existing memory data.  */

  if (remote_debugging)
	  return (remote_write_inferior_memory(memaddr, myaddr, len));

  buffer[0] = ptrace (1, inferior_pid, addr, 0);

  if (count > 1)
	buffer[count - 1]
	  = ptrace (1, inferior_pid,
		    addr + (count - 1) * sizeof (int), 0);

  /* Copy data to be written over corresponding part of buffer */

  bcopy (myaddr, (char *) buffer + (memaddr & (sizeof (int) - 1)), len);

  /* Write the entire buffer.  */

  errno = 0;
  for (i = 0; i < count && errno == 0; i++, addr += sizeof (int))
	ptrace (4, inferior_pid, addr, buffer[i]);

  return errno;
}


/* Machine-dependent code which would otherwise be in core.c */
/* Work with core dump and executable files, for GDB. */

#ifndef N_TXTADDR
#define N_TXTADDR(hdr) 0
#endif /* no N_TXTADDR */

#ifndef N_DATADDR
#define N_DATADDR(hdr) hdr.a_text
#endif /* no N_DATADDR */

/* Non-zero if this is an object (.o) file, rather than an executable.
   Distinguishing between the two is rarely necessary (and seems like
   a hack, but there is no other way to get the text and data
   addresses--N_TXTADDR should probably take care of
   this, but it doesn't).  */
/* This definition will not work
   if someone decides to make ld preserve relocation info.  */
#define IS_OBJECT_FILE(hdr) (hdr.a_trsize != 0)

/* Make COFF and non-COFF names for things a little more compatible
   to reduce conditionals later.  */

#ifdef COFF_FORMAT
#define a_magic magic
#endif

#ifndef COFF_FORMAT
#ifndef AOUTHDR
#define AOUTHDR struct exec
#endif
#endif

extern char *sys_siglist[];

/* Hook for `exec_file_command' command to call.  */

extern void (*exec_file_display_hook) ();
   
/* File names of core file and executable file.  */

extern char *corefile;
extern char *execfile;

/* Descriptors on which core file and executable file are open.
   Note that the execchan is closed when an inferior is created
   and reopened if the inferior dies or is killed.  */

extern int corechan;
extern int execchan;

/* Last modification time of executable file.
   Also used in source.c to compare against mtime of a source file.  */

extern int exec_mtime;

/* Virtual addresses of bounds of the two areas of memory in the core file.  */

extern CORE_ADDR data_start;
extern CORE_ADDR data_end;
extern CORE_ADDR stack_start;
extern CORE_ADDR stack_end;

/* Virtual addresses of bounds of two areas of memory in the exec file.
   Note that the data area in the exec file is used only when there is no core file.  */

extern CORE_ADDR text_start;
extern CORE_ADDR text_end;

extern CORE_ADDR exec_data_start;
extern CORE_ADDR exec_data_end;

/* Address in executable file of start of text area data.  */

extern int text_offset;

/* Address in executable file of start of data area data.  */

extern int exec_data_offset;

/* Address in core file of start of data area data.  */

extern int data_offset;

/* Address in core file of start of stack area data.  */

extern int stack_offset;
  
#ifdef COFF_FORMAT
/* various coff data structures */

extern FILHDR file_hdr;
extern SCNHDR text_hdr;
extern SCNHDR data_hdr;

#endif /* not COFF_FORMAT */

/* a.out header saved in core file.  */
  
extern AOUTHDR core_aouthdr;

/* a.out header of exec file.  */

extern AOUTHDR exec_aouthdr;

extern void validate_files ();

extern int (*core_file_hook)();

#ifdef KERNELDEBUG

/*
 * Process control block.
 */
static struct pcb pcb;
/*
 * Floating point unit.
 */
static struct fpu fpu;

/* XXX For misc_function_vector. */
#include "symtab.h" 

/*
 * Read the "thing" at kernel address 'addr' into the space pointed to
 * by point.  The length of the "thing" is determined by the type of p.
 * Result is non-zero if transfer fails.
 */
#define kvread(addr, p) \
    (read_memory((CORE_ADDR)(addr), (char *)(p), sizeof(*(p))))

static CORE_ADDR
ksym_lookup(name)
	char *name;
{
	struct symbol *sym;
	int i;

	if ((i = lookup_misc_func(name)) < 0)
		error("Kernel symbol `%s' not found.", name);

	return (misc_function_vector[i].address);
}

/*
 * Return the current proc.  masterprocp points to
 * current proc which points to current u area.
 */
struct proc *
curProc()
{
	struct proc *p;
	CORE_ADDR addr = ksym_lookup("masterprocp");

	if (kvread(addr, &p))
		error("cannot read proc pointer at %x\n", addr);
	return p;
}

/*
 * (re-)set the variables that make inside_kernstack() work.
 */
set_kernel_boundaries(p)
	struct proc *p;
{
	CORE_ADDR kstack;

	if (intstack_top == 0) {
		intstack_top = ksym_lookup("eintstack");
		intstack_bottom = ksym_lookup("intstack");
	}
	if (kvread(&p->p_segu, &kstack)) 
		error("cannot read kernel stack pointer at %x\n", &p->p_segu);
	kernstack_bottom = kstack;
	kernstack_top = kstack + KERNSTACK;
}

inside_kernstack(addr)
	CORE_ADDR addr;
{
	if (cur_proc == 0) {
		cur_proc = curProc();
		set_kernel_boundaries(cur_proc);
	}
	return (addr > intstack_bottom && addr < intstack_top) ||
		(addr > kernstack_bottom && addr < kernstack_top);
}

clear_regs()
{
	u_long reg = 0;
	float freg = 0.0;
	int i;

	for (i = 0; i < FP0_REGNUM; ++i) 
		supply_register(i, &reg);
	for (; i < FP0_REGNUM + 32; ++i) /* XXX */
		supply_register(i, &freg);
	for (; i < NUM_REGS; ++i) 
		supply_register(i, &reg);
}

static void
read_pcb()
{
	struct user *uaddr;
	int i;
	u_long cps, reg, sp;
	float freg;
	struct rwindow win;

	/* find the pcb for the current process */
	if (kvread(&cur_proc->p_uarea, &uaddr))
		error("cannot u area ptr for proc at 0x%x", cur_proc);
	if (kvread(&uaddr->u_pcb, &pcb))
		error("cannot read pcb at 0x%x", &uaddr->u_pcb);
	/*
	 * Zero out register set then fill in the ones we know about.
	 */
	clear_regs();
	sp = pcb.pcb_sp;
	printf("sp=%x pc=%x sr=%x\n", sp, pcb.pcb_pc, pcb.pcb_psr);
	supply_register(SP_REGNUM, (char *)&pcb.pcb_sp);
	supply_register(PC_REGNUM, (char *)&pcb.pcb_pc);
	/* PC came from o7. */
	supply_register(15, (char *)&pcb.pcb_pc);
	supply_register(PS_REGNUM, (char *)&pcb.pcb_psr);
	/* XXX There should be a WIM_REGNUM. */
	supply_register(66, (char *)&pcb.pcb_uwm);
	/*
	 * Read last register window saved on stack.
	 */
	if (kvread(sp, &win)) {
		printf("cannot read register window at sp=%x\n", pcb.pcb_sp);
		bzero((char *)&win, sizeof win);
	}
	for (i = 0; i < sizeof(win.rw_local); ++i)
		supply_register(i + 16, &win.rw_local[i]);
	for (i = 0; i < sizeof(win.rw_in); ++i)
		supply_register(i + 24, &win.rw_in[i]);
	/*
	 * read the globals & outs saved on the stack (for a trap frame).
	 */
	sp += 92 + 12; /* XXX - MINFRAME + R_Y */
	for (i = 1; i < 14; ++i) {
		u_long val;

		if (kvread(sp + i*4, &val) == 0)
			supply_register(i, (char *)&val);
	}
	if (kvread(pcb.pcb_cpctxp, &cps) == 0)
		supply_register(CPS_REGNUM, (char *)&cps);
}

/*
 * Set the process context to that of the proc structure at
 * system address paddr.
 *
 * This is REALLY STUPID.  The only way to tell libkvm that we want to 
 * change user address maps is with kvm_getu.
 */
set_procaddr(paddr)
	CORE_ADDR paddr;
{
	struct proc p;
	struct user *uaddr;

	if (paddr < KERNELBASE)
		return (1);

	if (cur_proc == NULL)
		if (kvread(ksym_lookup("proc"), &cur_proc))
			error("cannot find proc table");
	if (kvread(paddr, &p))
		error("cannot read proc struct at 0x%x", paddr);
	if (kd)
		if (kvm_getu(kd, (u_long *)&p) == 0) {
			(void)kvread(cur_proc, &p);
			(void)kvm_getu(kd, (u_long *)&p);
			error("cannot read uarea for proc at 0x%x", paddr);
			return (1);
		}
	cur_proc = (struct proc *)paddr;
	read_pcb();
	set_kernel_boundaries(cur_proc);
	return (0);
}

static void
setup_kernel_corefile(namefile, corefile)
	char *namefile, *corefile;
{
	struct stat stb;
	int kmem = 0;
	CORE_ADDR addr, paddr;
	char buf[256], *cp;

	if (strcmp(corefile, "/dev/mem") == 0)
		/* XXX - Sun libkvm botch: this is only way to get
		 * correct mappings for swap set up. */
		kd = kvm_open(namefile, (char *)0, (char *)0, O_RDONLY,
			      (char *)0);
	else
		kd = kvm_open(namefile, corefile, (char *)0, O_RDONLY,
			      (char *)0);
	if (kd == 0) {
		printf("Cannot open '%s' as core file of '%s'\n",
		       corefile, namefile);
		return;
	}
	/*
	 * Need to find current u area to get kernel stack and pcb
	 * where "panic" saved registers.  
	 * (libkvm also needs to know current u area to get user
	 * address space mapping).
	 */
	(void)set_procaddr(curProc());

	/* print out the panic string if there is one */
	if (kvread(ksym_lookup("panicstr"), &addr) || addr == 0 ||
	    read_memory(addr, buf, sizeof(buf)))
		return;

	for (cp = buf; cp < &buf[sizeof(buf)] && *cp; cp++)
		if (!isascii(*cp) || (!isprint(*cp) && !isspace(*cp)))
			*cp = '?';
	*cp = '\0';
	if (buf[0] != '\0')
		printf("panic: %s\n", buf);
}

set_paddr_command(arg)
	char *arg;
{
	u_int paddr, uaddr;
	
	if (!arg)
		error_no_arg("proc address for new current process");
	if (!kernel_debugging)
		error("not debugging kernel");

	paddr = (u_int)parse_and_eval_address(arg);
	if (set_procaddr(paddr))
		error("invalid proc address");

	flush_cached_frames();
	set_current_frame(create_new_frame(read_register(FP_REGNUM),
					   read_pc()));
	select_frame(get_current_frame(), 0);
}

/*
 * read len bytes from kernel virtual address 'addr' into local 
 * buffer 'buf'.  Return 0 if read ok, 1 otherwise.  On read
 * errors, portion of buffer not read is zeroed.
 */
kernel_core_file_hook(addr, buf, len)
	CORE_ADDR addr;
	char *buf;
	int len;
{
	int i, cc;

	if (kd == 0)
		error("no kernel core file");

	cc = kvm_read(kd, (u_long)addr, buf, len);
	if (cc == len)
		return (0);
	if (cc < 0)
		cc = 0;
	bzero(buf, len - cc);
	return (1);
}

static int
is_a_vmunix(name)
	char *name;
{
	register char *cp;

	if (name && (cp = strstr(name, "vmunix")) &&
	    (cp == name || cp[-1] == '/'))
		return (1);
	return (0);
}
#endif

void
core_file_command (filename, from_tty)
     char *filename;
     int from_tty;
{
	int val, sp;
	extern char registers[];
#ifdef KERNELDEBUG
	struct stat stb;

	if (kd != 0) {
		kvm_close(kd);
		kd = 0;
	}
#endif
	/* Discard all vestiges of any previous core file
	   and mark data and stack spaces as empty.  */
	
	if (corefile)
		free (corefile);
	corefile = 0;
	
	if (corechan >= 0)
		close (corechan);
	corechan = -1;
	
	data_start = 0;
	data_end = 0;
	stack_start = STACK_END_ADDR;
	stack_end = STACK_END_ADDR;

	if (filename == 0) {
		if (from_tty)
			printf("No core file now.\n");
		return;
	}
	filename = tilde_expand (filename);
	make_cleanup (free, filename);
      
	if (have_inferior_p ())
		error ("To look at a core file, you must kill the inferior with \"kill\".");
#ifdef KERNELDEBUG
	if (!kernel_debugging && is_a_vmunix(execfile)) {
		kernel_debugging = 1;
		set_prompt_command("(kgdb)");
	}
	if (kernel_debugging) {
		core_file_hook = kernel_core_file_hook;	
		setup_kernel_corefile(execfile, filename);
		goto finish;
	}
#endif
	corechan = open (filename, O_RDONLY, 0);
	if (corechan < 0)
		perror_with_name (filename);
#ifdef KERNELDEBUG
	fstat(corechan, &stb);
	if ((stb.st_mode & S_IFMT) == S_IFCHR &&
	    stb.st_rdev == makedev(3, 1)) {
		/* looking at /dev/kmem */
		data_offset = data_start = KERNELBASE;
		data_end = ~0; /* XXX */
		stack_end = stack_start = data_end;
		set_kernel_boundaries(curProc());
		goto finish;
	} 
#endif
	{
		struct core corestr;
		
		val = myread (corechan, &corestr, sizeof corestr);
		if (val < 0)
			perror_with_name (filename);
		if (corestr.c_magic != CORE_MAGIC)
			error ("\"%s\" does not appear to be a core dump file (magic 0x%x, expected 0x%x)",
			       filename, corestr.c_magic, (int) CORE_MAGIC);
		else if (sizeof (struct core) != corestr.c_len)
			error ("\"%s\" has an invalid struct core length (%d, expected %d)",
			       filename, corestr.c_len, (int) sizeof (struct core));
		
		data_start = exec_data_start;
		data_end = data_start + corestr.c_dsize;
		stack_start = stack_end - corestr.c_ssize;
		data_offset = sizeof corestr;
		stack_offset = sizeof corestr + corestr.c_dsize;
		
		/* G0 *always* holds 0.  */
		*(int *)&registers[REGISTER_BYTE (0)] = 0;
		/* The globals and output registers.  */
		
		bcopy (&corestr.c_regs.r_g1, ((int *) registers) + 1, 15 * 4);
		*(int *)&registers[REGISTER_BYTE (PS_REGNUM)] = corestr.c_regs.r_ps;
		*(int *)&registers[REGISTER_BYTE (PC_REGNUM)] = corestr.c_regs.r_pc;
		*(int *)&registers[REGISTER_BYTE (NPC_REGNUM)] = corestr.c_regs.r_npc;
		*(int *)&registers[REGISTER_BYTE (Y_REGNUM)] = corestr.c_regs.r_y;
		
		/* My best guess at where to get the locals and input
		   registers is exactly where they usually are, right above
		   the stack pointer.  If the core dump was caused by a bus
		   writing off the stack pointer (as is possible) then this
		   won't work, but it's worth the try. */
		
		sp = *(int *)&registers[REGISTER_BYTE (SP_REGNUM)];
		lseek (corechan, sp - stack_start + stack_offset, L_SET);
		if (16 * 4 != myread (corechan,
				      &registers[REGISTER_BYTE (16)],
				      16 * 4))
			/* fprintf so user can still use gdb */
			fprintf (stderr, "Couldn't read input and local registers from core file\n");
		
		bcopy (corestr.c_fpu.fpu_regs,
		       &registers[REGISTER_BYTE (FP0_REGNUM)],
		       sizeof corestr.c_fpu.fpu_regs);
#ifdef FPU
		bcopy (&corestr.c_fpu.fpu_fsr,
		       &registers[REGISTER_BYTE (FPS_REGNUM)],
		       sizeof (FPU_FSR_TYPE));
#endif
		
		bcopy (&corestr.c_aouthdr, &core_aouthdr, sizeof (struct exec));
		
		printf ("Core file is from \"%s\".\n", corestr.c_cmdname);
		if (corestr.c_signo > 0)
			printf ("Program terminated with signal %d, %s.\n",
				corestr.c_signo,
				corestr.c_signo < NSIG
				? sys_siglist[corestr.c_signo]
				: "(undocumented)");
	}
 finish:
	if (filename[0] == '/')
		corefile = savestring (filename, strlen (filename));
	else
		corefile = concat (current_directory, "/", filename);
	
	set_current_frame ( create_new_frame (read_register (FP_REGNUM),
					      read_pc ()));
	select_frame (get_current_frame (), 0);
	validate_files ();
}

void
exec_file_command (filename, from_tty)
     char *filename;
     int from_tty;
{
  int val;

  /* Eliminate all traces of old exec file.
     Mark text segment as empty.  */

  if (execfile)
    free (execfile);
  execfile = 0;
  data_start = 0;
  data_end -= exec_data_start;
  text_start = 0;
  text_end = 0;
  exec_data_start = 0;
  exec_data_end = 0;
  if (execchan >= 0)
    close (execchan);
  execchan = -1;

  /* Now open and digest the file the user requested, if any.  */

  if (filename)
    {
      filename = tilde_expand (filename);
      make_cleanup (free, filename);
      
      execchan = openp (getenv ("PATH"), 1, filename, O_RDONLY, 0,
			&execfile);
      if (execchan < 0)
	perror_with_name (filename);

#ifdef COFF_FORMAT
      {
	int aout_hdrsize;
	int num_sections;

	if (read_file_hdr (execchan, &file_hdr) < 0)
	  error ("\"%s\": not in executable format.", execfile);

	aout_hdrsize = file_hdr.f_opthdr;
	num_sections = file_hdr.f_nscns;

	if (read_aout_hdr (execchan, &exec_aouthdr, aout_hdrsize) < 0)
	  error ("\"%s\": can't read optional aouthdr", execfile);

	if (read_section_hdr (execchan, _TEXT, &text_hdr, num_sections,
			      aout_hdrsize) < 0)
	  error ("\"%s\": can't read text section header", execfile);

	if (read_section_hdr (execchan, _DATA, &data_hdr, num_sections,
			      aout_hdrsize) < 0)
	  error ("\"%s\": can't read data section header", execfile);

	text_start = exec_aouthdr.text_start;
	text_end = text_start + exec_aouthdr.tsize;
	text_offset = text_hdr.s_scnptr;
	exec_data_start = exec_aouthdr.data_start;
	exec_data_end = exec_data_start + exec_aouthdr.dsize;
	exec_data_offset = data_hdr.s_scnptr;
	data_start = exec_data_start;
	data_end += exec_data_start;
	exec_mtime = file_hdr.f_timdat;
      }
#else /* not COFF_FORMAT */
      {
	struct stat st_exec;
	val = myread (execchan, &exec_aouthdr, sizeof (AOUTHDR));

	if (val < 0)
	  perror_with_name (filename);

	if (kernel_debugging)
		text_start = exec_aouthdr.a_entry;
	else
		text_start = IS_OBJECT_FILE (exec_aouthdr) ? 
			0 : N_TXTADDR (exec_aouthdr);

	exec_data_start = IS_OBJECT_FILE (exec_aouthdr)
		? exec_aouthdr.a_text : N_DATADDR (exec_aouthdr);
	text_offset = N_TXTOFF (exec_aouthdr);
	exec_data_offset = N_DATOFF (exec_aouthdr);

	text_end = text_start + exec_aouthdr.a_text;
        exec_data_end = exec_data_start + exec_aouthdr.a_data;
	data_start = exec_data_start;
	data_end += exec_data_start;

	fstat (execchan, &st_exec);
	exec_mtime = st_exec.st_mtime;
      }
#endif /* not COFF_FORMAT */

      validate_files ();
    }
  else if (from_tty)
    printf ("No exec file now.\n");

  /* Tell display code (if any) about the changed file name.  */
  if (exec_file_display_hook)
    (*exec_file_display_hook) (filename);
}

#ifndef offsetof
#define offsetof(t, f)  ((int)(&((t *)0)->f))
#endif

/*
 * Return the address of the saved pc in frame.
 */
CORE_ADDR
addr_of_pc(frame)
	struct frame_info *frame;
{
	CORE_ADDR addr;

#ifdef KERNELDEBUG
	/*
	 * If we are kernel debugging, we must special case trap frames.
	 * We can tell if we are a trap frame by looking at the return 
	 * address of the frame below us.  If it is in locore, then
	 * we are such a frame and we can find our saved pc in %l1.
	 */
	if (kernel_debugging && frame->next) {
		static CORE_ADDR locore_h, locore_t;
	
		if (locore_h == 0) {
			locore_h = ksym_lookup("sys_trap");
			locore_t = ksym_lookup("kadb_tcode");
		}
		addr = GET_RWINDOW_REG(frame->next->bottom, rw_in[7]);
		if (addr > locore_h && addr < locore_t)
			return frame->bottom + 
				offsetof(struct rwindow, rw_local[1]);
	}
#endif
	return (CORE_ADDR)&((struct rwindow *)frame->bottom)->rw_in[7];
}

/*
 * Find the pc saved in frame FRAME.  
 */
CORE_ADDR
frame_saved_pc(frame)
	FRAME frame;
{
	return PC_ADJUST(read_memory_integer(addr_of_pc(frame), 4));
}

/*
 * Since an individual frame in the frame cache is defined by two
 * arguments (a frame pointer and a stack pointer), we need two
 * arguments to get info for an arbitrary stack frame.  This routine
 * takes two arguments and makes the cached frames look as if these
 * two arguments defined a frame on the cache.  This allows the rest
 * of info frame to extract the important arguments without
 * difficulty. 
 */
FRAME
setup_arbitrary_frame (frame, stack)
     FRAME_ADDR frame, stack;
{
  struct frame_info *fci;
  FRAME fid = create_new_frame (frame, 0);

  if (!fid)
    fatal ("internal: create_new_frame returned invalid frame id");
  
  fid->bottom = stack;

  return fid;
}

/* This code was written by Gary Beihl (beihl@mcc.com).
   It was modified by Michael Tiemann (tiemann@corto.inria.fr). */

struct command_line *get_breakpoint_commands ();

/*
 * This routine takes a program counter value.  It restores the
 * register window system to the frame above the current one, and sets
 * the pc and npc to the correct values.
 */

/*    The following insns translate to:
 
 	restore
 	t g0,0x1,o0
 	sethi %hi(0x0), g0	*/

static int restore_insn_opcodes[] = { 0x81e80000, 0x91d02001, 0x01000000 };

void
do_restore_insn (pc)
     CORE_ADDR pc;
{
  CORE_ADDR sp = read_register (SP_REGNUM);
  CORE_ADDR npc = pc + 4;
  CORE_ADDR fake_pc = sp - sizeof (restore_insn_opcodes);
  struct inferior_status inf_status;

  save_inferior_status (&inf_status, 0); /* Don't restore stack info */

  if (!pc)
    abort();

  write_memory (fake_pc, restore_insn_opcodes, sizeof (restore_insn_opcodes));

  clear_proceed_status ();
  stop_after_trap = 1;
  proceed (fake_pc, 0, 0);

  write_register (PC_REGNUM, pc);
  write_register (NPC_REGNUM, npc);
  restore_inferior_status (&inf_status);
}

/*
 * This routine should be more specific in it's actions; making sure
 * that it uses the same register in the initial prologue section.
 */
CORE_ADDR 
skip_prologue (pc)
     CORE_ADDR pc;
{
  union
    {
      unsigned long int code;
      struct
	{
	  unsigned int op:2;
	  unsigned int rd:5;
	  unsigned int op2:3;
	  unsigned int imm22:22;
	} sethi;
      struct
	{
	  unsigned int op:2;
	  unsigned int rd:5;
	  unsigned int op3:6;
	  unsigned int rs1:5;
	  unsigned int i:1;
	  unsigned int simm13:13;
	} add;
      int i;
    } x;
  int dest = -1;

  x.i = read_memory_integer (pc, 4);

  /* Recognize the `sethi' insn and record its destination.  */
  if (x.sethi.op == 0 && x.sethi.op2 == 4)
    {
      dest = x.sethi.rd;
      pc += 4;
      x.i = read_memory_integer (pc, 4);
    }

  /* Recognize an add immediate value to register to either %g1 or
     the destination register recorded above.  Actually, this might
     well recognize several different arithmetic operations.  */
  if (x.add.op == 2 && x.add.i && (x.add.rd == 1 || x.add.rd == dest))
    {
      pc += 4;
      x.i = read_memory_integer (pc, 4);
    }

  /* This recognizes any SAVE insn.  But why do the XOR and then
     the compare?  That's identical to comparing against 60 (as long
     as there isn't any sign extension).  */
  if (x.add.op == 2 && (x.add.op3 ^ 32) == 28)
    {
      pc += 4;
      x.i = read_memory_integer (pc, 4);
    }

  /* Now we need to recognize stores into the frame from the input
     registers.  This recognizes all non alternate stores of input
     register, into a location offset from the frame pointer.  */
  while (x.add.op == 3
	 && (x.add.op3 & 0x3c) == 4 /* Store, non-alternate.  */
	 && (x.add.rd & 0x18) == 0x18 /* Input register.  */
	 && x.add.i		/* Immediate mode.  */
	 && x.add.rs1 == 30	/* Off of frame pointer.  */
	 /* Into reserved stack space.  */
	 && x.add.simm13 >= 0x44
	 && x.add.simm13 < 0x5b)
    {
      pc += 4;
      x.i = read_memory_integer (pc, 4);
    }
  return pc;
}

int dummy_code[] = {
	0xd003a044,		/* ld	[%sp + 68], %o0 */
	0xd203a048,		/* ld	[%sp + 72], %o1 */
	0xd403a04c,		/* ld	[%sp + 76], %o2 */
	0xd603a050,		/* ld	[%sp + 80], %o3 */
	0xd803a054,		/* ld	[%sp + 84], %o4 */
#define DUMMY_CALL_INDEX 5
	0x40000000,		/* call .		*/
	0xda03a058,		/* ld	[%sp + 88], %o5 */
	0x01000000,		/* nop - extra insn for Sun cc */
	0x91d02001,		/* ta	1		*/
};

/*
 * Leave room on the stack for the kernel save area and the pointer
 * for structure return values.
 */
#define KSA_AND_STRUCT_ADJUST 68

/*
 * Build `dummy' call instructions on inferior's stack to cause
 * it to call a subroutine.
 *
 * N.B. - code in wait_for_inferior requires that sp < pc < fp when
 * we take the trap 2 above so it will recognize that we stopped
 * at a `dummy' call.  So, after the call sp is *not* decremented
 * to clean the arguments, code & other stuff we lay on the stack.
 * Since the regs are restored to saved values at the breakpoint,
 * sp will get reset correctly.  Also, this restore means we don't
 * have to construct frame linkage info to save pc & fp.  The lack
 * of frame linkage means we can't do a backtrace, etc., if the
 * called function gets a fault or hits a breakpoint but code in
 * run_stack_dummy makes this impossible anyway.
 */
CORE_ADDR
setup_dummy(sp, funaddr, nargs, args, struct_return_bytes, pushfn)
	CORE_ADDR sp;
	CORE_ADDR funaddr;
	int nargs;
	value *args;
	int struct_return_bytes;
	CORE_ADDR (*pushfn)();
{
	int len, padding, i;
	CORE_ADDR top = sp, struct_addr, pc;

	pc = sp - sizeof(dummy_code);
	len = arg_stacklen(nargs, args) + KSA_AND_STRUCT_ADJUST +
		sizeof(dummy_code) + struct_return_bytes;
	padding = STACK_ALIGN(len) - len;
	sp = pc - padding - struct_return_bytes;
	struct_addr = sp;
	for (i = 0; i < nargs; ++i) {
		/* pushfn doesn't actually change SP_REGNUM */
		sp = (*pushfn)(sp, args[i]);
	}
	sp -= KSA_AND_STRUCT_ADJUST;
	if (struct_return_bytes)
		write_memory(sp + KSA_AND_STRUCT_ADJUST - 4,
			     (char *)&struct_addr, 4);

	write_register(SP_REGNUM, sp);
	dummy_code[DUMMY_CALL_INDEX] = 0x40000000 | 
		((unsigned)(funaddr - (pc + 4 * DUMMY_CALL_INDEX)) >> 2);

	write_memory(pc, (char *)dummy_code, sizeof(dummy_code));

	return pc;
}

int default_function_nargs = 4;

set_default_funargs_command(arg)
	char *arg;
{
	if (arg == 0 || arg[0] == 0)
		printf("%d\n", default_function_nargs);
	else
		default_function_nargs = atoi(arg);
}

frame_find_saved_regs(fi, srp)
	struct frame_info *fi;
	struct frame_saved_regs *srp;
{
	register int i;
	register CORE_ADDR pc;

	FRAME_ADDR frame = read_register (FP_REGNUM);
	FRAME fid = FRAME_INFO_ID (fi);
	if (!fid) fatal ("Bad frame info struct in FRAME_FIND_SAVED_REGS");
	bzero ((char *)srp, sizeof *srp);
	if (fi->pc >= (fi->bottom ? fi->bottom :
			 read_register (SP_REGNUM)) &&
	    fi->pc <= FRAME_FP(fi)) {

		for (i = 1; i < 8; i++)
			srp->regs[i] = frame + i * 4 - 0xa0;
		for (i = 24; i < 32; i++)
			srp->regs[i] = frame + (i - 24) * 4 - 0xc0;
		for (i = FP0_REGNUM; i < FP0_REGNUM + 32; i++)
			srp->regs[i] = frame + (i - FP0_REGNUM) * 4 - 0x80;
		for (i = 64; i < NUM_REGS; i++)
			srp->regs[i] = frame + (i - 64) * 4 - 0xe0;
		frame = fi->bottom ? fi->bottom : read_register (SP_REGNUM);
	} else {
		frame = fi->bottom ? fi->bottom : read_register (SP_REGNUM);
		for (i = 16; i < 32; i++)
			srp->regs[i] = frame + (i-16) * 4;
	}
	if (fi->next) {
		/* Pull off either the next frame pointer or
		   the stack pointer */
		FRAME_ADDR next_next_frame =(fi->next->bottom ?
					     fi->next->bottom :
					     read_register (SP_REGNUM));
		for (i = 8; i < 16; i++)
			srp->regs[i] = next_next_frame + i * 4;
	}
	/* Otherwise, whatever we would get from ptrace(GETREGS) */
	/* is accurate */
	for (i = 30; i < 32; i++)
		srp->regs[i] = frame + (i-16) * 4;
	srp->regs[SP_REGNUM] = FRAME_FP (fi);
	srp->regs[PC_REGNUM] =
#ifdef KERNELDEBUG
		kernel_debugging ? addr_of_pc(fi) :
#endif
		frame + 15*4;
}

extern struct cmd_list_element *setlist;

#ifdef KERNELDEBUG
void
	_initialize_sparc_dep()
{
	add_com ("process-address", class_obscure, set_paddr_command,
"The process with proc structure at ADDR becomes the\n\
\"current\" process context for kernel debugging.");
	add_com_alias ("paddr", "process-address", class_obscure, 0);
	add_cmd("default-funargs", class_support,
		set_default_funargs_command,
"Set the number of arguments to be printed for functions with\n\
no debugging info.\n",
		&setlist);
}
#endif
