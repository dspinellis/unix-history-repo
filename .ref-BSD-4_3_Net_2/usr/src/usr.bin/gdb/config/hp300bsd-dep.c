/*-
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1991 by Donn Seeley at UUNET Technologies, Inc.
 * Modified 1990 by Van Jacobson at Lawrence Berkeley Laboratory.
 */

#ifndef lint
static char sccsid[] = "@(#)hp300bsd-dep.c	6.10 (Berkeley) 5/12/91";
#endif /* not lint */

/*
 * Machine-dependent code for a Hewlett-Packard 9000/300, running bsd.
 * Copyright (C) 1986, 1987, 1989 Free Software Foundation, Inc. 
 *
 * This file is part of GDB. 
 *
 * GDB is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 1, or (at your option) any later version. 
 *
 * GDB is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. 
 *
 * You should have received a copy of the GNU General Public License along with
 * GDB; see the file COPYING.  If not, write to the Free Software Foundation,
 * 675 Mass Ave, Cambridge, MA 02139, USA.  
 */
#ifndef lint
static char rcsid[] = "$Header: hp300bsd-dep.c,v 1.6 91/03/13 01:04:43 mccanne Exp $";
#endif lint

#include <stdio.h>
#include "defs.h"
#include "param.h"
#include "frame.h"
#include "inferior.h"
#include "value.h"


#include <sys/param.h>
#include <sys/dir.h>
#include <signal.h>
#include <sys/ioctl.h>
/* #include <fcntl.h>  Can we live without this?  */

#include <a.out.h>
#ifndef N_SET_MAGIC
#define N_SET_MAGIC(exec, val) ((exec).a_magic = (val))
#endif

#ifdef NEWVM
#include <hp300/hp300/pte.h>
#endif

#include <sys/time.h>
#include <sys/resource.h>
#include <sys/uio.h>
#include <sys/user.h>		/* After a.out.h  */
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/ptrace.h>

CORE_ADDR       kernel_u_addr;

#ifdef KERNELDEBUG
#ifndef NEWVM
#include <sys/vmmac.h>
#include <machine/pte.h>
#endif
#include <machine/vmparam.h>
#include <machine/cpu.h>
#include <ctype.h>
#include "symtab.h"	/* XXX */

extern int kernel_debugging;

#define	KERNOFF		((unsigned)KERNBASE)
#define	LOWRAM		((unsigned)0xfffffdce)
/* actually you can't really distinguish user and kernel by address */
#define	INKERNEL(x)	((x) >= KERNOFF && (x) < KERNOFF + ctob(slr))
#define INUDOT(x)	\
	((x) >= KERNEL_U_ADDR && (x) < KERNEL_U_ADDR + ctob(UPAGES))

#define	PT_ADDR_ANY	((caddr_t) 1)

/*
 * Convert from sysmap pte index to system virtual address & vice-versa.
 * (why aren't these in one of the system vm macro files???)
 */
#define smxtob(a)       (sbr + (a) * sizeof(pte))
#define btosmx(b)       (((b) - sbr) / sizeof(pte))

static int ok_to_cache();
#ifdef NEWVM
static int found_pcb;
static CORE_ADDR curpcb;
static CORE_ADDR kstack;
#endif
#endif

extern int      errno;

/*
 * This function simply calls ptrace with the given arguments.  It exists so
 * that all calls to ptrace are isolated in this machine-dependent file. 
 */
int
call_ptrace(request, pid, arg3, arg4)
	int request;
	pid_t pid;
	caddr_t arg3;
	int arg4;
{
	return(ptrace(request, pid, arg3, arg4));
}

kill_inferior()
{
	if (remote_debugging) {
#ifdef KERNELDEBUG
		if (kernel_debugging)
			/*
			 * It's a very, very bad idea to go away leaving
			 * breakpoints in a remote kernel or to leave it
			 * stopped at a breakpoint. 
			 */
			clear_breakpoints();
#endif
		remote_close(0);
		inferior_died();
	} else if (inferior_pid != 0) {
		ptrace(PT_KILL, inferior_pid, 0, 0);
		wait(0);
		inferior_died();
	}
}

/*
 * This is used when GDB is exiting.  It gives less chance of error.
 */
kill_inferior_fast()
{
	if (remote_debugging) {
#ifdef KERNELDEBUG
		if (kernel_debugging)
			clear_breakpoints();
#endif
		remote_close(0);
		return;
	}
	if (inferior_pid == 0)
		return;

	ptrace(PT_KILL, inferior_pid, 0, 0);
	wait(0);
}

/*
 * Resume execution of the inferior process. If STEP is nonzero, single-step
 * it. If SIGNAL is nonzero, give it that signal.  
 */
void
resume(step, signal)
	int step;
	int signal;
{
	errno = 0;
	if (remote_debugging)
		remote_resume(step, signal);
	else {
		ptrace(step ? PT_STEP : PT_CONTINUE, inferior_pid,
		       PT_ADDR_ANY, signal);
		if (errno)
			perror_with_name("ptrace");
	}
}

#ifdef ATTACH_DETACH
extern int attach_flag;

/*
 * Start debugging the process whose number is PID.
 */
attach(pid)
	int pid;
{
	errno = 0;
	ptrace(PT_ATTACH, pid, 0, 0);
	if (errno)
		perror_with_name("ptrace");
	attach_flag = 1;
	return pid;
}

/*
 * Stop debugging the process whose number is PID and continue it
 * with signal number SIGNAL.  SIGNAL = 0 means just continue it.  
 */
void
detach(signal)
	int signal;
{
	errno = 0;
	ptrace(PT_DETACH, inferior_pid, PT_ADDR_ANY, signal);
	if (errno)
		perror_with_name("ptrace");
	attach_flag = 0;
}
#endif	/* ATTACH_DETACH */

static unsigned int
get_register_offset()
{
	unsigned int offset;
	struct user u;	/* XXX */

#ifdef NEWVM
	offset = (char *) &u.u_kproc.kp_proc.p_regs - (char *) &u;
	offset = ptrace(PT_READ_U, inferior_pid, (caddr_t)offset, 0) -
		USRSTACK;
#else
	offset = (char *) &u.u_ar0 - (char *) &u;
	offset = ptrace(PT_READ_U, inferior_pid, (caddr_t)offset, 0) -
		KERNEL_U_ADDR;
#endif
	return offset;
}

void
fetch_inferior_registers()
{
	register int    regno;
	register unsigned int regaddr;
	char            buf[MAX_REGISTER_RAW_SIZE];
	register int    i;
	unsigned int	offset;

	if (remote_debugging) {
		extern char     registers[];

		remote_fetch_registers(registers);
		return;
	}

	offset = get_register_offset();

	for (regno = 0; regno < NUM_REGS; regno++) {
		regaddr = register_addr(regno, offset);
		for (i = 0; i < REGISTER_RAW_SIZE(regno); i += sizeof(int)) {
			*(int *)&buf[i] = ptrace(PT_READ_U, inferior_pid, 
						 (caddr_t)regaddr, 0);
			regaddr += sizeof(int);
		}
		supply_register(regno, buf);
	}
}

/*
 * Store our register values back into the inferior. If REGNO is -1, do this
 * for all registers. Otherwise, REGNO specifies which register (so we can
 * save time).  
 */
store_inferior_registers(regno)
	int             regno;
{
	register unsigned int regaddr;
	char            buf[80];
	extern char     registers[];
	register int    i;
	unsigned int	offset;

	if (remote_debugging) {
		extern char     registers[];

		remote_store_registers(registers);
		return;
	}

	offset = get_register_offset();

	if (regno >= 0) {
		regaddr = register_addr(regno, offset);
		for (i = 0; i < REGISTER_RAW_SIZE(regno); i += sizeof(int)) {
			errno = 0;
			ptrace(PT_WRITE_U, inferior_pid, (caddr_t)regaddr,
			     *(int *) &registers[REGISTER_BYTE(regno) + i]);
			if (errno != 0) {
				sprintf(buf, "writing register number %d(%d)",
					regno, i);
				perror_with_name(buf);
			}
			regaddr += sizeof(int);
		}
	} else
		for (regno = 0; regno < NUM_REGS; regno++) {
			regaddr = register_addr(regno, offset);
			for (i = 0; i < REGISTER_RAW_SIZE(regno);
			     i += sizeof(int)) {
				errno = 0;
				ptrace(PT_WRITE_U, inferior_pid,
				       (caddr_t)regaddr,
				       *(int *) &registers[REGISTER_BYTE(regno) + i]);
				if (errno != 0) {
					sprintf(buf,
					   "writing register number %d(%d)", 
					    regno, i);
					perror_with_name(buf);
				}
				regaddr += sizeof(int);
			}
		}
}

/*
 * Copy LEN bytes from inferior's memory starting at MEMADDR to debugger
 * memory starting at MYADDR. On failure (cannot read from inferior, usually
 * because address is out of bounds) returns the value of errno. 
 */
int
read_inferior_memory(memaddr, myaddr, len)
	CORE_ADDR       memaddr;
	char           *myaddr;
	int             len;
{
	register int i;
	/* Round starting address down to longword boundary.  */
	register CORE_ADDR addr = memaddr & -sizeof(int);
	/* Round ending address up; get number of longwords that makes.  */
	register int count = (((memaddr + len) - addr) + sizeof(int) - 1) / 
				sizeof(int);
	/* Allocate buffer of that many longwords.  */
	register int *buffer = (int *) alloca(count * sizeof(int));
	extern int errno;

	if (remote_debugging)
		return (remote_read_inferior_memory(memaddr, myaddr, len));

	/* Read all the longwords */
	errno = 0;
	for (i = 0; i < count && errno == 0; i++, addr += sizeof(int)) 
		buffer[i] = ptrace(PT_READ_I, inferior_pid, (caddr_t)addr, 0);

	/* Copy appropriate bytes out of the buffer.  */
	bcopy((char *) buffer + (memaddr & (sizeof(int) - 1)), myaddr, len);
	return(errno);
}

/*
 * Copy LEN bytes of data from debugger memory at MYADDR to inferior's memory
 * at MEMADDR. On failure (cannot write the inferior) returns the value of
 * errno.  
 */

int
write_inferior_memory(memaddr, myaddr, len)
	CORE_ADDR       memaddr;
	char           *myaddr;
	int             len;
{
	register int    i;
	/* Round starting address down to longword boundary.  */
	register CORE_ADDR addr = memaddr & -sizeof(int);
	/* Round ending address up; get number of longwords that makes.  */
	register int count = (((memaddr + len) - addr) + sizeof(int) - 1) / 
				sizeof(int);
	/* Allocate buffer of that many longwords.  */
	register int *buffer = (int *) alloca(count * sizeof(int));
	extern int errno;

	/*
	 * Fill start and end extra bytes of buffer with existing memory
	 * data.  
	 */
	if (remote_debugging)
		return (remote_write_inferior_memory(memaddr, myaddr, len));

	/*
	 * Fill start and end extra bytes of buffer with existing memory
	 * data.  
	 */
	buffer[0] = ptrace(PT_READ_I, inferior_pid, (caddr_t)addr, 0);

	if (count > 1)
		buffer[count - 1] = ptrace(PT_READ_I, inferior_pid,
				 (caddr_t)addr + (count - 1) * sizeof(int), 0);

	/* Copy data to be written over corresponding part of buffer */

	bcopy(myaddr, (char *) buffer + (memaddr & (sizeof(int) - 1)), len);

	/* Write the entire buffer.  */

	errno = 0;
	for (i = 0; i < count && errno == 0; i++, addr += sizeof(int))
		ptrace(PT_WRITE_I, inferior_pid, (caddr_t)addr, buffer[i]);

	return(errno);
}


/*
 * Work with core dump and executable files, for GDB. 
 * This code would be in core.c if it weren't machine-dependent. 
 */

#ifndef N_TXTADDR
#define N_TXTADDR(hdr) 0
#endif				/* no N_TXTADDR */

#ifndef N_DATADDR
#define N_DATADDR(hdr) hdr.a_text
#endif				/* no N_DATADDR */

/*
 * Make COFF and non-COFF names for things a little more compatible to reduce
 * conditionals later.  
 */


#ifndef AOUTHDR
#define AOUTHDR struct exec
#endif

extern char    *sys_siglist[];


/* Hook for `exec_file_command' command to call.  */

extern void     (*exec_file_display_hook) ();

/* File names of core file and executable file.  */

extern char    *corefile;
extern char    *execfile;

/* Descriptors on which core file and executable file are open.
   Note that the execchan is closed when an inferior is created
   and reopened if the inferior dies or is killed.  */

extern int      corechan;
extern int      execchan;

/* Last modification time of executable file.
   Also used in source.c to compare against mtime of a source file.  */

extern int      exec_mtime;

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

extern int      text_offset;

/* Address in executable file of start of data area data.  */

extern int      exec_data_offset;

/* Address in core file of start of data area data.  */

extern int      data_offset;

/* Address in core file of start of stack area data.  */

extern int      stack_offset;


/* a.out header saved in core file.  */

extern AOUTHDR  core_aouthdr;

/* a.out header of exec file.  */

extern AOUTHDR  exec_aouthdr;

extern void     validate_files();


extern int (*core_file_hook)();

#ifdef KERNELDEBUG
/*
 * Kernel debugging routines.
 */

static CORE_ADDR file_offset;
static CORE_ADDR lowram;
static CORE_ADDR sbr;
static CORE_ADDR slr;
static struct pcb pcb;
static CORE_ADDR kernel_udot_va;

#ifndef CFSIZE
#include <machine/frame.h>
#endif

static CORE_ADDR
ksym_lookup(name)
	char *name;
{
	struct symbol *sym;
	int i;

	if ((i = lookup_misc_func(name)) < 0)
		error("kernel symbol `%s' not found.", name);

	return (misc_function_vector[i].address);
}

/*
 * return true if 'len' bytes starting at 'addr' can be read out as
 * longwords and/or locally cached (this is mostly for memory mapped
 * i/o register access when debugging remote kernels).
 */
static int
ok_to_cache(addr, len)
{
#ifdef NEWVM
	static CORE_ADDR intiobase, extiobase;

	if (! intiobase) {
		intiobase = ksym_lookup("intiobase");
		(void)remote_read_inferior_memory(intiobase, &intiobase,
						  sizeof(intiobase));
		extiobase = ksym_lookup("extiobase");
		(void)remote_read_inferior_memory(extiobase, &extiobase,
						  sizeof(extiobase));
	}

	if (addr >= intiobase && addr < intiobase + ctob(IIOMAPSIZE))
		return (0);
	if (addr >= extiobase && addr < extiobase + ctob(EIOMAPSIZE))
		return (0);
#else
	static CORE_ADDR IObase;

	if (! IObase)
		IObase = ksym_lookup("IObase");

	if (addr >= IObase && addr < IObase + (IOTOP - IOBASE))
		return (0);
#endif

	return (1);
}

static
physrd(addr, dat, len)
	u_int addr;
	char *dat;
{
	if (lseek(corechan, addr - file_offset, L_SET) == -1)
		return (-1);
	if (read(corechan, dat, len) != len)
		return (-1);

	return (0);
}

/*
 * When looking at kernel data space through /dev/mem or with a core file, do
 * virtual memory mapping.
 */
#ifdef NEWVM
static CORE_ADDR
vtophys(addr)
	CORE_ADDR addr;
{
	CORE_ADDR v;
	struct pte pte;
	CORE_ADDR stp;
	CORE_ADDR oldaddr = addr;
	static CORE_ADDR curstp = -1;

	/*
	 * If we're looking at the kernel stack,
	 * munge the address to refer to the user space mapping instead;
	 * that way we get the requested process's kstack, not the running one.
	 */
	if (addr >= kstack && addr < kstack + ctob(UPAGES))
		addr = (addr - kstack) + curpcb;

	/*
	 * Identify the current segment table.
	 * Since the given VA could come from either kernel
	 * or user space, the following heuristics don't always work.
	 */
	if (INKERNEL(addr))
		stp = sbr;
	else if (found_pcb == 0) {
		/* We have a pcb address, but haven't read it yet.  Cheat. */
		if (curstp == -1) {
			v = vtophys((CORE_ADDR)&((struct pcb *)curpcb)->pcb_ustp);
			physrd(v, &curstp, sizeof curstp);
		}
		stp = curstp;
	} else
		stp = pcb.pcb_ustp;

	/*
	 * Read the current segment table.
	 */
	v = stp + ((addr >> SG_ISHIFT) * sizeof pte);
	if (physrd(v, (char *)&pte, sizeof(pte)))
		return (~0);
	if (*(int *)&pte == SG_NV)
		return (~0);
	v = hp300_btop(addr & SG_PMASK);
	addr = (CORE_ADDR)(hp300_ptob(pte.pg_pfnum) + v*sizeof pte);

	/*
	 * Addr is now address of the pte of the page we are interested in;
	 * get the pte and paste up the physical address.
	 */
	if (physrd(addr, (char *) &pte, sizeof(pte)))
		return (~0);
	if (pte.pg_v == 0 && pte.pg_pfnum == 0)
		return (~0);
	addr = (CORE_ADDR)hp300_ptob(pte.pg_pfnum) + (oldaddr & PGOFSET);
#if 0
	printf("vtophys(%x) -> %x\n", oldaddr, addr);
#endif
	return (addr);
}
#else
static CORE_ADDR
vtophys(addr)
	CORE_ADDR addr;
{
	CORE_ADDR v;
	struct pte pte;
	CORE_ADDR oldaddr = addr;

	/* permit direct reference to physical memory */
	if (addr >= lowram)
		return (addr);

	if (kernel_udot_va && INUDOT(addr)) {
		addr -= KERNEL_U_ADDR;
		addr = kernel_udot_va + btop(addr) * sizeof (struct pte);
		addr = vtophys(addr);
	} else if (INKERNEL(addr)) {
		/*
		 * In system space get system pte.  If valid or reclaimable
		 * then physical address is combination of its page number
		 * and the page offset of the original address.
		 */
		v = smxtob(btop(addr - KERNOFF));
		addr = v + lowram;
	} else {
		/* In p0 space must not be off end of region. */
		v = btop(addr);
		if (v >= pcb.pcb_p0lr)
			/* address out of segment */
			return (~0);

		addr = (CORE_ADDR)(pcb.pcb_p0br + v);
		/*
		 * For p0/p1 address, user-level page table should be in
		 * kernel vm.  Do second-level indirect by recursing.
		 */
		if (!INKERNEL(addr))
			return (~0);

		addr = vtophys(addr);
	}
	/*
	 * Addr is now address of the pte of the page we are interested in;
	 * get the pte and paste up the physical address.
	 */
	if (physrd(addr, (char *) &pte, sizeof(pte)))
		return (~0);

	if (pte.pg_v == 0 && (pte.pg_fod || pte.pg_pfnum == 0))
		return (~0);

	addr = (CORE_ADDR)ptob(pte.pg_pfnum) + (oldaddr & PGOFSET);
#if 0
	printf("vtophys(%x) -> %x\n", oldaddr, addr);
#endif
	return (addr);
}
#endif

static
kvread(addr)
	CORE_ADDR addr;
{
	CORE_ADDR paddr = vtophys(addr);

	if (paddr != ~0)
		if (physrd(paddr, (char *)&addr, sizeof(addr)) == 0);
			return (addr);

	return (~0);
}

static void
read_pcb(uaddr)
     u_int uaddr;
{
	int i;

#ifdef NEWVM
	if (physrd(uaddr, (char *)&pcb, sizeof pcb))
		error("cannot read pcb at %x\n", uaddr);
	printf("current pcb at %x\n", uaddr);
#else
	if (physrd (uaddr, (char *)&pcb, sizeof pcb))
		error ("cannot read pcb at %x.\n", uaddr);
	printf("p0br %x p0lr %x p1br %x p1lr %x\n",
	       pcb.pcb_p0br, pcb.pcb_p0lr, pcb.pcb_p1br, pcb.pcb_p1lr);

	kernel_udot_va = (CORE_ADDR) (pcb.pcb_p1br + BTOPUSRSTACK);
#endif

	/*
	 * get the register values out of the sys pcb and
	 * store them where `read_register' will find them.
	 */
	for (i = 2; i < 8; ++i)
		supply_register(i, &pcb.pcb_regs[i-2]);
	for (i = 10; i < 16; ++i)
		supply_register(i, &pcb.pcb_regs[i-4]);
	
	/* fake 'scratch' regs d0, d1, a0, a1 */
	i = 0;
	supply_register(0, &i); supply_register(1, &i);
	supply_register(8, &i); supply_register(9, &i);
	
	i = kvread(pcb.pcb_regs[10] + 4);
	if (i != -1)
		supply_register(PC_REGNUM, &i);
	
	supply_register(PS_REGNUM, &pcb.pcb_ps);
	
	for (i = FP0_REGNUM; i < NUM_REGS; ++i) {
		int fpreg;

		REGISTER_U_ADDR(fpreg, 0, i);
		supply_register(i, ((char *)&pcb) + fpreg);
	}
}

static void
setup_kernel_debugging()
{
	struct stat stb;
	int devmem = 0;
	CORE_ADDR addr;

	fstat(corechan, &stb);
	if ((stb.st_mode & S_IFMT) == S_IFCHR && stb.st_rdev == makedev(2, 0))
		devmem = 1;

	/*
	 * Must get value of lowram before we can read PCB.
	 */
	if (devmem)
		/* /dev/mem == physical memory */
		(void)physrd(LOWRAM, (char *)&lowram, sizeof(lowram));
	else
		/* normal file -- use standard offset */
		(void)physrd(ksym_lookup("lowram"), (char *)&lowram,
			     sizeof(lowram));
	lowram = roundup(lowram, NBPG);
	if (! devmem)
		file_offset = lowram;

	/*
	 * Get system mapping information.
	 */
#ifdef NEWVM
	sbr = ksym_lookup("Sysseg") + lowram;
	(void)physrd(sbr, (char *)&sbr, sizeof(sbr));
	sbr += lowram;		/* sbr is a physical address for NEWVM */
	slr = NPTEPG * (NPTEPG-1);
	curpcb = ksym_lookup("curpcb") + lowram;
	physrd(curpcb, &curpcb, sizeof curpcb);
	kstack = ksym_lookup("kstack");
#else
	sbr = ksym_lookup("Sysmap");
	slr = ksym_lookup("Syssize");
#endif
	printf("sbr %x slr %x\n", sbr, slr);

	/*
	 * pcb where "panic" saved registers in first thing in current
	 * u area.
	 */
#ifdef NEWVM
	read_pcb(vtophys(kstack));
	found_pcb = 1;
#else
	read_pcb(vtophys(ksym_lookup("u")));
#endif
	if (!devmem) {
		/* find stack frame */
		CORE_ADDR panicstr;
		char buf[256];
		register char *cp;

		panicstr = kvread(ksym_lookup("panicstr"));
		if (panicstr == ~0)
			return;
		(void) kernel_core_file_hook(panicstr, buf, sizeof(buf));
		for (cp = buf; cp < &buf[sizeof(buf)] && *cp; cp++)
			if (!isascii(*cp) || (!isprint(*cp) && !isspace(*cp)))
				*cp = '?';
		if (*cp)
			*cp = '\0';
		printf("panic: %s\n", buf);
	}

	stack_start = USRSTACK;
	stack_end = USRSTACK + ctob(UPAGES);
}

set_paddr_command(arg)
	char *arg;
{
	u_int uaddr;

	if (!arg)
		error_no_arg("ps-style address for new current process");
	if (!kernel_debugging)
		error("not debugging kernel");
	if (lowram == 0)
		error("need kernel core file");
	uaddr = (u_int) parse_and_eval_address(arg);
#ifndef NEWVM
	read_pcb(ctob(uaddr));
#else
	/* p_addr is now a pcb virtual address */
	read_pcb(vtophys(uaddr));
	curpcb = uaddr;
#endif

	flush_cached_frames();
	set_current_frame(create_new_frame(read_register(FP_REGNUM), read_pc()));
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
	int i;
	CORE_ADDR paddr;

	while (len > 0) {
		paddr = vtophys(addr);
		if (paddr == ~0) {
			bzero(buf, len);
			return (1);
		}
		/* we can't read across a page boundary */
		i = min(len, NBPG - (addr & PGOFSET));
		if (physrd(paddr, buf, i)) {
			bzero(buf, len);
			return (1);
		}
		buf += i;
		addr += i;
		len -= i;
	}
	return (0);
}
#endif

core_file_command(filename, from_tty)
	char           *filename;
	int             from_tty;
{
	int             val;
	extern char     registers[];
	unsigned int	reg_offset;
#ifdef KERNELDEBUG
	struct stat stb;
#endif
	struct user 	u;

	/*
	 * Discard all vestiges of any previous core file and mark data and
	 * stack spaces as empty.  
	 */
	if (corefile)
		free(corefile);
	corefile = 0;
	core_file_hook = 0;

	if (corechan >= 0)
		close(corechan);
	corechan = -1;

	/* Now, if a new core file was specified, open it and digest it.  */

	if (filename == 0) {
		if (from_tty)
			printf("No core file now.\n");
		return;
	}
	filename = tilde_expand(filename);
	make_cleanup(free, filename);
	if (have_inferior_p())
		error("To look at a core file, you must kill the inferior with \"kill\".");
	corechan = open(filename, O_RDONLY, 0);
	if (corechan < 0)
		perror_with_name(filename);

#ifdef KERNELDEBUG
	fstat(corechan, &stb);

	if (kernel_debugging) {
		setup_kernel_debugging();
		core_file_hook = kernel_core_file_hook;
	} else if ((stb.st_mode & S_IFMT) == S_IFCHR &&
		   stb.st_rdev == makedev(2, 1)) {
		/* looking at /dev/kmem */
		data_offset = data_start = KERNOFF;
		data_end = ~0; /* XXX */
		stack_end = stack_start = data_end;
	} else
#endif
	{
		val = myread(corechan, &u, sizeof u);
		if (val < 0)
			perror_with_name("Not a core file: reading upage");
		if (val != sizeof u)
			error("Not a core file: could only read %d bytes", val);

		/*
		 * We are depending on exec_file_command having been
		 * called previously to set exec_data_start.  Since
		 * the executable and the core file share the same
		 * text segment, the address of the data segment will
		 * be the same in both.  
		 */
		data_start = exec_data_start;

#ifndef NEWVM
		data_end = data_start + NBPG * u.u_dsize;
		stack_start = stack_end - NBPG * u.u_ssize;
		data_offset = NBPG * UPAGES;
		stack_offset = NBPG * (UPAGES + u.u_dsize);

		/*
		 * Some machines put an absolute address in here and
		 * some put the offset in the upage of the regs.  
		 */
		reg_offset = (int) u.u_ar0 - KERNEL_U_ADDR;
#else
		data_end = data_start +
			NBPG * u.u_kproc.kp_eproc.e_vm.vm_dsize;
		stack_start = stack_end -
			NBPG * u.u_kproc.kp_eproc.e_vm.vm_ssize;
		data_offset = NBPG * UPAGES;
		stack_offset = NBPG *
			(UPAGES + u.u_kproc.kp_eproc.e_vm.vm_dsize);

		reg_offset = (int) u.u_kproc.kp_proc.p_regs - USRSTACK;
#endif

		/*
		 * I don't know where to find this info. So, for now,
		 * mark it as not available.  
		 */
		N_SET_MAGIC(core_aouthdr, 0);

		/*
		 * Read the register values out of the core file and
		 * store them where `read_register' will find them.  
		 */
		{
			register int    regno;

			for (regno = 0; regno < NUM_REGS; regno++) {
				char buf[MAX_REGISTER_RAW_SIZE];

				val = lseek(corechan, register_addr(regno, reg_offset), 0);
				if (val < 0
				    || (val = myread(corechan, buf, sizeof buf)) < 0) {
					char *buffer = (char *) alloca(strlen(reg_names[regno]) + 30);
					strcpy(buffer, "Reading register ");
					strcat(buffer, reg_names[regno]);
					perror_with_name(buffer);
				}
				supply_register(regno, buf);
			}
		}
	}
	if (filename[0] == '/')
		corefile = savestring(filename, strlen(filename));
	else
		corefile = concat(current_directory, "/", filename);

	set_current_frame(create_new_frame(read_register(FP_REGNUM),
					   read_pc()));
	select_frame(get_current_frame(), 0);
	validate_files();
}


exec_file_command(filename, from_tty)
	char           *filename;
	int             from_tty;
{
	int             val;

	/*
	 * Eliminate all traces of old exec file. Mark text segment as empty.  
	 */

	if (execfile)
		free(execfile);
	execfile = 0;
	data_start = 0;
	data_end -= exec_data_start;
	text_start = 0;
	text_end = 0;
	exec_data_start = 0;
	exec_data_end = 0;
	if (execchan >= 0)
		close(execchan);
	execchan = -1;

	/* Now open and digest the file the user requested, if any.  */

	if (filename) {
		filename = tilde_expand(filename);
		make_cleanup(free, filename);

		execchan = openp(getenv("PATH"), 1, filename, O_RDONLY, 0,
				 &execfile);
		if (execchan < 0)
			perror_with_name(filename);

		{
			struct stat     st_exec;

#ifdef HEADER_SEEK_FD
			HEADER_SEEK_FD(execchan);
#endif

			val = myread(execchan, &exec_aouthdr, sizeof(AOUTHDR));

			if (val < 0)
				perror_with_name(filename);

			text_start = N_TXTADDR(exec_aouthdr);
			exec_data_start = N_DATADDR(exec_aouthdr);

			text_offset = N_TXTOFF(exec_aouthdr);
			exec_data_offset = N_TXTOFF(exec_aouthdr) + exec_aouthdr.a_text;

			text_end = text_start + exec_aouthdr.a_text;
			exec_data_end = exec_data_start + exec_aouthdr.a_data;
			data_start = exec_data_start;
			data_end += exec_data_start;

			fstat(execchan, &st_exec);
			exec_mtime = st_exec.st_mtime;
		}

		validate_files();
	} else if (from_tty)
		printf("No exec file now.\n");

	/* Tell display code (if any) about the changed file name.  */
	if (exec_file_display_hook)
		(*exec_file_display_hook) (filename);
}

int dummy_code[] = {
	0x4e714eb9,		/* nop, jsr @#32323232 */
	0x32323232,
#define DUMMY_CALL_INDEX 1
	0x4e424e71,		/* trap 2, nop */
};

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
	int padding, i;
	CORE_ADDR top = sp, struct_addr, pc;

	i = arg_stacklen(nargs, args) + struct_return_bytes
	    + sizeof(dummy_code);
	if (i & 3)
		padding = 4 - (i & 3);
	else
		padding = 0;
	pc = sp - sizeof(dummy_code);
	sp = pc - padding - struct_return_bytes;
	struct_addr = sp;
	while (--nargs >= 0)
		sp = (*pushfn)(sp, *args++);
	if (struct_return_bytes)
		STORE_STRUCT_RETURN(struct_addr, sp);
	write_register(SP_REGNUM, sp);

	dummy_code[DUMMY_CALL_INDEX] = (int)funaddr;
	write_memory(pc, (char *)dummy_code, sizeof(dummy_code));

	return pc;
}

void
_initialize_hp300bsd_dep()
{
#ifdef KERNELDEBUG
	add_com ("process-address", class_obscure, set_paddr_command,
		 "The process identified by (ps-style) ADDR becomes the\n\
\"current\" process context for kernel debugging.");
	add_com_alias ("paddr", "process-address", class_obscure, 0);
#endif
}
