/* Machine-dependent code which would otherwise be in inflow.c and core.c,
   for GDB, the GNU debugger.  This code is for the HP PA-RISC cpu.
   Copyright 1986, 1987, 1989, 1990, 1991, 1992 Free Software Foundation, Inc.

   Contributed by the Center for Software Science at the
   University of Utah (pa-gdb-bugs@cs.utah.edu).

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
#include "value.h"

/* For argument passing to the inferior */
#include "symtab.h"

#ifdef USG
#include <sys/types.h>
#endif

#include <sys/param.h>
#include <sys/dir.h>
#include <signal.h>
#include <sys/ioctl.h>

#ifdef COFF_ENCAPSULATE
#include "a.out.encap.h"
#else
#include <a.out.h>
#endif
#ifndef N_SET_MAGIC
#define N_SET_MAGIC(exec, val) ((exec).a_magic = (val))
#endif

/*#include <sys/user.h>		After a.out.h  */
#include <sys/file.h>
#include <sys/stat.h>
#include <machine/psl.h>

#ifdef KERNELDEBUG
#include <sys/vmmac.h>
#include <machine/machparam.h>
#include <machine/vmparam.h>
#include <machine/pde.h>
#include <machine/cpu.h>
#include <machine/iomod.h>
#include <machine/pcb.h>
#include <machine/rpb.h>
#include <ctype.h>

extern int kernel_debugging;
extern CORE_ADDR startup_file_start;
extern CORE_ADDR startup_file_end;

#define	KERNOFF		((unsigned)KERNBASE)
#define	INKERNEL(x)	((x) >= KERNOFF && (x) < KERNOFF + ctob(slr))

static int ok_to_cache();
static void set_kernel_boundaries();

int devmem = 0;
int vtophys_ready = 0;
int kerneltype;
#define	OS_BSD	1
#define	OS_MACH	2
#endif

#include "gdbcore.h"
#include "gdbcmd.h"

extern int errno;






/* Last modification time of executable file.
   Also used in source.c to compare against mtime of a source file.  */

extern int exec_mtime;

/* Virtual addresses of bounds of the two areas of memory in the core file.  */

/* extern CORE_ADDR data_start; */
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

struct header file_hdr;
struct som_exec_auxhdr exec_hdr;

#ifdef KERNELDEBUG
/*
 * Kernel debugging routines.
 */

static struct pcb pcb;
static struct pde *pdir;
static struct hte *htbl;
static u_int npdir, nhtbl;

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
 * (re-)set the variables that tell "inside_entry_file" where to end
 * a stack backtrace.
 */
void
set_kernel_boundaries()
{
	switch (kerneltype) {
	case OS_MACH:
		startup_file_start = ksym_lookup("$syscall");
		startup_file_end = ksym_lookup("trap");
		break;
	case OS_BSD:
		startup_file_start = ksym_lookup("syscallinit");
		startup_file_end = ksym_lookup("$syscallexit");
		break;
	}
}

/*
 * return true if 'len' bytes starting at 'addr' can be read out as
 * longwords and/or locally cached (this is mostly for memory mapped
 * i/o register access when debugging remote kernels).
 */
static int
ok_to_cache(addr, len)
{
	static CORE_ADDR ioptr;

	if (! ioptr)
		ioptr = ksym_lookup("ioptr");

	if (addr >= ioptr && addr < SPA_HIGH)
		return (0);

	return (1);
}

static
physrd(addr, dat, len)
	u_int addr;
	char *dat;
{
	if (lseek(corechan, addr, L_SET) == -1)
		return (-1);
	if (read(corechan, dat, len) != len)
		return (-1);

	return (0);
}

/*
 * When looking at kernel data space through /dev/mem or with a core file, do
 * virtual memory mapping.
 */
static CORE_ADDR
vtophys(space, addr)
	unsigned space;
	CORE_ADDR addr;
{
	struct pde *pptr;
	u_int hindx, vpageno, ppageno;
	CORE_ADDR phys = ~0;

	if (!vtophys_ready) {
		phys = addr;		/* XXX for kvread */
	} else if (kerneltype == OS_BSD) {
		/* make offset into a virtual page no */
		vpageno = btop(addr);
		/*
		 *  Determine index into hash table, initialize pptr to this
		 *  entry (since first word of pte & hte are same), and set
		 *  physical page number for first entry in chain.
		 */
		hindx = pdirhash(space, addr) & (nhtbl-1);
		pptr = (struct pde *) &htbl[hindx];
		ppageno = pptr->pde_next;
		while (1) {
			if (pptr->pde_end)
				break;
			pptr = &pdir[ppageno];
			/*
			 *  If space id & virtual page number match, return
			 *  "next PDIR entry of previous PDIR entry" as the
			 *  physical page or'd with offset into page.
			 */
			if (pptr->pde_space == space &&
			    pptr->pde_page == vpageno) {
				phys = (CORE_ADDR) ((u_int)ptob(ppageno) |
						    (addr & PGOFSET));
				break;
			}
			ppageno = pptr->pde_next;
		}
	}
#ifdef MACHKERNELDEBUG
	else if (kerneltype == OS_MACH) {
	  mach_vtophys(space, addr, &phys);
	}
#endif
#if 0
	printf("vtophys(%x.%x) -> %x\n", space, addr, phys);
#endif
	return (phys);
}

static
kvread(addr)
	CORE_ADDR addr;
{
	CORE_ADDR paddr;

	paddr = vtophys(0, addr);
	if (paddr != ~0)
		if (physrd(paddr, (char *)&addr, sizeof(addr)) == 0)
			return (addr);

	return (~0);
}

static void
read_pcb(addr)
     u_int addr;
{
	int i, off;
	extern char registers[];
	static int reg2pcb[] = {
		/* RPB */
		-1,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15, 16, 17,
		18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33,
		45, 52, 51, 75, 74, 49, 53, 54, 55, 56, -1, 70, 66, 67, 68, 69,
		71, 72, 73, 34, 42, 43, 44, 46, 47, 58, 59, 60, -1, -1, -1, -1,
		-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
		-1, -1, -1, -1,
		/* BSD */
		-1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14,
		15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
		43, 64, 67, 68, 67, 47, 51, 52, 53, 54, -1, 35, 31, 32, 33, 34,
		36, 37, 38, 39, 40, 41, 42, 44, 45, 56, 57, 58,102,103,104, -1,
		70, 71, 72, 73, 74, 75, 76, 77, 78, 80, 82, 84, 86, 88, 90, 92,
		94, 96, 98, 100,
		/* Mach */
		-1, -1,  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13,
		14, 15, 16, -1, -1, -1, -1, -1, -1, -1, -1, 17, -1, -1, 18, -1,
		25, -1, -1, -1, -1, 30, -1, -1, -1, -1, -1, 20, -1, -1, -1, 19,
		21, 22, 23, 24, 26, 27, -1, 28, 29, -1, -1, -1, -1, -1, -1, -1,
		34, 35, 36, 37, 38, 39, 40, 41, -1, -1, -1, -1, -1, -1, -1, -1,
		42, 44, 46, 48
	};
	static struct rpb *rpbaddr = (struct rpb *) 0;
	static u_int rpbpcbaddr = 0;

	if (!remote_debugging) {
		/*
		 * If we are debugging a post-mortem and this is the first
		 * call of read_pcb, read the RPB.  Also assoicate the
		 * thread/proc running at the time with the RPB.
		 */
		if (!devmem && rpbpcbaddr == 0) {
			CORE_ADDR raddr = ksym_lookup("rpb");
			int usepcb = 1;

			if (raddr != ~0) {
				rpbaddr = (struct rpb *) malloc(sizeof *rpbaddr);
				if (!physrd(raddr, (char *)rpbaddr, sizeof *rpbaddr)) {
					rpbpcbaddr = addr;
					usepcb = 0;
				}
			}
			if (usepcb) {
				error("cannot read rpb, using pcb for registers\n");
				if (rpbaddr)
					free((char *)rpbaddr);
				rpbpcbaddr = ~0;
			}
		}
		if (physrd (addr, (char *)&pcb, sizeof pcb))
			error ("cannot read pcb at %x.\n", addr);
	} else {
		if (remote_read_inferior_memory(addr, (char *)&pcb, sizeof pcb))
			error ("cannot read pcb at %x.\n", addr);
	}

	if (kerneltype == OS_BSD) {
		printf("p0br %lx p0lr %lx p1br %lx p1lr %lx\n",
		       pcb.pcb_p0br, pcb.pcb_p0lr, pcb.pcb_p1br, pcb.pcb_p1lr);
		off = NUM_REGS;
	} else {
		printf("pcb %lx psw %lx ksp %lx\n",
		       addr, ((int *)&pcb)[31], ((int *)&pcb)[32]);
		off = NUM_REGS * 2;
	}
	/*
	 * get the register values out of the sys pcb and
	 * store them where `read_register' will find them.
	 */
	bzero(registers, REGISTER_BYTES);
	for (i = 0; i < NUM_REGS; ++i)
		if (reg2pcb[i+off] != -1)
			supply_register(i, &((int *)&pcb)[reg2pcb[i+off]]);
	/*
	 * If the RPB is valid for this thread/proc use the register values
	 * contained there.
	 */
	if (addr == rpbpcbaddr) {
		off = 0;
		for (i = 0; i < NUM_REGS; ++i)
			if (reg2pcb[i+off] != -1)
				supply_register(i, &((int *)rpbaddr)[reg2pcb[i+off]]);
	}
}

void
setup_kernel_debugging()
{
	struct stat stb;
	CORE_ADDR addr;

	fstat(corechan, &stb);
	devmem = 0;
	if ((stb.st_mode & S_IFMT) == S_IFCHR && stb.st_rdev == makedev(2, 0))
		devmem = 1;

	/* XXX */
	if (lookup_misc_func("Sysmap") < 0)
		kerneltype = OS_MACH;
	else
		kerneltype = OS_BSD;

	if (kerneltype == OS_BSD) {
		int len, err = 0;

		/*
		 * Hash table and PDIR are equivalently mapped
		 */
		nhtbl = kvread(ksym_lookup("nhtbl"));
		if (nhtbl != ~0) {
			len = nhtbl * sizeof(*htbl);
			htbl = (struct hte *) malloc(len);
			if (htbl) {
				addr = kvread(ksym_lookup("htbl"));
				if (physrd(addr, (char *)htbl, len))
					err++;
			} else
				err++;
		} else
			err++;
		npdir = kvread(ksym_lookup("npdir"));
		if (npdir != ~0) {
			len = npdir * sizeof(*pdir);
			pdir = (struct pde *) malloc(len);
			if (pdir) {
				addr = kvread(ksym_lookup("pdir"));
				if (physrd(addr, (char *)pdir, len))
					err++;
			} else
				err++;
		} else
			err++;
		if (err) {
			error("cannot read PDIR/HTBL");
			return;
		}
		vtophys_ready = 1;

		/*
		 * pcb where "panic" saved registers in first thing in
		 * current u-area.  The current u-area is pointed to by
		 * "uptr".
		 */
		addr = kvread(ksym_lookup("uptr"));
		if (addr == ~0) {
			error("cannot read current u-area address");
			return;
		}
		read_pcb(vtophys(0, addr));	/* XXX space */
		if (!devmem) {
			/* find stack frame */
			CORE_ADDR panicstr;
			char buf[256];
			register char *cp;
			
			panicstr = kvread(ksym_lookup("panicstr"));
			if (panicstr == ~0)
				return;
			kernel_core_file_hook(panicstr, buf, sizeof(buf));
			for (cp = buf; cp < &buf[sizeof(buf)] && *cp; cp++)
				if (!isascii(*cp) || (!isprint(*cp) && !isspace(*cp)))
					*cp = '?';
			if (*cp)
				*cp = '\0';
			printf("panic: %s\n", buf);
		}
	}
#ifdef MACHKERNELDEBUG
	else {
		int *thread;

		/*
		 * Set up address translation
		 */
		if (mach_vtophys_init() == 0) {
			error("cannot initialize vtophys for Mach");
			return;
		}
		vtophys_ready = 1;

		/*
		 * Locate active thread and read PCB
		 * XXX MAJOR HACK
		 *	- assumes uni-processor
		 *	- assumes position of pcb to avoid mach includes
		 */
		thread = (int *)kvread(ksym_lookup("active_threads"));
		addr = kvread(&thread[9]);		/* XXX: pcb addr */
		read_pcb(vtophys(0, addr));
	}
#endif
}

vtop_command(arg)
	char *arg;
{
	u_int sp, off, pa;

	if (!arg)
		error_no_arg("kernel virtual address");
	if (!kernel_debugging)
		error("not debugging kernel");

	sp = 0;		/* XXX */
	off = (u_int) parse_and_eval_address(arg);
	pa = vtophys(sp, off);
	printf("%lx.%lx -> ", sp, off);
	if (pa == ~0)
		printf("<invalid>\n");
	else
		printf("%lx\n", pa);
}

set_paddr_command(arg)
	char *arg;
{
	u_int addr;

	if (!arg) {
		if (kerneltype == OS_BSD)
			error_no_arg("ps-style address for new process");
		else
			error_no_arg("thread structure virtual address");
	}
	if (!kernel_debugging)
		error("not debugging kernel");

	addr = (u_int) parse_and_eval_address(arg);
	if (kerneltype == OS_BSD)
		addr = ctob(addr);
	else {
		addr = kvread(&(((int *)addr)[9]));	/* XXX: pcb addr */
		addr = vtophys(0, addr);		/* XXX space */
	}
	read_pcb(addr);

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
		paddr = vtophys(0, addr);	/* XXX space */
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





/* Routines to extract various sized constants out of hppa 
   instructions. */

/* This assumes that no garbage lies outside of the lower bits of 
   value. */

int
sign_extend (val, bits)
     unsigned val, bits;
{
  return (int)(val >> bits - 1 ? (-1 << bits) | val : val);
}

/* For many immediate values the sign bit is the low bit! */

int
low_sign_extend (val, bits)
     unsigned val, bits;
{
  return (int)((val & 0x1 ? (-1 << (bits - 1)) : 0) | val >> 1);
}
/* extract the immediate field from a ld{bhw}s instruction */



unsigned
get_field (val, from, to)
     unsigned val, from, to;
{
  val = val >> 31 - to;
  return val & ((1 << 32 - from) - 1);
}

unsigned
set_field (val, from, to, new_val)
     unsigned *val, from, to;
{
  unsigned mask = ~((1 << (to - from + 1)) << (31 - from));
  return *val = *val & mask | (new_val << (31 - from));
}

/* extract a 3-bit space register number from a be, ble, mtsp or mfsp */

extract_3 (word)
     unsigned word;
{
  return GET_FIELD (word, 18, 18) << 2 | GET_FIELD (word, 16, 17);
}
       
extract_5_load (word)
     unsigned word;
{
  return low_sign_extend (word >> 16 & MASK_5, 5);
}

/* extract the immediate field from a st{bhw}s instruction */

int
extract_5_store (word)
     unsigned word;
{
  return low_sign_extend (word & MASK_5, 5);
}

/* extract an 11 bit immediate field */

int
extract_11 (word)
     unsigned word;
{
  return low_sign_extend (word & MASK_11, 11);
}

/* extract a 14 bit immediate field */

int
extract_14 (word)
     unsigned word;
{
  return low_sign_extend (word & MASK_14, 14);
}

/* deposit a 14 bit constant in a word */

unsigned
deposit_14 (opnd, word)
     int opnd;
     unsigned word;
{
  unsigned sign = (opnd < 0 ? 1 : 0);

  return word | ((unsigned)opnd << 1 & MASK_14)  | sign;
}

/* extract a 21 bit constant */

int
extract_21 (word)
     unsigned word;
{
  int val;

  word &= MASK_21;
  word <<= 11;
  val = GET_FIELD (word, 20, 20);
  val <<= 11;
  val |= GET_FIELD (word, 9, 19);
  val <<= 2;
  val |= GET_FIELD (word, 5, 6);
  val <<= 5;
  val |= GET_FIELD (word, 0, 4);
  val <<= 2;
  val |= GET_FIELD (word, 7, 8);
  return sign_extend (val, 21) << 11;
}

/* deposit a 21 bit constant in a word. Although 21 bit constants are
   usually the top 21 bits of a 32 bit constant, we assume that only
   the low 21 bits of opnd are relevant */

unsigned
deposit_21 (opnd, word)
     unsigned opnd, word;
{
  unsigned val = 0;

  val |= GET_FIELD (opnd, 11 + 14, 11 + 18);
  val <<= 2;
  val |= GET_FIELD (opnd, 11 + 12, 11 + 13);
  val <<= 2;
  val |= GET_FIELD (opnd, 11 + 19, 11 + 20);
  val <<= 11;
  val |= GET_FIELD (opnd, 11 + 1, 11 + 11);
  val <<= 1;
  val |= GET_FIELD (opnd, 11 + 0, 11 + 0);
  return word | val;
}

/* extract a 12 bit constant from branch instructions */

int
extract_12 (word)
     unsigned word;
{
  return sign_extend (GET_FIELD (word, 19, 28) |
		      GET_FIELD (word, 29, 29) << 10 |
		      (word & 0x1) << 11, 12) << 2;
}

/* extract a 17 bit constant from branch instructions, returning the
   19 bit signed value. */

int
extract_17 (word)
     unsigned word;
{
  return sign_extend (GET_FIELD (word, 19, 28) |
		      GET_FIELD (word, 29, 29) << 10 |
		      GET_FIELD (word, 11, 15) << 11 |
		      (word & 0x1) << 16, 17) << 2;
}


CORE_ADDR
frame_saved_pc (frame)
     FRAME frame;
{
  if (get_current_frame () == frame)
    {
      struct frame_saved_regs saved_regs;

      get_frame_saved_regs (frame, &saved_regs);
      if (saved_regs.regs[RP_REGNUM])
	return read_memory_integer (saved_regs.regs[RP_REGNUM], 4);
      else
	return read_register (RP_REGNUM);
    }
  return read_memory_integer (frame->frame - 20, 4) & ~0x3;
}

/* To see if a frame chain is valid, see if the caller looks like it
   was compiled with gcc. */

int frame_chain_valid (chain, thisframe)
     FRAME_ADDR chain;
     FRAME thisframe;
{
  if (chain && (chain > 0x60000000 
		/* || remote_debugging   -this is no longer used */
#ifdef KERNELDEBUG
		|| kernel_debugging
#endif
		))
    {
      CORE_ADDR pc = get_pc_function_start (FRAME_SAVED_PC (thisframe));

      if (!inside_entry_file (pc))
	return 0;
      /* look for stw rp, -20(0,sp); copy 4,1; copy sp, 4 */
      if (read_memory_integer (pc, 4) == 0x6BC23FD9)			
	pc = pc + 4;							
      
      if (read_memory_integer (pc, 4) == 0x8040241 &&
	  read_memory_integer (pc + 4, 4) == 0x81E0244)			
	return 1;
      else
	return 0;
    }
  else
    return 0;
}

/* Some helper functions. gcc_p returns 1 if the function beginning at 
   pc appears to have been compiled with gcc. hpux_cc_p returns 1 if
   fn was compiled with hpux cc. gcc functions look like :

   stw     rp,-0x14(sp) ; optional
   or      r4,r0,r1
   or      sp,r0,r4
   stwm    r1,framesize(sp)

   hpux cc functions look like:

   stw     rp,-0x14(sp) ; optional.
   stwm    r3,framesiz(sp)
   */

gcc_p (pc)
     CORE_ADDR pc;
{
  if (read_memory_integer (pc, 4) == 0x6BC23FD9)			
    pc = pc + 4;							
      
  if (read_memory_integer (pc, 4) == 0x8040241 &&
      read_memory_integer (pc + 4, 4) == 0x81E0244)			
    return 1;
  return 0;
}

  
find_dummy_frame_regs (frame, frame_saved_regs)
     struct frame_info *frame;
     struct frame_saved_regs *frame_saved_regs;
{
  CORE_ADDR fp = frame->frame;
  int i;
  
  frame_saved_regs->regs[RP_REGNUM] = fp - 20 & ~0x3;
  frame_saved_regs->regs[FP_REGNUM] = fp;
  frame_saved_regs->regs[1] = fp + 8;
  frame_saved_regs->regs[3] = fp + 12;
  for (fp += 16, i = 3; i < 30; fp += 4, i++)
    frame_saved_regs->regs[i] = fp;
  frame_saved_regs->regs[31] = fp;
  fp += 4;
  for (i = FP0_REGNUM; i < NUM_REGS; i++, fp += 8)
    frame_saved_regs->regs[i] = fp;
  /* depend on last increment of fp */
  frame_saved_regs->regs[IPSW_REGNUM] = fp - 4;
  frame_saved_regs->regs[SAR_REGNUM] = fp;
  fp += 4;
  frame_saved_regs->regs[PCOQ_TAIL_REGNUM] = fp;
  frame_saved_regs->regs[PCSQ_TAIL_REGNUM] = fp;
}

CORE_ADDR
hp_push_arguments (nargs, args, sp, struct_return, struct_addr)
     int nargs;
     value *args;
     CORE_ADDR sp;
     int struct_return;
     CORE_ADDR struct_addr;
{
  /* array of arguments' offsets */
  int *offset = (int *)alloca(nargs);
  int cum = 0;
  int i, alignment;
  
  for (i = 0; i < nargs; i++)
    {
      cum += TYPE_LENGTH (VALUE_TYPE (args[i]));
      /* value must go at proper alignment. Assume alignment is a
	 power of two.*/
      alignment = hp_alignof (VALUE_TYPE (args[i]));
      if (cum % alignment)
	cum = (cum + alignment) & -alignment;
      offset[i] = -cum;
    }
  for (i == 0; i < nargs; i++)
    {
      write_memory (sp + offset[i], VALUE_CONTENTS (args[i]), sizeof(int));
    }
  sp += min ((cum + 7) & -8, 48);
  if (struct_return)
    write_register (28, struct_addr);
  return sp + 48;
}

/* return the alignment of a type in bytes. Structures have the maximum
   alignment required by their fields. */

int
hp_alignof (arg)
     struct type *arg;
{
  int max_align, align, i;
  switch (TYPE_CODE (arg))
    {
    case TYPE_CODE_PTR:
    case TYPE_CODE_INT:
    case TYPE_CODE_FLT:
      return TYPE_LENGTH (arg);
    case TYPE_CODE_ARRAY:
      return hp_alignof (TYPE_FIELD_TYPE (arg, 0));
    case TYPE_CODE_STRUCT:
    case TYPE_CODE_UNION:
      max_align = 2;
      for (i = 0; i < TYPE_NFIELDS (arg); i++)
	{
	  /* Bit fields have no real alignment. */
	  if (!TYPE_FIELD_BITPOS (arg, i))
	    {
	      align = hp_alignof (TYPE_FIELD_TYPE (arg, i));
	      max_align = max (max_align, align);
	    }
	}
      return max_align;
    default:
      return 4;
    }
}

/* Print the register regnum, or all registers if regnum is -1 */

pa_do_registers_info (regnum, fpregs)
     int regnum;
     int fpregs;
{
  char raw_regs [REGISTER_BYTES];
  int i;
  
  for (i = 0; i < NUM_REGS; i++)
    read_relative_register_raw_bytes (i, raw_regs + REGISTER_BYTE (i));
  if (regnum = -1)
    pa_print_registers (raw_regs, regnum);
  else if (regnum < FP0_REGNUM)
    {
      printf ("%s %x\n", reg_names[regnum], *(long *)(raw_regs +
						      REGISTER_BYTE (regnum)));
    }
  else
    pa_print_fp_reg (regnum);
}

pa_print_registers (raw_regs, regnum)
     char *raw_regs;
     int regnum;
{
  int i;

  for (i = 0; i < 18; i++)
    printf ("%8.8s: %8x  %8.8s: %8x  %8.8s: %8x  %8.8s: %8x\n",
		     reg_names[i],
		     *(int *)(raw_regs + REGISTER_BYTE (i)),
		     reg_names[i + 18],
		     *(int *)(raw_regs + REGISTER_BYTE (i + 18)),
		     reg_names[i + 36],
		     *(int *)(raw_regs + REGISTER_BYTE (i + 36)),
		     reg_names[i + 54],
		     *(int *)(raw_regs + REGISTER_BYTE (i + 54)));
  for (i = 72; i < NUM_REGS; i++)
    pa_print_fp_reg (i);
}

pa_print_fp_reg (i)
     int i;
{
  unsigned char raw_buffer[MAX_REGISTER_RAW_SIZE];
  unsigned char virtual_buffer[MAX_REGISTER_VIRTUAL_SIZE];
  REGISTER_TYPE val;

  /* Get the data in raw format, then convert also to virtual format.  */
  read_relative_register_raw_bytes (i, raw_buffer);
  REGISTER_CONVERT_TO_VIRTUAL (i, raw_buffer, virtual_buffer);

  fputs_filtered (reg_names[i], stdout);
  print_spaces_filtered (15 - strlen (reg_names[i]), stdout);

  val_print (REGISTER_VIRTUAL_TYPE (i), virtual_buffer, 0, stdout, 0,
	     1, 0, Val_pretty_default);
  printf_filtered ("\n");

}

/*
 * Virtual to physical translation routines for Utah's Mach 3.0
 */
#ifdef MACHKERNELDEBUG

#define STATIC

#if 0	/* too many includes to resolve, too much crap */
#include <kern/queue.h>
#include <vm/pmap.h>	
#include <mach/vm_prot.h>
#else
/* queue.h */
struct queue_entry {
	struct queue_entry	*next;		/* next element */
	struct queue_entry	*prev;		/* previous element */
};

typedef struct queue_entry	*queue_t;
typedef	struct queue_entry	queue_head_t;
typedef	struct queue_entry	queue_chain_t;
typedef	struct queue_entry	*queue_entry_t;

/* pmap.h */
#define HP800_HASHSIZE		1024
#define HP800_HASHSIZE_LOG2	10

#define pmap_hash(space, offset) \
	(((unsigned) (space) << 5 ^ \
	  ((unsigned) (offset) >> 19 | (unsigned) (space) << 13) ^ \
	  (unsigned) (offset) >> 11) & (HP800_HASHSIZE-1))

struct mapping {
	queue_head_t	hash_link;	/* hash table links */
	queue_head_t	phys_link;	/* for mappings of a given PA */
	space_t		space;		/* virtual space */
	unsigned	offset;		/* virtual page number */
	unsigned	tlbpage;	/* physical page (for TLB load) */
	unsigned	tlbprot;	/* prot/access rights (for TLB load) */
	struct pmap	*pmap;		/* pmap mapping belongs to */
};

struct phys_entry {
	queue_head_t	phys_link;	/* head of mappings of a given PA */
	struct mapping	*writer;	/* mapping with R/W access */
	unsigned	tlbprot;	/* TLB format protection */
};

#endif

#define atop(a)		((unsigned)(a) >> 11)
#define ptoa(p)		((unsigned)(p) << 11)
#define trunc_page(a)	((unsigned)(a) & ~2047)

STATIC long equiv_end;
STATIC queue_head_t *Ovtop_table, *vtop_table, *Ofree_mapping, free_mapping;
STATIC struct phys_entry *Ophys_table, *phys_table;
STATIC long vm_last_phys, vm_first_phys;
STATIC struct mapping *firstmap, *lastmap, *Omap_table, *map_table;
STATIC unsigned Omlow, Omhigh, Omhead, Ovlow, Ovhigh, Oplow, Ophigh;
STATIC unsigned mlow, mhigh, mhead, vlow, vhigh, plow, phigh;
STATIC int vtopsize, physsize, mapsize;
STATIC int kmemfd;

#define IS_OVTOPPTR(p)	((unsigned)(p) >= Ovlow && (unsigned)(p) < Ovhigh)
#define IS_OMAPPTR(p)	((unsigned)(p) >= Omlow && (unsigned)(p) < Omhigh)
#define IS_OPHYSPTR(p)	((unsigned)(p) >= Oplow && (unsigned)(p) < Ophigh)
#define IS_VTOPPTR(p)	((unsigned)(p) >= vlow && (unsigned)(p) < vhigh)
#define IS_MAPPTR(p)	((unsigned)(p) >= mlow && (unsigned)(p) < mhigh)
#define IS_PHYSPTR(p)	((unsigned)(p) >= plow && (unsigned)(p) < phigh)

struct mapstate {
	char	unused;
	char	flags;
	short	hashix;
	short	physix;
} *mapstate;

/* flags */
#define M_ISFREE	1
#define M_ISHASH	2
#define M_ISPHYS	4

mach_vtophys_init()
{
	int errors = 0;

	if (!readdata())
		errors++;
	if (!verifydata())
		errors++;
	if (!errors)
		return(1);
	fflush(stdout);
	fprintf(stderr,
		"translate: may not be able to translate all addresses\n");
	return(0);
}

mach_vtophys(space, off, pa)
	unsigned space, off, *pa;
{
	register int i;
	register queue_t qp;
	register struct mapping *mp;
	int poff;

	/*
	 * Kernel IO or equivilently mapped, one to one.
	 */
	if (space == 0 && (long)off < equiv_end) {
		*pa = off;
		return(1);
	}
	/*
	 * Else look it up in specified space
	 */
	poff = off - trunc_page(off);
	off = trunc_page(off);
	qp = &vtop_table[pmap_hash(space, off)];
	for (mp = (struct mapping *)qp->next;
	     qp != (queue_entry_t)mp;
	     mp = (struct mapping *)mp->hash_link.next) {
		if (mp->space == space && mp->offset == off) {
			*pa = (mp->tlbpage << 7) | poff;
			return(1);
		}
	}
	return(0);
}

STATIC
readdata()
{
	char *tmp, *mach_malloc();
	long size;

	/* easy scalars */
	mach_read("equiv_end", ~0, (char *)&equiv_end, sizeof equiv_end);
	mach_read("vm_first_phys", ~0,
		  (char *)&vm_first_phys, sizeof vm_first_phys);
	mach_read("vm_last_phys", ~0,
		  (char *)&vm_last_phys, sizeof vm_last_phys);
	mach_read("firstmap", ~0, (char *)&firstmap, sizeof firstmap);
	mach_read("lastmap", ~0, (char *)&lastmap, sizeof lastmap);

	/* virtual to physical hash table */
	vtopsize = HP800_HASHSIZE;
	size = vtopsize * sizeof(queue_head_t);
	tmp = mach_malloc("vtop table", size);
	mach_read("vtop_table", ~0, (char *)&Ovtop_table, sizeof Ovtop_table);
	mach_read("vtop table", (CORE_ADDR)Ovtop_table, tmp, size);
	vtop_table = (queue_head_t *) tmp;

	/* inverted page table */
	physsize = atop(vm_last_phys - vm_first_phys);
	size = physsize * sizeof(struct phys_entry);
	tmp = mach_malloc("phys table", size);
	mach_read("phys_table", ~0, (char *)&Ophys_table, sizeof Ophys_table);
	mach_read("phys table", (CORE_ADDR)Ophys_table, tmp, size);
	phys_table = (struct phys_entry *) tmp;

	/* mapping structures */
	Ofree_mapping = (queue_head_t *) ksym_lookup("free_mapping");
	mach_read("free mapping", (CORE_ADDR)Ofree_mapping,
		  (char *) &free_mapping, sizeof free_mapping);
	Omap_table = firstmap;
	mapsize = lastmap - firstmap;
	size = mapsize * sizeof(struct mapping);
	tmp = mach_malloc("mapping table", size);
	mach_read("mapping table", (CORE_ADDR)Omap_table, tmp, size);
	map_table = (struct mapping *) tmp;

	/* set limits */
	Ovlow = (unsigned) Ovtop_table;
	Ovhigh = (unsigned) &Ovtop_table[vtopsize];
	Oplow = (unsigned) Ophys_table;
	Ophigh = (unsigned) &Ophys_table[physsize];
	Omhead = (unsigned) Ofree_mapping;
	Omlow = (unsigned) firstmap;
	Omhigh = (unsigned) lastmap;
	mlow = (unsigned) map_table;
	mhigh = (unsigned) &map_table[mapsize];
	mhead = (unsigned) &free_mapping;
	vlow = (unsigned) vtop_table;
	vhigh = (unsigned) &vtop_table[vtopsize];
	plow = (unsigned) phys_table;
	phigh = (unsigned) &phys_table[physsize];

#if 0
	fprintf(stderr, "Ovtop [%#x-%#x) Ophys [%#x-%#x) Omap %#x [%#x-%#x)\n",
		Ovlow, Ovhigh, Oplow, Ophigh, Omhead, Omlow, Omhigh);
	fprintf(stderr, "vtop [%#x-%#x) phys [%#x-%#x) map %#x [%#x-%#x)\n",
		vlow, vhigh, plow, phigh, mhead, mlow, mhigh);
#endif
	return(adjustdata());
}

STATIC unsigned
ptrcvt(ptr)
	unsigned ptr;
{
	unsigned ret;
	char *str;

	if (ptr == 0) {
		ret = ptr;
		str = "null";
	} else if (IS_OVTOPPTR(ptr)) {
		ret = vlow + (ptr - Ovlow);
		str = "vtop";
	} else if (IS_OPHYSPTR(ptr)) {
		ret = plow + (ptr - Oplow);
		str = "phys";
	} else if (IS_OMAPPTR(ptr)) {
		ret = mlow + (ptr - Omlow);
		str = "map";
	} else if (ptr == Omhead) {
		ret = mhead;
		str = "maphead";
	} else {
		error("bogus pointer %#x", ptr);
		str = "wild";
		ret = ptr;
	}
#if 0
	fprintf(stderr, "%x (%s) -> %x\n", ptr, str, ret);
#endif
	return(ret);
}

STATIC int
adjustdata()
{
	register int i, lim;
	queue_head_t *nq;
	struct phys_entry *np;
	struct mapping *nm;

	/* hash table */
	lim = vtopsize;
	for (nq = vtop_table; nq < &vtop_table[lim]; nq++) {
		nq->next = (queue_entry_t) ptrcvt((unsigned)nq->next);
		nq->prev = (queue_entry_t) ptrcvt((unsigned)nq->prev);
	}

	/* IPT */
	lim = physsize;
	for (np = phys_table; np < &phys_table[lim]; np++) {
		np->phys_link.next = (queue_entry_t)
			ptrcvt((unsigned)np->phys_link.next);
		np->phys_link.prev = (queue_entry_t)
			ptrcvt((unsigned)np->phys_link.prev);
		np->writer = (struct mapping *) ptrcvt((unsigned)np->writer);
	}

	/* mapping table */
	free_mapping.next = (queue_entry_t)ptrcvt((unsigned)free_mapping.next);
	free_mapping.prev = (queue_entry_t)ptrcvt((unsigned)free_mapping.prev);
	lim = mapsize;
	for (nm = map_table; nm < &map_table[lim]; nm++) {
		nm->hash_link.next = (queue_entry_t)
			ptrcvt((unsigned)nm->hash_link.next);
		nm->hash_link.prev = (queue_entry_t)
			ptrcvt((unsigned)nm->hash_link.prev);
		nm->phys_link.next = (queue_entry_t)
			ptrcvt((unsigned)nm->phys_link.next);
		nm->phys_link.prev = (queue_entry_t)
			ptrcvt((unsigned)nm->phys_link.prev);
	}
	return(1);
}

/*
 * Consistency checks, make sure:
 *
 *	1. all mappings are accounted for
 *	2. no cycles
 *	3. no wild pointers
 *	4. consisent TLB state
 */
STATIC int
verifydata()
{
	register struct mapstate *ms;
	register int i;
	int errors = 0;

	mapstate = (struct mapstate *)
		mach_malloc("map state", mapsize * sizeof(struct mapstate));
	for (ms = mapstate; ms < &mapstate[mapsize]; ms++) {
		ms->flags = 0;
		ms->hashix = ms->physix = -2;
	}

	/*
	 * Check the free list
	 */
	checkhashchain(&free_mapping, M_ISFREE, -1);
	/*
	 * Check every hash chain
	 */
	for (i = 0; i < vtopsize; i++)
		checkhashchain(&vtop_table[i], M_ISHASH, i);
	/*
	 * Check every phys chain
	 */
	for (i = 0; i < physsize; i++)
		checkphyschain(&phys_table[i].phys_link, M_ISPHYS, i);
	/*
	 * Cycle through mapstate looking for anomolies
	 */
	ms = mapstate;
	for (i = 0; i < mapsize; i++) {
		switch (ms->flags) {
		case M_ISFREE:
		case M_ISHASH|M_ISPHYS:
			break;
		case 0:
			merror(ms, "not found");
			errors++;
			break;
		case M_ISHASH:
			merror(ms, "in vtop but not phys");
			errors++;
			break;
		case M_ISPHYS:
			merror(ms, "in phys but not vtop");
			errors++;
			break;
		default:
			merror(ms, "totally bogus");
			errors++;
			break;
		}
		ms++;
	}
	return(errors ? 0 : 1);
}

STATIC void
checkhashchain(qhp, flag, ix)
	queue_entry_t qhp;
{
	register queue_entry_t qp, pqp;
	register struct mapping *mp;
	struct mapstate *ms;

	qp = qhp->next;
	/*
	 * First element is not a mapping structure,
	 * chain must be empty.
	 */
	if (!IS_MAPPTR(qp)) {
		if (qp != qhp || qp != qhp->prev)
			fatal("bad vtop_table header pointer");
	} else {
		pqp = qhp;
		do {
			mp = (struct mapping *) qp;
			qp = &mp->hash_link;
			if (qp->prev != pqp)
				fatal("bad hash_link prev pointer");
			ms = &mapstate[mp-map_table];
			ms->flags |= flag;
			ms->hashix = ix;
			pqp = (queue_entry_t) mp;
			qp = qp->next;
		} while (IS_MAPPTR(qp));
		if (qp != qhp)
			fatal("bad hash_link next pointer");
	}
}

STATIC void
checkphyschain(qhp, flag, ix)
	queue_entry_t qhp;
{
	register queue_entry_t qp, pqp;
	register struct mapping *mp;
	struct mapstate *ms;

	qp = qhp->next;
	/*
	 * First element is not a mapping structure,
	 * chain must be empty.
	 */
	if (!IS_MAPPTR(qp)) {
		if (qp != qhp || qp != qhp->prev)
			fatal("bad phys_table header pointer");
	} else {
		pqp = qhp;
		do {
			mp = (struct mapping *) qp;
			qp = &mp->phys_link;
			if (qp->prev != pqp)
				fatal("bad phys_link prev pointer");
			ms = &mapstate[mp-map_table];
			ms->flags |= flag;
			ms->physix = ix;
			pqp = (queue_entry_t) mp;
			qp = qp->next;
		} while (IS_MAPPTR(qp));
		if (qp != qhp)
			fatal("bad phys_link next pointer");
	}
}

STATIC void
merror(ms, str)
	struct mapstate *ms;
	char *str;
{
	terminal_ours();
	fflush(stdout);
	fprintf(stderr,
		"vtophys: %s: %c%c%c, hashix %d, physix %d, mapping %x\n",
		str,
		(ms->flags & M_ISFREE) ? 'F' : '-',
		(ms->flags & M_ISHASH) ? 'H' : '-',
		(ms->flags & M_ISPHYS) ? 'P' : '-',
		ms->hashix, ms->physix, &map_table[ms-mapstate]);
	return_to_top_level();
}

STATIC int
mach_read(str, from, top, size)
	char *str;
	CORE_ADDR from;
	char *top;
	int size;
{
	CORE_ADDR paddr;

	if (from == ~0)
		from = ksym_lookup(str);
	paddr = vtophys(0, from);
	if (paddr == ~0 || physrd(paddr, top, size) != 0)
		fatal("cannot read %s", str);
}

STATIC char *
mach_malloc(str, size)
	char *str;
	int size;
{
	char *ptr = (char *) malloc(size);

	if (ptr == 0)
		fatal("no memory for %s", str);
	return(ptr);
}
#endif

#ifdef KERNELDEBUG
void
_initialize_hp9k8_dep()
{
	add_com ("process-address", class_obscure, set_paddr_command,
"The process identified by (ps-style) ADDR becomes the\n\
\"current\" process context for kernel debugging.");
	add_com_alias ("paddr", "process-address", class_obscure, 0);
	add_com ("virtual-to-physical", class_obscure, vtop_command,
"Translates the kernel virtual address ADDR into a physical address.");
	add_com_alias ("vtop", "virtual-to-physical", class_obscure, 0);
}
#endif
