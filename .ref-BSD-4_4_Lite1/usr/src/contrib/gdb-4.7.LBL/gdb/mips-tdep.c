/* Target-dependent code for the MIPS architecture, for GDB, the GNU Debugger.
   Copyright 1988, 1989, 1990, 1991, 1992 Free Software Foundation, Inc.
   Contributed by Alessandro Forin(af@cs.cmu.edu) at CMU
   and by Per Bothner(bothner@cs.wisc.edu) at U.Wisconsin.

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
#include "target.h"
#include "frame.h"
#include "inferior.h"
#include "symtab.h"
#include "value.h"
#include "gdbcmd.h"
#include "language.h"

#ifdef USG
#include <sys/types.h>
#endif

#include <sys/param.h>
#include <sys/dir.h>
#include <signal.h>
#include <sys/ioctl.h>

#include "gdbcore.h"
#include "symfile.h"
#include "objfiles.h"

#ifndef	MIPSMAGIC
#ifdef MIPSEL
#define MIPSMAGIC	MIPSELMAGIC
#else
#define MIPSMAGIC	MIPSEBMAGIC
#endif
#endif

#define VM_MIN_ADDRESS (unsigned)0x400000

#include <sys/user.h>		/* After a.out.h  */
#include <sys/file.h>
#include <sys/stat.h>

#ifdef KERNELDEBUG
extern int kernel_debugging;
#endif

/*
 * The MIPS architecture does not provide a trace bit.  However, most
 * (if not all) OS's that run on the MIPS emulate single stepping in
 * the kernel.  This unfortunately does not work for the kernel debugger.
 * Thus, we duplicate the functionality here.
 */

/*
 * For the instruction INSN at PC, if it is a control transfer, return the
 * target address, otherwise return 0.
 *
 * Instruction decoding per "MIPS RISC Architecture" by Gerry Kane
 * and Joe Heinrich, Prentice Hall, 1992.  Fig. A-3, Page A-143.
 */
CORE_ADDR
branchtarget(insn, pc)
	u_long insn;
	CORE_ADDR pc;
{
	int opcode = (insn >> 26) & 0x3f;
	u_long v;

	switch (opcode) {
	case 0x00:		/* SPECIAL */
		v = insn & 0x3f;
		if (v == 8 || v == 9) {
			int rs = (insn >> 21) & 0x1f;
			return (read_register(rs));
		}
		return (0);

	case 0x01:		/* REGIMM */
		v = (insn >> 16) & 0x1f;
		switch (v) {
		default:
			return (0);
		case 0x00:	/* BLTZ */
		case 0x01:	/* BGEZ */
		case 0x02:	/* BLTZL */
		case 0x03:	/* BGEZL */
		case 0x10:	/* BLTZAL */
		case 0x11:	/* BGEZAL */
		case 0x12:	/* BLTZALL */
		case 0x13:	/* BGEZALL */
			break;
		}
		/* fall through */
	case 0x04:		/* BEQ */
	case 0x05:		/* BNE */
	case 0x06:		/* BLEZ */
	case 0x07:		/* GTRZ */
		v = insn & 0xffff;
		if (v & 0x8000)
			v |= 0xffff0000;
		v <<= 2;
		return (pc + 4 + v);

	case 0x02:		/* J */
	case 0x03:		/* JAL */
		v = (insn & 0x03ffffff) << 2;
		return (((pc + 4) & 0xf0000000) | v);

	case 0x10:		/* COP0 */
	case 0x11:		/* COP1 */
	case 0x12:		/* COP2 */
	case 0x13:		/* COP3 */
		v = (insn >> 21) & 0x1f;
		if (v == 8) {
			v = insn & 0xffff;
			if (v & 0x8000)
				v |= 0xffff0000;
			v <<= 2;
			return (pc + 4 + v);
		}
	}
	return (0);
}

int
is_call_insn(insn)
	u_long insn;
{
	int opcode = (insn >> 26) & 0x3f;
	if (opcode == 3)
		/* JAL */
		return (1);
	if (opcode == 0) {
		/* SPECIAL */
		int v = insn & 0x3f;
		/* JALR */
		return (v == 9);
	}
	if (opcode == 1) {
		/* REGIMM */
		int v = (insn >> 19) & 3;
		/* BLTZAL etc. */
		return (v == 2);
	}
	return (0);
}
/*
 * Non-zero if we just simulated a single-step ptrace call.  This is
 * needed because we cannot remove the breakpoints in the inferior
 * process until after the `wait' in `wait_for_inferior'. 
 * Used for sun4.
 */
int one_stepped;

/*
 * single_step() is called just before we want to resume the inferior,
 * if we want to single-step it but there is no hardware or kernel single-step
 * support (as on all SPARCs).  We find all the possible targets of the
 * coming instruction and breakpoint them.
 *
 * single_step is also called just after the inferior stops.  If we had
 * set up a simulated single-step, we undo our damage.
 *
 * Code written by Gary Beihl (beihl@mcc.com); modified by Steven McCanne
 * (mccanne@ee.lbl.gov).
 */
void
single_step(signal)
	int signal;
{
	CORE_ADDR pc;
	u_long insn;
	/*
	 * Storage for temporary breakpoints.  XXX There should be a uniform
	 * interface for breakpoints, so that we could just set one then 
	 * clear it.  Note that we need only two outstanding breakpoints,
	 * since there can be at most two possiblilities for control flow.
	 */
	static CORE_ADDR target0;
	static CORE_ADDR target1;
	static char shadow0[4];
	static char shadow1[4];

	pc = read_register(PC_REGNUM);
	insn = read_memory_integer(pc, 4);

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
		if (step_over_calls > 0 && is_call_insn(insn)) {
			target0 = pc + 8;
			target1 = 0;
		} else {
			target0 = pc + 4;
			target1 = branchtarget(insn, pc);
			if (target1 != 0) {
				/*
				 * Punt on stepping through delay slot.
				 * On the sparc, this is easy because of
				 * npc; on the mips, it's trickier
				 * because we'd have to keep track
				 * of when we're in the delay slot (and
				 * gdb will get confused because the
				 * cpu leaves the epc pointing to the
				 * previous instruction).  Moreover, the
				 * architecture would require that we
				 * interpret the branch intruction if
				 * we want to set a breakpoint in the
				 * delay slot.
				 */
				target0 += 4;
			}
			if (target1 == target0)
				target1 = 0;
		}
		target_insert_breakpoint(target0, shadow0);
		if (target1)
			target_insert_breakpoint(target1, shadow1);
		one_stepped = 1;
	} else {
		/* Remove breakpoints */
		if (target1)
			target_remove_breakpoint(target1, shadow1);
		target_remove_breakpoint(target0, shadow0);
		one_stepped = 0;
	}
}


#define PROC_LOW_ADDR(proc) ((proc)->pdr.adr) /* least address */
#define PROC_HIGH_ADDR(proc) ((proc)->pdr.iline) /* upper address bound */
#define PROC_FRAME_OFFSET(proc) ((proc)->pdr.frameoffset)
#define PROC_FRAME_REG(proc) ((proc)->pdr.framereg)
#define PROC_REG_MASK(proc) ((proc)->pdr.regmask)
#define PROC_FREG_MASK(proc) ((proc)->pdr.fregmask)
#define PROC_REG_OFFSET(proc) ((proc)->pdr.regoffset)
#define PROC_FREG_OFFSET(proc) ((proc)->pdr.fregoffset)
#define PROC_PC_REG(proc) ((proc)->pdr.pcreg)
#define PROC_SYMBOL(proc) (*(struct symbol**)&(proc)->pdr.isym)
#define _PROC_MAGIC_ 0x0F0F0F0F
#define PROC_DESC_IS_DUMMY(proc) ((proc)->pdr.isym == _PROC_MAGIC_)
#define SET_PROC_DESC_IS_DUMMY(proc) ((proc)->pdr.isym = _PROC_MAGIC_)

struct linked_proc_info
{
  struct mips_extra_func_info info;
  struct linked_proc_info *next;
} *linked_proc_desc_table = NULL;


#define READ_FRAME_REG(fi, regno) read_next_frame_reg((fi)->next, regno)

static int
read_next_frame_reg(fi, regno)
     FRAME fi;
     int regno;
{
#define SIGFRAME_BASE   sizeof(struct sigcontext)
#define SIGFRAME_PC_OFF (-SIGFRAME_BASE+ 2*sizeof(int))
#define SIGFRAME_SP_OFF (-SIGFRAME_BASE+32*sizeof(int))
#define SIGFRAME_RA_OFF (-SIGFRAME_BASE+34*sizeof(int))
  for (; fi; fi = fi->next) {
#ifdef KERNELDEBUG
      if (kernel_debugging && in_trap_handler(fi->pc)) {
	  int offset;
	  if (regno == PC_REGNUM) offset = 40;
	  else if (regno == RA_REGNUM) offset = 34;
	  else if (regno == SP_REGNUM) offset = 32;
	  else return 0;
	  return read_memory_integer(fi->frame + 4 * offset, 4);
      }
#endif
      if (in_sigtramp(fi->pc, 0)) {
	  /* No idea if this code works. --PB. */
	  int offset;
	  if (regno == PC_REGNUM) offset = SIGFRAME_PC_OFF;
	  else if (regno == RA_REGNUM) offset = SIGFRAME_RA_OFF;
	  else if (regno == SP_REGNUM) offset = SIGFRAME_SP_OFF;
	  else return 0;
	  return read_memory_integer(fi->frame + offset, 4);
      }
      else if (regno == SP_REGNUM) return fi->frame;
      else if (fi->saved_regs->regs[regno])
	return read_memory_integer(fi->saved_regs->regs[regno], 4);
  }
  return read_register(regno);
}

int
mips_frame_saved_pc(frame)
     FRAME frame;
{
  mips_extra_func_info_t proc_desc = frame->proc_desc;
  int pcreg = proc_desc ? PROC_PC_REG(proc_desc) : RA_REGNUM;

#ifdef KERNELDEBUG
  if (kernel_debugging && in_trap_handler(frame->pc))
	  return read_memory_integer(frame->frame + 4 * 40, 4);
#endif
  if (proc_desc && PROC_DESC_IS_DUMMY(proc_desc))
      return read_memory_integer(frame->frame - 4, 4);

  return read_next_frame_reg(frame, pcreg);
}

static struct mips_extra_func_info temp_proc_desc;
static struct frame_saved_regs temp_saved_regs;

static CORE_ADDR
heuristic_proc_start(pc)
    CORE_ADDR pc;
{
    CORE_ADDR start_pc = pc;
    CORE_ADDR fence = start_pc - 200;

    if (start_pc == 0)	return 0;
    if (fence < VM_MIN_ADDRESS) fence = VM_MIN_ADDRESS;

    /* search back for previous return */
    for (start_pc -= 4; ; start_pc -= 4)
	if (start_pc < fence) return 0; 
	else if (ABOUT_TO_RETURN(start_pc))
	    break;

    start_pc += 8; /* skip return, and its delay slot */
#if 0
    /* skip nops (usually 1) 0 - is this */
    while (start_pc < pc && read_memory_integer (start_pc, 4) == 0)
	start_pc += 4;
#endif
    return start_pc;
}

static mips_extra_func_info_t
heuristic_proc_desc(start_pc, limit_pc, next_frame)
    CORE_ADDR start_pc, limit_pc;
    FRAME next_frame;
{
    CORE_ADDR sp = next_frame ? next_frame->frame : read_register (SP_REGNUM);
    CORE_ADDR cur_pc;
    int frame_size;
    int has_frame_reg = 0;
    int reg30; /* Value of $r30. Used by gcc for frame-pointer */
    unsigned long reg_mask = 0;

    if (start_pc == 0) return NULL;
    bzero(&temp_proc_desc, sizeof(temp_proc_desc));
    bzero(&temp_saved_regs, sizeof(struct frame_saved_regs));
    PROC_LOW_ADDR(&temp_proc_desc) = start_pc;

    if (start_pc + 200 < limit_pc) limit_pc = start_pc + 200;
  restart:
    frame_size = 0;
    for (cur_pc = start_pc; cur_pc < limit_pc; cur_pc += 4) {
	unsigned long word;
	int status;

	status = read_memory_nobpt (cur_pc, (char *)&word, 4); 
	if (status) memory_error (status, cur_pc); 
	SWAP_TARGET_AND_HOST (&word, sizeof (word));
	if ((word & 0xFFFF0000) == 0x27bd0000) /* addiu $sp,$sp,-i */
	    frame_size += (-word) & 0xFFFF;
	else if ((word & 0xFFFF0000) == 0x23bd0000) /* addu $sp,$sp,-i */
	    frame_size += (-word) & 0xFFFF;
	else if ((word & 0xFFE00000) == 0xafa00000) { /* sw reg,offset($sp) */
	    int reg = (word & 0x001F0000) >> 16;
	    reg_mask |= 1 << reg;
	    temp_saved_regs.regs[reg] = sp + (short)word;
	}
	else if ((word & 0xFFFF0000) == 0x27be0000) { /* addiu $30,$sp,size */
	    if ((unsigned short)word != frame_size)
		reg30 = sp + (unsigned short)word;
	    else if (!has_frame_reg) {
		int alloca_adjust;
		has_frame_reg = 1;
		reg30 = read_next_frame_reg(next_frame, 30);
		alloca_adjust = reg30 - (sp + (unsigned short)word);
		if (alloca_adjust > 0) {
		    /* FP > SP + frame_size. This may be because
		    /* of an alloca or somethings similar.
		     * Fix sp to "pre-alloca" value, and try again.
		     */
		    sp += alloca_adjust;
		    goto restart;
		}
	    }
	}
	else if ((word & 0xFFE00000) == 0xafc00000) { /* sw reg,offset($30) */
	    int reg = (word & 0x001F0000) >> 16;
	    reg_mask |= 1 << reg;
	    temp_saved_regs.regs[reg] = reg30 + (short)word;
	}
    }
    if (has_frame_reg) {
	PROC_FRAME_REG(&temp_proc_desc) = 30;
	PROC_FRAME_OFFSET(&temp_proc_desc) = 0;
    }
    else {
	PROC_FRAME_REG(&temp_proc_desc) = SP_REGNUM;
	PROC_FRAME_OFFSET(&temp_proc_desc) = frame_size;
    }
    PROC_REG_MASK(&temp_proc_desc) = reg_mask;
    PROC_PC_REG(&temp_proc_desc) = RA_REGNUM;
    return &temp_proc_desc;
}

static mips_extra_func_info_t
find_proc_desc(pc, next_frame)
    CORE_ADDR pc;
    FRAME next_frame;
{
  mips_extra_func_info_t proc_desc;
  struct block *b = block_for_pc(pc);
  struct symbol *sym =
      b ? lookup_symbol(MIPS_EFI_SYMBOL_NAME, b, LABEL_NAMESPACE, 0, NULL) : NULL;

  if (sym)
    {
	/* IF this is the topmost frame AND
	 * (this proc does not have debugging information OR
	 * the PC is in the procedure prologue)
	 * THEN create a "heuristic" proc_desc (by analyzing
	 * the actual code) to replace the "official" proc_desc.
	 */
	proc_desc = (mips_extra_func_info_t)SYMBOL_VALUE(sym);
	if (next_frame == NULL) {
	    struct symtab_and_line val;
	    struct symbol *proc_symbol =
		PROC_DESC_IS_DUMMY(proc_desc) ? 0 : PROC_SYMBOL(proc_desc);

	    if (proc_symbol) {
		val = find_pc_line (BLOCK_START
				    (SYMBOL_BLOCK_VALUE(proc_symbol)),
				    0);
		val.pc = val.end ? val.end : pc;
	    }
	    if (!proc_symbol || pc < val.pc) {
		mips_extra_func_info_t found_heuristic =
		    heuristic_proc_desc(PROC_LOW_ADDR(proc_desc),
					pc, next_frame);
		if (found_heuristic) proc_desc = found_heuristic;
	    }
	}
    }
  else
    {
      /* Is linked_proc_desc_table really necessary?  It only seems to be used
	 by procedure call dummys.  However, the procedures being called ought
	 to have their own proc_descs, and even if they don't,
	 heuristic_proc_desc knows how to create them! */

      register struct linked_proc_info *link;
      for (link = linked_proc_desc_table; link; link = link->next)
	  if (PROC_LOW_ADDR(&link->info) <= pc
	      && PROC_HIGH_ADDR(&link->info) > pc)
	      return &link->info;
      proc_desc =
	  heuristic_proc_desc(heuristic_proc_start(pc), pc, next_frame);
    }
  return proc_desc;
}

/* XXX this is not a cache (i.e., you don't invalidate it) */
mips_extra_func_info_t cached_proc_desc;

FRAME_ADDR
mips_frame_chain(frame)
    FRAME frame;
{
    mips_extra_func_info_t proc_desc;
    CORE_ADDR saved_pc = FRAME_SAVED_PC(frame);

#ifdef KERNELDEBUG
    if (kernel_debugging) {
	    if (!inside_kernstack(frame->frame))
		    return (0);
	    if (in_trap_handler(saved_pc)) {
		    /* XXX this works? */
		    cached_proc_desc = find_proc_desc(saved_pc, frame);
		    /*XXX need to lookup frame of parent */
		    return read_memory_integer(frame->frame + 4 * 32, 4);
	    }
    } else
#endif
	    if (saved_pc == 0 || inside_entry_file (saved_pc))
		    return 0;

    proc_desc = find_proc_desc(saved_pc, frame);
    if (!proc_desc)
      return 0;

    cached_proc_desc = proc_desc;
    return read_next_frame_reg(frame, PROC_FRAME_REG(proc_desc))
      + PROC_FRAME_OFFSET(proc_desc);
}

void
init_extra_frame_info(fci)
     struct frame_info *fci;
{
  extern struct obstack frame_cache_obstack;
  /* Use proc_desc calculated in frame_chain */
  mips_extra_func_info_t proc_desc = fci->next ? cached_proc_desc :
      find_proc_desc(fci->pc, fci->next);

  fci->saved_regs = (struct frame_saved_regs*)
    obstack_alloc (&frame_cache_obstack, sizeof(struct frame_saved_regs));
  bzero(fci->saved_regs, sizeof(struct frame_saved_regs));
  fci->proc_desc =
      proc_desc == &temp_proc_desc ? 0 : proc_desc;

#ifdef KERNELDEBUG
  if (kernel_debugging && in_trap_handler(fci->pc)) {
	  int i;
	  if (fci->next != 0)
		  fci->frame = READ_FRAME_REG(fci, SP_REGNUM);
	  else
		  fci->frame = read_register(SP_REGNUM);
	  for (i = 1; i <= 32; ++i)
		  fci->saved_regs->regs[i] = fci->frame + 4 * (3 + i);
	  return;
  }
#endif
  if (proc_desc)
    {
      int ireg;
      CORE_ADDR reg_position;
      unsigned long mask;
      /* r0 bit means kernel trap */
      int kernel_trap = PROC_REG_MASK(proc_desc) & 1;

      /* Fixup frame-pointer - only needed for top frame */
      /* This may not be quite right, if proc has a real frame register */
      if (fci->pc == PROC_LOW_ADDR(proc_desc))
	fci->frame = read_register (SP_REGNUM);
      else if (fci->next == 0)
	fci->frame = read_register (SP_REGNUM) + PROC_FRAME_OFFSET(proc_desc);
      else
	fci->frame = READ_FRAME_REG(fci, PROC_FRAME_REG(proc_desc))
	              + PROC_FRAME_OFFSET(proc_desc);
      if (proc_desc == &temp_proc_desc)
	  *fci->saved_regs = temp_saved_regs;
      else
      {
	  /* find which general-purpose registers were saved */
	  reg_position = fci->frame + PROC_REG_OFFSET(proc_desc);
	  mask = kernel_trap ? 0xFFFFFFFF : PROC_REG_MASK(proc_desc);
	  for (ireg= 31; mask; --ireg, mask <<= 1)
	      if (mask & 0x80000000)
	      {
		  fci->saved_regs->regs[ireg] = reg_position;
		  reg_position -= 4;
	      }
	  /* find which floating-point registers were saved */
	  reg_position = fci->frame + PROC_FREG_OFFSET(proc_desc);
	  /* The freg_offset points to where the first *double* register is saved.
	   * So skip to the high-order word. */
	  reg_position += 4;
	  mask = kernel_trap ? 0xFFFFFFFF : PROC_FREG_MASK(proc_desc);
	  for (ireg = 31; mask; --ireg, mask <<= 1)
	      if (mask & 0x80000000)
	      {
		  fci->saved_regs->regs[FP0_REGNUM+ireg] = reg_position;
		  reg_position -= 4;
	      }
      }

      /* hack: if argument regs are saved, guess these contain args */
      if ((PROC_REG_MASK(proc_desc) & 0xF0) == 0) fci->num_args = -1;
      else if ((PROC_REG_MASK(proc_desc) & 0x80) == 0) fci->num_args = 4;
      else if ((PROC_REG_MASK(proc_desc) & 0x40) == 0) fci->num_args = 3;
      else if ((PROC_REG_MASK(proc_desc) & 0x20) == 0) fci->num_args = 2;
      else if ((PROC_REG_MASK(proc_desc) & 0x10) == 0) fci->num_args = 1;

      fci->saved_regs->regs[PC_REGNUM] = fci->saved_regs->regs[RA_REGNUM];
    }
}

/* MIPS stack frames are almost impenetrable.  When execution stops,
   we basically have to look at symbol information for the function
   that we stopped in, which tells us *which* register (if any) is
   the base of the frame pointer, and what offset from that register
   the frame itself is at.  

   This presents a problem when trying to examine a stack in memory
   (that isn't executing at the moment), using the "frame" command.  We
   don't have a PC, nor do we have any registers except SP.

   This routine takes two arguments, SP and PC, and tries to make the
   cached frames look as if these two arguments defined a frame on the
   cache.  This allows the rest of info frame to extract the important
   arguments without difficulty.  */

FRAME
setup_arbitrary_frame (stack, pc)
     FRAME_ADDR stack;
     CORE_ADDR pc;
{
  return create_new_frame (stack, pc);
}


CORE_ADDR
mips_push_arguments(nargs, args, sp, struct_return, struct_addr)
  int nargs;
  value *args;
  CORE_ADDR sp;
  int struct_return;
  CORE_ADDR struct_addr;
{
  CORE_ADDR buf;
  register i;
  int accumulate_size = struct_return ? 4 : 0;
  struct mips_arg { char *contents; int len; int offset; };
  struct mips_arg *mips_args =
      (struct mips_arg*)alloca(nargs * sizeof(struct mips_arg));
  register struct mips_arg *m_arg;
  for (i = 0, m_arg = mips_args; i < nargs; i++, m_arg++) {
    extern value value_arg_coerce();
    value arg = value_arg_coerce (args[i]);
    m_arg->len = TYPE_LENGTH (VALUE_TYPE (arg));
    /* This entire mips-specific routine is because doubles must be aligned
     * on 8-byte boundaries. It still isn't quite right, because MIPS decided
     * to align 'struct {int a, b}' on 4-byte boundaries (even though this
     * breaks their varargs implementation...). A correct solution
     * requires an simulation of gcc's 'alignof' (and use of 'alignof'
     * in stdarg.h/varargs.h).
     */
    if (m_arg->len > 4) accumulate_size = (accumulate_size + 7) & -8;
    m_arg->offset = accumulate_size;
    accumulate_size = (accumulate_size + m_arg->len + 3) & -4;
    m_arg->contents = VALUE_CONTENTS(arg);
  }
  accumulate_size = (accumulate_size + 7) & (-8);
  if (accumulate_size < 16) accumulate_size = 16; 
  sp -= accumulate_size;
  for (i = nargs; m_arg--, --i >= 0; )
    write_memory(sp + m_arg->offset, m_arg->contents, m_arg->len);
  if (struct_return) {
    buf = struct_addr;
    write_memory(sp, (char *)&buf, sizeof(CORE_ADDR));
  }
  return sp;
}

/* MASK(i,j) == (1<<i) + (1<<(i+1)) + ... + (1<<j)). Assume i<=j<31. */
#define MASK(i,j) ((1 << (j)+1)-1 ^ (1 << (i))-1)

void
mips_push_dummy_frame()
{
  int ireg;
  struct linked_proc_info *link = (struct linked_proc_info*)
      xmalloc(sizeof(struct linked_proc_info));
  mips_extra_func_info_t proc_desc = &link->info;
  CORE_ADDR sp = read_register (SP_REGNUM);
  CORE_ADDR save_address;
  REGISTER_TYPE buffer;
  link->next = linked_proc_desc_table;
  linked_proc_desc_table = link;
#define PUSH_FP_REGNUM 16 /* must be a register preserved across calls */
#define GEN_REG_SAVE_MASK MASK(1,16)|MASK(24,28)|(1<<31)
#define GEN_REG_SAVE_COUNT 22
#define FLOAT_REG_SAVE_MASK MASK(0,19)
#define FLOAT_REG_SAVE_COUNT 20
#define SPECIAL_REG_SAVE_COUNT 4
  /*
   * The registers we must save are all those not preserved across
   * procedure calls. Dest_Reg (see tm-mips.h) must also be saved.
   * In addition, we must save the PC, and PUSH_FP_REGNUM.
   * (Ideally, we should also save MDLO/-HI and FP Control/Status reg.)
   *
   * Dummy frame layout:
   *  (high memory)
   * 	Saved PC
   *	Saved MMHI, MMLO, FPC_CSR
   *	Saved R31
   *	Saved R28
   *	...
   *	Saved R1
   *    Saved D18 (i.e. F19, F18)
   *    ...
   *    Saved D0 (i.e. F1, F0)
   *	CALL_DUMMY (subroutine stub; see tm-mips.h)
   *	Parameter build area (not yet implemented)
   *  (low memory)
   */
  PROC_REG_MASK(proc_desc) = GEN_REG_SAVE_MASK;
  PROC_FREG_MASK(proc_desc) = FLOAT_REG_SAVE_MASK;
  PROC_REG_OFFSET(proc_desc) = /* offset of (Saved R31) from FP */
      -sizeof(long) - 4 * SPECIAL_REG_SAVE_COUNT;
  PROC_FREG_OFFSET(proc_desc) = /* offset of (Saved D18) from FP */
      -sizeof(double) - 4 * (SPECIAL_REG_SAVE_COUNT + GEN_REG_SAVE_COUNT);
  /* save general registers */
  save_address = sp + PROC_REG_OFFSET(proc_desc);
  for (ireg = 32; --ireg >= 0; )
    if (PROC_REG_MASK(proc_desc) & (1 << ireg))
      {
	buffer = read_register (ireg);
	write_memory (save_address, (char *)&buffer, sizeof(REGISTER_TYPE));
	save_address -= 4;
      }
  /* save floating-points registers */
  save_address = sp + PROC_FREG_OFFSET(proc_desc);
  for (ireg = 32; --ireg >= 0; )
    if (PROC_FREG_MASK(proc_desc) & (1 << ireg))
      {
	buffer = read_register (ireg + FP0_REGNUM);
	write_memory (save_address, (char *)&buffer, 4);
	save_address -= 4;
      }
  write_register (PUSH_FP_REGNUM, sp);
  PROC_FRAME_REG(proc_desc) = PUSH_FP_REGNUM;
  PROC_FRAME_OFFSET(proc_desc) = 0;
  buffer = read_register (PC_REGNUM);
  write_memory (sp - 4, (char *)&buffer, sizeof(REGISTER_TYPE));
  buffer = read_register (HI_REGNUM);
  write_memory (sp - 8, (char *)&buffer, sizeof(REGISTER_TYPE));
  buffer = read_register (LO_REGNUM);
  write_memory (sp - 12, (char *)&buffer, sizeof(REGISTER_TYPE));
  buffer = read_register (FCRCS_REGNUM);
  write_memory (sp - 16, (char *)&buffer, sizeof(REGISTER_TYPE));
  sp -= 4 * (GEN_REG_SAVE_COUNT+FLOAT_REG_SAVE_COUNT+SPECIAL_REG_SAVE_COUNT);
  write_register (SP_REGNUM, sp);
  PROC_LOW_ADDR(proc_desc) = sp - CALL_DUMMY_SIZE + CALL_DUMMY_START_OFFSET;
  PROC_HIGH_ADDR(proc_desc) = sp;
  SET_PROC_DESC_IS_DUMMY(proc_desc);
  PROC_PC_REG(proc_desc) = RA_REGNUM;
}

void
mips_pop_frame()
{
  register int regnum;
  FRAME frame = get_current_frame ();
  CORE_ADDR new_sp = frame->frame;

  mips_extra_func_info_t proc_desc = frame->proc_desc;

  write_register (PC_REGNUM, FRAME_SAVED_PC(frame));
  if (proc_desc)
    {
      for (regnum = 32; --regnum >= 0; )
	if (PROC_REG_MASK(proc_desc) & (1 << regnum))
	  write_register (regnum,
			  read_memory_integer (frame->saved_regs->regs[regnum],
					       4));
      for (regnum = 32; --regnum >= 0; )
	if (PROC_FREG_MASK(proc_desc) & (1 << regnum))
	  write_register (regnum + FP0_REGNUM,
			  read_memory_integer (frame->saved_regs->regs[regnum + FP0_REGNUM], 4));
    }
  write_register (SP_REGNUM, new_sp);
  flush_cached_frames ();
  /* We let mips_init_extra_frame_info figure out the frame pointer */
  set_current_frame (create_new_frame (0, read_pc ()));

  if (PROC_DESC_IS_DUMMY(proc_desc))
    {
      struct linked_proc_info *pi_ptr, *prev_ptr;

      for (pi_ptr = linked_proc_desc_table, prev_ptr = NULL;
	   pi_ptr != NULL;
	   prev_ptr = pi_ptr, pi_ptr = pi_ptr->next)
	{
	  if (&pi_ptr->info == proc_desc)
	    break;
	}

      if (pi_ptr == NULL)
	error ("Can't locate dummy extra frame info\n");

      if (prev_ptr != NULL)
	prev_ptr->next = pi_ptr->next;
      else
	linked_proc_desc_table = pi_ptr->next;

      free (pi_ptr);

      write_register (HI_REGNUM, read_memory_integer(new_sp - 8, 4));
      write_register (LO_REGNUM, read_memory_integer(new_sp - 12, 4));
      write_register (FCRCS_REGNUM, read_memory_integer(new_sp - 16, 4));
    }
}

static void
mips_print_register (regnum, all)
     int regnum, all;
{
      unsigned char raw_buffer[MAX_REGISTER_RAW_SIZE * 2]; /* *2 for doubles */
      REGISTER_TYPE val;

      /* Get the data in raw format.  */
      if (read_relative_register_raw_bytes (regnum, raw_buffer))
	{
	  printf_filtered ("%s: [Invalid]", reg_names[regnum]);
	  return;
	}
      
      /* If an even floating pointer register, also print as double. */
      if (regnum >= FP0_REGNUM && regnum < FP0_REGNUM+32
	  && !((regnum-FP0_REGNUM) & 1)) {
	  read_relative_register_raw_bytes (regnum+1, raw_buffer+4);
	  printf_filtered ("(d%d: ", regnum-FP0_REGNUM);
	  val_print (builtin_type_double, raw_buffer, 0,
		     stdout, 0, 1, 0, Val_pretty_default);
	  printf_filtered ("); ");
      }
      fputs_filtered (reg_names[regnum], stdout);
#ifndef NUMERIC_REG_NAMES
      if (regnum < 32)
	  printf_filtered ("(r%d): ", regnum);
      else
#endif
	  printf_filtered (": ");

      /* If virtual format is floating, print it that way.  */
      if (TYPE_CODE (REGISTER_VIRTUAL_TYPE (regnum)) == TYPE_CODE_FLT
	  && ! INVALID_FLOAT (raw_buffer, REGISTER_VIRTUAL_SIZE(regnum))) {
	  val_print (REGISTER_VIRTUAL_TYPE (regnum), raw_buffer, 0,
		     stdout, 0, 1, 0, Val_pretty_default);
      }
      /* Else print as integer in hex.  */
      else
	{
	  long val;

	  bcopy (raw_buffer, &val, sizeof (long));
	  SWAP_TARGET_AND_HOST ((char *)&val, sizeof (long));
	  if (val == 0)
	    printf_filtered ("0");
	  else if (all)
	    printf_filtered (local_hex_format(), val);
	  else
	    printf_filtered ("%s=%d", local_hex_string(val), val);
	}
}

/* Replacement for generic do_registers_info.  */
void
mips_do_registers_info (regnum, fpregs)
     int regnum;
     int fpregs;
{
  if (regnum != -1) {
      mips_print_register (regnum, 0);
      printf_filtered ("\n");
  }
  else {
      for (regnum = 0; regnum < NUM_REGS; ) {
	  if ((!fpregs) && regnum >= FP0_REGNUM && regnum <= FCRIR_REGNUM) {
	    regnum++;
	    continue;
	  }
	  mips_print_register (regnum, 1);
	  regnum++;
	  if ((regnum & 3) == 0 || regnum == NUM_REGS)
	      printf_filtered (";\n");
	  else
	      printf_filtered ("; ");
      }
  }
}
/* Return number of args passed to a frame. described by FIP.
   Can return -1, meaning no way to tell.  */

int
mips_frame_num_args(fip)
	FRAME fip;
{
#if 0
	struct chain_info_t *p;

	p = mips_find_cached_frame(FRAME_FP(fip));
	if (p->valid)
		return p->the_info.numargs;
#endif
	return -1;
}


/* Bad floats: Returns 0 if P points to a valid IEEE floating point number,
   1 if P points to a denormalized number or a NaN. LEN says whether this is
   a single-precision or double-precision float */
#define SINGLE_EXP_BITS  8
#define DOUBLE_EXP_BITS 11
int
isa_NAN(p, len)
     int *p, len;
{
  int exponent;
  if (len == 4)
    {
      exponent = *p;
      exponent = exponent << 1 >> (32 - SINGLE_EXP_BITS - 1);
      return ((exponent == -1) || (! exponent && *p));
    }
  else if (len == 8)
    {
      exponent = *(p+1);
      exponent = exponent << 1 >> (32 - DOUBLE_EXP_BITS - 1);
      return ((exponent == -1) || (! exponent && *p * *(p+1)));
    }
  else return 1;
}

/* To skip prologues, I use this predicate. Returns either PC
   itself if the code at PC does not look like a function prologue,
   PC+4 if it does (our caller does not need anything more fancy). */

CORE_ADDR
mips_skip_prologue(pc)
     CORE_ADDR pc;
{
    struct symbol *f;
    struct block *b;
    unsigned long inst;
    int offset;

    /* For -g modules and most functions anyways the
       first instruction adjusts the stack.
       But we allow some number of stores before the stack adjustment.
       (These are emitted by varags functions compiled by gcc-2.0. */
    for (offset = 0; offset < 100; offset += 4) {
	inst = read_memory_integer(pc + offset, 4);
	if ((inst & 0xffff0000) == 0x27bd0000) /* addiu $sp,$sp,offset */
	    return pc + offset + 4;
	if ((inst & 0xFFE00000) != 0xAFA00000) /* sw reg,n($sp) */
	    break;
    }

    /* Well, it looks like a frameless. Let's make sure.
       Note that we are not called on the current PC,
       but on the function`s start PC, and I have definitely
       seen optimized code that adjusts the SP quite later */
    b = block_for_pc(pc);
    if (!b) return pc;

    f = lookup_symbol(MIPS_EFI_SYMBOL_NAME, b, LABEL_NAMESPACE, 0, NULL);
    if (!f) return pc;
    /* Ideally, I would like to use the adjusted info
       from mips_frame_info(), but for all practical
       purposes it will not matter (and it would require
       a different definition of SKIP_PROLOGUE())

       Actually, it would not hurt to skip the storing
       of arguments on the stack as well. */
    if (((mips_extra_func_info_t)SYMBOL_VALUE(f))->pdr.frameoffset)
	return pc + 4;

    return pc;
}

#ifdef KERNELDEBUG
/*XXX*/
CORE_ADDR intstack_top;
CORE_ADDR intstack_bottom;

CORE_ADDR kernstack_top;
CORE_ADDR kernstack_bottom;
CORE_ADDR trap_pc_start;
CORE_ADDR trap_pc_end;

int
in_trap_handler(pc)
	CORE_ADDR pc;
{
	return (pc >= trap_pc_start && pc <= trap_pc_end);
}

int
inside_kernstack(addr)
	CORE_ADDR addr;
{
	return (addr < kernstack_top && addr >= kernstack_bottom);
}

/*
 * (re-)set the variables that make inside_kernstack() work.
 */
void
set_kernel_boundaries()
{
        if (trap_pc_start == 0) {
		trap_pc_start = ksym_lookup("trap_handler_start");
		trap_pc_end = ksym_lookup("trap_handler_end");
        }
        kernstack_bottom = UADDR + sizeof(struct user);
        kernstack_top = KERNELSTACK;
}

/*
 * Called from remote_wait, after the remote kernel has stopped.
 * Look up the current proc, and set up boundaries.
 * This is for active kernels only.
 */
void
set_curproc()
{
#ifdef notdef
	cpudata = fetch_cpudata();
#endif
	set_kernel_boundaries();
}

/*
 * Fetch registers from a crashdump or /dev/kmem.
 */
void
kvm_fetch_registers(uaddr)
	CORE_ADDR uaddr;
{
	int i;
	u_long cps, reg, sp;
	float freg;
	struct pcb pcb;

	if (target_read_memory(uaddr, (char *)&pcb, sizeof(pcb)) != 0)
		error("cannot read pcb at 0x%x", uaddr);
	/*
	 * Invalidate all the registers then fill in the ones we know about.
	 */
	registers_changed();

	for (i = 0; i < 32; ++i)
		supply_register(i, (char *)&pcb.pcb_regs[i]);
	supply_register(PC_REGNUM, (char *)&pcb.pcb_pc);
	supply_register(PS_REGNUM, (char *)&pcb.pcb_sr);
}

/*
 * Set the process context to that of the proc structure at
 * system address paddr.  Read in the register state.
 */
int
set_procaddr(addr)
	CORE_ADDR addr;
{
	if (addr == 0)
		addr = ksym_lookup("u");

#ifdef notdef
	if (addr < KERNELBASE)
		return (1);
#endif
	set_kernel_boundaries();
	kvm_fetch_registers(addr);
	return (0);
}

/*
 * Get the registers out of a crashdump or /dev/kmem.
 * XXX This somehow belongs in kcore.c.
 *
 * We just get all the registers, so we don't use regno.
 */
/* ARGSUSED */
void
kernel_core_registers(regno)
	int regno;
{
	/*
	 * Need to find current u area to get kernel stack and pcb
	 * where "panic" saved registers.
	 * (libkvm also needs to know current u area to get user
	 * address space mapping).
	 */
	/* XXX */
	(void)set_procaddr(0);
}

#endif
