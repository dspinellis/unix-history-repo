/* Target-machine dependent code for the Intel 960
   Copyright (C) 1991 Free Software Foundation, Inc.
   Contributed by Intel Corporation.
   examine_prologue and other parts contributed by Wind River Systems.

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

/* Miscellaneous i80960-dependent routines.
   Most are called from macros defined in "tm-i960.h".  */

#include "defs.h"
#include <signal.h>
#include "symtab.h"
#include "value.h"
#include "frame.h"
#include "ieee-float.h"

/* Structure of i960 extended floating point format.  */

const struct ext_format ext_format_i960 = {
/* tot sbyte smask expbyte manbyte */
   12, 9,    0x80, 9,8,	   4,0,		/* i960 */
};

/* gdb960 is always running on a non-960 host.  Check its characteristics.
   This routine must be called as part of gdb initialization.  */

static void
check_host()
{
	int i;

	static struct typestruct {
		int hostsize;		/* Size of type on host		*/
		int i960size;		/* Size of type on i960		*/
		char *typename;		/* Name of type, for error msg	*/
	} types[] = {
		{ sizeof(short),  2, "short" },
		{ sizeof(int),    4, "int" },
		{ sizeof(long),   4, "long" },
		{ sizeof(float),  4, "float" },
		{ sizeof(double), 8, "double" },
		{ sizeof(char *), 4, "pointer" },
	};
#define TYPELEN	(sizeof(types) / sizeof(struct typestruct))

	/* Make sure that host type sizes are same as i960
	 */
	for ( i = 0; i < TYPELEN; i++ ){
		if ( types[i].hostsize != types[i].i960size ){
			printf("sizeof(%s) != %d:  PROCEED AT YOUR OWN RISK!\n",
					types[i].typename, types[i].i960size );
		}

	}
}

/* Examine an i960 function prologue, recording the addresses at which
   registers are saved explicitly by the prologue code, and returning
   the address of the first instruction after the prologue (but not
   after the instruction at address LIMIT, as explained below).

   LIMIT places an upper bound on addresses of the instructions to be
   examined.  If the prologue code scan reaches LIMIT, the scan is
   aborted and LIMIT is returned.  This is used, when examining the
   prologue for the current frame, to keep examine_prologue () from
   claiming that a given register has been saved when in fact the
   instruction that saves it has not yet been executed.  LIMIT is used
   at other times to stop the scan when we hit code after the true
   function prologue (e.g. for the first source line) which might
   otherwise be mistaken for function prologue.

   The format of the function prologue matched by this routine is
   derived from examination of the source to gcc960 1.21, particularly
   the routine i960_function_prologue ().  A "regular expression" for
   the function prologue is given below:

   (lda LRn, g14
    mov g14, g[0-7]
    (mov 0, g14) | (lda 0, g14))?

   (mov[qtl]? g[0-15], r[4-15])*
   ((addo [1-31], sp, sp) | (lda n(sp), sp))?
   (st[qtl]? g[0-15], n(fp))*

   (cmpobne 0, g14, LFn
    mov sp, g14
    lda 0x30(sp), sp
    LFn: stq g0, (g14)
    stq g4, 0x10(g14)
    stq g8, 0x20(g14))?

   (st g14, n(fp))?
   (mov g13,r[4-15])?
*/

/* Macros for extracting fields from i960 instructions.  */

#define BITMASK(pos, width) (((0x1 << (width)) - 1) << (pos))
#define EXTRACT_FIELD(val, pos, width) ((val) >> (pos) & BITMASK (0, width))

#define REG_SRC1(insn)    EXTRACT_FIELD (insn, 0, 5)
#define REG_SRC2(insn)    EXTRACT_FIELD (insn, 14, 5)
#define REG_SRCDST(insn)  EXTRACT_FIELD (insn, 19, 5)
#define MEM_SRCDST(insn)  EXTRACT_FIELD (insn, 19, 5)
#define MEMA_OFFSET(insn) EXTRACT_FIELD (insn, 0, 12)

/* Fetch the instruction at ADDR, returning 0 if ADDR is beyond LIM or
   is not the address of a valid instruction, the address of the next
   instruction beyond ADDR otherwise.  *PWORD1 receives the first word
   of the instruction, and (for two-word instructions), *PWORD2 receives
   the second.  */

#define NEXT_PROLOGUE_INSN(addr, lim, pword1, pword2) \
  (((addr) < (lim)) ? next_insn (addr, pword1, pword2) : 0)

static CORE_ADDR
examine_prologue (ip, limit, frame_addr, fsr)
     register CORE_ADDR ip;
     register CORE_ADDR limit;
     FRAME_ADDR frame_addr;
     struct frame_saved_regs *fsr;
{
  register CORE_ADDR next_ip;
  register int src, dst;
  register unsigned int *pcode;
  unsigned int insn1, insn2;
  int size;
  int within_leaf_prologue;
  CORE_ADDR save_addr;
  static unsigned int varargs_prologue_code [] =
    {
       0x3507a00c,	/* cmpobne 0x0, g14, LFn */
       0x5cf01601,	/* mov sp, g14		 */
       0x8c086030,	/* lda 0x30(sp), sp	 */
       0xb2879000,	/* LFn: stq  g0, (g14)   */
       0xb2a7a010,	/* stq g4, 0x10(g14)	 */
       0xb2c7a020	/* stq g8, 0x20(g14)	 */
    };

  /* Accept a leaf procedure prologue code fragment if present.
     Note that ip might point to either the leaf or non-leaf
     entry point; we look for the non-leaf entry point first:  */

  within_leaf_prologue = 0;
  if ((next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn1, &insn2))
      && ((insn1 & 0xfffff000) == 0x8cf00000         /* lda LRx, g14 (MEMA) */
	  || (insn1 & 0xfffffc60) == 0x8cf03000))    /* lda LRx, g14 (MEMB) */
    {
      within_leaf_prologue = 1;
      next_ip = NEXT_PROLOGUE_INSN (next_ip, limit, &insn1, &insn2);
    }

  /* Now look for the prologue code at a leaf entry point:  */

  if (next_ip
      && (insn1 & 0xff87ffff) == 0x5c80161e         /* mov g14, gx */
      && REG_SRCDST (insn1) <= G0_REGNUM + 7)
    {
      within_leaf_prologue = 1;
      if ((next_ip = NEXT_PROLOGUE_INSN (next_ip, limit, &insn1, &insn2))
	  && (insn1 == 0x8cf00000                   /* lda 0, g14 */
	      || insn1 == 0x5cf01e00))              /* mov 0, g14 */
	{
	  ip = next_ip;
	  next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn1, &insn2);
	  within_leaf_prologue = 0;
	}
    }

  /* If something that looks like the beginning of a leaf prologue
     has been seen, but the remainder of the prologue is missing, bail.
     We don't know what we've got.  */

  if (within_leaf_prologue)
    return (ip);
	  
  /* Accept zero or more instances of "mov[qtl]? gx, ry", where y >= 4.
     This may cause us to mistake the moving of a register
     parameter to a local register for the saving of a callee-saved
     register, but that can't be helped, since with the
     "-fcall-saved" flag, any register can be made callee-saved.  */

  while (next_ip
	 && (insn1 & 0xfc802fb0) == 0x5c000610
	 && (dst = REG_SRCDST (insn1)) >= (R0_REGNUM + 4))
    {
      src = REG_SRC1 (insn1);
      size = EXTRACT_FIELD (insn1, 24, 2) + 1;
      save_addr = frame_addr + ((dst - R0_REGNUM) * 4);
      while (size--)
	{
	  fsr->regs[src++] = save_addr;
	  save_addr += 4;
	}
      ip = next_ip;
      next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn1, &insn2);
    }

  /* Accept an optional "addo n, sp, sp" or "lda n(sp), sp".  */

  if (next_ip &&
      ((insn1 & 0xffffffe0) == 0x59084800	/* addo n, sp, sp */
       || (insn1 & 0xfffff000) == 0x8c086000	/* lda n(sp), sp (MEMA) */
       || (insn1 & 0xfffffc60) == 0x8c087400))	/* lda n(sp), sp (MEMB) */
    {
      ip = next_ip;
      next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn1, &insn2);
    }

  /* Accept zero or more instances of "st[qtl]? gx, n(fp)".  
     This may cause us to mistake the copying of a register
     parameter to the frame for the saving of a callee-saved
     register, but that can't be helped, since with the
     "-fcall-saved" flag, any register can be made callee-saved.
     We can, however, refuse to accept a save of register g14,
     since that is matched explicitly below.  */

  while (next_ip &&
	 ((insn1 & 0xf787f000) == 0x9287e000      /* stl? gx, n(fp) (MEMA) */
	  || (insn1 & 0xf787fc60) == 0x9287f400   /* stl? gx, n(fp) (MEMB) */
	  || (insn1 & 0xef87f000) == 0xa287e000   /* st[tq] gx, n(fp) (MEMA) */
	  || (insn1 & 0xef87fc60) == 0xa287f400)  /* st[tq] gx, n(fp) (MEMB) */
	 && ((src = MEM_SRCDST (insn1)) != G14_REGNUM))
    {
      save_addr = frame_addr + ((insn1 & BITMASK (12, 1))
				? insn2 : MEMA_OFFSET (insn1));
      size = (insn1 & BITMASK (29, 1)) ? ((insn1 & BITMASK (28, 1)) ? 4 : 3)
	                               : ((insn1 & BITMASK (27, 1)) ? 2 : 1);
      while (size--)
	{
	  fsr->regs[src++] = save_addr;
	  save_addr += 4;
	}
      ip = next_ip;
      next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn1, &insn2);
    }

  /* Accept the varargs prologue code if present.  */

  size = sizeof (varargs_prologue_code) / sizeof (int);
  pcode = varargs_prologue_code;
  while (size-- && next_ip && *pcode++ == insn1)
    {
      ip = next_ip;
      next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn1, &insn2);
    }

  /* Accept an optional "st g14, n(fp)".  */

  if (next_ip &&
      ((insn1 & 0xfffff000) == 0x92f7e000	 /* st g14, n(fp) (MEMA) */
       || (insn1 & 0xfffffc60) == 0x92f7f400))   /* st g14, n(fp) (MEMB) */
    {
      fsr->regs[G14_REGNUM] = frame_addr + ((insn1 & BITMASK (12, 1))
				            ? insn2 : MEMA_OFFSET (insn1));
      ip = next_ip;
      next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn1, &insn2);
    }

  /* Accept zero or one instance of "mov g13, ry", where y >= 4.
     This is saving the address where a struct should be returned.  */

  if (next_ip
      && (insn1 & 0xff802fbf) == 0x5c00061d
      && (dst = REG_SRCDST (insn1)) >= (R0_REGNUM + 4))
    {
      save_addr = frame_addr + ((dst - R0_REGNUM) * 4);
      fsr->regs[G0_REGNUM+13] = save_addr;
      ip = next_ip;
#if 0  /* We'll need this once there is a subsequent instruction examined. */
      next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn1, &insn2);
#endif
    }

  return (ip);
}

/* Given an ip value corresponding to the start of a function,
   return the ip of the first instruction after the function 
   prologue.  */

CORE_ADDR
skip_prologue (ip)
     CORE_ADDR (ip);
{
  struct frame_saved_regs saved_regs_dummy;
  struct symtab_and_line sal;
  CORE_ADDR limit;

  sal = find_pc_line (ip, 0);
  limit = (sal.end) ? sal.end : 0xffffffff;

  return (examine_prologue (ip, limit, (FRAME_ADDR) 0, &saved_regs_dummy));
}

/* Put here the code to store, into a struct frame_saved_regs,
   the addresses of the saved registers of frame described by FRAME_INFO.
   This includes special registers such as pc and fp saved in special
   ways in the stack frame.  sp is even more special:
   the address we return for it IS the sp for the next frame.

   We cache the result of doing this in the frame_cache_obstack, since
   it is fairly expensive.  */

void
frame_find_saved_regs (fi, fsr)
     struct frame_info *fi;
     struct frame_saved_regs *fsr;
{
  register CORE_ADDR next_addr;
  register CORE_ADDR *saved_regs;
  register int regnum;
  register struct frame_saved_regs *cache_fsr;
  extern struct obstack frame_cache_obstack;
  CORE_ADDR ip;
  struct symtab_and_line sal;
  CORE_ADDR limit;

  if (!fi->fsr)
    {
      cache_fsr = (struct frame_saved_regs *)
		  obstack_alloc (&frame_cache_obstack,
				 sizeof (struct frame_saved_regs));
      bzero (cache_fsr, sizeof (struct frame_saved_regs));
      fi->fsr = cache_fsr;

      /* Find the start and end of the function prologue.  If the PC
	 is in the function prologue, we only consider the part that
	 has executed already.  */
         
      ip = get_pc_function_start (fi->pc);
      sal = find_pc_line (ip, 0);
      limit = (sal.end && sal.end < fi->pc) ? sal.end: fi->pc;

      examine_prologue (ip, limit, fi->frame, cache_fsr);

      /* Record the addresses at which the local registers are saved.
	 Strictly speaking, we should only do this for non-leaf procedures,
	 but no one will ever look at these values if it is a leaf procedure,
	 since local registers are always caller-saved.  */

      next_addr = (CORE_ADDR) fi->frame;
      saved_regs = cache_fsr->regs;
      for (regnum = R0_REGNUM; regnum <= R15_REGNUM; regnum++)
	{
	  *saved_regs++ = next_addr;
	  next_addr += 4;
	}

      cache_fsr->regs[FP_REGNUM] = cache_fsr->regs[PFP_REGNUM];
    }

  *fsr = *fi->fsr;

  /* Fetch the value of the sp from memory every time, since it
     is conceivable that it has changed since the cache was flushed.  
     This unfortunately undoes much of the savings from caching the 
     saved register values.  I suggest adding an argument to 
     get_frame_saved_regs () specifying the register number we're
     interested in (or -1 for all registers).  This would be passed
     through to FRAME_FIND_SAVED_REGS (), permitting more efficient
     computation of saved register addresses (e.g., on the i960,
     we don't have to examine the prologue to find local registers). 
	-- markf@wrs.com 
     FIXME, we don't need to refetch this, since the cache is cleared
     every time the child process is restarted.  If GDB itself
     modifies SP, it has to clear the cache by hand (does it?).  -gnu */

  fsr->regs[SP_REGNUM] = read_memory_integer (fsr->regs[SP_REGNUM], 4);
}

/* Return the address of the argument block for the frame
   described by FI.  Returns 0 if the address is unknown.  */

CORE_ADDR
frame_args_address (fi, must_be_correct)
     struct frame_info *fi;
{
  register FRAME frame;
  struct frame_saved_regs fsr;
  CORE_ADDR ap;

  /* If g14 was saved in the frame by the function prologue code, return
     the saved value.  If the frame is current and we are being sloppy,
     return the value of g14.  Otherwise, return zero.  */

  frame = FRAME_INFO_ID (fi);
  get_frame_saved_regs (fi, &fsr);
  if (fsr.regs[G14_REGNUM])
    ap = read_memory_integer (fsr.regs[G14_REGNUM],4);
  else {
    if (must_be_correct)
      return 0;			/* Don't cache this result */
    if (get_next_frame (frame))
      ap = 0;
    else
      ap = read_register (G14_REGNUM);
    if (ap == 0)
      ap = fi->frame;
  }
  fi->arg_pointer = ap;		/* Cache it for next time */
  return ap;
}

/* Return the address of the return struct for the frame
   described by FI.  Returns 0 if the address is unknown.  */

CORE_ADDR
frame_struct_result_address (fi)
     struct frame_info *fi;
{
  register FRAME frame;
  struct frame_saved_regs fsr;
  CORE_ADDR ap;

  /* If the frame is non-current, check to see if g14 was saved in the
     frame by the function prologue code; return the saved value if so,
     zero otherwise.  If the frame is current, return the value of g14.

     FIXME, shouldn't this use the saved value as long as we are past
     the function prologue, and only use the current value if we have
     no saved value and are at TOS?   -- gnu@cygnus.com */

  frame = FRAME_INFO_ID (fi);
  if (get_next_frame (frame)) {
    get_frame_saved_regs (fi, &fsr);
    if (fsr.regs[G13_REGNUM])
      ap = read_memory_integer (fsr.regs[G13_REGNUM],4);
    else
      ap = 0;
  } else {
    ap = read_register (G13_REGNUM);
  }
  return ap;
}

/* Return address to which the currently executing leafproc will return,
   or 0 if ip is not in a leafproc (or if we can't tell if it is).
  
   Do this by finding the starting address of the routine in which ip lies.
   If the instruction there is "mov g14, gx" (where x is in [0,7]), this
   is a leafproc and the return address is in register gx.  Well, this is
   true unless the return address points at a RET instruction in the current
   procedure, which indicates that we have a 'dual entry' routine that
   has been entered through the CALL entry point.  */

CORE_ADDR
leafproc_return (ip)
     CORE_ADDR ip;	/* ip from currently executing function	*/
{
  register struct minimal_symbol *msymbol;
  char *p;
  int dst;
  unsigned int insn1, insn2;
  CORE_ADDR return_addr;
  char *index ();

  if ((msymbol = lookup_minimal_symbol_by_pc (ip)) != NULL)
    {
      if ((p = index (msymbol -> name, '.')) && !strcmp (p, ".lf"))
	{
	  if (next_insn (msymbol -> address, &insn1, &insn2)
	      && (insn1 & 0xff87ffff) == 0x5c80161e       /* mov g14, gx */
	      && (dst = REG_SRCDST (insn1)) <= G0_REGNUM + 7)
	    {
	      /* Get the return address.  If the "mov g14, gx" 
		 instruction hasn't been executed yet, read
		 the return address from g14; otherwise, read it
		 from the register into which g14 was moved.  */

	      return_addr = read_register ((ip == msymbol->address)
				           ? G14_REGNUM : dst);

	      /* We know we are in a leaf procedure, but we don't know
		 whether the caller actually did a "bal" to the ".lf"
		 entry point, or a normal "call" to the non-leaf entry
		 point one instruction before.  In the latter case, the
		 return address will be the address of a "ret"
		 instruction within the procedure itself.  We test for
		 this below.  */

	      if (!next_insn (return_addr, &insn1, &insn2)
		  || (insn1 & 0xff000000) != 0xa000000   /* ret */
	          || lookup_minimal_symbol_by_pc (return_addr) != msymbol)
		return (return_addr);
	    }
	}
    }
  
  return (0);
}

/* Immediately after a function call, return the saved pc.
   Can't go through the frames for this because on some machines
   the new frame is not set up until the new function executes
   some instructions. 
   On the i960, the frame *is* set up immediately after the call,
   unless the function is a leaf procedure.  */

CORE_ADDR
saved_pc_after_call (frame)
     FRAME frame;
{
  CORE_ADDR saved_pc;
  CORE_ADDR get_frame_pc ();

  saved_pc = leafproc_return (get_frame_pc (frame));
  if (!saved_pc)
    saved_pc = FRAME_SAVED_PC (frame);

  return (saved_pc);
}

/* Discard from the stack the innermost frame,
   restoring all saved registers.  */

pop_frame ()
{
  register struct frame_info *current_fi, *prev_fi;
  register int i;
  CORE_ADDR save_addr;
  CORE_ADDR leaf_return_addr;
  struct frame_saved_regs fsr;
  char local_regs_buf[16 * 4];

  current_fi = get_frame_info (get_current_frame ());

  /* First, undo what the hardware does when we return.
     If this is a non-leaf procedure, restore local registers from
     the save area in the calling frame.  Otherwise, load the return
     address obtained from leafproc_return () into the rip.  */

  leaf_return_addr = leafproc_return (current_fi->pc);
  if (!leaf_return_addr)
    {
      /* Non-leaf procedure.  Restore local registers, incl IP.  */
      prev_fi = get_frame_info (get_prev_frame (FRAME_INFO_ID (current_fi)));
      read_memory (prev_fi->frame, local_regs_buf, sizeof (local_regs_buf));
      write_register_bytes (REGISTER_BYTE (R0_REGNUM), local_regs_buf, 
		            sizeof (local_regs_buf));

      /* Restore frame pointer.  */
      write_register (FP_REGNUM, prev_fi->frame);
    }
  else
    {
      /* Leaf procedure.  Just restore the return address into the IP.  */
      write_register (RIP_REGNUM, leaf_return_addr);
    }

  /* Now restore any global regs that the current function had saved. */
  get_frame_saved_regs (current_fi, &fsr);
  for (i = G0_REGNUM; i < G14_REGNUM; i++)
    {
      if (save_addr = fsr.regs[i])
	write_register (i, read_memory_integer (save_addr, 4));
    }

  /* Flush the frame cache, create a frame for the new innermost frame,
     and make it the current frame.  */

  flush_cached_frames ();
  set_current_frame (create_new_frame (read_register (FP_REGNUM), read_pc ()));
}

/* Print out text describing a "signal number" with which the i80960 halted.
  
   See the file "fault.c" in the nindy monitor source code for a list
   of stop codes.  */

void
print_fault( siggnal )
    int siggnal;	/* Signal number, as returned by target_wait() */
{
	static char unknown[] = "Unknown fault or trace";
	static char *sigmsgs[] = {
		/* FAULTS */
		"parallel fault",	/* 0x00 */
		unknown,		/* 0x01 */
		"operation fault",	/* 0x02 */
		"arithmetic fault",	/* 0x03 */
		"floating point fault",	/* 0x04 */
		"constraint fault",	/* 0x05 */
		"virtual memory fault",	/* 0x06 */
		"protection fault",	/* 0x07 */
		"machine fault",	/* 0x08 */
		"structural fault",	/* 0x09 */
		"type fault",		/* 0x0a */
		"reserved (0xb) fault",	/* 0x0b */
		"process fault",	/* 0x0c */
		"descriptor fault",	/* 0x0d */
		"event fault",		/* 0x0e */
		"reserved (0xf) fault",	/* 0x0f */

		/* TRACES */
		"single-step trace",	/* 0x10 */
		"branch trace",		/* 0x11 */
		"call trace",		/* 0x12 */
		"return trace",		/* 0x13 */
		"pre-return trace",	/* 0x14 */
		"supervisor call trace",/* 0x15 */
		"breakpoint trace",	/* 0x16 */
	};
#	define NUMMSGS ((int)( sizeof(sigmsgs) / sizeof(sigmsgs[0]) ))

	if (siggnal < NSIG) {
	      printf ("\nProgram received signal %d, %s\n",
		      siggnal, safe_strsignal (siggnal));
	} else {
		/* The various target_wait()s bias the 80960 "signal number"
		   by adding NSIG to it, so it won't get confused with any
		   of the Unix signals elsewhere in GDB.  We need to
		   "unbias" it before using it.  */
		siggnal -= NSIG;

		printf("Program stopped for reason #%d: %s.\n", siggnal,
				(siggnal < NUMMSGS && siggnal >= 0)?
				sigmsgs[siggnal] : unknown );
	}
}

/* Initialization stub */

_initialize_i960_tdep ()
{
  check_host ();
}
