/* Target-machine dependent code for the AMD 29000
   Copyright 1990, 1991 Free Software Foundation, Inc.
   Contributed by Cygnus Support.  Written by Jim Kingdon.

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
#include "gdbcore.h"
#include "frame.h"
#include "value.h"
/*#include <sys/param.h> */
#include "symtab.h"
#include "inferior.h"
#include "gdbcmd.h"

extern CORE_ADDR text_start;	/* FIXME, kludge... */

/* The user-settable top of the register stack in virtual memory.  We
   won't attempt to access any stored registers above this address, if set
   nonzero.  */

static CORE_ADDR rstack_high_address = UINT_MAX;

/* Structure to hold cached info about function prologues.  */
struct prologue_info
{
  CORE_ADDR pc;			/* First addr after fn prologue */
  unsigned rsize, msize;	/* register stack frame size, mem stack ditto */
  unsigned mfp_used : 1;	/* memory frame pointer used */
  unsigned rsize_valid : 1;	/* Validity bits for the above */
  unsigned msize_valid : 1;
  unsigned mfp_valid : 1;
};

/* Examine the prologue of a function which starts at PC.  Return
   the first addess past the prologue.  If MSIZE is non-NULL, then
   set *MSIZE to the memory stack frame size.  If RSIZE is non-NULL,
   then set *RSIZE to the register stack frame size (not including
   incoming arguments and the return address & frame pointer stored
   with them).  If no prologue is found, *RSIZE is set to zero.
   If no prologue is found, or a prologue which doesn't involve
   allocating a memory stack frame, then set *MSIZE to zero.

   Note that both msize and rsize are in bytes.  This is not consistent
   with the _User's Manual_ with respect to rsize, but it is much more
   convenient.

   If MFP_USED is non-NULL, *MFP_USED is set to nonzero if a memory
   frame pointer is being used.  */
CORE_ADDR
examine_prologue (pc, rsize, msize, mfp_used)
     CORE_ADDR pc;
     unsigned *msize;
     unsigned *rsize;
     int *mfp_used;
{
  long insn;
  CORE_ADDR p = pc;
  struct minimal_symbol *msymbol = lookup_minimal_symbol_by_pc (pc);
  struct prologue_info *mi = 0;

  if (msymbol != NULL)
    mi = (struct prologue_info *) msymbol -> info;

  if (mi != 0)
    {
      int valid = 1;
      if (rsize != NULL)
	{
	  *rsize = mi->rsize;
	  valid &= mi->rsize_valid;
	}
      if (msize != NULL)
	{
	  *msize = mi->msize;
	  valid &= mi->msize_valid;
	}
      if (mfp_used != NULL)
	{
	  *mfp_used = mi->mfp_used;
	  valid &= mi->mfp_valid;
	}
      if (valid)
	return mi->pc;
    }

  if (rsize != NULL)
    *rsize = 0;
  if (msize != NULL)
    *msize = 0;
  if (mfp_used != NULL)
    *mfp_used = 0;
  
  /* Prologue must start with subtracting a constant from gr1.
     Normally this is sub gr1,gr1,<rsize * 4>.  */
  insn = read_memory_integer (p, 4);
  if ((insn & 0xffffff00) != 0x25010100)
    {
      /* If the frame is large, instead of a single instruction it
	 might be a pair of instructions:
	 const <reg>, <rsize * 4>
	 sub gr1,gr1,<reg>
	 */
      int reg;
      /* Possible value for rsize.  */
      unsigned int rsize0;
      
      if ((insn & 0xff000000) != 0x03000000)
	{
	  p = pc;
	  goto done;
	}
      reg = (insn >> 8) & 0xff;
      rsize0 = (((insn >> 8) & 0xff00) | (insn & 0xff));
      p += 4;
      insn = read_memory_integer (p, 4);
      if ((insn & 0xffffff00) != 0x24010100
	  || (insn & 0xff) != reg)
	{
	  p = pc;
	  goto done;
	}
      if (rsize != NULL)
	*rsize = rsize0;
    }
  else
    {
      if (rsize != NULL)
	*rsize = (insn & 0xff);
    }
  p += 4;

  /* Next instruction must be asgeu V_SPILL,gr1,rab.  
   * We don't check the vector number to allow for kernel debugging.  The 
   * kernel will use a different trap number. 
   */
  insn = read_memory_integer (p, 4);
  if ((insn & 0xff00ffff) != (0x5e000100|RAB_HW_REGNUM))
    {
      p = pc;
      goto done;
    }
  p += 4;

  /* Next instruction usually sets the frame pointer (lr1) by adding
     <size * 4> from gr1.  However, this can (and high C does) be
     deferred until anytime before the first function call.  So it is
     OK if we don't see anything which sets lr1.  
     To allow for alternate register sets (gcc -mkernel-registers)  the msp
     register number is a compile time constant. */

  /* Normally this is just add lr1,gr1,<size * 4>.  */
  insn = read_memory_integer (p, 4);
  if ((insn & 0xffffff00) == 0x15810100)
    p += 4;
  else
    {
      /* However, for large frames it can be
	 const <reg>, <size *4>
	 add lr1,gr1,<reg>
	 */
      int reg;
      CORE_ADDR q;

      if ((insn & 0xff000000) == 0x03000000)
	{
	  reg = (insn >> 8) & 0xff;
	  q = p + 4;
	  insn = read_memory_integer (q, 4);
	  if ((insn & 0xffffff00) == 0x14810100
	      && (insn & 0xff) == reg)
	    p = q;
	}
    }

  /* Next comes "add lr{<rsize-1>},msp,0", but only if a memory
     frame pointer is in use.  We just check for add lr<anything>,msp,0;
     we don't check this rsize against the first instruction, and
     we don't check that the trace-back tag indicates a memory frame pointer
     is in use.  
     To allow for alternate register sets (gcc -mkernel-registers)  the msp
     register number is a compile time constant.

     The recommended instruction is actually "sll lr<whatever>,msp,0". 
     We check for that, too.  Originally Jim Kingdon's code seemed
     to be looking for a "sub" instruction here, but the mask was set
     up to lose all the time. */
  insn = read_memory_integer (p, 4);
  if (((insn & 0xff80ffff) == (0x15800000|(MSP_HW_REGNUM<<8)))     /* add */
   || ((insn & 0xff80ffff) == (0x81800000|(MSP_HW_REGNUM<<8))))    /* sll */
    {
      p += 4;
      if (mfp_used != NULL)
	*mfp_used = 1;
    }

  /* Next comes a subtraction from msp to allocate a memory frame,
     but only if a memory frame is
     being used.  We don't check msize against the trace-back tag.

     To allow for alternate register sets (gcc -mkernel-registers) the msp
     register number is a compile time constant.

     Normally this is just
     sub msp,msp,<msize>
     */
  insn = read_memory_integer (p, 4);
  if ((insn & 0xffffff00) == 
		(0x25000000|(MSP_HW_REGNUM<<16)|(MSP_HW_REGNUM<<8)))
    {
      p += 4;
      if (msize != NULL) 
	*msize = insn & 0xff;
    }
  else
    {
      /* For large frames, instead of a single instruction it might
	 be

	 const <reg>, <msize>
	 consth <reg>, <msize>     ; optional
	 sub msp,msp,<reg>
	 */
      int reg;
      unsigned msize0;
      CORE_ADDR q = p;

      if ((insn & 0xff000000) == 0x03000000)
	{
	  reg = (insn >> 8) & 0xff;
	  msize0 = ((insn >> 8) & 0xff00) | (insn & 0xff);
	  q += 4;
	  insn = read_memory_integer (q, 4);
	  /* Check for consth.  */
	  if ((insn & 0xff000000) == 0x02000000
	      && (insn & 0x0000ff00) == reg)
	    {
	      msize0 |= (insn << 8) & 0xff000000;
	      msize0 |= (insn << 16) & 0x00ff0000;
	      q += 4;
	      insn = read_memory_integer (q, 4);
	    }
	  /* Check for sub msp,msp,<reg>.  */
          if ((insn & 0xffffff00) == 
		(0x24000000|(MSP_HW_REGNUM<<16)|(MSP_HW_REGNUM<<8))
	      && (insn & 0xff) == reg)
	    {
	      p = q + 4;
	      if (msize != NULL)
		*msize = msize0;
	    }
	}
    }

 done:
  if (msymbol != NULL)
    {
      if (mi == 0)
	{
	  /* Add a new cache entry.  */
	  mi = (struct prologue_info *)xmalloc (sizeof (struct prologue_info));
	  msymbol -> info = (char *)mi;
	  mi->rsize_valid = 0;
	  mi->msize_valid = 0;
	  mi->mfp_valid = 0;
	}
      /* else, cache entry exists, but info is incomplete.  */
      mi->pc = p;
      if (rsize != NULL)
	{
	  mi->rsize = *rsize;
	  mi->rsize_valid = 1;
	}
      if (msize != NULL)
	{
	  mi->msize = *msize;
	  mi->msize_valid = 1;
	}
      if (mfp_used != NULL)
	{
	  mi->mfp_used = *mfp_used;
	  mi->mfp_valid = 1;
	}
    }
  return p;
}

/* Advance PC across any function entry prologue instructions
   to reach some "real" code.  */

CORE_ADDR
skip_prologue (pc)
     CORE_ADDR pc;
{
  return examine_prologue (pc, (unsigned *)NULL, (unsigned *)NULL,
			   (int *)NULL);
}
/*
 * Examine the one or two word tag at the beginning of a function.
 * The tag word is expect to be at 'p', if it is not there, we fail
 * by returning 0.  The documentation for the tag word was taken from
 * page 7-15 of the 29050 User's Manual.  We are assuming that the
 * m bit is in bit 22 of the tag word, which seems to be the agreed upon
 * convention today (1/15/92).
 * msize is return in bytes.
 */
static int	/* 0/1 - failure/success of finding the tag word  */
examine_tag(p, is_trans, argcount, msize, mfp_used)
     CORE_ADDR p;
     int *is_trans;
     int   *argcount;
     unsigned *msize;
     int *mfp_used;
{
  unsigned int tag1, tag2;

  tag1 = read_memory_integer (p, 4);
  if ((tag1 & 0xff000000) != 0)		/* Not a tag word */
    return 0;
  if (tag1 & (1<<23)) 			/* A two word tag */
    {
       tag2 = read_memory_integer (p+4, 4);
       if (msize)
	 *msize = tag2;
    }
  else					/* A one word tag */
    {
       if (msize)
	 *msize = tag1 & 0x7ff;
    }
  if (is_trans)
    *is_trans = ((tag1 & (1<<21)) ? 1 : 0); 
  if (argcount)
    *argcount = (tag1 >> 16) & 0x1f;
  if (mfp_used)
    *mfp_used = ((tag1 & (1<<22)) ? 1 : 0); 
  return(1);
}

/* Initialize the frame.  In addition to setting "extra" frame info,
   we also set ->frame because we use it in a nonstandard way, and ->pc
   because we need to know it to get the other stuff.  See the diagram
   of stacks and the frame cache in tm-29k.h for more detail.  */
static void
init_frame_info (innermost_frame, fci)
     int innermost_frame;
     struct frame_info *fci;
{
  CORE_ADDR p;
  long insn;
  unsigned rsize;
  unsigned msize;
  int mfp_used, trans;
  struct symbol *func;

  p = fci->pc;

  if (innermost_frame)
    fci->frame = read_register (GR1_REGNUM);
  else
    fci->frame = fci->next_frame + fci->next->rsize;
  
#if CALL_DUMMY_LOCATION == ON_STACK
  This wont work;
#else
  if (PC_IN_CALL_DUMMY (p, 0, 0))
#endif
    {
      fci->rsize = DUMMY_FRAME_RSIZE;
      /* This doesn't matter since we never try to get locals or args
	 from a dummy frame.  */
      fci->msize = 0;
      /* Dummy frames always use a memory frame pointer.  */
      fci->saved_msp = 
	read_register_stack_integer (fci->frame + DUMMY_FRAME_RSIZE - 4, 4);
      fci->flags |= (TRANSPARENT|MFP_USED);
      return;
    }
    
  func = find_pc_function (p);
  if (func != NULL)
    p = BLOCK_START (SYMBOL_BLOCK_VALUE (func));
  else
    {
      /* Search backward to find the trace-back tag.  However,
	 do not trace back beyond the start of the text segment
	 (just as a sanity check to avoid going into never-never land).  */
      while (p >= text_start
	     && ((insn = read_memory_integer (p, 4)) & 0xff000000) != 0)
	p -= 4;
      
      if (p < text_start)
	{
	  /* Couldn't find the trace-back tag.
	     Something strange is going on.  */
	  fci->saved_msp = 0;
	  fci->rsize = 0;
	  fci->msize = 0;
	  fci->flags = TRANSPARENT;
	  return;
	}
      else
	/* Advance to the first word of the function, i.e. the word
	   after the trace-back tag.  */
	p += 4;
    }
  /* We've found the start of the function.  
   * Try looking for a tag word that indicates whether there is a
   * memory frame pointer and what the memory stack allocation is.
   * If one doesn't exist, try using a more exhaustive search of
   * the prologue.  For now we don't care about the argcount or
   * whether or not the routine is transparent.
   */
  if (examine_tag(p-4,&trans,NULL,&msize,&mfp_used)) /* Found a good tag */
      examine_prologue (p, &rsize, 0, 0);
  else 						/* No tag try prologue */
      examine_prologue (p, &rsize, &msize, &mfp_used);

  fci->rsize = rsize;
  fci->msize = msize;
  fci->flags = 0;
  if (mfp_used)
  	fci->flags |= MFP_USED;
  if (trans)
  	fci->flags |= TRANSPARENT;
  if (innermost_frame)
    {
      fci->saved_msp = read_register (MSP_REGNUM) + msize;
    }
  else
    {
      if (mfp_used)
  	 fci->saved_msp =
	      read_register_stack_integer (fci->frame + rsize - 4, 4);
      else
  	    fci->saved_msp = fci->next->saved_msp + msize;
    }
}

void
init_extra_frame_info (fci)
     struct frame_info *fci;
{
  if (fci->next == 0)
    /* Assume innermost frame.  May produce strange results for "info frame"
       but there isn't any way to tell the difference.  */
    init_frame_info (1, fci);
  else {
      /* We're in get_prev_frame_info.
         Take care of everything in init_frame_pc.  */
      ;
    }
}

void
init_frame_pc (fromleaf, fci)
     int fromleaf;
     struct frame_info *fci;
{
  fci->pc = (fromleaf ? SAVED_PC_AFTER_CALL (fci->next) :
	     fci->next ? FRAME_SAVED_PC (fci->next) : read_pc ());
  init_frame_info (fromleaf, fci);
}

/* Local variables (i.e. LOC_LOCAL) are on the memory stack, with their
   offsets being relative to the memory stack pointer (high C) or
   saved_msp (gcc).  */

CORE_ADDR
frame_locals_address (fi)
     struct frame_info *fi;
{
  if (fi->flags & MFP_USED) 
    return fi->saved_msp;
  else
    return fi->saved_msp - fi->msize;
}

/* Routines for reading the register stack.  The caller gets to treat
   the register stack as a uniform stack in memory, from address $gr1
   straight through $rfb and beyond.  */

/* Analogous to read_memory except the length is understood to be 4.
   Also, myaddr can be NULL (meaning don't bother to read), and
   if actual_mem_addr is non-NULL, store there the address that it
   was fetched from (or if from a register the offset within
   registers).  Set *LVAL to lval_memory or lval_register, depending
   on where it came from.  */
void
read_register_stack (memaddr, myaddr, actual_mem_addr, lval)
     CORE_ADDR memaddr;
     char *myaddr;
     CORE_ADDR *actual_mem_addr;
     enum lval_type *lval;
{
  long rfb = read_register (RFB_REGNUM);
  long rsp = read_register (RSP_REGNUM);

  /* If we don't do this 'info register' stops in the middle. */
  if (memaddr >= rstack_high_address) 
    {
      int val = -1;			/* a bogus value */
      /* It's in a local register, but off the end of the stack.  */
      int regnum = (memaddr - rsp) / 4 + LR0_REGNUM;
      if (myaddr != NULL)
	*(int*)myaddr = val;		/* Provide bogusness */
      supply_register(regnum,&val);	/* More bogusness */
      if (lval != NULL)
	*lval = lval_register;
      if (actual_mem_addr != NULL)
	*actual_mem_addr = REGISTER_BYTE (regnum);
    }
  else if (memaddr < rfb)
    {
      /* It's in a register.  */
      int regnum = (memaddr - rsp) / 4 + LR0_REGNUM;
      if (regnum < LR0_REGNUM || regnum > LR0_REGNUM + 127)
	error ("Attempt to read register stack out of range.");
      if (myaddr != NULL)
	read_register_gen (regnum, myaddr);
      if (lval != NULL)
	*lval = lval_register;
      if (actual_mem_addr != NULL)
	*actual_mem_addr = REGISTER_BYTE (regnum);
    }
  else
    {
      /* It's in the memory portion of the register stack.  */
      if (myaddr != NULL) 
	   read_memory (memaddr, myaddr, 4);
      if (lval != NULL)
	*lval = lval_memory;
      if (actual_mem_addr != NULL)
	*actual_mem_addr = memaddr;
    }
}

/* Analogous to read_memory_integer
   except the length is understood to be 4.  */
long
read_register_stack_integer (memaddr, len)
     CORE_ADDR memaddr;
     int len;
{
  long buf;
  read_register_stack (memaddr, &buf, NULL, NULL);
  SWAP_TARGET_AND_HOST (&buf, 4);
  return buf;
}

/* Copy 4 bytes from GDB memory at MYADDR into inferior memory
   at MEMADDR and put the actual address written into in
   *ACTUAL_MEM_ADDR.  */
static void
write_register_stack (memaddr, myaddr, actual_mem_addr)
     CORE_ADDR memaddr;
     char *myaddr;
     CORE_ADDR *actual_mem_addr;
{
  long rfb = read_register (RFB_REGNUM);
  long rsp = read_register (RSP_REGNUM);
  /* If we don't do this 'info register' stops in the middle. */
  if (memaddr >= rstack_high_address) 
    {
      /* It's in a register, but off the end of the stack.  */
      if (actual_mem_addr != NULL)
	*actual_mem_addr = NULL; 
    }
  else if (memaddr < rfb)
    {
      /* It's in a register.  */
      int regnum = (memaddr - rsp) / 4 + LR0_REGNUM;
      if (regnum < LR0_REGNUM || regnum > LR0_REGNUM + 127)
	error ("Attempt to read register stack out of range.");
      if (myaddr != NULL)
	write_register (regnum, *(long *)myaddr);
      if (actual_mem_addr != NULL)
	*actual_mem_addr = NULL;
    }
  else
    {
      /* It's in the memory portion of the register stack.  */
      if (myaddr != NULL)
	write_memory (memaddr, myaddr, 4);
      if (actual_mem_addr != NULL)
	*actual_mem_addr = memaddr;
    }
}

/* Find register number REGNUM relative to FRAME and put its
   (raw) contents in *RAW_BUFFER.  Set *OPTIMIZED if the variable
   was optimized out (and thus can't be fetched).  If the variable
   was fetched from memory, set *ADDRP to where it was fetched from,
   otherwise it was fetched from a register.

   The argument RAW_BUFFER must point to aligned memory.  */
void
get_saved_register (raw_buffer, optimized, addrp, frame, regnum, lvalp)
     char *raw_buffer;
     int *optimized;
     CORE_ADDR *addrp;
     FRAME frame;
     int regnum;
     enum lval_type *lvalp;
{
  struct frame_info *fi;
  CORE_ADDR addr;
  enum lval_type lval;

  if (frame == 0)
    return;

  fi = get_frame_info (frame);

  /* Once something has a register number, it doesn't get optimized out.  */
  if (optimized != NULL)
    *optimized = 0;
  if (regnum == RSP_REGNUM)
    {
      if (raw_buffer != NULL)
	*(CORE_ADDR *)raw_buffer = fi->frame;
      if (lvalp != NULL)
	*lvalp = not_lval;
      return;
    }
  else if (regnum == PC_REGNUM)
    {
      if (raw_buffer != NULL)
	*(CORE_ADDR *)raw_buffer = fi->pc;

      /* Not sure we have to do this.  */
      if (lvalp != NULL)
	*lvalp = not_lval;

      return;
    }
  else if (regnum == MSP_REGNUM)
    {
      if (raw_buffer != NULL)
	{
	  if (fi->next != NULL)
	    *(CORE_ADDR *)raw_buffer = fi->next->saved_msp;
	  else
	    *(CORE_ADDR *)raw_buffer = read_register (MSP_REGNUM);
	}
      /* The value may have been computed, not fetched.  */
      if (lvalp != NULL)
	*lvalp = not_lval;
      return;
    }
  else if (regnum < LR0_REGNUM || regnum >= LR0_REGNUM + 128)
    {
      /* These registers are not saved over procedure calls,
	 so just print out the current values.  */
      if (raw_buffer != NULL)
	*(CORE_ADDR *)raw_buffer = read_register (regnum);
      if (lvalp != NULL)
	*lvalp = lval_register;
      if (addrp != NULL)
	*addrp = REGISTER_BYTE (regnum);
      return;
    }
      
  addr = fi->frame + (regnum - LR0_REGNUM) * 4;
  if (raw_buffer != NULL)
    read_register_stack (addr, raw_buffer, &addr, &lval);
  if (lvalp != NULL)
    *lvalp = lval;
  if (addrp != NULL)
    *addrp = addr;
}


/* Discard from the stack the innermost frame,
   restoring all saved registers.  */

void
pop_frame ()
{
  FRAME frame = get_current_frame ();					      
  struct frame_info *fi = get_frame_info (frame);			      
  CORE_ADDR rfb = read_register (RFB_REGNUM);				      
  CORE_ADDR gr1 = fi->frame + fi->rsize;
  CORE_ADDR lr1;							      
  int i;

  /* If popping a dummy frame, need to restore registers.  */
  if (PC_IN_CALL_DUMMY (read_register (PC_REGNUM),
			read_register (SP_REGNUM),
			FRAME_FP (fi)))
    {
      int lrnum = LR0_REGNUM + DUMMY_ARG/4;
      for (i = 0; i < DUMMY_SAVE_SR128; ++i)
	write_register (SR_REGNUM (i + 128),read_register (lrnum++));
      for (i = 0; i < DUMMY_SAVE_SR160; ++i)
	write_register (SR_REGNUM(i+160), read_register (lrnum++));
      for (i = 0; i < DUMMY_SAVE_GREGS; ++i)
	write_register (RETURN_REGNUM + i, read_register (lrnum++));
      /* Restore the PCs.  */
      write_register(PC_REGNUM, read_register (lrnum++));
      write_register(NPC_REGNUM, read_register (lrnum));
    }

  /* Restore the memory stack pointer.  */
  write_register (MSP_REGNUM, fi->saved_msp);				      
  /* Restore the register stack pointer.  */				      
  write_register (GR1_REGNUM, gr1);
  /* Check whether we need to fill registers.  */			      
  lr1 = read_register (LR0_REGNUM + 1);				      
  if (lr1 > rfb)							      
    {									      
      /* Fill.  */							      
      int num_bytes = lr1 - rfb;
      int i;								      
      long word;							      
      write_register (RAB_REGNUM, read_register (RAB_REGNUM) + num_bytes);  
      write_register (RFB_REGNUM, lr1);				      
      for (i = 0; i < num_bytes; i += 4)				      
        {
	  /* Note: word is in host byte order.  */
          word = read_memory_integer (rfb + i, 4);
          write_register (LR0_REGNUM + ((rfb - gr1) % 0x80) + i / 4, word);					      
        }								      
    }
  flush_cached_frames ();						      
  set_current_frame (create_new_frame (0, read_pc()));		      
}

/* Push an empty stack frame, to record the current PC, etc.  */

void 
push_dummy_frame ()
{
  long w;
  CORE_ADDR rab, gr1;
  CORE_ADDR msp = read_register (MSP_REGNUM);
  int lrnum,  i, saved_lr0;
  

  /* Allocate the new frame. */ 
  gr1 = read_register (GR1_REGNUM) - DUMMY_FRAME_RSIZE;
  write_register (GR1_REGNUM, gr1);

  rab = read_register (RAB_REGNUM);
  if (gr1 < rab)
    {
      /* We need to spill registers.  */
      int num_bytes = rab - gr1;
      CORE_ADDR rfb = read_register (RFB_REGNUM);
      int i;
      long word;

      write_register (RFB_REGNUM, rfb - num_bytes);
      write_register (RAB_REGNUM, gr1);
      for (i = 0; i < num_bytes; i += 4)
	{
	  /* Note:  word is in target byte order.  */
	  read_register_gen (LR0_REGNUM + i / 4, &word);
	  write_memory (rfb - num_bytes + i, &word, 4);
	}
    }

  /* There are no arguments in to the dummy frame, so we don't need
     more than rsize plus the return address and lr1.  */
  write_register (LR0_REGNUM + 1, gr1 + DUMMY_FRAME_RSIZE + 2 * 4);

  /* Set the memory frame pointer.  */
  write_register (LR0_REGNUM + DUMMY_FRAME_RSIZE / 4 - 1, msp);

  /* Allocate arg_slop.  */
  write_register (MSP_REGNUM, msp - 16 * 4);

  /* Save registers.  */
  lrnum = LR0_REGNUM + DUMMY_ARG/4;
  for (i = 0; i < DUMMY_SAVE_SR128; ++i)
    write_register (lrnum++, read_register (SR_REGNUM (i + 128)));
  for (i = 0; i < DUMMY_SAVE_SR160; ++i)
    write_register (lrnum++, read_register (SR_REGNUM (i + 160)));
  for (i = 0; i < DUMMY_SAVE_GREGS; ++i)
    write_register (lrnum++, read_register (RETURN_REGNUM + i));
  /* Save the PCs.  */
  write_register (lrnum++, read_register (PC_REGNUM));
  write_register (lrnum, read_register (NPC_REGNUM));
}

reginv_com (args, fromtty)
     char       *args;
     int        fromtty;
{
   registers_changed();
   if (fromtty)
	printf_filtered("Gdb's register cache invalidated.\n");
}

/* We use this mostly for debugging gdb */
void
_initialize_29k()
{
  extern CORE_ADDR text_end;

  add_com ("reginv ", class_obscure, reginv_com, 
        "Invalidate gdb's internal register cache.");

  /* FIXME, there should be a way to make a CORE_ADDR variable settable. */
  add_show_from_set
    (add_set_cmd ("rstack_high_address", class_support, var_uinteger,
		  (char *)&rstack_high_address,
		  "Set top address in memory of the register stack.\n\
Attempts to access registers saved above this address will be ignored\n\
or will produce the value -1.", &setlist),
     &showlist);

  /* FIXME, there should be a way to make a CORE_ADDR variable settable. */
  add_show_from_set
    (add_set_cmd ("call_scratch_address", class_support, var_uinteger,
		  (char *)&text_end,
"Set address in memory where small amounts of RAM can be used when\n\
making function calls into the inferior.", &setlist),
     &showlist);
}
