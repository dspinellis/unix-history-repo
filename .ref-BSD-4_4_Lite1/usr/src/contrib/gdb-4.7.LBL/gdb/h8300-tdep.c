/* Target-machine dependent code for Hitachi H8/300, for GDB.
   Copyright (C) 1988, 1990, 1991 Free Software Foundation, Inc.

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

/* 
 Contributed by Steve Chamberlain
                sac@cygnus.com 
 */

#include "defs.h"
#include "frame.h"
#include "obstack.h"
#include "symtab.h"
#define UNSIGNED_SHORT(X) ((X) & 0xffff)

/* an easy to debug H8 stack frame looks like:
0x6df2	push	r2
0x6df3	push	r3
0x6df6	push	r6
0x	mov.w	r7,r6
	subs	stuff,sp  mov.w #x,r5
	                  subs  r5,sp

 */

#define IS_PUSH(x) ((x & 0xff00)==0x6d00)
#define IS_MOVE_FP(x) (x == 0x0d76)
#define IS_MOV_SP_FP(x) (x == 0x0d76)
#define IS_SUB2_SP(x) (x==0x1b87)
#define IS_MOVK_R5(x) (x==0x7905)
CORE_ADDR examine_prologue();

void   frame_find_saved_regs ();
CORE_ADDR h8300_skip_prologue(start_pc)
CORE_ADDR start_pc;

{

  /* Skip past all push insns */
  short int w;
  
  w = read_memory_short(start_pc);
  while (IS_PUSH(w)) 
  {
    start_pc+=2;  
    w = read_memory_short(start_pc);
  }

  /* Skip past a move to FP */
  if (IS_MOVE_FP(w)) {
      start_pc +=2 ;
      w = read_memory_short(start_pc);
    }

  return start_pc;  
  
}


int
print_insn(memaddr, stream)
CORE_ADDR memaddr;
FILE *stream;
{
  /* Nothing is bigger than 8 bytes */
  char   data[8];
  read_memory (memaddr, data, sizeof(data));
  return print_insn_h8300(memaddr,  data, stream);
}
     
     
/* Given a GDB frame, determine the address of the calling function's frame.
   This will be used to create a new GDB frame struct, and then
   INIT_EXTRA_FRAME_INFO and INIT_FRAME_PC will be called for the new frame.

   For us, the frame address is its stack pointer value, so we look up
   the function prologue to determine the caller's sp value, and return it.  */

FRAME_ADDR
FRAME_CHAIN (thisframe)
     FRAME thisframe;
{

  frame_find_saved_regs (thisframe, (struct frame_saved_regs *) 0);
    return thisframe->fsr->regs[SP_REGNUM];
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

      /* This will fill in fields in *fi as well as in cache_fsr.  */
      examine_prologue (ip, limit, fi->frame, cache_fsr, fi);
    }

  if (fsr)
    *fsr = *fi->fsr;
}
     

/* Fetch the instruction at ADDR, returning 0 if ADDR is beyond LIM or
   is not the address of a valid instruction, the address of the next
   instruction beyond ADDR otherwise.  *PWORD1 receives the first word
   of the instruction.*/


CORE_ADDR
NEXT_PROLOGUE_INSN(addr, lim, pword1)
CORE_ADDR addr;
CORE_ADDR lim;
short *pword1;
{
  if (addr < lim+8)   
  {
    read_memory (addr, pword1, sizeof(*pword1));
    SWAP_TARGET_AND_HOST (pword1, sizeof (short));
    return addr + 2;
  }

  return 0;

}

/* Examine the prologue of a function.  `ip' points to the first instruction.
   `limit' is the limit of the prologue (e.g. the addr of the first 
   linenumber, or perhaps the program counter if we're stepping through).
   `frame_sp' is the stack pointer value in use in this frame.  
   `fsr' is a pointer to a frame_saved_regs structure into which we put
   info about the registers saved by this frame.  
   `fi' is a struct frame_info pointer; we fill in various fields in it
   to reflect the offsets of the arg pointer and the locals pointer.  */

/* We will find two sorts of prologue, framefull and non framefull:
   
   push   r2
   push   r3
   push   fp
   mov    sp,fp
   stack_ad
 
   and
   push   x
   push   y
   stack_ad

*/

static CORE_ADDR
examine_prologue (ip, limit, after_prolog_fp, fsr, fi)
     register CORE_ADDR ip;
     register CORE_ADDR limit;
     FRAME_ADDR after_prolog_fp;
     struct frame_saved_regs *fsr;
     struct frame_info *fi;
{
  register CORE_ADDR next_ip;
  int r;
  int i;
  int have_fp = 0;
  
  register int src;
  register struct pic_prologue_code *pcode;
  INSN_WORD insn_word;
  int size, offset;
  unsigned int reg_save_depth = 2; /* Number of things pushed onto
				      stack, starts at 2, 'cause the
				      PC is already there */

  unsigned int auto_depth = 0;	/* Number of bytes of autos */
  
  char in_frame[NUM_REGS];      /* One for each reg */
  
  memset(in_frame, 1, NUM_REGS);
  
  if (after_prolog_fp == 0) {
      after_prolog_fp = read_register(SP_REGNUM);
    }  
  if (ip == 0 || ip & ~0xffff) return 0;

  next_ip = NEXT_PROLOGUE_INSN (ip, limit, &insn_word);

  /* Skip over any push instructions, and remember where they were saved */


  while (next_ip && IS_PUSH(insn_word))
  {
    ip = next_ip;
    in_frame[insn_word & 0x7] = reg_save_depth;
    next_ip = NEXT_PROLOGUE_INSN(ip, limit, &insn_word);
    reg_save_depth +=2;

  }
  

  /* Is this a move into the fp */
  if (next_ip && IS_MOV_SP_FP(insn_word)) 
  {
    ip = next_ip;
    next_ip = NEXT_PROLOGUE_INSN(ip, limit, &insn_word);
    have_fp = 1;
    
  }
  

  /* Skip over any stack adjustment, happens either with a number of
     sub#2,sp or a mov #x,r5 sub r5,sp */

  
  if (next_ip && IS_SUB2_SP(insn_word))
  {
    while (next_ip && IS_SUB2_SP(insn_word)) 
    {
      auto_depth +=2 ;
      ip = next_ip;
      next_ip = NEXT_PROLOGUE_INSN(ip, limit, &insn_word);
    }
  }
  else  
  {
    if (next_ip && IS_MOVK_R5(insn_word)) 
    {
      ip = next_ip;
      next_ip = NEXT_PROLOGUE_INSN(ip, limit, &insn_word);
      auto_depth += insn_word;
      ip +=4;
      
    }
  }


  
  /* The args are always reffed based from the stack pointer */
  fi->args_pointer =  after_prolog_fp - auto_depth;
  /* Locals are always reffed based from the fp */
  fi->locals_pointer = after_prolog_fp ;
  /* The PC is at a known place */
  fi->from_pc = read_memory_short(after_prolog_fp + reg_save_depth-2 );
  

  /* Rememeber any others too */

  in_frame[PC_REGNUM] = 0;
  
  for (r = 0; r < NUM_REGS; r++) 
  {
    if (in_frame[r] != 1) 
    {
      fsr->regs[r] = after_prolog_fp + reg_save_depth - in_frame[r] -2;
    }
    else
    {
      fsr->regs[r] = 0;
    }
  }	
  if (have_fp) 
   /* We keep the old FP in the SP spot */
   fsr->regs[SP_REGNUM] = (read_memory_short(fsr->regs[6])) ;
  else 
   fsr->regs[SP_REGNUM] = after_prolog_fp + reg_save_depth;
  
  return (ip);
}

void
init_extra_frame_info (fromleaf, fi)
     int fromleaf;
     struct frame_info *fi;
{
  fi->fsr = 0;			/* Not yet allocated */
  fi->args_pointer = 0;		/* Unknown */
  fi->locals_pointer = 0;	/* Unknown */
  fi->from_pc = 0;
  
}
/* Return the saved PC from this frame.

   If the frame has a memory copy of SRP_REGNUM, use that.  If not,
   just use the register SRP_REGNUM itself.  */

CORE_ADDR
frame_saved_pc (frame)
FRAME frame;

{
  return frame->from_pc;
}


CORE_ADDR
frame_locals_address (fi)
     struct frame_info *fi;
{
  if (!fi->locals_pointer) 
  {
    struct frame_saved_regs ignore;
    get_frame_saved_regs(fi, &ignore);  

  }
  return fi->locals_pointer;
}

/* Return the address of the argument block for the frame
   described by FI.  Returns 0 if the address is unknown.  */

CORE_ADDR
frame_args_address (fi)
     struct frame_info *fi;
{
  if (!fi->args_pointer) 
  {
    struct frame_saved_regs ignore;
  get_frame_saved_regs(fi, &ignore);  

  }
  
  return fi->args_pointer;
}


void h8300_pop_frame()
{
  unsigned regnum;
  struct frame_saved_regs fsr;
  struct frame_info *fi;

  FRAME frame = get_current_frame();  
  fi = get_frame_info(frame);  
  get_frame_saved_regs(fi, &fsr);

  for (regnum = 0; regnum < NUM_REGS; regnum ++) 
  {
    if(fsr.regs[regnum]) 
    {
      write_register(regnum, read_memory_short (fsr.regs[regnum]));
    }
  
    flush_cached_frames();
    set_current_frame(create_new_frame(read_register(FP_REGNUM),
				       read_pc()));
  
  }

}
