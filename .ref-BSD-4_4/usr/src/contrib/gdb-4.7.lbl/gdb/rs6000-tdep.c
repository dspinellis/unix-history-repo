/* Target-dependent code for GDB, the GNU debugger.
   Copyright 1986, 1987, 1989, 1991, 1992 Free Software Foundation, Inc.

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
#include "symtab.h"
#include "target.h"

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <fcntl.h>

#include <a.out.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/core.h>
#include <sys/ldr.h>


extern struct obstack frame_cache_obstack;

extern int errno;

/* Nonzero if we just simulated a single step break. */
int one_stepped;

/* Breakpoint shadows for the single step instructions will be kept here. */

static struct sstep_breaks {
	int address;
	int data;
} stepBreaks[2];

/* Static function prototypes */

static void
add_text_to_loadinfo PARAMS ((CORE_ADDR textaddr, CORE_ADDR dataaddr));

static CORE_ADDR
find_toc_address PARAMS ((CORE_ADDR pc));

static CORE_ADDR
branch_dest PARAMS ((int opcode, int instr, CORE_ADDR pc, CORE_ADDR safety));

static void
frame_get_cache_fsr PARAMS ((struct frame_info *fi,
			     struct aix_framedata *fdatap));

/*
 * Calculate the destination of a branch/jump.  Return -1 if not a branch.
 */
static CORE_ADDR
branch_dest (opcode, instr, pc, safety)
     int opcode;
     int instr;
     CORE_ADDR pc;
     CORE_ADDR safety;
{
  register long offset;
  CORE_ADDR dest;
  int immediate;
  int absolute;
  int ext_op;

  absolute = (int) ((instr >> 1) & 1);

  switch (opcode) {
     case 18	:
	immediate = ((instr & ~3) << 6) >> 6;	/* br unconditional */

     case 16	:  
	if (opcode != 18)		        /* br conditional */
	  immediate = ((instr & ~3) << 16) >> 16;
	if (absolute)
	  dest = immediate;	
	else
	  dest = pc + immediate;
	break;

      case 19	:
	ext_op = (instr>>1) & 0x3ff;

	if (ext_op == 16)			/* br conditional register */
	  dest = read_register (LR_REGNUM) & ~3;

	else if (ext_op == 528)			/* br cond to count reg */
	  dest = read_register (CTR_REGNUM) & ~3;

	else return -1; 
	break;
	
       default: return -1;
  }
  return (dest < TEXT_SEGMENT_BASE) ? safety : dest;
}



/* AIX does not support PT_STEP. Simulate it. */

void
single_step (signal)
     int signal;
{
#define	INSNLEN(OPCODE)	 4

  static char breakp[] = BREAKPOINT;
  int ii, insn, ret, loc;
  int breaks[2], opcode;

  if (!one_stepped) {
    loc = read_pc ();

    ret = read_memory (loc, &insn, sizeof (int));
    if (ret)
      printf ("Error in single_step()!!\n");

    breaks[0] = loc + INSNLEN(insn);
    opcode = insn >> 26;
    breaks[1] = branch_dest (opcode, insn, loc, breaks[0]);

    /* Don't put two breakpoints on the same address. */
    if (breaks[1] == breaks[0])
      breaks[1] = -1;

    stepBreaks[1].address = -1;

    for (ii=0; ii < 2; ++ii) {

      /* ignore invalid breakpoint. */
      if ( breaks[ii] == -1)
        continue;

      read_memory (breaks[ii], &(stepBreaks[ii].data), sizeof(int));

      ret = write_memory (breaks[ii], breakp, sizeof(int));
      stepBreaks[ii].address = breaks[ii];
    }  

    one_stepped = 1;
  } else {

    /* remove step breakpoints. */
    for (ii=0; ii < 2; ++ii)
      if (stepBreaks[ii].address != -1)
        write_memory 
           (stepBreaks[ii].address, &(stepBreaks[ii].data), sizeof(int));

    one_stepped = 0;
  }
  errno = 0;			/* FIXME, don't ignore errors! */
}


/* return pc value after skipping a function prologue. */

skip_prologue (pc)
CORE_ADDR pc;
{
  unsigned int tmp;
  unsigned int op;    /* FIXME, assumes instruction size matches host int!!! */

  if (target_read_memory (pc, (char *)&op, sizeof (op)))
    return pc;			/* Can't access it -- assume no prologue. */
  SWAP_TARGET_AND_HOST (&op, sizeof (op));

  /* Assume that subsequent fetches can fail with low probability.  */

  if (op == 0x7c0802a6) {		/* mflr r0 */
    pc += 4;
    op = read_memory_integer (pc, 4);
  }

  if ((op & 0xfc00003e) == 0x7c000026) { /* mfcr Rx */
    pc += 4;
    op = read_memory_integer (pc, 4);
  }

  if ((op & 0xfc000000) == 0x48000000) { /* bl foo, to save fprs??? */
    pc += 4;
    op = read_memory_integer (pc, 4);

    /* At this point, make sure this is not a trampoline function
       (a function that simply calls another functions, and nothing else).
       If the next is not a nop, this branch was part of the function
       prologue. */

    if (op == 0x4def7b82 ||		/* crorc 15, 15, 15 */
	op == 0x0)
      return pc - 4;			/* don't skip over this branch */
  }

  if ((op & 0xfc1f0000) == 0xbc010000) { /* stm Rx, NUM(r1) */
    pc += 4;
    op = read_memory_integer (pc, 4);
  }

  while (((tmp = op >> 16) == 0x9001) || /* st   r0, NUM(r1) */
	 (tmp == 0x9421) ||		/* stu  r1, NUM(r1) */
	 (op == 0x93e1fffc)) 		/* st   r31,-4(r1) */
  {
    pc += 4;
    op = read_memory_integer (pc, 4);
  }

  while ((tmp = (op >> 22)) == 0x20f) {	/* l	r31, ... or */
    pc += 4;				/* l	r30, ...    */
    op = read_memory_integer (pc, 4);
  }

  /* store parameters into stack */
  while(
	(op & 0xfc1f0000) == 0xd8010000 || 	/* stfd Rx,NUM(r1) */
	(op & 0xfc1f0000) == 0x90010000 ||	/* st r?, NUM(r1)  */
	(op & 0xfc000000) == 0xfc000000 ||	/* frsp, fp?, .. */
	(op & 0xd0000000) == 0xd0000000)	/* stfs, fp?, .. */
    {
      pc += 4;					/* store fpr double */
      op = read_memory_integer (pc, 4);
    }

  if (op == 0x603f0000) {			/* oril r31, r1, 0x0 */
    pc += 4;					/* this happens if r31 is used as */
    op = read_memory_integer (pc, 4);		/* frame ptr. (gcc does that)	  */

    tmp = 0;
    while ((op >> 16) == (0x907f + tmp)) {	/* st r3, NUM(r31) */
      pc += 4;					/* st r4, NUM(r31), ... */
      op = read_memory_integer (pc, 4);
      tmp += 0x20;
    }
  }
#if 0
/* I have problems with skipping over __main() that I need to address
 * sometime. Previously, I used to use misc_function_vector which
 * didn't work as well as I wanted to be.  -MGO */

  /* If the first thing after skipping a prolog is a branch to a function,
     this might be a call to an initializer in main(), introduced by gcc2.
     We'd like to skip over it as well. Fortunately, xlc does some extra
     work before calling a function right after a prologue, thus we can
     single out such gcc2 behaviour. */
     

  if ((op & 0xfc000001) == 0x48000001) { /* bl foo, an initializer function? */
    op = read_memory_integer (pc+4, 4);

    if (op == 0x4def7b82) {		/* cror 0xf, 0xf, 0xf (nop) */

      /* check and see if we are in main. If so, skip over this initializer
         function as well. */

      tmp = find_pc_misc_function (pc);
      if (tmp >= 0 && !strcmp (misc_function_vector [tmp].name, "main"))
        return pc + 8;
    }
  }
#endif /* 0 */
 
  return pc;
}


/*************************************************************************
  Support for creating pushind a dummy frame into the stack, and popping
  frames, etc. 
*************************************************************************/

/* The total size of dummy frame is 436, which is;

	32 gpr's	- 128 bytes
	32 fpr's	- 256   "
	7  the rest	- 28    "
	and 24 extra bytes for the callee's link area. The last 24 bytes
	for the link area might not be necessary, since it will be taken
	care of by push_arguments(). */

#define DUMMY_FRAME_SIZE 436

#define	DUMMY_FRAME_ADDR_SIZE 10

/* Make sure you initialize these in somewhere, in case gdb gives up what it
   was debugging and starts debugging something else. FIXMEibm */

static int dummy_frame_count = 0;
static int dummy_frame_size = 0;
static CORE_ADDR *dummy_frame_addr = 0;

extern int stop_stack_dummy;

/* push a dummy frame into stack, save all register. Currently we are saving
   only gpr's and fpr's, which is not good enough! FIXMEmgo */
   
void
push_dummy_frame ()
{
  int sp, pc;				/* stack pointer and link register */
  int ii;

  target_fetch_registers (-1);

  if (dummy_frame_count >= dummy_frame_size) {
    dummy_frame_size += DUMMY_FRAME_ADDR_SIZE;
    if (dummy_frame_addr)
      dummy_frame_addr = (CORE_ADDR*) xrealloc 
        (dummy_frame_addr, sizeof(CORE_ADDR) * (dummy_frame_size));
    else
      dummy_frame_addr = (CORE_ADDR*) 
	xmalloc (sizeof(CORE_ADDR) * (dummy_frame_size));
  }
  
  sp = read_register(SP_REGNUM);
  pc = read_register(PC_REGNUM);  

  dummy_frame_addr [dummy_frame_count++] = sp;

  /* Be careful! If the stack pointer is not decremented first, then kernel 
     thinks he is free to use the space underneath it. And kernel actually 
     uses that area for IPC purposes when executing ptrace(2) calls. So 
     before writing register values into the new frame, decrement and update
     %sp first in order to secure your frame. */

  write_register (SP_REGNUM, sp-DUMMY_FRAME_SIZE);

  /* gdb relies on the state of current_frame. We'd better update it,
     otherwise things like do_registers_info() wouldn't work properly! */

  flush_cached_frames ();
  set_current_frame (create_new_frame (sp-DUMMY_FRAME_SIZE, pc));

  /* save program counter in link register's space. */
  write_memory (sp+8, &pc, 4);

  /* save all floating point and general purpose registers here. */

  /* fpr's, f0..f31 */
  for (ii = 0; ii < 32; ++ii)
    write_memory (sp-8-(ii*8), &registers[REGISTER_BYTE (31-ii+FP0_REGNUM)], 8);

  /* gpr's r0..r31 */
  for (ii=1; ii <=32; ++ii)
    write_memory (sp-256-(ii*4), &registers[REGISTER_BYTE (32-ii)], 4);

  /* so far, 32*2 + 32 words = 384 bytes have been written. 
     7 extra registers in our register set: pc, ps, cnd, lr, cnt, xer, mq */

  for (ii=1; ii <= (LAST_SP_REGNUM-FIRST_SP_REGNUM+1); ++ii) {
    write_memory (sp-384-(ii*4), 
	       &registers[REGISTER_BYTE (FPLAST_REGNUM + ii)], 4);
  }

  /* Save sp or so called back chain right here. */
  write_memory (sp-DUMMY_FRAME_SIZE, &sp, 4);
  sp -= DUMMY_FRAME_SIZE;

  /* And finally, this is the back chain. */
  write_memory (sp+8, &pc, 4);
}


/* Pop a dummy frame.

   In rs6000 when we push a dummy frame, we save all of the registers. This
   is usually done before user calls a function explicitly.

   After a dummy frame is pushed, some instructions are copied into stack,
   and stack pointer is decremented even more.  Since we don't have a frame
   pointer to get back to the parent frame of the dummy, we start having
   trouble poping it.  Therefore, we keep a dummy frame stack, keeping
   addresses of dummy frames as such.  When poping happens and when we
   detect that was a dummy frame, we pop it back to its parent by using
   dummy frame stack (`dummy_frame_addr' array). 

FIXME:  This whole concept is broken.  You should be able to detect
a dummy stack frame *on the user's stack itself*.  When you do,
then you know the format of that stack frame -- including its
saved SP register!  There should *not* be a separate stack in the
GDB process that keeps track of these dummy frames!  -- gnu@cygnus.com Aug92
 */
   
pop_dummy_frame ()
{
  CORE_ADDR sp, pc;
  int ii;
  sp = dummy_frame_addr [--dummy_frame_count];

  /* restore all fpr's. */
  for (ii = 1; ii <= 32; ++ii)
    read_memory (sp-(ii*8), &registers[REGISTER_BYTE (32-ii+FP0_REGNUM)], 8);

  /* restore all gpr's */
  for (ii=1; ii <= 32; ++ii) {
    read_memory (sp-256-(ii*4), &registers[REGISTER_BYTE (32-ii)], 4);
  }

  /* restore the rest of the registers. */
  for (ii=1; ii <=(LAST_SP_REGNUM-FIRST_SP_REGNUM+1); ++ii)
    read_memory (sp-384-(ii*4),
    		&registers[REGISTER_BYTE (FPLAST_REGNUM + ii)], 4);

  read_memory (sp-(DUMMY_FRAME_SIZE-8), 
  				&registers [REGISTER_BYTE(PC_REGNUM)], 4);

  /* when a dummy frame was being pushed, we had to decrement %sp first, in 
     order to secure astack space. Thus, saved %sp (or %r1) value, is not the
     one we should restore. Change it with the one we need. */

  *(int*)&registers [REGISTER_BYTE(FP_REGNUM)] = sp;

  /* Now we can restore all registers. */

  target_store_registers (-1);
  pc = read_pc ();
  flush_cached_frames ();
  set_current_frame (create_new_frame (sp, pc));
}


/* pop the innermost frame, go back to the caller. */

void
pop_frame ()
{
  int pc, lr, sp, prev_sp;		/* %pc, %lr, %sp */
  struct aix_framedata fdata;
  FRAME fr = get_current_frame ();
  int addr, ii;

  pc = read_pc ();
  sp = FRAME_FP (fr);

  if (stop_stack_dummy && dummy_frame_count) {
    pop_dummy_frame ();
    return;
  }

  /* figure out previous %pc value. If the function is frameless, it is 
     still in the link register, otherwise walk the frames and retrieve the
     saved %pc value in the previous frame. */

  addr = get_pc_function_start (fr->pc) + FUNCTION_START_OFFSET;
  function_frame_info (addr, &fdata);

  read_memory (sp, &prev_sp, 4);
  if (fdata.frameless)
    lr = read_register (LR_REGNUM);
  else
    read_memory (prev_sp+8, &lr, 4);

  /* reset %pc value. */
  write_register (PC_REGNUM, lr);

  /* reset register values if any was saved earlier. */
  addr = prev_sp - fdata.offset;

  if (fdata.saved_gpr != -1)
    for (ii=fdata.saved_gpr; ii <= 31; ++ii) {
      read_memory (addr, &registers [REGISTER_BYTE (ii)], 4);
      addr += sizeof (int);
    }

  if (fdata.saved_fpr != -1)
    for (ii=fdata.saved_fpr; ii <= 31; ++ii) {
      read_memory (addr, &registers [REGISTER_BYTE (ii+FP0_REGNUM)], 8);
      addr += 8;
  }

  write_register (SP_REGNUM, prev_sp);
  target_store_registers (-1);
  flush_cached_frames ();
  set_current_frame (create_new_frame (prev_sp, lr));
}


/* fixup the call sequence of a dummy function, with the real function address.
   its argumets will be passed by gdb. */

void
fix_call_dummy(dummyname, pc, fun, nargs, type)
  char *dummyname;
  CORE_ADDR pc;
  CORE_ADDR fun;
  int nargs;					/* not used */
  int type;					/* not used */
{
#define	TOC_ADDR_OFFSET		20
#define	TARGET_ADDR_OFFSET	28

  int ii;
  CORE_ADDR target_addr;
  CORE_ADDR tocvalue;

  target_addr = fun;
  tocvalue = find_toc_address (target_addr);

  ii  = *(int*)((char*)dummyname + TOC_ADDR_OFFSET);
  ii = (ii & 0xffff0000) | (tocvalue >> 16);
  *(int*)((char*)dummyname + TOC_ADDR_OFFSET) = ii;

  ii  = *(int*)((char*)dummyname + TOC_ADDR_OFFSET+4);
  ii = (ii & 0xffff0000) | (tocvalue & 0x0000ffff);
  *(int*)((char*)dummyname + TOC_ADDR_OFFSET+4) = ii;

  ii  = *(int*)((char*)dummyname + TARGET_ADDR_OFFSET);
  ii = (ii & 0xffff0000) | (target_addr >> 16);
  *(int*)((char*)dummyname + TARGET_ADDR_OFFSET) = ii;

  ii  = *(int*)((char*)dummyname + TARGET_ADDR_OFFSET+4);
  ii = (ii & 0xffff0000) | (target_addr & 0x0000ffff);
  *(int*)((char*)dummyname + TARGET_ADDR_OFFSET+4) = ii;
}


/* return information about a function frame.
   in struct aix_frameinfo fdata:
    - frameless is TRUE, if function does not save %pc value in its frame.
    - offset is the number of bytes used in the frame to save registers.
    - saved_gpr is the number of the first saved gpr.
    - saved_fpr is the number of the first saved fpr.
    - alloca_reg is the number of the register used for alloca() handling.
      Otherwise -1.
 */
void
function_frame_info (pc, fdata)
  CORE_ADDR pc;
  struct aix_framedata *fdata;
{
  unsigned int tmp;
  register unsigned int op;

  fdata->offset = 0;
  fdata->saved_gpr = fdata->saved_fpr = fdata->alloca_reg = -1;

  op  = read_memory_integer (pc, 4);
  if (op == 0x7c0802a6) {		/* mflr r0 */
    pc += 4;
    op = read_memory_integer (pc, 4);
    fdata->frameless = 0;
  }
  else				/* else, this is a frameless invocation */
    fdata->frameless = 1;


  if ((op & 0xfc00003e) == 0x7c000026) { /* mfcr Rx */
    pc += 4;
    op = read_memory_integer (pc, 4);
  }

  if ((op & 0xfc000000) == 0x48000000) { /* bl foo, to save fprs??? */
    pc += 4;
    op = read_memory_integer (pc, 4);
    /* At this point, make sure this is not a trampoline function
       (a function that simply calls another functions, and nothing else).
       If the next is not a nop, this branch was part of the function
       prologue. */

    if (op == 0x4def7b82 ||		/* crorc 15, 15, 15 */
	op == 0x0)
      return;				/* prologue is over */
  }

  if ((op & 0xfc1f0000) == 0xd8010000) { /* stfd Rx,NUM(r1) */
    pc += 4;				 /* store floating register double */
    op = read_memory_integer (pc, 4);
  }

  if ((op & 0xfc1f0000) == 0xbc010000) { /* stm Rx, NUM(r1) */
    int tmp2;
    fdata->saved_gpr = (op >> 21) & 0x1f;
    tmp2 = op & 0xffff;
    if (tmp2 > 0x7fff)
      tmp2 = 0xffff0000 | tmp2;

    if (tmp2 < 0) {
      tmp2 = tmp2 * -1;
      fdata->saved_fpr = (tmp2 - ((32 - fdata->saved_gpr) * 4)) / 8;
      if ( fdata->saved_fpr > 0)
        fdata->saved_fpr = 32 - fdata->saved_fpr;
      else
        fdata->saved_fpr = -1;
    }
    fdata->offset = tmp2;
    pc += 4;
    op = read_memory_integer (pc, 4);
  }

  while (((tmp = op >> 16) == 0x9001) ||	/* st   r0, NUM(r1) */
	 (tmp == 0x9421) ||			/* stu  r1, NUM(r1) */
	 (op == 0x93e1fffc)) 			/* st   r31,-4(r1) */
  {
    /* gcc takes a short cut and uses this instruction to save r31 only. */

    if (op == 0x93e1fffc) {
      if (fdata->offset)
/*        fatal ("Unrecognized prolog."); */
        printf ("Unrecognized prolog!\n");

      fdata->saved_gpr = 31;
      fdata->offset = 4;
    }
    pc += 4;
    op = read_memory_integer (pc, 4);
  }

  while ((tmp = (op >> 22)) == 0x20f) {	/* l	r31, ... or */
    pc += 4;				/* l	r30, ...    */
    op = read_memory_integer (pc, 4);
  }

  /* store parameters into stack */
  while(
	(op & 0xfc1f0000) == 0xd8010000 || 	/* stfd Rx,NUM(r1) */
	(op & 0xfc1f0000) == 0x90010000 ||	/* st r?, NUM(r1)  */
	(op & 0xfc000000) == 0xfc000000 ||	/* frsp, fp?, .. */
	(op & 0xd0000000) == 0xd0000000)	/* stfs, fp?, .. */
    {
      pc += 4;					/* store fpr double */
      op = read_memory_integer (pc, 4);
    }

  if (op == 0x603f0000)				/* oril r31, r1, 0x0 */
    fdata->alloca_reg = 31;
}


/* Pass the arguments in either registers, or in the stack. In RS6000, the first
   eight words of the argument list (that might be less than eight parameters if
   some parameters occupy more than one word) are passed in r3..r11 registers.
   float and double parameters are passed in fpr's, in addition to that. Rest of
   the parameters if any are passed in user stack. There might be cases in which
   half of the parameter is copied into registers, the other half is pushed into
   stack.

   If the function is returning a structure, then the return address is passed
   in r3, then the first 7 words of the parametes can be passed in registers,
   starting from r4. */

CORE_ADDR
push_arguments (nargs, args, sp, struct_return, struct_addr)
  int nargs;
  value *args;
  CORE_ADDR sp;
  int struct_return;
  CORE_ADDR struct_addr;
{
  int ii, len;
  int argno;					/* current argument number */
  int argbytes;					/* current argument byte */
  char tmp_buffer [50];
  value arg;
  int f_argno = 0;				/* current floating point argno */

  CORE_ADDR saved_sp, pc;

  if ( dummy_frame_count <= 0)
    printf ("FATAL ERROR -push_arguments()! frame not found!!\n");

  /* The first eight words of ther arguments are passed in registers. Copy
     them appropriately.

     If the function is returning a `struct', then the first word (which 
     will be passed in r3) is used for struct return address. In that
     case we should advance one word and start from r4 register to copy 
     parameters. */

  ii =  struct_return ? 1 : 0;

  for (argno=0, argbytes=0; argno < nargs && ii<8; ++ii) {

    arg = value_arg_coerce (args[argno]);
    len = TYPE_LENGTH (VALUE_TYPE (arg));

    if (TYPE_CODE (VALUE_TYPE (arg)) == TYPE_CODE_FLT) {

      /* floating point arguments are passed in fpr's, as well as gpr's.
         There are 13 fpr's reserved for passing parameters. At this point
         there is no way we would run out of them. */

      if (len > 8)
        printf (
"Fatal Error: a floating point parameter #%d with a size > 8 is found!\n", argno);

      bcopy (VALUE_CONTENTS (arg), 
         &registers[REGISTER_BYTE(FP0_REGNUM + 1 + f_argno)], len);
      ++f_argno;
    }

    if (len > 4) {

      /* Argument takes more than one register. */
      while (argbytes < len) {

	*(int*)&registers[REGISTER_BYTE(ii+3)] = 0;
	bcopy ( ((char*)VALUE_CONTENTS (arg))+argbytes, 
			&registers[REGISTER_BYTE(ii+3)], 
			(len - argbytes) > 4 ? 4 : len - argbytes);
	++ii, argbytes += 4;

	if (ii >= 8)
	  goto ran_out_of_registers_for_arguments;
      }
      argbytes = 0;
      --ii;
    }
    else {        /* Argument can fit in one register. No problem. */
      *(int*)&registers[REGISTER_BYTE(ii+3)] = 0;
      bcopy (VALUE_CONTENTS (arg), &registers[REGISTER_BYTE(ii+3)], len);
    }
    ++argno;
  }

ran_out_of_registers_for_arguments:

  /* location for 8 parameters are always reserved. */
  sp -= 4 * 8;

  /* another six words for back chain, TOC register, link register, etc. */
  sp -= 24;

  /* if there are more arguments, allocate space for them in 
     the stack, then push them starting from the ninth one. */

  if ((argno < nargs) || argbytes) {
    int space = 0, jj;
    value val;

    if (argbytes) {
      space += ((len - argbytes + 3) & -4);
      jj = argno + 1;
    }
    else
      jj = argno;

    for (; jj < nargs; ++jj) {
      val = value_arg_coerce (args[jj]);
      space += ((TYPE_LENGTH (VALUE_TYPE (val))) + 3) & -4;
    }

    /* add location required for the rest of the parameters */
    space = (space + 7) & -8;
    sp -= space;

    /* This is another instance we need to be concerned about securing our
	stack space. If we write anything underneath %sp (r1), we might conflict
	with the kernel who thinks he is free to use this area. So, update %sp
	first before doing anything else. */

    write_register (SP_REGNUM, sp);

    /* if the last argument copied into the registers didn't fit there 
       completely, push the rest of it into stack. */

    if (argbytes) {
      write_memory (
        sp+24+(ii*4), ((char*)VALUE_CONTENTS (arg))+argbytes, len - argbytes);
      ++argno;
      ii += ((len - argbytes + 3) & -4) / 4;
    }

    /* push the rest of the arguments into stack. */
    for (; argno < nargs; ++argno) {

      arg = value_arg_coerce (args[argno]);
      len = TYPE_LENGTH (VALUE_TYPE (arg));


      /* float types should be passed in fpr's, as well as in the stack. */
      if (TYPE_CODE (VALUE_TYPE (arg)) == TYPE_CODE_FLT && f_argno < 13) {

        if (len > 8)
          printf (
"Fatal Error: a floating point parameter #%d with a size > 8 is found!\n", argno);

        bcopy (VALUE_CONTENTS (arg), 
           &registers[REGISTER_BYTE(FP0_REGNUM + 1 + f_argno)], len);
        ++f_argno;
      }

      write_memory (sp+24+(ii*4), VALUE_CONTENTS (arg), len);
      ii += ((len + 3) & -4) / 4;
    }
  }
  else
    /* Secure stack areas first, before doing anything else. */
    write_register (SP_REGNUM, sp);

  saved_sp = dummy_frame_addr [dummy_frame_count - 1];
  read_memory (saved_sp, tmp_buffer, 24);
  write_memory (sp, tmp_buffer, 24);

    write_memory (sp, &saved_sp, 4);	/* set back chain properly */

  target_store_registers (-1);
  return sp;
}

/* a given return value in `regbuf' with a type `valtype', extract and copy its
   value into `valbuf' */

void
extract_return_value (valtype, regbuf, valbuf)
  struct type *valtype;
  char regbuf[REGISTER_BYTES];
  char *valbuf;
{

  if (TYPE_CODE (valtype) == TYPE_CODE_FLT) {

    double dd; float ff;
    /* floats and doubles are returned in fpr1. fpr's have a size of 8 bytes.
       We need to truncate the return value into float size (4 byte) if
       necessary. */

    if (TYPE_LENGTH (valtype) > 4) 		/* this is a double */
      bcopy (&regbuf[REGISTER_BYTE (FP0_REGNUM + 1)], valbuf, 
						TYPE_LENGTH (valtype));
    else {		/* float */
      bcopy (&regbuf[REGISTER_BYTE (FP0_REGNUM + 1)], &dd, 8);
      ff = (float)dd;
      bcopy (&ff, valbuf, sizeof(float));
    }
  }
  else
    /* return value is copied starting from r3. */
    bcopy (&regbuf[REGISTER_BYTE (3)], valbuf, TYPE_LENGTH (valtype));
}


/* keep structure return address in this variable.
   FIXME:  This is a horrid kludge which should not be allowed to continue
   living.  This only allows a single nested call to a structure-returning
   function.  Come on, guys!  -- gnu@cygnus.com, Aug 92  */

CORE_ADDR rs6000_struct_return_address;


/* Throw away this debugging code. FIXMEmgo. */
void
print_frame(fram)
int fram;
{
  int ii, val;
  for (ii=0; ii<40; ++ii) {
    if ((ii % 4) == 0)
      printf ("\n");
    val = read_memory_integer (fram + ii * 4, 4);
    printf ("0x%08x\t", val);
  }
  printf ("\n");
}



/* Indirect function calls use a piece of trampoline code to do context
   switching, i.e. to set the new TOC table. Skip such code if we are on
   its first instruction (as when we have single-stepped to here). 
   Result is desired PC to step until, or NULL if we are not in
   trampoline code.  */

CORE_ADDR
skip_trampoline_code (pc)
CORE_ADDR pc;
{
  register unsigned int ii, op;

  static unsigned trampoline_code[] = {
	0x800b0000,			/*     l   r0,0x0(r11)	*/
	0x90410014,			/*    st   r2,0x14(r1)	*/
	0x7c0903a6,			/* mtctr   r0		*/
	0x804b0004,			/*     l   r2,0x4(r11)	*/
	0x816b0008,			/*     l  r11,0x8(r11)	*/
	0x4e800420,			/*  bctr		*/
	0x4e800020,			/*    br		*/
	0
  };

  for (ii=0; trampoline_code[ii]; ++ii) {
    op  = read_memory_integer (pc + (ii*4), 4);
    if (op != trampoline_code [ii])
      return NULL;
  }
  ii = read_register (11);		/* r11 holds destination addr	*/
  pc = read_memory_integer (ii, 4);	/* (r11) value			*/
  return pc;
}


/* Determines whether the function FI has a frame on the stack or not.
   Called from the FRAMELESS_FUNCTION_INVOCATION macro in tm.h.  */

int
frameless_function_invocation (fi)
struct frame_info *fi;
{
  CORE_ADDR func_start;
  struct aix_framedata fdata;

  func_start = get_pc_function_start (fi->pc) + FUNCTION_START_OFFSET;

  /* If we failed to find the start of the function, it is a mistake
     to inspect the instructions. */

  if (!func_start)
    return 0;

  function_frame_info (func_start, &fdata);
  return fdata.frameless;
}


/* If saved registers of frame FI are not known yet, read and cache them.
   &FDATAP contains aix_framedata; TDATAP can be NULL,
   in which case the framedata are read.  */

static void
frame_get_cache_fsr (fi, fdatap)
     struct frame_info *fi;
     struct aix_framedata *fdatap;
{
  int ii;
  CORE_ADDR frame_addr; 
  struct aix_framedata work_fdata;

  if (fi->cache_fsr)
    return;
  
  if (fdatap == NULL) {
    fdatap = &work_fdata;
    function_frame_info (get_pc_function_start (fi->pc), fdatap);
  }

  fi->cache_fsr = (struct frame_saved_regs *)
      obstack_alloc (&frame_cache_obstack, sizeof (struct frame_saved_regs));
  bzero (fi->cache_fsr, sizeof (struct frame_saved_regs));

  if (fi->prev && fi->prev->frame)
    frame_addr = fi->prev->frame;
  else
    frame_addr = read_memory_integer (fi->frame, 4);
  
  /* if != -1, fdatap->saved_fpr is the smallest number of saved_fpr.
     All fpr's from saved_fpr to fp31 are saved right underneath caller
     stack pointer, starting from fp31 first. */

  if (fdatap->saved_fpr >= 0) {
    for (ii=31; ii >= fdatap->saved_fpr; --ii)
      fi->cache_fsr->regs [FP0_REGNUM + ii] = frame_addr - ((32 - ii) * 8);
    frame_addr -= (32 - fdatap->saved_fpr) * 8;
  }

  /* if != -1, fdatap->saved_gpr is the smallest number of saved_gpr.
     All gpr's from saved_gpr to gpr31 are saved right under saved fprs,
     starting from r31 first. */
  
  if (fdatap->saved_gpr >= 0)
    for (ii=31; ii >= fdatap->saved_gpr; --ii)
      fi->cache_fsr->regs [ii] = frame_addr - ((32 - ii) * 4);
}

/* Return the address of a frame. This is the inital %sp value when the frame
   was first allocated. For functions calling alloca(), it might be saved in
   an alloca register. */

CORE_ADDR
frame_initial_stack_address (fi)
     struct frame_info *fi;
{
  CORE_ADDR tmpaddr;
  struct aix_framedata fdata;
  struct frame_info *callee_fi;

  /* if the initial stack pointer (frame address) of this frame is known,
     just return it. */

  if (fi->initial_sp)
    return fi->initial_sp;

  /* find out if this function is using an alloca register.. */

  function_frame_info (get_pc_function_start (fi->pc), &fdata);

  /* if saved registers of this frame are not known yet, read and cache them. */

  if (!fi->cache_fsr)
    frame_get_cache_fsr (fi, &fdata);

  /* If no alloca register used, then fi->frame is the value of the %sp for
     this frame, and it is good enough. */

  if (fdata.alloca_reg < 0) {
    fi->initial_sp = fi->frame;
    return fi->initial_sp;
  }

  /* This function has an alloca register. If this is the top-most frame
     (with the lowest address), the value in alloca register is good. */

  if (!fi->next)
    return fi->initial_sp = read_register (fdata.alloca_reg);     

  /* Otherwise, this is a caller frame. Callee has usually already saved
     registers, but there are exceptions (such as when the callee
     has no parameters). Find the address in which caller's alloca
     register is saved. */

  for (callee_fi = fi->next; callee_fi; callee_fi = callee_fi->next) {

    if (!callee_fi->cache_fsr)
      frame_get_cache_fsr (fi, NULL);

    /* this is the address in which alloca register is saved. */

    tmpaddr = callee_fi->cache_fsr->regs [fdata.alloca_reg];
    if (tmpaddr) {
      fi->initial_sp = read_memory_integer (tmpaddr, 4); 
      return fi->initial_sp;
    }

    /* Go look into deeper levels of the frame chain to see if any one of
       the callees has saved alloca register. */
  }

  /* If alloca register was not saved, by the callee (or any of its callees)
     then the value in the register is still good. */

  return fi->initial_sp = read_register (fdata.alloca_reg);     
}

/* xcoff_relocate_symtab -	hook for symbol table relocation.
   also reads shared libraries.. */

xcoff_relocate_symtab (pid)
unsigned int pid;
{
#define	MAX_LOAD_SEGS 64		/* maximum number of load segments */

    struct ld_info *ldi;
    int temp;

    ldi = (void *) alloca(MAX_LOAD_SEGS * sizeof (*ldi));

    /* According to my humble theory, AIX has some timing problems and
       when the user stack grows, kernel doesn't update stack info in time
       and ptrace calls step on user stack. That is why we sleep here a little,
       and give kernel to update its internals. */

    usleep (36000);

    errno = 0;
    ptrace(PT_LDINFO, pid, (PTRACE_ARG3_TYPE) ldi,
	   MAX_LOAD_SEGS * sizeof(*ldi), ldi);
    if (errno) {
      perror_with_name ("ptrace ldinfo");
      return 0;
    }

    vmap_ldinfo(ldi);

   do {
     add_text_to_loadinfo (ldi->ldinfo_textorg, ldi->ldinfo_dataorg);
    } while (ldi->ldinfo_next
	     && (ldi = (void *) (ldi->ldinfo_next + (char *) ldi)));

#if 0
  /* Now that we've jumbled things around, re-sort them.  */
  sort_minimal_symbols ();
#endif

  /* relocate the exec and core sections as well. */
  vmap_exec ();
}

/* Keep an array of load segment information and their TOC table addresses.
   This info will be useful when calling a shared library function by hand. */
   
struct loadinfo {
  CORE_ADDR textorg, dataorg;
  unsigned long toc_offset;
};

#define	LOADINFOLEN	10

/* FIXME Warning -- loadinfotextindex is used for a nefarious purpose by
   tm-rs6000.h.  */

static	struct loadinfo *loadinfo = NULL;
static	int	loadinfolen = 0;
static	int	loadinfotocindex = 0;
int	loadinfotextindex = 0;


void
xcoff_init_loadinfo ()
{
  loadinfotocindex = 0;
  loadinfotextindex = 0;

  if (loadinfolen == 0) {
    loadinfo = (struct loadinfo *)
               xmalloc (sizeof (struct loadinfo) * LOADINFOLEN);
    loadinfolen = LOADINFOLEN;
  }
}


/* FIXME -- this is never called!  */
void
free_loadinfo ()
{
  if (loadinfo)
    free (loadinfo);
  loadinfo = NULL;
  loadinfolen = 0;
  loadinfotocindex = 0;
  loadinfotextindex = 0;
}

/* this is called from xcoffread.c */

void
xcoff_add_toc_to_loadinfo (unsigned long tocoff)
{
  while (loadinfotocindex >= loadinfolen) {
    loadinfolen += LOADINFOLEN;
    loadinfo = (struct loadinfo *)
               xrealloc (loadinfo, sizeof(struct loadinfo) * loadinfolen);
  }
  loadinfo [loadinfotocindex++].toc_offset = tocoff;
}


static void
add_text_to_loadinfo (textaddr, dataaddr)
     CORE_ADDR textaddr;
     CORE_ADDR dataaddr;
{
  while (loadinfotextindex >= loadinfolen) {
    loadinfolen += LOADINFOLEN;
    loadinfo = (struct loadinfo *)
               xrealloc (loadinfo, sizeof(struct loadinfo) * loadinfolen);
  }
  loadinfo [loadinfotextindex].textorg = textaddr;
  loadinfo [loadinfotextindex].dataorg = dataaddr;
  ++loadinfotextindex;
}


/* FIXME:  This assumes that the "textorg" and "dataorg" elements
   of a member of this array are correlated with the "toc_offset"
   element of the same member.  But they are sequentially assigned in wildly
   different places, and probably there is no correlation.  FIXME!  */

static CORE_ADDR
find_toc_address (pc)
     CORE_ADDR pc;
{
  int ii, toc_entry, tocbase = 0;

  for (ii=0; ii < loadinfotextindex; ++ii)
    if (pc > loadinfo[ii].textorg && loadinfo[ii].textorg > tocbase) {
      toc_entry = ii;
      tocbase = loadinfo[ii].textorg;
    }

  return loadinfo[toc_entry].dataorg + loadinfo[toc_entry].toc_offset;
}
