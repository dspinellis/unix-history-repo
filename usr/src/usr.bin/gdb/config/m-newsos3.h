/* Parameters for execution on a Sony/NEWS with News-OS version 3,
   for GDB, the GNU debugger.
   Copyright (C) 1989 Free Software Foundation, Inc.

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
#define NEWSOS3
#include "m-news.h"

#undef STACK_END_ADDR
/* <machine/vmparam.h> USRSTACK */
#define STACK_END_ADDR  (0x80000000 - UPAGES*NBPG)

/* Have siginterupt on NEWS OS 3.x.  */
#undef NO_SIGINTERRUPT

#undef USE_PCB

/* now old ptrace bug has fixed almost.  Remain constrain is;
     When some process do not touch the floating register,
     ptrace cannot change the floating register containts.
*/
#undef PTRACE_BUG

#undef PUSH_DUMMY_FRAME
#undef POP_FRAME
/* now ptrace has a bug to write floating register in NEWS OS version 2.x */
#define PUSH_DUMMY_FRAME \
{ register CORE_ADDR sp = read_register (SP_REGNUM);			\
  register int regnum;							\
  char raw_buffer[12];							\
  sp = push_word (sp, read_register (PC_REGNUM));			\
  sp = push_word (sp, read_register (FP_REGNUM));			\
  write_register (FP_REGNUM, sp);					\
  for (regnum = FP0_REGNUM + 7; regnum >= FP0_REGNUM; regnum--)		\
    { read_register_bytes (REGISTER_BYTE (regnum), raw_buffer, 12);	\
      sp = push_bytes (sp, raw_buffer, 12); }				\
  for (regnum = FP_REGNUM - 1; regnum >= 0; regnum--)			\
    sp = push_word (sp, read_register (regnum));			\
  sp = push_word (sp, read_register (PS_REGNUM));			\
  write_register (SP_REGNUM, sp);  }

/* Discard from the stack the innermost frame, restoring all registers.  */

#define POP_FRAME  \
{ register FRAME frame = get_current_frame ();			 	\
  register CORE_ADDR fp;					 	\
  register int regnum;							\
  struct frame_saved_regs fsr;						\
  struct frame_info *fi;						\
  char raw_buffer[12];							\
  fi = get_frame_info (frame);					 	\
  fp = fi->frame;						 	\
  get_frame_saved_regs (fi, &fsr);					\
  for (regnum = FP0_REGNUM + 7; regnum >= FP0_REGNUM; regnum--)		\
    if (fsr.regs[regnum])						\
      { read_memory (fsr.regs[regnum], raw_buffer, 12);			\
        write_register_bytes (REGISTER_BYTE (regnum), raw_buffer, 12); }\
  for (regnum = FP_REGNUM - 1; regnum >= 0; regnum--)			\
    if (fsr.regs[regnum])						\
      write_register (regnum, read_memory_integer (fsr.regs[regnum], 4)); \
  if (fsr.regs[PS_REGNUM])						\
    write_register (PS_REGNUM, read_memory_integer (fsr.regs[PS_REGNUM], 4)); \
  write_register (FP_REGNUM, read_memory_integer (fp, 4));		\
  write_register (PC_REGNUM, read_memory_integer (fp + 4, 4));  	\
  write_register (SP_REGNUM, fp + 8);					\
  flush_cached_frames ();					 	\
  set_current_frame (create_new_frame (read_register (FP_REGNUM),	\
				       read_pc ())); }

/* NewsOS 3 apparently dies on large alloca's -- roland@ai.mit.edu.  */
#define	BROKEN_LARGE_ALLOCA

/* Make this macro from peep of core file.  */
#undef REGISTER_U_ADDR
#ifdef USE_PCB
#define REGISTER_U_ADDR(addr, blockend, regno)		\
{	blockend += 4;  /* why? */ \
        if (regno <= FP_REGNUM) \
	  addr = blockend + regno * 4; \
	else if (regno == SP_REGNUM) \
	  addr = blockend - 4 - 4 * 4; \
	else if (regno <= PS_REGNUM) \
	  addr = blockend - 4 + (regno - PS_REGNUM) * 4; \
	else if (regno < FPC_REGNUM) \
	  addr = blockend + (14 + 1 + 3) * 4 + (regno - FP0_REGNUM) * 12; \
	else \
	  addr = blockend + 15 * 4 + (regno - FPC_REGNUM) * 4; \
}
#else
#define REGISTER_U_ADDR(addr, blockend, regno)		\
{	blockend += 4;  /* why? */ \
	if (regno <= FP_REGNUM) \
	  addr = blockend + regno * 4; \
	else if (regno == SP_REGNUM) \
	  addr = blockend -4 - 4 * 4; \
	else if (regno <= PS_REGNUM) \
	  addr = blockend - 4 + (regno - PS_REGNUM) * 4; \
	else if (regno < FPC_REGNUM) \
	  addr = blockend + (14 + 1 + 3) * 4 + (regno - FP0_REGNUM) * 12; \
	else \
	  addr = blockend + 15 * 4 + (regno - FPC_REGNUM) * 4; \
}
#endif
