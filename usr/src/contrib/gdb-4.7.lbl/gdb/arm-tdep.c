/* Target-dependent code for the Acorn Risc Machine, for GDB, the GNU Debugger.
   Copyright 1988, 1989, 1991 Free Software Foundation, Inc.

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
#include "arm-opcode.h"

#include <sys/param.h>
#include <sys/dir.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/ptrace.h>
#include <machine/reg.h>

#define N_TXTADDR(hdr) 0x8000
#define N_DATADDR(hdr) (hdr.a_text + 0x8000)

#include "gdbcore.h"
#include <sys/user.h>		/* After a.out.h  */
#include <sys/file.h>
#include <sys/stat.h>

#include <errno.h>


/* Work with core dump and executable files, for GDB. 
   This code would be in core.c if it weren't machine-dependent. */

/* Structure to describe the chain of shared libraries used
   by the execfile.
   e.g. prog shares Xt which shares X11 which shares c. */

struct shared_library {
    struct exec_header header;
    char name[SHLIBLEN];
    CORE_ADDR text_start;	/* CORE_ADDR of 1st byte of text, this file */
    long data_offset;		/* offset of data section in file */
    int chan;			/* file descriptor for the file */
    struct shared_library *shares; /* library this one shares */
};
static struct shared_library *shlib = 0;

/* Hook for `exec_file_command' command to call.  */

extern void (*exec_file_display_hook) ();
   
static CORE_ADDR unshared_text_start;

/* extended header from exec file (for shared library info) */

static struct exec_header exec_header;

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
  unshared_text_start = 0;
  text_end = 0;
  exec_data_start = 0;
  exec_data_end = 0;
  if (execchan >= 0)
    close (execchan);
  execchan = -1;
  if (shlib) {
      close_shared_library(shlib);
      shlib = 0;
  }

  /* Now open and digest the file the user requested, if any.  */

  if (filename)
    {
      filename = tilde_expand (filename);
      make_cleanup (free, filename);

      execchan = openp (getenv ("PATH"), 1, filename, O_RDONLY, 0,
			&execfile);
      if (execchan < 0)
	perror_with_name (filename);

      {
	struct stat st_exec;

#ifdef HEADER_SEEK_FD
	HEADER_SEEK_FD (execchan);
#endif
	
	val = myread (execchan, &exec_header, sizeof exec_header);
	exec_aouthdr = exec_header.a_exec;

	if (val < 0)
	  perror_with_name (filename);

	text_start = 0x8000;

	/* Look for shared library if needed */
	if (exec_header.a_exec.a_magic & MF_USES_SL)
	    shlib = open_shared_library(exec_header.a_shlibname, text_start);

	text_offset = N_TXTOFF (exec_aouthdr);
	exec_data_offset = N_TXTOFF (exec_aouthdr) + exec_aouthdr.a_text;

	if (shlib) {
	    unshared_text_start = shared_text_end(shlib) & ~0x7fff;
	    stack_start = shlib->header.a_exec.a_sldatabase;
	    stack_end = STACK_END_ADDR;
	} else
	    unshared_text_start = 0x8000;
	text_end = unshared_text_start + exec_aouthdr.a_text;

	exec_data_start = unshared_text_start + exec_aouthdr.a_text;
        exec_data_end = exec_data_start + exec_aouthdr.a_data;

	data_start = exec_data_start;
	data_end += exec_data_start;

	fstat (execchan, &st_exec);
	exec_mtime = st_exec.st_mtime;
      }

      validate_files ();
    }
  else if (from_tty)
    printf ("No exec file now.\n");

  /* Tell display code (if any) about the changed file name.  */
  if (exec_file_display_hook)
    (*exec_file_display_hook) (filename);
}

/* Read from the program's memory (except for inferior processes).
   This function is misnamed, since it only reads, never writes; and
   since it will use the core file and/or executable file as necessary.

   It should be extended to write as well as read, FIXME, for patching files.

   Return 0 if address could be read, EIO if addresss out of bounds.  */

int
xfer_core_file (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  register int i;
  register int val;
  int xferchan;
  char **xferfile;
  int fileptr;
  int returnval = 0;

  while (len > 0)
    {
      xferfile = 0;
      xferchan = 0;

      /* Determine which file the next bunch of addresses reside in,
	 and where in the file.  Set the file's read/write pointer
	 to point at the proper place for the desired address
	 and set xferfile and xferchan for the correct file.

	 If desired address is nonexistent, leave them zero.

	 i is set to the number of bytes that can be handled
	 along with the next address.

	 We put the most likely tests first for efficiency.  */

      /* Note that if there is no core file
	 data_start and data_end are equal.  */
      if (memaddr >= data_start && memaddr < data_end)
	{
	  i = min (len, data_end - memaddr);
	  fileptr = memaddr - data_start + data_offset;
	  xferfile = &corefile;
	  xferchan = corechan;
	}
      /* Note that if there is no core file
	 stack_start and stack_end define the shared library data.  */
      else if (memaddr >= stack_start && memaddr < stack_end)
	{
	    if (corechan < 0) {
		struct shared_library *lib;
		for (lib = shlib; lib; lib = lib->shares)
		    if (memaddr >= lib->header.a_exec.a_sldatabase &&
			memaddr < lib->header.a_exec.a_sldatabase +
			  lib->header.a_exec.a_data)
			break;
		if (lib) {
		    i = min (len, lib->header.a_exec.a_sldatabase +
			     lib->header.a_exec.a_data - memaddr);
		    fileptr = lib->data_offset + memaddr -
			lib->header.a_exec.a_sldatabase;
		    xferfile = execfile;
		    xferchan = lib->chan;
		}
	    } else {
		i = min (len, stack_end - memaddr);
		fileptr = memaddr - stack_start + stack_offset;
		xferfile = &corefile;
		xferchan = corechan;
	    }
	}
      else if (corechan < 0
	       && memaddr >= exec_data_start && memaddr < exec_data_end)
	{
	  i = min (len, exec_data_end - memaddr);
	  fileptr = memaddr - exec_data_start + exec_data_offset;
	  xferfile = &execfile;
	  xferchan = execchan;
	}
      else if (memaddr >= text_start && memaddr < text_end)
	{
	    struct shared_library *lib;
	    for (lib = shlib; lib; lib = lib->shares)
		if (memaddr >= lib->text_start &&
		    memaddr < lib->text_start + lib->header.a_exec.a_text)
		    break;
	    if (lib) {
		i = min (len, lib->header.a_exec.a_text +
			 lib->text_start - memaddr);
		fileptr = memaddr - lib->text_start + text_offset;
		xferfile = &execfile;
		xferchan = lib->chan;
	    } else {
		i = min (len, text_end - memaddr);
		fileptr = memaddr - unshared_text_start + text_offset;
		xferfile = &execfile;
		xferchan = execchan;
	    }
	}
      else if (memaddr < text_start)
	{
	  i = min (len, text_start - memaddr);
	}
      else if (memaddr >= text_end
	       && memaddr < (corechan >= 0? data_start : exec_data_start))
	{
	  i = min (len, data_start - memaddr);
	}
      else if (corechan >= 0
	       && memaddr >= data_end && memaddr < stack_start)
	{
	  i = min (len, stack_start - memaddr);
	}
      else if (corechan < 0 && memaddr >= exec_data_end)
	{
	  i = min (len, - memaddr);
	}
      else if (memaddr >= stack_end && stack_end != 0)
	{
	  i = min (len, - memaddr);
	}
      else
	{
	  /* Address did not classify into one of the known ranges.
	     This shouldn't happen; we catch the endpoints.  */
	  fatal ("Internal: Bad case logic in xfer_core_file.");
	}

      /* Now we know which file to use.
	 Set up its pointer and transfer the data.  */
      if (xferfile)
	{
	  if (*xferfile == 0)
	    if (xferfile == &execfile)
	      error ("No program file to examine.");
	    else
	      error ("No core dump file or running program to examine.");
	  val = lseek (xferchan, fileptr, 0);
	  if (val < 0)
	    perror_with_name (*xferfile);
	  val = myread (xferchan, myaddr, i);
	  if (val < 0)
	    perror_with_name (*xferfile);
	}
      /* If this address is for nonexistent memory,
	 read zeros if reading, or do nothing if writing.
	 Actually, we never right.  */
      else
	{
	  bzero (myaddr, i);
	  returnval = EIO;
	}

      memaddr += i;
      myaddr += i;
      len -= i;
    }
  return returnval;
}

/* APCS (ARM procedure call standard) defines the following prologue:

   mov		ip, sp
  [stmfd	sp!, {a1,a2,a3,a4}]
   stmfd	sp!, {...,fp,ip,lr,pc}
  [stfe		f7, [sp, #-12]!]
  [stfe		f6, [sp, #-12]!]
  [stfe		f5, [sp, #-12]!]
  [stfe		f4, [sp, #-12]!]
   sub		fp, ip, #nn	// nn == 20 or 4 depending on second ins
*/

CORE_ADDR
skip_prologue(pc)
CORE_ADDR pc;
{
    union insn_fmt op;
    CORE_ADDR skip_pc = pc;

    op.ins = read_memory_integer(skip_pc, 4);
    /* look for the "mov ip,sp" */
    if (op.generic.type != TYPE_ARITHMETIC ||
	op.arith.opcode != OPCODE_MOV ||
	op.arith.dest != SPTEMP ||
	op.arith.operand2 != SP) return pc;
    skip_pc += 4;
    /* skip the "stmfd sp!,{a1,a2,a3,a4}" if its there */
    op.ins = read_memory_integer(skip_pc, 4);
    if (op.generic.type == TYPE_BLOCK_BRANCH &&
	op.generic.subtype == SUBTYPE_BLOCK &&
	op.block.mask == 0xf &&
	op.block.base == SP &&
	op.block.is_load == 0 &&
	op.block.writeback == 1 &&
	op.block.increment == 0 &&
	op.block.before == 1) skip_pc += 4;
    /* skip the "stmfd sp!,{...,fp,ip,lr,pc} */
    op.ins = read_memory_integer(skip_pc, 4);
    if (op.generic.type != TYPE_BLOCK_BRANCH ||
	op.generic.subtype != SUBTYPE_BLOCK ||
	/* the mask should look like 110110xxxxxx0000 */
	(op.block.mask & 0xd800) != 0xd800 ||
	op.block.base != SP ||
	op.block.is_load != 0 ||
	op.block.writeback != 1 ||
	op.block.increment != 0 ||
	op.block.before != 1) return pc;
    skip_pc += 4;
    /* check for "sub fp,ip,#nn" */
    op.ins = read_memory_integer(skip_pc, 4);
    if (op.generic.type != TYPE_ARITHMETIC ||
	op.arith.opcode != OPCODE_SUB ||
	op.arith.dest != FP ||
	op.arith.operand1 != SPTEMP) return pc;
    return skip_pc + 4;
}

static void
print_fpu_flags(flags)
int flags;
{
    if (flags & (1 << 0)) fputs("IVO ", stdout);
    if (flags & (1 << 1)) fputs("DVZ ", stdout);
    if (flags & (1 << 2)) fputs("OFL ", stdout);
    if (flags & (1 << 3)) fputs("UFL ", stdout);
    if (flags & (1 << 4)) fputs("INX ", stdout);
    putchar('\n');
}

void
arm_float_info()
{
    register unsigned long status = read_register(FPS_REGNUM);
    int type;

    type = (status >> 24) & 127;
    printf("%s FPU type %d\n",
	   (status & (1<<31)) ? "Hardware" : "Software",
	   type);
    fputs("mask: ", stdout);
    print_fpu_flags(status >> 16);
    fputs("flags: ", stdout);
    print_fpu_flags(status);
}
