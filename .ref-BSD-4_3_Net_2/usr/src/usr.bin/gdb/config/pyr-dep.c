/* Low level interface to ptrace, for GDB when running under Unix.
   Copyright (C) 1988, 1989 Free Software Foundation, Inc.

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
#include "defs.h"
#include "param.h"
#include "frame.h"
#include "inferior.h"

#include <sys/param.h>
#include <sys/dir.h>
#include <signal.h>
#include <sys/ioctl.h>
/* #include <fcntl.h>  Can we live without this?  */

#include <a.out.h>
#ifndef N_SET_MAGIC
#define N_SET_MAGIC(exec, val) ((exec).a_magic = (val))
#endif

#include <sys/user.h>		/* After a.out.h  */
#include <sys/file.h>
#include <sys/stat.h>

extern int errno;

/* This function simply calls ptrace with the given arguments.  
   It exists so that all calls to ptrace are isolated in this 
   machine-dependent file. */
int
call_ptrace (request, pid, arg3, arg4)
     int request, pid, arg3, arg4;
{
  return ptrace (request, pid, arg3, arg4);
}

kill_inferior ()
{
  if (remote_debugging)
    return;
  if (inferior_pid == 0)
    return;
  ptrace (8, inferior_pid, 0, 0);
  wait (0);
  inferior_died ();
}

/* This is used when GDB is exiting.  It gives less chance of error.*/

kill_inferior_fast ()
{
  if (remote_debugging)
    return;
  if (inferior_pid == 0)
    return;
  ptrace (8, inferior_pid, 0, 0);
  wait (0);
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
  if (remote_debugging)
    remote_resume (step, signal);
  else
    {
      ptrace (step ? 9 : 7, inferior_pid, 1, signal);
      if (errno)
	perror_with_name ("ptrace");
    }
}

void
fetch_inferior_registers ()
{
  register int regno, datum;
  register unsigned int regaddr;
  int reg_buf[NUM_REGS+1];
  struct user u;
  register int skipped_frames = 0;

  if (remote_debugging)
    remote_fetch_registers ();
  else
    {
      for (regno = 0; regno < 64; regno++) {
	reg_buf[regno] = ptrace (3, inferior_pid, regno, 0);

#if defined(PYRAMID_CONTROL_FRAME_DEBUGGING)
	printf ("Fetching %s from inferior, got %0x\n",
		reg_names[regno],
		reg_buf[regno]);
#endif /* PYRAMID_CONTROL_FRAME_DEBUGGING */

	if (reg_buf[regno] == -1 && errno == EIO) {
	  printf("fetch_interior_registers: fetching %s from inferior\n",
		 reg_names[regno]);
	  errno = 0;
	}
	supply_register (regno, reg_buf+regno);
      }
      /* that leaves regs 64, 65, and 66 */
      datum = ptrace (3, inferior_pid,
		      ((char *)&u.u_pcb.pcb_csp) -
		      ((char *)&u), 0);



      /* FIXME: Find the Current Frame Pointer (CFP). CFP is a global
	 register (ie, NOT windowed), that gets saved in a frame iff
	 the code for that frame has a prologue (ie, "adsf N").  If
	 there is a prologue, the adsf insn saves the old cfp in
	 pr13, cfp is set to sp, and N bytes of locals are allocated
	 (sp is decremented by n).
	 This makes finding CFP hard. I guess the right way to do it
	 is: 
	     - If this is the innermost frame, believe ptrace() or
	     the core area.
	     - Otherwise:
		 Find the first insn of the current frame.
		 - find the saved pc;
		 - find the call insn that saved it;
		 - figure out where the call is to;
		 - if the first insn is an adsf, we got a frame
		   pointer. */
      

      /* Normal processors have separate stack pointers for user and
         kernel mode. Getting the last user mode frame on such
	 machines is easy: the kernel context of the ptrace()'d
	 process is on the kernel stack, and the USP points to what
	 we want. But Pyramids only have a single cfp for both user and
	 kernel mode.  And processes being ptrace()'d have some
	 kernel-context control frames on their stack.
	 To avoid tracing back into the kernel context of an inferior,
	 we skip 0 or more contiguous control frames where the pc is
	 in the kernel. */ 

      while (1) {
	register int inferior_saved_pc;
	inferior_saved_pc = ptrace (1, inferior_pid, datum+((32+15)*4), 0);
	if (inferior_saved_pc > 0) break;
#if defined(PYRAMID_CONTROL_FRAME_DEBUGGING)
	printf("skipping kernel frame %08x, pc=%08x\n", datum,
	       inferior_saved_pc);
#endif /* PYRAMID_CONTROL_FRAME_DEBUGGING */
	skipped_frames++;
	datum -= CONTROL_STACK_FRAME_SIZE;
      }
     
      reg_buf[CSP_REGNUM] = datum;
      supply_register(CSP_REGNUM, reg_buf+CSP_REGNUM);
#ifdef  PYRAMID_CONTROL_FRAME_DEBUGGING
      if (skipped_frames) {
	fprintf (stderr,
		 "skipped %d frames from %x to %x; cfp was %x, now %x\n",
		 skipped_frames, reg_buf[CSP_REGNUM]);
      }
#endif /* PYRAMID_CONTROL_FRAME_DEBUGGING */
    }
}

/* Store our register values back into the inferior.
   If REGNO is -1, do this for all registers.
   Otherwise, REGNO specifies which register (so we can save time).  */

store_inferior_registers (regno)
     int regno;
{
  register unsigned int regaddr;
  char buf[80];

  if (regno >= 0)
    {
      if ((0 <= regno) && (regno < 64)) {
	/*regaddr = register_addr (regno, offset);*/
	regaddr = regno;
	errno = 0;
	ptrace (6, inferior_pid, regaddr, read_register (regno));
	if (errno != 0)
	  {
	    sprintf (buf, "writing register number %d", regno);
	    perror_with_name (buf);
	  }
      }
    }
  else for (regno = 0; regno < NUM_REGS; regno++)
    {
      /*regaddr = register_addr (regno, offset);*/
      regaddr = regno;
      errno = 0;
      ptrace (6, inferior_pid, regaddr, read_register (regno));
      if (errno != 0)
	{
	  sprintf (buf, "writing all regs, number %d", regno);
	  perror_with_name (buf);
	}
    }
}

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

  /* Read all the longwords */
  for (i = 0; i < count; i++, addr += sizeof (int))
    {
      errno = 0;
#if 0
/*This is now done by read_memory, because when this function did it,
  reading a byte or short int hardware port read whole longs, causing
  serious side effects
  such as bus errors and unexpected hardware operation.  This would
  also be a problem with ptrace if the inferior process could read
  or write hardware registers, but that's not usually the case.  */
      if (remote_debugging)
	buffer[i] = remote_fetch_word (addr);
      else
#endif
	buffer[i] = ptrace (1, inferior_pid, addr, 0);
      if (errno)
	return errno;
    }

  /* Copy appropriate bytes out of the buffer.  */
  bcopy ((char *) buffer + (memaddr & (sizeof (int) - 1)), myaddr, len);
  return 0;
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
    buffer[0] = remote_fetch_word (addr);
  else
    buffer[0] = ptrace (1, inferior_pid, addr, 0);

  if (count > 1)
    {
      if (remote_debugging)
	buffer[count - 1]
	  = remote_fetch_word (addr + (count - 1) * sizeof (int));
      else
	buffer[count - 1]
	  = ptrace (1, inferior_pid,
		    addr + (count - 1) * sizeof (int), 0);
    }

  /* Copy data to be written over corresponding part of buffer */

  bcopy (myaddr, (char *) buffer + (memaddr & (sizeof (int) - 1)), len);

  /* Write the entire buffer.  */

  for (i = 0; i < count; i++, addr += sizeof (int))
    {
      errno = 0;
      if (remote_debugging)
	remote_store_word (addr, buffer[i]);
      else
	ptrace (4, inferior_pid, addr, buffer[i]);
      if (errno)
	return errno;
    }

  return 0;
}

/*** Extensions to  core and dump files, for GDB. */

extern unsigned int last_frame_offset;

#ifdef PYRAMID_CORE

/* Can't make definitions here static, since core.c needs them
   to do bounds checking on the core-file areas. O well. */

/* have two stacks: one for data, one for register windows. */
extern CORE_ADDR reg_stack_start;
extern CORE_ADDR reg_stack_end;

/* need this so we can find the global registers: they never get saved. */
static CORE_ADDR global_reg_offset;
static CORE_ADDR last_frame_address;
static CORE_ADDR last_frame_offset;


/* Address in core file of start of register window stack area.
   Don't know if is this any of meaningful, useful or necessary.   */
static CORE_ADDR reg_stack_offset;

#endif /* PYRAMID_CORE */  


/* Work with core dump and executable files, for GDB. 
   This code would be in core.c if it weren't machine-dependent. */

#ifndef N_TXTADDR
#define N_TXTADDR(hdr) 0
#endif /* no N_TXTADDR */

#ifndef N_DATADDR
#define N_DATADDR(hdr) hdr.a_text
#endif /* no N_DATADDR */

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

#ifdef PYRAMID_CORE
/* Well, "two areas of memory" on most machines; but pyramids have a
   third area, for the register-window stack, and we need its
   base and  bound too.  */

extern CORE_ADDR reg_stack_start;
extern CORE_ADDR reg_stack_start;
#endif /* PYRAMID_CORE */

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

core_file_command (filename, from_tty)
     char *filename;
     int from_tty;
{
  int val;
  extern char registers[];

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

#ifdef PYRAMID_CORE
  reg_stack_start = CONTROL_STACK_ADDR;
  reg_stack_end = CONTROL_STACK_ADDR;	/* this isn't strictly true...*/
#endif /* PYRAMID_CORE */

  /* Now, if a new core file was specified, open it and digest it.  */

  if (filename)
    {
      filename = tilde_expand (filename);
      make_cleanup (free, filename);
      
      if (have_inferior_p ())
	error ("To look at a core file, you must kill the inferior with \"kill\".");
      corechan = open (filename, O_RDONLY, 0);
      if (corechan < 0)
	perror_with_name (filename);
      /* 4.2-style (and perhaps also sysV-style) core dump file.  */
      {
	struct user u;

	unsigned int reg_offset;

	val = myread (corechan, &u, sizeof u);
	if (val < 0)
	  perror_with_name ("Not a core file: reading upage");
	if (val != sizeof u)
	  error ("Not a core file: could only read %d bytes", val);
	data_start = exec_data_start;

	data_end = data_start + NBPG * u.u_dsize;
	data_offset = NBPG * UPAGES;
	stack_offset = NBPG * (UPAGES + u.u_dsize);

	/* find registers in core file */
#ifdef PYRAMID_PTRACE
	stack_start = stack_end - NBPG * u.u_ussize;
	reg_stack_offset = stack_offset + (NBPG *u.u_ussize);
	reg_stack_end = reg_stack_start + NBPG * u.u_cssize;

	last_frame_address = ((int) u.u_pcb.pcb_csp);
	last_frame_offset = reg_stack_offset + last_frame_address
		- CONTROL_STACK_ADDR ;
	global_reg_offset = (char *)&u - (char *)&u.u_pcb.pcb_gr0 ;

	/* skip any control-stack frames that were executed in the
	   kernel. */

	while (1) {
	    char buf[4];
	    val = lseek (corechan, last_frame_offset+(47*4), 0);
	    if (val < 0)
		    perror_with_name (filename);
	    val = myread (corechan, buf, sizeof buf);
	    if (val < 0)
		    perror_with_name (filename);

	    if (*(int *)buf >= 0)
		    break;
	    printf ("skipping frame %0x\n", last_frame_address);
	    last_frame_offset -= CONTROL_STACK_FRAME_SIZE;
	    last_frame_address -= CONTROL_STACK_FRAME_SIZE;
	}
	reg_offset = last_frame_offset;

#if 1 || defined(PYRAMID_CONTROL_FRAME_DEBUGGING)
	printf ("Control stack pointer = 0x%08x\n",
		u.u_pcb.pcb_csp);
	printf ("offset to control stack %d outermost frame %d (%0x)\n",
	      reg_stack_offset, reg_offset, last_frame_address);
#endif /* PYRAMID_CONTROL_FRAME_DEBUGGING */

#else /* not PYRAMID_CORE */
	stack_start = stack_end - NBPG * u.u_ssize;
        reg_offset = (int) u.u_ar0 - KERNEL_U_ADDR;
#endif /* not PYRAMID_CORE */

#ifdef __not_on_pyr_yet
	/* Some machines put an absolute address in here and some put
	   the offset in the upage of the regs.  */
	reg_offset = (int) u.u_ar0;
	if (reg_offset > NBPG * UPAGES)
	  reg_offset -= KERNEL_U_ADDR;
#endif

	/* I don't know where to find this info.
	   So, for now, mark it as not available.  */
	N_SET_MAGIC (core_aouthdr, 0);

	/* Read the register values out of the core file and store
	   them where `read_register' will find them.  */

	{
	  register int regno;

	  for (regno = 0; regno < 64; regno++)
	    {
	      char buf[MAX_REGISTER_RAW_SIZE];

	      val = lseek (corechan, register_addr (regno, reg_offset), 0);
	      if (val < 0
		  || (val = myread (corechan, buf, sizeof buf)) < 0)
		{
		  char * buffer = (char *) alloca (strlen (reg_names[regno])
						   + 30);
		  strcpy (buffer, "Reading register ");
		  strcat (buffer, reg_names[regno]);
						   
		  perror_with_name (buffer);
		}

	      if (val < 0)
		perror_with_name (filename);
#ifdef PYRAMID_CONTROL_FRAME_DEBUGGING
      printf ("[reg %s(%d), offset in file %s=0x%0x, addr =0x%0x, =%0x]\n",
	      reg_names[regno], regno, filename,
	      register_addr(regno, reg_offset),
	      regno * 4 + last_frame_address,
	      *((int *)buf));
#endif /* PYRAMID_CONTROL_FRAME_DEBUGGING */
	      supply_register (regno, buf);
	    }
	}
      }
      if (filename[0] == '/')
	corefile = savestring (filename, strlen (filename));
      else
	{
	  corefile = concat (current_directory, "/", filename);
	}

#if 1 || defined(PYRAMID_CONTROL_FRAME_DEBUGGING)
      printf ("Providing CSP (%0x) as nominal address of current frame.\n",
	      last_frame_address);
#endif PYRAMID_CONTROL_FRAME_DEBUGGING
      /* FIXME: Which of the following is correct? */
#if 0
      set_current_frame ( create_new_frame (read_register (FP_REGNUM),
					    read_pc ()));
#else
      set_current_frame ( create_new_frame (last_frame_address,
					    read_pc ()));
#endif

      select_frame (get_current_frame (), 0);
      validate_files ();
    }
  else if (from_tty)
    printf ("No core file now.\n");
}

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
#else /* not COFF_FORMAT */
      {
	struct stat st_exec;

#ifdef gould
#endif /* gould */
	val = myread (execchan, &exec_aouthdr, sizeof (AOUTHDR));

	if (val < 0)
	  perror_with_name (filename);

        text_start = N_TXTADDR (exec_aouthdr);
        exec_data_start = N_DATADDR (exec_aouthdr);
#ifdef gould
#else
	text_offset = N_TXTOFF (exec_aouthdr);
	exec_data_offset = N_TXTOFF (exec_aouthdr) + exec_aouthdr.a_text;
#endif
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

/*** Prettier register printing. ***/

/* Print registers in the same format as pyramid's dbx, adb, sdb.  */
pyr_print_registers(reg_buf, regnum)
    long *reg_buf[];
{
  register int regno;
  int usp, ksp;
  struct user u;

  for (regno = 0; regno < 16; regno++) {
    printf/*_filtered*/ ("%6.6s: %8x  %6.6s: %8x  %6s: %8x  %6s: %8x\n",
		     reg_names[regno], reg_buf[regno],
		     reg_names[regno+16], reg_buf[regno+16],
		     reg_names[regno+32], reg_buf[regno+32],
		     reg_names[regno+48], reg_buf[regno+48]);
  }
  usp = ptrace (3, inferior_pid,
		      ((char *)&u.u_pcb.pcb_usp) -
		      ((char *)&u), 0);
  ksp = ptrace (3, inferior_pid,
		      ((char *)&u.u_pcb.pcb_ksp) -
		      ((char *)&u), 0);
  printf/*_filtered*/ ("\n%6.6s: %8x  %6.6s: %8x (%08x) %6.6s %8x\n",
		   reg_names[CSP_REGNUM],reg_buf[CSP_REGNUM],
		   reg_names[KSP_REGNUM], reg_buf[KSP_REGNUM], ksp,
		   "usp", usp);
}

/* Print the register regnum, or all registers if regnum is -1. */

pyr_do_registers_info (regnum)
    int regnum;
{
  /* On a pyr, we know a virtual register can always fit in an long.
     Here (and elsewhere) we take advantage of that.  Yuk.  */
  long raw_regs[MAX_REGISTER_RAW_SIZE*NUM_REGS];
  register int i;
  
  for (i = 0 ; i < 64 ; i++) {
    read_relative_register_raw_bytes(i, raw_regs+i);
  }
  if (regnum == -1)
    pyr_print_registers (raw_regs, regnum);
  else
    for (i = 0; i < NUM_REGS; i++)
      if (i == regnum) {
	long val = raw_regs[i];
	
	fputs_filtered (reg_names[i], stdout);
	printf_filtered(":");
	print_spaces_filtered (6 - strlen (reg_names[i]), stdout);
	if (val == 0)
	  printf_filtered ("0");
	else
	  printf_filtered ("0x%08x  %d", val, val);
	printf_filtered("\n");
      }
}

/*** Debugging editions of various macros from m-pyr.h ****/

CORE_ADDR frame_locals_address (frame)
    FRAME frame;
{
  register int addr = find_saved_register (frame,CFP_REGNUM);
  register int result = read_memory_integer (addr, 4);
#ifdef PYRAMID_CONTROL_FRAME_DEBUGGING
  fprintf (stderr,
	   "\t[[..frame_locals:%8x, %s= %x @%x fcfp= %x foo= %x\n\t gr13=%x pr13=%x tr13=%x @%x]]\n",
	   frame->frame,
	   reg_names[CFP_REGNUM],
	   result, addr,
	   frame->frame_cfp, (CFP_REGNUM),


	   read_register(13), read_register(29), read_register(61),
	   find_saved_register(frame, 61));
#endif /* PYRAMID_CONTROL_FRAME_DEBUGGING */

  /* FIXME: I thought read_register (CFP_REGNUM) should be the right answer;
     or at least CFP_REGNUM relative to FRAME (ie, result).
     There seems to be a bug in the way the innermost frame is set up.  */

    return ((frame->next) ? result: frame->frame_cfp);
}

CORE_ADDR frame_args_addr (frame)
    FRAME frame;
{
  register int addr = find_saved_register (frame,CFP_REGNUM);
  register int result = read_memory_integer (addr, 4);

#ifdef PYRAMID_CONTROL_FRAME_DEBUGGING
  fprintf (stderr,
	   "\t[[..frame_args:%8x, %s= %x @%x fcfp= %x r_r= %x\n\t gr13=%x pr13=%x tr13=%x @%x]]\n",
	   frame->frame,
	   reg_names[CFP_REGNUM],
	   result, addr,
	   frame->frame_cfp, read_register(CFP_REGNUM),

	   read_register(13), read_register(29), read_register(61),
	   find_saved_register(frame, 61));
#endif /*  PYRAMID_CONTROL_FRAME_DEBUGGING */

  /* FIXME: I thought read_register (CFP_REGNUM) should be the right answer;
     or at least CFP_REGNUM relative to FRAME (ie, result).
     There seems to be a bug in the way the innermost frame is set up.  */
    return ((frame->next) ? result: frame->frame_cfp);
}
