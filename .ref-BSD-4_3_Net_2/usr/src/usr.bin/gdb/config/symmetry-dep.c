/* Low level interface to ptrace, for GDB when running under Unix.
   Copyright (C) 1986, 1987, 1989 Free Software Foundation, Inc.

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

/* many 387-specific items of use taken from i386-dep.c */

#include <stdio.h>
#include "defs.h"
#include "param.h"
#include "frame.h"
#include "inferior.h"
#include "symtab.h"

#include <signal.h>
#include <sys/param.h>
#include <sys/user.h>
#include <sys/dir.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <a.out.h>
#include <fcntl.h>

static long i386_get_frame_setup ();
static i386_follow_jump ();

/* XPT_DEBUG doesn't work yet under Dynix 3.0.12, but UNDEBUG does... */
#define PTRACE_ATTACH XPT_DEBUG
#define PTRACE_DETACH XPT_UNDEBUG

#include <sgtty.h>
#define TERMINAL struct sgttyb

extern int errno;

/* Nonzero if we are debugging an attached outside process
   rather than an inferior.  */

static int attach_flag;

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

#ifdef ATTACH_DETACH

/* Start debugging the process whose number is PID.  */

attach (pid)
     int pid;
{
  errno = 0;
  ptrace (PTRACE_ATTACH, pid, 0, 0);
  if (errno)
    perror_with_name ("ptrace");
  attach_flag = 1;
  return pid;
}

/* Stop debugging the process whose number is PID
   and continue it with signal number SIGNAL.
   SIGNAL = 0 means just continue it.  */

void
detach (signal)
     int signal;
{
  errno = 0;
  ptrace (PTRACE_DETACH, inferior_pid, 1, signal);
  if (errno)
    perror_with_name ("ptrace");
  attach_flag = 0;
}
#endif /* ATTACH_DETACH */


store_inferior_registers(regno)
int regno;
{
  struct pt_regset regs;
  int reg_tmp, i;
  extern char registers[];
  
  if (-1 == regno)
    {
      regs.pr_eax = *(int *)&registers[REGISTER_BYTE(0)];
      regs.pr_ebx = *(int *)&registers[REGISTER_BYTE(5)];
      regs.pr_ecx = *(int *)&registers[REGISTER_BYTE(2)];
      regs.pr_edx = *(int *)&registers[REGISTER_BYTE(1)];
      regs.pr_esi = *(int *)&registers[REGISTER_BYTE(6)];
      regs.pr_edi = *(int *)&registers[REGISTER_BYTE(7)];
      regs.pr_esp = *(int *)&registers[REGISTER_BYTE(14)];
      regs.pr_ebp = *(int *)&registers[REGISTER_BYTE(15)];
      regs.pr_eip = *(int *)&registers[REGISTER_BYTE(16)];
      regs.pr_flags = *(int *)&registers[REGISTER_BYTE(17)];
      for (i = 0; i < 31; i++) {
	regs.pr_fpa.fpa_regs[i] =
	  *(int *)&registers[REGISTER_BYTE(FP1_REGNUM+i)];
      }
    }
  else
    {
      reg_tmp = *(int *)&registers[REGISTER_BYTE(regno)];
      ptrace(XPT_RREGS, inferior_pid, &regs, 0);
      switch (regno)
	{
	case 0:
	  regs.pr_eax = *(int *)&registers[REGISTER_BYTE(0)];
	  break;
	case 5:
	  regs.pr_ebx = *(int *)&registers[REGISTER_BYTE(5)];
	  break;
	case 2:
	  regs.pr_ecx = *(int *)&registers[REGISTER_BYTE(2)];
	  break;
	case 1:
	  regs.pr_edx = *(int *)&registers[REGISTER_BYTE(1)];
	  break;
	case 6:
	  regs.pr_esi = *(int *)&registers[REGISTER_BYTE(6)];
	  break;
	case 7:
	  regs.pr_edi = *(int *)&registers[REGISTER_BYTE(7)];
	  break;
	case 15:
	  regs.pr_ebp = *(int *)&registers[REGISTER_BYTE(15)];
	  break;
	case 14:
	  regs.pr_esp = *(int *)&registers[REGISTER_BYTE(14)];
	  break;
	case 16:
	  regs.pr_eip = *(int *)&registers[REGISTER_BYTE(16)];
	  break;
	case 17:
	  regs.pr_flags = *(int *)&registers[REGISTER_BYTE(17)];
	  break;
	}
    }
  ptrace(XPT_WREGS, inferior_pid, &regs, 0);
}

void
fetch_inferior_registers()
{
    int i;
    struct pt_regset regs;
    extern char registers[];

    ptrace(XPT_RREGS, inferior_pid, &regs, 0);
    *(int *)&registers[REGISTER_BYTE(0)] = regs.pr_eax;
    *(int *)&registers[REGISTER_BYTE(5)] = regs.pr_ebx;
    *(int *)&registers[REGISTER_BYTE(2)] = regs.pr_ecx;
    *(int *)&registers[REGISTER_BYTE(1)] = regs.pr_edx;
    *(int *)&registers[REGISTER_BYTE(6)] = regs.pr_esi;
    *(int *)&registers[REGISTER_BYTE(7)] = regs.pr_edi;
    *(int *)&registers[REGISTER_BYTE(15)] = regs.pr_ebp;
    *(int *)&registers[REGISTER_BYTE(14)] = regs.pr_esp;
    *(int *)&registers[REGISTER_BYTE(16)] = regs.pr_eip;
    *(int *)&registers[REGISTER_BYTE(17)] = regs.pr_flags;
    for (i = 0; i < FPA_NREGS; i++) {
	*(int *)&registers[REGISTER_BYTE(FP1_REGNUM+i)] = regs.pr_fpa.fpa_regs[i];
    }
    bcopy(regs.pr_fpu.fpu_stack[0], &registers[REGISTER_BYTE(3)], 10);
    bcopy(regs.pr_fpu.fpu_stack[1], &registers[REGISTER_BYTE(4)], 10);
    bcopy(regs.pr_fpu.fpu_stack[2], &registers[REGISTER_BYTE(8)], 10);
    bcopy(regs.pr_fpu.fpu_stack[3], &registers[REGISTER_BYTE(9)], 10);
    bcopy(regs.pr_fpu.fpu_stack[4], &registers[REGISTER_BYTE(10)], 10);
    bcopy(regs.pr_fpu.fpu_stack[5], &registers[REGISTER_BYTE(11)], 10);
    bcopy(regs.pr_fpu.fpu_stack[6], &registers[REGISTER_BYTE(12)], 10);
    bcopy(regs.pr_fpu.fpu_stack[7], &registers[REGISTER_BYTE(13)], 10);
}


/* Copy LEN bytes from inferior's memory starting at MEMADDR
   to debugger memory starting at MYADDR.  */

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

  /* Read all the longwords */
  for (i = 0; i < count; i++, addr += sizeof (int)) {
      errno = 0;
      if (remote_debugging)
	  buffer[i] = remote_fetch_word (addr);
      else
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


/* Recognize COFF format systems because a.out.h defines AOUTHDR.  */
#ifdef AOUTHDR
#define COFF_FORMAT
#endif


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
#define AOUTHDR struct exec
#endif

extern char *sys_siglist[];

/* Hook for `exec_file_command' command to call.  */

void (*exec_file_display_hook) ();
   
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

int exec_mtime;

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
unsigned int register_addr ();

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
	int reg_offset;

	val = myread (corechan, &u, sizeof u);
	if (val < 0)
	  perror_with_name (filename);
	data_start = exec_data_start;

	data_end = data_start + NBPG * (u.u_dsize - u.u_tsize);
	stack_start = stack_end - NBPG * u.u_ssize;
	data_offset = NBPG * UPAGES;
	stack_offset = ctob(UPAGES + u.u_dsize - u.u_tsize);
	reg_offset = (int) u.u_ar0 - KERNEL_U_ADDR;
printf("u.u_tsize= %#x, u.u_dsize= %#x, u.u_ssize= %#x, stack_off= %#x\n",
       u.u_tsize, u.u_dsize, u.u_ssize, stack_offset);

	core_aouthdr.a_magic = 0;

	/* Read the register values out of the core file and store
	   them where `read_register' will find them.  */

	{
	  register int regno;

	  for (regno = 0; regno < NUM_REGS; regno++)
	    {
	      char buf[MAX_REGISTER_RAW_SIZE];

	      val = lseek (corechan, register_addr (regno, reg_offset), 0);
	      if (val < 0)
		perror_with_name (filename);

 	      val = myread (corechan, buf, sizeof buf);
	      if (val < 0)
		perror_with_name (filename);
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

      set_current_frame(create_new_frame(read_register(FP_REGNUM),
					 read_pc()));
/*      set_current_frame (read_register (FP_REGNUM));*/
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
      {
	int aout_hdrsize;
	int num_sections;

	if (read_file_hdr (execchan, &file_hdr) < 0)
	  error ("\"%s\": not in executable format.", execfile);

	aout_hdrsize = file_hdr.f_opthdr;
	num_sections = file_hdr.f_nscns;

	if (read_aout_hdr (execchan, &exec_aouthdr, aout_hdrsize) < 0)
	  error ("\"%s\": can't read optional aouthdr", execfile);

	if (read_section_hdr (execchan, _TEXT, &text_hdr, num_sections,
			      aout_hdrsize) < 0)
	  error ("\"%s\": can't read text section header", execfile);

	if (read_section_hdr (execchan, _DATA, &data_hdr, num_sections,
			      aout_hdrsize) < 0)
	  error ("\"%s\": can't read data section header", execfile);

	text_start = exec_aouthdr.text_start;
	text_end = text_start + exec_aouthdr.tsize;
	text_offset = text_hdr.s_scnptr;
	exec_data_start = exec_aouthdr.data_start;
	exec_data_end = exec_data_start + exec_aouthdr.dsize;
	exec_data_offset = data_hdr.s_scnptr;
	data_start = exec_data_start;
	data_end += exec_data_start;
	exec_mtime = file_hdr.f_timdat;
      }
#else /* not COFF_FORMAT */
      {
	struct stat st_exec;

	val = myread (execchan, &exec_aouthdr, sizeof (AOUTHDR));

	if (val < 0)
	  perror_with_name (filename);

	text_start = N_ADDRADJ(exec_aouthdr);
        exec_data_start = round(exec_aouthdr.a_text, NBPG*CLSIZE);
	text_offset = N_TXTOFF (exec_aouthdr);
	exec_data_offset = N_TXTOFF (exec_aouthdr) + exec_aouthdr.a_text;
	text_end = exec_aouthdr.a_text;
        exec_data_end = exec_data_start + exec_aouthdr.a_data;
	data_start = exec_data_start;
	data_end = data_start + exec_aouthdr.a_data;
	exec_data_offset = N_TXTOFF(exec_aouthdr);
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

/* rounds 'one' up to divide evenly by 'two' */

int
round(one,two)
register int one, two;

{
    register int temp;
    temp = (one/two)*two;
    if (one != temp) {
	temp += two;
    }
    return temp;
}


static CORE_ADDR codestream_next_addr;
static CORE_ADDR codestream_addr;
static unsigned char codestream_buf[sizeof (int)];
static int codestream_off;
static int codestream_cnt;

#define codestream_tell() (codestream_addr + codestream_off)
#define codestream_peek() (codestream_cnt == 0 ? \
			   codestream_fill(1): codestream_buf[codestream_off])
#define codestream_get() (codestream_cnt-- == 0 ? \
			 codestream_fill(0) : codestream_buf[codestream_off++])


static unsigned char 
codestream_fill (peek_flag)
{
  codestream_addr = codestream_next_addr;
  codestream_next_addr += sizeof (int);
  codestream_off = 0;
  codestream_cnt = sizeof (int);
  read_memory (codestream_addr,
	       (unsigned char *)codestream_buf,
	       sizeof (int));
  
  if (peek_flag)
    return (codestream_peek());
  else
    return (codestream_get());
}

static void
codestream_seek (place)
{
  codestream_next_addr = place & -sizeof (int);
  codestream_cnt = 0;
  codestream_fill (1);
  while (codestream_tell() != place)
    codestream_get ();
}

static void
codestream_read (buf, count)
     unsigned char *buf;
{
  unsigned char *p;
  int i;
  p = buf;
  for (i = 0; i < count; i++)
    *p++ = codestream_get ();
}

/*
 * Following macro translates i386 opcode register numbers to Symmetry
 * register numbers.  This is used by FRAME_FIND_SAVED_REGS.
 *
 *           %eax  %ecx  %edx  %ebx  %esp  %ebp  %esi  %edi
 * i386        0     1     2     3     4     5     6     7
 * Symmetry    0     2     1     5    14    15     6     7
 *
 */
#define I386_REGNO_TO_SYMMETRY(n) \
((n)==0?0 :(n)==1?2 :(n)==2?1 :(n)==3?5 :(n)==4?14 :(n)==5?15 :(n))

/* from i386-dep.c */
i386_frame_find_saved_regs (fip, fsrp)
     struct frame_info *fip;
     struct frame_saved_regs *fsrp;
{
  unsigned long locals;
  unsigned char *p;
  unsigned char op;
  CORE_ADDR dummy_bottom;
  CORE_ADDR adr;
  int i;
  
  bzero (fsrp, sizeof *fsrp);
  
  /* if frame is the end of a dummy, compute where the
   * beginning would be
   */
  dummy_bottom = fip->frame - 4 - NUM_REGS*4 - CALL_DUMMY_LENGTH;
  
  /* check if the PC is in the stack, in a dummy frame */
  if (dummy_bottom <= fip->pc && fip->pc <= fip->frame) 
    {
      /* all regs were saved by push_call_dummy () */
      adr = fip->frame - 4;
      for (i = 0; i < NUM_REGS; i++) 
	{
	  fsrp->regs[i] = adr;
	  adr -= 4;
	}
      return;
    }
  
  locals = i386_get_frame_setup (get_pc_function_start (fip->pc));
  
  if (locals >= 0) 
    {
      adr = fip->frame - 4 - locals;
      for (i = 0; i < 8; i++) 
	{
	  op = codestream_get ();
	  if (op < 0x50 || op > 0x57)
	    break;
	  fsrp->regs[I386_REGNO_TO_SYMMETRY(op - 0x50)] = adr;
	  adr -= 4;
	}
    }
  
  fsrp->regs[PC_REGNUM] = fip->frame + 4;
  fsrp->regs[FP_REGNUM] = fip->frame;
}

/* from i386-dep.c */
static
print_387_control_word (control)
unsigned short control;
{
  printf ("control 0x%04x: ", control);
  printf ("compute to ");
  switch ((control >> 8) & 3) 
    {
    case 0: printf ("24 bits; "); break;
    case 1: printf ("(bad); "); break;
    case 2: printf ("53 bits; "); break;
    case 3: printf ("64 bits; "); break;
    }
  printf ("round ");
  switch ((control >> 10) & 3) 
    {
    case 0: printf ("NEAREST; "); break;
    case 1: printf ("DOWN; "); break;
    case 2: printf ("UP; "); break;
    case 3: printf ("CHOP; "); break;
    }
  if (control & 0x3f) 
    {
      printf ("mask:");
      if (control & 0x0001) printf (" INVALID");
      if (control & 0x0002) printf (" DENORM");
      if (control & 0x0004) printf (" DIVZ");
      if (control & 0x0008) printf (" OVERF");
      if (control & 0x0010) printf (" UNDERF");
      if (control & 0x0020) printf (" LOS");
      printf (";");
    }
  printf ("\n");
  if (control & 0xe080) printf ("warning: reserved bits on 0x%x\n",
				control & 0xe080);
}

static
print_387_status_word (status)
     unsigned short status;
{
  printf ("status %#04x: ", status);
  if (status & 0xff) {
      printf ("exceptions:");	/* exception names match <machine/fpu.h> */
      if (status & 0x0001) printf (" FLTINV");
      if (status & 0x0002) printf (" FLTDEN");
      if (status & 0x0004) printf (" FLTDIV");
      if (status & 0x0008) printf (" FLTOVF");
      if (status & 0x0010) printf (" FLTUND");
      if (status & 0x0020) printf (" FLTPRE");
      if (status & 0x0040) printf (" FLTSTK");
      printf ("; ");
    }
  printf ("flags: %d%d%d%d; ",
	  (status & 0x4000) != 0,
	  (status & 0x0400) != 0,
	  (status & 0x0200) != 0,
	  (status & 0x0100) != 0);
  
  printf ("top %d\n", (status >> 11) & 7);
}

static
print_fpu_status(ep)
struct pt_regset ep;

{
    int i;
    int bothstatus;
    int top;
    int fpreg;
    unsigned char *p;
    
    printf("80387:");
    if (ep.pr_fpu.fpu_ip == 0) {
	printf(" not in use.\n");
	return;
    } else {
	printf("\n");
    }
    if (ep.pr_fpu.fpu_status != 0) {
	print_387_status_word (ep.pr_fpu.fpu_status);
    }
    print_387_control_word (ep.pr_fpu.fpu_control);
    printf ("last exception: ");
    printf ("opcode 0x%x; ", ep.pr_fpu.fpu_rsvd4);
    printf ("pc 0x%x:0x%x; ", ep.pr_fpu.fpu_cs, ep.pr_fpu.fpu_ip);
    printf ("operand 0x%x:0x%x\n", ep.pr_fpu.fpu_data_offset, ep.pr_fpu.fpu_op_sel);
    
    top = (ep.pr_fpu.fpu_status >> 11) & 7;
    
    printf ("regno  tag  msb              lsb  value\n");
    for (fpreg = 7; fpreg >= 0; fpreg--) 
	{
	    double val;
	    
	    printf ("%s %d: ", fpreg == top ? "=>" : "  ", fpreg);
	    
	    switch ((ep.pr_fpu.fpu_tag >> (fpreg * 2)) & 3) 
		{
		case 0: printf ("valid "); break;
		case 1: printf ("zero  "); break;
		case 2: printf ("trap  "); break;
		case 3: printf ("empty "); break;
		}
	    for (i = 9; i >= 0; i--)
		printf ("%02x", ep.pr_fpu.fpu_stack[fpreg][i]);
	    
	    i387_to_double (ep.pr_fpu.fpu_stack[fpreg], (char *)&val);
	    printf ("  %g\n", val);
	}
    if (ep.pr_fpu.fpu_rsvd1)
	printf ("warning: rsvd1 is 0x%x\n", ep.pr_fpu.fpu_rsvd1);
    if (ep.pr_fpu.fpu_rsvd2)
	printf ("warning: rsvd2 is 0x%x\n", ep.pr_fpu.fpu_rsvd2);
    if (ep.pr_fpu.fpu_rsvd3)
	printf ("warning: rsvd3 is 0x%x\n", ep.pr_fpu.fpu_rsvd3);
    if (ep.pr_fpu.fpu_rsvd5)
	printf ("warning: rsvd5 is 0x%x\n", ep.pr_fpu.fpu_rsvd5);
}


print_1167_control_word(pcr)
unsigned int pcr;

{
    int pcr_tmp;

    pcr_tmp = pcr & FPA_PCR_MODE;
    printf("\tMODE= %#x; RND= %#x ", pcr_tmp, pcr_tmp & 12);
    switch (pcr_tmp & 12) {
    case 0:
	printf("RN (Nearest Value)");
	break;
    case 1:
	printf("RZ (Zero)");
	break;
    case 2:
	printf("RP (Positive Infinity)");
	break;
    case 3:
	printf("RM (Negative Infinity)");
	break;
    }
    printf("; IRND= %d ", pcr_tmp & 2);
    if (0 == pcr_tmp & 2) {
	printf("(same as RND)\n");
    } else {
	printf("(toward zero)\n");
    }
    pcr_tmp = pcr & FPA_PCR_EM;
    printf("\tEM= %#x", pcr_tmp);
    if (pcr_tmp & FPA_PCR_EM_DM) printf(" DM");
    if (pcr_tmp & FPA_PCR_EM_UOM) printf(" UOM");
    if (pcr_tmp & FPA_PCR_EM_PM) printf(" PM");
    if (pcr_tmp & FPA_PCR_EM_UM) printf(" UM");
    if (pcr_tmp & FPA_PCR_EM_OM) printf(" OM");
    if (pcr_tmp & FPA_PCR_EM_ZM) printf(" ZM");
    if (pcr_tmp & FPA_PCR_EM_IM) printf(" IM");
    printf("\n");
    pcr_tmp = FPA_PCR_CC;
    printf("\tCC= %#x", pcr_tmp);
    if (pcr_tmp & FPA_PCR_20MHZ) printf(" 20MHZ");
    if (pcr_tmp & FPA_PCR_CC_Z) printf(" Z");
    if (pcr_tmp & FPA_PCR_CC_C2) printf(" C2");
    if (pcr_tmp & FPA_PCR_CC_C1) printf(" C1");
    switch (pcr_tmp) {
    case FPA_PCR_CC_Z:
	printf(" (Equal)");
	break;
    case FPA_PCR_CC_C1:
	printf(" (Less than)");
	break;
    case 0:
	printf(" (Greater than)");
	break;
    case FPA_PCR_CC_Z | FPA_PCR_CC_C1 | FPA_PCR_CC_C2:
	printf(" (Unordered)");
	break;
    default:
	printf(" (Undefined)");
	break;
    }
    printf("\n");
    pcr_tmp = pcr & FPA_PCR_AE;
    printf("\tAE= %#x", pcr_tmp);
    if (pcr_tmp & FPA_PCR_AE_DE) printf(" DE");
    if (pcr_tmp & FPA_PCR_AE_UOE) printf(" UOE");
    if (pcr_tmp & FPA_PCR_AE_PE) printf(" PE");
    if (pcr_tmp & FPA_PCR_AE_UE) printf(" UE");
    if (pcr_tmp & FPA_PCR_AE_OE) printf(" OE");
    if (pcr_tmp & FPA_PCR_AE_ZE) printf(" ZE");
    if (pcr_tmp & FPA_PCR_AE_EE) printf(" EE");
    if (pcr_tmp & FPA_PCR_AE_IE) printf(" IE");
    printf("\n");
}

print_1167_regs(regs)
long regs[FPA_NREGS];

{
    int i;

    union {
	double	d;
	long	l[2];
    } xd;
    union {
	float	f;
	long	l;
    } xf;


    for (i = 0; i < FPA_NREGS; i++) {
	xf.l = regs[i];
	printf("%%fp%d: raw= %#x, single= %f", i+1, regs[i], xf.f);
	if (!(i & 1)) {
	    printf("\n");
	} else {
	    xd.l[1] = regs[i];
	    xd.l[0] = regs[i+1];
	    printf(", double= %f\n", xd.d);
	}
    }
}

print_fpa_status(ep)
struct pt_regset ep;

{

    printf("WTL 1167:");
    if (ep.pr_fpa.fpa_pcr !=0) {
	printf("\n");
	print_1167_control_word(ep.pr_fpa.fpa_pcr);
	print_1167_regs(ep.pr_fpa.fpa_regs);
    } else {
	printf(" not in use.\n");
    }
}

i386_float_info ()

{
    char ubuf[UPAGES*NBPG];
    struct pt_regset regset;
    extern int corechan;
    
    if (have_inferior_p()) {
	call_ptrace(XPT_RREGS, inferior_pid, &regset, 0);
    } else {
	if (lseek (corechan, 0, 0) < 0) {
	    perror ("seek on core file");
	}
	if (myread (corechan, ubuf, UPAGES*NBPG) < 0) {
	    perror ("read on core file");
	}
	/* only interested in the floating point registers */
	regset.pr_fpu = ((struct user *) ubuf)->u_fpusave;
	regset.pr_fpa = ((struct user *) ubuf)->u_fpasave;
    }
    print_fpu_status(regset);
    print_fpa_status(regset);
}

i387_to_double (from, to)
     char *from;
     char *to;
{
  long *lp;
  /* push extended mode on 387 stack, then pop in double mode
   *
   * first, set exception masks so no error is generated -
   * number will be rounded to inf or 0, if necessary 
   */
  asm ("pushl %eax"); 		/* grab a stack slot */
  asm ("fstcw (%esp)");		/* get 387 control word */
  asm ("movl (%esp),%eax");	/* save old value */
  asm ("orl $0x3f,%eax");		/* mask all exceptions */
  asm ("pushl %eax");
  asm ("fldcw (%esp)");		/* load new value into 387 */
  
  asm ("movl 8(%ebp),%eax");
  asm ("fldt (%eax)");		/* push extended number on 387 stack */
  asm ("fwait");
  asm ("movl 12(%ebp),%eax");
  asm ("fstpl (%eax)");		/* pop double */
  asm ("fwait");
  
  asm ("popl %eax");		/* flush modified control word */
  asm ("fnclex");			/* clear exceptions */
  asm ("fldcw (%esp)");		/* restore original control word */
  asm ("popl %eax");		/* flush saved copy */
}

double_to_i387 (from, to)
     char *from;
     char *to;
{
  /* push double mode on 387 stack, then pop in extended mode
   * no errors are possible because every 64-bit pattern
   * can be converted to an extended
   */
  asm ("movl 8(%ebp),%eax");
  asm ("fldl (%eax)");
  asm ("fwait");
  asm ("movl 12(%ebp),%eax");
  asm ("fstpt (%eax)");
  asm ("fwait");
}

static long
i386_get_frame_setup (pc)
{
  unsigned char op;
  
  codestream_seek (pc);
  
  i386_follow_jump ();
  
  op = codestream_get ();
  
  if (op == 0x58) /* popl %eax */
    {
      /*
       * this function must start with
       * 
       *    popl %eax		  0x58
       *    xchgl %eax, (%esp)  0x87 0x04 0x24
       * or xchgl %eax, 0(%esp) 0x87 0x44 0x24 0x00
       *
       * (the system 5 compiler puts out the second xchg
       * inst, and the assembler doesn't try to optimize it,
       * so the 'sib' form gets generated)
       * 
       * this sequence is used to get the address of the return
       * buffer for a function that returns a structure
       */
      int pos;
      unsigned char buf[4];
      static unsigned char proto1[3] = { 0x87,0x04,0x24 };
      static unsigned char proto2[4] = { 0x87,0x44,0x24,0x00 };
      pos = codestream_tell ();
      codestream_read (buf, 4);
      if (bcmp (buf, proto1, 3) == 0)
	pos += 3;
      else if (bcmp (buf, proto2, 4) == 0)
	pos += 4;
      
      codestream_seek (pos);
      op = codestream_get (); /* update next opcode */
    }
  
  if (op == 0x55) 			/* pushl %esp */
    {
      if (codestream_get () != 0x8b)	/* movl %esp, %ebp (2bytes) */
	return (-1);
      if (codestream_get () != 0xec)
	return (-1);
      /*
       * check for stack adjustment 
       *
       *  subl $XXX, %esp
       *
       * note: you can't subtract a 16 bit immediate
       * from a 32 bit reg, so we don't have to worry
       * about a data16 prefix 
       */
      op = codestream_peek ();
      if (op == 0x83)  /* subl with 8 bit immed */
	{
	  codestream_get ();
	  if (codestream_get () != 0xec)
	    return (-1);
	  /* subl with signed byte immediate 
	   * (though it wouldn't make sense to be negative)
	   */
	  return (codestream_get());
	}
      else if (op == 0x81)  /* subl with 32 bit immed */
	{
	  int locals;
	  if (codestream_get () != 0xec)
	    return (-1);
	  /* subl with 32 bit immediate */
	  codestream_read ((unsigned char *)&locals, 4);
	  return (locals);
	} 
      else 
	{
	  return (0);
	}
    } 
  else if (op == 0xc8) 
    {
      /* enter instruction: arg is 16 unsigned immed */
      unsigned short slocals;
      codestream_read ((unsigned char *)&slocals, 2);
      codestream_get (); /* flush final byte of enter instruction */
      return (slocals);
    }
  return (-1);
}

/* next instruction is a jump, move to target */
static
i386_follow_jump ()
{
  int long_delta;
  short short_delta;
  char byte_delta;
  int data16;
  int pos;
  
  pos = codestream_tell ();
  
  data16 = 0;
  if (codestream_peek () == 0x66)
    {
      codestream_get ();
      data16 = 1;
    }
  
  switch (codestream_get ())
    {
    case 0xe9:
      /* relative jump: if data16 == 0, disp32, else disp16 */
      if (data16)
	{
	  codestream_read ((unsigned char *)&short_delta, 2);
	  pos += short_delta + 3; /* include size of jmp inst */
	}
      else
	{
	  codestream_read ((unsigned char *)&long_delta, 4);
	  pos += long_delta + 5;
	}
      break;
    case 0xeb:
      /* relative jump, disp8 (ignore data16) */
      codestream_read ((unsigned char *)&byte_delta, 1);
      pos += byte_delta + 2;
      break;
    }
  codestream_seek (pos + data16);
}

/* return pc of first real instruction */
/* from i386-dep.c */

i386_skip_prologue (pc)
{
  unsigned char op;
  int i;
  
  if (i386_get_frame_setup (pc) < 0)
    return (pc);
  
  /* found valid frame setup - codestream now points to 
   * start of push instructions for saving registers
   */
  
  /* skip over register saves */
  for (i = 0; i < 8; i++)
    {
      op = codestream_peek ();
      /* break if not pushl inst */
      if (op < 0x50 || op > 0x57) 
	break;
      codestream_get ();
    }
  
  i386_follow_jump ();
  
  return (codestream_tell ());
}

symmetry_extract_return_value(type, regbuf, valbuf)
     struct type *type;
     char *regbuf;
     char *valbuf;
{
  union { 
    double	d; 
    int	l[2]; 
  } xd; 
  int i;
  float f;

  if (TYPE_CODE_FLT == TYPE_CODE(type)) { 
    for (i = 0; i < misc_function_count; i++) {
      if (!strcmp(misc_function_vector[i].name, "1167_flt"))
	break;
    }
    if (i < misc_function_count) {
      /* found "1167_flt" means 1167, %fp2-%fp3 */ 
      /* float & double; 19= %fp2, 20= %fp3 */
      /* no single precision on 1167 */
      xd.l[1] = *((int *)&regbuf[REGISTER_BYTE(19)]);
      xd.l[0] = *((int *)&regbuf[REGISTER_BYTE(20)]);
      switch (TYPE_LENGTH(type)) {
      case 4:
	f = (float) xd.d;
	bcopy(&f, valbuf, TYPE_LENGTH(type));
	break;
      case 8:
	bcopy(&xd.d, valbuf, TYPE_LENGTH(type)); 
	break;
      default:
	error("Unknown floating point size");
	break;
      }
    } else { 
      /* 387 %st(0), gcc uses this */ 
      i387_to_double(((int *)&regbuf[REGISTER_BYTE(3)]),
		     &xd.d); 
      switch (TYPE_LENGTH(type)) {
      case 4:			/* float */
	f = (float) xd.d;
	bcopy(&f, valbuf, 4); 
	break;
      case 8:			/* double */
	bcopy(&xd.d, valbuf, 8);
	break;
      default:
	error("Unknown floating point size");
	break;
      }
    }
  } else { 
    bcopy (regbuf, valbuf, TYPE_LENGTH (type)); 
  }
}
