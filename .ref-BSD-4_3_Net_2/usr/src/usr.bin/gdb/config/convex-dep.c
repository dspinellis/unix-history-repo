/* Convex stuff for GDB.
   Copyright (C) 1990 Free Software Foundation, Inc.

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
#include "command.h"
#include "symtab.h"
#include "value.h"
#include "frame.h"
#include "inferior.h"
#include "wait.h"

#include <signal.h>
#include <fcntl.h>
#include <a.out.h>

#include <sys/param.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/ioctl.h>
#include <sys/pcntl.h>
#include <sys/thread.h>
#include <sys/proc.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/mman.h>

#include <convex/vmparam.h>
#include <convex/filehdr.h>
#include <convex/opthdr.h>
#include <convex/scnhdr.h>
#include <convex/core.h>

/* Per-thread data, read from the inferior at each stop and written
   back at each resume.  */

/* Number of active threads.
   Tables are valid for thread numbers less than this.  */

static int n_threads;

#define MAXTHREADS 8
		
/* Thread state.  The remaining data is valid only if this is PI_TALIVE.  */

static int thread_state[MAXTHREADS];

/* Stop pc, signal, signal subcode */

static int thread_pc[MAXTHREADS];
static int thread_signal[MAXTHREADS];
static int thread_sigcode[MAXTHREADS];	

/* Thread registers.
   If thread is selected, the regs are in registers[] instead.  */

static char thread_regs[MAXTHREADS][REGISTER_BYTES];

/* 1 if the top frame on the thread's stack was a context frame,
   meaning that the kernel is up to something and we should not
   touch the thread at all except to resume it.  */

static char thread_is_in_kernel[MAXTHREADS];

/* The currently selected thread's number.  */

static int inferior_thread;

/* Inferior process's file handle and a process control block
   to feed args to ioctl with.  */

static int inferior_fd;
static struct pcntl ps;

/* SOFF file headers for exec or core file.  */

static FILEHDR filehdr;
static OPTHDR opthdr;
static SCNHDR scnhdr;

/* Address maps constructed from section headers of exec and core files.
   Defines process address -> file address translation.  */

struct pmap 
{
    long mem_addr;		/* process start address */
    long mem_end;		/* process end+1 address */
    long file_addr;		/* file start address */
    long thread;		/* -1 shared; 0,1,... thread-local */
    long type;			/* S_TEXT S_DATA S_BSS S_TBSS etc */
    long which;			/* used to sort map for info files */
};

static int n_exec, n_core;
static struct pmap exec_map[100];
static struct pmap core_map[100];

/* Offsets in the core file of core_context and core_tcontext blocks.  */

static int context_offset;
static int tcontext_offset[MAXTHREADS];

/* Core file control blocks.  */

static struct core_context_v70 c;
static struct core_tcontext_v70 tc;
static struct user u;
static thread_t th;
static proc_t pr;

/* The registers of the currently selected thread.  */

extern char registers[REGISTER_BYTES];

/* Vector and communication registers from core dump or from inferior.
   These are read on demand, ie, not normally valid.  */

static struct vecst vector_registers;
static struct creg_ctx comm_registers;

/* Flag, set on a vanilla CONT command and cleared when the inferior
   is continued.  */

static int all_continue;

/* Flag, set when the inferior is continued by a vanilla CONT command,
   cleared if it is continued for any other purpose.  */

static int thread_switch_ok;

/* Stack of signals recieved from threads but not yet delivered to gdb.  */

struct threadpid 
{
    int pid;
    int thread;
    int signo;
    int subsig;
    int pc;
};

static struct threadpid signal_stack_bot[100];
static struct threadpid *signal_stack = signal_stack_bot;

/* How to detect empty stack -- bottom frame is all zero.  */

#define signal_stack_is_empty() (signal_stack->pid == 0)

/* Mode controlled by SET PIPE command, controls the psw SEQ bit
   which forces each instruction to complete before the next one starts.  */

static int sequential = 0;

/* Mode controlled by the SET PARALLEL command.  Values are:
   0  concurrency limit 1 thread, dynamic scheduling
   1  no concurrency limit, dynamic scheduling
   2  no concurrency limit, fixed scheduling  */

static int parallel = 1;

/* Mode controlled by SET BASE command, output radix for unformatted
   integer typeout, as in argument lists, aggregates, and so on.
   Zero means guess whether it's an address (hex) or not (decimal).  */

static int output_radix = 0;

/* Signal subcode at last thread stop.  */

static int stop_sigcode;

/* Hack, see wait() below.  */

static int exec_trap_timer;

/* Chain containing all defined commands.  */

extern struct cmd_list_element *cmdlist;

/* Chain containing all defined set subcommands */

extern struct cmd_list_element *setlist;

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

/* Virtual addresses of bounds of the two areas of memory in the core file.
   NB: These variables are set to plausible but useless values on convex.  */

extern CORE_ADDR data_start;
extern CORE_ADDR data_end;
extern CORE_ADDR stack_start;
extern CORE_ADDR stack_end;

/* Virtual addresses of bounds of two areas of memory in the exec file.
   NB: Only text_start and text_end have meaningful values on convex.  */

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

/* a.out header saved in core file.  */
  
extern struct exec core_aouthdr;

/* a.out header of exec file.  */

extern struct exec exec_aouthdr;

/* Routine to check for exec-core mismatch.  */

extern void validate_files ();

/* Nonzero if we are debugging an attached outside process
   rather than an inferior.  */

extern int attach_flag;



static struct type *vector_type ();
static long *read_vector_register ();
static long *read_vector_register_1 ();
static void write_vector_register ();
static REGISTER_TYPE read_comm_register ();
static void write_comm_register ();
static void convex_cont_command ();
static void thread_continue ();
static void select_thread ();
static void scan_stack ();
static void set_fixed_scheduling ();
static char *subsig_name ();
static void psw_info ();
static sig_noop ();
static ptr_cmp ();

extern char *sys_siglist[];
extern int errno;

/* Execute ptrace.  Convex V7 replaced ptrace with pattach.
   Allow ptrace (0) as a no-op.  */

int
call_ptrace (request, pid, procaddr, buf)
     int request, pid, procaddr, buf;
{
  if (request == 0)
    return;
  error ("no ptrace");
}

/* Replacement for system execle routine.
   Convert it to an equivalent exect, which pattach insists on.  */

execle (name, argv)
     char *name, *argv;
{
  char ***envp = (char ***) &argv;
  while (*envp++) ;

  signal (SIGTRAP, sig_noop);
  exect (name, &argv, *envp);
}

/* Stupid handler for stupid trace trap that otherwise causes
   startup to stupidly hang.  */

static sig_noop () 
{}

/* Read registers from inferior into registers[] array.
   For convex, they are already there, read in when the inferior stops.  */

void
fetch_inferior_registers ()
{
}

/* Store our register values back into the inferior.
   For Convex, do this only once, right before resuming inferior.  */

store_inferior_registers (regno)
     int regno;
{
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
  errno = 0;
  while (len > 0)
    {
      /* little-known undocumented max request size */
      int i = (len < 12288) ? len : 12288;

      lseek (inferior_fd, memaddr, 0);
      read (inferior_fd, myaddr, i);

      memaddr += i;
      myaddr += i;
      len -= i;
    }
  if (errno) 
    bzero (myaddr, len);
  return errno;
}

/* Copy LEN bytes of data from debugger memory at MYADDR
   to inferior's memory at MEMADDR.
   Returns errno on failure (cannot write the inferior) */

int
write_inferior_memory (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  errno = 0;
  lseek (inferior_fd, memaddr, 0);
  write (inferior_fd, myaddr, len);
  return errno;
}

/* Here from create_inferior when the inferior process has been created
   and started up.  We must do a pattach to grab it for debugging.

   Also, intercept the CONT command by altering its dispatch address.  */

create_inferior_hook (pid)
    int pid;
{
  static char cont[] = "cont";
  static char cont1[] = "c";
  char *linep = cont;
  char *linep1 = cont1;
  char **line = &linep;
  char **line1 = &linep1;
  struct cmd_list_element *c;

  c = lookup_cmd (line, cmdlist, "", 0);
  c->function = convex_cont_command;
  c = lookup_cmd (line1, cmdlist, "", 0);
  c->function = convex_cont_command;

  inferior_fd = pattach (pid, O_EXCL);
  if (inferior_fd < 0)
    perror_with_name ("pattach");
  inferior_thread = 0;
  set_fixed_scheduling (pid, parallel == 2);
}

/* Attach process PID for debugging.  */

attach (pid)
    int pid;
{
  int fd = pattach (pid, O_EXCL);
  if (fd < 0)
    perror_with_name ("pattach");
  attach_flag = 1;
  /* wait for strange kernel reverberations to go away */
  sleep (1);

  setpgrp (pid, pid);

  inferior_fd = fd;
  inferior_thread = 0;
  return pid;
}

/* Stop debugging the process whose number is PID
   and continue it with signal number SIGNAL.
   SIGNAL = 0 means just continue it.  */

void
detach (signal)
     int signal;
{
  signal_stack = signal_stack_bot;
  thread_continue (-1, 0, signal);
  ioctl (inferior_fd, PIXDETACH, &ps);
  close (inferior_fd);
  inferior_fd = 0;
  attach_flag = 0;
}

/* Kill off the inferior process.  */

kill_inferior ()
{
  if (remote_debugging)
    return;
  if (inferior_pid == 0)
    return;
  ioctl (inferior_fd, PIXTERMINATE, 0);
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
  ioctl (inferior_fd, PIXTERMINATE, 0);
  wait (0);
}

/* Read vector register REG, and return a pointer to the value.  */

static long *
read_vector_register (reg)
    int reg;
{
  if (have_inferior_p ())
    {
      errno = 0;
      ps.pi_buffer = (char *) &vector_registers;
      ps.pi_nbytes = sizeof vector_registers;
      ps.pi_offset = 0;
      ps.pi_thread = inferior_thread;
      ioctl (inferior_fd, PIXRDVREGS, &ps);
      if (errno)
	bzero (&vector_registers, sizeof vector_registers);
    }
  else if (corechan >= 0)
    {
      lseek (corechan, tcontext_offset[inferior_thread], 0);
      if (myread (corechan, &tc, sizeof tc) < 0)
	perror_with_name (corefile);
      lseek (corechan, tc.core_thread_p, 0);
      if (myread (corechan, &th, sizeof th) < 0)
	perror_with_name (corefile);
      lseek (corechan, tc.core_vregs_p, 0);
      if (myread (corechan, &vector_registers, 16*128) < 0)
	perror_with_name (corefile);
      vector_registers.vm[0] = th.t_vect_ctx.vc_vm[0];
      vector_registers.vm[1] = th.t_vect_ctx.vc_vm[1];
      vector_registers.vls = th.t_vect_ctx.vc_vls;
    }

  return read_vector_register_1 (reg);
}

/* Return a pointer to vector register REG, which must already have been
   fetched from the inferior or core file.  */

static long *
read_vector_register_1 (reg) 
    int reg;
{
  switch (reg)
    {
    case VM_REGNUM:
      return (long *) vector_registers.vm;
    case VS_REGNUM:
      return (long *) &vector_registers.vls;
    case VL_REGNUM:
      return 1 + (long *) &vector_registers.vls;
    default:
      return (long *) &vector_registers.vr[reg];
    }
}

/* Write vector register REG, element ELEMENT, new value VAL.
   NB: must use read-modify-write on the entire vector state,
   since pattach does not do offsetted writes correctly.  */

static void
write_vector_register (reg, element, val)
    int reg, element;
    REGISTER_TYPE val;
{
  if (have_inferior_p ())
    {
      errno = 0;
      ps.pi_thread = inferior_thread;
      ps.pi_offset = 0;
      ps.pi_buffer = (char *) &vector_registers;
      ps.pi_nbytes = sizeof vector_registers;

      ioctl (inferior_fd, PIXRDVREGS, &ps);

      switch (reg)
	{
	case VL_REGNUM:
	  vector_registers.vls =
	    (vector_registers.vls & 0xffffffff00000000LL)
	      + (unsigned long) val;
	  break;

	case VS_REGNUM:
	  vector_registers.vls =
	    (val << 32) + (unsigned long) vector_registers.vls;
	  break;
	    
	default:
	  vector_registers.vr[reg].el[element] = val;
	  break;
	}

      ioctl (inferior_fd, PIXWRVREGS, &ps);

      if (errno)
	perror_with_name ("writing vector register");
    }
}

/* Return the contents of communication register NUM.  */ 

static REGISTER_TYPE 
read_comm_register (num)
     int num;
{
  if (have_inferior_p ())
    {
      ps.pi_buffer = (char *) &comm_registers;
      ps.pi_nbytes = sizeof comm_registers;
      ps.pi_offset = 0;
      ps.pi_thread = inferior_thread;
      ioctl (inferior_fd, PIXRDCREGS, &ps);
    }
  return comm_registers.crreg.r4[num];
}

/* Store a new value VAL into communication register NUM.  
   NB: Must use read-modify-write on the whole comm register set
   since pattach does not do offsetted writes correctly.  */

static void
write_comm_register (num, val)
     int num;
     REGISTER_TYPE val;
{
  if (have_inferior_p ())
    {
      ps.pi_buffer = (char *) &comm_registers;
      ps.pi_nbytes = sizeof comm_registers;
      ps.pi_offset = 0;
      ps.pi_thread = inferior_thread;
      ioctl (inferior_fd, PIXRDCREGS, &ps);
      comm_registers.crreg.r4[num] = val;
      ioctl (inferior_fd, PIXWRCREGS, &ps);
    }
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
    if (step || signal)
      thread_continue (inferior_thread, step, signal);
    else
      thread_continue (-1, 0, 0);
}

/* Maybe resume some threads.
   THREAD is which thread to resume, or -1 to resume them all.
   STEP and SIGNAL are as in resume.

   Global variable ALL_CONTINUE is set when we are here to do a
   `cont' command; otherwise we may be doing `finish' or a call or
   something else that will not tolerate an automatic thread switch.

   If there are stopped threads waiting to deliver signals, and
   ALL_CONTINUE, do not actually resume anything.  gdb will do a wait
   and see one of the stopped threads in the queue.  */

static void
thread_continue (thread, step, signal)
     int thread, step, signal;
{
  int n;

  /* If we are to continue all threads, but not for the CONTINUE command,
     pay no attention and continue only the selected thread.  */

  if (thread < 0 && ! all_continue)
    thread = inferior_thread;

  /* If we are not stepping, we have now executed the continue part
     of a CONTINUE command.  */

  if (! step)
    all_continue = 0;

  /* Allow wait() to switch threads if this is an all-out continue.  */

  thread_switch_ok = thread < 0;

  /* If there are threads queued up, don't resume.  */

  if (thread_switch_ok && ! signal_stack_is_empty ())
    return;

  /* OK, do it.  */

  for (n = 0; n < n_threads; n++)
    if (thread_state[n] == PI_TALIVE)
      {
	select_thread (n);

	if ((thread < 0 || n == thread) && ! thread_is_in_kernel[n])
	  {
	    /* Blam the trace bits in the stack's saved psws to match 
	       the desired step mode.  This is required so that
	       single-stepping a return doesn't restore a psw with a
	       clear trace bit and fly away, and conversely,
	       proceeding through a return in a routine that was
	       stepped into doesn't cause a phantom break by restoring
	       a psw with the trace bit set. */
	    scan_stack (PSW_T_BIT, step);
	    scan_stack (PSW_S_BIT, sequential);
	  }

	ps.pi_buffer = registers;
	ps.pi_nbytes = REGISTER_BYTES;
	ps.pi_offset = 0;
	ps.pi_thread = n;
	if (! thread_is_in_kernel[n])
	  if (ioctl (inferior_fd, PIXWRREGS, &ps))
	    perror_with_name ("PIXWRREGS");

	if (thread < 0 || n == thread)
	  {
	    ps.pi_pc = 1;
	    ps.pi_signo = signal;
	    if (ioctl (inferior_fd, step ? PIXSTEP : PIXCONTINUE, &ps) < 0)
	      perror_with_name ("PIXCONTINUE");
	  }
      }

  if (ioctl (inferior_fd, PIXRUN, &ps) < 0)
    perror_with_name ("PIXRUN");
}

/* Replacement for system wait routine.  

   The system wait returns with one or more threads stopped by
   signals.  Put stopped threads on a stack and return them one by
   one, so that it appears that wait returns one thread at a time.

   Global variable THREAD_SWITCH_OK is set when gdb can tolerate wait
   returning a new thread.  If it is false, then only one thread is
   running; we will do a real wait, the thread will do something, and
   we will return that.  */

pid_t
wait (w)
    union wait *w;
{
  int pid;

  if (!w)
    return wait3 (0, 0, 0);

  /* Do a real wait if we were told to, or if there are no queued threads.  */

  if (! thread_switch_ok || signal_stack_is_empty ())
    {
      int thread;

      pid = wait3 (w, 0, 0);

      if (!WIFSTOPPED (*w) || pid != inferior_pid)
	return pid;

      /* The inferior has done something and stopped.  Read in all the
	 threads' registers, and queue up any signals that happened.  */

      if (ioctl (inferior_fd, PIXGETTHCOUNT, &ps) < 0)
	perror_with_name ("PIXGETTHCOUNT");
      
      n_threads = ps.pi_othdcnt;
      for (thread = 0; thread < n_threads; thread++)
	{
	  ps.pi_thread = thread;
	  if (ioctl (inferior_fd, PIXGETSUBCODE, &ps) < 0)
	    perror_with_name ("PIXGETSUBCODE");
	  thread_state[thread] = ps.pi_otstate;

	  if (ps.pi_otstate == PI_TALIVE)
	    {
	      select_thread (thread);
	      ps.pi_buffer = registers;
	      ps.pi_nbytes = REGISTER_BYTES;
	      ps.pi_offset = 0;
	      ps.pi_thread = thread;
	      if (ioctl (inferior_fd, PIXRDREGS, &ps) < 0)
		perror_with_name ("PIXRDREGS");

	      thread_pc[thread] = read_pc ();
	      thread_signal[thread] = ps.pi_osigno;
	      thread_sigcode[thread] = ps.pi_osigcode;

	      /* If the thread's stack has a context frame
		 on top, something fucked is going on.  I do not
		 know what, but do I know this: the only thing you
		 can do with such a thread is continue it.  */

	      thread_is_in_kernel[thread] = 
		((read_register (PS_REGNUM) >> 25) & 3) == 0;

	      /* Signals push an extended frame and then fault
		 with a ridiculous pc.  Pop the frame.  */

	      if (thread_pc[thread] > STACK_END_ADDR)
		{
		  POP_FRAME;
		  if (is_break_pc (thread_pc[thread]))
		    thread_pc[thread] = read_pc () - 2;
		  else
		    thread_pc[thread] = read_pc ();
		  write_register (PC_REGNUM, thread_pc[thread]);
		}
	      
	      if (ps.pi_osigno || ps.pi_osigcode)
		{
		  signal_stack++;
		  signal_stack->pid = pid;
		  signal_stack->thread = thread;
		  signal_stack->signo = thread_signal[thread];
		  signal_stack->subsig = thread_sigcode[thread];
		  signal_stack->pc = thread_pc[thread];
		}

	      /* The following hackery is caused by a unix 7.1 feature:
		 the inferior's fixed scheduling mode is cleared when
		 it execs the shell (since the shell is not a parallel
		 program).  So, note the 5.4 trap we get when
		 the shell does its exec, then catch the 5.0 trap 
		 that occurs when the debuggee starts, and set fixed
		 scheduling mode properly.  */

	      if (ps.pi_osigno == 5 && ps.pi_osigcode == 4)
		exec_trap_timer = 1;
	      else
		exec_trap_timer--;
	      
	      if (ps.pi_osigno == 5 && exec_trap_timer == 0)
		set_fixed_scheduling (pid, parallel == 2);
	    }
	}

      if (signal_stack_is_empty ())
	error ("no active threads?!");
    }

  /* Select the thread that stopped, and return *w saying why.  */

  select_thread (signal_stack->thread);

  stop_signal = signal_stack->signo;
  stop_sigcode = signal_stack->subsig;

  WSETSTOP (*w, signal_stack->signo);
  w->w_thread = signal_stack->thread;
  return (signal_stack--)->pid;
}

/* Select thread THREAD -- its registers, stack, per-thread memory.
   This is the only routine that may assign to inferior_thread
   or thread_regs[].  */

static void
select_thread (thread)
     int thread;
{
  if (thread == inferior_thread)
    return;

  bcopy (registers, thread_regs[inferior_thread], REGISTER_BYTES);
  ps.pi_thread = inferior_thread = thread;
  if (have_inferior_p ())
    ioctl (inferior_fd, PISETRWTID, &ps);
  bcopy (thread_regs[thread], registers, REGISTER_BYTES);
}
  
/* Routine to set or clear a psw bit in the psw and also all psws
   saved on the stack.  Quits when we get to a frame in which the
   saved psw is correct. */

static void
scan_stack (bit, val)
    long bit, val;
{
  long ps = read_register (PS_REGNUM);
  long fp;
  if (val ? !(ps & bit) : (ps & bit))
    {    
      ps ^= bit;
      write_register (PS_REGNUM, ps);

      fp = read_register (FP_REGNUM);
      while (fp & 0x80000000)
	{
	  ps = read_memory_integer (fp + 4, 4);
	  if (val ? (ps & bit) : !(ps & bit))
	    break;
	  ps ^= bit;
	  write_memory (fp + 4, &ps, 4);
	  fp = read_memory_integer (fp + 8, 4);
	}
    }
}

/* Set fixed scheduling (alliant mode) of process PID to ARG (0 or 1).  */

static void
set_fixed_scheduling (pid, arg)
      int arg;
{
  struct pattributes pattr;
  getpattr (pid, &pattr);
  pattr.pattr_pfixed = arg;
  setpattr (pid, &pattr);
}

core_file_command (filename, from_tty)
     char *filename;
     int from_tty;
{
  int n;

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
  n_core = 0;

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

      if (myread (corechan, &filehdr, sizeof filehdr) < 0)
	perror_with_name (filename);

      if (!IS_CORE_SOFF_MAGIC (filehdr.h_magic))
	error ("%s: not a core file.\n", filename);

      if (myread (corechan, &opthdr, filehdr.h_opthdr) < 0)
	perror_with_name (filename);

      /* Read through the section headers.
	 For text, data, etc, record an entry in the core file map.
	 For context and tcontext, record the file address of
	 the context blocks.  */

      lseek (corechan, (long) filehdr.h_scnptr, 0);

      n_threads = 0;
      for (n = 0; n < filehdr.h_nscns; n++)
	{
	  if (myread (corechan, &scnhdr, sizeof scnhdr) < 0)
	    perror_with_name (filename);
	  if ((scnhdr.s_flags & S_TYPMASK) >= S_TEXT
	      && (scnhdr.s_flags & S_TYPMASK) <= S_COMON)
	    {
	      core_map[n_core].mem_addr = scnhdr.s_vaddr;
	      core_map[n_core].mem_end = scnhdr.s_vaddr + scnhdr.s_size;
	      core_map[n_core].file_addr = scnhdr.s_scnptr;
	      core_map[n_core].type = scnhdr.s_flags & S_TYPMASK;
	      if (core_map[n_core].type != S_TBSS
		  && core_map[n_core].type != S_TDATA
		  && core_map[n_core].type != S_TTEXT)
		core_map[n_core].thread = -1;
	      else if (n_core == 0
		       || core_map[n_core-1].mem_addr != scnhdr.s_vaddr)
		core_map[n_core].thread = 0;
	      else 
		core_map[n_core].thread = core_map[n_core-1].thread + 1;
	      n_core++;
	    }
	  else if ((scnhdr.s_flags & S_TYPMASK) == S_CONTEXT)
	    context_offset = scnhdr.s_scnptr;
	  else if ((scnhdr.s_flags & S_TYPMASK) == S_TCONTEXT) 
	    tcontext_offset[n_threads++] = scnhdr.s_scnptr;
	}

      /* Read the context block, struct user, struct proc,
	 and the comm regs.  */

      lseek (corechan, context_offset, 0);
      if (myread (corechan, &c, sizeof c) < 0)
	perror_with_name (filename);
      lseek (corechan, c.core_user_p, 0);
      if (myread (corechan, &u, sizeof u) < 0)
	perror_with_name (filename);
      lseek (corechan, c.core_proc_p, 0);
      if (myread (corechan, &pr, sizeof pr) < 0)
	perror_with_name (filename);
      comm_registers = pr.p_creg;

      /* Core file apparently is really there.  Make it really exist
	 for xfer_core_file so we can do read_memory on it. */

      if (filename[0] == '/')
	corefile = savestring (filename, strlen (filename));
      else
	corefile = concat (current_directory, "/", filename);

      printf_filtered ("Program %s ", u.u_comm);

      /* Read the thread registers and fill in the thread_xxx[] data.  */

      for (n = 0; n < n_threads; n++)
	{
	  select_thread (n);

	  lseek (corechan, tcontext_offset[n], 0);
	  if (myread (corechan, &tc, sizeof tc) < 0)
	    perror_with_name (corefile);
	  lseek (corechan, tc.core_thread_p, 0);
	  if (myread (corechan, &th, sizeof th) < 0)
	    perror_with_name (corefile);

	  lseek (corechan, tc.core_syscall_context_p, 0);
	  if (myread (corechan, registers, REGISTER_BYTES) < 0)
	    perror_with_name (corefile);

	  thread_signal[n] = th.t_cursig;
	  thread_sigcode[n] = th.t_code;
	  thread_state[n] = th.t_state;
	  thread_pc[n] = read_pc ();

	  if (thread_pc[n] > STACK_END_ADDR)
	    {
	      POP_FRAME;
	      if (is_break_pc (thread_pc[n]))
		thread_pc[n] = read_pc () - 2;
	      else
		thread_pc[n] = read_pc ();
	      write_register (PC_REGNUM, thread_pc[n]);
	    }

	  printf_filtered ("thread %d received signal %d, %s\n",
			   n, thread_signal[n],
			   thread_signal[n] < NSIG
			   ? sys_siglist[thread_signal[n]]
			   : "(undocumented)");
	}

      /* Select an interesting thread -- also-rans died with SIGKILL,
	 so find one that didn't.  */

      for (n = 0; n < n_threads; n++)
	if (thread_signal[n] != 0 && thread_signal[n] != SIGKILL)
	  {
	    select_thread (n);
	    stop_signal = thread_signal[n];
	    stop_sigcode = thread_sigcode[n];
	    break;
	  }

      core_aouthdr.a_magic = 0;

      flush_cached_frames ();
      set_current_frame (create_new_frame (read_register (FP_REGNUM),
					     read_pc ()));
      select_frame (get_current_frame (), 0);
      validate_files ();

      print_sel_frame (1);
    }
  else if (from_tty)
    printf_filtered ("No core file now.\n");
}

exec_file_command (filename, from_tty)
     char *filename;
     int from_tty;
{
  int val;
  int n;
  struct stat st_exec;

  /* Eliminate all traces of old exec file.
     Mark text segment as empty.  */

  if (execfile)
    free (execfile);
  execfile = 0;
  data_start = 0;
  data_end = 0;
  text_start = 0;
  text_end = 0;
  exec_data_start = 0;
  exec_data_end = 0;
  if (execchan >= 0)
    close (execchan);
  execchan = -1;

  n_exec = 0;

  /* Now open and digest the file the user requested, if any.  */

  if (filename)
    {
      filename = tilde_expand (filename);
      make_cleanup (free, filename);
      
      execchan = openp (getenv ("PATH"), 1, filename, O_RDONLY, 0,
			&execfile);
      if (execchan < 0)
	perror_with_name (filename);

      if (myread (execchan, &filehdr, sizeof filehdr) < 0)
	perror_with_name (filename);

      if (! IS_SOFF_MAGIC (filehdr.h_magic))
	error ("%s: not an executable file.", filename);

      if (myread (execchan, &opthdr, filehdr.h_opthdr) <= 0)
	perror_with_name (filename);

      /* Read through the section headers.
	 For text, data, etc, record an entry in the exec file map.
	 Record text_start and text_end.  */

      lseek (execchan, (long) filehdr.h_scnptr, 0);

      for (n = 0; n < filehdr.h_nscns; n++)
	{
	  if (myread (execchan, &scnhdr, sizeof scnhdr) < 0)
	    perror_with_name (filename);

	  if ((scnhdr.s_flags & S_TYPMASK) >= S_TEXT
	      && (scnhdr.s_flags & S_TYPMASK) <= S_COMON)
	    {
	      exec_map[n_exec].mem_addr = scnhdr.s_vaddr;
	      exec_map[n_exec].mem_end = scnhdr.s_vaddr + scnhdr.s_size;
	      exec_map[n_exec].file_addr = scnhdr.s_scnptr;
	      exec_map[n_exec].type = scnhdr.s_flags & S_TYPMASK;
	      n_exec++;

	      if ((scnhdr.s_flags & S_TYPMASK) == S_TEXT)
		{
		  text_start = scnhdr.s_vaddr;
		  text_end =  scnhdr.s_vaddr + scnhdr.s_size;
		}
	    }
	}

      fstat (execchan, &st_exec);
      exec_mtime = st_exec.st_mtime;
      
      validate_files ();
    }
  else if (from_tty)
    printf_filtered ("No exec file now.\n");

  /* Tell display code (if any) about the changed file name.  */
  if (exec_file_display_hook)
    (*exec_file_display_hook) (filename);
}

/* Read data from SOFF exec or core file.
   Return 0 on success, 1 if address could not be read. */

int
xfer_core_file (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  register int i;
  register int n;
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
	 along with the next address.  */

      i = len;

      for (n = 0; n < n_core; n++)
	{
	  if (memaddr >= core_map[n].mem_addr && memaddr < core_map[n].mem_end
	      && (core_map[n].thread == -1
		  || core_map[n].thread == inferior_thread))
	    {
	      i = min (len, core_map[n].mem_end - memaddr);
	      fileptr = core_map[n].file_addr + memaddr - core_map[n].mem_addr;
	      if (core_map[n].file_addr)
		{
		  xferfile = &corefile;
		  xferchan = corechan;
		}
	      break;
	    }
	  else if (core_map[n].mem_addr >= memaddr
		   && core_map[n].mem_addr < memaddr + i)
 	    i = core_map[n].mem_addr - memaddr;
        }

      if (!xferfile) 
	for (n = 0; n < n_exec; n++)
	  {
	    if (memaddr >= exec_map[n].mem_addr
		&& memaddr < exec_map[n].mem_end)
	      {
		i = min (len, exec_map[n].mem_end - memaddr);
		fileptr = exec_map[n].file_addr + memaddr
		  - exec_map[n].mem_addr;
		if (exec_map[n].file_addr)
		  {
		    xferfile = &execfile;
		    xferchan = execchan;
		  }
		break;
	      }
	    else if (exec_map[n].mem_addr >= memaddr
		     && exec_map[n].mem_addr < memaddr + i)
	      i = exec_map[n].mem_addr - memaddr;
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
	 read zeros if reading, or do nothing if writing.  */
      else
	{
	  bzero (myaddr, i);
	  returnval = 1;
	}

      memaddr += i;
      myaddr += i;
      len -= i;
    }
  return returnval;
}


/* Here from info files command to print an address map.  */

print_maps ()
{
  struct pmap ptrs[200];
  int n;

  /* ID strings for core and executable file sections */

  static char *idstr[] =
    {
      "0", "text", "data", "tdata", "bss", "tbss", 
      "common", "ttext", "ctx", "tctx", "10", "11", "12",
    };

  for (n = 0; n < n_core; n++)
    {
      core_map[n].which = 0;
      ptrs[n] = core_map[n];
    }
  for (n = 0; n < n_exec; n++)
    {
      exec_map[n].which = 1;
      ptrs[n_core+n] = exec_map[n];
    }

  qsort (ptrs, n_core + n_exec, sizeof *ptrs, ptr_cmp);

  for (n = 0; n < n_core + n_exec; n++)
    {
      struct pmap *p = &ptrs[n];
      if (n > 0)
	{
	  if (p->mem_addr < ptrs[n-1].mem_end)
	    p->mem_addr = ptrs[n-1].mem_end;
	  if (p->mem_addr >= p->mem_end)
	    continue;
	}
      printf_filtered ("%08x .. %08x  %-6s  %s\n",
		       p->mem_addr, p->mem_end, idstr[p->type],
		       p->which ? execfile : corefile);
    }
}

/* Compare routine to put file sections in order.
   Sort into increasing order on address, and put core file sections
   before exec file sections if both files contain the same addresses.  */

static ptr_cmp (a, b)
     struct pmap *a, *b;
{
  if (a->mem_addr != b->mem_addr) return a->mem_addr - b->mem_addr;
  return a->which - b->which;
}

/* Trapped internal variables are used to handle special registers.
   A trapped i.v. calls a hook here every time it is dereferenced,
   to provide a new value for the variable, and it calls a hook here
   when a new value is assigned, to do something with the value.
   
   The vector registers are $vl, $vs, $vm, $vN, $VN (N in 0..7).
   The communication registers are $cN, $CN (N in 0..63).
   They not handled as regular registers because it's expensive to
   read them, and their size varies, and they have too many names.  */


/* Return 1 if NAME is a trapped internal variable, else 0. */

int
is_trapped_internalvar (name)
     char *name;
{
    if ((name[0] == 'c' || name[0] == 'C')
	&& name[1] >= '0' && name[1] <= '9'
	&& (name[2] == '\0'
	    || (name[2] >= '0' && name[2] <= '9'
		&& name[3] == '\0' && name[1] != '0'))
	&& atoi (&name[1]) < 64) return 1;

  if ((name[0] == 'v' || name[0] == 'V')
      && (((name[1] & -8) == '0' && name[2] == '\0')
	  || !strcmp (name, "vl")
	  || !strcmp (name, "vs") 
	  || !strcmp (name, "vm")))
    return 1;
  else return 0;
}

/* Return the value of trapped internal variable VAR */

value
value_of_trapped_internalvar (var)
     struct internalvar *var;
{
  char *name = var->name;
  value val;
  struct type *type;
  long len = *read_vector_register (VL_REGNUM);
  if (len <= 0 || len > 128) len = 128;

  if (!strcmp (name, "vl"))
    {
      val = value_from_long (builtin_type_int,
			     (LONGEST) *read_vector_register_1 (VL_REGNUM));
    }
  else if (!strcmp (name, "vs"))
    {
      val = value_from_long (builtin_type_int,
			     (LONGEST) *read_vector_register_1 (VS_REGNUM));
    }
  else if (!strcmp (name, "vm"))
    {
      long vm[4];
      long i, *p;
      bcopy (read_vector_register_1 (VM_REGNUM), vm, sizeof vm);
      type = vector_type (builtin_type_int, len);
      val = allocate_value (type);
      p = (long *) VALUE_CONTENTS (val);
      for (i = 0; i < len; i++) 
	*p++ = !! (vm[3 - (i >> 5)] & (1 << (i & 037)));
    }
  else if (name[0] == 'V')
    {
      type = vector_type (builtin_type_long_long, len);
      val = allocate_value (type);
      bcopy (read_vector_register_1 (name[1] - '0'),
	     VALUE_CONTENTS (val), TYPE_LENGTH (type));
    }
  else if (name[0] == 'v')
    {
      long *p1, *p2;
      type = vector_type (builtin_type_long, len);
      val = allocate_value (type);
      p1 = read_vector_register_1 (name[1] - '0');
      p2 = (long *) VALUE_CONTENTS (val);
      while (--len >= 0) {p1++; *p2++ = *p1++;}
    }

  else if (name[0] == 'c')
    val = value_from_long (builtin_type_int,
			   read_comm_register (atoi (&name[1])));
  else if (name[0] == 'C')
    val = value_from_long (builtin_type_long_long,
			   read_comm_register (atoi (&name[1])));

  VALUE_LVAL (val) = lval_internalvar;
  VALUE_INTERNALVAR (val) = var;
  return val;
}

/* Construct the type for a vector register's value --
   array[LENGTH] of ELEMENT_TYPE.  */

static struct type *
vector_type (element_type, length)
     struct type *element_type;
     long length;
{
  struct type *type = (struct type *) xmalloc (sizeof (struct type));
  bzero (type, sizeof type);
  TYPE_CODE (type) = TYPE_CODE_ARRAY;
  TYPE_TARGET_TYPE (type) = element_type;
  TYPE_LENGTH (type) = length * TYPE_LENGTH (TYPE_TARGET_TYPE (type));
  return type;
}

/* Handle a new value assigned to a trapped internal variable */

void
set_trapped_internalvar (var, val, bitpos, bitsize, offset)
     struct internalvar *var;
     value val;
     int bitpos, bitsize, offset;
{ 
  char *name = var->name;
  long long newval = value_as_long (val);

  if (!strcmp (name, "vl")) 
    write_vector_register (VL_REGNUM, 0, newval);
  else if (!strcmp (name, "vs"))
    write_vector_register (VS_REGNUM, 0, newval);
  else if (name[0] == 'c' || name[0] == 'C')
    write_comm_register (atoi (&name[1]), newval);
  else if (!strcmp (name, "vm"))
    error ("can't assign to $vm");
  else
    {
      offset /= bitsize / 8;
      write_vector_register (name[1] - '0', offset, newval);
    }
}

/* Print an integer value when no format was specified.  gdb normally
   prints these values in decimal, but the the leading 0x80000000 of
   pointers produces intolerable 10-digit negative numbers.
   If it looks like an address, print it in hex instead.  */

decout (stream, type, val)
     FILE *stream;
     struct type *type;
     LONGEST val;
{
  long lv = val;

  switch (output_radix)
    {
    case 0:
      if ((lv == val || (unsigned) lv == val)
	  && ((lv & 0xf0000000) == 0x80000000
	      || ((lv & 0xf0000000) == 0xf0000000 && lv < STACK_END_ADDR)))
	{
	  fprintf_filtered (stream, "%#x", lv);
	  return;
	}

    case 10:
      fprintf_filtered (stream, TYPE_UNSIGNED (type) ? "%llu" : "%lld", val);
      return;

    case 8:
      if (TYPE_LENGTH (type) <= sizeof lv)
	fprintf_filtered (stream, "%#o", lv);
      else
	fprintf_filtered (stream, "%#llo", val);
      return;

    case 16:
      if (TYPE_LENGTH (type) <= sizeof lv)
	fprintf_filtered (stream, "%#x", lv);
      else
	fprintf_filtered (stream, "%#llx", val);
      return;
    }
}

/* Change the default output radix to 10 or 16, or set it to 0 (heuristic).
   This command is mostly obsolete now that the print command allows
   formats to apply to aggregates, but is still handy occasionally.  */

static void
set_base_command (arg)
    char *arg;
{
  int new_radix;

  if (!arg)
    output_radix = 0;
  else
    {
      new_radix = atoi (arg);
      if (new_radix != 10 && new_radix != 16 && new_radix != 8) 
	error ("base must be 8, 10 or 16, or null");
      else output_radix = new_radix;
    }
}

/* Turn pipelining on or off in the inferior. */

static void
set_pipelining_command (arg)
    char *arg;
{
  if (!arg)
    {
      sequential = !sequential;
      printf_filtered ("%s\n", sequential ? "off" : "on");
    }
  else if (!strcmp (arg, "on"))
    sequential = 0;
  else if (!strcmp (arg, "off"))
    sequential = 1;
  else error ("valid args are `on', to allow instructions to overlap, or\n\
`off', to prevent it and thereby pinpoint exceptions.");
}

/* Enable, disable, or force parallel execution in the inferior.  */

static void
set_parallel_command (arg)
     char *arg;
{
  struct rlimit rl;
  int prevparallel = parallel;

  if (!strncmp (arg, "fixed", strlen (arg)))
    parallel = 2;  
  else if (!strcmp (arg, "on"))
    parallel = 1;
  else if (!strcmp (arg, "off"))
    parallel = 0;
  else error ("valid args are `on', to allow multiple threads, or\n\
`fixed', to force multiple threads, or\n\
`off', to run with one thread only.");

  if ((prevparallel == 0) != (parallel == 0) && inferior_pid)
    printf_filtered ("will take effect at next run.\n");

  getrlimit (RLIMIT_CONCUR, &rl);
  rl.rlim_cur = parallel ? rl.rlim_max : 1;
  setrlimit (RLIMIT_CONCUR, &rl);

  if (inferior_pid)
    set_fixed_scheduling (inferior_pid, parallel == 2);
}

/* Add a new name for an existing command.  */

static void 
alias_command (arg)
    char *arg;
{
    static char *aliaserr = "usage is `alias NEW OLD', no args allowed";
    char *newname = arg;
    struct cmd_list_element *new, *old;

    if (!arg)
      error_no_arg ("newname oldname");
	
    new = lookup_cmd (&arg, cmdlist, "", -1);
    if (new && !strncmp (newname, new->name, strlen (new->name)))
      {
	newname = new->name;
	if (!(*arg == '-' 
	      || (*arg >= 'a' && *arg <= 'z')
	      || (*arg >= 'A' && *arg <= 'Z')
	      || (*arg >= '0' && *arg <= '9')))
	  error (aliaserr);
      }
    else
      {
	arg = newname;
	while (*arg == '-' 
	       || (*arg >= 'a' && *arg <= 'z')
	       || (*arg >= 'A' && *arg <= 'Z')
	       || (*arg >= '0' && *arg <= '9'))
	  arg++;
	if (*arg != ' ' && *arg != '\t')
	  error (aliaserr);
	*arg = '\0';
	arg++;
      }

    old = lookup_cmd (&arg, cmdlist, "", 0);

    if (*arg != '\0')
      error (aliaserr);

    if (new && !strncmp (newname, new->name, strlen (new->name)))
      {
	char *tem;
	if (new->class == (int) class_user || new->class == (int) class_alias)
	  tem = "Redefine command \"%s\"? ";
	else
	  tem = "Really redefine built-in command \"%s\"? ";
	if (!query (tem, new->name))
	  error ("Command \"%s\" not redefined.", new->name);
      }

    add_com (newname, class_alias, old->function, old->doc);
}



/* Print the current thread number, and any threads with signals in the
   queue.  */

thread_info ()
{
  struct threadpid *p;

  if (have_inferior_p ())
    {
      ps.pi_buffer = (char *) &comm_registers;
      ps.pi_nbytes = sizeof comm_registers;
      ps.pi_offset = 0;
      ps.pi_thread = inferior_thread;
      ioctl (inferior_fd, PIXRDCREGS, &ps);
    }

  printf_filtered ("Current thread %d stopped with signal %d.%d (%s).\n",
		   inferior_thread, stop_signal, stop_sigcode,
		   subsig_name (stop_signal, stop_sigcode));
  
  for (p = signal_stack; p->pid; p--)
    printf_filtered ("Thread %d stopped with signal %d.%d (%s).\n",
		     p->thread, p->signo, p->subsig,
		     subsig_name (p->signo, p->subsig));
		
  if (iscrlbit (comm_registers.crctl.lbits.cc, 64+13))
    printf_filtered ("New thread start pc %#x\n",
		     (long) (comm_registers.crreg.pcpsw >> 32));
}

/* Return string describing a signal.subcode number */

static char *
subsig_name (signo, subcode)
     int signo, subcode;
{
  static char *subsig4[] = {
    "error exit", "privileged instruction", "unknown",
    "unknown", "undefined opcode",
    0};
  static char *subsig5[] = {0,
    "breakpoint", "single step", "fork trap", "exec trap", "pfork trap",
    "join trap", "idle trap", "last thread", "wfork trap",
    "process breakpoint", "trap instruction",
    0};
  static char *subsig8[] = {0,
    "int overflow", "int divide check", "float overflow",
    "float divide check", "float underflow", "reserved operand",
    "sqrt error", "exp error", "ln error", "sin error", "cos error",
    0};
  static char *subsig10[] = {0,
    "invalid inward ring address", "invalid outward ring call",
    "invalid inward ring return", "invalid syscall gate",
    "invalid rtn frame length", "invalid comm reg address",
    "invalid trap gate",
    0};
  static char *subsig11[] = {0,
    "read access denied", "write access denied", "execute access denied",
    "segment descriptor fault", "page table fault", "data reference fault",
    "i/o access denied", "levt pte invalid",
    0};

  static char **subsig_list[] = 
    {0, 0, 0, 0, subsig4, subsig5, 0, 0, subsig8, 0, subsig10, subsig11, 0};

  int i;
  char *p = signo < NSIG ? sys_siglist[signo] : "unknown";

  if (signo >= (sizeof subsig_list / sizeof *subsig_list)
      || !subsig_list[signo])
    return p;
  for (i = 1; subsig_list[signo][i]; i++)
    if (i == subcode)
      return subsig_list[signo][subcode];
  return p;
}


/* Print a compact display of thread status, essentially x/i $pc
   for all active threads.  */

static void
threadstat ()
{
  int t;

  for (t = 0; t < n_threads; t++)
    if (thread_state[t] == PI_TALIVE)
      {
	printf_filtered ("%d%c %08x%c %d.%d ", t,
			 (t == inferior_thread ? '*' : ' '), thread_pc[t],
			 (thread_is_in_kernel[t] ? '#' : ' '),
			 thread_signal[t], thread_sigcode[t]);
	print_insn (thread_pc[t], stdout);
	printf_filtered ("\n");
      }
}

/* Change the current thread to ARG.  */

set_thread_command (arg)
     char *arg;
{
    int thread;

    if (!arg)
      {
	threadstat ();
	return;
      }

    thread = parse_and_eval_address (arg);

    if (thread < 0 || thread > n_threads || thread_state[thread] != PI_TALIVE)
      error ("no such thread.");

    select_thread (thread);

    stop_pc = read_pc ();
    flush_cached_frames ();
    set_current_frame (create_new_frame (read_register (FP_REGNUM),
					 read_pc ()));
    select_frame (get_current_frame (), 0);
    print_sel_frame (1);
}

/* Here on CONT command; gdb's dispatch address is changed to come here.
   Set global variable ALL_CONTINUE to tell resume() that it should
   start up all threads, and that a thread switch will not blow gdb's
   mind.  */

static void
convex_cont_command (proc_count_exp, from_tty)
     char *proc_count_exp;
     int from_tty;
{
  all_continue = 1;
  cont_command (proc_count_exp, from_tty);
}

/* Here on 1CONT command.  Resume only the current thread.  */

one_cont_command (proc_count_exp, from_tty)
     char *proc_count_exp;
     int from_tty;
{
  cont_command (proc_count_exp, from_tty);
}

/* Print the contents and lock bits of all communication registers,
   or just register ARG if ARG is a communication register,
   or the 3-word resource structure in memory at address ARG.  */

comm_registers_info (arg)
    char *arg;
{
  int i, regnum;

  if (arg)
    {
      if (sscanf (arg, "0x%x", &regnum) == 1
	  || sscanf (argc, "%d", &regnum) == 1)
	{
	  if (regnum > 0)
	    regnum &= ~0x8000;
	}
      else if (sscanf (arg, "$c%d", &regnum) == 1)
	;
      else if (sscanf (arg, "$C%d", &regnum) == 1)
	;
      else
	regnum = parse_and_eval_address (arg);

      if (regnum >= 64)
	error ("%s: invalid register name.", arg);

      /* if we got a (user) address, examine the resource struct there */

      if (regnum < 0)
	{
	  static int buf[3];
	  read_memory (regnum, buf, sizeof buf);
	  printf_filtered ("%08x  %08x%08x%s\n", regnum, buf[1], buf[2],
			   buf[0] & 0xff ? " locked" : "");
	  return;
	}
    }

  ps.pi_buffer = (char *) &comm_registers;
  ps.pi_nbytes = sizeof comm_registers;
  ps.pi_offset = 0;
  ps.pi_thread = inferior_thread;
  ioctl (inferior_fd, PIXRDCREGS, &ps);

  for (i = 0; i < 64; i++)
    if (!arg || i == regnum)
      printf_filtered ("%2d 0x8%03x %016llx%s\n", i, i,
		       comm_registers.crreg.r4[i],
		       (iscrlbit (comm_registers.crctl.lbits.cc, i)
			? " locked" : ""));
}

/* Print the psw */

static void 
psw_info (arg)
    char *arg;
{
  struct pswbit
    {
      int bit;
      int pos;
      char *text;
    };

  static struct pswbit pswbit[] =
    {
      { 0x80000000, -1, "A carry" }, 
      { 0x40000000, -1, "A integer overflow" }, 
      { 0x20000000, -1, "A zero divide" }, 
      { 0x10000000, -1, "Integer overflow enable" }, 
      { 0x08000000, -1, "Trace" }, 
      { 0x06000000, 25, "Frame length" }, 
      { 0x01000000, -1, "Sequential" }, 
      { 0x00800000, -1, "S carry" }, 
      { 0x00400000, -1, "S integer overflow" }, 
      { 0x00200000, -1, "S zero divide" }, 
      { 0x00100000, -1, "Zero divide enable" }, 
      { 0x00080000, -1, "Floating underflow" }, 
      { 0x00040000, -1, "Floating overflow" }, 
      { 0x00020000, -1, "Floating reserved operand" }, 
      { 0x00010000, -1, "Floating zero divide" }, 
      { 0x00008000, -1, "Floating error enable" }, 
      { 0x00004000, -1, "Floating underflow enable" }, 
      { 0x00002000, -1, "IEEE" }, 
      { 0x00001000, -1, "Sequential stores" }, 
      { 0x00000800, -1, "Intrinsic error" }, 
      { 0x00000400, -1, "Intrinsic error enable" }, 
      { 0x00000200, -1, "Trace thread creates" }, 
      { 0x00000100, -1, "Thread init trap" }, 
      { 0x000000e0,  5, "Reserved" },
      { 0x0000001f,  0, "Intrinsic error code" },
      {0, 0, 0},
    };

  long psw;
  struct pswbit *p;

  if (arg)
    psw = parse_and_eval_address (arg);
  else
    psw = read_register (PS_REGNUM);

  for (p = pswbit; p->bit; p++)
    {
      if (p->pos < 0)
	printf_filtered ("%08x  %s  %s\n", p->bit,
			 (psw & p->bit) ? "yes" : "no ", p->text);
      else
	printf_filtered ("%08x %3d   %s\n", p->bit,
			 (psw & p->bit) >> p->pos, p->text);
    }
}

_initialize_convex_dep ()
{
  add_com ("alias", class_support, alias_command,
	   "Add a new name for an existing command.");

  add_cmd ("base", class_vars, set_base_command,
	   "Change the integer output radix to 8, 10 or 16\n\
or use just `set base' with no args to return to the ad-hoc default,\n\
which is 16 for integers that look like addresses, 10 otherwise.",
	   &setlist);

  add_cmd ("pipeline", class_run, set_pipelining_command,
	   "Enable or disable overlapped execution of instructions.\n\
With `set pipe off', exceptions are reported with\n\
$pc pointing at the instruction after the faulting one.\n\
The default is `set pipe on', which runs faster.",
	   &setlist);

  add_cmd ("parallel", class_run, set_parallel_command,
	   "Enable or disable multi-threaded execution of parallel code.\n\
`set parallel off' means run the program on a single CPU.\n\
`set parallel fixed' means run the program with all CPUs assigned to it.\n\
`set parallel on' means run the program on any CPUs that are available.",
	   &setlist);

  add_com ("1cont", class_run, one_cont_command,
	   "Continue the program, activating only the current thread.\n\
Args are the same as the `cont' command.");

  add_com ("thread", class_run, set_thread_command,
	   "Change the current thread, the one under scrutiny and control.\n\
With no arg, show the active threads, the current one marked with *.");

  add_info ("threads", thread_info,
	    "List status of active threads.");

  add_info ("comm-registers", comm_registers_info,
	    "List communication registers and their contents.\n\
A communication register name as argument means describe only that register.\n\
An address as argument means describe the resource structure at that address.\n\
`Locked' means that the register has been sent to but not yet received from.");

  add_info ("psw", psw_info, 
	    "Display $ps, the processor status word, bit by bit.\n\
An argument means display that value's interpretation as a psw.");

  add_cmd ("convex", no_class, 0, "Convex-specific commands.\n\
32-bit registers  $pc $ps $sp $ap $fp $a1-5 $s0-7 $v0-7 $vl $vs $vm $c0-63\n\
64-bit registers  $S0-7 $V0-7 $C0-63\n\
\n\
info threads	    display info on stopped threads waiting to signal\n\
thread		    display list of active threads\n\
thread N	    select thread N (its registers, stack, memory, etc.)\n\
step, next, etc     step selected thread only\n\
1cont		    continue selected thread only\n\
cont		    continue all threads\n\
info comm-registers display contents of comm register(s) or a resource struct\n\
info psw	    display processor status word $ps\n\
set base N	    change integer radix used by `print' without a format\n\
set pipeline off    exceptions are precise, $pc points after the faulting insn\n\
set pipeline on     normal mode, $pc is somewhere ahead of faulting insn\n\
set parallel off    program runs on a single CPU\n\
set parallel fixed  all CPUs are assigned to the program\n\
set parallel on     normal mode, parallel execution on random available CPUs\n\
",
	   &cmdlist);

}
