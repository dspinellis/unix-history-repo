/* Convex stuff for GDB.
   Copyright (C) 1990, 1991 Free Software Foundation, Inc.

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
#include "command.h"
#include "symtab.h"
#include "value.h"
#include "frame.h"
#include "inferior.h"
#include "wait.h"

#include <signal.h>
#include <fcntl.h>

#include "gdbcore.h"
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

#include "gdbcmd.h"

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
   Return 0 on success, EIO if address out of bounds. */

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
	  returnval = EIO;
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
      val = value_from_longest (builtin_type_int,
			     (LONGEST) *read_vector_register_1 (VL_REGNUM));
    }
  else if (!strcmp (name, "vs"))
    {
      val = value_from_longest (builtin_type_int,
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
    val = value_from_longest (builtin_type_int,
			   read_comm_register (atoi (&name[1])));
  else if (name[0] == 'C')
    val = value_from_longest (builtin_type_long_long,
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
  char *p;

  if ((p = strsignal (signo)) == NULL)
    p = "unknown";
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
    print_stack_frame (selected_frame, selected_frame_level, -1);
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
             if (sscanf (arg, "$c%d", &regnum) == 1) {
	;
      } else if (sscanf (arg, "$C%d", &regnum) == 1) {
	;
      } else {
	regnum = parse_and_eval_address (arg);
	if (regnum > 0)
	  regnum &= ~0x8000;
      }

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
