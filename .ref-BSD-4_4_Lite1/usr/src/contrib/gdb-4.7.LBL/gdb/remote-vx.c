/* Memory-access and commands for remote VxWorks processes, for GDB.
   Copyright 1990, 1991, 1992 Free Software Foundation, Inc.
   Contributed by Wind River Systems and Cygnus Support.

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
#include "wait.h"
#include "target.h"
#include "gdbcore.h"
#include "command.h"
#include "symtab.h"
#include "symfile.h"		/* for struct complaint */

#include <string.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#define malloc bogon_malloc	/* Sun claims "char *malloc()" not void * */
#define free bogon_free		/* Sun claims "int free()" not void */
#define realloc bogon_realloc	/* Sun claims "char *realloc()", not void * */
#include <rpc/rpc.h>
#undef malloc
#undef free
#undef realloc
#include <sys/time.h>		/* UTek's <rpc/rpc.h> doesn't #incl this */
#include <netdb.h>
#include "vx-share/ptrace.h"
#include "vx-share/xdr_ptrace.h"
#include "vx-share/xdr_ld.h"
#include "vx-share/xdr_rdb.h"
#include "vx-share/dbgRpcLib.h"
#include "vx-share/reg.h"

#include <symtab.h>

extern void symbol_file_command ();
extern int stop_soon_quietly;		/* for wait_for_inferior */

static int net_ptrace_clnt_call ();	/* Forward decl */
static enum clnt_stat net_clnt_call ();	/* Forward decl */
extern struct target_ops vx_ops, vx_run_ops;	/* Forward declaration */

/* Saved name of target host and called function for "info files".
   Both malloc'd.  */

static char *vx_host;
static char *vx_running;		/* Called function */

/* Nonzero means target that is being debugged remotely has a floating
   point processor.  */

static int target_has_fp;

/* Default error message when the network is forking up.  */

static const char rpcerr[] = "network target debugging:  rpc error";

CLIENT *pClient;         /* client used in net debugging */
static int ptraceSock = RPC_ANYSOCK;

enum clnt_stat net_clnt_call();
static void parse_args ();

static struct timeval rpcTimeout = { 10, 0 };

static char *skip_white_space ();
static char *find_white_space ();
 
/* Tell the VxWorks target system to download a file.
   The load addresses of the text, data, and bss segments are
   stored in *pTextAddr, *pDataAddr, and *pBssAddr (respectively).
   Returns 0 for success, -1 for failure.  */

static int
net_load (filename, pTextAddr, pDataAddr, pBssAddr)
    char *filename;
    CORE_ADDR *pTextAddr;
    CORE_ADDR *pDataAddr;
    CORE_ADDR *pBssAddr;
    {
    enum clnt_stat status;
    struct ldfile ldstruct;
    struct timeval load_timeout;
 
    bzero ((char *) &ldstruct, sizeof (ldstruct));

    /* We invoke clnt_call () here directly, instead of through
       net_clnt_call (), because we need to set a large timeout value.
       The load on the target side can take quite a while, easily
       more than 10 seconds.  The user can kill this call by typing
       CTRL-C if there really is a problem with the load.  

       Do not change the tv_sec value without checking -- select() imposes
       a limit of 10**8 on it for no good reason that I can see...  */

    load_timeout.tv_sec = 99999999;   /* A large number, effectively inf. */
    load_timeout.tv_usec = 0;
 
    status = clnt_call (pClient, VX_LOAD, xdr_wrapstring, &filename, xdr_ldfile,
			&ldstruct, load_timeout);

    if (status == RPC_SUCCESS)
      {
        if (*ldstruct.name == 0)	/* load failed on VxWorks side */
          return -1;
	*pTextAddr = ldstruct.txt_addr;
	*pDataAddr = ldstruct.data_addr;
	*pBssAddr = ldstruct.bss_addr;
	return 0;
      }
    else
        return -1;
    }
      
/* returns 0 if successful, errno if RPC failed or VxWorks complains. */

static int
net_break (addr, procnum)
    int addr;
    u_long procnum;
    {
    enum clnt_stat status;
    int break_status;
    Rptrace ptrace_in;  /* XXX This is stupid.  It doesn't need to be a ptrace
                           structure.  How about something smaller? */

    bzero ((char *) &ptrace_in, sizeof (ptrace_in));
    break_status = 0;

    ptrace_in.addr = addr;
    ptrace_in.pid = inferior_pid;

    status = net_clnt_call (procnum, xdr_rptrace, &ptrace_in, xdr_int,
			    &break_status);

    if (status != RPC_SUCCESS)
	return errno;

    if (break_status == -1)
      return ENOMEM;
    return break_status;	/* probably (FIXME) zero */
    }
 
/* returns 0 if successful, errno otherwise */

static int
vx_insert_breakpoint (addr)
    int addr;
    {
    return net_break (addr, VX_BREAK_ADD);
    }

/* returns 0 if successful, errno otherwise */

static int
vx_remove_breakpoint (addr)
    int addr;
    {
    return net_break (addr, VX_BREAK_DELETE);
    }

/* Start an inferior process and sets inferior_pid to its pid.
   EXEC_FILE is the file to run.
   ALLARGS is a string containing the arguments to the program.
   ENV is the environment vector to pass.
   Returns process id.  Errors reported with error().
   On VxWorks, we ignore exec_file.  */
 
static void
vx_create_inferior (exec_file, args, env)
     char *exec_file;
     char *args;
     char **env;
{
  enum clnt_stat status;
  arg_array passArgs;
  TASK_START taskStart;

  bzero ((char *) &passArgs, sizeof (passArgs));
  bzero ((char *) &taskStart, sizeof (taskStart));

  /* parse arguments, put them in passArgs */

  parse_args (args, &passArgs);

  if (passArgs.arg_array_len == 0)
    error ("You must specify a function name to run, and arguments if any");

  status = net_clnt_call (PROCESS_START, xdr_arg_array, &passArgs,
			  xdr_TASK_START, &taskStart);

  if ((status != RPC_SUCCESS) || (taskStart.status == -1))
    error ("Can't create process on remote target machine");

  /* Save the name of the running function */
  vx_running = savestring (passArgs.arg_array_val[0],
			   strlen (passArgs.arg_array_val[0]));

#ifdef CREATE_INFERIOR_HOOK
  CREATE_INFERIOR_HOOK (pid);
#endif  

  push_target (&vx_run_ops);
  inferior_pid = taskStart.pid;

  /* We will get a trace trap after one instruction.
     Insert breakpoints and continue.  */

  init_wait_for_inferior ();

  /* Set up the "saved terminal modes" of the inferior
     based on what modes we are starting it with.  */
  target_terminal_init ();

  /* Install inferior's terminal modes.  */
  target_terminal_inferior ();

  stop_soon_quietly = 1;
  wait_for_inferior ();		/* Get the task spawn event */
  stop_soon_quietly = 0;

  /* insert_step_breakpoint ();  FIXME, do we need this?  */
  proceed(-1, -1, 0);
}

/* Fill ARGSTRUCT in argc/argv form with the arguments from the
   argument string ARGSTRING.  */

static void
parse_args (arg_string, arg_struct)
     register char *arg_string;
     arg_array *arg_struct;
{
  register int arg_count = 0;	/* number of arguments */
  register int arg_index = 0;
  register char *p0;
 
  bzero ((char *) arg_struct, sizeof (arg_array));
 
  /* first count how many arguments there are */

  p0 = arg_string;
  while (*p0 != '\0')
    {
      if (*(p0 = skip_white_space (p0)) == '\0')
	break;
      p0 = find_white_space (p0);
      arg_count++;
    }

  arg_struct->arg_array_len = arg_count;
  arg_struct->arg_array_val = (char **) xmalloc ((arg_count + 1)
						 * sizeof (char *));

  /* now copy argument strings into arg_struct.  */

  while (*(arg_string = skip_white_space (arg_string)))
    {
      p0 = find_white_space (arg_string);
      arg_struct->arg_array_val[arg_index++] = savestring (arg_string,
							   p0 - arg_string);
      arg_string = p0;
    }

  arg_struct->arg_array_val[arg_count] = NULL;
}

/* Advance a string pointer across whitespace and return a pointer
   to the first non-white character.  */

static char *
skip_white_space (p)
     register char *p;
{
  while (*p == ' ' || *p == '\t')
    p++;
  return p;
}
    
/* Search for the first unquoted whitespace character in a string.
   Returns a pointer to the character, or to the null terminator
   if no whitespace is found.  */

static char *
find_white_space (p)
     register char *p;
{
  register int c;

  while ((c = *p) != ' ' && c != '\t' && c)
    {
      if (c == '\'' || c == '"')
	{
	  while (*++p != c && *p)
	    {
	      if (*p == '\\')
		p++;
	    }
	  if (!*p)
	    break;
	}
      p++;
    }
  return p;
}
    
/* Poll the VxWorks target system for an event related
   to the debugged task.
   Returns -1 if remote wait failed, task status otherwise.  */

static int
net_wait (pEvent)
    RDB_EVENT *pEvent;
{
    int pid;
    enum clnt_stat status;

    bzero ((char *) pEvent, sizeof (RDB_EVENT));

    pid = inferior_pid;
    status = net_clnt_call (PROCESS_WAIT, xdr_int, &pid, xdr_RDB_EVENT, pEvent);

    return (status == RPC_SUCCESS)? pEvent->status: -1;
}
    
/* Suspend the remote task.
   Returns -1 if suspend fails on target system, 0 otherwise.  */

static int
net_quit ()
{
    int pid;
    int quit_status;
    enum clnt_stat status;

    quit_status = 0;

    /* don't let rdbTask suspend itself by passing a pid of 0 */

    if ((pid = inferior_pid) == 0)
	return -1;

    status = net_clnt_call (VX_TASK_SUSPEND, xdr_int, &pid, xdr_int,
			    &quit_status);

    return (status == RPC_SUCCESS)? quit_status: -1;
}

/* Read a register or registers from the remote system.  */

static void
vx_read_register (regno)
     int regno;
{
  int status;
  Rptrace ptrace_in;
  Ptrace_return ptrace_out;
  C_bytes in_data;
  C_bytes out_data;
  extern char registers[];

  bzero ((char *) &ptrace_in, sizeof (ptrace_in));
  bzero ((char *) &ptrace_out, sizeof (ptrace_out));

  /* FIXME, eventually only get the ones we need.  */
  registers_fetched ();
  
  ptrace_in.pid = inferior_pid;
  ptrace_out.info.more_data = (caddr_t) &out_data;
#ifndef I80960
  out_data.len   = 18 * REGISTER_RAW_SIZE (0);		/* FIXME 68k hack */
#else
  out_data.len = (16 + 16 + 3) * REGISTER_RAW_SIZE (0);
#endif
  out_data.bytes = (caddr_t) registers;
  
  status = net_ptrace_clnt_call (PTRACE_GETREGS, &ptrace_in, &ptrace_out);
  if (status)
    error (rpcerr);
  if (ptrace_out.status == -1)
    {
      errno = ptrace_out.errno;
      perror_with_name ("net_ptrace_clnt_call(PTRACE_GETREGS)");
    }
  
#ifdef I80960

  {
    /* If the target has floating point registers, fetch them.
       Otherwise, zero the floating point register values in
       registers[] for good measure, even though we might not
       need to.  */
    /* @@ Can't use this -- the rdb library for the 960 target
       doesn't support setting or retrieving FP regs.  KR  */
#if 0
    struct fp_status inferior_fp_registers;

    if (target_has_fp)
      {
	ptrace_in.pid = inferior_pid;
	ptrace_out.info.more_data = (caddr_t) &inferior_fp_registers;
	status = net_ptrace_clnt_call (PTRACE_GETFPREGS,
				       &ptrace_in, &ptrace_out);
	if (status)
	  error (rpcerr);
	if (ptrace_out.status == -1)
	  {
	    errno = ptrace_out.errno;
	    perror_with_name ("net_ptrace_clnt_call(PTRACE_GETFPREGS)");
	  }

	bcopy (&inferior_fp_registers, &registers[REGISTER_BYTE (FP0_REGNUM)],
	       REGISTER_RAW_SIZE (FP0_REGNUM) * 4);
      }
    else
      {
	bzero ((char *) &registers[REGISTER_BYTE (FP0_REGNUM)],
	       REGISTER_RAW_SIZE (FP0_REGNUM) * 4);
      }
#endif
  }
#else  /* not 960, thus must be 68000:  FIXME!  */

  if (target_has_fp)
    {
      ptrace_in.pid = inferior_pid;
      ptrace_out.info.more_data = (caddr_t) &out_data;
      out_data.len   =  8 * REGISTER_RAW_SIZE (FP0_REGNUM)	/* FIXME */
		     + (3 * sizeof (REGISTER_TYPE));
      out_data.bytes = (caddr_t) &registers[REGISTER_BYTE (FP0_REGNUM)];
  
      status = net_ptrace_clnt_call (PTRACE_GETFPREGS, &ptrace_in, &ptrace_out);
      if (status)
	error (rpcerr);
      if (ptrace_out.status == -1)
	{
	  errno = ptrace_out.errno;
	  perror_with_name ("net_ptrace_clnt_call(PTRACE_GETFPREGS)");
	}
    }
  else
    {
      bzero (&registers[REGISTER_BYTE (FP0_REGNUM)],
      	     8 * REGISTER_RAW_SIZE (FP0_REGNUM));
      bzero (&registers[REGISTER_BYTE (FPC_REGNUM)],
	     3 * sizeof (REGISTER_TYPE));
    }
#endif  /* various architectures */
}

/* Prepare to store registers.  Since we will store all of them,
   read out their current values now.  */

static void
vx_prepare_to_store ()
{
  /* Fetch all registers, if any of them are not yet fetched.  */
  read_register_bytes (0, NULL, REGISTER_BYTES);
}


/* Store our register values back into the inferior.
   If REGNO is -1, do this for all registers.
   Otherwise, REGNO specifies which register (so we can save time).  */
   /* FIXME, look at REGNO to save time here */

static void
vx_write_register (regno)
     int regno;
{
  C_bytes in_data;
  C_bytes out_data;
  extern char registers[];
  int status;
  Rptrace ptrace_in;
  Ptrace_return ptrace_out;

  bzero ((char *) &ptrace_in, sizeof (ptrace_in));
  bzero ((char *) &ptrace_out, sizeof (ptrace_out));

  ptrace_in.pid = inferior_pid;
  ptrace_in.info.ttype     = DATA;
  ptrace_in.info.more_data = (caddr_t) &in_data;

  in_data.bytes = registers;

#ifdef I80960

  in_data.len = (16 + 16 + 3) * sizeof (REGISTER_TYPE);

#else  /* not 960 -- assume 68k -- FIXME */

  in_data.len = 18 * sizeof (REGISTER_TYPE);

#endif  /* Different register sets */

  /* XXX change second param to be a proc number */
  status = net_ptrace_clnt_call (PTRACE_SETREGS, &ptrace_in, &ptrace_out);
  if (status)
      error (rpcerr);
  if (ptrace_out.status == -1)
    {
      errno = ptrace_out.errno;
      perror_with_name ("net_ptrace_clnt_call(PTRACE_SETREGS)");
    }

  /* Store floating point registers if the target has them.  */

  if (target_has_fp)
    {
      ptrace_in.pid = inferior_pid;
      ptrace_in.info.ttype     = DATA;
      ptrace_in.info.more_data = (caddr_t) &in_data;


#ifdef I80960
#if 0 /* @@ Not supported by target.  */
      in_data.bytes = &registers[REGISTER_BYTE (FP0_REGNUM)];
      in_data.len = 4 * REGISTER_RAW_SIZE (FP0_REGNUM);
#endif
#else  /* not 960 -- assume 68k -- FIXME */

      in_data.bytes = &registers[REGISTER_BYTE (FP0_REGNUM)];
      in_data.len = (8 * REGISTER_RAW_SIZE (FP0_REGNUM)
                      + (3 * sizeof (REGISTER_TYPE)));

#endif  /* Different register sets */

      status = net_ptrace_clnt_call (PTRACE_SETFPREGS, &ptrace_in, &ptrace_out);
      if (status)
	  error (rpcerr);
      if (ptrace_out.status == -1)
	{
	  errno = ptrace_out.errno;
	  perror_with_name ("net_ptrace_clnt_call(PTRACE_SETFPREGS)");
	}
    }
}

/* Copy LEN bytes to or from remote inferior's memory starting at MEMADDR
   to debugger memory starting at MYADDR.  WRITE is true if writing to the
   inferior.
   Result is the number of bytes written or read (zero if error).  The
   protocol allows us to return a negative count, indicating that we can't
   handle the current address but can handle one N bytes further, but
   vxworks doesn't give us that information.  */

static int
vx_xfer_memory (memaddr, myaddr, len, write, target)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
     int write;
     struct target_ops *target;			/* ignored */
{
  int status;
  Rptrace ptrace_in;
  Ptrace_return ptrace_out;
  C_bytes data;

  bzero ((char *) &ptrace_in, sizeof (ptrace_in));
  bzero ((char *) &ptrace_out, sizeof (ptrace_out));

  ptrace_in.pid = inferior_pid;		/* XXX pid unnecessary for READDATA */
  ptrace_in.addr = (int) memaddr;	/* Where from */
  ptrace_in.data = len;			/* How many bytes */

  if (write)
    {
      ptrace_in.info.ttype     = DATA;
      ptrace_in.info.more_data = (caddr_t) &data;

      data.bytes = (caddr_t) myaddr;	/* Where from */
      data.len   = len;			/* How many bytes (again, for XDR) */

      /* XXX change second param to be a proc number */
      status = net_ptrace_clnt_call (PTRACE_WRITEDATA, &ptrace_in, &ptrace_out);
    }
  else
    {
      ptrace_out.info.more_data = (caddr_t) &data;
      data.bytes = myaddr;		/* Where to */
      data.len   = len;			/* How many (again, for XDR) */

      /* XXX change second param to be a proc number */
      status = net_ptrace_clnt_call (PTRACE_READDATA, &ptrace_in, &ptrace_out);
    }

  if (status)
      error (rpcerr);
  if (ptrace_out.status == -1)
    {
      return 0;		/* No bytes moved */
    }
  return len;		/* Moved *all* the bytes */
}

static void
vx_files_info ()
{
  printf ("\tAttached to host `%s'", vx_host);
  printf (", which has %sfloating point", target_has_fp? "": "no ");
  printf (".\n");
}

static void
vx_run_files_info ()
{
  printf ("\tRunning %s VxWorks process %s", 
	  vx_running? "child": "attached",
	  local_hex_string(inferior_pid));
  if (vx_running)
    printf (", function `%s'", vx_running);
  printf(".\n");
}

static void
vx_resume (step, siggnal)
     int step;
     int siggnal;
{
  int status;
  Rptrace ptrace_in;
  Ptrace_return ptrace_out;

  if (siggnal != 0 && siggnal != stop_signal)
    error ("Cannot send signals to VxWorks processes");

  bzero ((char *) &ptrace_in, sizeof (ptrace_in));
  bzero ((char *) &ptrace_out, sizeof (ptrace_out));

  ptrace_in.pid = inferior_pid;
  ptrace_in.addr = 1;	/* Target side insists on this, or it panics.  */

  /* XXX change second param to be a proc number */
  status = net_ptrace_clnt_call (step? PTRACE_SINGLESTEP: PTRACE_CONT,
				 &ptrace_in, &ptrace_out);
  if (status)
      error (rpcerr);
  if (ptrace_out.status == -1)
    {
      errno = ptrace_out.errno;
      perror_with_name ("Resuming remote process");
    }
}

static void
vx_mourn_inferior ()
{
  pop_target ();		/* Pop back to no-child state */
  generic_mourn_inferior ();
}


/* This function allows the addition of incrementally linked object files.  */

static void
vx_load_command (arg_string, from_tty)
     char* arg_string;
     int from_tty;
{
  CORE_ADDR text_addr;
  CORE_ADDR data_addr;
  CORE_ADDR bss_addr;
  
  if (arg_string == 0)
    error ("The load command takes a file name");

  arg_string = tilde_expand (arg_string);
  make_cleanup (free, arg_string);

  dont_repeat ();

  QUIT;
  immediate_quit++;
  if (net_load (arg_string, &text_addr, &data_addr, &bss_addr) == -1)
    error ("Load failed on target machine");
  immediate_quit--;

  /* FIXME, for now we ignore data_addr and bss_addr.  */
  symbol_file_add (arg_string, from_tty, text_addr, 0, 0, 0);
}

#ifdef FIXME  /* Not ready for prime time */
/* Single step the target program at the source or machine level.
   Takes an error exit if rpc fails.
   Returns -1 if remote single-step operation fails, else 0.  */

static int
net_step ()
{
  enum clnt_stat status;
  int step_status;
  SOURCE_STEP source_step;

  source_step.taskId = inferior_pid;

  if (step_range_end)
    {
      source_step.startAddr = step_range_start;
      source_step.endAddr = step_range_end;
    }
  else
    {
      source_step.startAddr = 0;
      source_step.endAddr = 0;
    }

  status = net_clnt_call (VX_SOURCE_STEP, xdr_SOURCE_STEP, &source_step,
			  xdr_int, &step_status);

  if (status == RPC_SUCCESS)
    return step_status;
  else 
    error (rpcerr);
}
#endif

/* Emulate ptrace using RPC calls to the VxWorks target system.
   Returns nonzero (-1) if RPC status to VxWorks is bad, 0 otherwise.  */

static int
net_ptrace_clnt_call (request, pPtraceIn, pPtraceOut)
    enum ptracereq request;
    Rptrace *pPtraceIn;
    Ptrace_return *pPtraceOut;
{
  enum clnt_stat status;

  status = net_clnt_call (request, xdr_rptrace, pPtraceIn, xdr_ptrace_return,
			  pPtraceOut);

  if (status != RPC_SUCCESS)
      return -1;

  return 0;
}

/* Query the target for the name of the file from which VxWorks was
   booted.  pBootFile is the address of a pointer to the buffer to
   receive the file name; if the pointer pointed to by pBootFile is 
   NULL, memory for the buffer will be allocated by XDR.
   Returns -1 if rpc failed, 0 otherwise.  */

static int
net_get_boot_file (pBootFile)
     char **pBootFile;
{
  enum clnt_stat status;

  status = net_clnt_call (VX_BOOT_FILE_INQ, xdr_void, (char *) 0,
			  xdr_wrapstring, pBootFile);
  return (status == RPC_SUCCESS) ? 0 : -1;
}

/* Fetch a list of loaded object modules from the VxWorks target.
   Returns -1 if rpc failed, 0 otherwise
   There's no way to check if the returned loadTable is correct.
   VxWorks doesn't check it.  */

static int
net_get_symbols (pLoadTable)
     ldtabl *pLoadTable;		/* return pointer to ldtabl here */
{
  enum clnt_stat status;

  bzero ((char *) pLoadTable, sizeof (struct ldtabl));

  status = net_clnt_call (VX_STATE_INQ, xdr_void, 0, xdr_ldtabl, pLoadTable);
  return (status == RPC_SUCCESS) ? 0 : -1;
}

/* Look up a symbol in the VxWorks target's symbol table.
   Returns status of symbol read on target side (0=success, -1=fail)
   Returns -1 and complain()s if rpc fails.  */

struct complaint cant_contact_target =
  {"Lost contact with VxWorks target", 0, 0};

static int
vx_lookup_symbol (name, pAddr)
     char *name;		/* symbol name */
     CORE_ADDR *pAddr;
{
  enum clnt_stat status;
  SYMBOL_ADDR symbolAddr;

  *pAddr = 0;
  bzero ((char *) &symbolAddr, sizeof (symbolAddr));

  status = net_clnt_call (VX_SYMBOL_INQ, xdr_wrapstring, &name,
			  xdr_SYMBOL_ADDR, &symbolAddr);
  if (status != RPC_SUCCESS) {
      complain (&cant_contact_target, 0);
      return -1;
  }

  *pAddr = symbolAddr.addr;
  return symbolAddr.status;
}

/* Check to see if the VxWorks target has a floating point coprocessor.
   Returns 1 if target has floating point processor, 0 otherwise.
   Calls error() if rpc fails.  */

static int
net_check_for_fp ()
{
  enum clnt_stat status;
  bool_t fp = 0;	/* true if fp processor is present on target board */

  status = net_clnt_call (VX_FP_INQUIRE, xdr_void, 0, xdr_bool, &fp);
  if (status != RPC_SUCCESS)
      error (rpcerr);

   return (int) fp;
}

/* Establish an RPC connection with the VxWorks target system.
   Calls error () if unable to establish connection.  */

static void
net_connect (host)
     char *host;
{
  struct sockaddr_in destAddr;
  struct hostent *destHost;

  /* get the internet address for the given host */

  if ((destHost = (struct hostent *) gethostbyname (host)) == NULL)
      error ("Invalid hostname.  Couldn't find remote host address.");

  bzero (&destAddr, sizeof (destAddr));

  destAddr.sin_addr.s_addr = * (u_long *) destHost->h_addr;
  destAddr.sin_family      = AF_INET;
  destAddr.sin_port        = 0;	/* set to actual port that remote
			           ptrace is listening on.  */

  /* Create a tcp client transport on which to issue
     calls to the remote ptrace server.  */

  ptraceSock = RPC_ANYSOCK;
  pClient = clnttcp_create (&destAddr, RDBPROG, RDBVERS, &ptraceSock, 0, 0);
  /* FIXME, here is where we deal with different version numbers of the proto */
  
  if (pClient == NULL)
    {
      clnt_pcreateerror ("\tnet_connect");
      error ("Couldn't connect to remote target.");
    }
}

/* Sleep for the specified number of milliseconds 
 * (assumed to be less than 1000).
 * If select () is interrupted, returns immediately;
 * takes an error exit if select () fails for some other reason.
 */

static void
sleep_ms (ms)
     long ms;
{
  struct timeval select_timeout;
  int status;

  select_timeout.tv_sec = 0;
  select_timeout.tv_usec = ms * 1000;

  status = select (0, (fd_set *) 0, (fd_set *) 0, (fd_set *) 0, &select_timeout);

  if (status < 0 && errno != EINTR)
    perror_with_name ("select");
}

/* Wait for control to return from inferior to debugger.
   If inferior gets a signal, we may decide to start it up again
   instead of returning.  That is why there is a loop in this function.
   When this function actually returns it means the inferior
   should be left stopped and GDB should read more commands.  */

/* For network debugging with VxWorks.
 * VxWorks knows when tasks hit breakpoints, receive signals, exit, etc,
 * so vx_wait() receives this information directly from
 * VxWorks instead of trying to figure out what happenned via a wait() call.
 */

static int
vx_wait (status)
     int *status;
{
  register int pid;
  WAITTYPE w;
  RDB_EVENT rdbEvent;
  int quit_failed;

  do
    {
      /* If CTRL-C is hit during this loop,
	 suspend the inferior process.  */

      quit_failed = 0;
      if (quit_flag)
	{
	  quit_failed = (net_quit () == -1);
	  quit_flag = 0;
	}

      /* If a net_quit () or net_wait () call has failed,
	 allow the user to break the connection with the target.
	 We can't simply error () out of this loop, since the 
	 data structures representing the state of the inferior
	 are in an inconsistent state.  */

      if (quit_failed || net_wait (&rdbEvent) == -1)
	{
	  terminal_ours ();
	  if (query ("Can't %s.  Disconnect from target system? ",
		     (quit_failed) ? "suspend remote task"
		                   : "get status of remote task"))
	    {
	      target_mourn_inferior();
	      error ("Use the \"target\" command to reconnect.");
	    }
	  else
	    {
	      terminal_inferior ();
	      continue;
	    }
	}
      
      pid = rdbEvent.taskId;
      if (pid == 0)
	{
	  sleep_ms (200);	/* FIXME Don't kill the network too badly */
	}
      else if (pid != inferior_pid)
	fatal ("Bad pid for debugged task: %s\n", local_hex_string(pid));
    } while (pid == 0);

  /* FIXME, eventually do more then SIGTRAP on everything...  */
  switch (rdbEvent.eventType)
    {
    case EVENT_EXIT:
      WSETEXIT (w, 0);
      /* FIXME is it possible to distinguish between a
	 XXX   normal vs abnormal exit in VxWorks? */
      break;

    case EVENT_START:		/* Task was just started. */
      WSETSTOP (w, SIGTRAP);
      break;

    case EVENT_STOP:
      WSETSTOP (w, SIGTRAP);
      /* XXX was it stopped by a signal?  act accordingly */
      break;

    case EVENT_BREAK:		/* Breakpoint was hit. */
      WSETSTOP (w, SIGTRAP);
      break;

    case EVENT_SUSPEND:		/* Task was suspended, probably by ^C. */
      WSETSTOP (w, SIGINT);
      break;

    case EVENT_BUS_ERR:		/* Task made evil nasty reference. */
      WSETSTOP (w, SIGBUS);
      break;

    case EVENT_ZERO_DIV:	/* Division by zero */
      WSETSTOP (w, SIGFPE);	/* Like Unix, call it a float exception. */
      break;

    case EVENT_SIGNAL:
      /* The target is not running Unix, and its
	 faults/traces do not map nicely into Unix signals.
	 Make sure they do not get confused with Unix signals
	 by numbering them with values higher than the highest
	 legal Unix signal.  code in the arch-dependent PRINT_RANDOM_SIGNAL
	 routine will interpret the value for wait_for_inferior.  */
      WSETSTOP (w, rdbEvent.sigType + NSIG);
      break;
    } /* switch */
  *status = *(int *)&w;		/* Grumble union wait crap Grumble */
  return pid;
}

static int
symbol_stub (arg)
     char *arg;
{
  symbol_file_command (arg, 0);
  return 1;
}

static int
add_symbol_stub (arg)
     char *arg;
{
  struct ldfile *pLoadFile = (struct ldfile *)arg;

  printf("\t%s: ", pLoadFile->name);
  symbol_file_add (pLoadFile->name, 0, pLoadFile->txt_addr, 0, 0, 0);
  printf ("ok\n");
  return 1;
}
/* Target command for VxWorks target systems.

   Used in vxgdb.  Takes the name of a remote target machine
   running vxWorks and connects to it to initialize remote network
   debugging.  */

static void
vx_open (args, from_tty)
     char *args;
     int from_tty;
{
  extern int close ();
  char *bootFile;
  extern char *source_path;
  struct ldtabl loadTable;
  struct ldfile *pLoadFile;
  int i;
  extern CLIENT *pClient;

  if (!args)
    error_no_arg ("target machine name");

  target_preopen (from_tty);
  
  unpush_target (&vx_ops);
  printf ("Attaching remote machine across net...\n");
  fflush (stdout);

  /* Allow the user to kill the connect attempt by typing ^C.
     Wait until the call to target_has_fp () completes before
     disallowing an immediate quit, since even if net_connect ()
     is successful, the remote debug server might be hung.  */

  immediate_quit++;

  net_connect (args);
  target_has_fp = net_check_for_fp ();
  printf_filtered ("Connected to %s.\n", args);

  immediate_quit--;

  push_target (&vx_ops);

  /* Save a copy of the target host's name.  */
  vx_host = savestring (args, strlen (args));

  /* Find out the name of the file from which the target was booted
     and load its symbol table.  */

  printf_filtered ("Looking in Unix path for all loaded modules:\n");
  bootFile = NULL;
  if (!net_get_boot_file (&bootFile))
    {
      if (*bootFile) {
	printf_filtered ("\t%s: ", bootFile);
	if (catch_errors (symbol_stub, bootFile,
		"Error while reading symbols from boot file:\n"))
	  puts_filtered ("ok\n");
      } else if (from_tty)
	printf ("VxWorks kernel symbols not loaded.\n");
    }
  else
    error ("Can't retrieve boot file name from target machine.");

  clnt_freeres (pClient, xdr_wrapstring, &bootFile);

  if (net_get_symbols (&loadTable) != 0)
    error ("Can't read loaded modules from target machine");

  i = 0-1;
  while (++i < loadTable.tbl_size)
    {
      QUIT;	/* FIXME, avoids clnt_freeres below:  mem leak */
      pLoadFile = &loadTable.tbl_ent [i];
#ifdef WRS_ORIG
  {
    register int desc;
    struct cleanup *old_chain;
    char *fullname = NULL;

    desc = openp (source_path, 0, pLoadFile->name, O_RDONLY, 0, &fullname);
    if (desc < 0)
	perror_with_name (pLoadFile->name);
    old_chain = make_cleanup (close, desc);
    add_file_at_addr (fullname, desc, pLoadFile->txt_addr, pLoadFile->data_addr,
		      pLoadFile->bss_addr);
    do_cleanups (old_chain);
  }
#else
      /* Botches, FIXME:
	 (1)  Searches the PATH, not the source path.
	 (2)  data and bss are assumed to be at the usual offsets from text.  */
      catch_errors (add_symbol_stub, (char *)pLoadFile, (char *)0);
#endif
    }
  printf_filtered ("Done.\n");

  clnt_freeres (pClient, xdr_ldtabl, &loadTable);
}

/* Takes a task started up outside of gdb and ``attaches'' to it.
   This stops it cold in its tracks and allows us to start tracing it.  */

static void
vx_attach (args, from_tty)
     char *args;
     int from_tty;
{
  int pid;
  char *cptr = 0;
  Rptrace ptrace_in;
  Ptrace_return ptrace_out;
  int status;

  if (!args)
    error_no_arg ("process-id to attach");

  pid = strtol (args, &cptr, 0);
  if ((cptr == args) || (*cptr != '\0'))
    error ("Invalid process-id -- give a single number in decimal or 0xhex");

  if (from_tty)
      printf ("Attaching pid %s.\n", local_hex_string(pid));

  bzero ((char *)&ptrace_in,  sizeof (ptrace_in));
  bzero ((char *)&ptrace_out, sizeof (ptrace_out));
  ptrace_in.pid = pid;

  status = net_ptrace_clnt_call (PTRACE_ATTACH, &ptrace_in, &ptrace_out);
  if (status == -1)
    error (rpcerr);
  if (ptrace_out.status == -1)
    {
      errno = ptrace_out.errno;
      perror_with_name ("Attaching remote process");
    }

  /* It worked... */
  push_target (&vx_run_ops);
  inferior_pid = pid;
  vx_running = 0;
}


/* detach_command --
   takes a program previously attached to and detaches it.
   The program resumes execution and will no longer stop
   on signals, etc.  We better not have left any breakpoints
   in the program or it'll die when it hits one.  For this
   to work, it may be necessary for the process to have been
   previously attached.  It *might* work if the program was
   started via the normal ptrace (PTRACE_TRACEME).  */

static void
vx_detach (args, from_tty)
     char *args;
     int from_tty;
{
  Rptrace ptrace_in;
  Ptrace_return ptrace_out;
  int signal = 0;
  int status;

  if (args)
    error ("Argument given to VxWorks \"detach\".");

  if (from_tty)
      printf ("Detaching pid %s.\n", local_hex_string(inferior_pid));

  if (args)		/* FIXME, should be possible to leave suspended */
    signal = atoi (args);
  
  bzero ((char *)&ptrace_in,  sizeof (ptrace_in));
  bzero ((char *)&ptrace_out, sizeof (ptrace_out));
  ptrace_in.pid = inferior_pid;

  status = net_ptrace_clnt_call (PTRACE_DETACH, &ptrace_in, &ptrace_out);
  if (status == -1)
    error (rpcerr);
  if (ptrace_out.status == -1)
    {
      errno = ptrace_out.errno;
      perror_with_name ("Detaching VxWorks process");
    }

  inferior_pid = 0;
  pop_target ();	/* go back to non-executing VxWorks connection */
}

/* vx_kill -- takes a running task and wipes it out.  */

static void
vx_kill ()
{
  Rptrace ptrace_in;
  Ptrace_return ptrace_out;
  int status;

  printf ("Killing pid %s.\n", local_hex_string(inferior_pid));

  bzero ((char *)&ptrace_in,  sizeof (ptrace_in));
  bzero ((char *)&ptrace_out, sizeof (ptrace_out));
  ptrace_in.pid = inferior_pid;

  status = net_ptrace_clnt_call (PTRACE_KILL, &ptrace_in, &ptrace_out);
  if (status == -1)
    error (rpcerr);
  if (ptrace_out.status == -1)
    {
      errno = ptrace_out.errno;
      perror_with_name ("Killing VxWorks process");
    }

  /* If it gives good status, the process is *gone*, no events remain.  */
  inferior_pid = 0;
  pop_target ();	/* go back to non-executing VxWorks connection */
}

/* Clean up from the VxWorks process target as it goes away.  */

static void
vx_proc_close (quitting)
     int quitting;
{
  inferior_pid = 0;		/* No longer have a process.  */
  if (vx_running)
    free (vx_running);
  vx_running = 0;
}

/* Make an RPC call to the VxWorks target.
   Returns RPC status.  */

static enum clnt_stat
net_clnt_call (procNum, inProc, in, outProc, out)
    enum ptracereq procNum;
    xdrproc_t inProc;
    char *in;
    xdrproc_t outProc;
    char *out;
{
  enum clnt_stat status;
  
  status = clnt_call (pClient, procNum, inProc, in, outProc, out, rpcTimeout);

  if (status != RPC_SUCCESS)
      clnt_perrno (status);

  return status;
}

/* Clean up before losing control.  */

static void
vx_close (quitting)
     int quitting;
{
  if (pClient)
    clnt_destroy (pClient);	/* The net connection */
  pClient = 0;

  if (vx_host)
    free (vx_host);		/* The hostname */
  vx_host = 0;
}

/* A vxprocess target should be started via "run" not "target".  */
/*ARGSUSED*/
static void
vx_proc_open (name, from_tty)
     char *name;
     int from_tty;
{
  error ("Use the \"run\" command to start a VxWorks process.");
}

/* Target ops structure for accessing memory and such over the net */

struct target_ops vx_ops = {
	"vxworks", "VxWorks target memory via RPC over TCP/IP",
	"Use VxWorks target memory.  \n\
Specify the name of the machine to connect to.",
	vx_open, vx_close, vx_attach, 0, /* vx_detach, */
	0, 0, /* resume, wait */
	0, 0, /* read_reg, write_reg */
	0, /* prep_to_store, */
	vx_xfer_memory, vx_files_info,
	0, 0, /* insert_breakpoint, remove_breakpoint */
	0, 0, 0, 0, 0,	/* terminal stuff */
	0, /* vx_kill, */
	vx_load_command,
	vx_lookup_symbol,
	vx_create_inferior, 0,  /* mourn_inferior */
	0, /* can_run */
	0, /* notice_signals */
	core_stratum, 0, /* next */
	1, 1, 0, 0, 0,	/* all mem, mem, stack, regs, exec */
	0, 0,			/* Section pointers */
	OPS_MAGIC,		/* Always the last thing */
};

/* Target ops structure for accessing VxWorks child processes over the net */

struct target_ops vx_run_ops = {
	"vxprocess", "VxWorks process",
	"VxWorks process, started by the \"run\" command.",
	vx_proc_open, vx_proc_close, 0, vx_detach, /* vx_attach */
	vx_resume, vx_wait,
	vx_read_register, vx_write_register,
	vx_prepare_to_store,
	vx_xfer_memory, vx_run_files_info,
	vx_insert_breakpoint, vx_remove_breakpoint,
	0, 0, 0, 0, 0,	/* terminal stuff */
	vx_kill,
	vx_load_command,
	vx_lookup_symbol,
	0, vx_mourn_inferior,
	0,  /* can_run */
	0, /* notice_signals */
	process_stratum, 0, /* next */
	0, 1, 1, 1, 1,	/* all mem, mem, stack, regs, exec */
			/* all_mem is off to avoid spurious msg in "i files" */
	0, 0,			/* Section pointers */
	OPS_MAGIC,		/* Always the last thing */
};
/* ==> Remember when reading at end of file, there are two "ops" structs here. */

void
_initialize_vx ()
{
  add_target (&vx_ops);
  add_target (&vx_run_ops);
}
