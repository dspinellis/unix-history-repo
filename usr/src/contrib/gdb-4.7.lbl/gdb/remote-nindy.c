/* Memory-access and commands for remote NINDY process, for GDB.
   Copyright 1990, 1991, 1992 Free Software Foundation, Inc.
   Contributed by Intel Corporation.  Modified from remote.c by Chris Benenati.

GDB is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY.  No author or distributor accepts responsibility to anyone
for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.
Refer to the GDB General Public License for full details.

Everyone is granted permission to copy, modify and redistribute GDB,
but only under the conditions described in the GDB General Public
License.  A copy of this license is supposed to have been given to you
along with GDB so you can know your rights and responsibilities.  It
should be in a file named COPYING.  Among other things, the copyright
notice and this notice must be preserved on all copies.

In other words, go ahead and share GDB, but don't try to stop
anyone else from sharing it farther.  Help stamp out software hoarding!
*/

/*
Except for the data cache routines, this file bears little resemblence
to remote.c.  A new (although similar) protocol has been specified, and
portions of the code are entirely dependent on having an i80960 with a
NINDY ROM monitor at the other end of the line.
*/

/*****************************************************************************
 *
 * REMOTE COMMUNICATION PROTOCOL BETWEEN GDB960 AND THE NINDY ROM MONITOR.
 *
 *
 * MODES OF OPERATION
 * ----- -- ---------
 *	
 * As far as NINDY is concerned, GDB is always in one of two modes: command
 * mode or passthrough mode.
 *
 * In command mode (the default) pre-defined packets containing requests
 * are sent by GDB to NINDY.  NINDY never talks except in reponse to a request.
 *
 * Once the the user program is started, GDB enters passthrough mode, to give
 * the user program access to the terminal.  GDB remains in this mode until
 * NINDY indicates that the program has stopped.
 *
 *
 * PASSTHROUGH MODE
 * ----------- ----
 *
 * GDB writes all input received from the keyboard directly to NINDY, and writes
 * all characters received from NINDY directly to the monitor.
 *
 * Keyboard input is neither buffered nor echoed to the monitor.
 *
 * GDB remains in passthrough mode until NINDY sends a single ^P character,
 * to indicate that the user process has stopped.
 *
 * Note:
 *	GDB assumes NINDY performs a 'flushreg' when the user program stops.
 *
 *
 * COMMAND MODE
 * ------- ----
 *
 * All info (except for message ack and nak) is transferred between gdb
 * and the remote processor in messages of the following format:
 *
 *		<info>#<checksum>
 *
 * where 
 *	#	is a literal character
 *
 *	<info>	ASCII information;  all numeric information is in the
 *		form of hex digits ('0'-'9' and lowercase 'a'-'f').
 *
 *	<checksum>
 *		is a pair of ASCII hex digits representing an 8-bit
 *		checksum formed by adding together each of the
 *		characters in <info>.
 *
 * The receiver of a message always sends a single character to the sender
 * to indicate that the checksum was good ('+') or bad ('-');  the sender
 * re-transmits the entire message over until a '+' is received.
 *
 * In response to a command NINDY always sends back either data or
 * a result code of the form "Xnn", where "nn" are hex digits and "X00"
 * means no errors.  (Exceptions: the "s" and "c" commands don't respond.)
 *
 * SEE THE HEADER OF THE FILE "gdb.c" IN THE NINDY MONITOR SOURCE CODE FOR A
 * FULL DESCRIPTION OF LEGAL COMMANDS.
 *
 * SEE THE FILE "stop.h" IN THE NINDY MONITOR SOURCE CODE FOR A LIST
 * OF STOP CODES.
 *
 ******************************************************************************/

#include "defs.h"
#include <signal.h>
#include <sys/types.h>
#include <setjmp.h>

#include "frame.h"
#include "inferior.h"
#include "target.h"
#include "gdbcore.h"
#include "command.h"
#include "bfd.h"
#include "ieee-float.h"

#include "wait.h"
#include <sys/ioctl.h>
#include <sys/file.h>
#include <ctype.h>
#include "nindy-share/ttycntl.h"
#include "nindy-share/demux.h"
#include "nindy-share/env.h"
#include "nindy-share/stop.h"

extern int unlink();
extern char *getenv();
extern char *mktemp();

extern char *coffstrip();
extern void generic_mourn_inferior ();

extern struct target_ops nindy_ops;
extern jmp_buf to_top_level;
extern FILE *instream;
extern struct ext_format ext_format_i960;	/* i960-tdep.c */

extern char ninStopWhy ();

int nindy_initial_brk;	/* nonzero if want to send an initial BREAK to nindy */
int nindy_old_protocol;	/* nonzero if want to use old protocol */
char *nindy_ttyname;	/* name of tty to talk to nindy on, or null */

#define DLE	'\020'	/* Character NINDY sends to indicate user program has
			 * halted.  */
#define TRUE	1
#define FALSE	0

int nindy_fd = 0;	/* Descriptor for I/O to NINDY  */
static int have_regs = 0;	/* 1 iff regs read since i960 last halted */
static int regs_changed = 0;	/* 1 iff regs were modified since last read */

extern char *exists();

static void
dcache_flush (), dcache_poke (), dcache_init();

static int
dcache_fetch ();

static void
nindy_fetch_registers PARAMS ((int));

static void
nindy_store_registers PARAMS ((int));

/* FIXME, we can probably use the normal terminal_inferior stuff here.
   We have to do terminal_inferior and then set up the passthrough
   settings initially.  Thereafter, terminal_ours and terminal_inferior
   will automatically swap the settings around for us.  */

/* Restore TTY to normal operation */

static TTY_STRUCT orig_tty;	/* TTY attributes before entering passthrough */

static void
restore_tty()
{
	ioctl( 0, TIOCSETN, &orig_tty );
}


/* Recover from ^Z or ^C while remote process is running */

static void (*old_ctrlc)();    /* Signal handlers before entering passthrough */

#ifdef SIGTSTP
static void (*old_ctrlz)();
#endif

static
#ifdef USG
void
#endif
cleanup()
{
	restore_tty();
	signal(SIGINT, old_ctrlc);
#ifdef SIGTSTP
	signal(SIGTSTP, old_ctrlz);
#endif
	error("\n\nYou may need to reset the 80960 and/or reload your program.\n");
}

/* Clean up anything that needs cleaning when losing control.  */

static char *savename;

static void
nindy_close (quitting)
     int quitting;
{
  if (nindy_fd)
    close (nindy_fd);
  nindy_fd = 0;

  if (savename)
    free (savename);
  savename = 0;
}

/* Open a connection to a remote debugger.   
   FIXME, there should be a way to specify the various options that are
   now specified with gdb command-line options.  (baud_rate, old_protocol,
   and initial_brk)  */
void
nindy_open (name, from_tty)
    char *name;		/* "/dev/ttyXX", "ttyXX", or "XX": tty to be opened */
    int from_tty;
{

  if (!name)
    error_no_arg ("serial port device name");

  target_preopen (from_tty);
  
  nindy_close (0);

	have_regs = regs_changed = 0;
	dcache_init();

	/* Allow user to interrupt the following -- we could hang if
	 * there's no NINDY at the other end of the remote tty.
	 */
	immediate_quit++;
	nindy_fd = ninConnect( name, baud_rate? baud_rate: "9600",
			nindy_initial_brk, !from_tty, nindy_old_protocol );
	immediate_quit--;

	if ( nindy_fd < 0 ){
		nindy_fd = 0;
		error( "Can't open tty '%s'", name );
	}

        savename = savestring (name, strlen (name));
	push_target (&nindy_ops);
	target_fetch_registers(-1);
}

/* User-initiated quit of nindy operations.  */

static void
nindy_detach (name, from_tty)
     char *name;
     int from_tty;
{
  if (name)
    error ("Too many arguments");
  pop_target ();
}

static void
nindy_files_info ()
{
  printf("\tAttached to %s at %s bps%s%s.\n", savename,
	 baud_rate? baud_rate: "9600",
	 nindy_old_protocol? " in old protocol": "",
         nindy_initial_brk? " with initial break": "");
}

/******************************************************************************
 * remote_load:
 *	Download an object file to the remote system by invoking the "comm960"
 *	utility.  We look for "comm960" in $G960BIN, $G960BASE/bin, and
 *	DEFAULT_BASE/bin/HOST/bin where
 *		DEFAULT_BASE is defined in env.h, and
 *		HOST must be defined on the compiler invocation line.
 ******************************************************************************/

static void
nindy_load( filename, from_tty )
    char *filename;
    int from_tty;
{
  asection *s;
  /* Can't do unix style forking on a VMS system, so we'll use bfd to do
     all the work for us 
     */

  bfd *file = bfd_openr(filename,0);
  if (!file) 
  {
    perror_with_name(filename);
    return;
  }
  
  if (!bfd_check_format(file, bfd_object)) 
  {
    error("can't prove it's an object file\n");
    return;
  }
  
  for ( s = file->sections; s; s=s->next) 
  {
    if (s->flags & SEC_LOAD) 
    {
      char *buffer = xmalloc(s->_raw_size);
      bfd_get_section_contents(file, s, buffer, 0, s->_raw_size);
      printf("Loading section %s, size %x vma %x\n",
	     s->name, 
	     s->_raw_size,
	     s->vma);
      ninMemPut(s->vma, buffer, s->_raw_size);
      free(buffer);
    }
  }
  bfd_close(file);
}

/* Return the number of characters in the buffer before the first DLE character.
 */

static
int
non_dle( buf, n )
    char *buf;		/* Character buffer; NOT '\0'-terminated */
    int n;		/* Number of characters in buffer */
{
	int i;

	for ( i = 0; i < n; i++ ){
		if ( buf[i] == DLE ){
			break;
		}
	}
	return i;
}

/* Tell the remote machine to resume.  */

void
nindy_resume (step, siggnal)
     int step, siggnal;
{
	if (siggnal != 0 && siggnal != stop_signal)
	  error ("Can't send signals to remote NINDY targets.");

	dcache_flush();
	if ( regs_changed ){
		nindy_store_registers ();
		regs_changed = 0;
	}
	have_regs = 0;
	ninGo( step );
}

/* Wait until the remote machine stops. While waiting, operate in passthrough
 * mode; i.e., pass everything NINDY sends to stdout, and everything from
 * stdin to NINDY.
 *
 * Return to caller, storing status in 'status' just as `wait' would.
 */

static int
nindy_wait( status )
    WAITTYPE *status;
{
	DEMUX_DECL;	/* OS-dependent data needed by DEMUX... macros */
	char buf[500];	/* FIXME, what is "500" here? */
	int i, n;
	unsigned char stop_exit;
	unsigned char stop_code;
	TTY_STRUCT tty;
	long ip_value, fp_value, sp_value;	/* Reg values from stop */


	WSETEXIT( (*status), 0 );

	/* OPERATE IN PASSTHROUGH MODE UNTIL NINDY SENDS A DLE CHARACTER */

	/* Save current tty attributes, set up signals to restore them.
	 */
	ioctl( 0, TIOCGETP, &orig_tty );
	old_ctrlc = signal( SIGINT, cleanup );
#ifdef SIGTSTP
	old_ctrlz = signal( SIGTSTP, cleanup );
#endif

	/* Pass input from keyboard to NINDY as it arrives.
	 * NINDY will interpret <CR> and perform echo.
	 */
	tty = orig_tty;
	TTY_NINDYTERM( tty );
	ioctl( 0, TIOCSETN, &tty );

	while ( 1 ){
		/* Go to sleep until there's something for us on either
		 * the remote port or stdin.
		 */

		DEMUX_WAIT( nindy_fd );

		/* Pass input through to correct place */

		n = DEMUX_READ( 0, buf, sizeof(buf) );
		if ( n ){				/* Input on stdin */
			write( nindy_fd, buf, n );
		}

		n = DEMUX_READ( nindy_fd, buf, sizeof(buf) );
		if ( n ){				/* Input on remote */
			/* Write out any characters in buffer preceding DLE */
			i = non_dle( buf, n );
			if ( i > 0 ){
				write( 1, buf, i );
			}

			if ( i != n ){
				/* There *was* a DLE in the buffer */
				stop_exit = ninStopWhy( &stop_code,
					&ip_value, &fp_value, &sp_value);
				if ( !stop_exit && (stop_code==STOP_SRQ) ){
					immediate_quit++;
					ninSrq();
					immediate_quit--;
				} else {
					/* Get out of loop */
					supply_register (IP_REGNUM, &ip_value);
					supply_register (FP_REGNUM, &fp_value);
					supply_register (SP_REGNUM, &sp_value);
					break;
				}
			}
		}
	}

	signal( SIGINT, old_ctrlc );
#ifdef SIGTSTP
	signal( SIGTSTP, old_ctrlz );
#endif
	restore_tty();

	if ( stop_exit ){			/* User program exited */
		WSETEXIT( (*status), stop_code );
	} else {				/* Fault or trace */
		switch (stop_code){
		case STOP_GDB_BPT:
		case TRACE_STEP:
			/* Make it look like a VAX trace trap */
			stop_code = SIGTRAP;
			break;
		default:
			/* The target is not running Unix, and its
			   faults/traces do not map nicely into Unix signals.
			   Make sure they do not get confused with Unix signals
			   by numbering them with values higher than the highest
			   legal Unix signal.  code in i960_print_fault(),
			   called via PRINT_RANDOM_SIGNAL, will interpret the
			   value.  */
			stop_code += NSIG;
			break;
		}
		WSETSTOP( (*status), stop_code );
	}
	return inferior_pid;
}

/* Read the remote registers into the block REGS.  */

/* This is the block that ninRegsGet and ninRegsPut handles.  */
struct nindy_regs {
  char	local_regs[16 * 4];
  char	global_regs[16 * 4];
  char	pcw_acw[2 * 4];
  char	ip[4];
  char	tcw[4];
  char	fp_as_double[4 * 8];
};

static void
nindy_fetch_registers(regno)
     int regno;
{
  struct nindy_regs nindy_regs;
  int regnum, inv;
  double dub;

  immediate_quit++;
  ninRegsGet( (char *) &nindy_regs );
  immediate_quit--;

  bcopy (nindy_regs.local_regs, &registers[REGISTER_BYTE (R0_REGNUM)], 16*4);
  bcopy (nindy_regs.global_regs, &registers[REGISTER_BYTE (G0_REGNUM)], 16*4);
  bcopy (nindy_regs.pcw_acw, &registers[REGISTER_BYTE (PCW_REGNUM)], 2*4);
  bcopy (nindy_regs.ip, &registers[REGISTER_BYTE (IP_REGNUM)], 1*4);
  bcopy (nindy_regs.tcw, &registers[REGISTER_BYTE (TCW_REGNUM)], 1*4);
  for (regnum = FP0_REGNUM; regnum < FP0_REGNUM + 4; regnum++) {
    dub = unpack_double (builtin_type_double,
			 &nindy_regs.fp_as_double[8 * (regnum - FP0_REGNUM)],
			 &inv);
    /* dub now in host byte order */
    double_to_ieee_extended (&ext_format_i960, &dub,
			     &registers[REGISTER_BYTE (regnum)]);
  }

  registers_fetched ();
}

static void
nindy_prepare_to_store()
{
  /* Fetch all regs if they aren't already here.  */
  read_register_bytes (0, NULL, REGISTER_BYTES);
}

static void
nindy_store_registers(regno)
     int regno;
{
  struct nindy_regs nindy_regs;
  int regnum, inv;
  double dub;

  bcopy (&registers[REGISTER_BYTE (R0_REGNUM)], nindy_regs.local_regs,  16*4);
  bcopy (&registers[REGISTER_BYTE (G0_REGNUM)], nindy_regs.global_regs, 16*4);
  bcopy (&registers[REGISTER_BYTE (PCW_REGNUM)], nindy_regs.pcw_acw,     2*4);
  bcopy (&registers[REGISTER_BYTE (IP_REGNUM)], nindy_regs.ip,           1*4);
  bcopy (&registers[REGISTER_BYTE (TCW_REGNUM)], nindy_regs.tcw,         1*4);
  /* Float regs.  Only works on IEEE_FLOAT hosts.  */
  for (regnum = FP0_REGNUM; regnum < FP0_REGNUM + 4; regnum++) {
    ieee_extended_to_double (&ext_format_i960,
			     &registers[REGISTER_BYTE (regnum)], &dub);
    /* dub now in host byte order */
    /* FIXME-someday, the arguments to unpack_double are backward.
       It expects a target double and returns a host; we pass the opposite.
       This mostly works but not quite.  */
    dub = unpack_double (builtin_type_double, &dub, &inv);
    /* dub now in target byte order */
    bcopy ((char *)&dub, &nindy_regs.fp_as_double[8 * (regnum - FP0_REGNUM)],
	8);
  }

  immediate_quit++;
  ninRegsPut( (char *) &nindy_regs );
  immediate_quit--;
}

/* Read a word from remote address ADDR and return it.
 * This goes through the data cache.
 */
int
nindy_fetch_word (addr)
     CORE_ADDR addr;
{
	return dcache_fetch (addr);
}

/* Write a word WORD into remote address ADDR.
   This goes through the data cache.  */

void
nindy_store_word (addr, word)
     CORE_ADDR addr;
     int word;
{
	dcache_poke (addr, word);
}

/* Copy LEN bytes to or from inferior's memory starting at MEMADDR
   to debugger memory starting at MYADDR.   Copy to inferior if
   WRITE is nonzero.  Returns the length copied.

   This is stolen almost directly from infptrace.c's child_xfer_memory,
   which also deals with a word-oriented memory interface.  Sometime,
   FIXME, rewrite this to not use the word-oriented routines.  */

int
nindy_xfer_inferior_memory(memaddr, myaddr, len, write, target)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
     int write;
     struct target_ops *target;			/* ignored */
{
  register int i;
  /* Round starting address down to longword boundary.  */
  register CORE_ADDR addr = memaddr & - sizeof (int);
  /* Round ending address up; get number of longwords that makes.  */
  register int count
    = (((memaddr + len) - addr) + sizeof (int) - 1) / sizeof (int);
  /* Allocate buffer of that many longwords.  */
  register int *buffer = (int *) alloca (count * sizeof (int));

  if (write)
    {
      /* Fill start and end extra bytes of buffer with existing memory data.  */

      if (addr != memaddr || len < (int)sizeof (int)) {
	/* Need part of initial word -- fetch it.  */
        buffer[0] = nindy_fetch_word (addr);
      }

      if (count > 1)		/* FIXME, avoid if even boundary */
	{
	  buffer[count - 1]
	    = nindy_fetch_word (addr + (count - 1) * sizeof (int));
	}

      /* Copy data to be written over corresponding part of buffer */

      bcopy (myaddr, (char *) buffer + (memaddr & (sizeof (int) - 1)), len);

      /* Write the entire buffer.  */

      for (i = 0; i < count; i++, addr += sizeof (int))
	{
	  errno = 0;
	  nindy_store_word (addr, buffer[i]);
	  if (errno)
	    return 0;
	}
    }
  else
    {
      /* Read all the longwords */
      for (i = 0; i < count; i++, addr += sizeof (int))
	{
	  errno = 0;
	  buffer[i] = nindy_fetch_word (addr);
	  if (errno)
	    return 0;
	  QUIT;
	}

      /* Copy appropriate bytes out of the buffer.  */
      bcopy ((char *) buffer + (memaddr & (sizeof (int) - 1)), myaddr, len);
    }
  return len;
}

/* The data cache records all the data read from the remote machine
   since the last time it stopped.

   Each cache block holds 16 bytes of data
   starting at a multiple-of-16 address.  */

#define DCACHE_SIZE 64		/* Number of cache blocks */

struct dcache_block {
	struct dcache_block *next, *last;
	unsigned int addr;	/* Address for which data is recorded.  */
	int data[4];
};

struct dcache_block dcache_free, dcache_valid;

/* Free all the data cache blocks, thus discarding all cached data.  */ 
static
void
dcache_flush ()
{
  register struct dcache_block *db;

  while ((db = dcache_valid.next) != &dcache_valid)
    {
      remque (db);
      insque (db, &dcache_free);
    }
}

/*
 * If addr is present in the dcache, return the address of the block
 * containing it.
 */
static
struct dcache_block *
dcache_hit (addr)
     unsigned int addr;
{
  register struct dcache_block *db;

  if (addr & 3)
    abort ();

  /* Search all cache blocks for one that is at this address.  */
  db = dcache_valid.next;
  while (db != &dcache_valid)
    {
      if ((addr & 0xfffffff0) == db->addr)
	return db;
      db = db->next;
    }
  return NULL;
}

/*  Return the int data at address ADDR in dcache block DC.  */
static
int
dcache_value (db, addr)
     struct dcache_block *db;
     unsigned int addr;
{
  if (addr & 3)
    abort ();
  return (db->data[(addr>>2)&3]);
}

/* Get a free cache block, put or keep it on the valid list,
   and return its address.  The caller should store into the block
   the address and data that it describes, then remque it from the
   free list and insert it into the valid list.  This procedure
   prevents errors from creeping in if a ninMemGet is interrupted
   (which used to put garbage blocks in the valid list...).  */
static
struct dcache_block *
dcache_alloc ()
{
  register struct dcache_block *db;

  if ((db = dcache_free.next) == &dcache_free)
    {
      /* If we can't get one from the free list, take last valid and put
	 it on the free list.  */
      db = dcache_valid.last;
      remque (db);
      insque (db, &dcache_free);
    }

  remque (db);
  insque (db, &dcache_valid);
  return (db);
}

/* Return the contents of the word at address ADDR in the remote machine,
   using the data cache.  */
static
int
dcache_fetch (addr)
     CORE_ADDR addr;
{
  register struct dcache_block *db;

  db = dcache_hit (addr);
  if (db == 0)
    {
      db = dcache_alloc ();
      immediate_quit++;
      ninMemGet(addr & ~0xf, (unsigned char *)db->data, 16);
      immediate_quit--;
      db->addr = addr & ~0xf;
      remque (db);			/* Off the free list */
      insque (db, &dcache_valid);	/* On the valid list */
    }
  return (dcache_value (db, addr));
}

/* Write the word at ADDR both in the data cache and in the remote machine.  */
static void
dcache_poke (addr, data)
     CORE_ADDR addr;
     int data;
{
  register struct dcache_block *db;

  /* First make sure the word is IN the cache.  DB is its cache block.  */
  db = dcache_hit (addr);
  if (db == 0)
    {
      db = dcache_alloc ();
      immediate_quit++;
      ninMemGet(addr & ~0xf, (unsigned char *)db->data, 16);
      immediate_quit--;
      db->addr = addr & ~0xf;
      remque (db);			/* Off the free list */
      insque (db, &dcache_valid);	/* On the valid list */
    }

  /* Modify the word in the cache.  */
  db->data[(addr>>2)&3] = data;

  /* Send the changed word.  */
  immediate_quit++;
  ninMemPut(addr, (unsigned char *)&data, 4);
  immediate_quit--;
}

/* The cache itself. */
struct dcache_block the_cache[DCACHE_SIZE];

/* Initialize the data cache.  */
static void
dcache_init ()
{
  register i;
  register struct dcache_block *db;

  db = the_cache;
  dcache_free.next = dcache_free.last = &dcache_free;
  dcache_valid.next = dcache_valid.last = &dcache_valid;
  for (i=0;i<DCACHE_SIZE;i++,db++)
    insque (db, &dcache_free);
}


static void
nindy_create_inferior (execfile, args, env)
     char *execfile;
     char *args;
     char **env;
{
  int entry_pt;
  int pid;

  if (args && *args)
    error ("Can't pass arguments to remote NINDY process");

  if (execfile == 0 || exec_bfd == 0)
    error ("No exec file specified");

  entry_pt = (int) bfd_get_start_address (exec_bfd);

  pid = 42;

#ifdef CREATE_INFERIOR_HOOK
  CREATE_INFERIOR_HOOK (pid);
#endif  

/* The "process" (board) is already stopped awaiting our commands, and
   the program is already downloaded.  We just set its PC and go.  */

  inferior_pid = pid;		/* Needed for wait_for_inferior below */

  clear_proceed_status ();

  /* Tell wait_for_inferior that we've started a new process.  */
  init_wait_for_inferior ();

  /* Set up the "saved terminal modes" of the inferior
     based on what modes we are starting it with.  */
  target_terminal_init ();

  /* Install inferior's terminal modes.  */
  target_terminal_inferior ();

  /* insert_step_breakpoint ();  FIXME, do we need this?  */
  proceed ((CORE_ADDR)entry_pt, -1, 0);		/* Let 'er rip... */
}

static void
reset_command(args, from_tty)
     char *args;
     int from_tty;
{
	if ( !nindy_fd ){
	    error( "No target system to reset -- use 'target nindy' command.");
	}
	if ( query("Really reset the target system?",0,0) ){
		send_break( nindy_fd );
		tty_flush( nindy_fd );
	}
}

void
nindy_kill (args, from_tty)
     char *args;
     int from_tty;
{
  return;		/* Ignore attempts to kill target system */
}

/* Clean up when a program exits.

   The program actually lives on in the remote processor's RAM, and may be
   run again without a download.  Don't leave it full of breakpoint
   instructions.  */

void
nindy_mourn_inferior ()
{
  remove_breakpoints ();
  generic_mourn_inferior ();	/* Do all the proper things now */
}

/* This routine is run as a hook, just before the main command loop is
   entered.  If gdb is configured for the i960, but has not had its
   nindy target specified yet, this will loop prompting the user to do so.

   Unlike the loop provided by Intel, we actually let the user get out
   of this with a RETURN.  This is useful when e.g. simply examining
   an i960 object file on the host system.  */

nindy_before_main_loop ()
{
  char ttyname[100];
  char *p, *p2;

  setjmp(to_top_level);
  while (current_target != &nindy_ops) { /* remote tty not specified yet */
	if ( instream == stdin ){
		printf("\nAttach /dev/ttyNN -- specify NN, or \"quit\" to quit:  ");
		fflush( stdout );
	}
	fgets( ttyname, sizeof(ttyname)-1, stdin );

	/* Strip leading and trailing whitespace */
	for ( p = ttyname; isspace(*p); p++ ){
		;
	}
	if ( *p == '\0' ){
		return;		/* User just hit spaces or return, wants out */
	}
	for ( p2= p; !isspace(*p2) && (*p2 != '\0'); p2++ ){
		;
	}
	*p2= '\0';
	if ( !strcmp("quit",p) ){
		exit(1);
	}

	nindy_open( p, 1 );

	/* Now that we have a tty open for talking to the remote machine,
	   download the executable file if one was specified.  */
	if ( !setjmp(to_top_level) && exec_bfd ) {
	      target_load (bfd_get_filename (exec_bfd), 1);
	}
  }
}

/* Define the target subroutine names */

struct target_ops nindy_ops = {
	"nindy", "Remote serial target in i960 NINDY-specific protocol",
	"Use a remote i960 system running NINDY connected by a serial line.\n\
Specify the name of the device the serial line is connected to.\n\
The speed (baud rate), whether to use the old NINDY protocol,\n\
and whether to send a break on startup, are controlled by options\n\
specified when you started GDB.",
	nindy_open, nindy_close,
	0, nindy_detach, nindy_resume, nindy_wait,
	nindy_fetch_registers, nindy_store_registers,
	nindy_prepare_to_store,
	nindy_xfer_inferior_memory, nindy_files_info,
	0, 0, /* insert_breakpoint, remove_breakpoint, */
	0, 0, 0, 0, 0,	/* Terminal crud */
	nindy_kill,
	nindy_load,
	0, /* lookup_symbol */
	nindy_create_inferior,
	nindy_mourn_inferior,
	0,		/* can_run */
	0, /* notice_signals */
	process_stratum, 0, /* next */
	1, 1, 1, 1, 1,	/* all mem, mem, stack, regs, exec */
	0, 0,			/* Section pointers */
	OPS_MAGIC,		/* Always the last thing */
};

void
_initialize_nindy ()
{
  add_target (&nindy_ops);
  add_com ("reset", class_obscure, reset_command,
	   "Send a 'break' to the remote target system.\n\
Only useful if the target has been equipped with a circuit\n\
to perform a hard reset when a break is detected.");
}
