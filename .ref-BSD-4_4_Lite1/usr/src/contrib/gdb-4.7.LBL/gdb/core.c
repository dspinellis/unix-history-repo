/* Core dump and executable file functions above target vector, for GDB.
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
#include <errno.h>
#include <signal.h>
#include <fcntl.h>
#include "frame.h"  /* required by inferior.h */
#include "inferior.h"
#include "symtab.h"
#include "command.h"
#include "bfd.h"
#include "target.h"
#include "gdbcore.h"

extern char registers[];

/* Hook for `exec_file_command' command to call.  */

void (*exec_file_display_hook) PARAMS ((char *)) = NULL;

/* Binary file diddling handle for the core file.  */

bfd *core_bfd = NULL;


/* Backward compatability with old way of specifying core files.  */

void
core_file_command (filename, from_tty)
     char *filename;
     int from_tty;
{
#ifdef NOCOREFILE
  error ("GDB can't read core files on this machine.");
#else
  extern struct target_ops core_ops;
  struct target_ops *t = &core_ops;

  dont_repeat ();			/* Either way, seems bogus. */

  if (!filename)
    (t->to_detach) (filename, from_tty);
  else
    (t->to_open) (filename, from_tty);
#endif
}


/* Call this to specify the hook for exec_file_command to call back.
   This is called from the x-window display code.  */

void
specify_exec_file_hook (hook)
     void (*hook) PARAMS ((char *));
{
  exec_file_display_hook = hook;
}

/* The exec file must be closed before running an inferior.
   If it is needed again after the inferior dies, it must
   be reopened.  */

void
close_exec_file ()
{
#ifdef FIXME
  if (exec_bfd)
    bfd_tempclose (exec_bfd);
#endif
}

void
reopen_exec_file ()
{
#ifdef FIXME
  if (exec_bfd)
    bfd_reopen (exec_bfd);
#endif
}

/* If we have both a core file and an exec file,
   print a warning if they don't go together.  */

void
validate_files ()
{
  if (exec_bfd && core_bfd)
    {
      if (!core_file_matches_executable_p (core_bfd, exec_bfd))
	warning ("core file may not match specified executable file.");
      else if (bfd_get_mtime(exec_bfd) > bfd_get_mtime(core_bfd))
	warning ("exec file is newer than core file.");
    }
}

/* Return the name of the executable file as a string.
   ERR nonzero means get error if there is none specified;
   otherwise return 0 in that case.  */

char *
get_exec_file (err)
     int err;
{
  if (exec_bfd) return bfd_get_filename(exec_bfd);
  if (!err)     return NULL;

  error ("No executable file specified.\n\
Use the \"file\" or \"exec-file\" command.");
  return NULL;
}


/* Report a memory error with error().  */

void
memory_error (status, memaddr)
     int status;
     CORE_ADDR memaddr;
{

  if (status == EIO)
    {
      /* Actually, address between memaddr and memaddr + len
	 was out of bounds. */
      error ("Cannot access memory at address %s.", local_hex_string(memaddr));
    }
  else
    {
      error ("Error accessing memory address %s: %s.",
	     local_hex_string (memaddr), safe_strerror (status));
    }
}

/* Same as target_read_memory, but report an error if can't read.  */
void
read_memory (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  int status;
  status = target_read_memory (memaddr, myaddr, len);
  if (status != 0)
    memory_error (status, memaddr);
}

/* Same as target_write_memory, but report an error if can't write.  */
void
write_memory (memaddr, myaddr, len)
     CORE_ADDR memaddr;
     char *myaddr;
     int len;
{
  int status;

  status = target_write_memory (memaddr, myaddr, len);
  if (status != 0)
    memory_error (status, memaddr);
}

/* Read an integer from debugged memory, given address and number of bytes.  */

long
read_memory_integer (memaddr, len)
     CORE_ADDR memaddr;
     int len;
{
  char cbuf;
  short sbuf;
  int ibuf;
  long lbuf;

  if (len == sizeof (char))
    {
      read_memory (memaddr, &cbuf, len);
      return cbuf;
    }
  if (len == sizeof (short))
    {
      read_memory (memaddr, (char *)&sbuf, len);
      SWAP_TARGET_AND_HOST (&sbuf, sizeof (short));
      return sbuf;
    }
  if (len == sizeof (int))
    {
      read_memory (memaddr, (char *)&ibuf, len);
      SWAP_TARGET_AND_HOST (&ibuf, sizeof (int));
      return ibuf;
    }
  if (len == sizeof (lbuf))
    {
      read_memory (memaddr, (char *)&lbuf, len);
      SWAP_TARGET_AND_HOST (&lbuf, sizeof (lbuf));
      return lbuf;
    }
  error ("Cannot handle integers of %d bytes.", len);
  return -1;	/* for lint */
}

void
_initialize_core()
{

  add_com ("core-file", class_files, core_file_command,
	   "Use FILE as core dump for examining memory and registers.\n\
No arg means have no core file.  This command has been superseded by the\n\
`target core' and `detach' commands.");
}
