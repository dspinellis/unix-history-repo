/* Machine-dependent code which would otherwise be in core.c
   for GDB, the GNU debugger.  This code is for the HP PA-RISC cpu.
   Copyright 1986, 1987, 1989, 1990, 1991, 1992 Free Software Foundation, Inc.

   Contributed by the Center for Software Science at the
   University of Utah (pa-gdb-bugs@cs.utah.edu).

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

#ifdef USG
#include <sys/types.h>
#endif

#include <sys/param.h>
#include <sys/dir.h>
#include <signal.h>
#include <sys/ioctl.h>
/* #include <fcntl.h>  Can we live without this?  */

#ifndef hpux
#include <a.out.h>
#include <machine/pcb.h>
#include <sys/time.h>
#include "/usr/src/sys/hpux/hpux.h"
#define USRSTACK 0x68FF3000
#else
#include <sys/user.h>		/* After a.out.h  */
#endif

#include <sys/file.h>
#include <sys/stat.h>
#include <sys/ptrace.h>

#ifndef hpux
#undef USIZE
#undef UPAGES

#define USIZE 3
#define UPAGES 7
#endif

extern int errno;

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

extern struct header file_hdr;
extern struct som_exec_auxhdr exec_hdr;

extern int (*core_file_hook)();

#ifdef KERNELDEBUG

extern int kernel_debugging;
extern int kernel_core_file_hook();

#endif

core_file_command (filename, from_tty)
     char *filename;
     int from_tty;
{
  int val;
  extern char registers[];
#ifdef KERNELDEBUG
  struct stat stb;
#endif

  /* Discard all vestiges of any previous core file
     and mark data and stack spaces as empty.  */

  if (corefile)
    free (corefile);
  corefile = 0;
  core_file_hook = 0;

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

#ifdef KERNELDEBUG
      fstat(corechan, &stb);

      if (kernel_debugging) {
	      setup_kernel_debugging();
	      core_file_hook = kernel_core_file_hook;
	      set_kernel_boundaries();
      } else if ((stb.st_mode & S_IFMT) == S_IFCHR &&
		 stb.st_rdev == makedev(2, 1)) {
	      /* looking at /dev/kmem */
	      data_offset = data_start = KERNBASE;
	      data_end = ~0; /* XXX */
	      stack_end = stack_start = data_end;
	      set_kernel_boundaries();
      } else
#endif
      {
	/* HP PA-RISC style corefile. */
#ifndef hpux
	struct hpuxuser u;
#else
	struct user u;
#endif

	unsigned int reg_offset;

	val = myread (corechan, &u, sizeof u);
	if (val < 0)
	  perror_with_name ("Not a core file: reading upage");
	if (val != sizeof u)
	  error ("Not a core file: could only read %d bytes", val);

	/* We are depending on exec_file_command having been called
	   previously to set exec_data_start.  Since the executable
	   and the core file share the same text segment, the address
	   of the data segment will be the same in both.  */
	data_start = exec_data_start;

	data_end = data_start + NBPG * u.u_dsize;
	stack_start = USRSTACK;	/* from sys/param.h */
	stack_end = stack_start + NBPG * u.u_ssize;
	data_offset = NBPG * UPAGES;
	stack_offset = NBPG * (UPAGES + u.u_dsize);

	/* Some machines put an absolute address in here and some put
	   the offset in the upage of the regs.  */
	reg_offset = NBPG * USIZE;
	/* Read the register values out of the core file and store
	   them where `read_register' will find them.  */

	{
	  register int regno;

	  for (regno = 0; regno < NUM_REGS; regno++)
	    {
	      unsigned char buf[MAX_REGISTER_RAW_SIZE];

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
	      if (regno == PCOQ_HEAD_REGNUM || regno == PCOQ_TAIL_REGNUM)
		buf[3] &= ~0x3;
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

      set_current_frame ( create_new_frame (read_register (FP_REGNUM),
					    read_pc ()));
      select_frame (get_current_frame (), 0);
      validate_files ();
    }
  else if (from_tty)
    printf ("No core file now.\n");
}
