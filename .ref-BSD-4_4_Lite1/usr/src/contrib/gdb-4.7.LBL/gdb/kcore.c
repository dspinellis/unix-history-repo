/* Work with kernel crash dumps and live systems through kvm.
   This module was developed at Lawrence Berkeley Laboratory
   by Steven McCanne (mccanne@ee.lbl.gov).  It is derived from
   the gdb module core.c.

   Copyright 1986, 1987, 1989, 1991 Free Software Foundation, Inc.
   
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

#ifdef KERNELDEBUG

#include <sys/param.h>
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <fcntl.h>
#include "defs.h"
#include "frame.h"  /* required by inferior.h */
#include "inferior.h"
#include "symtab.h"
#include "command.h"
#include "bfd.h"
#include "target.h"
#include "gdbcore.h"

#include "kernel.h"

#include <kvm.h>
#include <sys/stat.h>

/* KVM library handle */

#ifndef KERNEL_XFER_MEMORY
#define KERNEL_XFER_MEMORY kernel_xfer_memory
#endif

static kvm_t *kd;

/* Forward decl */
extern struct target_ops kernel_core_ops;

/* Discard all vestiges of any previous core file
   and mark data and stack spaces as empty.  */

/* ARGSUSED */
void
kernel_core_close (quitting)
	int quitting;
{
	if (kd != 0) {
		kvm_close(kd);
		kd = 0;
	}
	kernel_debugging = 0;
}

/* This routine opens and sets up the core file bfd */

void
kernel_core_open(filename, from_tty)
	char *filename;
	int from_tty;
{
	char *cp;
	struct cleanup *old_chain;
	int ontop;
	char *execfile;
	struct stat stb;
	char *get_exec_file_name();
	
	if (kd != 0) 
		error("kvm already target -- must detach first");

	target_preopen(from_tty);
	if (filename == 0)
		error ("No core file specified.");
	filename = tilde_expand (filename);
	if (filename[0] != '/') {
		cp = concat (current_directory, "/", filename, NULL);
		free(filename);
		filename = cp;
	}
	old_chain = make_cleanup (free, filename);
	execfile = get_exec_file_name();
	if (execfile == 0) 
		error("No executable image specified");

	/*
	 * If we want the active memory file, just use a null arg for kvm.
	 * The SunOS kvm can't read from the default swap device, unless
	 * /dev/mem is indicated with a null pointer.  This has got to be 
	 * a bug.
	 */
	if (stat(filename, &stb) == 0 && S_ISCHR(stb.st_mode))
		filename = 0;

	kd = kvm_open(execfile, filename, (char *)0,
		      write_files ? O_RDWR : O_RDONLY, "gdb");
	if (kd == 0)
		error("Cannot open kernel core image");

	kernel_debugging = 1;
	unpush_target (&kernel_core_ops);
	old_chain = make_cleanup(kernel_core_close, (int)kd);
#ifdef notdef
	validate_files ();
#endif
	ontop = !push_target (&kernel_core_ops);
	discard_cleanups (old_chain);
	
	if (ontop) {
		/* Fetch all registers from core file */
		target_fetch_registers (-1);
		
		/* Now, set up the frame cache, and print the top of stack */
		set_current_frame (create_new_frame (read_register (FP_REGNUM),
						     read_pc ()));
		select_frame (get_current_frame (), 0);
		print_stack_frame (selected_frame, selected_frame_level, 1);
	} else {
		printf (
			"Warning: you won't be able to access this core file until you terminate\n\
your %s; do ``info files''\n", current_target->to_longname);
	}
	/* Machine dependent call to print out panic string etc. */
	kerninfo();
}

void
kernel_core_detach (args, from_tty)
	char *args;
	int from_tty;
{
	if (args)
		error ("Too many arguments");
	unpush_target (&kernel_core_ops);
	if (from_tty)
		printf ("No kernel core file now.\n");
}

static void
kernel_core_files_info (t)
	struct target_ops *t;
{
#ifdef notdef
	struct section_table *p;
	
	printf_filtered ("\t`%s', ", bfd_get_filename(core_bfd));
	wrap_here ("        ");
	printf_filtered ("file type %s.\n", bfd_get_target(core_bfd));
	
	for (p = t->sections; p < t->sections_end; p++) {
		printf_filtered ("\t%s", local_hex_string_custom (p->addr, "08"));
		printf_filtered (" - %s is %s",
				 local_hex_string_custom (p->endaddr, "08"),
				 bfd_section_name (p->bfd, p->sec_ptr));
		if (p->bfd != core_bfd) {
			printf_filtered (" in %s", bfd_get_filename (p->bfd));
		}
		printf_filtered ("\n");
	}
#endif
}

/*
 * Called by the machine dependent module to set a user context.
 * We call kvm_getu() for this desired side effect.
 * BSD kvm doesn't need to do this.
 */
kernel_getu(p)
	u_long *p;
{
#if BSD < 199103 && !defined(ultrix)
        if (kd != 0)
		return (kvm_getu(kd, p) != 0);
#endif
	return (0);
}

int
kernel_xfer_memory(addr, cp, len, write, target)
     CORE_ADDR addr;
     char *cp;
     int len;
     int write;
     struct target_ops *target;
{
	if (write)
		return kvm_write(kd, addr, cp, len);
	else
		return kvm_read(kd, addr, cp, len);
}


/* 
 * In target dependent module.
 */
extern void kernel_core_registers();

struct target_ops kernel_core_ops = {
	"kvm",			/* shortname */
	"Kernel core file",	/* longname */
	"Use a kernel core file as a target.\
  Specify the filename of the core file.", /* doc */
	kernel_core_open,	/* open */
	kernel_core_close,	/* close */
	0,			/* attach */
	kernel_core_detach,	/* detach */
	0,			/* resume */
	0,			/* wait */
	kernel_core_registers,	/* fetch_registers */
	0,			/* store_registers */
	0,			/* prepare_to_store */
	KERNEL_XFER_MEMORY,	/* xfer_memory */
	kernel_core_files_info,	/* files_info */
	0,			/* insert_breakpoint */
	0,			/* remove_breakpoint */
	0,			/* terminal_init */
	0,			/* terminal_inferior */
	0,			/* terminal_ours_for_output */
	0,			/* terminal_ours */
	0,			/* terminal_info */
	0,			/* kill */
	0,			/* load */
	0,			/* lookup_symbol */
	0,			/* create_inferior */
	0,			/* mourn_inferior */
	0,			/* can_run */
	0,			/* notice_signals */
	core_stratum,		/* stratum */
	0,			/* next */
	0,			/* has_all_memory */
	1,			/* has_memory */
	1,			/* has_stack */
	1,			/* has_registers */
	0,			/* has_execution */
	0,			/* sections */
	0,			/* sections_end */
	OPS_MAGIC,		/* magic */
};
#endif

void
_initialize_kernel_core()
{
#ifdef KERNELDEBUG
#ifdef notdef
	
	add_com ("kcore", class_files, core_file_command,
		 "Use FILE as core dump for examining memory and registers.\n\
No arg means have no core file.  This command has been superseded by the\n\
`target core' and `detach' commands.");
#endif
	add_target (&kernel_core_ops);
#endif
}
