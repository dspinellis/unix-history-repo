/* Shared library support for RS/6000 (xcoff) object files, for GDB.
   Copyright 1991, 1992 Free Software Foundation.
   Contributed by IBM Corporation.

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

#include <sys/types.h>
#include <sys/ldr.h>

#include "defs.h"
#include "bfd.h"
#include "xcoffsolib.h"

extern struct symtab *current_source_symtab;
extern int	      current_source_line;

/* The real work of adding a shared library file to the symtab and
   the section list.  */

void
solib_add (arg_string, from_tty, target)
     char *arg_string;
     int from_tty;
     struct target_ops *target;
{	
  char *val;
  struct vmap *vp = vmap;
  struct objfile *obj;
  struct symtab *saved_symtab;
  int saved_line;

  int loaded = 0;			/* true if any shared obj loaded */
  int matched = 0;			/* true if any shared obj matched */

  if (arg_string == 0)
      re_comp (".");
  else if (val = (char *) re_comp (arg_string)) {
      error ("Invalid regexp: %s", val);
  }
  if (!vp || !vp->nxt)
    return;

  /* save current symbol table and line number, in case they get changed
     in symbol loading process. */
 
  saved_symtab = current_source_symtab;
  saved_line = current_source_line;

  /* skip over the first vmap, it is the main program, always loaded. */
  vp = vp->nxt;

  for (; vp; vp = vp->nxt) {

    if (re_exec (vp->name) || (*vp->member && re_exec (vp->member))) {

      matched = 1;

      /* if already loaded, continue with the next one. */
      if (vp->loaded) {
	
	printf ("%s%s%s%s: already loaded.\n",
	  *vp->member ? "(" : "",
	  vp->member,
	  *vp->member ? ") " : "",
	  vp->name);
	continue;
      }

      printf ("Loading  %s%s%s%s...",
	  *vp->member ? "(" : "",
	  vp->member,
	  *vp->member ? ") " : "",
	  vp->name);
      fflush (stdout);

	obj = lookup_objfile_bfd (vp->bfd);
	if (!obj) {
	  warning ("\nObj structure for the shared object not found. Loading failed.");
	  continue;
	}

	syms_from_objfile (obj, 0, 0, 0);
	new_symfile_objfile (obj, 0, 0);
	vmap_symtab (vp, 0, 0);
	printf ("Done.\n");
	loaded = vp->loaded = 1;
    }
  }
  /* if any shared object is loaded, then misc_func_vector needs sorting. */
  if (loaded) {
#if 0
    sort_misc_function_vector ();
#endif
    current_source_symtab = saved_symtab;
    current_source_line = saved_line;

    /* Getting new symbols might change our opinion about what is frameless.
       Is this correct?? FIXME. */
/*    reinit_frame_cache(); */
  }
  else if (!matched)
    printf ("No matching shared object found.\n");
}


/* Return the module name of a given text address. Note that returned buffer
   is not persistent. */

char *
pc_load_segment_name (addr)
CORE_ADDR addr;
{
   static char buffer [BUFSIZ];
   struct vmap *vp = vmap;

   buffer [0] = buffer [1] = '\0';
   for (; vp; vp = vp->nxt)
     if (vp->tstart <= addr && addr < vp->tend) {
	if (*vp->member) {
	  buffer [0] = '(';
	  strcat (&buffer[1], vp->member);
	  strcat (buffer, ")");
	}
	strcat (buffer, vp->name);
	return buffer;
     }
   return "(unknown load module)";
}


solib_info (ldi)
register struct ld_info *ldi;
{

   struct vmap *vp = vmap;

   if (!vp || !vp->nxt) {
     printf("No shared libraries loaded at this time.\n");	
     return;
   }

   /* skip over the first vmap, it is the main program, always loaded. */
   vp = vp->nxt;

   printf ("\
Text Range		Data Range		Syms	Shared Object Library\n");

   for (; vp; vp = vp->nxt) {

     printf ("0x%08x-0x%08x	0x%08x-0x%08x	%s	%s%s%s%s\n",
	vp->tstart, vp->tend,
	vp->dstart, vp->dend,
	vp->loaded ? "Yes" : "No ",
	*vp->member ? "(" : "",
	vp->member,
	*vp->member ? ") " : "",
	vp->name);
   }
}


void
sharedlibrary_command (args, from_tty)
  char *args;
  int from_tty;
{
  dont_repeat();
  solib_add (args, from_tty, (struct target_ops *)0);
}

void
_initialize_solib()
{

  add_com("sharedlibrary", class_files, sharedlibrary_command,
	   "Load shared object library symbols for files matching REGEXP.");
  add_info("sharedlibrary", solib_info, 
	   "Status of loaded shared object libraries");
}
