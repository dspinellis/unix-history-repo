/* Generic symbol file reading for the GNU debugger, GDB.
   Copyright 1990, 1991, 1992 Free Software Foundation, Inc.
   Contributed by Cygnus Support, using pieces from other GDB modules.

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
#include "symtab.h"
#include "gdbtypes.h"
#include "gdbcore.h"
#include "frame.h"
#include "target.h"
#include "value.h"
#include "symfile.h"
#include "objfiles.h"
#include "gdbcmd.h"
#include "breakpoint.h"
#include "language.h"

#include <obstack.h>
#include <assert.h>

#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <ctype.h>

/* Global variables owned by this file */

int readnow_symbol_files;		/* Read full symbols immediately */

/* External variables and functions referenced. */

extern int info_verbose;

/* Functions this file defines */

static void
set_initial_language PARAMS ((void));

static void
load_command PARAMS ((char *, int));

static void
add_symbol_file_command PARAMS ((char *, int));

static void
cashier_psymtab PARAMS ((struct partial_symtab *));

static int
compare_psymbols PARAMS ((const void *, const void *));

static int
compare_symbols PARAMS ((const void *, const void *));

static bfd *
symfile_bfd_open PARAMS ((char *));

static void
find_sym_fns PARAMS ((struct objfile *));

void
clear_symtab_users_once PARAMS ((void));

/* List of all available sym_fns.  On gdb startup, each object file reader
   calls add_symtab_fns() to register information on each format it is
   prepared to read. */

static struct sym_fns *symtab_fns = NULL;

/* Structures with which to manage partial symbol allocation.  */

struct psymbol_allocation_list global_psymbols = {0}, static_psymbols = {0};

/* Flag for whether user will be reloading symbols multiple times.
   Defaults to ON for VxWorks, otherwise OFF.  */

#ifdef SYMBOL_RELOADING_DEFAULT
int symbol_reloading = SYMBOL_RELOADING_DEFAULT;
#else
int symbol_reloading = 0;
#endif

/* Structure to manage complaints about symbol file contents.  */

struct complaint complaint_root[1] = {
  {(char *) 0, 0, complaint_root},
};

/* Some actual complaints.  */

struct complaint oldsyms_complaint = {
	"Replacing old symbols for `%s'", 0, 0 };

struct complaint empty_symtab_complaint = {
	"Empty symbol table found for `%s'", 0, 0 };


/* In the following sort, we always make sure that
   register debug symbol declarations always come before regular
   debug symbol declarations (as might happen when parameters are
   then put into registers by the compiler).

   Since this function is called from within qsort, in an ANSI environment
   it must conform to the prototype for qsort, which specifies that the
   comparison function takes two "void *" pointers. */

static int
compare_symbols (s1p, s2p)
     const PTR s1p;
     const PTR s2p;
{
  register struct symbol **s1, **s2;
  register int namediff;

  s1 = (struct symbol **) s1p;
  s2 = (struct symbol **) s2p;

  /* Compare the initial characters.  */
  namediff = SYMBOL_NAME (*s1)[0] - SYMBOL_NAME (*s2)[0];
  if (namediff != 0) return namediff;

  /* If they match, compare the rest of the names.  */
  namediff = strcmp (SYMBOL_NAME (*s1), SYMBOL_NAME (*s2));
  if (namediff != 0) return namediff;

  /* For symbols of the same name, registers should come first.  */
  return ((SYMBOL_CLASS (*s2) == LOC_REGISTER)
	  - (SYMBOL_CLASS (*s1) == LOC_REGISTER));
}

/*

LOCAL FUNCTION

	compare_psymbols -- compare two partial symbols by name

DESCRIPTION

	Given pointer to two partial symbol table entries, compare
	them by name and return -N, 0, or +N (ala strcmp).  Typically
	used by sorting routines like qsort().

NOTES

	Does direct compare of first two characters before punting
	and passing to strcmp for longer compares.  Note that the
	original version had a bug whereby two null strings or two
	identically named one character strings would return the
	comparison of memory following the null byte.

 */

static int
compare_psymbols (s1p, s2p)
     const PTR s1p;
     const PTR s2p;
{
  register char *st1 = SYMBOL_NAME ((struct partial_symbol *) s1p);
  register char *st2 = SYMBOL_NAME ((struct partial_symbol *) s2p);

  if ((st1[0] - st2[0]) || !st1[0])
    {
      return (st1[0] - st2[0]);
    }
  else if ((st1[1] - st2[1]) || !st1[1])
    {
      return (st1[1] - st2[1]);
    }
  else
    {
      return (strcmp (st1 + 2, st2 + 2));
    }
}

void
sort_pst_symbols (pst)
     struct partial_symtab *pst;
{
  /* Sort the global list; don't sort the static list */

  qsort (pst -> objfile -> global_psymbols.list + pst -> globals_offset,
	 pst -> n_global_syms, sizeof (struct partial_symbol),
	 compare_psymbols);
}

/* Call sort_block_syms to sort alphabetically the symbols of one block.  */

void
sort_block_syms (b)
     register struct block *b;
{
  qsort (&BLOCK_SYM (b, 0), BLOCK_NSYMS (b),
	 sizeof (struct symbol *), compare_symbols);
}

/* Call sort_symtab_syms to sort alphabetically
   the symbols of each block of one symtab.  */

void
sort_symtab_syms (s)
     register struct symtab *s;
{
  register struct blockvector *bv;
  int nbl;
  int i;
  register struct block *b;

  if (s == 0)
    return;
  bv = BLOCKVECTOR (s);
  nbl = BLOCKVECTOR_NBLOCKS (bv);
  for (i = 0; i < nbl; i++)
    {
      b = BLOCKVECTOR_BLOCK (bv, i);
      if (BLOCK_SHOULD_SORT (b))
	sort_block_syms (b);
    }
}

void
sort_all_symtab_syms ()
{
  register struct symtab *s;
  register struct objfile *objfile;

  for (objfile = object_files; objfile != NULL; objfile = objfile -> next)
    {
      for (s = objfile -> symtabs; s != NULL; s = s -> next)
	{
	  sort_symtab_syms (s);
	}
    }
}

/* Make a copy of the string at PTR with SIZE characters in the symbol obstack
   (and add a null character at the end in the copy).
   Returns the address of the copy.  */

char *
obsavestring (ptr, size, obstackp)
     char *ptr;
     int size;
     struct obstack *obstackp;
{
  register char *p = (char *) obstack_alloc (obstackp, size + 1);
  /* Open-coded bcopy--saves function call time.
     These strings are usually short.  */
  {
    register char *p1 = ptr;
    register char *p2 = p;
    char *end = ptr + size;
    while (p1 != end)
      *p2++ = *p1++;
  }
  p[size] = 0;
  return p;
}

/* Concatenate strings S1, S2 and S3; return the new string.
   Space is found in the symbol_obstack.  */

char *
obconcat (obstackp, s1, s2, s3)
     struct obstack *obstackp;
     const char *s1, *s2, *s3;
{
  register int len = strlen (s1) + strlen (s2) + strlen (s3) + 1;
  register char *val = (char *) obstack_alloc (obstackp, len);
  strcpy (val, s1);
  strcat (val, s2);
  strcat (val, s3);
  return val;
}

/* Get the symbol table that corresponds to a partial_symtab.
   This is fast after the first time you do it.  In fact, there
   is an even faster macro PSYMTAB_TO_SYMTAB that does the fast
   case inline.  */

struct symtab *
psymtab_to_symtab (pst)
     register struct partial_symtab *pst;
{
  /* If it's been looked up before, return it. */
  if (pst->symtab)
    return pst->symtab;

  /* If it has not yet been read in, read it.  */
  if (!pst->readin)
    { 
      (*pst->read_symtab) (pst);
    }

  return pst->symtab;
}

/* Initialize entry point information for this objfile. */

void
init_entry_point_info (objfile)
     struct objfile *objfile;
{
  /* Save startup file's range of PC addresses to help blockframe.c
     decide where the bottom of the stack is.  */

  if (bfd_get_file_flags (objfile -> obfd) & EXEC_P)
    {
      /* Executable file -- record its entry point so we'll recognize
	 the startup file because it contains the entry point.  */
      objfile -> ei.entry_point = bfd_get_start_address (objfile -> obfd);
    }
  else
    {
      /* Examination of non-executable.o files.  Short-circuit this stuff.  */
      /* ~0 will not be in any file, we hope.  */
      objfile -> ei.entry_point = ~0;
      /* set the startup file to be an empty range.  */
      objfile -> ei.entry_file_lowpc = 0;
      objfile -> ei.entry_file_highpc = 0;
    }
}

/* Remember the lowest-addressed loadable section we've seen.  
   This function is called via bfd_map_over_sections.  */

#if 0 	/* Not used yet */
static void
find_lowest_section (abfd, sect, obj)
     bfd *abfd;
     asection *sect;
     PTR obj;
{
  asection **lowest = (asection **)obj;

  if (0 == (bfd_get_section_flags (abfd, sect) & SEC_LOAD))
    return;
  if (!*lowest)
    *lowest = sect;		/* First loadable section */
  else if (bfd_section_vma (abfd, *lowest) >= bfd_section_vma (abfd, sect))
    *lowest = sect;		/* A lower loadable section */
}
#endif 

/* Process a symbol file, as either the main file or as a dynamically
   loaded file.

   NAME is the file name (which will be tilde-expanded and made
   absolute herein) (but we don't free or modify NAME itself).
   FROM_TTY says how verbose to be.  MAINLINE specifies whether this
   is the main symbol file, or whether it's an extra symbol file such
   as dynamically loaded code.  If !mainline, ADDR is the address
   where the text segment was loaded.  If VERBO, the caller has printed
   a verbose message about the symbol reading (and complaints can be
   more terse about it).  */

void
syms_from_objfile (objfile, addr, mainline, verbo)
     struct objfile *objfile;
     CORE_ADDR addr;
     int mainline;
     int verbo;
{
  struct section_offsets *section_offsets;
  asection *lowest_sect;

  /* There is a distinction between having no symbol table
     (we refuse to read the file, leaving the old set of symbols around)
     and having no debugging symbols in your symbol table (we read
     the file and end up with a mostly empty symbol table).

     FIXME:  This strategy works correctly when the debugging symbols are
     intermixed with "normal" symbols.  However, when the debugging symbols
     are separate, such as with ELF/DWARF, it is perfectly plausible for
     the symbol table to be missing but still have all the DWARF info
     intact.  Thus in general it is wrong to assume that having no symbol
     table implies no debugging information. */

  if (!(bfd_get_file_flags (objfile -> obfd) & HAS_SYMS))
    return;

  init_entry_point_info (objfile);
  find_sym_fns (objfile);

  if (mainline) 
    {
      /* Since no error yet, throw away the old symbol table.  */

      if (symfile_objfile != NULL)
	{
	  free_objfile (symfile_objfile);
	  symfile_objfile = NULL;
	}

      (*objfile -> sf -> sym_new_init) (objfile);
    }

  /* Convert addr into an offset rather than an absolute address.
     We find the lowest address of a loaded segment in the objfile,
     and assume that <addr> is where that got loaded.  Due to historical
     precedent, we warn if that doesn't happen to be the ".text"
     segment.  */

  if (mainline)
    {
      addr = 0;		/* No offset from objfile addresses.  */
    }
  else
    {
      lowest_sect = bfd_get_section_by_name (objfile->obfd, ".text");
#if 0
      lowest_sect = 0;
      bfd_map_over_sections (objfile->obfd, find_lowest_section,
			     (PTR) &lowest_sect);
#endif

      if (lowest_sect == 0)
	warning ("no loadable sections found in added symbol-file %s",
		 objfile->name);
      else if (0 == bfd_get_section_name (objfile->obfd, lowest_sect)
	       || 0 != strcmp(".text",
			      bfd_get_section_name (objfile->obfd, lowest_sect)))
	warning ("Lowest section in %s is %s at 0x%x",
		 objfile->name,
		 bfd_section_name (objfile->obfd, lowest_sect),
		 bfd_section_vma (objfile->obfd, lowest_sect));

      if (lowest_sect)
	addr -= bfd_section_vma (objfile->obfd, lowest_sect);
    }

  /* Initialize symbol reading routines for this objfile, allow complaints to
     appear for this new file, and record how verbose to be, then do the
     initial symbol reading for this file. */

  (*objfile -> sf -> sym_init) (objfile);
  clear_complaints (1, verbo);
  section_offsets = (*objfile -> sf -> sym_offsets) (objfile, addr);
  (*objfile -> sf -> sym_read) (objfile, section_offsets, mainline);

  /* Don't allow char * to have a typename (else would get caddr_t.)  */
  /* Ditto void *.  FIXME should do this for all the builtin types.  */

  TYPE_NAME (lookup_pointer_type (builtin_type_char)) = 0;
  TYPE_NAME (lookup_pointer_type (builtin_type_void)) = 0;

  /* Mark the objfile has having had initial symbol read attempted.  Note
     that this does not mean we found any symbols... */

  objfile -> flags |= OBJF_SYMS;
}

/* Perform required actions immediately after either reading in the initial
   symbols for a new objfile, or mapping in the symbols from a reusable
   objfile. */
   
void
new_symfile_objfile (objfile, mainline, verbo)
     struct objfile *objfile;
     int mainline;
     int verbo;
{
  if (mainline)
    {
      /* OK, make it the "real" symbol file.  */
      symfile_objfile = objfile;
    }

  /* If we have wiped out any old symbol tables, clean up.  */
  clear_symtab_users_once ();

  /* We're done reading the symbol file; finish off complaints.  */
  clear_complaints (0, verbo);

  /* Fixup all the breakpoints that may have been redefined by this
     symbol file. */

  breakpoint_re_set ();
}

/* Process a symbol file, as either the main file or as a dynamically
   loaded file.

   NAME is the file name (which will be tilde-expanded and made
   absolute herein) (but we don't free or modify NAME itself).
   FROM_TTY says how verbose to be.  MAINLINE specifies whether this
   is the main symbol file, or whether it's an extra symbol file such
   as dynamically loaded code.  If !mainline, ADDR is the address
   where the text segment was loaded.

   Upon success, returns a pointer to the objfile that was added.
   Upon failure, jumps back to command level (never returns). */

struct objfile *
symbol_file_add (name, from_tty, addr, mainline, mapped, readnow)
     char *name;
     int from_tty;
     CORE_ADDR addr;
     int mainline;
     int mapped;
     int readnow;
{
  struct objfile *objfile;
  struct partial_symtab *psymtab;
  bfd *abfd;

  /* Open a bfd for the file and then check to see if the file has a
     symbol table.  There is a distinction between having no symbol table
     (we refuse to read the file, leaving the old set of symbols around)
     and having no debugging symbols in the symbol table (we read the file
     and end up with a mostly empty symbol table, but with lots of stuff in
     the minimal symbol table).  We need to make the decision about whether
     to continue with the file before allocating and building a objfile.

     FIXME:  This strategy works correctly when the debugging symbols are
     intermixed with "normal" symbols.  However, when the debugging symbols
     are separate, such as with ELF/DWARF, it is perfectly plausible for
     the symbol table to be missing but still have all the DWARF info
     intact.  Thus in general it is wrong to assume that having no symbol
     table implies no debugging information. */

  abfd = symfile_bfd_open (name);
  if (!(bfd_get_file_flags (abfd) & HAS_SYMS))
    {
      error ("%s has no symbol-table", name);
    }

  if ((have_full_symbols () || have_partial_symbols ())
      && mainline
      && from_tty
      && !query ("Load new symbol table from \"%s\"? ", name))
      error ("Not confirmed.");
      
  /* Getting new symbols may change our opinion about what is
     frameless.  */

  reinit_frame_cache ();

  objfile = allocate_objfile (abfd, mapped);

  /* If the objfile uses a mapped symbol file, and we have a psymtab for
     it, then skip reading any symbols at this time. */

  if ((objfile -> flags & OBJF_MAPPED) && (objfile -> flags & OBJF_SYMS))
    {
      /* We mapped in an existing symbol table file that already has had
	 initial symbol reading performed, so we can skip that part.  Notify
	 the user that instead of reading the symbols, they have been mapped.
	 */
      if (from_tty || info_verbose)
	{
	  printf_filtered ("Mapped symbols for %s...", name);
	  wrap_here ("");
	  fflush (stdout);
	}
      init_entry_point_info (objfile);
      find_sym_fns (objfile);
    }
  else
    {
      /* We either created a new mapped symbol table, mapped an existing
	 symbol table file which has not had initial symbol reading
	 performed, or need to read an unmapped symbol table. */
      if (from_tty || info_verbose)
	{
	  printf_filtered ("Reading symbols from %s...", name);
	  wrap_here ("");
	  fflush (stdout);
	}
      syms_from_objfile (objfile, addr, mainline, from_tty);
    }      

  new_symfile_objfile (objfile, mainline, from_tty);

  /* We now have at least a partial symbol table.  Check to see if the
     user requested that all symbols be read on initial access via either
     the gdb startup command line or on a per symbol file basis.  Expand
     all partial symbol tables for this objfile if so. */

  if (readnow || readnow_symbol_files)
    {
      if (from_tty || info_verbose)
	{
	  printf_filtered ("expanding to full symbols...");
	  wrap_here ("");
	  fflush (stdout);
	}

      for (psymtab = objfile -> psymtabs;
	   psymtab != NULL;
	   psymtab = psymtab -> next)
	{
	  psymtab_to_symtab (psymtab);
	}
    }

  if (from_tty || info_verbose)
    {
      printf_filtered ("done.\n");
      fflush (stdout);
    }

  return (objfile);
}

/* This is the symbol-file command.  Read the file, analyze its symbols,
   and add a struct symtab to a symtab list.  */

void
symbol_file_command (args, from_tty)
     char *args;
     int from_tty;
{
  char **argv;
  char *name = NULL;
  struct cleanup *cleanups;
  int mapped = 0;
  int readnow = 0;

  dont_repeat ();

  if (args == NULL)
    {
      if ((have_full_symbols () || have_partial_symbols ())
	  && from_tty
	  && !query ("Discard symbol table from `%s'? ",
		     symfile_objfile -> name))
	error ("Not confirmed.");
      free_all_objfiles ();
      symfile_objfile = NULL;
      current_source_symtab = NULL;
      current_source_line = 0;
      if (from_tty)
	{
	  printf ("No symbol file now.\n");
	}
    }
  else
    {
      if ((argv = buildargv (args)) == NULL)
	{
	  nomem (0);
	}
      cleanups = make_cleanup (freeargv, (char *) argv);
      while (*argv != NULL)
	{
	  if (strcmp (*argv, "-mapped") == 0)
	    {
	      mapped = 1;
	    }
	  else if (strcmp (*argv, "-readnow") == 0)
	    {
	      readnow = 1;
	    }
	  else if (**argv == '-')
	    {
	      error ("unknown option `%s'", *argv);
	    }
	  else
	    {
	      name = *argv;
	    }
	  argv++;
	}

      if (name == NULL)
	{
	  error ("no symbol file name was specified");
	}
      else
	{
	  symbol_file_add (name, from_tty, (CORE_ADDR)0, 1, mapped, readnow);
	  set_initial_language ();
	}
      do_cleanups (cleanups);
    }
}

/* Set the initial language.

   A better solution would be to record the language in the psymtab when reading
   partial symbols, and then use it (if known) to set the language.  This would
   be a win for formats that encode the language in an easily discoverable place,
   such as DWARF.  For stabs, we can jump through hoops looking for specially
   named symbols or try to intuit the language from the specific type of stabs
   we find, but we can't do that until later when we read in full symbols.
   FIXME.  */

static void
set_initial_language ()
{
  struct partial_symtab *pst;
  enum language lang = language_unknown;  	

  pst = find_main_psymtab ();
  if (pst != NULL)
    {
      if (pst -> filename != NULL)
	{
	  lang = deduce_language_from_filename (pst -> filename);
        }
      if (lang == language_unknown)
	{
	    /* Make C the default language */
	    lang = language_c;
	}
      set_language (lang);
      expected_language = current_language;	/* Don't warn the user */
    }
}

/* Open file specified by NAME and hand it off to BFD for preliminary
   analysis.  Result is a newly initialized bfd *, which includes a newly
   malloc'd` copy of NAME (tilde-expanded and made absolute).
   In case of trouble, error() is called.  */

static bfd *
symfile_bfd_open (name)
     char *name;
{
  bfd *sym_bfd;
  int desc;
  char *absolute_name;

  name = tilde_expand (name);	/* Returns 1st new malloc'd copy */

  /* Look down path for it, allocate 2nd new malloc'd copy.  */
  desc = openp (getenv ("PATH"), 1, name, O_RDONLY, 0, &absolute_name);
  if (desc < 0)
    {
      make_cleanup (free, name);
      perror_with_name (name);
    }
  free (name);			/* Free 1st new malloc'd copy */
  name = absolute_name;		/* Keep 2nd malloc'd copy in bfd */
				/* It'll be freed in free_objfile(). */

  sym_bfd = bfd_fdopenr (name, NULL, desc);
  if (!sym_bfd)
    {
      close (desc);
      make_cleanup (free, name);
      error ("\"%s\": can't open to read symbols: %s.", name,
	     bfd_errmsg (bfd_error));
    }

  if (!bfd_check_format (sym_bfd, bfd_object))
    {
      bfd_close (sym_bfd);	/* This also closes desc */
      make_cleanup (free, name);
      error ("\"%s\": can't read symbols: %s.", name,
	     bfd_errmsg (bfd_error));
    }

  return (sym_bfd);
}

/* Link a new symtab_fns into the global symtab_fns list.  Called on gdb
   startup by the _initialize routine in each object file format reader,
   to register information about each format the the reader is prepared
   to handle. */

void
add_symtab_fns (sf)
     struct sym_fns *sf;
{
  sf->next = symtab_fns;
  symtab_fns = sf;
}


/* Initialize to read symbols from the symbol file sym_bfd.  It either
   returns or calls error().  The result is an initialized struct sym_fns
   in the objfile structure, that contains cached information about the
   symbol file.  */

static void
find_sym_fns (objfile)
     struct objfile *objfile;
{
  struct sym_fns *sf;

  for (sf = symtab_fns; sf != NULL; sf = sf -> next)
    {
      if (strncmp (bfd_get_target (objfile -> obfd),
		    sf -> sym_name, sf -> sym_namelen) == 0)
	{
	  objfile -> sf = sf;
	  return;
	}
    }
  /* XXX What moron thought this error message was cute? */
#ifdef notdef
  error ("I'm sorry, Dave, I can't do that.  Symbol format `%s' unknown.",
	 bfd_get_target (objfile -> obfd));
#else
  error ("Symbol format `%s' unknown.", bfd_get_target (objfile->obfd));
#endif
}

/* This function runs the load command of our current target.  */

static void
load_command (arg, from_tty)
     char *arg;
     int from_tty;
{
  target_load (arg, from_tty);
}

/* This function allows the addition of incrementally linked object files.
   It does not modify any state in the target, only in the debugger.  */

/* ARGSUSED */
static void
add_symbol_file_command (args, from_tty)
     char *args;
     int from_tty;
{
  char *name = NULL;
  CORE_ADDR text_addr;
  char *arg;
  int readnow = 0;
  int mapped = 0;
  
  dont_repeat ();

  if (args == NULL)
    {
      error ("add-symbol-file takes a file name and an address");
    }

  /* Make a copy of the string that we can safely write into. */

  args = strdup (args);
  make_cleanup (free, args);

  /* Pick off any -option args and the file name. */

  while ((*args != '\000') && (name == NULL))
    {
      while (isspace (*args)) {args++;}
      arg = args;
      while ((*args != '\000') && !isspace (*args)) {args++;}
      if (*args != '\000')
	{
	  *args++ = '\000';
	}
      if (*arg != '-')
	{
	  name = arg;
	}
      else if (strcmp (arg, "-mapped") == 0)
	{
	  mapped = 1;
	}
      else if (strcmp (arg, "-readnow") == 0)
	{
	  readnow = 1;
	}
      else
	{
	  error ("unknown option `%s'", arg);
	}
    }

  /* After picking off any options and the file name, args should be
     left pointing at the remainder of the command line, which should
     be the address expression to evaluate. */

  if ((name == NULL) || (*args == '\000') )
    {
      error ("add-symbol-file takes a file name and an address");
    }
  name = tilde_expand (name);
  make_cleanup (free, name);

  text_addr = parse_and_eval_address (args);

  if (!query ("add symbol table from file \"%s\" at text_addr = %s?\n",
	      name, local_hex_string (text_addr)))
    error ("Not confirmed.");

  symbol_file_add (name, 0, text_addr, 0, mapped, readnow);
}

/* Re-read symbols if a symbol-file has changed.  */
void
reread_symbols ()
{
  struct objfile *objfile;
  long new_modtime;
  int reread_one = 0;
  struct stat new_statbuf;
  int res;

  /* With the addition of shared libraries, this should be modified,
     the load time should be saved in the partial symbol tables, since
     different tables may come from different source files.  FIXME.
     This routine should then walk down each partial symbol table
     and see if the symbol table that it originates from has been changed */

the_big_top:
  for (objfile = object_files; objfile; objfile = objfile->next) {
    if (objfile->obfd) {
#ifdef IBM6000_TARGET
     /* If this object is from a shared library, then you should
        stat on the library name, not member name. */

     if (objfile->obfd->my_archive)
       res = stat (objfile->obfd->my_archive->filename, &new_statbuf);
     else
#endif
      res = stat (objfile->name, &new_statbuf);
      if (res != 0) {
	/* FIXME, should use print_sys_errmsg but it's not filtered. */
	printf_filtered ("`%s' has disappeared; keeping its symbols.\n",
			 objfile->name);
	continue;
      }
      new_modtime = new_statbuf.st_mtime;
      if (new_modtime != objfile->mtime) {
	printf_filtered ("`%s' has changed; re-reading symbols.\n",
			 objfile->name);
	/* FIXME, this should use a different command...that would only
 	   affect this objfile's symbols, and would reset objfile->mtime.
                (objfile->mtime = new_modtime;)
 	   HOWEVER, that command isn't written yet -- so call symbol_file_
	   command, and restart the scan from the top, because it munges
	   the object_files list.  */
	symbol_file_command (objfile->name, 0);
	reread_one = 1;
	goto the_big_top;	/* Start over.  */
      }
    }
  }

  if (reread_one)
    breakpoint_re_set ();
}

/* Functions to handle complaints during symbol reading.  */

/* How many complaints about a particular thing should be printed before
   we stop whining about it?  Default is no whining at all, since so many
   systems have ill-constructed symbol files.  */

static unsigned stop_whining = 0;

/* Should each complaint be self explanatory, or should we assume that
   a series of complaints is being produced? 
   case 0:  self explanatory message.
   case 1:  First message of a series that must start off with explanation.
   case 2:  Subsequent message, when user already knows we are reading
            symbols and we can just state our piece.  */

static int complaint_series = 0;

/* Print a complaint about the input symbols, and link the complaint block
   into a chain for later handling.  */

void
complain (complaint, val)
     struct complaint *complaint;
     char *val;
{
  complaint->counter++;
  if (complaint->next == 0) {
    complaint->next = complaint_root->next;
    complaint_root->next = complaint;
  }
  if (complaint->counter > stop_whining)
    return;
  wrap_here ("");

  switch (complaint_series + (info_verbose << 1)) {

  /* Isolated messages, must be self-explanatory.  */
  case 0:
    puts_filtered ("During symbol reading, ");
    wrap_here("");
    printf_filtered (complaint->message, val);
    puts_filtered (".\n");
    break;

  /* First of a series, without `set verbose'.  */
  case 1:
    puts_filtered ("During symbol reading...");
    printf_filtered (complaint->message, val);
    puts_filtered ("...");
    wrap_here("");
    complaint_series++;
    break;

  /* Subsequent messages of a series, or messages under `set verbose'.
     (We'll already have produced a "Reading in symbols for XXX..." message
      and will clean up at the end with a newline.)  */
  default:
    printf_filtered (complaint->message, val);
    puts_filtered ("...");
    wrap_here("");
  }
}

/* Clear out all complaint counters that have ever been incremented.
   If sym_reading is 1, be less verbose about successive complaints,
   since the messages are appearing all together during a command that
   reads symbols (rather than scattered around as psymtabs get fleshed
   out into symtabs at random times).  If noisy is 1, we are in a
   noisy symbol reading command, and our caller will print enough
   context for the user to figure it out.  */

void
clear_complaints (sym_reading, noisy)
     int sym_reading;
     int noisy;
{
  struct complaint *p;

  for (p = complaint_root->next; p != complaint_root; p = p->next)
    p->counter = 0;

  if (!sym_reading && !noisy && complaint_series > 1) {
    /* Terminate previous series, since caller won't.  */
    puts_filtered ("\n");
  }

  complaint_series = sym_reading? 1 + noisy: 0;
}

enum language
deduce_language_from_filename (filename)
     char *filename;
{
  char *c = strrchr (filename, '.');
  
  if (!c) ; /* Get default. */
  else if(!strcmp(c,".mod"))
     return language_m2;
  else if(!strcmp(c,".c"))
     return language_c;
  else if(!strcmp(c,".cc") || !strcmp(c,".C"))
     return language_cplus;

  return language_unknown;		/* default */
}

/* allocate_symtab:

   Allocate and partly initialize a new symbol table.  Return a pointer
   to it.  error() if no space.

   Caller must set these fields:
	LINETABLE(symtab)
	symtab->blockvector
	symtab->dirname
	symtab->free_code
	symtab->free_ptr
	initialize any EXTRA_SYMTAB_INFO
	possibly free_named_symtabs (symtab->filename);
 */

struct symtab *
allocate_symtab (filename, objfile)
     char *filename;
     struct objfile *objfile;
{
  register struct symtab *symtab;

  symtab = (struct symtab *)
    obstack_alloc (&objfile -> symbol_obstack, sizeof (struct symtab));
  memset (symtab, 0, sizeof (*symtab));
  symtab -> filename = obsavestring (filename, strlen (filename),
				     &objfile -> symbol_obstack);
  symtab -> fullname = NULL;
  symtab -> language = deduce_language_from_filename (filename);

  /* Hook it to the objfile it comes from */

  symtab -> objfile = objfile;
  symtab -> next = objfile -> symtabs;
  objfile -> symtabs = symtab;

#ifdef INIT_EXTRA_SYMTAB_INFO
  INIT_EXTRA_SYMTAB_INFO (symtab);
#endif

  return (symtab);
}

struct partial_symtab *
allocate_psymtab (filename, objfile)
     char *filename;
     struct objfile *objfile;
{
  struct partial_symtab *psymtab;

  if (objfile -> free_psymtabs)
    {
      psymtab = objfile -> free_psymtabs;
      objfile -> free_psymtabs = psymtab -> next;
    }
  else
    psymtab = (struct partial_symtab *)
      obstack_alloc (&objfile -> psymbol_obstack,
		     sizeof (struct partial_symtab));

  memset (psymtab, 0, sizeof (struct partial_symtab));
  psymtab -> filename = obsavestring (filename, strlen (filename),
				      &objfile -> psymbol_obstack);
  psymtab -> symtab = NULL;

  /* Hook it to the objfile it comes from */

  psymtab -> objfile = objfile;
  psymtab -> next = objfile -> psymtabs;
  objfile -> psymtabs = psymtab;
  
  return (psymtab);
}


/* clear_symtab_users_once:

   This function is run after symbol reading, or from a cleanup.
   If an old symbol table was obsoleted, the old symbol table
   has been blown away, but the other GDB data structures that may 
   reference it have not yet been cleared or re-directed.  (The old
   symtab was zapped, and the cleanup queued, in free_named_symtab()
   below.)

   This function can be queued N times as a cleanup, or called
   directly; it will do all the work the first time, and then will be a
   no-op until the next time it is queued.  This works by bumping a
   counter at queueing time.  Much later when the cleanup is run, or at
   the end of symbol processing (in case the cleanup is discarded), if
   the queued count is greater than the "done-count", we do the work
   and set the done-count to the queued count.  If the queued count is
   less than or equal to the done-count, we just ignore the call.  This
   is needed because reading a single .o file will often replace many
   symtabs (one per .h file, for example), and we don't want to reset
   the breakpoints N times in the user's face.

   The reason we both queue a cleanup, and call it directly after symbol
   reading, is because the cleanup protects us in case of errors, but is
   discarded if symbol reading is successful.  */

static int clear_symtab_users_queued;
static int clear_symtab_users_done;

void
clear_symtab_users_once ()
{
  /* Enforce once-per-`do_cleanups'-semantics */
  if (clear_symtab_users_queued <= clear_symtab_users_done)
    return;
  clear_symtab_users_done = clear_symtab_users_queued;

  printf ("Resetting debugger state after updating old symbol tables\n");

  /* Someday, we should do better than this, by only blowing away
     the things that really need to be blown.  */
  clear_value_history ();
  clear_displays ();
  clear_internalvars ();
  breakpoint_re_set ();
  set_default_breakpoint (0, 0, 0, 0);
  current_source_symtab = 0;
}

/* Delete the specified psymtab, and any others that reference it.  */

static void
cashier_psymtab (pst)
     struct partial_symtab *pst;
{
  struct partial_symtab *ps, *pprev;
  int i;

  /* Find its previous psymtab in the chain */
  for (ps = pst->objfile->psymtabs; ps; ps = ps->next) {
    if (ps == pst)
      break;
    pprev = ps;
  }

  if (ps) {
    /* Unhook it from the chain.  */
    if (ps == pst->objfile->psymtabs)
      pst->objfile->psymtabs = ps->next;
    else
      pprev->next = ps->next;

    /* FIXME, we can't conveniently deallocate the entries in the
       partial_symbol lists (global_psymbols/static_psymbols) that
       this psymtab points to.  These just take up space until all
       the psymtabs are reclaimed.  Ditto the dependencies list and
       filename, which are all in the psymbol_obstack.  */

    /* We need to cashier any psymtab that has this one as a dependency... */
again:
    for (ps = pst->objfile->psymtabs; ps; ps = ps->next) {
      for (i = 0; i < ps->number_of_dependencies; i++) {
	if (ps->dependencies[i] == pst) {
	  cashier_psymtab (ps);
	  goto again;		/* Must restart, chain has been munged. */
	}
      }
    }
  }
}

/* If a symtab or psymtab for filename NAME is found, free it along
   with any dependent breakpoints, displays, etc.
   Used when loading new versions of object modules with the "add-file"
   command.  This is only called on the top-level symtab or psymtab's name;
   it is not called for subsidiary files such as .h files.

   Return value is 1 if we blew away the environment, 0 if not.
   FIXME.  The return valu appears to never be used.

   FIXME.  I think this is not the best way to do this.  We should
   work on being gentler to the environment while still cleaning up
   all stray pointers into the freed symtab.  */

int
free_named_symtabs (name)
     char *name;
{
#if 0
  /* FIXME:  With the new method of each objfile having it's own
     psymtab list, this function needs serious rethinking.  In particular,
     why was it ever necessary to toss psymtabs with specific compilation
     unit filenames, as opposed to all psymtabs from a particular symbol
     file?  -- fnf
     Well, the answer is that some systems permit reloading of particular
     compilation units.  We want to blow away any old info about these
     compilation units, regardless of which objfiles they arrived in. --gnu.  */

  register struct symtab *s;
  register struct symtab *prev;
  register struct partial_symtab *ps;
  struct blockvector *bv;
  int blewit = 0;

  /* We only wack things if the symbol-reload switch is set.  */
  if (!symbol_reloading)
    return 0;

  /* Some symbol formats have trouble providing file names... */
  if (name == 0 || *name == '\0')
    return 0;

  /* Look for a psymtab with the specified name.  */

again2:
  for (ps = partial_symtab_list; ps; ps = ps->next) {
    if (!strcmp (name, ps->filename)) {
      cashier_psymtab (ps);	/* Blow it away...and its little dog, too.  */
      goto again2;		/* Must restart, chain has been munged */
    }
  }

  /* Look for a symtab with the specified name.  */

  for (s = symtab_list; s; s = s->next)
    {
      if (!strcmp (name, s->filename))
	break;
      prev = s;
    }

  if (s)
    {
      if (s == symtab_list)
	symtab_list = s->next;
      else
	prev->next = s->next;

      /* For now, queue a delete for all breakpoints, displays, etc., whether
	 or not they depend on the symtab being freed.  This should be
	 changed so that only those data structures affected are deleted.  */

      /* But don't delete anything if the symtab is empty.
	 This test is necessary due to a bug in "dbxread.c" that
	 causes empty symtabs to be created for N_SO symbols that
	 contain the pathname of the object file.  (This problem
	 has been fixed in GDB 3.9x).  */

      bv = BLOCKVECTOR (s);
      if (BLOCKVECTOR_NBLOCKS (bv) > 2
	  || BLOCK_NSYMS (BLOCKVECTOR_BLOCK (bv, GLOBAL_BLOCK))
	  || BLOCK_NSYMS (BLOCKVECTOR_BLOCK (bv, STATIC_BLOCK)))
	{
	  complain (&oldsyms_complaint, name);

	  clear_symtab_users_queued++;
	  make_cleanup (clear_symtab_users_once, 0);
	  blewit = 1;
	} else {
	  complain (&empty_symtab_complaint, name);
	}

      free_symtab (s);
    }
  else
    {
      /* It is still possible that some breakpoints will be affected
	 even though no symtab was found, since the file might have
	 been compiled without debugging, and hence not be associated
	 with a symtab.  In order to handle this correctly, we would need
	 to keep a list of text address ranges for undebuggable files.
	 For now, we do nothing, since this is a fairly obscure case.  */
      ;
    }

  /* FIXME, what about the minimal symbol table? */
  return blewit;
#else
  return (0);
#endif
}

/* Allocate and partially fill a partial symtab.  It will be
   completely filled at the end of the symbol list.

   SYMFILE_NAME is the name of the symbol-file we are reading from, and ADDR
   is the address relative to which its symbols are (incremental) or 0
   (normal). */


struct partial_symtab *
start_psymtab_common (objfile, section_offsets,
		      filename, textlow, global_syms, static_syms)
     struct objfile *objfile;
     struct section_offsets *section_offsets;
     char *filename;
     CORE_ADDR textlow;
     struct partial_symbol *global_syms;
     struct partial_symbol *static_syms;
{
  struct partial_symtab *psymtab;

  psymtab = allocate_psymtab (filename, objfile);
  psymtab -> section_offsets = section_offsets;
  psymtab -> textlow = textlow;
  psymtab -> texthigh = psymtab -> textlow;  /* default */
  psymtab -> globals_offset = global_syms - objfile -> global_psymbols.list;
  psymtab -> statics_offset = static_syms - objfile -> static_psymbols.list;
  return (psymtab);
}

/* Debugging versions of functions that are usually inline macros
   (see symfile.h).  */

#if 0		/* Don't quite work nowadays... */

/* Add a symbol with a long value to a psymtab.
   Since one arg is a struct, we pass in a ptr and deref it (sigh).  */

void
add_psymbol_to_list (name, namelength, namespace, class, list, val)
     char *name;
     int namelength;
     enum namespace namespace;
     enum address_class class;
     struct psymbol_allocation_list *list;
     long val;
{
  ADD_PSYMBOL_VT_TO_LIST (name, namelength, namespace, class, (*list), val,
			  SYMBOL_VALUE);
}

/* Add a symbol with a CORE_ADDR value to a psymtab. */

void
add_psymbol_addr_to_list (name, namelength, namespace, class, list, val)
     char *name;
     int namelength;
     enum namespace namespace;
     enum address_class class;
     struct psymbol_allocation_list *list;
     CORE_ADDR val;
{
  ADD_PSYMBOL_VT_TO_LIST (name, namelength, namespace, class, (*list), val,
			  SYMBOL_VALUE_ADDRESS);
}

#endif /* 0 */

void
_initialize_symfile ()
{

  add_com ("symbol-file", class_files, symbol_file_command,
   "Load symbol table from executable file FILE.\n\
The `file' command can also load symbol tables, as well as setting the file\n\
to execute.");

  add_com ("add-symbol-file", class_files, add_symbol_file_command,
   "Load the symbols from FILE, assuming FILE has been dynamically loaded.\n\
The second argument provides the starting address of the file's text.");

  add_com ("load", class_files, load_command,
   "Dynamically load FILE into the running program, and record its symbols\n\
for access from GDB.");

  add_show_from_set
    (add_set_cmd ("complaints", class_support, var_zinteger,
		  (char *)&stop_whining,
	  "Set max number of complaints about incorrect symbols.",
		  &setlist),
     &showlist);

  add_show_from_set
    (add_set_cmd ("symbol-reloading", class_support, var_boolean,
		  (char *)&symbol_reloading,
	  "Set dynamic symbol table reloading multiple times in one run.",
		  &setlist),
     &showlist);

}
