/* Read coff symbol tables and convert to internal format, for GDB.
   Design and support routines derived from dbxread.c, and UMAX COFF
   specific routines written 9/1/87 by David D. Johnson, Brown University.
   Revised 11/27/87 ddj@cs.brown.edu
   Copyright (C) 1987, 1988, 1989 Free Software Foundation, Inc.

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
#ifdef COFF_FORMAT
#include "symtab.h"

#ifdef USG
#include <sys/types.h>
#include <fcntl.h>
#endif

#include <a.out.h>
#include <obstack.h>
#include <sys/param.h>
#include <sys/file.h>

static void add_symbol_to_list ();
static void read_coff_symtab ();
static void patch_opaque_types ();
static struct type *decode_function_type ();
static struct type *decode_type ();
static struct type *decode_base_type ();
static struct type *read_enum_type ();
static struct type *read_struct_type ();
static void finish_block ();
static struct blockvector *make_blockvector ();
static struct symbol *process_coff_symbol ();
static int init_stringtab ();
static void free_stringtab ();
static char *getfilename ();
static char *getsymname ();
static int init_lineno ();
static void enter_linenos ();

extern int fclose ();
extern void free_all_symtabs ();
extern void free_all_psymtabs ();


/* Name of source file whose symbol data we are now processing.
   This comes from a symbol named ".file".  */

static char *last_source_file;

/* Core address of start and end of text of current source file.
   This comes from a ".text" symbol where x_nlinno > 0.  */

static CORE_ADDR cur_src_start_addr;
static CORE_ADDR cur_src_end_addr;

/* Core address of the end of the first object file.  */
static CORE_ADDR first_object_file_end;

/* End of the text segment of the executable file,
   as found in the symbol _etext.  */

static CORE_ADDR end_of_text_addr;

/* The addresses of the symbol table stream and number of symbols
   of the object file we are reading (as copied into core).  */

static FILE *nlist_stream_global;
static int nlist_nsyms_global;

/* The file, a.out  and text section headers of the symbol file */

static FILHDR file_hdr;
static SCNHDR text_hdr;
static AOUTHDR aout_hdr;

/* The index in the symbol table of the last coff symbol that was processed.  */

static int symnum;

/* Vector of types defined so far, indexed by their coff symnum.  */

static struct typevector *type_vector;

/* Number of elements allocated for type_vector currently.  */

static int type_vector_length;

/* Vector of line number information.  */

static struct linetable *line_vector;

/* Index of next entry to go in line_vector_index.  */

static int line_vector_index;

/* Last line number recorded in the line vector.  */

static int prev_line_number;

/* Number of elements allocated for line_vector currently.  */

static int line_vector_length;

/* Chain of typedefs of pointers to empty struct/union types.
   They are chained thru the SYMBOL_VALUE.  */

#define HASHSIZE 127
static struct symbol *opaque_type_chain[HASHSIZE];

/* Record the symbols defined for each context in a list.
   We don't create a struct block for the context until we
   know how long to make it.  */

struct pending
{
  struct pending *next;
  struct symbol *symbol;
};

/* Here are the three lists that symbols are put on.  */

struct pending *file_symbols;	/* static at top level, and types */

struct pending *global_symbols;	/* global functions and variables */

struct pending *local_symbols;	/* everything local to lexical context */

/* List of unclosed lexical contexts
   (that will become blocks, eventually).  */

struct context_stack
{
  struct context_stack *next;
  struct pending *locals;
  struct pending_block *old_blocks;
  struct symbol *name;
  CORE_ADDR start_addr;
  int depth;
};

struct context_stack *context_stack;

/* Nonzero if within a function (so symbols should be local,
   if nothing says specifically).  */

int within_function;

/* List of blocks already made (lexical contexts already closed).
   This is used at the end to make the blockvector.  */

struct pending_block
{
  struct pending_block *next;
  struct block *block;
};

struct pending_block *pending_blocks;

extern CORE_ADDR startup_file_start;	/* From blockframe.c */
extern CORE_ADDR startup_file_end;	/* From blockframe.c */

/* File name symbols were loaded from.  */

static char *symfile;

/* Look up a coff type-number index.  Return the address of the slot
   where the type for that index is stored.
   The type-number is in INDEX. 

   This can be used for finding the type associated with that index
   or for associating a new type with the index.  */

static struct type **
coff_lookup_type (index)
     register int index;
{
  if (index >= type_vector_length)
    {
      int old_vector_length = type_vector_length;

      type_vector_length *= 2;
      if (type_vector_length < index) {
	type_vector_length = index * 2;
      }
      type_vector = (struct typevector *)
	xrealloc (type_vector, sizeof (struct typevector)
				+ type_vector_length * sizeof (struct type *));
      bzero (&type_vector->type[ old_vector_length ],
	     (type_vector_length - old_vector_length) * sizeof(struct type *));
    }
  return &type_vector->type[index];
}

/* Make sure there is a type allocated for type number index
   and return the type object.
   This can create an empty (zeroed) type object.  */

static struct type *
coff_alloc_type (index)
     int index;
{
  register struct type **type_addr = coff_lookup_type (index);
  register struct type *type = *type_addr;

  /* If we are referring to a type not known at all yet,
     allocate an empty type for it.
     We will fill it in later if we find out how.  */
  if (type == 0)
    {
      type = (struct type *) obstack_alloc (symbol_obstack,
					    sizeof (struct type));
      bzero (type, sizeof (struct type));
      *type_addr = type;
    }
  return type;
}

/* maintain the lists of symbols and blocks */

/* Add a symbol to one of the lists of symbols.  */
static void
add_symbol_to_list (symbol, listhead)
     struct symbol *symbol;
     struct pending **listhead;
{
  register struct pending *link
    = (struct pending *) xmalloc (sizeof (struct pending));

  link->next = *listhead;
  link->symbol = symbol;
  *listhead = link;
}

/* Take one of the lists of symbols and make a block from it.
   Put the block on the list of pending blocks.  */

static void
finish_block (symbol, listhead, old_blocks, start, end)
     struct symbol *symbol;
     struct pending **listhead;
     struct pending_block *old_blocks;
     CORE_ADDR start, end;
{
  register struct pending *next, *next1;
  register struct block *block;
  register struct pending_block *pblock;
  struct pending_block *opblock;
  register int i;

  /* Count the length of the list of symbols.  */

  for (next = *listhead, i = 0; next; next = next->next, i++);

  block = (struct block *)
	    obstack_alloc (symbol_obstack, sizeof (struct block) + (i - 1) * sizeof (struct symbol *));

  /* Copy the symbols into the block.  */

  BLOCK_NSYMS (block) = i;
  for (next = *listhead; next; next = next->next)
    BLOCK_SYM (block, --i) = next->symbol;

  BLOCK_START (block) = start;
  BLOCK_END (block) = end;
  BLOCK_SUPERBLOCK (block) = 0;	/* Filled in when containing block is made */

  /* Put the block in as the value of the symbol that names it.  */

  if (symbol)
    {
      SYMBOL_BLOCK_VALUE (symbol) = block;
      BLOCK_FUNCTION (block) = symbol;
    }
  else
    BLOCK_FUNCTION (block) = 0;

  /* Now free the links of the list, and empty the list.  */

  for (next = *listhead; next; next = next1)
    {
      next1 = next->next;
      free (next);
    }
  *listhead = 0;

  /* Install this block as the superblock
     of all blocks made since the start of this scope
     that don't have superblocks yet.  */

  opblock = 0;
  for (pblock = pending_blocks; pblock != old_blocks; pblock = pblock->next)
    {
      if (BLOCK_SUPERBLOCK (pblock->block) == 0)
	BLOCK_SUPERBLOCK (pblock->block) = block;
      opblock = pblock;
    }

  /* Record this block on the list of all blocks in the file.
     Put it after opblock, or at the beginning if opblock is 0.
     This puts the block in the list after all its subblocks.  */

  pblock = (struct pending_block *) xmalloc (sizeof (struct pending_block));
  pblock->block = block;
  if (opblock)
    {
      pblock->next = opblock->next;
      opblock->next = pblock;
    }
  else
    {
      pblock->next = pending_blocks;
      pending_blocks = pblock;
    }
}

static struct blockvector *
make_blockvector ()
{
  register struct pending_block *next, *next1;
  register struct blockvector *blockvector;
  register int i;

  /* Count the length of the list of blocks.  */

  for (next = pending_blocks, i = 0; next; next = next->next, i++);

  blockvector = (struct blockvector *)
		  obstack_alloc (symbol_obstack, sizeof (struct blockvector) + (i - 1) * sizeof (struct block *));

  /* Copy the blocks into the blockvector.
     This is done in reverse order, which happens to put
     the blocks into the proper order (ascending starting address).
     finish_block has hair to insert each block into the list
     after its subblocks in order to make sure this is true.  */

  BLOCKVECTOR_NBLOCKS (blockvector) = i;
  for (next = pending_blocks; next; next = next->next)
    BLOCKVECTOR_BLOCK (blockvector, --i) = next->block;

  /* Now free the links of the list, and empty the list.  */

  for (next = pending_blocks; next; next = next1)
    {
      next1 = next->next;
      free (next);
    }
  pending_blocks = 0;

  return blockvector;
}

/* Manage the vector of line numbers.  */

static
record_line (line, pc)
     int line;
     CORE_ADDR pc;
{
  struct linetable_entry *e;
  /* Make sure line vector is big enough.  */

  if (line_vector_index + 2 >= line_vector_length)
    {
      line_vector_length *= 2;
      line_vector = (struct linetable *)
	xrealloc (line_vector, sizeof (struct linetable)
		  + (line_vector_length
		     * sizeof (struct linetable_entry)));
    }

  e = line_vector->item + line_vector_index++;
  e->line = line; e->pc = pc;
}

/* Start a new symtab for a new source file.
   This is called when a COFF ".file" symbol is seen;
   it indicates the start of data for one original source file.  */

static void
start_symtab ()
{
  file_symbols = 0;
  global_symbols = 0;
  context_stack = 0;
  within_function = 0;
  last_source_file = 0;

  /* Initialize the source file information for this file.  */

  line_vector_index = 0;
  line_vector_length = 1000;
  prev_line_number = -2;	/* Force first line number to be explicit */
  line_vector = (struct linetable *)
    xmalloc (sizeof (struct linetable)
	     + line_vector_length * sizeof (struct linetable_entry));
}

/* Save the vital information for use when closing off the current file.
   NAME is the file name the symbols came from, START_ADDR is the first
   text address for the file, and SIZE is the number of bytes of text.  */

static void
complete_symtab (name, start_addr, size)
    char *name;
    CORE_ADDR start_addr;
    unsigned int size;
{
  last_source_file = savestring (name, strlen (name));
  cur_src_start_addr = start_addr;
  cur_src_end_addr = start_addr + size;

  if (aout_hdr.entry < cur_src_end_addr
      && aout_hdr.entry >= cur_src_start_addr)
    {
      startup_file_start = cur_src_start_addr;
      startup_file_end = cur_src_end_addr;
    }
}

/* Finish the symbol definitions for one main source file,
   close off all the lexical contexts for that file
   (creating struct block's for them), then make the
   struct symtab for that file and put it in the list of all such. */

static void
end_symtab ()
{
  register struct symtab *symtab;
  register struct context_stack *cstk;
  register struct blockvector *blockvector;
  register struct linetable *lv;

  /* Finish the lexical context of the last function in the file.  */

  if (context_stack)
    {
      cstk = context_stack;
      context_stack = 0;
      /* Make a block for the local symbols within.  */
      finish_block (cstk->name, &local_symbols, cstk->old_blocks,
		    cstk->start_addr, cur_src_end_addr);
      free (cstk);
    }

  /* Ignore a file that has no functions with real debugging info.  */
  if (pending_blocks == 0 && file_symbols == 0 && global_symbols == 0)
    {
      free (line_vector);
      line_vector = 0;
      line_vector_length = -1;
      last_source_file = 0;
      return;
    }

  /* Create the two top-level blocks for this file.  */
  finish_block (0, &file_symbols, 0, cur_src_start_addr, cur_src_end_addr);
  finish_block (0, &global_symbols, 0, cur_src_start_addr, cur_src_end_addr);

  /* Create the blockvector that points to all the file's blocks.  */
  blockvector = make_blockvector ();

  /* Now create the symtab object for this source file.  */
  symtab = (struct symtab *) xmalloc (sizeof (struct symtab));
  symtab->free_ptr = 0;

  /* Fill in its components.  */
  symtab->blockvector = blockvector;
  symtab->free_code = free_linetable;
  symtab->filename = last_source_file;
  lv = line_vector;
  lv->nitems = line_vector_index;
  symtab->linetable = (struct linetable *)
    xrealloc (lv, (sizeof (struct linetable)
		   + lv->nitems * sizeof (struct linetable_entry)));
  symtab->nlines = 0;
  symtab->line_charpos = 0;

  /* Link the new symtab into the list of such.  */
  symtab->next = symtab_list;
  symtab_list = symtab;

  /* Reinitialize for beginning of new file. */
  line_vector = 0;
  line_vector_length = -1;
  last_source_file = 0;
}

/* Accumulate the misc functions in bunches of 127.
   At the end, copy them all into one newly allocated structure.  */

#define MISC_BUNCH_SIZE 127

struct misc_bunch
{
  struct misc_bunch *next;
  struct misc_function contents[MISC_BUNCH_SIZE];
};

/* Bunch currently being filled up.
   The next field points to chain of filled bunches.  */

static struct misc_bunch *misc_bunch;

/* Number of slots filled in current bunch.  */

static int misc_bunch_index;

/* Total number of misc functions recorded so far.  */

static int misc_count;

static void
init_misc_functions ()
{
  misc_count = 0;
  misc_bunch = 0;
  misc_bunch_index = MISC_BUNCH_SIZE;
}

static void
record_misc_function (name, address)
     char *name;
     CORE_ADDR address;
{
  register struct misc_bunch *new;

  if (misc_bunch_index == MISC_BUNCH_SIZE)
    {
      new = (struct misc_bunch *) xmalloc (sizeof (struct misc_bunch));
      misc_bunch_index = 0;
      new->next = misc_bunch;
      misc_bunch = new;
    }
  misc_bunch->contents[misc_bunch_index].name = savestring (name, strlen (name));
  misc_bunch->contents[misc_bunch_index].address = address;
  misc_bunch->contents[misc_bunch_index].type = mf_unknown;
  misc_bunch_index++;
  misc_count++;
}

/* if we see a function symbol, we do record_misc_function.
 * however, if it turns out the next symbol is '.bf', then
 * we call here to undo the misc definition
 */
static void
unrecord_misc_function ()
{
  if (misc_bunch_index == 0)
    error ("Internal error processing symbol table, at symbol %d.",
	   symnum);
  misc_bunch_index--;
  misc_count--;
}


static int
compare_misc_functions (fn1, fn2)
     struct misc_function *fn1, *fn2;
{
  /* Return a signed result based on unsigned comparisons
     so that we sort into unsigned numeric order.  */
  if (fn1->address < fn2->address)
    return -1;
  if (fn1->address > fn2->address)
    return 1;
  return 0;
}

static void
discard_misc_bunches ()
{
  register struct misc_bunch *next;

  while (misc_bunch)
    {
      next = misc_bunch->next;
      free (misc_bunch);
      misc_bunch = next;
    }
}

static void
condense_misc_bunches ()
{
  register int i, j;
  register struct misc_bunch *bunch;
#ifdef NAMES_HAVE_UNDERSCORE
  int offset = 1;
#else
  int offset = 0;
#endif

  misc_function_vector
    = (struct misc_function *)
      xmalloc (misc_count * sizeof (struct misc_function));

  j = 0;
  bunch = misc_bunch;
  while (bunch)
    {
      for (i = 0; i < misc_bunch_index; i++)
	{
	  register char *tmp;

	  misc_function_vector[j] = bunch->contents[i];
	  tmp = misc_function_vector[j].name;
	  misc_function_vector[j].name = (tmp[0] == '_' ? tmp + offset : tmp);
	  j++;
	}
      bunch = bunch->next;
      misc_bunch_index = MISC_BUNCH_SIZE;
    }

  misc_function_count = j;

  /* Sort the misc functions by address.  */

  qsort (misc_function_vector, j, sizeof (struct misc_function),
	 compare_misc_functions);
}

/* Call sort_syms to sort alphabetically
   the symbols of each block of each symtab.  */

static int
compare_symbols (s1, s2)
     struct symbol **s1, **s2;
{
  /* Names that are less should come first.  */
  register int namediff = strcmp (SYMBOL_NAME (*s1), SYMBOL_NAME (*s2));
  if (namediff != 0) return namediff;
  /* For symbols of the same name, registers should come first.  */
  return ((SYMBOL_CLASS (*s2) == LOC_REGISTER)
	  - (SYMBOL_CLASS (*s1) == LOC_REGISTER));
}

static void
sort_syms ()
{
  register struct symtab *s;
  register int i, nbl;
  register struct blockvector *bv;
  register struct block *b;

  for (s = symtab_list; s; s = s->next)
    {
      bv = BLOCKVECTOR (s);
      nbl = BLOCKVECTOR_NBLOCKS (bv);
      for (i = 0; i < nbl; i++)
	{
	  b = BLOCKVECTOR_BLOCK (bv, i);
	  if (BLOCK_SHOULD_SORT (b))
		  qsort (&BLOCK_SYM (b, 0), BLOCK_NSYMS (b),
			 sizeof (struct symbol *), compare_symbols);
	}
    }
}

/* This is the symbol-file command.  Read the file, analyze its symbols,
   and add a struct symtab to symtab_list.  */

void
symbol_file_command (name)
     char *name;
{
  int desc;
  int num_symbols;
  int num_sections;
  int symtab_offset;
  extern void close ();
  register int val;
  struct cleanup *old_chain;

  dont_repeat ();

  if (name == 0)
    {
      if (symtab_list && !query ("Discard symbol table? ", 0))
	error ("Not confirmed.");
      if (symfile)
	free (symfile);
      symfile = 0;
      free_all_symtabs ();
      return;
    }

  name = tilde_expand (name);
  make_cleanup (free, name);

  if (symtab_list && !query ("Load new symbol table from \"%s\"? ", name))
    error ("Not confirmed.");

  if (symfile)
    free (symfile);
  symfile = 0;

  {
    char *absolute_name;

    desc = openp (getenv ("PATH"), 1, name, O_RDONLY, 0, &absolute_name);
    if (desc < 0)
      perror_with_name (name);
    else
      name = absolute_name;
  }

  old_chain = make_cleanup (close, desc);
  make_cleanup (free_current_contents, &name);

  if ((num_symbols = read_file_hdr (desc, &file_hdr)) < 0)
    error ("File \"%s\" not in executable format.", name);

  /* If an a.out header is present, read it in.  If not (e.g. a .o file)
     deal with its absence.  */
  if (file_hdr.f_opthdr == 0
      || read_aout_hdr (desc, &aout_hdr, file_hdr.f_opthdr) < 0)
    {
      /* We will not actually be able to run code, since backtraces would
	 fly off the bottom of the stack (there is no way to reliably
	 detect bottom of stack), but that's fine since the kernel won't
	 run something without an a.out header anyway.  Passive examination
	 of .o files is one place this might make sense.  */
      /* ~0 will not be in any file.  */
      aout_hdr.entry = ~0;
      /* set the startup file to be an empty range.  */
      startup_file_start = 0;
      startup_file_end = 0;
    }

  if (num_symbols == 0)
    {
      free_all_symtabs ();
      error ("%s does not have a symbol-table.\n", name);
    }

  printf ("Reading symbol data from %s...", name);
  fflush (stdout);

  /* Throw away the old symbol table.  */

  free_all_symtabs ();
  free_all_psymtabs ();		/* Make sure that partial_symtab_list */
				/* is 0 also. */

  num_sections = file_hdr.f_nscns;
  symtab_offset = file_hdr.f_symptr;

  if (read_section_hdr (desc, _TEXT, &text_hdr, num_sections,
			file_hdr.f_opthdr) < 0)
    error ("\"%s\": can't read text section header", name);

  /* Read the line number table, all at once.  */

  val = init_lineno (desc, text_hdr.s_lnnoptr, text_hdr.s_nlnno);
  if (val < 0)
    error ("\"%s\": error reading line numbers\n", name);

  /* Now read the string table, all at once.  */

  val = init_stringtab (desc, symtab_offset + num_symbols * SYMESZ);
  if (val < 0)
    {
      free_all_symtabs ();
      printf ("\"%s\": can't get string table", name);
      fflush (stdout);
      return;
    }
  make_cleanup (free_stringtab, 0);

  /* Position to read the symbol table.  Do not read it all at once. */
  val = lseek (desc, (long)symtab_offset, 0);
  if (val < 0)
    perror_with_name (name);

  init_misc_functions ();
  make_cleanup (discard_misc_bunches, 0);

  /* Now that the executable file is positioned at symbol table,
     process it and define symbols accordingly.  */

  read_coff_symtab (desc, num_symbols);

  patch_opaque_types ();

  /* Sort symbols alphabetically within each block.  */

  sort_syms ();

  /* Go over the misc functions and install them in vector.  */

  condense_misc_bunches ();

  /* Don't allow char * to have a typename (else would get caddr_t.)  */

  TYPE_NAME (lookup_pointer_type (builtin_type_char)) = 0;

  /* Make a default for file to list.  */

  select_source_symtab (0);

  symfile = savestring (name, strlen (name));

  do_cleanups (old_chain);

  printf ("done.\n");
  fflush (stdout);
}

/* Return name of file symbols were loaded from, or 0 if none..  */

char *
get_sym_file ()
{
  return symfile;
}

/* Simplified internal version of coff symbol table information */

struct coff_symbol {
  char *c_name;
  int c_symnum;		/* symbol number of this entry */
  int c_nsyms;		/* 1 if syment only, 2 if syment + auxent */
  long c_value;
  int c_sclass;
  int c_secnum;
  unsigned int c_type;
};

/* Given pointers to a symbol table in coff style exec file,
   analyze them and create struct symtab's describing the symbols.
   NSYMS is the number of symbols in the symbol table.
   We read them one at a time using read_one_sym ().  */

static void
read_coff_symtab (desc, nsyms)
     int desc;
     int nsyms;
{
  int newfd;			/* Avoid multiple closes on same desc */
  FILE *stream; 
  register struct context_stack *new;
  struct coff_symbol coff_symbol;
  register struct coff_symbol *cs = &coff_symbol;
  static SYMENT main_sym;
  static AUXENT main_aux;
  struct coff_symbol fcn_cs_saved;
  static SYMENT fcn_sym_saved;
  static AUXENT fcn_aux_saved;

  int num_object_files = 0;
  int next_file_symnum = -1;

  /* Name of the current file.  */
  char *filestring = "";
  int depth;
  int fcn_first_line;
  int fcn_last_line;
  int fcn_start_addr;
  long fcn_line_ptr;
  struct cleanup *old_chain;


  newfd = dup (desc);
  if (newfd == -1)
    fatal ("Too many open files");
  stream = fdopen (newfd, "r");

  old_chain = make_cleanup (free_all_symtabs, 0);
  make_cleanup (fclose, stream);
  nlist_stream_global = stream;
  nlist_nsyms_global = nsyms;
  last_source_file = 0;
  bzero (opaque_type_chain, sizeof opaque_type_chain);

  type_vector_length = 160;
  type_vector = (struct typevector *)
		xmalloc (sizeof (struct typevector)
				+ type_vector_length * sizeof (struct type *));
  bzero (type_vector->type, type_vector_length * sizeof (struct type *));

  start_symtab ();

  symnum = 0;
  while (symnum < nsyms)
    {
      QUIT;			/* Make this command interruptable.  */
      read_one_sym (cs, &main_sym, &main_aux);

      if (cs->c_symnum == next_file_symnum && cs->c_sclass != C_FILE)
	{
	  CORE_ADDR last_file_end = cur_src_end_addr;

	  if (last_source_file)
	    end_symtab ();

	  start_symtab ();
	  complete_symtab ("_globals_", 0, first_object_file_end);
	  /* done with all files, everything from here on out is globals */
	}

      /* Special case for file with type declarations only, no text.  */
      if (!last_source_file && cs->c_type != T_NULL && cs->c_secnum == N_DEBUG)
	complete_symtab (filestring, 0, 0);

      /* Typedefs should not be treated as symbol definitions.  */
      if (ISFCN (cs->c_type) && cs->c_sclass != C_TPDEF)
	{
	  /* record as misc function.  if we get '.bf' next,
	   * then we undo this step
	   */
	  record_misc_function (cs->c_name, cs->c_value);

	  fcn_line_ptr = main_aux.x_sym.x_fcnary.x_fcn.x_lnnoptr;
	  fcn_start_addr = cs->c_value;
	  fcn_cs_saved = *cs;
	  fcn_sym_saved = main_sym;
	  fcn_aux_saved = main_aux;
	  continue;
	}

      switch (cs->c_sclass)
	{
	  case C_EFCN:
	  case C_EXTDEF:
	  case C_ULABEL:
	  case C_USTATIC:
	  case C_LINE:
	  case C_ALIAS:
	  case C_HIDDEN:
	    printf ("Bad n_sclass = %d\n", cs->c_sclass);
	    break;

	  case C_FILE:
	    /*
	     * c_value field contains symnum of next .file entry in table
	     * or symnum of first global after last .file.
	     */
	    next_file_symnum = cs->c_value;
	    filestring = getfilename (&main_aux);
	    /*
	     * Complete symbol table for last object file
	     * containing debugging information.
	     */
	    if (last_source_file)
	      {
		end_symtab ();
		start_symtab ();
	      }
	    num_object_files++;
	    break;

          case C_STAT:
	    if (cs->c_name[0] == '.') {
		    if (strcmp (cs->c_name, _TEXT) == 0) {
			    if (num_object_files == 1) {
				    /* last address of startup file */
				    first_object_file_end = cs->c_value +
					    main_aux.x_scn.x_scnlen;
			    }
			    /* for some reason the old code didn't do
			     * this if this section entry had
			     * main_aux.x_scn.x_nlinno equal to 0
			     */
			    complete_symtab (filestring, cs->c_value,
					     main_aux.x_scn.x_scnlen);
		    }
		    /* flush rest of '.' symbols */
		    break;
	    }
	    /* fall in for static symbols that don't start with '.' */
	  case C_EXT:
	    if (cs->c_sclass == C_EXT &&
		cs->c_secnum == N_ABS &&
		strcmp (cs->c_name, _ETEXT) == 0)
		    end_of_text_addr = cs->c_value;
	    if (cs->c_type == T_NULL) {
		    if (cs->c_secnum <= 1) {	/* text or abs */
			    record_misc_function (cs->c_name, cs->c_value);
			    break;
		    } else {
			    cs->c_type = T_INT;
		    }
	    }
	    (void) process_coff_symbol (cs, &main_aux);
	    break;

	  case C_FCN:
	    if (strcmp (cs->c_name, ".bf") == 0)
	      {
#if 0
		/* Don't do this; we want all functions to be on the
		   mfl now.  */
		unrecord_misc_function ();
#endif

		within_function = 1;

		/* value contains address of first non-init type code */
		/* main_aux.x_sym.x_misc.x_lnsz.x_lnno
			    contains line number of '{' } */
		fcn_first_line = main_aux.x_sym.x_misc.x_lnsz.x_lnno;

		new = (struct context_stack *)
		  xmalloc (sizeof (struct context_stack));
		new->depth = depth = 0;
		new->next = 0;
		context_stack = new;
		new->locals = 0;
		new->old_blocks = pending_blocks;
		new->start_addr = fcn_start_addr;
		fcn_cs_saved.c_name = getsymname (&fcn_sym_saved);
		new->name = process_coff_symbol (&fcn_cs_saved,
						 &fcn_aux_saved);
	      }
	    else if (strcmp (cs->c_name, ".ef") == 0)
	      {
		      /* the value of .ef is the address of epilogue code;
		       * not useful for gdb
		       */
		/* { main_aux.x_sym.x_misc.x_lnsz.x_lnno
			    contains number of lines to '}' */
		fcn_last_line = main_aux.x_sym.x_misc.x_lnsz.x_lnno;
		enter_linenos (fcn_line_ptr, fcn_first_line, fcn_last_line);
		new = context_stack;

		if (new == 0)
		  error ("Invalid symbol data; .bf/.ef/.bb/.eb symbol mismatch, at symbol %d.",
			 symnum);
		
		finish_block (new->name, &local_symbols, new->old_blocks,
			      new->start_addr,
			      fcn_cs_saved.c_value +
			          fcn_aux_saved.x_sym.x_misc.x_fsize);
		context_stack = 0;
		within_function = 0;
		free (new);
	      }
	    break;

	  case C_BLOCK:
	    if (strcmp (cs->c_name, ".bb") == 0)
	      {
		new = (struct context_stack *)
			    xmalloc (sizeof (struct context_stack));
		depth++;
		new->depth = depth;
		new->next = context_stack;
		context_stack = new;
		new->locals = local_symbols;
		new->old_blocks = pending_blocks;
		new->start_addr = cs->c_value;
		new->name = 0;
		local_symbols = 0;
	      }
	    else if (strcmp (cs->c_name, ".eb") == 0)
	      {
		new = context_stack;
		if (new == 0 || depth != new->depth)
		  error ("Invalid symbol data: .bb/.eb symbol mismatch at symbol %d.",
			 symnum);
		if (local_symbols && context_stack->next)
		  {
		    /* Make a block for the local symbols within.  */
		    finish_block (0, &local_symbols, new->old_blocks,
				  new->start_addr, cs->c_value);
		  }
		depth--;
		local_symbols = new->locals;
		context_stack = new->next;
		free (new);
	      }
	    break;

	  default:
	    (void) process_coff_symbol (cs, &main_aux);
	    break;
	}
    }

  if (last_source_file)
    end_symtab ();
  fclose (stream);
  discard_cleanups (old_chain);
}

/* Routines for reading headers and symbols from executable.  */

/* Read COFF file header, check magic number,
   and return number of symbols. */
read_file_hdr (chan, file_hdr)
    int chan;
    FILHDR *file_hdr;
{
  lseek (chan, 0L, 0);
  if (myread (chan, (char *)file_hdr, FILHSZ) < 0)
    return -1;

  switch (file_hdr->f_magic)
    {
#ifdef MC68MAGIC
    case MC68MAGIC:
#endif
#ifdef NS32GMAGIC
      case NS32GMAGIC:
      case NS32SMAGIC:
#endif
#ifdef I386MAGIC
    case I386MAGIC:
#endif
#ifdef CLIPPERMAGIC
    case CLIPPERMAGIC:
#endif      
	return file_hdr->f_nsyms;

      default:
#ifdef BADMAG
	if (BADMAG(file_hdr))
	  return -1;
	else
	  return file_hdr->f_nsyms;
#else
	return -1;
#endif
    }
}

read_aout_hdr (chan, aout_hdr, size)
    int chan;
    AOUTHDR *aout_hdr;
    int size;
{
  lseek (chan, (long)FILHSZ, 0);
  if (size != sizeof (AOUTHDR))
    return -1;
  if (myread (chan, (char *)aout_hdr, size) != size)
    return -1;
  return 0;
}

/* Read a section header.  OPTIONAL_HEADER_SIZE is the size of the
   optional header (normally f_opthdr from the file header).

   Return nonnegative for success, -1 for failure.  */
int
read_section_hdr (chan, section_name, section_hdr, nsects,
                  optional_header_size)
    register int chan;
    register char *section_name;
    SCNHDR *section_hdr;
    register int nsects;
    int optional_header_size;
{
  register int i;

  if (lseek (chan, FILHSZ + optional_header_size, 0) < 0)
    return -1;

  for (i = 0; i < nsects; i++)
    {
      if (myread (chan, (char *)section_hdr, SCNHSZ) < 0)
	return -1;
      if (strncmp (section_hdr->s_name, section_name, 8) == 0)
	return 0;
    }
    return -1;
}

read_one_sym (cs, sym, aux)
    register struct coff_symbol *cs;
    register SYMENT *sym;
    register AUXENT *aux;
{
  cs->c_symnum = symnum;
  fread ((char *)sym, SYMESZ, 1, nlist_stream_global);
  cs->c_nsyms = (sym->n_numaux & 0xff) + 1;
  if (cs->c_nsyms == 2)
    {
      /* doc for coff says there is either no aux entry or just one */
      fread ((char *)aux, AUXESZ, 1, nlist_stream_global);
    }
  else if (cs->c_nsyms > 2)
    error ("more than one aux symbol table entry at symnum=%d\n", symnum);

  cs->c_name = getsymname (sym);
  cs->c_value = sym->n_value;
  cs->c_sclass = (sym->n_sclass & 0xff);
  cs->c_secnum = sym->n_scnum;
  cs->c_type = (unsigned) sym->n_type;

  symnum += cs->c_nsyms;
}

/* Support for string table handling */

static char *stringtab = NULL;

static int
init_stringtab (chan, offset)
    int chan;
    long offset;
{
  long buffer;
  int val;

  if (stringtab)
    {
      free (stringtab);
      stringtab = NULL;
    }

  if (lseek (chan, offset, 0) < 0)
    return -1;

  val = myread (chan, (char *)&buffer, sizeof buffer);

  /* If no string table is needed, then the file may end immediately
     after the symbols.  Just return with `stringtab' set to null. */
  if (val != sizeof buffer || buffer == 0)
    return 0;

  stringtab = (char *) xmalloc (buffer);
  if (stringtab == NULL)
    return -1;

  bcopy (&buffer, stringtab, sizeof buffer);

  val = myread (chan, stringtab + sizeof buffer, buffer - sizeof buffer);
  if (val != buffer - sizeof buffer || stringtab[buffer - 1] != '\0')
    return -1;

  return 0;
}

static void
free_stringtab ()
{
  if (stringtab)
    free (stringtab);
  stringtab = NULL;
}

static char *
getsymname (symbol_entry)
    SYMENT *symbol_entry;
{
  static char buffer[SYMNMLEN+1];
  char *result;

  if (symbol_entry->n_zeroes == 0)
    {
      result = stringtab + symbol_entry->n_offset;
    }
  else
    {
      strncpy (buffer, symbol_entry->n_name, SYMNMLEN);
      buffer[SYMNMLEN] = '\0';
      result = buffer;
    }
  return result;
}

static char *
getfilename (aux_entry)
    AUXENT *aux_entry;
{
  static char buffer[BUFSIZ];
  register char *temp;
  char *result;
  extern char *rindex ();

#ifndef COFF_NO_LONG_FILE_NAMES
  if (aux_entry->x_file.x_foff != 0)
    strcpy (buffer, stringtab + aux_entry->x_file.x_foff);
  else
#endif
    {
      strncpy (buffer, aux_entry->x_file.x_fname, FILNMLEN);
      buffer[FILNMLEN] = '\0';
    }
  result = buffer;
  if ((temp = rindex (result, '/')) != NULL)
    result = temp + 1;
  return (result);
}

/* Support for line number handling */
static char *linetab = NULL;
static long linetab_offset;
static int linetab_count;

static int
init_lineno (chan, offset, count)
    int chan;
    long offset;
    int count;
{
  int val;

  if (lseek (chan, offset, 0) < 0)
    return -1;
  
  if (linetab)
    free (linetab);
  linetab = (char *) xmalloc (count * LINESZ);

  val = myread (chan, linetab, count * LINESZ);
  if (val != count * LINESZ)
    return -1;

  linetab_offset = offset;
  linetab_count = count;
  return 0;
}

static void
enter_linenos (file_offset, first_line, last_line)
    long file_offset;
    register int first_line;
    register int last_line;
{
  register char *rawptr = &linetab[file_offset - linetab_offset];
  struct lineno lptr;

  /* skip first line entry for each function */
  rawptr += LINESZ;
  /* line numbers start at one for the first line of the function */
  first_line--;

  /* Bcopy since occaisionally rawptr isn't pointing at long
     boundaries.  */
  for (bcopy (rawptr, &lptr, LINESZ);
       lptr.l_lnno && lptr.l_lnno <= last_line;
       rawptr += LINESZ, bcopy (rawptr, &lptr, LINESZ))
    {
      record_line (first_line + lptr.l_lnno, lptr.l_addr.l_paddr);
    }
}

static int
hashname (name)
     char *name;
{
  register char *p = name;
  register int total = p[0];
  register int c;

  c = p[1];
  total += c << 2;
  if (c)
    {
      c = p[2];
      total += c << 4;
      if (c)
	total += p[3] << 6;
    }
  
  return total % HASHSIZE;
}

static void
patch_type (type, real_type)
    struct type *type;
    struct type *real_type;
{
  register struct type *target = TYPE_TARGET_TYPE (type);
  register struct type *real_target = TYPE_TARGET_TYPE (real_type);
  int field_size = TYPE_NFIELDS (real_target) * sizeof (struct field);

  TYPE_LENGTH (target) = TYPE_LENGTH (real_target);
  TYPE_NFIELDS (target) = TYPE_NFIELDS (real_target);
  TYPE_FIELDS (target) = (struct field *)
				obstack_alloc (symbol_obstack, field_size);

  bcopy (TYPE_FIELDS (real_target), TYPE_FIELDS (target), field_size);

  if (TYPE_NAME (real_target))
    {
      if (TYPE_NAME (target))
	free (TYPE_NAME (target));
      TYPE_NAME (target) = concat (TYPE_NAME (real_target), "", "");
    }
}

/* Patch up all appropriate typdef symbols in the opaque_type_chains
   so that they can be used to print out opaque data structures properly */

static void
patch_opaque_types ()
{
  struct symtab *s;

  /* Look at each symbol in the per-file block of each symtab.  */
  for (s = symtab_list; s; s = s->next)
    {
      register struct block *b;
      register int i;

      /* Go through the per-file symbols only */
      b = BLOCKVECTOR_BLOCK (BLOCKVECTOR (s), 1);
      for (i = BLOCK_NSYMS (b) - 1; i >= 0; i--)
	{
	  register struct symbol *real_sym;

	  /* Find completed typedefs to use to fix opaque ones.
	     Remove syms from the chain when their types are stored,
	     but search the whole chain, as there may be several syms
	     from different files with the same name.  */
	  real_sym = BLOCK_SYM (b, i);
	  if (SYMBOL_CLASS (real_sym) == LOC_TYPEDEF &&
	      SYMBOL_NAMESPACE (real_sym) == VAR_NAMESPACE &&
	      TYPE_CODE (SYMBOL_TYPE (real_sym)) == TYPE_CODE_PTR &&
	      TYPE_LENGTH (TYPE_TARGET_TYPE (SYMBOL_TYPE (real_sym))) != 0)
	    {
	      register char *name = SYMBOL_NAME (real_sym);
	      register int hash = hashname (name);
	      register struct symbol *sym, *prev;

	      prev = 0;
	      for (sym = opaque_type_chain[hash]; sym;)
		{
		  if (name[0] == SYMBOL_NAME (sym)[0] &&
		      !strcmp (name + 1, SYMBOL_NAME (sym) + 1))
		    {
		      if (prev)
			SYMBOL_VALUE (prev) = SYMBOL_VALUE (sym);
		      else
			opaque_type_chain[hash]
			  = (struct symbol *) SYMBOL_VALUE (sym);

		      patch_type (SYMBOL_TYPE (sym), SYMBOL_TYPE (real_sym));

		      if (prev)
			sym = (struct symbol *) SYMBOL_VALUE (prev);
		      else
			sym = opaque_type_chain[hash];
		    }
		  else
		    {
		      prev = sym;
		      sym = (struct symbol *) SYMBOL_VALUE (sym);
		    }
		}
	    }
	}
    }
}

static struct symbol *
process_coff_symbol (cs, aux)
     register struct coff_symbol *cs;
     register AUXENT *aux;
{
  register struct symbol *sym
    = (struct symbol *) obstack_alloc (symbol_obstack, sizeof (struct symbol));
  char *name;
  char *dot;
#ifdef NAMES_HAVE_UNDERSCORE
  int offset = 1;
#else
  int offset = 0;
#endif

  bzero (sym, sizeof (struct symbol));
  name = cs->c_name;
  name = (name[0] == '_' ? name + offset : name);
  SYMBOL_NAME (sym) = obstack_copy0 (symbol_obstack, name, strlen (name));

  /* default assumptions */
  SYMBOL_VALUE (sym) = cs->c_value;
  SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;

  if (ISFCN (cs->c_type))
    {
      SYMBOL_TYPE (sym) = 
	lookup_function_type (decode_function_type (cs, cs->c_type, aux));
      SYMBOL_CLASS (sym) = LOC_BLOCK;
      if (cs->c_sclass == C_STAT)
	add_symbol_to_list (sym, &file_symbols);
      else if (cs->c_sclass == C_EXT)
	add_symbol_to_list (sym, &global_symbols);
    }
  else
    {
      SYMBOL_TYPE (sym) = decode_type (cs, cs->c_type, aux);
      switch (cs->c_sclass)
	{
	  case C_NULL:
	    break;

	  case C_AUTO:
	    SYMBOL_CLASS (sym) = LOC_LOCAL;
	    add_symbol_to_list (sym, &local_symbols);
	    break;

	  case C_EXT:
	    SYMBOL_CLASS (sym) = LOC_STATIC;
	    add_symbol_to_list (sym, &global_symbols);
	    break;

	  case C_STAT:
	    SYMBOL_CLASS (sym) = LOC_STATIC;
	    if (within_function) {
	      /* Static symbol of local scope */
	      add_symbol_to_list (sym, &local_symbols);
	    }
	    else {
	      /* Static symbol at top level of file */
	      add_symbol_to_list (sym, &file_symbols);
	    }
	    break;

	  case C_REG:
	    SYMBOL_CLASS (sym) = LOC_REGISTER;
	    add_symbol_to_list (sym, &local_symbols);
	    break;

	  case C_LABEL:
	    break;

	  case C_ARG:
	    SYMBOL_CLASS (sym) = LOC_ARG;
	    add_symbol_to_list (sym, &local_symbols);
#ifndef clipper	   
	    /* If PCC says a parameter is a short or a char,
	       it is really an int.  */
	    if (SYMBOL_TYPE (sym) == builtin_type_char
		|| SYMBOL_TYPE (sym) == builtin_type_short)
	      SYMBOL_TYPE (sym) = builtin_type_int;
	    else if (SYMBOL_TYPE (sym) == builtin_type_unsigned_char
		     || SYMBOL_TYPE (sym) == builtin_type_unsigned_short)
	      SYMBOL_TYPE (sym) = builtin_type_unsigned_int;
#endif
	    break;

	  case C_REGPARM:
	    SYMBOL_CLASS (sym) = LOC_REGPARM;
	    add_symbol_to_list (sym, &local_symbols);
#ifndef clipper
	    /* If PCC says a parameter is a short or a char,
	       it is really an int.  */
	    if (SYMBOL_TYPE (sym) == builtin_type_char
		|| SYMBOL_TYPE (sym) == builtin_type_short)
	      SYMBOL_TYPE (sym) = builtin_type_int;
	    else if (SYMBOL_TYPE (sym) == builtin_type_unsigned_char
		     || SYMBOL_TYPE (sym) == builtin_type_unsigned_short)
	      SYMBOL_TYPE (sym) = builtin_type_unsigned_int;
#endif
	    break;
	    
	  case C_TPDEF:
	    SYMBOL_CLASS (sym) = LOC_TYPEDEF;
	    SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;

	    /* If type has no name, give it one */
	    if (TYPE_NAME (SYMBOL_TYPE (sym)) == 0 
		&& (TYPE_FLAGS (SYMBOL_TYPE (sym)) & TYPE_FLAG_PERM) == 0)
	      TYPE_NAME (SYMBOL_TYPE (sym))
					  = concat (SYMBOL_NAME (sym), "", "");

	    /* Keep track of any type which points to empty structured type,
		so it can be filled from a definition from another file */
	    if (TYPE_CODE (SYMBOL_TYPE (sym)) == TYPE_CODE_PTR &&
		TYPE_LENGTH (TYPE_TARGET_TYPE (SYMBOL_TYPE (sym))) == 0)
	      {
		register int i = hashname (SYMBOL_NAME (sym));

		SYMBOL_VALUE (sym) = (int) opaque_type_chain[i];
		opaque_type_chain[i] = sym;
	      }
	    add_symbol_to_list (sym, &file_symbols);
	    break;

	  case C_STRTAG:
	  case C_UNTAG:
	  case C_ENTAG:
	    SYMBOL_CLASS (sym) = LOC_TYPEDEF;
	    SYMBOL_NAMESPACE (sym) = STRUCT_NAMESPACE;
	    if (TYPE_NAME (SYMBOL_TYPE (sym)) == 0
		&& (TYPE_FLAGS (SYMBOL_TYPE (sym)) & TYPE_FLAG_PERM) == 0)
	      TYPE_NAME (SYMBOL_TYPE (sym))
		= concat ("",
			  (cs->c_sclass == C_ENTAG
			   ? "enum "
			   : (cs->c_sclass == C_STRTAG
			      ? "struct " : "union ")),
			  SYMBOL_NAME (sym));
	    add_symbol_to_list (sym, &file_symbols);
	    break;

	  default:
	    break;
	}
    }
  return sym;
}

/* Decode a coff type specifier;
   return the type that is meant.  */

static
struct type *
decode_type (cs, c_type, aux)
     register struct coff_symbol *cs;
     unsigned int c_type;
     register AUXENT *aux;
{
  register struct type *type = 0;
  register int n;
  unsigned int new_c_type;

  if (c_type & ~N_BTMASK)
    {
      new_c_type = DECREF (c_type);
      if (ISPTR (c_type))
	{
	  type = decode_type (cs, new_c_type, aux);
	  type = lookup_pointer_type (type);
	}
      else if (ISFCN (c_type))
	{
	  type = decode_type (cs, new_c_type, aux);
	  type = lookup_function_type (type);
	}
      else if (ISARY (c_type))
	{
	  int i, n;
	  register unsigned short *dim;
	  struct type *base_type;

	  /* Define an array type.  */
	  /* auxent refers to array, not base type */
	  if (aux->x_sym.x_tagndx == 0)
	    cs->c_nsyms = 1;

	  /* shift the indices down */
	  dim = &aux->x_sym.x_fcnary.x_ary.x_dimen[0];
	  i = 1;
	  n = dim[0];
	  for (i = 0; *dim && i < DIMNUM - 1; i++, dim++)
	    *dim = *(dim + 1);
	  *dim = 0;

	  type = (struct type *)
		    obstack_alloc (symbol_obstack, sizeof (struct type));
	  bzero (type, sizeof (struct type));

	  base_type = decode_type (cs, new_c_type, aux);

	  TYPE_CODE (type) = TYPE_CODE_ARRAY;
	  TYPE_TARGET_TYPE (type) = base_type;
	  TYPE_LENGTH (type) = n * TYPE_LENGTH (base_type);
	}
      return type;
    }

  /* Reference to existing type */
  if (cs->c_nsyms > 1 && aux->x_sym.x_tagndx != 0)
    {
      type = coff_alloc_type (aux->x_sym.x_tagndx);
      return type;
    }

  return decode_base_type (cs, BTYPE (c_type), aux);
}

/* Decode a coff type specifier for function definition;
   return the type that the function returns.  */

static
struct type *
decode_function_type (cs, c_type, aux)
     register struct coff_symbol *cs;
     unsigned int c_type;
     register AUXENT *aux;
{
  if (aux->x_sym.x_tagndx == 0)
    cs->c_nsyms = 1;	/* auxent refers to function, not base type */

  return decode_type (cs, DECREF (cs->c_type), aux);
}

/* basic C types */

static
struct type *
decode_base_type (cs, c_type, aux)
     register struct coff_symbol *cs;
     unsigned int c_type;
     register AUXENT *aux;
{
  struct type *type;

  switch (c_type)
    {
      case T_NULL:
        /* shows up with "void (*foo)();" structure members */
	return builtin_type_void;

      case T_ARG:
	/* shouldn't show up here */
	break;

      case T_CHAR:
	return builtin_type_char;

      case T_SHORT:
	return builtin_type_short;

      case T_INT:
	return builtin_type_int;

      case T_LONG:
	return builtin_type_long;

      case T_FLOAT:
	return builtin_type_float;

      case T_DOUBLE:
	return builtin_type_double;

      case T_STRUCT:
	if (cs->c_nsyms != 2)
	  {
	    /* anonymous structure type */
	    type = coff_alloc_type (cs->c_symnum);
	    TYPE_CODE (type) = TYPE_CODE_STRUCT;
	    TYPE_NAME (type) = concat ("struct ", "<opaque>", "");
	    TYPE_LENGTH (type) = 0;
	    TYPE_FIELDS (type) = 0;
	    TYPE_NFIELDS (type) = 0;
	  }
	else
	  {
	    type = read_struct_type (cs->c_symnum,
				    aux->x_sym.x_misc.x_lnsz.x_size,
				    aux->x_sym.x_fcnary.x_fcn.x_endndx);
	  }
	return type;

      case T_UNION:
	if (cs->c_nsyms != 2)
	  {
	    /* anonymous union type */
	    type = coff_alloc_type (cs->c_symnum);
	    TYPE_NAME (type) = concat ("union ", "<opaque>", "");
	    TYPE_LENGTH (type) = 0;
	    TYPE_FIELDS (type) = 0;
	    TYPE_NFIELDS (type) = 0;
	  }
	else
	  {
	    type = read_struct_type (cs->c_symnum,
				    aux->x_sym.x_misc.x_lnsz.x_size,
				    aux->x_sym.x_fcnary.x_fcn.x_endndx);
	  }
	TYPE_CODE (type) = TYPE_CODE_UNION;
	return type;

      case T_ENUM:
	return read_enum_type (cs->c_symnum,
				    aux->x_sym.x_misc.x_lnsz.x_size,
				    aux->x_sym.x_fcnary.x_fcn.x_endndx);

      case T_MOE:
	/* shouldn't show up here */
	break;

      case T_UCHAR:
	return builtin_type_unsigned_char;

      case T_USHORT:
	return builtin_type_unsigned_short;

      case T_UINT:
	return builtin_type_unsigned_int;

      case T_ULONG:
	return builtin_type_unsigned_long;
    }
  printf ("unexpected type %d at symnum %d\n", c_type, cs->c_symnum);
  return builtin_type_void;
}

/* This page contains subroutines of read_type.  */

/* Read the description of a structure (or union type)
   and return an object describing the type.  */

static struct type *
read_struct_type (index, length, lastsym)
     int index;
     int length;
     int lastsym;
{
  struct nextfield
    {
      struct nextfield *next;
      struct field field;
    };

  register struct type *type;
  register struct nextfield *list = 0;
  struct nextfield *new;
  int nfields = 0;
  register int n;
  char *name;
#ifdef NAMES_HAVE_UNDERSCORE
  int offset = 1;
#else
  int offset = 0;
#endif
  struct coff_symbol member_sym;
  register struct coff_symbol *ms = &member_sym;
  SYMENT sub_sym;
  AUXENT sub_aux;
  int done = 0;

  type = coff_alloc_type (index);
  TYPE_CODE (type) = TYPE_CODE_STRUCT;
  TYPE_LENGTH (type) = length;

  while (!done && symnum < lastsym && symnum < nlist_nsyms_global)
    {
      read_one_sym (ms, &sub_sym, &sub_aux);
      name = ms->c_name;
      name = (name[0] == '_' ? name + offset : name);

      switch (ms->c_sclass)
	{
	  case C_MOS:
	  case C_MOU:

	    /* Get space to record the next field's data.  */
	    new = (struct nextfield *) alloca (sizeof (struct nextfield));
	    new->next = list;
	    list = new;

	    /* Save the data.  */
	    list->field.name = savestring (name, strlen (name));
	    list->field.type = decode_type (ms, ms->c_type, &sub_aux);
	    list->field.bitpos = 8 * ms->c_value;
	    list->field.bitsize = 0;
	    nfields++;
	    break;

	  case C_FIELD:

	    /* Get space to record the next field's data.  */
	    new = (struct nextfield *) alloca (sizeof (struct nextfield));
	    new->next = list;
	    list = new;

	    /* Save the data.  */
	    list->field.name = savestring (name, strlen (name));
	    list->field.type = decode_type (ms, ms->c_type, &sub_aux);
	    list->field.bitpos = ms->c_value;
	    list->field.bitsize = sub_aux.x_sym.x_misc.x_lnsz.x_size;
	    nfields++;
	    break;

	  case C_EOS:
	    done = 1;
	    break;
	}
    }
  /* Now create the vector of fields, and record how big it is.  */

  TYPE_NFIELDS (type) = nfields;
  TYPE_FIELDS (type) = (struct field *)
		obstack_alloc (symbol_obstack, sizeof (struct field) * nfields);

  /* Copy the saved-up fields into the field vector.  */

  for (n = nfields; list; list = list->next)
    TYPE_FIELD (type, --n) = list->field;

  return type;
}

/* Read a definition of an enumeration type,
   and create and return a suitable type object.
   Also defines the symbols that represent the values of the type.  */

static struct type *
read_enum_type (index, length, lastsym)
     int index;
     int length;
     int lastsym;
{
  register struct symbol *sym;
  register struct type *type;
  int nsyms = 0;
  struct pending **symlist;
  struct coff_symbol member_sym;
  register struct coff_symbol *ms = &member_sym;
  SYMENT sub_sym;
  AUXENT sub_aux;
  struct pending *osyms, *syms;
  register int n;
  char *name;
#ifdef NAMES_HAVE_UNDERSCORE
  int offset = 1;
#else
  int offset = 0;
#endif

  type = coff_alloc_type (index);
  if (within_function)
    symlist = &local_symbols;
  else
    symlist = &file_symbols;
  osyms = *symlist;

  while (symnum < lastsym && symnum < nlist_nsyms_global)
    {
      read_one_sym (ms, &sub_sym, &sub_aux);
      name = ms->c_name;
      name = (name[0] == '_' ? name + offset : name);

      switch (ms->c_sclass)
	{
	  case C_MOE:
	    sym = (struct symbol *) xmalloc (sizeof (struct symbol));
	    bzero (sym, sizeof (struct symbol));

	    SYMBOL_NAME (sym) = savestring (name, strlen (name));
	    SYMBOL_CLASS (sym) = LOC_CONST;
	    SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
	    SYMBOL_VALUE (sym) = ms->c_value;
	    add_symbol_to_list (sym, symlist);
	    nsyms++;
	    break;

	  case C_EOS:
	    break;
	}
    }

  /* Now fill in the fields of the type-structure.  */

  TYPE_LENGTH (type) = sizeof (int);
  TYPE_CODE (type) = TYPE_CODE_ENUM;
  TYPE_NFIELDS (type) = nsyms;
  TYPE_FIELDS (type) = (struct field *)
		obstack_alloc (symbol_obstack, sizeof (struct field) * nsyms);

  /* Find the symbols for the values and put them into the type.
     The symbols can be found in the symlist that we put them on
     to cause them to be defined.  osyms contains the old value
     of that symlist; everything up to there was defined by us.  */

  for (syms = *symlist, n = nsyms; syms != osyms; syms = syms->next)
    {
      SYMBOL_TYPE (syms->symbol) = type;
      TYPE_FIELD_NAME (type, --n) = SYMBOL_NAME (syms->symbol);
      TYPE_FIELD_VALUE (type, n) = 0;
      TYPE_FIELD_BITPOS (type, n) = SYMBOL_VALUE (syms->symbol);
      TYPE_FIELD_BITSIZE (type, n) = 0;
    }
  return type;
}

/* This function is really horrible, but to avoid it, there would need
   to be more filling in of forward references.  THIS SHOULD BE MOVED
   OUT OF COFFREAD.C AND DBXREAD.C TO SOME PLACE WHERE IT CAN BE SHARED. */
int
fill_in_vptr_fieldno (type)
     struct type *type;
{
  if (TYPE_VPTR_FIELDNO (type) < 0)
    TYPE_VPTR_FIELDNO (type) =
      fill_in_vptr_fieldno (TYPE_BASECLASS (type, 1));
  return TYPE_VPTR_FIELDNO (type);
}

/* partial symbol tables are not implemented in coff, therefore
   block_for_pc() (and others) will never decide to call this. */

extern struct symtab *
psymtab_to_symtab ()
{
  fatal ("error: Someone called psymtab_to_symtab\n");
}

/* These will stay zero all the time */
struct psymbol_allocation_list global_psymbols, static_psymbols;

_initialize_coff ()
{
  symfile = 0;

  bzero (&global_psymbols, sizeof (global_psymbols));
  bzero (&static_psymbols, sizeof (static_psymbols));

  add_com ("symbol-file", class_files, symbol_file_command,
	   "Load symbol table (in coff format) from executable file FILE.");
}


#endif /* COFF_FORMAT */

