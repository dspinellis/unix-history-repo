/* Read coff symbol tables and convert to internal format, for GDB.
   Contributed by David D. Johnson, Brown University (ddj@cs.brown.edu).
   Copyright 1987, 1988, 1989, 1990, 1991 Free Software Foundation, Inc.

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
#include "breakpoint.h"
#include "bfd.h"
#include "symfile.h"
#include "objfiles.h"
#include "buildsym.h"
#include <obstack.h>

#include <string.h>

#include "coff/internal.h"	/* Internal format of COFF symbols in BFD */
#include "libcoff.h"		/* FIXME secret internal data from BFD */

/* To be an sdb debug type, type must have at least a basic or primary
   derived type.  Using this rather than checking against T_NULL is
   said to prevent core dumps if we try to operate on Michael Bloom
   dbx-in-coff file.  */

#define SDB_TYPE(type) (BTYPE(type) | (type & N_TMASK))

/*
 * Convert from an sdb register number to an internal gdb register number.
 * This should be defined in tm.h, if REGISTER_NAMES is not set up
 * to map one to one onto the sdb register numbers.
 */
#ifndef SDB_REG_TO_REGNUM
# define SDB_REG_TO_REGNUM(value)     (value)
#endif

/* Core address of start and end of text of current source file.
   This comes from a ".text" symbol where x_nlinno > 0.  */

static CORE_ADDR cur_src_start_addr;
static CORE_ADDR cur_src_end_addr;

/* Core address of the end of the first object file.  */
static CORE_ADDR first_object_file_end;

/* The addresses of the symbol table stream and number of symbols
   of the object file we are reading (as copied into core).  */

static FILE *nlist_stream_global;
static int nlist_nsyms_global;

/* Vector of line number information.  */

static struct linetable *line_vector;

/* Index of next entry to go in line_vector_index.  */

static int line_vector_index;

/* Last line number recorded in the line vector.  */

static int prev_line_number;

/* Number of elements allocated for line_vector currently.  */

static int line_vector_length;

/* Pointers to scratch storage, used for reading raw symbols and auxents.  */

static char *temp_sym;
static char *temp_aux;

/* Local variables that hold the shift and mask values for the
   COFF file that we are currently reading.  These come back to us
   from BFD, and are referenced by their macro names, as well as
   internally to the BTYPE, ISPTR, ISFCN, ISARY, ISTAG, and DECREF
   macros from ../internalcoff.h .  */

static unsigned	local_n_btmask;
static unsigned	local_n_btshft;
static unsigned	local_n_tmask;
static unsigned	local_n_tshift;

#define	N_BTMASK	local_n_btmask
#define	N_BTSHFT	local_n_btshft
#define	N_TMASK		local_n_tmask
#define	N_TSHIFT	local_n_tshift
 
/* Local variables that hold the sizes in the file of various COFF structures.
   (We only need to know this to read them from the file -- BFD will then
   translate the data in them, into `internal_xxx' structs in the right
   byte order, alignment, etc.)  */

static unsigned	local_linesz;
static unsigned	local_symesz;
static unsigned	local_auxesz;


/* Chain of typedefs of pointers to empty struct/union types.
   They are chained thru the SYMBOL_VALUE_CHAIN.  */

static struct symbol *opaque_type_chain[HASHSIZE];

/* Record the symbols defined for each context in a list.
   We don't create a struct block for the context until we
   know how long to make it.  */

struct coff_pending
{
  struct coff_pending *next;
  struct symbol *symbol;
};

/* Here are the three lists that symbols are put on.  */

struct coff_pending *coff_file_symbols;	/* static at top level, and types */

struct coff_pending *coff_global_symbols;  /* global functions and variables */

struct coff_pending *coff_local_symbols;  /* everything local to lexical context */

/* List of unclosed lexical contexts
   (that will become blocks, eventually).  */

struct coff_context_stack
{
  struct coff_context_stack *next;
  struct coff_pending *locals;
  struct pending_block *old_blocks;
  struct symbol *name;
  CORE_ADDR start_addr;
  int depth;
};

struct coff_context_stack *coff_context_stack;

/* Nonzero if within a function (so symbols should be local,
   if nothing says specifically).  */

int within_function;

#if 0
/* The type of the function we are currently reading in.  This is
   used by define_symbol to record the type of arguments to a function. */

struct type *in_function_type;
#endif

struct pending_block *pending_blocks;

/* Complaints about various problems in the file being read  */

struct complaint ef_complaint = 
  {"Unmatched .ef symbol(s) ignored starting at symnum %d", 0, 0};

struct complaint bf_no_aux_complaint =
  {"`.bf' symbol %d has no aux entry", 0, 0};

struct complaint ef_no_aux_complaint =
  {"`.ef' symbol %d has no aux entry", 0, 0};

struct complaint lineno_complaint =
  {"Line number pointer %d lower than start of line numbers", 0, 0};

struct complaint unexpected_type_complaint =
  {"Unexpected type for symbol %s", 0, 0};

struct complaint bad_sclass_complaint =
  {"Bad n_sclass for symbol %s", 0, 0};

struct complaint misordered_blocks_complaint =
  {"Blocks out of order at address %x", 0, 0};

struct complaint tagndx_bad_complaint =
  {"Symbol table entry for %s has bad tagndx value", 0, 0};

/* Simplified internal version of coff symbol table information */

struct coff_symbol {
  char *c_name;
  int c_symnum;		/* symbol number of this entry */
  int c_naux;		/* 0 if syment only, 1 if syment + auxent, etc */
  long c_value;
  int c_sclass;
  int c_secnum;
  unsigned int c_type;
};

static struct type *
coff_read_struct_type PARAMS ((int, int, int));

static struct type *
decode_base_type PARAMS ((struct coff_symbol *, unsigned int,
			  union internal_auxent *));

static struct type *
decode_type PARAMS ((struct coff_symbol *, unsigned int,
		     union internal_auxent *));

static struct type *
decode_function_type PARAMS ((struct coff_symbol *, unsigned int,
			      union internal_auxent *));

static struct type *
coff_read_enum_type PARAMS ((int, int, int));

static struct blockvector *
make_blockvector PARAMS ((struct objfile *));

static struct symbol *
process_coff_symbol PARAMS ((struct coff_symbol *, union internal_auxent *,
			     struct objfile *));

static void
patch_opaque_types PARAMS ((struct symtab *));

static void
patch_type PARAMS ((struct type *, struct type *));

static void
enter_linenos PARAMS ((long, int, int));

static int
init_lineno PARAMS ((int, long, int));

static char *
getfilename PARAMS ((union internal_auxent *));

static char *
getsymname PARAMS ((struct internal_syment *));

static void
free_stringtab PARAMS ((void));

static int
init_stringtab PARAMS ((int, long));

static void
read_one_sym PARAMS ((struct coff_symbol *, struct internal_syment *,
		      union internal_auxent *));

static void
read_coff_symtab PARAMS ((long, int, struct objfile *));

static void
find_linenos PARAMS ((bfd *, sec_ptr, PTR));

static void
coff_symfile_init PARAMS ((struct objfile *));

static void
coff_new_init PARAMS ((struct objfile *));

static void
coff_symfile_read PARAMS ((struct objfile *, struct section_offsets *, int));

static void
coff_symfile_finish PARAMS ((struct objfile *));

static void
record_minimal_symbol PARAMS ((char *, CORE_ADDR, enum minimal_symbol_type));

static void
coff_end_symtab PARAMS ((struct objfile *));

static void
complete_symtab PARAMS ((char *, CORE_ADDR, unsigned int));

static void
coff_start_symtab PARAMS ((void));

static void
coff_record_line PARAMS ((int, CORE_ADDR));

static void
coff_finish_block PARAMS ((struct symbol *, struct coff_pending **,
			   struct pending_block *, CORE_ADDR, CORE_ADDR,
			   struct objfile *));

static void
coff_add_symbol_to_list PARAMS ((struct symbol *, struct coff_pending **));

static struct type *
coff_alloc_type PARAMS ((int));

static struct type **
coff_lookup_type PARAMS ((int));


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
      type_vector = (struct type **)
	xrealloc ((char *) type_vector,
		  type_vector_length * sizeof (struct type *));
      memset (&type_vector[old_vector_length], 0,
	     (type_vector_length - old_vector_length) * sizeof(struct type *));
    }
  return &type_vector[index];
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
  if (type == NULL)
    {
      type = alloc_type (current_objfile);
      *type_addr = type;
    }
  return type;
}

/* maintain the lists of symbols and blocks */

/* Add a symbol to one of the lists of symbols.  */
static void
coff_add_symbol_to_list (symbol, listhead)
     struct symbol *symbol;
     struct coff_pending **listhead;
{
  register struct coff_pending *link
    = (struct coff_pending *) xmalloc (sizeof (struct coff_pending));

  link->next = *listhead;
  link->symbol = symbol;
  *listhead = link;
}

/* Take one of the lists of symbols and make a block from it.
   Put the block on the list of pending blocks.  */

static void
coff_finish_block (symbol, listhead, old_blocks, start, end, objfile)
     struct symbol *symbol;
     struct coff_pending **listhead;
     struct pending_block *old_blocks;
     CORE_ADDR start, end;
     struct objfile *objfile;
{
  register struct coff_pending *next, *next1;
  register struct block *block;
  register struct pending_block *pblock;
  struct pending_block *opblock;
  register int i;

  /* Count the length of the list of symbols.  */

  for (next = *listhead, i = 0; next; next = next->next, i++);

  block = (struct block *)
	    obstack_alloc (&objfile->symbol_obstack, sizeof (struct block) + (i - 1) * sizeof (struct symbol *));

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
      free ((PTR)next);
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
make_blockvector (objfile)
     struct objfile *objfile;
{
  register struct pending_block *next, *next1;
  register struct blockvector *blockvector;
  register int i;

  /* Count the length of the list of blocks.  */

  for (next = pending_blocks, i = 0; next; next = next->next, i++);

  blockvector = (struct blockvector *)
		  obstack_alloc (&objfile->symbol_obstack, sizeof (struct blockvector) + (i - 1) * sizeof (struct block *));

  /* Copy the blocks into the blockvector.
     This is done in reverse order, which happens to put
     the blocks into the proper order (ascending starting address).
     coff_finish_block has hair to insert each block into the list
     after its subblocks in order to make sure this is true.  */

  BLOCKVECTOR_NBLOCKS (blockvector) = i;
  for (next = pending_blocks; next; next = next->next)
    BLOCKVECTOR_BLOCK (blockvector, --i) = next->block;

  /* Now free the links of the list, and empty the list.  */

  for (next = pending_blocks; next; next = next1)
    {
      next1 = next->next;
      free ((PTR)next);
    }
  pending_blocks = 0;

  return blockvector;
}

/* Manage the vector of line numbers.  */

static void
coff_record_line (line, pc)
     int line;
     CORE_ADDR pc;
{
  struct linetable_entry *e;
  /* Make sure line vector is big enough.  */

  if (line_vector_index + 2 >= line_vector_length)
    {
      line_vector_length *= 2;
      line_vector = (struct linetable *)
	xrealloc ((char *) line_vector, sizeof (struct linetable)
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
coff_start_symtab ()
{
  coff_file_symbols = 0;
  coff_global_symbols = 0;
  coff_context_stack = 0;
  within_function = 0;
  last_source_file = NULL;

  /* Initialize the source file line number information for this file.  */

  if (line_vector)		/* Unlikely, but maybe possible? */
    free ((PTR)line_vector);
  line_vector_index = 0;
  line_vector_length = 1000;
  prev_line_number = -2;	/* Force first line number to be explicit */
  line_vector = (struct linetable *)
    xmalloc (sizeof (struct linetable)
	     + line_vector_length * sizeof (struct linetable_entry));
}

/* Save the vital information from when starting to read a file,
   for use when closing off the current file.
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

  if (current_objfile -> ei.entry_point >= cur_src_start_addr &&
      current_objfile -> ei.entry_point <  cur_src_end_addr)
    {
      current_objfile -> ei.entry_file_lowpc = cur_src_start_addr;
      current_objfile -> ei.entry_file_highpc = cur_src_end_addr;
    }
}

/* Finish the symbol definitions for one main source file,
   close off all the lexical contexts for that file
   (creating struct block's for them), then make the
   struct symtab for that file and put it in the list of all such. */

static void
coff_end_symtab (objfile)
     struct objfile *objfile;
{
  register struct symtab *symtab;
  register struct coff_context_stack *cstk;
  register struct blockvector *blockvector;
  register struct linetable *lv;

  /* Finish the lexical context of the last function in the file.  */

  if (coff_context_stack)
    {
      cstk = coff_context_stack;
      coff_context_stack = 0;
      /* Make a block for the local symbols within.  */
      coff_finish_block (cstk->name, &coff_local_symbols, cstk->old_blocks,
		    cstk->start_addr, cur_src_end_addr, objfile);
      free ((PTR)cstk);
    }

  /* Ignore a file that has no functions with real debugging info.  */
  if (pending_blocks == 0 && coff_file_symbols == 0 && coff_global_symbols == 0)
    {
      free ((PTR)line_vector);
      line_vector = 0;
      line_vector_length = -1;
      last_source_file = NULL;
      return;
    }

  /* It is unfortunate that in amdcoff, pending blocks might not be ordered
     in this stage. Especially, blocks for static functions will show up at
     the end.  We need to sort them, so tools like `find_pc_function' and
     `find_pc_block' can work reliably. */
  if (pending_blocks) {
    /* FIXME!  Remove this horrid bubble sort and use qsort!!! */
    int swapped;
    do {
      struct pending_block *pb, *pbnext;

      pb = pending_blocks, pbnext = pb->next;
      swapped = 0;

      while ( pbnext ) {

	  /* swap blocks if unordered! */

	  if (BLOCK_START(pb->block) < BLOCK_START(pbnext->block)) {
	    struct block *tmp = pb->block;
	    complain (&misordered_blocks_complaint,
		      (char *) BLOCK_START (pb->block));
	    pb->block = pbnext->block;
	    pbnext->block = tmp;
	    swapped = 1;
	  }
	  pb = pbnext;
	  pbnext = pbnext->next;
      }
    } while (swapped);
  }

  /* Create the two top-level blocks for this file (STATIC_BLOCK and
     GLOBAL_BLOCK).  */
  coff_finish_block (0, &coff_file_symbols, 0, cur_src_start_addr, cur_src_end_addr, objfile);
  coff_finish_block (0, &coff_global_symbols, 0, cur_src_start_addr, cur_src_end_addr, objfile);

  /* Create the blockvector that points to all the file's blocks.  */
  blockvector = make_blockvector (objfile);

  /* Now create the symtab object for this source file.  */
  symtab = allocate_symtab (last_source_file, objfile);

  /* Fill in its components.  */
  symtab->blockvector = blockvector;
  symtab->free_code = free_linetable;
  symtab->free_ptr = 0;
  symtab->filename = last_source_file;
  symtab->dirname = NULL;
  lv = line_vector;
  lv->nitems = line_vector_index;
  symtab->linetable = (struct linetable *)
    xrealloc ((char *) lv, (sizeof (struct linetable)
		   + lv->nitems * sizeof (struct linetable_entry)));

  free_named_symtabs (symtab->filename);

  /* Reinitialize for beginning of new file. */
  line_vector = 0;
  line_vector_length = -1;
  last_source_file = NULL;
}

static void
record_minimal_symbol (name, address, type)
     char *name;
     CORE_ADDR address;
     enum minimal_symbol_type type;
{
  /* We don't want TDESC entry points in the minimal symbol table */
  if (name[0] == '@') return;

  /* mst_text isn't true, but apparently COFF doesn't tell us what it really
     is, so this guess is more useful than mst_unknown.  */
  prim_record_minimal_symbol (savestring (name, strlen (name)),
			     address,
			     type);
}

/* coff_symfile_init ()
   is the coff-specific initialization routine for reading symbols.
   It is passed a struct objfile which contains, among other things,
   the BFD for the file whose symbols are being read, and a slot for
   a pointer to "private data" which we fill with cookies and other
   treats for coff_symfile_read ().

   We will only be called if this is a COFF or COFF-like file.
   BFD handles figuring out the format of the file, and code in symtab.c
   uses BFD's determination to vector to us.

   The ultimate result is a new symtab (or, FIXME, eventually a psymtab).  */

struct coff_symfile_info {
  file_ptr min_lineno_offset;		/* Where in file lowest line#s are */
  file_ptr max_lineno_offset;		/* 1+last byte of line#s in file */
};

static int text_bfd_scnum;

static void
coff_symfile_init (objfile)
     struct objfile *objfile;
{
  asection	*section;
  bfd *abfd = objfile->obfd;

  /* Allocate struct to keep track of the symfile */
  objfile -> sym_private = xmmalloc (objfile -> md,
				     sizeof (struct coff_symfile_info));

  init_entry_point_info (objfile);

  /* Save the section number for the text section */
  section = bfd_get_section_by_name(abfd,".text");
  if (section)
    text_bfd_scnum = section->index;
  else
    text_bfd_scnum = -1; 
}

/* This function is called for every section; it finds the outer limits
   of the line table (minimum and maximum file offset) so that the
   mainline code can read the whole thing for efficiency.  */

/* ARGSUSED */
static void
find_linenos (abfd, asect, vpinfo)
     bfd *abfd;
     sec_ptr asect;
     PTR vpinfo;
{
  struct coff_symfile_info *info;
  int size, count;
  file_ptr offset, maxoff;

/* WARNING WILL ROBINSON!  ACCESSING BFD-PRIVATE DATA HERE!  FIXME!  */
  count = asect->lineno_count;
/* End of warning */

  if (count == 0)
    return;
  size = count * local_linesz;

  info = (struct coff_symfile_info *)vpinfo;
/* WARNING WILL ROBINSON!  ACCESSING BFD-PRIVATE DATA HERE!  FIXME!  */
  offset = asect->line_filepos;
/* End of warning */

  if (offset < info->min_lineno_offset || info->min_lineno_offset == 0)
    info->min_lineno_offset = offset;

  maxoff = offset + size;
  if (maxoff > info->max_lineno_offset)
    info->max_lineno_offset = maxoff;
}


/* The BFD for this file -- only good while we're actively reading
   symbols into a psymtab or a symtab.  */

static bfd *symfile_bfd;

/* Read a symbol file, after initialization by coff_symfile_init.  */
/* FIXME!  Addr and Mainline are not used yet -- this will not work for
   shared libraries or add_file!  */

/* ARGSUSED */
static void
coff_symfile_read (objfile, section_offsets, mainline)
     struct objfile *objfile;
     struct section_offsets *section_offsets;
     int mainline;
{
  struct coff_symfile_info *info;
  bfd *abfd = objfile->obfd;
  coff_data_type *cdata = coff_data (abfd);
  char *name = bfd_get_filename (abfd);
  int desc;
  register int val;
  int num_symbols;
  int symtab_offset;
  int stringtab_offset;

  info = (struct coff_symfile_info *) objfile -> sym_private;
  symfile_bfd = abfd;			/* Kludge for swap routines */

/* WARNING WILL ROBINSON!  ACCESSING BFD-PRIVATE DATA HERE!  FIXME!  */
   desc = fileno ((FILE *)(abfd->iostream));	/* File descriptor */
   num_symbols = bfd_get_symcount (abfd);	/* How many syms */
   symtab_offset = cdata->sym_filepos;		/* Symbol table file offset */
   stringtab_offset = symtab_offset +		/* String table file offset */
		      num_symbols * cdata->local_symesz;

  /* Set a few file-statics that give us specific information about
     the particular COFF file format we're reading.  */
  local_linesz   = cdata->local_linesz;
  local_n_btmask = cdata->local_n_btmask;
  local_n_btshft = cdata->local_n_btshft;
  local_n_tmask  = cdata->local_n_tmask;
  local_n_tshift = cdata->local_n_tshift;
  local_linesz   = cdata->local_linesz;
  local_symesz   = cdata->local_symesz;
  local_auxesz   = cdata->local_auxesz;

  /* Allocate space for raw symbol and aux entries, based on their
     space requirements as reported by BFD.  */
  temp_sym = (char *) xmalloc
	 (cdata->local_symesz + cdata->local_auxesz);
  temp_aux = temp_sym + cdata->local_symesz;
  make_cleanup (free_current_contents, &temp_sym);
/* End of warning */

  /* Read the line number table, all at once.  */
  info->min_lineno_offset = 0;
  info->max_lineno_offset = 0;
  bfd_map_over_sections (abfd, find_linenos, (PTR)info);

  val = init_lineno (desc, info->min_lineno_offset, 
		     info->max_lineno_offset - info->min_lineno_offset);
  if (val < 0)
    error ("\"%s\": error reading line numbers\n", name);

  /* Now read the string table, all at once.  */

  val = init_stringtab (desc, stringtab_offset);
  if (val < 0)
    error ("\"%s\": can't get string table", name);
  make_cleanup (free_stringtab, 0);

  init_minimal_symbol_collection ();
  make_cleanup (discard_minimal_symbols, 0);

  /* Now that the executable file is positioned at symbol table,
     process it and define symbols accordingly.  */

  read_coff_symtab ((long)symtab_offset, num_symbols, objfile);

  /* Sort symbols alphabetically within each block.  */

  sort_all_symtab_syms ();

  /* Install any minimal symbols that have been collected as the current
     minimal symbols for this objfile. */

  install_minimal_symbols (objfile);
}

static void
coff_new_init (ignore)
     struct objfile *ignore;
{
	/* Nothin' to do */
}

/* Perform any local cleanups required when we are done with a particular
   objfile.  I.E, we are in the process of discarding all symbol information
   for an objfile, freeing up all memory held for it, and unlinking the
   objfile struct from the global list of known objfiles. */

static void
coff_symfile_finish (objfile)
     struct objfile *objfile;
{
  if (objfile -> sym_private != NULL)
    {
      mfree (objfile -> md, objfile -> sym_private);
    }
}


/* Given pointers to a symbol table in coff style exec file,
   analyze them and create struct symtab's describing the symbols.
   NSYMS is the number of symbols in the symbol table.
   We read them one at a time using read_one_sym ().  */

static void
read_coff_symtab (symtab_offset, nsyms, objfile)
     long symtab_offset;
     int nsyms;
     struct objfile *objfile;
{
  FILE *stream; 
  register struct coff_context_stack *new;
  struct coff_symbol coff_symbol;
  register struct coff_symbol *cs = &coff_symbol;
  static struct internal_syment main_sym;
  static union internal_auxent main_aux;
  struct coff_symbol fcn_cs_saved;
  static struct internal_syment fcn_sym_saved;
  static union internal_auxent fcn_aux_saved;
  struct symtab *s;
  
  /* A .file is open.  */
  int in_source_file = 0;
  int num_object_files = 0;
  int next_file_symnum = -1;

  /* Name of the current file.  */
  char *filestring = "";
  int depth = 0;
  int fcn_first_line = 0;
  int fcn_last_line = 0;
  int fcn_start_addr = 0;
  long fcn_line_ptr = 0;
  struct cleanup *old_chain;
  int val;

  stream = fopen (objfile->name, FOPEN_RB);
  if (!stream)
   perror_with_name(objfile->name);

  /* Position to read the symbol table. */
  val = fseek (stream, (long)symtab_offset, 0);
  if (val < 0)
    perror_with_name (objfile->name);

  /* These cleanups will be discarded below if we succeed.  */
  old_chain = make_cleanup (free_objfile, objfile);
  make_cleanup (fclose, stream);

  current_objfile = objfile;
  nlist_stream_global = stream;
  nlist_nsyms_global = nsyms;
  last_source_file = NULL;
  memset (opaque_type_chain, 0, sizeof opaque_type_chain);

  if (type_vector)			/* Get rid of previous one */
    free ((PTR)type_vector);
  type_vector_length = 160;
  type_vector = (struct type **)
		xmalloc (type_vector_length * sizeof (struct type *));
  memset (type_vector, 0, type_vector_length * sizeof (struct type *));

  coff_start_symtab ();

  symnum = 0;
  while (symnum < nsyms)
    {
      QUIT;			/* Make this command interruptable.  */
      read_one_sym (cs, &main_sym, &main_aux);

#ifdef SEM
      temp_sem_val = cs->c_name[0] << 24 | cs->c_name[1] << 16 |
                     cs->c_name[2] << 8 | cs->c_name[3];
      if (int_sem_val == temp_sem_val)
        last_coffsem = (int) strtol (cs->c_name+4, (char **) NULL, 10);
#endif

      if (cs->c_symnum == next_file_symnum && cs->c_sclass != C_FILE)
	{
	  if (last_source_file)
	    coff_end_symtab (objfile);

	  coff_start_symtab ();
	  complete_symtab ("_globals_", 0, first_object_file_end);
	  /* done with all files, everything from here on out is globals */
	}

      /* Special case for file with type declarations only, no text.  */
      if (!last_source_file && SDB_TYPE (cs->c_type)
	  && cs->c_secnum == N_DEBUG)
	complete_symtab (filestring, 0, 0);

      /* Typedefs should not be treated as symbol definitions.  */
      if (ISFCN (cs->c_type) && cs->c_sclass != C_TPDEF)
	{
	  /* record as a minimal symbol.  if we get '.bf' next,
	   * then we undo this step
	   */
	  record_minimal_symbol (cs->c_name, cs->c_value, mst_text);

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
	    complain (&bad_sclass_complaint, cs->c_name);
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
		coff_end_symtab (objfile);
		coff_start_symtab ();
	      }
	    in_source_file = 1;
	    break;

          case C_STAT:
	    if (cs->c_name[0] == '.') {
		    if (strcmp (cs->c_name, ".text") == 0) {
			    /* FIXME:  don't wire in ".text" as section name
				       or symbol name! */
			    if (++num_object_files == 1) {
				    /* last address of startup file */
				    first_object_file_end = cs->c_value +
					    main_aux.x_scn.x_scnlen;
			    }
			    /* Check for in_source_file deals with case of
			       a file with debugging symbols
			       followed by a later file with no symbols.  */
			    if (in_source_file)
			      complete_symtab (filestring, cs->c_value,
					       main_aux.x_scn.x_scnlen);
			    in_source_file = 0;
		    }
		    /* flush rest of '.' symbols */
		    break;
	    }
	    else if (!SDB_TYPE (cs->c_type)
		     && cs->c_name[0] == 'L'
		     && (strncmp (cs->c_name, "LI%", 3) == 0
			 || strncmp (cs->c_name, "LF%", 3) == 0
			 || strncmp (cs->c_name,"LC%",3) == 0
			 || strncmp (cs->c_name,"LP%",3) == 0
			 || strncmp (cs->c_name,"LPB%",4) == 0
			 || strncmp (cs->c_name,"LBB%",4) == 0
			 || strncmp (cs->c_name,"LBE%",4) == 0
			 || strncmp (cs->c_name,"LPBX%",5) == 0))
	      /* At least on a 3b1, gcc generates swbeg and string labels
		 that look like this.  Ignore them.  */
	      break;
	    /* fall in for static symbols that don't start with '.' */
	  case C_EXT:
	    if (!SDB_TYPE (cs->c_type)) {
		/* FIXME: This is BOGUS Will Robinson! 
	 	Coff should provide the SEC_CODE flag for executable sections,
	 	then if we could look up sections by section number we
  	 	could see if the flags indicate SEC_CODE.  If so, then
	 	record this symbol as a function in the minimal symbol table.
		But why are absolute syms recorded as functions, anyway?  */
		    if (cs->c_secnum <= text_bfd_scnum+1) {/* text or abs */
			    record_minimal_symbol (cs->c_name, cs->c_value,
						   mst_text);
			    break;
		    } else {
			    record_minimal_symbol (cs->c_name, cs->c_value,
						   mst_data);
			    break;
		    }
	    }
	    process_coff_symbol (cs, &main_aux, objfile);
	    break;

	  case C_FCN:
	    if (strcmp (cs->c_name, ".bf") == 0)
	      {
		within_function = 1;

		/* value contains address of first non-init type code */
		/* main_aux.x_sym.x_misc.x_lnsz.x_lnno
			    contains line number of '{' } */
		if (cs->c_naux != 1)
		  complain (&bf_no_aux_complaint, (char *) cs->c_symnum);
		fcn_first_line = main_aux.x_sym.x_misc.x_lnsz.x_lnno;

		new = (struct coff_context_stack *)
		  xmalloc (sizeof (struct coff_context_stack));
		new->depth = depth = 0;
		new->next = 0;
		coff_context_stack = new;
		new->locals = 0;
		new->old_blocks = pending_blocks;
		new->start_addr = fcn_start_addr;
		fcn_cs_saved.c_name = getsymname (&fcn_sym_saved);
		new->name = process_coff_symbol (&fcn_cs_saved,
						 &fcn_aux_saved, objfile);
	      }
	    else if (strcmp (cs->c_name, ".ef") == 0)
	      {
		      /* the value of .ef is the address of epilogue code;
		       * not useful for gdb
		       */
		/* { main_aux.x_sym.x_misc.x_lnsz.x_lnno
			    contains number of lines to '}' */
		new = coff_context_stack;
		if (new == 0)
		  {
		    complain (&ef_complaint, (char *) cs->c_symnum);
		    within_function = 0;
		    break;
		  }
		if (cs->c_naux != 1) {
		  complain (&ef_no_aux_complaint, (char *) cs->c_symnum);
		  fcn_last_line = 0x7FFFFFFF;
		} else {
		  fcn_last_line = main_aux.x_sym.x_misc.x_lnsz.x_lnno;
		}
		enter_linenos (fcn_line_ptr, fcn_first_line, fcn_last_line);

		coff_finish_block (new->name, &coff_local_symbols, new->old_blocks,
			      new->start_addr,
#if defined (FUNCTION_EPILOGUE_SIZE)
			      /* This macro should be defined only on
				 machines where the
				 fcn_aux_saved.x_sym.x_misc.x_fsize
				 field is always zero.
				 So use the .bf record information that
				 points to the epilogue and add the size
				 of the epilogue.  */
			      cs->c_value + FUNCTION_EPILOGUE_SIZE,
#else
			      fcn_cs_saved.c_value +
			          fcn_aux_saved.x_sym.x_misc.x_fsize,
#endif
			      objfile
			      );
		coff_context_stack = 0;
		within_function = 0;
		free ((PTR)new);
	      }
	    break;

	  case C_BLOCK:
	    if (strcmp (cs->c_name, ".bb") == 0)
	      {
		new = (struct coff_context_stack *)
			    xmalloc (sizeof (struct coff_context_stack));
		depth++;
		new->depth = depth;
		new->next = coff_context_stack;
		coff_context_stack = new;
		new->locals = coff_local_symbols;
		new->old_blocks = pending_blocks;
		new->start_addr = cs->c_value;
		new->name = 0;
		coff_local_symbols = 0;
	      }
	    else if (strcmp (cs->c_name, ".eb") == 0)
	      {
		new = coff_context_stack;
		if (new == 0 || depth != new->depth)
		  error ("Invalid symbol data: .bb/.eb symbol mismatch at symbol %d.",
			 symnum);
		if (coff_local_symbols && coff_context_stack->next)
		  {
		    /* Make a block for the local symbols within.  */
		    coff_finish_block (0, &coff_local_symbols, new->old_blocks,
				  new->start_addr, cs->c_value, objfile);
		  }
		depth--;
		coff_local_symbols = new->locals;
		coff_context_stack = new->next;
		free ((PTR)new);
	      }
	    break;

	  default:
	    process_coff_symbol (cs, &main_aux, objfile);
	    break;
	}
    }

  if (last_source_file)
    coff_end_symtab (objfile);
  fclose (stream);

  /* Patch up any opaque types (references to types that are not defined
     in the file where they are referenced, e.g. "struct foo *bar").  */
  ALL_OBJFILE_SYMTABS (objfile, s)
    patch_opaque_types (s);

  discard_cleanups (old_chain);
  current_objfile = NULL;
}

/* Routines for reading headers and symbols from executable.  */

#ifdef FIXME
/* Move these XXXMAGIC symbol defns into BFD!  */

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
#if defined (MC68KWRMAGIC) \
  && (!defined (MC68MAGIC) || MC68KWRMAGIC != MC68MAGIC)
    case MC68KWRMAGIC:
#endif
#ifdef MC68KROMAGIC
    case MC68KROMAGIC:
    case MC68KPGMAGIC:
#endif
#ifdef MC88DGMAGIC
    case MC88DGMAGIC:
#endif      
#ifdef MC88MAGIC
    case MC88MAGIC:
#endif      
#ifdef I960ROMAGIC
    case I960ROMAGIC:		/* Intel 960 */
#endif
#ifdef I960RWMAGIC
    case I960RWMAGIC:		/* Intel 960 */
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
#endif

/* Read the next symbol, swap it, and return it in both internal_syment
   form, and coff_symbol form.  Also return its first auxent, if any,
   in internal_auxent form, and skip any other auxents.  */

static void
read_one_sym (cs, sym, aux)
    register struct coff_symbol *cs;
    register struct internal_syment *sym;
    register union internal_auxent *aux;
{
  int i;

  cs->c_symnum = symnum;
  fread (temp_sym, local_symesz, 1, nlist_stream_global);
  bfd_coff_swap_sym_in (symfile_bfd, temp_sym, (char *)sym);
  cs->c_naux = sym->n_numaux & 0xff;
  if (cs->c_naux >= 1)
    {
    fread (temp_aux, local_auxesz, 1, nlist_stream_global);
    bfd_coff_swap_aux_in (symfile_bfd, temp_aux, sym->n_type, sym->n_sclass,
			  (char *)aux);
    /* If more than one aux entry, read past it (only the first aux
       is important). */
    for (i = 1; i < cs->c_naux; i++)
      fread (temp_aux, local_auxesz, 1, nlist_stream_global);
    }
  cs->c_name = getsymname (sym);
  cs->c_value = sym->n_value;
  cs->c_sclass = (sym->n_sclass & 0xff);
  cs->c_secnum = sym->n_scnum;
  cs->c_type = (unsigned) sym->n_type;
  if (!SDB_TYPE (cs->c_type))
    cs->c_type = 0;

  symnum += 1 + cs->c_naux;
}

/* Support for string table handling */

static char *stringtab = NULL;

static int
init_stringtab (chan, offset)
    int chan;
    long offset;
{
  long length;
  int val;
  unsigned char lengthbuf[4];

  if (stringtab)
    {
      free (stringtab);
      stringtab = NULL;
    }

  if (lseek (chan, offset, 0) < 0)
    return -1;

  val = myread (chan, (char *)lengthbuf, sizeof lengthbuf);
  length = bfd_h_get_32 (symfile_bfd, lengthbuf);

  /* If no string table is needed, then the file may end immediately
     after the symbols.  Just return with `stringtab' set to null. */
  if (val != sizeof length || length < sizeof length)
    return 0;

  stringtab = (char *) xmalloc (length);
  if (stringtab == NULL)
    return -1;

  memcpy (stringtab, &length, sizeof length);
  if (length == sizeof length)		/* Empty table -- just the count */
    return 0;

  val = myread (chan, stringtab + sizeof length, length - sizeof length);
  if (val != length - sizeof length || stringtab[length - 1] != '\0')
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
    struct internal_syment *symbol_entry;
{
  static char buffer[SYMNMLEN+1];
  char *result;

  if (symbol_entry->_n._n_n._n_zeroes == 0)
    {
      result = stringtab + symbol_entry->_n._n_n._n_offset;
    }
  else
    {
      strncpy (buffer, symbol_entry->_n._n_name, SYMNMLEN);
      buffer[SYMNMLEN] = '\0';
      result = buffer;
    }
  return result;
}

static char *
getfilename (aux_entry)
    union internal_auxent *aux_entry;
{
  static char buffer[BUFSIZ];
  register char *temp;
  char *result;

#ifndef COFF_NO_LONG_FILE_NAMES
#if defined (x_zeroes)
  /* Data General.  */
  if (aux_entry->x_zeroes == 0)
    strcpy (buffer, stringtab + aux_entry->x_offset);
#else /* no x_zeroes */
  if (aux_entry->x_file.x_n.x_zeroes == 0)
    strcpy (buffer, stringtab + aux_entry->x_file.x_n.x_offset);
#endif /* no x_zeroes */
  else
#endif /* COFF_NO_LONG_FILE_NAMES */
    {
#if defined (x_name)
      /* Data General.  */
      strncpy (buffer, aux_entry->x_name, FILNMLEN);
#else
      strncpy (buffer, aux_entry->x_file.x_fname, FILNMLEN);
#endif
      buffer[FILNMLEN] = '\0';
    }
  result = buffer;
  if ((temp = strrchr (result, '/')) != NULL)
    result = temp + 1;
  return (result);
}

/* Support for line number handling */
static char *linetab = NULL;
static long linetab_offset;
static unsigned long linetab_size;

/* Read in all the line numbers for fast lookups later.  Leave them in
   external (unswapped) format in memory; we'll swap them as we enter
   them into GDB's data structures.  */
 
static int
init_lineno (chan, offset, size)
    int chan;
    long offset;
    int size;
{
  int val;

  linetab_offset = offset;
  linetab_size = size;

  if (size == 0)
    return 0;

  if (lseek (chan, offset, 0) < 0)
    return -1;
  
  /* Allocate the desired table, plus a sentinel */
  linetab = (char *) xmalloc (size + local_linesz);

  val = myread (chan, linetab, size);
  if (val != size)
    return -1;

  /* Terminate it with an all-zero sentinel record */
  memset (linetab + size, 0, local_linesz);

  make_cleanup (free, linetab);		/* Be sure it gets de-allocated. */
  return 0;
}

#if !defined (L_LNNO32)
#define L_LNNO32(lp) ((lp)->l_lnno)
#endif

static void
enter_linenos (file_offset, first_line, last_line)
    long file_offset;
    register int first_line;
    register int last_line;
{
  register char *rawptr;
  struct internal_lineno lptr;

  if (file_offset < linetab_offset)
    {
      complain (&lineno_complaint, (char *) file_offset);
      if (file_offset > linetab_size)	/* Too big to be an offset? */
	return;
      file_offset += linetab_offset;  /* Try reading at that linetab offset */
    }
  
  rawptr = &linetab[file_offset - linetab_offset];

  /* skip first line entry for each function */
  rawptr += local_linesz;
  /* line numbers start at one for the first line of the function */
  first_line--;

  for (;;) {
    bfd_coff_swap_lineno_in (symfile_bfd, rawptr, &lptr);
    rawptr += local_linesz;
    /* The next function, or the sentinel, will have L_LNNO32 zero; we exit. */
    if (L_LNNO32 (&lptr) && L_LNNO32 (&lptr) <= last_line)
      coff_record_line (first_line + L_LNNO32 (&lptr), lptr.l_addr.l_paddr);
    else
      break;
  } 
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
  TYPE_FIELDS (target) = (struct field *) TYPE_ALLOC (target, field_size);

  memcpy (TYPE_FIELDS (target), TYPE_FIELDS (real_target), field_size);

  if (TYPE_NAME (real_target))
    {
      if (TYPE_NAME (target))
	free (TYPE_NAME (target));
      TYPE_NAME (target) = concat (TYPE_NAME (real_target), NULL);
    }
}

/* Patch up all appropriate typedef symbols in the opaque_type_chains
   so that they can be used to print out opaque data structures properly.  */

static void
patch_opaque_types (s)
     struct symtab *s;
{
  register struct block *b;
  register int i;
  register struct symbol *real_sym;
  
  /* Go through the per-file symbols only */
  b = BLOCKVECTOR_BLOCK (BLOCKVECTOR (s), STATIC_BLOCK);
  for (i = BLOCK_NSYMS (b) - 1; i >= 0; i--)
    {
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
		    {
		      SYMBOL_VALUE_CHAIN (prev) = SYMBOL_VALUE_CHAIN (sym);
		    }
		  else
		    {
		      opaque_type_chain[hash] = SYMBOL_VALUE_CHAIN (sym);
		    }
		  
		  patch_type (SYMBOL_TYPE (sym), SYMBOL_TYPE (real_sym));
		  
		  if (prev)
		    {
		      sym = SYMBOL_VALUE_CHAIN (prev);
		    }
		  else
		    {
		      sym = opaque_type_chain[hash];
		    }
		}
	      else
		{
		  prev = sym;
		  sym = SYMBOL_VALUE_CHAIN (sym);
		}
	    }
	}
    }
}

static struct symbol *
process_coff_symbol (cs, aux, objfile)
     register struct coff_symbol *cs;
     register union internal_auxent *aux;
     struct objfile *objfile;
{
  register struct symbol *sym
    = (struct symbol *) obstack_alloc (&objfile->symbol_obstack, sizeof (struct symbol));
  char *name;
#ifdef NAMES_HAVE_UNDERSCORE
  int offset = 1;
#else
  int offset = 0;
#endif
  struct type *temptype;

  memset (sym, 0, sizeof (struct symbol));
  name = cs->c_name;
  name = (name[0] == '_' ? name + offset : name);
  SYMBOL_NAME (sym) = obstack_copy0 (&objfile->symbol_obstack, name, strlen (name));

  /* default assumptions */
  SYMBOL_VALUE (sym) = cs->c_value;
  SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;

  if (ISFCN (cs->c_type))
    {
#if 0
       /* FIXME:  This has NOT been tested.  The DBX version has.. */
       /* Generate a template for the type of this function.  The 
	  types of the arguments will be added as we read the symbol 
	  table. */
       struct type *new = (struct type *)
		    obstack_alloc (&objfile->symbol_obstack, sizeof (struct type));
       
       memcpy (new, lookup_function_type (decode_function_type (cs, cs->c_type, aux)),
		      sizeof(struct type));
       SYMBOL_TYPE (sym) = new;
       in_function_type = SYMBOL_TYPE(sym);
#else
       SYMBOL_TYPE(sym) = 
	 lookup_function_type (decode_function_type (cs, cs->c_type, aux));
#endif

      SYMBOL_CLASS (sym) = LOC_BLOCK;
      if (cs->c_sclass == C_STAT)
	coff_add_symbol_to_list (sym, &coff_file_symbols);
      else if (cs->c_sclass == C_EXT)
	coff_add_symbol_to_list (sym, &coff_global_symbols);
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
	    coff_add_symbol_to_list (sym, &coff_local_symbols);
	    break;

	  case C_EXT:
	    SYMBOL_CLASS (sym) = LOC_STATIC;
	    SYMBOL_VALUE_ADDRESS (sym) = (CORE_ADDR) cs->c_value;
	    coff_add_symbol_to_list (sym, &coff_global_symbols);
	    break;

	  case C_STAT:
	    SYMBOL_CLASS (sym) = LOC_STATIC;
	    SYMBOL_VALUE_ADDRESS (sym) = (CORE_ADDR) cs->c_value;
	    if (within_function) {
	      /* Static symbol of local scope */
	      coff_add_symbol_to_list (sym, &coff_local_symbols);
	    }
	    else {
	      /* Static symbol at top level of file */
	      coff_add_symbol_to_list (sym, &coff_file_symbols);
	    }
	    break;

#ifdef C_GLBLREG		/* AMD coff */
	  case C_GLBLREG:
#endif
	  case C_REG:
	    SYMBOL_CLASS (sym) = LOC_REGISTER;
	    SYMBOL_VALUE (sym) = SDB_REG_TO_REGNUM(cs->c_value);
	    coff_add_symbol_to_list (sym, &coff_local_symbols);
	    break;

	  case C_LABEL:
	    break;

	  case C_ARG:
	    SYMBOL_CLASS (sym) = LOC_ARG;
#if 0
	    /* FIXME:  This has not been tested. */
	    /* Add parameter to function.  */
	    add_param_to_type(&in_function_type,sym);
#endif
	    coff_add_symbol_to_list (sym, &coff_local_symbols);
#if !defined (BELIEVE_PCC_PROMOTION)
	    /* If PCC says a parameter is a short or a char,
	       it is really an int.  */
	    temptype = lookup_fundamental_type (current_objfile, FT_INTEGER);
	    if (TYPE_LENGTH (SYMBOL_TYPE (sym)) < TYPE_LENGTH (temptype)
		&& TYPE_CODE (SYMBOL_TYPE (sym)) == TYPE_CODE_INT)
		{
		    SYMBOL_TYPE (sym) = TYPE_UNSIGNED (SYMBOL_TYPE (sym))
			? lookup_fundamental_type (current_objfile,
						   FT_UNSIGNED_INTEGER)
			    : temptype;
		}
#endif
	    break;

	  case C_REGPARM:
	    SYMBOL_CLASS (sym) = LOC_REGPARM;
	    SYMBOL_VALUE (sym) = SDB_REG_TO_REGNUM(cs->c_value);
	    coff_add_symbol_to_list (sym, &coff_local_symbols);
#if !defined (BELIEVE_PCC_PROMOTION)
	    /* If PCC says a parameter is a short or a char,
	       it is really an int.  */
	    temptype = lookup_fundamental_type (current_objfile, FT_INTEGER);
	    if (TYPE_LENGTH (SYMBOL_TYPE (sym)) < TYPE_LENGTH (temptype)
		&& TYPE_CODE (SYMBOL_TYPE (sym)) == TYPE_CODE_INT)
		{
		    SYMBOL_TYPE (sym) = TYPE_UNSIGNED (SYMBOL_TYPE (sym))
			? lookup_fundamental_type (current_objfile,
						   FT_UNSIGNED_INTEGER)
			    : temptype;
		}
#endif
	    break;
	    
	  case C_TPDEF:
	    SYMBOL_CLASS (sym) = LOC_TYPEDEF;
	    SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;

	    /* If type has no name, give it one */
	    if (TYPE_NAME (SYMBOL_TYPE (sym)) == 0)
	      TYPE_NAME (SYMBOL_TYPE (sym)) = concat (SYMBOL_NAME (sym), NULL);

	    /* Keep track of any type which points to empty structured type,
		so it can be filled from a definition from another file.  A
		simple forward reference (TYPE_CODE_UNDEF) is not an
		empty structured type, though; the forward references
		work themselves out via the magic of coff_lookup_type.  */
	    if (TYPE_CODE (SYMBOL_TYPE (sym)) == TYPE_CODE_PTR &&
		TYPE_LENGTH (TYPE_TARGET_TYPE (SYMBOL_TYPE (sym))) == 0 &&
		TYPE_CODE   (TYPE_TARGET_TYPE (SYMBOL_TYPE (sym))) !=
						TYPE_CODE_UNDEF)
	      {
		register int i = hashname (SYMBOL_NAME (sym));

		SYMBOL_VALUE_CHAIN (sym) = opaque_type_chain[i];
		opaque_type_chain[i] = sym;
	      }
	    coff_add_symbol_to_list (sym, &coff_file_symbols);
	    break;

	  case C_STRTAG:
	  case C_UNTAG:
	  case C_ENTAG:
	    SYMBOL_CLASS (sym) = LOC_TYPEDEF;
	    SYMBOL_NAMESPACE (sym) = STRUCT_NAMESPACE;
	    if (TYPE_NAME (SYMBOL_TYPE (sym)) == 0)
	      TYPE_NAME (SYMBOL_TYPE (sym))
		= concat ("",
			  (cs->c_sclass == C_ENTAG
			   ? "enum "
			   : (cs->c_sclass == C_STRTAG
			      ? "struct " : "union ")),
			  SYMBOL_NAME (sym), NULL);
	    coff_add_symbol_to_list (sym, &coff_file_symbols);
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
     register union internal_auxent *aux;
{
  register struct type *type = 0;
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
	  if (aux->x_sym.x_tagndx.l == 0)
	    cs->c_naux = 0;

	  /* shift the indices down */
	  dim = &aux->x_sym.x_fcnary.x_ary.x_dimen[0];
	  i = 1;
	  n = dim[0];
	  for (i = 0; *dim && i < DIMNUM - 1; i++, dim++)
	    *dim = *(dim + 1);
	  *dim = 0;

	  type = alloc_type (current_objfile);

	  base_type = decode_type (cs, new_c_type, aux);

	  TYPE_CODE (type) = TYPE_CODE_ARRAY;
	  TYPE_TARGET_TYPE (type) = base_type;
	  TYPE_LENGTH (type) = n * TYPE_LENGTH (base_type);
	}
      return type;
    }

  /* Reference to existing type.  This only occurs with the
     struct, union, and enum types.  EPI a29k coff
     fakes us out by producing aux entries with a nonzero
     x_tagndx for definitions of structs, unions, and enums, so we
     have to check the c_sclass field.  */
  if (cs->c_naux > 0 && aux->x_sym.x_tagndx.l != 0)
    {
      if  (cs->c_sclass != C_STRTAG
	&& cs->c_sclass != C_UNTAG
	&& cs->c_sclass != C_ENTAG)
	{
	  type = coff_alloc_type (aux->x_sym.x_tagndx.l);
	  return type;
	} else {
	  complain (&tagndx_bad_complaint, cs->c_name);
	  /* And fall through to decode_base_type... */
	}
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
     register union internal_auxent *aux;
{
  if (aux->x_sym.x_tagndx.l == 0)
    cs->c_naux = 0;	/* auxent refers to function, not base type */

  return decode_type (cs, DECREF (c_type), aux);
}

/* basic C types */

static
struct type *
decode_base_type (cs, c_type, aux)
     register struct coff_symbol *cs;
     unsigned int c_type;
     register union internal_auxent *aux;
{
  struct type *type;

  switch (c_type)
    {
      case T_NULL:
        /* shows up with "void (*foo)();" structure members */
	return lookup_fundamental_type (current_objfile, FT_VOID);

#if 0
/* DGUX actually defines both T_ARG and T_VOID to the same value.  */
#ifdef T_ARG
      case T_ARG:
	/* Shows up in DGUX, I think.  Not sure where.  */
	return lookup_fundamental_type (current_objfile, FT_VOID);	/* shouldn't show up here */
#endif
#endif /* 0 */

#ifdef T_VOID
      case T_VOID:
	/* Intel 960 COFF has this symbol and meaning.  */
	return lookup_fundamental_type (current_objfile, FT_VOID);
#endif

      case T_CHAR:
	return lookup_fundamental_type (current_objfile, FT_CHAR);

      case T_SHORT:
	return lookup_fundamental_type (current_objfile, FT_SHORT);

      case T_INT:
	return lookup_fundamental_type (current_objfile, FT_INTEGER);

      case T_LONG:
	return lookup_fundamental_type (current_objfile, FT_LONG);

      case T_FLOAT:
	return lookup_fundamental_type (current_objfile, FT_FLOAT);

      case T_DOUBLE:
	return lookup_fundamental_type (current_objfile, FT_DBL_PREC_FLOAT);

      case T_STRUCT:
	if (cs->c_naux != 1)
	  {
	    /* anonymous structure type */
	    type = coff_alloc_type (cs->c_symnum);
	    TYPE_CODE (type) = TYPE_CODE_STRUCT;
	    TYPE_NAME (type) = concat ("struct ", "<opaque>", NULL);
	    INIT_CPLUS_SPECIFIC(type);
	    TYPE_LENGTH (type) = 0;
	    TYPE_FIELDS (type) = 0;
	    TYPE_NFIELDS (type) = 0;
	  }
	else
	  {
	    type = coff_read_struct_type (cs->c_symnum,
				    aux->x_sym.x_misc.x_lnsz.x_size,
				    aux->x_sym.x_fcnary.x_fcn.x_endndx.l);
	  }
	return type;

      case T_UNION:
	if (cs->c_naux != 1)
	  {
	    /* anonymous union type */
	    type = coff_alloc_type (cs->c_symnum);
	    TYPE_NAME (type) = concat ("union ", "<opaque>", NULL);
	    INIT_CPLUS_SPECIFIC(type);
	    TYPE_LENGTH (type) = 0;
	    TYPE_LENGTH (type) = 0;
	    TYPE_FIELDS (type) = 0;
	    TYPE_NFIELDS (type) = 0;
	  }
	else
	  {
	    type = coff_read_struct_type (cs->c_symnum,
				    aux->x_sym.x_misc.x_lnsz.x_size,
				    aux->x_sym.x_fcnary.x_fcn.x_endndx.l);
	  }
	TYPE_CODE (type) = TYPE_CODE_UNION;
	return type;

      case T_ENUM:
	return coff_read_enum_type (cs->c_symnum,
				    aux->x_sym.x_misc.x_lnsz.x_size,
				    aux->x_sym.x_fcnary.x_fcn.x_endndx.l);

      case T_MOE:
	/* shouldn't show up here */
	break;

      case T_UCHAR:
	return lookup_fundamental_type (current_objfile, FT_UNSIGNED_CHAR);

      case T_USHORT:
	return lookup_fundamental_type (current_objfile, FT_UNSIGNED_SHORT);

      case T_UINT:
	return lookup_fundamental_type (current_objfile, FT_UNSIGNED_INTEGER);

      case T_ULONG:
	return lookup_fundamental_type (current_objfile, FT_UNSIGNED_LONG);
    }
  complain (&unexpected_type_complaint, cs->c_name);
  return lookup_fundamental_type (current_objfile, FT_VOID);
}

/* This page contains subroutines of read_type.  */

/* Read the description of a structure (or union type)
   and return an object describing the type.  */

static struct type *
coff_read_struct_type (index, length, lastsym)
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
  struct internal_syment sub_sym;
  union internal_auxent sub_aux;
  int done = 0;

  type = coff_alloc_type (index);
  TYPE_CODE (type) = TYPE_CODE_STRUCT;
  INIT_CPLUS_SPECIFIC(type);
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
    TYPE_ALLOC (type, sizeof (struct field) * nfields);

  /* Copy the saved-up fields into the field vector.  */

  for (n = nfields; list; list = list->next)
    TYPE_FIELD (type, --n) = list->field;

  return type;
}

/* Read a definition of an enumeration type,
   and create and return a suitable type object.
   Also defines the symbols that represent the values of the type.  */
/* Currently assumes it's sizeof (int) and doesn't use length.  */

/* ARGSUSED */
static struct type *
coff_read_enum_type (index, length, lastsym)
     int index;
     int length;
     int lastsym;
{
  register struct symbol *sym;
  register struct type *type;
  int nsyms = 0;
  int done = 0;
  struct coff_pending **symlist;
  struct coff_symbol member_sym;
  register struct coff_symbol *ms = &member_sym;
  struct internal_syment sub_sym;
  union internal_auxent sub_aux;
  struct coff_pending *osyms, *syms;
  register int n;
  char *name;
#ifdef NAMES_HAVE_UNDERSCORE
  int offset = 1;
#else
  int offset = 0;
#endif

  type = coff_alloc_type (index);
  if (within_function)
    symlist = &coff_local_symbols;
  else
    symlist = &coff_file_symbols;
  osyms = *symlist;

  while (!done && symnum < lastsym && symnum < nlist_nsyms_global)
    {
      read_one_sym (ms, &sub_sym, &sub_aux);
      name = ms->c_name;
      name = (name[0] == '_' ? name + offset : name);

      switch (ms->c_sclass)
	{
	  case C_MOE:
	    sym = (struct symbol *) xmalloc (sizeof (struct symbol));
	    memset (sym, 0, sizeof (struct symbol));

	    SYMBOL_NAME (sym) = savestring (name, strlen (name));
	    SYMBOL_CLASS (sym) = LOC_CONST;
	    SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
	    SYMBOL_VALUE (sym) = ms->c_value;
	    coff_add_symbol_to_list (sym, symlist);
	    nsyms++;
	    break;

	  case C_EOS:
	    /* Sometimes the linker (on 386/ix 2.0.2 at least) screws
	       up the count of how many symbols to read.  So stop
	       on .eos.  */
	    done = 1;
	    break;
	}
    }

  /* Now fill in the fields of the type-structure.  */

  TYPE_LENGTH (type) =  TARGET_INT_BIT / TARGET_CHAR_BIT;
  TYPE_CODE (type) = TYPE_CODE_ENUM;
  TYPE_NFIELDS (type) = nsyms;
  TYPE_FIELDS (type) = (struct field *)
    TYPE_ALLOC (type, sizeof (struct field) * nsyms);

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
  /* Is this Modula-2's BOOLEAN type?  Flag it as such if so. */
  if(TYPE_NFIELDS(type) == 2 &&
     ((!strcmp(TYPE_FIELD_NAME(type,0),"TRUE") &&
       !strcmp(TYPE_FIELD_NAME(type,1),"FALSE")) ||
      (!strcmp(TYPE_FIELD_NAME(type,1),"TRUE") &&
       !strcmp(TYPE_FIELD_NAME(type,0),"FALSE"))))
     TYPE_CODE(type) = TYPE_CODE_BOOL;
  return type;
}

/* Fake up support for relocating symbol addresses.  FIXME.  */

struct section_offsets coff_symfile_faker = {0};

struct section_offsets *
coff_symfile_offsets (objfile, addr)
     struct objfile *objfile;
     CORE_ADDR addr;
{
  return &coff_symfile_faker;
}

/* Register our ability to parse symbols for coff BFD files */

static struct sym_fns coff_sym_fns =
{
  "coff",		/* sym_name: name or name prefix of BFD target type */
  4,			/* sym_namelen: number of significant sym_name chars */
  coff_new_init,	/* sym_new_init: init anything gbl to entire symtab */
  coff_symfile_init,	/* sym_init: read initial info, setup for sym_read() */
  coff_symfile_read,	/* sym_read: read a symbol file into symtab */
  coff_symfile_finish,	/* sym_finish: finished with file, cleanup */
  coff_symfile_offsets, /* sym_offsets:  xlate external to internal form */
  NULL			/* next: pointer to next struct sym_fns */
};

void
_initialize_coffread ()
{
  add_symtab_fns(&coff_sym_fns);
}
