/* Support routines for building symbol tables in GDB's internal format.
   Copyright 1986, 1987, 1988, 1989, 1990, 1991, 1992
             Free Software Foundation, Inc.

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

/* This module provides subroutines used for creating and adding to
   the symbol table.  These routines are called from various symbol-
   file-reading routines.

   Routines to support specific debugging information formats (stabs,
   DWARF, etc) belong somewhere else. */

#include "defs.h"
#include "bfd.h"
#include "obstack.h"
#include "symtab.h"
#include "symfile.h"		/* Needed for "struct complaint" */
#include "objfiles.h"
#include <string.h>

/* Ask buildsym.h to define the vars it normally declares `extern'.  */
#define	EXTERN	/**/
#include "buildsym.h"		/* Our own declarations */
#undef	EXTERN

static int
compare_line_numbers PARAMS ((const void *, const void *));

static struct blockvector *
make_blockvector PARAMS ((struct objfile *));


/* Initial sizes of data structures.  These are realloc'd larger if needed,
   and realloc'd down to the size actually used, when completed.  */

#define	INITIAL_CONTEXT_STACK_SIZE	10
#define	INITIAL_LINE_VECTOR_LENGTH	1000


/* Complaints about the symbols we have encountered.  */

struct complaint innerblock_complaint =
  {"inner block not inside outer block in %s", 0, 0};

struct complaint innerblock_anon_complaint =
  {"inner block not inside outer block", 0, 0};

struct complaint blockvector_complaint = 
  {"block at 0x%x out of order", 0, 0};


/* maintain the lists of symbols and blocks */

/* Add a symbol to one of the lists of symbols.  */

void
add_symbol_to_list (symbol, listhead)
     struct symbol *symbol;
     struct pending **listhead;
{
  register struct pending *link;
      
  /* We keep PENDINGSIZE symbols in each link of the list.
     If we don't have a link with room in it, add a new link.  */
  if (*listhead == NULL || (*listhead)->nsyms == PENDINGSIZE)
    {
      if (free_pendings)
	{
	  link = free_pendings;
	  free_pendings = link->next;
	}
      else
	{
	  link = (struct pending *) xmalloc (sizeof (struct pending));
	}

      link->next = *listhead;
      *listhead = link;
      link->nsyms = 0;
    }

  (*listhead)->symbol[(*listhead)->nsyms++] = symbol;
}

/* Find a symbol on a pending list.  */

struct symbol *
find_symbol_in_list (list, name, length)
     struct pending *list;
     char *name;
     int length;
{
  int j;
  char *pp;

  while (list != NULL)
    {
      for (j = list->nsyms; --j >= 0; )
	{
	  pp = SYMBOL_NAME (list->symbol[j]);
	  if (*pp == *name && strncmp (pp, name, length) == 0 &&
	      pp[length] == '\0')
	    {
	      return (list->symbol[j]);
	    }
	}
      list = list->next;
    }
  return (NULL);
}

/* At end of reading syms, or in case of quit,
   really free as many `struct pending's as we can easily find. */

/* ARGSUSED */
void
really_free_pendings (foo)
     int foo;
{
  struct pending *next, *next1;
#if 0
  struct pending_block *bnext, *bnext1;
#endif

  for (next = free_pendings; next; next = next1)
    {
      next1 = next->next;
      free ((PTR)next);
    }
  free_pendings = NULL;

#if 0 /* Now we make the links in the symbol_obstack, so don't free them.  */
  for (bnext = pending_blocks; bnext; bnext = bnext1)
    {
      bnext1 = bnext->next;
      free ((PTR)bnext);
    }
#endif
  pending_blocks = NULL;

  for (next = file_symbols; next != NULL; next = next1)
    {
      next1 = next->next;
      free ((PTR)next);
    }
  file_symbols = NULL;

  for (next = global_symbols; next != NULL; next = next1)
    {
      next1 = next->next;
      free ((PTR)next);
    }
  global_symbols = NULL;
}

/* Take one of the lists of symbols and make a block from it.
   Keep the order the symbols have in the list (reversed from the input file).
   Put the block on the list of pending blocks.  */

void
finish_block (symbol, listhead, old_blocks, start, end, objfile)
     struct symbol *symbol;
     struct pending **listhead;
     struct pending_block *old_blocks;
     CORE_ADDR start, end;
     struct objfile *objfile;
{
  register struct pending *next, *next1;
  register struct block *block;
  register struct pending_block *pblock;
  struct pending_block *opblock;
  register int i;
  register int j;

  /* Count the length of the list of symbols.  */

  for (next = *listhead, i = 0;
       next;
       i += next->nsyms, next = next->next)
    {
      /*EMPTY*/;
    }

  block = (struct block *) obstack_alloc (&objfile -> symbol_obstack,
	  (sizeof (struct block) + ((i - 1) * sizeof (struct symbol *))));

  /* Copy the symbols into the block.  */

  BLOCK_NSYMS (block) = i;
  for (next = *listhead; next; next = next->next)
    {
      for (j = next->nsyms - 1; j >= 0; j--)
	{
	  BLOCK_SYM (block, --i) = next->symbol[j];
	}
    }

  BLOCK_START (block) = start;
  BLOCK_END (block) = end;
 /* Superblock filled in when containing block is made */
  BLOCK_SUPERBLOCK (block) = NULL;
  BLOCK_GCC_COMPILED (block) = processing_gcc_compilation;

  /* Put the block in as the value of the symbol that names it.  */

  if (symbol)
    {
      SYMBOL_BLOCK_VALUE (symbol) = block;
      BLOCK_FUNCTION (block) = symbol;
    }
  else
    {
      BLOCK_FUNCTION (block) = NULL;
    }

  /* Now "free" the links of the list, and empty the list.  */

  for (next = *listhead; next; next = next1)
    {
      next1 = next->next;
      next->next = free_pendings;
      free_pendings = next;
    }
  *listhead = NULL;

  /* Install this block as the superblock
     of all blocks made since the start of this scope
     that don't have superblocks yet.  */

  opblock = NULL;
  for (pblock = pending_blocks; pblock != old_blocks; pblock = pblock->next)
    {
      if (BLOCK_SUPERBLOCK (pblock->block) == NULL)
	{
#if 1
	  /* Check to be sure the blocks are nested as we receive them. 
	     If the compiler/assembler/linker work, this just burns a small
	     amount of time.  */
	  if (BLOCK_START (pblock->block) < BLOCK_START (block) ||
	      BLOCK_END   (pblock->block) > BLOCK_END   (block))
	    {
	      if (symbol)
		{
		  complain (&innerblock_complaint, SYMBOL_NAME (symbol));
		}
	      else
		{
		  complain (&innerblock_anon_complaint, 0);
		}
	      BLOCK_START (pblock->block) = BLOCK_START (block);
	      BLOCK_END   (pblock->block) = BLOCK_END   (block);
	    }
#endif
	  BLOCK_SUPERBLOCK (pblock->block) = block;
	}
      opblock = pblock;
    }

  /* Record this block on the list of all blocks in the file.
     Put it after opblock, or at the beginning if opblock is 0.
     This puts the block in the list after all its subblocks.  */

  /* Allocate in the symbol_obstack to save time.
     It wastes a little space.  */
  pblock = (struct pending_block *)
    obstack_alloc (&objfile -> symbol_obstack,
		   sizeof (struct pending_block));
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
  register struct pending_block *next;
  register struct blockvector *blockvector;
  register int i;

  /* Count the length of the list of blocks.  */

  for (next = pending_blocks, i = 0; next; next = next->next, i++) {;}

  blockvector = (struct blockvector *)
    obstack_alloc (&objfile -> symbol_obstack,
		   (sizeof (struct blockvector)
		    + (i - 1) * sizeof (struct block *)));

  /* Copy the blocks into the blockvector.
     This is done in reverse order, which happens to put
     the blocks into the proper order (ascending starting address).
     finish_block has hair to insert each block into the list
     after its subblocks in order to make sure this is true.  */

  BLOCKVECTOR_NBLOCKS (blockvector) = i;
  for (next = pending_blocks; next; next = next->next)
    {
      BLOCKVECTOR_BLOCK (blockvector, --i) = next->block;
    }

#if 0 /* Now we make the links in the obstack, so don't free them.  */
  /* Now free the links of the list, and empty the list.  */

  for (next = pending_blocks; next; next = next1)
    {
      next1 = next->next;
      free (next);
    }
#endif
  pending_blocks = NULL;

#if 1  /* FIXME, shut this off after a while to speed up symbol reading.  */
  /* Some compilers output blocks in the wrong order, but we depend
     on their being in the right order so we can binary search. 
     Check the order and moan about it.  FIXME.  */
  if (BLOCKVECTOR_NBLOCKS (blockvector) > 1)
    {
      for (i = 1; i < BLOCKVECTOR_NBLOCKS (blockvector); i++)
	{
	  if (BLOCK_START(BLOCKVECTOR_BLOCK (blockvector, i-1))
	      > BLOCK_START(BLOCKVECTOR_BLOCK (blockvector, i)))
	    {
	      complain (&blockvector_complaint, 
			(char *) BLOCK_START(BLOCKVECTOR_BLOCK (blockvector, i)));
	    }
	}
    }
#endif

  return (blockvector);
}


/* Start recording information about source code that came from an included
   (or otherwise merged-in) source file with a different name.  */

void
start_subfile (name, dirname)
     char *name;
     char *dirname;
{
  register struct subfile *subfile;

  /* See if this subfile is already known as a subfile of the
     current main source file.  */

  for (subfile = subfiles; subfile; subfile = subfile->next)
    {
      if (!strcmp (subfile->name, name))
	{
	  current_subfile = subfile;
	  return;
	}
    }

  /* This subfile is not known.  Add an entry for it.
     Make an entry for this subfile in the list of all subfiles
     of the current main source file.  */

  subfile = (struct subfile *) xmalloc (sizeof (struct subfile));
  subfile->next = subfiles;
  subfiles = subfile;
  current_subfile = subfile;

  /* Save its name and compilation directory name */
  subfile->name = strdup (name);
  subfile->dirname = (dirname == NULL) ? NULL : strdup (dirname);
  
  /* Initialize line-number recording for this subfile.  */
  subfile->line_vector = NULL;
}

/* For stabs readers, the first N_SO symbol is assumed to be the source
   file name, and the subfile struct is initialized using that assumption.
   If another N_SO symbol is later seen, immediately following the first
   one, then the first one is assumed to be the directory name and the
   second one is really the source file name.

   So we have to patch up the subfile struct by moving the old name value to
   dirname and remembering the new name.  Some sanity checking is performed
   to ensure that the state of the subfile struct is reasonable and that the
   old name we are assuming to be a directory name actually is (by checking
   for a trailing '/'). */

void
patch_subfile_names (subfile, name)
     struct subfile *subfile;
     char *name;
{
  if (subfile != NULL && subfile->dirname == NULL && subfile->name != NULL
      && subfile->name[strlen(subfile->name)-1] == '/')
    {
      subfile->dirname = subfile->name;
      subfile->name = strdup (name);
    }
}


/* Handle the N_BINCL and N_EINCL symbol types
   that act like N_SOL for switching source files
   (different subfiles, as we call them) within one object file,
   but using a stack rather than in an arbitrary order.  */

void
push_subfile ()
{
  register struct subfile_stack *tem
    = (struct subfile_stack *) xmalloc (sizeof (struct subfile_stack));

  tem->next = subfile_stack;
  subfile_stack = tem;
  if (current_subfile == NULL || current_subfile->name == NULL)
    {
      abort ();
    }
  tem->name = current_subfile->name;
}

char *
pop_subfile ()
{
  register char *name;
  register struct subfile_stack *link = subfile_stack;

  if (link == NULL)
    {
      abort ();
    }
  name = link->name;
  subfile_stack = link->next;
  free ((PTR)link);
  return (name);
}


/* Manage the vector of line numbers for each subfile.  */

void
record_line (subfile, line, pc)
     register struct subfile *subfile;
     int line;
     CORE_ADDR pc;
{
  struct linetable_entry *e;
  /* Ignore the dummy line number in libg.o */

  if (line == 0xffff)
    {
      return;
    }

  /* Make sure line vector exists and is big enough.  */
  if (!subfile->line_vector)
    {
      subfile->line_vector_length = INITIAL_LINE_VECTOR_LENGTH;
      subfile->line_vector = (struct linetable *)
	xmalloc (sizeof (struct linetable)
	  + subfile->line_vector_length * sizeof (struct linetable_entry));
      subfile->line_vector->nitems = 0;
    }

  if (subfile->line_vector->nitems + 1 >= subfile->line_vector_length)
    {
      subfile->line_vector_length *= 2;
      subfile->line_vector = (struct linetable *)
	xrealloc ((char *) subfile->line_vector, (sizeof (struct linetable)
	  + subfile->line_vector_length * sizeof (struct linetable_entry)));
    }

  e = subfile->line_vector->item + subfile->line_vector->nitems++;
  e->line = line; e->pc = pc;
}


/* Needed in order to sort line tables from IBM xcoff files.  Sigh!  */

static int
compare_line_numbers (ln1p, ln2p)
     const PTR ln1p;
     const PTR ln2p;
{
  return (((struct linetable_entry *) ln1p) -> line -
	  ((struct linetable_entry *) ln2p) -> line);
}


/* Start a new symtab for a new source file.
   Called, for example, when a stabs symbol of type N_SO is seen, or when
   a DWARF TAG_compile_unit DIE is seen.
   It indicates the start of data for one original source file.  */

void
start_symtab (name, dirname, start_addr)
     char *name;
     char *dirname;
     CORE_ADDR start_addr;
{

  last_source_file = name;
  last_source_start_addr = start_addr;
  file_symbols = NULL;
  global_symbols = NULL;
  within_function = 0;

  /* Context stack is initially empty.  Allocate first one with room for
     10 levels; reuse it forever afterward.  */
  if (context_stack == NULL)
    {
      context_stack_size = INITIAL_CONTEXT_STACK_SIZE;
      context_stack = (struct context_stack *)
	xmalloc (context_stack_size * sizeof (struct context_stack));
    }
  context_stack_depth = 0;

  /* Initialize the list of sub source files with one entry
     for this file (the top-level source file).  */

  subfiles = NULL;
  current_subfile = NULL;
  start_subfile (name, dirname);
}

/* Finish the symbol definitions for one main source file,
   close off all the lexical contexts for that file
   (creating struct block's for them), then make the struct symtab
   for that file and put it in the list of all such.

   END_ADDR is the address of the end of the file's text.

   Note that it is possible for end_symtab() to return NULL.  In particular,
   for the DWARF case at least, it will return NULL when it finds a
   compilation unit that has exactly one DIE, a TAG_compile_unit DIE.  This
   can happen when we link in an object file that was compiled from an empty
   source file.  Returning NULL is probably not the correct thing to do,
   because then gdb will never know about this empty file (FIXME). */

struct symtab *
end_symtab (end_addr, sort_pending, sort_linevec, objfile)
     CORE_ADDR end_addr;
     int sort_pending;
     int sort_linevec;
     struct objfile *objfile;
{
  register struct symtab *symtab;
  register struct blockvector *blockvector;
  register struct subfile *subfile;
  register struct context_stack *cstk;
  struct subfile *nextsub;

  /* Finish the lexical context of the last function in the file;
     pop the context stack.  */

  if (context_stack_depth > 0)
    {
      context_stack_depth--;
      cstk = &context_stack[context_stack_depth];
      /* Make a block for the local symbols within.  */
      finish_block (cstk->name, &local_symbols, cstk->old_blocks,
		    cstk->start_addr, end_addr, objfile);

      /* Debug: if context stack still has something in it,
	 we are in trouble.  */
      if (context_stack_depth > 0)
	{
	  abort ();
	}
    }

  /* It is unfortunate that in xcoff, pending blocks might not be ordered
     in this stage. Especially, blocks for static functions will show up at
     the end.  We need to sort them, so tools like `find_pc_function' and
     `find_pc_block' can work reliably. */

  if (sort_pending && pending_blocks)
    {
      /* FIXME!  Remove this horrid bubble sort and use qsort!!! */
      int swapped;
      do
	{
	  struct pending_block *pb, *pbnext;
	  
	  pb = pending_blocks;
	  pbnext = pb->next;
	  swapped = 0;

	  while (pbnext)
	    {
	      /* swap blocks if unordered! */
	  
	      if (BLOCK_START(pb->block) < BLOCK_START(pbnext->block)) 
		{
		  struct block *tmp = pb->block;
		  pb->block = pbnext->block;
		  pbnext->block = tmp;
		  swapped = 1;
		}
	      pb = pbnext;
	      pbnext = pbnext->next;
	    }
	} while (swapped);
    }

  /* Cleanup any undefined types that have been left hanging around
     (this needs to be done before the finish_blocks so that
     file_symbols is still good).
     FIXME:  Stabs specific. */
  cleanup_undefined_types ();
  finish_global_stabs (objfile);

  if (pending_blocks == NULL
      && file_symbols == NULL
      && global_symbols == NULL)
    {
      /* Ignore symtabs that have no functions with real debugging info */
      blockvector = NULL;
    }
  else
    {
      /* Define the STATIC_BLOCK & GLOBAL_BLOCK, and build the blockvector. */
      finish_block (0, &file_symbols, 0, last_source_start_addr, end_addr,
		    objfile);
      finish_block (0, &global_symbols, 0, last_source_start_addr, end_addr,
		    objfile);
      blockvector = make_blockvector (objfile);
    }

#ifdef PROCESS_LINENUMBER_HOOK
  PROCESS_LINENUMBER_HOOK ();			/* Needed for xcoff. */
#endif

  /* Now create the symtab objects proper, one for each subfile.  */
  /* (The main file is the last one on the chain.)  */

  for (subfile = subfiles; subfile; subfile = nextsub)
    {
      int linetablesize;
      /* If we have blocks of symbols, make a symtab.
	 Otherwise, just ignore this file and any line number info in it.  */
      symtab = NULL;
      if (blockvector)
	{
	  if (subfile->line_vector)
	    {
	      /* First, shrink the linetable to make more memory.  */
	      linetablesize = sizeof (struct linetable) +
		subfile->line_vector->nitems * sizeof (struct linetable_entry);
	      subfile->line_vector = (struct linetable *)
		xrealloc ((char *) subfile->line_vector, linetablesize);

	      if (sort_linevec)
		qsort (subfile->line_vector->item,
		       subfile->line_vector->nitems,
		       sizeof (struct linetable_entry), compare_line_numbers);
	    }

	  /* Now, allocate a symbol table.  */
	  symtab = allocate_symtab (subfile->name, objfile);

	  /* Fill in its components.  */
	  symtab->blockvector = blockvector;
	  if (subfile->line_vector)
	    {
	      /* Reallocate the line table on the symbol obstack */
	      symtab->linetable = (struct linetable *) 
		obstack_alloc (&objfile -> symbol_obstack, linetablesize);
	      memcpy (symtab->linetable, subfile->line_vector, linetablesize);
	    }
	  else
	    {
	      symtab->linetable = NULL;
	    }
	  if (subfile->dirname)
	    {
	      /* Reallocate the dirname on the symbol obstack */
	      symtab->dirname = (char *)
		obstack_alloc (&objfile -> symbol_obstack,
			       strlen (subfile -> dirname) + 1);
	      strcpy (symtab->dirname, subfile->dirname);
	    }
	  else
	    {
	      symtab->dirname = NULL;
	    }
	  symtab->free_code = free_linetable;
	  symtab->free_ptr = NULL;

#ifdef IBM6000_TARGET
	  /* In case we need to duplicate symbol tables (to represent include
	     files), and in case our system needs relocation, we want to
	     relocate the main symbol table node only (for the main file,
	     not for the include files). */

	  symtab->nonreloc = TRUE;
#endif
	}
      if (subfile->name != NULL)
	{
	  free ((PTR) subfile->name);
	}
      if (subfile->dirname != NULL)
	{
	  free ((PTR) subfile->dirname);
	}
      if (subfile->line_vector != NULL)
	{
	  free ((PTR) subfile->line_vector);
	}

      nextsub = subfile->next;
      free ((PTR)subfile);
    }

#ifdef IBM6000_TARGET
  /* all include symbol tables are non-relocatable, except the main source
     file's. */
  if (symtab)
    {
      symtab->nonreloc = FALSE;
    }
#endif

  last_source_file = NULL;
  current_subfile = NULL;

  return (symtab);
}


/* Push a context block.  Args are an identifying nesting level (checkable
   when you pop it), and the starting PC address of this context.  */

struct context_stack *
push_context (desc, valu)
     int desc;
     CORE_ADDR valu;
{
  register struct context_stack *new;

  if (context_stack_depth == context_stack_size)
    {
      context_stack_size *= 2;
      context_stack = (struct context_stack *)
	xrealloc ((char *) context_stack,
		  (context_stack_size * sizeof (struct context_stack)));
    }

  new = &context_stack[context_stack_depth++];
  new->depth = desc;
  new->locals = local_symbols;
  new->old_blocks = pending_blocks;
  new->start_addr = valu;
  new->name = NULL;

  local_symbols = NULL;

  return (new);
}


/* Initialize anything that needs initializing when starting to read
   a fresh piece of a symbol file, e.g. reading in the stuff corresponding
   to a psymtab.  */

void
buildsym_init ()
{
  free_pendings = NULL;
  file_symbols = NULL;
  global_symbols = NULL;
  pending_blocks = NULL;
}

/* Initialize anything that needs initializing when a completely new
   symbol file is specified (not just adding some symbols from another
   file, e.g. a shared library).  */

void
buildsym_new_init ()
{
  buildsym_init ();
}

/* Initializer for this module */

void
_initialize_buildsym ()
{
}
