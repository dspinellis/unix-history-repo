/* GDB routines for manipulating the minimal symbol tables.
   Copyright 1992 Free Software Foundation, Inc.
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


/* This file contains support routines for creating, manipulating, and
   destroying minimal symbol tables.

   Minimal symbol tables are used to hold some very basic information about
   all defined global symbols (text, data, bss, abs, etc).  The only two
   required pieces of information are the symbol's name and the address
   associated with that symbol.

   In many cases, even if a file was compiled with no special options for
   debugging at all, as long as was not stripped it will contain sufficient
   information to build useful minimal symbol tables using this structure.
   
   Even when a file contains enough debugging information to build a full
   symbol table, these minimal symbols are still useful for quickly mapping
   between names and addresses, and vice versa.  They are also sometimes used
   to figure out what full symbol table entries need to be read in. */


#include "defs.h"
#include "symtab.h"
#include "bfd.h"
#include "symfile.h"
#include "objfiles.h"

/* Accumulate the minimal symbols for each objfile in bunches of BUNCH_SIZE.
   At the end, copy them all into one newly allocated location on an objfile's
   symbol obstack.  */

#define BUNCH_SIZE 127

struct msym_bunch
{
  struct msym_bunch *next;
  struct minimal_symbol contents[BUNCH_SIZE];
};

/* Bunch currently being filled up.
   The next field points to chain of filled bunches.  */

static struct msym_bunch *msym_bunch;

/* Number of slots filled in current bunch.  */

static int msym_bunch_index;

/* Total number of minimal symbols recorded so far for the objfile.  */

static int msym_count;

/* Prototypes for local functions. */

static int
compare_minimal_symbols PARAMS ((const void *, const void *));

static int
compact_minimal_symbols PARAMS ((struct minimal_symbol *, int));

/* Look through all the current minimal symbol tables and find the first
   minimal symbol that matches NAME.  If OBJF is non-NULL, it specifies a
   particular objfile and the search is limited to that objfile.  Returns
   a pointer to the minimal symbol that matches, or NULL if no match is found.

   Note:  One instance where there may be duplicate minimal symbols with
   the same name is when the symbol tables for a shared library and the
   symbol tables for an executable contain global symbols with the same
   names (the dynamic linker deals with the duplication). */

struct minimal_symbol *
lookup_minimal_symbol (name, objf)
     register const char *name;
     struct objfile *objf;
{
  struct objfile *objfile;
  struct minimal_symbol *msymbol;
  struct minimal_symbol *found_symbol = NULL;
#ifdef IBM6000_TARGET
  struct minimal_symbol *trampoline_symbol = NULL;
#endif

  for (objfile = object_files;
       objfile != NULL && found_symbol == NULL;
       objfile = objfile -> next)
    {
      if (objf == NULL || objf == objfile)
	{
	  for (msymbol = objfile -> msymbols;
	       msymbol != NULL && msymbol -> name != NULL &&
	       found_symbol == NULL;
	       msymbol++)
	    {
	      if (strcmp (msymbol -> name, name) == 0)
		{
#ifdef IBM6000_TARGET
/* I *think* all platforms using shared libraries (and trampoline code)
 * will suffer this problem. Consider a case where there are 5 shared
 * libraries, each referencing `foo' with a trampoline entry. When someone
 * wants to put a breakpoint on `foo' and the only info we have is minimal
 * symbol vector, we want to use the real `foo', rather than one of those
 * trampoline entries. MGO */	
	  /* If a trampoline symbol is found, we prefer to keep looking
	     for the *real* symbol. If the actual symbol not found,
	     then we'll use the trampoline entry. Sorry for the machine
	     dependent code here, but I hope this will benefit other
	     platforms as well. For trampoline entries, we used mst_unknown
	     earlier. Perhaps we should define a `mst_trampoline' type?? */

		  if (msymbol->type != mst_unknown)
		    found_symbol = msymbol;
	          else if (msymbol->type == mst_unknown && !trampoline_symbol)
		    trampoline_symbol = msymbol;
		     
#else
		  found_symbol = msymbol;
#endif
		}
	    }
	}
    }
#ifdef IBM6000_TARGET
  return found_symbol ? found_symbol : trampoline_symbol;
#endif

  return (found_symbol);
}


/* Search through the minimal symbol table for each objfile and find the
   symbol whose address is the largest address that is still less than or
   equal to PC.  Returns a pointer to the minimal symbol if such a symbol
   is found, or NULL if PC is not in a suitable range.  Note that we need
   to look through ALL the minimal symbol tables before deciding on the
   symbol that comes closest to the specified PC. */

struct minimal_symbol *
lookup_minimal_symbol_by_pc (pc)
     register CORE_ADDR pc;
{
  register int lo;
  register int hi;
  register int new;
  register struct objfile *objfile;
  register struct minimal_symbol *msymbol;
  register struct minimal_symbol *best_symbol = NULL;

  for (objfile = object_files;
       objfile != NULL;
       objfile = objfile -> next)
    {
      /* If this objfile has a minimal symbol table, go search it using
	 a binary search.  Note that a minimal symbol table always consists
	 of at least two symbols, a "real" symbol and the terminating
	 "null symbol".  If there are no real symbols, then there is no
	 minimal symbol table at all. */

      if ((msymbol = objfile -> msymbols) != NULL)
	{
	  lo = 0;
	  hi = objfile -> minimal_symbol_count - 2;
	  
	  /* This code assumes that the minimal symbols are sorted by
	     ascending address values.  If the pc value is greater than or
	     equal to the first symbol's address, then some symbol in this
	     minimal symbol table is a suitable candidate for being the
	     "best" symbol.  This includes the last real symbol, for cases
	     where the pc value is larger than any address in this vector.

	     By iterating until the address associated with the current
	     hi index (the endpoint of the test interval) is less than
	     or equal to the desired pc value, we accomplish two things:
	     (1) the case where the pc value is larger than any minimal
	     symbol address is trivially solved, (2) the address associated
	     with the hi index is always the one we want when the interation
	     terminates.  In essence, we are iterating the test interval
	     down until the pc value is pushed out of it from the high end.

	     Warning: this code is trickier than it would appear at first. */

	  /* Should also requires that pc is <= end of objfile.  FIXME! */
	  if (pc >= msymbol[lo].address)
	    {
	      while (msymbol[hi].address > pc)
		{
		  /* pc is still strictly less than highest address */
		  /* Note "new" will always be >= lo */
		  new = (lo + hi) / 2;
		  if ((msymbol[new].address >= pc) || (lo == new))
		    {
		      hi = new;
		    }
		  else
		    {
		      lo = new;
		    }
		}
	      /* The minimal symbol indexed by hi now is the best one in this
		 objfile's minimal symbol table.  See if it is the best one
		 overall. */

	      if ((best_symbol == NULL) ||
		  (best_symbol -> address < msymbol[hi].address))
		{
		  best_symbol = &msymbol[hi];
		}
	    }
	}      
    }
  return (best_symbol);
}

/* Prepare to start collecting minimal symbols.  Note that presetting
   msym_bunch_index to BUNCH_SIZE causes the first call to save a minimal
   symbol to allocate the memory for the first bunch. */

void
init_minimal_symbol_collection ()
{
  msym_count = 0;
  msym_bunch = NULL;
  msym_bunch_index = BUNCH_SIZE;
}

void
prim_record_minimal_symbol (name, address, ms_type)
     const char *name;
     CORE_ADDR address;
     enum minimal_symbol_type ms_type;
{
  register struct msym_bunch *new;

  if (msym_bunch_index == BUNCH_SIZE)
    {
      new = (struct msym_bunch *) xmalloc (sizeof (struct msym_bunch));
      msym_bunch_index = 0;
      new -> next = msym_bunch;
      msym_bunch = new;
    }
  msym_bunch -> contents[msym_bunch_index].name = (char *) name;
  msym_bunch -> contents[msym_bunch_index].address = address;
  msym_bunch -> contents[msym_bunch_index].info = NULL;
  msym_bunch -> contents[msym_bunch_index].type = ms_type;
  msym_bunch_index++;
  msym_count++;
}

void
prim_record_minimal_symbol_and_info (name, address, ms_type, info)
     const char *name;
     CORE_ADDR address;
     enum minimal_symbol_type ms_type;
     char *info;
{
  register struct msym_bunch *new;

  if (msym_bunch_index == BUNCH_SIZE)
    {
      new = (struct msym_bunch *) xmalloc (sizeof (struct msym_bunch));
      msym_bunch_index = 0;
      new -> next = msym_bunch;
      msym_bunch = new;
    }
  msym_bunch -> contents[msym_bunch_index].name = (char *) name;
  msym_bunch -> contents[msym_bunch_index].address = address;
  msym_bunch -> contents[msym_bunch_index].info = NULL;
  msym_bunch -> contents[msym_bunch_index].type = ms_type;
    /* FIXME:  This info, if it remains, needs its own field.  */
  msym_bunch -> contents[msym_bunch_index].info = info;  /* FIXME! */
  msym_bunch_index++;
  msym_count++;
}

/* Compare two minimal symbols by address and return a signed result based
   on unsigned comparisons, so that we sort into unsigned numeric order.  */

static int
compare_minimal_symbols (fn1p, fn2p)
     const PTR fn1p;
     const PTR fn2p;
{
  register const struct minimal_symbol *fn1;
  register const struct minimal_symbol *fn2;

  fn1 = (const struct minimal_symbol *) fn1p;
  fn2 = (const struct minimal_symbol *) fn2p;

  if (fn1 -> address < fn2 -> address)
    {
      return (-1);
    }
  else if (fn1 -> address > fn2 -> address)
    {
      return (1);
    }
  else
    {
      return (0);
    }
}

/* Discard the currently collected minimal symbols, if any.  If we wish
   to save them for later use, we must have already copied them somewhere
   else before calling this function.

   FIXME:  We could allocate the minimal symbol bunches on their own
   obstack and then simply blow the obstack away when we are done with
   it.  Is it worth the extra trouble though? */

/* ARGSUSED */
void
discard_minimal_symbols (foo)
     int foo;
{
  register struct msym_bunch *next;

  while (msym_bunch != NULL)
    {
      next = msym_bunch -> next;
      free ((PTR)msym_bunch);
      msym_bunch = next;
    }
}

/* Compact duplicate entries out of a minimal symbol table by walking
   through the table and compacting out entries with duplicate addresses
   and matching names.  Return the number of entries remaining.

   On entry, the table resides between msymbol[0] and msymbol[mcount].
   On exit, it resides between msymbol[0] and msymbol[result_count].

   When files contain multiple sources of symbol information, it is
   possible for the minimal symbol table to contain many duplicate entries.
   As an example, SVR4 systems use ELF formatted object files, which
   usually contain at least two different types of symbol tables (a
   standard ELF one and a smaller dynamic linking table), as well as
   DWARF debugging information for files compiled with -g.

   Without compacting, the minimal symbol table for gdb itself contains
   over a 1000 duplicates, about a third of the total table size.  Aside
   from the potential trap of not noticing that two successive entries
   identify the same location, this duplication impacts the time required
   to linearly scan the table, which is done in a number of places.  So we
   just do one linear scan here and toss out the duplicates.

   Note that we are not concerned here about recovering the space that
   is potentially freed up, because the strings themselves are allocated
   on the symbol_obstack, and will get automatically freed when the symbol
   table is freed.  The caller can free up the unused minimal symbols at
   the end of the compacted region if their allocation strategy allows it.

   Also note we only go up to the next to last entry within the loop
   and then copy the last entry explicitly after the loop terminates.

   Since the different sources of information for each symbol may
   have different levels of "completeness", we may have duplicates
   that have one entry with type "mst_unknown" and the other with a
   known type.  So if the one we are leaving alone has type mst_unknown,
   overwrite its type with the type from the one we are compacting out.  */

static int
compact_minimal_symbols (msymbol, mcount)
     struct minimal_symbol *msymbol;
     int mcount;
{
  struct minimal_symbol *copyfrom;
  struct minimal_symbol *copyto;

  if (mcount > 0)
    {
      copyfrom = copyto = msymbol;
      while (copyfrom < msymbol + mcount - 1)
	{
	  if (copyfrom -> address == (copyfrom + 1) -> address
	      && (strcmp (copyfrom -> name, (copyfrom + 1) -> name) == 0))
	    {
	      if ((copyfrom + 1) -> type == mst_unknown)
		{
		  (copyfrom + 1) -> type = copyfrom -> type;
		}
	      copyfrom++;
	    }
	  else
	    {
	      *copyto++ = *copyfrom++;
	    }
	}
      *copyto++ = *copyfrom++;
      mcount = copyto - msymbol;
    }
  return (mcount);
}

/* Add the minimal symbols in the existing bunches to the objfile's
   official minimal symbol table.  99% of the time, this adds the
   bunches to NO existing symbols.  Once in a while for shared
   libraries, we add symbols (e.g. common symbols) to an existing
   objfile.  */

void
install_minimal_symbols (objfile)
     struct objfile *objfile;
{
  register int bindex;
  register int mcount;
  register struct msym_bunch *bunch;
  register struct minimal_symbol *msymbols;
  int alloc_count;

  if (msym_count > 0)
    {
      /* Allocate enough space in the obstack, into which we will gather the
	 bunches of new and existing minimal symbols, sort them, and then
	 compact out the duplicate entries.  Once we have a final table,
	 we will give back the excess space.  */

      alloc_count = msym_count + objfile->minimal_symbol_count + 1;
      obstack_blank (&objfile->symbol_obstack,
		     alloc_count * sizeof (struct minimal_symbol));
      msymbols = (struct minimal_symbol *)
		 obstack_base (&objfile->symbol_obstack);

      /* Copy in the existing minimal symbols, if there are any.  */

      if (objfile->minimal_symbol_count)
        memcpy ((char *)msymbols, (char *)objfile->msymbols, 
		objfile->minimal_symbol_count * sizeof (struct minimal_symbol));

      /* Walk through the list of minimal symbol bunches, adding each symbol
	 to the new contiguous array of symbols.  Note that we start with the
	 current, possibly partially filled bunch (thus we use the current
	 msym_bunch_index for the first bunch we copy over), and thereafter
	 each bunch is full. */
      
      mcount = objfile->minimal_symbol_count;
      
      for (bunch = msym_bunch; bunch != NULL; bunch = bunch -> next)
	{
	  for (bindex = 0; bindex < msym_bunch_index; bindex++, mcount++)
	    {
	      msymbols[mcount] = bunch -> contents[bindex];
#ifdef NAMES_HAVE_UNDERSCORE
	      if (msymbols[mcount].name[0] == '_')
		{
		  msymbols[mcount].name++;
		}
#endif
#ifdef SOME_NAMES_HAVE_DOT
	      if (msymbols[mcount].name[0] == '.')
		{
		  msymbols[mcount].name++;
		}
#endif
	    }
	  msym_bunch_index = BUNCH_SIZE;
	}

      /* Sort the minimal symbols by address.  */
      
      qsort (msymbols, mcount, sizeof (struct minimal_symbol),
	     compare_minimal_symbols);
      
      /* Compact out any duplicates, and free up whatever space we are
	 no longer using.  */
      
      mcount = compact_minimal_symbols (msymbols, mcount);

      obstack_blank (&objfile->symbol_obstack,
	(mcount + 1 - alloc_count) * sizeof (struct minimal_symbol));
      msymbols = (struct minimal_symbol *)
	obstack_finish (&objfile->symbol_obstack);

      /* We also terminate the minimal symbol table
	 with a "null symbol", which is *not* included in the size of
	 the table.  This makes it easier to find the end of the table
	 when we are handed a pointer to some symbol in the middle of it.
         Zero out the fields in the "null symbol" allocated at the end
	 of the array.  Note that the symbol count does *not* include
	 this null symbol, which is why it is indexed by mcount and not
	 mcount-1. */

      msymbols[mcount].name = NULL;
      msymbols[mcount].address = 0;
      msymbols[mcount].info = NULL;
      msymbols[mcount].type = mst_unknown;

      /* Attach the minimal symbol table to the specified objfile.
	 The strings themselves are also located in the symbol_obstack
	 of this objfile.  */

      objfile -> minimal_symbol_count = mcount;
      objfile -> msymbols = msymbols;
    }
}

