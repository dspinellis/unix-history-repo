/* Symbol table lookup for the GNU debugger, GDB.
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
#include "call-cmds.h"
#include "regex.h"
#include "expression.h"
#include "language.h"
#include "demangle.h"

#include <obstack.h>
#include <assert.h>

#include <sys/types.h>
#include <fcntl.h>
#include <string.h>
#include <sys/stat.h>
#include <ctype.h>

/* Prototypes for local functions */

static char *
expensive_mangler PARAMS ((const char *));

extern int
find_methods PARAMS ((struct type *, char *, char **, struct symbol **));

static void
completion_list_add_symbol PARAMS ((char *, char *, int));

static struct symtabs_and_lines
decode_line_2 PARAMS ((struct symbol *[], int, int));

static void
rbreak_command PARAMS ((char *, int));

static void
types_info PARAMS ((char *, int));

static void
functions_info PARAMS ((char *, int));

static void
variables_info PARAMS ((char *, int));

static void
sources_info PARAMS ((char *, int));

static void
list_symbols PARAMS ((char *, int, int));

static void
output_source_filename PARAMS ((char *, int *));

static char *
operator_chars PARAMS ((char *, char **));

static int
find_line_common PARAMS ((struct linetable *, int, int *));

static struct partial_symbol *
lookup_partial_symbol PARAMS ((struct partial_symtab *, const char *,
			       int, enum namespace));

static struct partial_symbol *
lookup_demangled_partial_symbol PARAMS ((const struct partial_symtab *,
					 const char *));

static struct symbol *
lookup_demangled_block_symbol PARAMS ((const struct block *, const char *));

static struct symtab *
lookup_symtab_1 PARAMS ((char *));

/* */

/* The single non-language-specific builtin type */
struct type *builtin_type_error;

/* Block in which the most recently searched-for symbol was found.
   Might be better to make this a parameter to lookup_symbol and 
   value_of_this. */

const struct block *block_found;

char no_symtab_msg[] = "No symbol table is loaded.  Use the \"file\" command.";

/* While the C++ support is still in flux, issue a possibly helpful hint on
   using the new command completion feature on single quoted demangled C++
   symbols.  Remove when loose ends are cleaned up.   FIXME -fnf */

void
cplusplus_hint (name)
     char *name;
{
  printf ("Hint: try '%s<TAB> or '%s<ESC-?>\n", name, name);
  printf ("(Note leading single quote.)\n");
}

/* Check for a symtab of a specific name; first in symtabs, then in
   psymtabs.  *If* there is no '/' in the name, a match after a '/'
   in the symtab filename will also work.  */

static struct symtab *
lookup_symtab_1 (name)
     char *name;
{
  register struct symtab *s;
  register struct partial_symtab *ps;
  register char *slash;
  register struct objfile *objfile;

 got_symtab:

  /* First, search for an exact match */

  ALL_SYMTABS (objfile, s)
    if (strcmp (name, s->filename) == 0)
      return s;

  slash = strchr (name, '/');

  /* Now, search for a matching tail (only if name doesn't have any dirs) */

  if (!slash)
    ALL_SYMTABS (objfile, s)
      {
	char *p = s -> filename;
	char *tail = strrchr (p, '/');

	if (tail)
	  p = tail + 1;

	if (strcmp (p, name) == 0)
	  return s;
      }

  /* Same search rules as above apply here, but now we look thru the
     psymtabs.  */

  ALL_PSYMTABS (objfile, ps)
    if (strcmp (name, ps -> filename) == 0)
      goto got_psymtab;

  if (!slash)
    ALL_PSYMTABS (objfile, ps)
      {
	char *p = ps -> filename;
	char *tail = strrchr (p, '/');

	if (tail)
	  p = tail + 1;

	if (strcmp (p, name) == 0)
	  goto got_psymtab;
      }

  return (NULL);

 got_psymtab:

  if (ps -> readin)
    error ("Internal: readin %s pst for `%s' found when no symtab found.",
	   ps -> filename, name);

  s = PSYMTAB_TO_SYMTAB (ps);

  if (s)
    return s;

  /* At this point, we have located the psymtab for this file, but
     the conversion to a symtab has failed.  This usually happens
     when we are looking up an include file.  In this case,
     PSYMTAB_TO_SYMTAB doesn't return a symtab, even though one has
     been created.  So, we need to run through the symtabs again in
     order to find the file.
     XXX - This is a crock, and should be fixed inside of the the
     symbol parsing routines. */
  goto got_symtab;
}

/* Lookup the symbol table of a source file named NAME.  Try a couple
   of variations if the first lookup doesn't work.  */

struct symtab *
lookup_symtab (name)
     char *name;
{
  register struct symtab *s;
  register char *copy;

  s = lookup_symtab_1 (name);
  if (s) return s;

  /* If name not found as specified, see if adding ".c" helps.  */

  copy = (char *) alloca (strlen (name) + 3);
  strcpy (copy, name);
  strcat (copy, ".c");
  s = lookup_symtab_1 (copy);
  if (s) return s;

  /* We didn't find anything; die.  */
  return 0;
}

/* Lookup the partial symbol table of a source file named NAME.  This
   only returns true on an exact match (ie. this semantics are
   different from lookup_symtab.  */

struct partial_symtab *
lookup_partial_symtab (name)
char *name;
{
  register struct partial_symtab *pst;
  register struct objfile *objfile;
  
  ALL_PSYMTABS (objfile, pst)
    {
      if (strcmp (name, pst -> filename) == 0)
	{
	  return (pst);
	}
    }
  return (NULL);
}

/* Demangle a GDB method stub type.  */
char *
gdb_mangle_name (type, i, j)
     struct type *type;
     int i, j;
{
  int mangled_name_len;
  char *mangled_name;
  struct fn_field *f = TYPE_FN_FIELDLIST1 (type, i);
  struct fn_field *method = &f[j];
  char *field_name = TYPE_FN_FIELDLIST_NAME (type, i);
  char *physname = TYPE_FN_FIELD_PHYSNAME (f, j);
  char *newname = type_name_no_tag (type);
  int is_constructor = strcmp(field_name, newname) == 0;
  int is_destructor = is_constructor && physname[0] == '_'
      && physname[1] == CPLUS_MARKER && physname[2] == '_';
  /* Need a new type prefix.  */
  char *const_prefix = method->is_const ? "C" : "";
  char *volatile_prefix = method->is_volatile ? "V" : "";
  char buf[20];
#ifndef GCC_MANGLE_BUG
  int len = strlen (newname);

  if (is_destructor)
    {
      mangled_name = (char*) xmalloc(strlen(physname)+1);
      strcpy(mangled_name, physname);
      return mangled_name;
    }

  sprintf (buf, "__%s%s%d", const_prefix, volatile_prefix, len);
  mangled_name_len = ((is_constructor ? 0 : strlen (field_name))
			  + strlen (buf) + len
			  + strlen (physname)
			  + 1);

  /* Only needed for GNU-mangled names.  ANSI-mangled names
     work with the normal mechanisms.  */
  if (OPNAME_PREFIX_P (field_name))
    {
      char *opname = cplus_mangle_opname (field_name + 3, 0);
      if (opname == NULL)
	error ("No mangling for \"%s\"", field_name);
      mangled_name_len += strlen (opname);
      mangled_name = (char *)xmalloc (mangled_name_len);

      strncpy (mangled_name, field_name, 3);
      mangled_name[3] = '\0';
      strcat (mangled_name, opname);
    }
  else
    {
      mangled_name = (char *)xmalloc (mangled_name_len);
      if (is_constructor)
	mangled_name[0] = '\0';
      else
	strcpy (mangled_name, field_name);
    }
  strcat (mangled_name, buf);
  strcat (mangled_name, newname);
#else
  char *opname;

  if (is_constructor)
    {
      buf[0] = '\0';
    }
  else
    {
      sprintf (buf, "__%s%s", const_prefix, volatile_prefix);
    }

  mangled_name_len = ((is_constructor ? 0 : strlen (field_name))
		      + strlen (buf) + strlen (physname) + 1);

  /* Only needed for GNU-mangled names.  ANSI-mangled names
     work with the normal mechanisms.  */
  if (OPNAME_PREFIX_P (field_name))
    {
      opname = cplus_mangle_opname (field_name + 3, 0);
      if (opname == NULL)
	{
	  error ("No mangling for \"%s\"", field_name);
	}
      mangled_name_len += strlen (opname);
      mangled_name = (char *) xmalloc (mangled_name_len);

      strncpy (mangled_name, field_name, 3);
      strcpy (mangled_name + 3, opname);
    }
  else
    {
      mangled_name = (char *) xmalloc (mangled_name_len);
      if (is_constructor)
	{
	  mangled_name[0] = '\0';
	}
      else
	{
	  strcpy (mangled_name, field_name);
	}
    }
  strcat (mangled_name, buf);

#endif
  strcat (mangled_name, physname);
  return (mangled_name);
}


/* Find which partial symtab on contains PC.  Return 0 if none.  */

struct partial_symtab *
find_pc_psymtab (pc)
     register CORE_ADDR pc;
{
  register struct partial_symtab *pst;
  register struct objfile *objfile;

  ALL_PSYMTABS (objfile, pst)
    {
      if (pc >= pst -> textlow && pc < pst -> texthigh)
	{
	  return (pst);
	}
    }
  return (NULL);
}

/* Find which partial symbol within a psymtab contains PC.  Return 0
   if none.  Check all psymtabs if PSYMTAB is 0.  */
struct partial_symbol *
find_pc_psymbol (psymtab, pc)
     struct partial_symtab *psymtab;
     CORE_ADDR pc;
{
  struct partial_symbol *best, *p;
  CORE_ADDR best_pc;
  
  if (!psymtab)
    psymtab = find_pc_psymtab (pc);
  if (!psymtab)
    return 0;

  best_pc = psymtab->textlow - 1;

  for (p = psymtab->objfile->static_psymbols.list + psymtab->statics_offset;
       (p - (psymtab->objfile->static_psymbols.list + psymtab->statics_offset)
	< psymtab->n_static_syms);
       p++)
    if (SYMBOL_NAMESPACE (p) == VAR_NAMESPACE
	&& SYMBOL_CLASS (p) == LOC_BLOCK
	&& pc >= SYMBOL_VALUE_ADDRESS (p)
	&& SYMBOL_VALUE_ADDRESS (p) > best_pc)
      {
	best_pc = SYMBOL_VALUE_ADDRESS (p);
	best = p;
      }
  if (best_pc == psymtab->textlow - 1)
    return 0;
  return best;
}


/* Find the definition for a specified symbol name NAME
   in namespace NAMESPACE, visible from lexical block BLOCK.
   Returns the struct symbol pointer, or zero if no symbol is found.
   If SYMTAB is non-NULL, store the symbol table in which the
   symbol was found there, or NULL if not found.
   C++: if IS_A_FIELD_OF_THIS is nonzero on entry, check to see if
   NAME is a field of the current implied argument `this'.  If so set
   *IS_A_FIELD_OF_THIS to 1, otherwise set it to zero. 
   BLOCK_FOUND is set to the block in which NAME is found (in the case of
   a field of `this', value_of_this sets BLOCK_FOUND to the proper value.) */

struct symbol *
lookup_symbol (name, block, namespace, is_a_field_of_this, symtab)
     const char *name;
     register const struct block *block;
     const enum namespace namespace;
     int *is_a_field_of_this;
     struct symtab **symtab;
{
  register struct symbol *sym;
  register struct symtab *s;
  register struct partial_symtab *ps;
  struct blockvector *bv;
  register struct objfile *objfile;
  register struct block *b;
  register struct minimal_symbol *msymbol;
  char *temp;
  extern char *gdb_completer_word_break_characters;

  /* If NAME contains any characters from gdb_completer_word_break_characters
     then it is probably from a quoted name string.  So check to see if it
     has a C++ mangled equivalent, and if so, use the mangled equivalent. */

  if (strpbrk (name, gdb_completer_word_break_characters) != NULL)
    {
      if ((temp = expensive_mangler (name)) != NULL)
	{
	  name = temp;
	}
    }

  /* Search specified block and its superiors.  */

  while (block != 0)
    {
      sym = lookup_block_symbol (block, name, namespace);
      if (sym) 
	{
	  block_found = block;
	  if (symtab != NULL)
	    {
	      /* Search the list of symtabs for one which contains the
		 address of the start of this block.  */
	      ALL_SYMTABS (objfile, s)
		{
		  bv = BLOCKVECTOR (s);
		  b = BLOCKVECTOR_BLOCK (bv, GLOBAL_BLOCK);
		  if (BLOCK_START (b) <= BLOCK_START (block)
		      && BLOCK_END (b) > BLOCK_START (block))
		    goto found;
		}
found:
	      *symtab = s;
	    }

	  return (sym);
	}
      block = BLOCK_SUPERBLOCK (block);
    }

  /* But that doesn't do any demangling for the STATIC_BLOCK.
     I'm not sure whether demangling is needed in the case of
     nested function in inner blocks; if so this needs to be changed.
     
     Don't need to mess with the psymtabs; if we have a block,
     that file is read in.  If we don't, then we deal later with
     all the psymtab stuff that needs checking.  */
  if (namespace == VAR_NAMESPACE && block != NULL)
    {
      struct block *b;
      /* Find the right symtab.  */
      ALL_SYMTABS (objfile, s)
	{
	  bv = BLOCKVECTOR (s);
	  b = BLOCKVECTOR_BLOCK (bv, STATIC_BLOCK);
	  if (BLOCK_START (b) <= BLOCK_START (block)
	      && BLOCK_END (b) > BLOCK_START (block))
	    {
	      sym = lookup_demangled_block_symbol (b, name);
	      if (sym)
		{
		  block_found = b;
		  if (symtab != NULL)
		    *symtab = s;
		  return sym;
		}
	    }
	}
    }


  /* C++: If requested to do so by the caller, 
     check to see if NAME is a field of `this'. */
  if (is_a_field_of_this)
    {
      struct value *v = value_of_this (0);
      
      *is_a_field_of_this = 0;
      if (v && check_field (v, name))
	{
	  *is_a_field_of_this = 1;
	  if (symtab != NULL)
	    *symtab = NULL;
	  return 0;
	}
    }

  /* Now search all global blocks.  Do the symtab's first, then
     check the psymtab's */
  
  ALL_SYMTABS (objfile, s)
    {
      bv = BLOCKVECTOR (s);
      block = BLOCKVECTOR_BLOCK (bv, GLOBAL_BLOCK);
      sym = lookup_block_symbol (block, name, namespace);
      if (sym) 
	{
	  block_found = block;
	  if (symtab != NULL)
	    *symtab = s;
	  return sym;
	}
    }

  /* Check for the possibility of the symbol being a global function
     that is stored in one of the minimal symbol tables.  Eventually, all
     global symbols might be resolved in this way.  */
  
  if (namespace == VAR_NAMESPACE)
    {
      msymbol = lookup_minimal_symbol (name, (struct objfile *) NULL);

      if (msymbol == NULL)
	{
	  /* Test each minimal symbol to see if the minimal symbol's name
	     is a C++ mangled name that matches a user visible name.  */

	  char *demangled;

	  ALL_MSYMBOLS (objfile, msymbol)
	    {
	      demangled = demangle_and_match (msymbol -> name, name,
					      DMGL_PARAMS | DMGL_ANSI);
	      if (demangled != NULL)
		{
		  free (demangled);
		  goto found_msym;
		}
	    }
	  msymbol = NULL;		/* Not found */
        }

found_msym:
      if (msymbol != NULL)
	{
	  s = find_pc_symtab (msymbol -> address);
	  /* If S is NULL, there are no debug symbols for this file.
	     Skip this stuff and check for matching static symbols below. */
	  if (s != NULL)
	    {
	      bv = BLOCKVECTOR (s);
	      block = BLOCKVECTOR_BLOCK (bv, GLOBAL_BLOCK);
	      sym = lookup_block_symbol (block, msymbol -> name, namespace);
              /* We kept static functions in minimal symbol table as well as
		 in static scope. We want to find them in the symbol table. */
		if (!sym) {
		  block = BLOCKVECTOR_BLOCK (bv, STATIC_BLOCK);
		  sym = lookup_block_symbol (block, msymbol -> name,
					     namespace);
		}

	      /* sym == 0 if symbol was found in the minimal symbol table
		 but not in the symtab.
		 Return 0 to use the msymbol definition of "foo_".

		 This happens for Fortran  "foo_" symbols,
		 which are "foo" in the symtab.

		 This can also happen if "asm" is used to make a
		 regular symbol but not a debugging symbol, e.g.
		 asm(".globl _main");
		 asm("_main:");
		 */

	      if (symtab != NULL)
		*symtab = s;
	      return sym;
	    }
	}
    }
      
  ALL_PSYMTABS (objfile, ps)
    {
      if (!ps->readin && lookup_partial_symbol (ps, name, 1, namespace))
	{
	  s = PSYMTAB_TO_SYMTAB(ps);
	  bv = BLOCKVECTOR (s);
	  block = BLOCKVECTOR_BLOCK (bv, GLOBAL_BLOCK);
	  sym = lookup_block_symbol (block, name, namespace);
	  if (!sym)
	    error ("Internal: global symbol `%s' found in %s psymtab but not in symtab", name, ps->filename);
	  if (symtab != NULL)
	    *symtab = s;
	  return sym;
	}
    }

  /* Now search all per-file blocks.
     Not strictly correct, but more useful than an error.
     Do the symtabs first, then check the psymtabs */

  ALL_SYMTABS (objfile, s)
    {
      bv = BLOCKVECTOR (s);
      block = BLOCKVECTOR_BLOCK (bv, STATIC_BLOCK);
      sym = lookup_block_symbol (block, name, namespace);
      if (sym) 
	{
	  block_found = block;
	  if (symtab != NULL)
	    *symtab = s;
	  return sym;
	}
    }

  ALL_PSYMTABS (objfile, ps)
    {
      if (!ps->readin && lookup_partial_symbol (ps, name, 0, namespace))
	{
	  s = PSYMTAB_TO_SYMTAB(ps);
	  bv = BLOCKVECTOR (s);
	  block = BLOCKVECTOR_BLOCK (bv, STATIC_BLOCK);
	  sym = lookup_block_symbol (block, name, namespace);
	  if (!sym)
	    error ("Internal: static symbol `%s' found in %s psymtab but not in symtab", name, ps->filename);
	  if (symtab != NULL)
	    *symtab = s;
	  return sym;
	}
    }

  /* Now search all per-file blocks for static mangled symbols.
     Do the symtabs first, then check the psymtabs.  */

  if (namespace == VAR_NAMESPACE)
    {
      ALL_SYMTABS (objfile, s)
	{
	  bv = BLOCKVECTOR (s);
	  block = BLOCKVECTOR_BLOCK (bv, STATIC_BLOCK);
	  sym = lookup_demangled_block_symbol (block, name);
	  if (sym) 
	    {
	      block_found = block;
	      if (symtab != NULL)
		*symtab = s;
	      return sym;
	    }
	}

      ALL_PSYMTABS (objfile, ps)
	{
	  if (!ps->readin && lookup_demangled_partial_symbol (ps, name))
	    {
	      s = PSYMTAB_TO_SYMTAB(ps);
	      bv = BLOCKVECTOR (s);
	      block = BLOCKVECTOR_BLOCK (bv, STATIC_BLOCK);
	      sym = lookup_demangled_block_symbol (block, name);
	      if (!sym)
		error ("Internal: mangled static symbol `%s' found in %s psymtab but not in symtab", name, ps->filename);
	      if (symtab != NULL)
		*symtab = s;
	      return sym;
	    }
	}
    }

  if (symtab != NULL)
    *symtab = NULL;
  return 0;
}

/* Look for a static demangled symbol in block BLOCK.  */

static struct symbol *
lookup_demangled_block_symbol (block, name)
     register const struct block *block;
     const char *name;
{
  register int bot, top;
  register struct symbol *sym;
  char *demangled;

  bot = 0;
  top = BLOCK_NSYMS (block);

  while (bot < top)
    {
      sym = BLOCK_SYM (block, bot);
      if (SYMBOL_NAMESPACE (sym) == VAR_NAMESPACE)
	{
	  demangled = demangle_and_match (SYMBOL_NAME (sym), name,
					  DMGL_PARAMS | DMGL_ANSI);
	  if (demangled != NULL)
	    {
	      free (demangled);
	      return (sym);
	    }
	}
      bot++;
    }

  return (NULL);
}

/* Look, in partial_symtab PST, for static mangled symbol NAME. */

static struct partial_symbol *
lookup_demangled_partial_symbol (pst, name)
     const struct partial_symtab *pst;
     const char *name;
{
  struct partial_symbol *start, *psym;
  int length = pst->n_static_syms;
  char *demangled;

  if (!length)
    return (struct partial_symbol *) 0;
  
  start = pst->objfile->static_psymbols.list + pst->statics_offset;
  for (psym = start; psym < start + length; psym++)
    {
      if (SYMBOL_NAMESPACE (psym) == VAR_NAMESPACE)
	{
	  demangled = demangle_and_match (SYMBOL_NAME (psym), name,
					  DMGL_PARAMS | DMGL_ANSI);
	  if (demangled != NULL)
	    {
	      free (demangled);
	      return (psym);
	    }
	}
    }

  return (NULL);
}

/* Look, in partial_symtab PST, for symbol NAME.  Check the global
   symbols if GLOBAL, the static symbols if not */

static struct partial_symbol *
lookup_partial_symbol (pst, name, global, namespace)
     struct partial_symtab *pst;
     const char *name;
     int global;
     enum namespace namespace;
{
  struct partial_symbol *start, *psym;
  int length = (global ? pst->n_global_syms : pst->n_static_syms);

  if (!length)
    return (struct partial_symbol *) 0;
  
  start = (global ?
	   pst->objfile->global_psymbols.list + pst->globals_offset :
	   pst->objfile->static_psymbols.list + pst->statics_offset  );

  if (global)			/* This means we can use a binary */
				/* search.  */
    {
      struct partial_symbol *top, *bottom, *center;

      /* Binary search.  This search is guaranteed to end with center
         pointing at the earliest partial symbol with the correct
	 name.  At that point *all* partial symbols with that name
	 will be checked against the correct namespace. */
      bottom = start;
      top = start + length - 1;
      while (top > bottom)
	{
	  center = bottom + (top - bottom) / 2;

	  assert (center < top);
	  
	  if (strcmp (SYMBOL_NAME (center), name) >= 0)
	    top = center;
	  else
	    bottom = center + 1;
	}
      assert (top == bottom);
      
      while (!strcmp (SYMBOL_NAME (top), name))
	{
	  if (SYMBOL_NAMESPACE (top) == namespace)
	    return top;
	  top ++;
	}
    }
  else
    {
      /* Can't use a binary search */
      for (psym = start; psym < start + length; psym++)
	if (namespace == SYMBOL_NAMESPACE (psym)
	    && !strcmp (name, SYMBOL_NAME (psym)))
	  return psym;
    }

  return (struct partial_symbol *) 0;
}

/* Find the psymtab containing main(). */
/* FIXME:  What about languages without main() or specially linked
   executables that have no main() ? */

struct partial_symtab *
find_main_psymtab ()
{
  register struct partial_symtab *pst;
  register struct objfile *objfile;

  ALL_PSYMTABS (objfile, pst)
    {
      if (lookup_partial_symbol (pst, "main", 1, VAR_NAMESPACE))
	{
	  return (pst);
	}
    }
  return (NULL);
}

/* Look for a symbol in block BLOCK.  */

struct symbol *
lookup_block_symbol (block, name, namespace)
     register const struct block *block;
     const char *name;
     const enum namespace namespace;
{
  register int bot, top, inc;
  register struct symbol *sym, *parameter_sym;

  top = BLOCK_NSYMS (block);
  bot = 0;

  /* If the blocks's symbols were sorted, start with a binary search.  */

  if (BLOCK_SHOULD_SORT (block))
    {
      /* First, advance BOT to not far before
	 the first symbol whose name is NAME.  */

      while (1)
	{
	  inc = (top - bot + 1);
	  /* No need to keep binary searching for the last few bits worth.  */
	  if (inc < 4)
	    break;
	  inc = (inc >> 1) + bot;
	  sym = BLOCK_SYM (block, inc);
	  if (SYMBOL_NAME (sym)[0] < name[0])
	    bot = inc;
	  else if (SYMBOL_NAME (sym)[0] > name[0])
	    top = inc;
	  else if (strcmp (SYMBOL_NAME (sym), name) < 0)
	    bot = inc;
	  else
	    top = inc;
	}

      /* Now scan forward until we run out of symbols,
	 find one whose name is greater than NAME,
	 or find one we want.
	 If there is more than one symbol with the right name and namespace,
	 we return the first one.  dbxread.c is careful to make sure
	 that if one is a register then it comes first.  */

      top = BLOCK_NSYMS (block);
      while (bot < top)
	{
	  sym = BLOCK_SYM (block, bot);
	  inc = SYMBOL_NAME (sym)[0] - name[0];
	  if (inc == 0)
	    inc = strcmp (SYMBOL_NAME (sym), name);
	  if (inc == 0 && SYMBOL_NAMESPACE (sym) == namespace)
	    return sym;
	  if (inc > 0)
	    return 0;
	  bot++;
	}
      return 0;
    }

  /* Here if block isn't sorted.
     This loop is equivalent to the loop above,
     but hacked greatly for speed.

     Note that parameter symbols do not always show up last in the
     list; this loop makes sure to take anything else other than
     parameter symbols first; it only uses parameter symbols as a
     last resort.  Note that this only takes up extra computation
     time on a match.  */

  parameter_sym = (struct symbol *) 0;
  top = BLOCK_NSYMS (block);
  inc = name[0];
  while (bot < top)
    {
      sym = BLOCK_SYM (block, bot);
      if (SYMBOL_NAME (sym)[0] == inc
	  && !strcmp (SYMBOL_NAME (sym), name)
	  && SYMBOL_NAMESPACE (sym) == namespace)
	{
	  if (SYMBOL_CLASS (sym) == LOC_ARG
	      || SYMBOL_CLASS (sym) == LOC_LOCAL_ARG
	      || SYMBOL_CLASS (sym) == LOC_REF_ARG
	      || SYMBOL_CLASS (sym) == LOC_REGPARM)
	    parameter_sym = sym;
	  else
	    return sym;
	}
      bot++;
    }
  return parameter_sym;		/* Will be 0 if not found. */
}

/* Return the symbol for the function which contains a specified
   lexical block, described by a struct block BL.  */

struct symbol *
block_function (bl)
     struct block *bl;
{
  while (BLOCK_FUNCTION (bl) == 0 && BLOCK_SUPERBLOCK (bl) != 0)
    bl = BLOCK_SUPERBLOCK (bl);

  return BLOCK_FUNCTION (bl);
}

/* Subroutine of find_pc_line */

struct symtab *
find_pc_symtab (pc)
     register CORE_ADDR pc;
{
  register struct block *b;
  struct blockvector *bv;
  register struct symtab *s = 0;
  register struct partial_symtab *ps;
  register struct objfile *objfile;

  /* Search all symtabs for one whose file contains our pc */

  ALL_SYMTABS (objfile, s)
    {
      bv = BLOCKVECTOR (s);
      b = BLOCKVECTOR_BLOCK (bv, GLOBAL_BLOCK);
      if (BLOCK_START (b) <= pc
	  && BLOCK_END (b) > pc)
	goto found;
    }

  if (!s)
    {
      ps = find_pc_psymtab (pc);
      if (ps && ps->readin)
	{
	  printf_filtered ("(Internal error: pc 0x%x in read in psymtab, but not in symtab.)\n", pc);
	}
      if (ps)
	{
	  s = PSYMTAB_TO_SYMTAB (ps);
	}
    }

found:
  return (s);
}

/* Find the source file and line number for a given PC value.
   Return a structure containing a symtab pointer, a line number,
   and a pc range for the entire source line.
   The value's .pc field is NOT the specified pc.
   NOTCURRENT nonzero means, if specified pc is on a line boundary,
   use the line that ends there.  Otherwise, in that case, the line
   that begins there is used.  */

struct symtab_and_line
find_pc_line (pc, notcurrent)
     CORE_ADDR pc;
     int notcurrent;
{
  struct symtab *s;
  register struct linetable *l;
  register int len;
  register int i;
  register struct linetable_entry *item;
  struct symtab_and_line val;
  struct blockvector *bv;

  /* Info on best line seen so far, and where it starts, and its file.  */

  int best_line = 0;
  CORE_ADDR best_pc = 0;
  CORE_ADDR best_end = 0;
  struct symtab *best_symtab = 0;

  /* Store here the first line number
     of a file which contains the line at the smallest pc after PC.
     If we don't find a line whose range contains PC,
     we will use a line one less than this,
     with a range from the start of that file to the first line's pc.  */
  int alt_line = 0;
  CORE_ADDR alt_pc = 0;
  struct symtab *alt_symtab = 0;

  /* Info on best line seen in this file.  */

  int prev_line;
  CORE_ADDR prev_pc;

  /* Info on first line of this file.  */

  int first_line;
  CORE_ADDR first_pc;

  /* If this pc is not from the current frame,
     it is the address of the end of a call instruction.
     Quite likely that is the start of the following statement.
     But what we want is the statement containing the instruction.
     Fudge the pc to make sure we get that.  */

  if (notcurrent) pc -= 1;

  s = find_pc_symtab (pc);
  if (s == 0)
    {
      val.symtab = 0;
      val.line = 0;
      val.pc = pc;
      val.end = 0;
      return val;
    }

  bv = BLOCKVECTOR (s);

  /* Look at all the symtabs that share this blockvector.
     They all have the same apriori range, that we found was right;
     but they have different line tables.  */

  for (; s && BLOCKVECTOR (s) == bv; s = s->next)
    {
      /* Find the best line in this symtab.  */
      l = LINETABLE (s);
      if (!l)
        continue;
      len = l->nitems;
      prev_line = -1;
      first_line = -1;
      for (i = 0; i < len; i++)
	{
	  item = &(l->item[i]);
	  
	  if (first_line < 0)
	    {
	      first_line = item->line;
	      first_pc = item->pc;
	    }
	  /* Return the last line that did not start after PC.  */
	  if (pc >= item->pc)
	    {
	      prev_line = item->line;
	      prev_pc = item->pc;
	    }
	  else
	    break;
	}

      /* Is this file's best line closer than the best in the other files?
	 If so, record this file, and its best line, as best so far.  */
      if (prev_line >= 0 && prev_pc > best_pc)
	{
	  best_pc = prev_pc;
	  best_line = prev_line;
	  best_symtab = s;
	  /* If another line is in the linetable, and its PC is closer
	     than the best_end we currently have, take it as best_end.  */
	  if (i < len && (best_end == 0 || best_end > item->pc))
	    best_end = item->pc;
	}
      /* Is this file's first line closer than the first lines of other files?
	 If so, record this file, and its first line, as best alternate.  */
      if (first_line >= 0 && first_pc > pc
	  && (alt_pc == 0 || first_pc < alt_pc))
	{
	  alt_pc = first_pc;
	  alt_line = first_line;
	  alt_symtab = s;
	}
    }
  if (best_symtab == 0)
    {
      val.symtab = alt_symtab;
      val.line = alt_line - 1;
      val.pc = BLOCK_END (BLOCKVECTOR_BLOCK (bv, GLOBAL_BLOCK));
      val.end = alt_pc;
    }
  else
    {
      val.symtab = best_symtab;
      val.line = best_line;
      val.pc = best_pc;
      if (best_end && (alt_pc == 0 || best_end < alt_pc))
	val.end = best_end;
      else if (alt_pc)
	val.end = alt_pc;
      else
	val.end = BLOCK_END (BLOCKVECTOR_BLOCK (bv, GLOBAL_BLOCK));
    }
  return val;
}

/* Find the PC value for a given source file and line number.
   Returns zero for invalid line number.
   The source file is specified with a struct symtab.  */

CORE_ADDR
find_line_pc (symtab, line)
     struct symtab *symtab;
     int line;
{
  register struct linetable *l;
  register int ind;
  int dummy;

  if (symtab == 0)
    return 0;
  l = LINETABLE (symtab);
  ind = find_line_common(l, line, &dummy);
  return (ind >= 0) ? l->item[ind].pc : 0;
}

/* Find the range of pc values in a line.
   Store the starting pc of the line into *STARTPTR
   and the ending pc (start of next line) into *ENDPTR.
   Returns 1 to indicate success.
   Returns 0 if could not find the specified line.  */

int
find_line_pc_range (symtab, thisline, startptr, endptr)
     struct symtab *symtab;
     int thisline;
     CORE_ADDR *startptr, *endptr;
{
  register struct linetable *l;
  register int ind;
  int exact_match;		/* did we get an exact linenumber match */

  if (symtab == 0)
    return 0;

  l = LINETABLE (symtab);
  ind = find_line_common (l, thisline, &exact_match);
  if (ind >= 0)
    {
      *startptr = l->item[ind].pc;
      /* If we have not seen an entry for the specified line,
	 assume that means the specified line has zero bytes.  */
      if (!exact_match || ind == l->nitems-1)
	*endptr = *startptr;
      else
	/* Perhaps the following entry is for the following line.
	   It's worth a try.  */
	if (ind+1 < l->nitems
	 && l->item[ind+1].line == thisline + 1)
	  *endptr = l->item[ind+1].pc;
	else
	  *endptr = find_line_pc (symtab, thisline+1);
      return 1;
    }

  return 0;
}

/* Given a line table and a line number, return the index into the line
   table for the pc of the nearest line whose number is >= the specified one.
   Return -1 if none is found.  The value is >= 0 if it is an index.

   Set *EXACT_MATCH nonzero if the value returned is an exact match.  */

static int
find_line_common (l, lineno, exact_match)
     register struct linetable *l;
     register int lineno;
     int *exact_match;
{
  register int i;
  register int len;

  /* BEST is the smallest linenumber > LINENO so far seen,
     or 0 if none has been seen so far.
     BEST_INDEX identifies the item for it.  */

  int best_index = -1;
  int best = 0;

  if (lineno <= 0)
    return -1;
  if (l == 0)
    return -1;

  len = l->nitems;
  for (i = 0; i < len; i++)
    {
      register struct linetable_entry *item = &(l->item[i]);

      if (item->line == lineno)
	{
	  *exact_match = 1;
	  return i;
	}

      if (item->line > lineno && (best == 0 || item->line < best))
	{
	  best = item->line;
	  best_index = i;
	}
    }

  /* If we got here, we didn't get an exact match.  */

  *exact_match = 0;
  return best_index;
}

int
find_pc_line_pc_range (pc, startptr, endptr)
     CORE_ADDR pc;
     CORE_ADDR *startptr, *endptr;
{
  struct symtab_and_line sal;
  sal = find_pc_line (pc, 0);
  *startptr = sal.pc;
  *endptr = sal.end;
  return sal.symtab != 0;
}

/* If P is of the form "operator[ \t]+..." where `...' is
   some legitimate operator text, return a pointer to the
   beginning of the substring of the operator text.
   Otherwise, return "".  */
static char *
operator_chars (p, end)
     char *p;
     char **end;
{
  *end = "";
  if (strncmp (p, "operator", 8))
    return *end;
  p += 8;

  /* Don't get faked out by `operator' being part of a longer
     identifier.  */
  if (isalpha(*p) || *p == '_' || *p == '$' || *p == '\0')
    return *end;

  /* Allow some whitespace between `operator' and the operator symbol.  */
  while (*p == ' ' || *p == '\t')
    p++;

  /* Recognize 'operator TYPENAME'. */

  if (isalpha(*p) || *p == '_' || *p == '$')
    {
      register char *q = p+1;
      while (isalnum(*q) || *q == '_' || *q == '$')
	q++;
      *end = q;
      return p;
    }

  switch (*p)
    {
    case '!':
    case '=':
    case '*':
    case '/':
    case '%':
    case '^':
      if (p[1] == '=')
	*end = p+2;
      else
	*end = p+1;
      return p;
    case '<':
    case '>':
    case '+':
    case '-':
    case '&':
    case '|':
      if (p[1] == '=' || p[1] == p[0])
	*end = p+2;
      else
	*end = p+1;
      return p;
    case '~':
    case ',':
      *end = p+1;
      return p;
    case '(':
      if (p[1] != ')')
	error ("`operator ()' must be specified without whitespace in `()'");
      *end = p+2;
      return p;
    case '?':
      if (p[1] != ':')
	error ("`operator ?:' must be specified without whitespace in `?:'");
      *end = p+2;
      return p;
    case '[':
      if (p[1] != ']')
	error ("`operator []' must be specified without whitespace in `[]'");
      *end = p+2;
      return p;
    default:
      error ("`operator %s' not supported", p);
      break;
    }
  *end = "";
  return *end;
}

/* Recursive helper function for decode_line_1.
 * Look for methods named NAME in type T.
 * Return number of matches.
 * Put matches in PHYSNAMES and SYM_ARR (which better be big enough!).
 * These allocations seem to define "big enough":
 * sym_arr = (struct symbol **) alloca(TYPE_NFN_FIELDS_TOTAL (t) * sizeof(struct symbol*));
 * physnames = (char **) alloca (TYPE_NFN_FIELDS_TOTAL (t) * sizeof(char*));
 */

int
find_methods (t, name, physnames, sym_arr)
     struct type *t;
     char *name;
     char **physnames;
     struct symbol **sym_arr;
{
  int i1 = 0;
  int ibase;
  struct symbol *sym_class;
  char *class_name = type_name_no_tag (t);
  /* Ignore this class if it doesn't have a name.
     This prevents core dumps, but is just a workaround
     because we might not find the function in
     certain cases, such as
     struct D {virtual int f();}
     struct C : D {virtual int g();}
     (in this case g++ 1.35.1- does not put out a name
     for D as such, it defines type 19 (for example) in
     the same stab as C, and then does a
     .stabs "D:T19" and a .stabs "D:t19".
     Thus
     "break C::f" should not be looking for field f in
     the class named D, 
     but just for the field f in the baseclasses of C
     (no matter what their names).
     
     However, I don't know how to replace the code below
     that depends on knowing the name of D.  */
  if (class_name
      && (sym_class = lookup_symbol (class_name,
				     (struct block *)NULL,
				     STRUCT_NAMESPACE,
				     (int *)NULL,
				     (struct symtab **)NULL)))
    {
      int method_counter;
      t = SYMBOL_TYPE (sym_class);
      for (method_counter = TYPE_NFN_FIELDS (t) - 1;
	   method_counter >= 0;
	   --method_counter)
	{
	  int field_counter;
	  struct fn_field *f = TYPE_FN_FIELDLIST1 (t, method_counter);

	  char *method_name = TYPE_FN_FIELDLIST_NAME (t, method_counter);
	  if (!strcmp (name, method_name))
	    /* Find all the fields with that name.  */
	    for (field_counter = TYPE_FN_FIELDLIST_LENGTH (t, method_counter) - 1;
		 field_counter >= 0;
		 --field_counter)
	      {
		char *phys_name;
		if (TYPE_FN_FIELD_STUB (f, field_counter))
		  check_stub_method (t, method_counter, field_counter);
		phys_name = TYPE_FN_FIELD_PHYSNAME (f, field_counter);
		physnames[i1] = (char*) alloca (strlen (phys_name) + 1);
		strcpy (physnames[i1], phys_name);
		sym_arr[i1] = lookup_symbol (phys_name,
					     SYMBOL_BLOCK_VALUE (sym_class),
					     VAR_NAMESPACE,
					     (int *) NULL,
					     (struct symtab **) NULL);
		if (sym_arr[i1]) i1++;
		else
		  {
		    fputs_filtered("(Cannot find method ", stdout);
		    fputs_demangled(phys_name, stdout, DMGL_PARAMS);
		    fputs_filtered(" - possibly inlined.)\n", stdout);
		  }
	      }
	}
    }
  /* Only search baseclasses if there is no match yet,
   * since names in derived classes override those in baseclasses.
   */
  if (i1)
    return i1;
  for (ibase = 0; ibase < TYPE_N_BASECLASSES (t); ibase++)
    i1 += find_methods(TYPE_BASECLASS(t, ibase), name,
		       physnames + i1, sym_arr + i1);
  return i1;
}

/* Parse a string that specifies a line number.
   Pass the address of a char * variable; that variable will be
   advanced over the characters actually parsed.

   The string can be:

   LINENUM -- that line number in current file.  PC returned is 0.
   FILE:LINENUM -- that line in that file.  PC returned is 0.
   FUNCTION -- line number of openbrace of that function.
      PC returned is the start of the function.
   VARIABLE -- line number of definition of that variable.
      PC returned is 0.
   FILE:FUNCTION -- likewise, but prefer functions in that file.
   *EXPR -- line in which address EXPR appears.

   FUNCTION may be an undebuggable function found in minimal symbol table.

   If the argument FUNFIRSTLINE is nonzero, we want the first line
   of real code inside a function when a function is specified.

   DEFAULT_SYMTAB specifies the file to use if none is specified.
   It defaults to current_source_symtab.
   DEFAULT_LINE specifies the line number to use for relative
   line numbers (that start with signs).  Defaults to current_source_line.

   Note that it is possible to return zero for the symtab
   if no file is validly specified.  Callers must check that.
   Also, the line number returned may be invalid.  */

struct symtabs_and_lines
decode_line_1 (argptr, funfirstline, default_symtab, default_line)
     char **argptr;
     int funfirstline;
     struct symtab *default_symtab;
     int default_line;
{
  struct symtabs_and_lines values;
#ifdef HPPA_COMPILER_BUG
  /* FIXME: The native HP 9000/700 compiler has a bug which appears
     when optimizing this file with target i960-vxworks.  I haven't
     been able to construct a simple test case.  The problem is that
     in the second call to SKIP_PROLOGUE below, the compiler somehow
     does not realize that the statement val = find_pc_line (...) will
     change the values of the fields of val.  It extracts the elements
     into registers at the top of the block, and does not update the
     registers after the call to find_pc_line.  You can check this by
     inserting a printf at the end of find_pc_line to show what values
     it is returning for val.pc and val.end and another printf after
     the call to see what values the function actually got (remember,
     this is compiling with cc -O, with this patch removed).  You can
     also examine the assembly listing: search for the second call to
     skip_prologue; the LDO statement before the next call to
     find_pc_line loads the address of the structure which
     find_pc_line will return; if there is a LDW just before the LDO,
     which fetches an element of the structure, then the compiler
     still has the bug.

     Setting val to volatile avoids the problem.  We must undef
     volatile, because the HPPA native compiler does not define
     __STDC__, although it does understand volatile, and so volatile
     will have been defined away in defs.h.  */
#undef volatile
  volatile struct symtab_and_line val;
#define volatile /*nothing*/
#else
  struct symtab_and_line val;
#endif
  register char *p, *p1;
  char *q, *q1;
  register struct symtab *s;

  register struct symbol *sym;
  /* The symtab that SYM was found in.  */
  struct symtab *sym_symtab;

  register CORE_ADDR pc;
  register struct minimal_symbol *msymbol;
  char *copy;
  struct symbol *sym_class;
  int i1;
  int is_quoted;
  struct symbol **sym_arr;
  struct type *t;
  char **physnames;
  char *saved_arg = *argptr;
  extern char *gdb_completer_quote_characters;
  
  /* Defaults have defaults.  */

  if (default_symtab == 0)
    {
      default_symtab = current_source_symtab;
      default_line = current_source_line;
    }

  /* See if arg is *PC */

  if (**argptr == '*')
    {
      if (**argptr == '*')
	{
	  (*argptr)++;
	}
      pc = parse_and_eval_address_1 (argptr);
      values.sals = (struct symtab_and_line *)
	xmalloc (sizeof (struct symtab_and_line));
      values.nelts = 1;
      values.sals[0] = find_pc_line (pc, 0);
      values.sals[0].pc = pc;
      return values;
    }

  /* Maybe arg is FILE : LINENUM or FILE : FUNCTION */

  s = NULL;
  is_quoted = (strchr (gdb_completer_quote_characters, **argptr) != NULL);

  for (p = *argptr; *p; p++)
    {
      if (p[0] == ':' || p[0] == ' ' || p[0] == '\t')
	break;
    }
  while (p[0] == ' ' || p[0] == '\t') p++;

  if ((p[0] == ':') && !is_quoted)
    {

      /*  C++  */
      if (p[1] ==':')
	{
	  /* Extract the class name.  */
	  p1 = p;
	  while (p != *argptr && p[-1] == ' ') --p;
	  copy = (char *) alloca (p - *argptr + 1);
	  memcpy (copy, *argptr, p - *argptr);
	  copy[p - *argptr] = 0;

	  /* Discard the class name from the arg.  */
	  p = p1 + 2;
	  while (*p == ' ' || *p == '\t') p++;
	  *argptr = p;

	  sym_class = lookup_symbol (copy, 0, STRUCT_NAMESPACE, 0, 
				     (struct symtab **)NULL);
       
	  if (sym_class &&
	      (   TYPE_CODE (SYMBOL_TYPE (sym_class)) == TYPE_CODE_STRUCT
	       || TYPE_CODE (SYMBOL_TYPE (sym_class)) == TYPE_CODE_UNION))
	    {
	      /* Arg token is not digits => try it as a function name
		 Find the next token (everything up to end or next whitespace). */
	      p = *argptr;
	      while (*p && *p != ' ' && *p != '\t' && *p != ',' && *p !=':') p++;
	      q = operator_chars (*argptr, &q1);

	      if (q1 - q)
		{
		  char *opname;
		  char *tmp = alloca (q1 - q + 1);
		  memcpy (tmp, q, q1 - q);
		  tmp[q1 - q] = '\0';
		  opname = cplus_mangle_opname (tmp, DMGL_ANSI);
		  if (opname == NULL)
		    {
		      warning ("no mangling for \"%s\"", tmp);
		      cplusplus_hint (saved_arg);
		      return_to_top_level ();
		    }
		  copy = (char*) alloca (3 + strlen(opname));
		  sprintf (copy, "__%s", opname);
		  p = q1;
		}
	      else
		{
		  copy = (char *) alloca (p - *argptr + 1 + (q1 - q));
		  memcpy (copy, *argptr, p - *argptr);
		  copy[p - *argptr] = '\0';
		}

	      /* no line number may be specified */
	      while (*p == ' ' || *p == '\t') p++;
	      *argptr = p;

	      sym = 0;
	      i1 = 0;		/*  counter for the symbol array */
	      t = SYMBOL_TYPE (sym_class);
	      sym_arr = (struct symbol **) alloca(TYPE_NFN_FIELDS_TOTAL (t) * sizeof(struct symbol*));
	      physnames = (char **) alloca (TYPE_NFN_FIELDS_TOTAL (t) * sizeof(char*));

	      if (destructor_name_p (copy, t))
		{
		  /* destructors are a special case.  */
		  struct fn_field *f = TYPE_FN_FIELDLIST1 (t, 0);
		  int len = TYPE_FN_FIELDLIST_LENGTH (t, 0) - 1;
		  char *phys_name = TYPE_FN_FIELD_PHYSNAME (f, len);
		  physnames[i1] = (char *)alloca (strlen (phys_name) + 1);
		  strcpy (physnames[i1], phys_name);
		  sym_arr[i1] =
		    lookup_symbol (phys_name, SYMBOL_BLOCK_VALUE (sym_class),
				   VAR_NAMESPACE, 0, (struct symtab **)NULL);
		  if (sym_arr[i1]) i1++;
		}
	      else
		i1 = find_methods (t, copy, physnames, sym_arr);
	      if (i1 == 1)
		{
		  /* There is exactly one field with that name.  */
		  sym = sym_arr[0];

		  if (sym && SYMBOL_CLASS (sym) == LOC_BLOCK)
		    {
		      /* Arg is the name of a function */
		      pc = BLOCK_START (SYMBOL_BLOCK_VALUE (sym)) + FUNCTION_START_OFFSET;
		      if (funfirstline)
			SKIP_PROLOGUE (pc);
		      values.sals = (struct symtab_and_line *)xmalloc (sizeof (struct symtab_and_line));
		      values.nelts = 1;
		      values.sals[0] = find_pc_line (pc, 0);
		      values.sals[0].pc = (values.sals[0].end && values.sals[0].pc != pc) ? values.sals[0].end : pc;
		    }
		  else
		    {
		      values.nelts = 0;
		    }
		  return values;
		}
	      if (i1 > 0)
		{
		  /* There is more than one field with that name
		     (overloaded).  Ask the user which one to use.  */
		  return decode_line_2 (sym_arr, i1, funfirstline);
		}
	      else
		{
		  char *tmp;

		  if (OPNAME_PREFIX_P (copy))
		    {
		      tmp = (char *)alloca (strlen (copy+3) + 9);
		      strcpy (tmp, "operator ");
		      strcat (tmp, copy+3);
		    }
		  else
		    tmp = copy;
		  if (tmp[0] == '~')
		    warning ("the class `%s' does not have destructor defined",
			     sym_class->name);
		  else
		    warning ("the class %s does not have any method named %s",
			     sym_class->name, tmp);
		  cplusplus_hint (saved_arg);
		  return_to_top_level ();
		}
	    }
	  else
	    {
	      /* The quotes are important if copy is empty.  */
	      warning ("can't find class, struct, or union named \"%s\"",
		       copy);
	      cplusplus_hint (saved_arg);
	      return_to_top_level ();
	    }
	}
      /*  end of C++  */


      /* Extract the file name.  */
      p1 = p;
      while (p != *argptr && p[-1] == ' ') --p;
      copy = (char *) alloca (p - *argptr + 1);
      memcpy (copy, *argptr, p - *argptr);
      copy[p - *argptr] = 0;

      /* Find that file's data.  */
      s = lookup_symtab (copy);
      if (s == 0)
	{
	  if (!have_full_symbols () && !have_partial_symbols ())
	    error (no_symtab_msg);
	  error ("No source file named %s.", copy);
	}

      /* Discard the file name from the arg.  */
      p = p1 + 1;
      while (*p == ' ' || *p == '\t') p++;
      *argptr = p;
    }

  /* S is specified file's symtab, or 0 if no file specified.
     arg no longer contains the file name.  */

  /* Check whether arg is all digits (and sign) */

  p = *argptr;
  if (*p == '-' || *p == '+') p++;
  while (*p >= '0' && *p <= '9')
    p++;

  if (p != *argptr && (*p == 0 || *p == ' ' || *p == '\t' || *p == ','))
    {
      /* We found a token consisting of all digits -- at least one digit.  */
      enum sign {none, plus, minus} sign = none;

      /* This is where we need to make sure that we have good defaults.
	 We must guarantee that this section of code is never executed
	 when we are called with just a function name, since
	 select_source_symtab calls us with such an argument  */

      if (s == 0 && default_symtab == 0)
	{
	  select_source_symtab (0);
	  default_symtab = current_source_symtab;
	  default_line = current_source_line;
	}

      if (**argptr == '+')
	sign = plus, (*argptr)++;
      else if (**argptr == '-')
	sign = minus, (*argptr)++;
      val.line = atoi (*argptr);
      switch (sign)
	{
	case plus:
	  if (p == *argptr)
	    val.line = 5;
	  if (s == 0)
	    val.line = default_line + val.line;
	  break;
	case minus:
	  if (p == *argptr)
	    val.line = 15;
	  if (s == 0)
	    val.line = default_line - val.line;
	  else
	    val.line = 1;
	  break;
	case none:
	  break;	/* No need to adjust val.line.  */
	}

      while (*p == ' ' || *p == '\t') p++;
      *argptr = p;
      if (s == 0)
	s = default_symtab;
      val.symtab = s;
      val.pc = 0;
      values.sals = (struct symtab_and_line *)xmalloc (sizeof (struct symtab_and_line));
      values.sals[0] = val;
      values.nelts = 1;
      return values;
    }

  /* Arg token is not digits => try it as a variable name
     Find the next token (everything up to end or next whitespace).  */

  p = skip_quoted (*argptr);
  copy = (char *) alloca (p - *argptr + 1);
  memcpy (copy, *argptr, p - *argptr);
  copy[p - *argptr] = '\0';
  if ((copy[0] == copy [p - *argptr - 1])
      && strchr (gdb_completer_quote_characters, copy[0]) != NULL)
    {
      char *temp;
      copy [p - *argptr - 1] = '\0';
      copy++;
      if ((temp = expensive_mangler (copy)) != NULL)
	{
	  copy = temp;
	}
    }
  while (*p == ' ' || *p == '\t') p++;
  *argptr = p;

  /* Look up that token as a variable.
     If file specified, use that file's per-file block to start with.  */

  sym = lookup_symbol (copy,
		       (s ? BLOCKVECTOR_BLOCK (BLOCKVECTOR (s), STATIC_BLOCK)
			: get_selected_block ()),
		       VAR_NAMESPACE, 0, &sym_symtab);

  if (sym != NULL)
    {
      if (SYMBOL_CLASS (sym) == LOC_BLOCK)
	{
	  /* Arg is the name of a function */
	  pc = BLOCK_START (SYMBOL_BLOCK_VALUE (sym)) + FUNCTION_START_OFFSET;
	  if (funfirstline)
	    SKIP_PROLOGUE (pc);
	  val = find_pc_line (pc, 0);
#ifdef PROLOGUE_FIRSTLINE_OVERLAP
	  /* Convex: no need to suppress code on first line, if any */
	  val.pc = pc;
#else
	  /* If SKIP_PROLOGUE left us in mid-line, and the next line is still
	     part of the same function:
		advance to next line, 
	        recalculate its line number (might not be N+1).  */
	  if (val.pc != pc && val.end &&
	      lookup_minimal_symbol_by_pc (pc) == lookup_minimal_symbol_by_pc (val.end)) {
	    pc = val.end;	/* First pc of next line */
	    val = find_pc_line (pc, 0);
	  }
	  val.pc = pc;
#endif
	  values.sals = (struct symtab_and_line *)xmalloc (sizeof (struct symtab_and_line));
	  values.sals[0] = val;
	  values.nelts = 1;
	  
	  /* I think this is always the same as the line that
	     we calculate above, but the general principle is
	     "trust the symbols more than stuff like
	     SKIP_PROLOGUE".  */
	  if (SYMBOL_LINE (sym) != 0)
	    values.sals[0].line = SYMBOL_LINE (sym);
	  
	  return values;
	}
      else if (SYMBOL_LINE (sym) != 0)
	{
	  /* We know its line number.  */
	  values.sals = (struct symtab_and_line *)
	    xmalloc (sizeof (struct symtab_and_line));
	  values.nelts = 1;
	  memset (&values.sals[0], 0, sizeof (values.sals[0]));
	  values.sals[0].symtab = sym_symtab;
	  values.sals[0].line = SYMBOL_LINE (sym);
	  return values;
	}
      else
	/* This can happen if it is compiled with a compiler which doesn't
	   put out line numbers for variables.  */
	error ("Line number not known for symbol \"%s\"", copy);
    }

  msymbol = lookup_minimal_symbol (copy, (struct objfile *) NULL);
  if (msymbol != NULL)
    {
      val.symtab = 0;
      val.line = 0;
      val.pc = msymbol -> address + FUNCTION_START_OFFSET;
      if (funfirstline)
	SKIP_PROLOGUE (val.pc);
      values.sals = (struct symtab_and_line *)xmalloc (sizeof (struct symtab_and_line));
      values.sals[0] = val;
      values.nelts = 1;
      return values;
    }

  if (!have_full_symbols () &&
      !have_partial_symbols () && !have_minimal_symbols ())
    error (no_symtab_msg);

  error ("Function \"%s\" not defined.", copy);
  return values;	/* for lint */
}

struct symtabs_and_lines
decode_line_spec (string, funfirstline)
     char *string;
     int funfirstline;
{
  struct symtabs_and_lines sals;
  if (string == 0)
    error ("Empty line specification.");
  sals = decode_line_1 (&string, funfirstline,
			current_source_symtab, current_source_line);
  if (*string)
    error ("Junk at end of line specification: %s", string);
  return sals;
}

/* Given a list of NELTS symbols in sym_arr (with corresponding
   mangled names in physnames), return a list of lines to operate on
   (ask user if necessary).  */
static struct symtabs_and_lines
decode_line_2 (sym_arr, nelts, funfirstline)
     struct symbol *sym_arr[];
     int nelts;
     int funfirstline;
{
  struct symtabs_and_lines values, return_values;
  register CORE_ADDR pc;
  char *args, *arg1;
  int i;
  char *prompt;
  char *demangled;

  values.sals = (struct symtab_and_line *) alloca (nelts * sizeof(struct symtab_and_line));
  return_values.sals = (struct symtab_and_line *) xmalloc (nelts * sizeof(struct symtab_and_line));

  i = 0;
  printf("[0] cancel\n[1] all\n");
  while (i < nelts)
    {
      if (sym_arr[i] && SYMBOL_CLASS (sym_arr[i]) == LOC_BLOCK)
	{
	  /* Arg is the name of a function */
	  pc = BLOCK_START (SYMBOL_BLOCK_VALUE (sym_arr[i])) 
	       + FUNCTION_START_OFFSET;
	  if (funfirstline)
	    SKIP_PROLOGUE (pc);
	  values.sals[i] = find_pc_line (pc, 0);
	  values.sals[i].pc = (values.sals[i].end && values.sals[i].pc != pc) ?
			       values.sals[i].end                      :  pc;
	  demangled = cplus_demangle (SYMBOL_NAME (sym_arr[i]),
				      DMGL_PARAMS | DMGL_ANSI);
	  printf("[%d] %s at %s:%d\n", (i+2),
		 demangled ? demangled : SYMBOL_NAME (sym_arr[i]), 
		 values.sals[i].symtab->filename, values.sals[i].line);
	  if (demangled != NULL)
	    {
	      free (demangled);
	    }
	}
      else printf ("?HERE\n");
      i++;
    }
  
  if ((prompt = getenv ("PS2")) == NULL)
    {
      prompt = ">";
    }
  printf("%s ",prompt);
  fflush(stdout);

  args = command_line_input ((char *) NULL, 0);
  
  if (args == 0)
    error_no_arg ("one or more choice numbers");

  i = 0;
  while (*args)
    {
      int num;

      arg1 = args;
      while (*arg1 >= '0' && *arg1 <= '9') arg1++;
      if (*arg1 && *arg1 != ' ' && *arg1 != '\t')
	error ("Arguments must be choice numbers.");

      num = atoi (args);

      if (num == 0)
	error ("cancelled");
      else if (num == 1)
	{
	  memcpy (return_values.sals, values.sals,
		  (nelts * sizeof(struct symtab_and_line)));
	  return_values.nelts = nelts;
	  return return_values;
	}

      if (num > nelts + 2)
	{
	  printf ("No choice number %d.\n", num);
	}
      else
	{
	  num -= 2;
	  if (values.sals[num].pc)
	    {
	      return_values.sals[i++] = values.sals[num];
	      values.sals[num].pc = 0;
	    }
	  else
	    {
	      printf ("duplicate request for %d ignored.\n", num);
	    }
	}

      args = arg1;
      while (*args == ' ' || *args == '\t') args++;
    }
  return_values.nelts = i;
  return return_values;
}


/* Slave routine for sources_info.  Force line breaks at ,'s.
   NAME is the name to print and *FIRST is nonzero if this is the first
   name printed.  Set *FIRST to zero.  */
static void
output_source_filename (name, first)
     char *name;
     int *first;
{
  /* Table of files printed so far.  Since a single source file can
     result in several partial symbol tables, we need to avoid printing
     it more than once.  Note: if some of the psymtabs are read in and
     some are not, it gets printed both under "Source files for which
     symbols have been read" and "Source files for which symbols will
     be read in on demand".  I consider this a reasonable way to deal
     with the situation.  I'm not sure whether this can also happen for
     symtabs; it doesn't hurt to check.  */
  static char **tab = NULL;
  /* Allocated size of tab in elements.
     Start with one 256-byte block (when using GNU malloc.c).
     24 is the malloc overhead when range checking is in effect.  */
  static int tab_alloc_size = (256 - 24) / sizeof (char *);
  /* Current size of tab in elements.  */
  static int tab_cur_size;

  char **p;

  if (*first)
    {
      if (tab == NULL)
	tab = (char **) xmalloc (tab_alloc_size * sizeof (*tab));
      tab_cur_size = 0;
    }

  /* Is NAME in tab?  */
  for (p = tab; p < tab + tab_cur_size; p++)
    if (strcmp (*p, name) == 0)
      /* Yes; don't print it again.  */
      return;
  /* No; add it to tab.  */
  if (tab_cur_size == tab_alloc_size)
    {
      tab_alloc_size *= 2;
      tab = (char **) xrealloc ((char *) tab, tab_alloc_size * sizeof (*tab));
    }
  tab[tab_cur_size++] = name;

  if (*first)
    {
      *first = 0;
    }
  else
    {
      printf_filtered (", ");
    }

  wrap_here ("");
  fputs_filtered (name, stdout);
}  

static void
sources_info (ignore, from_tty)
     char *ignore;
     int from_tty;
{
  register struct symtab *s;
  register struct partial_symtab *ps;
  register struct objfile *objfile;
  int first;
  
  if (!have_full_symbols () && !have_partial_symbols ())
    {
      error (no_symtab_msg);
    }
  
  printf_filtered ("Source files for which symbols have been read in:\n\n");

  first = 1;
  ALL_SYMTABS (objfile, s)
    {
      output_source_filename (s -> filename, &first);
    }
  printf_filtered ("\n\n");
  
  printf_filtered ("Source files for which symbols will be read in on demand:\n\n");

  first = 1;
  ALL_PSYMTABS (objfile, ps)
    {
      if (!ps->readin)
	{
	  output_source_filename (ps -> filename, &first);
	}
    }
  printf_filtered ("\n");
}

static int
name_match (name)
     char *name;
{
  char *demangled = cplus_demangle (name, DMGL_ANSI);
  if (demangled != NULL)
    {
      int cond = re_exec (demangled);
      free (demangled);
      return (cond);
    }
  return (re_exec (name));
}
#define NAME_MATCH(NAME) name_match(NAME)

/* List all symbols (if REGEXP is 0) or all symbols matching REGEXP.
   If CLASS is zero, list all symbols except functions, type names, and
		     constants (enums).
   If CLASS is 1, list only functions.
   If CLASS is 2, list only type names.
   If CLASS is 3, list only method names.

   BPT is non-zero if we should set a breakpoint at the functions
   we find.  */

static void
list_symbols (regexp, class, bpt)
     char *regexp;
     int class;
     int bpt;
{
  register struct symtab *s;
  register struct partial_symtab *ps;
  register struct blockvector *bv;
  struct blockvector *prev_bv = 0;
  register struct block *b;
  register int i, j;
  register struct symbol *sym;
  struct partial_symbol *psym;
  struct objfile *objfile;
  struct minimal_symbol *msymbol;
  char *val;
  static char *classnames[]
    = {"variable", "function", "type", "method"};
  int found_in_file = 0;
  int found_misc = 0;
  static enum minimal_symbol_type types[]
    = {mst_data, mst_text, mst_abs, mst_unknown};
  static enum minimal_symbol_type types2[]
    = {mst_bss,  mst_text, mst_abs, mst_unknown};
  enum minimal_symbol_type ourtype = types[class];
  enum minimal_symbol_type ourtype2 = types2[class];

  if (regexp)
    {
      /* Make sure spacing is right for C++ operators.
	 This is just a courtesy to make the matching less sensitive
	 to how many spaces the user leaves between 'operator'
	 and <TYPENAME> or <OPERATOR>. */
      char *opend;
      char *opname = operator_chars (regexp, &opend);
      if (*opname)
	{
          int fix = -1; /* -1 means ok; otherwise number of spaces needed. */
	  if (isalpha(*opname) || *opname == '_' || *opname == '$')
	    {
	      /* There should 1 space between 'operator' and 'TYPENAME'. */
	      if (opname[-1] != ' ' || opname[-2] == ' ')
	        fix = 1;
	    }
	  else
	    {
	      /* There should 0 spaces between 'operator' and 'OPERATOR'. */
	      if (opname[-1] == ' ')
	        fix = 0;
	    }
	  /* If wrong number of spaces, fix it. */
	  if (fix >= 0)
	    {
	      char *tmp = (char*) alloca(opend-opname+10);
	      sprintf(tmp, "operator%.*s%s", fix, " ", opname);
	      regexp = tmp;
	    }
        }
      
      if (0 != (val = re_comp (regexp)))
	error ("Invalid regexp (%s): %s", val, regexp);
    }

  /* Search through the partial symtabs *first* for all symbols
     matching the regexp.  That way we don't have to reproduce all of
     the machinery below. */

  ALL_PSYMTABS (objfile, ps)
    {
      struct partial_symbol *bound, *gbound, *sbound;
      int keep_going = 1;
      
      if (ps->readin) continue;
      
      gbound = objfile->global_psymbols.list + ps->globals_offset + ps->n_global_syms;
      sbound = objfile->static_psymbols.list + ps->statics_offset + ps->n_static_syms;
      bound = gbound;
      
      /* Go through all of the symbols stored in a partial
	 symtab in one loop. */
      psym = objfile->global_psymbols.list + ps->globals_offset;
      while (keep_going)
	{
	  if (psym >= bound)
	    {
	      if (bound == gbound && ps->n_static_syms != 0)
		{
		  psym = objfile->static_psymbols.list + ps->statics_offset;
		  bound = sbound;
		}
	      else
		keep_going = 0;
	      continue;
	    }
	  else
	    {
	      QUIT;

	      /* If it would match (logic taken from loop below)
		 load the file and go on to the next one */
	      if ((regexp == 0 || NAME_MATCH (SYMBOL_NAME (psym)))
		  && ((class == 0 && SYMBOL_CLASS (psym) != LOC_TYPEDEF
		       && SYMBOL_CLASS (psym) != LOC_BLOCK)
		      || (class == 1 && SYMBOL_CLASS (psym) == LOC_BLOCK)
		      || (class == 2 && SYMBOL_CLASS (psym) == LOC_TYPEDEF)
		      || (class == 3 && SYMBOL_CLASS (psym) == LOC_BLOCK)))
		{
		  PSYMTAB_TO_SYMTAB(ps);
		  keep_going = 0;
		}
	    }
	  psym++;
	}
    }

  /* Here, we search through the minimal symbol tables for functions that
     match, and call find_pc_symtab on them to force their symbols to
     be read.  The symbol will then be found during the scan of symtabs
     below.  If find_pc_symtab fails, set found_misc so that we will
     rescan to print any matching symbols without debug info.  */

  if (class == 1)
    {
      ALL_MSYMBOLS (objfile, msymbol)
	{
	  if (msymbol -> type == ourtype || msymbol -> type == ourtype2)
	    {
	      if (regexp == 0 || NAME_MATCH (msymbol -> name))
		{
		  if (0 == find_pc_symtab (msymbol -> address))
		    {
		      found_misc = 1;
		    }
		}
	    }
	}
    }

  /* Printout here so as to get after the "Reading in symbols"
     messages which will be generated above.  */
  if (!bpt)
    printf_filtered (regexp
	  ? "All %ss matching regular expression \"%s\":\n"
	  : "All defined %ss:\n",
	  classnames[class],
	  regexp);

  ALL_SYMTABS (objfile, s)
    {
      found_in_file = 0;
      bv = BLOCKVECTOR (s);
      /* Often many files share a blockvector.
	 Scan each blockvector only once so that
	 we don't get every symbol many times.
	 It happens that the first symtab in the list
	 for any given blockvector is the main file.  */
      if (bv != prev_bv)
	for (i = GLOBAL_BLOCK; i <= STATIC_BLOCK; i++)
	  {
	    b = BLOCKVECTOR_BLOCK (bv, i);
	    /* Skip the sort if this block is always sorted.  */
	    if (!BLOCK_SHOULD_SORT (b))
	      sort_block_syms (b);
	    for (j = 0; j < BLOCK_NSYMS (b); j++)
	      {
		QUIT;
		sym = BLOCK_SYM (b, j);
		if ((regexp == 0 || NAME_MATCH (SYMBOL_NAME (sym)))
		    && ((class == 0 && SYMBOL_CLASS (sym) != LOC_TYPEDEF
			 && SYMBOL_CLASS (sym) != LOC_BLOCK
			 && SYMBOL_CLASS (sym) != LOC_CONST)
			|| (class == 1 && SYMBOL_CLASS (sym) == LOC_BLOCK)
			|| (class == 2 && SYMBOL_CLASS (sym) == LOC_TYPEDEF)
			|| (class == 3 && SYMBOL_CLASS (sym) == LOC_BLOCK)))
		  {
		    if (bpt)
		      {
			/* Set a breakpoint here, if it's a function */
			if (class == 1)
			  break_command (SYMBOL_NAME(sym), 0);
		      }
		    else if (!found_in_file)
		      {
			fputs_filtered ("\nFile ", stdout);
			fputs_filtered (s->filename, stdout);
			fputs_filtered (":\n", stdout);
		      }
		    found_in_file = 1;
		    
		    if (class != 2 && i == STATIC_BLOCK)
		      printf_filtered ("static ");
		    
		    /* Typedef that is not a C++ class */
		    if (class == 2
			&& SYMBOL_NAMESPACE (sym) != STRUCT_NAMESPACE)
		      typedef_print (SYMBOL_TYPE(sym), sym, stdout);
		    /* variable, func, or typedef-that-is-c++-class */
		    else if (class < 2 || 
			     (class == 2 && 
			      SYMBOL_NAMESPACE(sym) == STRUCT_NAMESPACE))
		      {
			type_print (SYMBOL_TYPE (sym),
				    (SYMBOL_CLASS (sym) == LOC_TYPEDEF
				     ? "" : SYMBOL_NAME (sym)),
				    stdout, 0);
			
			printf_filtered (";\n");
		      }
		    else
		      {
# if 0
/* FIXME, why is this zapped out? */
			char buf[1024];
			type_print_base (TYPE_FN_FIELD_TYPE(t, i), stdout, 0, 0); 
			type_print_varspec_prefix (TYPE_FN_FIELD_TYPE(t, i), stdout, 0); 
			sprintf (buf, " %s::", type_name_no_tag (t));
			type_print_method_args (TYPE_FN_FIELD_ARGS (t, i), buf, name, stdout);
# endif
		      }
		  }
	      }
	  }
      prev_bv = bv;
    }

  /* If there are no eyes, avoid all contact.  I mean, if there are
     no debug symbols, then print directly from the msymbol_vector.  */

  if (found_misc || class != 1)
    {
      found_in_file = 0;
      ALL_MSYMBOLS (objfile, msymbol)
	{
	  if (msymbol -> type == ourtype || msymbol -> type == ourtype2)
	    {
	      if (regexp == 0 || NAME_MATCH (msymbol -> name))
		{
		  /* Functions:  Look up by address. */
		  if (class != 1 ||
		      (0 == find_pc_symtab (msymbol -> address)))
		    {
		      /* Variables/Absolutes:  Look up by name */
		      if (lookup_symbol (msymbol -> name, 
					 (struct block *) 0, VAR_NAMESPACE, 0,
					 (struct symtab **) 0) == NULL)
			{
			  if (!found_in_file)
			    {
			      printf_filtered ("\nNon-debugging symbols:\n");
			      found_in_file = 1;
			    }
			  printf_filtered ("	%08x  %s\n",
					   msymbol -> address,
					   msymbol -> name);
			}
		    }
		}
	    }
	}
    }
}

static void
variables_info (regexp, from_tty)
     char *regexp;
     int from_tty;
{
  list_symbols (regexp, 0, 0);
}

static void
functions_info (regexp, from_tty)
     char *regexp;
     int from_tty;
{
  list_symbols (regexp, 1, 0);
}

static void
types_info (regexp, from_tty)
     char *regexp;
     int from_tty;
{
  list_symbols (regexp, 2, 0);
}

#if 0
/* Tiemann says: "info methods was never implemented."  */
static void
methods_info (regexp)
     char *regexp;
{
  list_symbols (regexp, 3, 0);
}
#endif /* 0 */

/* Breakpoint all functions matching regular expression. */
static void
rbreak_command (regexp, from_tty)
     char *regexp;
     int from_tty;
{
  list_symbols (regexp, 1, 1);
}


/* Return Nonzero if block a is lexically nested within block b,
   or if a and b have the same pc range.
   Return zero otherwise. */
int
contained_in (a, b)
     struct block *a, *b;
{
  if (!a || !b)
    return 0;
  return BLOCK_START (a) >= BLOCK_START (b)
      && BLOCK_END (a)   <= BLOCK_END (b);
}


/* Helper routine for make_symbol_completion_list.  */

static int return_val_size;
static int return_val_index;
static char **return_val;

static void
completion_list_add_name(name)
     char *name;
{
  if (return_val_index + 3 > return_val_size)
    return_val = (char **) xrealloc ((char *) return_val,
				     (return_val_size *= 2) * sizeof (char *));
  
  return_val[return_val_index] = name;
  return_val[++return_val_index] = (char *)NULL;
}

static void
completion_list_check_symbol (symname, text, text_len)
     char *symname;
     char *text;
     int text_len;
{
  register char *cp;

  if ((cp = cplus_demangle(symname, -1)) != NULL)
    {
      if (strncmp(cp, text, text_len) == 0 && strncmp(symname, "_vt$", 4))
	completion_list_add_name(cp);
      else
	free(cp);
    }
  else if ((strncmp (symname, text, text_len) == 0))
    {
      cp = (char *)xmalloc (1 + strlen (symname));
      strcpy(cp, symname);
      completion_list_add_name(cp);
    }
}

/* Return a NULL terminated array of all symbols (regardless of class) which
   begin by matching TEXT.  If the answer is no symbols, then the return value
   is an array which contains only a NULL pointer.

   Problem: All of the symbols have to be copied because readline frees them.
   I'm not going to worry about this; hopefully there won't be that many.  */

char **
make_completion_list (text, wantclass)
  char *text;
  enum address_class wantclass;
{
  register struct symbol *sym;
  register struct symtab *s;
  register struct partial_symtab *ps;
  register struct minimal_symbol *msymbol;
  register struct objfile *objfile;
  register struct block *b, *surrounding_static_block = 0;
  register int i, j;
  int text_len;
  struct partial_symbol *psym;

  text_len = strlen (text);
  return_val_size = 100;
  return_val_index = 0;
  return_val = (char **) xmalloc ((return_val_size + 1) * sizeof (char *));
  return_val[0] = NULL;

  /* Look through the partial symtabs for all symbols which begin
     by matching TEXT.  Add each one that you find to the list.  */

  ALL_PSYMTABS (objfile, ps)
    {
      /* If the psymtab's been read in we'll get it when we search
	 through the blockvector.  */
      if (ps->readin) continue;
      
      for (psym = objfile->global_psymbols.list + ps->globals_offset;
	   psym < (objfile->global_psymbols.list + ps->globals_offset
		   + ps->n_global_syms);
	   psym++)
	{
	  /* If interrupted, then quit. */
	  QUIT;
	  if (wantclass && wantclass != SYMBOL_CLASS (psym))
	    continue;
	  completion_list_check_symbol (SYMBOL_NAME (psym), text, text_len);
	}
      
      for (psym = objfile->static_psymbols.list + ps->statics_offset;
	   psym < (objfile->static_psymbols.list + ps->statics_offset
		   + ps->n_static_syms);
	   psym++)
	{
	  QUIT;
	  if (wantclass && wantclass != SYMBOL_CLASS (psym))
	    continue;
	  completion_list_check_symbol (SYMBOL_NAME (psym), text, text_len);
	}
    }

  /* At this point scan through the misc symbol vectors and add each
     symbol you find to the list.  Eventually we want to ignore
     anything that isn't a text symbol (everything else will be
     handled by the psymtab code above).  */

  ALL_MSYMBOLS (objfile, msymbol)
    {
      QUIT;
      completion_list_check_symbol (msymbol -> name, text, text_len);
    }

  /* Search upwards from currently selected frame (so that we can
     complete on local vars.  */

  for (b = get_selected_block (); b != NULL; b = BLOCK_SUPERBLOCK (b))
    {
      if (!BLOCK_SUPERBLOCK (b))
	{
	  surrounding_static_block = b; 	/* For elmin of dups */
	}
      
      /* Also catch fields of types defined in this places which match our
	 text string.  Only complete on types visible from current context. */

      for (i = 0; i < BLOCK_NSYMS (b); i++)
	{
	  sym = BLOCK_SYM (b, i);
	  if (wantclass && wantclass != SYMBOL_CLASS (sym))
	    continue;
	  completion_list_check_symbol (SYMBOL_NAME (sym), text, text_len);
	  if (SYMBOL_CLASS (sym) == LOC_TYPEDEF)
	    {
	      struct type *t = SYMBOL_TYPE (sym);
	      enum type_code c = TYPE_CODE (t);

	      if (c == TYPE_CODE_UNION || c == TYPE_CODE_STRUCT)
		{
		  for (j = TYPE_N_BASECLASSES (t); j < TYPE_NFIELDS (t); j++)
		    {
		      if (TYPE_FIELD_NAME (t, j))
			{
			  completion_list_check_symbol (TYPE_FIELD_NAME (t, j),
						      text, text_len);
			}
		    }
		}
	    }
	}
    }

  /* Go through the symtabs and check the externs and statics for
     symbols which match.  */

  ALL_SYMTABS (objfile, s)
    {
      QUIT;
      b = BLOCKVECTOR_BLOCK (BLOCKVECTOR (s), GLOBAL_BLOCK);
      for (i = 0; i < BLOCK_NSYMS (b); i++)
	{
	  sym = BLOCK_SYM (b, i);
	  if (wantclass && wantclass != SYMBOL_CLASS (sym))
	    continue;
	  completion_list_check_symbol (SYMBOL_NAME (sym), text, text_len);
	}
    }

  ALL_SYMTABS (objfile, s)
    {
      QUIT;
      b = BLOCKVECTOR_BLOCK (BLOCKVECTOR (s), STATIC_BLOCK);
      /* Don't do this block twice.  */
      if (b == surrounding_static_block) continue;
      for (i = 0; i < BLOCK_NSYMS (b); i++)
	{
	  sym = BLOCK_SYM (b, i);
	  if (wantclass && wantclass != SYMBOL_CLASS (sym))
	    continue;
	  completion_list_check_symbol (SYMBOL_NAME (sym), text, text_len);
	}
    }

  return (return_val);
}

char **
make_function_completion_list (text)
  char *text;
{
  return (make_completion_list(text, LOC_BLOCK));
}

char **
make_symbol_completion_list (text)
  char *text;
{
  return (make_completion_list(text, 0));
}

/* Find a mangled symbol that corresponds to LOOKFOR using brute force.
   Basically we go munging through available symbols, demangling each one,
   looking for a match on the demangled result. */

static char *
expensive_mangler (lookfor)
     const char *lookfor;
{
  register struct symbol *sym;
  register struct symtab *s;
  register struct partial_symtab *ps;
  register struct minimal_symbol *msymbol;
  register struct objfile *objfile;
  register struct block *b, *surrounding_static_block = 0;
  register int i, j;
  struct partial_symbol *psym;
  char *demangled;

  /* Look through the partial symtabs for a symbol that matches */

  ALL_PSYMTABS (objfile, ps)
    {
      /* If the psymtab's been read in we'll get it when we search
	 through the blockvector.  */
      if (ps->readin) continue;
      
      for (psym = objfile->global_psymbols.list + ps->globals_offset;
	   psym < (objfile->global_psymbols.list + ps->globals_offset
		   + ps->n_global_syms);
	   psym++)
	{
	  QUIT;			/* If interrupted, then quit. */
	  demangled = demangle_and_match (SYMBOL_NAME (psym), lookfor,
					  DMGL_PARAMS | DMGL_ANSI);
	  if (demangled != NULL)
	    {
	      free (demangled);
	      return (SYMBOL_NAME (psym));
	    }
	}
      
      for (psym = objfile->static_psymbols.list + ps->statics_offset;
	   psym < (objfile->static_psymbols.list + ps->statics_offset
		   + ps->n_static_syms);
	   psym++)
	{
	  QUIT;
	  demangled = demangle_and_match (SYMBOL_NAME (psym), lookfor,
					  DMGL_PARAMS | DMGL_ANSI);
	  if (demangled != NULL)
	    {
	      free (demangled);
	      return (SYMBOL_NAME (psym));
	    }
	}
    }

  /* Scan through the misc symbol vectors looking for a match. */

  ALL_MSYMBOLS (objfile, msymbol)
    {
      QUIT;
      demangled = demangle_and_match (msymbol -> name, lookfor,
				      DMGL_PARAMS | DMGL_ANSI);
      if (demangled != NULL)
	{
	  free (demangled);
	  return (msymbol -> name);
	}
    }

  /* Search upwards from currently selected frame looking for a match */

  for (b = get_selected_block (); b; b = BLOCK_SUPERBLOCK (b))
    {
      if (!BLOCK_SUPERBLOCK (b))
	surrounding_static_block = b; /* For elmin of dups */
      
      /* Also catch fields of types defined in this places which
	 match our text string.  Only complete on types visible
	 from current context.  */
      for (i = 0; i < BLOCK_NSYMS (b); i++)
	{
	  sym = BLOCK_SYM (b, i);
	  demangled = demangle_and_match (SYMBOL_NAME (sym), lookfor,
					  DMGL_PARAMS | DMGL_ANSI);
	  if (demangled != NULL)
	    {
	      free (demangled);
	      return (SYMBOL_NAME (sym));
	    }
	  if (SYMBOL_CLASS (sym) == LOC_TYPEDEF)
	    {
	      struct type *t = SYMBOL_TYPE (sym);
	      enum type_code c = TYPE_CODE (t);

	      if (c == TYPE_CODE_UNION || c == TYPE_CODE_STRUCT)
		{
		  for (j = TYPE_N_BASECLASSES (t); j < TYPE_NFIELDS (t); j++)
		    {
		      if (TYPE_FIELD_NAME (t, j))
			{
			  demangled =
			    demangle_and_match (TYPE_FIELD_NAME (t, j),
						lookfor,
						DMGL_PARAMS | DMGL_ANSI);
			  if (demangled != NULL)
			    {
			      free (demangled);
			      return (TYPE_FIELD_NAME (t, j));
			    }
			}
		    }
		}
	    }
	}
    }

  /* Go through the symtabs and check the externs and statics for
     symbols which match.  */

  ALL_SYMTABS (objfile, s)
    {
      QUIT;
      b = BLOCKVECTOR_BLOCK (BLOCKVECTOR (s), GLOBAL_BLOCK);
      for (i = 0; i < BLOCK_NSYMS (b); i++)
	{
	  sym = BLOCK_SYM (b, i);
	  demangled = demangle_and_match (SYMBOL_NAME (sym), lookfor,
					  DMGL_PARAMS | DMGL_ANSI);
	  if (demangled != NULL)
	    {
	      free (demangled);
	      return (SYMBOL_NAME (sym));
	    }
	}
    }

  ALL_SYMTABS (objfile, s)
    {
      QUIT;
      b = BLOCKVECTOR_BLOCK (BLOCKVECTOR (s), STATIC_BLOCK);
      /* Don't do this block twice.  */
      if (b == surrounding_static_block) continue;
      for (i = 0; i < BLOCK_NSYMS (b); i++)
	{
	  sym = BLOCK_SYM (b, i);
	  demangled = demangle_and_match (SYMBOL_NAME (sym), lookfor,
					  DMGL_PARAMS | DMGL_ANSI);
	  if (demangled != NULL)
	    {
	      free (demangled);
	      return (SYMBOL_NAME (sym));
	    }
	}
    }

  return (NULL);
}

#if 0
/* Add the type of the symbol sym to the type of the current
   function whose block we are in (assumed).  The type of
   this current function is contained in *TYPE.
   
   This basically works as follows:  When we find a function
   symbol (N_FUNC with a 'f' or 'F' in the symbol name), we record
   a pointer to its type in the global in_function_type.  Every 
   time we come across a parameter symbol ('p' in its name), then
   this procedure adds the name and type of that parameter
   to the function type pointed to by *TYPE.  (Which should correspond
   to in_function_type if it was called correctly).

   Note that since we are modifying a type, the result of 
   lookup_function_type() should be memcpy()ed before calling
   this.  When not in strict typing mode, the expression
   evaluator can choose to ignore this.

   Assumption:  All of a function's parameter symbols will
   appear before another function symbol is found.  The parameters 
   appear in the same order in the argument list as they do in the
   symbol table. */

void
add_param_to_type (type,sym)
   struct type **type;
   struct symbol *sym;
{
   int num = ++(TYPE_NFIELDS(*type));

   if(TYPE_NFIELDS(*type)-1)
      TYPE_FIELDS(*type) = (struct field *)
	  (*current_objfile->xrealloc) ((char *)(TYPE_FIELDS(*type)),
					num*sizeof(struct field));
   else
      TYPE_FIELDS(*type) = (struct field *)
	  (*current_objfile->xmalloc) (num*sizeof(struct field));
   
   TYPE_FIELD_BITPOS(*type,num-1) = num-1;
   TYPE_FIELD_BITSIZE(*type,num-1) = 0;
   TYPE_FIELD_TYPE(*type,num-1) = SYMBOL_TYPE(sym);
   TYPE_FIELD_NAME(*type,num-1) = SYMBOL_NAME(sym);
}
#endif 

void
_initialize_symtab ()
{
  add_info ("variables", variables_info,
	    "All global and static variable names, or those matching REGEXP.");
  add_info ("functions", functions_info,
	    "All function names, or those matching REGEXP.");

  /* FIXME:  This command has at least the following problems:
     1.  It prints builtin types (in a very strange and confusing fashion).
     2.  It doesn't print right, e.g. with
         typedef struct foo *FOO
	 type_print prints "FOO" when we want to make it (in this situation)
	 print "struct foo *".
     I also think "ptype" or "whatis" is more likely to be useful (but if
     there is much disagreement "info types" can be fixed).  */
  add_info ("types", types_info,
	    "All type names, or those matching REGEXP.");

#if 0
  add_info ("methods", methods_info,
	    "All method names, or those matching REGEXP::REGEXP.\n\
If the class qualifier is omitted, it is assumed to be the current scope.\n\
If the first REGEXP is omitted, then all methods matching the second REGEXP\n\
are listed.");
#endif
  add_info ("sources", sources_info,
	    "Source files in the program.");

  add_com ("rbreak", no_class, rbreak_command,
	    "Set a breakpoint for all functions matching REGEXP.");

  /* Initialize the one built-in type that isn't language dependent... */
  builtin_type_error = init_type (TYPE_CODE_ERROR, 0, 0,
				  "<unknown type>", (struct objfile *) NULL);
}
