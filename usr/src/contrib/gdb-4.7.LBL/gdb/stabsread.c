/* Support routines for decoding "stabs" debugging information format.
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

/* Support routines for reading and decoding debugging information in
   the "stabs" format.  This format is used with many systems that use
   the a.out object file format, as well as some systems that use
   COFF or ELF where the stabs data is placed in a special section.
   Avoid placing any object file format specific code in this file. */

#include "defs.h"
#include "bfd.h"
#include "obstack.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "symfile.h"		/* Needed for "struct complaint" */
#include "objfiles.h"
#include "aout/stab_gnu.h"	/* We always use GNU stabs, not native */
#include "buildsym.h"

/* Ask stabsread.h to define the vars it normally declares `extern'.  */
#define	EXTERN	/**/
#include "stabsread.h"		/* Our own declarations */
#undef	EXTERN

static struct type *
dbx_alloc_type PARAMS ((int [2], struct objfile *));

static void
read_huge_number PARAMS ((char **, int, long *, int *));

static void
patch_block_stabs PARAMS ((struct pending *, struct pending_stabs *,
			   struct objfile *));

static void
fix_common_block PARAMS ((struct symbol *, int));

static struct type *
read_range_type PARAMS ((char **, int [2], struct objfile *));

static struct type *
read_sun_builtin_type PARAMS ((char **, int [2], struct objfile *));

static struct type *
read_sun_floating_type PARAMS ((char **, int [2], struct objfile *));

static struct type *
read_enum_type PARAMS ((char **, struct type *, struct objfile *));

static struct type *
read_struct_type PARAMS ((char **, struct type *, struct objfile *));

static struct type *
read_array_type PARAMS ((char **, struct type *, struct objfile *));

static struct type **
read_args PARAMS ((char **, int, struct objfile *));

static const char vptr_name[] = { '_','v','p','t','r',CPLUS_MARKER,'\0' };
static const char vb_name[] =   { '_','v','b',CPLUS_MARKER,'\0' };

/* Define this as 1 if a pcc declaration of a char or short argument
   gives the correct address.  Otherwise assume pcc gives the
   address of the corresponding int, which is not the same on a
   big-endian machine.  */

#ifndef BELIEVE_PCC_PROMOTION
#define BELIEVE_PCC_PROMOTION 0
#endif

/* During some calls to read_type (and thus to read_range_type), this
   contains the name of the type being defined.  Range types are only
   used in C as basic types.  We use the name to distinguish the otherwise
   identical basic types "int" and "long" and their unsigned versions.
   FIXME, this should disappear with better type management.  */

static char *long_kludge_name;

#if 0
struct complaint dbx_class_complaint =
{
  "encountered DBX-style class variable debugging information.\n\
You seem to have compiled your program with \
\"g++ -g0\" instead of \"g++ -g\".\n\
Therefore GDB will not know about your class variables", 0, 0
};
#endif

struct complaint invalid_cpp_abbrev_complaint =
  {"invalid C++ abbreviation `%s'", 0, 0};

struct complaint invalid_cpp_type_complaint =
  {"C++ abbreviated type name unknown at symtab pos %d", 0, 0};

struct complaint member_fn_complaint =
  {"member function type missing, got '%c'", 0, 0};

struct complaint const_vol_complaint =
  {"const/volatile indicator missing, got '%c'", 0, 0};

struct complaint error_type_complaint =
  {"debug info mismatch between compiler and debugger", 0, 0};

struct complaint invalid_member_complaint =
  {"invalid (minimal) member type data format at symtab pos %d.", 0, 0};

struct complaint range_type_base_complaint =
  {"base type %d of range type is not defined", 0, 0};

struct complaint reg_value_complaint =
  {"register number too large in symbol %s", 0, 0};

/* Make a list of forward references which haven't been defined.  */

static struct type **undef_types;
static int undef_types_allocated;
static int undef_types_length;


int
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
	{
	  total += p[3] << 6;
	}
    }

  /* Ensure result is positive.  */
  if (total < 0)
    {
      total += (1000 << 6);
    }
  return (total % HASHSIZE);
}


/* Look up a dbx type-number pair.  Return the address of the slot
   where the type for that number-pair is stored.
   The number-pair is in TYPENUMS.

   This can be used for finding the type associated with that pair
   or for associating a new type with the pair.  */

struct type **
dbx_lookup_type (typenums)
     int typenums[2];
{
  register int filenum = typenums[0];
  register int index = typenums[1];
  unsigned old_len;
  register int real_filenum;
  register struct header_file *f;
  int f_orig_length;

  if (filenum == -1)		/* -1,-1 is for temporary types.  */
    return 0;

  if (filenum < 0 || filenum >= n_this_object_header_files)
    error ("Invalid symbol data: type number (%d,%d) out of range at symtab pos %d.",
	   filenum, index, symnum);

  if (filenum == 0)
    {
      /* Type is defined outside of header files.
	 Find it in this object file's type vector.  */
      if (index >= type_vector_length)
	{
	  old_len = type_vector_length;
	  if (old_len == 0)
	    {
	      type_vector_length = INITIAL_TYPE_VECTOR_LENGTH;
	      type_vector = (struct type **)
		malloc (type_vector_length * sizeof (struct type *));
	    }
	  while (index >= type_vector_length)
	    {
	      type_vector_length *= 2;
	    }
	  type_vector = (struct type **)
	    xrealloc ((char *) type_vector,
		      (type_vector_length * sizeof (struct type *)));
	  memset (&type_vector[old_len], 0,
		  (type_vector_length - old_len) * sizeof (struct type *));
	}
      return (&type_vector[index]);
    }
  else
    {
      real_filenum = this_object_header_files[filenum];

      if (real_filenum >= n_header_files)
	{
	  abort ();
	}

      f = &header_files[real_filenum];

      f_orig_length = f->length;
      if (index >= f_orig_length)
	{
	  while (index >= f->length)
	    {
	      f->length *= 2;
	    }
	  f->vector = (struct type **)
	    xrealloc ((char *) f->vector, f->length * sizeof (struct type *));
	  memset (&f->vector[f_orig_length], 0,
		  (f->length - f_orig_length) * sizeof (struct type *));
	}
      return (&f->vector[index]);
    }
}

/* Make sure there is a type allocated for type numbers TYPENUMS
   and return the type object.
   This can create an empty (zeroed) type object.
   TYPENUMS may be (-1, -1) to return a new type object that is not
   put into the type vector, and so may not be referred to by number. */

static struct type *
dbx_alloc_type (typenums, objfile)
     int typenums[2];
     struct objfile *objfile;
{
  register struct type **type_addr;

  if (typenums[0] == -1)
    {
      return (alloc_type (objfile));
    }

  type_addr = dbx_lookup_type (typenums);

  /* If we are referring to a type not known at all yet,
     allocate an empty type for it.
     We will fill it in later if we find out how.  */
  if (*type_addr == 0)
    {
      *type_addr = alloc_type (objfile);
    }

  return (*type_addr);
}

/* for all the stabs in a given stab vector, build appropriate types 
   and fix their symbols in given symbol vector. */

static void
patch_block_stabs (symbols, stabs, objfile)
     struct pending *symbols;
     struct pending_stabs *stabs;
     struct objfile *objfile;
{
  int ii;
  char *name;
  char *pp;
  struct symbol *sym;

  if (stabs)
    {
      
      /* for all the stab entries, find their corresponding symbols and 
	 patch their types! */
      
      for (ii = 0; ii < stabs->count; ++ii)
	{
	  name = stabs->stab[ii];
	  pp = (char*) strchr (name, ':');
	  sym = find_symbol_in_list (symbols, name, pp-name);
	  if (!sym)
	    {
#ifndef IBM6000_TARGET
	      printf ("ERROR! stab symbol not found!\n");	/* FIXME */
#endif
	    }
	  else
	    {
	      pp += 2;
	      if (*(pp-1) == 'F' || *(pp-1) == 'f')
		{
		  SYMBOL_TYPE (sym) =
		    lookup_function_type (read_type (&pp, objfile));
		}
	      else
		{
		  SYMBOL_TYPE (sym) = read_type (&pp, objfile);
		}
	    }
	}
    }
}


/* Read a number by which a type is referred to in dbx data,
   or perhaps read a pair (FILENUM, TYPENUM) in parentheses.
   Just a single number N is equivalent to (0,N).
   Return the two numbers by storing them in the vector TYPENUMS.
   TYPENUMS will then be used as an argument to dbx_lookup_type.  */

void
read_type_number (pp, typenums)
     register char **pp;
     register int *typenums;
{
  if (**pp == '(')
    {
      (*pp)++;
      typenums[0] = read_number (pp, ',');
      typenums[1] = read_number (pp, ')');
    }
  else
    {
      typenums[0] = 0;
      typenums[1] = read_number (pp, 0);
    }
}


/* To handle GNU C++ typename abbreviation, we need to be able to
   fill in a type's name as soon as space for that type is allocated.
   `type_synonym_name' is the name of the type being allocated.
   It is cleared as soon as it is used (lest all allocated types
   get this name).  */

static char *type_synonym_name;

/* ARGSUSED */
struct symbol *
define_symbol (valu, string, desc, type, objfile)
     unsigned int valu;
     char *string;
     int desc;
     int type;
     struct objfile *objfile;
{
  register struct symbol *sym;
  char *p = (char *) strchr (string, ':');
  int deftype;
  int synonym = 0;
  register int i;
  struct type *temptype;

  /* We would like to eliminate nameless symbols, but keep their types.
     E.g. stab entry ":t10=*2" should produce a type 10, which is a pointer
     to type 2, but, should not creat a symbol to address that type. Since
     the symbol will be nameless, there is no way any user can refer to it. */

  int nameless;

  /* Ignore syms with empty names.  */
  if (string[0] == 0)
    return 0;

  /* Ignore old-style symbols from cc -go  */
  if (p == 0)
    return 0;

  /* If a nameless stab entry, all we need is the type, not the symbol.
     e.g. ":t10=*2" */
  nameless = (p == string);

  sym = (struct symbol *) 
    obstack_alloc (&objfile -> symbol_obstack, sizeof (struct symbol));
  memset (sym, 0, sizeof (struct symbol));

  if (processing_gcc_compilation)
    {
      /* GCC 2.x puts the line number in desc.  SunOS apparently puts in the
	 number of bytes occupied by a type or object, which we ignore.  */
      SYMBOL_LINE(sym) = desc;
    }
  else
    {
      SYMBOL_LINE(sym) = 0;			/* unknown */
    }

  if (string[0] == CPLUS_MARKER)
    {
      /* Special GNU C++ names.  */
      switch (string[1])
	{
	  case 't':
	    SYMBOL_NAME (sym) = obsavestring ("this", strlen ("this"),
					      &objfile -> symbol_obstack);
	    break;

	  case 'v': /* $vtbl_ptr_type */
	    /* Was: SYMBOL_NAME (sym) = "vptr"; */
	    goto normal;

	  case 'e':
	    SYMBOL_NAME (sym) = obsavestring ("eh_throw", strlen ("eh_throw"),
					      &objfile -> symbol_obstack);
	    break;

	  case '_':
	    /* This was an anonymous type that was never fixed up.  */
	    goto normal;

	  default:
	    abort ();
	}
    }
  else
    {
    normal:
      SYMBOL_NAME (sym)	= (char *)
	obstack_alloc (&objfile -> symbol_obstack, ((p - string) + 1));
      /* Open-coded bcopy--saves function call time.  */
      {
	register char *p1 = string;
	register char *p2 = SYMBOL_NAME (sym);
	while (p1 != p)
	  {
	    *p2++ = *p1++;
	  }
	*p2++ = '\0';
      }
    }
  p++;

  /* Determine the type of name being defined.  */
  /* The Acorn RISC machine's compiler can put out locals that don't
     start with "234=" or "(3,4)=", so assume anything other than the
     deftypes we know how to handle is a local.  */
  if (!strchr ("cfFGpPrStTvVXCR", *p))
    deftype = 'l';
  else
    deftype = *p++;

  /* c is a special case, not followed by a type-number.
     SYMBOL:c=iVALUE for an integer constant symbol.
     SYMBOL:c=rVALUE for a floating constant symbol.
     SYMBOL:c=eTYPE,INTVALUE for an enum constant symbol.
        e.g. "b:c=e6,0" for "const b = blob1"
	(where type 6 is defined by "blobs:t6=eblob1:0,blob2:1,;").  */
  if (deftype == 'c')
    {
      if (*p++ != '=')
	error ("Invalid symbol data at symtab pos %d.", symnum);
      switch (*p++)
	{
	case 'r':
	  {
	    double d = atof (p);
	    char *dbl_valu;

	    SYMBOL_TYPE (sym) = lookup_fundamental_type (objfile,
							 FT_DBL_PREC_FLOAT);
	    dbl_valu = (char *)
	      obstack_alloc (&objfile -> symbol_obstack, sizeof (double));
	    memcpy (dbl_valu, &d, sizeof (double));
	    SWAP_TARGET_AND_HOST (dbl_valu, sizeof (double));
	    SYMBOL_VALUE_BYTES (sym) = dbl_valu;
	    SYMBOL_CLASS (sym) = LOC_CONST_BYTES;
	  }
	  break;
	case 'i':
	  {
	    SYMBOL_TYPE (sym) = lookup_fundamental_type (objfile,
							 FT_INTEGER);
	    SYMBOL_VALUE (sym) = atoi (p);
	    SYMBOL_CLASS (sym) = LOC_CONST;
	  }
	  break;
	case 'e':
	  /* SYMBOL:c=eTYPE,INTVALUE for an enum constant symbol.
	     e.g. "b:c=e6,0" for "const b = blob1"
	     (where type 6 is defined by "blobs:t6=eblob1:0,blob2:1,;").  */
	  {
	    int typenums[2];
	    
	    read_type_number (&p, typenums);
	    if (*p++ != ',')
	      error ("Invalid symbol data: no comma in enum const symbol");
	    
	    SYMBOL_TYPE (sym) = *dbx_lookup_type (typenums);
	    SYMBOL_VALUE (sym) = atoi (p);
	    SYMBOL_CLASS (sym) = LOC_CONST;
	  }
	  break;
	default:
	  error ("Invalid symbol data at symtab pos %d.", symnum);
	}
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      add_symbol_to_list (sym, &file_symbols);
      return sym;
    }

  /* Now usually comes a number that says which data type,
     and possibly more stuff to define the type
     (all of which is handled by read_type)  */

  if (deftype == 'p' && *p == 'F')
    /* pF is a two-letter code that means a function parameter in Fortran.
       The type-number specifies the type of the return value.
       Translate it into a pointer-to-function type.  */
    {
      p++;
      SYMBOL_TYPE (sym)
	= lookup_pointer_type (lookup_function_type (read_type (&p, objfile)));
    }
  else
    {
      /* The symbol class letter is followed by a type (typically the
	 type of the symbol, or its return-type, or etc).  Read it.  */

      synonym = *p == 't';

      if (synonym)
	{
	  p += 1;
	  type_synonym_name = obsavestring (SYMBOL_NAME (sym),
					    strlen (SYMBOL_NAME (sym)),
					    &objfile -> symbol_obstack);
	}

      /* Here we save the name of the symbol for read_range_type, which
	 ends up reading in the basic types.  In stabs, unfortunately there
	 is no distinction between "int" and "long" types except their
	 names.  Until we work out a saner type policy (eliminating most
	 builtin types and using the names specified in the files), we
	 save away the name so that far away from here in read_range_type,
	 we can examine it to decide between "int" and "long".  FIXME.  */
      long_kludge_name = SYMBOL_NAME (sym);

      SYMBOL_TYPE (sym) = read_type (&p, objfile);
    }

  switch (deftype)
    {
    case 'C':
      /* The name of a caught exception.  */
      SYMBOL_CLASS (sym) = LOC_LABEL;
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      SYMBOL_VALUE_ADDRESS (sym) = valu;
      add_symbol_to_list (sym, &local_symbols);
      break;

    case 'f':
      /* A static function definition.  */
      SYMBOL_CLASS (sym) = LOC_BLOCK;
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      add_symbol_to_list (sym, &file_symbols);
      /* fall into process_function_types.  */

    process_function_types:
      /* Function result types are described as the result type in stabs.
	 We need to convert this to the function-returning-type-X type
	 in GDB.  E.g. "int" is converted to "function returning int".  */
      if (TYPE_CODE (SYMBOL_TYPE (sym)) != TYPE_CODE_FUNC)
	{
#if 0
	  /* This code doesn't work -- it needs to realloc and can't.  */
	  /* Attempt to set up to record a function prototype... */
	  struct type *new = alloc_type (objfile);

	  /* Generate a template for the type of this function.  The 
	     types of the arguments will be added as we read the symbol 
	     table. */
	  *new = *lookup_function_type (SYMBOL_TYPE(sym));
	  SYMBOL_TYPE(sym) = new;
	  TYPE_OBJFILE (new) = objfile;
	  in_function_type = new;
#else
	  SYMBOL_TYPE (sym) = lookup_function_type (SYMBOL_TYPE (sym));
#endif
	}
      /* fall into process_prototype_types */

    process_prototype_types:
      /* Sun acc puts declared types of arguments here.  We don't care
	 about their actual types (FIXME -- we should remember the whole
	 function prototype), but the list may define some new types
	 that we have to remember, so we must scan it now.  */
      while (*p == ';') {
	p++;
	read_type (&p, objfile);
      }
      break;

    case 'F':
      /* A global function definition.  */
      SYMBOL_CLASS (sym) = LOC_BLOCK;
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      add_symbol_to_list (sym, &global_symbols);
      goto process_function_types;

    case 'G':
      /* For a class G (global) symbol, it appears that the
	 value is not correct.  It is necessary to search for the
	 corresponding linker definition to find the value.
	 These definitions appear at the end of the namelist.  */
      i = hashname (SYMBOL_NAME (sym));
      SYMBOL_VALUE_CHAIN (sym) = global_sym_chain[i];
      global_sym_chain[i] = sym;
      SYMBOL_CLASS (sym) = LOC_STATIC;
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      add_symbol_to_list (sym, &global_symbols);
      break;

      /* This case is faked by a conditional above,
	 when there is no code letter in the dbx data.
	 Dbx data never actually contains 'l'.  */
    case 'l':
      SYMBOL_CLASS (sym) = LOC_LOCAL;
      SYMBOL_VALUE (sym) = valu;
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      add_symbol_to_list (sym, &local_symbols);
      break;

    case 'p':
      /* Normally this is a parameter, a LOC_ARG.  On the i960, it
	 can also be a LOC_LOCAL_ARG depending on symbol type.  */
#ifndef DBX_PARM_SYMBOL_CLASS
#define	DBX_PARM_SYMBOL_CLASS(type)	LOC_ARG
#endif
      SYMBOL_CLASS (sym) = DBX_PARM_SYMBOL_CLASS (type);
      SYMBOL_VALUE (sym) = valu;
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
#if 0
      /* This doesn't work yet.  */
      add_param_to_type (&in_function_type, sym);
#endif
      add_symbol_to_list (sym, &local_symbols);

      /* If it's gcc-compiled, if it says `short', believe it.  */
      if (processing_gcc_compilation || BELIEVE_PCC_PROMOTION)
	break;

#if defined(BELIEVE_PCC_PROMOTION_TYPE)
      /* This macro is defined on machines (e.g. sparc) where
	 we should believe the type of a PCC 'short' argument,
	 but shouldn't believe the address (the address is
	 the address of the corresponding int).  Note that
	 this is only different from the BELIEVE_PCC_PROMOTION
	 case on big-endian machines.

	 My guess is that this correction, as opposed to changing
	 the parameter to an 'int' (as done below, for PCC
	 on most machines), is the right thing to do
	 on all machines, but I don't want to risk breaking
	 something that already works.  On most PCC machines,
	 the sparc problem doesn't come up because the calling
	 function has to zero the top bytes (not knowing whether
	 the called function wants an int or a short), so there
	 is no practical difference between an int and a short
	 (except perhaps what happens when the GDB user types
	 "print short_arg = 0x10000;"). 

	 Hacked for SunOS 4.1 by gnu@cygnus.com.  In 4.1, the compiler
	 actually produces the correct address (we don't need to fix it
	 up).  I made this code adapt so that it will offset the symbol
	 if it was pointing at an int-aligned location and not
	 otherwise.  This way you can use the same gdb for 4.0.x and
	 4.1 systems.

	If the parameter is shorter than an int, and is integral
	(e.g. char, short, or unsigned equivalent), and is claimed to
	be passed on an integer boundary, don't believe it!  Offset the
	parameter's address to the tail-end of that integer.  */

      temptype = lookup_fundamental_type (objfile, FT_INTEGER);
      if (TYPE_LENGTH (SYMBOL_TYPE (sym)) < TYPE_LENGTH (temptype)
	  && TYPE_CODE (SYMBOL_TYPE (sym)) == TYPE_CODE_INT
	  && 0 == SYMBOL_VALUE (sym) % TYPE_LENGTH (temptype))
	{
	  SYMBOL_VALUE (sym) += TYPE_LENGTH (temptype)
			      - TYPE_LENGTH (SYMBOL_TYPE (sym));
	}
      break;

#else /* no BELIEVE_PCC_PROMOTION_TYPE.  */

      /* If PCC says a parameter is a short or a char,
	 it is really an int.  */
      temptype = lookup_fundamental_type (objfile, FT_INTEGER);
      if (TYPE_LENGTH (SYMBOL_TYPE (sym)) < TYPE_LENGTH (temptype)
	  && TYPE_CODE (SYMBOL_TYPE (sym)) == TYPE_CODE_INT)
	{
	  SYMBOL_TYPE (sym) = TYPE_UNSIGNED (SYMBOL_TYPE (sym))
	    ? lookup_fundamental_type (objfile, FT_UNSIGNED_INTEGER)
	      : temptype;
      }
      break;

#endif /* no BELIEVE_PCC_PROMOTION_TYPE.  */

    case 'P':
      /* acc seems to use P to delare the prototypes of functions that
         are referenced by this file.  gdb is not prepared to deal
         with this extra information.  FIXME, it ought to.  */
      if (type == N_FUN)
	goto process_prototype_types;

      /* Parameter which is in a register.  */
      SYMBOL_CLASS (sym) = LOC_REGPARM;
      SYMBOL_VALUE (sym) = STAB_REG_TO_REGNUM (valu);
      if (SYMBOL_VALUE (sym) >= NUM_REGS)
	{
	  complain (&reg_value_complaint, SYMBOL_NAME (sym));
	  SYMBOL_VALUE (sym) = SP_REGNUM;  /* Known safe, though useless */
	}
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      add_symbol_to_list (sym, &local_symbols);
      break;

    case 'R':
    case 'r':
      /* Register variable (either global or local).  */
      /* XXX */
#ifdef sparc
      if (! processing_gcc_compilation) {
	      struct symbol *s0;
	      
	      /*
	       * If we see a parm decl immediately followed by a reg decl of
	       * the same name (and in the same block), we change it to a
	       * single instance of a reg parm.  Sun's cc will generate these.
	       */
	      if (local_symbols && 
		  (s0 = local_symbols->symbol[local_symbols->nsyms - 1]) &&
		  SYMBOL_CLASS(s0) == LOC_ARG && 
		  strcmp(SYMBOL_NAME(s0), SYMBOL_NAME(sym)) == 0) {
		      SYMBOL_CLASS (s0) = LOC_REGPARM;
		      SYMBOL_VALUE (s0) = STAB_REG_TO_REGNUM (valu);
		      SYMBOL_NAMESPACE (s0) = VAR_NAMESPACE;
		      return s0;
	      }
      }
#endif
      SYMBOL_CLASS (sym) = LOC_REGISTER;
      SYMBOL_VALUE (sym) = STAB_REG_TO_REGNUM (valu);
      if (SYMBOL_VALUE (sym) >= NUM_REGS)
	{
	  complain (&reg_value_complaint, SYMBOL_NAME (sym));
	  SYMBOL_VALUE (sym) = SP_REGNUM;  /* Known safe, though useless */
	}
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      if (within_function)
        add_symbol_to_list (sym, &local_symbols);
      else
        add_symbol_to_list (sym, &file_symbols);
      break;

    case 'S':
      /* Static symbol at top level of file */
      SYMBOL_CLASS (sym) = LOC_STATIC;
      SYMBOL_VALUE_ADDRESS (sym) = valu;
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      add_symbol_to_list (sym, &file_symbols);
      break;

    case 't':
      /* For a nameless type, we don't want a create a symbol, thus we
	 did not use `sym'. Return without further processing. */
      if (nameless) return NULL;

      SYMBOL_CLASS (sym) = LOC_TYPEDEF;
      SYMBOL_VALUE (sym) = valu;
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      /* C++ vagaries: we may have a type which is derived from
	a base type which did not have its name defined when the
	derived class was output.  We fill in the derived class's
	base part member's name here in that case.  */
      if (TYPE_NAME (SYMBOL_TYPE (sym)) != NULL)
        if ((TYPE_CODE (SYMBOL_TYPE (sym)) == TYPE_CODE_STRUCT
		 || TYPE_CODE (SYMBOL_TYPE (sym)) == TYPE_CODE_UNION)
		&& TYPE_N_BASECLASSES (SYMBOL_TYPE (sym)))
	 {
	   int j;
	   for (j = TYPE_N_BASECLASSES (SYMBOL_TYPE (sym)) - 1; j >= 0; j--)
	     if (TYPE_BASECLASS_NAME (SYMBOL_TYPE (sym), j) == 0)
	       TYPE_BASECLASS_NAME (SYMBOL_TYPE (sym), j) =
		 type_name_no_tag (TYPE_BASECLASS (SYMBOL_TYPE (sym), j));
	 }

      add_symbol_to_list (sym, &file_symbols);
      break;

    case 'T':
      /* For a nameless type, we don't want a create a symbol, thus we
	 did not use `sym'. Return without further processing. */
      if (nameless) return NULL;

      SYMBOL_CLASS (sym) = LOC_TYPEDEF;
      SYMBOL_VALUE (sym) = valu;
      SYMBOL_NAMESPACE (sym) = STRUCT_NAMESPACE;
      if (TYPE_NAME (SYMBOL_TYPE (sym)) == 0)
	TYPE_NAME (SYMBOL_TYPE (sym))
	  = obconcat (&objfile -> type_obstack, "",
		      (TYPE_CODE (SYMBOL_TYPE (sym)) == TYPE_CODE_ENUM
		       ? "enum "
		       : (TYPE_CODE (SYMBOL_TYPE (sym)) == TYPE_CODE_STRUCT
			  ? "struct " : "union ")),
		      SYMBOL_NAME (sym));
      add_symbol_to_list (sym, &file_symbols);

      if (synonym)
	{
	  register struct symbol *typedef_sym = (struct symbol *)
	    obstack_alloc (&objfile -> symbol_obstack, sizeof (struct symbol));
	  memset (typedef_sym, 0, sizeof (struct symbol));
	  SYMBOL_NAME (typedef_sym) = SYMBOL_NAME (sym);
	  SYMBOL_TYPE (typedef_sym) = SYMBOL_TYPE (sym);

	  SYMBOL_CLASS (typedef_sym) = LOC_TYPEDEF;
	  SYMBOL_VALUE (typedef_sym) = valu;
	  SYMBOL_NAMESPACE (typedef_sym) = VAR_NAMESPACE;
	  add_symbol_to_list (typedef_sym, &file_symbols);
	}
      break;

    case 'V':
      /* Static symbol of local scope */
      SYMBOL_CLASS (sym) = LOC_STATIC;
      SYMBOL_VALUE_ADDRESS (sym) = valu;
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      add_symbol_to_list (sym, &local_symbols);
      break;

    case 'v':
      /* Reference parameter */
      SYMBOL_CLASS (sym) = LOC_REF_ARG;
      SYMBOL_VALUE (sym) = valu;
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      add_symbol_to_list (sym, &local_symbols);
      break;

    case 'X':
      /* This is used by Sun FORTRAN for "function result value".
	 Sun claims ("dbx and dbxtool interfaces", 2nd ed)
	 that Pascal uses it too, but when I tried it Pascal used
	 "x:3" (local symbol) instead.  */
      SYMBOL_CLASS (sym) = LOC_LOCAL;
      SYMBOL_VALUE (sym) = valu;
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      add_symbol_to_list (sym, &local_symbols);
      break;

    default:
      error ("Invalid symbol data: unknown symbol-type code `%c' at symtab pos %d.", deftype, symnum);
    }
  return sym;
}


/* Skip rest of this symbol and return an error type.

   General notes on error recovery:  error_type always skips to the
   end of the symbol (modulo cretinous dbx symbol name continuation).
   Thus code like this:

   if (*(*pp)++ != ';')
     return error_type (pp);

   is wrong because if *pp starts out pointing at '\0' (typically as the
   result of an earlier error), it will be incremented to point to the
   start of the next symbol, which might produce strange results, at least
   if you run off the end of the string table.  Instead use

   if (**pp != ';')
     return error_type (pp);
   ++*pp;

   or

   if (**pp != ';')
     foo = error_type (pp);
   else
     ++*pp;

   And in case it isn't obvious, the point of all this hair is so the compiler
   can define new types and new syntaxes, and old versions of the
   debugger will be able to read the new symbol tables.  */

struct type *
error_type (pp)
     char **pp;
{
  complain (&error_type_complaint, 0);
  while (1)
    {
      /* Skip to end of symbol.  */
      while (**pp != '\0')
	(*pp)++;

      /* Check for and handle cretinous dbx symbol name continuation!  */
      if ((*pp)[-1] == '\\')
	*pp = next_symbol_text ();
      else
	break;
    }
  return builtin_type_error;
}


/* Read a dbx type reference or definition;
   return the type that is meant.
   This can be just a number, in which case it references
   a type already defined and placed in type_vector.
   Or the number can be followed by an =, in which case
   it means to define a new type according to the text that
   follows the =.  */

struct type *
read_type (pp, objfile)
     register char **pp;
     struct objfile *objfile;
{
  register struct type *type = 0;
  struct type *type1;
  int typenums[2];
  int xtypenums[2];

  /* Read type number if present.  The type number may be omitted.
     for instance in a two-dimensional array declared with type
     "ar1;1;10;ar1;1;10;4".  */
  if ((**pp >= '0' && **pp <= '9')
      || **pp == '(')
    {
      read_type_number (pp, typenums);
      
      /* Type is not being defined here.  Either it already exists,
	 or this is a forward reference to it.  dbx_alloc_type handles
	 both cases.  */
      if (**pp != '=')
	return dbx_alloc_type (typenums, objfile);

      /* Type is being defined here.  */
#if 0 /* Callers aren't prepared for a NULL result!  FIXME -- metin!  */
      {
	struct type *tt;

	/* if such a type already exists, this is an unnecessary duplication
	   of the stab string, which is common in (RS/6000) xlc generated
	   objects.  In that case, simply return NULL and let the caller take
	   care of it. */

	tt = *dbx_lookup_type (typenums);
	if (tt && tt->length && tt->code)
	  return NULL;
      }
#endif

      *pp += 2;
    }
  else
    {
      /* 'typenums=' not present, type is anonymous.  Read and return
	 the definition, but don't put it in the type vector.  */
      typenums[0] = typenums[1] = -1;
      *pp += 1;
    }

  switch ((*pp)[-1])
    {
    case 'x':
      {
	enum type_code code;

	/* Used to index through file_symbols.  */
	struct pending *ppt;
	int i;
	
	/* Name including "struct", etc.  */
	char *type_name;
	
	/* Name without "struct", etc.  */
	char *type_name_only;

	{
	  char *prefix;
	  char *from, *to;
	  
	  /* Set the type code according to the following letter.  */
	  switch ((*pp)[0])
	    {
	    case 's':
	      code = TYPE_CODE_STRUCT;
	      prefix = "struct ";
	      break;
	    case 'u':
	      code = TYPE_CODE_UNION;
	      prefix = "union ";
	      break;
	    case 'e':
	      code = TYPE_CODE_ENUM;
	      prefix = "enum ";
	      break;
	    default:
	      return error_type (pp);
	    }
	  
	  to = type_name = (char *)
	    obstack_alloc (&objfile -> type_obstack,
			   (strlen (prefix) +
			    ((char *) strchr (*pp, ':') - (*pp)) + 1));
	
	  /* Copy the prefix.  */
	  from = prefix;
	  while (*to++ = *from++)
	    ;
	  to--; 
	
	  type_name_only = to;

	  /* Copy the name.  */
	  from = *pp + 1;
	  while ((*to++ = *from++) != ':')
	    ;
	  *--to = '\0';
	  
	  /* Set the pointer ahead of the name which we just read.  */
	  *pp = from;
	
#if 0
	  /* The following hack is clearly wrong, because it doesn't
	     check whether we are in a baseclass.  I tried to reproduce
	     the case that it is trying to fix, but I couldn't get
	     g++ to put out a cross reference to a basetype.  Perhaps
	     it doesn't do it anymore.  */
	  /* Note: for C++, the cross reference may be to a base type which
	     has not yet been seen.  In this case, we skip to the comma,
	     which will mark the end of the base class name.  (The ':'
	     at the end of the base class name will be skipped as well.)
	     But sometimes (ie. when the cross ref is the last thing on
	     the line) there will be no ','.  */
	  from = (char *) strchr (*pp, ',');
	  if (from)
	    *pp = from;
#endif /* 0 */
	}

	/* Now check to see whether the type has already been declared.  */
	/* This is necessary at least in the case where the
	   program says something like
	     struct foo bar[5];
	   The compiler puts out a cross-reference; we better find
	   set the length of the structure correctly so we can
	   set the length of the array.  */
	for (ppt = file_symbols; ppt; ppt = ppt->next)
	  for (i = 0; i < ppt->nsyms; i++)
	    {
	      struct symbol *sym = ppt->symbol[i];

	      if (SYMBOL_CLASS (sym) == LOC_TYPEDEF
		  && SYMBOL_NAMESPACE (sym) == STRUCT_NAMESPACE
		  && (TYPE_CODE (SYMBOL_TYPE (sym)) == code)
		  && !strcmp (SYMBOL_NAME (sym), type_name_only))
		{
		  obstack_free (&objfile -> type_obstack, type_name);
		  type = SYMBOL_TYPE (sym);
		  return type;
		}
	    }
	
	/* Didn't find the type to which this refers, so we must
	   be dealing with a forward reference.  Allocate a type
	   structure for it, and keep track of it so we can
	   fill in the rest of the fields when we get the full
	   type.  */
	type = dbx_alloc_type (typenums, objfile);
	TYPE_CODE (type) = code;
	TYPE_NAME (type) = type_name;
	INIT_CPLUS_SPECIFIC(type);
	TYPE_FLAGS (type) |= TYPE_FLAG_STUB;

	add_undefined_type (type);
	return type;
      }

#ifdef IBM6000_HOST
    case '-':				/* RS/6000 built-in type */
      (*pp)--;
      type = builtin_type (pp);		/* (in xcoffread.c) */
      goto after_digits;
#endif

    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
    case '(':
      (*pp)--;
      read_type_number (pp, xtypenums);
      type = *dbx_lookup_type (xtypenums);
      /* fall through */

    after_digits:
      if (type == 0)
	type = lookup_fundamental_type (objfile, FT_VOID);
      if (typenums[0] != -1)
	*dbx_lookup_type (typenums) = type;
      break;

    /* In the following types, we must be sure to overwrite any existing
       type that the typenums refer to, rather than allocating a new one
       and making the typenums point to the new one.  This is because there
       may already be pointers to the existing type (if it had been
       forward-referenced), and we must change it to a pointer, function,
       reference, or whatever, *in-place*.  */

    case '*':
      type1 = read_type (pp, objfile);
      type = make_pointer_type (type1, dbx_lookup_type (typenums));
      break;

    case '&':				/* Reference to another type */
      type1 = read_type (pp, objfile);
      type = make_reference_type (type1, dbx_lookup_type (typenums));
      break;

    case 'f':				/* Function returning another type */
      type1 = read_type (pp, objfile);
      type = make_function_type (type1, dbx_lookup_type (typenums));
      break;

    case 'k':				/* Const qualifier on some type (Sun) */
      type = read_type (pp, objfile);
      /* FIXME! For now, we ignore const and volatile qualifiers.  */
      break;

    case 'B':				/* Volatile qual on some type (Sun) */
      type = read_type (pp, objfile);
      /* FIXME! For now, we ignore const and volatile qualifiers.  */
      break;

/* FIXME -- we should be doing smash_to_XXX types here.  */
    case '@':				/* Member (class & variable) type */
      {
	struct type *domain = read_type (pp, objfile);
	struct type *memtype;

	if (**pp != ',')
	  /* Invalid member type data format.  */
	  return error_type (pp);
	++*pp;

	memtype = read_type (pp, objfile);
	type = dbx_alloc_type (typenums, objfile);
	smash_to_member_type (type, domain, memtype);
      }
      break;

    case '#':				/* Method (class & fn) type */
      if ((*pp)[0] == '#')
	{
	  /* We'll get the parameter types from the name.  */
	  struct type *return_type;

	  *pp += 1;
	  return_type = read_type (pp, objfile);
	  if (*(*pp)++ != ';')
	    complain (&invalid_member_complaint, (char *) symnum);
	  type = allocate_stub_method (return_type);
	  if (typenums[0] != -1)
	    *dbx_lookup_type (typenums) = type;
	}
      else
	{
	  struct type *domain = read_type (pp, objfile);
	  struct type *return_type;
	  struct type **args;

	  if (*(*pp)++ != ',')
	    error ("invalid member type data format, at symtab pos %d.",
		   symnum);

	  return_type = read_type (pp, objfile);
	  args = read_args (pp, ';', objfile);
	  type = dbx_alloc_type (typenums, objfile);
	  smash_to_method_type (type, domain, return_type, args);
	}
      break;

    case 'r':				/* Range type */
      type = read_range_type (pp, typenums, objfile);
      if (typenums[0] != -1)
	*dbx_lookup_type (typenums) = type;
      break;

    case 'b':				/* Sun ACC builtin int type */
      type = read_sun_builtin_type (pp, typenums, objfile);
      if (typenums[0] != -1)
	*dbx_lookup_type (typenums) = type;
      break;

    case 'R':				/* Sun ACC builtin float type */
      type = read_sun_floating_type (pp, typenums, objfile);
      if (typenums[0] != -1)
	*dbx_lookup_type (typenums) = type;
      break;
    
    case 'e':				/* Enumeration type */
      type = dbx_alloc_type (typenums, objfile);
      type = read_enum_type (pp, type, objfile);
      *dbx_lookup_type (typenums) = type;
      break;

    case 's':				/* Struct type */
      type = dbx_alloc_type (typenums, objfile);
      if (!TYPE_NAME (type))
        TYPE_NAME (type) = type_synonym_name;
      type_synonym_name = 0;
      type = read_struct_type (pp, type, objfile);
      break;

    case 'u':				/* Union type */
      type = dbx_alloc_type (typenums, objfile);
      if (!TYPE_NAME (type))
	TYPE_NAME (type) = type_synonym_name;
      type_synonym_name = 0;
      type = read_struct_type (pp, type, objfile);
      TYPE_CODE (type) = TYPE_CODE_UNION;
      break;

    case 'a':				/* Array type */
      if (**pp != 'r')
	return error_type (pp);
      ++*pp;
      
      type = dbx_alloc_type (typenums, objfile);
      type = read_array_type (pp, type, objfile);
      break;

    default:
      --*pp;			/* Go back to the symbol in error */
				/* Particularly important if it was \0! */
      return error_type (pp);
    }

  if (type == 0)
    abort ();

  return type;
}

/* This page contains subroutines of read_type.  */

/* Read the description of a structure (or union type)
   and return an object describing the type.  */

static struct type *
read_struct_type (pp, type, objfile)
     char **pp;
     register struct type *type;
     struct objfile *objfile;
{
  /* Total number of methods defined in this class.
     If the class defines two `f' methods, and one `g' method,
     then this will have the value 3.  */
  int total_length = 0;

  struct nextfield
    {
      struct nextfield *next;
      int visibility;			/* 0=public, 1=protected, 2=public */
      struct field field;
    };

  struct next_fnfield
    {
      struct next_fnfield *next;
      struct fn_field fn_field;
    };

  struct next_fnfieldlist
    {
      struct next_fnfieldlist *next;
      struct fn_fieldlist fn_fieldlist;
    };

  register struct nextfield *list = 0;
  struct nextfield *new;
  register char *p;
  int nfields = 0;
  int non_public_fields = 0;
  register int n;

  register struct next_fnfieldlist *mainlist = 0;
  int nfn_fields = 0;

  TYPE_CODE (type) = TYPE_CODE_STRUCT;
  INIT_CPLUS_SPECIFIC(type);
  TYPE_FLAGS (type) &= ~TYPE_FLAG_STUB;

  /* First comes the total size in bytes.  */

  TYPE_LENGTH (type) = read_number (pp, 0);

  /* C++: Now, if the class is a derived class, then the next character
     will be a '!', followed by the number of base classes derived from.
     Each element in the list contains visibility information,
     the offset of this base class in the derived structure,
     and then the base type. */
  if (**pp == '!')
    {
      int i, n_baseclasses, offset;
      struct type *baseclass;
      int via_public;

      /* Nonzero if it is a virtual baseclass, i.e.,

	 struct A{};
	 struct B{};
	 struct C : public B, public virtual A {};

	 B is a baseclass of C; A is a virtual baseclass for C.  This is a C++
	 2.0 language feature.  */
      int via_virtual;

      *pp += 1;

      ALLOCATE_CPLUS_STRUCT_TYPE(type);

      n_baseclasses = read_number (pp, ',');
      /* Some stupid compilers have trouble with the following, so break
	 it up into simpler expressions.  */
#if 0
      TYPE_FIELD_VIRTUAL_BITS (type) = (B_TYPE *)
	TYPE_ALLOC (type, B_BYTES (n_baseclasses));
#else
      {
	int num_bytes = B_BYTES (n_baseclasses);
	char *pointer;
	
	pointer = (char *) TYPE_ALLOC (type, num_bytes);
	TYPE_FIELD_VIRTUAL_BITS (type) = (B_TYPE *) pointer;
      }
#endif /* 0 */

      B_CLRALL (TYPE_FIELD_VIRTUAL_BITS (type), n_baseclasses);

      for (i = 0; i < n_baseclasses; i++)
	{
	  if (**pp == '\\')
	    *pp = next_symbol_text ();

	  switch (**pp)
	    {
	    case '0':
	      via_virtual = 0;
	      break;
	    case '1':
	      via_virtual = 1;
	      break;
	    default:
	      /* Bad visibility format.  */
	      return error_type (pp);
	    }
	  ++*pp;

	  switch (**pp)
	    {
	    case '0':
	      via_public = 0;
	      non_public_fields++;
	      break;
	    case '2':
	      via_public = 2;
	      break;
	    default:
	      /* Bad visibility format.  */
	      return error_type (pp);
	    }
	  if (via_virtual) 
	    SET_TYPE_FIELD_VIRTUAL (type, i);
	  ++*pp;

	  /* Offset of the portion of the object corresponding to
	     this baseclass.  Always zero in the absence of
	     multiple inheritance.  */
	  offset = read_number (pp, ',');
	  baseclass = read_type (pp, objfile);
	  *pp += 1;		/* skip trailing ';' */

	  /* Make this baseclass visible for structure-printing purposes.  */
	  new = (struct nextfield *) alloca (sizeof (struct nextfield));
	  memset (new, 0, sizeof (struct nextfield));
	  new->next = list;
	  list = new;
	  list->visibility = via_public;
	  list->field.type = baseclass;
	  list->field.name = type_name_no_tag (baseclass);
	  list->field.bitpos = offset;
	  list->field.bitsize = 0;	/* this should be an unpacked field! */
	  nfields++;
	}
      TYPE_N_BASECLASSES (type) = n_baseclasses;
    }

  /* Now come the fields, as NAME:?TYPENUM,BITPOS,BITSIZE; for each one.
     At the end, we see a semicolon instead of a field.

     In C++, this may wind up being NAME:?TYPENUM:PHYSNAME; for
     a static field.

     The `?' is a placeholder for one of '/2' (public visibility),
     '/1' (protected visibility), '/0' (private visibility), or nothing
     (C style symbol table, public visibility).  */

  /* We better set p right now, in case there are no fields at all...    */
  p = *pp;

  while (**pp != ';')
    {
      /* Check for and handle cretinous dbx symbol name continuation!  */
      if (**pp == '\\') *pp = next_symbol_text ();

      /* Get space to record the next field's data.  */
      new = (struct nextfield *) alloca (sizeof (struct nextfield));
      memset (new, 0, sizeof (struct nextfield));
      new->next = list;
      list = new;

      /* Get the field name.  */
      p = *pp;
      if (*p == CPLUS_MARKER)
	{
	  if (*p == '_')    /* GNU C++ anonymous type.  */
	    ;
	  /* Special GNU C++ name.  */
	  else if (*++p == 'v')
	    {
	      const char *prefix;
	      char *name = 0;
	      struct type *context;

	      switch (*++p)
		{
		case 'f':
		  prefix = vptr_name;
		  break;
		case 'b':
		  prefix = vb_name;
		  break;
		default:
		  complain (&invalid_cpp_abbrev_complaint, *pp);
		  prefix = "INVALID_C++_ABBREV";
		  break;
		}
	      *pp = p + 1;
	      context = read_type (pp, objfile);
#ifdef maybe_this_is_fixed
	      /*
	       * Patch from Michael N. Lipp.
	       * 9204021327.AA02196@mailhost.dtro.e-technik.th-darmstadt.de>
	       * XXX - this isn't the right way to fix it, but it will at 
	       * least keep gdb from faulting.
	       */
	      /* if the ommitted type name is the name of the struct
		 being read, the name is not yet in TYPE_NAME(context).
		 Use long_kludge_name as a quick fix. */
	      if (context == type)
		      name = long_kludge_name;
	      else
#endif
	      name = type_name_no_tag (context);
	      if (name == 0)
		{
		  complain (&invalid_cpp_type_complaint, (char *) symnum);
		  name = "FOO";
		}
	      list->field.name = obconcat (&objfile -> type_obstack,
					   prefix, name, "");
	      p = ++(*pp);
	      if (p[-1] != ':')
		complain (&invalid_cpp_abbrev_complaint, *pp);
	      list->field.type = read_type (pp, objfile);
	      (*pp)++;			/* Skip the comma.  */
	      list->field.bitpos = read_number (pp, ';');
	      /* This field is unpacked.  */
	      list->field.bitsize = 0;
	      list->visibility = 0;	/* private */
	      non_public_fields++;

	      nfields++;
	      continue;
	    }
	  else
	    complain (&invalid_cpp_abbrev_complaint, *pp);
	}

      while (*p != ':') p++;
      list->field.name = obsavestring (*pp, p - *pp,
				       &objfile -> type_obstack);

      /* C++: Check to see if we have hit the methods yet.  */
      if (p[1] == ':')
	break;

      *pp = p + 1;

      /* This means we have a visibility for a field coming. */
      if (**pp == '/')
	{
	  switch (*++*pp)
	    {
	    case '0':
	      list->visibility = 0;	/* private */
	      non_public_fields++;
	      *pp += 1;
	      break;

 	    case '1':
 	      list->visibility = 1;	/* protected */
	      non_public_fields++;
 	      *pp += 1;
 	      break;

 	    case '2':
 	      list->visibility = 2;	/* public */
 	      *pp += 1;
 	      break;
 	    }
 	}
       else /* normal dbx-style format.  */
	list->visibility = 2;		/* public */

      list->field.type = read_type (pp, objfile);
      if (**pp == ':')
 	{
 	  p = ++(*pp);
#if 0
	  /* Possible future hook for nested types. */
	  if (**pp == '!')
	    {
	      list->field.bitpos = (long)-2; /* nested type */
 	      p = ++(*pp);
	    }
	  else
#endif
	    { /* Static class member.  */
	      list->field.bitpos = (long)-1;
	    }
 	  while (*p != ';') p++;
 	  list->field.bitsize = (long) savestring (*pp, p - *pp);
 	  *pp = p + 1;
 	  nfields++;
 	  continue;
 	}
       else if (**pp != ',')
	 /* Bad structure-type format.  */
	 return error_type (pp);

      (*pp)++;			/* Skip the comma.  */
      list->field.bitpos = read_number (pp, ',');
      list->field.bitsize = read_number (pp, ';');

#if 0
      /* FIXME-tiemann: Can't the compiler put out something which
	 lets us distinguish these? (or maybe just not put out anything
	 for the field).  What is the story here?  What does the compiler
	really do?  Also, patch gdb.texinfo for this case; I document
	it as a possible problem there.  Search for "DBX-style".  */

      /* This is wrong because this is identical to the symbols
	 produced for GCC 0-size arrays.  For example:
         typedef union {
	   int num;
	   char str[0];
	 } foo;
	 The code which dumped core in such circumstances should be
	 fixed not to dump core.  */

      /* g++ -g0 can put out bitpos & bitsize zero for a static
	 field.  This does not give us any way of getting its
	 class, so we can't know its name.  But we can just
	 ignore the field so we don't dump core and other nasty
	 stuff.  */
      if (list->field.bitpos == 0
	  && list->field.bitsize == 0)
	{
	  complain (&dbx_class_complaint, 0);
	  /* Ignore this field.  */
	  list = list->next;
	}
      else
#endif /* 0 */
	{
	  /* Detect an unpacked field and mark it as such.
	     dbx gives a bit size for all fields.
	     Note that forward refs cannot be packed,
	     and treat enums as if they had the width of ints.  */
	  if (TYPE_CODE (list->field.type) != TYPE_CODE_INT
	      && TYPE_CODE (list->field.type) != TYPE_CODE_ENUM)
	    list->field.bitsize = 0;
	  if ((list->field.bitsize == 8 * TYPE_LENGTH (list->field.type)
	       || (TYPE_CODE (list->field.type) == TYPE_CODE_ENUM
		   && (list->field.bitsize
		       == 8 * TYPE_LENGTH (lookup_fundamental_type (objfile, FT_INTEGER)))
		   )
	       )
	      &&
	      list->field.bitpos % 8 == 0)
	    list->field.bitsize = 0;
	  nfields++;
	}
    }

  if (p[1] == ':')
    /* chill the list of fields: the last entry (at the head)
       is a partially constructed entry which we now scrub.  */
    list = list->next;

  /* Now create the vector of fields, and record how big it is.
     We need this info to record proper virtual function table information
     for this class's virtual functions.  */

  TYPE_NFIELDS (type) = nfields;
  TYPE_FIELDS (type) = (struct field *)
    TYPE_ALLOC (type, sizeof (struct field) * nfields);
  memset (TYPE_FIELDS (type), 0, sizeof (struct field) * nfields);

  if (non_public_fields)
    {
      ALLOCATE_CPLUS_STRUCT_TYPE (type);

      TYPE_FIELD_PRIVATE_BITS (type) = (B_TYPE *)
	TYPE_ALLOC (type, B_BYTES (nfields));
      B_CLRALL (TYPE_FIELD_PRIVATE_BITS (type), nfields);

      TYPE_FIELD_PROTECTED_BITS (type) = (B_TYPE *)
	TYPE_ALLOC (type, B_BYTES (nfields));
      B_CLRALL (TYPE_FIELD_PROTECTED_BITS (type), nfields);
    }

  /* Copy the saved-up fields into the field vector.  */

  for (n = nfields; list; list = list->next)
    {
      n -= 1;
      TYPE_FIELD (type, n) = list->field;
      if (list->visibility == 0)
	SET_TYPE_FIELD_PRIVATE (type, n);
      else if (list->visibility == 1)
	SET_TYPE_FIELD_PROTECTED (type, n);
    }

  /* Now come the method fields, as NAME::methods
     where each method is of the form TYPENUM,ARGS,...:PHYSNAME;
     At the end, we see a semicolon instead of a field.

     For the case of overloaded operators, the format is
     op$::*.methods, where $ is the CPLUS_MARKER (usually '$'),
     `*' holds the place for an operator name (such as `+=')
     and `.' marks the end of the operator name.  */
  if (p[1] == ':')
    {
      /* Now, read in the methods.  To simplify matters, we
	 "unread" the name that has been read, so that we can
	 start from the top.  */

      ALLOCATE_CPLUS_STRUCT_TYPE (type);
      /* For each list of method lists... */
      do
	{
	  int i;
	  struct next_fnfield *sublist = 0;
	  struct type *look_ahead_type = NULL;
	  int length = 0;
	  struct next_fnfieldlist *new_mainlist;
	  char *main_fn_name;

	  new_mainlist = (struct next_fnfieldlist *)
	      alloca (sizeof (struct next_fnfieldlist));
	  memset (new_mainlist, 0, sizeof (struct next_fnfieldlist));

	  p = *pp;

	  /* read in the name.  */
	  while (*p != ':') p++;
	  if ((*pp)[0] == 'o' && (*pp)[1] == 'p' && (*pp)[2] == CPLUS_MARKER)
	    {
	      /* This is a completely wierd case.  In order to stuff in the
		 names that might contain colons (the usual name delimiter),
		 Mike Tiemann defined a different name format which is
		 signalled if the identifier is "op$".  In that case, the
		 format is "op$::XXXX." where XXXX is the name.  This is
		 used for names like "+" or "=".  YUUUUUUUK!  FIXME!  */
	      /* This lets the user type "break operator+".
	         We could just put in "+" as the name, but that wouldn't
		 work for "*".  */
	      static char opname[32] = {'o', 'p', CPLUS_MARKER};
	      char *o = opname + 3;

	      /* Skip past '::'.  */
	      *pp = p + 2;
	      if (**pp == '\\') *pp = next_symbol_text ();
	      p = *pp;
	      while (*p != '.')
		*o++ = *p++;
	      main_fn_name = savestring (opname, o - opname);
	      /* Skip past '.'  */
	      *pp = p + 1;
	    }
	  else
	    {
	      main_fn_name = savestring (*pp, p - *pp);
	      /* Skip past '::'.  */
	      *pp = p + 2;
	    }
	  new_mainlist->fn_fieldlist.name = main_fn_name;

	  do
	    {
	      struct next_fnfield *new_sublist =
		(struct next_fnfield *) alloca (sizeof (struct next_fnfield));
	      memset (new_sublist, 0, sizeof (struct next_fnfield));

	      /* Check for and handle cretinous dbx symbol name continuation!  */
	      if (look_ahead_type == NULL) /* Normal case. */
		{
		  if (**pp == '\\') *pp = next_symbol_text ();

		  new_sublist->fn_field.type = read_type (pp, objfile);
		  if (**pp != ':')
		    /* Invalid symtab info for method.  */
		    return error_type (pp);
	        }
	      else
		{ /* g++ version 1 kludge */
		  new_sublist->fn_field.type = look_ahead_type;
		  look_ahead_type = NULL;
	        }

	      *pp += 1;
	      p = *pp;
	      while (*p != ';') p++;

	      /* If this is just a stub, then we don't have the
		 real name here.  */
	      if (TYPE_FLAGS (new_sublist->fn_field.type) & TYPE_FLAG_STUB)
		new_sublist->fn_field.is_stub = 1;
	      new_sublist->fn_field.physname = savestring (*pp, p - *pp);
	      *pp = p + 1;

	      /* Set this method's visibility fields.  */
	      switch (*(*pp)++ - '0')
		{
		case 0:
		  new_sublist->fn_field.is_private = 1;
		  break;
		case 1:
		  new_sublist->fn_field.is_protected = 1;
		  break;
		}

	      if (**pp == '\\') *pp = next_symbol_text ();
	      switch (**pp)
		{
		case 'A': /* Normal functions. */
		  new_sublist->fn_field.is_const = 0;
		  new_sublist->fn_field.is_volatile = 0;
	          (*pp)++;
		  break;
		case 'B': /* `const' member functions. */
		  new_sublist->fn_field.is_const = 1;
		  new_sublist->fn_field.is_volatile = 0;
	          (*pp)++;
		  break;
		case 'C': /* `volatile' member function. */
		  new_sublist->fn_field.is_const = 0;
		  new_sublist->fn_field.is_volatile = 1;
	          (*pp)++;
		  break;
		case 'D': /* `const volatile' member function. */
		  new_sublist->fn_field.is_const = 1;
		  new_sublist->fn_field.is_volatile = 1;
	          (*pp)++;
		  break;
		case '*': /* File compiled with g++ version 1 -- no info */
		case '?':
		case '.':
		  break;
		default:
		  complain (&const_vol_complaint, (char *) (long) **pp);
		  break;
		}

	      switch (*(*pp)++)
		{
		case '*':
		  /* virtual member function, followed by index.  */
		  /* The sign bit is set to distinguish pointers-to-methods
		     from virtual function indicies.  Since the array is
		     in words, the quantity must be shifted left by 1
		     on 16 bit machine, and by 2 on 32 bit machine, forcing
		     the sign bit out, and usable as a valid index into
		     the array.  Remove the sign bit here.  */
		  new_sublist->fn_field.voffset =
		      (0x7fffffff & read_number (pp, ';')) + 2;

		  if (**pp == '\\') *pp = next_symbol_text ();

		  if (**pp == ';' || **pp == '\0')
		    /* Must be g++ version 1.  */
		    new_sublist->fn_field.fcontext = 0;
		  else
		    {
		      /* Figure out from whence this virtual function came.
			 It may belong to virtual function table of
			 one of its baseclasses.  */
		      look_ahead_type = read_type (pp, objfile);
		      if (**pp == ':')
			{ /* g++ version 1 overloaded methods. */ }
		      else
			{
			  new_sublist->fn_field.fcontext = look_ahead_type;
			  if (**pp != ';')
			    return error_type (pp);
			  else
			    ++*pp;
			  look_ahead_type = NULL;
		        }
		    }
		  break;

		case '?':
		  /* static member function.  */
		  new_sublist->fn_field.voffset = VOFFSET_STATIC;
		  if (strncmp (new_sublist->fn_field.physname,
			       main_fn_name, strlen (main_fn_name)))
		    new_sublist->fn_field.is_stub = 1;
		  break;

		default:
		  /* error */
		  complain (&member_fn_complaint, (char *) (long) (*pp)[-1]);
		  /* Fall through into normal member function.  */

		case '.':
		  /* normal member function.  */
		  new_sublist->fn_field.voffset = 0;
		  new_sublist->fn_field.fcontext = 0;
		  break;
		}

	      new_sublist->next = sublist;
	      sublist = new_sublist;
	      length++;
	      if (**pp == '\\') *pp = next_symbol_text ();
	    }
	  while (**pp != ';' && **pp != '\0');

	  *pp += 1;

	  new_mainlist->fn_fieldlist.fn_fields = (struct fn_field *)
	    obstack_alloc (&objfile -> type_obstack,
			   sizeof (struct fn_field) * length);
	  memset (new_mainlist->fn_fieldlist.fn_fields, 0,
		  sizeof (struct fn_field) * length);
	  for (i = length; (i--, sublist); sublist = sublist->next)
	    new_mainlist->fn_fieldlist.fn_fields[i] = sublist->fn_field;

	  new_mainlist->fn_fieldlist.length = length;
	  new_mainlist->next = mainlist;
	  mainlist = new_mainlist;
	  nfn_fields++;
	  total_length += length;
	  if (**pp == '\\') *pp = next_symbol_text ();
	}
      while (**pp != ';');
    }

  *pp += 1;


  if (nfn_fields)
    {
      TYPE_FN_FIELDLISTS (type) = (struct fn_fieldlist *)
	TYPE_ALLOC (type, sizeof (struct fn_fieldlist) * nfn_fields);
      memset (TYPE_FN_FIELDLISTS (type), 0,
	      sizeof (struct fn_fieldlist) * nfn_fields);
      TYPE_NFN_FIELDS (type) = nfn_fields;
      TYPE_NFN_FIELDS_TOTAL (type) = total_length;
    }

  {
    int i;
    for (i = 0; i < TYPE_N_BASECLASSES (type); ++i)
      {
	if (TYPE_CODE (TYPE_BASECLASS (type, i)) == TYPE_CODE_UNDEF)
	  /* @@ Memory leak on objfile->type_obstack?  */
	  return error_type (pp);
	TYPE_NFN_FIELDS_TOTAL (type) +=
	  TYPE_NFN_FIELDS_TOTAL (TYPE_BASECLASS (type, i));
      }
  }

  for (n = nfn_fields; mainlist; mainlist = mainlist->next) {
    --n;                      /* Circumvent Sun3 compiler bug */
    TYPE_FN_FIELDLISTS (type)[n] = mainlist->fn_fieldlist;
  }

  if (**pp == '~')
    {
      *pp += 1;

      if (**pp == '=' || **pp == '+' || **pp == '-')
	{
	  /* Obsolete flags that used to indicate the presence
	     of constructors and/or destructors. */
	  *pp += 1;
	}

      /* Read either a '%' or the final ';'.  */
      if (*(*pp)++ == '%')
	{
	  /* We'd like to be able to derive the vtable pointer field
	     from the type information, but when it's inherited, that's
	     hard.  A reason it's hard is because we may read in the
	     info about a derived class before we read in info about
	     the base class that provides the vtable pointer field.
	     Once the base info has been read, we could fill in the info
	     for the derived classes, but for the fact that by then,
	     we don't remember who needs what.  */

#if 0
	  int predicted_fieldno = -1;
#endif

	  /* Now we must record the virtual function table pointer's
	     field information.  */

	  struct type *t;
	  int i;


#if 0
	  {
	    /* In version 2, we derive the vfield ourselves.  */
	    for (n = 0; n < nfields; n++)
	      {
		if (! strncmp (TYPE_FIELD_NAME (type, n), vptr_name, 
			       sizeof (vptr_name) -1))
		  {
		    predicted_fieldno = n;
		    break;
		  }
	      }
	    if (predicted_fieldno < 0)
	      for (n = 0; n < TYPE_N_BASECLASSES (type); n++)
		if (! TYPE_FIELD_VIRTUAL (type, n)
		    && TYPE_VPTR_FIELDNO (TYPE_BASECLASS (type, n)) >= 0)
		  {
		    predicted_fieldno = TYPE_VPTR_FIELDNO (TYPE_BASECLASS (type, n));
		    break;
		  }
	  }
#endif

	  t = read_type (pp, objfile);
	  p = (*pp)++;
	  while (*p != '\0' && *p != ';')
	    p++;
	  if (*p == '\0')
	    /* Premature end of symbol.  */
	    return error_type (pp);
	  
	  TYPE_VPTR_BASETYPE (type) = t;
	  if (type == t)
	    {
	      if (TYPE_FIELD_NAME (t, TYPE_N_BASECLASSES (t)) == 0)
		{
		  /* FIXME-tiemann: what's this?  */
#if 0
		  TYPE_VPTR_FIELDNO (type) = i = TYPE_N_BASECLASSES (t);
#else
		  error_type (pp);
#endif
		}
	      else for (i = TYPE_NFIELDS (t) - 1; i >= TYPE_N_BASECLASSES (t); --i)
		if (! strncmp (TYPE_FIELD_NAME (t, i), vptr_name, 
			       sizeof (vptr_name) - 1))
		  {
		    TYPE_VPTR_FIELDNO (type) = i;
		    break;
		  }
	      if (i < 0)
		/* Virtual function table field not found.  */
		return error_type (pp);
	    }
	  else
	    TYPE_VPTR_FIELDNO (type) = TYPE_VPTR_FIELDNO (t);

#if 0
	  if (TYPE_VPTR_FIELDNO (type) != predicted_fieldno)
	    error ("TYPE_VPTR_FIELDNO miscalculated");
#endif

	  *pp = p + 1;
	}
    }

  return type;
}

/* Read a definition of an array type,
   and create and return a suitable type object.
   Also creates a range type which represents the bounds of that
   array.  */

static struct type *
read_array_type (pp, type, objfile)
     register char **pp;
     register struct type *type;
     struct objfile *objfile;
{
  struct type *index_type, *element_type, *range_type;
  int lower, upper;
  int adjustable = 0;

  /* Format of an array type:
     "ar<index type>;lower;upper;<array_contents_type>".  Put code in
     to handle this.

     Fortran adjustable arrays use Adigits or Tdigits for lower or upper;
     for these, produce a type like float[][].  */

  index_type = read_type (pp, objfile);
  if (**pp != ';')
    /* Improper format of array type decl.  */
    return error_type (pp);
  ++*pp;

  if (!(**pp >= '0' && **pp <= '9'))
    {
      *pp += 1;
      adjustable = 1;
    }
  lower = read_number (pp, ';');

  if (!(**pp >= '0' && **pp <= '9'))
    {
      *pp += 1;
      adjustable = 1;
    }
  upper = read_number (pp, ';');
  
  element_type = read_type (pp, objfile);

  if (adjustable)
    {
      lower = 0;
      upper = -1;
    }

  {
    /* Create range type.  */
    range_type = alloc_type (objfile);
    TYPE_CODE (range_type) = TYPE_CODE_RANGE;
    TYPE_TARGET_TYPE (range_type) = index_type;

    /* This should never be needed.  */
    TYPE_LENGTH (range_type) = sizeof (int);

    TYPE_NFIELDS (range_type) = 2;
    TYPE_FIELDS (range_type) = (struct field *)
      TYPE_ALLOC (range_type, 2 * sizeof (struct field));
    memset (TYPE_FIELDS (range_type), 0, 2 * sizeof (struct field));
    TYPE_FIELD_BITPOS (range_type, 0) = lower;
    TYPE_FIELD_BITPOS (range_type, 1) = upper;
  }

  TYPE_CODE (type) = TYPE_CODE_ARRAY;
  TYPE_TARGET_TYPE (type) = element_type;
  TYPE_LENGTH (type) = (upper - lower + 1) * TYPE_LENGTH (element_type);
  TYPE_NFIELDS (type) = 1;
  TYPE_FIELDS (type) = (struct field *)
    TYPE_ALLOC (type, sizeof (struct field));
  memset (TYPE_FIELDS (type), 0, sizeof (struct field));
  TYPE_FIELD_TYPE (type, 0) = range_type;

  /* If we have an array whose element type is not yet known, but whose
     bounds *are* known, record it to be adjusted at the end of the file.  */
  if (TYPE_LENGTH (element_type) == 0 && !adjustable)
    add_undefined_type (type);

  return type;
}


/* Read a definition of an enumeration type,
   and create and return a suitable type object.
   Also defines the symbols that represent the values of the type.  */

static struct type *
read_enum_type (pp, type, objfile)
     register char **pp;
     register struct type *type;
     struct objfile *objfile;
{
  register char *p;
  char *name;
  register long n;
  register struct symbol *sym;
  int nsyms = 0;
  struct pending **symlist;
  struct pending *osyms, *syms;
  int o_nsyms;

#if 0
  /* FIXME!  The stabs produced by Sun CC merrily define things that ought
     to be file-scope, between N_FN entries, using N_LSYM.  What's a mother
     to do?  For now, force all enum values to file scope.  */
  if (within_function)
    symlist = &local_symbols;
  else
#endif
    symlist = &file_symbols;
  osyms = *symlist;
  o_nsyms = osyms ? osyms->nsyms : 0;

  /* Read the value-names and their values.
     The input syntax is NAME:VALUE,NAME:VALUE, and so on.
     A semicolon or comma instead of a NAME means the end.  */
  while (**pp && **pp != ';' && **pp != ',')
    {
      /* Check for and handle cretinous dbx symbol name continuation!  */
      if (**pp == '\\')	*pp = next_symbol_text ();

      p = *pp;
      while (*p != ':') p++;
      name = obsavestring (*pp, p - *pp, &objfile -> symbol_obstack);
      *pp = p + 1;
      n = read_number (pp, ',');

      sym = (struct symbol *)
	obstack_alloc (&objfile -> symbol_obstack, sizeof (struct symbol));
      memset (sym, 0, sizeof (struct symbol));
      SYMBOL_NAME (sym) = name;
      SYMBOL_CLASS (sym) = LOC_CONST;
      SYMBOL_NAMESPACE (sym) = VAR_NAMESPACE;
      SYMBOL_VALUE (sym) = n;
      add_symbol_to_list (sym, symlist);
      nsyms++;
    }

  if (**pp == ';')
    (*pp)++;			/* Skip the semicolon.  */

  /* Now fill in the fields of the type-structure.  */

  TYPE_LENGTH (type) = sizeof (int);
  TYPE_CODE (type) = TYPE_CODE_ENUM;
  TYPE_FLAGS (type) &= ~TYPE_FLAG_STUB;
  TYPE_NFIELDS (type) = nsyms;
  TYPE_FIELDS (type) = (struct field *)
    TYPE_ALLOC (type, sizeof (struct field) * nsyms);
  memset (TYPE_FIELDS (type), 0, sizeof (struct field) * nsyms);

  /* Find the symbols for the values and put them into the type.
     The symbols can be found in the symlist that we put them on
     to cause them to be defined.  osyms contains the old value
     of that symlist; everything up to there was defined by us.  */
  /* Note that we preserve the order of the enum constants, so
     that in something like "enum {FOO, LAST_THING=FOO}" we print
     FOO, not LAST_THING.  */

  for (syms = *symlist, n = 0; syms; syms = syms->next)
    {
      int j = 0;
      if (syms == osyms)
	j = o_nsyms;
      for (; j < syms->nsyms; j++,n++)
	{
	  struct symbol *xsym = syms->symbol[j];
	  SYMBOL_TYPE (xsym) = type;
	  TYPE_FIELD_NAME (type, n) = SYMBOL_NAME (xsym);
	  TYPE_FIELD_VALUE (type, n) = 0;
	  TYPE_FIELD_BITPOS (type, n) = SYMBOL_VALUE (xsym);
	  TYPE_FIELD_BITSIZE (type, n) = 0;
	}
      if (syms == osyms)
	break;
    }

#if 0
  /* This screws up perfectly good C programs with enums.  FIXME.  */
  /* Is this Modula-2's BOOLEAN type?  Flag it as such if so. */
  if(TYPE_NFIELDS(type) == 2 &&
     ((!strcmp(TYPE_FIELD_NAME(type,0),"TRUE") &&
       !strcmp(TYPE_FIELD_NAME(type,1),"FALSE")) ||
      (!strcmp(TYPE_FIELD_NAME(type,1),"TRUE") &&
       !strcmp(TYPE_FIELD_NAME(type,0),"FALSE"))))
     TYPE_CODE(type) = TYPE_CODE_BOOL;
#endif

  return type;
}

/* Sun's ACC uses a somewhat saner method for specifying the builtin
   typedefs in every file (for int, long, etc):

	type = b <signed> <width>; <offset>; <nbits>
	signed = u or s.  Possible c in addition to u or s (for char?).
	offset = offset from high order bit to start bit of type.
	width is # bytes in object of this type, nbits is # bits in type.

   The width/offset stuff appears to be for small objects stored in
   larger ones (e.g. `shorts' in `int' registers).  We ignore it for now,
   FIXME.  */

static struct type *
read_sun_builtin_type (pp, typenums, objfile)
     char **pp;
     int typenums[2];
     struct objfile *objfile;
{
  int nbits;
  int signed_type;

  switch (**pp)
    {
      case 's':
        signed_type = 1;
	break;
      case 'u':
	signed_type = 0;
	break;
      default:
	return error_type (pp);
    }
  (*pp)++;

  /* For some odd reason, all forms of char put a c here.  This is strange
     because no other type has this honor.  We can safely ignore this because
     we actually determine 'char'acterness by the number of bits specified in
     the descriptor.  */

  if (**pp == 'c')
    (*pp)++;

  /* The first number appears to be the number of bytes occupied
     by this type, except that unsigned short is 4 instead of 2.
     Since this information is redundant with the third number,
     we will ignore it.  */
  read_number (pp, ';');

  /* The second number is always 0, so ignore it too. */
  read_number (pp, ';');

  /* The third number is the number of bits for this type. */
  nbits = read_number (pp, 0);

  /* FIXME.  Here we should just be able to make a type of the right
     number of bits and signedness.  FIXME.  */

  if (nbits == TARGET_LONG_LONG_BIT)
    return (lookup_fundamental_type (objfile,
		 signed_type? FT_LONG_LONG: FT_UNSIGNED_LONG_LONG));
  
  if (nbits == TARGET_INT_BIT)
    {
      /* FIXME -- the only way to distinguish `int' from `long'
	 is to look at its name!  */
      if (signed_type)
	{
	  if (long_kludge_name && long_kludge_name[0] == 'l' /* long */)
	    return lookup_fundamental_type (objfile, FT_LONG);
	  else
	    return lookup_fundamental_type (objfile, FT_INTEGER);
	}
      else
	{
	  if (long_kludge_name
	      && ((long_kludge_name[0] == 'u' /* unsigned */ &&
		   long_kludge_name[9] == 'l' /* long */)
		  || (long_kludge_name[0] == 'l' /* long unsigned */)))
	    return lookup_fundamental_type (objfile, FT_UNSIGNED_LONG);
	  else
	    return lookup_fundamental_type (objfile, FT_UNSIGNED_INTEGER);
	}
    }
    
  if (nbits == TARGET_SHORT_BIT)
    return (lookup_fundamental_type (objfile,
		 signed_type? FT_SHORT: FT_UNSIGNED_SHORT));
  
  if (nbits == TARGET_CHAR_BIT)
    return (lookup_fundamental_type (objfile,
		 signed_type? FT_CHAR: FT_UNSIGNED_CHAR));
  
  if (nbits == 0)
    return lookup_fundamental_type (objfile, FT_VOID);
  
  return error_type (pp);
}

static struct type *
read_sun_floating_type (pp, typenums, objfile)
     char **pp;
     int typenums[2];
     struct objfile *objfile;
{
  int nbytes;

  /* The first number has more details about the type, for example
     FN_COMPLEX.  See the sun stab.h.  */
  read_number (pp, ';');

  /* The second number is the number of bytes occupied by this type */
  nbytes = read_number (pp, ';');

  if (**pp != 0)
    return error_type (pp);

  if (nbytes == TARGET_FLOAT_BIT / TARGET_CHAR_BIT)
    return lookup_fundamental_type (objfile, FT_FLOAT);

  if (nbytes == TARGET_DOUBLE_BIT / TARGET_CHAR_BIT)
    return lookup_fundamental_type (objfile, FT_DBL_PREC_FLOAT);

  if (nbytes == TARGET_LONG_DOUBLE_BIT / TARGET_CHAR_BIT)
    return lookup_fundamental_type (objfile, FT_EXT_PREC_FLOAT);

  return error_type (pp);
}

/* Read a number from the string pointed to by *PP.
   The value of *PP is advanced over the number.
   If END is nonzero, the character that ends the
   number must match END, or an error happens;
   and that character is skipped if it does match.
   If END is zero, *PP is left pointing to that character.

   If the number fits in a long, set *VALUE and set *BITS to 0.
   If not, set *BITS to be the number of bits in the number.

   If encounter garbage, set *BITS to -1.  */

static void
read_huge_number (pp, end, valu, bits)
     char **pp;
     int end;
     long *valu;
     int *bits;
{
  char *p = *pp;
  int sign = 1;
  long n = 0;
  int radix = 10;
  char overflow = 0;
  int nbits = 0;
  int c;
  long upper_limit;
  
  if (*p == '-')
    {
      sign = -1;
      p++;
    }

  /* Leading zero means octal.  GCC uses this to output values larger
     than an int (because that would be hard in decimal).  */
  if (*p == '0')
    {
      radix = 8;
      p++;
    }

  upper_limit = LONG_MAX / radix;
  while ((c = *p++) >= '0' && c <= ('0' + radix))
    {
      if (n <= upper_limit)
	{
	  n *= radix;
	  n += c - '0';		/* FIXME this overflows anyway */
	}
      else
	overflow = 1;
      
      /* This depends on large values being output in octal, which is
	 what GCC does. */
      if (radix == 8)
	{
	  if (nbits == 0)
	    {
	      if (c == '0')
		/* Ignore leading zeroes.  */
		;
	      else if (c == '1')
		nbits = 1;
	      else if (c == '2' || c == '3')
		nbits = 2;
	      else
		nbits = 3;
	    }
	  else
	    nbits += 3;
	}
    }
  if (end)
    {
      if (c && c != end)
	{
	  if (bits != NULL)
	    *bits = -1;
	  return;
	}
    }
  else
    --p;

  *pp = p;
  if (overflow)
    {
      if (nbits == 0)
	{
	  /* Large decimal constants are an error (because it is hard to
	     count how many bits are in them).  */
	  if (bits != NULL)
	    *bits = -1;
	  return;
	}
      
      /* -0x7f is the same as 0x80.  So deal with it by adding one to
	 the number of bits.  */
      if (sign == -1)
	++nbits;
      if (bits)
	*bits = nbits;
    }
  else
    {
      if (valu)
	*valu = n * sign;
      if (bits)
	*bits = 0;
    }
}

static struct type *
read_range_type (pp, typenums, objfile)
     char **pp;
     int typenums[2];
     struct objfile *objfile;
{
  int rangenums[2];
  long n2, n3;
  int n2bits, n3bits;
  int self_subrange;
  struct type *result_type;

  /* First comes a type we are a subrange of.
     In C it is usually 0, 1 or the type being defined.  */
  read_type_number (pp, rangenums);
  self_subrange = (rangenums[0] == typenums[0] &&
		   rangenums[1] == typenums[1]);

  /* A semicolon should now follow; skip it.  */
  if (**pp == ';')
    (*pp)++;

  /* The remaining two operands are usually lower and upper bounds
     of the range.  But in some special cases they mean something else.  */
  read_huge_number (pp, ';', &n2, &n2bits);
  read_huge_number (pp, ';', &n3, &n3bits);

  if (n2bits == -1 || n3bits == -1)
    return error_type (pp);
  
  /* If limits are huge, must be large integral type.  */
  if (n2bits != 0 || n3bits != 0)
    {
      char got_signed = 0;
      char got_unsigned = 0;
      /* Number of bits in the type.  */
      int nbits;

      /* Range from 0 to <large number> is an unsigned large integral type.  */
      if ((n2bits == 0 && n2 == 0) && n3bits != 0)
	{
	  got_unsigned = 1;
	  nbits = n3bits;
	}
      /* Range from <large number> to <large number>-1 is a large signed
	 integral type.  */
      else if (n2bits != 0 && n3bits != 0 && n2bits == n3bits + 1)
	{
	  got_signed = 1;
	  nbits = n2bits;
	}

      /* Check for "long long".  */
      if (got_signed && nbits == TARGET_LONG_LONG_BIT)
	return (lookup_fundamental_type (objfile, FT_LONG_LONG));
      if (got_unsigned && nbits == TARGET_LONG_LONG_BIT)
	return (lookup_fundamental_type (objfile, FT_UNSIGNED_LONG_LONG));

      if (got_signed || got_unsigned)
	{
	  result_type = alloc_type (objfile);
	  TYPE_LENGTH (result_type) = nbits / TARGET_CHAR_BIT;
	  TYPE_CODE (result_type) = TYPE_CODE_INT;
	  if (got_unsigned)
	    TYPE_FLAGS (result_type) |= TYPE_FLAG_UNSIGNED;
	  return result_type;
	}
      else
	return error_type (pp);
    }

  /* A type defined as a subrange of itself, with bounds both 0, is void.  */
  if (self_subrange && n2 == 0 && n3 == 0)
    return (lookup_fundamental_type (objfile, FT_VOID));

  /* If n3 is zero and n2 is not, we want a floating type,
     and n2 is the width in bytes.

     Fortran programs appear to use this for complex types also,
     and they give no way to distinguish between double and single-complex!
     We don't have complex types, so we would lose on all fortran files!
     So return type `double' for all of those.  It won't work right
     for the complex values, but at least it makes the file loadable.

     FIXME, we may be able to distinguish these by their names. FIXME.  */

  if (n3 == 0 && n2 > 0)
    {
      if (n2 == sizeof (float))
	return (lookup_fundamental_type (objfile, FT_FLOAT));
      return (lookup_fundamental_type (objfile, FT_DBL_PREC_FLOAT));
    }

  /* If the upper bound is -1, it must really be an unsigned int.  */

  else if (n2 == 0 && n3 == -1)
    {
      /* FIXME -- the only way to distinguish `unsigned int' from `unsigned
         long' is to look at its name!  */
      if (
       long_kludge_name && ((long_kludge_name[0] == 'u' /* unsigned */ &&
       			     long_kludge_name[9] == 'l' /* long */)
			 || (long_kludge_name[0] == 'l' /* long unsigned */)))
	return (lookup_fundamental_type (objfile, FT_UNSIGNED_LONG));
      else
	return (lookup_fundamental_type (objfile, FT_UNSIGNED_INTEGER));
    }

  /* Special case: char is defined (Who knows why) as a subrange of
     itself with range 0-127.  */
  else if (self_subrange && n2 == 0 && n3 == 127)
    return (lookup_fundamental_type (objfile, FT_CHAR));

  /* Assumptions made here: Subrange of self is equivalent to subrange
     of int.  FIXME:  Host and target type-sizes assumed the same.  */
  /* FIXME:  This is the *only* place in GDB that depends on comparing
     some type to a builtin type with ==.  Fix it! */
  else if (n2 == 0
	   && (self_subrange ||
	       *dbx_lookup_type (rangenums) == lookup_fundamental_type (objfile, FT_INTEGER)))
    {
      /* an unsigned type */
#ifdef LONG_LONG
      if (n3 == - sizeof (long long))
	return (lookup_fundamental_type (objfile, FT_UNSIGNED_LONG_LONG));
#endif
      /* FIXME -- the only way to distinguish `unsigned int' from `unsigned
	 long' is to look at its name!  */
      if (n3 == (unsigned long)~0L &&
       long_kludge_name && ((long_kludge_name[0] == 'u' /* unsigned */ &&
       			     long_kludge_name[9] == 'l' /* long */)
			 || (long_kludge_name[0] == 'l' /* long unsigned */)))
	return (lookup_fundamental_type (objfile, FT_UNSIGNED_LONG));
      if (n3 == (unsigned int)~0L)
	return (lookup_fundamental_type (objfile, FT_UNSIGNED_INTEGER));
      if (n3 == (unsigned short)~0L)
	return (lookup_fundamental_type (objfile, FT_UNSIGNED_SHORT));
      if (n3 == (unsigned char)~0L)
	return (lookup_fundamental_type (objfile, FT_UNSIGNED_CHAR));
    }
#ifdef LONG_LONG
  else if (n3 == 0 && n2 == -sizeof (long long))
    return (lookup_fundamental_type (objfile, FT_LONG_LONG));
#endif  
  else if (n2 == -n3 -1)
    {
      /* a signed type */
      /* FIXME -- the only way to distinguish `int' from `long' is to look
	 at its name!  */
      if ((n3 ==(long)(((unsigned long)1 << (8 * sizeof (long)  - 1)) - 1)) &&
       long_kludge_name && long_kludge_name[0] == 'l' /* long */)
	 return (lookup_fundamental_type (objfile, FT_LONG));
      if (n3 == (long)(((unsigned long)1 << (8 * sizeof (int)   - 1)) - 1))
	return (lookup_fundamental_type (objfile, FT_INTEGER));
      if (n3 ==        (               1 << (8 * sizeof (short) - 1)) - 1)
	return (lookup_fundamental_type (objfile, FT_SHORT));
      if (n3 ==        (               1 << (8 * sizeof (char)  - 1)) - 1)
	return (lookup_fundamental_type (objfile, FT_SIGNED_CHAR));
    }

  /* We have a real range type on our hands.  Allocate space and
     return a real pointer.  */

  /* At this point I don't have the faintest idea how to deal with
     a self_subrange type; I'm going to assume that this is used
     as an idiom, and that all of them are special cases.  So . . .  */
  if (self_subrange)
    return error_type (pp);

  result_type = alloc_type (objfile);

  TYPE_CODE (result_type) = TYPE_CODE_RANGE;

  TYPE_TARGET_TYPE (result_type) = *dbx_lookup_type(rangenums);
  if (TYPE_TARGET_TYPE (result_type) == 0) {
    complain (&range_type_base_complaint, (char *) rangenums[1]);
    TYPE_TARGET_TYPE (result_type) = lookup_fundamental_type (objfile, FT_INTEGER);
  }

  TYPE_NFIELDS (result_type) = 2;
  TYPE_FIELDS (result_type) = (struct field *)
    TYPE_ALLOC (result_type, 2 * sizeof (struct field));
  memset (TYPE_FIELDS (result_type), 0, 2 * sizeof (struct field));
  TYPE_FIELD_BITPOS (result_type, 0) = n2;
  TYPE_FIELD_BITPOS (result_type, 1) = n3;

  TYPE_LENGTH (result_type) = TYPE_LENGTH (TYPE_TARGET_TYPE (result_type));

  return result_type;
}

/* Read a number from the string pointed to by *PP.
   The value of *PP is advanced over the number.
   If END is nonzero, the character that ends the
   number must match END, or an error happens;
   and that character is skipped if it does match.
   If END is zero, *PP is left pointing to that character.  */

long
read_number (pp, end)
     char **pp;
     int end;
{
  register char *p = *pp;
  register long n = 0;
  register int c;
  int sign = 1;

  /* Handle an optional leading minus sign.  */

  if (*p == '-')
    {
      sign = -1;
      p++;
    }

  /* Read the digits, as far as they go.  */

  while ((c = *p++) >= '0' && c <= '9')
    {
      n *= 10;
      n += c - '0';
    }
  if (end)
    {
      if (c && c != end)
	error ("Invalid symbol data: invalid character \\%03o at symbol pos %d.", c, symnum);
    }
  else
    --p;

  *pp = p;
  return n * sign;
}

/* Read in an argument list.  This is a list of types, separated by commas
   and terminated with END.  Return the list of types read in, or (struct type
   **)-1 if there is an error.  */

static struct type **
read_args (pp, end, objfile)
     char **pp;
     int end;
     struct objfile *objfile;
{
  /* FIXME!  Remove this arbitrary limit!  */
  struct type *types[1024], **rval; /* allow for fns of 1023 parameters */
  int n = 0;

  while (**pp != end)
    {
      if (**pp != ',')
	/* Invalid argument list: no ','.  */
	return (struct type **)-1;
      *pp += 1;

      /* Check for and handle cretinous dbx symbol name continuation! */
      if (**pp == '\\')
	*pp = next_symbol_text ();

      types[n++] = read_type (pp, objfile);
    }
  *pp += 1;			/* get past `end' (the ':' character) */

  if (n == 1)
    {
      rval = (struct type **) xmalloc (2 * sizeof (struct type *));
    }
  else if (TYPE_CODE (types[n-1]) != TYPE_CODE_VOID)
    {
      rval = (struct type **) xmalloc ((n + 1) * sizeof (struct type *));
      memset (rval + n, 0, sizeof (struct type *));
    }
  else
    {
      rval = (struct type **) xmalloc (n * sizeof (struct type *));
    }
  memcpy (rval, types, n * sizeof (struct type *));
  return rval;
}

/* Add a common block's start address to the offset of each symbol
   declared to be in it (by being between a BCOMM/ECOMM pair that uses
   the common block name).  */

static void
fix_common_block (sym, valu)
    struct symbol *sym;
    int valu;
{
  struct pending *next = (struct pending *) SYMBOL_NAMESPACE (sym);
  for ( ; next; next = next->next)
    {
      register int j;
      for (j = next->nsyms - 1; j >= 0; j--)
	SYMBOL_VALUE_ADDRESS (next->symbol[j]) += valu;
    }
}



/* What about types defined as forward references inside of a small lexical
   scope?  */
/* Add a type to the list of undefined types to be checked through
   once this file has been read in.  */

void
add_undefined_type (type)
     struct type *type;
{
  if (undef_types_length == undef_types_allocated)
    {
      undef_types_allocated *= 2;
      undef_types = (struct type **)
	xrealloc ((char *) undef_types,
		  undef_types_allocated * sizeof (struct type *));
    }
  undef_types[undef_types_length++] = type;
}

/* Go through each undefined type, see if it's still undefined, and fix it
   up if possible.  We have two kinds of undefined types:

   TYPE_CODE_ARRAY:  Array whose target type wasn't defined yet.
			Fix:  update array length using the element bounds
			and the target type's length.
   TYPE_CODE_STRUCT, TYPE_CODE_UNION:  Structure whose fields were not
			yet defined at the time a pointer to it was made.
   			Fix:  Do a full lookup on the struct/union tag.  */
void
cleanup_undefined_types ()
{
  struct type **type;

  for (type = undef_types; type < undef_types + undef_types_length; type++)
    {
      switch (TYPE_CODE (*type))
	{

	  case TYPE_CODE_STRUCT:
	  case TYPE_CODE_UNION:
	  case TYPE_CODE_ENUM:
	  {
	    /* Check if it has been defined since.  */
	    if (TYPE_FLAGS (*type) & TYPE_FLAG_STUB)
	      {
		struct pending *ppt;
		int i;
		/* Name of the type, without "struct" or "union" */
		char *typename = TYPE_NAME (*type);

		if (!strncmp (typename, "struct ", 7))
		  typename += 7;
		if (!strncmp (typename, "union ", 6))
		  typename += 6;
		if (!strncmp (typename, "enum ", 5))
		  typename += 5;

		for (ppt = file_symbols; ppt; ppt = ppt->next)
		  {
		    for (i = 0; i < ppt->nsyms; i++)
		      {
			struct symbol *sym = ppt->symbol[i];
			
			if (SYMBOL_CLASS (sym) == LOC_TYPEDEF
			    && SYMBOL_NAMESPACE (sym) == STRUCT_NAMESPACE
			    && (TYPE_CODE (SYMBOL_TYPE (sym)) ==
				TYPE_CODE (*type))
			    && !strcmp (SYMBOL_NAME (sym), typename))
			  {
			    memcpy (*type, SYMBOL_TYPE (sym),
				    sizeof (struct type));
			  }
		      }
		  }
	      }
	  }
	  break;

	  case TYPE_CODE_ARRAY:
	  {
	    struct type *range_type;
	    int lower, upper;

	    if (TYPE_LENGTH (*type) != 0)		/* Better be unknown */
	      goto badtype;
	    if (TYPE_NFIELDS (*type) != 1)
	      goto badtype;
	    range_type = TYPE_FIELD_TYPE (*type, 0);
	    if (TYPE_CODE (range_type) != TYPE_CODE_RANGE)
	      goto badtype;

	    /* Now recompute the length of the array type, based on its
	       number of elements and the target type's length.  */
	    lower = TYPE_FIELD_BITPOS (range_type, 0);
	    upper = TYPE_FIELD_BITPOS (range_type, 1);
	    TYPE_LENGTH (*type) = (upper - lower + 1)
	      * TYPE_LENGTH (TYPE_TARGET_TYPE (*type));
	  }
	  break;

	  default:
	  badtype:
	  error ("GDB internal error.  cleanup_undefined_types with bad type %d.", TYPE_CODE (*type));
	  break;
	}
    }
  undef_types_length = 0;
}

/* Scan through all of the global symbols defined in the object file,
   assigning values to the debugging symbols that need to be assigned
   to.  Get these symbols from the minimal symbol table.  */

void
scan_file_globals (objfile)
     struct objfile *objfile;
{
  int hash;
  struct minimal_symbol *msymbol;
  struct symbol *sym, *prev;

  if (objfile->msymbols == 0)		/* Beware the null file.  */
    return;

  for (msymbol = objfile -> msymbols; msymbol -> name != NULL; msymbol++)
    {
      QUIT;

      prev = NULL;

      /* Get the hash index and check all the symbols
	 under that hash index. */

      hash = hashname (msymbol -> name);

      for (sym = global_sym_chain[hash]; sym;)
	{
	  if (*(msymbol -> name) == SYMBOL_NAME (sym)[0]
	      && !strcmp(msymbol -> name + 1, SYMBOL_NAME (sym) + 1))
	    {
	      /* Splice this symbol out of the hash chain and
		 assign the value we have to it. */
	      if (prev)
		{
		  SYMBOL_VALUE_CHAIN (prev) = SYMBOL_VALUE_CHAIN (sym);
		}
	      else
		{
		  global_sym_chain[hash] = SYMBOL_VALUE_CHAIN (sym);
		}
	      
	      /* Check to see whether we need to fix up a common block.  */
	      /* Note: this code might be executed several times for
		 the same symbol if there are multiple references.  */

	      if (SYMBOL_CLASS (sym) == LOC_BLOCK)
		{
		  fix_common_block (sym, msymbol -> address);
		}
	      else
		{
		  SYMBOL_VALUE_ADDRESS (sym) = msymbol -> address;
		}
	      
	      if (prev)
		{
		  sym = SYMBOL_VALUE_CHAIN (prev);
		}
	      else
		{
		  sym = global_sym_chain[hash];
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

/* Initialize anything that needs initializing when starting to read
   a fresh piece of a symbol file, e.g. reading in the stuff corresponding
   to a psymtab.  */

void
stabsread_init ()
{
}

/* Initialize anything that needs initializing when a completely new
   symbol file is specified (not just adding some symbols from another
   file, e.g. a shared library).  */

void
stabsread_new_init ()
{
  /* Empty the hash table of global syms looking for values.  */
  memset (global_sym_chain, 0, sizeof (global_sym_chain));
}

/* Initialize anything that needs initializing at the same time as
   start_symtab() is called. */

void start_stabs ()
{
  global_stabs = NULL;		/* AIX COFF */
  /* Leave FILENUM of 0 free for builtin types and this file's types.  */
  n_this_object_header_files = 1;
  type_vector_length = 0;
  type_vector = (struct type **) 0;
}

/* Call after end_symtab() */

void end_stabs ()
{
  if (type_vector)
    {
      free ((char *) type_vector);
    }
  type_vector = 0;
  type_vector_length = 0;
  previous_stab_code = 0;
}

void
finish_global_stabs (objfile)
     struct objfile *objfile;
{
  if (global_stabs)
    {
      patch_block_stabs (global_symbols, global_stabs, objfile);
      free ((PTR) global_stabs);
      global_stabs = NULL;
    }
}

/* Initializer for this module */

void
_initialize_stabsread ()
{
  undef_types_allocated = 20;
  undef_types_length = 0;
  undef_types = (struct type **)
    xmalloc (undef_types_allocated * sizeof (struct type *));
}
