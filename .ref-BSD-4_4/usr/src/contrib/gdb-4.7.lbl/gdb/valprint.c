/* Print values for GDB, the GNU debugger.
   Copyright 1986, 1988, 1989, 1991 Free Software Foundation, Inc.

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
#include <string.h>
#include "symtab.h"
#include "gdbtypes.h"
#include "value.h"
#include "gdbcore.h"
#include "gdbcmd.h"
#include "target.h"
#include "obstack.h"
#include "language.h"
#include "demangle.h"

#include <errno.h>

/* Prototypes for local functions */

static void
print_string PARAMS ((FILE *, char *, unsigned int, int));

static void
show_print PARAMS ((char *, int));

static void
set_print PARAMS ((char *, int));

static void
set_radix PARAMS ((char *, int, struct cmd_list_element *));

static void
set_output_radix PARAMS ((char *, int, struct cmd_list_element *));

static void
type_print_base PARAMS ((struct type *, FILE *, int, int));

static void
type_print_args PARAMS ((struct type *, FILE *));

static void
type_print_varspec_suffix PARAMS ((struct type *, FILE *, int, int, int));

static void
type_print_varspec_prefix PARAMS ((struct type *, FILE *, int, int));

static void
type_print_derivation_info PARAMS ((FILE *, struct type *));

static void
type_print_method_args PARAMS ((struct type **, char *, char *, int, FILE *));

static void
cplus_val_print PARAMS ((struct type *, char *, FILE *, int, int,
			 enum val_prettyprint, struct type **));

static void
val_print_fields PARAMS ((struct type *, char *, FILE *, int, int,
			  enum val_prettyprint, struct type **));

static int
is_vtbl_member PARAMS ((struct type *));

static int
is_vtbl_ptr_type PARAMS ((struct type *));

static void
print_hex_chars PARAMS ((FILE *, unsigned char *, unsigned));

extern int demangle;	/* whether to print C++ syms raw or source-form */

/* Maximum number of chars to print for a string pointer value
   or vector contents, or UINT_MAX for no limit.  */

static unsigned int print_max;

/* Default input and output radixes, and output format letter.  */

unsigned input_radix = 10;
unsigned output_radix = 10;
int output_format = 0;

/* Print repeat counts if there are more than this
   many repetitions of an element in an array.  */
#define	REPEAT_COUNT_THRESHOLD	10

/* Define a mess of print controls.  */

int prettyprint;	/* Controls pretty printing of structures */
int vtblprint;		/* Controls printing of vtbl's */
int unionprint;		/* Controls printing of nested unions.  */
int arrayprint;		/* Controls pretty printing of arrays.  */
int addressprint;	/* Controls pretty printing of addresses.  */
int objectprint;	/* Controls looking up an object's derived type
			   using what we find in its vtables.  */

struct obstack dont_print_obstack;


/* Print the character string STRING, printing at most LENGTH characters.
   Printing stops early if the number hits print_max; repeat counts
   are printed as appropriate.  Print ellipses at the end if we
   had to stop before printing LENGTH characters, or if FORCE_ELLIPSES.  */

static void
print_string (stream, string, length, force_ellipses)
     FILE *stream;
     char *string;
     unsigned int length;
     int force_ellipses;
{
  register unsigned int i;
  unsigned int things_printed = 0;
  int in_quotes = 0;
  int need_comma = 0;
  extern int inspect_it;

  if (length == 0)
    {
      fputs_filtered ("\"\"", stdout);
      return;
    }

  for (i = 0; i < length && things_printed < print_max; ++i)
    {
      /* Position of the character we are examining
	 to see whether it is repeated.  */
      unsigned int rep1;
      /* Number of repetitions we have detected so far.  */
      unsigned int reps;

      QUIT;

      if (need_comma)
	{
	  fputs_filtered (", ", stream);
	  need_comma = 0;
	}

      rep1 = i + 1;
      reps = 1;
      while (rep1 < length && string[rep1] == string[i])
	{
	  ++rep1;
	  ++reps;
	}

      if (reps > REPEAT_COUNT_THRESHOLD)
	{
	  if (in_quotes)
	    {
	      if (inspect_it)
		fputs_filtered ("\\\", ", stream);
	      else
		fputs_filtered ("\", ", stream);
	      in_quotes = 0;
	    }
	  fputs_filtered ("'", stream);
	  printchar (string[i], stream, '\'');
	  fprintf_filtered (stream, "' <repeats %u times>", reps);
	  i = rep1 - 1;
	  things_printed += REPEAT_COUNT_THRESHOLD;
	  need_comma = 1;
	}
      else
	{
	  if (!in_quotes)
	    {
	      if (inspect_it)
		fputs_filtered ("\\\"", stream);
	      else
		fputs_filtered ("\"", stream);
	      in_quotes = 1;
	    }
	  printchar (string[i], stream, '"');
	  ++things_printed;
	}
    }

  /* Terminate the quotes if necessary.  */
  if (in_quotes)
    {
      if (inspect_it)
	fputs_filtered ("\\\"", stream);
      else
	fputs_filtered ("\"", stream);
    }

  if (force_ellipses || i < length)
    fputs_filtered ("...", stream);
}

/* Print a floating point value of type TYPE, pointed to in GDB by VALADDR,
   on STREAM.  */

void
print_floating (valaddr, type, stream)
     char *valaddr;
     struct type *type;
     FILE *stream;
{
  double doub;
  int inv;
  unsigned len = TYPE_LENGTH (type);
  
#if defined (IEEE_FLOAT)

  /* Check for NaN's.  Note that this code does not depend on us being
     on an IEEE conforming system.  It only depends on the target
     machine using IEEE representation.  This means (a)
     cross-debugging works right, and (2) IEEE_FLOAT can (and should)
     be defined for systems like the 68881, which uses IEEE
     representation, but is not IEEE conforming.  */

  {
    long low, high;
    /* Is the sign bit 0?  */
    int nonnegative;
    /* Is it is a NaN (i.e. the exponent is all ones and
       the fraction is nonzero)?  */
    int is_nan;

    if (len == sizeof (float))
      {
	/* It's single precision. */
	memcpy ((char *) &low, valaddr, sizeof (low));
	/* target -> host.  */
	SWAP_TARGET_AND_HOST (&low, sizeof (float));
	nonnegative = low >= 0;
	is_nan = ((((low >> 23) & 0xFF) == 0xFF) 
		  && 0 != (low & 0x7FFFFF));
	low &= 0x7fffff;
	high = 0;
      }
    else
      {
	/* It's double precision.  Get the high and low words.  */

#if TARGET_BYTE_ORDER == BIG_ENDIAN
	memcpy (&low, valaddr+4,  sizeof (low));
	memcpy (&high, valaddr+0, sizeof (high));
#else
	memcpy (&low, valaddr+0,  sizeof (low));
	memcpy (&high, valaddr+4, sizeof (high));
#endif
	SWAP_TARGET_AND_HOST (&low, sizeof (low));
	SWAP_TARGET_AND_HOST (&high, sizeof (high));
	nonnegative = high >= 0;
	is_nan = (((high >> 20) & 0x7ff) == 0x7ff
		  && ! ((((high & 0xfffff) == 0)) && (low == 0)));
	high &= 0xfffff;
      }

    if (is_nan)
      {
	/* The meaning of the sign and fraction is not defined by IEEE.
	   But the user might know what they mean.  For example, they
	   (in an implementation-defined manner) distinguish between
	   signaling and quiet NaN's.  */
	if (high)
	  fprintf_filtered (stream, "-NaN(0x%lx%.8lx)" + nonnegative,
			    high, low);
	else
	  fprintf_filtered (stream, "-NaN(0x%lx)" + nonnegative, low);
	return;
      }
  }
#endif /* IEEE_FLOAT.  */

  doub = unpack_double (type, valaddr, &inv);
  if (inv)
    fprintf_filtered (stream, "<invalid float value>");
  else
    fprintf_filtered (stream, len <= sizeof(float) ? "%.9g" : "%.17g", doub);
}

/* VALADDR points to an integer of LEN bytes.  Print it in hex on stream.  */
static void
print_hex_chars (stream, valaddr, len)
     FILE *stream;
     unsigned char *valaddr;
     unsigned len;
{
  unsigned char *p;
  
  fprintf_filtered (stream, "0x");
#if TARGET_BYTE_ORDER == BIG_ENDIAN
  for (p = valaddr;
       p < valaddr + len;
       p++)
#else /* Little endian.  */
  for (p = valaddr + len - 1;
       p >= valaddr;
       p--)
#endif
    {
      fprintf_filtered (stream, "%02x", *p);
    }
}

/* Print the value VAL in C-ish syntax on stream STREAM.
   FORMAT is a format-letter, or 0 for print in natural format of data type.
   If the object printed is a string pointer, returns
   the number of string bytes printed.  */

int
value_print (val, stream, format, pretty)
     value val;
     FILE *stream;
     int format;
     enum val_prettyprint pretty;
{
  register unsigned int i, n, typelen;

  if (val == 0)
    {
      printf_filtered ("<address of value unknown>");
      return 0;
    }
  if (VALUE_OPTIMIZED_OUT (val))
    {
      printf_filtered ("<value optimized out>");
      return 0;
    }

  /* A "repeated" value really contains several values in a row.
     They are made by the @ operator.
     Print such values as if they were arrays.  */

  else if (VALUE_REPEATED (val))
    {
      n = VALUE_REPETITIONS (val);
      typelen = TYPE_LENGTH (VALUE_TYPE (val));
      fprintf_filtered (stream, "{");
      /* Print arrays of characters using string syntax.  */
      if (typelen == 1 && TYPE_CODE (VALUE_TYPE (val)) == TYPE_CODE_INT
	  && format == 0)
	print_string (stream, VALUE_CONTENTS (val), n, 0);
      else
	{
	  unsigned int things_printed = 0;
	  
	  for (i = 0; i < n && things_printed < print_max; i++)
	    {
	      /* Position of the array element we are examining to see
		 whether it is repeated.  */
	      unsigned int rep1;
	      /* Number of repetitions we have detected so far.  */
	      unsigned int reps;

	      if (i != 0)
		fprintf_filtered (stream, ", ");
	      wrap_here ("");

	      rep1 = i + 1;
	      reps = 1;
	      while (rep1 < n
		     && !memcmp (VALUE_CONTENTS (val) + typelen * i,
			       VALUE_CONTENTS (val) + typelen * rep1, typelen))
		{
		  ++reps;
		  ++rep1;
		}

	      if (reps > REPEAT_COUNT_THRESHOLD)
		{
		  val_print (VALUE_TYPE (val),
			     VALUE_CONTENTS (val) + typelen * i,
			     VALUE_ADDRESS (val) + typelen * i,
			     stream, format, 1, 0, pretty);
		  fprintf (stream, " <repeats %u times>", reps);
		  i = rep1 - 1;
		  things_printed += REPEAT_COUNT_THRESHOLD;
		}
	      else
		{
		  val_print (VALUE_TYPE (val),
			     VALUE_CONTENTS (val) + typelen * i,
			     VALUE_ADDRESS (val) + typelen * i,
			     stream, format, 1, 0, pretty);
		  things_printed++;
		}
	    }
	  if (i < n)
	    fprintf_filtered (stream, "...");
	}
      fprintf_filtered (stream, "}");
      return n * typelen;
    }
  else
    {
      struct type *type = VALUE_TYPE (val);

      /* If it is a pointer, indicate what it points to.

	 Print type also if it is a reference.

         C++: if it is a member pointer, we will take care
	 of that when we print it.  */
      if (TYPE_CODE (type) == TYPE_CODE_PTR
	  || TYPE_CODE (type) == TYPE_CODE_REF)
	{
	  /* Hack:  remove (char *) for char strings.  Their
	     type is indicated by the quoted string anyway. */
          if (TYPE_CODE (type) == TYPE_CODE_PTR
	      && TYPE_LENGTH (TYPE_TARGET_TYPE (type)) == sizeof(char)
	      && TYPE_CODE (TYPE_TARGET_TYPE (type)) == TYPE_CODE_INT
	      && !TYPE_UNSIGNED (TYPE_TARGET_TYPE (type)))
	    {
		/* Print nothing */
	    }
	  else
	    {
	      fprintf_filtered (stream, "(");
	      type_print (type, "", stream, -1);
	      fprintf_filtered (stream, ") ");
	    }
	}
      return val_print (type, VALUE_CONTENTS (val),
			VALUE_ADDRESS (val), stream, format, 1, 0, pretty);
    }
}

/* Return truth value for assertion that TYPE is of the type
   "pointer to virtual function".  */
static int
is_vtbl_ptr_type(type)
     struct type *type;
{
  char *typename = type_name_no_tag (type);
  static const char vtbl_ptr_name[] =
    { CPLUS_MARKER,'v','t','b','l','_','p','t','r','_','t','y','p','e', 0 };

  return (typename != NULL && !strcmp(typename, vtbl_ptr_name));
}

/* Return truth value for the assertion that TYPE is of the type
   "pointer to virtual function table".  */
static int
is_vtbl_member(type)
     struct type *type;
{
  if (TYPE_CODE (type) == TYPE_CODE_PTR)
    type = TYPE_TARGET_TYPE (type);
  else
    return 0;

  if (TYPE_CODE (type) == TYPE_CODE_ARRAY
      && TYPE_CODE (TYPE_TARGET_TYPE (type)) == TYPE_CODE_STRUCT)
    /* Virtual functions tables are full of pointers to virtual functions.  */
    return is_vtbl_ptr_type (TYPE_TARGET_TYPE (type));
  return 0;
}

/* Mutually recursive subroutines of cplus_val_print and val_print to print out
   a structure's fields: val_print_fields and cplus_val_print.

   TYPE, VALADDR, STREAM, RECURSE, and PRETTY have the
   same meanings as in cplus_val_print and val_print.

   DONT_PRINT is an array of baseclass types that we
   should not print, or zero if called from top level.  */

static void
val_print_fields (type, valaddr, stream, format, recurse, pretty, dont_print)
     struct type *type;
     char *valaddr;
     FILE *stream;
     int format;
     int recurse;
     enum val_prettyprint pretty;
     struct type **dont_print;
{
  int i, len, n_baseclasses;

  check_stub_type (type);

  fprintf_filtered (stream, "{");
  len = TYPE_NFIELDS (type);
  n_baseclasses = TYPE_N_BASECLASSES (type);

  /* Print out baseclasses such that we don't print
     duplicates of virtual baseclasses.  */
  if (n_baseclasses > 0)
    cplus_val_print (type, valaddr, stream, format, recurse+1, pretty, dont_print);

  if (!len && n_baseclasses == 1)
    fprintf_filtered (stream, "<No data fields>");
  else
    {
      extern int inspect_it;
      int fields_seen = 0;

      for (i = n_baseclasses; i < len; i++)
	{
	  /* Check if static field */
	  if (TYPE_FIELD_STATIC (type, i))
	    continue;
	  if (fields_seen)
	    fprintf_filtered (stream, ", ");
	  else if (n_baseclasses > 0)
	    {
	      if (pretty)
		{
		  fprintf_filtered (stream, "\n");
		  print_spaces_filtered (2 + 2 * recurse, stream);
		  fputs_filtered ("members of ", stream);
		  fputs_filtered (type_name_no_tag (type), stream);
		  fputs_filtered (": ", stream);
		}
	    }
	  fields_seen = 1;

	  if (pretty)
	    {
	      fprintf_filtered (stream, "\n");
	      print_spaces_filtered (2 + 2 * recurse, stream);
	    }
	  else 
	    {
	      wrap_here (n_spaces (2 + 2 * recurse));
	    }
	  if (inspect_it)
	    {
	      if (TYPE_CODE (TYPE_FIELD_TYPE (type, i)) == TYPE_CODE_PTR)
		fputs_filtered ("\"( ptr \"", stream);
	      else
		fputs_filtered ("\"( nodef \"", stream);
	      fprint_symbol (stream, TYPE_FIELD_NAME (type, i));
	      fputs_filtered ("\" \"", stream);
	      fprint_symbol (stream, TYPE_FIELD_NAME (type, i));
	      fputs_filtered ("\") \"", stream);
	    }
	  else
	    {
	      fprint_symbol (stream, TYPE_FIELD_NAME (type, i));
	      fputs_filtered (" = ", stream);
	    }
	  if (TYPE_FIELD_PACKED (type, i))
	    {
	      value v;

	      /* Bitfields require special handling, especially due to byte
		 order problems.  */
	      v = value_from_longest (TYPE_FIELD_TYPE (type, i),
				   unpack_field_as_long (type, valaddr, i));

	      val_print (TYPE_FIELD_TYPE (type, i), VALUE_CONTENTS (v), 0,
			 stream, format, 0, recurse + 1, pretty);
	    }
	  else
	    {
	      val_print (TYPE_FIELD_TYPE (type, i), 
			 valaddr + TYPE_FIELD_BITPOS (type, i) / 8,
			 0, stream, format, 0, recurse + 1, pretty);
	    }
	}
      if (pretty)
	{
	  fprintf_filtered (stream, "\n");
	  print_spaces_filtered (2 * recurse, stream);
	}
    }
  fprintf_filtered (stream, "}");
}

/* Special val_print routine to avoid printing multiple copies of virtual
   baseclasses.  */

static void
cplus_val_print (type, valaddr, stream, format, recurse, pretty, dont_print)
     struct type *type;
     char *valaddr;
     FILE *stream;
     int format;
     int recurse;
     enum val_prettyprint pretty;
     struct type **dont_print;
{
  struct obstack tmp_obstack;
  struct type **last_dont_print
    = (struct type **)obstack_next_free (&dont_print_obstack);
  int i, n_baseclasses = TYPE_N_BASECLASSES (type);

  if (dont_print == 0)
    {
      /* If we're at top level, carve out a completely fresh
	 chunk of the obstack and use that until this particular
	 invocation returns.  */
      tmp_obstack = dont_print_obstack;
      /* Bump up the high-water mark.  Now alpha is omega.  */
      obstack_finish (&dont_print_obstack);
    }

  for (i = 0; i < n_baseclasses; i++)
    {
      char *baddr;
      int err;

      if (BASETYPE_VIA_VIRTUAL (type, i))
	{
	  struct type **first_dont_print
	    = (struct type **)obstack_base (&dont_print_obstack);

	  int j = (struct type **)obstack_next_free (&dont_print_obstack)
	    - first_dont_print;

	  while (--j >= 0)
	    if (TYPE_BASECLASS (type, i) == first_dont_print[j])
	      goto flush_it;

	  obstack_ptr_grow (&dont_print_obstack, TYPE_BASECLASS (type, i));
	}

      /* Fix to use baseclass_offset instead. FIXME */
      baddr = baseclass_addr (type, i, valaddr, 0, &err);
      if (err == 0 && baddr == 0)
	error ("could not find virtual baseclass `%s'\n",
	       type_name_no_tag (TYPE_BASECLASS (type, i)));

      if (pretty)
	{
	  fprintf_filtered (stream, "\n");
	  print_spaces_filtered (2 * recurse, stream);
	}
      fputs_filtered ("<", stream);
      fputs_filtered (type_name_no_tag (TYPE_BASECLASS (type, i)), stream);
      fputs_filtered ("> = ", stream);
      if (err != 0)
	fprintf_filtered (stream, "<invalid address 0x%x>", baddr);
      else
	val_print_fields (TYPE_BASECLASS (type, i), baddr, stream, format,
			  recurse, pretty,
			  (struct type **)obstack_base (&dont_print_obstack));
      fputs_filtered (", ", stream);

    flush_it:
      ;
    }

  if (dont_print == 0)
    {
      /* Free the space used to deal with the printing
	 of this type from top level.  */
      obstack_free (&dont_print_obstack, last_dont_print);
      /* Reset watermark so that we can continue protecting
	 ourselves from whatever we were protecting ourselves.  */
      dont_print_obstack = tmp_obstack;
    }
}

static void
print_class_member (valaddr, domain, stream, prefix)
     char *valaddr;
     struct type *domain;
     FILE *stream;
     char *prefix;
{
  
  /* VAL is a byte offset into the structure type DOMAIN.
     Find the name of the field for that offset and
     print it.  */
  int extra = 0;
  int bits = 0;
  register unsigned int i;
  unsigned len = TYPE_NFIELDS (domain);
  /* @@ Make VAL into bit offset */
  LONGEST val = unpack_long (builtin_type_int, valaddr) << 3;
  for (i = TYPE_N_BASECLASSES (domain); i < len; i++)
    {
      int bitpos = TYPE_FIELD_BITPOS (domain, i);
      QUIT;
      if (val == bitpos)
	break;
      if (val < bitpos && i != 0)
	{
	  /* Somehow pointing into a field.  */
	  i -= 1;
	  extra = (val - TYPE_FIELD_BITPOS (domain, i));
	  if (extra & 0x7)
	    bits = 1;
	  else
	    extra >>= 3;
	  break;
	}
    }
  if (i < len)
    {
      char *name;
      fprintf_filtered (stream, prefix);
      name = type_name_no_tag (domain);
      if (name)
        fputs_filtered (name, stream);
      else
	type_print_base (domain, stream, 0, 0);
      fprintf_filtered (stream, "::");
      fputs_filtered (TYPE_FIELD_NAME (domain, i), stream);
      if (extra)
	fprintf_filtered (stream, " + %d bytes", extra);
      if (bits)
	fprintf_filtered (stream, " (offset in bits)");
    }
  else
    fprintf_filtered (stream, "%d", val >> 3);
}

/* Print data of type TYPE located at VALADDR (within GDB),
   which came from the inferior at address ADDRESS,
   onto stdio stream STREAM according to FORMAT
   (a letter or 0 for natural format).  The data at VALADDR
   is in target byte order.

   If the data are a string pointer, returns the number of
   sting characters printed.

   if DEREF_REF is nonzero, then dereference references,
   otherwise just print them like pointers.

   The PRETTY parameter controls prettyprinting.  */

int
val_print (type, valaddr, address, stream, format, deref_ref, recurse, pretty)
     struct type *type;
     char *valaddr;
     CORE_ADDR address;
     FILE *stream;
     int format;
     int deref_ref;
     int recurse;
     enum val_prettyprint pretty;
{
  register unsigned int i;
  unsigned len;
  struct type *elttype;
  unsigned eltlen;
  LONGEST val;
  unsigned char c;

  if (pretty == Val_pretty_default)
    {
      pretty = prettyprint ? Val_prettyprint : Val_no_prettyprint;
    }
  
  QUIT;

  check_stub_type (type);
  
  if (TYPE_FLAGS (type) & TYPE_FLAG_STUB)
    {
      fprintf_filtered (stream, "<unknown struct>");
      fflush (stream);
      return 0;
    }
  
  switch (TYPE_CODE (type))
    {
    case TYPE_CODE_ARRAY:
      if (TYPE_LENGTH (type) > 0
	  && TYPE_LENGTH (TYPE_TARGET_TYPE (type)) > 0)
	{
	  elttype = TYPE_TARGET_TYPE (type);
	  eltlen = TYPE_LENGTH (elttype);
	  len = TYPE_LENGTH (type) / eltlen;
	  if (arrayprint)
	    print_spaces_filtered (2 + 2 * recurse, stream);
	  fprintf_filtered (stream, "{");
	  /* For an array of chars, print with string syntax.  */
	  if (eltlen == 1 && TYPE_CODE (elttype) == TYPE_CODE_INT
	      && (format == 0 || format == 's') )
	    print_string (stream, valaddr, len, 0);
	  else
	    {
	      unsigned int things_printed = 0;
	      
	      /* If this is a virtual function table, print the 0th
		 entry specially, and the rest of the members normally.  */
	      if (is_vtbl_ptr_type (elttype))
		{
		  fprintf_filtered (stream, "%d vtable entries", len-1);
		  i = 1;
		}
	      else
		i = 0;

	      for (; i < len && things_printed < print_max; i++)
		{
		  /* Position of the array element we are examining to see
		     whether it is repeated.  */
		  unsigned int rep1;
		  /* Number of repetitions we have detected so far.  */
		  unsigned int reps;
		  
		  if (i != 0)
		    if (arrayprint)
		      {
		        fprintf_filtered (stream, ",\n");
	                print_spaces_filtered (2 + 2 * recurse, stream);
		      }
		    else
		      fprintf_filtered (stream, ", ");
		    wrap_here (n_spaces (2 + 2 * recurse));
		  
		  rep1 = i + 1;
		  reps = 1;
		  while (rep1 < len
			 && !memcmp (valaddr + i * eltlen,
				     valaddr + rep1 * eltlen, eltlen))
		    {
		      ++reps;
		      ++rep1;
		    }

		  if (reps > REPEAT_COUNT_THRESHOLD)
		    {
		      val_print (elttype, valaddr + i * eltlen,
				 0, stream, format, deref_ref,
				 recurse + 1, pretty);
		      fprintf_filtered (stream, " <repeats %u times>", reps);
		      i = rep1 - 1;
		      things_printed += REPEAT_COUNT_THRESHOLD;
		    }
		  else
		    {
		      val_print (elttype, valaddr + i * eltlen,
				 0, stream, format, deref_ref,
				 recurse + 1, pretty);
		      things_printed++;
		    }
		}
	      if (i < len)
		fprintf_filtered (stream, "...");
	    }
	  fprintf_filtered (stream, "}");
	  break;
	}
      /* Array of unspecified length: treat like pointer to first elt.  */
      valaddr = (char *) &address;

    case TYPE_CODE_PTR:
      if (format && format != 's')
	{
	  print_scalar_formatted (valaddr, type, format, 0, stream);
	  break;
	}
      if (TYPE_CODE (TYPE_TARGET_TYPE (type)) == TYPE_CODE_METHOD)
	{
	  struct type *domain, *ttype;
	  struct fn_field *f;
	  int j, len2;
	  char *kind = "";
	  CORE_ADDR addr;

	  ttype = TYPE_TARGET_TYPE (type);
	  if (ttype == 0 || (domain = TYPE_DOMAIN_TYPE(ttype)) == 0) {
		  printf("<bug workaround: can't print ptr to method>\n");
		  break;
	  }

	  addr = unpack_pointer (lookup_pointer_type (builtin_type_void),
				valaddr);
	  if (METHOD_PTR_IS_VIRTUAL(addr))
	    {
	      int offset = METHOD_PTR_TO_VOFFSET(addr);
	      len = TYPE_NFN_FIELDS (domain);
	      for (i = 0; i < len; i++)
		{
		  f = TYPE_FN_FIELDLIST1 (domain, i);
		  len2 = TYPE_FN_FIELDLIST_LENGTH (domain, i);

		  for (j = 0; j < len2; j++)
		    {
		      QUIT;
		      if (TYPE_FN_FIELD_VOFFSET (f, j) == offset)
			{
			  kind = "virtual ";
			  goto common;
			}
		    }
		}
	    }
	  else
	    {
	      struct symbol *sym = find_pc_function (addr);
	      if (sym == 0)
		error ("invalid pointer to member function");
	      len = TYPE_NFN_FIELDS (domain);
	      for (i = 0; i < len; i++)
		{
		  f = TYPE_FN_FIELDLIST1 (domain, i);
		  len2 = TYPE_FN_FIELDLIST_LENGTH (domain, i);

		  for (j = 0; j < len2; j++)
		    {
		      QUIT;
		      if (!strcmp (SYMBOL_NAME (sym), TYPE_FN_FIELD_PHYSNAME (f, j)))
			goto common;
		    }
		}
	    }
	common:
	  if (i < len)
	    {
	      fprintf_filtered (stream, "&");
	      type_print_varspec_prefix (TYPE_FN_FIELD_TYPE (f, j), stream, 0, 0);
	      fprintf (stream, kind);
	      if (TYPE_FN_FIELD_PHYSNAME (f, j)[0] == '_'
		  && TYPE_FN_FIELD_PHYSNAME (f, j)[1] == CPLUS_MARKER)
		type_print_method_args
		  (TYPE_FN_FIELD_ARGS (f, j) + 1, "~",
		   TYPE_FN_FIELDLIST_NAME (domain, i), 0, stream);
	      else
		type_print_method_args
		  (TYPE_FN_FIELD_ARGS (f, j), "",
		   TYPE_FN_FIELDLIST_NAME (domain, i), 0, stream);
	      break;
	    }
	  fprintf_filtered (stream, "(");
  	  type_print (type, "", stream, -1);
	  fprintf_filtered (stream, ") %d", (int) addr >> 3);
	}
      else if (TYPE_CODE (TYPE_TARGET_TYPE (type)) == TYPE_CODE_MEMBER)
	{
	  print_class_member (valaddr,
			      TYPE_DOMAIN_TYPE (TYPE_TARGET_TYPE (type)),
			      stream, "&");
	}
      else
	{
	  CORE_ADDR addr = unpack_pointer (type, valaddr);
	  elttype = TYPE_TARGET_TYPE (type);

	  if (TYPE_CODE (elttype) == TYPE_CODE_FUNC)
	    {
	      /* Try to print what function it points to.  */
	      print_address_demangle (addr, stream, demangle);
	      /* Return value is irrelevant except for string pointers.  */
	      return 0;
	    }

	  if (addressprint && format != 's')
	    fprintf_filtered (stream, "0x%x", addr);

	  /* For a pointer to char or unsigned char,
	     also print the string pointed to, unless pointer is null.  */
	  i = 0;		/* Number of characters printed.  */
	  if (TYPE_LENGTH (elttype) == 1 
	      && TYPE_CODE (elttype) == TYPE_CODE_INT
	      && (format == 0 || format == 's')
	      && addr != 0
	      /* If print_max is UINT_MAX, the alloca below will fail.
	         In that case don't try to print the string.  */
	      && print_max < UINT_MAX)
	    {
	      int first_addr_err = 0;
	      int errcode = 0;
	      
	      /* Get first character.  */
	      errcode = target_read_memory (addr, (char *)&c, 1);
	      if (errcode != 0)
		{
		  /* First address out of bounds.  */
		  first_addr_err = 1;
		}
	      else
		{
		  /* A real string.  */
		  char *string = (char *) alloca (print_max);

		  /* If the loop ends by us hitting print_max characters,
		     we need to have elipses at the end.  */
		  int force_ellipses = 1;

		  /* This loop always fetches print_max characters, even
		     though print_string might want to print more or fewer
		     (with repeated characters).  This is so that
		     we don't spend forever fetching if we print
		     a long string consisting of the same character
		     repeated.  Also so we can do it all in one memory
		     operation, which is faster.  However, this will be
		     slower if print_max is set high, e.g. if you set
		     print_max to 1000, not only will it take a long
		     time to fetch short strings, but if you are near
		     the end of the address space, it might not work. */
		  QUIT;
		  errcode = target_read_memory (addr, string, print_max);
		  if (errcode != 0)
		    {
		      /* Try reading just one character.  If that succeeds,
			 assume we hit the end of the address space, but
			 the initial part of the string is probably safe. */
		      char x[1];
		      errcode = target_read_memory (addr, x, 1);
		    }
		  if (errcode != 0)
		      force_ellipses = 0;
		  else 
		    for (i = 0; i < print_max; i++)
		      if (string[i] == '\0')
			{
			  force_ellipses = 0;
			  break;
		        }
		  QUIT;

		  if (addressprint)
		    fputs_filtered (" ", stream);
		  print_string (stream, string, i, force_ellipses);
		}

	      if (errcode != 0)
		{
		  if (errcode == EIO)
		    {
		      fprintf_filtered (stream,
					(" <Address 0x%x out of bounds>"
					 + first_addr_err),
					addr + i);
		    }
		  else
		    {
		      error ("Error reading memory address 0x%x: %s.",
			     addr + i, safe_strerror (errcode));
		    }
		}

	      fflush (stream);
	    }
	  else /* print vtbl's nicely */
 	  if (is_vtbl_member(type))
  	    {
	      CORE_ADDR vt_address = unpack_pointer (type, valaddr);

	      struct minimal_symbol *msymbol =
		lookup_minimal_symbol_by_pc (vt_address);
	      if ((msymbol != NULL) && (vt_address == msymbol -> address))
		{
		  fputs_filtered (" <", stream);
		  fputs_demangled (msymbol -> name, stream,
				   DMGL_ANSI | DMGL_PARAMS);
		  fputs_filtered (">", stream);
		}
	      if (vtblprint)
	        {
		  value vt_val;

		  vt_val = value_at (TYPE_TARGET_TYPE (type), vt_address);
		  val_print (VALUE_TYPE (vt_val), VALUE_CONTENTS (vt_val),
			     VALUE_ADDRESS (vt_val), stream, format,
			     deref_ref, recurse + 1, pretty);
		  if (pretty)
		    {
		      fprintf_filtered (stream, "\n");
		      print_spaces_filtered (2 + 2 * recurse, stream);
		    }
	        }
	      }

	  /* Return number of characters printed, plus one for the
	     terminating null if we have "reached the end".  */
	  return i + (print_max && i != print_max);
	}
      break;

    case TYPE_CODE_MEMBER:
      error ("not implemented: member type in val_print");
      break;

    case TYPE_CODE_REF:
      {
	CORE_ADDR addr = unpack_long (builtin_type_int, valaddr);
	if (TYPE_CODE (TYPE_TARGET_TYPE (type)) == TYPE_CODE_MEMBER)
	  {
	    print_class_member (valaddr,
				TYPE_DOMAIN_TYPE (TYPE_TARGET_TYPE (type)),
				stream, "");
	    break;
	  }
	if (addressprint)
	  {
	    fprintf_filtered (stream, "@0x%lx", addr);
	    if (deref_ref)
	      fputs_filtered (": ", stream);
	  }
	/* De-reference the reference.  */
	if (deref_ref)
	  {
	    if (target_read_memory(addr, (char *)&c, 1))
	     {
	       fprintf_filtered (stream, "<Address out of bounds>");
	       break;
	     }
	    if (TYPE_CODE (TYPE_TARGET_TYPE (type)) != TYPE_CODE_UNDEF)
	      {
		value deref_val = value_at (TYPE_TARGET_TYPE (type), addr);
		val_print (VALUE_TYPE (deref_val), VALUE_CONTENTS (deref_val),
			   VALUE_ADDRESS (deref_val), stream, format,
			   deref_ref, recurse + 1, pretty);
	      }
	    else
	      fputs_filtered ("???", stream);
	  }
      }
      break;

    case TYPE_CODE_UNION:
      if (recurse && !unionprint)
	{
	  fprintf_filtered (stream, "{...}");
	  break;
	}
      /* Fall through.  */
    case TYPE_CODE_STRUCT:
      if (vtblprint && is_vtbl_ptr_type(type))
	{
          /* Print the unmangled name if desired.  */
	  print_address_demangle(*((int *) (valaddr +	/* FIXME bytesex */
	      TYPE_FIELD_BITPOS (type, VTBL_FNADDR_OFFSET) / 8)),
	      stream, demangle);
	  break;
	}
      val_print_fields (type, valaddr, stream, format, recurse, pretty, 0);
      break;

    case TYPE_CODE_ENUM:
      if (format)
	{
	  print_scalar_formatted (valaddr, type, format, 0, stream);
	  break;
	}
      len = TYPE_NFIELDS (type);
      val = unpack_long (builtin_type_int, valaddr);
      for (i = 0; i < len; i++)
	{
	  QUIT;
	  if (val == TYPE_FIELD_BITPOS (type, i))
	    break;
	}
      if (i < len)
	fputs_filtered (TYPE_FIELD_NAME (type, i), stream);
      else
#ifdef LONG_LONG
	fprintf_filtered (stream, "%lld", val);
#else
	fprintf_filtered (stream, "%ld", val);
#endif
      break;

    case TYPE_CODE_FUNC:
      if (format)
	{
	  print_scalar_formatted (valaddr, type, format, 0, stream);
	  break;
	}
      /* FIXME, we should consider, at least for ANSI C language, eliminating
	 the distinction made between FUNCs and POINTERs to FUNCs.  */
      fprintf_filtered (stream, "{");
      type_print (type, "", stream, -1);
      fprintf_filtered (stream, "} ");
      /* Try to print what function it points to, and its address.  */
      print_address_demangle (address, stream, demangle);
      break;

    case TYPE_CODE_INT:
      if (format || output_format)
	{
	  print_scalar_formatted (valaddr, type,
				  format? format: output_format,
				  0, stream);
	  break;
	}
      if (TYPE_LENGTH (type) > sizeof (LONGEST))
	{
	  if (TYPE_UNSIGNED (type))
	    {
	      /* First figure out whether the number in fact has zeros
		 in all its bytes more significant than least significant
		 sizeof (LONGEST) ones.  */
	      char *p;
	      /* Pointer to first (i.e. lowest address) nonzero character.  */
	      char *first_addr;
	      len = TYPE_LENGTH (type);

#if TARGET_BYTE_ORDER == BIG_ENDIAN
	      for (p = valaddr;
		   len > sizeof (LONGEST)
		   && p < valaddr + TYPE_LENGTH (type);
		   p++)
#else /* Little endian.  */
	      first_addr = valaddr;
	      for (p = valaddr + TYPE_LENGTH (type);
		   len > sizeof (LONGEST) && p >= valaddr;
		   p--)
#endif /* Little endian.  */
		{
		  if (*p == 0)
		    len--;
		  else
		    break;
		}
#if TARGET_BYTE_ORDER == BIG_ENDIAN
	      first_addr = p;
#endif
	      
	      if (len <= sizeof (LONGEST))
		{
		  /* We can print it in decimal.  */
		  fprintf_filtered
		    (stream, 
#if defined (LONG_LONG)
		     "%llu",
#else
		     "%lu",
#endif
		     unpack_long (BUILTIN_TYPE_LONGEST, first_addr));
		}
	      else
		{
		  /* It is big, so print it in hex.  */
		  print_hex_chars (stream, (unsigned char *)first_addr, len);
		}
	    }
	  else
	    {
	      /* Signed.  One could assume two's complement (a reasonable
		 assumption, I think) and do better than this.  */
	      print_hex_chars (stream, (unsigned char *)valaddr,
			       TYPE_LENGTH (type));
	    }
	  break;
	}
#ifdef PRINT_TYPELESS_INTEGER
      PRINT_TYPELESS_INTEGER (stream, type, unpack_long (type, valaddr));
#else
      /* No format type is specified.  Just use /x for unsigned
	 and /d for signed. */
 
      print_scalar_formatted (valaddr, type, TYPE_UNSIGNED (type) ? 'x' : 'd',
			      0, stream);
#endif
			
      if (TYPE_LENGTH (type) == 1)
	{
	  fprintf_filtered (stream, " '");
	  printchar ((unsigned char) unpack_long (type, valaddr), 
		     stream, '\'');
	  fprintf_filtered (stream, "'");
	}
      break;

    case TYPE_CODE_FLT:
      if (format)
	print_scalar_formatted (valaddr, type, format, 0, stream);
      else
	print_floating (valaddr, type, stream);
      break;

    case TYPE_CODE_VOID:
      fprintf_filtered (stream, "void");
      break;

    case TYPE_CODE_UNDEF:
      /* This happens (without TYPE_FLAG_STUB set) on systems which don't use
	 dbx xrefs (NO_DBX_XREFS in gcc) if a file has a "struct foo *bar"
	 and no complete type for struct foo in that file.  */
      fprintf_filtered (stream, "<unknown struct>");
      break;

    case TYPE_CODE_ERROR:
      fprintf_filtered (stream, "?");
      break;

    case TYPE_CODE_RANGE:
      /* FIXME, we should not ever have to print one of these yet.  */
      fprintf_filtered (stream, "<range type>");
      break;

    default:
      error ("Invalid type code in symbol table.");
    }
  fflush (stream);
  return 0;
}

/* Print a description of a type in the format of a 
   typedef for the current language.
   NEW is the new name for a type TYPE. */
void
typedef_print (type, new, stream)
   struct type *type;
   struct symbol *new;
   FILE *stream;
{
   switch (current_language->la_language)
   {
#ifdef _LANG_c
   case language_c:
   case language_cplus:
      fprintf_filtered(stream, "typedef ");
      type_print(type,"",stream,0);
      if(TYPE_NAME ((SYMBOL_TYPE (new))) == 0
	 || 0 != strcmp (TYPE_NAME ((SYMBOL_TYPE (new))),
			 SYMBOL_NAME (new)))
	 fprintf_filtered(stream,  " %s", SYMBOL_NAME(new));
      break;
#endif
#ifdef _LANG_m2
   case language_m2:
      fprintf_filtered(stream, "TYPE ");
      if(!TYPE_NAME(SYMBOL_TYPE(new)) ||
	 strcmp (TYPE_NAME(SYMBOL_TYPE(new)),
		    SYMBOL_NAME(new)))
	 fprintf_filtered(stream, "%s = ", SYMBOL_NAME(new));
      else
	 fprintf_filtered(stream, "<builtin> = ");
      type_print(type,"",stream,0);
      break;
#endif
   default:
      error("Language not supported.");
   }
   fprintf_filtered(stream, ";\n");
}


/* Print a description of a type TYPE
   in the form of a declaration of a variable named VARSTRING.
   (VARSTRING is demangled if necessary.)
   Output goes to STREAM (via stdio).
   If SHOW is positive, we show the contents of the outermost level
   of structure even if there is a type name that could be used instead.
   If SHOW is negative, we never show the details of elements' types.  */

void
type_print (type, varstring, stream, show)
     struct type *type;
     char *varstring;
     FILE *stream;
     int show;
{
  type_print_1 (type, varstring, stream, show, 0);
}

/* LEVEL is the depth to indent lines by.  */

void
type_print_1 (type, varstring, stream, show, level)
     struct type *type;
     char *varstring;
     FILE *stream;
     int show;
     int level;
{
  register enum type_code code;
  char *demangled = NULL;
  int demangled_args;

  type_print_base (type, stream, show, level);
  code = TYPE_CODE (type);
  if ((varstring && *varstring)
      ||
      /* Need a space if going to print stars or brackets;
	 but not if we will print just a type name.  */
      ((show > 0 || TYPE_NAME (type) == 0)
       &&
       (code == TYPE_CODE_PTR || code == TYPE_CODE_FUNC
	|| code == TYPE_CODE_METHOD
	|| code == TYPE_CODE_ARRAY
	|| code == TYPE_CODE_MEMBER
	|| code == TYPE_CODE_REF)))
    fprintf_filtered (stream, " ");
  type_print_varspec_prefix (type, stream, show, 0);

  /* See if the name has a C++ demangled equivalent, and if so, print that
     instead. */

  if (demangle)
    {
      demangled = cplus_demangle (varstring, DMGL_ANSI | DMGL_PARAMS);
    }
  fputs_filtered ((demangled != NULL) ? demangled : varstring, stream);

  /* For demangled function names, we have the arglist as part of the name,
     so don't print an additional pair of ()'s */

  demangled_args = (demangled != NULL) && (code == TYPE_CODE_FUNC);
  type_print_varspec_suffix (type, stream, show, 0, demangled_args);

  if (demangled)
    {
      free (demangled);
    }
}

/* Print the method arguments ARGS to the file STREAM.  */
static void
type_print_method_args (args, prefix, varstring, staticp, stream)
     struct type **args;
     char *prefix, *varstring;
     int staticp;
     FILE *stream;
{
  int i;

  fputs_demangled (prefix, stream, DMGL_ANSI | DMGL_PARAMS);
  fputs_demangled (varstring, stream, DMGL_ANSI | DMGL_PARAMS);
  fputs_filtered (" (", stream);
  if (args && args[!staticp] && args[!staticp]->code != TYPE_CODE_VOID)
    {
      i = !staticp;		/* skip the class variable */
      while (1)
	{
	  type_print (args[i++], "", stream, 0);
	  if (!args[i]) 
	    {
	      fprintf_filtered (stream, " ...");
	      break;
	    }
	  else if (args[i]->code != TYPE_CODE_VOID)
	    {
	      fprintf_filtered (stream, ", ");
	    }
	  else break;
	}
    }
  fprintf_filtered (stream, ")");
}
  
/* If TYPE is a derived type, then print out derivation information.
   Print only the actual base classes of this type, not the base classes
   of the base classes.  I.E.  for the derivation hierarchy:

	class A { int a; };
	class B : public A {int b; };
	class C : public B {int c; };

   Print the type of class C as:

   	class C : public B {
		int c;
	}

   Not as the following (like gdb used to), which is not legal C++ syntax for
   derived types and may be confused with the multiple inheritance form:

	class C : public B : public A {
		int c;
	}

   In general, gdb should try to print the types as closely as possible to
   the form that they appear in the source code. */

static void
type_print_derivation_info (stream, type)
     FILE *stream;
     struct type *type;
{
  char *name;
  int i;

  for (i = 0; i < TYPE_N_BASECLASSES (type); i++)
    {
      fputs_filtered (i == 0 ? ": " : ", ", stream);
      fprintf_filtered (stream, "%s%s ",
			BASETYPE_VIA_PUBLIC (type, i) ? "public" : "private",
			BASETYPE_VIA_VIRTUAL(type, i) ? " virtual" : "");
      name = type_name_no_tag (TYPE_BASECLASS (type, i));
      fprintf_filtered (stream, "%s", name ? name : "(null)");
    }
  if (i > 0)
    {
      fputs_filtered (" ", stream);
    }
}

/* Print any asterisks or open-parentheses needed before the
   variable name (to describe its type).

   On outermost call, pass 0 for PASSED_A_PTR.
   On outermost call, SHOW > 0 means should ignore
   any typename for TYPE and show its details.
   SHOW is always zero on recursive calls.  */

static void
type_print_varspec_prefix (type, stream, show, passed_a_ptr)
     struct type *type;
     FILE *stream;
     int show;
     int passed_a_ptr;
{
  char *name;
  if (type == 0)
    return;

  if (TYPE_NAME (type) && show <= 0)
    return;

  QUIT;

  switch (TYPE_CODE (type))
    {
    case TYPE_CODE_PTR:
      type_print_varspec_prefix (TYPE_TARGET_TYPE (type), stream, 0, 1);
      fprintf_filtered (stream, "*");
      break;

    case TYPE_CODE_MEMBER:
      if (passed_a_ptr)
	fprintf_filtered (stream, "(");
      type_print_varspec_prefix (TYPE_TARGET_TYPE (type), stream, 0,
				 0);
      fprintf_filtered (stream, " ");
      name = type_name_no_tag (TYPE_DOMAIN_TYPE (type));
      if (name)
	fputs_filtered (name, stream);
      else
        type_print_base (TYPE_DOMAIN_TYPE (type), stream, 0, passed_a_ptr);
      fprintf_filtered (stream, "::");
      break;

    case TYPE_CODE_METHOD:
      if (passed_a_ptr)
	fprintf (stream, "(");
      type_print_varspec_prefix (TYPE_TARGET_TYPE (type), stream, 0,
				 0);
      if (passed_a_ptr)
	{
	  fprintf_filtered (stream, " ");
	  type_print_base (TYPE_DOMAIN_TYPE (type), stream, 0,
			   passed_a_ptr);
	  fprintf_filtered (stream, "::");
	}
      break;

    case TYPE_CODE_REF:
      type_print_varspec_prefix (TYPE_TARGET_TYPE (type), stream, 0, 1);
      fprintf_filtered (stream, "&");
      break;

    case TYPE_CODE_FUNC:
      type_print_varspec_prefix (TYPE_TARGET_TYPE (type), stream, 0,
				 0);
      if (passed_a_ptr)
	fprintf_filtered (stream, "(");
      break;

    case TYPE_CODE_ARRAY:
      type_print_varspec_prefix (TYPE_TARGET_TYPE (type), stream, 0,
				 0);
      if (passed_a_ptr)
	fprintf_filtered (stream, "(");
      break;

    case TYPE_CODE_UNDEF:
    case TYPE_CODE_STRUCT:
    case TYPE_CODE_UNION:
    case TYPE_CODE_ENUM:
    case TYPE_CODE_INT:
    case TYPE_CODE_FLT:
    case TYPE_CODE_VOID:
    case TYPE_CODE_ERROR:
    case TYPE_CODE_CHAR:
    case TYPE_CODE_BOOL:
    case TYPE_CODE_SET:
    case TYPE_CODE_RANGE:
    case TYPE_CODE_PASCAL_ARRAY:
      /* These types need no prefix.  They are listed here so that
	 gcc -Wall will reveal any types that haven't been handled.  */
      break;
    }
}

static void
type_print_args (type, stream)
     struct type *type;
     FILE *stream;
{
  int i;
  struct type **args;

  fprintf_filtered (stream, "(");
  args = TYPE_ARG_TYPES (type);
  if (args != NULL)
    {
      if (args[1] == NULL)
	{
	  fprintf_filtered (stream, "...");
	}
      else
	{
	  for (i = 1;
	       args[i] != NULL && args[i]->code != TYPE_CODE_VOID;
	       i++)
	    {
	      type_print_1 (args[i], "", stream, -1, 0);
	      if (args[i+1] == NULL)
		{
		  fprintf_filtered (stream, "...");
		}
	      else if (args[i+1]->code != TYPE_CODE_VOID)
		{
		  fprintf_filtered (stream, ",");
		  wrap_here ("    ");
		}
	    }
	}
    }
  fprintf_filtered (stream, ")");
}

/* Print any array sizes, function arguments or close parentheses
   needed after the variable name (to describe its type).
   Args work like type_print_varspec_prefix.  */

static void
type_print_varspec_suffix (type, stream, show, passed_a_ptr, demangled_args)
     struct type *type;
     FILE *stream;
     int show;
     int passed_a_ptr;
     int demangled_args;
{
  if (type == 0)
    return;

  if (TYPE_NAME (type) && show <= 0)
    return;

  QUIT;

  switch (TYPE_CODE (type))
    {
    case TYPE_CODE_ARRAY:
      if (passed_a_ptr)
	fprintf_filtered (stream, ")");
      
      fprintf_filtered (stream, "[");
      if (TYPE_LENGTH (type) > 0
	  && TYPE_LENGTH (TYPE_TARGET_TYPE (type)) > 0)
	fprintf_filtered (stream, "%d",
			  (TYPE_LENGTH (type)
			   / TYPE_LENGTH (TYPE_TARGET_TYPE (type))));
      fprintf_filtered (stream, "]");
      
      type_print_varspec_suffix (TYPE_TARGET_TYPE (type), stream, 0,
				 0, 0);
      break;

    case TYPE_CODE_MEMBER:
      if (passed_a_ptr)
	fprintf_filtered (stream, ")");
      type_print_varspec_suffix (TYPE_TARGET_TYPE (type), stream, 0, 0, 0);
      break;

    case TYPE_CODE_METHOD:
      if (passed_a_ptr)
	fprintf_filtered (stream, ")");
      type_print_varspec_suffix (TYPE_TARGET_TYPE (type), stream, 0, 0, 0);
      if (passed_a_ptr)
	{
	  type_print_args (type, stream);
	}
      break;

    case TYPE_CODE_PTR:
    case TYPE_CODE_REF:
      type_print_varspec_suffix (TYPE_TARGET_TYPE (type), stream, 0, 1, 0);
      break;

    case TYPE_CODE_FUNC:
      type_print_varspec_suffix (TYPE_TARGET_TYPE (type), stream, 0,
				 passed_a_ptr, 0);
      if (passed_a_ptr)
	fprintf_filtered (stream, ")");
      if (!demangled_args)
	fprintf_filtered (stream, "()");
      break;

    case TYPE_CODE_UNDEF:
    case TYPE_CODE_STRUCT:
    case TYPE_CODE_UNION:
    case TYPE_CODE_ENUM:
    case TYPE_CODE_INT:
    case TYPE_CODE_FLT:
    case TYPE_CODE_VOID:
    case TYPE_CODE_ERROR:
    case TYPE_CODE_CHAR:
    case TYPE_CODE_BOOL:
    case TYPE_CODE_SET:
    case TYPE_CODE_RANGE:
    case TYPE_CODE_PASCAL_ARRAY:
      /* These types do not need a suffix.  They are listed so that
	 gcc -Wall will report types that may not have been considered.  */
      break;
    }
}

/* Print the name of the type (or the ultimate pointer target,
   function value or array element), or the description of a
   structure or union.

   SHOW nonzero means don't print this type as just its name;
   show its real definition even if it has a name.
   SHOW zero means print just typename or struct tag if there is one
   SHOW negative means abbreviate structure elements.
   SHOW is decremented for printing of structure elements.

   LEVEL is the depth to indent by.
   We increase it for some recursive calls.  */

static void
type_print_base (type, stream, show, level)
     struct type *type;
     FILE *stream;
     int show;
     int level;
{
  char *name;
  register int i;
  register int len;
  register int lastval;
  char *mangled_name;
  char *demangled_name;
  enum {s_none, s_public, s_private, s_protected} section_type;
  QUIT;

  wrap_here ("    ");
  if (type == NULL)
    {
      fputs_filtered ("<type unknown>", stream);
      return;
    }

  /* When SHOW is zero or less, and there is a valid type name, then always
     just print the type name directly from the type. */

  if ((show <= 0) && (TYPE_NAME (type) != NULL))
    {
      fputs_filtered (TYPE_NAME (type), stream);
      return;
    }

  switch (TYPE_CODE (type))
    {
    case TYPE_CODE_ARRAY:
    case TYPE_CODE_PTR:
    case TYPE_CODE_MEMBER:
    case TYPE_CODE_REF:
    case TYPE_CODE_FUNC:
    case TYPE_CODE_METHOD:
      type_print_base (TYPE_TARGET_TYPE (type), stream, show, level);
      break;

    case TYPE_CODE_STRUCT:
      fprintf_filtered (stream,
			HAVE_CPLUS_STRUCT (type) ? "class " : "struct ");
      goto struct_union;

    case TYPE_CODE_UNION:
      fprintf_filtered (stream, "union ");
    struct_union:
      if (name = type_name_no_tag (type))
	{
	  fputs_filtered (name, stream);
	  fputs_filtered (" ", stream);
	  wrap_here ("    ");
	}
      if (show < 0)
	fprintf_filtered (stream, "{...}");
      else
	{
	  check_stub_type (type);
	  
	  type_print_derivation_info (stream, type);
	  
	  fprintf_filtered (stream, "{\n");
	  if ((TYPE_NFIELDS (type) == 0) && (TYPE_NFN_FIELDS (type) == 0))
	    {
	      if (TYPE_FLAGS (type) & TYPE_FLAG_STUB)
		fprintfi_filtered (level + 4, stream, "<incomplete type>\n");
	      else
		fprintfi_filtered (level + 4, stream, "<no data fields>\n");
	    }

	  /* Start off with no specific section type, so we can print
	     one for the first field we find, and use that section type
	     thereafter until we find another type. */

	  section_type = s_none;

	  /* If there is a base class for this type,
	     do not print the field that it occupies.  */

	  len = TYPE_NFIELDS (type);
	  for (i = TYPE_N_BASECLASSES (type); i < len; i++)
	    {
	      QUIT;
	      /* Don't print out virtual function table.  */
	      if ((TYPE_FIELD_NAME (type, i))[5] == CPLUS_MARKER &&
		  !strncmp (TYPE_FIELD_NAME (type, i), "_vptr", 5))
		continue;

	      /* If this is a C++ class we can print the various C++ section
		 labels. */

	      if (HAVE_CPLUS_STRUCT (type))
		{
		  if (TYPE_FIELD_PROTECTED (type, i))
		    {
		      if (section_type != s_protected)
			{
			  section_type = s_protected;
			  fprintfi_filtered (level + 2, stream,
					     "protected:\n");
			}
		    }
		  else if (TYPE_FIELD_PRIVATE (type, i))
		    {
		      if (section_type != s_private)
			{
			  section_type = s_private;
			  fprintfi_filtered (level + 2, stream, "private:\n");
			}
		    }
		  else
		    {
		      if (section_type != s_public)
			{
			  section_type = s_public;
			  fprintfi_filtered (level + 2, stream, "public:\n");
			}
		    }
		}

	      print_spaces_filtered (level + 4, stream);
	      if (TYPE_FIELD_STATIC (type, i))
		{
		  fprintf_filtered (stream, "static ");
		}
	      type_print_1 (TYPE_FIELD_TYPE (type, i),
			    TYPE_FIELD_NAME (type, i),
			    stream, show - 1, level + 4);
	      if (!TYPE_FIELD_STATIC (type, i)
		  && TYPE_FIELD_PACKED (type, i))
		{
		  /* It is a bitfield.  This code does not attempt
		     to look at the bitpos and reconstruct filler,
		     unnamed fields.  This would lead to misleading
		     results if the compiler does not put out fields
		     for such things (I don't know what it does).  */
		  fprintf_filtered (stream, " : %d",
				    TYPE_FIELD_BITSIZE (type, i));
		}
	      fprintf_filtered (stream, ";\n");
	    }

	  /* If there are both fields and methods, put a space between. */
	  len = TYPE_NFN_FIELDS (type);
	  if (len && section_type != s_none)
	     fprintf_filtered (stream, "\n");

	  /* C++: print out the methods */

	  for (i = 0; i < len; i++)
	    {
	      struct fn_field *f = TYPE_FN_FIELDLIST1 (type, i);
	      int j, len2 = TYPE_FN_FIELDLIST_LENGTH (type, i);
	      char *method_name = TYPE_FN_FIELDLIST_NAME (type, i);
	      int is_constructor = name && strcmp(method_name, name) == 0;
	      for (j = 0; j < len2; j++)
		{
		  QUIT;
		  if (TYPE_FN_FIELD_PROTECTED (f, j))
		    {
		      if (section_type != s_protected)
			{
			  section_type = s_protected;
			  fprintfi_filtered (level + 2, stream,
					     "protected:\n");
			}
		    }
		  else if (TYPE_FN_FIELD_PRIVATE (f, j))
		    {
		      if (section_type != s_private)
			{
			  section_type = s_private;
			  fprintfi_filtered (level + 2, stream, "private:\n");
			}
		    }
		  else
		    {
		      if (section_type != s_public)
			{
			  section_type = s_public;
			  fprintfi_filtered (level + 2, stream, "public:\n");
			}
		    }

		  print_spaces_filtered (level + 4, stream);
		  if (TYPE_FN_FIELD_VIRTUAL_P (f, j))
		    fprintf_filtered (stream, "virtual ");
		  else if (TYPE_FN_FIELD_STATIC_P (f, j))
		    fprintf_filtered (stream, "static ");
		  if (TYPE_TARGET_TYPE (TYPE_FN_FIELD_TYPE (f, j)) == 0)
		    {
		      /* Keep GDB from crashing here.  */
		      fprintf (stream, "<undefined type> %s;\n",
			       TYPE_FN_FIELD_PHYSNAME (f, j));
		      break;
		    }
		  else if (!is_constructor)
		    {
		      type_print (TYPE_TARGET_TYPE (TYPE_FN_FIELD_TYPE (f, j)),
				  "", stream, 0);
		      fputs_filtered (" ", stream);
		    }
		  if (TYPE_FN_FIELD_STUB (f, j))
		    {
		      /* Build something we can demangle.  */
		      mangled_name = gdb_mangle_name (type, i, j);
		      demangled_name =
			  cplus_demangle (mangled_name,
					  DMGL_ANSI | DMGL_PARAMS);
		      if (demangled_name == NULL)
			fprintf_filtered (stream, "<badly mangled name %s>",
			    mangled_name);
		      else 
			{
			  fprintf_filtered (stream, "%s",
			      strchr (demangled_name, ':') + 2);
			  free (demangled_name);
			}
		      free (mangled_name);
		    }
		  else if (TYPE_FN_FIELD_PHYSNAME (f, j)[0] == '_'
		        && TYPE_FN_FIELD_PHYSNAME (f, j)[1] == CPLUS_MARKER)
		    type_print_method_args
		      (TYPE_FN_FIELD_ARGS (f, j) + 1, "~",
		       method_name, 0, stream);
		  else
		    type_print_method_args
		      (TYPE_FN_FIELD_ARGS (f, j), "",
		       method_name,
		       TYPE_FN_FIELD_STATIC_P (f, j), stream);

		  fprintf_filtered (stream, ";\n");
		}
	    }

	  fprintfi_filtered (level, stream, "}");
	}
      break;

    case TYPE_CODE_ENUM:
      fprintf_filtered (stream, "enum ");
      if (name = type_name_no_tag (type))
	{
	  fputs_filtered (name, stream);
	  fputs_filtered (" ", stream);
	}
      wrap_here ("    ");
      if (show < 0)
	fprintf_filtered (stream, "{...}");
      else
	{
	  fprintf_filtered (stream, "{");
	  len = TYPE_NFIELDS (type);
	  lastval = 0;
	  for (i = 0; i < len; i++)
	    {
	      QUIT;
	      if (i) fprintf_filtered (stream, ", ");
	      wrap_here ("    ");
	      fputs_filtered (TYPE_FIELD_NAME (type, i), stream);
	      if (lastval != TYPE_FIELD_BITPOS (type, i))
		{
		  fprintf_filtered (stream, " = %d", TYPE_FIELD_BITPOS (type, i));
		  lastval = TYPE_FIELD_BITPOS (type, i);
		}
	      lastval++;
	    }
	  fprintf_filtered (stream, "}");
	}
      break;

    case TYPE_CODE_VOID:
      fprintf_filtered (stream, "void");
      break;

    case TYPE_CODE_UNDEF:
      fprintf_filtered (stream, "struct <unknown>");
      break;

    case TYPE_CODE_ERROR:
      fprintf_filtered (stream, "<unknown type>");
      break;

    case TYPE_CODE_RANGE:
      /* This should not occur */
      fprintf_filtered (stream, "<range type>");
      break;

    default:
      /* Handle types not explicitly handled by the other cases,
	 such as fundamental types.  For these, just print whatever
	 the type name is, as recorded in the type itself.  If there
	 is no type name, then complain. */
      if (TYPE_NAME (type) != NULL)
	{
	  fputs_filtered (TYPE_NAME (type), stream);
	}
      else
	{
	  error ("Invalid type code (%d) in symbol table.", TYPE_CODE (type));
	}
      break;
    }
}

#if 0
/* Validate an input or output radix setting, and make sure the user
   knows what they really did here.  Radix setting is confusing, e.g.
   setting the input radix to "10" never changes it!  */

/* ARGSUSED */
static void
set_input_radix (args, from_tty, c)
     char *args;
     int from_tty;
     struct cmd_list_element *c;
{
  unsigned radix = *(unsigned *)c->var;

  if (from_tty)
    printf_filtered ("Input radix set to decimal %d, hex %x, octal %o\n",
	radix, radix, radix);
}
#endif

/* ARGSUSED */
static void
set_output_radix (args, from_tty, c)
     char *args;
     int from_tty;
     struct cmd_list_element *c;
{
  unsigned radix = *(unsigned *)c->var;

  if (from_tty)
    printf_filtered ("Output radix set to decimal %d, hex %x, octal %o\n",
	radix, radix, radix);

  /* FIXME, we really should be able to validate the setting BEFORE
     it takes effect.  */
  switch (radix)
    {
    case 16:
      output_format = 'x';
      break;
    case 10:
      output_format = 0;
      break;
    case 8:
      output_format = 'o';		/* octal */
      break;
    default:
      output_format = 0;
      error ("Unsupported radix ``decimal %d''; using decimal output",
	      radix);
    }
}

/* Both at once */
static void
set_radix (arg, from_tty, c)
     char *arg;
     int from_tty;
     struct cmd_list_element *c;
{
  unsigned radix = *(unsigned *)c->var;

  if (from_tty)
    printf_filtered ("Radix set to decimal %d, hex %x, octal %o\n",
	radix, radix, radix);

  input_radix = radix;
  output_radix = radix;

  set_output_radix (arg, 0, c);
}

/*ARGSUSED*/
static void
set_print (arg, from_tty)
     char *arg;
     int from_tty;
{
  printf (
"\"set print\" must be followed by the name of a print subcommand.\n");
  help_list (setprintlist, "set print ", -1, stdout);
}

/*ARGSUSED*/
static void
show_print (args, from_tty)
     char *args;
     int from_tty;
{
  cmd_show_list (showprintlist, from_tty, "");
}

void
_initialize_valprint ()
{
  struct cmd_list_element *c;

  add_prefix_cmd ("print", no_class, set_print,
		  "Generic command for setting how things print.",
		  &setprintlist, "set print ", 0, &setlist);
  add_alias_cmd ("p", "print", no_class, 1, &setlist); 
  add_alias_cmd ("pr", "print", no_class, 1, &setlist); /* prefer set print
														   to     set prompt */
  add_prefix_cmd ("print", no_class, show_print,
		  "Generic command for showing print settings.",
		  &showprintlist, "show print ", 0, &showlist);
  add_alias_cmd ("p", "print", no_class, 1, &showlist); 
  add_alias_cmd ("pr", "print", no_class, 1, &showlist); 

  add_show_from_set
    (add_set_cmd ("elements", no_class, var_uinteger, (char *)&print_max,
		  "Set limit on string chars or array elements to print.\n\
\"set print elements 0\" causes there to be no limit.",
		  &setprintlist),
     &showprintlist);

  add_show_from_set
    (add_set_cmd ("pretty", class_support, var_boolean, (char *)&prettyprint,
		  "Set prettyprinting of structures.",
		  &setprintlist),
     &showprintlist);

  add_show_from_set
    (add_set_cmd ("union", class_support, var_boolean, (char *)&unionprint,
		  "Set printing of unions interior to structures.",
		  &setprintlist),
     &showprintlist);
  
  add_show_from_set
    (add_set_cmd ("vtbl", class_support, var_boolean, (char *)&vtblprint,
		  "Set printing of C++ virtual function tables.",
		  &setprintlist),
     &showprintlist);

  add_show_from_set
    (add_set_cmd ("array", class_support, var_boolean, (char *)&arrayprint,
		  "Set prettyprinting of arrays.",
		  &setprintlist),
     &showprintlist);

  add_show_from_set
    (add_set_cmd ("object", class_support, var_boolean, (char *)&objectprint,
	  "Set printing of object's derived type based on vtable info.",
		  &setprintlist),
     &showprintlist);

  add_show_from_set
    (add_set_cmd ("address", class_support, var_boolean, (char *)&addressprint,
		  "Set printing of addresses.",
		  &setprintlist),
     &showprintlist);

#if 0
  /* The "show radix" cmd isn't good enough to show two separate values.
     The rest of the code works, but the show part is confusing, so don't
     let them be set separately 'til we work out "show".  */
  c = add_set_cmd ("input-radix", class_support, var_uinteger,
		   (char *)&input_radix,
		  "Set default input radix for entering numbers.",
		  &setlist);
  add_show_from_set (c, &showlist);
  c->function = set_input_radix;

  c = add_set_cmd ("output-radix", class_support, var_uinteger,
		   (char *)&output_radix,
		  "Set default output radix for printing of values.",
		  &setlist);
  add_show_from_set (c, &showlist);
  c->function = set_output_radix;
#endif 

  c = add_set_cmd ("radix", class_support, var_uinteger,
		   (char *)&output_radix,
		  "Set default input and output number radix.",
		  &setlist);
  add_show_from_set (c, &showlist);
  c->function.sfunc = set_radix;

  /* Give people the defaults which they are used to.  */
  prettyprint = 0;
  unionprint = 1;
  vtblprint = 0;
  arrayprint = 0;
  addressprint = 1;
  objectprint = 0;

  print_max = 200;

  obstack_begin (&dont_print_obstack, 32 * sizeof (struct type *));
}
