/*-
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1991 by Donn Seeley at UUNET Technologies, Inc.
 */

#ifndef lint
static char sccsid[] = "@(#)cplus-lex.c	6.4 (Berkeley) 5/8/91";
#endif /* not lint */

/* Separate lexical analyzer for GNU C++.
   Copyright (C) 1987 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@mcc.com)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This file is the lexical analyzer for GNU C++.  */

#include <sys/types.h>
#include <stdio.h>
#include <errno.h>
#include <setjmp.h>
#include <string.h>
#include "config.h"
#include "input.h"
#include "tree.h"
#ifdef VMS
#define _IOFBF 2		/* Missing from GNU's stdio.h */
#endif
#include "cplus-tab.h"
#include "cplus-parse.h"
#include "cplus-tree.h"
#include "flags.h"
#include "obstack.h"
#include "assert.h"
extern int errno;		/* needed for VAX.  */
extern jmp_buf toplevel;

#ifdef VMS
#define	NULL_FILE	"nla0:"
#else
#define	NULL_FILE	"/dev/null"
#endif

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern int xmalloc ();
extern void free ();

extern double atof ();

/* If you don't have strrchr, but instead have rindex,
   add your machine to this list, and send mail to
   tiemann@wheaties.ai.mit.edu.  */
#if defined(sequent) || defined(convex)
#define strrchr rindex
#endif
extern char *strrchr ();

/* This obstack is needed to hold text.  It is not safe to use
   TOKEN_BUFFER because `check_newline' calls `yylex'.  */
static struct obstack inline_text_obstack;
static char *inline_text_firstobj;

/* Holds translations from TREE_CODEs to operator name strings,
   i.e., opname_tab[PLUS_EXPR] == "+".  */
char **opname_tab;
char **assignop_tab;

#define YYEMPTY		-1
int	yychar;			/*  the lookahead symbol		*/
YYSTYPE	yylval;			/*  the semantic value of the		*/
				/*  lookahead symbol			*/

#if 0
YYLTYPE yylloc;			/*  location data for the lookahead	*/
				/*  symbol				*/
#endif

int end_of_file;

/* the declaration found for the last IDENTIFIER token read in.
   yylex must look this up to detect typedefs, which get token type TYPENAME,
   so it is left around in case the identifier is not a typedef but is
   used in a context which makes it a reference to a variable.  */
tree lastiddecl;

/* C++ extensions */
tree ridpointers[];		/* need this up here */

/* We may keep statistics about how long which files took to compile.  */
static int header_time, body_time;
static tree get_time_identifier ();
static tree filename_times;
static tree this_filename_time;

/* For implementing #pragma unit.  */
tree current_unit_name;
tree current_unit_language;

/* Array for holding counts of the numbers of tokens seen.  */
int *token_count;

/* Return something to represent absolute declarators containing a *.
   TARGET is the absolute declarator that the * contains.
   TYPE_QUALS is a list of modifiers such as const or volatile
   to apply to the pointer type, represented as identifiers.

   We return an INDIRECT_REF whose "contents" are TARGET
   and whose type is the modifier list.  */

tree
make_pointer_declarator (type_quals, target)
     tree type_quals, target;
{
  if (target && TREE_CODE (target) == IDENTIFIER_NODE
      && ANON_AGGRNAME_P (target))
    error ("type name expected before `*'");
  return build1 (INDIRECT_REF, type_quals, target);
}

/* Return something to represent absolute declarators containing a &.
   TARGET is the absolute declarator that the & contains.
   TYPE_QUALS is a list of modifiers such as const or volatile
   to apply to the reference type, represented as identifiers.

   We return an ADDR_EXPR whose "contents" are TARGET
   and whose type is the modifier list.  */
   
tree
make_reference_declarator (type_quals, target)
     tree type_quals, target;
{
  if (target)
    {
      if (TREE_CODE (target) == ADDR_EXPR)
	{
	  error ("cannot declare references to references");
	  return target;
	}
      if (TREE_CODE (target) == INDIRECT_REF)
	{
	  error ("cannot declare pointers to references");
	  return target;
	}
      if (TREE_CODE (target) == IDENTIFIER_NODE && ANON_AGGRNAME_P (target))
	  error ("type name expected before `&'");
    }
  return build1 (ADDR_EXPR, type_quals, target);
}

/* Given a chain of STRING_CST nodes,
   concatenate them into one STRING_CST
   and give it a suitable array-of-chars data type.  */

tree
combine_strings (strings)
     tree strings;
{
  register tree value, t;
  register int length = 1;
  int wide_length = 0;
  int wide_flag = 0;

  if (TREE_CHAIN (strings))
    {
      /* More than one in the chain, so concatenate.  */
      register char *p, *q;

      /* Don't include the \0 at the end of each substring,
	 except for the last one.
	 Count wide strings and ordinary strings separately.  */
      for (t = strings; t; t = TREE_CHAIN (t))
	{
	  if (TREE_TYPE (t) == int_array_type_node)
	    {
	      wide_length += (TREE_STRING_LENGTH (t) - 1);
	      wide_flag = 1;
	    }
	  else
	    length += (TREE_STRING_LENGTH (t) - 1);
	}

      /* If anything is wide, the non-wides will be converted,
	 which makes them take more space.  */
      if (wide_flag)
	length = length * UNITS_PER_WORD + wide_length;

      p = (char *) savealloc (length);

      /* Copy the individual strings into the new combined string.
	 If the combined string is wide, convert the chars to ints
	 for any individual strings that are not wide.  */

      q = p;
      for (t = strings; t; t = TREE_CHAIN (t))
	{
	  int len = TREE_STRING_LENGTH (t) - 1;
	  if ((TREE_TYPE (t) == int_array_type_node) == wide_flag)
	    {
	      bcopy (TREE_STRING_POINTER (t), q, len);
	      q += len;
	    }
	  else
	    {
	      int i;
	      for (i = 0; i < len; i++)
		((int *) q)[i] = TREE_STRING_POINTER (t)[i];
	      q += len * UNITS_PER_WORD;
	    }
	}
      *q = 0;

      value = make_node (STRING_CST);
      TREE_STRING_POINTER (value) = p;
      TREE_STRING_LENGTH (value) = length;
      TREE_LITERAL (value) = 1;
    }
  else
    {
      value = strings;
      length = TREE_STRING_LENGTH (value);
      if (TREE_TYPE (value) == int_array_type_node)
	wide_flag = 1;
    }

  /* Create the array type for the string constant.
     -Wwrite-strings says make the string constant an array of const char
     so that copying it to a non-const pointer will get a warning.  */
  if (warn_write_strings)
    {
      tree elements
	= build_type_variant (wide_flag ? integer_type_node : char_type_node,
			      1, 0);
      TREE_TYPE (value)
	= build_array_type (elements,
			    build_index_type (build_int_2 (length - 1, 0)));
    }
  else
    TREE_TYPE (value)
      = build_array_type (wide_flag ? integer_type_node : char_type_node,
			  build_index_type (build_int_2 (length - 1, 0)));
  TREE_LITERAL (value) = 1;
  TREE_STATIC (value) = 1;
  return value;
}

/* Build names and nodes for overloaded operators.  */

/* Memoized table for operator names.  */
tree *node_table;

tree
build_opid (code1, code2)
     enum tree_code code1, code2;
{
  register tree t = make_node (OP_IDENTIFIER);
  register tree tmp;
  extern struct obstack *expression_obstack, permanent_obstack;
  struct obstack *ambient_obstack = expression_obstack;
  expression_obstack = &permanent_obstack;
  if (code1 != 0)
    {
      if ((tmp = node_table[(int)code1]) == 0)
	node_table[(int)code1] = tmp = make_node (code1);
      TREE_PURPOSE (t) = tmp;
    }
  if ((tmp = node_table[(int)code2]) == 0)
    node_table[(int)code2] = tmp = make_node (code2);
  TREE_VALUE (t) = tmp;
  expression_obstack = ambient_obstack;
  return t;
}

#ifdef __GNUC__
#define DEFTREECODE(SYM, NAME, TYPE, LEN) sizeof (NAME),
#else
#define DEFTREECODE(SYM, NAME, TYPE, LEN) -1,
#endif
static short opname_end[] = {
#include "tree.def"
7,				/* sizeof ("@@dummy"), */
#include "cplus-tree.def"
};
#undef DEFTREECODE

/* Given a TOKEN and its estimated tree code CODE, produce a name which
   can be recognized by lookup_name.  Based on the number of PARMS,
   build an appropriate operator fnname.  This function is needed because
   until we know how many parameters we have, we cannot reliably tell
   what function indeed we are trying to declare.

   NPARMS is the number of additional parameters that this operator
   will ultimately have.  If NPARMS == -1, then we are just building
   a name, and should not complain.

   This would be a good candidate for memoizing.  */
tree
build_operator_fnname (declp, parms, nparms)
     tree *declp;
     tree parms;
     int nparms;
{
  tree decl = *declp;
  char **opname_table, *opname;
  int assignop_p = 0;
  tree rval;
  enum tree_code code;
  char buf[1024];
  int saw_class = nparms;
  int erred = 0;

  while (parms)
    {
      tree type;
      if (parms == void_list_node)
	break;

      if (! saw_class)
	{
	  type = TREE_VALUE (parms);
	  if (TREE_CODE (type) == REFERENCE_TYPE)
	    type = TREE_TYPE (type);
	  if (TREE_CODE (type) == POINTER_TYPE)
	    type = TREE_TYPE (type);
	  if (IS_AGGR_TYPE (type))
	    saw_class = 1;
	}
      nparms++;
      parms = TREE_CHAIN (parms);
    }

  if (TREE_CODE (decl) == TYPE_EXPR)
    {
      /* @@ may need to perform type instantiation here.  */
      if (nparms > 1)
	error ("wrong number of arguments to type conversion operator");

      /* The grammar will swallow an "()" if one was given.
	 We attempt to correct for this lossage here.  */
      if (TREE_OPERAND (decl, 0)
	  && TREE_CODE (TREE_OPERAND (decl, 0)) == CALL_EXPR)
	{
	  rval = build_typename_overload (groktypename (build_tree_list (TREE_TYPE (decl), NULL_TREE)));
	  yychar = LEFT_RIGHT;
	}
      else
	{
	  rval = build_typename_overload (groktypename (build_tree_list (TREE_TYPE (decl), TREE_OPERAND (decl, 0))));
	}
      return rval;
    }

  if (TREE_PURPOSE (decl))
    if (TREE_CODE (TREE_PURPOSE (decl)) == MODIFY_EXPR)
      {
	opname_table = assignop_tab;
	assignop_p = 1;
      }
    else
      abort ();
  else
    opname_table = opname_tab;

  code = TREE_CODE (TREE_VALUE (decl));
  opname = opname_table[(int) code];

  if (assignop_p)
    {
      if (nparms == 1 || nparms > 2)
	error ("wrong number of parameters op `operator %s'", opname);
    }
  else switch (code)
    {
    case ERROR_MARK:
      rval = get_identifier ("<invalid operator>");
      TREE_OVERLOADED (rval) = 1;
      return rval;

      /* AC/DC */
    case PLUS_EXPR:
      if (nparms == 1)
	code = CONVERT_EXPR;
      else if (nparms != 2)
	erred = 1;
      break;

    case ADDR_EXPR:
    case BIT_AND_EXPR:
      if (nparms == 1)
	code = ADDR_EXPR;
      else if (nparms == 2)
	code = BIT_AND_EXPR;
      else
	{
	  code = BIT_AND_EXPR;
	  erred = 1;
	}
      break;

    case MULT_EXPR:
    case INDIRECT_REF:
      if (nparms == 1)
	code = INDIRECT_REF;
      else if (nparms == 2)
	code = MULT_EXPR;
      else
	{
	  code = MULT_EXPR;
	  erred = 1;
	}
      break;

    case MINUS_EXPR:
    case NEGATE_EXPR:
      if (nparms == 1)
	code = NEGATE_EXPR;
      else if (nparms == 2)
	code = MINUS_EXPR;
      else
	{
	  code = MINUS_EXPR;
	  erred = 1;
	}
      break;

    case POINTSAT:
      if (nparms == 1 || nparms < 0)
	code = COMPONENT_REF;
      else
	{
	  erred = -1;
	  error ("wrong number of parameters to `operator ->()'");
	}
      break;

    case METHOD_CALL_EXPR:
      switch (nparms)
	{
	case 0:
	case 1:
	  erred = -1;
	  error ("too few arguments to `operator ->()(...)'");
	  break;
	  /* 4 happens when we pass in the canonical number
	     of arguments.  */
	case 4:
	  nparms = 3;
	case -1:
	case 2:
	case 3:
	  break;
	default:
	  erred = -1;
	  error ("too many arguments to `operator ->()(...)'");
	  break;
	}
      break;

      /* The two following entrys are for two different ways of
	 encoding `operator ='.  */
    case NOP_EXPR:
      if (nparms != 2 && nparms >= 0)
	erred = 1;
      break;

    case MODIFY_EXPR:
      if (nparms != 2 && nparms >= 0)
	erred = 1;
      break;

    case NEW_EXPR:
      if (saw_class == 0)
	{
	  if (nparms > 1)
	    return get_identifier (OPERATOR_NEW_FORMAT);
	  return get_identifier ("__builtin_new");
	}
      break;

    case DELETE_EXPR:
      if (saw_class == 0)
	{
	  if (nparms > 1)
	    error ("too many parameters to `operator ::delete'");
	  return get_identifier ("__builtin_delete");
	}
      if (nparms > 2)
	erred = 1;
      break;

      /* Whatever it was, we know its arity.  Just check that it
         has the right number of parameters defined.  */
    default:
      /* These are the only operators which do not need
	 to have a class-type associated with them.  */

      if (code == PREDECREMENT_EXPR
	  || code == POSTINCREMENT_EXPR
	  || code == COMPONENT_REF)
	{
	  if (nparms > 1)
	    erred = 1;
	}
      else if (nparms < 0
	       || code == CALL_EXPR
	       || code == METHOD_CALL_EXPR)
	;
      else if (nparms != tree_code_length [(int) code])
	erred = 1;
      break;
    }

  if (erred > 0)
    error ("wrong number of parameters to `operator %s'", opname);
  else if (erred == 0 && code != TREE_CODE (TREE_VALUE (decl)))
    {
      enum tree_code assign_code = ERROR_MARK;
      if (TREE_PURPOSE (decl))
	assign_code = TREE_CODE (TREE_PURPOSE (decl));
      decl = build_opid (assign_code, code);
      *declp = decl;
    }

  if (! saw_class)
    error ("`operator %s' must have at least one class type", opname);

  if (assignop_p)
    {
      sprintf (buf, OPERATOR_ASSIGN_FORMAT, tree_code_name [(int) code]);
      buf[opname_end[(int) code] + sizeof (OPERATOR_ASSIGN_FORMAT) - 3] = '\0';
    }
  else
    {
      sprintf (buf, OPERATOR_FORMAT, tree_code_name [(int) code]);
      buf[opname_end[(int) code] + sizeof (OPERATOR_FORMAT) - 3] = '\0';
    }      
  rval = get_identifier (buf);
  TREE_OVERLOADED (rval) = 1;
  return rval;
}

char *
operator_name_string (name)
     tree name;
{
  char *opname = IDENTIFIER_POINTER (name)
    + sizeof (OPERATOR_FORMAT) - sizeof ("%s");
  int i, assign;

  /* Works for builtin and user defined types.  */
  if (IDENTIFIER_GLOBAL_VALUE (name)
      && TREE_CODE (IDENTIFIER_GLOBAL_VALUE (name)) == TYPE_DECL)
    return IDENTIFIER_POINTER (name);

  if (! strncmp (opname, "assign", 6))
    {
      opname += 7;
      assign = 1;
    }
  else
    assign = 0;

  for (i = 0; i < LAST_CPLUS_TREE_CODE; i++)
    {
      if (! strncmp (opname, tree_code_name[i], opname_end[i]))
	break;
    }

  assert (i != LAST_CPLUS_TREE_CODE);

  if (assign)
    return assignop_tab[i];
  else
    return opname_tab[i];
}

int lineno;			/* current line number in file being read */

FILE *finput;			/* input file.
				   Normally a pipe from the preprocessor.  */
static FILE *finput1;		/* Real input files: 1 is main input file */
static FILE *finput2;		/* 2 is input file for inline functions */

int interface_only;		/* whether or not current file is only for
				   interface definitions.  */
int interface_unknown;		/* whether or not we know this class
				   to behave according to #pragma interface.  */

/* lexical analyzer */

static int maxtoken;		/* Current nominal length of token buffer.  */
char *token_buffer;		/* Pointer to token buffer.
				   Actual allocated length is maxtoken + 2.  */
static int max_wide;		/* Current nominal length of wide_buffer.  */
static int *wide_buffer;	/* Pointer to wide-string buffer.
				   Actual allocated length is max_wide + 1.  */

#define NORID RID_UNUSED

/* Command-line: gperf -p -j1 -g -o -t -N is_reserved_word -k1,4,$ gplus.gperf  */
struct resword { char *name; short token; enum rid rid;};

#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 13
#define MIN_HASH_VALUE 4
#define MAX_HASH_VALUE 147
/*
   71 keywords
  144 is the maximum key range
*/

#ifdef __GNUC__
inline
#endif
static int
hash (str, len)
     register char *str;
     register int unsigned len;
{
  static unsigned char hash_table[] =
    {
     147, 147, 147, 147, 147, 147, 147, 147, 147, 147,
     147, 147, 147, 147, 147, 147, 147, 147, 147, 147,
     147, 147, 147, 147, 147, 147, 147, 147, 147, 147,
     147, 147, 147, 147, 147, 147, 147, 147, 147, 147,
     147, 147, 147, 147, 147, 147, 147, 147, 147, 147,
     147, 147, 147, 147, 147, 147, 147, 147, 147, 147,
     147, 147, 147, 147, 147, 147, 147, 147, 147, 147,
     147, 147, 147, 147, 147, 147, 147, 147, 147, 147,
     147, 147, 147, 147, 147, 147, 147, 147, 147, 147,
     147, 147, 147, 147, 147,   0, 147,  19,   6,  27,
      37,   0,  12,   1,  15,  63, 147,   4,   0,  56,
      20,  15,  42, 147,  31,   5,  26,  39,  32,  10,
     147,  40, 147, 147, 147, 147, 147, 147,
    };
  register int hval = len ;

  switch (hval)
    {
      default:
      case 4:
        hval += hash_table[str[3]];
      case 3:
      case 2:
      case 1:
        hval += hash_table[str[0]];
    }
  return hval + hash_table[str[len - 1]] ;
}

#ifdef __GNUC__
inline
#endif
struct resword *
is_reserved_word (str, len)
     register char *str;
     register unsigned int len;
{

  static struct resword  wordlist[] =
    {
      {"",}, {"",}, {"",}, {"",}, 
      {"else",  ELSE, NORID,},
      {"",}, 
      {"long",  TYPESPEC, RID_LONG,},
      {"",}, {"",}, {"",}, {"",}, 
      {"__alignof__",  ALIGNOF, NORID},
      {"__asm__",  ASM, NORID},
      {"",}, {"",}, 
      {"while",  WHILE, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"__alignof",  ALIGNOF, NORID},
      {"all",  ALL, NORID			/* Extension */,},
      {"sizeof",  SIZEOF, NORID,},
      {"__const__",  TYPE_QUAL, RID_CONST},
      {"__volatile",  TYPE_QUAL, RID_VOLATILE},
      {"extern",  SCSPEC, RID_EXTERN,},
      {"__volatile__",  TYPE_QUAL, RID_VOLATILE},
      {"__inline",  SCSPEC, RID_INLINE},
      {"exception",  AGGR, RID_EXCEPTION	/* Extension */,},
      {"__inline__",  SCSPEC, RID_INLINE},
      {"case",  CASE, NORID,},
      {"except",  EXCEPT, NORID		/* Extension */,},
      {"new",  NEW, NORID,},
      {"break",  BREAK, NORID,},
      {"goto",  GOTO, NORID,},
      {"",}, 
      {"__attribute",  ATTRIBUTE, NORID},
      {"",}, 
      {"__attribute__",  ATTRIBUTE, NORID},
      {"this",  THIS, NORID,},
      {"raise",  RAISE, NORID		/* Extension */,},
      {"class",  AGGR, RID_CLASS,},
      {"delete",  DELETE, NORID,},
      {"typeof",  TYPEOF, NORID,},
      {"typedef",  SCSPEC, RID_TYPEDEF,},
      {"for",  FOR, NORID,},
      {"raises",  RAISES, NORID		/* Extension */,},
      {"__const",  TYPE_QUAL, RID_CONST},
      {"double",  TYPESPEC, RID_DOUBLE,},
      {"__typeof__",  TYPEOF, NORID},
      {"",}, 
      {"switch",  SWITCH, NORID,},
      {"auto",  SCSPEC, RID_AUTO,},
      {"do",  DO, NORID,},
      {"friend",  SCSPEC, RID_FRIEND,},
      {"",}, 
      {"reraise",  RERAISE, NORID		/* Extension */,},
      {"",}, 
      {"volatile",  TYPE_QUAL, RID_VOLATILE,},
      {"__typeof",  TYPEOF, NORID},
      {"continue",  CONTINUE, NORID,},
      {"float",  TYPESPEC, RID_FLOAT,},
      {"const",  TYPE_QUAL, RID_CONST,},
      {"static",  SCSPEC, RID_STATIC,},
      {"virtual",  SCSPEC, RID_VIRTUAL,},
      {"__asm",  ASM, NORID},
      {"short",  TYPESPEC, RID_SHORT,},
      {"signed",  TYPESPEC, RID_SIGNED,},
      {"try",  TRY, NORID			/* Extension */,},
      {"",}, {"",}, {"",}, 
      {"__signed__",  TYPESPEC, RID_SIGNED},
      {"catch",  CATCH, NORID,},
      {"public",  PUBLIC, NORID,},
      {"struct",  AGGR, RID_RECORD,},
      {"if",  IF, NORID,},
      {"asm",  ASM, NORID,},
      {"union",  AGGR, RID_UNION,},
      {"",}, 
      {"private",  PRIVATE, NORID,},
      {"",}, {"",}, {"",}, 
      {"operator",  OPERATOR, NORID,},
      {"",}, {"",}, {"",}, 
      {"default",  DEFAULT, NORID,},
      {"dynamic",  DYNAMIC, NORID,},
      {"overload",  OVERLOAD, NORID,},
      {"int",  TYPESPEC, RID_INT,},
      {"char",  TYPESPEC, RID_CHAR,},
      {"",}, {"",}, 
      {"return",  RETURN, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, 
      {"__signed",  TYPESPEC, RID_SIGNED},
      {"",}, 
      {"void",  TYPESPEC, RID_VOID,},
      {"",}, {"",}, {"",}, 
      {"protected",  PROTECTED, NORID,},
      {"",}, 
      {"enum",  ENUM, NORID,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"inline",  SCSPEC, RID_INLINE,},
      {"register",  SCSPEC, RID_REGISTER,},
      {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, {"",}, 
      {"",}, {"",}, {"",}, {"",}, 
      {"unsigned",  TYPESPEC, RID_UNSIGNED,},
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= MIN_HASH_VALUE)
        {
          register char *s = wordlist[key].name;

          if (*s == *str && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}

/* The elements of `ridpointers' are identifier nodes
   for the reserved type names and storage classes.
   It is indexed by a RID_... value.  */

tree ridpointers[(int) RID_MAX];

int check_newline ();

static int skip_white_space ();

static tree
get_time_identifier (name)
     char *name;
{
  tree time_identifier;
  int len = strlen (name);
  char *buf = (char *)alloca (len + 6);
  strcpy (buf, "file ");
  bcopy (name, buf+5, len);
  buf[len+5] = '\0';
  time_identifier = get_identifier (buf);
  if (IDENTIFIER_LOCAL_VALUE (time_identifier) == NULL_TREE)
    {
      int temp = allocation_temporary_p ();
      if (temp)
	end_temporary_allocation ();
      IDENTIFIER_LOCAL_VALUE (time_identifier) = build_int_2 (0, 0);
      IDENTIFIER_CLASS_VALUE (time_identifier) = build_int_2 (0, 1);
      IDENTIFIER_GLOBAL_VALUE (time_identifier) = filename_times;
      filename_times = time_identifier;
      if (temp)
	resume_temporary_allocation ();
    }
  return time_identifier;
}

#ifdef __GNUC__
__inline
#endif
static int
my_gettime ()
{
  int old_quiet_flag = quiet_flag;
  int this_time;
  quiet_flag = 0;
  this_time = gettime ();
  quiet_flag = old_quiet_flag;
  return this_time;
}

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r and e.  See cplus-tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

char *cplus_tree_code_type[] = {
  "x",
#include "cplus-tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

int cplus_tree_code_length[] = {
  0,
#include "cplus-tree.def"
};
#undef DEFTREECODE

/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

char *cplus_tree_code_name[] = {
  "@@dummy",
#include "cplus-tree.def"
};
#undef DEFTREECODE

void
init_filename_times ()
{
  this_filename_time = get_time_identifier ("<top level>");
  if (flag_detailed_statistics)
    {
      header_time = 0;
      body_time = my_gettime ();
      TREE_INT_CST_LOW (IDENTIFIER_LOCAL_VALUE (this_filename_time)) = body_time;
    }
}

/* Use 4.4 BSD fropen() stdio function to fake re-reads of inline functions.
   We just decrement the buffer count until it reaches zero.  */

static unsigned next_inline_count = 0;

static int
read_next_inline(unused_cookie, unused_buf, count)
     void *unused_cookie;
     char *unused_buf;
     unsigned count;
{
  count = count > next_inline_count ? next_inline_count : count;
  next_inline_count -= count;
  return count;
}

/* Change by Bryan Boreham, Kewill, Thu Jul 27 09:46:05 1989.
   Stuck this hack in to get the files open correctly; this is called
   in place of init_lex if we are an unexec'd binary.    */
void
reinit_lex_for_unexec ()
{
  finput1 = finput;
  finput2 = fropen ((void *) 0, read_next_inline);
  init_filename_times ();
}

void
init_lex ()
{
  extern int *init_parse ();
  extern char *(*decl_printable_name) ();
  extern char *lang_printable_name ();
  extern struct rtx_def *(*lang_expand_expr) ();
  extern struct rtx_def *cplus_expand_expr ();

  int i;

  /* Make identifier nodes long enough for the language-specific slots.  */
  set_identifier_size (sizeof (struct lang_identifier));
  decl_printable_name = lang_printable_name;
  lang_expand_expr = cplus_expand_expr;

  tree_code_type
    = (char **) realloc (tree_code_type,
			 sizeof (char *) * LAST_CPLUS_TREE_CODE);
  tree_code_length
    = (int *) realloc (tree_code_length,
		       sizeof (int) * LAST_CPLUS_TREE_CODE);
  tree_code_name
    = (char **) realloc (tree_code_name,
			 sizeof (char *) * LAST_CPLUS_TREE_CODE);
  bcopy (cplus_tree_code_type,
	 tree_code_type + LAST_AND_UNUSED_TREE_CODE,
	 (LAST_CPLUS_TREE_CODE - LAST_AND_UNUSED_TREE_CODE) * sizeof (char *));
  bcopy (cplus_tree_code_length,
	 tree_code_length + LAST_AND_UNUSED_TREE_CODE,
	 (LAST_CPLUS_TREE_CODE - LAST_AND_UNUSED_TREE_CODE) * sizeof (int));
  bcopy (cplus_tree_code_name,
	 tree_code_name + LAST_AND_UNUSED_TREE_CODE,
	 (LAST_CPLUS_TREE_CODE - LAST_AND_UNUSED_TREE_CODE) * sizeof (char *));

  node_table = (tree *)oballoc (LAST_CPLUS_TREE_CODE * sizeof (tree));
  opname_tab = (char **)oballoc (LAST_CPLUS_TREE_CODE * sizeof (char *));
  assignop_tab = (char **)oballoc (LAST_CPLUS_TREE_CODE * sizeof (char *));

  for (i = 0; i < LAST_CPLUS_TREE_CODE; i++)
    /* Our only interest is _ref and _expr.  */
    if (tree_code_type[i][0] == 'r' || tree_code_type[i][0] == 'e')
      {
	char *end = (char *)strrchr (tree_code_name[i], '_');
	if (end)
	  opname_end[i] = end - tree_code_name[i];
#ifndef __GNUC__
	else
	  opname_end[i] = strlen (tree_code_name[i]);
#endif
      }
#ifndef __GNUC__
    else
      opname_end[i] = strlen (tree_code_name[i]);
#endif

  init_method ();
  obstack_init (&inline_text_obstack);
  inline_text_firstobj = (char *) obstack_alloc (&inline_text_obstack, 0);

  /* Start it at 0, because check_newline is called at the very beginning
     and will increment it to 1.  */
  lineno = 0;
  finput1 = finput;
  finput2 = fropen ((void *) 0, read_next_inline);
  current_function_decl = NULL;

  maxtoken = 40;
  token_buffer = (char *) xmalloc (maxtoken + 2);
  max_wide = 40;
  wide_buffer = (int *) xmalloc (max_wide + 1);

  ridpointers[(int) RID_INT] = get_identifier ("int");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_INT],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_INT]));
  ridpointers[(int) RID_CHAR] = get_identifier ("char");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_CHAR],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_CHAR]));
  ridpointers[(int) RID_VOID] = get_identifier ("void");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_VOID],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_VOID]));
  ridpointers[(int) RID_FLOAT] = get_identifier ("float");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_FLOAT],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_FLOAT]));
  ridpointers[(int) RID_DOUBLE] = get_identifier ("double");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_DOUBLE],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_DOUBLE]));
  ridpointers[(int) RID_SHORT] = get_identifier ("short");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_SHORT],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_SHORT]));
  ridpointers[(int) RID_LONG] = get_identifier ("long");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_LONG],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_LONG]));
  ridpointers[(int) RID_UNSIGNED] = get_identifier ("unsigned");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_UNSIGNED],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_UNSIGNED]));
  ridpointers[(int) RID_SIGNED] = get_identifier ("signed");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_SIGNED],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_SIGNED]));
  ridpointers[(int) RID_INLINE] = get_identifier ("inline");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_INLINE],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_INLINE]));
  ridpointers[(int) RID_CONST] = get_identifier ("const");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_CONST],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_CONST]));
  ridpointers[(int) RID_VOLATILE] = get_identifier ("volatile");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_VOLATILE],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_VOLATILE]));
  ridpointers[(int) RID_AUTO] = get_identifier ("auto");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_AUTO],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_AUTO]));
  ridpointers[(int) RID_STATIC] = get_identifier ("static");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_STATIC],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_STATIC]));
  ridpointers[(int) RID_EXTERN] = get_identifier ("extern");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_EXTERN],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_EXTERN]));
  ridpointers[(int) RID_TYPEDEF] = get_identifier ("typedef");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_TYPEDEF],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_TYPEDEF]));
  ridpointers[(int) RID_REGISTER] = get_identifier ("register");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_REGISTER],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_REGISTER]));

  /* C++ extensions. These are probably not correctly named. */
  class_type_node = build_int_2 (class_type, 0);
  TREE_TYPE (class_type_node) = class_type_node;
  ridpointers[(int) RID_CLASS] = class_type_node;

  record_type_node = build_int_2 (record_type, 0);
  TREE_TYPE (record_type_node) = record_type_node;
  ridpointers[(int) RID_RECORD] = record_type_node;

  union_type_node = build_int_2 (union_type, 0);
  TREE_TYPE (union_type_node) = union_type_node;
  ridpointers[(int) RID_UNION] = union_type_node;

  enum_type_node = build_int_2 (enum_type, 0);
  TREE_TYPE (enum_type_node) = enum_type_node;
  ridpointers[(int) RID_ENUM] = enum_type_node;

  ridpointers[(int) RID_VIRTUAL] = get_identifier ("virtual");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_VIRTUAL],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_VIRTUAL]));
  ridpointers[(int) RID_FRIEND] = get_identifier ("friend");
  SET_IDENTIFIER_AS_LIST (ridpointers[(int) RID_FRIEND],
			  build_tree_list (NULL_TREE, ridpointers[(int) RID_FRIEND]));

  /* Exception handling extensions.  */
  exception_type_node = build_int_2 (exception_type, 0);
  TREE_TYPE (exception_type_node) = exception_type_node;
  ridpointers[(int) RID_EXCEPTION] = exception_type_node;

  opname_tab[(int) COMPONENT_REF] = "->";
  opname_tab[(int) METHOD_CALL_EXPR] = "->()";
  opname_tab[(int) INDIRECT_REF] = "(unary *)";
  opname_tab[(int) ARRAY_REF] = "[]";
  opname_tab[(int) MODIFY_EXPR] = "=";
  opname_tab[(int) NEW_EXPR] = "new";
  opname_tab[(int) DELETE_EXPR] = "delete";
  opname_tab[(int) COND_EXPR] = "... ? ... : ...";
  opname_tab[(int) CALL_EXPR] = "()";
  opname_tab[(int) PLUS_EXPR] = "+";
  opname_tab[(int) MINUS_EXPR] = "-";
  opname_tab[(int) MULT_EXPR] = "*";
  opname_tab[(int) TRUNC_DIV_EXPR] = "/";
  opname_tab[(int) CEIL_DIV_EXPR] = "(ceiling /)";
  opname_tab[(int) FLOOR_DIV_EXPR] = "(floor /)";
  opname_tab[(int) ROUND_DIV_EXPR] = "(round /)";
  opname_tab[(int) TRUNC_MOD_EXPR] = "%";
  opname_tab[(int) CEIL_MOD_EXPR] = "(ceiling %)";
  opname_tab[(int) FLOOR_MOD_EXPR] = "(floor %)";
  opname_tab[(int) ROUND_MOD_EXPR] = "(round %)";
  opname_tab[(int) NEGATE_EXPR] = "-";
  opname_tab[(int) MIN_EXPR] = "<?";
  opname_tab[(int) MAX_EXPR] = ">?";
  opname_tab[(int) ABS_EXPR] = "abs";
  opname_tab[(int) FFS_EXPR] = "ffs";
  opname_tab[(int) LSHIFT_EXPR] = "<<";
  opname_tab[(int) RSHIFT_EXPR] = ">>";
  opname_tab[(int) BIT_IOR_EXPR] = "|";
  opname_tab[(int) BIT_XOR_EXPR] = "^";
  opname_tab[(int) BIT_AND_EXPR] = "&";
  opname_tab[(int) BIT_ANDTC_EXPR] = "&~";
  opname_tab[(int) BIT_NOT_EXPR] = "~";
  opname_tab[(int) TRUTH_ANDIF_EXPR] = "&&";
  opname_tab[(int) TRUTH_ORIF_EXPR] = "||";
  opname_tab[(int) TRUTH_AND_EXPR] = "strict &&";
  opname_tab[(int) TRUTH_OR_EXPR] = "strict ||";
  opname_tab[(int) TRUTH_NOT_EXPR] = "!";
  opname_tab[(int) LT_EXPR] = "<";
  opname_tab[(int) LE_EXPR] = "<=";
  opname_tab[(int) GT_EXPR] = ">";
  opname_tab[(int) GE_EXPR] = ">=";
  opname_tab[(int) EQ_EXPR] = "==";
  opname_tab[(int) NE_EXPR] = "!=";
  opname_tab[(int) IN_EXPR] = "in";
  opname_tab[(int) SET_LE_EXPR] = "subset";
  opname_tab[(int) CARD_EXPR] = "#";
  opname_tab[(int) RANGE_EXPR] = "..";
  opname_tab[(int) CONVERT_EXPR] = "(unary +)";
  opname_tab[(int) ADDR_EXPR] = "(unary &)";
  opname_tab[(int) PREDECREMENT_EXPR] = "--";
  opname_tab[(int) PREINCREMENT_EXPR] = "++";
  opname_tab[(int) POSTDECREMENT_EXPR] = "--";
  opname_tab[(int) POSTINCREMENT_EXPR] = "++";
  opname_tab[(int) COMPOUND_EXPR] = ",";
  assignop_tab[(int) NOP_EXPR] = "=";
  assignop_tab[(int) PLUS_EXPR] =  "+=";
  assignop_tab[(int) MINUS_EXPR] = "-=";
  assignop_tab[(int) MULT_EXPR] = "*=";
  assignop_tab[(int) TRUNC_DIV_EXPR] = "/=";
  assignop_tab[(int) CEIL_DIV_EXPR] = "(ceiling /=)";
  assignop_tab[(int) FLOOR_DIV_EXPR] = "(floor /=)";
  assignop_tab[(int) ROUND_DIV_EXPR] = "(round /=)";
  assignop_tab[(int) TRUNC_MOD_EXPR] = "%=";
  assignop_tab[(int) CEIL_MOD_EXPR] = "(ceiling %=)";
  assignop_tab[(int) FLOOR_MOD_EXPR] = "(floor %=)";
  assignop_tab[(int) ROUND_MOD_EXPR] = "(round %=)";
  assignop_tab[(int) MIN_EXPR] = "<?=";
  assignop_tab[(int) MAX_EXPR] = ">?=";
  assignop_tab[(int) LSHIFT_EXPR] = "<<=";
  assignop_tab[(int) RSHIFT_EXPR] = ">>=";
  assignop_tab[(int) BIT_IOR_EXPR] = "|=";
  assignop_tab[(int) BIT_XOR_EXPR] = "^=";
  assignop_tab[(int) BIT_AND_EXPR] = "&=";

  init_filename_times ();

#define UNSET_RESERVED_WORD(STRING) \
  do { is_reserved_word (STRING, sizeof (STRING) - 1)->name = ""; } while (0)

  if (! flag_handle_exceptions)
    {
      /* Easiest way to not reconize exception
	 handling extenions...  */
      UNSET_RESERVED_WORD ("all");
      UNSET_RESERVED_WORD ("except");
      UNSET_RESERVED_WORD ("exception");
      UNSET_RESERVED_WORD ("raise");
      UNSET_RESERVED_WORD ("raises");
      UNSET_RESERVED_WORD ("reraise");
      UNSET_RESERVED_WORD ("try");
    }
  if (flag_no_asm)
    UNSET_RESERVED_WORD ("asm");
  if (flag_no_asm || flag_traditional)
    UNSET_RESERVED_WORD ("typeof");
  token_count = init_parse ();
  interface_unknown = 1;
}

void
reinit_parse_for_function ()
{
  current_base_init_list = NULL_TREE;
  current_member_init_list = NULL_TREE;
}

/* Functions and data structures for #pragma interface.

   `#pragma implementation' means that the main file being compiled
   is considered to implement (provide) the classes that appear in
   its main body.  I.e., if this is file "foo.cc", and class `bar'
   is defined in "foo.cc", then we say that "foo.cc implements bar".

   All main input files "implement" themselves automagically.

   `#pragma interface' means that unless this file (of the form "foo.h"
   is not presently being included by file "foo.cc", the
   CLASSTYPE_INTERFACE_ONLY bit gets set.  The effect is that none
   of the vtables nor any of the inline functions defined in foo.h
   will every be output.

   There are cases when we want to link files such as "defs.h" and
   "main.cc".  In this case, we give "defs.h" a `#pragma interface',
   and "main.cc" has `#pragma implementation "defs.h"'.  */

struct impl_files
{
  char *filename;
  struct impl_files *next;
};

static struct impl_files *impl_file_chain;

/* Helper function to load global variables with interface
   information.  */
static void
extract_interface_info ()
{
  tree fileinfo = get_time_identifier (input_filename);
  fileinfo = IDENTIFIER_CLASS_VALUE (fileinfo);
  interface_only = TREE_INT_CST_LOW (fileinfo);
  interface_unknown = TREE_INT_CST_HIGH (fileinfo);
}

/* Return nonzero if S and T are not considered part of an
   INTERFACE/IMPLEMENTATION pair.  Otherwise, return 0.  */
static int
interface_strcmp (s)
     char *s;
{
  /* Set the interface/implementation bits for this scope.  */
  struct impl_files *ifiles;
  char *s1 = strrchr (s, '/');
  if (s1++ == 0)
    s1 = s;
  s = s1;

  for (ifiles = impl_file_chain; ifiles; ifiles = ifiles->next)
    {
      char *t1 = ifiles->filename;
      s1 = s;

      if (*s1 != *t1 || *s1 == 0)
	continue;

      while (*s1 == *t1 && *s1 != 0)
	s1++, t1++;

      /* A match.  */
      if (*s1 == *t1)
	return 0;

      /* Don't get faked out by xxx.yyy.cc vs xxx.zzz.cc.  */
      if (strchr (s1, '.'))
	continue;

      if (*s1 == '\0' || s1[-1] != '.' || t1[-1] != '.')
	continue;

      /* A match.  */
      return 0;
    }

  /* No matches.  */
  return 1;
}

void
set_typedecl_interface_info (prev, vars)
     tree prev, vars;
{
  tree id = get_time_identifier (DECL_SOURCE_FILE (vars));
  tree fileinfo = IDENTIFIER_CLASS_VALUE (id);
  tree type = TREE_TYPE (vars);

  CLASSTYPE_INTERFACE_ONLY (type) = TREE_INT_CST_LOW (fileinfo)
    = interface_strcmp (DECL_SOURCE_FILE (vars));
}

void
set_vardecl_interface_info (prev, vars)
     tree prev, vars;
{
#if 0
  tree type = DECL_VPARENT (vars);
#else
  tree type = DECL_CONTEXT (vars);
#endif

  if (CLASSTYPE_INTERFACE_UNKNOWN (type) == 0)
    {
      if (CLASSTYPE_INTERFACE_ONLY (type))
	set_typedecl_interface_info (prev, TYPE_NAME (type));
      TREE_EXTERNAL (vars) = CLASSTYPE_INTERFACE_ONLY (type);
      TREE_PUBLIC (vars) = ! CLASSTYPE_INTERFACE_ONLY (type);
      CLASSTYPE_VTABLE_NEEDS_WRITING (type) |= TREE_PUBLIC (vars);
    }
}

/* Called from the top level: if there are any pending inlines to
   do, set up to process them now.  */
void
do_pending_inlines ()
{
  if (finput == finput1)
    {
      struct pending_inline *prev = 0, *tail;
      struct pending_inline *t =
	(struct pending_inline *) obstack_alloc (&inline_text_obstack,
						 sizeof (struct pending_inline));

      /* Record state we were in when we decided to process
	 inline functions instead.  */
      t->next = pending_inlines;
      pending_inlines = t;
      t->lineno = lineno;
      t->filename = input_filename;
      t->fndecl = NULL_TREE;
      t->token = yychar;
      t->token_value = yylval.itype;

      /* Reverse the pending inline functions, since
	 they were cons'd instead of appended.  */

      for (; t; t = tail)
	{
	  tail = t->next;
	  t->next = prev;
	  prev = t;
	}
      pending_inlines = prev;

      /* Now start processing the first inline function.  */
      t = pending_inlines;
      pending_inlines = pending_inlines->next;

      finput = finput2;
      clearerr(finput2);
      setvbuf (finput2, t->buf, _IOFBF, t->len - 1);
      next_inline_count = t->len - 1;

      lineno = t->lineno;
      input_filename = t->filename;
      yychar = PRE_PARSED_FUNCTION_DECL;
      yylval.ttype = t->fndecl;
      if (flag_default_inline)
	TREE_INLINE (t->fndecl) = 1;
    }
}

/* Since inline methods can refer to text which has not yet been seen,
   we store the text of the method in a structure which is placed in the
   DECL_PENDING_INLINE_INFO field of the FUNCTION_DECL.
   After parsing the body of the class definition, the FUNCTION_DECL's are
   scanned to see which ones have this field set.  Those are then digested
   one at a time.

   This function's FUNCTION_DECL will have a bit set in its common so
   that we know to watch out for it.  */

void
consume_string (this_obstack)
     register struct obstack *this_obstack;
{
  register char c;
  do
    {
      c = getc (finput);
      if (c == '\\')
	{
	  obstack_1grow (this_obstack, c);
	  c = getch ();
	  obstack_1grow (this_obstack, c);
	  continue;
	}
      if (c == '\n')
	{
	  if (pedantic)
	    warning ("ANSI C forbids newline in string constant");
	  lineno++;
	}
      obstack_1grow (this_obstack, c);
    }
  while (c != '\"');
}

static int nextchar = -1;
static int nextyychar = -1;
static YYSTYPE nextyylval;
static tree nextlastiddecl;

/* Get input from stream.  When compiling under Cadillac,
   the bytes must be coaxed out via their read protocol.
   Otherwise, they come easily via standard input interface.  */
int
getch ()
{
  register int ch = getc (finput);
  return ch;
}

/* Return next non-whitespace input character, which may come
   from `finput', or from `nextchar'.  */
static int
yynextch ()
{
  int c;

  if (nextchar >= 0)
    {
      c = nextchar;
      nextchar = -1;
    }
  else c = getc (finput);
  return skip_white_space (c);
}

/* Unget character CH from the input stream.
   If RESCAN is non-zero, then we want to `see' this
   character as the next input token.  */
void
yyungetc (ch, rescan)
     int ch;
     int rescan;
{
  /* Unget a characater from the input stream.  */
  if (yychar == YYEMPTY || rescan == 0)
    ungetc (ch, finput);
  else
    {
      if (nextyychar >= 0)
	abort ();
      nextyychar = yychar;
      nextyylval = yylval;
      yychar = ch;
    }
}

void
reinit_parse_for_method (yychar, decl)
     int yychar;
     tree decl;
{
  register char c = 0;
  int blev = 1;
  tree fndecl = decl;
  int starting_lineno = lineno;
  char *starting_filename = input_filename;
  int len;

  if (yychar != '{')
    {
      if (yychar != ':' && yychar != RETURN)
	{
	  yyerror ("parse error in method specification");
	  yychar = '{';
	}
      obstack_1grow (&inline_text_obstack, yychar);
      while (c >= 0)
	{
	  int this_lineno = lineno;

	  c = yynextch ();

	  /* Don't lose our cool if there are lots of comments.  */
	  if (lineno - this_lineno)
	    if (lineno - this_lineno == 1)
	      obstack_1grow (&inline_text_obstack, '\n');
	    else
	      {
		char buf[12];
		sprintf (buf, "\n# %d \"", lineno);
		len = strlen (buf);
		obstack_grow (&inline_text_obstack, buf, len);

		len = strlen (input_filename);
		obstack_grow (&inline_text_obstack, input_filename, len);
		obstack_1grow (&inline_text_obstack, '\"');
		obstack_1grow (&inline_text_obstack, '\n');
	      }

	  /* strings must be read differently than text.  */
	  if (c == '\"')
	    {
	      obstack_1grow (&inline_text_obstack, c);
	      consume_string (&inline_text_obstack);
	      c = yynextch ();
	    }
	  while (c > ' ')	/* ASCII dependent! */
	    {
	      obstack_1grow (&inline_text_obstack, c);
	      if (c == '{') goto main_loop;
	      if (c == '\"')
		consume_string (&inline_text_obstack);
	      if (c == ';')
		{
		  error ("function body for constructor missing");
		  obstack_1grow (&inline_text_obstack, '{');
		  obstack_1grow (&inline_text_obstack, '}');
		  len += 2;
		  goto done;
		}
	      c = getch ();
	    }
	  if (c == '\n')
	    lineno++;
	  obstack_1grow (&inline_text_obstack, c);
	}
      if (c == EOF)
	{
	  error_with_file_and_line (starting_filename,
				    starting_lineno,
				    "parse error in method specification");
	}	  
    }
  else obstack_1grow (&inline_text_obstack, '{');

 main_loop:
  while (c >= 0)
    {
      int this_lineno = lineno;

      c = skip_white_space (getc (finput));

      /* Don't lose our cool if there are lots of comments.  */
      if (lineno - this_lineno)
	if (lineno - this_lineno == 1)
	  obstack_1grow (&inline_text_obstack, '\n');
	else
	  {
	    char buf[12];
	    sprintf (buf, "\n# %d \"", lineno);
	    len = strlen (buf);
	    obstack_grow (&inline_text_obstack, buf, len);

	    len = strlen (input_filename);
	    obstack_grow (&inline_text_obstack, input_filename, len);
	    obstack_1grow (&inline_text_obstack, '\"');
	    obstack_1grow (&inline_text_obstack, '\n');
	  }

      while (c > ' ')
	{
	  obstack_1grow (&inline_text_obstack, c);
	  if (c == '{') blev++;
	  else if (c == '}')
	    {
	      blev--;
	      if (blev == 0)
		goto done;
	    }
	  else if (c == '\"')
	    consume_string (&inline_text_obstack);
	  c = getch ();
	}
      if (c == '\n')
	lineno++;
      obstack_1grow (&inline_text_obstack, c);
    }
 done:
  current_base_init_list = NULL_TREE;
  current_member_init_list = NULL_TREE;

#ifdef USG_STDIO
  len = obstack_object_size (&inline_text_obstack);
  /* If the buffer given to setvbuf is shorter than eight bytes long,
     setvbuf will (in violation of its man page) ignore the buffer
     and call malloc to get a bigger one.  */
  while (len < 8)
    {
      len++;
      obstack_1grow (&inline_text_obstack, ' ');
    }
#endif

  obstack_1grow (&inline_text_obstack, '\0');
  len = obstack_object_size (&inline_text_obstack);

  if (fndecl == void_type_node)
    {
      /* Happens when we get two declarations of the same
	 function in the same scope.  */
      char *buf = obstack_base (&inline_text_obstack);
      obstack_free (&inline_text_obstack, buf);
      return;
    }
  else
    {
      struct pending_inline *t;
      char *buf = obstack_base (&inline_text_obstack);

      obstack_finish (&inline_text_obstack);

      t = (struct pending_inline *) obstack_alloc (&inline_text_obstack,
						   sizeof (struct pending_inline));
      t->buf = buf;
      t->len = len;
      t->lineno = starting_lineno;
      t->filename = starting_filename;
      t->token = YYEMPTY;
      DECL_PENDING_INLINE_INFO (fndecl) = t;
    }
}

/* Build a default function named NAME for type TYPE.
   KIND says what to build.  Currently only two kinds of default functions
   are recognized:

   When KIND == 0, build default X(X&) constructor.
   When KIND == 1, build default destructor.  */

tree
cons_up_default_function (type, name, kind)
     tree type, name;
     int kind;
{
  extern tree void_list_node;
  int len;
  tree fn, args;
  tree argtype;

  switch (kind)
    {
    case 0:
      /* Destructor.  */
      name = build_parse_node (BIT_NOT_EXPR, name);
      /* Fall through...  */
    case 2:
      /* Default constructor.  */
      args = void_list_node;
      break;

    case 3:
      type = build_type_variant (type, 1, 0);
      /* Fall through...  */
    case 1:
      argtype = build_reference_type (type);
      args = tree_cons (NULL_TREE,
			build_tree_list (hash_tree_chain (argtype, NULL_TREE),
					 get_identifier ("arg")),
			void_list_node);
      break;

    default:
      abort ();
    }

  fn = start_method (NULL_TREE,
		     build_parse_node (CALL_EXPR, name, args, NULL_TREE),
		     NULL_TREE);
  if (fn == void_type_node)
    return fn;

  obstack_1grow (&inline_text_obstack, '{');
  obstack_1grow (&inline_text_obstack, '}');
#ifdef USG_STDIO
  len = 2;
  while (len++ < 8)
    obstack_1grow (&inline_text_obstack, ' ');
#endif

  obstack_1grow (&inline_text_obstack, '\0');
  current_base_init_list = NULL_TREE;
  current_member_init_list = NULL_TREE;

  len = obstack_object_size (&inline_text_obstack);

  {
    struct pending_inline *t;
    char *buf = obstack_base (&inline_text_obstack);

    obstack_finish (&inline_text_obstack);

    t = (struct pending_inline *) obstack_alloc (&inline_text_obstack,
						 sizeof (struct pending_inline));
    t->buf = buf;
    t->len = len;
    t->lineno = lineno;
    t->filename = input_filename;
    t->token = YYEMPTY;
    DECL_PENDING_INLINE_INFO (fn) = t;
    /* We make this declaration private (static in the C sense).  */
    TREE_PUBLIC (fn) = 0;
  }
  finish_method (fn);
  DECL_COMPILER_GENERATED_P (fn) = 1;
  return fn;
}

/* Heuristic to tell whether the user is missing a semicolon
   after a struct or enum declaration.  Emit an error message
   if we know the user has blown it.  */
void
check_for_missing_semicolon (type)
     tree type;
{
  if (yychar < 0)
    yychar = yylex ();

  if (yychar > 255
      && yychar != IDENTIFIER
      && yychar != TYPENAME)
    {
      if (ANON_AGGRNAME_P (DECL_NAME (TYPE_NAME (type))))
	error ("semicolon missing after %s declaration",
	       TREE_CODE (type) == ENUMERAL_TYPE ? "enum" : "struct");
      else
	error ("semicolon missing after declaration of `%s'",
	       TYPE_NAME_STRING (type));
      shadow_tag (build_tree_list (0, type));
    }
  /* Could probably also hack cases where class { ... } f (); appears.  */
}

void
note_got_semicolon (type)
     tree type;
{
  if (IS_AGGR_TYPE (type))
    CLASSTYPE_GOT_SEMICOLON (type) = 1;
}

/* If C is not whitespace, return C.
   Otherwise skip whitespace and return first nonwhite char read.  */

static int
skip_white_space (c)
     register int c;
{
#if 0
  register int inside;
#endif

  for (;;)
    {
      switch (c)
	{
	  /* Don't recognize comments in cc1: all comments are removed by cpp,
	     and cpp output can include / and * consecutively as operators.  */
#if 0
	case '/':
	  c = getc (finput);
	  if (c != '*' && c != '/')
	    {
	      ungetc (c, finput);
	      return '/';
	    }

	  if (c == '/')
	    {
	      while (c != EOF)
		{
		  c = getch ();
		  if (c == '\n')
		    {
		      ungetc (c, finput);
		      break;
		    }
		}
	      if (c == EOF)
		{
		  error ("unterminated comment");
		  return EOF;
		}
	      c = getch ();
	      break;
	    }

	  c = getch ();

	  inside = 1;
	  while (inside)
	    {
	      if (c == '*')
		{
		  while (c == '*')
		    c = getch ();

		  if (c == '/')
		    {
		      inside = 0;
		      c = getch ();
		    }
		}
	      else if (c == '\n')
		{
		  lineno++;
		  c = getch ();
		}
	      else if (c == EOF)
		{
		  error ("unterminated comment");
		  break;
		}
	      else
		c = getch ();
	    }

	  break;
#endif

	case '\n':
	  c = check_newline ();
	  break;

	case ' ':
	case '\t':
	case '\f':
	case '\r':
	case '\v':
	case '\b':
	  do
	    c = getc (finput);
	  while (c == ' ' || c == '\t');
	  break;

	case '\\':
	  c = getch ();
	  if (c == '\n')
	    lineno++;
	  else
	    error ("stray '\\' in program");
	  c = getch ();
	  break;

	default:
	  return (c);
	}
    }
}



/* Make the token buffer longer, preserving the data in it.
   P should point to just beyond the last valid character in the old buffer.
   The value we return is a pointer to the new buffer
   at a place corresponding to P.  */

static char *
extend_token_buffer (p)
     char *p;
{
  int offset = p - token_buffer;

  maxtoken = maxtoken * 2 + 10;
  token_buffer = (char *) xrealloc (token_buffer, maxtoken + 2);

  return token_buffer + offset;
}

#ifdef HAVE_UNDUMP
#ifndef MERGED
/* This includes code from write_segment, stolen from unexec.c */

void dump_data()
{
  int new;
  register caddr_t ptr, end;
  register int i, nwrite, ret;
  char buf[80];
  extern int errno;
  char zeros[128];

  extern int been_here_before, just_done_unexec, my_edata;
  extern char *dump_source_name;
  extern char *asm_file_name, previous_asm_file_name[];
  char dump_file_name[256];	/* Fixed-sized buffer -- sigh. */
  caddr_t end_of_data, end_of_heap;
  int data_size, token;
  register int c;

  bzero (zeros, sizeof zeros);

  /* Here we have just seen `#pragma dump '.
     The name to dump to, a string constant, may follow.  */

  do
    c = getch ();
  while (c == ' ' || c == '\t');

  /* If no argument, default to something like "dumped-cc1plus".  */
  if (c == '\n')
    {
      char *tmp;
      strcpy (dump_file_name, "dumped-");
      if (tmp = strrchr (dump_source_name, '/'))
	dump_source_name = tmp + 1;
      strcat (dump_file_name, dump_source_name);
    }
  else
    {
      ungetc (c, finput);
      token = yylex ();
      if (token != STRING
	  || TREE_CODE (yylval.ttype) != STRING_CST)
	{
	  error ("invalid #pragma dump");
	  return;
	}

      strcpy (dump_file_name, TREE_STRING_POINTER (yylval.ttype));
    }

  been_here_before = 1;		/* Raise the flag! */
  strcpy(previous_asm_file_name, asm_file_name);
  printf("\nDumping %s to %s...\n", dump_source_name, dump_file_name);

  end_of_heap = (caddr_t)sbrk(0);
  end_of_data = (caddr_t)((int)(&my_edata)&~(getpagesize()-1));
  data_size = (int)(end_of_heap-end_of_data);
  printf("Data size = %d\n", data_size);
		  
  new = creat (dump_file_name, 0666);

  ptr = end_of_data;
  end = end_of_heap;

  for (i = 0; ptr < end;)
    {
      /* distance to next multiple of 128.  */
      nwrite = (((int) ptr + 128) & -128) - (int) ptr;
      /* But not beyond specified end.  */
      if (nwrite > end - ptr) nwrite = end - ptr;
      ret = write (new, ptr, nwrite);
      /* If write gets a page fault, it means we reached
	 a gap between the old text segment and the old data segment.
	 This gap has probably been remapped into part of the text segment.
	 So write zeros for it.  */
      if (ret == -1 && errno == EFAULT)
	write (new, zeros, nwrite);
      else if (nwrite != ret)
	{
	  sprintf (buf,
		   "unexec write failure: addr 0x%x, fileno %d, size 0x%x, wrote 0x%x, errno %d",
		   ptr, new, nwrite, ret, errno);
	  perror (buf);
	  return;
	}
      i += nwrite;
      ptr += nwrite;
    }

  close (new);

  just_done_unexec = 1;		/* Tell toplev not to output ending. */
}
#endif
#endif

static int
get_last_nonwhite_on_line ()
{
  register int c;

  /* Is this the last nonwhite stuff on the line?  */
  if (nextchar >= 0)
    c = nextchar, nextchar = -1;
  else
    c = getc (finput);

  while (c == ' ' || c == '\t')
    c = getc (finput);
  return c;
}

/* At the beginning of a line, increment the line number
   and process any #-directive on this line.
   If the line is a #-directive, read the entire line and return a newline.
   Otherwise, return the line's first non-whitespace character.  */

int
check_newline ()
{
  register int c;
  register int token;

  lineno++;

  /* Read first nonwhite char on the line.  */

  do
    c = getc (finput);
  while (c == ' ' || c == '\t');

  if (c != '#')
    {
      /* If not #, return it so caller will use it.  */
      return c;
    }

  /* Read first nonwhite char after the `#'.  */

  do
    c = getch ();
  while (c == ' ' || c == '\t');

  /* If a letter follows, then if the word here is `line', skip
     it and ignore it; otherwise, ignore the line, with an error
     if the word isn't `pragma'.  */

  if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))
    {
      if (c == 'p')
	{
	  if (getch () == 'r'
	      && getch () == 'a'
	      && getch () == 'g'
	      && getch () == 'm'
	      && getch () == 'a')
/* Change by Bryan Boreham, Kewill, Sun Jul 23 15:53:24 1989.
   This whole section added to support dumping of
   compilations in the middle. */
	    {
	      /* Read first nonwhite char after the `#pragma'.  */

	      do
		c = getch ();
	      while (c == ' ' || c == '\t');

#ifndef MERGED
	      /* See if it is "dump" */

	      if (c == 'd'
		  && getch () == 'u'
		  && getch () == 'm'
		  && getch () == 'p'
		  && ((c = getch ()) == ' ' || c == '\t' || c == '\n'))
		{
#ifndef HAVE_UNDUMP
			;		/* Are you crazy? */
#else
		  ungetc (c, finput);
		  dump_data();
		  longjmp (toplevel, 1);
#endif
		}
	      else
#endif
		if (c == 'v'
		       && getch () == 't'
		       && getch () == 'a'
		       && getch () == 'b'
		       && getch () == 'l'
		       && getch () == 'e'
		       && ((c = getch ()) == ' ' || c == '\t' || c == '\n'))
		{
		  extern tree pending_vtables;

		  /* More follows: it must be a string constant (class name).  */
		  token = yylex ();
		  if (token != STRING || TREE_CODE (yylval.ttype) != STRING_CST)
		    {
		      error ("invalid #pragma vtable");
		      goto skipline;
		    }
		  if (write_virtuals != 2)
		    {
		      warning ("use `+e2' option to enable #pragma vtable");
		      goto skipline;
		    }
		  pending_vtables = perm_tree_cons (NULL_TREE, get_identifier (TREE_STRING_POINTER (yylval.ttype)), pending_vtables);
		  if (nextchar < 0)
		    nextchar = getch ();
		  c = nextchar;
		  if (c != '\n')
		    warning ("trailing characters ignored");
		}
	      else if (c == 'u'
		       && getch () == 'n'
		       && getch () == 'i'
		       && getch () == 't'
		       && ((c = getch ()) == ' ' || c == '\t' || c == '\n'))
		{
		  /* More follows: it must be a string constant (unit name).  */
		  token = yylex ();
		  if (token != STRING || TREE_CODE (yylval.ttype) != STRING_CST)
		    {
		      error ("invalid #pragma unit");
		      goto skipline;
		    }
		  current_unit_name = get_identifier (TREE_STRING_POINTER (yylval.ttype));
		  current_unit_language = current_lang_name;
		  if (nextchar < 0)
		    nextchar = getch ();
		  c = nextchar;
		  if (c != '\n')
		    warning ("trailing characters ignored");
		}
	      else if (c == 'i')
		{
		  tree fileinfo = IDENTIFIER_CLASS_VALUE (get_time_identifier (input_filename));
		  c = getch ();

		  if (c == 'n'
		      && getch () == 't'
		      && getch () == 'e'
		      && getch () == 'r'
		      && getch () == 'f'
		      && getch () == 'a'
		      && getch () == 'c'
		      && getch () == 'e'
		      && ((c = getch ()) == ' ' || c == '\t' || c == '\n'))
		    {
		      /* read to newline.  */
		      while (c != '\n')
			c = getch ();

		      write_virtuals = 3;

		      if (impl_file_chain == 0)
			{
			  char *filename;
			  tree fi;

			  /* If this is zero at this point, then we are
			     auto-implementing.  */
			  if (main_input_filename == 0)
			    main_input_filename = input_filename;

			  filename = strrchr (main_input_filename, '/');
			  if (filename++ == 0)
			    filename = main_input_filename;
			  fi = get_time_identifier (filename);
			  fi = IDENTIFIER_CLASS_VALUE (fi);
			  TREE_INT_CST_LOW (fi) = 0;
			  TREE_INT_CST_LOW (fi) = 0;
			  /* Get default.  */
			  impl_file_chain = (struct impl_files *)permalloc (sizeof (struct impl_files));
			  impl_file_chain->filename = filename;
			  impl_file_chain->next = 0;
			}

		      interface_only = interface_strcmp (input_filename);
		      interface_unknown = 0;
		      TREE_INT_CST_LOW (fileinfo) = interface_only;
		      TREE_INT_CST_HIGH (fileinfo) = interface_unknown;
		    }
		  else if (c == 'm'
			   && getch () == 'p'
			   && getch () == 'l'
			   && getch () == 'e'
			   && getch () == 'm'
			   && getch () == 'e'
			   && getch () == 'n'
			   && getch () == 't'
			   && getch () == 'a'
			   && getch () == 't'
			   && getch () == 'i'
			   && getch () == 'o'
			   && getch () == 'n'
			   && ((c = getch ()) == ' ' || c == '\t' || c == '\n'))
		    {
		      char *main_filename = main_input_filename ? main_input_filename : input_filename;
		      char *tmp;

		      while (c == ' ' || c == '\t')
			c = getch ();
		      if (c != '\n')
			{
			  ungetc (c, finput);
			  token = yylex ();
			  if (token != STRING
			      || TREE_CODE (yylval.ttype) != STRING_CST)
			    {
			      error ("invalid `#pragma implementation'");
			      goto skipline;
			    }
			  main_filename = TREE_STRING_POINTER (yylval.ttype);
			}
		      tmp = strrchr (main_filename, '/');
		      if (tmp++)
			main_filename = tmp;

		      /* read to newline.  */
		      while (c != '\n')
			c = getch ();

		      if (write_virtuals == 3)
			{
			  struct impl_files *ifiles = impl_file_chain;
			  while (ifiles)
			    {
			      if (! strcmp (ifiles->filename, main_filename))
				break;
			      ifiles = ifiles->next;
			    }
			  if (ifiles == 0)
			    {
			      ifiles = (struct impl_files*) permalloc (sizeof (struct impl_files));
			      ifiles->filename = main_filename;
			      ifiles->next = impl_file_chain;
			      impl_file_chain = ifiles;
			    }
			}
		      else if (main_input_filename == input_filename
			       || ! strcmp (input_filename, main_filename))
			{
			  write_virtuals = 3;
			  if (impl_file_chain == 0)
			    {
			      impl_file_chain = (struct impl_files*) permalloc (sizeof (struct impl_files));
			      impl_file_chain->filename = main_filename;
			      impl_file_chain->next = 0;
			    }
			}
		      else
			error ("`#pragma implementation' can only appear at top-level");
		      interface_only = 0;
		      interface_unknown = 0;
		      TREE_INT_CST_LOW (fileinfo) = interface_only;
		      TREE_INT_CST_HIGH (fileinfo) = interface_unknown;
		    }
		}
	    }
	  goto skipline;
	}

      else if (c == 'l')
	{
	  if (getch () == 'i'
	      && getch () == 'n'
	      && getch () == 'e'
	      && ((c = getch ()) == ' ' || c == '\t'))
	    goto linenum;
	}
      else if (c == 'i')
	{
	  if (getch () == 'd'
	      && getch () == 'e'
	      && getch () == 'n'
	      && getch () == 't'
	      && ((c = getch ()) == ' ' || c == '\t'))
	    {
	      /* Conditionally used.  */
              extern FILE *asm_out_file;

	      if (pedantic)
		error ("ANSI C does not allow #ident");

	      /* Here we have just seen `#ident '.
		 A string constant should follow.  */

	      while (c == ' ' || c == '\t')
		c = getch ();

	      /* If no argument, ignore the line.  */
	      if (c == '\n')
		return c;

	      ungetc (c, finput);
	      token = yylex ();
	      if (token != STRING
		  || TREE_CODE (yylval.ttype) != STRING_CST)
		{
		  error ("invalid #ident");
		  goto skipline;
		}

#ifdef ASM_OUTPUT_IDENT
	      ASM_OUTPUT_IDENT (asm_out_file, TREE_STRING_POINTER (yylval.ttype));
#endif

	      /* Skip the rest of this line.  */
	      goto skipline;
	    }
	}
      else if (c == 'n')
	{
	  if (getch () == 'e'
	      && getch () == 'w'
	      && getch () == 'w'
	      && getch () == 'o'
	      && getch () == 'r'
	      && getch () == 'l'
	      && getch () == 'd'
	      && ((c = getch ()) == ' ' || c == '\t'))
	    {
	      /* Used to test incremental compilation.  */
	      sorry ("#pragma newworld");
	      goto skipline;
	    }
	}
      error ("undefined or invalid # directive");
      goto skipline;
    }

linenum:
  /* Here we have either `#line' or `# <nonletter>'.
     In either case, it should be a line number; a digit should follow.  */

  while (c == ' ' || c == '\t')
    c = getch ();

  /* If the # is the only nonwhite char on the line,
     just ignore it.  Check the new newline.  */
  if (c == '\n')
    return c;

  /* Something follows the #; read a token.  */

  ungetc (c, finput);
  token = yylex ();

  if (token == CONSTANT
      && TREE_CODE (yylval.ttype) == INTEGER_CST)
    {
      int old_lineno = lineno;
      /* subtract one, because it is the following line that
	 gets the specified number */

      int l = TREE_INT_CST_LOW (yylval.ttype) - 1;
      c = get_last_nonwhite_on_line ();
      if (c == '\n')
	{
	  /* No more: store the line number and check following line.  */
	  lineno = l;
	  return c;
	}
      ungetc (c, finput);

      /* More follows: it must be a string constant (filename).  */

      token = yylex ();
      if (token != STRING || TREE_CODE (yylval.ttype) != STRING_CST)
	{
	  error ("invalid #line");
	  goto skipline;
	}

      /* Changing files again.  This means currently collected time
	 is charged against header time, and body time starts back
	 at 0.  */
      if (flag_detailed_statistics)
	{
	  int this_time = my_gettime ();
	  tree time_identifier = get_time_identifier (TREE_STRING_POINTER (yylval.ttype));
	  header_time += this_time - body_time;
	  TREE_INT_CST_LOW (IDENTIFIER_LOCAL_VALUE (this_filename_time))
	    += this_time - body_time;
	  this_filename_time = time_identifier;
	  body_time = this_time;
	}

      if (flag_cadillac)
	cadillac_note_source ();

      input_filename
	= (char *) permalloc (TREE_STRING_LENGTH (yylval.ttype) + 1);
      strcpy (input_filename, TREE_STRING_POINTER (yylval.ttype));
      lineno = l;
#ifdef FIELD_XREF
      FIELD_xref_file(input_filename);
#endif

      if (main_input_filename == 0)
	{
	  extern int been_here_before;
	  struct impl_files *ifiles = impl_file_chain;

	  if (ifiles)
	    {
	      while (ifiles->next)
		ifiles = ifiles->next;
	      ifiles->filename = (char *)strrchr (input_filename, '/');
	      if (ifiles->filename++ == 0)
		ifiles->filename = input_filename;
	    }

	  main_input_filename = input_filename;
	  if (write_virtuals == 3)
	    walk_vtables (set_typedecl_interface_info, set_vardecl_interface_info);
	}

      extract_interface_info ();

      c = get_last_nonwhite_on_line ();
      if (c == '\n')
	{
	  if (flag_cadillac)
	    cadillac_switch_source (-1);
	  return c;
	}
      ungetc (c, finput);

      token = yylex ();

      /* `1' after file name means entering new file.
	 `2' after file name means just left a file.  */

      if (token == CONSTANT
	  && TREE_CODE (yylval.ttype) == INTEGER_CST)
	{
	  if (TREE_INT_CST_LOW (yylval.ttype) == 1)
	    {
	      struct file_stack *p
		= (struct file_stack *) xmalloc (sizeof (struct file_stack));
	      input_file_stack->line = old_lineno;
	      p->next = input_file_stack;
	      p->name = input_filename;
	      input_file_stack = p;
	      input_file_stack_tick++;

	      if (flag_cadillac)
		cadillac_push_source ();
	    }
	  else if (input_file_stack->next)
	    {
	      struct file_stack *p = input_file_stack;

	      if (flag_cadillac)
		cadillac_pop_source ();

	      input_file_stack = p->next;
	      free (p);
	      input_file_stack_tick++;
	    }
	  else
	    error ("#-lines for entering and leaving files don't match");
	}
      else if (flag_cadillac)
	cadillac_switch_source (-1);

      /* If NEXTCHAR is not end of line, we don't care what it is.  */
      if (nextchar == '\n')
	return '\n';
    }
  else
    error ("invalid #-line");

  /* skip the rest of this line.  */
 skipline:
  if (c == '\n')
    return c;
  while ((c = getch ()) != EOF && c != '\n');
  return c;
}

#if 0
#define isalnum(char) (char >= 'a' ? char <= 'z' : char >= '0' ? char <= '9' || (char >= 'A' && char <= 'Z') : 0)
#define isdigit(char) (char >= '0' && char <= '9')
#else
#include <ctype.h>
#endif

#define ENDFILE -1  /* token that represents end-of-file */

static int
readescape ()
{
  register int c = getc (finput);
  register int count, code;
  int firstdig;

  switch (c)
    {
    case 'x':
      code = 0;
      count = 0;
      while (1)
	{
	  c = getch ();
	  if (! isxdigit (c))
	    {
	      ungetc (c, finput);
	      break;
	    }
	  code *= 16;
	  if (c >= 'a' && c <= 'f')
	    code += c - 'a' + 10;
	  if (c >= 'A' && c <= 'F')
	    code += c - 'A' + 10;
	  if (c >= '0' && c <= '9')
	    code += c - '0';
	  if (count == 0)
	    firstdig = code;
	  count++;
	}
      if (count == 0)
	error ("\\x used with no following hex digits");
      else if ((count - 1) * 4 >= TYPE_PRECISION (integer_type_node)
	       || ((1 << (TYPE_PRECISION (integer_type_node) - (count - 1) * 4))
		   <= firstdig))
	warning ("hex escape out of range");
      return code;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':
      code = 0;
      count = 0;
      while ((c <= '7') && (c >= '0') && (count++ < 3))
	{
	  code = (code * 8) + (c - '0');
	  c = getch ();
	}
      ungetc (c, finput);
      return code;

    case '\\': case '\'': case '"':
      return c;

    case '\n':
      lineno++;
      return -1;

    case 'n':
      return TARGET_NEWLINE;

    case 't':
      return TARGET_TAB;

    case 'r':
      return TARGET_CR;

    case 'f':
      return TARGET_FF;

    case 'b':
      return TARGET_BS;

    case 'a':
      return TARGET_BELL;

    case 'v':
      return TARGET_VT;

    case 'E':
      return 033;

    case '?':
      /* `\(', etc, are used at beginning of line to avoid confusing Emacs.  */
    case '(':
    case '{':
    case '[':
      return c;
    }
  if (c >= 040 && c <= 0177)
    warning ("unknown escape sequence `\\%c'", c);
  else
    warning ("unknown escape sequence: `\\' followed by char code 0x%x", c);
  return c;
}

/* Value is 1 if we should try to make the next identifier look like a
   typename (when it may be a local variable or a class variable).
   Value is 0 if we treat this name in a default fashion.
   Value is -1 if we must not see a type name.  */
int looking_for_typename = 0;

void
dont_see_typename ()
{
  looking_for_typename = -1;
  if (yychar == TYPENAME)
    {
      yychar = IDENTIFIER;
      lastiddecl = 0;
    }
}

void
see_typename ()
{
  looking_for_typename = 0;
  if (yychar == IDENTIFIER)
    {
      lastiddecl = lookup_name (yylval.ttype);
      if (lastiddecl == 0 && flag_labels_ok)
	lastiddecl = IDENTIFIER_LABEL_VALUE (yylval.ttype);
      else if (lastiddecl != 0
	       && TREE_CODE (lastiddecl) == TYPE_DECL)
	yychar = TYPENAME;
    }
}

tree do_identifier (token)
     register tree token;
{
  register tree id = lastiddecl;

  if (yychar == YYEMPTY)
    yychar = yylex ();
  /* Scope class declarations before global
     declarations.  */
  if (id == IDENTIFIER_GLOBAL_VALUE (token)
      && current_class_type != 0
      && TYPE_SIZE (current_class_type) == 0)
    {
      /* Could be from one of the base classes.  */
      tree field = lookup_field (current_class_type, token, 1);
      if (field == 0)
	;
      else if (field == error_mark_node)
	/* We have already generated the error message.
	   But we still want to return this value.  */
	id = lookup_field (current_class_type, token, 0);
      else if (TREE_CODE (field) == VAR_DECL
	       || TREE_CODE (field) == CONST_DECL)
	id = field;
      else if (TREE_CODE (field) != FIELD_DECL)
	abort ();
      else
	{
	  error_with_decl (field, "invalid use of member `%s' from base class `%s'",
			   TYPE_NAME_STRING (DECL_FIELD_CONTEXT (field)));
	  id = error_mark_node;
	  return id;
	}
    }

  if (!id || id == error_mark_node)
    {
      if (yychar == '(' || yychar == LEFT_RIGHT)
	{
	  id = implicitly_declare (token);
	  assemble_external (id);
	  TREE_USED (id) = 1;
	}
      else if (current_function_decl == 0)
	{
	  error ("`%s' undeclared, outside of functions",
		 IDENTIFIER_POINTER (token));
	  id = error_mark_node;
	}
      else
	{
	  if (IDENTIFIER_GLOBAL_VALUE (token) != error_mark_node
	      || IDENTIFIER_ERROR_LOCUS (token) != current_function_decl)
	    {
	      extern int undeclared_variable_notice;

	      error ("`%s' undeclared (first use this function)",
		     IDENTIFIER_POINTER (token));

	      if (! undeclared_variable_notice)
		{
		  error ("(Each undeclared identifier is reported only once");
		  error ("for each function it appears in.)");
		  undeclared_variable_notice = 1;
		}
	    }
	  id = error_mark_node;
	  /* Prevent repeated error messages.  */
	  IDENTIFIER_GLOBAL_VALUE (token) = error_mark_node;
	  SET_IDENTIFIER_ERROR_LOCUS (token, current_function_decl);
	}
    }
  /* TREE_USED is set in `hack_identifier'.  */
  if (TREE_CODE (id) == CONST_DECL)
    {
      if (IDENTIFIER_CLASS_VALUE (token) == id)
	{
	  /* Check visibility.  */
	  enum visibility_type visibility
	    = compute_visibility (CLASSTYPE_AS_LIST (current_class_type), id);
	  if (visibility == visibility_private)
	    error_with_decl (id, "enum `%s' is private");
	  /* protected is OK, since it's an enum of `this'.  */
	}
      id = DECL_INITIAL (id);
    }
  else id = hack_identifier (id, token, yychar);
  return id;
}

int
yylex ()
{
  tree tmp;
  register int c;
  register int value;
  int wide_flag = 0;
  int dollar_seen = 0;

 relex:
  if (nextyychar >= 0)
    {
      value = nextyychar;
      yylval = nextyylval;
      lastiddecl = nextlastiddecl;
      nextyychar = -1;
      if (value == IDENTIFIER)
	{
	  tmp = yylval.ttype;
	  goto resume_identifier_processing;
	}
      goto done;
    }
  if (nextchar >= 0)
    c = nextchar, nextchar = -1;
  else
    c = getc (finput);

  /* Effectively do c = skip_white_space (c)
     but do it faster in the usual cases.  */
  while (1)
    switch (c)
      {
      case ' ':
      case '\t':
      case '\f':
      case '\r':
      case '\v':
      case '\b':
	c = getc (finput);
	break;

      case '\n':
      case '/':
      case '\\':
	c = skip_white_space (c);
      default:
	goto found_nonwhite;
      }
 found_nonwhite:

  token_buffer[0] = c;
  token_buffer[1] = 0;

/*  yylloc.first_line = lineno; */

  switch (c)
    {
    case EOF:
      token_buffer[0] = '\0';
      if (pending_inlines)
	{
	  struct pending_inline *t;

	  t = pending_inlines;
#ifdef DO_METHODS_THE_OLD_WAY
	  yylval.itype = t->token_value;
	  value = t->token;
#else
	  if (t->fndecl == 0)
	    {
	      yylval.itype = t->token_value;
	      value = t->token;
	    }
	  else
	    {
	      yylval.ttype = t->fndecl;
	      value = PRE_PARSED_FUNCTION_DECL;
	    }
#endif

	  lineno = t->lineno;
/*	    yylloc.first_line = lineno; */
	  input_filename = t->filename;

	  if (t->next)
	    {
	      /* The buffer we used will be freed at the
		 end of this function.  */
	      pending_inlines = pending_inlines->next;
	      clearerr(finput2);
	      setvbuf (finput2, t->buf, _IOFBF, t->len - 1);
	      next_inline_count = t->len - 1;
	    }
	  else
	    {
	      pending_inlines = NULL;
	      finput = finput1;
	      obstack_free (&inline_text_obstack, inline_text_firstobj);
	    }
	  /* The space used by T will be freed after all inline
	     functions have been processed.  */
	  if (value <= 0)
	    goto relex;
	  else
	    goto done;
	}
      end_of_file = 1;
      value = ENDFILE;
      break;

    case '$':
      if (dollars_in_ident)
	{
	  dollar_seen = 1;
	  goto letter;
	}
      value = '$';
      goto done;

    case 'L':
      /* Capital L may start a wide-string or wide-character constant.  */
      {
	register int c = getch ();
	if (c == '\'')
	  {
	    wide_flag = 1;
	    goto char_constant;
	  }
	if (c == '"')
	  {
	    wide_flag = 1;
	    goto string_constant;
	  }
	ungetc (c, finput);
      }

    case 'A':  case 'B':  case 'C':  case 'D':  case 'E':
    case 'F':  case 'G':  case 'H':  case 'I':  case 'J':
    case 'K':		  case 'M':  case 'N':  case 'O':
    case 'P':  case 'Q':  case 'R':  case 'S':  case 'T':
    case 'U':  case 'V':  case 'W':  case 'X':  case 'Y':
    case 'Z':
    case 'a':  case 'b':  case 'c':  case 'd':  case 'e':
    case 'f':  case 'g':  case 'h':  case 'i':  case 'j':
    case 'k':  case 'l':  case 'm':  case 'n':  case 'o':
    case 'p':  case 'q':  case 'r':  case 's':  case 't':
    case 'u':  case 'v':  case 'w':  case 'x':  case 'y':
    case 'z':
    case '_':
    letter:
      {
	register char *p;

	p = token_buffer;
	while (isalnum(c) || (c == '_') || c == '$')
	  {
	    if (p >= token_buffer + maxtoken)
	      p = extend_token_buffer (p);
	    if (c == '$' && ! dollars_in_ident)
	      break;

	    *p++ = c;
	    c = getc (finput);
	  }

	*p = 0;
	nextchar = c;

	value = IDENTIFIER;
	yylval.itype = 0;

      /* Try to recognize a keyword.  Uses minimum-perfect hash function */

	{
	  register struct resword *ptr;

	  if (ptr = is_reserved_word (token_buffer, p - token_buffer))
	    {
	      if (current_lang_name != lang_name_cplusplus)
		{
		  if (ptr->rid != 0
		      && (ptr->rid == RID_CLASS
			  || ptr->rid == RID_FRIEND
			  || ptr->rid == RID_VIRTUAL
			  || (flag_no_asm && ptr->rid == RID_INLINE)))
		    {
		      ptr = 0;
		      goto not_reserved_word_after_all;
		    }
		  if (flag_traditional
		      && ((int) ptr->token == TYPEOF
			  || ptr->rid == RID_SIGNED
			  || ptr->rid == RID_INLINE))
		    {
		      ptr = 0;
		      goto not_reserved_word_after_all;
		    }  
		}
	      if (ptr->rid)
		{
		  tree old_ttype = ridpointers[(int) ptr->rid];

		  /* If this provides a type for us, then revert lexical
		     state to standard state.  */
		  if (TREE_CODE (old_ttype) == IDENTIFIER_NODE
		      && IDENTIFIER_GLOBAL_VALUE (old_ttype) != 0
		      && TREE_CODE (IDENTIFIER_GLOBAL_VALUE (old_ttype)) == TYPE_DECL)
		    looking_for_typename = 0;

		  /* Check if this is a language-type declaration.
		     Just glimpse the next non-white character.  */
		  nextchar = skip_white_space (nextchar);
		  if (nextchar == '"')
		    {
		      /* We are looking at a string.  Complain
			 if the token before the string is no `extern'.
			 
			 Could cheat some memory by placing this string
			 on the temporary_, instead of the saveable_
			 obstack.  */

		      if (ptr->rid != RID_EXTERN)
			error ("invalid modifier `%s' for language string",
			       ptr->name);
		      yylex ();
		      value = EXTERN_LANG_STRING;
		      yylval.ttype = get_identifier (TREE_STRING_POINTER (yylval.ttype));
		      break;
		    }
		  yylval.ttype = old_ttype;
		}
	      value = (int) ptr->token;
	    }
	  not_reserved_word_after_all:
	  ;
	}

	/* If we did not find a keyword, look for an identifier
	   (or a typename).  */

#ifdef FIELD_XREF
	if (value == IDENTIFIER || value == TYPESPEC)
	  FIELD_xref_ref(current_function_decl,token_buffer);
#endif
	if (value == IDENTIFIER)
	  {
	    tmp = get_identifier (token_buffer);
#ifndef VMS
	    /* Make sure that user does not collide with our internal
	       naming scheme.  */
	    if (JOINER == '$'
		&& dollar_seen
		&& (THIS_NAME_P (tmp)
		    || VPTR_NAME_P (tmp)
		    || DESTRUCTOR_NAME_P (tmp)
		    || WRAPPER_OR_ANTI_WRAPPER_NAME_P (tmp)
		    || OPERATOR_NAME_P (tmp)
		    || VTABLE_NAME_P (tmp)
		    || OPERATOR_TYPENAME_P (tmp)
		    || TEMP_NAME_P (tmp)
		    || ANON_AGGRNAME_P (tmp)
		    || ANON_PARMNAME_P (tmp)))
	      warning ("identifier name `%s' conflicts with GNU C++ internal naming strategy",
		       token_buffer);
#endif

	    /* Come into here if we must reprocess an identifier.  */
	  resume_identifier_processing:

	    if (looking_for_typename == 1
		&& TREE_TYPE (tmp) != 0)
	      lastiddecl = TREE_TYPE (tmp);
	    else lastiddecl = lookup_name (tmp);

	    if (lastiddecl && TREE_CODE (lastiddecl) == TYPE_DECL
		&& looking_for_typename >= 0)
	      {
		/* This call could blow away yylval.  */

		c = skip_white_space (nextchar);
		if (c == ':')
		  {
		    c = getch ();
		    if (c == ':')
		      {
			nextchar = -1;
			value = TYPENAME_SCOPE;
		      }
		    else
		      {
			nextchar = c;
			value = TYPENAME_COLON;
		      }
		  }
		else if (c == '.'
			 && current_function_decl == NULL_TREE
			 && current_class_type == NULL_TREE)
		  {
		    c = getch ();
		    if (c == '.')
		      {
			nextchar = -1;
			c = getch ();
			if (c != '.')
			  error ("missing '.' in `...'");
			value = TYPENAME_ELLIPSIS;
			tmp = build_tree_list (NULL_TREE, build_tree_list (TREE_TYPE (lastiddecl), NULL_TREE));
		      }
		    else
		      {
			nextchar = c;
			warning ("use of obsolete scope operator `.'; use `::' instead");
			value = TYPENAME_SCOPE;
		      }
		    looking_for_typename = 0;
		  }
		else
		  {
		    nextchar = c;
		    value = TYPENAME;
		    if (looking_for_typename == 1)
		      {
			looking_for_typename = 0;
#if 0
			yylval.ttype = TREE_TYPE (lastiddecl);
			break;
#endif
		      }
		  }
	      }
	    else if (lastiddecl == 0 && flag_labels_ok)
	      lastiddecl = IDENTIFIER_LABEL_VALUE (tmp);

	    yylval.ttype = tmp;
	  }
	if (value == NEW && ! global_bindings_p ())
	  {
	    looking_for_typename = 1;
	    value = NEW;
	    goto done;
	  }
      }
      break;

    case '0':  case '1':  case '2':  case '3':  case '4':
    case '5':  case '6':  case '7':  case '8':  case '9':
    case '.':
      {
	register char *p;
	int base = 10;
	int count = 0;
	int largest_digit = 0;
	int numdigits = 0;
	/* for multi-precision arithmetic,
	   we store only 8 live bits in each short,
	   giving us 64 bits of reliable precision */
	short shorts[8];

	enum anon1 { NOT_FLOAT, AFTER_POINT, TOO_MANY_POINTS} floatflag
	  = NOT_FLOAT;

	p = token_buffer;
	*p++ = c;

	/* Optimize for most frequent case.  */
	if (c == '0' || c == '1')
	  {
	    register int c1 = getch ();
	    if (! isalnum (c1) && c1 != '.')
	      {
		/* Terminate string.  */
		*p = 0;
		if (c == '0')
		  yylval.ttype = integer_zero_node;
		else
		  yylval.ttype = integer_one_node;
		nextchar = c1;
		value = CONSTANT;
		goto done;
	      }
	    ungetc (c1, finput);
	  }

	for (count = 0; count < 8; count++)
	  shorts[count] = 0;

	if (c == '0')
	  {
	    *p++ = (c = getch ());
	    if ((c == 'x') || (c == 'X'))
	      {
		base = 16;
		*p++ = (c = getch ());
	      }
	    else
	      {
		base = 8;
		numdigits++;
	      }
	  }

	/* Read all the digits-and-decimal-points.  */

	while (c == '.'
	       || (isalnum (c) && (c != 'l') && (c != 'L')
		   && (c != 'u') && (c != 'U')
		   && (floatflag == NOT_FLOAT || ((c != 'f') && (c != 'F')))))
	  {
	    if (c == '.')
	      {
		if (base == 16)
		  error ("floating constant may not be in radix 16");
		if (floatflag == AFTER_POINT)
		  {
		    error ("malformed floating constant");
		    floatflag = TOO_MANY_POINTS;
		  }
		else
		  floatflag = AFTER_POINT;

		base = 10;
		*p++ = c = getch ();
		/* Accept '.' as the start of a floating-point number
		   only when it is followed by a digit.
		   Otherwise, unread the following non-digit
		   and use the '.' as a structural token.  */
		if (p == token_buffer + 2 && !isdigit (c))
		  {
		    if (c == '.')
		      {
			c = getch ();
			if (c == '.')
			  {
			    *p++ = '.';
			    *p = '\0';
			    value = ELLIPSIS;
			    goto done;
			  }
			nextchar = c;
			token_buffer[2] = '\0';
			value = RANGE;
			goto done;
		      }
		    nextchar = c;
		    token_buffer[1] = '\0';
		    value = '.';
		    goto done;
		  }
	      }
	    else
	      {
		/* It is not a decimal point.
		   It should be a digit (perhaps a hex digit).  */

		if (isdigit (c))
		  {
		    c = c - '0';
		  }
		else if (base <= 10)
		  {
		    if ((c&~040) == 'E')
		      {
			base = 10;
			floatflag = AFTER_POINT;
			break;   /* start of exponent */
		      }
		    error ("nondigits in number and not hexadecimal");
		    c = 0;
		  }
		else if (c >= 'a')
		  {
		    c = c - 'a' + 10;
		  }
		else
		  {
		    c = c - 'A' + 10;
		  }
		if (c >= largest_digit)
		  largest_digit = c;
		numdigits++;

		for (count = 0; count < 8; count++)
		  {
		    (shorts[count] *= base);
		    if (count)
		      {
			shorts[count] += (shorts[count-1] >> 8);
			shorts[count-1] &= (1<<8)-1;
		      }
		    else shorts[0] += c;
		  }

		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = (c = getch ());
	      }
	  }

	if (numdigits == 0)
	  error ("numeric constant with no digits");

	if (largest_digit >= base)
	  error ("numeric constant contains digits beyond the radix");

	/* Remove terminating char from the token buffer and delimit the string */
	*--p = 0;

	if (floatflag != NOT_FLOAT)
	  {
	    tree type = double_type_node;
	    char f_seen = 0;
	    char l_seen = 0;
	    double value;

	    /* Read explicit exponent if any, and put it in tokenbuf.  */

	    if ((c == 'e') || (c == 'E'))
	      {
		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = c;
		c = getch ();
		if ((c == '+') || (c == '-'))
		  {
		    *p++ = c;
		    c = getch ();
		  }
		if (! isdigit (c))
		  error ("floating constant exponent has no digits");
	        while (isdigit (c))
		  {
		    if (p >= token_buffer + maxtoken - 3)
		      p = extend_token_buffer (p);
		    *p++ = c;
		    c = getch ();
		  }
	      }

	    *p = 0;
	    errno = 0;
	    value = atof (token_buffer);
#ifdef ERANGE
	    if (errno == ERANGE && !flag_traditional)
	      {
		char *p1 = token_buffer;
		/* Check for "0.0" and variants;
		   Sunos 4 spuriously returns ERANGE for them.  */
		while (*p1 == '0') p1++;
		if (*p1 == '.') p1++;
		while (*p1 == '0') p1++;
		if (*p1 != 0)
		  warning ("floating point number exceeds range of `double'");
	      }
#endif

	    /* Read the suffixes to choose a data type.  */
	    while (1)
	      {
		if (c == 'f' || c == 'F')
		  {
		    if (f_seen)
		      error ("two `f's in floating constant");
		    f_seen = 1;
		    type = float_type_node;
		  }
		else if (c == 'l' || c == 'L')
		  {
		    if (l_seen)
		      error ("two `l's in floating constant");
		    l_seen = 1;
		    type = long_double_type_node;
		  }
		else
		  {
		    if (isalnum (c))
		      {
			error ("garbage at end of number");
			while (isalnum (c))
			  {
			    if (p >= token_buffer + maxtoken - 3)
			      p = extend_token_buffer (p);
			    *p++ = c;
			    c = getch ();
			  }
		      }
		    break;
		  }
		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = c;
		c = getch ();
	      }

	    /* Create a node with determined type and value.  */
	    yylval.ttype = build_real (type, value);

	    ungetc (c, finput);
	    *p = 0;
	  }
	else
	  {
	    tree type;
	    int spec_unsigned = 0;
	    int spec_long = 0;

	    while (1)
	      {
		if (c == 'u' || c == 'U')
		  {
		    if (spec_unsigned)
		      error ("two `u's in integer constant");
		    spec_unsigned = 1;
		  }
		else if (c == 'l' || c == 'L')
		  {
		    if (spec_long)
		      error ("two `l's in integer constant");
		    spec_long = 1;
		  }
		else
		  {
		    if (isalnum (c))
		      {
			error ("garbage at end of number");
			while (isalnum (c))
			  {
			    if (p >= token_buffer + maxtoken - 3)
			      p = extend_token_buffer (p);
			    *p++ = c;
			    c = getch ();
			  }
		      }
		    break;
		  }
		if (p >= token_buffer + maxtoken - 3)
		  p = extend_token_buffer (p);
		*p++ = c;
		c = getch ();
	      }

	    ungetc (c, finput);

	    if (shorts[7] | shorts[6] | shorts[5] | shorts[4])
	      warning ("integer constant out of range");

	    /* This is simplified by the fact that our constant
	       is always positive.  */
	    yylval.ttype
	      = build_int_2 ((shorts[3]<<24) + (shorts[2]<<16) + (shorts[1]<<8) + shorts[0],
			     0);

	    if (!spec_long && !spec_unsigned
		&& int_fits_type_p (yylval.ttype, integer_type_node))
	      type = integer_type_node;

	    else if (!spec_long && base != 10
		     && int_fits_type_p (yylval.ttype, unsigned_type_node))
	      type = unsigned_type_node;

	    else if (!spec_unsigned
		     && int_fits_type_p (yylval.ttype, long_integer_type_node))
	      type = long_integer_type_node;

	    else
	      {
		type = long_unsigned_type_node;
		if (! int_fits_type_p (yylval.ttype, long_unsigned_type_node))
		  warning ("integer constant out of range");
	      }
	    TREE_TYPE (yylval.ttype) = type;
	  }

	value = CONSTANT; break;
      }

    case '\'':
    char_constant:
      {
	register int result = 0;
	register num_chars = 0;
	int width = TYPE_PRECISION (char_type_node);
	int max_chars;

	if (wide_flag) width = TYPE_PRECISION (integer_type_node);

	max_chars = TYPE_PRECISION (integer_type_node) / width;

	while (1)
	  {
	  tryagain:

	    c = getch ();

	    if (c == '\'' || c == EOF)
	      break;

	    if (c == '\\')
	      {
		c = readescape ();
		if (c < 0)
		  goto tryagain;
		if (width < HOST_BITS_PER_INT
		    && (unsigned) c >= (1 << width))
		  warning ("escape sequence out of range for character");
	      }
	    else if (c == '\n')
	      {
		if (pedantic)
		  warning ("ANSI C forbids newline in character constant");
		lineno++;
	      }

	    num_chars++;
	    if (num_chars > maxtoken - 4)
	      extend_token_buffer (token_buffer);

	    token_buffer[num_chars] = c;

	    /* Merge character into result; ignore excess chars.  */
	    if (num_chars < max_chars + 1)
	      {
		if (width < HOST_BITS_PER_INT)
		  result = (result << width) | (c & ((1 << width) - 1));
		else
		  result = c;
	      }
	  }

	token_buffer[num_chars + 1] = '\'';
	token_buffer[num_chars + 2] = 0;

	if (c != '\'')
	  error ("malformatted character constant");
	else if (num_chars == 0)
	  error ("empty character constant");
	else if (num_chars > max_chars)
	  {
	    num_chars = max_chars;
	    error ("character constant too long");
	  }
	else if (num_chars != 1 && ! flag_traditional)
	  warning ("multi-character character constant");

	/* If char type is signed, sign-extend the constant.  */
	if (! wide_flag)
	  {
	    int num_bits = num_chars * width;
	    if (TREE_UNSIGNED (char_type_node)
		|| ((result >> (num_bits - 1)) & 1) == 0)
	      yylval.ttype
		= build_int_2 (result & ((unsigned) ~0
					 >> (HOST_BITS_PER_INT - num_bits)),
			       0);
	    else
	      yylval.ttype
		= build_int_2 (result | ~((unsigned) ~0
					  >> (HOST_BITS_PER_INT - num_bits)),
			       -1);
	    TREE_TYPE (yylval.ttype) = char_type_node;
	  }
	else
	  {
	    yylval.ttype = build_int_2 (result, 0);
	    TREE_TYPE (yylval.ttype) = integer_type_node;
	  }
	value = CONSTANT; break;
      }

    case '"':
    string_constant:
      {
	int *widep;
	register char *p;

	c = getch ();
	p = token_buffer + 1;

	if (wide_flag)
	  widep = wide_buffer;

	while (c != '"' && c >= 0)
	  {
	    if (c == '\\')
	      {
		c = readescape ();
		if (c < 0)
		  goto skipnewline;
		if (!wide_flag && c >= (1 << BITS_PER_UNIT))
		  warning ("escape sequence out of range for character");
	      }
	    else if (c == '\n')
	      {
		if (pedantic)
		  warning ("ANSI C forbids newline in string constant");
		lineno++;
	      }

	    /* Store the char in C into the appropriate buffer.  */

	    if (wide_flag)
	      {
		if (widep == wide_buffer + max_wide)
		  {
		    int n = widep - wide_buffer;
		    max_wide *= 2;
		    wide_buffer = (int *) xrealloc (wide_buffer, max_wide + 1);
		    widep = wide_buffer + n;
		  }
		*widep++ = c;
	      }
	    else
	      {
		if (p == token_buffer + maxtoken)
		  p = extend_token_buffer (p);
		*p++ = c;
	      }

	  skipnewline:
	    c = getch ();
	    if (c == EOF) {
		error("Unterminated string");
		break;
	    }
	  }

	/* We have read the entire constant.
	   Construct a STRING_CST for the result.  */

	if (wide_flag)
	  {
	    /* If this is a L"..." wide-string, make a vector
	       of the ints in wide_buffer.  */
	    *widep = 0;
	    /* We have not implemented the case where `int'
	       on the target and on the execution machine differ in size.  */
	    assert (TYPE_PRECISION (integer_type_node) == sizeof (int) * BITS_PER_UNIT);
	    yylval.ttype = build_string ((widep - wide_buffer) * sizeof (int),
					 (char *)wide_buffer);
	    TREE_TYPE (yylval.ttype) = int_array_type_node;
	  }
	else
	  {
	    *p = 0;
	    yylval.ttype = build_string (p - token_buffer, token_buffer + 1);
	    TREE_TYPE (yylval.ttype) = char_array_type_node;
	  }

	*p++ = '"';
	*p = 0;

	value = STRING; break;
      }

    case '+':
    case '-':
    case '&':
    case '|':
    case '<':
    case '>':
    case '*':
    case '/':
    case '%':
    case '^':
    case '!':
    case '=':
      {
	register int c1;

      combine:

	switch (c)
	  {
	  case '+':
	    yylval.code = PLUS_EXPR; break;
	  case '-':
	    yylval.code = MINUS_EXPR; break;
	  case '&':
	    yylval.code = BIT_AND_EXPR; break;
	  case '|':
	    yylval.code = BIT_IOR_EXPR; break;
	  case '*':
	    yylval.code = MULT_EXPR; break;
	  case '/':
	    yylval.code = TRUNC_DIV_EXPR; break;
	  case '%':
	    yylval.code = TRUNC_MOD_EXPR; break;
	  case '^':
	    yylval.code = BIT_XOR_EXPR; break;
	  case LSHIFT:
	    yylval.code = LSHIFT_EXPR; break;
	  case RSHIFT:
	    yylval.code = RSHIFT_EXPR; break;
	  case '<':
	    yylval.code = LT_EXPR; break;
	  case '>':
	    yylval.code = GT_EXPR; break;
	  }

	token_buffer[1] = c1 = getch ();
	token_buffer[2] = 0;

	if (c1 == '=')
	  {
	    switch (c)
	      {
	      case '<':
		value = ARITHCOMPARE; yylval.code = LE_EXPR; goto done;
	      case '>':
		value = ARITHCOMPARE; yylval.code = GE_EXPR; goto done;
	      case '!':
		value = EQCOMPARE; yylval.code = NE_EXPR; goto done;
	      case '=':
		value = EQCOMPARE; yylval.code = EQ_EXPR; goto done;
	      }
	    value = ASSIGN; goto done;
	  }
	else if (c == c1)
	  switch (c)
	    {
	    case '+':
	      value = PLUSPLUS; goto done;
	    case '-':
	      value = MINUSMINUS; goto done;
	    case '&':
	      value = ANDAND; goto done;
	    case '|':
	      value = OROR; goto done;
	    case '<':
	      c = LSHIFT;
	      goto combine;
	    case '>':
	      c = RSHIFT;
	      goto combine;
	    }
	else if ((c == '-') && (c1 == '>'))
	  {
	    nextchar = skip_white_space (getch ());
	    if (nextchar == '(')
	      {
		int next_c = skip_white_space (getch ());
		if (next_c == ')')
		  {
		    nextchar = -1;
		    value = POINTSAT_LEFT_RIGHT;
		    goto done;
		  }
		ungetc (next_c, finput);
	      }
	    value = POINTSAT;
	    goto done;
	  }
	else if (c1 == '?' && (c == '<' || c == '>'))
	  {
	    token_buffer[3] = 0;

	    c1 = getch ();
	    yylval.code = (c == '<' ? MIN_EXPR : MAX_EXPR);
	    if (c1 == '=')
	      {
		/* <?= or >?= expression.  */
		token_buffer[2] = c1;
		value = ASSIGN;
	      }
	    else
	      {
		value = MIN_MAX;
		nextchar = c1;
	      }
	    if (pedantic)
	      error ("use of `operator %s' is not standard C++",
		     token_buffer);
	    goto done;
	  }

	nextchar = c1;
	token_buffer[1] = 0;

	if ((c == '<') || (c == '>'))
	  value = ARITHCOMPARE;
	else value = c;
	goto done;
      }

    case ':':
      c = getch ();
      if (c == ':')
	{
	  token_buffer[1] = ':';
	  token_buffer[2] = '\0';
	  value = SCOPE;
	  yylval.itype = 1;
	}
      else
	{
	  nextchar = c;
	  value = ':';
	}
      break;

    case 0:
      /* Don't make yyparse think this is eof.  */
      value = 1;
      break;

    case '(':
      /* try, weakly, to handle casts to pointers to functions.  */
      nextchar = skip_white_space (getch ());
      if (nextchar == '*')
	{
	  int next_c = skip_white_space (getch ());
	  if (next_c == ')')
	    {
	      nextchar = -1;
	      yylval.ttype = build1 (INDIRECT_REF, 0, 0);
	      value = PAREN_STAR_PAREN;
	    }
	  else
	    {
	      ungetc (next_c, finput);
	      value = c;
	    }
	}
      /* Go down for a (X::*) or (X::&).  */
      else if (isalpha (nextchar) || nextchar == '_' || nextchar == '$')
	{
	  YYSTYPE this_yylval = yylval;
	  tree this_lastiddecl = lastiddecl;
	  nextyychar = yylex ();
	  if (nextyychar == TYPENAME_SCOPE)
	    {
	      if (nextchar < 0)
		nextchar = skip_white_space (getch ());
	      if (nextchar == '*' || nextchar == '&')
		{
		  int next_c = skip_white_space (getch ());
		  if (next_c == ')')
		    {
		      nextyychar = -1;
		      if (nextchar == '*')
			{
			  value = PAREN_X_SCOPE_STAR_PAREN;
			  yylval.ttype = build_parse_node (SCOPE_REF, yylval.ttype,
							   build_parse_node (INDIRECT_REF, 0));
			}
		      else
			{
			  value = PAREN_X_SCOPE_REF_PAREN;
			  yylval.ttype = build_parse_node (SCOPE_REF, yylval.ttype,
							   build_parse_node (ADDR_EXPR, 0));
			}
		      nextchar = -1;
		    }
		  else
		    {
		      ungetc (next_c, finput);
		      nextyylval = yylval;
		      nextlastiddecl = lastiddecl;
		      yylval = this_yylval;
		      lastiddecl = this_lastiddecl;
		      value = c;
		    }
		}
	      else
		{
		  nextyylval = yylval;
		  nextlastiddecl = lastiddecl;
		  yylval = this_yylval;
		  lastiddecl = this_lastiddecl;
		  value = c;
		}
	    }
	  else
	    {
	      nextyylval = yylval;
	      nextlastiddecl = lastiddecl;
	      yylval = this_yylval;
	      lastiddecl = this_lastiddecl;
	      value = c;
	    }
	}
      else if (nextchar == ')')
	{
	  nextchar = -1;
	  yylval.ttype = NULL_TREE;
	  value = LEFT_RIGHT;
	}
      else value = c;
      break;

    default:
      value = c;
    }

done:
/*  yylloc.last_line = lineno; */
#ifdef GATHER_STATISTICS
  token_count[value] += 1;
#endif

  return value;
}

typedef enum
{
  d_kind, t_kind, s_kind, r_kind, e_kind, c_kind,
  id_kind, op_id_kind, perm_list_kind, temp_list_kind,
  x_kind, lang_decl, lang_type, all_kinds
} tree_node_kind;
extern int tree_node_kinds[];
extern int tree_node_sizes[];
extern char *tree_node_kind_names[];

/* Place to save freed lang_decls which were allocated on the
   permanent_obstack.  @@ Not currently used.  */
tree free_lang_decl_chain;

tree
build_lang_decl (code, name, type)
     enum tree_code code;
     tree name;
     tree type;
{
  extern struct obstack *current_obstack, *saveable_obstack;
  extern struct obstack permanent_obstack;
  register tree t = build_decl (code, name, type);
  struct obstack *obstack = current_obstack;
  register int i = sizeof (struct lang_decl) / sizeof (int);
  register int *pi;

  if (! TREE_PERMANENT (t))
    obstack = saveable_obstack;

#ifdef LANG_DECL_PERMANENT
  if (free_lang_decl_chain && obstack == &permanent_obstack)
    {
      pi = (int *)free_lang_decl_chain;
      free_lang_decl_chain = TREE_CHAIN (free_lang_decl_chain);
    }
  else
    pi = (int *) obstack_alloc (obstack, sizeof (struct lang_decl));
#else
  pi = (int *) obstack_alloc (obstack, sizeof (struct lang_decl));
#endif

  while (i > 0)
    pi[--i] = 0;

  DECL_LANG_SPECIFIC (t) = (struct lang_decl *) pi;
#ifdef LANG_DECL_PERMANENT
  LANG_DECL_PERMANENT ((struct lang_decl *) pi)
    = obstack == &permanent_obstack;
#endif
  DECL_MAIN_VARIANT (t) = t;
  DECL_ORIGINAL_NAME (t) = name;
  if (current_lang_name == lang_name_cplusplus)
    {
      DECL_LANGUAGE (t) = lang_cplusplus;

#ifndef NO_AUTO_OVERLOAD
      if (code == FUNCTION_DECL && name != 0
	  && ! (IDENTIFIER_LENGTH (name) == 4
		&& IDENTIFIER_POINTER (name)[0] == 'm'
		&& strcmp (IDENTIFIER_POINTER (name), "main") == 0)
	  && ! (IDENTIFIER_LENGTH (name) > 10
		&& IDENTIFIER_POINTER (name)[0] == '_'
		&& IDENTIFIER_POINTER (name)[1] == '_'
		&& strncmp (IDENTIFIER_POINTER (name)+2, "builtin_", 8) == 0))
	TREE_OVERLOADED (name) = 1;
#endif
    }
  else if (current_lang_name == lang_name_c)
    DECL_LANGUAGE (t) = lang_c;
  else abort ();

#ifdef GATHER_STATISTICS
  tree_node_kinds[(int)lang_decl] += 1;
  tree_node_sizes[(int)lang_decl] += sizeof(struct lang_decl);
#endif

  return t;
}

tree
build_lang_field_decl (code, name, type)
     enum tree_code code;
     tree name;
     tree type;
{
  extern struct obstack *current_obstack, *saveable_obstack;
  register tree t = build_decl (code, name, type);
  struct obstack *obstack = current_obstack;
  register int i = sizeof (struct lang_decl_flags) / sizeof (int);
  register int *pi;

  if (! TREE_PERMANENT (t))
    obstack = saveable_obstack;

  pi = (int *) obstack_alloc (obstack, sizeof (struct lang_decl_flags));
  while (i > 0)
    pi[--i] = 0;

  DECL_LANG_SPECIFIC (t) = (struct lang_decl *) pi;
  return t;
}

tree
make_lang_type (code)
     enum tree_code code;
{
  extern struct obstack *current_obstack, *saveable_obstack;
  register tree t = make_node (code);
  struct obstack *obstack = current_obstack;
  register int i = sizeof (struct lang_type) / sizeof (int);
  register int *pi;

  if (! TREE_PERMANENT (t))
    obstack = saveable_obstack;

  pi = (int *) obstack_alloc (obstack, sizeof (struct lang_type));
  while (i > 0)
    pi[--i] = 0;

  TYPE_LANG_SPECIFIC (t) = (struct lang_type *) pi;
  CLASSTYPE_MAIN_VARIANT (t) = t;
  CLASSTYPE_AS_LIST (t) = build_tree_list (NULL_TREE, t);
  CLASSTYPE_INTERFACE_UNKNOWN (t) = interface_unknown;
  CLASSTYPE_INTERFACE_ONLY (t) = interface_only;

  /* Make sure this is laid out, for ease of use later.
     In the presence of parse errors, the normal was of assuring
     this might not ever get executed, so we lay it out *immediately*.  */
  build_pointer_type (t);

#ifdef GATHER_STATISTICS
  tree_node_kinds[(int)lang_type] += 1;
  tree_node_sizes[(int)lang_type] += sizeof(struct lang_type);
#endif

  return t;
}

void
copy_decl_lang_specific (decl)
     tree decl;
{
  extern struct obstack *current_obstack, *saveable_obstack;
  register int *old = (int *)DECL_LANG_SPECIFIC (decl);
  struct obstack *obstack = current_obstack;
  register int i = sizeof (struct lang_decl) / sizeof (int);
  register int *pi;

  if (! TREE_PERMANENT (decl))
    obstack = saveable_obstack;

  pi = (int *) obstack_alloc (obstack, sizeof (struct lang_decl));
  while (i-- > 0)
    pi[i] = old[i];

  DECL_LANG_SPECIFIC (decl) = (struct lang_decl *) pi;

#ifdef GATHER_STATISTICS
  tree_node_kinds[(int)lang_decl] += 1;
  tree_node_sizes[(int)lang_decl] += sizeof(struct lang_decl);
#endif
}

void
copy_type_lang_specific (type)
     tree type;
{
  extern struct obstack *current_obstack, *saveable_obstack;
  register int *old = (int *)TYPE_LANG_SPECIFIC (type);
  struct obstack *obstack = current_obstack;
  register int i = sizeof (struct lang_type) / sizeof (int);
  register int *pi;

  if (! TREE_PERMANENT (type))
    obstack = saveable_obstack;

  pi = (int *) obstack_alloc (obstack, sizeof (struct lang_type));
  while (i-- > 0)
    pi[i] = old[i];

  TYPE_LANG_SPECIFIC (type) = (struct lang_type *) pi;
  CLASSTYPE_AS_LIST (type) = build_tree_list (NULL_TREE, type);
  if (CLASSTYPE_N_BASECLASSES (type) > 0)
    CLASSTYPE_BASECLASSES (type) = (tree *)obstack_copy (obstack, CLASSTYPE_BASECLASSES (type), (CLASSTYPE_N_BASECLASSES (type)+1) * sizeof (tree));

#ifdef GATHER_STATISTICS
  tree_node_kinds[(int)lang_type] += 1;
  tree_node_sizes[(int)lang_type] += sizeof(struct lang_type);
#endif
}

tree
build_with_cleanup (exp, type, rtl)
     tree exp;
     tree type;
     struct rtx_def *rtl;
{
  if (type != NULL_TREE || TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (exp)))
    {
      tree rval = make_node (WITH_CLEANUP_EXPR);

      if (type == NULL_TREE)
	type = TREE_TYPE (exp);

      TREE_OPERAND (rval, 0) = exp;
      TREE_OPERAND (rval, 1) = make_node (RTL_EXPR);
      TREE_OPERAND (rval, 2) = build_delete (TYPE_POINTER_TO (type),
					     build1 (ADDR_EXPR, TYPE_POINTER_TO (type), TREE_OPERAND (rval, 1)),
					     integer_two_node, LOOKUP_NORMAL, 0);
      if (rtl != 0)
	RTL_EXPR_RTL (TREE_OPERAND (rval, 1)) = rtl;
      if (TREE_CODE (exp) == CALL_EXPR
	  && TREE_VALUE (TREE_OPERAND (exp, 1)) == NULL_TREE)
	TREE_VALUE (TREE_OPERAND (exp, 1)) = TREE_OPERAND (rval, 1);
      TREE_TYPE (rval) = type;
      return rval;
    }
  return NULL_TREE;
}

void
dump_time_statistics ()
{
  register tree prev = 0, decl, next;
  int this_time = my_gettime ();
  TREE_INT_CST_LOW (IDENTIFIER_LOCAL_VALUE (this_filename_time))
    += this_time - body_time;

  fprintf (stderr, "\n******\n");
  print_time ("header files (total)", header_time);
  print_time ("main file (total)", this_time - body_time);
  fprintf (stderr, "ratio = %g : 1\n",
	   (double)header_time / (double)(this_time - body_time));
  fprintf (stderr, "\n******\n");

  for (decl = filename_times; decl; decl = next)
    {
      next = IDENTIFIER_GLOBAL_VALUE (decl);
      IDENTIFIER_GLOBAL_VALUE (decl) = prev;
      prev = decl;
    }

  for (decl = prev; decl; decl = IDENTIFIER_GLOBAL_VALUE (decl))
    print_time (IDENTIFIER_POINTER (decl),
		TREE_INT_CST_LOW (IDENTIFIER_LOCAL_VALUE (decl)));
}

void
compiler_error (s, v, v2)
     char *s;
     int v, v2;			/* @@also used as pointer */
{
  char buf[1024];
  sprintf (buf, s, v, v2);
  error_with_file_and_line (input_filename, lineno, "%s (compiler error)", buf);
}

void
compiler_error_with_decl (decl, s)
     tree decl;
     char *s;
{
  char *name;
  count_error (0);

  report_error_function (0);

  if (TREE_CODE (decl) == PARM_DECL)
    fprintf (stderr, "%s:%d: ",
	     DECL_SOURCE_FILE (DECL_CONTEXT (decl)),
	     DECL_SOURCE_LINE (DECL_CONTEXT (decl)));
  else
    fprintf (stderr, "%s:%d: ",
	     DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));

  name = lang_printable_name (decl);
  if (name)
    fprintf (stderr, s, name);
  else
    fprintf (stderr, s, "((anonymous))");
  fprintf (stderr, " (compiler error)\n");
}
