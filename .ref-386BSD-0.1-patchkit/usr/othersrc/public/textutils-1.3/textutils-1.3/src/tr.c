/* tr -- a filter to translate characters
   Copyright (C) 1991 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by Jim Meyering. */

#define _GNU_SOURCE
#include <ctype.h>
#ifndef isblank
#define isblank(c) ((c) == ' ' || (c) == '\t')
#endif
#ifndef isgraph
#define isgraph(c) (isprint (c) && !isspace (c))
#endif
#include <stdio.h>
#include <assert.h>
#include <errno.h>
#include <sys/types.h>
#include "getopt.h"
#include "system.h"

#ifndef LONG_MAX
#define LONG_MAX 0x7FFFFFFF
#endif

#ifndef UCHAR_MAX
#define UCHAR_MAX 0xFF
#endif

#define N_CHARS (UCHAR_MAX + 1)

/* A pointer to a function that returns an int. */
typedef int (*PFI) ();

/* Convert from character C to its index in the collating
   sequence array.  Just cast to an unsigned int to avoid
   problems with sign-extension. */
#define ORD(c) (unsigned int)(c)

/* The inverse of ORD. */
#define CHR(i) (unsigned char)(i)

/* The value for Spec_list->state that indicates to
   get_next that it should initialize the tail pointer.
   Its value doesn't matter as long as it can't be
   confused with a valid character code. */
#define BEGIN_STATE (2 * N_CHARS)

/* The value for Spec_list->state that indicates to
   get_next that the element pointed to by Spec_list->tail is
   being considered for the first time on this pass through the
   list -- it indicates that get_next should make any necessary
   initializations. */
#define NEW_ELEMENT (BEGIN_STATE + 1)

/* A value distinct from any character that may have been stored in a
   buffer as the result of a block-read in the function squeeze_filter. */
#define NOT_A_CHAR (unsigned int)(-1)

/* The following (but not CC_NO_CLASS) are indices into the array of
   valid character class strings. */
enum Char_class
{
  CC_ALNUM = 0, CC_ALPHA = 1, CC_BLANK = 2, CC_CNTRL = 3,
  CC_DIGIT = 4, CC_GRAPH = 5, CC_LOWER = 6, CC_PRINT = 7,
  CC_PUNCT = 8, CC_SPACE = 9, CC_UPPER = 10, CC_XDIGIT = 11,
  CC_NO_CLASS = 9999
};

/* Character class to which a character (returned by get_next) belonged;
   but it is set only if the construct from which the character was obtained
   was one of the character classes [:upper:] or [:lower:].  The value
   is used only when translating and then, only to make sure that upper
   and lower class constructs have the same relative positions in string1
   and string2. */
enum Upper_Lower_class
{
  UL_LOWER = 0,
  UL_UPPER = 1,
  UL_NONE = 2
};

/* A shortcut to ensure that when constructing the translation array,
   one of the values returned by paired calls to get_next (from s1 and s2) is
   from [:upper:] and the other is from [:lower:], or neither is
   from upper or lower.  In fact, no other character classes are allowed
   when translating, but that condition is tested elsewhere.  This array
   is indexed by values of type enum Upper_Lower_class. */
static int class_ok[3][3] =
{
  {0, 1, 0},
  {1, 0, 0},
  {0, 0, 1}
};

/* The type of a List_element.  See build_spec_list for more details. */
enum Range_element_type
{
  RE_NO_TYPE = 0,
  RE_NORMAL_CHAR,
  RE_RANGE,
  RE_CHAR_CLASS,
  RE_EQUIV_CLASS,
  RE_REPEATED_CHAR
};

/* One construct in one of tr's argument strings.
   For example, consider the POSIX version of the
   classic tr command:
       tr -cs 'a-zA-Z_' '[\n*]'
   String1 has 3 constructs, two of which are ranges (a-z and A-Z),
   and a single normal character, `_'.  String2 has one construct. */
struct List_element
{
  enum Range_element_type type;
  struct List_element *next;
  union
  {
    int normal_char;
    struct			/* unnamed */
    {
      unsigned int first_char;
      unsigned int last_char;
    } range;
    enum Char_class char_class;
    int equiv_code;
    struct			/* unnamed */
    {
      unsigned int the_repeated_char;
      long repeat_count;
    } repeated_char;
  } u;
};

/* Each of tr's argument strings is parsed into a form that is easier
   to work with: a linked list of constructs (struct List_element).
   Each Spec_list structure also encapsulates various attributes of
   the corresponding argument string.  The attributes are used mainly
   to verify that the strings are legal in the context of any options
   specified (like -s, -d, or -c).  The main exception is the member
   `tail', which is first used to construct the list.  After construction,
   it is used by get_next to save its state when traversing the list.
   The member `state' serves a similar function. */
struct Spec_list
{
  /* Points to the head of the list of range elements.
     The first struct is a dummy; its members are never used. */
  struct List_element *head;

  /* When appending, points to the last element.  When traversing via
     get_next(), points to the element to process next.  Setting
     Spec_list.state to the value BEGIN_STATE before calling get_next
     signals get_next to initialize tail to point to head->next. */
  struct List_element *tail;

  /* Used to save state between calls to get_next(). */
  unsigned int state;

  /* Length, in the sense that length('a-z[:digit:]123abc')
     is 42 ( = 26 + 10 + 6). */
  int length;

  /* The number of [c*] and [c*0] constructs that appear in this spec. */
  int n_indefinite_repeats;

  /* Non-zero if this spec contains at least one equivalence
     class construct e.g. [=c=]. */
  int has_equiv_class;

  /* Non-zero if this spec contains at least one of [:upper:] or
     [:lower:] class constructs. */
  int has_upper_or_lower;

  /* Non-zero if this spec contains at least one of the character class
     constructs (all but upper and lower) that aren't allowed in s2. */
  int has_restricted_char_class;
};

char *xmalloc ();
char *stpcpy ();
void error ();

/* The name by which this program was run. */
char *program_name;

/* When non-zero, each sequence in the input of a repeated character
   (call it c) is replaced (in the output) by a single occurrence of c
   for every c in the squeeze set. */
static int squeeze_repeats = 0;

/* When non-zero, removes characters in the delete set from input. */
static int delete = 0;

/* Use the complement of set1 in place of set1. */
static int complement = 0;

/* When non-zero, this flag causes GNU tr to provide strict
   compliance with POSIX draft 1003.2.11.2.  The POSIX spec
   says that when -d is used without -s, string2 (if present)
   must be ignored.  Silently ignoring arguments is a bad idea.
   The default GNU behavior is to give a usage message and exit.
   Additionally, when this flag is non-zero, tr prints warnings
   on stderr if it is being used in a manner that is not portable.
   Applicable warnings are given by default, but are suppressed
   if the environment variable `POSIXLY_CORRECT' is set, since
   being POSIX conformant means we can't issue such messages.
   Warnings on the following topics are suppressed when this
   variable is non-zero:
   1. Ambiguous octal escapes. */
static int posix_pedantic;

/* When tr is performing translation and string1 is longer than string2,
   POSIX says that the result is undefined.  That gives the implementor
   of a POSIX conforming version of tr two reasonable choices for the
   semantics of this case.

   * The BSD tr pads string2 to the length of string1 by
   repeating the last character in string2.

   * System V tr ignores characters in string1 that have no
   corresponding character in string2.  That is, string1 is effectively
   truncated to the length of string2.

   When non-zero, this flag causes GNU tr to imitate the behavior
   of System V tr when translating with string1 longer than string2.
   The default is to emulate BSD tr.  This flag is ignored in modes where
   no translation is performed.  Emulating the System V tr
   in this exceptional case causes the relatively common BSD idiom:

       tr -cs A-Za-z0-9 '\012'

   to break (it would convert only zero bytes, rather than all
   non-alphanumerics, to newlines).

   WARNING: This switch does not provide general BSD or System V
   compatibility.  For example, it doesn't disable the interpretation
   of the POSIX constructs [:alpha:], [=c=], and [c*10], so if by
   some unfortunate coincidence you use such constructs in scripts
   expecting to use some other version of tr, the scripts will break. */
static int truncate_set1 = 0;

/* An alias for (!delete && non_option_args == 2).
   It is set in main and used there and in validate(). */
static int translating;

#ifndef BUFSIZ
#define BUFSIZ 8192
#endif

#define IO_BUF_SIZE BUFSIZ
static unsigned char io_buf[IO_BUF_SIZE];

char *char_class_name[] =
{
  "alnum", "alpha", "blank", "cntrl", "digit", "graph",
  "lower", "print", "punct", "space", "upper", "xdigit"
};
#define N_CHAR_CLASSES (sizeof(char_class_name) / sizeof(char_class_name[0]))

typedef char SET_TYPE;

/* Array of boolean values.  A character `c' is a member of the
   squeeze set if and only if in_squeeze_set[c] is true.  The squeeze
   set is defined by the last (possibly, the only) string argument
   on the command line when the squeeze option is given.  */
static SET_TYPE in_squeeze_set[N_CHARS];

/* Array of boolean values.  A character `c' is a member of the
   delete set if and only if in_delete_set[c] is true.  The delete
   set is defined by the first (or only) string argument on the
   command line when the delete option is given.  */
static SET_TYPE in_delete_set[N_CHARS];

/* Array of character values defining the translation (if any) that
   tr is to perform.  Translation is performed only when there are
   two specification strings and the delete switch is not given. */
static char xlate[N_CHARS];

static struct option long_options[] =
{
  {"complement", 0, NULL, 'c'},
  {"delete", 0, NULL, 'd'},
  {"squeeze-repeats", 0, NULL, 's'},
  {"truncate-set1", 0, NULL, 't'},
  {NULL, 0, NULL, 0}
};


static void
usage ()
{
  fprintf (stderr, "\
Usage: %s [-cdst] [--complement] [--delete] [--squeeze-repeats]\n\
       [--truncate-set1] string1 [string2]\n",
	   program_name);
  exit (2);
}

/* Return non-zero if the character C is a member of the
   equivalence class containing the character EQUIV_CLASS. */

static int
is_equiv_class_member (equiv_class, c)
     unsigned int equiv_class;
     unsigned int c;
{
  return (equiv_class == c);
}

/* Return non-zero if the character C is a member of the
   character class CHAR_CLASS. */

static int
is_char_class_member (char_class, c)
     enum Char_class char_class;
     unsigned int c;
{
  switch (char_class)
    {
    case CC_ALNUM:
      return isalnum (c);
      break;
    case CC_ALPHA:
      return isalpha (c);
      break;
    case CC_BLANK:
      return isblank (c);
      break;
    case CC_CNTRL:
      return iscntrl (c);
      break;
    case CC_DIGIT:
      return isdigit (c);
      break;
    case CC_GRAPH:
      return isgraph (c);
      break;
    case CC_LOWER:
      return islower (c);
      break;
    case CC_PRINT:
      return isprint (c);
      break;
    case CC_PUNCT:
      return ispunct (c);
      break;
    case CC_SPACE:
      return isspace (c);
      break;
    case CC_UPPER:
      return isupper (c);
      break;
    case CC_XDIGIT:
      return isxdigit (c);
      break;
    case CC_NO_CLASS:
      abort ();
      return 0;
      break;
    }
}

/* Perform the first pass over each range-spec argument S,
   converting all \c and \ddd escapes to their one-byte representations.
   The conversion is done in-place, so S must point to writable
   storage.  If an illegal quote sequence is found, an error message is
   printed and the function returns non-zero.  Otherwise the length of
   the resulting string is returned through LEN and the function returns 0.
   The resulting array of characters may contain zero-bytes; however,
   on input, S is assumed to be null-terminated, and hence
   cannot contain actual (non-escaped) zero bytes. */

static int
unquote (s, len)
     unsigned char *s;
     int *len;
{
  int i, j;

  j = 0;
  for (i = 0; s[i]; i++)
    {
      switch (s[i])
	{
	  int c;
	case '\\':
	  switch (s[i + 1])
	    {
	      int oct_digit;
	    case '\\':
	      c = '\\';
	      break;
	    case 'a':
	      c = '\007';
	      break;
	    case 'b':
	      c = '\b';
	      break;
	    case 'f':
	      c = '\f';
	      break;
	    case 'n':
	      c = '\n';
	      break;
	    case 'r':
	      c = '\r';
	      break;
	    case 't':
	      c = '\t';
	      break;
	    case 'v':
	      c = '\v';
	      break;
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	      c = s[i + 1] - '0';
	      oct_digit = s[i + 2] - '0';
	      if (0 <= oct_digit && oct_digit <= 7)
		{
		  c = 8 * c + oct_digit;
		  ++i;
		  oct_digit = s[i + 2] - '0';
		  if (0 <= oct_digit && oct_digit <= 7)
		    {
		      if (8 * c + oct_digit < N_CHARS)
			{
			  c = 8 * c + oct_digit;
			  ++i;
			}
		      else if (!posix_pedantic)
			{
			  /* Any octal number larger than 0377 won't
			     fit in 8 bits.  So we stop when adding the
			     next digit would put us over the limit and
			     give a warning about the ambiguity.  POSIX
			     isn't clear on this, but one person has said
			     that in his interpretation, POSIX says tr
			     can't even give a warning. */
			  error (0, 0, "warning: the ambiguous octal escape \
\\%c%c%c is being\n\tinterpreted as the 2-byte sequence \\0%c%c, `%c'",
				 s[i], s[i + 1], s[i + 2],
				 s[i], s[i + 1], s[i + 2]);
			}
		    }
		}
	      break;
	    case '\0':
	      error (0, 0, "invalid backslash escape at end of string");
	      return 1;
	      break;
	    default:
	      error (0, 0, "invalid backslash escape `\\%c'", s[i + 1]);
	      return 1;
	      break;
	    }
	  ++i;
	  s[j++] = c;
	  break;
	default:
	  s[j++] = s[i];
	  break;
	}
    }
  *len = j;
  return 0;
}

/* If CLASS_STR is a valid character class string, return its index
   in the global char_class_name array.  Otherwise, return CC_NO_CLASS. */

static enum Char_class
look_up_char_class (class_str)
     unsigned char *class_str;
{
  unsigned int i;

  for (i = 0; i < N_CHAR_CLASSES; i++)
    if (strcmp ((char *) class_str, char_class_name[i]) == 0)
      return (enum Char_class) i;
  return CC_NO_CLASS;
}

/* Return a newly allocated string with a printable version of C.
   This function is used solely for formatting error messages. */

static char *
make_printable_char (c)
     unsigned int c;
{
  char *buf = xmalloc (5);

  assert (c < N_CHARS);
  if (isprint (c))
    {
      buf[0] = c;
      buf[1] = '\0';
    }
  else
    {
      sprintf (buf, "\\%03o", c);
    }
  return buf;
}

/* Return a newly allocated copy of S which is suitable for printing.
   LEN is the number of characters in S.  Most non-printing
   (isprint) characters are represented by a backslash followed by
   3 octal digits.  However, the characters represented by \c escapes
   where c is one of [abfnrtv] are represented by their 2-character \c
   sequences.  This function is used solely for printing error messages. */

static char *
make_printable_str (s, len)
     unsigned char *s;
     int len;
{
  /* Worst case is that every character expands to a backslash
     followed by a 3-character octal escape sequence. */
  char *printable_buf = xmalloc (4 * len + 1);
  char *p = printable_buf;
  int i;

  for (i = 0; i < len; i++)
    {
      char buf[5];
      char *tmp = NULL;

      switch (s[i])
	{
	case '\\':
	  tmp = "\\";
	  break;
	case '\007':
	  tmp = "\\a";
	  break;
	case '\b':
	  tmp = "\\b";
	  break;
	case '\f':
	  tmp = "\\f";
	  break;
	case '\n':
	  tmp = "\\n";
	  break;
	case '\r':
	  tmp = "\\r";
	  break;
	case '\t':
	  tmp = "\\t";
	  break;
	case '\v':
	  tmp = "\\v";
	  break;
	default:
	  if (isprint (s[i]))
	    {
	      buf[0] = s[i];
	      buf[1] = '\0';
	    }
	  else
	    sprintf (buf, "\\%03o", s[i]);
	  tmp = buf;
	  break;
	}
      p = stpcpy (p, tmp);
    }
  return printable_buf;
}

/* Append a newly allocated structure representing a
   character C to the specification list LIST. */

static void
append_normal_char (list, c)
     struct Spec_list *list;
     unsigned int c;
{
  struct List_element *new;

  new = (struct List_element *) xmalloc (sizeof (struct List_element));
  new->next = NULL;
  new->type = RE_NORMAL_CHAR;
  new->u.normal_char = c;
  assert (list->tail);
  list->tail->next = new;
  list->tail = new;
}

/* Append a newly allocated structure representing the range
   of characters from FIRST to LAST to the specification list LIST.
   Return non-zero if LAST precedes FIRST in the collating sequence,
   zero otherwise.  This means that '[c-c]' is acceptable.  */

static int
append_range (list, first, last)
     struct Spec_list *list;
     unsigned int first;
     unsigned int last;
{
  struct List_element *new;

  if (ORD (first) > ORD (last))
    {
      char *tmp1 = make_printable_char (first);
      char *tmp2 = make_printable_char (last);

      error (0, 0,
	     "range-endpoints of `%s-%s' are in reverse collating sequence order",
	     tmp1, tmp2);
      free (tmp1);
      free (tmp2);
      return 1;
    }
  new = (struct List_element *) xmalloc (sizeof (struct List_element));
  new->next = NULL;
  new->type = RE_RANGE;
  new->u.range.first_char = first;
  new->u.range.last_char = last;
  assert (list->tail);
  list->tail->next = new;
  list->tail = new;
  return 0;
}

/* If CHAR_CLASS_STR is a valid character class string, append a
   newly allocated structure representing that character class to the end
   of the specification list LIST and return 0.  If CHAR_CLASS_STR is not
   a valid string, give an error message and return non-zero. */

static int
append_char_class (list, char_class_str, len)
     struct Spec_list *list;
     unsigned char *char_class_str;
     int len;
{
  enum Char_class char_class;
  struct List_element *new;

  char_class = look_up_char_class (char_class_str);
  if (char_class == CC_NO_CLASS)
    {
      char *tmp = make_printable_str (char_class_str, len);

      error (0, 0, "invalid character class `%s'", tmp);
      free (tmp);
      return 1;
    }
  new = (struct List_element *) xmalloc (sizeof (struct List_element));
  new->next = NULL;
  new->type = RE_CHAR_CLASS;
  new->u.char_class = char_class;
  assert (list->tail);
  list->tail->next = new;
  list->tail = new;
  return 0;
}

/* Append a newly allocated structure representing a [c*n]
   repeated character construct, to the specification list LIST.
   THE_CHAR is the single character to be repeated, and REPEAT_COUNT
   is non-negative repeat count. */

static void
append_repeated_char (list, the_char, repeat_count)
     struct Spec_list *list;
     unsigned int the_char;
     long int repeat_count;
{
  struct List_element *new;

  new = (struct List_element *) xmalloc (sizeof (struct List_element));
  new->next = NULL;
  new->type = RE_REPEATED_CHAR;
  new->u.repeated_char.the_repeated_char = the_char;
  new->u.repeated_char.repeat_count = repeat_count;
  assert (list->tail);
  list->tail->next = new;
  list->tail = new;
}

/* Given a string, EQUIV_CLASS_STR, from a [=str=] context and
   the length of that string, LEN, if LEN is exactly one, append
   a newly allocated structure representing the specified
   equivalence class to the specification list, LIST and return zero.
   If LEN is not 1, issue an error message and return non-zero. */

static int
append_equiv_class (list, equiv_class_str, len)
     struct Spec_list *list;
     unsigned char *equiv_class_str;
     int len;
{
  struct List_element *new;

  if (len != 1)
    {
      char *tmp = make_printable_str (equiv_class_str, len);

      error (0, 0, "%s: equivalence class operand must be a single character",
	     tmp);
      free (tmp);
      return 1;
    }
  new = (struct List_element *) xmalloc (sizeof (struct List_element));
  new->next = NULL;
  new->type = RE_EQUIV_CLASS;
  new->u.equiv_code = *equiv_class_str;
  assert (list->tail);
  list->tail->next = new;
  list->tail = new;
  return 0;
}

/* Return a newly allocated copy of P[FIRST_IDX..LAST_IDX]. */

static unsigned char *
substr (p, first_idx, last_idx)
     unsigned char *p;
     int first_idx;
     int last_idx;
{
  int len = last_idx - first_idx + 1;
  unsigned char *tmp = (unsigned char *) xmalloc (len);

  assert (first_idx <= last_idx);
  /* We must use bcopy or memcopy rather than strncpy
     because `p' may contain zero-bytes. */
  bcopy (p + first_idx, tmp, len);
  tmp[len] = '\0';
  return tmp;
}

/* Search forward starting at START_IDX for the 2-char sequence
   (PRE_BRACKET_CHAR,']') in the string P of length P_LEN.  If such
   a sequence is found, return the index of the first character,
   otherwise return -1.  P may contain zero bytes. */

static int
find_closing_delim (p, start_idx, p_len, pre_bracket_char)
     unsigned char *p;
     int start_idx;
     int p_len;
     unsigned int pre_bracket_char;
{
  int i;

  for (i = start_idx; i < p_len - 1; i++)
    if (p[i] == pre_bracket_char && p[i + 1] == ']')
      return i;
  return -1;
}

/* Convert a string S with explicit length LEN, possibly
   containing embedded zero bytes, to a long integer value.
   If the string represents a negative value, a value larger
   than LONG_MAX, or if all LEN characters do not represent a
   valid integer, return non-zero and do not modify *VAL.
   Otherwise, return zero and set *VAL to the converted value. */

static int
non_neg_strtol (s, len, val)
     unsigned char *s;
     int len;
     long int *val;
{
  int i;
  long sum = 0;
  unsigned int base;

  if (len <= 0)
    return 1;
  if (s[0] == '0')
    base = 8;
  else if (isdigit (s[0]))
    base = 10;
  else
    return 1;

  for (i = 0; i < len; i++)
    {
      int c = s[i] - '0';

      if (c >= base || c < 0)
	return 1;
      if (i > 8 && sum > (LONG_MAX - c) / base)
	return 1;
      sum = sum * base + c;
    }
  *val = sum;
  return 0;
}

/* Parse the bracketed repeat-char syntax.  If the P_LEN characters
   beginning with P[ START_IDX ] comprise a valid [c*n] construct,
   return the character and the repeat count through the arg pointers,
   CHAR_TO_REPEAT and N, and then return the index of the closing
   bracket as the function value.  If the second character following
   the opening bracket is not `*' or if no closing bracket can be
   found, return -1.  If a closing bracket is found and the
   second char is `*', but the string between the `*' and `]' isn't
   empty, an octal number, or a decimal number, print an error message
   and return -2. */

static int
find_bracketed_repeat (p, start_idx, p_len, char_to_repeat, n)
     unsigned char *p;
     int start_idx;
     int p_len;
     unsigned int *char_to_repeat;
     long int *n;
{
  int i;

  assert (start_idx + 1 < p_len);
  if (p[start_idx + 1] != '*')
    return -1;

  for (i = start_idx + 2; i < p_len; i++)
    {
      if (p[i] == ']')
	{
	  unsigned char *digit_str;
	  int digit_str_len = i - start_idx - 2;

	  *char_to_repeat = p[start_idx];
	  if (digit_str_len == 0)
	    {
	      /* We've matched [c*] -- no explicit repeat count. */
	      *n = 0;
	      return i;
	    }

	  /* Here, we have found [c*s] where s should be a string
	     of octal or decimal digits. */
	  digit_str = &p[start_idx + 2];
	  if (non_neg_strtol (digit_str, digit_str_len, n))
	    {
	      char *tmp = make_printable_str (digit_str, digit_str_len);
	      error (0, 0, "invalid repeat count `%s' in [c*n] construct", tmp);
	      free (tmp);
	      return -2;
	    }
	  return i;
	}
    }
  return -1;			/* No bracket found. */
}

/* Convert string UNESACPED_STRING (which has been preprocessed to
   convert backslash-escape sequences) of length LEN characters into
   a linked list of the following 5 types of constructs:
      - [:str:] Character class where `str' is one of the 12 valid strings.
      - [=c=] Equivalence class where `c' is any single character.
      - [c*n] Repeat the single character `c' `n' times. n may be omitted.
	  However, if `n' is present, it must be a non-negative octal or
	  decimal integer.
      - r-s Range of characters from `r' to `s'.  The second endpoint must
	  not precede the first in the current collating sequence.
      - c Any other character is interpreted as itself. */

static int
build_spec_list (unescaped_string, len, result)
     unsigned char *unescaped_string;
     int len;
     struct Spec_list *result;
{
  unsigned char *p;
  int i;

  p = unescaped_string;

  /* The main for-loop below recognizes the 4 multi-character constructs.
     A character that matches (in its context) none of the multi-character
     constructs is classified as `normal'.  Since all multi-character
     constructs have at least 3 characters, any strings of length 2 or
     less are composed solely of normal characters.  Hence, the index of
     the outer for-loop runs only as far as LEN-2. */

  for (i = 0; i < len - 2;)
    {
      switch (p[i])
	{
	  int fall_through;
	case '[':
	  fall_through = 0;
	  switch (p[i + 1])
	    {
	      int closing_delim_idx;
	      int closing_bracket_idx;
	      unsigned int char_to_repeat;
	      long repeat_count;
	    case ':':
	    case '=':
	      closing_delim_idx = find_closing_delim (p, i + 2, len, p[i + 1]);
	      if (closing_delim_idx >= 0)
		{
		  int parse_failed;
		  unsigned char *opnd_str = substr (p, i + 2, closing_delim_idx - 1);
		  if (p[i + 1] == ':')
		    parse_failed = append_char_class (result, opnd_str,
				     (closing_delim_idx - 1) - (i + 2) + 1);
		  else
		    parse_failed = append_equiv_class (result, opnd_str,
				     (closing_delim_idx - 1) - (i + 2) + 1);
		  free (opnd_str);

		  /* Return non-zero if append_*_class reports a problem. */
		  if (parse_failed)
		    return 1;
		  else
		    i = closing_delim_idx + 2;
		  break;
		}
	      /* Else fall through.  This could be [:*] or [=*]. */
	    default:
	      /* Determine whether this is a bracketed repeat range
		 matching the RE \[.\*(dec_or_oct_number)?\]. */
	      closing_bracket_idx = find_bracketed_repeat (p, i + 1,
				       len, &char_to_repeat, &repeat_count);
	      if (closing_bracket_idx >= 0)
		{
		  append_repeated_char (result, char_to_repeat, repeat_count);
		  i = closing_bracket_idx + 1;
		  break;
		}
	      else if (closing_bracket_idx == -1)
		{
		  fall_through = 1;
		}
	      else
		/* Found a string that looked like [c*n] but the
		   numeric part was invalid. */
		return 1;
	      break;
	    }
	  if (!fall_through)
	    break;

	  /* Here if we've tried to match [c*n], [:str:], and [=c=]
	     and none of them fit.  So we still have to consider the
	     range `[-c' (from `[' to `c'). */
	default:
	  /* Look ahead one char for ranges like a-z. */
	  if (p[i + 1] == '-')
	    {
	      if (append_range (result, p[i], p[i + 2]))
		return 1;
	      i += 3;
	    }
	  else
	    {
	      append_normal_char (result, p[i]);
	      ++i;
	    }
	  break;
	}
    }

  /* Now handle the (2 or fewer) remaining characters p[i]..p[len - 1]. */
  for (; i < len; i++)
    append_normal_char (result, p[i]);

  return 0;
}


/* Given a Spec_list S (with its saved state implicit in the values
   of its members `tail' and `state'), return the next single character
   in the expansion of S's constructs.  If the last character of S was
   returned on the previous call or if S was empty, this function
   returns -1.  For example, successive calls to get_next where S
   represents the spec-string 'a-d[y*3]' will return the sequence
   of values a, b, c, d, y, y, y, -1.  Finally, if the construct from
   which the returned character comes is [:upper:] or [:lower:], the
   parameter CLASS is given a value to indicate which it was.  Otherwise
   CLASS is set to UL_NONE.  This value is used only when constructing
   the translation table to verify that any occurrences of upper and
   lower class constructs in the spec-strings appear in the same relative
   positions. */

static int
get_next (s, class)
     struct Spec_list *s;
     enum Upper_Lower_class *class;
{
  struct List_element *p;
  int return_val;
  int i;

  return_val = -1;		/* Appease the compiler. */
  if (class)
    *class = UL_NONE;

  if (s->state == BEGIN_STATE)
    {
      s->tail = s->head->next;
      s->state = NEW_ELEMENT;
    }

  p = s->tail;
  if (p == NULL)
    return -1;

  switch (p->type)
    {
    case RE_NORMAL_CHAR:
      return_val = p->u.normal_char;
      s->state = NEW_ELEMENT;
      s->tail = p->next;
      break;

    case RE_RANGE:
      if (s->state == NEW_ELEMENT)
	s->state = ORD (p->u.range.first_char);
      else
	++(s->state);
      return_val = CHR (s->state);
      if (s->state == ORD (p->u.range.last_char))
	{
	  s->tail = p->next;
	  s->state = NEW_ELEMENT;
	}
      break;

    case RE_CHAR_CLASS:
      if (s->state == NEW_ELEMENT)
	{
	  for (i = 0; i < N_CHARS; i++)
	    if (is_char_class_member (p->u.char_class, i))
	      break;
	  assert (i < N_CHARS);
	  s->state = i;
	}
      assert (is_char_class_member (p->u.char_class, s->state));
      return_val = CHR (s->state);
      for (i = s->state + 1; i < N_CHARS; i++)
	if (is_char_class_member (p->u.char_class, i))
	  break;
      if (i < N_CHARS)
	s->state = i;
      else
	{
	  s->tail = p->next;
	  s->state = NEW_ELEMENT;
	}
      if (class)
	{
	  switch (p->u.char_class)
	    {
	    case CC_LOWER:
	      *class = UL_LOWER;
	      break;
	    case CC_UPPER:
	      *class = UL_UPPER;
	      break;
	    default:
	      /* empty */
	      break;
	    }
	}
      break;

    case RE_EQUIV_CLASS:
      /* FIXME: this assumes that each character is alone in its own
	 equivalence class (which appears to be correct for my
	 LC_COLLATE.  But I don't know of any function that allows
	 one to determine a character's equivalence class. */

      return_val = p->u.equiv_code;
      s->state = NEW_ELEMENT;
      s->tail = p->next;
      break;

    case RE_REPEATED_CHAR:
      /* Here, a repeat count of n == 0 means don't repeat at all. */
      assert (p->u.repeated_char.repeat_count >= 0);
      if (p->u.repeated_char.repeat_count == 0)
	{
	  s->tail = p->next;
	  s->state = NEW_ELEMENT;
	  return_val = get_next (s, class);
	}
      else
	{
	  if (s->state == NEW_ELEMENT)
	    {
	      s->state = 0;
	    }
	  ++(s->state);
	  return_val = p->u.repeated_char.the_repeated_char;
	  if (p->u.repeated_char.repeat_count > 0
	      && s->state == p->u.repeated_char.repeat_count)
	    {
	      s->tail = p->next;
	      s->state = NEW_ELEMENT;
	    }
	}
      break;

    case RE_NO_TYPE:
      assert (0);
      break;
    }
  return return_val;
}

/* This is a minor kludge.  This function is called from
   get_spec_stats to determine the cardinality of a set derived
   from a complemented string.  It's a kludge in that some of
   the same operations are (duplicated) performed in set_initialize. */

static int
card_of_complement (s)
     struct Spec_list *s;
{
  int c;
  int cardinality = N_CHARS;
  SET_TYPE in_set[N_CHARS];

  bzero (in_set, N_CHARS * sizeof (in_set[0]));
  s->state = BEGIN_STATE;
  while ((c = get_next (s, NULL)) != -1)
    if (!in_set[c]++)
      --cardinality;
  return cardinality;
}

/* Gather statistics about the spec-list S in preparation for the tests
   in validate that determine the legality of the specs.  This function
   is called at most twice; once for string1, and again for any string2.
   LEN_S1 < 0 indicates that this is the first call and that S represents
   string1.  When LEN_S1 >= 0, it is the length of the expansion of the
   constructs in string1, and we can use its value to resolve any
   indefinite repeat construct in S (which represents string2).  Hence,
   this function has the side-effect that it converts a valid [c*]
   construct in string2 to [c*n] where n is large enough (or 0) to give
   string2 the same length as string1.  For example, with the command
   tr a-z 'A[\n*]Z' on the second call to get_spec_stats, LEN_S1 would
   be 26 and S (representing string2) would be converted to 'A[\n*24]Z'. */

static void
get_spec_stats (s, len_s1)
     struct Spec_list *s;
     int len_s1;
{
  struct List_element *p;
  struct List_element *indefinite_repeat_element = NULL;
  int len = 0;

  s->n_indefinite_repeats = 0;
  s->has_equiv_class = 0;
  s->has_restricted_char_class = 0;
  s->has_upper_or_lower = 0;
  for (p = s->head->next; p; p = p->next)
    {
      switch (p->type)
	{
	  int i;
	case RE_NORMAL_CHAR:
	  ++len;
	  break;

	case RE_RANGE:
	  assert (p->u.range.last_char > p->u.range.first_char);
	  len += p->u.range.last_char - p->u.range.first_char + 1;
	  break;

	case RE_CHAR_CLASS:
	  for (i = 0; i < N_CHARS; i++)
	    if (is_char_class_member (p->u.char_class, i))
	      ++len;
	  switch (p->u.char_class)
	    {
	    case CC_UPPER:
	    case CC_LOWER:
	      s->has_upper_or_lower = 1;
	      break;
	    default:
	      s->has_restricted_char_class = 1;
	      break;
	    }
	  break;

	case RE_EQUIV_CLASS:
	  for (i = 0; i < N_CHARS; i++)
	    if (is_equiv_class_member (p->u.equiv_code, i))
	      ++len;
	  s->has_equiv_class = 1;
	  break;

	case RE_REPEATED_CHAR:
	  if (p->u.repeated_char.repeat_count > 0)
	    len += p->u.repeated_char.repeat_count;
	  else if (p->u.repeated_char.repeat_count == 0)
	    {
	      indefinite_repeat_element = p;
	      ++(s->n_indefinite_repeats);
	    }
	  break;

	case RE_NO_TYPE:
	  assert (0);
	  break;
	}
    }

  if (len_s1 >= len && s->n_indefinite_repeats == 1)
    {
      indefinite_repeat_element->u.repeated_char.repeat_count = len_s1 - len;
      len = len_s1;
    }
  if (complement && len_s1 < 0)
    s->length = card_of_complement (s);
  else
    s->length = len;
  return;
}

static void
spec_init (spec_list)
     struct Spec_list *spec_list;
{
  spec_list->head = spec_list->tail =
    (struct List_element *) xmalloc (sizeof (struct List_element));
  spec_list->head->next = NULL;
}

/* This function makes two passes over the argument string S.  The first
   one converts all \c and \ddd escapes to their one-byte representations.
   The second constructs a linked specification list, SPEC_LIST, of the
   characters and constructs that comprise the argument string.  If either
   of these passes detects an error, this function returns non-zero. */

static int
parse_str (s, spec_list)
     unsigned char *s;
     struct Spec_list *spec_list;
{
  int len;

  if (unquote (s, &len))
    return 1;
  if (build_spec_list (s, len, spec_list))
    return 1;
  return 0;
}

/* Given two specification lists, S1 and S2, and assuming that
   S1->length > S2->length, append a single [c*n] element to S2 where c
   is the last character in the expansion of S2 and n is the difference
   between the two lengths.
   Upon successful completion, S2->length is set to S1->length.  The only
   way this function can fail to make S2 as long as S1 is when S2 has
   zero-length, since in that case, there is no last character to repeat.

   Providing this functionality allows the user to do some pretty
   non-BSD (and non-portable) things:  For example, the command
       tr -cs '[:upper:]0-9' '[:lower:]'
   is almost guaranteed to give results that depend on your collating
   sequence.  */

static void
string2_extend (s1, s2)
     struct Spec_list *s1;
     struct Spec_list *s2;
{
  struct List_element *p;
  int char_to_repeat;
  int i;

  assert (translating);
  assert (s1->length > s2->length);
  if (s2->length == 0)
    return;

  char_to_repeat = -1;		/* Appease the compiler. */
  p = s2->tail;
  switch (p->type)
    {
    case RE_NORMAL_CHAR:
      char_to_repeat = p->u.normal_char;
      break;
    case RE_RANGE:
      char_to_repeat = p->u.range.last_char;
      break;
    case RE_CHAR_CLASS:
      for (i = N_CHARS; i >= 0; i--)
	if (is_char_class_member (p->u.char_class, i))
	  break;
      assert (i >= 0);
      char_to_repeat = CHR (i);
      break;
    case RE_EQUIV_CLASS:
      /* This shouldn't happen, because validate exits with an error
	 if it finds an equiv class in string2 when translating. */
      assert (0);
      break;
    case RE_REPEATED_CHAR:
      char_to_repeat = p->u.repeated_char.the_repeated_char;
      break;
    case RE_NO_TYPE:
      assert (0);
      break;
    }
  append_repeated_char (s2, char_to_repeat, s1->length - s2->length);
  s2->length = s1->length;
  return;
}

/* Die with an error message if S1 and S2 describe strings that
   are not valid with the given command line switches.
   A side effect of this function is that if a legal [c*] or
   [c*0] construct appears in string2, it is converted to [c*n]
   with a value for n that makes s2->length == s1->length.  By
   the same token, if the --truncate-set1 option is not
   given, S2 may be extended. */

static void
validate (s1, s2)
     struct Spec_list *s1;
     struct Spec_list *s2;
{
  get_spec_stats (s1, -1);
  if (s1->n_indefinite_repeats > 0)
    {
      error (1, 0, "the [c*] repeat construct may not appear in string1");
    }

  /* FIXME: it isn't clear from the POSIX spec that this is illegal,
     but in the spirit of the other restrictions put on translation
     with character classes, this seems a logical interpretation. */
  if (complement && s1->has_upper_or_lower)
    {
      error (1, 0,
	     "character classes may not be used when translating and complementing");
    }

  if (s2)
    {
      get_spec_stats (s2, s1->length);
      if (s2->has_restricted_char_class)
	{
	  error (1, 0,
		 "when translating, the only character classes that may appear in\n\
\tstring2 are `upper' and `lower'");
	}

      if (s2->n_indefinite_repeats > 1)
	{
	  error (1, 0, "only one [c*] repeat construct may appear in string2");
	}

      if (translating)
	{
	  if (s2->has_equiv_class)
	    {
	      error (1, 0,
		     "[=c=] expressions may not appear in string2 when translating");
	    }

	  if (s1->length > s2->length)
	    {
	      if (!truncate_set1)
		string2_extend (s1, s2);
	    }

	  if (complement && s2->has_upper_or_lower)
	    error (1, 0,
		   "character classes may not be used when translating and complementing");
	}
      else
	/* Not translating. */
	{
	  if (s2->n_indefinite_repeats > 0)
	    error (1, 0,
		   "the [c*] construct may appear in string2 only when translating");
	}
    }
}

/* Read buffers of SIZE bytes via the function READER (if READER is
   NULL, read from stdin) until EOF.  When non-NULL, READER is either
   read_and_delete or read_and_xlate.  After each buffer is read, it is
   processed and written to stdout.  The buffers are processed so that
   multiple consecutive occurrences of the same character in the input
   stream are replaced by a single occurrence of that character if the
   character is in the squeeze set. */

static void
squeeze_filter (buf, size, reader)
     unsigned char *buf;
     long int size;
     PFI reader;
{
  unsigned int char_to_squeeze = NOT_A_CHAR;
  int i = 0;
  int nr = 0;

  for (;;)
    {
      int begin;

      if (i >= nr)
	{
	  if (reader == NULL)
	    nr = read (0, (char *) buf, size);
	  else
	    nr = (*reader) (buf, size, NULL);

	  if (nr < 0)
	    error (1, errno, "read error");
	  if (nr == 0)
	    break;
	  i = 0;
	}

      begin = i;

      if (char_to_squeeze == NOT_A_CHAR)
	{
	  int out_len;
	  /* Here, by being a little tricky, we can get a significant
	     performance increase in most cases when the input is
	     reasonably large.  Since tr will modify the input only
	     if two consecutive (and identical) input characters are
	     in the squeeze set, we can step by two through the data
	     when searching for a character in the squeeze set.  This
	     means there may be a little more work in a few cases and
	     perhaps twice as much work in the worst cases where most
	     of the input is removed by squeezing repeats.  But most
	     uses of this functionality seem to remove less than 20-30%
	     of the input. */
	  for (; i < nr && !in_squeeze_set[buf[i]]; i += 2)
	    ;			/* empty */

	  /* There is a special case when i == nr and we've just
	     skipped a character (the last one in buf) that is in
	     the squeeze set. */
	  if (i == nr && in_squeeze_set[buf[i - 1]])
	    --i;

	  if (i >= nr)
	    out_len = nr - begin;
	  else
	    {
	      char_to_squeeze = buf[i];
	      /* We're about to output buf[begin..i]. */
	      out_len = i - begin + 1;

	      /* But since we stepped by 2 in the loop above,
		 out_len may be one too large. */
	      if (i > 0 && buf[i - 1] == char_to_squeeze)
		--out_len;

	      /* Advance i to the index of first character to be
		 considered when looking for a char different from
		 char_to_squeeze. */
	      ++i;
	    }
	  if (out_len > 0
	      && fwrite ((char *) &buf[begin], 1, out_len, stdout) == 0)
	    error (1, errno, "write error");
	}

      if (char_to_squeeze != NOT_A_CHAR)
	{
	  /* Advance i to index of first char != char_to_squeeze
	     (or to nr if all the rest of the characters in this
	     buffer are the same as char_to_squeeze). */
	  for (; i < nr && buf[i] == char_to_squeeze; i++)
	    ;			/* empty */
	  if (i < nr)
	    char_to_squeeze = NOT_A_CHAR;
	  /* If (i >= nr) we've squeezed the last character in this buffer.
	     So now we have to read a new buffer and continue comparing
	     characters against char_to_squeeze. */
	}
    }
}

/* Read buffers of SIZE bytes from stdin until one is found that
   contains at least one character not in the delete set.  Store
   in the array BUF, all characters from that buffer that are not
   in the delete set, and return the number of characters saved
   or 0 upon EOF. */

static long
read_and_delete (buf, size, not_used)
     unsigned char *buf;
     long int size;
     PFI not_used;
{
  long n_saved;
  static int hit_eof = 0;

  assert (not_used == NULL);
  assert (size > 0);

  if (hit_eof)
    return 0;

  /* This enclosing do-while loop is to make sure that
     we don't return zero (indicating EOF) when we've
     just deleted all the characters in a buffer. */
  do
    {
      int i;
      int nr = read (0, (char *) buf, size);

      if (nr < 0)
	error (1, errno, "read error");
      if (nr == 0)
	{
	  hit_eof = 1;
	  return 0;
	}

      /* This first loop may be a waste of code, but gives much
         better performance when no characters are deleted in
         the beginning of a buffer.  It just avoids the copying
         of buf[i] into buf[n_saved] when it would be a NOP. */

      for (i = 0; i < nr && !in_delete_set[buf[i]]; i++)
	/* empty */ ;
      n_saved = i;

      for (++i; i < nr; i++)
	if (!in_delete_set[buf[i]])
	  buf[n_saved++] = buf[i];
    }
  while (n_saved == 0);

  return n_saved;
}

/* Read at most SIZE bytes from stdin into the array BUF.  Then
   perform the in-place and one-to-one mapping specified by the global
   array `xlate'.  Return the number of characters read, or 0 upon EOF. */

static long
read_and_xlate (buf, size, not_used)
     unsigned char *buf;
     long int size;
     PFI not_used;
{
  long chars_read = 0;
  static int hit_eof = 0;
  int i;

  assert (not_used == NULL);
  assert (size > 0);

  if (hit_eof)
    return 0;

  chars_read = read (0, (char *) buf, size);
  if (chars_read < 0)
    error (1, errno, "read error");
  if (chars_read == 0)
    {
      hit_eof = 1;
      return 0;
    }

  for (i = 0; i < chars_read; i++)
    buf[i] = xlate[buf[i]];

  return chars_read;
}

/* Initialize a boolean membership set IN_SET with the character
   values obtained by traversing the linked list of constructs S
   using the function `get_next'.  If COMPLEMENT_THIS_SET is
   non-zero the resulting set is complemented. */

static void
set_initialize (s, complement_this_set, in_set)
     struct Spec_list *s;
     int complement_this_set;
     SET_TYPE *in_set;
{
  int c;
  int i;

  bzero (in_set, N_CHARS * sizeof (in_set[0]));
  s->state = BEGIN_STATE;
  while ((c = get_next (s, NULL)) != -1)
    in_set[c] = 1;
  if (complement_this_set)
    for (i = 0; i < N_CHARS; i++)
      in_set[i] = (!in_set[i]);
}

void
main (argc, argv)
     int argc;
     char **argv;
{
  int c;
  int non_option_args;
  struct Spec_list buf1, buf2;
  struct Spec_list *s1 = &buf1;
  struct Spec_list *s2 = &buf2;

  program_name = argv[0];

  while ((c = getopt_long (argc, argv, "cdst", long_options,
			   (int *) 0)) != EOF)
    {
      switch (c)
	{
	case 0:
	  break;

	case 'c':
	  complement = 1;
	  break;

	case 'd':
	  delete = 1;
	  break;

	case 's':
	  squeeze_repeats = 1;
	  break;

	case 't':
	  truncate_set1 = 1;
	  break;

	default:
	  usage ();
	  break;
	}
    }

  posix_pedantic = (getenv ("POSIXLY_CORRECT") != 0);

  non_option_args = argc - optind;
  translating = (non_option_args == 2 && !delete);

  /* Change this test if it is legal to give tr no options and
     no args at all.  POSIX doesn't specifically say anything
     either way, but it looks like they implied it's illegal
     by omission.  If you want to make tr do a slow imitation
     of `cat' use `tr a a'. */
  if (non_option_args > 2)
    usage ();

  if (!delete && !squeeze_repeats && non_option_args != 2)
    error (1, 0, "two strings must be given when translating");

  if (delete && squeeze_repeats && non_option_args != 2)
    error (1, 0, "two strings must be given when both \
deleting and squeezing repeats");

  /* If --delete is given without --squeeze-repeats, then
     only one string argument may be specified.  But POSIX
     says to ignore any string2 in this case, so if POSIXLY_CORRECT
     is set, pretend we never saw string2.  But I think
     this deserves a fatal error, so that's the default. */
  if ((delete && !squeeze_repeats) && non_option_args != 1)
    {
      if (posix_pedantic && non_option_args == 2)
	--non_option_args;
      else
	error (1, 0,
	       "only one string may be given when deleting without squeezing repeats");
    }

  spec_init (s1);
  if (parse_str ((unsigned char *) argv[optind], s1))
    exit (1);

  if (non_option_args == 2)
    {
      spec_init (s2);
      if (parse_str ((unsigned char *) argv[optind + 1], s2))
	exit (1);
    }
  else
    s2 = NULL;

  validate (s1, s2);

  if (squeeze_repeats && non_option_args == 1)
    {
      set_initialize (s1, complement, in_squeeze_set);
      squeeze_filter (io_buf, IO_BUF_SIZE, NULL);
    }
  else if (delete && non_option_args == 1)
    {
      int nr;

      set_initialize (s1, complement, in_delete_set);
      do
	{
	  nr = read_and_delete (io_buf, IO_BUF_SIZE, NULL);
	  if (nr > 0 && fwrite ((char *) io_buf, 1, nr, stdout) == 0)
	    error (1, errno, "write error");
	}
      while (nr > 0);
    }
  else if (squeeze_repeats && delete && non_option_args == 2)
    {
      set_initialize (s1, complement, in_delete_set);
      set_initialize (s2, 0, in_squeeze_set);
      squeeze_filter (io_buf, IO_BUF_SIZE, (PFI) read_and_delete);
    }
  else if (translating)
    {
      if (complement)
	{
	  int i;
	  SET_TYPE *in_s1 = in_delete_set;

	  set_initialize (s1, 0, in_s1);
	  s2->state = BEGIN_STATE;
	  for (i = 0; i < N_CHARS; i++)
	    xlate[i] = i;
	  for (i = 0; i < N_CHARS; i++)
	    {
	      if (!in_s1[i])
		{
		  int c = get_next (s2, NULL);
		  assert (c != -1 || truncate_set1);
		  if (c == -1)
		    {
		      /* This will happen when tr is invoked like e.g.
			 tr -cs A-Za-z0-9 '\012'.  */
		      break;
		    }
		  xlate[i] = c;
		}
	    }
	  assert (get_next (s2, NULL) == -1 || truncate_set1);
	}
      else
	{
	  int c1, c2;
	  int i;
	  enum Upper_Lower_class class_s1;
	  enum Upper_Lower_class class_s2;

	  for (i = 0; i < N_CHARS; i++)
	    xlate[i] = i;
	  s1->state = BEGIN_STATE;
	  s2->state = BEGIN_STATE;
	  for (;;)
	    {
	      c1 = get_next (s1, &class_s1);
	      c2 = get_next (s2, &class_s2);
	      if (!class_ok[(int) class_s1][(int) class_s2])
		error (1, 0,
		    "misaligned or mismatched upper and/or lower classes");
	      /* The following should have been checked by validate... */
	      if (c2 == -1)
		break;
	      xlate[c1] = c2;
	    }
	  assert (c1 == -1 || truncate_set1);
	}
      if (squeeze_repeats)
	{
	  set_initialize (s2, 0, in_squeeze_set);
	  squeeze_filter (io_buf, IO_BUF_SIZE, (PFI) read_and_xlate);
	}
      else
	{
	  int chars_read;

	  do
	    {
	      chars_read = read_and_xlate (io_buf, IO_BUF_SIZE, NULL);
	      if (chars_read > 0
		  && fwrite ((char *) io_buf, 1, chars_read, stdout) == 0)
		error (1, errno, "write error");
	    }
	  while (chars_read > 0);
	}
    }

  exit (0);
}

