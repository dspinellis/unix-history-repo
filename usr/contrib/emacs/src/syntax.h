/* Declarations having to do with GNU Emacs syntax tables.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


extern Lisp_Object Qsyntax_table_p;
extern Lisp_Object Fsyntax_table_p (), Fsyntax_table (), Fset_syntax_table ();

extern Lisp_Object Vstandard_syntax_table;

/* A syntax table is a Lisp vector of length 0400, whose elements are integers.

The low 8 bits of the integer is a code, as follows:
*/

enum syntaxcode
  {
    Swhitespace, /* for a whitespace character */
    Spunct,	 /* for random punctuation characters */
    Sword,	 /* for a word constituent */
    Ssymbol,	 /* symbol constituent but not word constituent */
    Sopen,	 /* for a beginning delimiter */
    Sclose,      /* for an ending delimiter */
    Squote,	 /* for a prefix character like Lisp ' */
    Sstring,	 /* for a string-grouping character like Lisp " */
    Smath,	 /* for delimiters like $ in Tex. */
    Sescape,	 /* for a character that begins a C-style escape */
    Scharquote,  /* for a character that quotes the following character */
    Scomment,    /* for a comment-starting character */
    Sendcomment, /* for a comment-ending character */
    Smax	 /* Upper bound on codes that are meaningful */
  };

#define SYNTAX(c) \
  ((enum syntaxcode) (XINT (bf_cur->syntax_table_v->contents[c]) & 0377))

/* The next 8 bits of the number is a character,
 the matching delimiter in the case of Sopen or Sclose. */

#define SYNTAX_MATCH(c) \
  ((XINT (bf_cur->syntax_table_v->contents[c]) >> 8) & 0377)

/* Then there are four single-bit flags that have the following meanings:
  1. This character is the first of a two-character comment-start sequence.
  2. This character is the second of a two-character comment-start sequence.
  3. This character is the first of a two-character comment-end sequence.
  4. This character is the second of a two-character comment-end sequence.
 Note that any two-character sequence whose first character has flag 1
  and whose second character has flag 2 will be interpreted as a comment start. */

#define SYNTAX_COMSTART_FIRST(c) \
  ((XINT (bf_cur->syntax_table_v->contents[c]) >> 16) & 1)

#define SYNTAX_COMSTART_SECOND(c) \
  ((XINT (bf_cur->syntax_table_v->contents[c]) >> 17) & 1)

#define SYNTAX_COMEND_FIRST(c) \
  ((XINT (bf_cur->syntax_table_v->contents[c]) >> 18) & 1)

#define SYNTAX_COMEND_SECOND(c) \
  ((XINT (bf_cur->syntax_table_v->contents[c]) >> 19) & 1)

/* This array, indexed by a character, contains the syntax code which that
 character signifies (as a char).  For example,
 (enum syntaxcode) syntax_spec_code['w'] is Sword. */

extern char syntax_spec_code[0400];
