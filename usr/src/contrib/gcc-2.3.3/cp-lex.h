/* Define constants and variables for communication with cp-parse.y.
   Copyright (C) 1987, 1992 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@mcc.com)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */



enum rid
{
  RID_UNUSED,
  RID_INT,
  RID_CHAR,
  RID_FLOAT,
  RID_DOUBLE,
  RID_VOID,
  RID_UNUSED1,

  /* C++ extension */
  RID_CLASS,
  RID_RECORD,
  RID_UNION,
  RID_ENUM,
  RID_LONGLONG,

  RID_UNSIGNED,
  RID_SHORT,
  RID_LONG,
  RID_AUTO,
  RID_STATIC,
  RID_EXTERN,
  RID_REGISTER,
  RID_TYPEDEF,
  RID_SIGNED,
  RID_CONST,
  RID_VOLATILE,
  RID_INLINE,
  RID_WCHAR,

  /* extensions */
  RID_FRIEND,
  RID_VIRTUAL,
  RID_EXCEPTION,
  RID_RAISES,
  RID_PUBLIC,
  RID_PRIVATE,
  RID_PROTECTED,

  RID_MAX
};

#define NORID RID_UNUSED

#define RID_FIRST_MODIFIER RID_UNSIGNED

/* The elements of `ridpointers' are identifier nodes
   for the reserved type names and storage classes.
   It is indexed by a RID_... value.  */
extern tree ridpointers[(int) RID_MAX];

/* the declaration found for the last IDENTIFIER token read in.
   yylex must look this up to detect typedefs, which get token type TYPENAME,
   so it is left around in case the identifier is not a typedef but is
   used in a context which makes it a reference to a variable.  */
extern tree lastiddecl;

extern char *token_buffer;	/* Pointer to token buffer.  */

/* Back-door communication channel to the lexer.  */
extern int looking_for_typename;

extern tree make_pointer_declarator (), make_reference_declarator ();
extern void reinit_parse_for_function ();
extern void reinit_parse_for_method ();
extern int yylex ();
