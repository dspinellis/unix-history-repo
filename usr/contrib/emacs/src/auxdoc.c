/* Auxiliary documentation strings for built-in functions of GNU Emacs.
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


*********
This resembles C code for GNU Emacs but it is not.
It is processed only by make-docfile.
The reason these functions' doc strings are here
is that the C preprocessor crashes on strings this long.
So we put a 0 for the doc string in the real C source file
and give this file to make-docfile as if it were the C source.
*********


DEFUN ("modify-syntax-entry", foo, bar, 0, 0, 0,
  "Set syntax for character CHAR according to string S.\n\
The syntax is changed only for table TABLE, which defaults to\n\
 the current buffer's syntax table.\n\
The first character of S should be one of the following:\n\
  Space    whitespace syntax.    w   word constituent.\n\
  _        symbol constituent.   .   punctuation.\n\
  (        open-parenthesis.     )   close-parenthesis.\n\
  \"        string quote.         \\   character-quote.\n\
  $        paired delimiter.     '   expression prefix operator.\n\
  <	   comment starter.	 >   comment ender.\n\
Only single-character comment start and end sequences are represented thus.\n\
Two-character sequences are represented as described below.\n\
The second character of S is the matching parenthesis,\n\
 used only if the first character is ( or ).\n\
Any additional characters are flags.\n\
Defined flags are the characters 1, 2, 3 and 4.\n\
 1 means C is the start of a two-char comment start sequence.\n\
 2 means C is the second character of such a sequence.\n\
 3 means C is the start of a two-char comment end sequence.\n\
 4 means C is the second character of such a sequence.")

DEFUN ("parse-partial-sexp", Ffoo, Sfoo, 0, 0, 0,
  "Parse Lisp syntax starting at FROM until TO; return status of parse at TO.\n\
Parsing stops at TO or when certain criteria are met;\n\
 point is set to where parsing stops.\n\
If fifth arg STATE is omitted or nil,\n\
 parsing assumes that FROM is the beginning of a function.\n\
Value is a list of six elements describing final state of parsing:\n\
 1. depth in parens.\n\
 2. character address of start of innermost containing list; nil if none.\n\
 3. character address of start of last complete sexp terminated.\n\
 4. non-nil if inside a string.\n\
    (it is the character that will terminate the string.)\n\
 5. t if inside a comment.\n\
 6. t if following a quote character.\n\
If third arg TARGETDEPTH is non-nil, parsing stops if the depth\n\
in parentheses becomes equal to TARGETDEPTH.\n\
Fourth arg STOPBEFORE non-nil means stop when come to\n\
 any character that starts a sexp.\n\
Fifth arg STATE is a six-list like what this function returns.\n\
It is used to initialize the state of the parse.")


DEFUN ("interactive", Ffoo, Sfoo, 0, 0, 0,
 "Specify a way of parsing arguments for interactive use of a function.\n\
For example, write\n\
  (defun fun (arg) \"Doc string\" (interactive \"p\") ...use arg...)\n\
to make arg be the prefix numeric argument when foo is called as a command.\n\
This is actually a declaration rather than a function;\n\
 it tells  call-interactively  how to read arguments\n\
 to pass to the function.\n\
When actually called,  interactive  just returns nil.\n\
\n\
The argument of  interactive  is usually a string containing a code letter\n\
 followed by a prompt.  (Some code letters do not use I/O to get\n\
 the argument and do not need prompts.)  To prompt for multiple arguments,\n\
 give a code letter, its prompt, a newline, and another code letter, etc.\n\
If the argument is not a string, it is evaluated to get a list of\n\
 arguments to pass to the function.\n\
Just  (interactive)  means pass no args when calling interactively.\n\
\nCode letters available are:\n\
a -- Function name: symbol with a function definition.\n\
b -- Name of existing buffer.\n\
B -- Name of buffer, possibly nonexistent.\n\
c -- Character.\n\
C -- Command name: symbol with interactive function definition.\n\
d -- Value of point as number.  Does not do I/O.\n\
D -- Directory name.\n\
f -- Existing file name.\n\
F -- Possibly nonexistent file name.\n\
k -- Key sequence (string).\n\
m -- Value of mark as number.  Does not do I/O.\n\
n -- Number read using minibuffer.\n\
p -- Prefix arg converted to number.  Does not do I/O.\n\
P -- Prefix arg in raw form.  Does not do I/O.\n\
r -- Region: point and mark as 2 numeric args, smallest first.  Does no I/O.\n\
s -- Any string.\n\
S -- Any symbol.\n\
v -- Variable name: symbol that is user-variable-p.\n\
x -- Lisp expression read but not evaluated.\n\
X -- Lisp expression read and evaluated.\n\
In addition, if the first character of the string is '*' then an error is\n\
 signaled if the buffer is read-only.\n\
 This happens before reading any arguments.")
