/* Interface from Emacs to terminfo.
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

/* This is to avoid need to conditionalize interface to termcap.  */

#include "config.h"

char UP, BC, PC;
short ospeed;

#ifdef NO_ARG_ARRAY
tparam (string, outstring,
	arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
     char *string;
     char *outstring;
     int arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9;
{
  char argp[10];
  argp[0] = arg0; argp[1] = arg1; argp[2] = arg2;
  argp[3] = arg3; argp[4] = arg4; argp[5] = arg5;
  argp[6] = arg6; argp[7] = arg7; argp[8] = arg8;
  argp[9] = arg9;
  tparam1(string, outstring, &argp[0]);
}
#else /* no NO_ARG_ARRAY */
tparam (string, outstring, arg)
     char *string;
     char *outstring;
     int arg;
{
  tparam1 (string, outstring, &arg);
}
#endif /* no NO_ARG_ARRAY */

#define todigit(c) ((c) - '0')

/* Virtual TERMINFO machine */

#define TO_REG(c) (((int) c) - ((int) 'a'))
#define REG(c) the_registers[TO_REG (c)]
#define PUSH(x) *--sp = ((int) x);
#define POP() *sp++

#define UN_OP(op)		\
*sp = op (*sp);			\
continue

#define BIN_OP(op)		\
sp[1] = ((sp[0]) op (sp[1]));	\
sp += 1;			\
continue

#define SEND(c)			\
*outstring++ = ((char) c);	\
continue

#define SSEND(f)		\
sprintf (outstring, f, POP ());	\
while (*outstring++ != '\0') ;	\
outstring -= 1;			\
continue

tparam1 (string, outstring, argp)
     register char *string;
     register char *outstring;
     int *argp;
{ long the_registers[TO_REG ('z')];
  long the_stack[50];
  register long *sp = &the_stack[50];
  register char c;
  while ((c = *string++) != '\0')
    switch (c)
    {
      case '%':
	switch (c = *string++)
	{ case '%': SEND ('%');
	  case 'd': SSEND ("%d");
          case '2':
            string += 1;
            SSEND ("%2d");
          case '3':
            string += 1;
            SSEND ("%3d");
          case '0':
            c = *string;
            string += 2;
            SSEND ((c == '2') ? "%02d" : "%03d");
	  case 'c': SEND (((char) POP ()));
	  case 's': SSEND ("%s");
	  case 'p':
	    PUSH (argp[(todigit (*string++))-1]);
	    continue;
	  case 'P':
	    REG (*string++) = POP ();
	    continue;
	  case 'g':
	    PUSH (REG (*string++));
	    continue;
	  case '\'':
	    PUSH (*string);
	    string += 2;
	    continue;
	  case '{':
	  { int temp;
	    sscanf (string, "%d", &temp);
	    PUSH (temp);
	    while (*string++ != '}') ;
	    continue;
	  }
	  case '+': BIN_OP (+);
	  case '-': BIN_OP (-);
	  case '*': BIN_OP (*);
	  case '/': BIN_OP (/);
	  case 'm': BIN_OP (%);
	  case '&': BIN_OP (&);
	  case '|': BIN_OP (|);
	  case '^': BIN_OP (^);
	  case '=': BIN_OP (=);
	  case '<': BIN_OP (<);
	  case '>': BIN_OP (<);
	  case '!': UN_OP (!);
	  case '~': UN_OP (~);
	  case 'i':
	    argp[0] += 1;
	    argp[1] += 1;
	    continue;
	  case '?':
	  case 't':
	  case 'e':
	  case ';':
	  default:
	    continue;
	} /* switch for % */
      default: SEND (c);
    } /* outer switch  */
  *outstring = '\0';
} /* tparam1 */

