/* Merge parameters into a termcap entry string.
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


/* Assuming `string' is the value of a termcap string entry
   containing `%' constructs to expand parameters,
   merge in parameter values and store result in block `outstring' points to.
   No check is made for overflowing `outstring';
   the caller is wise to allocate space for it based on the size of
   `string', knowing that the size can increase by at most a couple
   of characters per parameter.
   The third and following args to tparam serve as the parameter values.  */

/* VARARGS 2 */
tparam (string, outstring, arg)
     char *string;
     char *outstring;
     int arg;
{
  tparam1 (string, outstring, 0, 0, &arg);
}

char *BC;
char *UP;

static char tgoto_buf[50];

char *
tgoto (cm, hpos, vpos)
     char *cm;
     int hpos, vpos;
{
  int args[2];
  if (!cm)
    return 0;
  args[0] = vpos;
  args[1] = hpos;
  tparam1 (cm, tgoto_buf, BC, UP, args);
  return tgoto_buf;
}

tparam1 (string, outstring, up, left, argp)
     char *string;
     register char *outstring;
     char *up, *left;
     register int *argp;
{
  register int c;
  register char *p = string;

  register int tem;
  int *oargp = argp;
  char *oleft = left;

  while (c = *p++)
    {
      if (c == '%')
	{
	  c = *p++;
	  tem = *argp;
	  switch (c)
	    {
	    case 'd':		/* %d means output in decimal */
	      if (tem < 10)
		goto onedigit;
	      if (tem < 100)
		goto twodigit;
	    case '3':		/* %3 means output in decimal, 3 digits. */
	      *outstring++ = tem / 100 + '0';
	    case '2':		/* %2 means output in decimal, 2 digits. */
	    twodigit:
	      tem %= 100;
	      *outstring++ = tem / 10 + '0';
	    onedigit:
	      *outstring++ = tem % 10 + '0';
	      argp++;
	      break;

	    case 'C':
	      /* For c-100: print quotient of value by 96, if nonzero,
		 then do like %+ */
	      if (tem >= 96)
		{
		  *outstring++ = tem / 96;
		  tem %= 96;
		}
	    case '+':		/* %+x means add character code of char x */
	      tem += *p++;
	    case '.':		/* %. means output as character */
	      if (oleft)
		{
		  /* If want to forbid output of 0 and \n,
		     and this is one, increment it.  */
		  if (tem == 0 || tem == '\n')
		    tem++;
		  /* If this isn't one, cancel the compensation string
		     that would otherwise compensate for
		     the incrementation that we are not going to do.  */
		  else
		    {
		      if (argp == oargp)
			left = 0;
		      else
			up = 0;
		    }
		}
	      *outstring++ = tem | 0200;
	    case 'f':		/* %f means discard next arg */
	      argp++;
	      break;

	    case 'b':		/* %b means back up one arg (and re-use it) */
	      argp--;
	      break;

	    case 'r':		/* %r means interchange following two args */
	      argp[0] = argp[1];
	      argp[1] = tem;
	      oargp++;
	      break;

	    case '>':		/* %>xy means if arg is > char code of x, */
	      if (argp[0] > *p++) /* then add char code of y to the arg, */
		argp[0] += *p;	/* and in any case don't output. */
	      p++;		/* Leave the arg to be output later. */
	      break;

	    case 'a':		/* %a means arithmetic */
	      /* Next character says what operation.
		 Add or subtract either a constant or some other arg */
	      /* First following character is + to add or - to subtract
		 or = to assign.  */
	      /* Next following char is 'p' and an arg spec
		 (0100 plus position of that arg relative to this one)
		 or 'c' and a constant stored in a character */
	      tem = p[2] & 0177;
	      if (p[1] == 'p')
		tem = argp[tem - 0100];
	      if (p[0] == '-')
		argp[0] -= tem;
	      else if (p[0] == '+')
		argp[0] += tem;
	      else
		argp[0] = tem;

	      p += 3;
	      break;

	    case 'i':		/* %i means add one to arg, */
	      argp[0] ++;	/* and leave it to be output later. */
	      argp[1] ++;	/* Increment the following arg, too!  */
	      break;

	    case '%':		/* %% means output %; no arg. */
	      goto ordinary;

	    case 'n':		/* %n means xor each of next two args with 140 */
	      argp[0] ^= 0140;
	      argp[1] ^= 0140;
	      break;

	    case 'm':		/* %m means xor each of next two args with 177 */
	      argp[0] ^= 0177;
	      argp[1] ^= 0177;
	      break;

	    case 'B':		/* %B means express arg as BCD char code. */
	      argp[0] += 6 * (tem / 10);
	      break;

	    case 'D':		/* %D means weird Delta Data transformation */
	      argp[0] -= 2 * (tem % 16);
	      break;
	    }
	}
      else
	/* Ordinary character in the argument string.  */
      ordinary:
	*outstring++ = c;
    }
  *outstring = 0;
  if (left)
    strcat (outstring, left);
  if (up)
    strcat (outstring, up);
}

#ifdef DEBUG

main (argc, argv)
     int argc;
     char **argv;
{
  char buf[50];
  int args[3];
  args[0] = atoi (argv[2]);
  args[1] = atoi (argv[3]);
  args[2] = atoi (argv[4]);
  tparam1 (argv[1], buf, "LEFT", "UP", args);
  printf ("%s\n", buf);
  return 0;
}

#endif /* DEBUG */
