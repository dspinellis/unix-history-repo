# include	<stdio.h>
# include	"constants.h"
# include	"globals.h"
# include	<sccs.h>

SCCSID(@(#)prtout.c	7.2	10/27/81)


/*
**  PRTOUT.C -- output routines
**
**	Output routines for non-specific data structures
**	(i.e. w_display is in [display.c])
*/



int	Fillcnt		= FILLCNT;
/*
**  W_CON -- write out a constant
**	Writes out a constant of type 'type'
**	pointed to by 'string'.
*/


w_con(type, string)
int	type;
char	*string;
{
	if (type == Tokens.sp_sconst)
		w_string(string, 1);
	else
		w_key(string);
}
/*
**  W_OP -- Writes out a string which doesn't need a blank
**	    to separate it from a keyword.
*/


w_op(string)
char		*string;
{
	w_raw(string);
	Lastc = OPCHAR;
}
/*
**  W_VAR -- writes out code to send the
**	     value of a C variable down to
**	     the Quel scanner.
**
**	Conserves the state of In_quote.
*/


w_var(disp, type)
int		type;
struct display	*disp;
{
	register struct disp_node	*d;
	register			savestat;

	savestat = In_quote;

	/* if was In_quote, then will want a space before the
	 * string written down
	 */
	if (savestat)
		w_key("");
	if (type != opIDSTRING)
	{
		w_new("IIcvar(");
		if (type != opSTRING)
			w_op("&");
	}
	else
		w_new("IIwrite(");
	w_display(disp);
	switch (type)
	{
	  case opSHORT:
		w_op(",1,2);");

	  case opFLOAT :
		w_op(",2,4);");
		break;

	  case opSTRING :
		w_op(",3,0);");
		break;

	  case opDOUBLE :
		w_op(",4,8);");
		break;

	  case opCHAR :
		w_op(",5,1);");
		break;

	  case opLONG :		/* also ints, since this is a VAX */
		w_op(",6,4);");
		break;

	  case opIDSTRING :
		w_op(");");
		break;

	  default :
		syserr("invalid type %d in w_var",
		type);
	}
	if (savestat)
	{
		begin_quote();
		/* if was In_quote, then will want a space 
		 * before next keyword
		 */
		w_key("");
	}
}
/*
**  W_KEY -- write out a string needing a blank to
**	     separate it from other keywords.
*/


w_key(string)
char	*string;
{
	if (Lastc == KEYCHAR)
		w_raw(" ");
	w_raw(string);
	Lastc = KEYCHAR;
}
/*
**  W_NEW -- write out a string after getting out of
**	     any pending IIwrite's.
*/

w_new(string)
char	*string;
{
	end_quote();
	w_op(string);
}
/*
**  BEGIN_QUOTE -- Issue an IIwrite("
*/


begin_quote()
{
	In_string = 1;
	In_quote = 1;
	Fillmode = 1;
	w_op("IIwrite(\"");
}
/*
**  END_QUOTE -- End any pending IIwrite("
*/


end_quote()
{
	In_string = 0;
	if (In_quote)
		w_op("\");");
	In_quote = 0;
}
/*
**  EQUATE_LINES -- Make subsequent lines be output on the
**		    same line they were read (lines of C_CODE only).
**
**	Note: Because of the algorithm used, it is possible that
**		the correct line in the output has already been passed,
**		in which case equate_lines does nothing.
*/


equate_lines()
{
	Fillmode = 0;
	while (Lineout < yyline)
		w_raw("\n");
	Lastc = OPCHAR;
}
/*
**  W_SYNC -- Put out an IIsync() call
*/


w_sync()
{
	w_new("IIsync(");
	w_file();
	w_op(");");
}
/*
**  W_FLUSH -- Put out an IIflush_tup() call
*/


w_flush()
{
	w_new("IIflushtup(");
	w_file();
	w_op(");");
}
/*
**  W_FILE -- Writes out the name and line number of the
**	      input file if Rtdb is specified, else a 0.
*/


w_file()
{
	char		itemp [6];

	if (Rtdb)
	{
		w_string(Input_file_name, 0);
		itoa(yyline, itemp);
		w_op(",");
		w_key(itemp);
	}
	else
		w_key("0");
}
/*
**  W_STRING -- Writes out a string
**
**	String is output as a string constant if type == 0
**	otherwise writes out string inside an IIwrite(
*/


w_string(string, type)
char	*string;
int	type;
{
	register char	*t;
	register char	*s;

	if (type)
	{
		if (!In_quote)
			begin_quote();
		w_raw("\\\"");
	}
	else
		w_raw("\"");

	s = t = string;
	In_string += 1;
	for ( ;*t ; )
	{
		if (*t == '\\')
		{
		  
			if (t [1] == '\n')
			{
				*t = '\0';
				w_raw(s);
				s = t = &t [2];
				w_raw("\\\n");
			}
			else
			{
				*t++ = '\0';
				w_raw(s);
				s = t;
				/* note that this call must be atomic,
				 * as w_raw would feel free to put newlines
				 * in if not.
				 */
				if (type)
					w_raw("\\\\");
				else
					w_raw("\\");
			}
		}
		else if (*t == '"')
		{
			w_raw("\\\"");
			s = ++t;
		}
		else
			t++;
	}
	w_raw(s);
	In_string -= 1;
	if (type)
		w_raw("\\\"");
	else
		w_raw("\"");
}
/*
**  W_RAW -- Lowest level output character routine
**
**	Outputs string depending on Fillcnt and In_quote
**	and In_string and Fillmode.
**	When not in Fillmode does straight output.
**	When on Fillmode, fills lines to Fillmode.
**
**	NOTE : w_raw will feel free to output a newline after 
**		'string' if the string causes more than Fillcnt
**		characters to be output.
**		Inside strings (In_string != 0) w_raw will put
**		a '\\' before the newline issued.
**		When In_quote != 0 when the fillcnt is exceeded,
**		the IIwrite( is ended an continued on the next line
**		so that the query string won't overflow the C 
**		pre-processor's line buffer.
*/


w_raw(string)
char	*string;
{
	register char	*s;
	register	charcnt;

	charcnt = 0;
	for (s = string; *s; s++)
	{
		if (*s != '\n')
		{
			putc(*s, Out_file);
			charcnt++;
		}
		else
		{
			if (Fillmode == 0 ||
			   Charcnt + charcnt > Fillcnt ||
			   In_string)
			{
				putc(*s, Out_file);
				Lineout++;
				charcnt = 0;
				Charcnt = 0;
			}
			else
			{
				putc(' ', Out_file);
				charcnt++;
			}
		}
	}
	if ((Charcnt += charcnt) > Fillcnt && Fillmode == 1)
	{
		if (In_string)
		{
			if (In_quote)
			{
				puts("\");\nIIwrite(\"", Out_file);
				Charcnt = 9;
			}
			else
			{
				puts("\\\n", Out_file);
				Charcnt = 0;
			}
		}
		else
		{
			putc('\n', Out_file);
			Charcnt = 0;
		}
		Lineout++;
	}
}
/*
**  PUTS -- put a string on an output file using putc()
*/

puts(s, file)
char	*s;
FILE	*file;
{
	register char	*sp;
	register FILE	*f;

	f = file;
	for (sp = s; *sp; )
		putc(*sp++, f);
}
