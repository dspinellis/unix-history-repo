%{
/*
 * Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 *  texttoip - convert a textual representation of interpress to a real
 *	       interpress file.  The text produced by "iptotext" is the input
 *	       that is recognized.
 *
 *  This is the lex specification of the input scanner.
 *
 *  Written for Xerox Corporation by William LeFebvre
 *
 *  6-May-1984
 *
 * HISTORY
 * 15-Jul-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Escaped characters (as octal numbers) are no longer processed
 *	here.  The popular null was causing problems.  Processing
 *	of these escapes has been moved to AppendString1 in libip.a .
 *
 * 01-Dec-85  lee at Xerox, WRC
 *	Linted.
 *
 * 28-apr-85  ed flint
 *	add conditional compilation for vax11-c (vms)
 *
 *
 * if using this file on vms, you must edit this file and add the following 
 * lines around the #include "stdio.h" line which is at the top of the file
 *
 *		#ifdef vax11c
 *		# include stdio
 *		#else
 *		# include "stdio.h"
 *		#endif
 */


#ifdef vax11c
# include ctype
#else
# include <ctype.h>
#endif

# include "iptokens.h"
# include "texttoken.h"
# include "ipnames.h"

# define    short_or_long(ch)	(ch != '>' ? 1 : 2)

extern long yylval_long;
extern char *yylval_charP;
char *buffptr;
char string_buffer[1024];
%}

%%

^"File: "	{
		    return(T_file);
		}

^"Header: "	{
		    return(T_header);
		}

^>[> ]"Comment: "	{
			    yylval_long = short_or_long(yytext[1]);
			    return(T_seq_comment);
			}

^>[> ]"Identifier: "	{
			    yylval_long = short_or_long(yytext[1]);
			    return(T_seq_identifier);
			}

^>[> ]"Insert file: "	{
			    yylval_long = short_or_long(yytext[1]);
			    return(T_seq_insert_file);
			}

^>[> ]"Integer: "	{
			    yylval_long = short_or_long(yytext[1]);
			    return(T_seq_integer);
			}

^>[> ]"Rational: "	{
			    yylval_long = short_or_long(yytext[1]);
			    return(T_seq_rational);
			}

^>[> ]"String: "	{
			    yylval_long = short_or_long(yytext[1]);
			    return(T_seq_string);
			}

^>[> ]"Adaptive Pixel Vector: "	{
			    yylval_long = short_or_long(yytext[1]);
			    return(T_seq_apv);
			}

^>[> ]"Compressed Pixel Vector: "	{
			    yylval_long = short_or_long(yytext[1]);
			    return(T_seq_cpv);
			}

^>[> ]"Packed Pixel Vector: "	{
			    yylval_long = short_or_long(yytext[1]);
			    return(T_seq_ppv);
			}

[+-]?[0-9A-Fa-f][0-9A-Fa-f]*	{
			    yylval_long = getnum(yytext);
			    return(T_number);
			}

^[a-zA-Z{}][a-zA-Z]*	{
			    register char *ptr;

			    for (ptr = yytext; *ptr; ptr++)
			    {
				if (isupper(*ptr))
				{
				    *ptr = tolower(*ptr);
				}
			    }
			    for (yylval_long = 0; yylval_long <= OP_LIMIT;
					yylval_long++)
			    {
				if (strcmp(yytext, op_names[yylval_long]) == 0)
				{
				    return(T_operator);
				}
			    }
			    return(T_operator);
			}

\"			{
			    unsigned char ch;
			    int val = 0;
			    int cnt = 0;

			    yylval_charP = buffptr = string_buffer;

			    while ((ch = input()) != '"')
			    {
				if (ch == '\\')
				{
				    ch = input();
				    if (ch >= '0' && ch <= '9')
				    {
					*buffptr++ = '\\';
				    }
				}
				*buffptr++ = ch;
			    }
			    *buffptr = '\0';
			    return(T_string);
			}

[a-zA-Z][a-zA-Z0-9\-]*	{
			    yylval_charP = yytext;
			    return(T_identifier);
			}

[ \t][ \t]*		{
			    yyleng = 0;		/* gobble white space */
			    yymore();
			}

\(.*\)			{
			    yyleng = 0;		/* gobble comments */
			    yymore();
			}

^\(.*\)\n		{
			    /* comment that spans the whole line */
			    yyleng = 0;
			    yymore();
			}

\n			{
			    return(T_newline);
			}

.			{
			    yylval_long = yytext[yyleng-1];
			    return(T_character);
			}

%%

extern int radix;

getnum(str)

char *str;

{
    register int val;
    register int digit;

    /* take the easy and fast way out for base 10 */
    if (radix == 10)
    {
	return(atoi(str));
    }

    /* otherwise this is a standard "convert any radix" algorithm */
    val = 0;
    while ((digit = *str++) != '\0')
    {
	if ((digit -= '0') > 9)
	{
	    /* it is a letter */
	    if ((digit -= ('A' - '0' - 10)) > 15)
	    {
		/* it is a lower case digit */
		digit -= ('a' - 'A');
	    }
	}
	if (digit >= radix)
	{
	    /* this is really too much!  give up at this point */
	    return(val);
	}
	val *= radix;
	val += digit;
    }
    return(val);
}
