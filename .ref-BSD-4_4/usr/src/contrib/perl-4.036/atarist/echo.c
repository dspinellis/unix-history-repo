/*
 * echo args, globbing is necessary.
 * usage:
 *	echo [-n] [args ...]
 * \n \r \b \c \v \\ \f \t \NNN escapes supported
 * -n and \c mean dont echo the final newline.
 *
 * ++jrb  bammi@cadence.com
 */

#include <stdio.h>

#if __STDC__
# include <compiler.h>
#else
# define __PROTO(X) ()
#endif

char	**glob __PROTO((char *patt, int decend_dir));
int	contains_wild __PROTO((char *patt));
void	free_all __PROTO((void));


int final_newline = 1; /* turned off by -n or \c */

int main(argc, argv)
int argc;
char **argv;
{
    --argc; ++argv;
    if((*argv)[0] == '-')
    {
	if ((*argv)[1] == 'n')
	    final_newline = 0;
	else
	{
	    fputs("usage: echo [-n] [arguement ... ]\n", stderr);
	    return 1;
	}
	--argc; ++argv;
    }

    while(argc--)
    {
	char *word = *argv;
	char **list;

	if(contains_wild(word) && (list = glob(word, 0)))
	{
	    while(*list)
	    {
		fputs(*list, stdout);
	        if(*++list) putchar(' ');
	    }
	    free_all();
	}
	else
	{
	    char c;
	    for(c = *word; c; c = (*word)? *++word : 0)
	    {
		if(c != '\\')
		    putchar(c);
		else
		{
		    switch(*++word)
		    {
			case 'b':  putchar('\b'); break;
			case 'f':  putchar('\f'); break;
			case 'n':  putchar('\n'); break;
			case 'r':  putchar('\r'); break;
		        case 't':  putchar('\t'); break;
			case 'v':  putchar('\v'); break;
		        case '\\': putchar('\\'); break;
			case 'c':  final_newline = 0; break;
			default:   putchar(*word); /* ?? */
			case '0':
			{
			    int n = 0;
			    for(c = *++word; (c >= '0') && (c <= '7'); c = *++word)
				n = (n << 3) + (c - '0');
			    putchar(n);
			}
		    }
		}
	    }
	}
	if(*++argv) putchar(' ');
    }
    if(final_newline) putchar('\n');
    return 0;
}

				  
	
	
	
	
