/*
 *	A quick and dirty C program to spit out possible identifiers
 *	from a stream of *.c and *.h files.  Takes a single parameter
 *	which specifies the minimum length of an identifier to be
 *	extracted.
 *
 */
 
#include <stdio.h>
#include <ctype.h>

#define FIRSTCHAR(a) (isalpha(a) || (a)=='_')
#define OTHERCHAR(a) (FIRSTCHAR(a) || isdigit(a))
#define TRUE 1
#define FALSE 0

int size = 0;
char buffer[512];
char *bp = buffer;

main (argc, argv)
     int argc;
     char *argv[];
{
  register int ch;
  register int spitout;
  register int eating_comment;

  if (argc == 2)
    {
      size = atoi (argv[1]);
    }
  spitout = FALSE;
  eating_comment = FALSE;
  while ((ch = getchar()) != EOF)
    {
      if (ch == '/')
	{
	  if ((ch = getchar()) == EOF)
	    {
	      fprintf (stderr, "unexpected EOF!\n");
	      exit (1);
	    }
	  else
	    {
	      if (ch == '*')
		{
		  eating_comment = TRUE;
		}
	      else
		{
		  ungetc (ch, stdin);
		}
	    }
	}
      else if (eating_comment && ch == '*')
	{
	  if ((ch = getchar()) == EOF)
	    {
	      fprintf (stderr, "unexpected EOF!\n");
	      exit (1);
	    }
	  else
	    {
	      if (ch == '/')
		{
		  eating_comment = FALSE;
		}
	      else
		{
		  ungetc (ch, stdin);
		}
	    }
	}
      else if (!eating_comment)
	{
	  if (!spitout && FIRSTCHAR(ch))
	    {
	      spitout = TRUE;
	      *bp++ = ch;
	    }
	  else if (spitout && OTHERCHAR(ch))
	    {
	      *bp++ = ch;
	    }
	  else if (spitout)
	    {
	      *bp++ = '\000';
	      bp = buffer;
	      if (strlen (bp) >= size)
		{
		  printf ("%s\n", bp);
		}
	      spitout = FALSE;
	    }
	}
    }
  return (0);
}
