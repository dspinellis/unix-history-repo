/*
 *	Quick and dirty program to select adjacent records that are common
 *	in the first <arg> character positions.
 *
 */
 
#include <stdio.h>

#define MAXSIZE 512

char ping[MAXSIZE];
char pong[MAXSIZE];

int flipflop = 0;
int size = MAXSIZE-1;

main (argc, argv)
     int argc;
     char *argv[];
{
    register int index;
    char *newbuf();
    
    if (argc == 2)
      {
	size = atoi (argv[1]);
      }
    while (newbuf() != NULL)
      {
	for (index=0; index < size; index++)
	  {
	    if (ping[index] != pong[index])
	      {
		break;
	      }
	  }
	if (index == size)
	  {
	    printf ("%s\n", ping);
	    printf ("%s\n", pong);
	  }
      }
    return (0);
}

char *
newbuf ()
{
  char *bufp;

  if (flipflop)
    {
      bufp = ping;
      flipflop = 0;
    }
  else
    {
      bufp = pong;
      flipflop = 1;
    }
  return (gets (bufp));
}
