/*
   Tests the generated perfect has function.
   The -v option prints diagnostics as to whether a word is in 
   the set or not.  Without -v the program is useful for timing.
*/ 
  
#include <stdio.h>

#define MAX_LEN 200

char *in_word_set (const char *, int);

int 
main (int argc, char **argv)
{
  int   verbose = argc > 1 ? 1 : 0;
  char  buf[MAX_LEN];
  char *s;

  while (gets (buf)) 
    if ((s = in_word_set (buf, strlen (buf))) && verbose)
      printf ("%s is prefix for %s\n", buf, s);
    else if (verbose) 
      printf ("NOT in word set %s\n", buf);

  return 0;
}
