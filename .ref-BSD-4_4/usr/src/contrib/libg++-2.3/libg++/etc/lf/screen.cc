/* Handles screen manipulations for screen width and inverse mode. */
#include "screen.h"

#ifndef hpux
extern "C" int tgetent(void *, const char *);
extern "C" int tgetnum(const char*);
extern "C" char *tgetstr(const char *, char**);
typedef int (*int_func)(int);
extern "C" void tputs(char *, int, int_func);
#endif

/* Initializes the current screen width via
   the terminal independent operation routines. */

char   Screen_Handler::term_entry[1024]; 
char   Screen_Handler::temp_buf[100];
int    Screen_Handler::width; 
char * Screen_Handler::current_ptr;
char * Screen_Handler::inverse_start;
char * Screen_Handler::inverse_end;

Screen_Handler::Screen_Handler (void)
{
  if (tgetent (term_entry, getenv ("TERM")) != 1)
    {
      perror ("main");
      exit (1);
    }
  else
    {
      width         = tgetnum ("co") - 1;
      current_ptr   = temp_buf;
      inverse_start = tgetstr("so", &current_ptr);
      inverse_end   = tgetstr("se", &current_ptr);
    }
}


static int fputchar (int i)
{
  return putchar (i);
}

/* Prints out leading blanks so as to center buf 
   assuming a screen width of width. */

void 
Screen_Handler::center (char *buf)
{
  int offset;
  int len = strlen (buf);

  offset = width - len >> 1;

  for (int i = 0; i < offset; i++)
    putchar (' ');
}

/* Centers, ``inverse-videos'' and prints buf. */

void 
Screen_Handler::print_inverse_centered (char *buf)
{
  putchar ('\n');
  center (buf);
  tputs (inverse_start, 1, fputchar);
  printf ("%s\n\n", buf);
  tputs (inverse_end, 1, fputchar);
}
