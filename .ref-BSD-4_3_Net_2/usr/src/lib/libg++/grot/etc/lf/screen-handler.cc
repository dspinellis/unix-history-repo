/* Handles screen manipulations for screen width and inverse mode. */
#include "screen-handler.h"

/* Initializes the current screen width via
   the terminal independent operation routines. */

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

/* Returns current screen width. */

#ifndef __OPTIMIZE__
int 
Screen_Handler::screen_width (void)
{
  return width;
}

int 
Screen_Handler::fputchar (int i)
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
#endif // __optimize__
