// This may look like C code, but it is really -*- C++ -*-
#pragma once
#include <stdio.h>
#include <std.h>

/* Handles necessary screen-manipultations. */
class Screen_Handler
{
 private:
  static char  term_entry[1024];   /* Holds termcap entry for current terminal. */
  static char  temp_buf[100];      /* Holds inverse screen attributes. */
  static int   width;              /* Current screen width, needed to format output. */
  static char *current_ptr;        /* Pointer to current position in temp_buf. */
  static char *inverse_start;      /* Control sequence beginning inverse mode. */
  static char *inverse_end;        /* Control sequence ending inverse mode. */
  
  static void  center (char *buf); /* Prints out leading spaces to center BUF. */
  static int   fputchar (int i);   /* Prints a character to standard output. */

public:
              Screen_Handler (void);              /* Initialize the screen width. */
  static int  screen_width (void);                /* Return current screen width. */
  static void print_inverse_centered (char *buf); /* Centers, inverts, and prints BUF. */
};

/* See comments in .cc file for inline functions. */

#ifdef __OPTIMIZE__
inline int 
Screen_Handler::screen_width (void)
{
  return width;
}

inline int 
Screen_Handler::fputchar (int i)
{
  putchar (i);
}

inline void 
Screen_Handler::center (char *buf)
{
  int offset;
  int len = strlen (buf);
  
  offset = width - len >> 1;
  
  for (int i = 0; i < offset; i++)
    putchar (' ');
}

inline void 
Screen_Handler::print_inverse_centered (char *buf)
{
  putchar ('\n');
  center (buf);
  tputs (inverse_start, 1, fputchar);
  printf ("%s\n\n", buf);
  tputs (inverse_end, 1, fputchar);
}

#endif // __OPTIMIZE__

