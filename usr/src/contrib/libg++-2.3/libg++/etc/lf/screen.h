// This may look like C code, but it is really -*- C++ -*-
#ifndef screen_h
#define screen_h 1
#include <std.h>
#include <stdio.h>
#include <curses.h>



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

public:
              Screen_Handler (void);              /* Initialize the screen width. */
  static int  screen_width (void);                /* Return current screen width. */
  static void print_inverse_centered (char *buf); /* Centers, inverts, and prints BUF. */
};

/* See comments in .cc file for inline functions. */

/* Returns current screen width. */
inline int 
Screen_Handler::screen_width (void)
{
  return width;
}
#endif
