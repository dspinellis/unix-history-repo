// This may look like C code, but it is really -*- C++ -*-
#ifndef option_h
#define option_h 1

/* Process directory listing program options. */

/* Enumeration of all directory listing options. */
enum option_type
{
  HIDDEN = 01, /* Print hidden files (those beginning with '.') */
  LINK   = 02, /* Distinguish between directory and file links. */
};

class Option_Handler
{
private:
  static unsigned option_word;  /* Compact bitwise-storage for program options. */
  static char    *program_name; /* Name of listing program. */
  static void     usage (void); /* Prints usage then exits. */
  
public:
                  Option_Handler (void);                /* Initialize options. */
  void            operator() (int argc, char *argv[]);  /* Process command-line options. */
  int             operator[] (option_type option);      /* Check if option is enabled. */
};

/* Speed things up a bit if we're optimizing. */
#ifdef __OPTIMIZE__
inline int
Option_Handler::operator[] (option_type option) 
{ 
  return option_word & option;
}
#endif // __OPTIMIZE__
#endif
