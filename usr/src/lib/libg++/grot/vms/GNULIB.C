/* Subroutines needed by GCC output code on some machines.  */
/* Compile this file with the Unix C compiler!  */

/* Define the C data type to use for an SImode value.  */

#ifndef SItype
#define SItype long int
#endif

/* Define the type to be used for returning an SF mode value
   and the method for turning a float into that type.
   These definitions work for machines where an SF value is
   returned in the same register as an int.  */

#ifndef SFVALUE  
#define SFVALUE int
#endif

#ifndef INTIFY
#define INTIFY(FLOATVAL)  (intify.f = (FLOATVAL), intify.i)
#endif

union flt_or_int { int i; float f; };


#include <stdio.h>
/* This is used by the `assert' macro.  */
void
__eprintf (string, expression, line, filename)
     char *string;
     char *expression;
     int line;
     char *filename;
{
  fprintf (stderr, string, expression, line, filename);
}
