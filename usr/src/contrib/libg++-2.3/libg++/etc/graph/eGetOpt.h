// -*- C++ -*-
#ifndef eGetOpt_h
#pragma interface
#define eGetOpt_h 1

#include <GetOpt.h>
#include <String.h>

// eGetOpt is a subclass of GetOpt which provides functions for
// handling arguments to the options.

class eGetOpt : public GetOpt
{
public:
  eGetOpt (int argc, char **argv, char *optstring) 
    : GetOpt(argc,argv,optstring) {}

  // first_char returns the first character of the argument.

  int first_char () { return nargv[optind][0];};

  // next_arg looks at next argument for an interger, double or string
  // depending on the type of argument given to it. If the correct type is
  // found, the value is set and next_arg returns 1.  If the type is not
  // correct, next_arg returns 0.
  
  // double arguments start with a digit, plus, minus or period.
  // integer arguments start with a digit.
  // String arguments have no restriction.

  // If the next argument is an integer, set the reference variable to it
  // and increment the index to the options.  Return 1 if an integer is
  // found, else return 0.
  
  int next_arg (int &i);
  int next_arg (double &d);
  int next_arg (String &s);
};

#endif
