/* Library code for programs which use -fhandle-exceptions.
   Note: do *not* compile this with -fhandle-exceptions.  */


#ifdef __GNUG__
#pragma implementation
#endif
#include <setjmp.h>
#include <stream.h>

struct
ExceptionHandler
{
  ExceptionHandler *prev;
  jmp_buf handler;
  void *name;
  void *parameters;
  ExceptionHandler ();
  ~ExceptionHandler ();
} EHS, *exceptionHandlerStack = &EHS;

ExceptionHandler::ExceptionHandler ()
{
  if (this == &EHS)
    {
      if (setjmp (EHS.handler))
	{
	  cerr << ("unhandled exception, aborting...\n");
	  abort ();
	}
    }
  else
    {
      this->prev = exceptionHandlerStack;
      exceptionHandlerStack = this;
    }
}

ExceptionHandler::~ExceptionHandler ()
{
  exceptionHandlerStack = this->prev;
}

