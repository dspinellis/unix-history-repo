/* This file provides the startup routine for files loaded with
   the -A option of GNU ld.  When a program makes a call to the
   initial address of code incrementally loaded, it expects that the
   first function laid out in the object files loaded will
   be the function called.  In GNU C++, we slip crt1.o in front of
   the object files the user specifies, so that we can call any needed
   global constructors, and set up calls for global destructors.
   Control is then passed to the first routine that the user specified.  */

typedef struct set_vector {
  unsigned int length;
  unsigned int vector[1];
} set_vector;

/*		********  WARNING ********
    Note that the address of _incstart() should be the start
    of text space.

    Michael Tiemann, Stanford University.  */
 
extern set_vector __CTOR_LIST__;
extern set_vector __DTOR_LIST__;
extern set_vector *__dlp;
extern int __dli;
void (*_initfn)() = 0;

static void
_incstart ()
{
  register void (**ppf)() = (void (**)())__CTOR_LIST__.vector;
  int i, len = __CTOR_LIST__.length;

  __dli = __DTOR_LIST__.length;
  __DTOR_LIST__.vector[__dli] = (int)__dlp;
  __dlp = &__DTOR_LIST__;

  for (i = 0; i < len; i++)
    (*ppf[i]) ();
  (*_initfn)();
}

static int
_end_crt1 ()
{
}
