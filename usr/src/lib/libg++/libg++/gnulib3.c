
#ifndef NO_GNULIB3  /* skip entire file if NO_GNULIB3 */

typedef struct set_vector
{
  int length;
  int vector[1];
  /* struct set_vector *next; */
} set_vector;

set_vector __CTOR_LIST__;
set_vector __DTOR_LIST__;
set_vector *__dlp;
int __dli;

extern  void exit (int);
extern void __do_global_init ();
extern void __do_global_cleanup ();
extern  void on_exit(void*, void*);
extern  void _cleanup();
extern  void _exit(int);


#if defined(i386) && !defined(sequent)
#define COFF
#endif

#ifdef COFF_ENCAPSULATE
#undef COFF
#endif

/* 
  ON_EXIT macro commented out - dl 
  Sun on_exit doesn't get linked in right!!!
*/

#if 0
#if defined(sun)
#define ON_EXIT(PROCP, ARG) \
  do { extern void PROCP (); on_exit (PROCP, ARG); } while (0)
#endif
#endif

int
__main ()
{
  /* Gross hack for GNU ld.  This is defined in `builtin.cc'
     from libg++.  */
#ifndef COFF
  extern int __1xyzzy__;
#endif

#ifdef ON_EXIT

#ifdef sun
  ON_EXIT (_cleanup, 0);
#endif

  ON_EXIT (__do_global_cleanup, 0);

#endif
  __dli = __DTOR_LIST__.length;
  __dlp = &__DTOR_LIST__;
#ifndef COFF
  __do_global_init (&__1xyzzy__);
#else
  __do_global_init ();
#endif
}

#ifndef ON_EXIT

void 
exit (status)
     int status;
{
  __do_global_cleanup ();
  _cleanup ();
  _exit (status);
}
#endif

void
__do_global_init ()
{
  register int i, len;
  register void (**ppf)() = (void (**)())__CTOR_LIST__.vector;

  len = __CTOR_LIST__.length;
  for (i = 0; i < len; i++)
    (*ppf[i])();
}

void
__do_global_cleanup ()
{
  while (__dlp)
    {
      while (--__dli >= 0)
	{
	  void (*pf)() = (void (*)())__dlp->vector[__dli];
	  (*pf)();
	}
      __dlp = (struct set_vector *)__dlp->vector[__dlp->length];
      if (__dlp) __dli = __dlp->length;
    }
}

#endif
