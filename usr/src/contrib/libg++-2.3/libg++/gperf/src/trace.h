#ifndef trace_h
#define trace_h 1

#ifdef TRACE
#define T(X) X
#else
#define T(X)
#endif

class Trace
{
private:
  static int nesting;
  char *name;
public:
  Trace (char *n) { fprintf (stderr, "%*scalling %s\n", 3 * nesting++, "", name = n); }
 ~Trace (void) { fprintf (stderr, "%*sleaving %s\n", 3 * --nesting, "", name); }
};

#endif
