typedef void (*vfp)();

extern vfp __new_handler;

char *
__builtin_new (sz)
     long sz;
{
  char *p;

  p = (char *)malloc (sz);
  if (p == 0)
    (*__new_handler) ();
  return p;
}
