typedef void (*vfp)();

static void
default_new_handler ();

vfp __new_handler = default_new_handler;

char *
__builtin_vec_new (p, maxindex, size, ctor)
     char *p;
     int maxindex, size;
     void (*ctor)();
{
  int i, nelts = maxindex + 1;
  char *rval;

  if (p == 0)
    p = (char *)__builtin_new (nelts * size);

  rval = p;

  for (i = 0; i < nelts; i++)
    {
      (*ctor) (p);
      p += size;
    }

  return rval;
}

vfp
__set_new_handler (handler)
     vfp handler;
{
  vfp prev_handler;

  prev_handler = __new_handler;
  if (handler == 0) handler = default_new_handler;
  __new_handler = handler;
  return prev_handler;
}

vfp
set_new_handler (handler)
     vfp handler;
{
  return __set_new_handler (handler);
}

static void
default_new_handler ()
{
  /* don't use fprintf (stderr, ...) because it may need to call malloc.  */
  write (2, "default_new_handler: out of memory... aaaiiiiiieeeeeeeeeeeeee!\n", 65);
  /* don't call exit () because that may call global destructors which
     may cause a loop.  */
  _exit (-1);
}
