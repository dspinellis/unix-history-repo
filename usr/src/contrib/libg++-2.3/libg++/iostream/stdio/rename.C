#include <stdioprivate.h>

int rename (const char *_old, const char *_new)
{
  if (link (_old, _new) == -1)
   return -1;

  if (unlink (_old) == -1)
   return -1;

  return 0;
}
