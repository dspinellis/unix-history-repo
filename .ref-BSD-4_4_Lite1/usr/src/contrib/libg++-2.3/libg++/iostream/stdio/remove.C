#include <stdioprivate.h>

int remove(const char* filename)
{
  if (unlink (filename) == -1)
   return -1;

  return 0;
}
