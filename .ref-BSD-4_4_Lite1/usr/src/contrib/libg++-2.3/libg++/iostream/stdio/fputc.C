#include <stdioprivate.h>

extern "C" int fputc(int c, FILE *fp)
{
    return putc(c, fp);
}
