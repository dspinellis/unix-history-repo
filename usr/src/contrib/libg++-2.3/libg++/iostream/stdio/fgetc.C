#include "stdioprivate.h"

extern "C" int fgetc(FILE *fp)
{
    return getc(fp);
}
