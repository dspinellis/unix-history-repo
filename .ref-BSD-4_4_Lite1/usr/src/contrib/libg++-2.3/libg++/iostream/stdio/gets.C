#include "stdioprivate.h"

extern "C" char* gets(char* s)
{
    long len = ((streambuf*)stdin)->sgetline(s, 50000, '\n', 0);
    if (len <= 0)
	return NULL;
    return s;
}
