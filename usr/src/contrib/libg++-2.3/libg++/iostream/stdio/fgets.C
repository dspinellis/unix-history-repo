#include "stdioprivate.h"

extern "C" char* fgets(char* s, int n, FILE* stream)
{
    if (!__validfp(stream))
	return NULL;
    long len = ((streambuf*)stream)->sgetline(s, n, '\n', 1);
    if (len <= 0)
	return NULL;
    return s;
}
