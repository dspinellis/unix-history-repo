#include <stdioprivate.h>
#include <strstream.h>

int sscanf(const char* string, const char* format, ...)
{
    va_list args;
    va_start(args, format);
    strstreambuf stream((char*)string, 0, NULL);
    int ret = stream.vscan(format, args);
    va_end(args);
    return ret;
}
