#include <stdioprivate.h>
#include <strstream.h>

int vsscanf(const char* string, const char* format, _G_va_list args)
{
    strstreambuf stream((char*)string, 0, NULL);
    int ret = stream.vscan(format, args);
    return ret;
}
