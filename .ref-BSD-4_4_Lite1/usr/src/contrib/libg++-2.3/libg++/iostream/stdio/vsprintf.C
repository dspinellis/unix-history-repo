#include <stdioprivate.h>
#include <strstream.h>

int vsprintf(char* string, const char* format, _G_va_list args)
{
    strstreambuf stream(string, -1, string);
    int ret = stream.vform(format, args);
    stream.sputc(0);
    return ret;
}
