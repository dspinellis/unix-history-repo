#include <stdarg.h>
#include <stdioprivate.h>

extern "C" int sprintf(char *string, const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    int ret = vsprintf(string, format, ap);
    va_end(ap);
    return ret;
}
