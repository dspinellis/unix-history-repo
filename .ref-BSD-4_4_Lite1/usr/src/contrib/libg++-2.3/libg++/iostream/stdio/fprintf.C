#include <stdarg.h>
#include <stdioprivate.h>

// Define non-macro versions of stdin/stdout/stderr, for use by debuggers.
// Define them here, assuming that almost all programs require fprintf().

#undef stdin
#undef stdout
#undef stderr
FILE* stdin = (FILE*)&__std_filebuf_0;
FILE* stdout = (FILE*)&__std_filebuf_1;
FILE* stderr = (FILE*)&__std_filebuf_2;

int fprintf(FILE *fp, const char* format, ...)
{
    streambuf* sb = FILE_to_streambuf(fp);
    if (!sb)
	return EOF;
    va_list args;
    va_start(args, format);
    int ret = sb->vform(format, args);
    va_end(args);
    return ret;
}
