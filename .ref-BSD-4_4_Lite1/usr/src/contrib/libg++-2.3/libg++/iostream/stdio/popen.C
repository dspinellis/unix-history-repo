#include "stdioprivate.h"
#include <errno.h>
#include <procbuf.h>

extern "C" FILE * popen(const char *command, const char *mode)
{
    return (FILE*)(new procbuf(command, mode[0] == 'r' ? ios::in : ios::out));
}

extern "C" int pclose(FILE *stream)
{
#if 0
    if (stream is not a (procbuf*))
	return -1;
#endif
    // Does not actually test that stream was created by popen(). Instead,
    // it depends on the filebuf::sys_close() virtual to Do The Right Thing.
    return fclose(stream);
}
