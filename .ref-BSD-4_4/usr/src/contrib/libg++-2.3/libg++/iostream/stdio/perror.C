#include <stdioprivate.h>
#include <errno.h>
#include <string.h>
#ifndef errno
extern int errno;
#endif
extern "C" char* strerror(int);

void perror (const char *s)
{
    char *error;

    if (s != NULL && *s != '\0') {
	fputs (s, stderr);
	fputs (": ", stderr);
    }

    if ((error = strerror (errno)) != NULL)
	fputs (error, stderr);

    fputc ('\n', stderr);
}
