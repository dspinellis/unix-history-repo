#include <stdio.h>
int	errno;

warn (str)
char	*str;
{
    fprintf (stderr, "%s [errno = %d]\n", str, errno);
}

error (str)
char	*str;
{
    warn (str);
    exit (-1);
}
