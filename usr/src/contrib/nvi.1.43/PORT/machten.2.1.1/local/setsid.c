/*
 * setsid() -- create session and set process group ID.
 */

#include <sys/types.h>
#include <unistd.h>


pid_t
setsid()
{
	return( setpgrp(getpid(), 0) );
}
