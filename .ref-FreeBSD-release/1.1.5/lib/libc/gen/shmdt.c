#if defined(LIBC_SCCS) && !defined(lint)
static char *rcsid = "$Id: shmdt.c,v 1.1 1993/09/27 00:57:48 rgrimes Exp $";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#if __STDC__
int shmdt(void *shmaddr)
#else
int shmdt(shmaddr)
	void *shmaddr;
#endif
{
	return (shmsys(2, shmaddr));
}
