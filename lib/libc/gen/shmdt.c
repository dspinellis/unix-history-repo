#if defined(LIBC_SCCS) && !defined(lint)
static char *rcsid = "$Id: shmdt.c,v 1.2 1993/08/26 15:26:20 brezak Exp $";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#if __STDC__
int shmdt(caddr_t shmaddr)
#else
int shmdt(shmaddr)
	caddr_t shmaddr;
#endif
{
	return (shmsys(2, shmaddr));
}
