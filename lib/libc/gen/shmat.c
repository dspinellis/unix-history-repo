#if defined(LIBC_SCCS) && !defined(lint)
static char *rcsid = "$Id: shmat.c,v 1.2 1993/08/26 15:26:18 brezak Exp $";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#if __STDC__
int shmat(int shmid, caddr_t shmaddr, int shmflg)
#else
int shmat(shmid, shmaddr, shmflg)
	int shmid;
	caddr_t shmaddr;
	int shmflg;
#endif
{
	return (shmsys(0, shmid, shmaddr, shmflg));
}
