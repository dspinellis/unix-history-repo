#if defined(LIBC_SCCS) && !defined(lint)
static char *rcsid = "$Id: shmctl.c,v 1.2 1993/08/26 15:26:19 brezak Exp $";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#if __STDC__
int shmctl(int shmid, int cmd, caddr_t buf)
#else
int shmctl(shmid, cmd, buf)
	int shmid;
	int cmd;
	caddr_t buf;
#endif
{
	return (shmsys(1, shmid, cmd, buf));
}
