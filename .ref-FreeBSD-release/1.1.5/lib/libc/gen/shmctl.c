#if defined(LIBC_SCCS) && !defined(lint)
static char *rcsid = "$Id: shmctl.c,v 1.1 1993/09/27 00:57:47 rgrimes Exp $";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>

#if __STDC__
int shmctl(int shmid, int cmd, void *buf)
#else
int shmctl(shmid, cmd, buf)
	int shmid;
	int cmd;
	void *buf;
#endif
{
	return (shmsys(1, shmid, cmd, buf));
}
