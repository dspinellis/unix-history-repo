#include <sys/types.h>
#include <sys/wait.h>
#include <sys/errno.h>

#define SIZE0	3000000
#define INCR	64000
#define	SMALLINCR	1024

int nproc = 0;
int size0 = SIZE0;
int incr = INCR;

main(ac, av)
char *av[];
{
	int i;
	union wait status;

	if (ac > 1)
		nproc = atoi(av[1]);
	if (ac > 2)
		size0 = atoi(av[2]);
	if (ac > 3)
		incr = atoi(av[3]);
	for (i = 0; i == 0 || i < nproc; i++)
		grow(size0, incr);
	if (nproc > 0) for (;;) {
		i = wait(&status);
		if (i == -1)
			perror("wait");
		else
			grow(size0, incr);
	}
	exit(0);
}

grow(size0, incr)
{
	u_long top;
	extern int etext, errno;

	if (nproc <= 1 || fork() == 0) {
		if (brk(size0) == -1)
			perror("brk");
		while (sbrk(incr) != -1)
			;
		while (sbrk(SMALLINCR) != -1)
			;
		if (errno != ENOMEM)
			perror("sbrk");
		top = sbrk(0);
		printf("ended at %d (0x%x), data size %d (0x%x)\n",
			top, top, top - (int)&etext, top - (int)&etext);
		exit(0);
	}
}
