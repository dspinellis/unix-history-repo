#include <sys/param.h>
#include <sys/proc.h>
#include <stdio.h>

struct	proc proc[NPROC];
struct	{
	char		name[8];
	int		type;
	unsigned	value;
} nl[] = {
	"_proc", 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,	0, 0
};

/*
 * Change the running priority (nice) of a process which is already
 * running.
 *
 * Author: Kurt Shoens
 */

main(argc, argv)
	char **argv;
{
	register struct proc *pp;
	int pid, nice;
	int addr, mem, a1, a2, coreaddr;

	if (argc != 2 && argc != 3) {
		fprintf(stderr, "usage: renice pid [ priority ]\n");
		exit(1);
	}
	if (geteuid()) {
		fprintf(stderr, "NOT super user\n");
		exit(1);
	}
	pid = atoi(argv[1]);
	nice = atoi(argc == 3 ? argv[2] : "19");
	if (nice > 20)
		nice = 20;
	if (nice < -20)
		nice = -20;
	nice += NZERO;
	mem = open("/dev/kmem", 2);
	if (mem < 0) {
		perror("/dev/kmem");
		exit(1);
	}
	nlist("/vmunix", nl);
	addr = nl[0].value;
	if (addr == 0) {
		fprintf(stderr, "/vmunix: _proc not in namelist");
		exit(1);
	}
	lseek(mem, addr, 0);
	read(mem, &proc[0], sizeof proc);
	for (pp = &proc[0]; pp < &proc[NPROC]; pp++)
		if (pp->p_pid == pid)
			break;
	if (pp >= &proc[NPROC]) {
		fprintf(stderr, "%d: process not found\n", pid);
		exit(1);
	}
	fprintf(stderr, "%d: old nice = %d, new nice = %d\n",
			pid, 
			pp->p_nice - NZERO,
			nice - NZERO);
	a1 = (int)&pp->p_nice;
	a2 = (int)&proc[0];
	coreaddr = a1-a2+addr;
	lseek(mem, (long)coreaddr, 0);
	write(mem, &nice, 1);
}
