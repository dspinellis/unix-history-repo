static	char *sccsid = "@(#)renice.c	4.3 (Berkeley) 81/03/11";
#include <sys/param.h>
#include <sys/dir.h>
#include <sys/user.h>
#include <sys/proc.h>
#include <nlist.h>
#include <stdio.h>

struct	proc *proc;
struct nlist nl[] = {
	{"_proc"},
	{ "_nproc" },
	{0},
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
	register struct proc *pp, *pend;
	int pid, nice, nproc;
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
	if ((addr = nl[1].n_value) == 0) {
		fprintf(stderr, "/vmunix: _nproc not in namelist");
		exit(1);
	}
	lseek(mem, addr, 0);
	read(mem, &nproc, sizeof nproc);
	proc = (struct proc *) calloc(nproc, sizeof *proc);
	addr = nl[0].n_value;
	if (addr == 0) {
		fprintf(stderr, "/vmunix: _proc not in namelist");
		exit(1);
	}
	lseek(mem, addr, 0);
	read(mem, &addr, sizeof addr);
	lseek(mem, addr, 0);
	read(mem, proc, nproc * (sizeof *proc));
	pend = proc+nproc;
	for (pp = proc; pp < pend; pp++)
		if (pp->p_pid == pid)
			break;
	if (pp >= pend) {
		fprintf(stderr, "%d: process not found\n", pid);
		exit(1);
	}
	fprintf(stderr, "%d: old nice = %d, new nice = %d\n",
			pid, pp->p_nice - NZERO,
			nice - NZERO);
	a1 = (int)&pp->p_nice;
	a2 = (int)proc;
	coreaddr = a1-a2+addr;
	lseek(mem, (long)coreaddr, 0);
	write(mem, &nice, sizeof pp->p_nice);
}
