#include "/sys/nsys/param.h"
#include "/sys/nsys/proc.h"

prproc()
{
	int i;
	register *p;

	for(i=0; i<NPROC; i++) {
		p = &proc[i];
		if(p->p_stat != NULL) {
			printf("%d %o %o %d %d %o %o %o %o\n",
			i,
			p->p_stat,
			p->p_flag,
			p->p_pid,
			p->p_ppid,
			p->p_addr,
			p->p_size,
			p->p_wchan,
			p->p_textp);
		}
	}
}
