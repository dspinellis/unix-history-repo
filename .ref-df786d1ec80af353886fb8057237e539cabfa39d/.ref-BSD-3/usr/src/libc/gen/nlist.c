#include <a.out.h>
int a_magic[] = {A_MAGIC1, A_MAGIC2, A_MAGIC3, A_MAGIC4, 0};
#define SPACE 100		/* number of symbols read at a time */

nlist(name, list)
char *name;
struct nlist *list;
{
	register struct nlist *p, *q;
	register int f, n, m, i, nreq;
	long sa;
	struct exec buf;
	struct nlist space[SPACE];

	for(p = list, nreq = 0; p->n_name[0]; p++, nreq++) {
		p->n_type = 0;
		p->n_value = 0;
	}
	f = open(name, 0);
	if(f < 0)
		return(-1);
	read(f, (char *)&buf, sizeof buf);
	for(i=0; a_magic[i]; i++)
		if(a_magic[i] == buf.a_magic) break;
	if(a_magic[i] == 0){
		close(f);
		return(-1);
	}
	sa = buf.a_text + (long)buf.a_data;
	sa += buf.a_trsize + buf.a_drsize;
	sa += sizeof buf;
	lseek(f, sa, 0);
	n = buf.a_syms;

	while(n){
		m = sizeof space;
		if(n < sizeof space)
			m = n;
		read(f, (char *)space, m);
		n -= m;
		for(q = space; (m -= sizeof(struct nlist)) >= 0; q++) {
			for(p = list; p->n_name[0]; p++) {
				for(i=0;i<8;i++)
					if(p->n_name[i] != q->n_name[i]) goto cont;
				p->n_value = q->n_value;
				p->n_type = q->n_type;
				if(--nreq == 0)
					goto alldone;
				break;
		cont:		;
			}
		}
	}
alldone:
	close(f);
	return(0);
}


