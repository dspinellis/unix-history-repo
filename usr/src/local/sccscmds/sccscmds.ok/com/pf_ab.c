# include	"../hdr/defines.h"

SCCSID(@(#)pf_ab	2.1.1.1);

pf_ab(s,pp,all)
char *s;
register struct pfile *pp;
int all;
{
	register char *p;
	register int i;
	extern char *Datep;
	char *xp;

	xp = p = alloca(size(s));
	copy(s,p);
	for (; *p; p++)
		if (*p == '\n') {
			*p = 0;
			break;
		}
	p = xp;
	p = sid_ab(p,&pp->pf_gsid);
	++p;
	p = sid_ab(p,&pp->pf_nsid);
	++p;
	i = index(p," ");
	pp->pf_user[0] = 0;
	if (((unsigned)i) < 8) {
		move(p,pp->pf_user,i);
		pp->pf_user[i] = 0;
	}
	else
		fatal("bad p-file format (co17)");
	p = p + i + 1;
	date_ab(p,&pp->pf_date);
	p = Datep;
	pp->pf_ilist = 0;
	pp->pf_elist = 0;
	if (!all || !*p)
		return;
	p =+ 2;
	xp = alloc(size(p));
	copy(p,xp);
	p = xp;
	if (*p == 'i') {
		pp->pf_ilist = ++p;
		for (; *p; p++)
			if (*p == ' ') {
				*p++ = 0;
				p++;
				break;
			}
	}
	if (*p == 'x')
		pp->pf_elist = ++p;
}
