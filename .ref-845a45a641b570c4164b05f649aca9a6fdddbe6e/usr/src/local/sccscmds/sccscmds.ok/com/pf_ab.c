# include	"../hdr/defines.h"

static char Sccsid[] = "@(#)pf_ab.c	1.5	%G%";

pf_ab(s,pp,all)
char *s;
register struct pfile *pp;
int all;
{
	register char *p;
	register int i;
	extern char *Datep;
	char *xp, *buf;

	xp = p = buf = alloc(size(s));
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
	xp = index(p,' ');
	pp->pf_user[0] = 0;
	if ((unsigned)(i = xp-p) < SZLNAM) {
		bcopy(p,pp->pf_user,i);
		pp->pf_user[i] = 0;
	}
	else
		fatal("bad p-file format (co17)");
	p = xp + 1;
	date_ab(p,&pp->pf_date);
	p = Datep;
	pp->pf_ilist = 0;
	pp->pf_elist = 0;
	if (!all || !*p) {
		free(buf);
		return;
	}
	p += 2;
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
	free(buf);
}
