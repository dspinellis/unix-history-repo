#include <sys/param.h>
# include	"../hdr/defines.h"

SCCSID(@(#)permiss.c	1.3	%G%);

finduser(pkt)
register struct packet *pkt;
{
	register char *p;
	register int i;
	char *user;
	char *index();
	int gid, ngroups = NGROUPS, groups[NGROUPS];
	int none;

	none = 1;
	user = logname();
	if (getgroups(&ngroups, groups) < 0) {
		perror("getgroups");
		ngroups = 0;
	}
	while ((p = getline(pkt)) != NULL && *p != CTLCHAR) {
		none = 0;
		repl(p,'\n','\0');	/* this is done for equal test below */
		if (!pkt->p_user) {
			if (equal(user,p))
				pkt->p_user = 1;
			else if (gid = atoi(p))
				for (i = 0; i < ngroups; i++)
				    if (gid == groups[i]) {
					pkt->p_user = 1;
					break;
				    }
		}
		*(index(p,'\0')) = '\n';/* repl \0 end of line w/ \n again */
	}
	if (none)
		pkt->p_user = 1;
	if (p == NULL || p[1] != EUSERNAM)
		fmterr(pkt);
}


char	*Sflags[NFLAGS];

doflags(pkt)
struct packet *pkt;
{
	register char *p;
	register int k;

	for (k = 0; k < NFLAGS; k++)
		Sflags[k] = 0;
	while ((p = getline(pkt)) != NULL && *p++ == CTLCHAR && *p++ == FLAG) {
		NONBLANK(p);
		k = *p++ - 'a';
		NONBLANK(p);
		Sflags[k] = alloc(size(p));
		copy(p,Sflags[k]);
		for (p = Sflags[k]; *p++ != '\n'; )
			;
		*--p = 0;
	}
}


permiss(pkt)
register struct packet *pkt;
{
	extern char *Sflags[];
	register char *p;
	register int n;

	if (!pkt->p_user)
		fatal("not authorized to make deltas (co14)");
	if (p = Sflags[FLORFLAG - 'a']) {
		if (((unsigned)pkt->p_reqsid.s_rel) < (n = patoi(p))) {
			sprintf(Error,"release %u < %u (floor) (co15)",pkt->p_reqsid.s_rel,n);
			fatal(Error);
		}
	}
	if (p = Sflags[CEILFLAG - 'a'])
		if (((unsigned)pkt->p_reqsid.s_rel) > (n = patoi(p))) {
			sprintf(Error,"release %u > %u (ceiling) (co16)",pkt->p_reqsid.s_rel,n);
			fatal(Error);
		}
}
