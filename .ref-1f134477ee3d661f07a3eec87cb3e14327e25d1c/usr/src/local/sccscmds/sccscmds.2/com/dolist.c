# include	"../hdr/defines.h"

static char Sccsid[] = "@(#)dolist.c	4.3	%G%";

static char br[] = "bad range (co12)";

dolist(pkt,list,ch)
struct packet *pkt;
register char *list;
char ch;
{
	char str[32];
	struct sid lowsid, highsid, sid;
	int n;

	while (*list) {
		list = getasid(list,&lowsid);
		if (*list == '-') {
			++list;
			list = getasid(list,&highsid);
			if (lowsid.s_br == 0) {
				if ((highsid.s_br || highsid.s_seq ||
					highsid.s_rel < lowsid.s_rel ||
					(highsid.s_rel == lowsid.s_rel &&
					highsid.s_lev < lowsid.s_lev)))
						fatal(br);
				sid.s_br = sid.s_seq = 0;
				for (sid.s_rel = lowsid.s_rel; sid.s_rel <= highsid.s_rel; sid.s_rel++) {
					sid.s_lev = (sid.s_rel == lowsid.s_rel ? lowsid.s_lev : 1);
					for ( ; (sid.s_rel < highsid.s_rel ||
						 sid.s_lev <= highsid.s_lev) &&
						(n = sidtoser(&sid,pkt)); sid.s_lev++)
						enter(pkt,ch,n,&sid);
				}
			}
			else {
				if (!(highsid.s_rel == lowsid.s_rel &&
					highsid.s_lev == lowsid.s_lev &&
					highsid.s_br == lowsid.s_br &&
					highsid.s_seq >= lowsid.s_seq))
						fatal(br);
				for (; lowsid.s_seq <= highsid.s_seq &&
					(n = sidtoser(&lowsid,pkt)); lowsid.s_seq++)
						enter(pkt,ch,n,&lowsid);
			}
		}
		else {
			if (n = sidtoser(&lowsid,pkt))
				enter(pkt,ch,n,&lowsid);
		}
		if (*list == ',')
			++list;
	}
}


static char dls[] = "delta list syntax (co13)";

getasid(p,sp)
register char *p;
register struct sid *sp;
{
	register char *old;

	p = sid_ab(old = p,sp);
	if (old == p || sp->s_rel == 0)
		fatal(dls);
	if (sp->s_lev == 0) {
		sp->s_lev = MAX;
		if (sp->s_br || sp->s_seq)
			fatal(dls);
	}
	else if (sp->s_br) {
		if (sp->s_seq == 0)
			sp->s_seq = MAX;
	}
	else if (sp->s_seq)
		fatal(dls);
	return(p);
}
