# include	"../hdr/defines.h"

static char Sccsid[] = "@(#)setup.c	1.3	%G%";

setup(pkt,serial)
register struct packet *pkt;
int serial;
{
	register int n;
	register struct apply *rap;
	int	first_app = 1;

	pkt->p_apply[serial].a_inline = 1;
	for (n = maxser(pkt); n; n--) {
		rap = &pkt->p_apply[n];
		if (rap->a_inline) {
			if (n != 1 && pkt->p_idel[n].i_pred == 0)
				fmterr(pkt);
			pkt->p_apply[pkt->p_idel[n].i_pred].a_inline = 1;
			if (pkt->p_idel[n].i_datetime > pkt->p_cutoff)
				condset(rap,NOAPPLY,CUTOFF);
			else {
				if (first_app)
					bcopy(&pkt->p_idel[n].i_sid,
					     &pkt->p_gotsid,
					     sizeof(pkt->p_gotsid));
				first_app = 0;
				condset(rap,APPLY,EMPTY);
			}
		}
		else
			condset(rap,NOAPPLY,EMPTY);
		if (rap->a_code == APPLY)
			ixgsetup(pkt->p_apply,&(pkt->p_idel[n].i_ixg));
	}
}


ixgsetup(ap,ixgp)
struct apply *ap;
struct ixg *ixgp;
{
	int n;
	int code, reason;
	register int *ip;
	register struct ixg *cur, *prev;

	for (cur = ixgp; cur = (prev = cur)->i_next; ) {
		switch (cur->i_type) {

		case INCLUDE:
			code = APPLY;
			reason = INCL;
			break;
		case EXCLUDE:
			code = NOAPPLY;
			reason = EXCL;
			break;
		case IGNORE:
			code = EMPTY;
			reason = IGNR;
			break;
		}
		ip = cur->i_ser;
		for (n = cur->i_cnt; n; n--)
			condset(&ap[*ip++],code,reason);
	}
}


condset(ap,code,reason)
register struct apply *ap;
int code, reason;
{
	if (code == EMPTY)
		ap->a_reason |= reason;
	else if (ap->a_code == EMPTY) {
		ap->a_code = code;
		ap->a_reason |= reason;
	}
}
