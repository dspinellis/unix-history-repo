# include	"../hdr/defines.h"

SCCSID(@(#)newstats	2.1);

newstats(pkt,strp,ch)
register struct packet *pkt;
register char *strp;
register char *ch;
{
	char fivech[6];
	repeat(fivech,ch,5);
	putline(pkt,sprintf(strp,"%c%c %s/%s/%s\n",CTLCHAR,STATS,
					fivech,fivech,fivech),0);
}
