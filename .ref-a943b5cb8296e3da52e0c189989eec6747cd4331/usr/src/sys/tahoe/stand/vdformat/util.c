#ifndef lint
static char sccsid[] = "@(#)util.c	1.1 (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"
#include	"cmd.h"

/*
*/

to_track(daddr)
dskadr	daddr;
{
	return ((daddr.cylinder * CURRENT->vc_ntrak) + daddr.track);
}


/*
*/

dskadr *from_track(trk)
int	trk;
{
	static dskadr	temp;

	temp.cylinder = trk / CURRENT->vc_ntrak;
	temp.track = trk % CURRENT->vc_ntrak;
	temp.sector = 0;
	return &temp;
}


/*
*/

to_sector(daddr)
dskadr	daddr;
{
	return ((to_track(daddr) * CURRENT->vc_nsec) + daddr.sector);
}


/*
*/

dskadr *from_sector(sec)
unsigned int	sec;
{
	static dskadr	temp;

	temp = *from_track((int)(sec / CURRENT->vc_nsec));
	temp.sector = sec % CURRENT->vc_nsec;
	return &temp;
}


/*
**
*/

print_unix_block(dskaddr)
dskadr	dskaddr;
{
	char	fs;
	int	blk;
	int	sec_per_blk=DEV_BSIZE / SECSIZ;
			
	indent();
	blk = to_sector(dskaddr) / sec_per_blk;
	print("** Warning - Unable to relocate sector %d:\n",to_sector(dskaddr)); 
#ifdef notdef
	indent();
	print("to map out using BADSECT use the following values:\n");
	indent();
	for(fs = 0; fs < 8; fs++) {
		int	s, l;

		s = CURRENT->partition[fs].par_start;
		l = CURRENT->partition[fs].par_len;
		if((blk < (s+l)) && (blk >= s)) {
			print("On the '%c' Partition use block %d.\n",
			    fs+'a', blk-s);
		}
	}
#endif
	exdent(3);
}

dskadr	*from_unix(par, block)
unsigned char	par;
unsigned int	block;
{
	unsigned int	sector;
	register int	fs;
	static dskadr		dskaddr;

#ifdef notdef
	fs =  tolower(par) - 'a';
	if((fs < 8) && (block <= CURRENT->partition[fs].par_len))
		dskaddr = *from_sector(
		    (CURRENT->partition[fs].par_start + block) *
		     DEV_BSIZE/SECSIZE);
	else {
#endif
		dskaddr.cylinder = -1;
		dskaddr.track = -1;
		dskaddr.sector = -1;
#ifdef notdef
	}
#endif
	return &dskaddr;
}


blkzero(addr, len)
register char	*addr;
register int	len;
{
	while(len--)
		*(addr++) = (char)0;
}


blkcopy(from, to, len)
register char	*from;
register char	*to;
register int	len;
{
	while(len--)
		*(to++) = *(from++);
}


boolean blkcmp(a, b, len)
register char	*a;
register char	*b;
register int	len;
{
	while(len--) {
		if(*(a++) != *(b++))
			return false;
	}
	return true;
}

