#ifndef lint
static char sccsid[] = "@(#)util.c	1.3 (Berkeley/CCI) %G%";
#endif

#include	"vdfmt.h"
#include	"cmd.h"

/*
*/

to_track(daddr)
dskadr	daddr;
{
	return ((daddr.cylinder * lab->d_ntracks) + daddr.track);
}


/*
*/

dskadr *from_track(trk)
int	trk;
{
	static dskadr	temp;

	temp.cylinder = trk / lab->d_ntracks;
	temp.track = trk % lab->d_ntracks;
	temp.sector = 0;
	return &temp;
}


/*
*/

to_sector(daddr)
dskadr	daddr;
{
	return ((to_track(daddr) * lab->d_nsectors) + daddr.sector);
}


/*
*/

dskadr *from_sector(sec)
unsigned int	sec;
{
	static dskadr	temp;

	temp = *from_track((int)(sec / lab->d_nsectors));
	temp.sector = sec % lab->d_nsectors;
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
	register struct partition *pp;
			
	indent();
	blk = to_sector(dskaddr);
	print("** Warning - Unable to relocate sector %d:\n",to_sector(dskaddr)); 
	indent();
	print("to map out using BADSECT use the following values:\n");
	indent();
	for(fs = 0; fs < lab->d_npartitions; fs++) {
		int	s, l;

		pp = &lab->d_partitions[fs];
		s = pp->p_offset;
		l = pp->p_size;
		if (pp->p_fsize == 0)
			pp->p_fsize = DEV_BSIZE;
		if((blk < (s+l)) && (blk >= s)) {
			print("On the `%c' Partition use filesystem block %d.\n",
			    fs+'a', (blk - s) * lab->d_secsize / pp->p_fsize);
		}
	}
	exdent(3);
}

dskadr	*from_unix(par, block)
unsigned char	par;
unsigned int	block;
{
	unsigned int	sector;
	register int	fs;
	register struct partition *pp;
	static dskadr		dskaddr;

	fs =  tolower(par) - 'a';
	if((fs < lab->d_npartitions) &&
	    (block <= (pp = &lab->d_partitions[fs])->p_size)) {
		if (pp->p_fsize == 0)
			pp->p_fsize = DEV_BSIZE;
		dskaddr = *from_sector(pp->p_offset +
		    block * pp->p_fsize / lab->d_secsize);
	} else {
		dskaddr.cylinder = -1;
		dskaddr.track = -1;
		dskaddr.sector = -1;
	}
	return &dskaddr;
}
