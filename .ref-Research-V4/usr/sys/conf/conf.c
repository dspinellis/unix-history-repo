/*
 *	Copyright 1974 Bell Telephone Laboratories Inc
 */

int	(*bdevsw[])()
{
	&nulldev,	&nulldev,	&rkstrategy, 	&rktab,
	&nulldev,	&tcclose,	&tcstrategy, 	&tctab,
	&tmopen,	&tmclose,	&tmstrategy, 	&tmtab,
	0
};

int	(*cdevsw[])()
{
	&klopen,   &klclose,   &klread,   &klwrite,   &klsgtty,
	&nulldev,  &nulldev,   &rkread,   &rkwrite,   &nodev,
	&tmopen,   &tmclose,   &tmread,   &tmwrite,   &nodev,
	&dhopen,   &dhclose,   &dhread,   &dhwrite,   &dhsgtty,
	&pcopen,   &pcclose,   &pcread,   &pcwrite,   &nodev,
	0
};

int	rootdev	{(0<<8)|0};
int	swapdev	{(0<<8)|0};
int	swplo	4000;
int	nswap	872;
