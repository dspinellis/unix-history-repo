/*
 *	Copyright 1974 Bell Telephone Laboratories Inc
 */

int	(*bdevsw[])()
{
	&nulldev,	&nulldev,	&rkstrategy, 	&rktab,
	0
};

int	(*cdevsw[])()
{
	&klopen,   &klclose,   &klread,   &klwrite,   &klsgtty,
	&nulldev,  &nulldev,   &mmread,   &mmwrite,   &nodev,
	&nulldev,  &nulldev,   &rkread,   &rkwrite,   &nodev,
	&pcopen,   &pcclose,   &pcread,   &pcwrite,   &nodev,
	&dcopen,   &dcclose,   &dcread,   &dcwrite,   &dcsgtty,
	0
};

int	rootdev	{(0<<8)|0};
int	swapdev	{(0<<8)|0};
int	swplo	4000;
int	nswap	872;
