int	(*bdevsw[])()
{
	&nulldev,	&nulldev,	&rfstrategy,
	&nulldev,	&nulldev,	&rkstrategy,
	&nulldev,	&tcclose,	&tcstrategy,
	&tmopen,	&tmclose,	&tmstrategy,
	&nulldev,	&nulldev,	&rkstrategy
};

int	(*cdevsw[])()
{
	&klopen,   &klclose,   &klread,   &klwrite,   &klsgtty,
	&dcopen,   &dcclose,   &dcread,   &dcwrite,   &dcsgtty,
	&pcopen,   &pcclose,   &pcread,   &pcwrite,   &nodev,
	&dpopen,   &dpclose,   &dpread,   &dpwrite,   &nodev,
	&dnopen,   &dnclose,   &nodev,    &dnwrite,   &nodev,
	&nulldev,  &nulldev,   &mmread,   &mmwrite,   &nodev,
	&vtopen,   &vtclose,   &nodev,    &vtwrite,   &nodev,
	&daopen,   &daclose,   &daread,   &dawrite,   &nodev,
	&ctopen,   &ctclose,   &nodev,    &ctwrite,   &nodev,
	&vsopen,   &vsclose,   &vsread,   &vswrite,   &nodev,
	&nodev,   &nodev,   &nodev,   &nodev,   &nodev
};
