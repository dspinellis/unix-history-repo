int	center[]
{
	2,3,4,4,4,4,3,2,
	3,6,8,8,8,8,6,3,
	4,8,12,12,12,12,8,4,
	4,8,12,14,14,12,8,4,
	4,8,12,14,14,12,8,4,
	4,8,12,12,12,12,8,4,
	3,6,8,8,8,8,6,3,
	2,3,4,4,4,4,3,2
};

int	wheur1();
int	wheur2();
int	wheur3();
int	wheur4();
int	wheur5();
int	wheur6();
int	wheur[]
{
	&wheur1,
	&wheur2,
	&wheur3,
	&wheur4,
	&wheur5,
	&wheur6,
	0
};

int	bheur1();
int	bheur2();
int	bheur3();
int	bheur4();
int	bheur5();
int	bheur6();
int	bheur[]
{
	&bheur1,
	&bheur2,
	&bheur3,
	&bheur4,
	&bheur5,
	&bheur6,
	0
};

int	ipval[]
{
	-3000, -900, -500, -300, -300, -100,
	0,
	100, 300, 300, 500, 900, 3000
};

int	moveno	1;
int	depth	2;
int	qdepth	8;
int	mdepth	4;
int	flag	033;
int	eppos	64;
int	bkpos	4;
int	wkpos	60;
int	edge[]
{
	040, 020, 010, 0, 0, 1, 2, 4
};
int	board[]
{
	4, 2, 3, 5, 6, 3, 2, 4,
	1, 1, 1, 1, 1, 1, 1, 1,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0,
	-1, -1, -1, -1, -1, -1, -1, -1,
	-4, -2, -3, -5, -6, -3, -2, -4,
};
