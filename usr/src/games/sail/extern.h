/*
 * @(#)extern.h	1.6 83/10/05
 */
#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#include "machdep.h"

#define die()		((rand() >> 3) % 6 + 1)
#define sqr(a)		((a) * (a))
#define abs(a)		((a) > 0 ? (a) : -(a))
#define min(a,b)	((a) < (b) ? (a) : (b))

#define grappled(a)	Snagged(a, 1)
#define fouled(a)	Snagged(a, 0)
#define snagged(a)	(Snagged(a, 0) || Snagged(a, 1))
#define grappled2(a,b)	Snagged2(a, b, 1, 0)
#define fouled2(a,b)	Snagged2(a, b, 0, 0)
#define snagged2(a,b)	(Snagged2(a, b, 0, 0) || Snagged2(a, b, 1, 0))
#define Xgrappled2(a,b)	Snagged2(a, b, 1, 1)
#define Xfouled2(a,b)	Snagged2(a, b, 0, 1)
#define Xsnagged2(a,b)	(Snagged2(a, b, 0, 1) || Snagged2(a, b, 1, 1))

#define sterncolour(sp)	((sp)->file->stern+'0'-((sp)->file->captured?16:0))
#define sternrow(sp)	((sp)->file->row + dr[(sp)->file->dir])
#define sterncol(sp)	((sp)->file->col + dc[(sp)->file->dir])

#define capship(sp)	((sp)->file->captured?(sp)->file->captured:(sp))

#define readyname(r)	((r) & R_LOADING ? '*' : ((r) & R_INITIAL ? '!' : ' '))

/* loadL and loadR, should match loadname[] */
#define L_EMPTY		0		/* should be 0, don't change */
#define L_GRAPE		1
#define L_CHAIN		2
#define L_ROUND		3
#define L_DOUBLE	4
#define L_EXPLODE	5

/*
 * readyL and readyR, these are bits, except R_EMPTY
 */
#define R_EMPTY		0		/* not loaded and not loading */
#define R_LOADING	1		/* loading */
#define R_DOUBLE	2		/* loading double */
#define R_LOADED	4		/* loaded */
#define R_INITIAL	8		/* loaded initial */

#define HULL		0
#define RIGGING		1

#define W_CAPTAIN	1
#define W_CAPTURED	2
#define W_CLASS		3
#define W_CREW		4
#define W_DBP		5
#define W_DRIFT		6
#define W_EXPLODE	7
#define W_FILE		8
#define W_FOUL		9
#define W_GUNL		10
#define W_GUNR		11
#define W_HULL		12
#define W_LAST		13
#define W_OBP		14
#define W_PCREW		15
#define W_PEOPLE	16
#define W_POINTS	17
#define W_QUAL		18
/* 19 */
#define W_RIGG		20
#define W_SHIPCOL	21
#define W_SHIPDIR	22
#define W_SHIPROW	23
#define W_SIGNAL	24
#define W_SINK		25
#define W_STRUCK	26
#define W_TA		27
#define W_TIME		28
#define W_TURN		29
#define W_WIND		30
#define W_FS		31
#define W_GRAP		32
#define W_RIG1		33
#define W_RIG2		34
#define W_RIG3		35
#define W_RIG4		36

struct logs {
	char l_name[20];
	int l_uid;
	int l_shipnum;
	int l_gamenum;
	int l_netpoints;
};

struct BP {
	short turnsent;
	struct ship *toship;
	short mensent;
};

struct snag {
	short turnfoul;
	struct ship *toship;
};

#define NSCENE	31
#define NSHIP	10
#define NBP	3

#define NNATION	5
#define N_A	0
#define N_B	1
#define N_S	2
#define N_F	3
#define N_J	4

struct File {
	char captain[20];		/* 0 */
	short points;			/* 20 */
	char loadL;			/* 22 */
	char loadR;			/* 24 */
	char readyL;			/* 26 */
	char readyR;			/* 28 */
	struct BP OBP[NBP];		/* 30 */
	struct BP DBP[NBP];		/* 48 */
	char struck;			/* 66 */
	struct ship *captured;		/* 68 */
	short pcrew;			/* 70 */
	char last[10];			/* 72 */
	char drift;			/* 82 */
	struct snag fouls[NSHIP];	/* 84 */
	struct snag grapples[NSHIP];	/* 124 */
	char signal[60];		/* 164 */
	char RH;			/* 224 */
	char RG;			/* 226 */
	char RR;			/* 228 */
	char FS;			/* 230 */
	char explode;			/* 232 */
	char sink;			/* 234 */
	char dir;
	short col;
	short row;
	char loadwith;
	char stern;
};

struct ship {
	char *shipname;			/* 0 */
	struct shipspecs *specs;	/* 2 */
	char nationality;		/* 4 */
	short shiprow;			/* 6 */
	short shipcol;			/* 8 */
	char shipdir;			/* 10 */
	struct File *file;		/* 12 */
};

struct scenario {
	char winddir;			/* 0 */
	char windspeed;			/* 2 */
	char windchange;		/* 4 */
	short turn;			/* 6 */
	char people;			/* 8 */
	char time;			/* 10 */
	char vessels;			/* 12 */
	char *name;			/* 14 */
	struct ship ship[NSHIP];	/* 16 */
};
struct scenario scene[NSCENE];

struct shipspecs {
	char bs;
	char fs;
	char ta;
	short guns;
	char class;
	char hull;
	char qual;
	char crew1;
	char crew2;
	char crew3;
	char gunL;
	char gunR;
	char carL;
	char carR;
	char rig1;
	char rig2;
	char rig3;
	char rig4;
	short pts;
};
struct shipspecs specs[];

struct scenario *cc;		/* the current scenario */
struct ship *ls;		/* &cc->ship[cc->vessels] */

#define SHIP(s)		(&cc->ship[s])
#define foreachship(sp)	for ((sp) = cc->ship; (sp) < ls; (sp)++)

struct windeffects {
	char A, B, C, D;
};
struct windeffects WET[7][6];

struct Tables {
	char H, G, C, R;
};
struct Tables RigTable[11][6];
struct Tables HullTable[11][6];

char AMMO[9][4];
char HDT[9][10];
char HDTrake[9][10];
char QUAL[9][5];
char MT[9][3];

char *countryname[];
char *classname[];
char *directionname[];
char *qualname[];
char loadname[];

char rangeofshot[];

char dr[], dc[];

long lastsync;
int winddir, windspeed, turn;
int game;
char Outbuf[BUFSIZE];
FILE *syncfile;

char *info();
char *quality();
double arctan();
char *saywhat();
struct ship *closestenemy();

char *calloc();
char *strcpy();
char *strcat();
char *strncpy();
char *getenv();
char *gets();
