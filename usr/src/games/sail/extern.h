/*
 * sccsid = "@(#)extern.h	1.2 83/05/19";
 */
#include <stdio.h>
#include <signal.h>
#include <ctype.h>
#include "machdep.h"

#define NUMOFSCENES 31

#define die() (((rand() >> 6) % 6) + 1)
#define fouled(a) Fouled(a, 342)
#define grappled(a) Fouled(a, 382)
#define grapple(a,b) Grapple(a,b,382)
#define foul(a,b) Grapple(a,b,342)
#define sqr(a) ((a) * (a))
#define abs(a) ((a) > 0 ? (a) : -(a))
#define min(a,b) ((a) < (b) ? (a) : (b))

#define GRAPE   1
#define CHAIN   2
#define ROUND   3
#define DOUBLE  4

#define HULL    0
#define RIGGING 1

#define SPECS	32
#define FILES	64
#define SCENARIO	128

struct logs {
    char fname[20];
    int uid, fshipnum, fgamenum, netpoints;
};

extern struct BP {
	int turnsent, toship, mensent;
};

extern struct snag {
	int turnfoul, toship;
};

typedef struct {
	int row, col, dir;
} postype;
extern postype pos[20];

extern struct File {
	char captain[20];
	int points;
	int loadL, loadR, readyL, readyR;
	struct BP OBP[3], DBP[3];
	int struck, captured, pcrew;
	char last[10];
	int drift;
	struct snag fouls[10], grapples[10];
	char signal[60];
	int RH, RG, RR, FS, explode, sink;
};

typedef struct {
	char *shipname;
	int shipnum, nationality;
	int shiprow, shipcol, shipdir;
	struct File *file;
} ships;

struct scenario {
	int winddir, windspeed, windchange;
	int turn, people, time, vessels;
	char *name;
	ships ship[10];
};
extern struct scenario scene[NUMOFSCENES];

struct shipspecs {
	int bs, fs, ta, guns, class, hull, qual, crew1,
		crew2, crew3, gunL, gunR, carL, carR,
		rig1, rig2, rig3, rig4, pts;
};
extern struct shipspecs specs[];

extern struct windeffects {
	int A, B, C, D;
} WET[7][6];
extern struct Tables {
	int H, G, C, R;
} RigTable[11][6];
extern struct Tables HullTable[11][6];

extern int AMMO[9][4];

extern int HDT[9][10];

extern int HDTrake[9][10];

extern int QUAL[9][5];

extern int MT[9][3];

extern int loaded, fired, changed, repaired, buffercount, xlast, ylast;
extern long lastsync;
extern int winddir, windspeed, turn, viewrow, viewcol;
extern int player, nation[5], scroll, game;
extern int MIGHTYCAPTAIN;
extern char Outbuf[BUFSIZE], movebuf[10], loadwith[20];
extern FILE *syncfile;

char colours();
char gunsbear();
char *info();
char iinitial();
char *quality();
char sterncolor();
char strend();
char symbol();
double arctan();
