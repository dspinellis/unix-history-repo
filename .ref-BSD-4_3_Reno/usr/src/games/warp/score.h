/* $Header: score.h,v 7.0 86/10/08 15:13:21 lwall Exp $ */

/* $Log:	score.h,v $
 * Revision 7.0  86/10/08  15:13:21  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#define ENTBOUNDARY 100000	/*  point boundary across which a new E is
					awarded */

#define BASEBOUNDARY 250000	/*  point boundary across which a new B is
					awarded */

EXT int oldstatus;
EXT int oldetorp;
EXT int oldbtorp;
EXT int oldstrs;
EXT int oldenemies;

EXT long totalscore;
EXT long lastscore INIT(0);
EXT long curscore;
EXT long possiblescore;
EXT long oldeenergy;
EXT long oldbenergy;
EXT long oldcurscore;

EXT char savefilename[40];

#ifdef SCOREFULL
#define COMPOFF 0
#define COMPNAME longlognam
#define COMPLEN 24
#else
#define COMPOFF 24
#define COMPNAME longlognam
#define COMPLEN 8
#endif
EXT char longlognam[128];

EXT char c INIT(' ');

void score_init();
void wscore();
void display_status();
void wavescore();
void score();
void save_game();
