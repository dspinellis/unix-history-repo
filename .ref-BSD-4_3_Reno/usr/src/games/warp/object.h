/* $Header: object.h,v 7.0.1.2 86/12/12 17:01:38 lwall Exp $ */

/* $Log:	object.h,v $
 * Revision 7.0.1.2  86/12/12  17:01:38  lwall
 * Baseline for net release.
 * 
 * Revision 7.0.1.1  86/10/16  10:52:30  lwall
 * Added Damage.  Fixed random bugs.
 * 
 * Revision 7.0  86/10/08  15:13:04  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

#define Root 0
#define Base 1
#define Enterprise 2
#define Star 3
#define Torp 4
#define Enemy 5
#define Web 6
#define Crusher 7

typedef struct object {
    char posx, posy;
#ifdef SIGNEDCHAR
    char velx, vely;
#else
    short velx, vely;
#endif
    struct object *next, *prev, *contend;
    long energy;
    long mass;
    char type;
    char image;
    char strategy;
    char flags;
} OBJECT;

#define PIRATE 1	/* we may mutiny */
#define FRIENDLY 2	/* we aren't really an enemy, for now */
#define STATIC 4	/* we are not in the movers list at the moment */
#define COUNTDOWN 8	/* we are counting down for something */
#define CLOAKS 16	/* we can cloak */

#ifdef DOINIT
OBJECT root = {0, 0, 0, 0, &root, &root, 0, 0, 0, Root, '?', 0, 0};
#else
EXT OBJECT root;
#endif

#ifdef DOINIT
OBJECT free_root = {0, 0, 0, 0, &free_root, &free_root, 0, 0, 0, Root, '?', 0, 0};
#else
EXT OBJECT free_root;
#endif

EXT OBJECT *ent;
EXT OBJECT *base;
EXT OBJECT *enemies;
EXT OBJECT *movers;
EXT OBJECT *realapollo;
EXT OBJECT *nuke;

EXT OBJECT *occupant[YSIZE][XSIZE];

OBJECT *make_object();

void unmake_object();
void free_object();
void object_init();
