/* $Header: weapon.h,v 7.0 86/10/08 15:18:20 lwall Exp $ */

/* $Log:	weapon.h,v $
 * Revision 7.0  86/10/08  15:18:20  lwall
 * Split into separate files.  Added amoebas and pirates.
 * 
 */

EXT int tractor INIT(0);

EXT int etorp;
EXT int btorp;

EXT OBJECT *isatorp[2][3][3];

EXT int aretorps;

void fire_torp();
void attack();
void fire_phaser();
int tract();
void weapon_init();
