#ifndef lint
static	char *sccsid = "@(#)assorted.c	1.1 83/03/17";
#endif
#define EXPLODE 5
#include "externs.h"

table(rig, shot, hittable, onship, fromship, roll)
int rig, shot, hittable, onship, fromship, roll;
{
	register int hhits = 0, chits = 0, ghits = 0, rhits = 0;
	int Ghit = 0, Hhit = 0, Rhit = 0, Chit = 0;
	int guns, car, pc, hull;
	int crew[3];
	register int n;
	int rigg[4];
	char message[60];
	struct shipspecs *ptr;
	struct File *ptr1;

	ptr = &specs[scene[game].ship[onship].shipnum];
	ptr1 = scene[game].ship[onship].file;
	pc = ptr1 -> pcrew;
	hull = ptr -> hull;
	crew[0] = ptr -> crew1;
	crew[1] = ptr -> crew2;
	crew[2] = ptr -> crew3;
	rigg[0] = ptr -> rig1;
	rigg[1] = ptr -> rig2;
	rigg[2] = ptr -> rig3;
	rigg[3] = ptr -> rig4;
	if (shot == GRAPE)
		Chit = chits = hittable;
	else {
		Chit = chits = rig ? RigTable[hittable][roll-1].C : HullTable[hittable][roll-1].C;
		Rhit = rhits = rig ? RigTable[hittable][roll-1].R : HullTable[hittable][roll-1].R;
		Hhit = hhits = rig ? RigTable[hittable][roll-1].H : HullTable[hittable][roll-1].H;
		Ghit = ghits = rig ? RigTable[hittable][roll-1].G : HullTable[hittable][roll-1].G;
		if (ptr1 -> FS)
			rhits *= 2;
		if (shot == CHAIN){
			Ghit = ghits = 0;
			Hhit = hhits = 0;
		}
	}
	if (ptr1 -> captured > -1){
		pc -= (chits + 1)/2;
		chits /= 2;
	}
	for (n=0; n < 3; n++)
		if (chits > crew[n]){
			chits -= crew[n];
			crew[n] = 0;
		}
		else {
			crew[n] -= chits;
			chits = 0;
		}
	for (n=0; n < 3; n++)
		if (rhits > rigg[n]){
			rhits -= rigg[n];
			rigg[n] = 0;
		}
		else {
			rigg[n] -= rhits;
			rhits = 0;
		}
	if (rigg[3] != -1 && rhits > rigg[3]){
		rhits -= rigg[3];
		rigg[3] = 0;
	} else if (rigg[3] != -1){
		rigg[3] -= rhits;
	}
	if (rig && !rigg[2] && (!rigg[3] || rigg[3] == -1))
		makesignal("dismasted!", 0, onship);
	if (portside(fromship, onship, 0)){
		guns = ptr -> gunL;
		car = ptr -> carL;
	} else {
		guns = ptr -> gunR;
		car = ptr -> carR;
	}
	if (ghits > car){
		ghits -= car;
		car = 0;
	}
	else {
		car -= ghits;
		ghits = 0;
	}
	if (ghits > guns){
		ghits -= guns;
		guns = 0;
	}
	else {
		guns -= ghits;
		ghits = 0;
	}
	hull -= ghits;
	if (portside(fromship, onship, 0) && Ghit){
		Write(SPECS + onship, 0, 20, guns);
		Write(SPECS + onship, 0, 24, car);
	}
	else if (Ghit){
		Write(SPECS + onship, 0, 22, guns);
		Write(SPECS + onship, 0, 26, car);
	}
	hull -= hhits;
	hull = hull < 0 ? 0 : hull;
	if (ptr1 -> captured > -1 && Chit){
		Write(FILES + onship, 0, 70, pc);
	}
	if (Hhit)
		Write(SPECS + onship, 0, 10, hull);
	if (Chit)
		for (n=0; n < 3; n++)
			Write(SPECS + onship, 0, 14 + 2*n, crew[n]);
	if (Rhit)
		for (n=0; n < 4; n++)
			Write(SPECS + onship, 0, 28 + 2*n, rigg[n]);
	switch(shot){
		case ROUND:
			strcpy(message, "firing round");
			break;
		case GRAPE:
			strcpy(message, "firing grape");
			break;
		case CHAIN:
			strcpy(message, "firing chain");
			break;
		case DOUBLE:
			strcpy(message, "firing double");
			break;
		case EXPLODE:
			strcpy(message, "exploding");
	}
	strcat(message, " shot on %s (%c%c)");
	makesignal(message, onship, fromship);
	if (roll == 6 && rig){
		switch(Rhit){
			case 0:
				strcpy(message, "fore topsail sheets parted");
				break;
			case 1:
				strcpy(message, "mizzen shrouds parted");
				break;
			case 2:
				strcpy(message, "main topsail yard shot away");
				break;
			case 4:
				strcpy(message, "fore topmast and foremast shrouds shot away");
				break;
			case 5:
				strcpy(message, "mizzen mast and yard shot through");
				break;
			case 6:
				strcpy(message, "foremast and spritsail yard shattered");
				break;
			case 7:
				strcpy(message, "main topmast and mizzen mast shattered");
				break;
		}
		makesignal(message, 0, onship);
	}
	else if (roll == 6) {
		switch(Hhit){
			case 0:
				strcpy(message, "anchor cables severed");
				break;
			case 1:
				strcpy(message, "two anchor stocks shot away");
				break;
			case 2:
				strcpy(message, "quarterdeck bulwarks damaged");
				break;
			case 3:
				strcpy(message, "three gun ports shot away");
				break;
			case 4:
				strcpy(message, "four guns dismounted");
				break;
			case 5:
				strcpy(message, "rudder cables shot through");
				Write(SPECS + onship, 0, 4, 0);
				break;
			case 6:
				strcpy(message, "shot holes below the water line");
				break;
		}
		makesignal(message, 0, onship);
	}
	if ((Chit > 1 && ptr1 -> readyL <= -30000 && ptr1 -> readyR <= -30000) || Chit == 4){
		ptr -> qual--;
		if (ptr -> qual <= 0){
			makesignal("crew mutinying!", 0, onship);
			ptr -> qual = 5;
			Write(FILES + onship, 0, 68, onship);
		} else 
			makesignal("crew demoralized", 0, onship);
		Write(SPECS + onship, 0, 12, ptr -> qual);
	}
	if (!hull)
		strike(onship, fromship);
}


cleanfoul(fromship, toship, offset)
int fromship, toship, offset;
{
	register int n = -1;

	Write(FILES + fromship, 0, 84 + offset*4, 0);
	do {
		n++;
	} while ((!scene[game].ship[toship].file -> fouls[n].turnfoul || scene[game].ship[toship].file -> fouls[n].toship != fromship) && n < 10);
	if (n < 10)
		Write(FILES + toship, 0, 84 + 4*n, 0);
	if (!grapple(fromship, toship) && !foul(fromship,toship)){
		if (!fouled(fromship) && !grappled(fromship)){
			unboard(fromship,fromship, 1);		/* defense */
			unboard(fromship,fromship, 0);		/* defense */
		} else
			unboard(fromship,toship, 0);		/* defense */
		if (!fouled(toship) && !grappled(toship)){	/* defense */
			unboard(toship,toship, 1);
			unboard(toship,toship, 0);
		} else
			unboard(toship, fromship, 0);			/* offense */
	}
}


cleangrapple(fromship, toship, offset)
int fromship, toship, offset;
{
	register int n = -1;

	Write(FILES + fromship, 0, 124 + offset*4, 0);
	do {
		n++;
	} while ((!scene[game].ship[toship].file -> grapples[n].turnfoul || scene[game].ship[toship].file -> grapples[n].toship != fromship) && n < 10);
	if (n < 10)
		Write(FILES + toship, 0, 124 + 4*n, 0);
	if (!grapple(fromship, toship) && !foul(fromship,toship)){
		if (!fouled(fromship) && !grappled(fromship)){
			unboard(fromship,fromship, 1);		/* defense */
			unboard(fromship,fromship, 0);		/* defense */
		} else
			unboard(fromship,toship, 0);		/* defense */
		if (!fouled(toship) && !grappled(toship)){	/* defense */
			unboard(toship,toship, 1);
			unboard(toship,toship, 0);
		} else
			unboard(toship, fromship, 0);			/* offense */
	}
}


strike(shipnum, fromship)
int shipnum, fromship;
{
	int points;

	if (!scene[game].ship[shipnum].file -> struck){
		Write(FILES + shipnum, 0, 66, 1);
		points = specs[scene[game].ship[shipnum].shipnum].pts + scene[game].ship[fromship].file -> points;
		Write(FILES + fromship, 0, 20, points);
		unboard(shipnum, shipnum, 0);		/* all offense */
		unboard(shipnum, shipnum, 1);		/* all defense */
		switch(die()){

			case 3:
			case 4:		/* ship may sink */
				Write(FILES + shipnum, 0, 234, 1);
				break;
			case 5:
			case 6:		/* ship may explode */
				Write(FILES + shipnum, 0, 232, 1);
				break;
		}
		Write(FILES + shipnum, 1, 164, "striking her colours!");
	}
}


