#ifndef lint
static	char *sccsid = "@(#)driver2.c	1.1 83/03/17";
#endif
#define EXPLODE 5
#include "externs.h"

#define couldwin(from, to)	(specs[scene[game].ship[from].shipnum].crew2 > specs[scene[game].ship[to].shipnum].crew2 * 1.5)

thinkofgrapples()
{
	register int n, l, friendly;

	for (n=0; n < scene[game].vessels; n++){
		if (!scene[game].ship[n].file -> captain[0] && pos[n].dir){
			for (l=0; l < scene[game].vessels; l++){
				if ((friendly = scene[game].ship[l].file -> captured) < 0)
					friendly = scene[game].ship[n].nationality == scene[game].ship[l].nationality;
				else
					friendly = scene[game].ship[n].nationality == scene[game].ship[friendly].nationality;
				if (!friendly){
					if (range(n,l) == 1 && !scene[game].ship[n].file -> struck && scene[game].ship[n].file -> captured < 0){
						if (grapple(n,l))
							if (toughmelee(n,l,0,0))
								ungrap(n, l);
							else
								grap(n, l);
						else if (couldwin(n,l)){
							grap(n, l);
							loadwith[n] = GRAPE;
						}
					}
				}
				else
					ungrap(n,l);
			}
		}
	}
}


checkup()
{
	register int k, n, explode, sink;

	readpos();
	for (n=0; n < scene[game].vessels; n++){
		explode = scene[game].ship[n].file -> explode;
		sink = scene[game].ship[n].file -> sink;
		if (die() >= 5 && (explode == 1 || sink == 1)){
			explode = 2;
			if (sink != 1)
				Write(FILES + n, 0, 232, explode);
			else
				Write(FILES + n, 0, 234, explode);
			pos[n].dir = 0;	/* hopefully enough to kill ship */
			Write(n, 0, 10, 0);
			if (fouled(n) || grappled(n)){
				for (k=0; k < 10; k++){
					if (scene[game].ship[n].file -> fouls[k].turnfoul)
						cleanfoul(n,scene[game].ship[n].file -> fouls[k].toship,k);
				}
				for (k=0; k < 10; k++){
					if (scene[game].ship[n].file -> grapples[k].turnfoul)
						cleangrapple(n,scene[game].ship[n].file -> grapples[k].toship,k);
				}
			}
			if (sink != 1){
				makesignal("exploding!", 0, n);
				for (k=0; k < scene[game].vessels; k++)
					if (k != n && pos[k].dir && range(n,k) < 4)
						table(RIGGING, EXPLODE, specs[scene[game].ship[n].shipnum].guns/13, k, n, 6);
			}
			else {
				makesignal("sinking!", 0, n);
			}
		}
	}
}

prizecheck()
{
	register int n, captured, pc, prisoners, points;
	struct shipspecs *ptr;
	struct File *ptr1;

	for (n=0; n < scene[game].vessels; n++){
		ptr1 = scene[game].ship[n].file;
		if ((captured = ptr1 -> captured) > -1 && !scene[game].ship[n].file -> struck && pos[n].dir){
			ptr = &specs[scene[game].ship[n].shipnum];
			points = scene[game].ship[captured].file -> points;
			prisoners = ptr -> crew1 + ptr -> crew2 + ptr -> crew3;
			if (prisoners > ptr1 -> pcrew * 6){
				Write(FILES + n, 0, 68,-1);
				points -= 2 * ptr -> pts;
				Write(FILES + n, 1, 164, "prize crew overthrown");
				Write(FILES + captured, 0, 20, points);
			}
		}
	}
}

char strend(str)
char *str;
{
	register int n;

	for (n=0; *(str+n); n++);
	return( n ? *(str + n -1) : 0);
}


closeon(fromship, toship, command, ta, ma, af)
int fromship, toship, ma, ta, af;
char command[10];
{
	int high;
	char temp[10];

	temp[0] = command[0] = '\0';
	high = -30000;
	try(command, temp, ma, ta, af, ma, pos[fromship].dir, fromship, toship, &high, 0);
}

int dtab[8] = {0,1,1,2,3,4,4,5};	/* diagonal distances in x==y */

score(movement,shipnum, toship, permanent, onlytemp)
int shipnum, toship, permanent, onlytemp;
char movement[10];
{
	int drift, row, col, dir, n, dist, total, ran;
	int dr = 0, dc = 0;

	if (dir = pos[shipnum].dir){
		row = pos[shipnum].row;
		col = pos[shipnum].col;
		for (n=0; movement[n]; n++)
			switch(movement[n]){
				case 'r':
					if ((++pos[shipnum].dir) == 9)
						pos[shipnum].dir = 1;
					break;
				case 'l':
					if (!(--pos[shipnum].dir))
						pos[shipnum].dir = 8;
					break;
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
					drdc(&dr, &dc, pos[shipnum].dir);
					if (!(pos[shipnum].dir % 2))
						dist = dtab[movement[n] - '0'];
					else
						dist = movement[n] - '0';
					pos[shipnum].row -= dr * dist;
					pos[shipnum].col -= dc * dist;
					dr = dc = 0;
					break;
			}
		drift = scene[game].ship[shipnum].file -> drift;
		if (!windspeed) drift = 1;
		if (row == pos[shipnum].row && col == pos[shipnum].col){
			drift++;
			if (drift > 2){
				drdc(&dr, &dc, winddir);
				dist = 1 - ((specs[scene[game].ship[shipnum].shipnum].class < 3 || fouled(shipnum) || grappled(shipnum)) && turn % 2);
				pos[shipnum].row -= dr * dist;
				pos[shipnum].col -= dc * dist;
				if (permanent)
					strcat(movement, "d");
			}
		}
		else
			drift = 0;
		ran = range(shipnum, toship);
		total = -50*ran;
		if (ran < 4 && gunsbear(shipnum, toship))
			total += 60;
		if ((ran = portside(shipnum, toship, 1) - pos[shipnum].dir) == 4 || ran == -4)
			total = -30000;
		if (!*movement) strcpy(movement, "d");
		if (permanent){
			if (drift != scene[game].ship[shipnum].file -> drift)
				Write(FILES + shipnum, 0, 82, drift);
			if (pos[shipnum].row != scene[game].ship[shipnum].shiprow)
				Write(shipnum, 0, 6, pos[shipnum].row);
			if (pos[shipnum].col != scene[game].ship[shipnum].shipcol)
				Write(shipnum, 0, 8, pos[shipnum].col);
			if (pos[shipnum].dir != scene[game].ship[shipnum].shipdir)
				Write(shipnum, 0, 10, pos[shipnum].dir);
		}
		if (!onlytemp){
			pos[shipnum].row = row;
			pos[shipnum].col = col;
			pos[shipnum].dir = dir;
		}
		return(total);
	}
	return(0);
}

try(command, temp,ma, ta, af, vma, dir, shipnum, toship, high, rakeme)
int ma, ta, af, shipnum, toship, *high, rakeme;
char command[10], temp[10];
{
	register int new, n;
	char st[4];

	if ((n = strend(temp)) < '1' || n > '9')
		for(n = 1; vma - n >= 0; n++){
			sprintf(st, "%d", n);
			strcat(temp, st);
			new = score(temp, shipnum, toship, 0, rakeme);
			if (new > *high && (!rakeme || (gunsbear(shipnum, toship) && !gunsbear(toship, shipnum)))){
				*high = new;
				strcpy(command, temp);
			}
			try(command, temp, ma-n, ta, af, vma-n, dir, shipnum, toship, high, rakeme);
			rmend(temp);
		}
	if ((ma > 0 && ta > 0 && (n = strend(temp)) != 'l' && n != 'r') || !lengthof(temp)){
		strcat(temp, "r");
		new = score(temp, shipnum, toship, 0, rakeme);
		if (new > *high && (!rakeme || (gunsbear(shipnum, toship) && !gunsbear(toship, shipnum)))){
			*high = new;
			strcpy(command, temp);
		}
		try(command, temp, ma-1, ta-1, af, (min(ma-1,maxmove(shipnum, (dir == 8 ? 1 : dir+1), 0))), (dir == 8 ? 1 : dir+1),shipnum,toship,high,rakeme);
		rmend(temp);
	}
	if ((ma > 0 && ta > 0 && (n = strend(temp)) != 'l' && n != 'r') || !lengthof(temp)){
		strcat(temp, "l");
		new = score(temp, shipnum, toship, 0, rakeme);
		if (new > *high && (!rakeme || (gunsbear(shipnum, toship) && !gunsbear(toship, shipnum)))){
			*high = new;
			strcpy(command, temp);
		}
		try(command, temp, ma-1, ta-1, af, (min(ma-1,maxmove(shipnum, (dir-1 ? dir-1 : 8), 0))), (dir-1 ? dir -1 : 8), shipnum, toship, high, rakeme);
		rmend(temp);
	}
}


closestenemy(fromship, side, anyship) /*returns the NUMBER of the closest ship*/
int fromship; 
char side, anyship;
{
	register int n, olddist = 30000, ident, captured;
	register int dist, eclosest = 30000;

	if ((ident = scene[game].ship[fromship].file -> captured) < 0)
		ident = fromship;
	for (n=0; n < scene[game].vessels; n++){
		if ((captured = scene[game].ship[n].file -> captured) < 0)
			captured = n;
		if (n != fromship && pos[n].dir && (scene[game].ship[ident].nationality != scene[game].ship[captured].nationality || anyship)){
			if (!side || gunsbear(fromship, n) == side){
				dist = range(fromship, n);
				if (dist < olddist){
					eclosest = n;
					olddist = dist;
				}
			}
		}
	}
	return(eclosest);
}

choke()
{
}

