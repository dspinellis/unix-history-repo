#ifndef lint
static	char *sccsid = "@(#)driver3.c	1.3 83/05/20";
#endif
#include "externs.h"

extern int dtab[];

moveall()		/* move all comp ships */
{
	register int n, k, l, m, ma, closest;
	int weakest[5];
	int ta, af, jj;
	char command[10], clast[20][10];
	int row[20], col[20], dir[20], r1, r2, c1, c2, d1, d2;
	struct File *ptr;

	for (n=0; n < scene[game].vessels; n++){
		ptr = scene[game].ship[n].file;
		if (!ptr -> captain[0] && scene[game].ship[n].shipdir){
				
			if (!ptr -> struck && windspeed && !grappled(n) && !fouled(n) && specs[scene[game].ship[n].shipnum].crew3){
				ta = maxturns(n);
				jj = 0100000;
				af = ta & jj;
				jj = 077777;
				ta = ta & jj;
				ma = maxmove(n, pos[n].dir, 0);
				closest = closestenemy(n, 0, 0);
				if (closest == 30000) 
					command[0] = '\0';
				else
					closeon(n, closest, command, ta, ma, af);
			}
			else
				command[0] = '\0';
			strcpy(ptr -> last, command);
		}
	}
	for (n=0; n < scene[game].vessels; n++){
		strcpy(clast[n], scene[game].ship[n].file -> last);
		if (fouled(n) || grappled(n))
			clast[n][0] = '\0';
		row[n] = pos[n].row;
		col[n] = pos[n].col;
		dir[n] = pos[n].dir;
		score(clast[n],n, 0, 1, 0);
	}
	for (k=0; stillmoving(clast, k); k++){
		for (n=0; n < scene[game].vessels; n++){
			if (dir[n])
				step(clast[n][k], n, row, col, dir);
			if (!clast[n][k])
				clast[n][k+1] = '\0';
		}
		for (n=0; n < scene[game].vessels; n++){
			if ((d1 = pos[n].dir) && !isolated(n)){
				r1 = pos[n].row;
				c1 = pos[n].col;
				pos[n].dir = dir[n];
				pos[n].row = row[n];
				pos[n].col = col[n];
				for (l=0; l < scene[game].vessels; l++){
					if (d2 = pos[l].dir && l != n){
						r2 = pos[l].row;
						c2 = pos[l].col;
						pos[l].dir = dir[l];
						pos[l].row = row[l];
						pos[l].col = col[l];
						if ((foul(n,l) || grapple(n,l)) && push(n,l) && range(n,l) > 1){
							Write(l,0,6,pos[n].row-1);
							if (pos[n].dir == 1 || pos[n].dir == 5)
								Write(l,0,8,pos[n].col-1);
							else
								Write(l,0,8,pos[n].col);
							Write(l,0,10,pos[n].dir);
						}
						if (!range(n,l) && !foul(n,l) && push(n,l)){
							makesignal("collision with %s (%c%c)", l, n);
							if (die() < 4){
								makesignal("fouled with %s (%c%c)", l, n);
								for (m=0;scene[game].ship[n].file -> fouls[m].turnfoul && m < 10; m++);
								if (m < 10){
									Write(FILES + n, 0, 84 + m*4, turn);
									Write(FILES + n, 0, 84 + m*4 + 2, l);
								}
								for (m=0;scene[game].ship[l].file -> fouls[m].turnfoul && m < 10; m++);
								if (m < 10){
									Write(FILES + l, 0, 84 + m*4, turn);
									Write(FILES + l, 0, 84 + m*4 + 2, n);
								}
							}
							clast[n][k+1] = '\0';
							pos[n].row = r2;
							pos[n].col = c2;
							pos[n].dir = d2;
							score(clast[n],n,n,1,0);
							Write(l,0,6,pos[n].row-1);
							if (pos[n].dir == 1 || pos[n].dir == 5)
								Write(l,0,8,pos[n].col-1);
							else
								Write(l,0,8,pos[n].col);
							Write(l,0,10,pos[n].dir);
							Write(FILES + l, 0, 82, 0);
							Write(FILES + n, 0, 82, 0);
						}
						else {
							pos[l].row = r2;
							pos[l].col = c2;
							pos[l].dir = d2;
						}
					}
				}
				pos[n].row = r1;
				pos[n].col = c1;
				pos[n].dir = d1;
			}
		}
	}
	for(n=0; n < scene[game].vessels; n++)
		scene[game].ship[n].file -> last[0] = NULL;
}

stillmoving(last, k)
register char last[20][10];
register int k;
{
	register int n;

	for (n=0; n < scene[game].vessels; n++)
		if (last[n][k]) return(1);
	return(0);
}

isolated(ship)
register int ship;
{
	register int n;

	for (n=0; n < scene[game].vessels; n++)
		if (ship != n && range(ship, n) <= 10)
			return(0);
		return(1);
}

push(from, to)
register int from, to;
{
	int bow1r, bow1c, bow2r, bow2c, stern1r, stern1c, stern2r, stern2c;
	register int bs, sb;

	stern1r = bow1r = pos[from].row;
	stern1c = bow1c = pos[from].col;
	stern2r = bow2r = pos[to].row;
	stern2c = bow2c = pos[to].col;
	drdc(&stern2r, &stern2c, pos[to].dir);
	bs = bow1r - stern2r + bow1c - stern2c;
	sb = stern1r - bow2r + stern1c - bow2c;
	if (!bs)
		return(1);
	drdc(&stern1r, &stern1c, pos[from].dir);
	if(!sb)
		return(0);
	if ((sb = specs[scene[game].ship[to].shipnum].class) > (bs = specs[scene[game].ship[from].shipnum].class))
		return(1);
	if (sb < bs)
		return(0);
	return(from < to);
}

step(com, shipnum, row, col, dir)
int shipnum, row[20], col[20], dir[20];
char com;
{
	int dr = 0, dc = 0;
	register int dist;

	switch(com){

		case 'r':
			if (++dir[shipnum] == 9)
				dir[shipnum] = 1;
			break;
		case 'l':
			if (!--dir[shipnum])
				dir[shipnum] = 8;
			break;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
			drdc(&dr, &dc, dir[shipnum]);
			if (!(dir[shipnum] % 2))
				dist = dtab[com - '0'];
			else
				dist = com - '0';
			row[shipnum] -= dr * dist;
			col[shipnum] -= dc * dist;
			dr = dc = 0;
			break;
		case 'b':
			break;
		case 'd':
			drdc(&dr, &dc, winddir);
			dist = 1 - ((specs[scene[game].ship[shipnum].shipnum].class < 3 || fouled(shipnum) || grappled(shipnum)) && turn % 2);
			row[shipnum] -= dr * dist;
			col[shipnum] -= dc * dist;
			break;
	}
}

send(from, to, sections, offset)
int from, to, sections, offset;
{
	int n;
	struct BP *ptr;

	ptr = offset == 200 ? scene[game].ship[from].file -> OBP : scene[game].ship[from].file -> DBP;
	for (n=0; n<3 && ptr[n].turnsent; n++);
	if (n < 3 && sections){
		Write(FILES + from, 0, 30 + (offset > 200)*18 + 6*n, turn);
		Write(FILES + from, 0, 30 + (offset > 200)*18 + 6*n + 2, to);
		Write(FILES + from, 0, 30 + (offset > 200)*18 + 6*n + 4, sections);
		if (offset == 200)
			makesignal("boarding the %s (%c%c)", to, from);
		else
			makesignal("repelling boarders", 0, from);
	}
}

toughmelee(shipnum, toship, defense, count)
int shipnum, toship, defense, count;
{
	int n, OBP = 0, DBP = 0, obp = 0, dbp = 0;
	int qual;
	struct BP *ptr1;
	struct shipspecs *ptr;

	ptr = &specs[scene[game].ship[shipnum].shipnum];
	ptr1 = defense ? scene[game].ship[shipnum].file -> DBP : scene[game].ship[shipnum].file -> OBP ;
	qual = ptr -> qual;
	for (n=0; n < 3; n++){
		if (ptr1[n].turnsent && (toship == ptr1[n].toship || defense)){
			obp += ptr1[n].mensent / 100 ? ptr -> crew1 * qual : 0;
			obp += (ptr1[n].mensent % 100)/10 ? ptr -> crew2 * qual : 0;
			obp += ptr1[n].mensent % 10 ? ptr -> crew3 * qual : 0;
		}
	}
	if (count || defense)
		return(obp);
	OBP = toughmelee(toship, shipnum, 0, count + 1);
	dbp = toughmelee(shipnum, toship, 1, count + 1);
	DBP = toughmelee(toship, shipnum, 1, count + 1);
	if (OBP > obp + 10 || OBP + DBP >= obp + dbp + 10)
		return(1);
	else
		return(0);
}

reload()
{
	register int n;

	for (n=0; n < scene[game].vessels; n++)
		loadwith[n] = 0;
}

checksails()
{
	register int n, rig, full; 
	struct shipspecs *ptr;
	int close;

	for (n=0; n < scene[game].vessels; n++){
		ptr = &specs[scene[game].ship[n].shipnum];
		rig = ptr -> rig1;
		if (windspeed == 6 || (windspeed == 5 && ptr -> class > 4))
			rig = 0;
		if (!scene[game].ship[n].file -> captain[0]){
			if (rig && ptr -> crew3) {
				close = closestenemy(n,0,0);
				if (close != 30000) {
					if (range(n, close) > 9)
						full = 1;
					else
						full = 0;
				} else 
					full = 0;
			} else
				full = 0;
			if ((scene[game].ship[n].file -> FS != 0) != full)
				Write(FILES + n, 0, 230, full);
		}
	}
}
