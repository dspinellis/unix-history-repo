#ifndef lint
static	char *sccsid = "@(#)driver1.c	1.3 83/05/20";
#endif

#include "externs.h"
#include <sys/types.h>

#define couldwin(from, to)	(specs[scene[game].ship[from].shipnum].crew2 > specs[scene[game].ship[to].shipnum].crew2 * 1.5)

unfoul()
{
	char name;
	register int n, k, ship;
	int ident, captured;
	struct File *ptr;

	for (n=0; n < scene[game].vessels; n++){
		ptr = scene[game].ship[n].file;
		if (!ptr -> captain[0]){
			for(k=0; k < 10; k++){
				if (ptr -> fouls[k].turnfoul){
					if ((ident = ptr -> captured) < 0)
						ident = n;
					ship = ptr -> fouls[k].toship;
					if ((captured = scene[game].ship[ship].file -> captured) < 0)
						captured = ship;
					if (scene[game].ship[ident].nationality == scene[game].ship[ship].nationality || toughmelee(n, ship, 0, 0))
						if (die() <= 2){
							cleanfoul(n, ship, k);
						}
				}
			}
		}
	}
}



boardcomp()
{
	register int n, k, l, men = 0, captured;
	int crew[3];
	struct shipspecs *ptr;
	struct File *ptr1;

	for (n = 0; n < scene[game].vessels; n++){
		ptr = &specs[scene[game].ship[n].shipnum];
		ptr1 = scene[game].ship[n].file;
		if (!ptr1 -> captain[0] && (fouled(n) || grappled(n)) && pos[n].dir && !ptr1 -> struck && ptr1 -> captured < 0){
			crew[0] = ptr -> crew1 != 0;
			crew[1] = ptr -> crew2 != 0;
			crew[2] = ptr -> crew3 != 0;
			for (l=0; l < scene[game].vessels; l++)
				if (foul(n+100,l) || grapple(n+100,l)){
					if (!meleeing(n,l)){
						if ((captured = scene[game].ship[l].file -> captured) < 0) 
							captured = l;
						if (pos[l].dir && scene[game].ship[n].nationality != scene[game].ship[captured].nationality){
							switch(specs[scene[game].ship[n].shipnum].class - specs[scene[game].ship[l].shipnum].class){
								case -3:
								case -4:
								case -5:
									if (crew[0]){
										send(n, l, crew[0]*100, 200);		/* OBP */
										crew[0] = 0;
									}
									else if (crew[1]){
										send(n, l, crew[1]*10, 200);		/* OBP */
										crew[1] = 0;
									}
									break;
								case -2:
									if (crew[0] || crew[1]){
										send(n, l, crew[0]*100+crew[1]*10, 200);	/* OBP */
										crew[0] = crew[1] = 0;
									}
									break;
								case -1:
								case 0:
								case 1:
									if (crew[0]){
										send(n, l, crew[0]*100+crew[1]*10, 200);	/* OBP */
										crew[0] = crew[1] = 0;
									}
									break;
								case 2:
								case 3:
								case 4:
								case 5:
									send(n, l, crew[0]*100+crew[1]*10+crew[2], 200);	/* OBP */
									crew[0] = crew[1] = crew[2] = 0;
									break;
							}
						}
					}
				}
			}
	}
}


fightitout(from, to, key)
int from, to, key;
{
	int crewfrom[3], crewto[3], menfrom, mento, fromcap, tocap;
	int pcto, pcfrom, fromstrength, strengthto, frominjured, toinjured;
	int index, totalfrom = 0, totalto = 0, topoints, frompoints, struck;
	int junk, count;
	struct File *ptr;
	struct shipspecs *ptr1;
	char message[60];

	ptr = scene[game].ship[from].file;
	ptr1 = &specs[scene[game].ship[from].shipnum];
	menfrom = mensent(from, to, crewfrom, &fromcap, &pcfrom, &frompoints, key);
	mento = mensent(to, from, crewto, &tocap, &pcto, &topoints, 0);
	if (fromcap < 0)
		fromcap = from;
	if (tocap < 0)
		tocap = to;
	fromstrength = menfrom * specs[scene[game].ship[fromcap].shipnum].qual;
	strengthto = mento * specs[scene[game].ship[tocap].shipnum].qual;
	if (key && !menfrom){
		if (fromcap == from)
			menfrom = ptr1 -> crew1 + ptr1 -> crew2 + ptr1 -> crew3;
		else
			menfrom = ptr -> pcrew;
		fromstrength = -1;
		strengthto *= 2;
	}
	for (count = 0; (!(fromstrength >= strengthto * 3 || strengthto >= fromstrength * 3) || fromstrength == -1) && count < 4;count++){
		index = fromstrength/10;
		if (index > 8)
			index = 8;
		toinjured = MT[index][2 - die() / 3];
		totalto += toinjured;
		index = strengthto/10;
		if (index > 8)
			index = 8;
		frominjured = MT[index][2 - die() / 3];
		totalfrom += frominjured;
		menfrom -= frominjured;
		mento -= toinjured;
		fromstrength = menfrom * specs[scene[game].ship[fromcap].shipnum].qual;
		strengthto = mento * specs[scene[game].ship[tocap].shipnum].qual;
	}
	if (fromstrength >= strengthto * 3 || count == 4){
		unboard(to, from, 0);
		subtract(from, totalfrom, crewfrom, fromcap, pcfrom);
		subtract(to, totalto, crewto, tocap, pcto);
		makesignal("boarders from %s repelled", to, from);
		sprintf(message, "killed in melee: %d.  %s: %d", totalto, scene[game].ship[from].shipname, totalfrom);
		Write(FILES + to, 1, 164, message);
		if (key)
			return(1);
	}
	else if (strengthto >= fromstrength * 3){
		unboard(from, to, 0);
		subtract(from, totalfrom, crewfrom, fromcap, pcfrom);
		subtract(to, totalto, crewto, tocap, pcto);
		if (key){
			if (fromcap != from)
/* Write(FILES + fromcap, 0, 20, scene[game].ship[fromcap].file -> points - (2 - ptr -> struck)*ptr1 -> points); */ /* original line... */

Write(FILES + fromcap, 0, 20,
(scene[game].ship[fromcap].file -> points) -
(2 - (ptr -> struck))*(ptr1 -> pts)
/* (2 - (ptr -> struck))*(ptr1 -> points) */
);

/* ptr1 points to the shipspec for the ship that was just unboarded.
   I guess that what is going on here is that the pointer is multiplied
   or something. */

			Write(FILES + from, 0, 68, to);
			topoints = 2*ptr1 -> pts + scene[game].ship[to].file -> points;
			if (ptr -> struck)
				topoints -= ptr1 -> pts;
			Write(FILES + to, 0, 20, topoints);
			mento = crewto[0] ? crewto[0] : crewto[1];
			if (mento){
				subtract(to, mento, crewto, tocap, pcto);
				subtract(from, -mento, crewfrom, to, 0);
			}
			sprintf(message, "captured by the %s!",scene[game].ship[to].shipname);
			Write(FILES + from, 1, 164, message);
			sprintf(message, "killed in melee: %d.  %s: %d", totalto, scene[game].ship[from].shipname, totalfrom);
			Write(FILES + to, 1, 164, message);
			mento = 0;
			return(0);
		}
	}
	return(0);
}		/* end of fightitout */


resolve()
{
	register int n, l, k, thwart;

	for (n=0; n < scene[game].vessels; n++){
		thwart = 2;
		if (pos[n].dir){
			for (l=n+1; l < scene[game].vessels; l++){
				if (pos[l].dir && meleeing(n,l) && meleeing(l,n)){	/* offense */
					fightitout(n,l,0);
				}
			}
			for (l=0; l < scene[game].vessels; l++){ /* defense */
				if (pos[l].dir && meleeing(l,n)){
					thwart = fightitout(n,l,1);
				}
				if (!thwart)
					break;
			}
			if (!thwart){
				for (k=0; k < scene[game].vessels; k++){
					if (pos[k].dir && meleeing(k,n)){
						unboard(k,n,0);
					}
					unboard(n,k,0);
				}
				unboard(n,n,1);
			}
			if (thwart == 2)
				unboard(n,n,1);
		}
	}
}


compcombat()
{
	int crew[3], men = 0, target, temp;
	int n, r, guns[2], load[2], car[2], roll[2];
	int ready[2], index, rakehim, sternrake;
	int shootat[2], hit[2], closest[2], ship;
	struct shipspecs *ptr;
	struct File *ptr1;

	for (ship = 0; ship < scene[game].vessels; ship++){
		ptr = &specs[scene[game].ship[ship].shipnum];
		ptr1 = scene[game].ship[ship].file;
		if (!ptr1 -> captain[0] && pos[ship].dir){
			crew[0] = ptr -> crew1;
			crew[1] = ptr -> crew2;
			crew[2] = ptr -> crew3;
			ready[0] = ptr1 -> readyL;
			ready[1] = ptr1 -> readyR;
			guns[0] = ptr -> gunL;
			guns[1] = ptr -> gunR;
			car[0] = ptr -> carL;
			car[1] = ptr -> carR;
			for (n = 0; n < 3; n++){
				if (ptr1 -> OBP[n].turnsent)
					men += ptr1 -> OBP[n].mensent;
			}
			for (n = 0; n < 3; n++){
				if (ptr1 -> DBP[n].turnsent)
					men += ptr1 -> DBP[n].mensent;
			}
			if (men){
				crew[0] = men/100 ? 0 : crew[0] != 0;
				crew[1] = (men%100)/10 ? 0 : crew[1] != 0;
				crew[2] = men%10 ? 0 : crew[2] != 0;
			}
			for (r = 0; r < 2; r++){
				if ((guns[r] || car[r]) && crew[2] && ready[r] <= 0 && !ptr1 -> struck && ((closest[r] = closestenemy(ship, (r ? 'r' : 'l'), 0)) != 30000) && range(closest[r], ship) <= range(ship, closestenemy(ship, (r ? 'r' : 'l'), 1))){
					if ((target = range(ship, closest[r])) <= 10 && !scene[game].ship[closest[r]].file -> struck && (guns[r] || (car[r] && target < 3))){
						load[r] = ROUND;
						if (target == 1 && loadwith[ship] == GRAPE)
							load[r] = GRAPE;
						if (target <= 3 && scene[game].ship[closest[r]].file -> FS)
							load[r] = CHAIN;
						if (target == 1 && load[r] != GRAPE)
							load[r] = DOUBLE;
						if (load[r] > CHAIN && target < 6){
							shootat[r] = HULL;
						}
						else {
							shootat[r] = RIGGING;
						}
						rakehim = gunsbear(ship, closest[r]) && !gunsbear(closest[r], ship);
						temp = portside(closest[r], ship, 1) - pos[closest[r]].dir + 1;
						if (temp < 1)
							temp += 8;
						if (temp > 8)
							temp -= 8;
						sternrake = temp > 4 && temp < 6;
						index = guns[r];
						if (target < 3){
							index += car[r];
						}
						index = (index - 1)/3;
						index = index > 8 ? 8 : index;
						if (!rakehim){
							hit[r] = HDT[index][target-1];
						}
						else {
							hit[r] = HDTrake[index][target-1];
						}
						if (rakehim && sternrake){
							hit[r]++;
						}
						hit[r] += QUAL[index][ptr1 -> captured < 0 ? ptr -> qual-1 : specs[scene[game].ship[ptr1 -> captured].shipnum].qual -1];
						for (n=0; n < 3 && ptr1 -> captured < 0; n++)
							if (!crew[n]){
								if (index <= 5)
									hit[r]--;
								else
									hit[r] -= 2;
							}
						if (ready[r] <= -30000){
							if (index <= 3)
								hit[r]++;
							else
								hit[r] += 2;
						}
						if (ptr1 -> captured > -1){
							if (index <= 1)
								hit[r]--;
							else
								hit[r] -= 2;
						}
						hit[r] += AMMO[index][load[r] - 1];
						if (((temp = ptr -> class) >= 5 || temp == 1) && windspeed == 5)
							hit[r]--;
						if (windspeed == 6 && temp == 4)
							hit[r] -= 2;
						if (windspeed == 6 && temp <= 3)
							hit[r]--;
						if (hit[r] >= 0){
							if (load[r] != GRAPE)
								hit[r] = hit[r] > 10 ? 10 : hit[r];
							roll[r] = die();
							table(shootat[r], load[r], hit[r], closest[r], ship, roll[r]);
						}
						load[r] = 0;
						if (!r)
							ptr1 -> readyL = 0;
						else
							ptr1 -> readyR = 0;
					}
				}
				else
					load[r] = 0;
			}
		}
	}
}

next()
{
	char string[25];
	int vec[3];

	turn++;
	if (turn % 55 == 0)
		if (scene[game].time)
			scene[game].time = 0;
		else
			scene[game].people = 0;		/* die if no one */
	if (scene[game].people <= 0 || windspeed == 7){
		fclose(syncfile);
		sprintf(string, "/tmp/.%d", game);
		if (unlink(string) == -1)
			perror(string);
		exit(0);
	}
	Write(SCENARIO, 0, 6, turn);
	if (turn % 7 == 0){
		if (die() >= scene[game].windchange || !windspeed){
			switch(die()){
				case 1:
					winddir = 1;
					break;
				case 2:
					break;
				case 3:
					winddir++;
					break;
				case 4:
					winddir--;
					break;
				case 5:
					winddir += 2;
					break;
				case 6:
					winddir -= 2;
					break;
			}
			if (winddir > 8)
				winddir -= 8;
			if (winddir < 1)
				winddir += 8;
			Write(SCENARIO, 0, 0, winddir);
			if (windspeed)
				switch(die()){
					case 1:
					case 2:
						windspeed--;
						break;
					case 5:
					case 6:
						windspeed++;
						break;
				}
			else
				windspeed++;
			Write(SCENARIO, 0, 2, windspeed);
		} 
/*		if (!MIGHTYCAPTAIN){
			gldav(vec);
			if ((vec[2] >> 8) > 9)
			{
				makesignal("*Load getting high, brace yourselves.", 0, 0);
			}
			if ((vec[2] >> 8) > 12)
			{
				makesignal("*Load average is blowing a gale!", 0, 0);
				Write(SCENARIO, 0, 2, 7);
			}
		} */
	}
}

main(argc, argv)
int argc;
char **argv;
{
	register int n, k;
	char file[25];
	int uid;

	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	srand(getpid());
	/* ;;; add code here to check the game number. */
	sprintf(file, "/tmp/.%s",argv[1]);
	for (n = 0; access(file, 0) < 0 && n < 20; n++)
		sleep(5);
	syncfile = fopen(file, "r+");
	if (syncfile == NULL) {
		perror(file);
		exit(1);
	}
	sscanf(argv[1], "%d", &game);
	for (n=0; n < scene[game].vessels; n++){
		nation[scene[game].ship[n].nationality + 1] = n + 1;
		if ((scene[game].ship[n].file = (struct File *) calloc(1, sizeof(struct File))) == NULL){
			printf("OUT OF MEMORY\n");
			exit(0);
		}
		scene[game].ship[n].file -> captured = -1;
	}
	for (n = 0; n < scene[game].vessels; n++){	/* initial loads */
		scene[game].ship[n].file -> loadL = ROUND;
		scene[game].ship[n].file -> loadR = ROUND;
		scene[game].ship[n].file -> readyR = -30000;
		scene[game].ship[n].file -> readyL = -30000;
	}
	if (!nation[2])
		nation[2] = nation[1];
	if (!nation[3])
		nation[3] = nation[2];
	sync();
	for(;;) {
		windspeed = scene[game].windspeed;
		winddir = scene[game].winddir;
		turn = scene[game].turn;
		next();
		unfoul();
		checkup();
		prizecheck();
		moveall();
		readpos();
		thinkofgrapples();
		boardcomp();
		compcombat();
		readpos();
		resolve();
		reload();
		checksails();
		sync();
		sleep(7);
		sync();
	}
}
