#ifndef lint
static	char *sccsid = "@(#)player1.c	1.10 83/07/03";
#endif

#include "player.h"
#include <sys/types.h>

WINDOW *view,*slot;
char done_curses = 0;

acceptcombat()
{
	int crew[3], men = 0, rangeofshot, target, temp;
	int n, r, guns[2], load[2], car[2], roll[2];
	int ready[2], index, rakehim, sternrake;
	int shootat[2], hit[2], closest[2], ship;
	int hhits = 0, ghits = 0, rhits = 0, chits = 0;
	int buf;
	struct shipspecs *ptr;
	struct File *ptr1;

	ptr = &specs[scene[game].ship[player].shipnum];
	ptr1 = scene[game].ship[player].file;
	crew[0] = ptr -> crew1;
	crew[1] = ptr -> crew2;
	crew[2] = ptr -> crew3;
	ready[0] = ptr1 -> readyL;
	ready[1] = ptr1 -> readyR;
	load[0] = ptr1 -> loadL;
	load[1] = ptr1 -> loadR;
	guns[0] = ptr -> gunL;
	guns[1] = ptr -> gunR;
	car[0] = ptr -> carL;
	car[1] = ptr -> carR;
	for (n = 0; n < 3; n++) {
		if (ptr1 -> OBP[n].turnsent)
			men += ptr1 -> OBP[n].mensent;
	}
	for (n = 0; n < 3; n++) {
		if (ptr1 -> DBP[n].turnsent)
			men += ptr1 -> DBP[n].mensent;
	}
	if (men) {
		crew[0] = men/100 ? 0 : crew[0] != 0;
		crew[1] = (men%100)/10 ? 0 : crew[1] != 0;
		crew[2] = men%10 ? 0 : crew[2] != 0;
	}
	for (r = 0; r < 2; r++) {
		if ((guns[r] || car[r]) && crew[2] && load[r] && ready[r] <= 0 && !ptr1 -> struck && ((closest[r] = closestenemy(player, (r ? 'r' : 'l'), 1)) != 30000)) {
			switch(load[r]) {
			case GRAPE:
				rangeofshot = 1;
				break;
			case CHAIN:
				rangeofshot = 3;
				break;
			case DOUBLE:
				rangeofshot = 1;
				break;
			case ROUND:
				rangeofshot = 10;
				break;
			}
			if ((target = range(player, closest[r])) <= rangeofshot && !scene[game].ship[closest[r]].file -> struck && (guns[r] || (car[r] && target < 3))) {
				Signal("%s (%c%c) within range of %s broadside.", closest[r], (r?"right":"left"));
				if (load[r] > CHAIN && target < 6) {
					Signal("Aim for hull or rigging? ", -1, 0);
					while ((buf = getch()) == EOF);
					addch(buf);
					if(buf == 'r')
						shootat[r] = RIGGING;
					else if (buf == 'h')
						shootat[r] = HULL;
					else {
						shootat[r] = -1;
						Signal("'Avast there! Hold your fire.'", -1, 0);
					}
				} else {
					shootat[r] = RIGGING;
					Signal("Fire? ", -1, 0);
					while ((buf = getch()) == EOF);
					addch(buf);
					if (buf == 'n') {
						shootat[r] = -1;
						Signal("Belay that! Hold your fire.", -1, 0);
					}
				}
				if (shootat[r] != -1) {
					fired = 1;
					rakehim = gunsbear(player, closest[r]) && !gunsbear(closest[r], player);
					temp = portside(closest[r], player, 1) - pos[closest[r]].dir + 1;
					if (temp < 1)
						temp += 8;
					if (temp > 8)
						temp -= 8;
					sternrake = temp > 4 && temp < 6;
					if (rakehim && !sternrake)
						Signal("Raking the %s!", closest[r], 0);
					else if (rakehim && sternrake)
						Signal("Stern Rake! %s splintering!", closest[r], 0);
					index = guns[r];
					if (target < 3)
						index += car[r];
					index = (index - 1)/3;
					index = index > 8 ? 8 : index;
					if (!rakehim)
						hit[r] = HDT[index][target-1];
					else
						hit[r] = HDTrake[index][target-1];
					if (rakehim && sternrake)
						hit[r]++;
					hit[r] += QUAL[index][ptr -> qual-1];
					for (n=0; n < 3 && ptr1 -> captured < 0; n++)
						if (!crew[n])
							if (index <= 5)
								hit[r]--;
							else
								hit[r] -= 2;
					if (ready[r] <= -30000)
						if (index <= 3)
							hit[r]++;
						else
							hit[r] += 2;
					if (ptr1 -> captured > -1)
						if (index <= 1)
							hit[r]--;
						else
							hit[r] -= 2;
					hit[r] += AMMO[index][load[r] - 1];
					if (((temp = ptr -> class) >= 5 || temp == 1) && windspeed == 5)
						hit[r]--;
					if (windspeed == 6 && temp == 4)
						hit[r] -= 2;
					if (windspeed == 6 && temp <= 3)
						hit[r]--;
					if (hit[r] >= 0) {
						roll[r] = die();
						if (load[r] == GRAPE)
							chits = hit[r];
						else {
							struct Tables *t;
							hit[r] = hit[r] > 10 ? 10 : hit[r];
							t = &(shootat[r] ? RigTable : HullTable)[hit[r]][roll[r]-1];
							chits = t->C;
							rhits = t->R;
							hhits = t->H;
							ghits = t->G;
							if (scene[game].ship[closest[r]].file -> FS)
								rhits *= 2;
							if (load[r] == CHAIN) {
								ghits = 0;
								hhits = 0;
							}
						}
						table(shootat[r], load[r], hit[r], closest[r], player, roll[r]);
					}
					scroll = 18;
					move(scroll++, 0);
					clearline();
					printw("Damage inflicted on the %s:", scene[game].ship[closest[r]].shipname);
					move(scroll++, 0);
					clearline();
					printw("\t%d HULL, %d GUNS, %d CREW, %d RIGGING", hhits, ghits, chits, rhits);
					load[r] = 0;
					if (!r) {
						ptr1 -> loadL = 0;
						ptr1 -> readyL = 0;
					} else {
						ptr1 -> loadR = 0;
						ptr1 -> readyR = 0;
					}
				}
			} else {
				load[r] = 0;
				Signal("Unable to fire %s broadside", -1, (r?"right":"left"));
			}
		} else
			Signal("Unable to fire %s broadside", -1, (r?"right":"left"));
	}
}

leave(conditions)
int conditions;
{
	FILE *fp;
	int people;
	float net;
	char * capn;
	char message[60];
	register int n;
	struct logs log[10], temp;

	signal(SIGHUP, SIG_IGN);
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	signal(SIGALRM, SIG_IGN);
	signal(SIGCHLD, SIG_IGN);

	if (conditions != -1) {
		capn = scene[game].ship[player].file -> captain;
		sprintf(message,"Captain %s relinquishing.",capn);
		Write(FILES + player, 1, 164, message);

		if (fp = fopen(LOGFILE, "r+")) {
			net = (float) (scene[game].ship[player].file -> points) / specs[scene[game].ship[player].shipnum].pts;
			people = getw(fp);
			n = fread(log, sizeof(struct logs), 10, fp);
			for (; n < 10; n++)
				log[n].fname[0] = log[n].uid = log[n].fshipnum = log[n].fgamenum = log[n].netpoints = 0;
			rewind(fp);
			if(people < 0)
				putw(1, fp);
			else
				putw(people+1, fp);
			for (n=0; n < 10; n++)
				if (net > (float) log[n].netpoints / specs[scene[log[n].fgamenum].ship[log[n].fshipnum].shipnum].pts) {
					fwrite(log, sizeof(struct logs), n, fp);
					strcpy(temp.fname, capn);
					temp.uid = getuid();
					temp.fshipnum = player;
					temp.fgamenum = game;
					temp.netpoints = scene[game].ship[player].file -> points;
					fwrite(&temp, sizeof(struct logs), 1, fp);
					fwrite(log + n, sizeof(struct logs), 9 - n, fp);
					break;
				}
			fclose(fp);
		}
		Write(FILES + player, 1, 0, " ");
		Write(SCENARIO, 0, 8, scene[game].people - 1);
		if (done_curses) {
			screen();
			Signal("It looks like you've had it!", -1, 0);
			if (conditions == 1)
				Signal("Your ship was captured.", -1, 0);
			else if (conditions == 2)
				Signal("No more enemies.", -1, 0);
			else if (conditions == 3)
				Signal("Hurricane!  All ships destroyed.",
					-1, 0);
			else if (conditions == 4)
				Signal("The driver died.", -1, 0);
			move(0, LINES-1);
			scroll = LINES;
			clearline();
			refresh();
		} else {
			if (conditions == 4)
				printf("The driver died.\n");
		}
		fclose(syncfile);
	}
	if (done_curses) {
		nocrmode();
		echo();
		endwin();
	}
	exit(0);
}

choke()
{
	leave(0);
}

#include <sys/wait.h>

child()
{
	union wait status;
	int pid;

	signal(SIGCHLD, SIG_IGN);
	do {
		pid = wait3(&status, WNOHANG|WUNTRACED, 0);
		if (pid < 0 || pid > 0 && !WIFSTOPPED(status))
			leave(4);
	} while (pid != 0);
	signal(SIGCHLD, child);
}

grapungrap()
{
	register int n, k, l, number, captured;
	int buf;

	for (n=0; n < scene[game].vessels ; n++) {
		if (n != player && (range(player, n) <= 1 || grapple(player, n))) {
			if ((captured = scene[game].ship[n].file -> captured) < 0)
				captured = n;
			Signal("Attempt to grapple or ungrapple %s (%c%c): ", n, 0);
			while ((buf = getch()) == EOF);
			addch(buf);
			if (buf == 'g') {
				number = die() < 3;
				if (!number && scene[game].ship[player].nationality == scene[game].ship[captured].nationality)
					number = 1;
				if (number) {
					for (l=0; l < 10 && scene[game].ship[player].file -> grapples[l].turnfoul; l++);
					if (!scene[game].ship[player].file -> grapples[l].turnfoul) {
						Write(FILES + player, 0, 124 + l*4, turn);
						Write(FILES + player, 0, 124 + l*4 + 2, n);
					}
					for (l=0; l < 10 && scene[game].ship[n].file -> grapples[l].turnfoul; l++);
					if (!scene[game].ship[n].file -> grapples[l].turnfoul) {
						Write(FILES + n, 0, 124 + l*4, turn);
						Write(FILES + n, 0, 124 + l*4 + 2, player);
					}
				}
				if (number) {
					Signal("Attempt succeeds!", 0, 0);
					makesignal("grappled with %s (%c%c)", n, player);
				} else
					Signal("Attempt fails.", 0, 0);
			}
			if (buf == 'u') {
				for (k=0; k < 10; k++) {
					if (scene[game].ship[player].file -> grapples[k].turnfoul && n == scene[game].ship[player].file -> grapples[k].toship) {
						if (die() < 3 || scene[game].ship[player].nationality == scene[game].ship[captured].nationality) {
							cleangrapple(player, n, k);
							Signal("Attempt succeeds!", 0, 0);
							makesignal("ungrappling with %s (%c%c)", n, player);
						} else
							Signal("Attempt fails.", 0, 0);
					}
				}
			}
		}
	}
}

unfoulplayer()
{
	register int n, toship;
	int buf;

	for (n=0; n < 10; n++) {
		if (scene[game].ship[player].file -> fouls[n].turnfoul) {
			Signal("Attempt to unfoul with the %s (%c%c)? ", (toship = scene[game].ship[player].file -> fouls[n].toship), 0);
			while ((buf = getch()) == EOF);
			addch(buf);
			if (buf == 'y' && die() < 3) {
				cleanfoul(player, toship, n);
				Signal("Attempt succeeds!", 0, 0);
				makesignal("unfouling %s (%c%c)", toship, player);
			} else if (buf == 'y')
				Signal("Attempt fails.", 0, 0);
		}
	}
}

initialize(nodriver, randomize, debug)
char randomize, nodriver, debug;
{
	char comm[80], file[25], capn;
	char message[60];
	int load = ROUND, ready = -30000;
	int people = 0;
	int pid;
	register int n;
	register int k;
	char *nameptr;

	srand(pid = getpid());

	if (game < 0) {
		puts("Choose a scenario:\n");
		puts("\n\tNUMBER\tSHIPS\tIN PLAY\tTITLE");
		for (n=0; n < NUMOFSCENES; n++) {
			sprintf(file, "/tmp/.%d", n);
			printf("\t%d):\t%d", n, scene[n].vessels);
			if (access(file, 0) >= 0)
				printf("\tYES");
			else
				printf("\tno");
			printf("\t%s\n", scene[n].name);
		}
reprint:
		printf("\nScenario number? ");
		fflush(stdout);
		scanf("%d", &game);
		while (getchar() != '\n');
	}
	if (game < 0 || game >= NUMOFSCENES) {
		puts("Very funny.");
		exit(1);
	}
	sprintf(file, "/tmp/.%d", game);
	if (access(file, 0) < 0) {
		int omask;
#ifdef SETUID
		omask = umask(077);
#else
		omask = umask(011);
#endif
		syncfile = fopen(file, "w+");
		umask(omask);
	} else {
		syncfile = fopen(file, "r+");
		people = 1;
	}
	lastsync = 0;
	for (n=0; n < scene[game].vessels; n++) {
		nation[scene[game].ship[n].nationality + 1] = n + 1;
		if ((scene[game].ship[n].file =  (struct File *) calloc(1, sizeof(struct File))) == NULL) {
			puts("OUT OF MEMORY");
			exit(0);
		}
		scene[game].ship[n].file -> captured = -1;
	}
	if (!nation[2])
		nation[2] = nation[1];
	if (!nation[3])
		nation[3] = nation[2];
	if (people > 0) {
		puts("Synchronizing with the other players...");
		fflush(stdout);
		sync();
		capn = 1;
		for (n=0; n < scene[game].vessels && capn; n++) {
			capn = scene[game].ship[n].file -> captain[0];
			if (scene[game].ship[n].file -> struck || scene[game].ship[n].file -> captured > -1)
				capn = 1;
		}
		if (!capn)
			player = n-1;
		else {
			puts("All ships taken in that scenario.");
			for (n=0; n < scene[game].vessels; n++)
				free(scene[game].ship[n].file);
			people = 0;
			for (n=0; n < 5; n++)
				nation[n] = 0;
			fclose(syncfile);
			goto reprint;
		}
	} else
		player = 0;
	while (randomize) {
		static char *color[] =
			{ "(American)", "(British)", "(Spanish)", "(French)" };
		puts(scene[game].name);
		putchar('\n');
		for (n=0; n < scene[game].vessels; n++) {
			printf("  %2d:  %-10s %-15s  (%-2d pts)   ",
				n,
				color[scene[game].ship[n].nationality],
				scene[game].ship[n].shipname,
				specs[scene[game].ship[n].shipnum].pts);
			if(scene[game].ship[n].file -> captain[0])
				puts(scene[game].ship[n].file -> captain);
			else if (scene[game].ship[n].file -> struck)
				puts("(struck)");
			else if(scene[game].ship[n].file -> captured > -1)
				puts("(captured)");
			else
				puts("(available)");
		}
		putchar('\n');
		printf("Which ship do you want (0-%d)? ",scene[game].vessels-1);
		fflush(stdout);
		if (scanf("%d",&player) != 1 || player < 0 || player >= scene[game].vessels) {
			while (getchar() != '\n');
			puts("Say what?");
		} else {
			while (getchar() != '\n');
			sync();
			if (scene[game].ship[player].file -> captain[0] || scene[game].ship[player].file -> struck || scene[game].ship[player].file -> captured > -1)
				puts("Sorry, that ship is taken.");
			else
				break;
		}
	}

	signal(SIGHUP, choke);
	signal(SIGINT, choke);
	signal(SIGQUIT, choke);
	signal(SIGCHLD, child);

	Write(FILES + player, 1, 0, "begin");  /* he now exists */
	if (people)
		Write(SCENARIO, 0, 8, scene[game].people + 1);
	sync();
	printf("Your ship is the %s, a %s (%s crew).\n", scene[game].ship[player].shipname, info(player, message), quality(player));
	if ((nameptr = (char *) getenv("NAME")) && *nameptr)
		strcpy(comm,nameptr);
	else {
		fputs("Your name, Captain? ", stdout);
		fflush(stdout);
		gets(comm);
		if (!*comm) strcpy(comm, "no name");
	}
	comm[19] = '\0';
	for (k=0; k < 2; k++) {
		printf("\nInitial broadside %s (grape, chain, round, double): ", (k ? "right" : "left"));
		fflush(stdout);
		scanf("%s", file);
		switch(*file) {
		case 'g':
			load = GRAPE;
			break;
		case 'c':
			load = CHAIN;
			break;
		case 'r':
			load = ROUND;
			break;
		case 'd':
			load = DOUBLE;
			break;
		}
		if (k) {
			scene[game].ship[player].file -> loadR = load;
			scene[game].ship[player].file -> readyR = ready;
		} else {
			scene[game].ship[player].file -> loadL = load;
			scene[game].ship[player].file -> readyL = ready;
		}
	}
	Write(FILES + player, 1, 0, comm);
	if (!people && !nodriver) {
		char num[10];
		sprintf(num, "%d", game);
		if (!fork()) {
			if (debug)
				execl(DEBUGDRIVER, DRIVERNAME, num, 0);
			else
				execl(DRIVER, DRIVERNAME, num, 0);
			perror(DRIVER);
			kill(pid, SIGKILL);
			exit(1);
		}
	}

	initscr();
	view = newwin(ROWSINVIEW, COLSINVIEW, 2, 1);
	slot = newwin(ROWSINVIEW, 3, 2, 1+COLSINVIEW);
	done_curses++;
	noecho();
	crmode();

	board();
	sprintf(message, "Captain %s assuming command",comm);
	Write(FILES + player, 1, 164, message);
	windspeed = scene[game].windspeed;
	winddir = scene[game].winddir;
	turn = scene[game].turn;

	iplotships();	/* new turn */
}

/*
 * Returns the NUMBER of the closest ship
 */
closestenemy(fromship, side, anyship)
int fromship;
char side, anyship;
{
	register int n, olddist = 30000, ident, captured;
	register int dist, eclosest = 30000;

	if ((ident = scene[game].ship[fromship].file -> captured) < 0)
		ident = fromship;
	for (n=0; n < scene[game].vessels; n++) {
		if ((captured = scene[game].ship[n].file -> captured) < 0)
			captured = n;
		if (n != fromship && pos[n].dir && (scene[game].ship[ident].nationality != scene[game].ship[captured].nationality || anyship)) {
			if (!side || gunsbear(fromship, n) == side) {
				dist = range(fromship, n);
				if (dist < olddist) {
					eclosest = n;
					olddist = dist;
				}
			}
		}
	}
	if (!side && olddist == 30000)
		leave(2);
	return(eclosest);
}

main(argc, argv)
int argc;
char ** argv;
{
	register int crew, aheadfirst, ma, n;
	int ta;
	char message[60], ch;
	int uid;
	char nodrive = 0, randomize = 0, debug = 0;
	char *badstring();
	extern char _sobuf[];

	setbuf(stdout, _sobuf);

	while (*++argv && **argv == '-')
		switch (*++*argv) {
		case 'd':
			nodrive = 1;
			break;
		case 'D':
			debug++;
			break;
		case 'x':
			randomize = 1;
			break;
		default:
			printf("Unknown flag '%s'\n",*argv);
			break;
		}
	if (*argv)
		game = atoi(*argv);
	else
		game = -1;
	initialize(nodrive, randomize, debug);
	Signal("Aye aye, Sir", 0, 0);
	for(;;) {
		move(scroll++,0);
		clearline();
		addch('~');
		move(--scroll,0);
		refresh();
		ch = getch();
		switch(ch) {
		case 'm':
			crew = specs[scene[game].ship[player].shipnum].crew3;
			if (crew && !grappled(player) && !fouled(player) && windspeed) {
				ta = maxturns(player);
				aheadfirst = ta & 0100000;
				ma = maxmove(player, pos[player].dir, 0);
				ta &= 077777;
				acceptmove(ma, ta, aheadfirst);
			} else
				Signal("Unable to move", 0, 0);
			break;
		case 's':
			acceptsignal();
			break;
		case 'g':
			grapungrap();
			break;
		case 'u':
			unfoulplayer();
			break;
		case 'v':
			version();
			break;
		case 'b':
			boarding();
			break;
		case 'f':
			acceptcombat();
			break;
		case 'l':
			loadplayer();
			break;
		case 'c':
			changesail();
			break;
		case 'r':
			repair();
			break;
		case 'B':
			Signal("'Hands to stations!'", 0, 0);
			unboard(player, player, 1);	/* cancel DBP's */
			unboard(player, player, 0);	/* cancel offense */
			break;
		case '\f':
			signal(SIGALRM, SIG_IGN);
			viewrow = pos[player].row - ROWSINVIEW / 2;
			viewcol = pos[player].col - COLSINVIEW / 2;
			board();
			plotships(); /* don't create new turn */
			break;
		case 'L':
			scene[game].ship[player].file -> loadL = 0;
			scene[game].ship[player].file -> loadR = 0;
			scene[game].ship[player].file -> readyL = 0;
			scene[game].ship[player].file -> readyR = 0;
			Signal("Broadsides unloaded", 0, 0);
			break;
		case 'q':
			Signal("Type 'Q' to quit", 0, 0);
			break;
		case 'Q':
			leave(0);
			break;
		case 'I':
			for (n = 0; n < scene[game].vessels; n++)
				eyeball(player, n);
			break;
		case 'i':
			n = closestenemy(player, 0, 1);
			eyeball(player, n);
			break;
		}
		signalflags();
		lost();
	}
}

char *saywhat(n)
int n;
{
	if(scene[game].ship[n].file -> captain[0])
		return(scene[game].ship[n].file -> captain);
	else if (scene[game].ship[n].file -> struck)
		return("(struck)");
	else if(scene[game].ship[n].file -> captured > -1)
		return("(captured)");
	else
		return("(computer)");
}

eyeball(player, n)
int player, n;
{
	char message[80];
	register i;

	if (scene[game].ship[n].shipdir) {
		sprintf(message, "%d, %s", range(player, n), saywhat(n));
		Signal("Sail ho! (range %s)", -1, message);
		switch(scene[game].ship[n].nationality) {
		case 0:
			strcpy(message, "American ");
			break;
		case 1:
			strcpy(message, "British ");
			break;
		case 2:
			strcpy(message, "Spanish ");
			break;
		case 3:
			strcpy(message, "French ");
			break;
		default:
			strcpy(message, "Gremlin ");
			break;
		}
		switch(specs[scene[game].ship[n].shipnum].class) {
		case 1:
		case 2:
			strcat(message, "Ship of the Line ");
			break;
		case 3:
		case 4:
			strcat(message, "Frigate ");
			break;
		case 5:
		case 6:
			strcat(message, "Sloop ");
			break;
		}
		switch(portside(player, n, 1) - pos[player].dir) {
		case -7:
		case 1:
			strcat(message, "off the starboard bow.");
			break;
		case -6:
		case 2:
			strcat(message, "off the starboard beam.");
			break;
		case -5:
		case 3:
			strcat(message, "off the starboard quarter.");
			break;
		case -4:
		case 4:
			strcat(message, "dead astern.");
			break;
		case -3:
		case 5:
			strcat(message, "off the port quarter.");
			break;
		case -2:
		case 6:
			strcat(message, "off the port beam.");
			break;
		case -1:
		case 7:
			strcat(message, "off the port bow.");
			break;
		case 0:
			strcat(message, "dead ahead.");
			break;
		}
		Signal(message, 0, 0);
	}
}
