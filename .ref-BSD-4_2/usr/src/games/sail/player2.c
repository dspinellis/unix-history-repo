#ifndef lint
static	char *sccsid = "@(#)player2.c	1.2 83/05/20";
#endif

#include "player.h"

#define turnfirst(buf)  (*buf == 'r' || *buf == 'l')

lost()
{
	if (scene[game].ship[player].file -> struck)
		leave(0);
	if (scene[game].ship[player].file -> captured > -1)
		leave(1);
	if (windspeed == 7)
		leave(3);
	if(scene[game].ship[player].file -> FS && (!specs[scene[game].ship[player].shipnum].rig1 || windspeed == 6))
		Write(FILES + player, 0, 230, 0);
}


acceptmove(ma, ta, af)
int ma, ta, af;
{
	register int n;
	int moved = 0;
	int full, vma, dir, ch;
	char buf[60], last = '\0';

	if (scroll >= 22) scroll = 18;
	move(scroll++, 0);
	clearline();
	printw("move (%d,%c%d): ", ma, (af ? '\'' : ' '), ta);
	refresh();
	n = 0;
	while((ch = getch()) != '\n'){
		if (ch != EOF){
			buf[n++] = ch;
			addch(ch);
			refresh();
		}
		if(ch == '' && n > 1)
			n -= 2;
	}
	buf[n] = '\0';
	buf[9] = '\0';
	dir = pos[player].dir;
	vma = ma;
	for (n = 0; buf[n]; n++)
		if (!(buf[n] == 'r' || buf[n] == '\n' || buf[n] == 'l' || (buf[n] == '0' && !n) || isdigit(buf[n]) || buf[n] == 'b' || (buf[n] == 'd' && !n))){
			if (isspace(buf[n])){
				strcpy(buf+n, buf+n+1);
				n--;
			} else {
				Signal("Syntax error.",0,0);
				buf[n--] = '\0';
			}
		}
		else
			switch(buf[n]){
				case 'l':
					dir -= 2;
				case 'r':
					dir++;
					if (!dir) dir = 8;
					if (dir == 9) dir = 1;
					if (last == 't'){
						Signal("Error; ship can't turn that fast.", 0, 0);
						buf[n--] = '\0';
					}
					last = 't';
					ma--;
					ta--;
					vma = min(ma, maxmove(player, dir, 0));
					if (ta < 0 && moved || vma < 0 && moved)
						buf[n--] = '\0';
					break;

				case 'b':
					ma--;
					vma--;
					last = 'b';
					if (ta < 0 && moved || vma < 0 && moved)
						buf[n--] = '\0';
					break;

				case '0':
				case 'd':
					buf[n--] = '\0';
					break;

				case '\n':
					buf[n] = '\0';
					break;

				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
					if (last == '0'){
						Signal("Error; value out of range.", 0, 0);
						buf[n--] = '\0';
					}
					last = '0';
					moved = 1;
					ma -= buf[n] - '0';
					vma -= buf[n] - '0';
					if (ta < 0 && moved || vma < 0 && moved)
						buf[n--] = '\0';
					break;

			} /* end switch and else and for */
	if (ta < 0 && moved || vma < 0 && moved || af && turnfirst(buf) && moved){
		Signal("Movement error.", 0, 0);
		if (ta < 0 && moved){
			if (scene[game].ship[player].file -> FS == 1){
				Write(FILES + player, 0, 230, 0);
				Signal("No hands to set full sails.", 0, 0);
			}
		}
		else if (ma >= 0)
			buf[1] = '\0';
	}
	if (af && !moved){
		if (scene[game].ship[player].file -> FS == 1){
			Write(FILES + player, 0, 230, 0);
			Signal("No hands to set full sails.", 0, 0);
		}
	}
	strcpy(movebuf, buf);
	if (!*movebuf) strcpy(movebuf, "d");
	Write(FILES + player, 1, 72, movebuf);
	Signal("Helm: %s.", -1, movebuf);
}

boarding()
{
	register int n;
	int crew[3];
	int captured, men = 0;
	struct shipspecs *ptr;

	ptr = &specs[scene[game].ship[player].shipnum];
	crew[0] = ptr -> crew1;
	crew[1] = ptr -> crew2;
	crew[2] = ptr -> crew3;
	for(n=0; n < 3; n++){
		if (scene[game].ship[player].file -> OBP[n].turnsent)
			    men += scene[game].ship[player].file -> OBP[n].turnsent;
	}
	for(n=0; n < 3; n++){
		if (scene[game].ship[player].file -> DBP[n].turnsent)
			    men += scene[game].ship[player].file -> DBP[n].turnsent;
	}
	if (men){
		crew[0] = men/100 ? 0 : crew[0] != 0;
		crew[1] = (men%100)/10 ? 0 : crew[1] != 0;
		crew[2] = men%10 ? 0 : crew[2] != 0;
	} else {
		crew[0] = crew[0] != 0;
		crew[1] = crew[1] != 0;
		crew[2] = crew[2] != 0;
	}
	for (n=0; n < scene[game].vessels; n++){
		if ((captured = scene[game].ship[n].file -> captured) < 0)
			captured = n;
		if (n != player && pos[n].dir && range(player, n) <= 1 && scene[game].ship[player].nationality != scene[game].ship[captured].nationality){
			if (meleeing(player, n) && crew[2]){
				Signal("How many more to board the %s (%c%c)? ",n,0);
				parties(crew, n, 200);
			}
			else if ((foul(player, n) || grapple(player, n)) && crew[2]){
				Signal("Crew sections to board the %s (%c%c) (3 max) ?", n, 0);
				parties(crew, n, 200);
			}
		}
	}
	if (crew[2]){
		Signal("How many sections to repel boarders? ", 0, 0);
		parties(crew, player, 260);
	}
}

parties(crew, n, offset)
int crew[3], n, offset;
{
	register int k, j, men; 
	struct BP * ptr;
	int  buf;
	int temp[3];

	for (k=0; k < 3; k++)
		temp[k] = crew[k];
	while((buf = getch()) == EOF);
	addch(buf);
	if (isdigit(buf)){
		ptr = offset == 200 ? scene[game].ship[player].file -> OBP : scene[game].ship[player].file -> DBP ; 
		for (j = 0; j < 3 && ptr[j].turnsent; j++);
		if (!ptr[j].turnsent && buf > '0'){
			men = 0;
			for (k=0; k < 3 && buf > '0'; k++){
				men += crew[k]*power(10, 2-k);
				crew[k] = 0;
				if (men) buf -= 1;
			}
			if (buf > '0')
				Signal("Sending all crew sections.", 0, 0);
			Write(FILES + player, 0, 30 + (offset > 200)*18 + 6*j, turn);
			Write(FILES + player, 0, 30 + (offset > 200)*18 + 6*j + 2, n);
			Write(FILES + player, 0, 30 + (offset > 200)*18 + 6*j + 4, men);
			switch(offset){
				case 200:
					wmove(slot, 0, 0);
					for (k=0; k < 3; k++)
						if (temp[k] && !crew[k])
							waddch(slot, k + '1');
						else
							wmove(slot, 0, 1 + k);
					wmove(slot, 1, 0);
					waddstr(slot, "OBP");
					makesignal("boarding the %s (%c%c)", n, player);
					break;
				case 260:
					wmove(slot, 2, 0);
					for (k=0; k < 3; k++)
						if (temp[k] && !crew[k])
							waddch(slot, k + '1');
						else
							wmove(slot, 2, 1 + k);
					wmove(slot, 3, 0);
					waddstr(slot, "DBP");
					makesignal("repelling boarders", 0, player);
					break;
			}
			wrefresh(slot);
		}
		else
			Signal("Sending no crew sections.", 0, 0);
	}
}

power(base, exp)
int base, exp;
{
	switch(exp){
		case 0:
			return(1);
		case 1:
			return(base);
		case 2:
			return(base * base);
	}
	return(0);
}

repair()
{
	int buf;
	int *repairs;
	struct shipspecs *ptr;

	if (!repaired && !loaded && !fired && !changed && !turned()){
		ptr = &specs[scene[game].ship[player].shipnum];
		Signal("Repair (hull, guns, rigging)? ", 0, 0);
		while((buf = getch()) == EOF);
		addch(buf);
		switch(buf){
			case 'h':
				repairs = &scene[game].ship[player].file -> RH;
				break;
			case 'g':
				repairs = &scene[game].ship[player].file -> RG;
				break;
			case 'r':
				repairs = &scene[game].ship[player].file -> RR;
				break;
			default:
				Signal("Avast heaving!", 0, 0);
				return;
		}
		repaired = 1;
		*repairs += 1;
		if (*repairs >= 3){
			*repairs = 0;
		}
		if (!*repairs){
			switch(buf){
				case 'h':
					if (ptr -> hull < ptr -> guns/4)
						Write(SPECS + player, 0, 10, ptr -> hull + 2);
					else buf = 0;
					break;
				case 'g':
					if (ptr -> gunL < ptr -> gunR){
						if (ptr -> gunL + ptr -> carL < ptr -> guns/5)
							Write(SPECS + player, 0, 20, ptr -> gunL + 2);
						else buf = 0;
					} else
						if (ptr -> gunR + ptr -> carR < ptr -> guns/5)
							Write(SPECS + player, 0, 22, ptr -> gunR + 2);
						else buf = 0;
					break;
				case 'r':
					if (!ptr -> rig4)
						Write(SPECS + player, 0, 34, ptr -> rig4 + 2);
					else if (!ptr -> rig3)
						Write(SPECS + player, 0, 32, 2);
					else if (!ptr -> rig2)
						Write(SPECS + player, 0, 30, 2);
					else if (ptr -> rig1 < 4)
						Write(SPECS + player, 0, 28, 2);
					else buf = 0;
					break;
			}
			if (!buf)
				Signal("Repairs completed.", 0, 0);
		}
	}
	else
		Signal("No hands free to repair",0,0);
}

turned()
{
	register int n;

	for (n=0; movebuf[n]; n++)
		if (movebuf[n] == 'r' || movebuf[n] == 'l')
			return(1);
	return(0);
}

loadplayer()
{
	int buf;
	int loadL, loadR, ready, load, *Ready, *Load;

	if (!specs[scene[game].ship[player].shipnum].crew3){
		Signal("out of crew",0,0);
		return(0);
	}
	Load = &scene[game].ship[player].file -> loadL;
	Ready = &scene[game].ship[player].file -> readyL;
	loadL = *Load;
	loadR = *(Load + 1);
	if (!loadL && !loadR){
		Signal("Load which broadside (left or right)? ", 0, 0);
		while((buf = getch()) == EOF);
		addch(buf);
		if (buf == 'r')
			loadL = 1;
		else
			loadR = 1;
	}
	if ((!loadL && loadR || loadL && !loadR)){
		Signal("Reload with (round, double, chain, grape)? ", 0, 0);
		while((buf = getch()) == EOF);
		addch(buf);
		switch(buf){
			case 'r':
				load = ROUND;
				ready = 1;
				break;
			case 'd':
				load = DOUBLE;
				ready = 2;
				break;
			case 'c':
				load = CHAIN;
				ready = 1;
				break;
			case 'g':
				load = GRAPE;
				ready = 1;
				break;
			default:
				Signal("Broadside not loaded.", 0, 0);
				return;
		}
		if (!loadR){
			*(Load + 1) = load;
			*(Ready + 1) = ready;
		}
		else {
			*Load = load;
			*Ready = ready;
		}
		loaded = 1;
	}
}

changesail()
{
	int buf;
	int rig, full;

	rig = specs[scene[game].ship[player].shipnum].rig1;
	full = scene[game].ship[player].file -> FS;
	if ((windspeed == 6) || (windspeed == 5 && specs[scene[game].ship[player].shipnum].class > 4))
		rig = 0;
	if (specs[scene[game].ship[player].shipnum].crew3 && rig){
		if (!full){
			Signal("Increase to Full sails? ", 0, 0);
			while((buf = getch()) == EOF);
			addch(buf);
			if (buf == 'y'){
				changed = 1;
				Write(FILES + player, 0, 230, 1);
			}
		}
		else {
			Signal("Reduce to Battle sails? ", 0, 0);
			while((buf = getch()) == EOF);
			addch(buf);
			if (buf == 'y'){
				Write(FILES + player, 0, 230, 0);
				changed = 1;
			}
		}
	}
	else if (!rig)
		Signal("Sails rent to pieces",0,0);
}


signalflags()
{
	register int n;

	for(n=0; n < scene[game].vessels; n++){
		if (*scene[game].ship[n].file -> signal){
			putchar('\7');
			Signal("%s (%c%c): %s",n,scene[game].ship[n].file -> signal);
			*scene[game].ship[n].file -> signal = '\0';
		}
	}
}

iplotships()		/* new turn; also plot-ships */
{
	repaired = loaded = fired = changed = 0;
	plotships();
}


plotships()			/* uses ken's package */
{
	register int n;
	char ch;
	int sternr,sternc;

	getyx(stdscr, ylast, xlast);
	screen();
	readpos();
	werase(view);
	if (pos[player].row < viewrow + 5)
		viewrow = pos[player].row - ROWSINVIEW + 5;
	else if (pos[player].row > viewrow + ROWSINVIEW - 5)
		viewrow = pos[player].row - 5;
	if (pos[player].col < viewcol + 10)
		viewcol = pos[player].col - COLSINVIEW + 10;
	else if (pos[player].col > viewcol + COLSINVIEW - 10)
		viewcol = pos[player].col - 10;
	for (n=0; n < scene[game].vessels; n++)
		if (pos[n].dir && pos[n].row > viewrow && pos[n].row < viewrow + ROWSINVIEW && pos[n].col > viewcol && pos[n].col < viewcol + COLSINVIEW){
			wmove(view, pos[n].row - viewrow, pos[n].col - viewcol);
			waddch(view, colours(n));
			ch = sterncolor(n, &sternr, &sternc);
			wmove(view, sternr - viewrow, sternc - viewcol);
			waddch(view, ch);
		}
	wrefresh(view);
	move(ylast, xlast);
	refresh();
}

acceptsignal()
{
	int ch;
	char buf[60];
	register int n;

	if(scroll == 23) scroll = 18;
	Signal("Message? ",0,0);
	buf[0] = 34;
	n = 1;
	while((ch = getch()) != '\n'){
		if (ch != EOF){
			buf[n++] = ch;
			addch(ch);
			refresh();
		}
		if (ch == '' && n > 1)
			n -= 2;
	}
	buf[n] = 34;
	buf[n+1] = '\0';
	buf[59] = '\0';
	Write(FILES + player, 1, 164, buf);
}


board()
{
	register int n;
	char *name;
	int class, junk;

	clear();
	werase(view);
	werase(slot);
	move(1,0);
	for (n=0; n < 80; n++)
		addch('-');
	move(17,0);
	for (n=0; n < 80; n++)
		addch('-');
	for (n=2; n < 17; n++){
		mvaddch(n, 0,'|');
		mvaddch(n, 79, '|');
	}
	mvaddch(1,0,'+');
	mvaddch(17,0,'+');
	mvaddch(1,79,'+');
	mvaddch(17,79,'+');
	wmove(view, 2, 27);
	waddstr(view, "Wooden Ships & Iron Men");
	wmove(view, 4, (77 - lengthof(scene[game].name))/2);
	waddstr(view, scene[game].name);
	refresh();
	wrefresh(view);
	switch((class = specs[scene[game].ship[player].shipnum].class)){
		case 2:
		case 1:
			name = "SOL";
			break;
		case 3:
		case 4:
			name = "Frigate";
			break;
		case 5:
		case 6:
			name = "Sloop";
			break;
	}
	move(0,0);
	printw("Class %d %s (%d guns) '%s' (%c%c)", class, name, specs[scene[game].ship[player].shipnum].guns, scene[game].ship[player].shipname, colours(player), sterncolor(player, &junk, &junk));
}

clearline()
{
	register int n;

	move(scroll-1, 0);
	for (n=0; n < 59; n++)
		addch(' ');
	move(scroll-1, 0);
}



Signal(fmt, shipnum, string)
int shipnum;
char *fmt, *string;
{
	int junk;

	move(scroll++, 0);
	clearline();
	if (scroll > 23) scroll = 18;
	if (shipnum == -1)
		printw(fmt, string);
	else if (*string == '*')
		printw(fmt, "computer", '0','0', string);
	else
		printw(fmt, scene[game].ship[shipnum].shipname, colours(shipnum), sterncolor(shipnum, &junk, &junk), string);
	refresh();
}

char *quality(shipnum)
int shipnum;
{
	switch(specs[scene[game].ship[shipnum].shipnum].qual){

		case 5:
			return("elite");
		case 4:
			return("crack");
		case 3:
			return("mundane");
		case 2:
			return("green");
		case 1:
			return("mutinous");
	}
}

char *info(ship, final)
int ship;
char *final;
{
	sprintf(final, "%d gun \0", specs[scene[game].ship[ship].shipnum].guns);
	switch(specs[scene[game].ship[ship].shipnum].class){
		case 1:
		case 2:
			strcat(final, "Ship of the Line");
			break;
		case 3:
			strcat(final, "Frigate");
			break;
		case 4:
			strcat(final, "Corvette");
			break;
		case 5:
			strcat(final, "Sloop");
			break;
		case 6:
			strcat(final, "Brig");
			break;
	}
	return(final);
}

screen()
{
	int class;
	register int n;
	int dr = 0, dc = 0;
	struct shipspecs *data;
	struct File *ptr;

	scene[game].ship[player].file -> readyL--;
	scene[game].ship[player].file -> readyR--;
	movebuf[0] = '\0';
	sync();
	if (turn % 50 == 0)
		Write(SCENARIO, 0, 10, 1);	/* still playing */
	windspeed = scene[game].windspeed;
	winddir = scene[game].winddir;
	turn = scene[game].turn;
	move(0, 47);
	ptr = scene[game].ship[player].file;
	data = &specs[scene[game].ship[player].shipnum];
	if (ptr -> FS == 1)
		Write(FILES + player, 0, 230, 2);
	printw("Points:%3d Fouls:%2d  Grapples:%2d",ptr -> points, fouled(player), grappled(player));
	move(17, 36);
	printw("Turn %d", turn);
	move(18, 59);
	printw("Load  %c%c %c%c", symbol(ptr -> loadL), iinitial(ptr -> readyL), symbol(ptr -> loadR), iinitial(ptr -> readyR));
	move(19, 59);
	printw("Hull %2d", data -> hull);
	move(20, 59);
	printw("Crew %2d %2d %2d", data -> crew1, data -> crew2, data -> crew3);
	move(21, 59);
	printw("Guns %2d %2d", data -> gunL, data -> gunR);
	move(22, 59);
	printw("Carr %2d %2d", data -> carR, data -> carL);
	move(23, 59);
	printw("Rigg %d %d %d ", data -> rig1, data -> rig2, data -> rig3);
	if (data -> rig4 < 0)
		addch('-');
	else
		printw("%d", data -> rig4);
	move(18, 74);
	printw("0 %d(%d)", maxmove(player, winddir + 3, -1), maxmove(player, winddir + 3, 1));
	move(19, 73);
	addstr("\\|/");
	move(20, 73);
	printw("-^-%d(%d)", maxmove(player, winddir + 2, -1), maxmove(player, winddir + 2, 1));
	move(21, 73);
	addstr("/|\\");
	move(22, 74);
	printw("| %d(%d)", maxmove(player, winddir + 1, -1), maxmove(player, winddir + 1, 1));
	move(23, 73);
	printw("%d(%d)", maxmove(player, winddir, -1), maxmove(player, winddir, 1));
	refresh();
	if (!boarders(player, 0)){
		wmove(slot, 0, 0);
		waddstr(slot, "   ");
		wmove(slot, 1, 0);
		waddstr(slot, "   ");
	} else {
		wmove(slot, 1, 0);
		waddstr(slot, "OBP");
	}
	if (!boarders(player, 1)){
		wmove(slot, 2, 0);
		waddstr(slot, "   ");
		wmove(slot, 3, 0);
		waddstr(slot, "   ");
	} else {
		wmove(slot, 3, 0);
		waddstr(slot, "DBP");
	}
	wmove(slot, 12, 0);
	if (n = scene[game].ship[player].file -> RH)
		wprintw(slot, "%dRH", n);
	else
		waddstr(slot, "   ");
	wmove(slot, 13, 0);
	if (n = scene[game].ship[player].file -> RG)
		wprintw(slot, "%dRG", n);
	else
		waddstr(slot, "   ");
	wmove(slot, 14, 0);
	if (n = scene[game].ship[player].file -> RR)
		wprintw(slot, "%dRR", n);
	else
		waddstr(slot, "   ");
	wmove(slot, 7, 1);
	wprintw(slot,"%d", windspeed);
	drdc(&dr, &dc, winddir);
	wmove(slot, 7, 0); waddch(slot, ' ');
	wmove(slot, 7, 2); waddch(slot, ' ');
	wmove(slot, 6, 0); waddch(slot, ' ');
	wmove(slot, 6, 1); waddch(slot, ' ');
	wmove(slot, 6, 2); waddch(slot, ' ');
	wmove(slot, 8, 0); waddch(slot, ' ');
	wmove(slot, 8, 1); waddch(slot, ' ');
	wmove(slot, 8, 2); waddch(slot, ' ');
	wmove(slot, (7 - dr), (1 - dc));
	switch(winddir){
		case 1:
		case 5:
			waddch(slot, '|');
			break;
		case 2:
		case 6:
			waddch(slot, '/');
			break;
		case 3:
		case 7:
			waddch(slot, '-');
			break;
		case 4:
		case 8:
			waddch(slot, '\\');
			break;
	}
	wmove(slot, (7 + dr), (1 + dc));
	waddch(slot, '+');
	wrefresh(slot);
	signal(SIGALRM, iplotships);	/* new turn and plot-ships */
	alarm(7);
}

extern char iinitial(ready)
int ready;
{
	if (ready <= -30000)
		return('!');
	if (ready > 0)
		return('*');
	return(' ');
}

char symbol(number)
int number;
{
	switch(number){
		case GRAPE:
			return('G');
		case ROUND:
			return('R');
		case DOUBLE:
			return('D');
		case CHAIN:
			return('C');
	}
	return('-');
}

