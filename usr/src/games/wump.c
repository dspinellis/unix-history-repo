
static char sccsid[] = "	wump.c	4.1	82/10/24	";

#
#include <stdio.h>

/*
 *	wumpus
 *	stolen from PCC Vol 2 No 1
 */

#define	NBAT	3
#define	NROOM	20
#define	NTUNN	3
#define	NPIT	3
#define BIGINT 2147483648.0

struct room
{
	int	tunn[NTUNN];
	int	flag;
} room[NROOM];

char	*intro[] =
{
	"\n",
	"Welcome to 'Hunt the Wumpus.'\n",
	"\n",
	"The Wumpus lives in a cave of %d rooms.\n",
	"Each room has %d tunnels leading to other rooms.\n",
	"\n",
	"Hazards:\n",
	"\n",
	"Bottomless Pits - Some rooms have Bottomless Pits in them.\n",
	"	If you go there, you fall into the pit and lose!\n",
	"Super Bats - Some other rooms have super bats.\n",
	"	If you go there, a bat will grab you and take you to\n",
	"	somewhere else in the cave where you could\n",
	"	fall into a pit or run into the . . .\n",
	"\n",
	"Wumpus:\n",
	"\n",
	"The Wumpus is not bothered by the hazards since\n",
	"he has sucker feet and is too big for a bat to lift.\n",
	"\n",
	"Usually he is asleep.\n",
	"Two things wake him up:\n",
	"	your entering his room\n",
	"	your shooting an arrow anywhere in the cave.\n",
	"If the wumpus wakes, he either decides to move one room or\n",
	"stay where he was.  But if he ends up where you are,\n",
	"he eats you up and you lose!\n",
	"\n",
	"You:\n",
	"\n",
	"Each turn you may either move or shoot a crooked arrow.\n",
	"\n",
	"Moving - You can move to one of the adjoining rooms;\n",
	"	that is, to one that has a tunnel connecting it with\n",
	"	the room you are in.\n",
	"\n",
	"Shooting - You have 5 arrows.  You lose when you run out.\n",
	"	Each arrow can go from 1 to 5 rooms.\n",
	"	You aim by telling the computer\n",
	"	The arrow's path is a list of room numbers\n",
	"	telling the arrow which room to go to next.\n",
	"	The list is terminated with a 0.\n",
	"	The first room in the path must be connected to the\n",
	"	room you are in.  Each succeeding room must be\n",
	"	connected to the previous room.\n",
	"	If there is no tunnel between two of the rooms\n",
	"	in the arrow's path, the arrow chooses one of the\n",
	"	three tunnels from the room it's in and goes its\n",
	"	own way.\n",
	"\n",
	"	If the arrow hits the wumpus, you win!\n",
	"	If the arrow hits you, you lose!\n",
	"\n",
	"Warnings:\n",
	"\n",
	"When you are one or two rooms away from the wumpus,\n",
	"the computer says:\n",
	"		'I smell a Wumpus'\n",
	"When you are one room away from some other hazard, it says:\n",
	"		Bat    - 'Bats nearby'\n",
	"		Pit    - 'I feel a draft'\n",
	"\n",
	0,
};

#define	BAT	01
#define	PIT	02
#define	WUMP	04

int	arrow;
int	loc;
int	wloc;
int	tchar;

main()
{
	register i, j;
	register struct room *p;
	int k, icomp();

	printf("Instructions? (y-n) ");
	if(rline() == 'y')
		for(i=0; intro[i]; i++)
			printf(intro[i], i&1? NROOM: NTUNN);


/*
 * initialize the room connections
 */

init:
	p = &room[0];
	for(i=0; i<NROOM; i++) {
		for(j=0; j<NTUNN; j++)
			p->tunn[j] = -1;
		p++;
	}
	k = 0;
	for(i=1; i<NROOM; ) {
		j = rnum(NROOM);
		p = &room[j];
		if(j == k || p->tunn[0] >= 0 || p->tunn[1] >= 0)
			continue;
		p->tunn[1] = k;
		room[k].tunn[0] = j;
		k = j;
		i++;
	}
	p = &room[0];
	for(i=0; i<NROOM; i++) {
		for(j=0; j<NTUNN; j++) {
			if(p->tunn[j] < 0)
				p->tunn[j] = tunnel(i);
			if(p->tunn[j] == i)
				goto init;
			for(k=0; k<j; k++)
				if(p->tunn[j] == p->tunn[k])
					goto init;
		}
		qsort(&p->tunn[0], NTUNN, sizeof(p->tunn[0]), icomp);
		p++;
	}

/*
 * put in player, wumpus,
 * pits and bats
 */

setup:
	arrow = 5;
	p = &room[0];
	for(i=0; i<NROOM; i++) {
		p->flag = 0;
		p++;
	}
	for(i=0; i<NPIT; ) {
		p = &room[rnum(NROOM)];
		if((p->flag&PIT) == 0) {
			p->flag |= PIT;
			i++;
		}
	}
	for(i=0; i<NBAT; ) {
		p = &room[rnum(NROOM)];
		if((p->flag&(PIT|BAT)) == 0) {
			p->flag |= BAT;
			i++;
		}
	}
	i = rnum(NROOM);
	wloc = i;
	room[i].flag |= WUMP;
	for(;;) {
		i = rnum(NROOM);
		if((room[i].flag&(PIT|BAT|WUMP)) == 0) {
			loc = i;
			break;
		}
	}

/*
 *	main loop of the game
 */

loop:
	printf("You are in room %d\n", loc+1);
	p = &room[loc];
	if(p->flag&PIT) {
		printf("You fell into a pit\n");
		goto done;
	}
	if(p->flag&WUMP) {
		printf("You were eaten by the wumpus\n");
		goto done;
	}
	if(p->flag&BAT) {
		printf("Theres a bat in your room\n");
		loc = rnum(NROOM);
		goto loop;
	}
	for(i=0; i<NTUNN; i++)
	if(near(&room[p->tunn[i]], WUMP))
		goto nearwump;
	if (near(p, WUMP)) {
	nearwump:
		printf("I smell a wumpus\n");
	}
	if (near(p, BAT))
		printf("Bats nearby\n");
	if (near(p, PIT))
		printf("I feel a draft\n");
	printf("There are tunnels to");
	for(i=0; i<NTUNN; i++)
		printf(" %d", p->tunn[i]+1);
	printf("\n");

again:
	printf("Move or shoot (m-s) ");
	switch(rline()) {
	case 'm':
		if(tchar == '\n')
			printf("which room? ");
		i = rin()-1;
		for(j=0; j<NTUNN; j++)
			if(i == p->tunn[j])
				goto groom;
		printf("You hit the wall\n");
		goto again;
	groom:
		loc = i;
		if(i == wloc)
			goto mwump;
		goto loop;

	case 's':
		if(tchar == '\n')
			printf("Give list of rooms terminated by 0\n");
		for(i=0; i<5; i++) {
			j = rin()-1;
			if(j == -1)
				break;
		ranarw:
			for(k=0; k<NTUNN; k++)
				if(j == p->tunn[k])
					goto garow;
			j = rnum(NROOM);
			goto ranarw;
		garow:
			p = &room[j];
			if(j == loc) {
				printf("You shot yourself\n");
				goto done;
			}
			if(p->flag&WUMP) {
				printf("You slew the wumpus\n");
				goto done;
			}
		}
		if(--arrow == 0) {
			printf("That was your last shot\n");
			goto done;
		}
		goto mwump;
	}

	goto again;

mwump:
	p = &room[wloc];
	p->flag &= ~WUMP;
	i = rnum(NTUNN+1);
	if(i != NTUNN)
		wloc = p->tunn[i];
	room[wloc].flag |= WUMP;
	goto loop;

done:
	drain();
	printf("Another game? (y-n) ");
	if(rline() != 'n') {
		drain();
		printf("Same room setup? (y-n) ");
		if(rline() != 'n')
			goto setup;
		goto init;
	}
}

tunnel(i)
{
	register struct room *p;
	register n, j;
	int c;

	c = 20;

loop:
	n = rnum(NROOM);
	if(n == i)
		if(--c > 0)
			goto loop;
	p = &room[n];
	for(j=0; j<NTUNN; j++)
	if(p->tunn[j] == -1) {
		p->tunn[j] = i;
		return(n);
	}
	goto loop;
}

rline()
{
	register char c, r;

	while((c=getchar()) == ' ');
	r = c;
	while(c != '\n' && c != ' ') {
		if(c == EOF)
			exit();
		c = getchar();
	}
	tchar = c;
	return(r);
}

rnum(n)
{
	static short first[2];

	if(first[1] == 0) {
		time(first);
		if(first[1]==0) first[1] = 1;
		srand((first[1]*first[0])^first[1]);
	}
	return((int)((rand()/BIGINT) * n));
}

rin()
{
	register n, c;

	n = 0;
	c = getchar();
	while(c != '\n' && c != ' ') {
		if(c<'0' || c>'9') {
			while(c != '\n') {
				if(c == EOF)
					exit();
				c = getchar();
			}
			return(0);
		}
		n = n*10 + c-'0';
		c = getchar();
	}
	return(n);
}

near(ap, ahaz)
struct room *ap;
{
	register struct room *p;
	register haz, i;

	p = ap;
	haz = ahaz;
	for(i=0; i<NTUNN; i++)
	if(room[p->tunn[i]].flag & haz)
		return (1);
	return(0);
}

icomp(p1, p2)
int *p1, *p2;
{

	return(*p1 - *p2);
}
#include <sgtty.h>
drain()
{
	register FILE *port = stdin;
	register int iodes = fileno(port);
	struct sgttyb arg;

	port->_cnt = 0;
	port->_ptr = port->_base;
	if(gtty(iodes,&arg) != -1) stty(iodes,&arg);
}
