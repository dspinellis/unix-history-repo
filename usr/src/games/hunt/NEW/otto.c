# ifdef OTTO
/*
 *	otto	- a hunt otto-matic player
 *
 *		This guy is buggy, unfair, stupid, and not extensible.
 *	Future versions of hunt will have a subroutine library for
 *	automatic players to link to.  If you write your own "otto"
 *	please let us know what subroutines you would expect in the
 *	subroutine library.
 *
 *	$Header: otto.c,v 1.8 89/04/19 19:52:39 gregc Exp $
 */

# include	<curses.h>
# include	<ctype.h>
# include	"hunt.h"
# include	<sys/time.h>
# include	<signal.h>
# undef		WALL
# undef		NORTH
# undef		SOUTH
# undef		WEST
# undef		EAST
# undef		FRONT
# undef		LEFT
# undef		BACK
# undef		RIGHT

extern char	*index();

extern	char	screen[SCREEN_HEIGHT][SCREEN_WIDTH2];

# ifndef DEBUG
# define	STATIC		static
# else
# define	STATIC
# endif DEBUG

# define	OPPONENT	"{}i!"
# define	PROPONENT	"^v<>"
# define	WALL		"+\\/#*-|"
# define	PUSHOVER	" bg;*#&"
# define	SHOTS		"$@Oo:"

/* number of "directions" */
# define	NUMDIRECTIONS	4

/* absolute directions (facings) - counterclockwise */
# define	NORTH		0
# define	WEST		1
# define	SOUTH		2
# define	EAST		3
# define	ALLDIRS		0xf

/* relative directions - counterclockwise */
# define	FRONT		0
# define	LEFT		1
# define	BACK		2
# define	RIGHT		3

# define	ABSCHARS	"NWSE"
# define	RELCHARS	"FLBR"
# define	DIRKEYS		"khjl"

STATIC	char	command[BUFSIZ];
STATIC	int	comlen;

# ifdef	DEBUG
STATIC FILE	*debug = NULL;
# endif	DEBUG

# define	DEADEND		0x1
# define	ON_LEFT		0x2
# define	ON_RIGHT	0x4
# define	ON_SIDE		(ON_LEFT|ON_RIGHT)
# define	BEEN		0x8
# define	BEEN_SAME	0x10

struct	item	{
	char	what;
	int	distance;
	int	flags;
};

STATIC	struct	item	flbr[NUMDIRECTIONS];

# define	fitem	flbr[FRONT]
# define	litem	flbr[LEFT]
# define	bitem	flbr[BACK]
# define	ritem	flbr[RIGHT]

STATIC	int		facing;
STATIC	int		row, col;
STATIC	int		num_turns;		/* for wandering */
STATIC	char		been_there[HEIGHT][WIDTH2];
STATIC	struct itimerval	pause_time	= { { 0, 0 }, { 0, 55000 }};

STATIC
nothing()
{
}

otto(y, x, face)
	int	y, x;
	char	face;
{
	register int	i;
	extern	int	Otto_count;
	int		old_mask;

# ifdef	DEBUG
	if (debug == NULL) {
		debug = fopen("bug", "w");
		setbuf(debug, NULL);
	}
	fprintf(debug, "\n%c(%d,%d)", face, y, x);
# endif	DEBUG
	(void) signal(SIGALRM, nothing);
	old_mask = sigblock(sigmask(SIGALRM));
	setitimer(ITIMER_REAL, &pause_time, NULL);
	sigpause(old_mask);
	sigsetmask(old_mask);

	/* save away parameters so other functions may use/update info */
	switch (face) {
	case '^':	facing = NORTH; break;
	case '<':	facing = WEST; break;
	case 'v':	facing = SOUTH; break;
	case '>':	facing = EAST; break;
	default:	abort();
	}
	row = y; col = x;
	been_there[row][col] |= 1 << facing;

	/* initially no commands to be sent */
	comlen = 0;

	/* find something to do */
	look_around();
	for (i = 0; i < NUMDIRECTIONS; i++) {
		if (index(OPPONENT, flbr[i].what) != NULL) {
			attack(i, &flbr[i]);
			bzero(been_there, sizeof been_there);
			goto done;
		}
	}

	if (index(SHOTS, bitem.what) != NULL && !(bitem.what & ON_SIDE)) {
		duck(BACK);
		bzero(been_there, sizeof been_there);
# ifdef BOOTS
	} else if (go_for_ammo(BOOT_PAIR)) {
		bzero(been_there, sizeof been_there);
	} else if (go_for_ammo(BOOT)) {
		bzero(been_there, sizeof been_there);
# endif
	} else if (go_for_ammo(GMINE))
		bzero(been_there, sizeof been_there);
	else if (go_for_ammo(MINE))
		bzero(been_there, sizeof been_there);
	else
		wander();

done:
	(void) write(Socket, command, comlen);
	Otto_count += comlen;
# ifdef	DEBUG
	(void) fwrite(command, 1, comlen, debug);
# endif	DEBUG
}

# define	direction(abs,rel)	(((abs) + (rel)) % NUMDIRECTIONS)

STATIC
stop_look(itemp, c, dist, side)
	struct	item	*itemp;
	char	c;
	int	dist;
	int	side;
{
	switch (c) {

	case SPACE:
		if (side)
			itemp->flags &= ~DEADEND;
		return 0;

	case MINE:
	case GMINE:
# ifdef BOOTS
	case BOOT:
	case BOOT_PAIR:
# endif
		if (itemp->distance == -1) {
			itemp->distance = dist;
			itemp->what = c;
			if (side < 0)
				itemp->flags |= ON_LEFT;
			else if (side > 0)
				itemp->flags |= ON_RIGHT;
		}
		return 0;

	case SHOT:
	case GRENADE:
	case SATCHEL:
	case BOMB:
# ifdef OOZE
	case SLIME:
# endif OOZE
		if (itemp->distance == -1 || (!side
		    && (itemp->flags & ON_SIDE
		    || itemp->what == GMINE || itemp->what == MINE))) {
			itemp->distance = dist;
			itemp->what = c;
			itemp->flags &= ~ON_SIDE;
			if (side < 0)
				itemp->flags |= ON_LEFT;
			else if (side > 0)
				itemp->flags |= ON_RIGHT;
		}
		return 0;

	case '{':
	case '}':
	case 'i':
	case '!':
		itemp->distance = dist;
		itemp->what = c;
		itemp->flags &= ~(ON_SIDE|DEADEND);
		if (side < 0)
			itemp->flags |= ON_LEFT;
		else if (side > 0)
			itemp->flags |= ON_RIGHT;
		return 1;

	default:
		/* a wall or unknown object */
		if (side)
			return 0;
		if (itemp->distance == -1) {
			itemp->distance = dist;
			itemp->what = c;
		}
		return 1;
	}
}

look(rel_dir, itemp)
	int		rel_dir;
	struct	item	*itemp;
{
	register int		r, c;
	register char		ch;

	itemp->what = 0;
	itemp->distance = -1;
	itemp->flags = DEADEND|BEEN;		/* true until proven false */

	switch (direction(facing, rel_dir)) {

	case NORTH:
		if (been_there[row - 1][col] & NORTH)
			itemp->flags |= BEEN_SAME;
		for (r = row - 1; r >= 0; r--)
			for (c = col - 1; c < col + 2; c++) {
				ch = screen[r][c];
				if (stop_look(itemp, ch, row - r, c - col))
					goto cont_north;
				if (c == col && !been_there[r][c])
					itemp->flags &= ~BEEN;
			}
	cont_north:
		if (itemp->flags & DEADEND) {
			itemp->flags |= BEEN;
			been_there[r][col] |= NORTH;
			for (r = row - 1; r > row - itemp->distance; r--)
				been_there[r][col] = ALLDIRS;
		}
		break;

	case SOUTH:
		if (been_there[row + 1][col] & SOUTH)
			itemp->flags |= BEEN_SAME;
		for (r = row + 1; r < HEIGHT; r++)
			for (c = col - 1; c < col + 2; c++) {
				ch = screen[r][c];
				if (stop_look(itemp, ch, r - row, col - c))
					goto cont_south;
				if (c == col && !been_there[r][c])
					itemp->flags &= ~BEEN;
			}
	cont_south:
		if (itemp->flags & DEADEND) {
			itemp->flags |= BEEN;
			been_there[r][col] |= SOUTH;
			for (r = row + 1; r < row + itemp->distance; r++)
				been_there[r][col] = ALLDIRS;
		}
		break;

	case WEST:
		if (been_there[row][col - 1] & WEST)
			itemp->flags |= BEEN_SAME;
		for (c = col - 1; c >= 0; c--)
			for (r = row - 1; r < row + 2; r++) {
				ch = screen[r][c];
				if (stop_look(itemp, ch, col - c, row - r))
					goto cont_east;
				if (r == row && !been_there[r][c])
					itemp->flags &= ~BEEN;
			}
	cont_west:
		if (itemp->flags & DEADEND) {
			itemp->flags |= BEEN;
			been_there[r][col] |= WEST;
			for (c = col - 1; c > col - itemp->distance; c--)
				been_there[row][c] = ALLDIRS;
		}
		break;

	case EAST:
		if (been_there[row][col + 1] & EAST)
			itemp->flags |= BEEN_SAME;
		for (c = col + 1; c < WIDTH; c++)
			for (r = row - 1; r < row + 2; r++) {
				ch = screen[r][c];
				if (stop_look(itemp, ch, c - col, r - row))
					goto cont_east;
				if (r == row && !been_there[r][c])
					itemp->flags &= ~BEEN;
			}
	cont_east:
		if (itemp->flags & DEADEND) {
			itemp->flags |= BEEN;
			been_there[r][col] |= EAST;
			for (c = col + 1; c < col + itemp->distance; c++)
				been_there[row][c] = ALLDIRS;
		}
		break;

	default:
		abort();
	}
}

STATIC
look_around()
{
	register int	i;

	for (i = 0; i < NUMDIRECTIONS; i++) {
		look(i, &flbr[i]);
# ifdef	DEBUG
		fprintf(debug, " look(%c)=%c(%d)(0x%x)",
			RELCHARS[i], flbr[i].what, flbr[i].distance, flbr[i].flags);
# endif	DEBUG
	}
}

/*
 *	as a side effect modifies facing and location (row, col)
 */

STATIC
face_and_move_direction(rel_dir, distance)
	int	rel_dir, distance;
{
	register int	old_facing;
	register char	cmd;

	old_facing = facing;
	cmd = DIRKEYS[facing = direction(facing, rel_dir)];

	if (rel_dir != FRONT) {
		register int	i;
		struct	item	items[NUMDIRECTIONS];

		command[comlen++] = toupper(cmd);
		if (distance == 0) {
			/* rotate look's to be in right position */
			for (i = 0; i < NUMDIRECTIONS; i++)
				items[i] =
					flbr[(i + old_facing) % NUMDIRECTIONS];
			bcopy(items, flbr, sizeof flbr);
		}
	}
	while (distance--) {
		command[comlen++] = cmd;
		switch (facing) {

		case NORTH:	row--; break;
		case WEST:	col--; break;
		case SOUTH:	row++; break;
		case EAST:	col++; break;
		}
		if (distance == 0)
			look_around();
	}
}

STATIC
attack(rel_dir, itemp)
	int		rel_dir;
	struct	item	*itemp;
{
	if (!(itemp->flags & ON_SIDE)) {
		face_and_move_direction(rel_dir, 0);
		command[comlen++] = 'o';
		command[comlen++] = 'o';
		duck(FRONT);
		command[comlen++] = ' ';
	} else if (itemp->distance > 1) {
		face_and_move_direction(rel_dir, 2);
		duck(FRONT);
	} else {
		face_and_move_direction(rel_dir, 1);
		if (itemp->flags & ON_LEFT)
			rel_dir = LEFT;
		else
			rel_dir = RIGHT;
		(void) face_and_move_direction(rel_dir, 0);
		command[comlen++] = 'f';
		command[comlen++] = 'f';
		duck(FRONT);
		command[comlen++] = ' ';
	}
}

STATIC
duck(rel_dir)
	int	rel_dir;
{
	int	dir;

	switch (dir = direction(facing, rel_dir)) {

	case NORTH:
	case SOUTH:
		if (index(PUSHOVER, screen[row][col - 1]) != NULL)
			command[comlen++] = 'h';
		else if (index(PUSHOVER, screen[row][col + 1]) != NULL)
			command[comlen++] = 'l';
		else if (dir == NORTH
			&& index(PUSHOVER, screen[row + 1][col]) != NULL)
				command[comlen++] = 'j';
		else if (dir == SOUTH
			&& index(PUSHOVER, screen[row - 1][col]) != NULL)
				command[comlen++] = 'k';
		else if (dir == NORTH)
			command[comlen++] = 'k';
		else
			command[comlen++] = 'j';
		break;

	case WEST:
	case EAST:
		if (index(PUSHOVER, screen[row - 1][col]) != NULL)
			command[comlen++] = 'k';
		else if (index(PUSHOVER, screen[row + 1][col]) != NULL)
			command[comlen++] = 'j';
		else if (dir == WEST
			&& index(PUSHOVER, screen[row][col + 1]) != NULL)
				command[comlen++] = 'l';
		else if (dir == EAST
			&& index(PUSHOVER, screen[row][col - 1]) != NULL)
				command[comlen++] = 'h';
		else if (dir == WEST)
			command[comlen++] = 'h';
		else
			command[comlen++] = 'l';
		break;
	}
}

/*
 *	go for the closest mine if possible
 */

STATIC
go_for_ammo(mine)
	char	mine;
{
	register int	i, rel_dir, dist;

	rel_dir = -1;
	dist = WIDTH;
	for (i = 0; i < NUMDIRECTIONS; i++) {
		if (flbr[i].what == mine && flbr[i].distance < dist) {
			rel_dir = i;
			dist = flbr[i].distance;
		}
	}
	if (rel_dir == -1)
		return FALSE;

	if (!(flbr[rel_dir].flags & ON_SIDE)
	|| flbr[rel_dir].distance > 1) {
		if (dist > 4)
			dist = 4;
		face_and_move_direction(rel_dir, dist);
	} else
		return FALSE;		/* until it's done right */
	return TRUE;
}

STATIC
wander()
{
	register int	i, j, rel_dir, dir_mask, dir_count;

	for (i = 0; i < NUMDIRECTIONS; i++)
		if (!(flbr[i].flags & BEEN) || flbr[i].distance <= 1)
			break;
	if (i == NUMDIRECTIONS)
		bzero(been_there, sizeof been_there);
	dir_mask = dir_count = 0;
	for (i = 0; i < NUMDIRECTIONS; i++) {
		j = (RIGHT + i) % NUMDIRECTIONS;
		if (flbr[j].distance <= 1 || flbr[j].flags & DEADEND)
			continue;
		if (!(flbr[j].flags & BEEN_SAME)) {
			dir_mask = 1 << j;
			dir_count = 1;
			break;
		}
		if (j == FRONT
		&& num_turns > 4 + (random() %
				((flbr[FRONT].flags & BEEN) ? 7 : HEIGHT)))
			continue;
		dir_mask |= 1 << j;
# ifdef notdef
		dir_count++;
# else
		dir_count = 1;
		break;
# endif
	}
	if (dir_count == 0) {
		duck(random() % NUMDIRECTIONS);
		num_turns = 0;
		return;
	} else if (dir_count == 1)
		rel_dir = ffs(dir_mask) - 1;
	else {
		rel_dir = ffs(dir_mask) - 1;
		dir_mask &= ~(1 << rel_dir);
		while (dir_mask != 0) {
			i = ffs(dir_mask) - 1;
			if (random() % 5 == 0)
				rel_dir = i;
			dir_mask &= ~(1 << i);
		}
	}
	if (rel_dir == FRONT)
		num_turns++;
	else
		num_turns = 0;

# ifdef DEBUG
	fprintf(debug, " w(%c)", RELCHARS[rel_dir]);
# endif DEBUG
	face_and_move_direction(rel_dir, 1);
}

# endif OTTO
