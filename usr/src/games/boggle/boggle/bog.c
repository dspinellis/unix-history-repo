/* vi: set tabstop=4 : */

/*
 * bog - the game of boggle
 *
 * 6-Mar-89     changed loaddict() to use a large buffer and check for overflow,
 *              minor cleanup
 */

#include "bog.h"

#include <ctype.h>
#include <stdio.h>

char *version = "bog V1.3 brachman@ubc.csnet 6-Mar-89";

struct dictindex dictindex[26];

/*
 * Cube position numbering:
 *
 *	0 1 2 3
 *	4 5 6 7
 *	8 9 A B
 *	C D E F
 */
static int adjacency[16][16] = {
/*    0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F */
	{ 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 },		/* 0 */
	{ 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0 },		/* 1 */
	{ 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 },		/* 2 */
	{ 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 },		/* 3 */
	{ 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0 },		/* 4 */
	{ 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0, 0 },		/* 5 */
	{ 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 0, 0 },		/* 6 */
	{ 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0 },		/* 7 */
	{ 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0 },		/* 8 */
	{ 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1, 0 },		/* 9 */
	{ 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0, 1, 1, 1 },		/* A */
	{ 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 1, 1 },		/* B */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0 },		/* C */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1, 0 },		/* D */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1 },		/* E */
	{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0 }		/* F */
};

static int letter_map[26][16];

char board[17];
int wordpath[MAXWORDLEN + 1];
int wordlen;		/* Length of last word returned by nextword() */
int usedbits;

char *pword[MAXPWORDS], pwords[MAXPSPACE], *pwordsp;
int npwords;

char *mword[MAXMWORDS], mwords[MAXMSPACE], *mwordsp;
int nmwords;

int ngames = 0;
int tnmwords = 0, tnpwords = 0;

#ifdef TIMER
#include <setjmp.h>

jmp_buf env;
#endif TIMER

long start_t;

static FILE *dictfp = (FILE *) NULL;

int batch;
int debug;
int minlength;
int reuse;
int tlimit;

char *batchword(), *getline();

long atol();
long random();

main(argc, argv)
int argc;
char **argv;
{
	int done, i, selfuse, sflag;
	char *bspec, *p;
	long seed;
	FILE *opendict();

	debug = 0;
	bspec = (char *) NULL;
	reuse = 0;
	batch = 0;
	selfuse = 0;
	minlength = 3;
	tlimit = 180;		/* 3 minutes is standard */
	sflag = 0;

	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
			case 'b':
				batch = 1;
				break;
			case 'd':
				debug = 1;
				break;
			case 's':
				sflag = 1;
				seed = atol(&argv[i][2]);
				break;
			case 't':
				if ((tlimit = atoi(&argv[i][2])) < 1) {
					(void) fprintf(stderr, "Bad time limit\n");
					exit(1);
				}
				break;
			case 'w':
				if ((minlength = atoi(&argv[i][2])) < 3) {
					(void) fprintf(stderr, "Min word length must be > 2\n");
					exit(1);
				}
				break;
			default:
				usage();
				/*NOTREACHED*/
			}
		}
		else if (strcmp(argv[i], "+") == 0)
			reuse = 1;
		else if (strcmp(argv[i], "++") == 0)
			selfuse = 1;
		else if (islower(argv[i][0])) {
			if (strlen(argv[i]) != 16) {
				usage();
				/*NOTREACHED*/
			}
			/* This board is assumed to be valid... */
			bspec = argv[i];
		}
		else {
			usage();
			/*NOREACHED*/
		}
	}

	if (batch && bspec == (char *) NULL) {
		(void) fprintf(stderr, "Must give both -b and a board setup\n");
		exit(1);
	}

	if (selfuse) {
		for (i = 0; i < 16; i++)
			adjacency[i][i] = 1;
	}

	if (batch) {
		newgame(bspec);
		while ((p = batchword(stdin)) != (char *) NULL)
			(void) printf("%s\n", p);
	}
	else {
		setup(sflag, seed);
		prompt("Loading the dictionary...");
		if ((dictfp = opendict(DICT)) == (FILE *) NULL) {
			(void) fprintf(stderr, "Can't load %s\n", DICT);
			cleanup();
			exit(1);
		}
#ifdef LOADDICT
		if (loaddict(dictfp) < 0) {
			(void) fprintf(stderr, "Can't load %s\n", DICT);
			cleanup();
			exit(1);
		}
		(void) fclose(dictfp);
		dictfp = (FILE *) NULL;
#endif
		if (loadindex(DICTINDEX) < 0) {
			(void) fprintf(stderr, "Can't load %s\n", DICTINDEX);
			cleanup();
			exit(1);
		}

		prompt("Type <space> to begin...");
		while (inputch() != ' ')
			;

		done = 0;
		while (!done) {
			newgame(bspec);
			bspec = (char *) NULL;		/* reset for subsequent games */
			playgame();
			prompt("Type <space> to continue, any cap to quit...");
			delay(50);					/* wait for user to quit typing */
			flushin(stdin);
			while (1) {
				int ch;

				ch = inputch();
				if (ch == '\033')
					findword();
				else if (ch == '\014' || ch == '\022')	/* ^l or ^r */
					redraw();
				else {
					if (isupper(ch)) {
						done = 1;
						break;
					}
					if (ch == ' ')
						break;
				}
			}
		}
		cleanup();
	}
	exit(0);
}

/*
 * Read a line from the given stream and check if it is legal
 * Return a pointer to a legal word or a null pointer when EOF is reached
 */
char *
batchword(fp)
FILE *fp;
{
	register int *p, *q;
	register char *w;
	char *nextword();

	q = &wordpath[MAXWORDLEN + 1];
	p = wordpath;
	while (p < q)
		*p++ = -1;
	while ((w = nextword(fp)) != (char *) NULL) {
		if (wordlen < minlength)
			continue;
		p = wordpath;
		while (p < q && *p != -1)
			*p++ = -1;
		usedbits = 0;
		if (checkword(w, -1, wordpath) != -1)
			return(w);
	}
	return((char *) NULL);
}

/*
 * Play a single game
 * Reset the word lists from last game
 * Keep track of the running stats
 */
playgame()
{
	/* Can't use register variables if setjmp() is used! */
	int i, *p, *q;
	long t;
	char buf[MAXWORDLEN + 1];
	int compar();

	ngames++;
	npwords = 0;
	pwordsp = pwords;
	nmwords = 0;
	mwordsp = mwords;

	time(&start_t);

	q = &wordpath[MAXWORDLEN + 1];
	p = wordpath;
	while (p < q)
		*p++ = -1;
	showboard(board);
	startwords();
	if (setjmp(env)) {
		badword();
		goto timesup;
	}

	while (1) {
		if (getline(buf) == (char *) NULL) {
			if (feof(stdin))
				clearerr(stdin);
			break;
		}
		time(&t);
		if (t - start_t >= tlimit) {
			badword();
			break;
		}
		if (buf[0] == '\0') {
			int remaining;

			remaining = tlimit - (int) (t - start_t);
			(void) sprintf(buf, "%d:%02d", remaining / 60, remaining % 60);
			showstr(buf, 1);
			continue;
		}
		if (strlen(buf) < minlength) {
			badword();
			continue;
		}

		p = wordpath;
		while (p < q && *p != -1)
			*p++ = -1;
		usedbits = 0;

		if (checkword(buf, -1, wordpath) < 0)
			badword();
		else {
			if (debug) {
				(void) printf("[");
				for (i = 0; wordpath[i] != -1; i++)
					(void) printf(" %d", wordpath[i]);
				(void) printf(" ]\n");
			}
			for (i = 0; i < npwords; i++) {
				if (strcmp(pword[i], buf) == 0)
					break;
			}
			if (i != npwords) {			/* already used the word */
				badword();
				showword(i);
			}
			else if (!validword(buf))
				badword();
			else {
				int len;

				len = strlen(buf) + 1;
				if (npwords == MAXPWORDS - 1 ||
						pwordsp + len >= &pwords[MAXPSPACE]) {
					(void) fprintf(stderr, "Too many words!\n");
					cleanup();
					exit(1);
				}
				pword[npwords++] = pwordsp;
				(void) strcpy(pwordsp, buf);
				pwordsp += len;
				addword(buf);
			}
		}
	}

timesup: ;

	/*
	 * Sort the player's words and terminate the list with a null
	 * entry to help out checkdict()
	 */
	qsort(pword, npwords, sizeof(pword[0]), compar);
	pword[npwords] = (char *) NULL;

	/*
	 * These words don't need to be sorted since the dictionary is sorted
	 */
	checkdict();

	tnmwords += nmwords;
	tnpwords += npwords;

	results();
}

/*
 * Check if the given word is present on the board, with the constraint
 * that the first letter of the word is adjacent to square 'prev'
 * Keep track of the current path of squares for the word
 * A 'q' must be followed by a 'u'
 * Words must end with a null
 * Return 1 on success, -1 on failure
 */
checkword(word, prev, path)
char *word;
int prev, *path;
{
	register char *p, *q;
	register int i, *lm;

	if (debug) {
		(void) printf("checkword(%s, %d, [", word, prev);
			for (i = 0; wordpath[i] != -1; i++)
				(void) printf(" %d", wordpath[i]);
			(void) printf(" ]\n");
	}

	if (*word == '\0')
		return(1);

	lm = letter_map[*word - 'a'];

	if (prev == -1) {
		char subword[MAXWORDLEN + 1];

		/*
		 * Check for letters not appearing in the cube to eliminate some
		 * recursive calls
		 * Fold 'qu' into 'q'
		 */
		p = word;
		q = subword;
		while (*p != '\0') {
			if (*letter_map[*p - 'a'] == -1)
				return(-1);
			*q++ = *p;
			if (*p++ == 'q') {
				if (*p++ != 'u')
					return(-1);
			}
		}
		*q = '\0';
		while (*lm != -1) {
			*path = *lm;
			usedbits |= (1 << *lm);
			if (checkword(subword + 1, *lm, path + 1) > 0)
				return(1);
			usedbits &= ~(1 << *lm);
			lm++;
		}
		return(-1);
	}

	/*
	 * A cube is only adjacent to itself in the adjacency matrix if selfuse
	 * was set, so a cube can't be used twice in succession if only the reuse
	 * flag is set
	 */
	for (i = 0; lm[i] != -1; i++) {
		if (adjacency[prev][lm[i]]) {
			int used;

			used = 1 << lm[i];
			/* If necessary, check if the square has already been used */
			if (!reuse && (usedbits & used))
					continue;
			*path = lm[i];
			usedbits |= used;
			if (checkword(word + 1, lm[i], path + 1) > 0)
				return(1);
			usedbits &= ~used;
		}
	}
	*path = -1;		/* in case of a backtrack */
	return(-1);
}

/*
 * A word is invalid if it is not in the dictionary
 * At this point it is already known that the word can be formed from
 * the current board
 */
validword(word)
char *word;
{
	register int j;
	register char *q, *w;
	char *nextword();

	j = word[0] - 'a';
	if (dictseek(dictfp, dictindex[j].start, 0) < 0) {
		(void) fprintf(stderr, "Seek error\n");
		cleanup();
		exit(1);
	}

	while ((w = nextword(dictfp)) != (char *) NULL) {
		int ch;

		if (*w != word[0])	/* end of words starting with word[0] */
			break;
		q = word;
		while ((ch = *w++) == *q++ && ch != '\0')
			;
		if (*(w - 1) == '\0' && *(q - 1) == '\0')
			return(1);
	}
	if (dictfp != (FILE *) NULL && feof(dictfp))	/* Special case for z's */
		clearerr(dictfp);
	return(0);
}

/*
 * Check each word in the dictionary against the board
 * Delete words from the machine list that the player has found
 * Assume both the dictionary and the player's words are already sorted
 */
checkdict()
{
	register char *p, **pw, *w;
	register int i;
	int prevch, previndex, *pi, *qi, st;

	mwordsp = mwords;
	nmwords = 0;
	pw = pword;
	prevch ='a';
	qi = &wordpath[MAXWORDLEN + 1];

	(void) dictseek(dictfp, 0L, 0);
	while ((w = nextword(dictfp)) != (char *) NULL) {
		if (wordlen < minlength)
			continue;
		if (*w != prevch) {
			/*
			 * If we've moved on to a word with a different first letter
			 * then we can speed things up by skipping all words starting
			 * with a letter that doesn't appear in the cube
			 */
			i = (int) (*w - 'a');
			while (i < 26 && letter_map[i][0] == -1)
				i++;
			if (i == 26)
				break;
			previndex = prevch - 'a';
			prevch = i + 'a';
			/*
			 * Fall through if the word's first letter appears in the cube
			 * (i.e., if we can't skip ahead), otherwise seek to the
			 * beginning of words in the dictionary starting with the
			 * next letter (alphabetically) appearing in the cube and then
			 * read the first word
			 */
			if (i != previndex + 1) {
				if (dictseek(dictfp, dictindex[i].start, 0) < 0) {
					(void) fprintf(stderr, "Seek error in checkdict()\n");
					cleanup();
					exit(1);
				}
				continue;
			}
		}

		pi = wordpath;
		while (pi < qi && *pi != -1)
			*pi++ = -1;
		usedbits = 0;
		if (checkword(w, -1, wordpath) == -1)
			continue;

		st = 1;
		while (*pw != (char *) NULL && (st = strcmp(*pw, w)) < 0)
			pw++;
		if (st == 0)			/* found it */
			continue;
		if (nmwords == MAXMWORDS ||
					mwordsp + wordlen + 1 >= &mwords[MAXMSPACE]) {
			(void) fprintf(stderr, "Too many words!\n");
			cleanup();
			exit(1);
		}
		mword[nmwords++] = mwordsp;
		p = w;
		while (*mwordsp++ = *p++)
			;
	}
}

/*
 * Crank up a new game
 * If the argument is non-null then it is assumed to be a legal board spec
 * in ascending cube order, oth. make a random board
 */
newgame(b)
char *b;
{
	register int i, p, q;
	char *tmp;
	int *lm[26];
	static char *cubes[16] = {
		"ednosw", "aaciot", "acelrs", "ehinps",
		"eefhiy", "elpstu", "acdemp", "gilruw",
		"egkluy", "ahmors", "abilty", "adenvz",
		"bfiorx", "dknotu", "abjmoq", "egintv"
	};

	if (b == (char *) NULL) {
		/*
		 * Shake the cubes and make the board
		 */
		i = 0;
		while (i < 100) {
			p = (int) (random() % 16);
			q = (int) (random() % 16);
			if (p != q) {
				tmp = cubes[p];
				cubes[p] = cubes[q];
				cubes[q] = tmp;
				i++;
			}
			/* else try again */
		}

		for (i = 0; i < 16; i++)
			board[i] = cubes[i][random() % 6];
	}
	else {
		for (i = 0; i < 16; i++)
			board[i] = b[i];
	}
	board[16] = '\0';

	/*
	 * Set up the map from letter to location(s)
	 * Each list is terminated by a -1 entry
	 */
	for (i = 0; i < 26; i++) {
		lm[i] = letter_map[i];
		*lm[i] = -1;
	}

	for (i = 0; i < 16; i++) {
		register int j;

		j = (int) (board[i] - 'a');
		*lm[j] = i;
		*(++lm[j]) = -1;
	}

	if (debug) {
		for (i = 0; i < 26; i++) {
			int ch, j;

			(void) printf("%c:", 'a' + i);
			for (j = 0; (ch = letter_map[i][j]) != -1; j++)
				(void) printf(" %d", ch);
			(void) printf("\n");
		}
	}

}

compar(p, q)
char **p, **q;
{
	return(strcmp(*p, *q));
}

usage()
{
(void) fprintf(stderr,
"Usage: bog [-b] [-d] [-s#] [-t#] [-w#] [+[+]] [boardspec]\n");
(void) fprintf(stderr, "-b: 'batch mode' (boardspec must be present)\n");
(void) fprintf(stderr, "-d: debug\n");
(void) fprintf(stderr, "-s#: use # as the random number seed\n");
(void) fprintf(stderr, "-t#: time limit is # seconds\n");
(void) fprintf(stderr, "-w#: minimum word length is # letters\n");
(void) fprintf(stderr, "+: can reuse a cube, but not twice in succession\n");
(void) fprintf(stderr, "++: can reuse cubes arbitrarily\n");
(void) fprintf(stderr, "boardspec: the first board to use (use 'q' for 'qu')\n");
	exit(1);
}

