/*
 * Copyright (c) 1987 by Ed James, UC Berkeley.  All rights reserved.
 *
 * Copy permission is hereby granted provided that this notice is
 * retained on all partial or complete copies.
 *
 * For more info on this and all of my stuff, mail edjames@berkeley.edu.
 */

/*
 * SPECIAL_DIR - Where the special files are kept.
 * Change this to whatever you want.  Be sure to have to path end 
 * with a '/', like "/usr/games/lib/atc/".
 */

char	SPECIAL_DIR[] =		DEST;

/*
 * NUM_SCORES - Number of scores that are kept track of.
 * Keep this greater than 0, but less than 100.
 * 4 lines are printed above the score, one below + your prompt, so
 * to prevent scrolling: 6 + NUM_SCORES <= 24 (lines on an average terminal).
 */
int	NUM_SCORES =		18;
