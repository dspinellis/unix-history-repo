/*
 * Copyright (c) 1987 by Ed James, UC Berkeley.  All rights reserved.
 *
 * Copy permission is hereby granted provided that this notice is
 * retained on all partial or complete copies.
 *
 * For more info on this and all of my stuff, mail edjames@berkeley.edu.
 */

#include "include.h"

char		GAMES[] =	"Game_List",
		LOG[] =		"ATC_scores";

int		clock, safe_planes, start_time, test_mode;

char		*file;

FILE		*filein, *fileout;

C_SCREEN		screen, *sp = &screen;

LIST		air, ground;

struct sgttyb	tty_start, tty_new;

DISPLACEMENT	displacement[MAXDIR] = {
		{  0, -1 },
		{  1, -1 },
		{  1,  0 },
		{  1,  1 },
		{  0,  1 },
		{ -1,  1 },
		{ -1,  0 },
		{ -1, -1 }
};
