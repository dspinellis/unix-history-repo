/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ed James.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)struct.h	5.2 (Berkeley) 4/30/90
 */

/*
 * Copyright (c) 1987 by Ed James, UC Berkeley.  All rights reserved.
 *
 * Copy permission is hereby granted provided that this notice is
 * retained on all partial or complete copies.
 *
 * For more info on this and all of my stuff, mail edjames@berkeley.edu.
 */

typedef struct {
	int	x, y;
	int	dir;	/* used only sometimes */
} SCREEN_POS;

typedef struct {
	SCREEN_POS	p1, p2;
} LINE;

typedef SCREEN_POS	EXIT;
typedef SCREEN_POS	BEACON;
typedef SCREEN_POS	AIRPORT;

typedef struct {
	int	width, height;
	int	update_secs;
	int	newplane_time;
	int	num_exits;
	int	num_lines;
	int	num_beacons;
	int	num_airports;
	EXIT	*exit;
	LINE	*line;
	BEACON	*beacon;
	AIRPORT	*airport;
} C_SCREEN;

typedef struct plane {
	struct plane	*next, *prev;
	int		status;
	int		plane_no;
	int		plane_type;
	int		orig_no;
	int		orig_type;
	int		dest_no;
	int		dest_type;
	int		altitude;
	int		new_altitude;
	int		dir;
	int		new_dir;
	int		fuel;
	int		xpos;
	int		ypos;
	int		delayd;
	int		delayd_no;
} PLANE;

typedef struct {
	PLANE	*head, *tail;
} LIST;

typedef struct {
	char	name[10];
	char	host[256];
	char	game[256];
	int	planes;
	int	time;
	int	real_time;
} SCORE;

typedef struct displacement {
	int	dx;
	int	dy;
} DISPLACEMENT;
