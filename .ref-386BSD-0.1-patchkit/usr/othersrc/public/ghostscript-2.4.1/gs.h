/* Copyright (C) 1989, 1990 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gs.h */
/* Common definitions for Ghostscript library */
#include <stdio.h>
#include "std.h"

/* Ghostscript never writes directly to stdout. */
extern FILE *gs_out;
#ifdef DEBUG
extern FILE *gs_debug_out;
#undef dstderr
#define dstderr gs_debug_out
#endif

/* Representation of a point. */
typedef struct gs_point_s {
	double x, y;
} gs_point;
typedef struct gs_int_point_s {
	int x, y;
} gs_int_point;
/* Representation of a rectangle. */
/* Note that rectangles are half-open, i.e.: their width is */
/* q.x-p.x and their height is q.y-p.y; they include the points */
/* (x,y) such that p.x<=x<q.x and p.y<=y<q.y. */
typedef struct gs_rect_s {
	gs_point p, q;			/* origin point, corner point */
} gs_rect;
typedef struct gs_int_rect_s {
	gs_int_point p, q;
} gs_int_rect;

/* So many routines use the graphics state */
/* that we may as well declare the abstract type here. */
typedef struct gs_state_s gs_state;

/* A number of interfaces involve user-specified allocation */
/* and deallocation procedures, so we define the structure here. */
typedef struct {
	proc_alloc_t alloc;
	proc_free_t free;
} gs_memory_procs;
/* We define our own versions of malloc and free that conform */
/* to the types proc_alloc_t and proc_free_t: */
char *gs_malloc(P3(uint, uint, const char *));
void gs_free(P4(char *, uint, uint, const char *));
