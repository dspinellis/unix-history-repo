/* Copyright (C) 1991 Aladdin Enterprises.  All rights reserved.
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

/* gscolor.h */
/* Client interface to color routines for Ghostscript library */

#ifndef gscolor_INCLUDED
#  define gscolor_INCLUDED

/* Color spaces */
typedef enum {
	gs_color_space_DeviceGray = 0,
	gs_color_space_DeviceRGB,
	gs_color_space_DeviceCMYK	/* not used yet */
} gs_color_space;

/* Color and gray interface */
typedef struct gs_color_s gs_color;
extern const uint gs_color_sizeof;
int	gs_setgray(P2(gs_state *, floatp));
float	gs_currentgray(P1(gs_state *));
int	gs_sethsbcolor(P4(gs_state *, floatp, floatp, floatp)),
	gs_currenthsbcolor(P2(gs_state *, float [3])),
	gs_setrgbcolor(P4(gs_state *, floatp, floatp, floatp)),
	gs_currentrgbcolor(P2(gs_state *, float [3]));
int	gs_currentcolorspace(P2(gs_state *, gs_color_space *));
/* Transfer function */
typedef float (*gs_transfer_proc)(P2(gs_state *, floatp));
int	gs_settransfer(P2(gs_state *, gs_transfer_proc)),
	gs_settransfer_remap(P3(gs_state *, gs_transfer_proc, int));
gs_transfer_proc	gs_currenttransfer(P1(gs_state *));
int	gs_setcolortransfer(P5(gs_state *, gs_transfer_proc /*red*/,
			gs_transfer_proc /*green*/, gs_transfer_proc /*blue*/,
			gs_transfer_proc /*gray*/)),
	gs_setcolortransfer_remap(P6(gs_state *, gs_transfer_proc /*red*/,
			gs_transfer_proc /*green*/, gs_transfer_proc /*blue*/,
			gs_transfer_proc /*gray*/, int));
void	gs_currentcolortransfer(P2(gs_state *, gs_transfer_proc [4]));

#endif					/* gscolor_INCLUDED */
