#ifndef lint
static char *rcsid_bitblt_aed_c = "$Header: bitblt_aed.c,v 10.1 86/11/19 10:50:45 jg Exp $";
#endif	lint
/* aed.c - AED display dependent routines
 *
 *	aed_echo_rect		Echos rectangular areas of shared memory to AED
 *	aed_screen_copy		Does screen to screen copy
 *	aed_draw_line		Draws lines directly on screen
 *	UnionOfRects		Finds the union of two rectangles
 *	AreaOfRect	`	Computes the area of a rectangle
 *
 *  	Author:
 *		Scott Bates
 *		Brown University
 *		IRIS, Box 1946
 *      	Providence, RI 02912
 *
 *
 *		Copyright (c) 1986 Brown University
 *
 * Permission to use, copy, modify and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, and that both
 * that copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Brown University not be used in
 * advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission. Brown University makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as-is" without express or implied warranty.
 */

#include "bitblt_int.h"
#include "bitblt_aed.h"

/*
 * Build echo rect command and place it on
 * on command queue
 */

aed_echo_rect(new_area)
	register Blt_Rectangle *new_area;
{
        register i;
        Blt_Rectangle merge_area;
	register struct aed_cmd *cmd_queue = &AED_CMDQ[0];

        AED_CMDQ_SEMA = 1;

	for (i = AED_CMDQ_INDEX - 1; i >= 0; i--) {
	    if (cmd_queue[i].cmd == AED_ECHO_RECT) {
                UnionOfRects(new_area,(Blt_Rectangle *)&cmd_queue[i].ECHO_RECT,
			     &merge_area);
                if (AreaOfRect(&merge_area) <= (AreaOfRect(new_area) +
		    AreaOfRect((Blt_Rectangle *) &cmd_queue[i].ECHO_RECT)) * 2)
		{
        		cmd_queue[i].ECHO_RECT = 
				*((struct aed_echo_rect *) &merge_area);
                       	goto reset_sema;
                }
	    } else {
		break;
	    }
        }

        while (AED_CMDQ_INDEX == AED_MAX_CMDS) {
        	AED_CMDQ_SEMA = 0;
        }
        AED_CMDQ_SEMA = 1;

	cmd_queue[AED_CMDQ_INDEX].cmd = AED_ECHO_RECT;
	cmd_queue[AED_CMDQ_INDEX].ECHO_RECT =
			*((struct aed_echo_rect *) new_area);
	AED_CMDQ_INDEX++;

reset_sema:
        AED_CMDQ_SEMA = 0;
}

/*
 * Build screen to screen copy command and 
 * place it on command queue
 */

aed_screen_copy(from_x, from_y, to_x, to_y, width, height, rule)
	short from_x, from_y, to_x, to_y, width, height, rule;
{
	register struct aed_cmd *cmd_queue = &AED_CMDQ[0];
	register struct aed_screen_copy *cmd;
	register i;

	if (AED_CMDQ_INDEX != 0) {
		for (i = 0; i < AED_CMDQ_INDEX; i++) {
			if (cmd_queue[i].cmd == AED_ECHO_RECT) {
				while (AED_CMDQ_INDEX != 0);
				break;
			}
		}
		while (AED_CMDQ_INDEX == AED_MAX_CMDS);
	}

	AED_CMDQ_SEMA = 1;

	cmd_queue[AED_CMDQ_INDEX].cmd = AED_SCREEN_COPY;
	cmd = &cmd_queue[AED_CMDQ_INDEX].SCREEN_COPY;
	cmd->from_x = from_x;
	cmd->from_y = from_y;
	cmd->to_x = to_x;
	cmd->to_y = to_y;
	cmd->width = width;
	cmd->height = height;
	cmd->rule = AED_RULE(rule);
	AED_CMDQ_INDEX++;

	AED_CMDQ_SEMA = 0;
}

/*
 * Build command to draw a line and place
 * it on command queue
 */

aed_draw_line(from_x, from_y, to_x, to_y, rule, width, color, pat, patlen, clip)
	short from_x, from_y, to_x, to_y;
	short rule, width, color;
	u_short pat;
	register short patlen;
	Blt_Rectangle *clip;
{
	register struct aed_cmd *cmd_queue = &AED_CMDQ[0];
	register struct aed_draw_line *cmd;
	register u_short aed_pattern;
	register i;

	if(patlen > 0 && patlen < 16) {
		
		/*
		 * pattern must be left justified for AED microcode
		 */

		aed_pattern = (pat <<= (16 - patlen));

		/*
		 * make all patterns 16 bits long.
		 * This is to get around microcode bug
		 * (bug happens when pattern length < 16)
		 */

		for(i = patlen; i < 16; i += patlen)
			pat |= (aed_pattern >> i);
		patlen = 16;
	}

	if (AED_CMDQ_INDEX != 0) {
		for (i = 0; i < AED_CMDQ_INDEX; i++) {
			if (cmd_queue[i].cmd == AED_ECHO_RECT) {
				while (AED_CMDQ_INDEX != 0);
				break;
			}
		}
		while (AED_CMDQ_INDEX == AED_MAX_CMDS);
	}

	AED_CMDQ_SEMA = 1;

	cmd_queue[AED_CMDQ_INDEX].cmd = AED_DRAW_LINE;
	cmd = &cmd_queue[AED_CMDQ_INDEX].DRAW_LINE;
	cmd->from_x = from_x;
	cmd->from_y = from_y;
	cmd->to_x = to_x;
	cmd->to_y = to_y;
	cmd->rule = AED_RULE(rule);
	cmd->width = width;
	cmd->color = color;
	cmd->pat = pat;
	cmd->patlen = patlen;
	cmd->top = clip->origin_y;
	cmd->left = clip->origin_x;
	cmd->bottom = clip->corner_y - 1;
	cmd->right = clip->corner_x - 1;
	AED_CMDQ_INDEX++;

	AED_CMDQ_SEMA = 0;
}

/*
 * Find union to two rectangles
 */

static
UnionOfRects(rectA,rectB,dstrect)
	register Blt_Rectangle *rectA, *rectB, *dstrect;
{
	if (rectA->origin_y >= rectA->corner_y ||
	    rectA->origin_x >= rectA->corner_x)
		*dstrect = *rectB;
	else if (rectB->origin_y >= rectB->corner_y ||
	    rectB->origin_x >= rectB->corner_x)
		*dstrect = *rectA;
	else {
		dstrect->origin_y = MIN(rectA->origin_y, rectB->origin_y);
		dstrect->corner_y = MAX(rectA->corner_y, rectB->corner_y);
		dstrect->origin_x = MIN(rectA->origin_x, rectB->origin_x);
		dstrect->corner_x = MAX(rectA->corner_x, rectB->corner_x);
	}
}

/*
 * Compute area of a rectangle
 */

static
AreaOfRect(r)
	register Blt_Rectangle *r;
{
        return ((r->corner_x - r->origin_x) * (r->corner_y - r->origin_y));
}
