/* $Header: bitblt_aed.h,v 10.1 86/11/19 10:51:37 jg Exp $ */
/* aed.h - Defines and macros needed to support AED
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

#define AED_BM_ADDR	0xEF100000
#define AED_WIDTH	1024
#define AED_HEIGHT	800
#define AED_BM_SIZE	(((((AED_WIDTH/8) * AED_HEIGHT) + 2047) / 2048) * 2048)
#define AED_COLOR	((int *)(AED_BM_ADDR + AED_BM_SIZE + 0))
#define AED_CMDQ_SEMA	(*((int *)(AED_BM_ADDR + AED_BM_SIZE + 4)))
#define AED_CMDQ_INDEX	(*((int *)(AED_BM_ADDR + AED_BM_SIZE + 8)))
#define AED_CMDQ	((struct aed_cmd *)(AED_BM_ADDR + AED_BM_SIZE + 128))

#define AED_CMD_SIZE	64	/* size of a command on queue */
#define AED_MAX_CMDS 	((2048 - 128) / AED_CMD_SIZE)

/*
 * Types of aed commands
 */

#define AED_ECHO_RECT	1
#define AED_DRAW_LINE	2
#define AED_SCREEN_COPY	3

/*
 * echo a rectangle from the simulated bitmap to the display.
 */

struct aed_echo_rect {
	short origin_y;
	short origin_x;
	short corner_y;
	short corner_x;
};

/*
 * draw a line
 */

struct aed_draw_line {
	short 	from_x;
	short 	from_y;
	short 	to_x;
	short 	to_y;
	short 	rule;
	short 	width;
	short 	color;
	u_short pat;
	short	patlen;
	short 	top;
	short 	left;
	short 	bottom;
	short 	right;
};

/*
 * screen to screen copy
 */

struct aed_screen_copy {
	short from_x;
	short from_y;
	short to_x;
	short to_y;
	short width;
	short height;
	short rule;
};

/*
 * AED command struct
 */

struct aed_cmd {
	long cmd;
	union cmd_block {
		struct aed_echo_rect echo_rect;
		struct aed_draw_line draw_line;
		struct aed_screen_copy screen_copy;
	} aed_cmd_block;
	char padding[AED_CMD_SIZE - sizeof(long) - sizeof(union cmd_block)];
};

#define ECHO_RECT	aed_cmd_block.echo_rect
#define SCREEN_COPY	aed_cmd_block.screen_copy
#define DRAW_LINE	aed_cmd_block.draw_line

/*
 * map bltter combination rule to AED combination rule
 */

static u_char aed_rules[16] = {
	0, 8, 4, 12, 2, 10, 6, 14,
	1, 9, 5, 13, 3, 11, 7, 15,
};

#define AED_RULE(x_rule) (((u_short)aed_rules[x_rule]) & 0x0F)
