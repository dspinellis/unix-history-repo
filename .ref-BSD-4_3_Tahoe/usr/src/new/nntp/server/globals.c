#ifndef lint
static char	*sccsid = "@(#)globals.c	1.4	(Berkeley) 7/17/87";
#endif

/*
 * Common variables.
 */

#include "common.h"

/*
 * Variables initialized from ../common/conf.h
 */

char	spooldir[] = SPOOLDIR;
char	activefile[] = ACTIVE_FILE;
char	accessfile[] = ACCESS_FILE;
char	historyfile[] = HISTORY_FILE;
char	ngdatefile[] = NGDATE_FILE;
char	inews[] = INEWS;
char	rnews[] = RNEWS;

/*
 * Other random externals.
 */

char	**group_array;
int	num_groups;
int	ingroup = 0;
int	art_ptr;
int	num_arts;
int	art_array[MAX_ARTICLES];
FILE	*art_fp;
int	uid_poster, gid_poster;
int	canpost, canread, canxfer;
char	**ngpermlist;
int	ngpermcount;

#ifdef LOG
int	arts_acsd;
int	grps_acsd;
#endif
