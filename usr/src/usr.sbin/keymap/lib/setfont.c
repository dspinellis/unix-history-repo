/*
 * Contributed to 386bsd 0.1 and later versions
 *
 *	Copyright 1992 by Holger Veit
 *	May be freely used with Bill Jolitz's port of 
 *	386bsd and may be included in a 386bsd collection
 *	as long as binary and source are available and reproduce the above
 *	copyright.
 *
 *	You may freely modify this code and contribute improvements based
 *	on this code as long as you don't claim to be the original author.
 *	Commercial use of this source requires permittance of the copyright 
 *	holder. A general license for 386bsd will override this restriction.
 *
 *	Use at your own risk. The copyright holder or any person who makes
 *	this code available for the public (administrators of public archives
 *	for instance) are not responsible for any harm to hardware or software
 *	that might happen due to wrong application or program faults.
 *
 * You must have the codriver driver in the same package generated
 * into the 386bsd kernel, otherwise this program does not work.
 *
 *	@(#)setfont.c	1.1 (386bsd contribution) 01/10/93
 */
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/ioctl_pc.h>
#include "pathnames.h"

static char *getparam();

#define ISOFONT	0
#define PC8FONT	0xFFFE

/* setfont returns the following codes:
 * 0:  		ok
 * -1:		error
 *
 * ENOENT:	cant open font file
 * EFTYPE:	invalid file or improper entry
 * EINVAL:	valid parameter, but cannot be accepted
 *		e.g. a 100x100 pixel font
 */
setfont(char *path,int verbose,int fpage)
{
	FILE		*fd;

	int		registry;
	int		font_defchar;
	char		line[133];	/* this size is traditional */
	char		*token,*param;
	int		incdef=0,inpixdef=0;
	int		i;
	char		*defflags;
	struct fmap	fmap;
	struct fchar	*fchar,*defchar;
	int		count;
	int		fv;
	int		which;

	/* find console type */
	which = whichcons();
	if (which != 4) {
		if (verbose) printf("setfont: no kernel support\n");
		errno = ENOENT;
		return -1;
	}
	
	/* initialize fontmap structure */
	fmap.page = fpage;
	fmap.nr	  = -1;
	fmap.x    = -1;
	fmap.y    = -1;
	fmap.start= 0;
	fmap.fntmap = 0;

	/* initialize parameters */
	registry= 
	font_defchar = -1;

	/* open font file */
	if ((fd = fopen(path,"r"))==NULL) {
		if (verbose) 
			fprintf(stderr,"setfont: cannot open file %s\n", path);
		return -1;
	}
	
	while (fgets(line,132,fd) != NULL) {

		/* process line */
		token = strtok(line," \n\t");

		if (!incdef) {
			if (!strcasecmp(token,"CHARS")) {
				param = strtok(NULL," \n\t");
				fmap.nr = atoi(param);
				fmap.fntmap = (struct fchar*)malloc(
					fmap.nr*sizeof(struct fchar));
				defflags = malloc(fmap.nr);
				fchar = fmap.fntmap;
				if (!fmap.fntmap || !defflags) {
					if (verbose)
						printf("setfont: malloc error\n");
					errno = ENOMEM;
					return -1;
				}
				continue;
			}
			if (!strcasecmp(token,"STARTFONT")) continue;
			if (!strcasecmp(token,"COMMENT")) continue;
			if (!strcasecmp(token,"FONT")) continue;
			if (!strcasecmp(token,"SIZE")) {
				param = strtok(NULL," \n\t");
				if (atoi(param) > VGA_MAXY) goto einval;
				continue;
			}
			if (!strcasecmp(token,"FONTBOUNDINGBOX")) {
				param = strtok(NULL," \n\t");
				fmap.x = atoi(param);
				param = strtok(NULL," \n\t");
				fmap.y = atoi(param);
				if (fmap.x > VGA_MAXX || fmap.y > VGA_MAXY)
					goto einval;
				continue;
			}
			if (!strcasecmp(token,"STARTPROPERTIES")) continue;
			if (!strcasecmp(token,"FONTNAME_REGISTRY")) continue;
			if (!strcasecmp(token,"FOUNDRY")) continue;
			if (!strcasecmp(token,"FAMILY_NAME")) {
				param = getparam();
				if (strcasecmp(param,"fixed"))
					goto einval;
				continue;
			}
			if (!strcasecmp(token,"WEIGHT_NAME")) continue;
			if (!strcasecmp(token,"SLANT")) continue;
			if (!strcasecmp(token,"SETWIDTH_NAME")) continue;
			if (!strcasecmp(token,"ADD_STYLE_NAME")) continue;
			if (!strcasecmp(token,"PIXEL_SIZE")) {
				param = strtok(NULL," \n\t");
				if (atoi(param) > VGA_MAXX) goto einval;
				continue;
			}
			if (!strcasecmp(token,"POINT_SIZE")) continue;
			if (!strcasecmp(token,"RESOLUTION_X")) continue;
			if (!strcasecmp(token,"RESOLUTION_Y")) continue;
			if (!strcasecmp(token,"SPACING")) continue;
			if (!strcasecmp(token,"AVERAGE_WIDTH")) continue;
			if (!strcasecmp(token,"CHARSET_REGISTRY")) {
				param = getparam();
				if (!strcasecmp(param,"ISO8859"))
					registry = ISOFONT;
				else if (!strcasecmp(param,"IBMPC8"))
					registry = PC8FONT;
				else
					goto einval;
				continue;
			}
			if (!strcasecmp(token,"CHARSET_ENCODING")) continue;
			if (!strcasecmp(token,"DEFAULT_CHAR")) {
				param = strtok(NULL," \n\t");
				font_defchar = atoi(param);
				continue;
			}
			if (!strcasecmp(token,"FONT_DESCENT")) continue;
			if (!strcasecmp(token,"FONT_ASCENT")) continue;
			if (!strcasecmp(token,"COPYRIGHT")) continue;
			if (!strcasecmp(token,"ENDPROPERTIES")) continue;
			if (!strcasecmp(token,"STARTCHAR")) {
				if (!fmap.fntmap) goto einval;
				incdef = 1;
				continue;
			}
		} 
		else if (!inpixdef) {
			/* character definition */
			if (!strcasecmp(token,"COMMENT")) continue;
			if (!strcasecmp(token,"BBX")) continue;
			if (!strcasecmp(token,"ENDFONT")) break;
			if (!strcasecmp(token,"STARTCHAR")) {
				fchar++;
				continue;
			}
			if (!strcasecmp(token,"ENCODING")) {
				param = strtok(NULL," \n\t");
				fchar->encoding = atoi(param);
				defflags[fchar->encoding] = 1;
				continue;
			}
			if (!strcasecmp(token,"SWIDTH")) continue;
			if (!strcasecmp(token,"DWIDTH")) continue;
			if (!strcasecmp(token,"BBX")) continue;
			if (!strcasecmp(token,"BITMAP")) {
				inpixdef = 1;
				count = 0;
				continue;
			}
		} else
		{
			if (!strcasecmp(token,"COMMENT")) continue;
			if (!strcasecmp(token,"ENDCHAR")) {
				inpixdef = 0;
				continue;
			}
			/* in bitmap definition
			 * XXX accept 2 hex digits maximum
			 * change with GFX console
			 */
			token[2]=0;
			fchar->map[count++] = strtol(token,NULL,16);
			continue;
		}
		if (verbose) {
			fprintf(stderr,"setfont: invalid string %s\n",token);
		}
		errno = EFTYPE;
		return -1;
	}

	/* end font */
	fclose(fd);

	/* fill default positions */
	if (font_defchar >= 0) {
		defchar = fmap.fntmap+font_defchar;
		for (i=0; i<fmap.nr; i++) {
			if (defflags[i]==0)
				bcopy(defchar,fmap.fntmap+i,sizeof(struct fchar));
		}
	}

	/* some hack */
	fmap.start = registry;

	/* set ioctl */
	if ((fv=open(_PATH_VIDEO,0)) <= 0) {
		if (verbose) perror("setfont: open video device");
		return -1;
	}

	if (ioctl(fv,VGASFONTMAP,&fmap)<0) {
		if (verbose) perror("setfont: load font");
		return -1;
	}
	close(fv);
	return 0;

einval:
	if (verbose) {
		fprintf(stderr,"setfont: invalid parameter in %s\n",
			token);
	}
	fclose(fd);

	errno = EINVAL;
	return -1;
}

static char *getparam() 
{
	char *s,*param = strtok(NULL," \t\n");
	if (param && param[0]=='\"') {
		for (s= ++param; *s; s++)
			if (*s=='\"') { *s=0; break; }
	}
	return param;
}
