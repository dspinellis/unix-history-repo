/******************************************************************************
 *  font - 	manages the fonts and their associated descriptor files for
 *		processing ditroff commands into interpress
 *
 *
 *	William LeFebvre (Xerox Corp)
 *	with modifications by John Mellor-Crummey (Xerox Corp)
 *
 * 	Copyright (c) 1984, 1985, 1986 Xerox Corp.
 *
 *
 * HISTORY
 * 15-Jan-86  lee at Xerox, WRC
 *	Fixed NULL pointer de-reference.
 *
 *
 ******************************************************************************/

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>


#include "deviceinfo.h"	/* typesetter characteristics */
#include "defs.h"	/* constant and macro definitions */
#include "externs.h"	/* declarations for global variables */


/*-----------------------------------------------------------------------------
 *	readDeviceInitInfo	read the device descriptor and font 
 *				initialization info into tables in memory
 *				from a typesetter descriptor file created using 
 *				by makedev
 *---------------------------------------------------------------------------*/
readDeviceInitInfo()
{
	int devicefd,entries,fontno;
	char deviceFilename[80],*buffer,*ptr;

	/* read in device descriptor , fonts character and width info */

	/* form descriptor file name */
	(void) sprintf(deviceFilename, "%s/dev%s/DESC.out", fontdirectory, devicename);

	if ((devicefd = open(deviceFilename,O_RDONLY,0)) < 0)
		reportError(QUIT, "can't open file %s for device characteristics (%s)",
			deviceFilename, sys_errlist[errno]);

	/* read in device characteristics */
	(void) read(devicefd, &device, sizeof(struct device_entry));

	/* allocate space to read in the rest of 
	 * the tables in the descriptor file 
	 */
	buffer = malloc(device.font_size);	
	(void) read(devicefd, buffer, device.font_size);	

	/* set up global pointers to tables */
	pointsizeTab = (short *) buffer;
	specCharTab = (short *) pointsizeTab + device.num_sizes + 1;
	specCharStrTab = (char *) (specCharTab + device.spec_char_num);
	
	/* walk through the rest of the fonts */
	ptr = specCharStrTab + device.spec_name_len;
	for (fontno = 1; fontno <= device.num_fonts; fontno++) 
	{
		fontPtr[fontno] = (struct font_entry *) ptr;
		entries = (unsigned char)(*ptr) & 255;	/* get width entries */
		if (specFontPos == 0 && fontPtr[fontno]->special_flag == 1)
			specFontPos = fontno;	/* position of 1st special font */
		ptr += sizeof(struct font_entry);	/* that's what's on the beginning */
		charWidthTab[fontno] = (unsigned char *) ptr;
		/* ignore kerning info */
		ptr += entries * 2;
		charCodeTab[fontno] =  ptr;
		ptr += entries;
		fontIndexTab[fontno] = (unsigned char *) ptr;
		ptr += device.spec_char_num + 128 - 32;
		setFontPosition(fontno, fontPtr[fontno]->font_name, fontPtr[fontno]->font_number);
	}
	fontPtr[0] = (struct font_entry *) malloc(3*255 + device.spec_char_num + (128-32) + sizeof (struct font_entry));
	charWidthTab[0] = (unsigned char *) fontPtr[0] + sizeof (struct font_entry);
	fontPtr[0]->num_char_wid = 255;
	if (dbg && device.num_stiptypes > 0)
		fprintf(stderr,"%d stipple families: ", device.num_stiptypes);
	for (fontno = 1; fontno <= device.num_stiptypes; fontno++) {
		stipTypeName[fontno] = ptr;
		if (dbg)
			fprintf(stderr,"%s ", ptr);
		while(*ptr++); /* advance to next one */
		if (dbg && fontno == device.num_stiptypes) putc('\n',stderr);
	}
	(void) close(devicefd);
}

/*-----------------------------------------------------------------------------
 *	loadFont	load the descriptor and tables for a font into position
 *			n in the font table
 *---------------------------------------------------------------------------*/
loadFont(fontno, fname)
int fontno;
char *fname;
{
	int fontsfd, entries;
	int num_widths;
	char fontfilename[60];

	if (fontno < 0 || fontno > MAX_NUM_FONTS)
		reportError(QUIT, "load fail for font %s, index out of bounds %d",fname,fontno);
	if (strcmp(fname, fontPtr[fontno]->font_name) == 0) return; /* already loaded */

	(void) sprintf(fontfilename, "%s/dev%s/%s.out", fontdirectory, devicename, fname);
	if ((fontsfd = open(fontfilename,O_RDONLY,0)) < 0)
		reportError(QUIT, "can't open font file %s (%s)",fontfilename, sys_errlist[errno]);
	/* record the current length of this font table */
	entries = (unsigned char)(fontPtr[fontno]->num_char_wid) & 255;

	/* read in the new font */
	(void) read(fontsfd, fontPtr[fontno], 3*entries + device.spec_char_num+128-32 + sizeof(struct font_entry));
	(void) close(fontsfd);

	if (((unsigned char)(fontPtr[fontno]->num_char_wid) & 255) > entries)
		reportError(QUIT, "Font %s too big for position %d\n", fname, fontno);

	num_widths = (unsigned char)(fontPtr[fontno]->num_char_wid) & 255; /* width of new font tables */
	charWidthTab[fontno] = (unsigned char *) fontPtr[fontno] + sizeof(struct font_entry);
	charCodeTab[fontno] = (char *) charWidthTab[fontno] + 2 * num_widths;
	fontIndexTab[fontno] = (unsigned char *) charWidthTab[fontno] + 3 * num_widths;

	setFontPosition(fontno, fontPtr[fontno]->font_name, fontPtr[fontno]->font_number);

	/* restore to old value to allow use of the maximum space later */
	fontPtr[fontno]->num_char_wid = entries;	
}


/*-----------------------------------------------------------------------------
 *	setFontPosition		set up a font with troff name *name in the 
 *				position given by index of the current fonts 
 *				table
 *---------------------------------------------------------------------------*/
setFontPosition(index, name, iname)
int index;			/* index into fontPtr */
char *name;			/* troff name */
char *iname;			/* internal name */

{
    int  cnt;
    char **trp;
    register struct ifont *ifp;

    if (dbg) printf("setFontPosition(%d, %s, %s)\n", index, name, iname);

    /* do nothing if it is the same font */
    if (currfonts[index] != NULL  && strcmp(name, currfonts[index]->name) == 0)
    {
	if (dbg) printf("trivial case -- no action taken\n");
	return;
    }

    /* place any old and used font on the inactive list if not already there */
    ifp = currfonts[index];
    if (ifp != NULL && ifp->frames != NULL && ifp->next == NULL)
    {
	if (dbg) printf("placing old font (%s) on inactive list\n", ifp->name);
	ifp->next = inactfonts;
	inactfonts = ifp;
    }

    /* try to find the new font on the inactive list */
    if (dbg) printf("checking inactive list: ");
    for (ifp = inactfonts; ifp != NULL; ifp = ifp->next)
    {
	if (dbg) printf("%s  ", ifp->name);
	if (strcmp(ifp->name, name) == 0)
		break;
    }

    if (ifp != NULL)
    {
	if (dbg) printf("\ngot it!\n");
	/* it was on the inactive list -- pull it back */
	currfonts[index] = ifp;
    }
    else
    {
	/* brand new font */
	if (dbg) printf("\nnot there ... making new font\ninterpress equiv: ");
	currfonts[index] = ifp = (struct ifont *)malloc(sizeof(struct ifont));
	strcpy(ifp->name, name);
	ifp->frames = NULL;
	ifp->extab  = NULL;
	ifp->next   = NULL;

	/* map the troff name to an interpress name */
	for (cnt = 0, trp = trname; cnt < mapcnt; cnt++, trp++)
	{
	    if (dbg) printf("%s  ", *trp);
	    if (strcmp(*trp, name) == 0)
		break;
	}
	if (cnt == mapcnt)
	{
	    reportError(CONTINUE, "no interpress equivalent for troff font %s",
		    name);
	    currfonts[index]->uname = ipname[0];		/* punt */
	}
	else
	{
	    if (dbg) printf("\ngot it ... using %s\n", ipname[cnt]);
	    currfonts[index]->uname = ipname[cnt];
	}
    }
}
