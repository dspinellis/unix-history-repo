#ifndef lint
static char *rcsid = "$Header: get_type.c,v 1.2 87/04/01 10:09:16 ed Exp $";
#endif lint

/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	get_type.c,v $
 * Revision 1.2  87/04/01  10:09:16  ed
 * Lots of new Viewpoint related types and how to recognize them.
 * 
 * Revision 1.1  87/01/14  11:26:06  ed
 * Initial revision
 * 
 * 
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <netns/ns.h>
#include <netns/sp.h>
#include <xnscourier/filetypes.h>
#include <xnscourier/courier.h>

#define	CHARS_TO_READ	2048

/*
 * routine:
 *	get_type
 * input:
 *	pointer to pathname of file
 *	read first CHARS_TO_READ
 *		if it contains "RasterEncoding/", assume RES (return TYPE_VPCanvas)
 *		if it contains "Interpress/Xerox/", assume IP (return TYPE_Interpress)
 *		if a char is 0 or high order bit set, assume binary (return tUNSPECIFIED)
 *		else assume text (tText)
 *	(Note order of RES/IP checking is important since RES files contain 
 *	 IP string also)
 *
 * returns:
 *	LongCardinal containing filing defined file type
 */

LongCardinal get_type(pathname)
char *pathname;
{
	FILE *file_desc;
	char buffer[CHARS_TO_READ];
	int count;
	char *ptr, *rindex();
	LongCardinal type, GetTypeAttribute();
	Boolean is_type();
	struct stat fs;

	if ( stat(pathname, &fs) == -1 )
		return(TYPE_I);		/* if error, assume tUnspecified */

	/*
	 * if not a regular file look for directory...
	 * if a directory and in root, then assume file drawer
	 */

	if ( (fs.st_mode & S_IFMT) != S_IFREG ) {
		if ( (fs.st_mode & S_IFDIR) != 0 ) {
			if ( rindex(pathname, '/') == pathname )
				return(TYPE_VPDrawer);
			else
				return(TYPE_Directory);
		} else
			return(TYPE_I);
	}

	if ( (file_desc= fopen(pathname, "r")) == NULL )
		return(TYPE_I);		/* if error, assume tUnspecified */

	if ( (count= fread(buffer,sizeof(char),CHARS_TO_READ,file_desc)) == 0 )
		type= TYPE_I;		/* if error, assume tUnspecified */
	else {
		if ( is_type(buffer, RESHDR) == TRUE ) {
			type= TYPE_VPCanvas;
		} else 	if ( is_type(buffer, INTERPRESSHDR) == TRUE ) {
			type= TYPE_Interpress;
		} else if ( is_type(buffer, VPHDR) == TRUE ) {
			type= GetTypeAttribute(file_desc);
		} else {
			type= TYPE_A;		/* assume tAsciiText */
			for ( ptr= buffer; ptr < buffer + count - 1; ) {	/* for each character */
				if ( (*ptr == 0) || (*ptr++ & 0200) ) {		/* if 0 or high order bit */
					type= TYPE_I;		/* assume tUnspecified */
					break;
				}
			}
		}
	}

	fclose(file_desc);				/* close file */
	return(type);
}


Boolean is_type(string1, string2)
char *string1, *string2;

{
	char string[128];		/* 128 should be enough for all cases */
	char *ptr;
	int i, len;

	bcopy(string1, string, 128);

	lowercasen(string,128);

	len= strlen(string2);

	/* if we don't get a match in the first 50 characters, chances are
	 * it's not Interpress or RES
	 */
	for ( ptr= string, i= 0; i < 50; ptr++, i++ ) {
		if ( bcmp(string2, ptr, len) == 0 )
			return(TRUE);
	}

	return(FALSE);
}

char *typetostring(type)
LongCardinal type;

{
	static char string[80];

	switch ( type ) {
	case  TYPE_A :
		return("text");
	case TYPE_I :
		return("binary");
	case TYPE_Directory :
		return("directory");
	case TYPE_VP :
		return("VP doc");
	case TYPE_Interpress :
		return("Interpress");
	case TYPE_VPCanvas :
		return("VP Canvas");
	case TYPE_VPDictionary :
		return("VP Dictionary");
	case TYPE_VPMailNote :
		return("VP MailNote");
	case TYPE_VPReference :
		return("VP Reference");
	case TYPE_S :
		return("serialized");
	case TYPE_VPDrawer :
		return("VP file drawer");
	case TYPE_VPApplication :
		return("VP application");
	case TYPE_VPApplication2 :
		return("VP application");
	case TYPE_VPSpreadsheet :
		return("VP spreadsheet");
	case TYPE_VPBook :
		return("VP Book");
	case TYPE_VPRecordsfile :
		return("VP RecordFile");
	case TYPE_VPCalendar :
		return("VP Calendar");
	case TYPE_VPIcons :
		return("VP Icons");
	case TYPE_Font :
		return("Printer Font");
	case TYPE_860 :
		return("860 doc");
	default :
		sprintf(string,"%d", type);
		return(string);
	}
}

LongCardinal stringtotype(string)
char *string;

{
	if ( *string == '\0' )
		return(TYPE_VPMailNote);

	lowercase(string);

	if ( strcmp(string, "text") == 0 )
		return (TYPE_A);
	if ( strcmp(string, "binary") == 0 )
		return (TYPE_I);
	if ( strcmp(string, "directory") == 0 )
		return (TYPE_Directory);
	if ( strcmp(string, "vp doc") == 0 )
		return (TYPE_VP);
	if ( strcmp(string, "interpress") == 0 )
		return (TYPE_Interpress);
	if ( strcmp(string, "vp canvas") == 0 )
		return (TYPE_VPCanvas);
	if ( strcmp(string, "vp dictionary") == 0 )
		return (TYPE_VPDictionary);
	if ( strcmp(string, "vp mailnote") == 0 )
		return (TYPE_VPMailNote);
	if ( strcmp(string, "vp reference") == 0 )
		return (TYPE_VPReference);
	if ( strcmp(string, "serialized") == 0 )
		return (TYPE_S);
	if ( strcmp(string, "vp filedrawer") == 0 )
		return (TYPE_VPDrawer);
	if ( strcmp(string, "vp application") == 0 )
		return (TYPE_VPApplication);
	if ( strcmp(string, "vp spreadsheet") == 0 )
		return (TYPE_VPSpreadsheet);
	if ( strcmp(string, "vp book") == 0 )
		return (TYPE_VPBook);
	if ( strcmp(string, "vp recordfile") == 0 )
		return (TYPE_VPRecordsfile);
	if ( strcmp(string, "vp calendar") == 0 )
		return (TYPE_VPCalendar);
	if ( strcmp(string, "vp icons") == 0 )
		return (TYPE_VPIcons);
	if ( strcmp(string, "printerfont") == 0 )
		return (TYPE_Font);
	if ( strcmp(string, "860 doc") == 0 )
		return (TYPE_860);
	return(atoi(string));
}

