/*
 * Copyright (c) 1986, 1987 Xerox Corporation.
 */

/* $Log:	filetypes.h,v $
 * Revision 1.2  87/03/17  16:32:04  ed
 * Yet more file types added.
 * 
 * Revision 1.1  87/01/14  11:41:51  ed
 * Initial revision
 * 
 * 
 */

/*
 * Common Viewpoint file types
 */

#define	TYPE_I			0	/* image and Type=tUnspecified */
#define TYPE_Directory		1	/* Type=tDirectory: image */
#define	TYPE_A			2	/* ASCII -- map CR <=> LF and Type=tText */
#define TYPE_S			3	/* Serialized file Type=tSerialized */
#define TYPE_VPMailNote		4	/* VP Mail Note: ASCII */
#define TYPE_VPDrawer		4098	/* VP File Drawer: image */
#define TYPE_Font		4290	/* printer fonts: image */
#define	TYPE_VP			4353	/* ViewPoint: image */
#define TYPE_Interpress		4361	/* VP Interpress master: image */
#define TYPE_VPRecordsfile	4365	/* VP Records file: image */
#define TYPE_VPSpreadsheet	4381	/* VP Spreadsheet: image */
#define TYPE_VPDictionary	4383	/* VP Dictionary: image */
#define TYPE_VPApplication	4387	/* VP Application: image */
#define TYPE_VPApplication2	4423	/* VP Application: image */
#define TYPE_VPReference	4427	/* VP Reference Icon: image */
#define TYPE_VPCalendar		4436	/* VP Calendar: image */
#define TYPE_VPBook		4444	/* VP Book: image */
#define TYPE_VPCanvas		4428	/* VP Canvas: image */
#define TYPE_860		5120	/* 860 file: image */
#define TYPE_VPIcons		6010	/* VP Icon file: image */

#define TYPE_Guess		4294967295	/* escape to use actual file type */

/*
 * last type reserved for Filing
 * files of type > will have attributes saved with file
 */

#define LAST_FILING_TYPE	4095

/*
 * Headers for common file types
 * (lower case is used to ease string comparisons)
 *
 * NOTE: only Interpress and RES are "official" Xerox headers, the others
 * 	 are merely a convenience for recognizing other Viewpoint related
 *	 formats when stored locally.
 */

#define INTERPRESSHDR	"interpress/xerox"
#define RESHDR		"rasterencoding"
#define VPHDR		"viewpoint-file/xerox"
