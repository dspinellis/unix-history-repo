/******************************************************************************
 *
 *	parser --	parses device independent troff commands and calls
 *			appropriate action routines
 *
 *	John Mellor-Crummey (Xerox Corp)
 *
 *	Copyright 1985,6 Xerox Corporation
 *
 * HISTORY
 * 24-Feb-86  Lee Moore (lee) at Xerox Webster Research Center
 *	Added new Xerox device control parsing.  The syntax for the only
 *	comand is:
 *		x Xerox IP fileName
 *		x Xerox RES anchor resolution fileName
 *
 ******************************************************************************/


#include <stdio.h>
#include <ctype.h>

#include "deviceinfo.h"	
#include "defs.h"	/* constant and macro definitions */
#include "externs.h"	/* declarations for global variables */


/*-----------------------------------------------------------------------------
 * ditroffToIpress --	parses the input file calling routines to perform
 *			requested operations
 *---------------------------------------------------------------------------*/
ditroffToIpress(fptr)
register FILE *fptr;
{
	char buffer[BUFFERSIZE];
	int c1,c2;
	int cmd;
	int flag = 1;

	while((cmd = getc(fptr)) != EOF)
	{
		switch(cmd)
		{
		case cmdPointSize:
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
				setPointSize(internalSize(c1)+1);
			break;
		case cmdFont:
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
				setFont(fontNumber(c1));
			break;
		case cmdChar:
			if (flag = ((cmd = getc(fptr)) != EOF))
				outputChar((unsigned int) cmd);
			break;
		case cmdSpecChar:
			if (flag = (fscanf(fptr,"%s",buffer) == 1))
				outputString(buffer);
			break;
		case cmdAbsHoriz:
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
				hMov(c1);
			break;
		case cmdRelHoriz:
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
				hInc(c1);
			break;
		case cmdAbsVert:
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
				vMov(c1);
			break;
		case cmdRelVert:
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
				vInc(c1);
			break;
		case cmdEol:
			/* ignore arguments, call routine for newline */
			flag = (fscanf(fptr,"%d %d",&c1,&c2) == 2);
			newLine();
			break;
		case cmdWordSep:
			break;
		case cmdStippleFamily:
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
				newStippleFamily(c1);
			break;
		case cmdNewPage:
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
				newpage(c1);
			break;
		case cmdPushEnv:
			pushCurrentEnv();
			break;
		case cmdPopEnv:
			popSavedEnv();
			break;
		case cmdCharString:
			(void) fgets(buffer,BUFFERSIZE,fptr);
			/* no op -- this command causes no conversion
			 * not expected in ditroff output
			 */
			break;
		case cmdComment:
			skipToNextLine(fptr);
			break;
		case cmdDraw:
			drawCommand(fptr);
			skipToNextLine(fptr);
			break;
		case cmdDevice:
			deviceCommand(fptr);
			skipToNextLine(fptr);
			break;
		case '\n':
			linenumber++;
			break;
		default:
			if(isdigit(cmd)) 
			{
				/* command is 2 digit horizontal displacement
				 * followed by a char
				 */
				if (flag = ((c1 = getc(fptr)) != EOF))
				{
					hInc(c1 - '0' + 10 * (cmd - '0'));
					if (flag = ((c1 = getc(fptr)) != EOF))
						outputChar((unsigned int) c1);
				}
				break;
			}
			else	if (white(cmd) || (cmd == '\0'))
					break;	/* handle trash in stream */
			reportError(QUIT,"unrecognized character in input %o %c", (char *) cmd, (char *) cmd);
		}
		if (!flag)
			reportError(QUIT,"error in input for ditroff command %c", (char *) cmd); 

	}
}


/*-----------------------------------------------------------------------------
 *	drawCommand --	process ditroff draw commands
 *---------------------------------------------------------------------------*/
drawCommand(fptr)
register FILE *fptr;
{
	int c1,c2,c3,c4;
	int cmd,flag;
	char buffer[BUFFERSIZE];

	if (flag = ((cmd = getc(fptr)) != EOF)) 
	{
		switch(cmd)
		{
		case	drawThick:		/* set line thickness */
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
			    lineThickness = c1;
			break;
		case	drawStyle:		/* set line style */
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
			    lineStyle = c1;
			break;
		case	drawLine:		/* draw from current x,y to c1,c2 */
			/* read termination point */
			if (flag = (fscanf(fptr,"%d %d",&c1,&c2) == 2))
			    drawline(c1,c2);
			break;
		case	drawCircle:		/* draw circle of diameter c1 with current x,y */
						/* leftmost position on circle */
			/* read circle diameter */
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
			{
				c2 = (c1 + 1) / 2;
				if(!outputflag) /* won't be printed anyway */
					break; 
				gobj_size(hor_pos,ver_pos - c2,hor_pos + c1,ver_pos + c2);
				bitmapDrawCircle(c1);
			}
			break;
		case	drawEllipse:		/* draw ellipse with axes c1,c2 with current x,y */
						/* leftmost point on ellipse */
			/* read height and width */
			if (flag = (fscanf(fptr,"%d %d",&c1,&c2) == 2))
			{
				c3 = (c2 + 1) / 2;
				if(!outputflag) /* won't be printed anyway */
					break; 
				gobj_size(hor_pos,ver_pos - c3,hor_pos + c1,ver_pos + c3);
				bitmapDrawEllipse(c1,c2);
			}
			break;
		case	drawArc:		/* draw counter-clockwise arc with current x,y */
						/* as start, c1,c2 represent relative x,y */
						/* offset to center, c3,c4 represent relative */
						/* x,y offset from center to ending point on arc */
			/* read relative coords to current point: xc yc xt yt */
			if (flag = (fscanf(fptr,"%d %d %d %d",&c1,&c2,&c3,&c4) == 4))
			{
				if(!outputflag) /* won't be printed anyway */
					break; 
				g_sizearc(hor_pos,ver_pos,c1,c2,c3,c4);
				bitmapDrawArc(c1,c2,c3,c4);
			}
			break;
		case	drawGremlinSpline:	/* close approximation... */
		case	drawWigglyLine:
			/* read the rest of the command line  containing a
			 * list of x,y pairs into a buffer 
			 */
			(void) fgets(buffer,BUFFERSIZE,fptr);

			if(!outputflag) /* won't be printed anyway */
				break; 
			g_sizeWigglyLine(buffer);
			bitmapDrawWigglyLine(buffer);
			break;
		case	drawPolygon:
		case	drawUbPolygon:
			/* read the rest of the command line  containing a
			 * list of x,y pairs into a buffer 
			 */
			fgets(buffer,BUFFERSIZE,fptr);
			iPdrawPolygon(buffer, cmd);
			break;

		default:
			flag = 0;
			break; 
		}
	}
	if (!flag)
		reportError(QUIT,"ill formed draw command");
}


/*-----------------------------------------------------------------------------
 *	deviceCommand --	process ditroff device commands
 *---------------------------------------------------------------------------*/
deviceCommand(fptr)
register FILE *fptr;
{
	int flag,n;
	int c1;
	char st[20];
	char buffer[80];

	if (flag = (fscanf(fptr,"%s",st) == 1))
	{
		switch(*st)
		{
		case deviceInit:
			initDevice();
			readDeviceInitInfo();
			break;
		case deviceName:
			flag = (fscanf(fptr,"%s",devicename) == 1);
			break;
		case deviceResolution:
			/* expect only resolution in spots per inch */
			if (flag = (fscanf(fptr,"%d",&spotsPerInch) == 1))
				setScale(spotsPerInch);
			break;
		case devicePause:
			/* ignore device pauses */
			break;
		case deviceStop:
			resetDevice();
			break;
		case deviceTrailer:
			/* a no op */
			break;
		case deviceFont:
			if (flag = (fscanf(fptr,"%d %s",&n,buffer) == 2))
				loadFont(n,buffer);
			break;
		case deviceHeight:
			/* a no op */
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
			break;
		case deviceSlant:
			/* a no op */
			if (flag = (fscanf(fptr,"%d",&c1) == 1))
			break;
		case deviceXerox:
			/* handle Xerox extended commands */
			parseXeroxCommand(fptr);
			break;
		}
	}
	if (!flag)
		reportError(QUIT,"ill formed device command");
}


/*-----------------------------------------------------------------------------
 *	skipToNextLine	--	eat any trailing junk and newline
 *---------------------------------------------------------------------------*/
skipToNextLine(fptr)
FILE *fptr;
{
	int n;

	while ((n = getc(fptr)) != '\n') 
		if (n == EOF) return;
	linenumber++;
}

/*---------------------------------------------------------------------------
 *	parseXeroxCommand --		parse extended Xerox commands
 *---------------------------------------------------------------------------*/
parseXeroxCommand(fptr)
FILE *fptr;
{
	char operation[80],
	     fileName[1024],
	     anchor[40],
	     resolution[40];

	if( fscanf(fptr, "%s", operation) != 1 ) {
		reportError(QUIT,"ill formed device command");
		return;
	}

	switch (operation[0]) {
	case xeroxDeviceInsertIP:
		if (fscanf(fptr, "%s", fileName) != 1) {
			reportError(QUIT,"ill formed device command");
			return;
		}

		doSequenceInsertIP(fileName);
		break;

	case xeroxDeviceInsertRES:
		if (fscanf(fptr, "%s %s %s", anchor, resolution, fileName) != 3) {
			reportError(QUIT,"ill formed device command");
			return;
		}

		doSequenceInsertRES(anchor, resolution, fileName);
		break;

	default:
		reportError(QUIT,"unknown Xerox device request");
		break;
	}
}
