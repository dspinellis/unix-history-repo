#include <X11/Xos.h>
#include <X11/IntrinsicP.h>
#include <X11/StringDefs.h>
#include <stdio.h>
#include "DviP.h"

DviGetAndPut(dw, cp)
    DviWidget	dw;
    int		*cp;
{
    if (dw->dvi.ungot)
    {
	dw->dvi.ungot = 0;
	*cp = getc (dw->dvi.file);
    }
    else
    {
	*cp = getc (dw->dvi.file);
	putc (*cp, dw->dvi.tmpFile);
    }
    return *cp;
}

char *
GetLine(dw, Buffer, Length)
	DviWidget	dw;
	char	*Buffer;
	int	Length;
{
	int 	i = 0, c;
	char	*p = Buffer;
	
	Length--;			    /* Save room for final NULL */
	
	while (i < Length && DviGetC (dw, &c) != EOF && c != '\n')
		if (p)
			*p++ = c;
	if (c == '\n' && p)		    /* Retain the newline like fgets */
		*p++ = c;
	if (c == '\n')
		DviUngetC(dw, c);
	if (p)	
		*p = NULL;
	return (Buffer);
} 

char *
GetWord(dw, Buffer, Length)
	DviWidget	dw;
	char	*Buffer;
	int	Length;
{
	int 	i = 0, c;
	char	*p = Buffer;
	
	Length--;			    /* Save room for final NULL */
	while (DviGetC(dw, &c) != EOF && (c == ' ' || c == '\n'))
		;
	if (c != EOF)
		DviUngetC(dw, c);
	while (i < Length && DviGetC(dw, &c) != EOF && c != ' ' && c != '\n')
		if (p)
			*p++ = c;
	if (c != EOF)
		DviUngetC(dw, c);
	if (p)
		*p = NULL;
	return (Buffer);
} 

GetNumber(dw)
	DviWidget	dw;
{
	int	i = 0,  c;
	int	negative = 0;

	while (DviGetC(dw, &c) != EOF && (c == ' ' || c == '\n'))
		;
	if (c != EOF)
		DviUngetC(dw, c);
	
	while (DviGetC(dw, &c) != EOF && c >= '0' && c <= '9') {
		if (negative)
			i = i*10 - (c - '0');
		else
			i = i*10 + c - '0';
	}

	if (c != EOF)
		DviUngetC(dw, c);
	return (i);
}
	
/*
Local Variables:
c-indent-level: 8
c-continued-statement-offset: 8
c-brace-offset: -8
c-argdecl-indent: 8
c-label-offset: -8
c-tab-always-indent: nil
End:
*/
