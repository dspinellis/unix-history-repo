% lib/psdit.pro -- prolog for psdit (ditroff) files
% Copyright (c) 1984, 1985 Adobe Systems Incorporated. All Rights Reserved.
% last edit: shore Sat Nov 23 20:28:03 1985
% RCSID: %Header: psdit.pro,v 2.1 85/11/24 12:19:43 shore Rel %
% Psfig RCSID $Header: psdit.pro,v 1.4 87/07/05 23:28:58 trevor Exp $

/$DITroff 180 dict def $DITroff begin

%% Psfig additions
/DocumentInitState [ matrix currentmatrix currentlinewidth currentlinecap
currentlinejoin currentdash currentgray currentmiterlimit ] cvx def

/startFig {
	/SavedState save def
	userdict maxlength dict begin
	currentpoint transform

	DocumentInitState setmiterlimit setgray setdash setlinejoin setlinecap
		setlinewidth setmatrix

	itransform moveto

	/ury exch def
	/urx exch def
	/lly exch def
	/llx exch def
	/y exch 72 mul resolution div def
	/x exch 72 mul resolution div def
	
	currentpoint /cy exch def /cx exch def

	/sx x urx llx sub div def 	% scaling for x
	/sy y ury lly sub div def	% scaling for y

	sx sy scale			% scale by (sx,sy)

	cx sx div llx sub
	cy sy div ury sub translate
	
	/DefFigCTM matrix currentmatrix def

	/initmatrix {
		DefFigCTM setmatrix
	} def
	/defaultmatrix {
		DefFigCTM exch copy
	} def

	/initgraphics {
		DocumentInitState setmiterlimit setgray setdash 
			setlinejoin setlinecap setlinewidth setmatrix
		DefFigCTM setmatrix
	} def

	/showpage {
		initgraphics
	} def

} def
% Args are llx lly urx ury (in figure coordinates)
/clipFig {
	currentpoint 6 2 roll
	newpath 4 copy
	4 2 roll moveto
	6 -1 roll exch lineto
	exch lineto
	exch lineto
	closepath clip
	newpath
	moveto
} def
% doclip, if called, will always be just after a `startfig'
/doclip { llx lly urx ury clipFig } def
/endFig {
	end SavedState restore
} def
/globalstart {
	% Push details about the enviornment on the stack.
	fontnum fontsize fontslant fontheight firstpage 
	mh my resolution slotno currentpoint 
	pagesave restore gsave 
} def
/globalend {
	grestore moveto
	/slotno exch def /resolution exch def /my exch def
	/mh exch def /firstpage exch def /fontheight exch def
	/fontslant exch def /fontsize exch def /fontnum exch def
	F
	/pagesave save def
} def

%% end XMOD additions

/fontnum 1 def /fontsize 10 def /fontheight 10 def /fontslant 0 def
/xi {0 72 11 mul translate 72 resolution div dup neg scale 0 0 moveto
  /fontnum 1 def /fontsize 10 def /fontheight 10 def /fontslant 0 def F
  /pagesave save def}def
/PB{save /psv exch def currentpoint translate 
  resolution 72 div dup neg scale 0 0 moveto}def
/PE{psv restore}def
