
% included prolog for enscript files
% last edit: shore Sat Nov  9 13:28:41 1985
% Copyright (c) 1983, 1984 Adobe Systems Incorporated
% RCSID: $Header: enscript.pro,v 1.5 89/03/10 00:45:30 van Exp $
save/EnscriptJob exch def
/StartEnscriptDoc{$enscript begin}def
/$enscript 50 dict def $enscript begin
/EndEnscriptDoc{end}def
/S/show load def
/X{exch 0 rmoveto S}def
/Y{exch 0 exch rmoveto S}def
/B{3 1 roll moveto S}def
/F{$fd exch get setfont}def
/StartPage{/svpg save def .05 dup scale}def
/EndPage{svpg restore showpage}def
/DoPreFeed{/statusdict where{pop
 statusdict/prefeed known{statusdict exch/prefeed exch put 0}if}if pop}def
/Landscape{90 rotate 0 -15840 translate}def
/SetUpFonts
 {dup/$fd exch array def{findfont exch scalefont $fd 3 1 roll put}repeat}def
/InitGaudy{/TwoColumn exch def /BarLength exch def}def
/U{1440 mul}def
/UP{U 72 div}def
/LB{/pts exch UP def /charcolor exch def /boxcolor exch def /font exch def
 /label exch def /dy exch def /dx exch def /lly exch def /llx exch def
 gsave boxcolor setgray
 llx lly moveto dx 0 rlineto 0 dy rlineto dx neg 0 rlineto closepath fill
 /lines label length def
 /yp lly dy add dy lines pts mul sub 2 div sub pts .85 mul sub def
 font setfont charcolor setgray
 label {dup stringwidth pop 2 div llx dx 2 div add exch sub yp moveto show
   /yp yp pts sub def}forall grestore}def
/Gaudy{/Page exch def /Date exch def /File exch def /Comment exch def
 .25 U 10.2 U BarLength .1 sub U .25 U [File] $fd 2 get .97 0 14 LB
 .25 U 10.45 U BarLength .1 sub U .25 U [Comment] $fd 1 get 1 0 14 LB
 .25 U 10.2 U .75 U .5 U Date $fd 3 get .97 0 12 LB
 BarLength .5 sub U 10.2 U .75 U .5 U [Page] $fd 4 get .97 1 30 LB
 TwoColumn{BarLength 2 div .19 add U 10.2 U moveto 0 -10 U rlineto stroke}if
}def
end
