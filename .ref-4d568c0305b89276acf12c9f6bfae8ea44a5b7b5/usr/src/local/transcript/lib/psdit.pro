%	@(#)psdit.pro	1.6 %G%
% lib/psdit.pro -- prolog for psdit (ditroff) files
% Copyright (c) 1984, 1985 Adobe Systems Incorporated. All Rights Reserved.
% last edit: shore Sat Nov 23 20:28:03 1985
% RCSID: $Header: psdit.pro,v 2.1 85/11/24 12:19:43 shore Rel $

% Changed by Edward Wang (edward@ucbarpa.berkeley.edu) to handle graphics,
% 17 Feb, 87.

/$DITroff 140 dict def $DITroff begin
/fontnum 1 def /fontsize 10 def /fontheight 10 def /fontslant 0 def
/xi{0 72 11 mul translate 72 resolution div dup neg scale 0 0 moveto
 /fontnum 1 def /fontsize 10 def /fontheight 10 def /fontslant 0 def F}def
/PB{save /psv exch def currentpoint translate 
 resolution 72 div dup neg scale 0 0 moveto}def
/PE{psv restore}def
/arctoobig 90 def /arctoosmall .05 def
/m1 matrix def /m2 matrix def /m3 matrix def /oldmat matrix def
/tan{dup sin exch cos div}def
/point{resolution 72 div mul}def
/dround	{transform round exch round exch itransform}def
/xT{/devname exch def}def
/xr{/mh exch def /my exch def /resolution exch def}def
/xp{}def
/xs{docsave restore end}def
/xt{}def
/xf{/fontname exch def /slotno exch def fontnames slotno get fontname eq not
 {fonts slotno fontname findfont put fontnames slotno fontname put}if}def
/xH{/fontheight exch def F}def
/xS{/fontslant exch def F}def
/s{/fontsize exch def /fontheight fontsize def F}def
/f{/fontnum exch def F}def
/F{fontheight 0 le{/fontheight fontsize def}if
 fonts fontnum get fontsize point 0 0 fontheight point neg 0 0 m1 astore
 fontslant 0 ne{1 0 fontslant tan 1 0 0 m2 astore m3 concatmatrix}if
 makefont setfont .04 fontsize point mul 0 dround pop setlinewidth}def
/X{exch currentpoint exch pop moveto show}def
/N{3 1 roll moveto show}def
/Y{exch currentpoint pop exch moveto show}def
/S{show}def
/ditpush{}def/ditpop{}def
/AX{3 -1 roll currentpoint exch pop moveto 0 exch ashow}def
/AN{4 2 roll moveto 0 exch ashow}def
/AY{3 -1 roll currentpoint pop exch moveto 0 exch ashow}def
/AS{0 exch ashow}def
/MX{currentpoint exch pop moveto}def
/MY{currentpoint pop exch moveto}def
/MXY{moveto}def
/cb{pop}def	% action on unknown char -- nothing for now
/n{}def/w{}def
/p{pop showpage xi}def
/Dt{/Dlinewidth exch def}def 1 Dt
/Ds{/Ddash exch def}def -1 Ds
/i{/Dstipple exch def}def 1 i
/Dsetlinewidth{2 Dlinewidth mul setlinewidth}def
/Dsetdash{Ddash 4 eq{[8 12]}{Ddash 16 eq{[32 36]}
 {Ddash 20 eq{[32 12 8 12]}{[]}ifelse}ifelse}ifelse 0 setdash}def
/Dstroke{gsave Dsetlinewidth Dsetdash 1 setlinecap stroke grestore
 currentpoint newpath moveto}def
/Dl{rlineto Dstroke}def
/arcellipse{/diamv exch def /diamh exch def oldmat currentmatrix pop
 currentpoint translate 1 diamv diamh div scale /rad diamh 2 div def
 currentpoint exch rad add exch rad -180 180 arc oldmat setmatrix}def
/Dc{dup arcellipse Dstroke}def
/De{arcellipse Dstroke}def
/Da{/endv exch def /endh exch def /centerv exch def /centerh exch def
 /cradius centerv centerv mul centerh centerh mul add sqrt def
 /eradius endv endv mul endh endh mul add sqrt def
 /endang endv endh atan def
 /startang centerv neg centerh neg atan def
 /sweep startang endang sub dup 0 lt{360 add}if def
 sweep arctoobig gt
 {/midang startang sweep 2 div sub def /midrad cradius eradius add 2 div def
  /midh midang cos midrad mul def /midv midang sin midrad mul def
  midh neg midv neg endh endv centerh centerv midh midv Da
  Da}
 {sweep arctoosmall ge
  {/controldelt 1 sweep 2 div cos sub 3 sweep 2 div sin mul div 4 mul def
   centerv neg controldelt mul centerh controldelt mul
   endv neg controldelt mul centerh add endh add
   endh controldelt mul centerv add endv add
   centerh endh add centerv endv add rcurveto Dstroke}
  {centerh endh add centerv endv add rlineto Dstroke}
  ifelse}
 ifelse}def
/Dpatterns[
[%cf[widthbits]
[8<0000000000000010>]
[8<0411040040114000>]
[8<0204081020408001>]
[8<0000103810000000>]
[8<6699996666999966>]
[8<0000800100001008>]
[8<81c36666c3810000>]
[8<0f0e0c0800000000>]
[8<0000000000000010>]
[8<0411040040114000>]
[8<0204081020408001>]
[8<0000001038100000>]
[8<6699996666999966>]
[8<0000800100001008>]
[8<81c36666c3810000>]
[8<0f0e0c0800000000>]
[8<0042660000246600>]
[8<0000990000990000>]
[8<0804020180402010>]
[8<2418814242811824>]
[8<6699996666999966>]
[8<8000000008000000>]
[8<00001c3e363e1c00>]
[8<0000000000000000>]
[32<00000040000000c00000004000000040000000e0000000000000000000000000>]
[32<00000000000060000000900000002000000040000000f0000000000000000000>]
[32<000000000000000000e0000000100000006000000010000000e0000000000000>]
[32<00000000000000002000000060000000a0000000f00000002000000000000000>]
[32<0000000e0000000000000000000000000000000f000000080000000e00000001>]
[32<0000090000000600000000000000000000000000000007000000080000000e00>]
[32<00010000000200000004000000040000000000000000000000000000000f0000>]
[32<0900000006000000090000000600000000000000000000000000000006000000>]]
[%ug
[8<0000020000000000>]
[8<0000020000002000>]
[8<0004020000002000>]
[8<0004020000402000>]
[8<0004060000402000>]
[8<0004060000406000>]
[8<0006060000406000>]
[8<0006060000606000>]
[8<00060e0000606000>]
[8<00060e000060e000>]
[8<00070e000060e000>]
[8<00070e000070e000>]
[8<00070e020070e000>]
[8<00070e020070e020>]
[8<04070e020070e020>]
[8<04070e024070e020>]
[8<04070e064070e020>]
[8<04070e064070e060>]
[8<06070e064070e060>]
[8<06070e066070e060>]
[8<06070f066070e060>]
[8<06070f066070f060>]
[8<060f0f066070f060>]
[8<060f0f0660f0f060>]
[8<060f0f0760f0f060>]
[8<060f0f0760f0f070>]
[8<0e0f0f0760f0f070>]
[8<0e0f0f07e0f0f070>]
[8<0e0f0f0fe0f0f070>]
[8<0e0f0f0fe0f0f0f0>]
[8<0f0f0f0fe0f0f0f0>]
[8<0f0f0f0ff0f0f0f0>]
[8<1f0f0f0ff0f0f0f0>]
[8<1f0f0f0ff1f0f0f0>]
[8<1f0f0f8ff1f0f0f0>]
[8<1f0f0f8ff1f0f0f8>]
[8<9f0f0f8ff1f0f0f8>]
[8<9f0f0f8ff9f0f0f8>]
[8<9f0f0f9ff9f0f0f8>]
[8<9f0f0f9ff9f0f0f9>]
[8<9f8f0f9ff9f0f0f9>]
[8<9f8f0f9ff9f8f0f9>]
[8<9f8f1f9ff9f8f0f9>]
[8<9f8f1f9ff9f8f1f9>]
[8<bf8f1f9ff9f8f1f9>]
[8<bf8f1f9ffbf8f1f9>]
[8<bf8f1fdffbf8f1f9>]
[8<bf8f1fdffbf8f1fd>]
[8<ff8f1fdffbf8f1fd>]
[8<ff8f1fdffff8f1fd>]
[8<ff8f1ffffff8f1fd>]
[8<ff8f1ffffff8f1ff>]
[8<ff9f1ffffff8f1ff>]
[8<ff9f1ffffff9f1ff>]
[8<ff9f9ffffff9f1ff>]
[8<ff9f9ffffff9f9ff>]
[8<ffbf9ffffff9f9ff>]
[8<ffbf9ffffffbf9ff>]
[8<ffbfdffffffbf9ff>]
[8<ffbfdffffffbfdff>]
[8<ffffdffffffbfdff>]
[8<ffffdffffffffdff>]
[8<fffffffffffffdff>]
[8<ffffffffffffffff>]]
[%mg
[8<8000000000000000>]
[8<0822080080228000>]
[8<0204081020408001>]
[8<40e0400000000000>]
[8<66999966>]
[8<8001000010080000>]
[8<81c36666c3810000>]
[8<f0e0c08000000000>]
[16<07c00f801f003e007c00f800f001e003c007800f001f003e007c00f801f003e0>]
[16<1f000f8007c003e001f000f8007c003e001f800fc007e003f001f8007c003e00>]
[8<c3c300000000c3c3>]
[16<0040008001000200040008001000200040008000000100020004000800100020>]
[16<0040002000100008000400020001800040002000100008000400020001000080>]
[16<1fc03fe07df0f8f8f07de03fc01f800fc01fe03ff07df8f87df03fe01fc00f80>]
[8<80>]
[8<8040201000000000>]
[8<84cc000048cc0000>]
[8<9900009900000000>]
[8<08040201804020100800020180002010>]
[8<2418814242811824>]
[8<66999966>]
[8<8000000008000000>]
[8<70f8d8f870000000>]
[8<0814224180402010>]
[8<aa00440a11a04400>]
[8<018245aa45820100>]
[8<221c224180808041>]
[8<88000000>]
[8<0855800080550800>]
[8<2844004482440044>]
[8<0810204080412214>]
[8<00>]]]def
/Dfill{
 save 6 1 roll
 transform /maxy exch def /maxx exch def
 transform /miny exch def /minx exch def
 minx maxx gt{/minx maxx /maxx minx def def}if
 miny maxy gt{/miny maxy /maxy miny def def}if
 Dpatterns Dstipple 1 sub get exch 1 sub get
 aload pop /stip exch def /stipw exch def /stiph 128 def
 /imatrix[stipw 0 0 stiph 0 0]def
 /tmatrix[stipw 0 0 stiph 0 0]def
 /minx minx cvi stiph idiv stiph mul def
 /miny miny cvi stipw idiv stipw mul def
 eoclip 0 setgray
 miny stiph maxy{
  tmatrix exch 5 exch put
  minx stipw maxx{
   tmatrix exch 4 exch put tmatrix setmatrix
   stipw stiph true imatrix {stip} imagemask
  }for
 }for
 restore
}def
/Dp{Dfill Dstroke}def
/DP{Dfill currentpoint newpath moveto}def
end

/ditstart{$DITroff begin
 /nfonts 60 def			% NFONTS makedev/ditroff dependent!
 /fonts[nfonts{0}repeat]def
 /fontnames[nfonts{()}repeat]def
/docsave save def
}def

% character outcalls
/oc{
 /pswid exch def /cc exch def /name exch def
 /ditwid pswid fontsize mul resolution mul 72000 div def
 /ditsiz fontsize resolution mul 72 div def
 ocprocs name known{ocprocs name get exec}{name cb}ifelse
}def
/fractm [.65 0 0 .6 0 0] def
/fraction{
 /fden exch def /fnum exch def gsave /cf currentfont def
 cf fractm makefont setfont 0 .3 dm 2 copy neg rmoveto
 fnum show rmoveto currentfont cf setfont(\244)show setfont fden show 
 grestore ditwid 0 rmoveto
}def
/oce{grestore ditwid 0 rmoveto}def
/dm{ditsiz mul}def
/ocprocs 50 dict def ocprocs begin
(14){(1)(4)fraction}def
(12){(1)(2)fraction}def
(34){(3)(4)fraction}def
(13){(1)(3)fraction}def
(23){(2)(3)fraction}def
(18){(1)(8)fraction}def
(38){(3)(8)fraction}def
(58){(5)(8)fraction}def
(78){(7)(8)fraction}def
(sr){gsave 0 .06 dm rmoveto(\326)show oce}def
(is){gsave 0 .15 dm rmoveto(\362)show oce}def
(->){gsave 0 .02 dm rmoveto(\256)show oce}def
(<-){gsave 0 .02 dm rmoveto(\254)show oce}def
(==){gsave 0 .05 dm rmoveto(\272)show oce}def
(uc){gsave currentpoint 400 .009 dm mul add translate
     8 -8 scale ucseal oce}def
end

% an attempt at a PostScript FONT to implement ditroff special chars
% this will enable us to 
%	cache the little buggers
%	generate faster, more compact PS out of psdit
%	confuse everyone (including myself)!
50 dict dup begin
/FontType 3 def
/FontName /DIThacks def
/FontMatrix [.001 0 0 .001 0 0] def
/FontBBox [-260 -260 900 900] def% a lie but ...
/Encoding 256 array def
0 1 255{Encoding exch /.notdef put}for
Encoding
 dup 8#040/space put %space
 dup 8#110/rc put %right ceil
 dup 8#111/lt put %left  top curl
 dup 8#112/bv put %bold vert
 dup 8#113/lk put %left  mid curl
 dup 8#114/lb put %left  bot curl
 dup 8#115/rt put %right top curl
 dup 8#116/rk put %right mid curl
 dup 8#117/rb put %right bot curl
 dup 8#120/rf put %right floor
 dup 8#121/lf put %left  floor
 dup 8#122/lc put %left  ceil
 dup 8#140/sq put %square
 dup 8#141/bx put %box
 dup 8#142/ci put %circle
 dup 8#143/br put %box rule
 dup 8#144/rn put %root extender
 dup 8#145/vr put %vertical rule
 dup 8#146/ob put %outline bullet
 dup 8#147/bu put %bullet
 dup 8#150/ru put %rule
 dup 8#151/ul put %underline
 pop
/DITfd 100 dict def
/BuildChar{0 begin
 /cc exch def /fd exch def
 /charname fd /Encoding get cc get def
 /charwid fd /Metrics get charname get def
 /charproc fd /CharProcs get charname get def
 charwid 0 fd /FontBBox get aload pop setcachedevice
 2 setlinejoin 40 setlinewidth
 newpath 0 0 moveto gsave charproc grestore
 end}def
/BuildChar load 0 DITfd put
/CharProcs 50 dict def
CharProcs begin
/space{}def
/.notdef{}def
/ru{500 0 rls}def
/rn{0 840 moveto 500 0 rls}def
/vr{0 800 moveto 0 -770 rls}def
/bv{0 800 moveto 0 -1000 rls}def
/br{0 840 moveto 0 -1000 rls}def
/ul{0 -140 moveto 500 0 rls}def
/ob{200 250 rmoveto currentpoint newpath 200 0 360 arc closepath stroke}def
/bu{200 250 rmoveto currentpoint newpath 200 0 360 arc closepath fill}def
/sq{80 0 rmoveto currentpoint dround newpath moveto
    640 0 rlineto 0 640 rlineto -640 0 rlineto closepath stroke}def
/bx{80 0 rmoveto currentpoint dround newpath moveto
    640 0 rlineto 0 640 rlineto -640 0 rlineto closepath fill}def
/ci{500 360 rmoveto currentpoint newpath 333 0 360 arc
    50 setlinewidth stroke}def

/lt{0 -200 moveto 0 550 rlineto currx 800 2cx s4 add exch s4 a4p stroke}def
/lb{0 800 moveto 0 -550 rlineto currx -200 2cx s4 add exch s4 a4p stroke}def
/rt{0 -200 moveto 0 550 rlineto currx 800 2cx s4 sub exch s4 a4p stroke}def
/rb{0 800 moveto 0 -500 rlineto currx -200 2cx s4 sub exch s4 a4p stroke}def
/lk{0 800 moveto 0 300 -300 300 s4 arcto pop pop 1000 sub
    0 300 4 2 roll s4 a4p 0 -200 lineto stroke}def
/rk{0 800 moveto 0 300 s2 300 s4 arcto pop pop 1000 sub
    0 300 4 2 roll s4 a4p 0 -200 lineto stroke}def
/lf{0 800 moveto 0 -1000 rlineto s4 0 rls}def
/rf{0 800 moveto 0 -1000 rlineto s4 neg 0 rls}def
/lc{0 -200 moveto 0 1000 rlineto s4 0 rls}def
/rc{0 -200 moveto 0 1000 rlineto s4 neg 0 rls}def
end

/Metrics 50 dict def Metrics begin
/.notdef 0 def
/space 500 def
/ru 500 def
/br 0 def
/lt 416 def
/lb 416 def
/rt 416 def
/rb 416 def
/lk 416 def
/rk 416 def
/rc 416 def
/lc 416 def
/rf 416 def
/lf 416 def
/bv 416 def
/ob 350 def
/bu 350 def
/ci 750 def
/bx 750 def
/sq 750 def
/rn 500 def
/ul 500 def
/vr 0 def
end

DITfd begin
/s2 500 def /s4 250 def /s3 333 def
/a4p{arcto pop pop pop pop}def
/2cx{2 copy exch}def
/rls{rlineto stroke}def
/currx{currentpoint pop}def
/dround{transform round exch round exch itransform} def
end
end
/DIThacks exch definefont pop
