/*
This file is part of GNU Binutils.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */
#include <stdio.h>
#define BYTE_INFO_LEN 10
/*#include "z8opcode.h"*/
struct op {
char  *bits;
char *name;
} ;
#define iswhite(x) ((x) == ' ' || (x) == '\t')
struct op opt[] = 
{
"1011 0101 ssss dddd","adc rd,rs",
"1011 0100 ssss dddd","adcb rbd,rbs",
"0000 0001 ssN0 dddd","add rd,@rs",
"0100 0001 0000 dddd address","add rd,address",
"0100 0001 ssN0 dddd address","add rd,address(rs)",
"0000 0001 0000 dddd imm16","add rd,imm16",
"1000 0001 ssss dddd","add rd,rs",
"0000 0000 ssN0 dddd","addb rbd,@rs",
"0100 0000 0000 dddd address","addb rbd,address",
"0100 0000 ssN0 dddd address","addb rbd,address(rs)",
"0000 0000 0000 dddd imm8 imm8","addb rbd,imm8",
"1000 0000 ssss dddd","addb rbd,rbs",
"0001 0110 ssN0 dddd","addl rrd,@rs",
"0101 0110 0000 dddd address","addl rrd,address",
"0101 0110 ssN0 dddd address","addl rrd,address(rs)",
"0001 0110 0000 dddd imm32","addl rrd,imm32",
"1001 0110 ssss dddd","addl rrd,rrs",
"0000 0111 ssN0 dddd","and rd,@rs",
"0100 0111 0000 dddd address","and rd,address",
"0100 0111 ssN0 dddd address","and rd,address(rs)",
"0000 0111 0000 dddd imm16","and rd,imm16",
"1000 0111 ssss dddd","and rd,rs",
"0000 0110 ssN0 dddd","andb rbd,@rs",
"0100 0110 0000 dddd address","andb rbd,address",
"0100 0110 ssN0 dddd address","andb rbd,address(rs)",
"0000 0110 0000 dddd imm8 imm8","andb rbd,imm8",
"1000 0110 ssss dddd","andb rbd,rbs",
"0010 0111 ddN0 imm4","bit @rd,imm4",
"0110 0111 ddN0 imm4 address","bit address(rd),imm4",
"0110 0111 0000 imm4 address","bit address,imm4",
"1010 0111 dddd imm4","bit rd,imm4",
"0010 0111 0000 ssss 0000 dddd 0000 0000","bit rd,rs",
"0010 0110 ddN0 imm4","bitb @rd,imm4",
"0110 0110 ddN0 imm4 address","bitb address(rd),imm4",
"0110 0110 0000 imm4 address","bitb address,imm4",
"1010 0110 dddd imm4","bitb rbd,imm4",
"0010 0110 0000 ssss 0000 dddd 0000 0000","bitb rbd,rs",
"0001 1111 ddN0 0000","call @rd",
"0101 1111 0000 0000 address","call address",
"0101 1111 ddN0 0000 address","call address(rd)",
"1101 disp12","calr disp12",
"0000 1101 ddN0 1000","clr @rd",
"0100 1101 0000 1000 address","clr address",
"0100 1101 ddN0 1000 address","clr address(rd)",
"1000 1101 dddd 1000","clr rd",
"0000 1100 ddN0 1000","clrb @rd",
"0100 1100 0000 1000 address","clrb address",
"0100 1100 ddN0 1000 address","clrb address(rd)",
"1000 1100 dddd 1000","clrb rbd",
"1011 1011 ssN0 1010 0000 rrrr ddN0 cccc","cpsd @rd,@rs,rr,cc",
"1011 1010 ssN0 1010 0000 rrrr ddN0 cccc","cpsdb @rd,@rs,rr,cc",
"1011 1011 ssN0 1110 0000 rrrr ddN0 cccc","cpsdr @rd,@rs,rr,cc",
"1011 1010 ssN0 1110 0000 rrrr ddN0 cccc","cpsdrb @rd,@rs,rr,cc",
"1011 1011 ssN0 0010 0000 rrrr ddN0 cccc","cpsi @rd,@rs,rr,cc",
"1011 1010 ssN0 0010 0000 rrrr ddN0 cccc","cpsib @rd,@rs,rr,cc",
"1011 1011 ssN0 0110 0000 rrrr ddN0 cccc","cpsir @rd,@rs,rr,cc",
"1011 1010 ssN0 0110 0000 rrrr ddN0 cccc","cpsirb @rd,@rs,rr,cc",
"0000 1101 ddN0 0000","com @rd",
"0100 1101 0000 0000 address","com address",
"0100 1101 ddN0 0000 address","com address(rd)",
"1000 1101 dddd 0000","com rd",
"0000 1100 ddN0 0000","comb @rd",
"0100 1100 0000 0000 address","comb address",
"0100 1100 ddN0 0000 address","comb address(rd)",
"1000 1100 dddd 0000","comb rbd",
"1000 1101 imm4 0101","comflg flags",
"0000 1101 ddN0 0001 imm16","cp @rd,imm16",
"0100 1101 ddN0 0001 address imm16","cp address(rd),imm16",
"0100 1101 0000 0001 address imm16","cp address,imm16",
"0000 1011 ssN0 dddd","cp rd,@rs",
"0100 1011 0000 dddd address","cp rd,address",
"0100 1011 ssN0 dddd address","cp rd,address(rs)",
"0000 1011 0000 dddd imm16","cp rd,imm16",
"1000 1011 ssss dddd","cp rd,rs",
"0000 1100 ddN0 0001 imm8 imm8","cpb @rd,imm8",
"0100 1100 ddN0 0001 address imm8 imm8","cpb address(rd),imm8",
"0100 1100 0000 0001 address imm8 imm8","cpb address,imm8",
"0000 1010 ssN0 dddd","cpb rbd,@rs",
"0100 1010 0000 dddd address","cpb rbd,address",
"0100 1010 ssN0 dddd address","cpb rbd,address(rs)",
"0000 1010 0000 dddd imm8 imm8","cpb rbd,imm8",
"1000 1010 ssss dddd","cpb rbd,rbs",
"1011 1011 ssN0 1000 0000 rrrr dddd cccc","cpd rd,@rs,rr,cc",
"1011 1010 ssN0 1000 0000 rrrr dddd cccc","cpdb rbd,@rs,rr,cc",
"1011 1011 ssN0 1100 0000 rrrr dddd cccc","cpdr rd,@rs,rr,cc",
"1011 1010 ssN0 1100 0000 rrrr dddd cccc","cpdrb rbd,@rs,rr,cc",
"1011 1011 ssN0 0000 0000 rrrr dddd cccc","cpi rbd,@rs,rr,cc",
"1011 1010 ssN0 0000 0000 rrrr dddd cccc","cpib rd,@rs,rr,cc",
"1011 1011 ssN0 0100 0000 rrrr dddd cccc","cpir rbd,@rs,rr,cc",
"1011 1010 ssN0 0100 0000 rrrr dddd cccc","cpirb rd,@rs,rr,cc",
"0001 0000 ssN0 dddd","cpl rrd,@rs",
"0101 0000 0000 dddd address","cpl rrd,address",
"0101 0000 ssN0 dddd address","cpl rrd,address(rs)",
"0001 0000 0000 dddd imm32","cpl rrd,imm32",
"1001 0000 ssss dddd","cpl rrd,rrs",
"1011 0000 dddd 0000","dab rbd",
"1111 dddd 1disp7","dbjnz rbd,disp7",
"0010 1011 ddN0 imm4","dec @rd,imm4",
"0110 1011 ddN0 imm4 address","dec address(rd),imm4",
"0110 1011 0000 imm4 address","dec address,imm4",
"1010 1011 dddd imm4","dec rd,imm4",
"0010 1010 ddN0 imm4","decb @rd,imm4",
"0110 1010 ddN0 imm4 address","decb address(rd),imm4",
"0110 1010 0000 imm4 address","decb address,imm4",
"1010 1010 dddd imm4","decb rbd,imm4",
"0111 1100 0000 00ii","di i2",
"0001 1011 ssN0 dddd","div rrd,@rs",
"0101 1011 0000 dddd address","div rrd,address",
"0101 1011 ssN0 dddd address","div rrd,address(rs)",
"0001 1011 0000 dddd imm16","div rrd,imm16",
"1001 1011 ssss dddd","div rrd,rs",
"0001 1010 ssN0 dddd","divl rqd,@rs",
"0101 1010 0000 dddd address","divl rqd,address",
"0101 1010 ssN0 dddd address","divl rqd,address(rs)",
"0001 1010 0000 dddd imm32","divl rqd,imm32",
"1001 1010 ssss dddd","divl rqd,rrs",
"1111 dddd 0disp7","djnz rd,disp7",
"0111 1100 0000 01ii","ei i2",
"1010 1101 ssss dddd","ex rbd,rbs",
"0010 1101 ssN0 dddd","ex rd,@rs",
"0110 1101 0000 dddd address","ex rd,address",
"0110 1101 ssN0 dddd address","ex rd,address(rs)",
"0010 1100 ssN0 dddd","exb rbd,@rs",
"0110 1100 0000 dddd address","exb rbd,address",
"0110 1100 ssN0 dddd address","exb rbd,address(rs)",
"1010 1100 ssss dddd","exb rbd,rbs",
"1011 0001 dddd 1010","exts rrd",
"1011 0001 dddd 0000","extsb rd",
"1011 0001 dddd 0111","extsl rqd",
"0111 1010 0000 0000","halt",
"0011 1101 ssN0 dddd","in rd,@rs",
"0011 1101 dddd 0100 imm16","in rd,imm16",
"0011 1100 ssN0 dddd","inb rbd,@rs",
"0011 1100 dddd 0100 imm16","inb rbd,imm16",
"0010 1001 ddN0 imm4","inc @rd,imm4",
"0110 1001 ddN0 imm4 address","inc address(rd),imm4",
"0110 1001 0000 imm4 address","inc address,imm4",
"1010 1001 dddd imm4","inc rd,imm4",
"0010 1000 ddN0 imm4","incb @rd,imm4",
"0110 1000 ddN0 imm4 address","incb address(rd),imm4",
"0110 1000 0000 imm4 address","incb address,imm4",
"1010 1000 dddd imm4","incb rbd,imm4",
"0011 1011 ssN0 1000 0000 aaaa ddN0 1000","ind @rd,@rs,ra",
"0011 1010 ssN0 1000 0000 aaaa ddN0 1000","indb @rd,@rs,rba",
"0011 1100 ssN0 0000 0000 aaaa ddN0 1000","inib @rd,@rs,ra",
"0011 1100 ssN0 0000 0000 aaaa ddN0 0000","inibr @rd,@rs,ra",
"0111 1011 0000 0000","iret",
"0001 1110 ddN0 cccc","jp cc,@rd",
"0101 1110 0000 cccc address","jp cc,address",
"0101 1110 ddN0 cccc address","jp cc,address(rd)",
"1110 cccc disp8","jr cc,disp8",
"0000 1101 ddN0 0101 imm16","ld @rd,imm16",
"0010 1111 ddN0 ssss","ld @rd,rs",
"0100 1101 ddN0 0101 address imm16","ld address(rd),imm16",
"0110 1111 ddN0 ssss address","ld address(rd),rs",
"0100 1101 0000 0101 address imm16","ld address,imm16",
"0110 1111 0000 ssss address","ld address,rs",
"0011 0011 ddN0 ssss disp16","ld rd(disp16),rs",
"0111 0011 ddN0 ssss 0000 xxxx 0000 0000","ld rd(rx),rs",
"0010 0001 ssN0 dddd","ld rd,@rs",
"0110 0001 0000 dddd address","ld rd,address",
"0110 0001 ssN0 dddd address","ld rd,address(rs)",
"0010 0001 0000 dddd imm16","ld rd,imm16",
"1010 0001 ssss dddd","ld rd,rs",
"0011 0001 ssN0 dddd disp16","ld rd,rs(disp16)",
"0111 0001 ssN0 dddd 0000 xxxx 0000 0000","ld rd,rs(rx)",
"0111 0110 0000 dddd address","lda rd,address",
"0111 0110 ssN0 dddd address","lda rd,address(rs)",
"0011 0100 ssN0 dddd disp16","lda rd,rs(disp16)",
"0111 0100 ssN0 dddd 0000 xxxx 0000 0000","lda rd,rs(rx)",
"0011 0100 0000 dddd disp16","ldar rd,disp16",
"0000 1100 ddN0 0101 imm8 imm8","ldb @rd,imm8",
"0010 1110 ddN0 ssss","ldb @rd,rbs",
"0100 1100 ddN0 0101 address imm8 imm8","ldb address(rd),imm8",
"0100 1110 ddN0 ssss address","ldb address(rd),rbs",
"0100 1100 0000 0101 address imm8 imm8","ldb address,imm8",
"0110 1110 0000 ssss address","ldb address,rbs",
"0010 0000 ssN0 dddd","ldb rbd,@rs",
"0110 0000 0000 dddd address","ldb rbd,address",
"0110 0000 ssN0 dddd address","ldb rbd,address(rs)",
"1100 dddd imm8","ldb rbd,imm8",
"0010 0000 0000 dddd imm8 imm8","ldb rbd,imm8",
"1010 0000 ssss dddd","ldb rbd,rbs",
"0011 0000 ssN0 dddd disp16","ldb rbd,rs(disp16)",
"0111 0000 ssN0 dddd 0000 xxxx 0000 0000","ldb rbd,rs(rx)",
"0011 0010 ddN0 ssss disp16","ldb rd(disp16),rbs",
"0111 0010 ddN0 ssss 0000 xxxx 0000 0000","ldb rd(rx),rbs",
"0111 1101 ssss 1ccc","ldctl ctrl,rs",
"0111 1101 dddd 0ccc","ldctl rd,ctrl",
"0001 1101 ddN0 ssss","ldl @rd,rrs",
"0101 1101 ddN0 ssss address","ldl address(rd),rrs",
"0101 1101 0000 ssss address","ldl address,rrs",
"0011 0111 ddN0 ssss disp16","ldl rd(disp16),rrs",
"0111 0111 ddN0 ssss 0000 xxxx 0000 0000","ldl rd(rx),rrs",
"0001 0100 ssN0 dddd","ldl rrd,@rs",
"0101 0100 0000 dddd address","ldl rrd,address",
"0101 0100 ssN0 dddd address","ldl rrd,address(rs)",
"0001 0100 0000 dddd imm32","ldl rrd,imm32",
"1001 0100 ssss dddd","ldl rrd,rrs",
"0011 0101 ssN0 dddd disp16","ldl rrd,rs(disp16)",
"0111 0101 ssN0 dddd 0000 xxxx 0000 0000","ldl rrd,rs(rx)",
"0001 1100 ddN0 1001 0000 ssss 0000 nminus1","ldm @rd,rs,n",
"0101 1100 ddN0 1001 0000 ssN0 0000 nminus1 address","ldm address(rd),rs,n",
"0101 1100 0000 1001 0000 ssss 0000 nminus1 address","ldm address,rs,n",
"0001 1100 ssN0 0001 0000 dddd 0000 nminus1","ldm rd,@rs,n",
"0101 1100 ssN0 0001 0000 dddd 0000 nminus1 address","ldm rd,address(rs),n",
"0101 1100 0000 0001 0000 dddd 0000 nminus1 address","ldm rd,address,n",
"0011 1001 ssN0 0000","ldps @rs",
"1011 1101 dddd imm4","ldk rd,imm4",
"1011 1010 ssN0 0001 0000 rrrr ddN0 0000","ldirb @rd,@rs,rr",
"1011 1011 ssN0 0001 0000 rrrr ddN0 0000","ldir @rd,@rs,rr",

"0111 1001 0000 0000 address","ldps address",
"0111 1001 ssN0 0000 address","ldps address(rs)",
"0011 0011 0000 ssss disp16","ldr disp16,rs",
"0011 0001 0000 dddd disp16","ldr rd,disp16",
"0011 0010 0000 ssss disp16","ldrb disp16,rbs",
"0011 0000 0000 dddd disp16","ldrb rbd,disp16",
"0011 0111 0000 ssss disp16","ldrl disp16,rrs",
"0011 0101 0000 dddd disp16","ldrl rrd,disp16",
"0111 1011 0000 1010","mbit",
"0111 1011 dddd 1101","mreq rd",
"0111 1011 0000 1001","mres",
"0111 1011 0000 1000","mset",
"0001 1001 ssN0 dddd","mult rrd,@rs",
"0101 1001 0000 dddd address","mult rrd,address",
"0101 1001 ssN0 dddd address","mult rrd,address(rs)",
"0001 1001 0000 dddd imm16","mult rrd,imm16",
"1001 1001 ssss dddd","mult rrd,rs",
"0001 1000 ssN0 dddd","multl rqd,@rs",
"0101 1000 0000 dddd address","multl rqd,address",
"0101 1000 ssN0 dddd address","multl rqd,address(rs)",
"0001 1000 0000 dddd imm32","multl rqd,imm32",
"1001 1000 ssss dddd","multl rqd,rrs",
"0000 1101 ddN0 0010","neg @rd",
"0100 1101 0000 0010 address","neg address",
"0100 1101 ddN0 0010 address","neg address(rd)",
"1000 1101 dddd 0010","neg rd",
"0000 1100 ddN0 0010","negb @rd",
"0100 1100 0000 0010 address","negb address",
"0100 1100 ddN0 0010 address","negb address(rd)",
"1000 1100 dddd 0010","negb rbd",
"1000 1101 0000 0111","nop",
"0000 0101 ssN0 dddd","or rd,@rs",
"0100 0101 0000 dddd address","or rd,address",
"0100 0101 ssN0 dddd address","or rd,address(rs)",
"0000 0101 0000 dddd imm16","or rd,imm16",
"1000 0101 ssss dddd","or rd,rs",
"0000 0100 ssN0 dddd","orb rbd,@rs",
"0100 0100 0000 dddd address","orb rbd,address",
"0100 0100 ssN0 dddd address","orb rbd,address(rs)",
"0000 0100 0000 dddd imm8 imm8","orb rbd,imm8",
"1000 0100 ssss dddd","orb rbd,rbs",
"0011 1111 ddN0 ssss","out @rd,rs",
"0011 1011 ssss 0110 imm16","out imm16,rs",
"0011 1110 ddN0 ssss","outb @rd,rbs",
"0011 1010 ssss 0110 imm16","outb imm16,rbs",
"0011 1011 ssN0 1010 0000 aaaa ddN0 1000","outd @rd,@rs,ra",
"0011 1010 ssN0 1010 0000 aaaa ddN0 1000","outdb @rd,@rs,rba",
"0011 1100 ssN0 0010 0000 aaaa ddN0 1000","outib @rd,@rs,ra",
"0011 1100 ssN0 0010 0000 aaaa ddN0 0000","outibr @rd,@rs,ra",
"0001 0111 ssN0 ddN0","pop @rd,@rs",
"0101 0111 ssN0 ddN0 address","pop address(rd),@rs",
"0101 0111 ssN0 0000 address","pop address,@rs",
"1001 0111 ssN0 dddd","pop rd,@rs",
"0001 0101 ssN0 ddN0","popl @rd,@rs",
"0101 0101 ssN0 ddN0 address","popl address(rd),@rs",
"0101 0101 ssN0 0000 address","popl address,@rs",
"1001 0101 ssN0 dddd","popl rrd,@rs",
"0001 0011 ddN0 ssN0","push @rd,@rs",
"0101 0011 ddN0 0000 address","push @rd,address",
"0000 1101 ddN0 1001 imm16","push @rd,imm16",
"1001 0011 ddN0 ssss","push @rd,rs",
"0101 0011 ddN0 ssN0 address","push @rd,address(rs)",
"0001 0001 ddN0 ssN0","pushl @rd,@rs",
"0101 0001 ddN0 0000 address","push @rd,address",
"1001 0001 ddN0 ssss","pushl @rd,rrs",
"0101 0001 ddN0 ssN0 address","pushl @rd,address(rs)",
"0010 0011 ddN0 imm4","res @rd,imm4",
"0110 0011 ddN0 imm4 address","res address(rd),imm4",
"0110 0011 0000 imm4 address","res address,imm4",
"1010 0011 dddd imm4","res rd,imm4",
"0010 0011 0000 ssss 0000 dddd 0000 0000","res rd,rs",
"0010 0010 ddN0 imm4","resb @rd,imm4",
"0110 0010 ddN0 imm4 address","resb address(rd),imm4",
"0110 0010 0000 imm4 address","resb address,imm4",
"1010 0010 dddd imm4","resb rbd,imm4",
"0010 0010 0000 ssss 0000 dddd 0000 0000","resb rbd,rs",
"1000 1101 imm4 0011","resflg imm4",
"1001 1110 0000 cccc","ret cc",
"1011 0011 dddd 0000","rl rd,1",
"1011 0011 dddd 0010","rl rd,2",
"1011 0010 dddd 0000","rlb rbd,1",
"1011 0010 dddd 0010","rlb rbd,2",
"1011 0011 dddd 1000","rlc rd,1",
"1011 0011 dddd 1010","rlc rd,2",
"1011 0010 dddd 1000","rlcb rbd,1",
"1011 0010 dddd 1010","rlcb rbd,2",
"1011 1110 aaaa bbbb","rldb rbb,rba",
"1011 0011 dddd 0100","rr rd,1",
"1011 0011 dddd 0110","rr rd,2",
"1011 0010 dddd 0100","rrb rbd,1",
"1011 0010 dddd 0110","rrb rbd,2",
"1011 0011 dddd 1100","rrc rd,1",
"1011 0011 dddd 1110","rrc rd,2",
"1011 0010 dddd 1100","rrcb rbd,1",
"1011 0010 dddd 1110","rrcb rbd,2",
"1011 1100 aaaa bbbb","rrdb rbb,rba",
"1011 0111 ssss dddd","sbc rd,rs",
"1011 0110 ssss dddd","sbcb rbd,rbs",
"1011 0011 dddd 1011 0000 ssss 0000 0000","sda rd,rs",
"1011 0010 dddd 1011 0000 ssss 0000 0000","sdab rbd,rs",
"1011 0011 dddd 1111 0000 ssss 0000 0000","sdal rrd,rs",
"1011 0011 dddd 0011 0000 ssss 0000 0000","sdl rd,rs",
"1011 0010 dddd 0011 0000 ssss 0000 0000","sdlb rbd,rs",
"1011 0011 dddd 0111 0000 ssss 0000 0000","sdll rrd,rs",
"0010 0101 ddN0 imm4","set @rd,imm4",
"0110 0101 ddN0 imm4 address","set address(rd),imm4",
"0110 0101 0000 imm4 address","set address,imm4",
"1010 0101 dddd imm4","set rd,imm4",
"0010 0101 0000 ssss 0000 dddd 0000 0000","set rd,rs",
"0010 0100 ddN0 imm4","setb @rd,imm4",
"0110 0100 ddN0 imm4 address","setb address(rd),imm4",
"0110 0100 0000 imm4 address","setb address,imm4",
"1010 0100 dddd imm4","setb rbd,imm4",
"0010 0100 0000 ssss 0000 dddd 0000 0000","setb rbd,rs",
"1000 1101 imm4 0001","setflg imm4",
"0011 1100 dddd 0101 imm16","sinb rbd,imm16",
"0011 1101 dddd 0101 imm16","sinb rd,imm16",
"0011 1011 ssN0 1000 0001 aaaa ddN0 1000","sind @rd,@rs,ra",
"0011 1010 ssN0 1000 0001 aaaa ddN0 1000","sindb @rd,@rs,rba",
"0011 1100 ssN0 0001 0000 aaaa ddN0 1000","sinib @rd,@rs,ra",
"0011 1100 ssN0 0001 0000 aaaa ddN0 0000","sinibr @rd,@rs,ra",
"1011 0011 dddd 1001 imm16","sla rd,imm16",
"1011 0010 dddd 1001 imm16","slab rbd,imm16",
"1011 0011 dddd 1101 imm16","slal rrd,imm16",
"1011 0011 dddd 0001 imm16","sll rd,imm16",
"1011 0010 dddd 0001 imm16","sllb rbd,imm16",
"1011 0011 dddd 0101 imm16","slll rrd,imm16",
"0011 1011 ssss 0111 imm16","sout imm16,rs",
"0011 1010 ssss 0111 imm16","soutb imm16,rbs",
"0011 1011 ssN0 1011 0000 aaaa ddN0 1000","soutd @rd,@rs,ra",
"0011 1010 ssN0 1011 0000 aaaa ddN0 1000","soutdb @rd,@rs,rba",
"0011 1100 ssN0 0011 0000 aaaa ddN0 1000","soutib @rd,@rs,ra",
"0011 1100 ssN0 0011 0000 aaaa ddN0 0000","soutibr @rd,@rs,ra",
"1011 0011 dddd 1001 nim16","sra rd,imm16",
"1011 0010 dddd 1001 nim16","srab rbd,imm16",
"1011 0011 dddd 1101 nim16","sral rrd,imm16",
"1011 0011 dddd 0001 nim16","srl rd,imm16",
"1011 0010 dddd 0001 nim16","srlb rbd,imm16",
"1011 0011 dddd 0101 nim16","srll rrd,imm16",
"0000 0011 ssN0 dddd","sub rd,@rs",
"0100 0011 0000 dddd address","sub rd,address",
"0100 0011 ssN0 dddd address","sub rd,address(rs)",
"0000 0010 0000 dddd imm16","sub rd,imm16",
"1000 0011 ssss dddd","sub rd,rs",
"0000 0010 ssN0 dddd","subb rbd,@rs",
"0100 0010 0000 dddd address","subb rbd,address",
"0100 0010 ssN0 dddd address","subb rbd,address(rs)",
"0000 0010 0000 dddd imm8 imm8","subb rbd,imm8",
"1000 0010 ssss dddd","subb rbd,rbs",
"0001 0010 ssN0 dddd","subl rrd,@rs",
"0101 0010 0000 dddd address","subl rrd,address",
"0101 0010 ssN0 dddd address","subl rrd,address(rs)",
"0001 0010 0000 dddd imm32","subl rrd,imm32",
"1001 0010 ssss dddd","subl rrd,rrs",
"1010 1111 dddd cccc","tcc cc,rd",
"1010 1110 dddd cccc","tccb cc,rbd",
"0000 1101 ddN0 0100","test @rd",
"0100 1101 0000 0100 address","test address",
"0100 1101 ddN0 0100 address","test address(rd)",
"1000 1101 dddd 0100","test rd",
"0000 1100 ddN0 0100","testb @rd",
"0100 1100 0000 0100 address","testb address",
"0100 1100 ddN0 0100 address","testb address(rd)",
"1000 1100 dddd 0100","testb rbd",
"0001 1100 ddN0 1000","testl @rd",
"0101 1100 0000 1000 address","testl address",
"1001 1100 dddd 1000","testl rrd",
"1011 1000 ddN0 1000 0000 aaaa ssN0 0000","trdb @rd,@rs,rba",
"1011 1000 ddN0 1100 0000 aaaa ssN0 0000","trdrb @rd,@rs,rba",
"1011 1000 ddN0 0000 0000 rrrr ssN0 0000","trib @rd,@rs,rbr",
"1011 1000 ddN0 0100 0000 rrrr ssN0 0000","trirb @rd,@rs,rbr",
"1011 1000 aaN0 1110 0000 rrrr bbN0 1110","trtdrb @ra,@rb,rbr",
"1011 1000 aaN0 0010 0000 rrrr bbN0 0000","trtib @ra,@rb,rr",
"1011 1000 aaN0 0110 0000 rrrr bbN0 1110","trtirb @ra,@rb,rbr",
"1011 1000 aaN0 1010 0000 rrrr bbN0 0000","trtrb @ra,@rb,rbr",
"0000 1101 ddN0 0110","tset @rd",
"0100 1101 0000 0110 address","tset address",
"0100 1101 ddN0 0110 address","tset address(rd)",
"1000 1101 dddd 0110","tset rd",
"0000 1100 ddN0 0110","tsetb @rd",
"0100 1100 0000 0110 address","tsetb address",
"0100 1100 ddN0 0110 address","tsetb address(rd)",
"1000 1100 dddd 0110","tsetb rbd",
"0000 1001 ssN0 dddd","xor rd,@rs",
"0100 1001 0000 dddd address","xor rd,address",
"0100 1001 ssN0 dddd address","xor rd,address(rs)",
"0000 1001 0000 dddd imm16","xor rd,imm16",
"1000 1001 ssss dddd","xor rd,rs",
"0000 1000 ssN0 dddd","xorb rbd,@rs",
"0100 1000 0000 dddd address","xorb rbd,address",
"0100 1000 ssN0 dddd address","xorb rbd,address(rs)",
"0000 1000 0000 dddd imm8 imm8","xorb rbd,imm8",
"1000 1000 ssss dddd","xorb rbd,rbs",


0,0
}
;

int count()
{
 struct op *p = opt;
 int r = 0;
 while (p->name) 
 {
 r++;
 p++;
 }
 return r;

}
func(a,b)
struct op *a;
struct op *b;
{
return strcmp((a)->name, (b)->name);


}

func1(a,b)
struct op *a;
struct op *b;
{
return strcmp((a)->bits, (b)->bits);


}

/* opcode 

 literal  0000 nnnn insert nnn into stream
 operand  0001 nnnn  insert operand reg nnn into stream
*/

typedef struct tok_struct 
{

char *match;
char *token;
int length;
};



struct tok_struct args[] =
{

{ "address(rs)", "CLASS_X+(ARG_RS)",},
{ "address(rd)", "CLASS_X+(ARG_RD)",},

{ "rs(disp16)","CLASS_BA+(ARG_RS)",},
{ "rd(disp16)","CLASS_BA+(ARG_RD)",},

{ "address", "CLASS_DA",},
{ "rd(rx)", "CLASS_BX+(ARG_RD)",},
{ "rs(rx)","CLASS_BX+(ARG_RS)",},
{ "disp16", "CLASS_DISP",},
{ "disp12", "CLASS_DISP",},
{ "disp7", "CLASS_DISP",},
{ "disp8", "CLASS_DISP",},
{ "flags","CLASS_FLAGS",},
{ "imm16", "CLASS_IMM+(ARG_IMM16)",},
{ "imm32", "CLASS_IMM+(ARG_IMM32)",},
{ "imm4", "CLASS_IMM +(ARG_IMM4)",},
{ "n", "CLASS_IMM + (ARG_IMMN)",},
{ "ctrl", "CLASS_CTRL",},
{ "rba", "CLASS_REG_BYTE+(ARG_RA)",},
{ "rbb", "CLASS_REG_BYTE+(ARG_RB)",},
{ "rbd", "CLASS_REG_BYTE+(ARG_RD)",},
{ "rbs", "CLASS_REG_BYTE+(ARG_RS)",},
{ "rbr", "CLASS_REG_BYTE+(ARG_RR)",},

{ "rrd","CLASS_REG_LONG+(ARG_RD)",},
{ "rrs","CLASS_REG_LONG+(ARG_RS)",},

{ "rqd", "CLASS_REG_QUAD+(ARG_RD)",},

{ "rd", "CLASS_REG_WORD+(ARG_RD)",},
{ "rs", "CLASS_REG_WORD+(ARG_RS)",},

{ "@rd", "CLASS_IR+(ARG_RD)",},
{ "@ra", "CLASS_IR+(ARG_RA)",},
{ "@rb", "CLASS_IR+(ARG_RB)",},
{ "@rs", "CLASS_IR+(ARG_RS)",},

{ "imm8", "CLASS_IMM+(ARG_IMM8)",},
{ "i2", "CLASS_IMM+(ARG_IMM2)",},
{ "cc", "CLASS_CC",},

{ "rr", "CLASS_REG_WORD+(ARG_RR)",},
{ "ra", "CLASS_REG_WORD+(ARG_RA)",},
{ "rs", "CLASS_REG_WORD+(ARG_RS)",},

{ "1", "CLASS_IMM+(ARG_IMM_1)",},
{ "2", "CLASS_IMM+(ARG_IMM_2)",},

 0,0
 };

struct tok_struct toks[] = 
{ 
 "0000", "CLASS_BIT+0",1,
 "0001", "CLASS_BIT+1",1,
 "0010", "CLASS_BIT+2",1,
 "0011", "CLASS_BIT+3",1,
 "0100", "CLASS_BIT+4",1,
 "0101", "CLASS_BIT+5",1,
 "0110", "CLASS_BIT+6",1,
 "0111", "CLASS_BIT+7",1,
 "1000", "CLASS_BIT+8",1,
 "1001", "CLASS_BIT+9",1,
 "1010", "CLASS_BIT+10",1,
 "1011", "CLASS_BIT+11",1,
 "1100", "CLASS_BIT+12",1,
 "1101", "CLASS_BIT+13",1,
 "1110", "CLASS_BIT+14",1,
 "1111", "CLASS_BIT+15",1,

 "ssss", "CLASS_REG+(ARG_RS)",1,
 "dddd", "CLASS_REG+(ARG_RD)",1,
 "aaaa", "CLASS_REG+(ARG_RA)",1,
 "bbbb", "CLASS_REG+(ARG_RB)",1,
 "rrrr", "CLASS_REG+(ARG_RR)",1,

 "ssN0", "CLASS_REGN0+(ARG_RS)",1,
 "ddN0", "CLASS_REGN0+(ARG_RD)",1,
 "aaN0", "CLASS_REGN0+(ARG_RA)",1,
 "bbN0", "CLASS_REGN0+(ARG_RB)",1,
 "rrN0", "CLASS_REGN0+(ARG_RR)",1,

 "cccc", "CLASS_CC",1,
 "nnnn", "CLASS_IMM+(ARG_IMMN)",1,
 "xxxx", "CLASS_REG+(ARG_RX)",1,
 "xxN0", "CLASS_REGN0+(ARG_RX)",1,
 "nminus1", "CLASS_IMM+(ARG_IMMNMINUS1)",1,

 "disp16", "CLASS_DISP+(ARG_DISP16)",4,
 "disp12", "CLASS_DISP+(ARG_DISP12)",3,
 "flags", "CLASS_FLAGS",1,
 "address", "CLASS_ADDRESS",4,
 "imm4", "CLASS_IMM+(ARG_IMM4)",1,
 "imm8", "CLASS_IMM+(ARG_IMM8)",2,
 "imm16", "CLASS_IMM+(ARG_IMM16)",4,
 "imm32", "CLASS_IMM+(ARG_IMM32)",8,
 "nim16", "CLASS_IMM+(ARG_NIM16)",4,
 "0ccc", "CLASS_0CCC",1,
 "1ccc", "CLASS_1CCC",1,
 "disp8", "CLASS_DISP8",2,
 "0disp7", "CLASS_0DISP7",2,
 "1disp7", "CLASS_1DISP7",2,
 "01ii", "CLASS_01II",1,
 "00ii", "CLASS_00II",1,
 0,0

 };


char *translate(table, x, length)
struct tok_struct *table;
char *x;
int *length;
{

 int found;
 found = 0;
 while (table->match) 
 {
 int l = strlen(table->match);
 if (strncmp(table->match, x, l) == 0) 
 {
 /* Got a hit */
 printf("%s", table->token);
 *length += table->length;
 return x + l;
 }

 table++;
 }
 fprintf(stderr,"Can't find %s\n", x);
 while (*x) 
 x++;
 return x;
}


void
 chewbits(bits, length)
char *bits;
int *length;
{
 int i;
 int found;
 
 int n = 0;
 *length = 0;
 printf("{");
 while (*bits) 
 {
 while (*bits == ' ') 
 {
 bits++;
 }
 bits = translate(toks, bits, length);
 n++;
 printf(",");

 }
 while (n < BYTE_INFO_LEN-1) {
 printf("0,");
 n++;
 }
 printf("}");
}

doreg(x)
char *x;
{
printf("REGH %c ", x[0]);

}
int chewname(name)
char *name;
{
 char *n;
 int nargs = 0;
 int nbytes= 0;
 n = name;
 printf("\"");
 while (*n && !iswhite(*n)) {
   printf("%c", *n );
   n++;
 }
 printf("\",{");
 /* Scan the operands and make entires for them -remember indirect things */
 while (*n) {
 int d;
 while (*n == ',' || iswhite(*n))
     n++;
 nargs++;
 n= translate(args, n, &d);
 printf(",");
 }
 if (nargs == 0) {
 printf("0");
 }
 printf("},");
 return nargs;
}
sub(x,c)
char *x;
char c;
{
 while (*x) 
 {
 if (x[0] == c 
 && x[1] == c 
 && x[2] == c 
 && x[3] == c) {
 x[2] = 'N';
 x[3] = '0';
 }
 x++;
 }
}
internal()
{
 int c = count();
 struct op *new = malloc(sizeof(struct op) * c);
 struct op *p = opt;
 memcpy(new, p, c * sizeof(struct op));

 /* sort all names in table alphabetically */
 qsort(new, c, sizeof(struct op), func);
 p = new;
 while (p->name) {
 /* If there are any @rs, sub the ssss into a ssn0,
 (rs), (ssn0)
 */
 int loop = 1;
 while (loop) {
 char *s = p->name;
 loop = 0;
 while (*s) {
 if(s[0] == '@') {
  char c ;
  /* skip the r and sub the string */
  s++;
  c = s[1];
  sub(p->bits,c);
 }
 if (s[0] == '(' && s[3] == ')')
 {
  sub(p->bits, s[2]);
 }
 if (s[0] == '(')
 {
  sub(p->bits, s[-1]);
 }

 s++;
 }
  
 }
 printf("\"%s\",\"%s\",\n", p->bits, p->name);
 p++;
 }
}
gas()
{

 int c = count();
 int i;
 struct op *p = opt;
 int idx = 0;
 char *oldname = "";
 struct op *new = malloc(sizeof(struct op) * c);

 memcpy(new, p, c * sizeof(struct op));

 /* sort all names in table alphabetically */
 qsort(new, c, sizeof(struct op), func);

 printf("   /* THIS FILE IS AUTOMAGICALLY GENERATED, DON'T EDIT IT */\n");

 printf("#define ARG_MASK 0x0f\n"); 
 printf("#define ARG_RS 0x01\n");
 printf("#define ARG_RD 0x02\n");
 printf("#define ARG_RA 0x03\n");
 printf("#define ARG_RB 0x04\n");
 printf("#define ARG_RR 0x05\n");
 printf("#define ARG_RX 0x06\n");
 printf("#define ARG_IMM4 0x01\n");
 printf("#define ARG_IMM8 0x02\n");
 printf("#define ARG_IMM16 0x03\n");
 printf("#define ARG_IMM32 0x04\n");
 printf("#define ARG_IMMN 0x05\n");
 printf("#define ARG_IMMNMINUS1 0x05\n");
 printf("#define ARG_IMM_1 0x06\n");
 printf("#define ARG_IMM_2 0x07\n");
 printf("#define ARG_DISP16 0x08\n");
 printf("#define ARG_NIM16 0x09\n");
 printf("#define ARG_IMM2 0x0a\n");
 printf("#define ARG_DISP12 0x0b\n");
 printf("#define ARG_DISP8 0x0c\n");

 printf("#define CLASS_MASK 0xfff0\n");
 printf("#define CLASS_X 0x10\n");
 printf("#define CLASS_BA 0x20\n");
 printf("#define CLASS_DA 0x30\n");
 printf("#define CLASS_BX 0x40\n");
 printf("#define CLASS_DISP 0x50\n");
 printf("#define CLASS_IMM 0x60\n");
 printf("#define CLASS_CC 0x70\n");
 printf("#define CLASS_CTRL 0x80\n");
 printf("#define CLASS_ADDRESS 0xd0\n");
 printf("#define CLASS_0CCC 0xe0\n");
 printf("#define CLASS_1CCC 0xf0\n");
 printf("#define CLASS_0DISP7 0x100\n");
 printf("#define CLASS_1DISP7 0x200\n");
 printf("#define CLASS_01II 0x300\n");
 printf("#define CLASS_00II 0x400\n");
 printf("#define CLASS_BIT 0x500\n");
 printf("#define CLASS_FLAGS 0x600\n");
 printf("#define CLASS_IR 0x700\n");
 printf("#define CLASS_DISP8 0x800\n");


 printf("#define CLASS_REG 0x7000\n");
 printf("#define CLASS_REG_BYTE 0x2000\n");
 printf("#define CLASS_REG_WORD 0x3000\n");
 printf("#define CLASS_REG_QUAD 0x4000\n");
 printf("#define CLASS_REG_LONG 0x5000\n");
 printf("#define CLASS_REGN0 0x8000\n");



#if 0
 for (i = 0; toks[i].token; i++)
 printf("#define %s\t0x%x\n",toks[i].token,i*16);
#endif
 printf("typedef struct {\n");
 printf("char *name;\n");
 printf("unsigned short arg_info[4];\n");
 printf("unsigned short byte_info[%d];\n", BYTE_INFO_LEN);
 printf("int noperands;\n");
 printf("int length;\n");
 printf("int idx;\n");
 printf("} opcode_entry_type;\n");
 printf("#ifdef DEFINE_TABLE\n");
 printf("opcode_entry_type z8k_table[] = {\n");

 while (new->name) {
 int nargs;
 int length;
 printf("{");
 nargs = chewname(new->name);

printf("\n\t");
 chewbits(new->bits, &length);
 length /=2;
 if (length &1) fail();

 printf(",%d,%d,%d", nargs, length, idx);
 if(strcmp(oldname, new->name)) 
 {
 idx++;
 oldname = new->name;
 }
 printf("},\n");
 new++;
 }
 printf("0,0};\n");
 printf("#endif\n");
}
main(ac,av)
int ac;
char **av;
{
 struct op *p = opt;

 if (ac == 2 && strcmp(av[1],"-t")==0) 
 {
 internal();
 }
else if (ac == 2 && strcmp(av[1],"-h")==0) 
 {
 while (p->name) {
 printf("%-25s\t%s\n", p->name, p->bits);
 p++;
 }
 }

 else if (ac== 2 && strcmp(av[1], "-a") == 0)
 {
 gas();
 }
 else if (ac== 2 && strcmp(av[1], "-d") == 0)
 {
 /*dis();*/
 }
 else {
 printf("Usage: %s -t\n", av[0]);
 printf("-t : generate new z8.c internal table\n");
 printf("-a : generate new table for gas\n");
 printf("-d : generate new table for disassemble\n");
 printf("-h : generate new table for humans\n");
 }


}

fail()
{
}


