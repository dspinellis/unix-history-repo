REM Simple brute force command file for building gawk under msdos
REM
REM *** This has only been using MSC 5.1 ***
REM
REM Written by Arnold Robbins, May 1991
REM Modified by Scott Deifik, July, 1992
REM Based on earlier makefile for dos
REM
REM Copyright (C) 1986, 1988, 1989, 1991 the Free Software Foundation, Inc.
REM 
REM This file is part of GAWK, the GNU implementation of the
REM AWK Progamming Language.
REM 
REM GAWK is free software; you can redistribute it and/or modify
REM it under the terms of the GNU General Public License as published by
REM the Free Software Foundation; either version 2 of the License, or
REM (at your option) any later version.
REM 
REM GAWK is distributed in the hope that it will be useful,
REM but WITHOUT ANY WARRANTY; without even the implied warranty of
REM MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
REM GNU General Public License for more details.
REM 
REM You should have received a copy of the GNU General Public License
REM along with GAWK; see the file COPYING.  If not, write to
REM the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
REM
REM  compile debug flags: -DDEBUG -DFUNC_TRACE -DMEMDEBUG -Zi -Od
REM			  
REM
cl -c -AL array.c
cl -c -AL awktab.c
cl -c -AL builtin.c
cl -c -AL dfa.c
cl -c -AL eval.c
cl -c -AL field.c
cl -c -AL io.c
cl -c -AL iop.c
cl -c -AL main.c
cl -c -AL missing.c
cl -c -AL msg.c
cl -c -AL node.c
cl -c -AL popen.c
cl -c -AL re.c
REM You can ignore the warnings you will get
cl -c -AL regex.c
cl -c -AL version.c
REM
REM link debug flags: /CO /NOE /NOI /st:30000
REM
link @names.lnk,gawk.exe /NOE /NOI /st:30000;
