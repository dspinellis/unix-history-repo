/*
 * Copyright (c) 1983, 1985, 1991 Peter J. Nicklin.
 * Copyright (c) 1991 Version Technology.
 * All Rights Reserved.
 *
 * $License: VT.1.1 $
 * Redistribution and use in source and binary forms,  with or without
 * modification,  are permitted provided that the following conditions
 * are met:  (1) Redistributions of source code must retain the  above
 * copyright  notice,  this  list  of  conditions  and  the  following
 * disclaimer.  (2) Redistributions in binary form must reproduce  the
 * above  copyright notice,  this list of conditions and the following
 * disclaimer in the  documentation  and/or other  materials  provided
 * with  the  distribution.  (3) All advertising materials  mentioning
 * features or  use  of  this  software  must  display  the  following
 * acknowledgement:  ``This  product  includes  software  developed by
 * Version Technology.''  Neither the name of Version  Technology  nor
 * the  name  of  Peter J. Nicklin  may  be used to endorse or promote
 * products derived from this software without specific prior  written
 * permission.
 *
 * THIS SOFTWARE IS PROVIDED BY VERSION TECHNOLOGY ``AS IS''  AND  ANY
 * EXPRESS OR IMPLIED WARRANTIES,  INCLUDING,  BUT NOT LIMITED TO, THE
 * IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL  VERSION  TECHNOLOGY  BE
 * LIABLE  FOR ANY DIRECT,  INDIRECT,  INCIDENTAL, SPECIAL, EXEMPLARY,
 * OR  CONSEQUENTIAL DAMAGES   (INCLUDING,   BUT   NOT   LIMITED   TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;  LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY
 * OF  LIABILITY,  WHETHER  IN  CONTRACT,  STRICT LIABILITY,  OR  TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE)  ARISING  IN ANY WAY OUT OF THE
 * USE OF THIS SOFTWARE,  EVEN  IF  ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 * Report problems and direct questions to nicklin@netcom.com
 *
 * $Header: defaultsfx.h,v 4.3 91/11/25 19:45:47 nicklin Exp $
 *
 * Default suffixes
 *
 * Author: Peter J. Nicklin
 */
".c",   SFXSRC,  INCLUDE_C,	  "C",	 /* C */
".cc",  SFXSRC,  INCLUDE_CXX,	  "C++", /* C++ */
".cxx", SFXSRC,  INCLUDE_CXX,	  "C++", /* C++ */
".cpp", SFXSRC,  INCLUDE_CXX,	  "C++", /* C++ */
".C",   SFXSRC,  INCLUDE_CXX,	  "C++", /* C++ */
".F",   SFXSRC,  INCLUDE_FORTRAN, "F",	 /* Fortran */
".f",   SFXSRC,  INCLUDE_FORTRAN, "F",	 /* Fortran */
".h",   SFXHEAD, INCLUDE_NONE,	   NULL, /* header */
".hxx", SFXHEAD, INCLUDE_NONE,	   NULL, /* header */
".hpp", SFXHEAD, INCLUDE_NONE,	   NULL, /* header */
".H",   SFXHEAD, INCLUDE_NONE,	   NULL, /* header */
".i",   SFXHEAD, INCLUDE_NONE,	   NULL, /* Pascal include */
".l",   SFXSRC,  INCLUDE_C,	  "C",	 /* Lex */
".o",   SFXOBJ,  INCLUDE_NONE,	   NULL, /* object */
".p",   SFXSRC,  INCLUDE_PASCAL,  "P",	 /* Pascal */
".r",   SFXSRC,  INCLUDE_FORTRAN, "F",	 /* Ratfor */
".s",   SFXSRC,  INCLUDE_NONE,	   NULL, /* Assembler */
".y",   SFXSRC,  INCLUDE_C,	  "C",	 /* Yacc */
NULL,   SFXNULL, INCLUDE_NONE,	   NULL	 /* mandatory last line */
