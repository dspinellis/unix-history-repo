#$Header: Makefile,v 1.12 83/09/12 15:26:36 layer Exp $
#
#makefile for misc things -- lxref and tags

DESTDIR =
ObjDir = /usr/ucb
Liszt = ${ObjDir}/liszt
XTR = /na/lbc/bin/extract

CTESrc = chead.l cmacros.l const.l

Src =	array.l datab.l decl.l expr.l fixnum.l funa.l funb.l func.l io.l \
	vector.l instr.l tlev.l util.l lversion.l

LxrefSrc = lxref.l

AllSrc =  Makefile ChangeLog cmake.l ${CTESrc} ${Src} ${LxrefSrc} ltags tags

.DEFAULT:lxref

xtra:	${DotSSrc}

doc:
	${XTR} -clE "(load 'const.l)(load 'chead.l)" $(CTESrc) $(Src) > doc

doc.n:
	${XTR} -cnlE "(load 'const.l)(load 'chead.l)" $(CTESrc) $(Src) > doc.n

index:
	${XTR} -cli\
	-E "(progn (chdir'vax)(load'../cmacros.l)(load'../chead.l)(chdir'..))"\
		$(CTESrc) $(Src) > index

index.n:
	${XTR} -cnli -T "Liszt Index" -p 8\
	-E "(progn (chdir'vax)(load'lisprc.l)(load'../cmacros.l)(load'../chead.l)(chdir'..))"\
	$(CTESrc) $(Src) > index.n

lxref:	lxref.l
	${Liszt} -xrq -o lxref lxref.l

install: lxref
	mv lxref ${DESTDIR}${ObjDir}/lxref

scriptcatall: ${DistSrc}
	@../scriptcat . liszt ${AllSrc}

copysource: ${AllSrc}
	(tar cf - ${AllSrc} | (cd ${CopyTo} ; tar xf -))

copyobjects: ${AllObj}
	(tar cf - ${AllObj} | (cd ${CopyTo} ; tar xf -))
