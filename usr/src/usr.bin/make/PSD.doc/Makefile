#
LOCALS		= tutorial.ms tutorial.psc \
                  pmake.mansp pmake.psc \
		  Lst.mansp Lst.psc Makefile makefile tmac.ansp \
                  cctrl.8 customs.8 importquota.8 reginfo.1

SUBDIRS		= index ex1 ex2 customs


tutorial.t	: index/tmac.index tutorial.ms 
	ditroff -Plw -ms -t $(.ALLSRC) > tut.temp
	index/index -Plw -t tut.temp > $(.TARGET)
	rm -f tut.temp

print 		: tutorial.t
	lpr -Plw -h -n tutorial.t

package		::
	if [ ! -d $(PKGDIR) ]; then
		mkdir $(PKGDIR)
	fi
	cp $(LOCALS) $(PKGDIR)
	for i in $(SUBDIRS); do
		(cd $i; $(MAKE) "PKGDIR=$(PKGDIR)/$i" package)
	done
