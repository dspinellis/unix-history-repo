HDRS	      = construct.tbl \
		cwp.tbl \
		develop.tbl \
		help.tbl \
		label.tbl \
		layer.tbl \
		pathname.tbl \
		print.tbl \
		src.tbl \
		test.tbl

MAKEFILE      = Makefile

PAGE	      =

PRINT	      = vtroff

SRCS	      = 0.abstract.ms \
		1.intro.ms \
		2.simple.ms \
		3.advanced.ms \
		4.manage.ms \
		5.retro.ms \
		6.acknow.ms \
		7.ref.ms \
		A.appendix.ms \
		B.appendix.ms \
		C.appendix.ms \
		D.appendix.ms \
		TOC.ms

SUFFIX	      = .ms:s .tbl:h

all:		print

clean:;

depend:;	@mkmf -f $(MAKEFILE)

diagrams:;	gprint Fig1.g
		gprint -W -1 12 Fig2.g Fig3.g Fig4.g Fig5.g
		gprint -W -1 12 Fig6.g Fig7.g Fig8.g
		gprint -W Fig9.g Fig10.g Fig11.g Fig12.g Fig13.g

index:;

install:;

print: 		$(SRCS) $(HDRS)
		@soelim $(SRCS) | tbl | $(PRINT) -o$(PAGE) -ms
		@touch print

update:;
###
