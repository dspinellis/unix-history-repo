FONTDIR=/usr/local/lib/groff/font
DEVICEDIR=$(FONTDIR)/devX75

all:

install:
	-[ -d $(FONTDIR) ] || mkdir $(FONTDIR)
	-[ -d $(DEVICEDIR) ] || mkdir $(DEVICEDIR)
	cp DESC eqnchar $(DEVICEDIR)
	dir=`pwd`; \
	cd $(DEVICEDIR); \
	$$dir/../xtotroff -g $$dir/FontMap
	-[ -d $(DEVICEDIR)-12 ] || mkdir $(DEVICEDIR)-12
	cp eqnchar $(DEVICEDIR)-12
	sed -e 's/unitwidth 10/unitwidth 12/' DESC >$(DEVICEDIR)-12/DESC
	sed -e 's/100-75-75/120-75-75/' FontMap >FontMap-12
	dir=`pwd`; \
	cd $(DEVICEDIR)-12; \
	$$dir/../xtotroff -g $$dir/FontMap-12
	rm FontMap-12
