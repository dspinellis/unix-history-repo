rm -f postnews defs.h Makefile
cp postnews.v7 postnews
cp defs.dist defs.h
chmod u+w defs.h
ed defs.h << DONE
/INTERNET/s;/\* ;;
/OLD/s;/\* ;;
/ROOTID/s/10/1/
/MYORG/s/".*"/"U.C. Berkeley"/
w
q
DONE
cp Makefile.v7 Makefile
chmod 666 Makefile
ed Makefile << DONE
/NEWSUSR/s/news/daemon/
/NEWSGRP/s/news/daemon/
w
q
DONE
echo "Warning: if this changed Makefile, abort and restart make"
