#
if (-r toc.nr) rm toc.nr
echo '.th "TABLE OF CONTENTS" INGRES 3/1/81' > toc.nr
chdir quel
csh ../toc.sh *.nr
chdir ../unix
csh ../toc.sh *.nr
chdir ../files
csh ../toc.sh *.nr
chdir ../error
csh ../toc.sh *.nr
