:
: make dtext.dat from individual parts
:
if test -s dtext.dat
 then exit 0
fi
for fname in dtext.aa dtext.ab dtext.ac dtext.ad dtext.ae dtext.af
do
 if test -f $fname
  then continue
  else echo "maketxt.sh: missing file: $fname"; exit 1
 fi
done
cat dtext.aa dtext.ab dtext.ac dtext.ad dtext.ae dtext.af >dtext.dat.uu
rm dtext.aa dtext.ab dtext.ac dtext.ad dtext.ae dtext.af
uudecode dtext.dat.uu
rm dtext.dat.uu
exit 0
