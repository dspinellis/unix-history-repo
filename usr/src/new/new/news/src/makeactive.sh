: "Create active file and newsgroup heirarchy for new machine"
: "Usage: sh makeactive.sh friends-active-file LIBDIR SPOOLDIR"
cp $1 /tmp/active
cd $2
mv /tmp/active active
ed - active << DONE
g/ /s/ .*//
w /tmp/active
g/$/s/$/ 00000/
w
e /tmp/active
1,$s;\.;/;
w
1,$s;/[^/]*$;;
w /tmp/parents.in
q
DONE
sort -u < /tmp/parents.in > /tmp/parents
cd $3
echo "expect some error messages about dirs that already exist"
: "test is to ignore error exits"
test mkdir `cat /tmp/parents`
test mkdir `cat /tmp/active`
