: "Convert from B 2.9 to 2.10 spool format, with dots turned into slashes."
L=$1
S=$2
echo Do not be surprised at error messages for local or duplicate groups.
cd $S
cat $L/active | sed 's/\.[^.]*$//' | sort -u > parents
mkdir `tr '.' '/' < parents`
mkdir `tr '.' '/' < $L/active | grep /`
