: "Install active file from another site"
A=$1
L=$2
S=$3
sed 's/ .*//' < $A > /tmp/cvt$$
cp /tmp/cvt$$ $L/active
sh cvt.mkdirs.sh $L $S
mv $L/active $L/oactive
sed 's/$/ 00000/' < $L/oactive > $L/active
