: change truncated group names to their full length
: not tested on a real 2.9 system
if test $# -lt 2
then
	echo Useage: cvt.names.sh libdir spooldir
fi
echo Expect some errors about missing directories
cd $2
mv fa/laser-lover fa/laser-lovers
mv net/announce/n net/announce/newusers
mv net/astro/expe net/astro/expert
mv net/games/rogu net/games/rogue
mv net/games/triv net/games/trivia
mv net/games/vide net/games/video
mv net/lang/pasca net/lang/pascal
mv net/lang/prolo net/lang/prolog
mv net/mail/heade net/mail/headers
mv net/mail/msggr net/mail/msggroup
mv net/micro/appl net/micro/apple
mv net/micro/atar net/micro/atari
mv net/micro/trs- net/micro/trs-80
mv net/music/clas net/music/classical
mv net/news/confi net/news/config
mv net/news/newsi net/news/newsite
mv net/nlang/celt net/nlang/celts
mv net/nlang/gree net/nlang/greek
mv net/rec/skydiv net/rec/skydive
mv net/religion/j net/religion/jewish
mv net/sport/base net/sport/baseball
mv net/sport/foot net/sport/football
mv net/sport/hock net/sport/hockey
mv net/sport/hoop net/sport/hoops
mv net/unix-wizar net/unix-wizards
mv net/works/apol net/works/apollo

cd $1
sed 's/^fa.laser-lover /fa.laser-lovers /
s/^net.announce.n /net.announce.newusers /
s/^net.astro.expe /net.astro.expert /
s/^net.games.rogu /net.games.rogue /
s/^net.games.triv /net.games.trivia /
s/^net.games.vide /net.games.video /
s/^net.lang.pasca /net.lang.pascal /
s/^net.lang.prolo /net.lang.prolog /
s/^net.mail.heade /net.mail.headers /
s/^net.mail.msggr /net.mail.msggroup /
s/^net.micro.appl /net.micro.apple /
s/^net.micro.atar /net.micro.atari /
s/^net.micro.trs- /net.micro.trs-80 /
s/^net.music.clas /net.music.classical /
s/^net.news.confi /net.news.config /
s/^net.news.newsi /net.news.newsite /
s/^net.nlang.celt /net.nlang.celts /
s/^net.nlang.gree /net.nlang.greek /
s/^net.rec.skydiv /net.rec.skydive /
s/^net.religion.j /net.religion.jewish /
s/^net.sport.base /net.sport.baseball /
s/^net.sport.foot /net.sport.football /
s/^net.sport.hock /net.sport.hockey /
s/^net.sport.hoop /net.sport.hoops /
s/^net.unix-wizar /net.unix-wizards /
s/^net.works.apol /net.works.apollo /' active > nactive
mv active oactive
mv nactive active
