{*** ROUTINE DECLARATIONS ***}

ROUTINE inside;
ROUTINE ei8;
ROUTINE cdrop;
ROUTINE ctake;
ROUTINE exitg;
ROUTINE skore;
ROUTINE darkq;
ROUTINE darkt;
ROUTINE objl;
ROUTINE objs;
ROUTINE onlmp;
ROUTINE scord;
ROUTINE ratng;
ROUTINE ratnx;
ROUTINE scads;

{*** GLOBALS ***}

kludge = 2;
onoff = 2;
signed = 2;
tlakst = 2;
fedmky = 2;
ropstf = 2;
readl = 3;
wellbt = 2;
cutflg = 2;
earplg = 3;
tookbt = 2;
movbl = 1;
darkbt = 2;
rtied = 3;
stond = 2;
killd = 2;
wiped = 2;
blokd = 2;
tooky = 2;
abrad = 3;
hitbr = 2;
dugbt = 2;
debug = 10;
VISIT = 11;

rarea = 19;
pfound = 17;
pplace = 18;
pscore = 17;
pmax = 18;
ratvl = 19;
weight = 20;

Myloc = 15;

{*** DEAD-END ROUTINES ***}


cg = ($say "Can't go that way.
");

tosml = ($say "The hole is too small for you to fit through.
");

dumdir = ($say "I don't know which direction that is.
");

tohigh = ($say "The hole is too high for you to reach.
");

nothe = ($say "This section is not implemented yet.\n");

{*** MUSEUM ROOMS ***}

NOUN mrm1;
NOUN mrm2;
NOUN mrm3;
NOUN mrm4;
NOUN mrm5;
NOUN mrm6;

{*** INDIAN ROOMS ***}

NOUN irm1;
NOUN irm2;
NOUN irm3;
NOUN irm4;
NOUN irm5;
NOUN irm6;
NOUN irm7;
NOUN irm8;
NOUN irm9;
NOUN irm10;
NOUN irm11;
NOUN irm12;
NOUN irm13;
NOUN irm14;
NOUN irm15;
NOUN irm16;
NOUN irm17;
NOUN irm18;
NOUN irm19;
NOUN irm20;
NOUN irm21;
NOUN irm22;
NOUN irm23;

{*** PREHISTORIC ROOMS ***}

NOUN prehs;
NOUN prm1(prehs);
prm1(darkbt) = 1;
NOUN prm2(prehs);
NOUN prm3(prehs);
NOUN prm4(prehs);
NOUN prm5(prehs);
prm5(darkbt) = 1;
NOUN prm6(prehs);
prm6(darkbt) = 1;
NOUN prm7(prehs);
prm7(darkbt) = 1;
NOUN prm8(prehs);
prm8(darkbt) = 1;
NOUN prm9(prehs);
NOUN prm10(prehs);
NOUN prm11(prehs);
NOUN prm12(prehs);
NOUN prm13(prehs);
NOUN prm14(prehs);
NOUN prm15(prehs);
NOUN prm16(prehs);
NOUN prm17(prehs);
prm17(darkbt) = 1;
NOUN prm18(prehs);
prm18(darkbt) = 1;
NOUN prm19(prehs);
prm19(darkbt) = 1;
NOUN prm20(prehs);
prm20(darkbt) = 1;
NOUN prm21(prehs);
NOUN prm22(prehs);
NOUN prm23(prehs);

NOUN .ME;
.ME(SDESC) = ($exit 0);
.ME(LDESC) = ($exit 0);

{*** PREPOSITIONS ***}

PREP into;

{*** OBJECTS ***}

NOUN pen(mrm2);
pen(movbl) = 1;
pen(weight) = 1;
pen(LDESC) = ($say "There is a ball-point pen here.\n");
pen(SDESC) = ($say "ball-point pen");

NOUN paper(mrm2);
release = paper;
form = paper;
paper(movbl) = 1;
paper(weight) = 1;
paper(LDESC) = (($eq ($prop paper readl) 0) :
		($say "There is a slip of paper here.\n"))
		(($eq ($prop paper readl) 1) :
		($say "There is a release form here.\n"));
paper(SDESC) = (($eq ($prop paper readl) 0) : ($say "slip of paper"))
		(($eq ($prop paper readl) 1) : ($say "release form"));

NOUN spices(irm6);
spices(movbl) = 1;
spices(weight) = 3;
spices(LDESC) = ($say "There is a bag of spices here.\n");
spices(SDESC) = ($say "spices");
spices(rarea) = 1;
spices(pfound) = 5;
spices(pplace) = 9;

NOUN rupees(irm7);
coins=rupees;
rupees(movbl) = 1;
rupees(weight) = 5;
rupees(LDESC) = ($say "There is a bag of rupees (Indian coins) here.\n");
rupees(SDESC) = ($say "rupees (coins)");
rupees(rarea) = 1;
rupees(pfound) = 5;
rupees(pplace) = 15;

NOUN coconut(irm3);
coconut(movbl) = 1;
coconut(weight) = 3;
coconut(LDESC) = ($say "There is a large coconut here.\n");
coconut(SDESC) = ($say "coconut");

NOUN rope(mrm1);
rope(movbl) = 1;
rope(weight) = 3;
rope(LDESC) = (($eq ($prop rope ropstf) 0):
		($say "There is a coil of rope here.\n"))
		(($eq ($prop rope ropstf) 1) :
		($say "There is a long piece of rope here, magically
rising up in mid-air.\n"));
rope(SDESC) = ($say "rope");

NOUN clarinet(irm2);
clarinet(movbl) = 1;
clarinet(weight) = 2;
clarinet(LDESC) = ($say "There is a clarinet here.\n");
clarinet(SDESC) = ($say "clarinet");

NOUN cobra(irm22);
cobra(LDESC) = ($say "There is a king cobra at the bottom of the pit.\n");
cobra(SDESC) = ($say "cobra");
snake = cobra;
elapid = cobra;

NOUN river;

NOUN statue;

NOUN banana(irm3);
banana(movbl) = 1;
banana(weight) = 2;
banana(LDESC) = ($say "There is a banana here.\n");
banana(SDESC) = ($say "banana");

NOUN peel;
peel(movbl) = 1;
peel(weight) = 1;
peel(LDESC) = ($say "On the ground, there is a banana peel.\n");
peel(SDESC) = ($say "banana peel");

NOUN vishnu;

NOUN mara;

NOUN lakshmi;

NOUN crocodile;

NOUN rhino(irm13);
rhino(LDESC) = (($eq ($prop rhino cutflg) 1) :
	($say "There is a rhino sleeping here with his horn removed.\n"))
	(($eq ($prop rhino cutflg) 0) : 
	($say "There is a rhinoceros sleeping in the corner.\n"))
	(($eq ($prop rhino earplg) 1) :
	($say "There are two pieces of cotton stuck in his ears.\n"));
rhino(SDESC) = ($say "sleeping rhino");

NOUN saw(irm2);
saw(movbl) = 1;
saw(weight) = 2;
saw(LDESC) = ($say "There is a hack-saw on the ground.\n");
saw(SDESC) = ($say "hack-saw");
hacksaw = saw;

NOUN horn;
horn(movbl) = 1;
horn(weight) = 2;
horn(LDESC) = ($say "There is a rhinoceros horn here.\n");
horn(SDESC) = ($say "rhino horn");
horn(rarea) = 1;
horn(pfound) = 14;
horn(pplace) = 7;

NOUN cotton;
cotton(movbl) = 1;
cotton(weight) = 1;
cotton(LDESC) = ($say "There is a large wad of cotton on the ground.\n");
cotton(SDESC) = ($say "cotton");

NOUN lattice(irm13);
lattice(LDESC) = ($exit 0);
lattice(SDESC) = ($exit 0);

NOUN monkey(irm20);
monkey(LDESC) = ($say "There is a sacred temple monkey here.\n");
monkey(SDESC) = ($say "a monkey");

NOUN tandoor;
oven = tandoor;

NOUN comb(irm14);
comb(movbl) = 1;
comb(weight) = 1;
comb(LDESC) = ($say "There is an expertly carved jade comb here.\n");
comb(SDESC) = ($say "jade comb");
comb(rarea) = 1;
comb(pfound) = 7;
comb(pplace) = 6;

NOUN ruby(irm17);
ruby(movbl) = 1;
ruby(weight) = 2;
ruby(LDESC) = ($say "There is a crimson ruby here.\n");
ruby(SDESC) = ($say "ruby");
ruby(rarea) = 1;
ruby(pfound) = 8;
ruby(pplace) = 7;

NOUN bowl(irm21);
bowl(movbl) = 1;
bowl(weight) = 3;
bowl(LDESC) = ($say "There is a crystal bowl here.\n");
bowl(SDESC) = ($say "bowl");
bowl(rarea) = 1;
bowl(pfound) = 10;
bowl(pplace) = 10;

NOUN bracelet(irm23);
bracelet(movbl) = 1;
bracelet(weight) = 1;
bracelet(LDESC) = ($say 
"There is a golden bracelet in the shape of a snake here.\n");
bracelet(SDESC) = ($say "bracelet");
bracelet(rarea) = 1;
bracelet(pfound) = 11;
bracelet(pplace) = 6;

NOUN shovel(mrm1);
shovel(movbl) = 1;
shovel(weight) = 3;
shovel(LDESC) = ($say 
"There is a shovel here.\n");
shovel(SDESC) = ($say "shovel");

NOUN ears;

NOUN pole;

NOUN amber(prm3);
amber(movbl) = 1;
amber(weight) = 2;
amber(LDESC) = ($say "There is a polished piece of amber here.\n");
amber(SDESC) = ($say "amber");
amber(rarea) = 2;
amber(pfound) = 12;
amber(pplace) = 6;

NOUN lamp(mrm1);
lamp(movbl) = 1;
lamp(weight) = 3;
lamp(LDESC) = ($say  "There is a carbide-flame lamp here.\n");
lamp(SDESC) = ($say "lamp");

NOUN grate;

NOUN nugget;
gold = nugget;
nugget(movbl) = 1;
nugget(weight) = 4;
nugget(LDESC) = ($say "There is a gold nugget lying on the ground.\n");
nugget(SDESC) = ($say "gold nugget");
nugget(rarea) = 2;
nugget(pfound) = 13;
nugget(pplace) = 7;

NOUN axe(prm1);
axe(movbl) = 1;
axe(weight) = 1;
axe(LDESC) = ($say "There is an stone-age axe here.\n");
axe(SDESC) = ($say "axe");

NOUN spear(prm17);
spear(movbl) = 1;
spear(weight) = 2;
spear(LDESC) = (($eq ($prop spear tooky) 1) : 
		($say "There is a Neanderthal hunting spear here.\n"))
		(($eq ($prop spear tooky) 0) :
		($say
"There is a Neanderthal hunting spear stuck in the ground.\n"));
spear(SDESC) = ($say "spear");

NOUN plant(prm2);
plant(movbl) = 1;
plant(weight) = 4;
plant(LDESC) = ($say "There is a strange looking potted plant here.\n");
plant(SDESC) = ($say "plant");

NOUN smilodon;  {Putty tat}
sabre = smilodon;
smilo = smilodon;

NOUN troglodyte;
trogl = troglodyte;

NOUN cheese(prm4);
cheese(movbl) = 1;
cheese(weight) = 1;
cheese(LDESC) = ($say
"There is a piece of Swiss cheese here. (Aged 1,000,000 years).
");
cheese(SDESC) = ($say "cheese");

NOUN towel(prm4);
towel(movbl) = 1;
towel(weight) = 2;
towel(LDESC) = ($say "There is an old towel here.\n");
towel(SDESC) = ($say "towel");

NOUN mammoth(prm14);
mammoth(LDESC) = ($say
"There is a large wooly mammoth blocking the path to the south.\n");
mammoth(SDESC) = ($say "mammoth");
elephant = mammoth;
pachyderm = mammoth;

NOUN feet;

NOUN diamond(prm12);
diamond(movbl) = 1;
diamond(weight) = 1;
diamond(LDESC) = ($say "There is a small diamond here.\n");
diamond(SDESC) = ($say "diamond");
diamond(rarea) = 2;
diamond(pfound) = 7;
diamond(pplace) = 8;

NOUN ivory(prm15);
ivory(movbl) = 1;
ivory(weight) = 2;
ivory(LDESC) = ($say "There is a piece of ivory here.\n");
ivory(SDESC) = ($say "ivory");
ivory(rarea) = 2;
ivory(pfound) = 9;
ivory(pplace) = 8;

NOUN pendant;
pendant(movbl) = 1;
pendant(weight) = 2;
pendant(LDESC) = ($say "There is a ancient pendant here.\n");
pendant(SDESC) = ($say "pendant");
pendant(rarea) = 2;
pendant(pfound) = 14;
pendant(pplace) = 4;

NOUN cairn;
skulls = cairn;

NOUN bear(prm19);
bear(LDESC) = ($say "There is a ferocious cave bear blocking your path to the
north.\n");
bear(SDESC) = ($say "cave bear");

NOUN necklace(prm20);
necklace(movbl) = 1;
necklace(weight) = 1;
necklace(LDESC) = ($say "There is a pearl necklace here.\n");
necklace(SDESC) = ($say "necklace");
necklace(rarea) = 2;
necklace(pfound) = 13;
necklace(pplace) = 6;

NOUN Tyranosaur;
Tyran = Tyranosaur;

NOUN ring(prm23);
ring(movbl) = 1;
ring(weight) = 1;
ring(LDESC) = ($say "There is a large diamond ring here.\n");
ring(SDESC) = ($say "ring");
ring(rarea) = 2;
ring(pfound) = 13;
ring(pplace) = 10;

NOUN hole;

NOUN newspaper(mrm1);
newspaper(movbl) = 1;
newspaper(weight) = 1;
newspaper(LDESC) = ($say "There is a copy of a newspaper here.\n");
newspaper(SDESC) = ($say "newspaper");

NOUN crack;
NOUN shaft;

{*** VERBS ***}

VERB sign;
VERB read;
VERB throw;
VERB drop;
VERB take;
VERB play;
VERB quit;
VERB look;
VERB inven;
i=inven;l=look;
VERB touch;
VERB eat;
VERB swim;
VERB feed;
VERB score;
VERB abracadabra;
VERB dig;
VERB cut;
VERB plug;
VERB tie;
VERB untie;
VERB on;
VERB off;
VERB light;
VERB open;
VERB close;
VERB wipe;
VERB shake;
VERB xyzzy;
VERB jamb;
plugh = xyzzy;
clean = wipe;
shut = close;
peruse = read;
chuck = throw;
hurl = throw;
toss = throw;
heave = throw;
put = drop;
discard = drop;
get = take;
grab = take;
push = touch;
pet = touch;
devour = eat;
consume = eat;
wade = swim;
ford = swim;
shazam = abracadabra;
hocus = abracadabra;
abra = abracadabra;
excavate = dig;
remove = cut;
end = quit;
bye = quit;

{*** FUNNY OBJECT ROUTINES ***}

paper(ACTION) = (($eq ($verb) sign) :
		(($eq ($prop paper signed) 1) :
		($say "You already signed it.\n")
		($exit 1))
		(($ne ($loc pen) .ME) :
		($say "You don't have anything to write with.\n")
		($exit 1))
		(($ne ($loc paper) .ME) :
		($say "You don't have the release with you.\n")
		($exit 1))
		($setp paper signed 1)
		(($eq ($loc .ME) mrm3) : ($say 
"In a blinding flash of light, a stone archway appears in the east wall!\n")
		($exit 1))
		(($eq ($loc .ME) mrm5) :
		($say "The grate magically disappears into thin air.\n")
		($exit 1))
		($say "You hear strange noises in the nearby rooms.\n")
		($exit 1))
	(($eq ($verb) read) :
		(($ne ($loc paper) .ME) :
		($say "You don't have the paper with you.\n")
		($exit 1))
		($say
"                   THIS CONTRACT LIMITS OUR LIABILITY
                                READ IT

I, the undersigned, will not hold the AARDVARK, the UCLA Computer Sciences
Department, the UCLA Computer Club, Bell Labs, or the Digital Equipment
Corporation responsible for any injuries or deaths due to my executing
this program.\n")
		($setp paper readl 1)($exit 1));

spices(ACTION) = (($and ($eq ($verb) throw) ($eq ($loc .ME) irm8)) :
		($say "The bag of spices lands on the other side of the river.
")
		($move spices irm9)($exit 1))
		(($eq ($verb) eat) :
		($say "Munch, Munch Munch.  It needed a little more salt.\n")
		($move spices .ALL)
		($exit 1) )
		(($and ($eq ($verb) throw) ($eq ($loc .ME) irm9)) :
		($say "The bag of spices gets intercepted by one of the
crocodiles, who promptly swallows it.\n")($move spices .ALL) 
		($exit 1));

rupees(ACTION) = (($and ($eq ($verb) throw)
			($or ($eq ($loc .ME) irm8) ($eq ($loc .ME) irm9))) :
			($say
"The bag is too heavy to throw across the river.  It lands in the middle
of the river and sinks to the bottom.\n")($move rupees .ALL)
			($exit 1));

coconut(ACTION) = (($and ($eq ($loc .ME) irm19) ($eq ($verb) drop)) :
		(($ne ($loc coconut) .ME) :($exit 0))
		(($eq ($prop rupees wellbt) 0) :
		($say "The water in the well rises.\n")
		($move coconut irm19) ($exit 1))
		($say "The water level in the well slowly rises.
Magically floating on the water is a bag of rupees.\n")
		($setp rupees wellbt 0)
		($move rupees irm19)
		($move coconut irm19) ($exit 1))
		(($and ($eq ($verb) eat) ($eq ($loc coconut) .ME)) :
		($say "The coconut is too large for you to consume.\n")
		($exit 1) );

clarinet(ACTION) = (($and ($eq ($verb) play) ($eq ($loc clarinet) .ME)) :
		(($eq ($loc .ME) irm22) :
		(($eq ($loc cobra) irm22) :
		($say "Your clarinet playing sounds so poor that the cobra
slithers off in terror.\n")
		($move cobra .ALL) ($exit 1)))
		(($eq ($loc .ME) irm16) :
		(($eq ($loc rope) irm16) :
		(($eq ($prop rope ropstf) 0) :
		($say "The rope magically extends itself up into the air.\n")
		($setp rope ropstf 1)($exit 1))))
		($say "Benny Goodman you ain't.\n")
		($exit 1));

rope(ACTION) = (($eq ($prop rope ropstf) 1) :
		(($eq ($verb) take) :
		($setp rope ropstf 0))($exit 0))
		(($or ($eq ($loc .ME) prm2) ($eq ($loc .ME) prm3)) :
		(($eq ($prop rope rtied) 1) :
		(($eq ($verb) take) :
		($say "You have to untie the rope first.\n") ($exit 1))
		(($eq ($verb) tie) :
		($say "It's already tied, turkey.\n") ($exit 1))
		(($eq ($verb) untie) :
		(($eq ($loc .ME) prm2) :
		($move rope .ME) ($setp rope rtied 0)
		($say "You untie the rope and coil it up.\n"))
		(($eq ($loc .ME) prm3) :
		($say "It's tied down at the other end.\n"))
		($exit 1)))
		(($eq ($prop rope rtied) 0) :
		(($eq ($verb) tie) :
		(($eq ($loc .ME) prm2) :
		($say "You tie one end of the rope around the pole, the other
end dangles down into the canyon.\n")
		($move rope .ALL)
		($setp rope rtied 1) ($exit 1))
		($say "I see nothing to tie it onto.\n") ($exit 1))
		(($eq ($verb) untie) :
		($say "It's already untied.\n"))))
		(($eq ($loc .ME) prm3) :
		(($eq ($verb) take) : ($say
"You can't take it, the other end is tied down.\n") ($exit 1))
		(($eq ($verb) untie) : ($say
"the knot is on the other end.\n") ($exit 1)));

river(ACTION) = (($and ($ne ($loc .ME) irm8) ($ne ($loc .ME) irm9)) :
		($say "I see no river here.\n")($exit 1))
		(($eq ($verb) take) :
		($say "I can't do that.\n") ($exit 1));

cobra(ACTION) = (($and ($eq ($verb) take) ($eq ($loc .ME) ($loc cobra))) :
		($say "That would be a poor idea.\n")($exit 1));

crocodile(ACTION) = (($eq ($verb) take) :
		($say "I can't get to any crocodiles from here.\n")
		($exit 1));

cmara = (($eq ($verb) take) :
	($say "The statue is too heavy for you to carry.\n")
	($exit 1));
claks = (($and ($or ($eq ($verb) take) ($eq ($verb) touch))
	($eq ($prop statue tlakst) 0)) :
	($say
"The statue slides away very easily, revealing a secret passage.\n")
		($setp statue tlakst 1)($exit 1));
mara(ACTION) = (($eq ($loc .ME) irm10) : (cmara));
vishnu(ACTION) = (($eq ($loc .ME) irm11) : (cmara));
lakshmi(ACTION) = (($eq ($loc .ME) irm12) : (claks));
statue(ACTION) = (($or ($eq ($loc .ME) irm10) ($eq ($loc .ME) irm11)) :
		(cmara)
		($say "That won't accomplish anything.\n")
		($exit 1))
		(($eq ($loc .ME) irm12) : (claks)
		($say "I don't see how you can do that to a statue.\n")
		($exit 1));

banana(ACTION) = (($eq ($loc banana) .ME) :
		(($eq ($verb) eat) :
		($say "You eat the banana, peel and all.\n")
		($move banana .ALL)
		($exit 1)))
		(($eq ($loc .ME) irm20) :
		(($eq ($verb) drop) :
		($say "The monkey picks up the banana, eats it, and discards
the banana-peel.  As soon as the monkey finishes eating
the banana, a bolt of lighting hits the stone slab and 
cracks it open.\n")
		($setp monkey fedmky 1)
		($move banana .ALL)
		($move peel irm20) ($exit 1)));

horn(ACTION) = (($eq ($prop rhino cutflg) 1) : ($exit 0))
		(($eq ($loc .ME) irm13) : 
		(($eq ($verb) take) :
		($say "It is still attached to the rhino.\n")
		($exit 1))
		(($eq ($verb) cut) :
		(($ne ($loc saw) .ME) :
		($say "You don't have something to cut the horn with.\n")
		($exit 1))
		(($eq ($prop rhino earplg) 1) :
		($say "You cut the horn off without waking up the rhino.\n")
		($setp rhino cutflg 1)
		($move horn irm13) ($exit 1))
		($say "The noise of the sawing wakes up the rhinoceros who
tramples you to death because you disturbed his beauty sleep.\n")
		(exitg)));

lattice(ACTION) = (($eq ($loc .ME) irm13) :
		(($eq ($verb) take) :
		(($eq ($prop rhino earplg) 1) :
		($say "You successfully take the lattice without waking the
rhino.  Unfortunately, the lattice was the structure which supported the roof
of the room, and the ceiling comes crashing down upon you and breaks every 
bone in your body.\n"))
		(($eq ($prop rhino earplg) 0) :
		($say "As you take the lattice, a large part of the bamboo
falls down.  The noise caused by the falling bamboo scares the sleeping
rhinoceros who tramples you in his panicked exit from the room.\n"))
		(exitg)));

monkey(ACTION) = (($eq ($loc .ME) irm20) :
		(($eq ($verb) feed) :
		(($eq ($loc banana) .ME) :
		($say "The monkey takes your banana, peels it, eats it,
and throws away the banana peel.  As soon as he finishes eating
the banana, there is a rumble from the depths of the earth and 
a crack appears in the stone slab.\n")
		($setp monkey fedmky 1)
		($move banana .ALL)
		($move peel irm20) ($exit 1))));

cotton(ACTION) = (($eq ($loc .ME) irm13) :
		(($and ($eq ($verb) drop) ($eq ($iobj) ears)) :
		($say
"You stick the wads of cotton into the rhino's ears.\n")
		($move cotton .ALL)
		($setp rhino earplg 1)
		($exit 1)))
		(($eq ($loc cotton) .ALL):
		(($eq ($loc .ME) irm13):
		(($eq ($verb) take) :
		($say "OK\n")
		($move cotton .ME)
		($setp rhino earplg 0)
		($exit 1))));

pole(ACTION) = (($and ($eq ($loc .ME) prm2) ($eq ($verb) take)) :
		($say "The pole is firmly cemented into the ground.\n")
		($exit 1));

lamp(ACTION) = (($eq ($verb) light) : (onlmp) ($exit 1));

oven(ACTION) = (($and ($eq ($loc .ME) irm4) ($eq ($verb) open)) :
		($say "The door of the oven doesn't budge.\n")
		($exit 1));

grate(ACTION) = (($and ($eq ($prop paper signed) 0)
			($eq ($loc .ME) mrm5)) :
		(($or ($eq ($verb) open) ($eq ($verb) take)) :
			($say
"The grate appears to be firmly cemented into the wall.\n")
		($exit 1))
		(($eq ($verb) shut) :
		($say "The grate is already shut.\n")($exit 1)));

weapo = (($eq ($verb) throw) :
	(($and ($eq ($prop bear hitbr) 0) ($eq ($loc .ME) prm19)) :
	($say "The axe bounces harmlessly off of the bear.\n")
	($move axe prm19) ($exit 1))
	(($eq ($loc .ME) prm14) :
	($say "The weapon that you just threw imbeds itself deep into the 
skull of the mammoth and is covered by the mammoth's long fur so that it
can't be seen.  The mammoth just stares back at you blankly.\n")
	($move ($dobj) .ALL) ($exit 1))
	(($or ($eq ($loc .ME) irm8) ($eq ($loc .ME) irm9)) :
	($say "You miss the crocodile and your weapon sinks into the river.\n")
	($move ($dobj) .ALL) ($exit 1))
	(($eq ($loc .ME) irm13):
	($say "Your weapon bounces harmlessly off of the rhino.\n")
	($move ($dobj) irm13) ($exit 1))
	(($and ($eq ($loc .ME) prm6) ($eq ($prop smilo stond) 0)) :
	($say "The sabre-tooth catches your weapon in its mouth and promptly
swallows it.\n") ($move ($dobj) .ALL) ($exit 1))
	(($and ($eq ($loc .ME) prm8) ($eq ($prop trogl killd) 0)) :
	($say "You hit the troglodyte, who, screaming in pain, drops the
gold nugget and falls over the cliff.\n") ($setp trogl killd 1)
	($move ($dobj) .ALL)
	($move nugget prm8) ($exit 1)));

spear(ACTION) = (($eq ($loc spear) .ME) : 
		(($ne ($loc .ME) prm19) :
		(weapo))
		(($eq ($loc .ME) prm19) :
		(($eq ($verb) throw) :
		($say
"You repeatedly throw your spear at the bear.  Eventually, he gets
bored and wanders off.\n")
		($move bear .ALL)
		($move spear prm19)
		($setp bear hitbr 1) ($exit 1) )))
		(($eq ($loc .ME) prm17) :
		(($eq ($verb) take) :
		(($eq ($prop spear tooky) 0) :
		($say
"You hear a loud roar as an avalanche blocks the path to your north.\n")
		($move spear .ME)
		($setp spear tooky 1) ($exit 1))))
		(($eq ($verb) shake) :
($say "I don't see any playwrights here.\n"))
		(($and ($eq ($loc .ME) prm23) ($eq ($verb) take)) :
		($say 
"As soon as you take the spear, the Tyranosaurus Rex closes his mouth and
swallows you.\n") (exitg));

axe(ACTION) = (($eq ($loc axe) .ME) : (weapo));

smilo(ACTION) = (($and ($eq ($loc .ME) prm6) ($eq ($verb) take)) :
		($say "You must be dumber than you look.\n")
		($exit 1));

troglodyte(ACTION) = (($and ($eq ($loc .ME) prm8) ($eq ($verb) take)) :
		($say "The troglodyte does not look like he wants to be
carried.\n") ($exit 1));

plant(ACTION) = (($eq ($loc .ME) prm6) :
		(($or ($eq ($verb) throw) ($eq ($verb) drop)) :
		(($eq ($prop smilo stond) 0) :
		($say 
"The plant you were holding turns out to be primo grade catnip.  The
sabre-tooth cat grabs the plant and runs off purring loudly.\n")
		($move plant .ALL)
		($setp smilo stond 1) ($exit 1))))
		(($eq ($verb) eat) :
		($say 
"You completely eat the plant and now feel quite nauseous.\n")
		($move plant .ALL) ($exit 1));

nugget(ACTION) = (($and ($eq ($verb) take)
		($eq ($prop trogl killd) 0)) :
		($say "He ain't gonna let you take it.\n")($exit 1));

cheese(ACTION) = (($eq ($verb) eat) :
		($say "You eat the cheese, but nothing peculiar happens.\n")
		($move cheese .ALL) ($exit 1))
		(($and ($eq ($verb) drop) ($eq ($loc .ME) prm14)) :
		($say 
"As soon as you drop the cheese, a mouse runs out of the hole in the east 
wall and takes it.  This scares the mammoth who runs off in terror.\n")
		($setp mammoth blokd 1)
		($move mammoth .ALL)
		($move cheese .ALL) ($exit 1));

mammoth(ACTION) = (($eq ($loc mammoth) ($loc .ME)) :
		(($eq ($verb) feed) :
		($say "He doesn't look hungry.\n")($exit 1))
		(($eq ($verb) take) : ($say
"He doesn't look too easy to carry.\n") ($exit 1)));

feet(ACTION) = (($eq ($verb) wipe) :
		(($ne ($loc towel) .ME) :
		($say "You don't have something to wipe them with.\n")
		($exit 1))
		($say "You just cleaned off your feet.\n")
		($setp feet wiped 0) ($exit 1))
		(($eq ($verb) touch) :
		($say "You can't without bending your knees.\n"));

cairn(ACTION) = (($eq ($loc .ME) prm18) :
		($say 
"How dare you do such a thing.  Any idiot with just the slightest knowledge
of prehistoric etiquette knows that you aren't supposed to do anything to
a cairn.\n") ($exit 1) );

bear(ACTION) = (($eq ($loc .ME) prm19) :
		(($eq ($verb) take) :
		(($eq ($prop bear hitbr) 0) :
		($say 
"The bear is a little too bulky to carry.\n")($exit 1))));

Tyranosaur(ACTION) = (($and ($ne ($loc .ME) prm22) ($ne ($loc .ME) prm23)) :
		($say
"What?  One of those things hasn't existed in several million years.\n"));

hole(ACTION) = (($eq ($loc .ME) prm14) :
		($say "That hole is too small to do anything with.\n")
		($exit 1))
		(($eq ($loc .ME) irm19) :
		(($eq ($verb) drop) :
		(($eq ($iobj) hole) :
		($say "It falls back out of the hole.\n")
		($exit 1))))
		(($eq ($loc .ME) irm6) :
		(($eq ($verb) drop) :
		(($eq ($iobj) hole) :
		(($eq ($dobj) spices) :
		(($ne ($loc spices) .ME) :
		($say "You ain't got it with you.\n")
		($exit 1))
($say "The bag of spices rips against the jagged sides of the shaft and 
almost all of the spices get blown away in the wind.\n")
		($move spices .ALL)
		($exit 1))
		(($eq ($dobj) rupees) :
		(($ne ($loc rupees) .ME) :
		($say "You ain't got the coins with you.\n")
		($exit 1))
($say "The rupees fall down the shaft into the darkness below.  You
eventually hear a barely audible splash.\n")
		($setp rupees wellbt 1)
		($move rupees .ALL)
		($exit 1))
		($say "That doesn't fit in the hole.\n")
		($exit 1)
		)))
		($say "I do not see any holes here.\n");

newspaper(ACTION) = (($and ($eq ($verb) read) ($eq ($loc newspaper) .ME)) :
		($say
"			   DAILY GNUS
Vol 2.								25 AUGUST

")
		($say
"Spies in the far reaches of the Museum have reported that new construction
is now taking place.\n\n")
		($say
"Again, Adventurers are reminded that restocking of the museum sections will
be greatly rewarded.\n\n"
			)($exit 1));

crack(ACTION) = (($eq ($verb) jamb) :
		($say "The only climbable cracks in this place are A4.\n")
		($exit 1))
		($say "I can't figure that out. I'm not as smart as I am 
cracked up to be.\n")($exit 1);

shaft(ACTION) = (($eq ($verb) jamb) :
		($say "Try jamb crack (How do you jamb a shaft?)\n")
		($exit 1));

{*** DIRECTIONS ***}

VERB north;
VERB south;
VERB east;
VERB west;
VERB up;
VERB down;
VERB in;
VERB out;
n=north;
s=south;
e=east;
w=west;
u=up;
d=down;
enter = in;
exit=out;
leave=out;
climb=up;

{*** VERB ROUTINES ***}

plug(ACTION) = (($eq ($loc .ME) irm13) :
		(($eq ($loc cotton) .ME) :
		($say
"You stick the cotton into the rhino's ears.\n")
		($move cotton .ALL)
		($setp rhino earplg 1)
		($exit 1)))
		($say "I don't quite understand what you mean.\n");

dig(ACTION) = (($ne ($loc shovel) .ME) :
		($say "You don't have something to dig with.\n")
		($exit 1))
		(($eq ($loc .ME) irm9) :
		(($eq ($prop cotton tookbt) 0) : ($setp cotton tookbt 1)
		($say "You find some cotton buried in the sand.\n")
		($move cotton irm9)($exit 0))
		($say "OK\n") ($exit 0))
		(($eq ($loc .ME) prm17) :
		(($eq ($prop pendant dugbt) 0) : ($setp pendant dugbt 1)
		($say "You find an ancient pendant buried in the grave.\n")
		($move pendant prm17) ($exit 0))
		($say "You dig some but you don't find anything.\n")
		($exit 0))
		($say "The ground is too hard to dig into.\n");

cantdo = ($say "I don't believe I can do that.\n");
sign(ACTION) = (cantdo);
play(ACTION) = (cantdo);

arolg = ($say "It will probably cost you an arm or a leg to feed that.\n")
	($exit 1);
feed(ACTION) = (($or ($eq ($loc .ME) irm8) ($eq ($loc .ME) irm9)) :
		(($eq ($dobj) crocodile) :
		(arolg)))
		(($and ($eq ($loc .ME) prm19) ($eq ($dobj) bear)) :
		(($eq ($prop bear hitbr) 0) :
		(arolg)))
		(($and ($eq ($loc .ME) prm6) ($eq ($dobj) smilo)) :
		(($eq ($prop smilo stond) 0) :
		(arolg)))
		(($or ($eq ($loc .ME) prm22) ($eq ($loc .ME) prm23)) :
		(($eq ($dobj) Tyran) : (arolg)))
		($say "I don't quite understand what you mean.\n");

cut(ACTION) = (cantdo);

read(ACTION) = ($say "I don't see any text here that I can read.\n");

throw(ACTION) = (cdrop);

touch(ACTION) = ($say "That doesn't seem to accomplish much.\n");

eat(ACTION) = ($say "I think I just lost my appetite.\n");

abracadabra(ACTION) = (($ne prehs ($loc ($loc .ME))) :
		($say
"That phrase hasn't worked in at least ten thousand years.\n")
		($exit 0))
		(($ne ($loc .ME) prm17) :
		($say "Nothing happens.\n") ($exit 0))
		(($eq ($prop spear tooky) 1) :
		(($eq ($prop spear abrad) 0) :
		($setp spear abrad 1)
		($say "The rubble gets magically cleared away.\n")
		($exit 0)))
		($say "Nothing much happens.\n");

tie(ACTION) =  ($say "It is impossible to tie a knot in that right now. \n");

untie(ACTION) = ($say "I don't quite understand what you mean.\n");

on(ACTION) = (($eq ($prop lamp onoff) 1) :
		($say "The lamp is already on.\n") ($exit 1))
		(onlmp) ($exit 1);

off(ACTION) = (($ne ($loc lamp) .ME) :
		($say "You don't have the lamp with you.\n")
		($exit 1))
		(($eq ($prop lamp onoff) 0) :
		($say "The lamp is already off.\n")
		($exit 1))
		($setp lamp onoff 0)
		($say "The lamp is now off.\n");

light(ACTION) = ($say "I don't know how to light that.\n");

open(ACTION) = ($say "That thing is un-openable.\n");

close(ACTION) = ($say "I don't quite understand what you mean.\n" );

swim(ACTION) = (($eq ($loc .ME) irm8) :
		(ei8) ($exit 0))
		(($eq ($loc .ME) irm9) :
		(ei8) ($move .ME irm8) ($exit 0))
		($say "I don't see enough water here to even wade in.\n");

wipe(ACTION) = ($say "I don't quite comprehend what you are saying.\n");

shake(ACTION) = ($say "That probably won't accomplish much.\n");

jamb(ACTION) = ($say "I don't quite comprehend what you mean.\n");

xyzzy(ACTION) = ($say 
"You are transported to a room where you are faced by a wizard who points to
you and says, ''Them's fighting words!''  You immediately get attacked by
all sorts of denizens of the museum: there is a cobra chewing on your leg,") 
	($say "
a troglodyte is bashing your brains out with a gold nugget, a crocodile is 
removing large chunks of flesh from you, a rhinoceros is goring you with his
horn, a sabre-tooth cat is busy trying to disembowel you, you are being")
	($say "
trampled by a large mammoth, a vampire is sucking you dry, a Tyranosaurus
Rex is sinking his six inch long fangs into various parts of your anatomy,
a large bear is dismembering your body, a gargoyle is bouncing up and")
	($say "
down on your head, a burly troll is tearing you limb from limb, several
dire wolves are making mince meat out of your torso, and the wizard is about
to transport you to the corner of Westwood and Broxton.\n")
	($say "Oh dear, you seem to have gotten yourself killed, as well.\n")
	(skore) (ratng) (ratnx) ($spec 3 0 0 0 0);

score(ACTION) = (skore)(ratng);
skore = ($say "You scored")
	($num ($prop .ME pscore))
	($say "out of")
	($num ($prop .ME pmax))
	($say "possible points.\n");

ratng =	($setp .ME ratvl 0)
	(($ne ($prop .ME pscore) 0) :
	($setp .ME ratvl ($quotient ($times ($prop .ME pscore) 8)
					($prop .ME pmax))))
	($say "That gives you a ranking of ")
	(($eq ($prop .ME ratvl) 0) :
	($say "junior beginning adventurer.\n"))
	(($eq ($prop .ME ratvl) 1) :
	($say "senior beginning adventurer.\n"))
	(($eq ($prop .ME ratvl) 2) :
	($say "intermediate adventurer.\n"))
	(($eq ($prop .ME ratvl) 3) :
	($say "expert adventurer.\n"))
	(($eq ($prop .ME ratvl) 4) :
	($say "junior master adventurer.\n"))
	(($eq ($prop .ME ratvl) 5) :
	($say "master adventurer.\n"))
	(($eq ($prop .ME ratvl) 6) :
	($say "senior master adventurer.\n"))
	(($eq ($prop .ME ratvl) 7) :
	($say "life master adventurer.\n"))
	(($eq ($prop .ME ratvl) 8) :
	($say "super-stud adventurer.\n"));

ratnx = (($eq ($prop .ME ratvl) 8) : 
	($say "CONGRATULATIONS.\n") ($spec 3 0 0 0 0))
	($say "To achieve the next higher rating, you need to score")
	($setp .ME ratvl ($plus ($prop .ME ratvl) 1))
	($setp .ME ratvl ($times ($prop .ME ratvl) ($prop .ME pmax)))
	($setp .ME ratvl ($quotient ($prop .ME ratvl) 8))
	($setp .ME ratvl ($plus ($prop .ME ratvl) 1))
	($setp .ME ratvl ($minus ($prop .ME ratvl) ($prop .ME pscore)))
	($num ($prop .ME ratvl))
	($say "more points.\n");

objl = (($eq %1 0) : ($exit 0))
	(objl ($link %1))
	(($ldisc %1));

objs = (($eq %1 0) : ($exit 0))
	(objs ($link %1))
	(($eq %1 .ME) : ($exit 0))
	(($eq ($prop .ME kludge) 1) : ($say "You can see:\n")
				($setp .ME kludge 0))
	(($sdisc %1))
	($say "\n");

LLOOK =  (($ldisc ($loc .ME)))
	(objl ($cont ($loc .ME)));

SLOOK = (($sdisc ($loc .ME)))
	($setp .ME kludge 1)
	(objs ($cont ($loc .ME)));

LOOK = ( ($prop ($loc .ME) VISIT) :
		(($ne @Myloc ($loc .ME)) :   (SLOOK)) :
		(($ne @Myloc ($loc .ME)) :
			(LLOOK) ($setp ($loc .ME) VISIT 1))
	)
	($setg Myloc ($loc .ME))
	($say "> ");

START = ($setp .ME pmax 250)
	($setp .ME debug 0)
	($move .ME mrm1)
	($sdem LOOK);

quit(ACTION) = (skore) (ratng) (ratnx) ($spec 3 0 0 0 0);

	 look(ACTION) = (LLOOK);

take(ACTION) = (ctake);
scale = ($setp .ME weight 0)
	(($ne ($cont .ME) 0) :
	($setg 12 ($cont .ME)) (scads));
scads = (($ne @12 0):
	($setp .ME weight ($plus ($prop .ME weight) ($prop @12 weight)))
	($setg 12 ($link @12))
	(scads));
ctake = (scale)
	(($ne ($loc .ME) ($loc ($dobj))):
	($say "I don't see that item here.\n")($exit 0))
	(($eq ($prop ($dobj) movbl) 0) :
	($say "That thing is too heavy to carry.\n")($exit 0))
	(($gt ($plus ($prop ($dobj) weight) ($prop .ME weight)) 20) :
	($say
"You are carrying too much.  You will have to at least drop something first.
") ($exit 0))
	($setp .ME weight ($plus ($prop ($dobj) weight) ($prop .ME weight)))
	($say "OK\n") ($move ($dobj) .ME)
	(($eq ($prop ($dobj) rarea) 0 ) : ($exit 0))
	($setp .ME pscore ($plus ($prop .ME pscore) ($prop ($dobj) pfound)))
	($setp ($dobj) pfound 0);

drop(ACTION) = (cdrop);
cdrop = (($ne ($loc ($dobj)) .ME) :
	($say "You don't have it with you.\n")($exit 0))
	(($eq ($loc .ME) prm21) :
	($say "OK, it falls further down into the crack.\n")
	($move ($dobj) prm22) ($exit 1))
	($say "OK\n") ($move ($dobj) ($loc .ME))
	(($eq ($prop ($dobj) rarea) 0 ) : ($exit 0))
	(($eq ($prop ($dobj) rarea) 2) : (($eq ($loc .ME) mrm5) :
			(scord)))
	(($eq ($prop ($dobj) rarea) 1): (($eq ($loc .ME) mrm3) : 
		(scord)));
	
	scord = 
	($setp .ME pscore ($plus ($prop .ME pscore) ($prop ($dobj) pplace)))
	($setp ($dobj) pplace 0);

inven(ACTION) = ($say "You are carrying:\n")
		(($eq ($cont .ME) 0):($say "Nothing\n")($exit 1))
		($setg 12 ($cont .ME)) (inside);
inside = (($ne @12 0) : (($sdisc @12)) ($say "\n")
	($setg 12 ($link @12)) (inside));

{*** ROOM DESCRIPTIONS ***}

mrm1(LDESC) = ($say "You are standing outside the north entrance of a large
brick building.  Inscribed above the doorway, appear the 
text: 'AARDVARK'S MUSEUM -- GATEWAY TO ADVENTURELAND'.
");
mrm1(SDESC) = ($say "Museum entrance
");

mrm2(LDESC) = ($say "You are in a large rotunda of an old museum.  Doors lead
to the north, south, east, and west, and a narrow stairway 
in the north-east corner of the room leads down.
");
mrm2(SDESC) = ($say "Museum rotunda
");

mrom3a = ($say "You are in a dimly lit room containing an empty display case.
A portion of a vandalized sign above the case reads: 
'ARTIFACTS OF ANCIENT INDIA -- Several of these items,
including the sacred rhinoceros horn, the deadly ...'.
The rest of the sign is unreadable.
");
mrm3(LDESC) = (mrom3a)
		(($eq ($prop paper signed) 0) : ($say 
"To the west, you can look through a large door into the rotunda
of the museum. On the east wall of the hall there is an outline
of an arch.
"))
		(($eq ($prop paper signed) 1) : ($say
"Through the door to the west, you can see part of the rotunda
of the museum.  To the east, there is a stone archway.
"));
mrm3(SDESC) = ($say "East wing of the museum
");

mrm4(LDESC) = ($say "You are in a non-descript room with absolutely 
nothing in it.  A hollow voice says ''This room is unavailable for use at this
time.  Please leave through the doorway to your north.''
");
mrm4(SDESC) = ($say "Closed room
");

mrom5 = ($say "You are standing before a large empty display case in a poorly
lit basement-like room.  Encircling the room high on the walls appear the text
''HALL OF PREHISTORY.''  A stairway leads up, and in the south wall, there is
a small hole");
mrm5(LDESC) = (mrom5)
		(($eq ($prop paper signed) 1) :
		($say ".\n"))
		(($eq ($prop paper signed) 0) :
		($say " covered by a grate.\n"));
mrm5(SDESC) = ($say "Hall of Prehistory.\n");

mrm6(LDESC) = ($say
"You are wandering through a dense forest past twisted birch trees
rising toward the sky in contorted agony.  Enormous skeletons of burned
out sycamores are scattered throughout the area, and gnarled stumps of")
($say
"\nonce proud oak trees make the appearance of the forest even more 
disturbing.  Nothing is stirring, a pall of death seems to hang over the
forest like a blanket, and you can't seem to figure out which direction\n")
($say "is which.\n");
mrm6(SDESC) = ($say "You are lost in the forest.\n");

irm1(LDESC) = ($say "You  are at the top of a highly ornate spiral stairway.
A wing of the museum can be seen to the west.  The room is moist 
and damp, and the scent of cumin and saffron fills the air.
");
irm1(SDESC) = ($say "Top of spiral stairway
");

irm2(LDESC) = ($say "You are in an east-west passage at the bottom of
a spiral stairway.  A slight breeze blows from east to west.
");
irm2(SDESC) = ($say "Bottom of spiral stairway
");

irm3(LDESC) = ($say "You are standing in a deserted Indian marketplace.
Although it seems to have once been a bustling area, 
there is very little left here.  Most of the vacant
booths form an alley running north, and a small path
goes east.
");
irm3(SDESC) = ($say "Marketplace
");

irm4(LDESC) = ($say "You are in a small kitchen adjoining the marketplace.
Most of the furniture in the room has been broken, and all               
of the pottery once used for cooking has been stolen. In the
north-west corner, there is a tandoori oven.  A door to the
south leads back out to the marketplace, and a trap-door 
leads down to what appears to be a murky basement.
");
irm4(SDESC) = ($say "Kitchen
");

irm5(LDESC) = ($say "You are in a cellar which apparently has been used to
store spices.  Empty racks line all the walls of the room, 
and everything in the room seems to be blanketed in a thin
layer of dust.  An unreachable hole appears in the ceiling
and exits lead to the north, south, and east.
");
irm5(SDESC) = ($say "Spice cellar
");

irm6(LDESC) = ($say "You are in a small cave-like room apparently once used
for the cultivation of mushrooms.  You find the smell of
this room slightly displeasing.  There are exits to the  
south and west, and there is a small hole in the 
north wall, through which you can see a long vertical
shaft with jagged sides.
");
irm6(SDESC) = ($say "Mushroom room
");

irm7(LDESC) = ($say "You are in an abandoned warehouse-like room which, 
though once used for storage, is now predominantly full of  
rubble.  Exits lead to the north and east.
");
irm7(SDESC) = ($say "Warehouse
");

irm8(LDESC) = ($say "You are standing on a rocky beach on the west bank
of a slow-moving subterranean river. Several large 
crocodiles are lazily sleeping at the north end of the river. 
A path leads to the west.
");
irm8(SDESC) = ($say "West bank of river
");

irm9(LDESC) = ($say "You are on a sandy beach on the east bank of a crocodile
infested river.  A roughly hewn stairway in the rock leads up 
far beyond your range of vision.
");
irm9(SDESC) = ($say "East bank of river
");

irm10(LDESC) = ($say "You are in a room containing an enormous statue of
Mara, Hindu goddess and consort to Vishnu.  Passages lead 
to the east, west, and south, and a roughly hewn stairway
seems to go down to an area further underground.
");
irm10(SDESC) =($say "In front of Mara's statue
");

irm11(LDESC) = ($say "You are standing in front of a gigantic statue of the
Hindu god Vishnu.  Passages lead north and south, and it appears
that you can squeeze through a narrow crack in the western wall.
");
irm11(SDESC) = ($say "In front of Vishnu's statue
");

irom12a = ($say "You are in a room containing an enormous statue of 
Lakshmi, Hindu goddess and consort to Vishnu.  
");
irm12(LDESC) = (irom12a)
		(($eq ($prop statue tlakst) 0) : ($say 
"Passages lead to the north and east.
"))
		(($eq ($prop statue tlakst) 1) : ($say
"Passages lead to the east and north, and on the west wall
there is a hole large enough to crawl through.
"));
irm12(SDESC) = ($say "In front of Lakshmi's statue
");

irm13(LDESC) = ($say "You are in what would appear to be a totally man-made
cave.  The walls are covered with bamboo shafts cut and 
tied together to form a very complex lattice pattern.
An extremely narrow passage leads east, and there is a 
large door to the south.
");
irm13(SDESC) = ($say "Bamboo Room
");

irm14(LDESC) = ($say "You are in a small room with a very low ceiling. The
only exit is to the east.
");
irm14(SDESC) = ($say "Flat Room
");

irm15(LDESC) = ($say "You are standing in the center of a large dome-like 
room.  Exits lead to the north, east, and west, and around the perimeter of
the room appear the words 'ABRACADABRA -- VARUNA SUCKS COCONUTS.'
");
irm15(SDESC) = ($say "Inside Dome
");

irm16(LDESC) = ($say
"You are at the base of a tall naturally formed shaft.
On all sides, you are surrounded by gigantic basalt columns
towering above you in a serpentine manner, as if great 
likenesses of Vasuki himself.  Passages lead to the north,
east, and west, and above you appears a small ledge.
");
irm16(SDESC) = ($say "Vertical Shaft
");

irm17(LDESC) = ($say
"You are on a small ledge high above the base of a large
shaft.  There is a rope seemingly standing rigid allowing you to
climb down, but the climb above you seems quite dificult.
");
irm17(SDESC) = ($say "On small ledge
");

irm18(LDESC) = ($say
"You are in a small room which smells strongly of
incense.  Stone archways lead to the south, and east, and
a smaller passage leads to the west.  You also hear some
chanting and Sitar music in the background.
");
irm18(SDESC) = ($say "Incense room
");

irm19(LDESC) = ($say
"You are at the holy well of Varuna, the water god.  The well is in the
middle of the room, leading straight down into some water far below.
Legend has it that those who climb down into the well suffer a fate
worse than death, so it is advisable not to go down.  In the 
ceiling, there is a round hole")
	(($eq ($prop rupees wellbt) 1) :
	($say " and the water in the well appears to have
been disturbed recently.\n"))
	(($eq ($prop rupees wellbt) 0) :
	($say ".\n"));
irm19(SDESC) = ($say "Varuna's well
");

irom20a = ($say
"You are at the entrance to Siva's temple.
");
irm20(LDESC) = (irom20a)
		(($eq ($prop monkey fedmky) 1) : ($say
"A passage leads back to the west, and a crack in a huge stone slab allows
one to proceed east.
"))
		(($eq ($prop monkey fedmky) 0) : ($say
"A passage leads back to the west, but the entrance to the temple is blocked
by a huge stone slab.
"));
irm20(SDESC) = ($say "Entrance to Siva's temple
");

irm21(LDESC) = ($say 
"You are standing in front of Siva's altar, a small stone slab in front of
a large monolith.  The exit is to the west.
");
irm21(SDESC) = ($say "Siva's Altar
");

irm22(LDESC) = ($say
"You are standing at the top of a six-foot deep snake pit.  The walls of the
room are covered with bas relief figures of cobras, kraits, and other 
miscellaneous elapids.  Exits lead to the west and south, through archways
ringed by pythons carved into the rock.
");
irm22(SDESC) = ($say "Top of Snake pit
");

irm23(LDESC) = ($say
"You are at the bottom of a snake pit.  The only direction it appears you 
can go is back out.
");
irm23(SDESC) =($say "Bottom of snake pit
");

prm1(LDESC) = (darkq) ($say
"You are in the north-south corridor of an ancient paleolithic cave.  Its 
low ceiling is covered with paintings of wild game which have faded due to
the passing of time.\n");
prm1(SDESC) = (darkq)($say "North-south corridor.\n");

prm2(LDESC) = ($say
"You are standing on a ledge above the floor of a subterranean canyon
running east-west.  Shafts of sunlight penetrate through cracks in the
ceiling high above, washing the room with an alabaster murkiness;
casting shadows on the weathered rocks which have lain undisturbed
for countless centuries. A small crawlway leads north, and there is a
short, stout pole sticking out of the ground.")
	(($eq ($prop rope rtied) 1) : ($say "  A section of rope is tied 
around the pole, with the other end hanging down into the canyon.\n"))
	(($eq ($prop rope rtied) 0) : ($say "\n"));

prm2(SDESC) = ($say "Above East-west canyon.\n");

prm3(LDESC) = ($say 
"You are at the bottom of a canyon running east-west.  The passage to the
east is blocked by rubble, so it appears that the only direction you can
go is west. A rope dangles down from above.\n");
prm3(SDESC) = ($say "East end of canyon.\n");

prm4(LDESC) = ($say
"You are standing in front of a limestone cavern, the entrance of which looks
like a gaping mouth of a grotesque monster.  It appears that you can enter
the cave to your south, or go either direction in the east-west canyon that
you are in.\n");
prm4(SDESC) = ($say "Entrance to limestone cavern.\n");

prm5(LDESC) = (darkq) ($say
"You are in a very narrow room which probably was once much larger.
However, it appears that a recent cave-in has closed off much of the
room to the east.  To your north, there is a corridor which apparently
leads back out, and in the eastern wall, there is a hole which you can
probably crawl through.\n");
prm5(SDESC) = (darkq) ($say "Narrow room in cave.\n");

prm6(LDESC) = (darkq) (($eq ($prop smilo stond) 0) : ($say
"You are standing above a shallow pit which is empty except for a
large smilodon (sabre-tooth tiger) which is growling at you menacingly.
The only other direction it looks like you can go is back out through a
small hole in the rubble forming the west wall of the room you are in.\n"))
			(($eq ($prop smilo stond) 1) : ($say
"You are standing above an empty shallow pit.  There is a small hole in the
west wall of the room you are in.\n"));
prm6(SDESC) = (darkq) ($say "Above shallow pit.\n");

prm7(LDESC) = (darkq) ($say
"You are standing on the loose gravel of a shallow pit.  An exit leads
south, and above you, there is a small ledge which you can climb up to.\n");
prm7(SDESC) = (darkq) ($say "In shallow pit.\n");

trogf = (($eq ($prop trogl killd) 0) : 
	($say " and there is an angry
troglodyte here holding a large gold nugget under his arm"))
	($say ".\n");
trgfs = (($eq ($prop trogl killd) 0) :
	($say "There is an angry troglodyte holding a gold nugget here.\n"));
prm8(LDESC) = (darkq) ($say
"You are standing near the west rim of sheer cliff which drops down into a deep
almost bottomless abyss. A passage leads north") (trogf);
prm8(SDESC) = (darkq) ($say "West rim of canyon.\n") (trgfs);

prm9(LDESC) = ($say 
"You are at the junction of two canyons.  The larger one runs east-west
and the smaller one runs north.\n");
prm9(SDESC) = ($say "Canyon junction\n");

prm10(LDESC) = ($say
"You are standing on a shale slab tilted from west to east at about a 
forty-five degree angle.  At the east end of the slab, there is a short
drop into what appears to be a lake.  Above you and to the west, it appears
that there is some more explorable terrain, and the canyon leads back to
to the south.  The air is quite warm, and there is a slight scent similar
to asphalt in the air.\n");
prm10(SDESC) = ($say "On shale slab.\n");

prm11(LDESC) = ($say 
"You are standing in the south end of a short canyon running north-south.
The ground below your feet is littered with flakes of a dark rock, which
seems to comprise most of the surrounding canyon walls.  There is a path to
your north, and a steep slope down and to the east.\n");
prm11(SDESC) = ($say "Canyon full of rubble.\n");

prm12(LDESC) = ($say
"You are standing in the north end of a short canyon.  The ground is
covered with a thin film of oil which apppears to be seeping out of
a crack in the earth.  The only apparent direction you can go is south.\n");
prm12(SDESC) = ($say "Oily end of canyon.\n");

prm13(LDESC) = ($say "You are standing in what was once a tributary into
the now-extinct river which formed the large canyon in this area.  There
is a streambed running to the south, and you can also go east and west.\n");
prm13(SDESC) = ($say "End of dry river bed.\n");

prm14(LDESC) = ($say
"You are standing in a narrow north-south canyon with a river bed running 
down the middle of it.  There is a semicirular shaped hole at the base of
the eastern wall.\n");
prm14(SDESC) = ($say "In narrow part of river bed.\n");

prm15(LDESC) = ($say 
"You are at the base of three water-eroded cliffs to your south, east and 
west.  It appears that a U-shaped waterfall once flowed into this area, and
exited out to the north.\n");
prm15(SDESC) = ($say "Below dried waterfall.\n");

prm16(LDESC) = ($say 
"You are in a section of the canyon where much of the rock appears to be
primarily sandstone.  Water has apparently flowed out of a cave to your 
north into three-foot wide crack in the ground.  A substantial amount of
rubble blocks your way west, and the rest of the canyon winds east.\n");
prm16(SDESC) = ($say "Sandstone area.\n");

prm17(LDESC) = (darkq) ($say
"You are standing in front of the freshly dug gravesite of a Nearderthal
hunter.  There is an exit to your south, and it seems that more of the 
cave can be explored to the north.\n")
(($and ($eq ($prop spear tooky) 1) ($eq ($prop spear abrad) 0)) :
($say "However, a large amount of rubble blocks that path.\n"));
prm17(SDESC) = (darkq)($say "Neanderthal's gravesite\n");

prm18(LDESC) = (darkq) ($say
"You are in a narrow room of a north-south running cave.  A large cairn of
prehistoric cave bear skulls is piled high in one corner of the room.
There are exits to the north and south.\n");
prm18(SDESC) = (darkq) ($say "In front of cave bear cairn.\n");

prm19(LDESC) = (darkq) ($say
"You are in a large cavern room with a high ceiling.  Cracks in the walls
lead in all directions but none of them seem to lead anywhere.  There are
however, two major paths to the north and the south.\n");
prm19(SDESC) = (darkq) ($say
"Large Cavern room.\n");

prm20(LDESC) = (darkq) ($say
"You are in a room which was the site of a recent cave-in.  It appears to
be futile to go any other direction except south.\n");
prm20(SDESC) = (darkq) ($say "Cave-in site.\n");

prm21(LDESC) = ($say
"You are in a crack in the earth which seems to have been formed by an ancient
earthquake.  Various strata in the rock of different darkness seem to indicate
that much time has passed since the formation of the rock at the lower end of
this crack.  It seems that you can chimney up back out of the crack, or climb
further down into it.\n");
prm21(SDESC) = ($say "In large crack.\n");

prm22(LDESC) = ($say
"You are standing on a narrow ledge about twenty-five feet above the ground.
Aside from going back up, the only other direction you can go is to step off 
the ledge to the west into the mouth of the large Tyranosaurus Rex waiting to
swallow you whole.\n");
prm22(SDESC) = ($say
"On ledge in front of Tyranosaurus Rex.\n");

prm23(LDESC) = ($say
"You are standing inside the mouth of a Tyranosaurus Rex, whose jaws are
being held open by a spear wedged length-wise in his mouth.  Various pieces
of former adventurers can be found among the six-inch long teeth here.
You can step back out of the Tyranosaur onto the small ledge, or go
further down into the Tyranosaur and get digested.\n");
prm23(SDESC) = ($say "Inside mouth of Tyranosaur.\n");

{*** FUNNY TRANSITION ROUTINES ***}

exitg = ($say "Oh dear, you seem to have gotten yourself killed.\n")
(skore)
(ratng)
(ratnx)
(($eq ($prop .ME debug) 0) : ($spec 3 0 0 0 0));

em3 = (($eq ($prop paper signed) 0):(cg) ($exit 0))
      ($move .ME irm1);

ei8 = (($eq ($loc spices) .ME) : ($say
"The savory blend of spices and human wakes up the sleeping crocodiles who
decide to have you for breakfast.
") (exitg))
      (($eq ($loc rupees) .ME) : ($say
"The weight of the coins pulls you underwater and you eventually drown.
") (exitg))
      ($say "You manage to get across the river, just as the crocodiles
begin to stir.
")($move .ME irm9);

wi12 = (($eq ($prop statue tlakst) 0) : (cg) ($exit 0))
	($move .ME irm14);

ui16 = (($eq ($prop rope ropstf) 0) : (cg) ($exit 0))
	($say "You scramble up the rope and get to the ledge.
") ($move .ME irm17);

ui17 = ($say "Are you a rock climber? \n")
	(($eq ($yorn) 0) : ($say "Then you better not try it.\n")
		($exit 0))
	($say "That climb is rated 5.11. Do you still want to try it?\n")
	(($eq ($yorn) 0) : ($say "Wise move.\n") ($exit 0))
	($say "Half way up, you fall off a 1/32 inch wide ledge and
plummet to your death.\n") (exitg);

di19 = ($say "You magically get transported to the corner of Westwood and
Broxton, where you are doomed to spend the rest of eternity surrounded by
chanting Hare-Krishnas. (That'll teach you not to believe legends.)\n")
	($spec 3 0 0 0 0);

ei20 = (($eq ($prop monkey fedmky) 0):(cg)($exit 0))
	($move .ME irm21);

wi21 = (($eq ($loc peel) irm20) : ($say
"As you leave the temple, you slip on a banana peel and break your neck.\n")
	(exitg)) ($move .ME irm20);

di22 = (($eq ($loc cobra) irm22) : ($say
"The cobra takes a bite at your unprotected leg and injects an
unhealthy dose of neurotoxin.  You start to lose your senses, your 
cognitive abilities, an yer rekcogiskdfsdk.\n")
(exitg))($move .ME irm23);

sm5 = (($eq ($prop paper signed) 0) :
	($say "You bumped into the grate.\n")
	($exit 0))
	($move .ME prm1);

dp2 = (($eq ($prop rope rtied) 0) :
	($say "It is too dangerous to climb down without a rope.\n")
	($exit 0))
	(($and ($eq ($loc lamp) .ME) ($eq ($prop lamp onoff) 1)) :
	($say "Half-way down the rope, the carbide lamp burns through the
rope above you and you plummet to your death.\n" )
	(exitg))
	($say "You climb down the rope without any problems.\n")
	($move .ME prm3);

ep5 = (($eq ($loc spear) .ME) :
	($say "You can't fit the spear you are carrying through that crack.\n")
	($exit 1))
	($move .ME prm6);

ep8 = ($say "You fall off the cliff and plummet several hundred feet to your
death.\n") (exitg);

dp6 = (($eq ($prop smilo stond) 0) :
	($say "As soon as you reach the bottom of the pit, the sabre-tooth
tiger tears you to shreads.\n") (exitg))
	($move .ME prm7);

ep10 = ($say "You fall into the lake, which actually turns out to be a
tar-pit covered with a thin layer of rain water.  You eventually sink and
get fossilized.\n") (exitg);

ep11 = (($eq ($prop feet wiped) 0) :
	($move .ME prm10) ($exit 1))
	($say "As you walk down the slab, your feet slip and you tumble
off the slab.\n")(ep10);

sp12 = ($say "You find your feet to be quite slippery as you walk.\n")
	($setp feet wiped 1)
	($move .ME prm11);

sp14 = (($eq ($prop mammoth blokd) 0) :
	($say "The mammoth keeps you from going that way.\n")
	($exit 1))
	($move .ME prm15);

np17 = (($and ($eq ($prop spear tooky) 1) ($eq ($prop spear abrad) 0)) :
	($say
"The rubble keeps you from going in that direction.\n")($exit 1))
	($move .ME prm18);

np19 = (($eq ($prop bear hitbr) 0) :
	($say "The bear keeps you from going that direction.\n")
	($exit 1))
	($move .ME prm20);

wp22 = (($eq ($loc spear) .ME) :
	($say "The spear that you were carrying lodges in the tyranosaur's
mouth.\n") ($move spear prm23)($move .ME prm23))
	(($eq ($loc spear) prm23) :
	($move .ME prm23))
	(($and ($ne ($loc spear) .ME) ($ne ($loc spear) prm23)) :
	($say "The tyranosaur crushes you in his jaw and then swallows you.\n")
	(exitg));

dp23 = ($say "You slide down the gullet of the tyranosaur and get 
digested alive.\n") (exitg);

dm6 = ($say "After wandering around in the forest for a while, you eventually
find your way out.\n")($move .ME mrm1);

darkq = (($or ($and ($ne ($loc .ME) ($loc lamp)) ($ne ($loc lamp) .ME))
	($eq ($prop lamp onoff) 0 )) :
	($say "It's too dark to see anything in here.\n")
	($exit 1));

darkt = (($or ($and ($ne ($loc .ME) ($loc lamp)) ($ne ($loc lamp) .ME))
	($eq ($prop lamp onoff) 0)) :
	(($pct 50) : ($say
"While stumbling around in the darkness, you trip and impale yourself on a 
stalagmite.\n")(exitg)));  

onlmp = (($ne ($loc lamp) .ME) :
	($say "You don't have the lamp with you.\n")
	($exit 1))
	(($eq ($prop lamp onoff) 0) :
	($say "A blue flame now flickers out of the lantern.\n")
	($setp lamp onoff 1)
		  ($exit 1));

{*** TRANSITIONS ***}

mrm1(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME mrm6 mrm2 mrm6 mrm6 0 0 mrm2 0 0 0)
		($miss 0 0 0 0 cg cg 0 dumdir 0 0);

mrm2(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME mrm1 mrm4 mrm3 mrm4 0 mrm5 0 0 0 0)
		($miss 0 0 0 0 cg 0 dumdir dumdir 0 0);

mrm3(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 mrm2 0 0 0 0 0 0)
		($miss cg cg em3 0 cg cg dumdir dumdir 0 0);

mrm4(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME mrm2 0 0 0 0 0 0 mrm2 0 0)
		($miss 0 cg cg cg cg cg cg 0 0 0);

mrm5(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 0 mrm2 0 0 0 0 0)
		($miss cg sm5 cg cg 0 cg dumdir dumdir 0 0);

mrm6(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 0 0 0 0 0 0 0)
		($miss dm6 dm6 dm6 dm6 cg cg dumdir dm6 0 0);

irm1(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 mrm3 0 irm2 0 0 0 0)
		($miss cg cg cg 0 cg 0 dumdir dumdir 0 0);

irm2(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 irm10 irm3 irm1 0 0 0 0 0)
		($miss cg cg 0 0 0 cg dumdir dumdir 0 0);

irm3(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME irm4 0 irm2 0 0 0 0 0  0 0)
		($miss 0 cg 0 cg cg cg dumdir dumdir 0 0);

irm4(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 irm3 0 0 0 irm5 0 irm3 0 0)
		($miss cg 0 cg cg cg 0 dumdir 0 0 0);

irm5(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME irm6 irm7 irm8 0 0 0 0 0 0 0)
		($miss 0 0 0 cg tohigh cg dumdir dumdir 0 0);

irm6(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 irm5 0 irm7 0 0 0 0 0 0)
		($miss tosml 0 cg 0 cg cg dumdir dumdir 0 0);

irm7(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME irm5 0 irm6 0 0 0 0 0 0 0)
		($miss 0 cg 0 cg cg cg dumdir dumdir 0 0);

irm8(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 irm5 0 0 0 irm5 0 0)
		($miss cg cg ei8 0 cg cg dumdir 0 0 0);

irm9(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 irm8 irm10 0 0 irm10 0 0)
		($miss cg cg cg 0 0 cg dumdir 0 0 0);

irm10(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 irm11 irm22 irm2 0 irm9 0 0 0 0)
		($miss cg 0 0 0 cg 0 dumdir dumdir 0 0);

irm11(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME irm10 irm12 0 irm13 0 0 0 0 0 0)
		($miss 0 0 cg 0 cg cg cg cg 0 0);

irm12(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME irm11 0 irm15 0 0 0 0 0 0 0)
		($miss 0 cg 0 wi12 cg cg dumdir dumdir 0 0);

irm13(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 irm15 irm11 0 0 0 0 0 0 0)
		($miss cg 0 0 cg cg cg dumdir dumdir 0 0);

irm14(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 irm12 0 0 0 0 irm12 0 0)
		($miss cg cg 0 cg cg cg dumdir 0 0 0);

irm15(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME irm13 0 irm16 irm12 0 0 0 0 0 0)
		($miss 0 cg 0 0 cg cg dumdir dumdir 0 0);

irm16(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME irm22 0 irm18 irm15 0 0 0 0 0 0)
		($miss 0 cg 0 0 ui16 cg dumdir dumdir 0 0);

irm17(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 0 0 irm16 0 0 0 0)
		($miss cg cg cg cg ui17 0 dumdir dumdir 0 0);

irm18(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 irm19 irm20 irm16 0 0 0 0 0 0)
		($miss cg 0 0 0 cg cg dumdir dumdir 0 0);

irm19(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME irm18 0 0 0 0 0 0 irm18 0 0)
		($miss 0 cg cg tosml cg di19 dumdir 0 0 0);

irm20(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 irm18 0 0 0 0 0 0)
		($miss cg cg ei20 0 cg cg dumdir dumdir 0 0);

irm21(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 0 0 0 0 0 0 0)
		($miss cg cg cg wi21 cg cg dumdir wi21 0 0);

irm22(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 irm16 0 irm10 0 0 0 0 0 0)
		($miss cg 0 cg 0 cg di22 di22 cg 0 0);

irm23(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 0 irm22 0 0 irm22 0 0)
		($miss cg cg cg cg 0 cg dumdir 0 0 0);

prm1(ACTION) = (darkt)
	($setv n s e w u d in out 0 0)
	($hit .ME mrm5 prm2 0 0 0 0 0 0 0 0)
	($miss 0 0 cg cg cg cg dumdir dumdir 0 0);

prm2(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME prm1 0 0 0 0 0 0 0 0 0)
		($miss 0 cg cg cg cg dp2 dumdir dumdir 0 0);

prm3(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 prm4 prm2 0 0 0 0 0)
		($miss cg cg cg 0 0 cg dumdir dumdir 0 0);

prm4(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 prm5 prm3 prm9 0 0 prm5 0 0 0)
		($miss cg 0 0 0 cg cg 0 dumdir 0 0);

prm5(ACTION) = (darkt) ($setv n s e w u d in out 0 0)
		($hit .ME prm4 0 0 0 0 0 0 prm4 0 0)
		($miss 0 cg ep5 cg cg cg dumdir 0 0 0);

prm6(ACTION) = (darkt) ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 prm5 0 0 0 0 0 0)
		($miss cg cg cg 0 cg dp6 dumdir dumdir 0 0);

prm7(ACTION) = (darkt) ($setv n s e w u d in out 0 0)
		($hit .ME 0 prm8 0 0 prm6 0 0 0 0 0)
		($miss cg 0 cg cg 0 cg dumdir dumdir 0 0);

prm8(ACTION) = (darkt) ($setv n s e w u d in out 0 0)
		($hit .ME prm7 0 0 0 0 0 0 0 0 0)
		($miss 0 cg ep8 cg cg cg dumdir dumdir 0 0);

prm9(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME prm10 0 prm4 prm13 0 0 0 0 0 0)
		($miss 0 cg 0 0 cg cg dumdir dumdir 0 0);

prm10(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 prm9 0 prm11 prm11 0 0 0 0 0)
		($miss cg 0 ep10 0 0 cg dumdir dumdir 0 0);

prm11(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME prm12 0 0 0 0 0 0 0 0 0)
		($miss 0 cg ep11 cg cg ep11 dumdir dumdir 0 0);

prm12(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 0 0 0 0 0 0 0)
		($miss cg  sp12 cg cg cg cg dumdir dumdir 0 0);

prm13(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 prm14 prm9 prm16 0 0 0 0 0 0)
		($miss cg 0 0 0 cg cg dumdir dumdir 0 0);

prm14(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME prm13 0 0 0 0 0 0 0 0 0)
		($miss 0 sp14 tosml cg cg cg dumdir dumdir 0 0);

prm15(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME prm14 0 0 0 0 0 0 0 0 0)
		($miss 0 cg cg cg cg cg dumdir dumdir 0 0);

prm16(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME prm17 0 prm13 0 0 prm21 0 0 0 0)
		($miss 0 cg 0 cg cg 0 dumdir dumdir 0 0);

prm17(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 prm16 0 0 0 0 0 0 0 0)
		($miss np17 0 cg cg cg cg dumdir dumdir 0 0);

prm18(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME prm19 prm17 0 0 0 0 0 0 0 0)
		($miss 0 0 cg cg cg cg dumdir dumdir 0 0);

prm19(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 prm18 0 0 0 0 0 0 0 0)
		($miss np19 0 cg cg cg cg dumdir dumdir 0 0);

prm20(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 prm19 0 0 0 0 0 prm19 0 0)
		($miss cg 0 cg cg cg cg dumdir 0 0 0);

prm21(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 0 prm16 prm22 0 0 0 0)
		($miss cg cg cg cg 0 0 dumdir dumdir 0 0);

prm22(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 0 0 prm21 0 0 0 0 0)
		($miss cg cg cg wp22 0 cg dumdir dumdir 0 0);

prm23(ACTION) = ($setv n s e w u d in out 0 0)
		($hit .ME 0 0 prm22 0 0 0 0 prm22 0 0)
		($miss cg cg 0 cg cg dp23 dumdir 0 0 0);

{*** CHEAT ROUTINES ***}

chetr = (($eq ($prop .ME debug) 0) : ($exit 1));
VERB BEAM;
BEAM(ACTION) = (chetr) ($move .ME ($dobj))(LLOOK);
VERB TRACTOR;
TRACTOR(ACTION) = (chetr) ($move ($dobj) ($loc .ME))
		($say "The ")(($sdisc ($dobj)))($say
" magically appears.\n");
VERB DRIBBLE;
DRIBBLE(ACTION) = (chetr) (ctake) (cdrop);
