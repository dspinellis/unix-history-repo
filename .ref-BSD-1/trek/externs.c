# include	"trek.h"

/*
**	global variable definitions
*/

struct device	Device[NDEV]
{
	"warp drive",		"Scotty",
	"S.R. scanners",	"Scotty",
	"L.R. scanners",	"Scotty",
	"phasers",		"Sulu",
	"photon tubes",		"Sulu",
	"impulse engines",	"Scotty",
	"shield control",	"Sulu",
	"computer",		"Spock",
	"subspace radio",	"Uhura",
	"life support",		"Scotty",
	"navigation system",	"Chekov",
	"cloaking device",	"Scotty",
	"transporter",		"Scotty",
	"shuttlecraft",		"Scotty",
	"*ERR 14*",		"Nobody",
	"*ERR 15*",		"Nobody"
};

char	*Systemname[NINHAB]
{
	"ERROR",
	"Talos IV",
	"Rigel III",
	"Deneb VII",
	"Canopus V",
	"Icarus I",
	"Prometheus II",
	"Omega VII",
	"Elysium I",
	"Scalos IV",
	"Procyon IV",
	"Arachnid I",
	"Argo VIII",
	"Triad III",
	"Echo IV",
	"Nimrod III",
	"Nemisis IV",
	"Centarurus I",
	"Kronos III",
	"Spectros V",
	"Beta III",
	"Gamma Tranguli VI",
	"Pyris III",
	"Triachus",
	"Marcus XII",
	"Kaland",
	"Ardana",
	"Stratos",
	"Eden",
	"Arrikis",
	"Epsilon Erandi IV",
	"Exo III"
};
