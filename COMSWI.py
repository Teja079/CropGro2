from dataclasses import dataclass
from UTILS import *
from ModuleDefs import *
# replaces COMSWI.blk

# Class for common block SWITCH in COMSWI.blk variables
@dataclass
class SWITCH(metaclass=FrozenMeta):
    ISWWAT, ISWNIT, ISWSYM, ISWPHO, ISWPOT, ISWDIS, ICO2 = [' '] * 7
    ISWCHE, ISWTIL, MEWTH, MESIC, MELI, MEEVP, MEINF, MEPHO, MEHYD = [' '] * 9
    IDETO, IDETS, IDETG, IDETC, IDETW, IDETN, IDETP, IDETD, IDETL = [' '] * 9
    IOX, IDETH, IDETR, IPLTI, IIRRI, IFERI, IRESI, IHARI = [' '] * 8
    FILEA, FILEC, FILEE, FILEG, FILES, FILET, FILEW, FILEP = [' '] * 8
    FILEWC, FILEWG = [' '] * 2
    FILEF, FILEH, PATHSL, PATHWT, PATHCR, PATHGE, PATHPE, PATHEC = [' '] * 8
    PATHWTC, PATHWTG, PATHWTW = [' '] * 3
    OUTO, OUTP, OUTD, OUTF, OUTH, OUTR = [' '] * 6
    CG, TITLER, DSSATP, ENAME, MESOM, METMP, MEGHG = [' '] * 7

# Class for common block SWITCH1 in COMSWI.blk variables
@dataclass
class SWITCH1(metaclass=FrozenMeta):
    NSWITCH : int = -99
    MESOL, MESEV = [' '] * 2

# Class for common block SWITCH2 in COMSWI.blk variables
@dataclass
class SWITCH2(metaclass=FrozenMeta):
    DBUG1, DBUG2 = [0] * 2