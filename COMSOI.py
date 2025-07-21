from dataclasses import dataclass
from UTILS import create_float_array, FrozenMeta
from ModuleDefs import NL
# replaces COMSOI.blk

# Class for common block SOI01 in COMSOI.blk variables
@dataclass
class SOI01(metaclass=FrozenMeta):
    PEDON, SLTXS, TAXON, SLSOUR, SSITE, SCOUNT, SLDESC, SLNO = [' '] * 8
    SMHB, SMPX, SMKE, SGRP, SCOM = [' '] * 5

# Class for common block SOI02 in COMSOI.blk variables
@dataclass
class SOI02(metaclass=FrozenMeta):
    NLAYR: int = 20

# Class for common block SOI03 in COMSOI.blk variables
@dataclass
class SOI03(metaclass=FrozenMeta):
    U, SWCON, CN2, SALB, DEPMAX = [0.0] * 5
    LL, DUL, SAT = (create_float_array(NL) for _ in range(3))
    SHF, SWCN, BD, OC, PH, DLAYR, EXTP, TOTP, ORGP = (create_float_array(NL) for _ in range(9))
    SLNF, SLPF = [0.0] * 2
    DS, CEC, ADCOEF, STONES, CLAY, SILT, PHKCL = (create_float_array(NL) for _ in range(7))
    SLAT, SLONG = [0.0] * 2
    CACO, EXTAL, EXTFE, EXTMN, TOTBAS, TOTN = (create_float_array(NL) for _ in range(6))
    PTERMA, PTERMB, EXK, EXMG, EXNA, EXTS, SLEC, EXCA, SASC = (create_float_array(NL) for _ in range(9))
    SAEA = create_float_array(NL)
    alphaVG, mVG, nVG, WCR = (create_float_array(NL) for _ in range(4))
