# replaces COMGEN.blk
import numpy as np
from dataclasses import dataclass
from UTILS import *


# Class for common block GEN01 in comgen.blk variables
@dataclass
class GEN01(metaclass=FrozenMeta):
    P1, P1V, P1D, P2, P2O, P2R, P3, P4, P5, G1, G2, G3, G4, PHINT, O1 = [0.0] * 15
    PBASE, PSAT, AX, LX, PANTH, G0, G5 = [0.0] * 7
    LFN = 0
    B01ND, B12ND, B23ND, B34ND, B45ND, B56ND = [0.0] * 6
    BR1FX, BR2FX, BR3FX, BR4FX = [0.0] * 4
    SRNWT, SRFR, HMPC, LA1S, LAXS, LAXND = [0.0] * 6
    LAXN2, LAFS, LAFND, SLASS, LLIFA, LPEFR, STFR = [0.0] * 7
    VREQ, VBASE, VEFF, PPS1, PPS2 = [0.0] * 5
    P6, P7, P8 = [0.0] * 3
    GRNOW, GRWTX, SHWMS = [0.0] * 3
    GNOWT, GWTS, SHWTS, LAFV, LAFR = [0.0] * 5
    LNSLP, NODWT, NODLT = [0.0] * 3
# !WDB Add new sugarbeet coefficients
    PHYL1, PHYL2, FRSUG, DRCER = [0.0] * 4

# Class for common block GEN02 in comgen.blk variables
@dataclass
class GEN02(metaclass=FrozenMeta):
    PD, TC, GPROT, LT50H = [0.0] * 4

# Class for common block GEN03 in comgen.blk variables
@dataclass
class GEN03(metaclass=FrozenMeta):
    CSDVAR, SDPDVR, SLAVAR, LFMAX, XFRUIT, WTPSD = [0.0] * 6
    SFDUR, PODUR, PPSEN, PH2T5, SIZELF, THRESH, SDPRO, SDLIP = [0.0] * 8
    PHTHRS = np.zeros(20)
    # PHTHRS = create_float_array(20)

# Class for common block GEN04 in comgen.blk variables
@dataclass
class GEN04(metaclass=FrozenMeta):
    GCOEFF = create_float_array(15)

# Class for common block GEN05 in comgen.blk variables
@dataclass
class GEN05(metaclass=FrozenMeta):
    RATPOT, PI1, PI2, DTTPI = [0.0] * 4

# Class for common block GEN06 in comgen.blk variables
@dataclass
class GEN06(metaclass=FrozenMeta):
    PCINT, PCGRD = [0.0] * 2

# Class for common block GEN07 in comgen.blk variables
@dataclass
class GEN07(metaclass=FrozenMeta):
    SCPB, RESPC, SQCON, FCUT, FLAI, DDISQ = [0.0] * 6

# Class for common block GEN08 in comgen.blk variables
@dataclass
class GEN08(metaclass=FrozenMeta):
    DTPI, SMAX, SO, GMAX, GO, M1, M2, SLAVR, SIZLF = [0.0] * 9
    LA = 0

# Class for common block GEN10 in comgen.blk variables
@dataclass
class GEN10(metaclass=FrozenMeta):
    PLAINTXT : str

# Class for common block GEN11 in comgen.blk variables
@dataclass
class GEN11(metaclass=FrozenMeta):
    PHTMAX, StkH2OFac, SuH2OFac, empty, PLF1, PLF2, Gamma = [0.0] * 7
    LIsun, LIshd = [0.0] * 2
    TB, TO1, TO2, TM = [create_float_array(3)] * 4
    StkB, StkM, LI1, TELOM, LSFAC = [0.0] * 5
    Ph1P, Ph1R, Ph2, Ph3, Ph4, StkHrNO, RTNFAC, MinGr = [0.0] * 8
    RTNNO, RES30C, RLF30C, R30C2 = [0.0] * 4

# Class for common block GEN12 in comgen.blk variables
@dataclass
class GEN12(metaclass=FrozenMeta):
    MaxPARCE, APFMX, STKPFMAX, SUCA, TBFT = [0.0] * 5
    MXLFAREA, MXLFARNO, PSWITCH, TTPLNTEM = [0.0] * 4
    TTRATNEM, CHUPIBASE, TT_POPGROWTH, POPTT16 = [0.0] * 4
    TAR0, TDELAY, LER0, SER0, LG_AMBASE, AQP_UP5 = [0.0] * 6

#Only missing GEN13-15 but those are for cereal and likely not used in CROPGRO