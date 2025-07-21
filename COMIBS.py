# replaces COMIBS.blk
from dataclasses import dataclass
from numpy import ndarray
from UTILS import FrozenMeta



# Class for common block IBS01 in COMIBS.blk variables
@dataclass
class IBS01(metaclass=FrozenMeta):
    PLME, PLDS, DFDRN, FLST = [0.0] * 4
    FLDNAM, ISIMI, TITSIM, IOFF, IAME = [' '] * 5
    NCODE, NEND = [' '] * 2
    DAYFAC, RADFAC, TXFAC, TMFAC = [ndarray(9000, str)] * 4
    PRCFAC, CO2FAC, DPTFAC, WNDFAC, HSTG, HCOM, HSIZ = [ndarray(9000, str)] * 7
    IOFFX, IAMEX = [' '] * 2
    IRRCOD, RESCOD, FERCOD, FOCOD, IFTYPE = [ndarray(9000, str)] * 5
    EXPER, WSTA, CROPD, SLTX = [' '] * 4
    RMET = ndarray(9000, str)
    CHCOD, CHMET, CHT, TIMPL = [ndarray(9000, float)] * 4
    FldHist = ' '

# Class for common block IBS02 in COMIBS.blk variables
@dataclass
class IBS02(metaclass=FrozenMeta):
    YRSIM, YRPLT, IEMRG, TRTNO, ROTNO, ROTOPT, CRPNO, RSEED1 = [0] * 8
    PWDINF, PWDINL, NRESDL, HDLAY, HLATE = [0] * 5
    NEV, NHAR, NIRR = [0] * 3
    WMDATE, HDATE, IDLAPL, RESDAY = [ndarray(9000, int)] * 4
    FDAY = [ndarray(9000, int)] * 4
    NFERT, YEAR, NARES, NAPW, NREPSQ = [0] * 5
    NCHEM, NTIL, FHDur, FODAT = [0] * 4
    CDATE, TDATE = [ndarray(9000, int)] * 2

# Class for common block IBS03 in COMIBS.blk variables
@dataclass
class IBS03(metaclass=FrozenMeta):
    PLANTS, PLTPOP, ROWSPC, AZIR, SDEPTH, SDWTPL = [0.0] * 6
    SDAGE, ATEMP, PLPH, FLOB, FLDD, SFDRN, SWPLTL, SWPLTH = [0.0] * 8
    SWPLTD, PTX, PTTN, DSOIL, THETAC, IEPT, AIRAMT, EFFIRR = [0.0] * 8
    DSOILN, SOILNC, SOILNX, RIP, DRESMG, HPP, HRP = [0.0] * 7
    HBPC = ndarray(9000, float)
    DAYADJ, RADADJ, TXADJ, TMADJ, PRCADJ = [ndarray(9000, float)] * 5
    CO2ADJ, DPTADJ, WNDADJ, HPC = [ndarray(9000, float)] * 4
    DSOILX, THETCX, IEPTX = [0.0] * 3
    AIRAMX, EFFIRX = [0.0] * 2
    AMT, RESN, RESP, RESK, RESIDUE = [ndarray(9000, float)] * 5
    RINP, DEPRES, DFERT, ANFER, APFER, AKFER, ACFER, AOFER = [ndarray(9000, float)] * 8
    SLOPE, XCRD, YCRD, ELEV, AREA, SLEN, FLWR, SLAS = [0.0] * 8
    CHAMT, CHDEP, TDEP = [ndarray(9000, float)] * 3
    SPRLAP =  0.0

# Class for common block IBS04 in COMIBS.blk variables
@dataclass
class IBS04(metaclass=FrozenMeta):
    TOTNAP, RESAMT, TOTAPW, WTHADJ, SLDP = [0.0] * 5
#    WTHADJ(2,8) float
    ICWD, ICRES, ICREN, ICREP, ICRIP, ICRID, PMALB = [0.0] * 7