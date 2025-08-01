# Contains definitions of derived data types and constants which are used throughout the model.

import numpy as np
from dataclasses import dataclass, field
from typing import List

# Global constants
NL : int = 20         # Maximum number of soil layers
TS : int = 24         # Number of hourly time steps per day
NAPPL : int = 9000    # Maximum number of applications or operations
NCOHORTS: int = 300   # Maximum number of cohorts
NPHS: int = 13
MaxFiles : int = 500  # Maximum number of output files
EvaluateNum : int = 40
NELEM : int   = 3   #Number of elements modeled (currently N & P)
# Note: set NELEM to 3 for now so Century arrays will match
N : int   = 1

PI = 3.14159265
RAD=PI/180.0

#Temporary version data
Version = 0
VBranch = "Development"
MonthTxt = "A"


# STDPATH = "C:/CropGro/"
STDPATH = "C:\\DSSAT48\\"

NOHEADER = {}

VPDFPHR = np.zeros(TS, dtype=float)               # Hourly VPD factor, 0-1
EOPH = np.zeros(TS, dtype=float)                  # Hourly potential transpiration

#Global parameters for control
@dataclass(frozen=True)
class RunConstants:
    RUNINIT : int = 1
    INIT : int = 2
    SEASINIT : int = 2
    RATE : int = 3
    EMERG : int = 3  #Used for some plant processes.
    INTEGR : int = 4
    OUTPUT : int = 5
    SEASEND : int = 6
    ENDRUN : int = 7



# MAKEFILEW VARIABLES
FirstWeatherDate : int = -99
NEWSDATE : int = -99

# Class for Array of output files, aliases, unit numbers, etc.
@dataclass
class OutputType:
    FileName : list[str] = field(default_factory=list)    # 16 char
    OPCODE : list[str] = field(default_factory=list)     # 2 char
                                                    
    Description : list[str] = field(default_factory=list) # 50 char
    ALIAS : list[str] = field(default_factory=list) # 50 char
    LUN : list[int] = field(default_factory=list)
    NumFiles: int = 0

# Class for control variables
@dataclass
class ControlType:
    RNMODE = ' '
    CROP = ' '
    MODEL, ENAME = ' ' * 2
    FILEX = ' '
    FILEIO = ' '
    DSSATP: str = ' '
    DAS = 0            # Days after simulation
    DYNAMIC = 1        # Integer indicating part of simulation
    LUNIO : int = -99
    YRDOY = 0          # Year and day of year
    YRSIM = 0          # Year of simulation
    ROTNUM, RUN, TRTNUM, ERRCODE, MULTI = [0] * 5

    CropStatus = 0
    SimControl = ' ' * 120

# Class for switch variables
@dataclass
class SwitchType:
    ICO2 : str = ' '
    IDETL : str = ' '
    IFERI : str = ' '
    IHARI : str = ' '
    IIRRI : str = ' '
    IPLTI : str = ' '
    IRESI : str = ' '
    ISIMI : str = ' '
    ISWNIT : str = ' '
    ISWPHO : str = ' '
    ISWSYM : str = ' '
    ISWTIL : str = ' '
    ISWWAT : str = ' '
    MEEVP : str = ' '
    MEHYD : str = ' '
    MEINF : str = ' '
    MEPHO : str = ' '
    MESEV : str = ' '
    MESOL : str = ' '
    METMP : str = ' '
    MEWTH : str = ' '
    NSWI : int = 0

# Class for header info for output files
@dataclass
class HeaderType:
    ICOUNT : int = 0
    ShortCount : int = 0
    RUN : int = 0
    Header = [''] * 100   #header for Overview

Headers = HeaderType()

@dataclass
class WeathType:
    WYEAR : int = 0
    WSTAT : str =" "
    CELEV : str =" "
    CYCRD : str =" "
    CXCRD : str =" "



@dataclass
class WaterType:
    WTDEP : int = 0


@dataclass
class MHarveType:
    HARVF: int = -99
    ISH_date: int = -99
    ISH_wt: int = -99

@dataclass
#Data transferred from management routine
class MgmtType:
    # REAL DEPIR, EFFIRR, FERNIT, IRRAMT, TOTIR, TOTEFFIRR
    # REAL MgmtWTD, ICWD
#   Vectors to save growth stage based irrigation
    V_AVWAT = [-99.0]*20
    V_IMDEP = [-99.0]*20
    V_ITHRL = [-99.0]*20
    V_ITHRU = [-99.0]*20
    V_IRON = [-99] *20
    V_IRAMT = [-99.0]*20
    V_IREFF = [-99.0]*20
    V_IFREQ = [-99] *20
    GSIRRIG = [-99] *20
    V_IRONC = ["     "]*20  #5 char
    ICWD = -99.0            #Initial water table depth (cm)
    ActWTD = -99.0          #Depth to water table (cm)
    WTDEP = -99.0
    WATTAB = -99.0

@dataclass
# Data construct for weather variables
class WeatherType:
    # Weather station information
    REFHT, WINDHT, XLAT, XLONG, XELEV = [0.0] * 5

    # Daily weather data
    CLOUDS, CO2, DAYL, DCO2, PAR, RAIN, RHUM, SNDN, SNUP, \
    SRAD, TAMP, TA, TAV, TAVG, TDAY, TDEW, TGROAV, TGRODY, \
    TMAX, TMIN, TWILEN, VAPR, WINDRUN, WINDSP, VPDF, VPD_TRANSP, \
    OZON7 = [0.0] * 27

    NOTDEW : bool = False
    NOWIND : bool = False

    # Hourly weather data
    AMTRH = np.zeros(TS, dtype=float)
    AZZON = np.zeros(TS, dtype=float)
    BETA = np.zeros(TS, dtype=float)
    FRDIFP = np.zeros(TS, dtype=float)
    FRDIFR = np.zeros(TS, dtype=float)
    PARHR = np.zeros(TS, dtype=float)
    RADHR = np.zeros(TS, dtype=float)
    RHUMHR = np.zeros(TS, dtype=float)
    TAIRHR = np.zeros(TS, dtype=float)
    TGRO = np.zeros(TS, dtype=float)
    WINDHR = np.zeros(TS, dtype=float)


@dataclass
# Data construct for Fertilizer application data
class FertType:
    FERTDAY : int = 0
    FERTYPE : int = 0

@dataclass
# Data construct for residue (harvest residue, senesced matter, etc.)
class ResidueType:
    CumResWt : float = 0.0                      #cumul. kg[dry matter]/ha
    ResWt = np.zeros(NL, dtype=float)           #kg[dry matter]/ha/d
    ResLig = np.zeros(NL, dtype=float)          #kg[lignin]/ha/d
    CumResE = np.zeros(NELEM, dtype=float)      #cumulative kg[E]/ha
    ResE = np.zeros((NL,NELEM), dtype=float) #kg[E]/ha/d (E=N,P,K,..)

@dataclass
# Data construct for Organic Matter Application data
class OrgMatAppType:
    CumResWt : float = 0.0                         #cumul. kg[dry matter]/ha

@dataclass
# Data construct for tillage operations
class TillType:
     NTIL : int = 0     #Total number of tillage events in FILEX

@dataclass
# Data construct for flood data
class FloodNType:
    NDAT : int = 0

@dataclass
# Data construct for flood data
class FloodWatType:
    CEF : float = 0.0
    EF : float = 0.0

@dataclass
# Data construct for mulch layer
class MulchType:
    MULCHMASS : float = 0.0    #Mass of surface mulch layer (kg[dry mat.]/ha)
    MULCHALB : float = 0.0     #Albedo of mulch layer

@dataclass
class SoilType:
      CN: float = 0.0
      DLAYR = [0.0] * NL #TODO this will create a class attribute, not an instance attribute. Might need to changes this
      DS    = [0.0] * NL #How are these different from the values stored in COMSOI?
      DUL   = [0.0] * NL
      ETDR: float = 0.0
      LL    = [0.0] * NL
      NLAYR: int = 0
      SAT   = [0.0] * NL
      SWCN  = [0.0] * NL
      SWCON: float = 0.0
      TEXTURE = np.empty(NL,dtype='U12')
      POROS = np.zeros(NL, dtype=float)

@dataclass
class CH4_type:
    CH4Consumption : float = 0.0
    CH4Emission : float = 0.0
    CH4Leaching : float = 0.0
    CH4Stored : float = 0.0
    CumCH4Consumpt :  float = 0.0
    CumCH4Emission :  float = 0.0
    CumCH4Leaching : float = 0.0
    CO2emission : float = 0.0
    CumCO2Emission : float = 0.0

# Data transferred from CROPGRO routine
@dataclass
class PlantType:
    CANHT : float = 0.0
    CANWH : float = 0.0
    DXR57 : float = 0.0
    EXCESS : float = 0.0
    PLTPOP : float = 0.0
    RNITP : float = 0.0
    SLAAD : float = 0.0
    XPOD : float = 0.0
    BIOMAS : float = 0.0
    NR5 : int = 0
    iSTAGE : int = 0
    iSTGDOY : int = 0
    iSTNAME : str = ' '

# Data which can be transferred between modules
@dataclass
class TransferType:
    CONTROL : ControlType
    ISWITCH : SwitchType
    WEATH : WeathType   # Supplemental weather data
    SOILPROP : SoilType
    MHARVEST : MHarveType
    MGMT : MgmtType
    WEATHER : WeatherType
    OUTPUT : OutputType
    WATER : WaterType

# The variable SAVE_data contains all of the components to be
# stored and retrieved.
CONTROL = ControlType()
ISWITCH = SwitchType()
WEATH = WeathType()
SOILPROP = SoilType()
MHARVEST = MHarveType()
MGMT = MgmtType()
WEATHER = WeatherType()
OUTPUT = OutputType()
WATER = WaterType

SAVE_data = TransferType(CONTROL, ISWITCH, WEATH, SOILPROP, MHARVEST, MGMT, WEATHER, OUTPUT, WATER)

def PUT_Char(ModuleName, VarName, Value):
    from WARNING import WARNING
    MSG = []
    ERR = False

    match ModuleName:
        case 'WEATHER':
            match VarName:
                case 'WSTA' : SAVE_data.WEATH.WSTAT  = Value
                case _: ERR = True

        case 'FIELD':
            match VarName:
                case 'CXCRD' : SAVE_data.WEATH.CXCRD = Value
                case 'CYCRD' : SAVE_data.WEATH.CYCRD = Value
                case 'CELEV' : SAVE_data.WEATH.CELEV = Value
                case _: ERR = True

        case 'PLANT':
            match VarName:
                case 'iSTNAME' : SAVE_data.PLANT.iSTNAME = Value
                case _: ERR = True
        case _: ERR = True

    if ERR:
        MSG[1] ='Error transferring variable'
        MSG[2] ='Value not saved! Errors may result.'
        WARNING(2, 'PUT_Integer',MSG)

    return Value

def Put_CONTROL (CONTROL_arg):
    #CONTROL_arg = ControlType()
    SAVE_data.CONTROL = CONTROL_arg

def Put_ISWITCH (ISWITCH_arg):
    #ISWITCH_arg = SwitchType()
    SAVE_data.ISWITCH = ISWITCH_arg

def Put_WEATHER (WEATHER_arg):
    #ISWITCH_arg = SwitchType()
    SAVE_data.WEATHER = WEATHER_arg

def PUT_SOILPROP(SOIL_ARG):
    SAVE_data.SOILPROP = SOIL_ARG

def PUT_Integer(ModuleName, VarName, Value):
    ERR = False
    from WARNING import WARNING
    match ModuleName:
        case 'WEATHER':
            match VarName:
                case 'WYEAR':
                    SAVE_data.WEATH.WYEAR = Value
                case _:
                    ERR = True
        case 'MHARVEST':
            match VarName:
                case 'HARVF':
                    SAVE_data.MHARVEST.HARVF = Value
                case 'ISH_date':
                    SAVE_data.MHARVEST.ISH_date = Value
                case _:
                    ERR = True
        case _:
            ERR = True

    if ERR:
        MSG = [None] * 2
        MSG[0] = f"Error transferring variable: {VarName} in {ModuleName}"
        MSG[1] = 'Value not saved! Errors may result.'
        WARNING(2, 'PUT_Integer', MSG)
    return

def PUT_float(ModuleName, VarName, Value):
    ERR = False
    from WARNING import WARNING

    match ModuleName:
        case 'SPAM':
            match VarName:
                case 'AGEFAC': SAVE_data.SPAM.AGEFAC = Value
                case 'PG' : SAVE_data.SPAM.PG = Value
                case _:
                    ERR = True
        case _:
            ERR = True

    if ERR:
        MSG = [None] * 2
        MSG[0] = f"Error transferring variable: {VarName} in {ModuleName}"
        MSG[1] = 'Value not saved! Errors may result.'
        WARNING(2, 'PUT_Float', MSG)

    return Value

def PUT_OUTPUT(OUTPUT_arg):
    SAVE_data.OUTPUT = OUTPUT_arg

# def GET(var):
#     """
#     :param var: An instance of classtype
#     :return: The corresponding classtype stored in SAVE_data
#     """
#     if isinstance(var, ControlType):
#         return SAVE_data.CONTROL
#     elif isinstance(var, SwitchType):
#         return SAVE_data.ISWITCH

def GET_CONTROL():
    return SAVE_data.CONTROL

def GET_ISWITCH():
    return SAVE_data.ISWITCH

def GET_WEATHER():
    return SAVE_data.WEATHER

def GET_Char(ModuleName, VarName, Value):
    from WARNING import WARNING
    if ModuleName == 'WEATHER':
        if VarName == 'WSTA':
            Value = SAVE_data.WEATH.WSTAT
        else:
            ERR = True

    if ERR:
        MSG = [None] * 2
        MSG[0] = f"Error transferring variable: {VarName} in {ModuleName}"
        MSG[1] = 'Value set to zero.'
        WARNING(2, 'GET_Integer', MSG)
    return

def GET_float(ModuleName, VarName):
    match ModuleName:
        case 'PLANT':
            match VarName:
                case 'BIOMAS': Value = SAVE_data.PLANT.BIOMAS
                case 'CANHT' : Value = SAVE_data.PLANT.CANHT
                case 'CANWH' : Value = SAVE_data.PLANT.CANWH
                case 'DXR57' : Value = SAVE_data.PLANT.DXR57
                case 'EXCESS': Value = SAVE_data.PLANT.EXCESS
                case 'PLTPOP': Value = SAVE_data.PLANT.PLTPOP
                case 'RNITP' : Value = SAVE_data.PLANT.RNITP
                case 'SLAAD' : Value = SAVE_data.PLANT.SLAAD
                case 'XPOD'  : Value = SAVE_data.PLANT.XPOD
                case _: ERR = True
        case 'SPAM':
            match VarName:
                case 'AGEFAC': Value = SAVE_data.SPAM.AGEFAC
                case 'PG':     Value = SAVE_data.SPAM.PG
                case 'CEF':    Value = SAVE_data.SPAM.CEF
                case 'CEM':    Value = SAVE_data.SPAM.CEM
                case 'CEO':    Value = SAVE_data.SPAM.CEO
                case 'CEP':    Value = SAVE_data.SPAM.CEP
                case 'CES':    Value = SAVE_data.SPAM.CES
                case 'CET':    Value = SAVE_data.SPAM.CET
                case 'CEVAP':  Value = SAVE_data.SPAM.CEVAP
                case 'EF':     Value = SAVE_data.SPAM.EF
                case 'EM':     Value = SAVE_data.SPAM.EM
                case 'EO':     Value = SAVE_data.SPAM.EO
                case 'EP':     Value = SAVE_data.SPAM.EP
                case 'ES':     Value = SAVE_data.SPAM.ES
                case 'ET':     Value = SAVE_data.SPAM.ET
                case 'EOP':    Value = SAVE_data.SPAM.EOP
                case 'EVAP':   Value = SAVE_data.SPAM.EVAP
                case 'REFET':  Value = SAVE_data.SPAM.REFET
                case 'SKC':    Value = SAVE_data.SPAM.SKC
                case 'KCBMAX': Value = SAVE_data.SPAM.KCBMAX
                case 'KCB':    Value = SAVE_data.SPAM.KCB
                case 'KE':     Value = SAVE_data.SPAM.KE
                case 'KC':     Value = SAVE_data.SPAM.KC
                case 'PHSV':   Value = SAVE_data.SPAM.PHSV
                case 'PHTV':   Value = SAVE_data.SPAM.PHTV
                case _: ERR = True
    return Value

def GET_OUTPUT():
    return SAVE_data.OUTPUT

#Control type dictionary
ControlDict = {
    "MESIC": "", "RNMODE": "",               # CHARACTER(len=1)
    "CROP": "",                              # CHARACTER(len=2)
    "MODEL": "", "ENAME": "",                # CHARACTER(len=8)
    "FILEX": "",                             # CHARACTER(len=12)
    "FILEIO": "",                            # CHARACTER(len=30)
    "DSSATP": "",                            # CHARACTER(len=102)
    "SimControl": " " * 120,                 # CHARACTER(len=120)
    "DAS": 0, "DYNAMIC": 0, "FROP": 0,       # INTEGER
    "ErrCode": 0, "LUNIO": 0, "MULTI": 0,
    "N_ELEMS": 0, "NYRS": 0, "REPNO": 0,
    "ROTNUM": 0, "RUN": 0, "TRTNUM": 0,
    "YRDIF": 0, "YRDOY": 0, "YRSIM": 0,
    "FODAT": 0, "ENDYRS": 0, "CropStatus": 0 # INTEGER
}

# Python dictionary for SwitchType
SwitchTypeDict = {
    "FNAME": "",                             # CHARACTER(len=1)
    "IDETC": "", "IDETD": "", "IDETG": "", "IDETH": "", "IDETL": "", "IDETN": "", # CHARACTER(len=1)
    "IDETO": "", "IDETP": "", "IDETR": "", "IDETS": "", "IDETW": "",              # CHARACTER(len=1)
    "IHARI": "", "IPLTI": "", "IIRRI": "", "ISIMI": "",                          # CHARACTER(len=1)
    "ISWCHE": "", "ISWDIS": "", "ISWNIT": "",                                     # CHARACTER(len=1)
    "ISWPHO": "", "ISWPOT": "", "ISWSYM": "", "ISWTIL": "", "ISWWAT": "",         # CHARACTER(len=1)
    "MEEVP": "", "MEGHG": "", "MEHYD": "", "MEINF": "", "MELI": "", "MEPHO": "",  # CHARACTER(len=1)
    "MESOM": "", "MESOL": "", "MESEV": "", "MEWTH": "",                          # CHARACTER(len=1)
    "METMP": "",                                                                  # CHARACTER(len=1)
    "IFERI": "", "IRESI": "", "ICO2": "", "FMOPT": "",                            # CHARACTER(len=1)
    "NSWI": 0                                                                     # INTEGER
}

#without instances
# class ControlClass2:
#     DAS = 0
#     DYNAMIC=0
#     YRDOY = 0
#     YRSIM = 0
#     CROP = ''
#     FILEIO = ''