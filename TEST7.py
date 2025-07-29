# !===========================================================================
# ! Variable listing for Alt_Plant - updated 08/18/2003
# ! --------------------------------------------------------------------------
# ! CANHT     Canopy height (m)
# ! CO2       Atmospheric carbon dioxide concentration (µmol[CO2] / mol[air])
# ! CONTROL   Composite variable containing variables related to control
# !             and/or timing of simulation.  The structure of the variable
# !             (ControlType) is defined in ModuleDefs.for.
# ! CROP      Crop identification code
# ! DAYL      Day length on day of simulation (from sunrise to sunset) (hr)
# ! EOP       Potential plant transpiration rate (mm/d)
# ! EORATIO   Ratio of increase in EO with increase in LAI (up to LAI=6.0)
# !             for use with FAO-56 Penman reference EO.
# ! ERRKEY    Subroutine name for error file
# ! FIXCANHT  Logical variable, =TRUE if default canopy height is to be set
# !             by Alt_Plant routine upon emergence
# ! FLOODN    Composite variable which contains flood nitrogen mass and
# !             concentrations. Structure of variable is defined in
# !             ModuleDefs.for. (var.)
# ! FLOODWAT  Composite variable containing information related to bund
# !             management. Structure of variable is defined in ModuleDefs.for.
# ! HARVFRAC  Two-element array containing fractions of (1) yield harvested
# !             and (2) by-product harvested (fraction)
# ! HARVRES   Composite variable containing harvest residue amounts for total
# !             dry matter, lignin, and N amounts.  Structure of variable is
# !             defined in ModuleDefs.for.
# ! ISWITCH   Composite variable containing switches which control flow of
# !             execution for model.  The structure of the variable
# !             (SwitchType) is defined in ModuleDefs.for.
# ! IRRAMT    Irrigation amount (mm)
# ! KCAN      Canopy light extinction coefficient for daily PAR, for
# !             equidistant plant spacing, modified when in-row and between
# !             row spacing are not equal
# ! KEP       Energy extinction coefficient for partitioning EO to EP
# ! KSEVAP    Light extinction coefficient used for computation of soil
# !             evaporation
# ! KTRANS    Light extinction coefficient used for computation of plant
# !             transpiration
# ! MDATE     Harvest maturity date (YYYYDDD)
# ! MEEVP     Method of evapotranspiration ('P'=Penman, 'R'=Priestly-Taylor,
# !             'Z'=Zonal)
# ! MESSAGE   Text array containing information to be written to WARNING.OUT
# !             file.
# ! MODEL     Name of CROPGRO executable file
# ! NH4(L)    Ammonium N in soil layer L (µg[N] / g[soil])
# ! NL        Maximum number of soil layers = 20
# ! NO3(L)    Nitrate in soil layer L (µg[N] / g[soil])
# ! NSTRES    Nitrogen stress factor (1=no stress, 0=max stress)
# ! NVALP0    Set to 100,000 in PHENOLOG, used for comparison of times of
# !             plant stages (d)
# ! PORMIN    Minimum pore space required for supplying oxygen to roots for
# !             optimal growth and function (cm3/cm3)
# ! RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
# ! RNMODE    Simulation run mode (I=Interactive, A=All treatments,
# !             B=Batch mode, E=Sensitivity, D=Debug, N=Seasonal, Q=Sequence)
# ! RUN       Change in date between two observations for linear
# !             interpolation
# ! RWUEP1    Threshold for reducing leaf expansion compared w/ ratio of
# !             TRWU/EP1 (total potential daily root water uptake/ actual
# !             transpiration)
# ! RWUMX     Maximum water uptake per unit root length, constrained by soil
# !             water (cm3[water] / cm [root])
# ! SENESCE   Composite variable containing data about daily senesced plant
# !             matter. Structure of variable is defined in ModuleDefs.for
# ! SNOW      Snow accumulation (mm)
# ! SOILPROP  Composite variable containing soil properties including bulk
# !             density, drained upper limit, lower limit, pH, saturation
# !             water content.  Structure defined in ModuleDefs.
# ! SRAD      Solar radiation (MJ/m2-d)
# ! ST(L)     Soil temperature in soil layer L (°C)
# ! STGDOY(I) Day when plant stage I occurred (YYYYDDD)
# ! SW(L)     Volumetric soil water content in layer L
# !            (cm3 [water] / cm3 [soil])
# ! TMAX      Maximum daily temperature (°C)
# ! TMIN      Minimum daily temperature (°C)
# ! TRWUP     Potential daily root water uptake over soil profile (cm/d)
# ! TWILEN    Daylength from twilight to twilight (h)
# ! UNH4(L)   Rate of root uptake of NH4, computed in NUPTAK
# !            (kg [N] / ha - d)
# ! UNO3(L)   Rate of root uptake of NO3, computed in NUPTAK (kg [N] / ha -d)
# ! XHLAI     Healthy leaf area index (m2[leaf] / m2[ground])
# ! XLAI      Leaf area (one side) per unit of ground area
# !            (m2[leaf] / m2[ground])
# ! YREMRG    Day of emergence (YYYYDDD)
# ! YREND     Date for end of season (usually harvest date) (YYYYDDD)
# ! YRPLT     Planting date (YYYYDDD)
# !===========================================================================
#
# !===========================================================================

def READ_ASCE_KT(CONTROL, MEEVP):
    import numpy as np
    from ERROR import ERROR
    from READS import FIND, IGNORE
    from WARNING import WARNING
    from ModuleDefs import PUT_Char
# !     Generic routine to read evapotranspiration species parameters
# !     KEP, EORATIO
# !     TSKC, TKCBmax ASCE tall ref (50 cm alfalfa)
# !     SSKC, SKCBmax ASCE short ref (12 cm grass)
#
#       USE ModuleData
#       External IGNORE, WARNING, ERROR, GETLUN, FIND
#
#       CHARACTER*1  BLANK, MEEVP
    BLANK  = ' '
#       CHARACTER*2 CROP
#       CHARACTER*6 SECTION
#       CHARACTER*6  ERRKEY
    ERRKEY = 'IPASCE'
#
#       CHARACTER*12 FILEC
#       CHARACTER*30 FILEIO
    MSG = np.full(11, '', dtype='U78')
#       CHARACTER*80 PATHCR, CHAR
#       CHARACTER*92 FILECC
#
#       INTEGER LUNCRP, LUNIO, NMSG
#       INTEGER PATHL, FOUND, ERR, LINC, LNUM, ISECT
#
# !     Species-dependant variables exported to SPAM:
#       REAL KEP, EORATIO, SSKC, SKCBMAX, TSKC, TKCBMAX
#
# !     The variable "CONTROL" is of constructed type "ControlType" as
# !     defined in ModuleDefs.for, and contains the following variables.
# !     The components are copied into local variables for use here.
#       TYPE (ControlType) CONTROL
#
    if MEEVP not in ('S', 'T'):
        return
#
    NMSG = 0
#
# !-----------------------------------------------------------------------
# !     Read file plus path for species file
# !-----------------------------------------------------------------------
    FILEIO = CONTROL.FILEIO
    LUNIO  = CONTROL.LUNIO
    CROP   = CONTROL.CROP
    try:
        LUNIO = open(FILEIO, "r")
        ERR = 0
    except OSError as e:
        ERR = e.errno               
        ERROR(ERRKEY, ERR, FILEIO, 0)
    try:
        LUNIO = open(FILEIO, "r")
        ERR = 0
    except OSError as e:
        ERR = e.errno
        ERROR(ERRKEY, ERR, FILEIO, 0)
    for _ in range(6):
        LUNIO.readline()
    LNUM = 6
    line = LUNIO.readline()
    LNUM += 1
    try:
        FILEC  = line[15:27].strip()
        PATHCR = line[28:108].strip()
        ERR = 0
    except Exception:
        ERR = 1
        ERROR(ERRKEY, ERR, FILEIO, LNUM)
#
# !-----------------------------------------------------------------------
    if CROP != "FA":
        LNUM = 0
# !       open species file
        PATHL = PATHCR.find(" ") + 1
        if PATHL <= 1:
            FILECC = FILEC
        else:
            FILECC = PATHCR[:PATHL - 1] + FILEC
        GETLUN('FILEC', LUNCRP)
        try:
            LUNCRP = open(FILECC, "r")
            ERR    = 0
        except OSError as e:
            ERR = e.errno
        if ERR != 0:
            ERROR(ERRKEY, ERR, FILECC, 0)
        LNUM = 0
# !       ***** READ ASCE EVAPOTRANSPIRATION PARAMETERS *****
        SECTION = '!*EVAP'
        FOUND, LINC = FIND(LUNCRP, SECTION)
        LNUM += LINC
        if FOUND==0:
            NMSG += 1
            MSG[NMSG]=("*EVAP section missing from species file.")
#           GOTO 100
        else:
            ISECT,CHAR=IGNORE(LUNCRP,LNUM)
#           !KEP and EORATIO are not used in ASCE PET method,
#           !but must be read in prior to ASCE parameters.
            try:
                KEP     = float(CHAR[0:6])
                EORATIO = float(CHAR[6:12])
                ERR = 0
            except Exception:
                ERR = 1
            if ERR != 0:
                  NMSG = NMSG + 1
                  MSG[NMSG] = "Error reading KEP and EORATIO."
#
# !       Read short reference crop parameters
        ISECT,CHAR=IGNORE(LUNCRP,LNUM)
        if ISECT != 1: ERROR(ERRKEY, 1, FILECC, LNUM)
        try:
            SSKC    = float(CHAR[0:6])
            SKCBMAX = float(CHAR[6:12])
            ERR = 0
        except ValueError:
            ERR = 1
        #         IF (ERR .NE. 0) THEN
        if ERR != 0:
            NMSG = NMSG + 1
            MSG[NMSG] = "Error reading SSKC, SKCBMAX for ASCE PET method."
#         ENDIF
#
# !       Read tall reference crop parameters
        ISECT,CHAR=IGNORE(LUNCRP,LNUM)
        if ISECT != 1: ERROR(ERRKEY, 1, FILECC, LNUM)
        try:
            TSKC    = float(CHAR[0:6])
            TKCBMAX = float(CHAR[6:12])
            ERR = 0
        except ValueError:
            ERR = 1
        if ERR != 0:
            NMSG = NMSG + 1
            MSG[NMSG] = "Error reading TSKC, TKCBMAX for ASCE PET method."
#
        LUNCRP.close()
#
# !       Check for values with valid ranges.
        if MEEVP == 'S':
            if SSKC < 0.30 or SSKC > 1.0:
                NMSG = NMSG + 1
                MSG[NMSG] = "SSKC for ASCE PET method is out of range."
#           ENDIF
            if SKCBMAX < 0.25 or SKCBMAX > 1.5:
                NMSG = NMSG + 1
                MSG[NMSG] = "SKCBMAX for ASCE PET method is out of range."
#           ENDIF
#         ENDIF
#
        if MEEVP == 'T':
            if TSKC < 0.30 or TSKC > 1.0:
                NMSG = NMSG + 1
                MSG[NMSG] = "TSKC for ASCE PET method is out of range."
#           ENDIF
            if TKCBMAX < 0.25 or TKCBMAX > 1.5:
                NMSG = NMSG + 1
                MSG[NMSG] = "TKCBMAX for ASCE PET method is out of range."
#           ENDIF
#         ENDIF
#
    else:
# !       If fallow, use minimum values
        SSKC    = 0.30
        SKCBMAX = 0.25
        TSKC    = 0.30
        TKCBMAX = 0.25

    if NMSG > 0:
        MSG[NMSG + 1] = "ASCE PET method may not be valid for this crop."
        MSG[NMSG + 2] = "Model will stop."
        WARNING(NMSG + 2, ERRKEY, MSG)
        ERROR(ERRKEY, 1, FILECC, LNUM)
#       ENDIF
#
# !     Store the values for retrieval in SPAM (actually in PET.for).
    if MEEVP == 'S':
        PUT_Char('SPAM', 'SKC',    SSKC)
        PUT_Char('SPAM', 'KCBMAX', SKCBMAX)
    #       ELSEIF (MEEVP.EQ.'T') THEN
    elif MEEVP == 'T':
        PUT_Char('SPAM', 'SKC',    TSKC)
        PUT_Char('SPAM', 'KCBMAX', TKCBMAX)
#
    return MEEVP
#       END SUBROUTINE READ_ASCE_KT
# !===========================================================================


#
# Test Drive READ_ASCE_KT
#
#
# CONTROL=
# MEEVP='R'
# print(READ_ASCE_KT(CONTROL, MEEVP))