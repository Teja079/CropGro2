#***********************************************************************
#     TRANSPIRATION MODULE - File TRANS.for
#***********************************************************************
#     Includes subroutines:
#         TRANS - Calculates actual transpiration rate.
#         TRATIO - Calculates relative transpiration rate.
#         BLRRES - Computes boundary layer resistance.
#***********************************************************************
#=======================================================================
#  TRANS, Subroutine, J.T. Ritchie
#  Calculates potential transpiration rate, EOP (mm/d).
#-----------------------------------------------------------------------
#  Called by: WATBAL
#  Calls:     None
#=======================================================================
from ModuleDefs import GET_float


def TRANS(DYNAMIC, MEEVP, CO2, CROP, EO, ET0, EVAP, KTRANS, WINDSP, XHLAI, WEATHER):
    import numpy as np
    import math
    from ModuleDefs import RunConstants as RC, TS, VPDFPHR, EOPH
    from ERROR import ERROR
    from WARNING import WARNING
#
# !-----------------------------------------------------------------------
#       USE ModuleDefs
#       USE ModuleData
#       USE YCA_Growth_VPD
#       IMPLICIT NONE
#       EXTERNAL WARNING, ERROR, TRATIO
#
#       TYPE (WeatherType) WEATHER
#
#       CHARACTER*2  CROP
#       CHARACTER*1  MEEVP
    ERRKEY = 'IPECO'
    MSG = np.full(2, '', dtype='U78')
#
#       INTEGER DYNAMIC
#       INTEGER hour
#
#       REAL CO2, EO, EVAP, FDINT, KTRANS, TAVG, WINDSP, XHLAI
#       REAL EOP, TRAT, EOP_reduc, EOP_max
#       REAL KCB, REFET
#       REAL PHSV, PHTV, TDEW, TMIN
    TAIRHR = np.zeros(TS, dtype=float)
    ET0    = np.zeros(TS, dtype=float)
#
# !     FUNCTION SUBROUTINES:
#       REAL TRATIO
#
    TAVG   = WEATHER.TAVG
    TDEW   = WEATHER.TDEW
    TMIN   = WEATHER.TMIN
    TAIRHR = WEATHER.TAIRHR

#
    KCB   = GET_float('SPAM', 'KCB')
    REFET = GET_float('SPAM', 'REFET')
#
#***********************************************************************
#***********************************************************************
#     Seasonal initialization - run once per season
#***********************************************************************
    if (DYNAMIC == RC.SEASINIT):
# !-----------------------------------------------------------------------
        EOP = 0.0
#
#***********************************************************************
#***********************************************************************
#     DAILY RATE CALCULATIONS
#***********************************************************************
    elif (DYNAMIC == RC.RATE):
#-----------------------------------------------------------------------
        EOP = 0.0
        TRAT = TRATIO(CROP, CO2, TAVG, WINDSP, XHLAI)

#-----------------------------------------------------------------------
#       Estimate light interception.  NOTE 01/15/03 We don't want PAR
#       Estimate ENERGY interception.  NOTE 01/15/03 We don't want PAR
#       interception.  Changed to reflect energy interception which
#       correctly considers both PAR and the infrared.  Villalobos
#       measured 0.52 for Transpiration component.  Theory of Goudriaan
#       supports combined interception coefficient of 0.5 for PAR and NIR
#
#       01/15/03 - Work of Sau et al, shows that a K of 0.5 was better in
#       all cases, for PT form as well as the Dynamic form for predicting
#       soil water balance and predicting measured ET.
#
        if KCB >= 0.0:
            EOP = KCB * REFET          # KRT added for ASCE dual Kc ET approach
        else:
            FDINT = 1.0 - math.exp(-KTRANS * XHLAI)
            if MEEVP != 'H':
                EOP = EO * FDINT
            else:
                phsv = GET_float('SPAM', 'PHSV')
                phtv = GET_float('SPAM', 'PHTV')
#
                if phsv <= 0.0:
                    MSG[1] = "VPD sensitivity parameter PHSV is not defined for EVAPO method (H)."
                    MSG[2] = "Program will stop."
                    WARNING(2, ERRKEY, MSG)
                    ERROR(ERRKEY, 4, "", 0)

                if phtv <= 0.0:
                    MSG[1] = "VPD threshold parameter PHTV is not defined for EVAPO method (H)."
                    MSG[2] = "Program will stop."
                    WARNING(2, ERRKEY, MSG)
                    ERROR(ERRKEY, 4, "", 0)

                for hour in range(1, TS + 1):  # keep 1-based indexing like Fortran
                    VPDFPHR[hour] = get_Growth_VPDFPHR(PHSV, PHTV, TDEW,
                                                       TMIN, TAIRHR, hour)
                    EOPH[hour] = (ET0[hour] * FDINT) * VPDFPHR[hour]
                    EOP += EOPH[hour]

            EOP_reduc = EOP * (1.0 - TRAT)
            EOP       = EOP * TRAT

#
#       01/15/03 KJB  I think the change to "Same" K for EOS and EOP
#       may cause next function to be less driving, but below still
#       will depend on whether actual soil evapo (EVAP) meets EOS
#
#       IF ((EOP + EVAP) .GT. (EO * TRAT)) EOP = EO * TRAT - EVAP
#
#       Need to limit EOP to no more than EO (reduced by TRAT effect on EOP)
#       minus actual evaporation from soil, mulch and flood
            EOP_max = EO - EOP_reduc - EVAP
            EOP     = min(EOP, EOP_max)
#         ENDIF
#
        EOP = max(EOP, 0.0)

#
#***********************************************************************
#***********************************************************************
#     END OF DYNAMIC IF CONSTRUCT
#***********************************************************************
#       ENDIF
#-----------------------------------------------------------------------
    return EOP
#       END SUBROUTINE TRANS
#
#-----------------------------------------------------------------------
#     TRANS VARIABLE DEFINITIONS:
#-----------------------------------------------------------------------
# CO2     Atmospheric carbon dioxide concentration (ppm)
# EO      Potential evapotranspiration rate (mm/d)
# EOP     Potential plant transpiration  (mm/d)
# EVAP    Actual soil + mulch + flood evaporation rate (mm/d)
# FDINT   Fraction of direct solar radiation captured by canopy
# KCAN    Canopy light extinction coefficient for daily PAR, for
#           equidistant plant spacing, modified when in-row and between row
#           spacings are not equal
# KCB     Basal crop coefficient for ASCE dual Kc ET method
# LNUM    Current line number of input file
# REFET   ASCE Standardized Reference Evapotranspiration (alfalfa or grass)
# TAVG    Average daily temperature (�C)
# TRAT    Relative transpiration rate for CO2 values other than 330 ppm
# TRATIO  Function subroutine which calculates relative transpiration rate.
#
# WINDSP  Wind speed (km/d)
# XHLAI   Leaf area index (m2[leaf] / m2[ground])
#-----------------------------------------------------------------------
#     END SUBROUTINE TRANS
#-----------------------------------------------------------------------
#
#=======================================================================
#  TRATIO, Real Function, J.W. Jones
#  Calculates relative transpiration rate (CO2=330 vpm gives 1.0).
#-----------------------------------------------------------------------
#  Called by: TRANS
#  Calls:     BLRRES
#=======================================================================
def TRATIO(CROP, CO2, TAVG, WINDSP, XHLAI):
    from HMET import VPSLOP

#       REAL CO2, TAVG, WINDSP, XHLAI
#       REAL TRATIO
#       REAL CHIGHT, DELTA, GAMMA, LAIMAX, LHV, RA, RB, !RATIO, RC1, RC2,
#      &  RL, RLC, RLF, RLFC, UAVG, XDEN, XNUM    !, RS1, RS2
#
    if XHLAI < 0.01:
        return 1.0

#   Initialize.
    UAVG   = WINDSP / 86.4
    RB     = 10.0
    LAIMAX = 3.5

#
#   10/16/2006 CHP/MJ added sugarcane to 1.5m crops
#   Need to look at how to add new crops -- put this in species file?
#   Other new crop modules: TR, TN, ....???
#
    if CROP in "MZMLSGSCSU":
        CHIGHT = 1.5
    elif CROP in "WHBA":
        CHIGHT = 1.0
    else:
        CHIGHT = 0.5

#-----------------------------------------------------------------------
#     RLF = Leaf stomatal resistance at 330.0 ppm CO2, s/m
#     RLFC = Leaf stomatal resistance at other CO2 conc., s/m
#     (Allen, 1986), Plant responses to rising CO2.
#     CO2    = CO2    Conc of the increased ATM case
#     CO2    = 330    Ambient CO2
#-----------------------------------------------------------------------
    if CROP in {"MZ", "ML", "SG", "SC", "SW", "BM", "BH", "BR", "NP", "SI"}:
# C     C-4 Crops
# C       EQ 7 from Allen (1986) for corn.
        RLF  = (1.0 / (0.0328 - 5.49e-5 * 330.0 + 2.96e-8 * 330.0**2)) + RB
        RLFC = (1.0 / (0.0328 - 5.49e-5 * CO2   + 2.96e-8 * CO2**2)) + RB
    else:
        # C-3 crops
        RLF  = 9.72 + 0.0757 * 330.0 + 10.0
        RLFC = 9.72 + 0.0757 * CO2   + 10.0
#
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#     CHP / JWJ 8/23/2011
#
#C     Compute canopy resistances, RL and RLC.
#      RL = RLF / XHLAI
#      RLC = RLFC / XHLAI
#
#C     Compute boundary layer resistance (Jagtap and Jones, 1990).
#      CALL BLRRES(
#     &    CHIGHT, UAVG,                   !Input
#     &    RS1, RS2, RC1, RC2)             !Output
#
#      RATIO = XHLAI/LAIMAX
#      IF (RATIO .GT. 1.0) RATIO = 1.0
#      RA = RC1 + (RC2 - RC1) * RATIO
#
#     Use FAO-56 assumption LAI = 2.88 reference for canopy resistances
    RL  = RLF  / (0.5 * 2.88)
    RLC = RLFC / (0.5 * 2.88)
#
#   Replace boundary layer resistance function with this eqn
    RA = 208.0 / UAVG  # FAO-56

#
#     End changes CHP / JWJ 8/23/2011
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
#   Compute delta and gamma.
#
    DELTA = VPSLOP(TAVG) / 100.0
    LHV   = 2500.9 - 2.345 * TAVG
    GAMMA = 1013.0 * 1.005 / (LHV * 0.622)

#
    XNUM   = DELTA + GAMMA * (1.0 + RL  / RA)
    XDEN   = DELTA + GAMMA * (1.0 + RLC / RA)
    # TRATIO = XNUM / XDEN
    value = XNUM / XDEN
    return value
#-----------------------------------------------------------------------
#     TRATIO VARIABLE DEFINITIONS:
#-----------------------------------------------------------------------
# CHIGHT   Reference height for crop (m)
# CO2      Atmospheric carbon dioxide concentration (ppm)
# CROP     Crop identification code
# DELTA
# GAMMA
# LAIMAX   Maximum value of leaf area index above which no further changes
#            to canopy resistance occur.
# LHV
# RA
# RATIO    Ratio of leaf area index to maximum leaf area index
# RB
# RC1, RC2
# RL       Canopy resistance for CO2 = 330 ppm (s/m)
# RLC      Canopy resistance for actual CO2 concentration (s/m)
# RLF      Leaf stomatal resistance at 330.0 ppm CO2 (s/m)
# RLFC     Leaf stomatal resistance at other CO2 concentration (s/m)
# RS1, RS2
# TAVG     Average daily temperature (�C)
# TRATIO   Function subroutine which calculates relative transpiration
#            rate.
# UAVG     Average wind speed (m/s)
# VPSLOP   Calculates slope of saturated vapor pressure versus temperature
#            curve (Pa/K)
# WINDSP   Wind speed (km/d)
# XDEN
# XHLAI    Leaf area index (m2[leaf] / m2[ground])
# XNUM
#-----------------------------------------------------------------------
#     END FUNCTION TRATIO
#-----------------------------------------------------------------------
#
# !C=======================================================================
# !C  BLRRES, Subroutine, J.W. Jones
# !C  Computes boundary layer resistance (Jagtap and Jones, 1990).
# !C-----------------------------------------------------------------------
# !C  REVISION HISTORY
# !C  ??/??/?? JWJ Written
# !!  10/17/97 CHP Modified for modular format.
# !!-----------------------------------------------------------------------
# !!  Called by: TRATIO
# !!  Calls:     None
# !C=======================================================================
# !      SUBROUTINE BLRRES(
# !     &    CHIGHT, UAVG,                   !Input
# !     &    RS1, RS2, RC1, RC2)             !Output
# !
# !      IMPLICIT NONE
# !!-----------------------------------------------------------------------
# !      REAL CHIGHT, UAVG
# !      REAL RC1, RC2, RS1, RS2
# !      REAL D, E1, E2, E3, E4, E5, E6, EN, X, Z0, Z1
# !
# !!-----------------------------------------------------------------------
# !      EN = 3.0
# !      Z1 = 0.01
# !      X = 2.0
# !      D = 0.7 * CHIGHT**0.979
# !      Z0 = 0.13 * CHIGHT**0.997
# !
# !      E1 = EXP(EN*(1. - (D + Z0) / CHIGHT))
# !      E2 = EXP(EN)
# !      E3 = CHIGHT/(EN*(CHIGHT-D))
# !      E4 = ALOG((X - D)/Z0)
# !      E5 = 0.4 * 0.4 * UAVG
# !      E6 = ALOG((X - D) / (CHIGHT - D))
# !
# !      RS2 = E4 * E3 * (E2 - E1) / E5
# !      RC2 = E4 * (E6 + E3 * (E1 - 1.)) / E5
# !      RS1 = ALOG(X/Z1) * ALOG((D + Z0)/Z1) / E5
# !      RC1 = (ALOG(X/Z1)**2) / E5
# !      RC1 = RC1 - RS1
# !
# !      RETURN
# !      END SUBROUTINE BLRRES
# !
# !!-----------------------------------------------------------------------
# !!     BLRRES VARIABLE DEFINITIONS:
# !!-----------------------------------------------------------------------
# !! CHIGHT   Reference height for crop (m)
# !! D
# !! E1, E2, E3, E4, E5, E6
# !! EN
# !! RC1, RC2
# !! RS1, RS2
# !! UAVG     Average wind speed (m/s)
# !! X
# !! Z0
# !! Z1
# !!-----------------------------------------------------------------------
# !!     END SUBROUTINE BLRRES
# !!-----------------------------------------------------------------------
# !***********************************************************************
# !     END OF TRANSPIRATION MODULE
# !***********************************************************************
