#=======================================================================
#  SOIL, Subroutine
#-----------------------------------------------------------------------
#  Soil Processes subroutine.  Calls the following modules:
#     SOILDYN     - integrates soil properties variables
#     WATBAL      - soil water balance
#     SoilN_inorg - inorganic soil N (from NTRANS)
#     SoilPi      - inorganic soil P
#     SoilKi      - inorganic soil K
#     SoilOrg     - Ceres soil organic matter (from NTRANS)
#     CENTURY     - Century soil organic matter
#=====================================================================
#
def SOIL(CONTROL, ISWITCH,ES, FERTDATA, FracRts, HARVRES, IRRAMT, KTRANS,
         KUptake, OMAData, PUptake, RLV,SENESCE, ST, SWDELTX,TILLVALS, UNH4,
         UNO3,WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH, UPFLOW) :
    from SOILDYN import SOILDYN
    from WATBAL import WATBAL
    from ModuleDefs import NL, RunConstants as RC
    import numpy as np
#
# !-----------------------------------------------------------------------
#       USE ModuleDefs
#       USE FloodModule
#       USE GHG_mod
#       IMPLICIT NONE
#       EXTERNAL SOILDYN, WATBAL, CENTURY, SoilOrg, SoilNi, SoilPi, SoilKi
#       SAVE
# !-----------------------------------------------------------------------
# !     Interface variables:
# !-----------------------------------------------------------------------
# !     Input:
#       TYPE (ControlType) , INTENT(IN) :: CONTROL
#       TYPE (SwitchType)  , INTENT(IN) :: ISWITCH
#       REAL               , INTENT(IN) :: ES
#       TYPE (FertType)    , INTENT(IN) :: FERTDATA
#       REAL, DIMENSION(NL), INTENT(IN) :: FracRts
#       Type (ResidueType) , INTENT(IN) :: HARVRES
#       REAL               , INTENT(IN) :: IRRAMT
#       REAL               , INTENT(IN) :: KTRANS
#       TYPE (OrgMatAppType),INTENT(IN) :: OMAData
#       REAL, DIMENSION(NL), INTENT(IN) :: PUptake, KUptake
#       REAL, DIMENSION(NL), INTENT(IN) :: RLV
#       Type (ResidueType) , INTENT(IN) :: SENESCE
# !     REAL               , INTENT(IN) :: SRFTEMP
#       REAL, DIMENSION(NL), INTENT(IN) :: ST
#       REAL, DIMENSION(NL), INTENT(IN) :: SWDELTX
#       TYPE (TillType)    , INTENT(IN) :: TILLVALS
#       REAL, DIMENSION(NL), INTENT(IN) :: UNH4, UNO3
#       TYPE (WeatherType) , INTENT(IN) :: WEATHER
#       REAL               , INTENT(IN) :: XHLAI
#
    SomLit = np.zeros(NL)
#
# !     Input/Output:
#       REAL, DIMENSION(NL), INTENT(INOUT) :: UPFLOW
#       TYPE (FloodNType),   INTENT(INOUT) :: FLOODN
#       TYPE (FloodWatType), INTENT(INOUT) :: FLOODWAT
#       TYPE (MulchType),    INTENT(INOUT) :: MULCH
#
# !     Output:
    NH4_plant = np.zeros(NL)
    NO3_plant = np.zeros(NL)
    SKi_AVAIL = np.zeros(NL)
    SPi_AVAIL = np.zeros(NL)
#       REAL               , INTENT(OUT) :: SNOW
#       TYPE (SoilType)    , INTENT(OUT) :: SOILPROP
    SomLitC = np.zeros(NL)
    SomLitE = np.zeros((NL,3))
    SW = np.zeros(NL)
#       REAL, DIMENSION(NL), INTENT(OUT) :: SWDELTS
    SWDELTU = np.zeros(NL)
#       REAL               , INTENT(OUT) :: WINF
    UPPM = np.zeros(NL)
    YREND = -99
#
# !-----------------------------------------------------------------------
# !     Local variables:
#       INTEGER DYNAMIC
#       CHARACTER*1  MESOM
#
#       REAL, DIMENSION(0:NL) :: newCO2 !DayCent
#       REAL, DIMENSION(NL) :: DRN
#       REAL, DIMENSION(NL) :: SPi_Labile, NO3, NH4
#       REAL, DIMENSION(0:NL) :: LITC, SSOMC
#       REAL, DIMENSION(0:NL,NELEM) :: IMM, MNR
#
# !     Added for tile drainage:
#       REAL TDFC
#       INTEGER TDLNO
#
# !     Added for methane
#       REAL DRAIN
#       TYPE (CH4_type) CH4_data
#
# !-----------------------------------------------------------------------
# !     Transfer values from constructed data types into local variables.
    DYNAMIC = CONTROL.DYNAMIC
    MESOM   = ISWITCH.MESOM
#
#***********************************************************************
#     Call Soil Dynamics module
#      IF (DYNAMIC < OUTPUT) THEN
    SOILPROP = SOILDYN(CONTROL, ISWITCH,KTRANS, MULCH, SomLit, SomLitC, SW, TILLVALS,
                       WEATHER, XHLAI)
#
#      Call WATBAL first for all except seasonal initialization
    if DYNAMIC != RC.SEASINIT:
        (DRAIN, DRN, SNOW, SW, SWDELTS,TDFC, TDLNO, UPFLOW, WINF,FLOODWAT, MULCH, SWDELTU) = (
            WATBAL(CONTROL, ISWITCH,ES, IRRAMT, SOILPROP, SWDELTX,TILLVALS,
               WEATHER,FLOODWAT, MULCH, SWDELTU))

#
# !     Soil organic matter modules
#       IF (MESOM .EQ. 'P') THEN
# !       Parton (Century-based) soil organic matter module
#         CALL CENTURY(CONTROL, ISWITCH,
#      &  DRAIN, FERTDATA, FLOODWAT, FLOODN, HARVRES,   !Input
#      &  NH4, NO3, OMADATA, RLV, SENESCE,              !Input
#      &  SOILPROP, SPi_Labile, ST, SW, TILLVALS,       !Input
#      &  CH4_data, IMM, LITC, MNR, MULCH, newCO2,      !Output
#      &  SomLit, SomLitC, SomLitE, SSOMC)              !Output
#       ELSE
# !      ELSEIF (MESOM .EQ. 'G') THEN
# !       Godwin (Ceres-based) soil organic matter module (formerly NTRANS)
#         CALL SoilOrg (CONTROL, ISWITCH,
#      &    DRAIN, FERTDATA, FLOODWAT, FLOODN, HARVRES,     !Input
#      &    NH4, NO3, OMAData, RLV,                         !Input
#      &    SENESCE, SOILPROP, SPi_Labile, ST, SW, TILLVALS,!Input
#      &    CH4_data, IMM, LITC, MNR, MULCH, newCO2,        !Output
#      &    SomLit, SomLitC, SomLitE, SSOMC)                !Output
#       ENDIF
#
# !     Inorganic N (formerly NTRANS)
#       CALL SoilNi (CONTROL, ISWITCH,
#      &    CH4_data, DRN, ES, FERTDATA, FLOODWAT, IMM,     !Input
#      &    LITC, MNR, newCO2, SNOW, SOILPROP, SSOMC, ST,   !Input
#      &    SW, TDFC, TDLNO, TILLVALS, UNH4, UNO3, UPFLOW,  !Input
#      &    WEATHER, XHLAI,                                 !Input
#      &    FLOODN,                                         !I/O
#      &    NH4, NO3, NH4_plant, NO3_plant, UPPM)           !Output
#
# !     Inorganic P
#       CALL SoilPi(CONTROL, ISWITCH, FLOODWAT,
#      &    FERTDATA, IMM, MNR, PUptake, SOILPROP,          !Input
#      &    FracRts, SW, TillVals,                          !Input
#      &    SPi_AVAIL, SPi_Labile, YREND)                   !Output
#
# !     Inorganic K
#       CALL SoilKi(CONTROL, ISWITCH,
#      &    FERTDATA, KUptake, SOILPROP, TILLVALS,          !Input
#      &    SKi_Avail)                                      !Output
#
    if DYNAMIC == RC.SEASINIT:
#        Soil water balance -- call last for initialization
        (DRAIN, DRN, SNOW, SW, SWDELTS, TDFC, TDLNO, UPFLOW, WINF) = (
            WATBAL(CONTROL, ISWITCH, ES, IRRAMT, SOILPROP, SWDELTX, TILLVALS,
            WEATHER, FLOODWAT, MULCH, SWDELTU))

# !***********************************************************************
#
# !-----------------------------------------------------------------------
#
#       RETURN
#       END SUBROUTINE SOIL
    return (NH4_plant, NO3_plant, SKi_AVAIL, SNOW,SPi_AVAIL, SOILPROP, SomLitC,
            SomLitE,SW, SWDELTS, SWDELTU, UPPM, WINF, YREND)
# !=======================================================================
