# !***********************************************************************
# !  POTENTIAL EVAPOTRANSPIRATION
# !     File PET.for
# !***********************************************************************
# !  Includes subroutines:
#
# !  MEEVP Routine Description
# !   S  PETASCE ASCE Standardized Reference Evapotranspiration Equation
# !                for the short reference crop (12-cm grass) with dual
# !                FAO-56 crop coefficient method (potential E and T
# !                calculated independently).
# !   T  PETASCE ASCE Standardized Reference Evapotranspiration Equation
# !                for the tall reference crop (50-cm alfalfa) with dual
# !                FAO-56 crop coefficient method (potential E and T
# !                calculated independently).
# !   R  PETPT   Calculates Priestley-Taylor potential evapotranspiration
# !                (default method with potential E and T partitioned as a
# !                function of LAI).
# !   F  PETPEN  FAO Penman-Monteith (FAO-56) reference evapotranspiration
# !                with EORATIO adjustment for CROPGRO models and KC = 1.0
# !                for non-CROPGRO models (potential E and T partioned as
# !                a function of LAI).
# !   D  PETDYN  Dynamic Penman-Monteith, pot. evapotranspiration, with
# !                dynamic input of LAI, crop height effects on Ra and Rs
# !   P  PETPNO  FAO Penman (FAO-24) potential evapotranspiration
# !   M  PETMEY  "Standard reference evaporation calculation for inland
# !                south eastern Australia" By Wayne Meyer 1993
# !   H  PETPTH Calculates Priestly-Taylor potential evapotranspiration
# !             using hourly temperature and radiation. Also includes a VPD
# !             effect to the transpiration
#
# !  Also includes these subroutines:
# !      PSE        Potential soil evaporation
# !      FLOOD_EVAP Evaporation from water surface
# C=======================================================================
#
#
# C=======================================================================
# !     SUBROUTINE PET
# !     Calls appropriate potential evapotranspiration routine
from WARNING import WARNING
from DATES import YR_DOY
from ERROR import ERROR
import numpy as np
def PET(CONTROL, ET_ALB, XHLAI, MEEVP, WEATHER, EORATIO, CANHT):
#       SUBROUTINE PET(CONTROL,
#      &      ET_ALB, XHLAI, MEEVP, WEATHER,  !Input for all
#      &      EORATIO, !Needed by Penman-Monteith
#      &      CANHT,   !Needed by dynamic Penman-Monteith
#      &      EO,      !Output
#      &      ET0)     !Output hourly Priestly-Taylor with VPD effect
#
#       USE ModuleDefs
#       IMPLICIT NONE
#       EXTERNAL YR_DOY, PETPT, PETPEN, PETASCE, PETDYN, PETPNO, PETMEY,
#      &  PETPTH, WARNING, ERROR
#       SAVE
#
#       TYPE (WeatherType) WEATHER
#       TYPE (ControlType) CONTROL
#       CHARACTER*1 MEEVP
#       INTEGER YRDOY, YEAR, DOY
#       REAL CANHT, CLOUDS, EO, EORATIO, ET_ALB, RHUM, SRAD, TAVG
#       REAL TDEW, TMAX, TMIN, VAPR, WINDHT, WINDSP, XHLAI
#       REAL WINDRUN, XLAT, XELEV
#       REAL, DIMENSION(TS)    ::RADHR, TAIRHR, ET0
#       LOGICAL NOTDEW, NOWIND
#       CHARACTER*78  MSG(2)
#       CHARACTER*12 FILEX
    MSG = np.full((2,), '', dtype='U78')
    ERRKEY = "PET   "
#
    CLOUDS  = WEATHER.CLOUDS
    SRAD    = WEATHER.SRAD
    NOTDEW  = WEATHER.NOTDEW
    NOWIND  = WEATHER.NOWIND
    RHUM    = WEATHER.RHUM
    TAVG    = WEATHER.TAVG
    TDEW    = WEATHER.TDEW
    TMAX    = WEATHER.TMAX
    TMIN    = WEATHER.TMIN
    VAPR    = WEATHER.VAPR
    WINDHT  = WEATHER.WINDHT
    WINDSP  = WEATHER.WINDSP
    WINDRUN = WEATHER.WINDRUN
    XLAT    = WEATHER.XLAT
    XELEV   = WEATHER.XELEV
    RADHR   = WEATHER.RADHR
    TAIRHR  = WEATHER.TAIRHR

    YRDOY = CONTROL.YRDOY
    FILEX = CONTROL.FILEX
    YEAR, DOY=YR_DOY(YRDOY)

    # !         ------------------------
    #           !Priestley-Taylor potential evapotranspiration
    if MEEVP == 'R':
        PETPT(ET_ALB, SRAD, TMAX, TMIN, XHLAI, EO)

    # !         ------------------------
    #           !FAO Penman-Monteith (FAO-56) potential evapotranspiration,
    # !             with KC = 1.0
    elif MEEVP == 'F':
        PETPEN(
            CLOUDS, EORATIO, ET_ALB, SRAD, TAVG, TDEW,
            TMAX, TMIN, VAPR, WINDSP, WINDHT, XHLAI, EO
        )

    # !         ------------------------
    #           !ASCE Standardized Reference Evapotranspiration Equation
    #           !for the short reference crop (12-cm grass, "S") or the
    #           !tall reference crop (50-cm grass, "T") with dual
    #           !FAO-56 crop coefficient method.
    elif MEEVP in ('S', 'T'):
        PETASCE(
            CANHT, DOY, ET_ALB, MEEVP, NOTDEW, NOWIND,
            RHUM, SRAD, TDEW, TMAX, TMIN, WINDHT,
            WINDRUN, VAPR, XHLAI, XLAT, XELEV, EO
        )

    # !         ------------------------
    #           !Dynamic Penman-Monteith, pot. evapotranspiration, with
    # !             dynamic input of LAI, crop height effects on Ra and Rs
    elif MEEVP == 'D':
        PETDYN(CANHT, CLOUDS, ET_ALB, SRAD, TAVG, TDEW,
               TMAX, TMIN, WINDSP, XHLAI, EO)

    # !         ------------------------
    #           !FAO Penman (FAO-24) potential evapotranspiration
    elif MEEVP == 'P':
        PETPNO(CLOUDS, ET_ALB, SRAD, TAVG, TDEW,
               TMAX, TMIN, WINDSP, XHLAI, EO)

    # !         ------------------------
    #           !Penman - Meyer routine for estimation of Et in Southern NSW
    elif MEEVP == 'M':
        PETMEY(CONTROL, TAVG, WINDSP, SRAD, TDEW, XHLAI,
               ET_ALB, EO)

    # !         ------------------------
    #           !Observed Potential ET from Weather file (Future)
    #           !CASE ('O')
    #           !    EO = EOMEAS

    # !         ------------------------
    #           !Priestley-Taylor potential evapotranspiration hourly
    #           !including a VPD effect on transpiration
    elif MEEVP == 'H':
        PETPTH(ET_ALB, TMAX, XHLAI, RADHR, TAIRHR, EO, ET0)

    else:
        MSG[1] = "Undefined EVAPO parameter in FileX."
        MSG[2] = "Unknown MEEVP in PET.for."
        WARNING(2, ERRKEY, MSG)
        ERROR(ERRKEY, 1, FILEX, 0)


    return EO, ET0
#       END SUBROUTINE PET
#
# C=======================================================================