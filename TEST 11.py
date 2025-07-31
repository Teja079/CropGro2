# !****************************************************************************************
# ! Hourly VPD Factor for Photosynthesis function
# !****************************************************************************************
#
from HMET import VPSAT
from ModuleDefs import TS
def get_Growth_VPDFPHR(PHSV, PHTV, TDEW, TMIN, TAIRHR, targetHour):
#     USE ModuleDefs
#
#         IMPLICIT NONE
#         EXTERNAL VPSAT
#         SAVE
# !       TYPE (WeatherType) WEATHER
#
#         INTEGER, intent (in) :: targetHour
#         REAL, intent (in) :: PHSV
#         REAL, intent (in) :: PHTV
#
#
#         REAL, DIMENSION(TS) ::   TAIRHR, VPDHR ! PARHR   , RADHR  , RHUMHR  , These are all declared in ModuleDefs
# !       REAL    INTEGVPDFPHR                                                                         ! Added for VPD response of transpiration
#         REAL    TDEW        , TMIN  !SNDN        , SNUP        , TMAX        ,
#         REAL    VPDFPPREV   , VPDMAXHRPREV
#         REAL    VPSAT                                                                            ! REAL function
# !       REAL    AlphaPT     , DeltaVP     , GammaPS     , LambdaLH                                 ! Added for formulation of Priestley-Taylor
# !       REAL    MSALB       , SRAD        , DTEMP       , ETPT        , EOP                        ! Added for formulation of Priestley-Taylor
#         INTEGER hour                                                                                  ! Loop counter
# !       REAL ALBEDO
#         REAL VPD_TRANSP  !VPDFP, INTEG_1, INTEG_2,
#
#         SAVE
#
# !     Transfer values from constructed data types into local variables.
#
#
#
#
#         ! Vapour pressure                                                            ! Code for VPD reponse moved from CS_Growth and modified for hourly response.
#
#         !Hourly loop for VPD stomatal response
#
    if TDEW <= -98.0: # validating if it is -99
        TDEW = TMIN
    VPD_TRANSP = 0.0
    for hour in range(1, TS+1):
        VPDHR[hour] = (VPSAT(TAIRHR[hour]) - VPSAT(TDEW)) / 1000.0  # VPDHR = VPD, hourly (kPa)
        VPD_TRANSP += VPDHR[hour]

    VPD_TRANSP = VPD_TRANSP / TS
#
#         ! If VPD > PHTV, reduce VPFPHR by the amount it exceeds PHVT (the threshold), scaled by PHSV (the slope).
    VPDFPHR = 1.0                                                               # VPDFPHR = VPD factor, hourly (#)
    VPDFPPREV = 1.0                                                          # VPDFPREV = VPD factor, hourly, previous hour (#)
    VPDStartHr = 0.0
    VPDMaxHr = 0.0
    for hour in range(1, TS+1):
        if PHTV > 0.0:                                             # If PHTV has a value (.NE. -99)
            if VPDHR[hour] > PHTV:
                VPDFPHR[hour] = min(1.0, max(0.0, 1.0 - PHSV * (VPDHR[hour] - PHTV)))
                if hour > 1 and VPDStartHr == 0.0:
                    VPDStartHr = float(hour)    # Hour start of stomatal closure in response to VPD
                VPDMaxHr = float(hour)                                           # Hour start of stomatal closure in response to VPD
#                 END IF
#             ENDIF                                                                    ! PHTV is VPD response threshold, PHSV is the slope (negative) both set in CSCAS046.SPE. PHTV >= 5 shuts off the response.
        if VPDFPHR[hour] > VPDFPPREV:                                      # If stomata close in response to VPD (VPDFPHR < 1.0), do not reopen (maintain VPDFPHR at its minimum value).
            VPDFPHR[hour] = VPDFPPREV
            VPDMaxHr = VPDMaxHrPrev                                              # Hour of maximum stomatal closure in response to VPD.
        else:
            VPDFPPREV = VPDFPHR[hour]                                               # Current hourly stomatal factor response to VPD
            VPDMaxHrPrev = float(hour)                                           # Current hour.
#             ENDIF
#         END DO
#
#
    get_Growth_VPDFPHR = VPDFPHR[targetHour]

#
#     END function get_Growth_VPDFPHR
#