#=======================================================================
#  WATBAL, Subroutine
#  Calculates water balance components.
#-----------------------------------------------------------------------
#  Called by: SOIL module
#  Calls:     SNOWFALL, IPWBAL, WBSUM, WTDEPT,        (File WBSUBS.for)
#             UP_FLOW                                  (File WBSUBS.for)
#             INFIL   (File INFIL.for)
#             OPWBAL  (File OPWBAL.for)
#             RNOFF   (File RNOFF.for)
#             SATFLO  (File SATFLO.for)
#=======================================================================
def WATBAL(CONTROL, ISWITCH, ES, IRRAMT, SOILPROP, SWDELTX, TILLVALS, WEATHER, FLOODWAT, MULCH, SWDELTU): #Inputs/Outputs

    from ModuleDefs import NL, RunConstants as RC, SAVE_data
    from WSUBS import IPWBAL, WTDEPT
    from TILEDRAIN import TILEDRAIN
    from WaterTable import WaterTable


    DLAYR, DLAYR_YEST, DS, DUL, LL = ([-99.0]*NL for _ in range(5))
    SAT, SWCN, SW_AVAIL = ([-99.0] * NL for _ in range(3))
    TDFC : float = 0
    TDLNO : int = 0
    #Tile drain variables:
    SWDELTT = [-99.0]*NL

    #Tillage variables
    SW_MIX, SW_UNMIX, SWDELTL, SW_mm_NEW =  ([-99.0] * NL for _ in range(4))
    SW_mm, SWDELTS_mm, SWDELTU_mm = ([-99.0] * NL for _ in range(3))
    SWDELTX_mm, SWDELTT_mm, SWDELTL_mm = ([-99.0] * NL for _ in range(3))

    #Water table variables:
    SWDELTW, SWDELTW_mm = ([-99.0] * NL for _ in range(2))
    DYNAMIC = CONTROL.DYNAMIC
    YRDOY   = CONTROL.YRDOY

    #Output Lists
    DRN = [-99.0] * NL
    SW = [-99.0] * NL
    SWDELTS = [-99.0] * NL
    UPFLOW = [-99.0] * NL

    CN     = SOILPROP.CN
    DLAYR  = SOILPROP.DLAYR
    DS     = SOILPROP.DS
    DUL    = SOILPROP.DUL
    ETDR   = SOILPROP.ETDR
    LL     = SOILPROP.LL
    NLAYR  = SOILPROP.NLAYR
    SAT    = SOILPROP.SAT
    SWCN   = SOILPROP.SWCN
    SWCON  = SOILPROP.SWCON

    # FLOOD   = FLOODWAT . FLOOD
    # PUDDLED = FLOODWAT . PUDDLED
    # PUDPERC = FLOODWAT . PUDPERC
    # BUNDED  = FLOODWAT . BUNDED

    RAIN = WEATHER . RAIN
    TMAX = WEATHER . TMAX

# ***********************************************************************
# ***********************************************************************
#      Run Initialization - Called once per simulation
# ***********************************************************************
    if DYNAMIC == RC.RUNINIT:
       SW, MgmtWTD = IPWBAL(CONTROL, LL, NLAYR) #Read in values from input file
       DRN, SWDELTT, TDFC, TDFD, TDLNO = (
           TILEDRAIN(CONTROL, DLAYR, DUL, ETDR, NLAYR, SAT, SW, SWDELTS)) #Read in Tiledrain vars

# !     Save infiltration and runoff information for floodwater calcs
#       FLOODWAT % INFILT = 0.0
#       FLOODWAT % RUNOFF = 0.0
#
#       SNOW = 0.0
#       CALL PUT('WATER','SNOW'  , SNOW)
# ***********************************************************************
# ***********************************************************************
#      Seasonal initialization - run once per season
# ***********************************************************************
    elif DYNAMIC == RC.SEASINIT:
        ISWWAT = ISWITCH.ISWWAT
        MEINF = ISWITCH . MEINF
        MESEV = ISWITCH . MESEV
        MEEVP = ISWITCH . MEEVP

#      IF (ISWWAT .EQ. 'Y') THEN
#        IF (CONTROL%MULTI .GT. 1 .OR. CONTROL%RNMODE .EQ. 'Y') THEN
#        !Re-read initial conditions if multi-season or forecast run
#          CALL IPWBAL (CONTROL, LL, NLAYR,                !Input
#     &    SW, MgmtWTD)                                    !Output
#        ENDIF
#      ENDIF
        #     Initialize water table
        ActWTD, LatInflow, LatOutflow, MgmtWTD, SWDELTW = WaterTable(RC.SEASINIT, SOILPROP, SW)

#     Use inital water table depth and capillary rise to set initial
        for L in range(NLAYR):
            SW[L] = SW[L] + SWDELTW[L]
        WTDEP = 9999.
#
        if ActWTD > DS[NLAYR]:
            WTDEP = WTDEPT(NLAYR, DLAYR, DS, DUL, SAT, SW, WTDEP)

# !     Initialize summary variables
#       CALL WBSUM(SEASINIT,
#      &    NLAYR, DRAIN, RAIN, RUNOFF, DLAYR, SW,          !Input
#      &    CRAIN, TDRAIN, TRUNOF, TSW, TSWINI)             !Output
#
# !     Initialize snow accumulation
#       CALL SNOWFALL (SEASINIT,
#      &    TMAX, RAIN,                                     !Input
#      &    SNOW, WATAVL)                                   !Output
#
# !     Initialize mulch water
#       CALL MULCHWATER(CONTROL, ISWITCH,
#      &    WATAVL, MULCH)
#
# !     Initialize tile drainage
#       IF (TDLNO .GT. 0) THEN
#         CALL TILEDRAIN(CONTROL,
#      &    DLAYR, DUL, ETDR, NLAYR, SAT, SW, SWDELTS,      !Input
#      &    DRN, SWDELTT, TDFC, TDFD, TDLNO)                !Output
#       ENDIF
#
#       IF (ISWWAT == 'Y') THEN
# !       Water balance output initialization
#         CALL Wbal(CONTROL, ISWITCH,
#      &    CRAIN, DLAYR, DRAIN, FLOODWAT, LatInflow, LatOutflow,
#      &    IRRAMT, MULCH, NLAYR, RAIN, RUNOFF, SNOW,
#      &    SWDELTS, SWDELTT, SWDELTU, SWDELTX, SWDELTL,
#      &    TDFC, TDFD, TDRAIN, TRUNOF, TSW, TSWINI)
#
# !       Call OPWBAL to write headers to output file
#         CALL OPWBAL(CONTROL, ISWITCH,
#      &    CRAIN, DLAYR, FLOODWAT, IRRAMT, LL, MULCH,      !Input
#      &    NLAYR, RUNOFF, SOILPROP, SW, TDFC, TDFD,        !Input
#      &    TDRAIN, TRUNOF, ActWTD, LatInflow, LatOutflow,  !Input
#      &    EXCS, WTDEP)                                    !Input
#
#         CALL OPSWBL(CONTROL, ISWITCH,
#      &    SOILPROP, SW)                                   !Input
#
#       ENDIF
        DRAIN  = 0.0
        DRN    = [0.0] * NL
        UPFLOW = 0.0
        WINF   = 0.0
# !     Set process rates to zero.
        SWDELTS = [0.0] * NL
        SWDELTU = [0.0] * NL
        SWDELTT = [0.0] * NL
        SWDELTL = [0.0] * NL
        SWDELTW = [0.0] * NL

        DLAYR_YEST = DLAYR
#
# !***********************************************************************
# !***********************************************************************
# !     DAILY RATE CALCULATIONS
# !***********************************************************************
#   elif DYNAMIC == RC.RATE:
# !-----------------------------------------------------------------------
#       !Convert snowfall into precip ( = rain)
#       !This is needed for winter crops even if water not simulated.
# C-GH   IF (TMAX .LE. 1.0 .OR. SNOW .GT. 0.001) THEN
# C     Conflict with CERES-Wheat
#       IF (TMAX .LE. 1.0 .OR. SNOW .GT. 0.0) THEN
#         CALL SNOWFALL (RATE,
#      &    TMAX, RAIN,                                 !Input
#      &    SNOW, WATAVL)                               !Output
#       ELSE
#         WATAVL = RAIN
#       ENDIF
#
# !     Rates not calculated unless water switch is on.
#       IF (ISWWAT .NE. 'Y') RETURN
#
# !     Maintain water table depth and calculate capillary rise
#       IF (FLOOD < 1.E-6) THEN
#         Call WaterTable(RATE,
#      &    SOILPROP, SW,                                   !Input
#      &    ActWTD, LatInflow, LatOutflow,                  !Output
#      &    MgmtWTD, SWDELTW)                               !Output
#       ENDIF
#
# !     Set process rates to zero.
#       SWDELTS = 0.0
#       !SWDELTX = 0.0
#       SWDELTU = 0.0
#       SWDELTT = 0.0
#       SWDELTL = 0.0
#
#       PINF   = 0.0
#       WINF   = 0.0
#       INFILT = 0.0
#       EXCS   = 0.0
#       DRAIN  = 0.0
# !--------------------------------------------------------------------
#       !Puddled conditions added for RICE routines. - Puddled implies
#       !  bunded conditions exist and soil has been modified to minimize
#       !  percolation.  Perc rates read from irrigation data.
#       IF (PUDDLED) THEN
#       !Puddled field - fill soil profile and limit drainage to PUDPERC
#
#         !Water available for infiltration
#         WINF = MAX(0.0, FLOOD + IRRAMT + RAIN)
#
#         !First fill soil profile
#         INFILT = 0.0
#         DO L = 1, NLAYR
#           SWDELTS(L) = SAT(L) - SW(L)
#           INFILT = INFILT + SWDELTS(L) * DLAYR(L) * 10.  !(in mm)
#           IF (INFILT .GT. WINF) THEN
#             SWDELTS(L) = SWDELTS(L) - (INFILT - WINF) / DLAYR(L) / 10.0
#             INFILT = WINF
#             EXIT
#           ENDIF
#         ENDDO
#
#         !Drainage, if any, limited to PUDPERC
#         DRAIN = MIN(PUDPERC, WINF - INFILT)
#         INFILT = INFILT + DRAIN
#         DO L = 1, NLAYR
#           DRN(L) = DRAIN   !used for soil N fluxes
#         ENDDO
#
# !--------------------------------------------------------------------
#       ELSE
#       !Non-puddled, flooded or non-flooded field
#         PINF = 0.0
#         WINF = 0.0
#         INFILT = 0.0
#
# !       -------------------------------------------------------------
#         IF (BUNDED) THEN  !potential for flooding exists
#           !No runoff from field with bund (RICE)
#           RUNOFF = 0.0
#           WINF = MAX(0.0, FLOOD + IRRAMT + RAIN)  !(mm)
#
# !       -------------------------------------------------------------
#         ELSE
# !         Upland field
# !         Not bunded, flooded conditions not possible
#
# !         Water first absorbed by mulch, if present
# !         IF (INDEX('RSN',MEINF) .LE. 0) THEN
#           IF (INDEX('RSM',MEINF) > 0) THEN
#             CALL MULCHWATER(CONTROL, ISWITCH,
#      &            WATAVL, MULCH)
#           ENDIF
#
#           CALL RNOFF(
#      &        CN, LL, MEINF, MULCH, SAT, SW, WATAVL,      !Input
#      &        RUNOFF)                                     !Output
#
#           WINF = WATAVL - RUNOFF + IRRAMT     !(mm)
#         ENDIF
#
# !       Potential for infilitration
#         PINF = WINF * 0.1                       !(cm)
#
# !       Call INFIL to calculate infiltration rates on days with irrigation
# !         or rainfall.  Call SATFLO on days with no irrigation or rain
# !         to calculate saturated flow.
#         IF (PINF .GT. 0.0001) THEN
#           CALL INFIL(
#      &      DLAYR, DS, DUL, NLAYR, PINF, SAT, SW,         !Input
#      &      SWCN, SWCON, MgmtWTD,                         !Input
#      &      DRAIN, DRN, EXCS, SWDELTS)                    !Output
#
#           INFILT = 0.0
#           DO L = 1, NLAYR
#             INFILT = INFILT + SWDELTS(L) * DLAYR(L) * 10.  !(in mm)
#           ENDDO
#           INFILT = INFILT + DRAIN
#
#           !Excess water not infiltrated is added to overland runoff.
#           !If bunded, excess water is accounted for in INFILT variable.
#           IF (EXCS > 0 .AND. .NOT. BUNDED) THEN
#             RUNOFF = RUNOFF + EXCS * 10.0
#           ENDIF
#
#         ELSE
#           CALL SATFLO(
#      &      DLAYR, DUL, NLAYR, SAT, SW, SWCN, SWCON,      !Input
#      &      DRAIN, DRN, SWDELTS)                          !Output
#         ENDIF
#
#         IF (TDLNO .GT. 0) THEN
#           CALL TILEDRAIN(CONTROL,
#      &      DLAYR, DUL, ETDR, NLAYR, SAT, SW, SWDELTS,    !Input
#      &      DRN, SWDELTT, TDFC, TDFD, TDLNO)              !Output
#         ENDIF
#
#       ENDIF   !End of IF block for PUDDLED conditions
#
# !-----------------------------------------------------------------------
#       IF (FLOOD .LE. 0.0 .AND. MESEV .NE. 'S') THEN
# !       Calculate the availability of soil water for use in UPFLOW.
#         DO L = 1, NLAYR
#           SW_AVAIL(L) = MAX(0.0, SW(L) + SWDELTS(L))
#         ENDDO
#
# C       Calculate upward movement of water due to evaporation and root
# C       extraction (based on yesterday's values) for each soil layer.
# !       Don't call when using SALUS soil evaporation routine (MESEV = 'S')
#         CALL UP_FLOW(
#      &    NLAYR, DLAYR, DUL, LL, SAT, SW, SW_AVAIL,       !Input
#      &    UPFLOW, SWDELTU)                                !Output
#       ENDIF
#
# !-----------------------------------------------------------------------
# !     Check if tillage occurred today -- if so, mix soil water in
# !       tillage depth
#       IF (TILLVALS % NTIL .GT. 0) THEN
#         TILDATE = TILLVALS % TILDATE
#         YRDOY = CONTROL % YRDOY
#         IF (YRDOY .EQ. TILDATE) THEN
#           !Call mixing routine for soil properties
#           MIXPCT = TILLVALS % TILMIX
#           TDEP = TILLVALS % TILDEP
#
#           DO L = 1, NLAYR
#             !Depths of soil water (cm)
#             SW_UNMIX(L) = SW(L) * DLAYR(L)
#           ENDDO
#
#           CALL SoilMixing(DLAYR, MIXPCT, NLAYR, TDEP, SW_UNMIX, SW_MIX)
#
#           DO L = 1, NLAYR
#             !Change in SW due to tillage
#             SWDELTL(L) = SW_MIX(L) / DLAYR(L) - SW(L)
#           ENDDO
#          CALL SUMSW(NLAYR, DLAYR, SWDELTL, SWTOT)
#         ENDIF
#       ENDIF
# !-----------------------------------------------------------------------
# !     Save infiltration and runoff information for floodwater calcs
#       FLOODWAT % INFILT = INFILT
#       FLOODWAT % RUNOFF = RUNOFF
#
#       CALL PUT('WATER','RUNOFF', RUNOFF)
#       CALL PUT('WATER','DRAIN' , DRAIN)
#       CALL PUT('WATER','SNOW'  , SNOW)
#
# !***********************************************************************
# !***********************************************************************
# !     DAILY INTEGRATION
# !***********************************************************************
#       ELSEIF (DYNAMIC .EQ. INTEGR) THEN
# !-----------------------------------------------------------------------
#       IF (ISWWAT .EQ. 'Y') THEN
#
# !       CALL SUMSW(NLAYR, DLAYR, SW, SWTOT1)
#
#         IF (MESEV .NE. 'S' .OR. MEEVP == 'Z') THEN
# !         Perform integration of soil water fluxes
# !         Subtract soil evaporation from layer 1
#           SW(1) = SW(1) - 0.1 * ES / DLAYR_YEST(1)
#         ENDIF
#
# !       Perform integration of soil water fluxes
#         DO L = 1, NLAYR
# !         With introduction of tillage routine, DLAYR can change daily.
# !         To conserve mass, convert all SW values from mm3/mm3 to mm
# !         based on yesterday's values of DLAYR.  All rates were computed
# !         prior to updating DLAYR in SOILDYN.
# !         Ultimately, need to compute all SW fluxes as mm, not
# !         volumetric fraction.
#
#           SW_mm(L)      =      SW(L) * DLAYR_YEST(L) * 10. !current SW
#           SWDELTS_mm(L) = SWDELTS(L) * DLAYR_YEST(L) * 10. !drainage
#           SWDELTX_mm(L) = SWDELTX(L) * DLAYR_YEST(L) * 10. !root extr.
#           SWDELTL_mm(L) = SWDELTL(L) * DLAYR_YEST(L) * 10. !tillage
#           SWDELTU_mm(L) = SWDELTU(L) * DLAYR_YEST(L) * 10. !upflow
#           SWDELTT_mm(L) = SWDELTT(L) * DLAYR_YEST(L) * 10. !tiledrain
#           SWDELTW_mm(L) = SWDELTW(L) * DLAYR_YEST(L) * 10. !water table
#
# !         Perform integration of soil water fluxes
#           SW_mm_NEW(L) = SW_mm(L) + SWDELTS_mm(L) + SWDELTU_mm(L)
#      &        + SWDELTL_mm(L) + SWDELTX_mm(L) + SWDELTT_mm(L)
#      &        + SWDELTW_mm(L) !(including capillary rise)
#
# !         Convert to volumetric content based on today's layer thickness
#           SW(L) = SW_mm_NEW(L) / DLAYR(L) / 10.
#
# !         Round SW to 5 decimal places
#           NewSW = ANINT(SW(L) * 1.e6)/ 1.e6
#           IF (abs(NewSW) < 1.e-4) NewSW = 0.0
#           SW(L) = NewSW
#         ENDDO
#
#         !Update mulch water content
# !       IF (INDEX('RSN',MEINF) .LE. 0) THEN
#         IF (INDEX('RSM',MEINF) > 0) THEN
#           CALL MULCHWATER(CONTROL, ISWITCH,
#      &    WATAVL, MULCH)
#         ENDIF
#
# !       CALL SUMSW(NLAYR, DLAYR, SW, SWTOT2)
# !       DIFFSW = SWTOT2 - SWTOT1
#
# !       Perform daily summation of water balance variables.
#         CALL WBSUM(INTEGR,
#      &    NLAYR, DRAIN, RAIN, RUNOFF, DLAYR, SW,          !Input
#      &    CRAIN, TDRAIN, TRUNOF, TSW, TSWINI)             !Output
#
#         IF (ActWTD .GT. DS(NLAYR)) THEN
# !         Calculate perched water table depth
#           CALL WTDEPT(
#      &      NLAYR, DLAYR, DS, DUL, SAT, SW,               !Input
#      &      WTDEP)                                        !Output
#         ENDIF
#       ENDIF
#
# !     Keep yesterday's value of DLAYR for updating tomorrow's water
# !       content
#       DLAYR_YEST = DLAYR
#
# !      CALL OPWBAL(CONTROL, ISWITCH,
# !     &    CRAIN, DLAYR, FLOODWAT, IRRAMT, LL, MULCH,      !Input
# !     &    NLAYR, RUNOFF, SOILPROP, SW, TDFC, TDFD,        !Input
# !     &    TDRAIN, TRUNOF, WTDEP)                          !Input
#
# !***********************************************************************
# !***********************************************************************
# !     OUTPUT - Daily output
# !***********************************************************************
#       ELSEIF (DYNAMIC .EQ. OUTPUT) THEN
# C-----------------------------------------------------------------------
#       IF (ISWWAT .NE. 'Y') RETURN
#
#       CALL OPWBAL(CONTROL, ISWITCH,
#      &    CRAIN, DLAYR, FLOODWAT, IRRAMT, LL, MULCH,      !Input
#      &    NLAYR, RUNOFF, SOILPROP, SW, TDFC, TDFD,        !Input
#      &    TDRAIN, TRUNOF, ActWTD, LatInflow, LatOutflow,  !Input
#      &    EXCS, WTDEP)                                    !Input
#
#       CALL OPSWBL(CONTROL, ISWITCH,
#      &    SOILPROP, SW)                                   !Input
#
# !     Water balance daily output
#       CALL Wbal(CONTROL, ISWITCH,
#      &    CRAIN, DLAYR, DRAIN, FLOODWAT, LatInflow, LatOutflow,
#      &    IRRAMT, MULCH, NLAYR, RAIN, RUNOFF, SNOW,
#      &    SWDELTS, SWDELTT, SWDELTU, SWDELTX, SWDELTL,
#      &    TDFC, TDFD, TDRAIN, TRUNOF, TSW, TSWINI)
#
# !     IF (INDEX('RSN',MEINF) .LE. 0) THEN
#       IF (INDEX('RSM',MEINF) > 0) THEN
#         CALL MULCHWATER(CONTROL, ISWITCH,
#      &    WATAVL, MULCH)
#       ENDIF
#
# !***********************************************************************
# !***********************************************************************
# !     SEASEND - Seasonal output
# !***********************************************************************
#       ELSEIF (DYNAMIC .EQ. SEASEND) THEN
# C-----------------------------------------------------------------------
#       IF (ISWWAT .NE. 'Y') RETURN
#
#       CALL OPWBAL(CONTROL, ISWITCH,
#      &    CRAIN, DLAYR, FLOODWAT, IRRAMT, LL, MULCH,      !Input
#      &    NLAYR, RUNOFF, SOILPROP, SW, TDFC, TDFD,        !Input
#      &    TDRAIN, TRUNOF, ActWTD, LatInflow, LatOutflow,  !Input
#      &    EXCS, WTDEP)                                    !Input
#
#       CALL OPSWBL(CONTROL, ISWITCH,
#      &    SOILPROP, SW)                                   !Input
#
# !     Water balance seasonal output
#       CALL Wbal(CONTROL, ISWITCH,
#      &    CRAIN, DLAYR, DRAIN, FLOODWAT, LatInflow, LatOutflow,
#      &    IRRAMT, MULCH, NLAYR, RAIN, RUNOFF, SNOW,
#      &    SWDELTS, SWDELTT, SWDELTU, SWDELTX, SWDELTL,
#      &    TDFC, TDFD, TDRAIN, TRUNOF, TSW, TSWINI)
#
# !     IF (INDEX('RSN',MEINF) .LE. 0) THEN
#       IF (INDEX('RSM',MEINF) > 0) THEN
#         CALL MULCHWATER(CONTROL, ISWITCH,
#      &    WATAVL, MULCH)
#       ENDIF
#
# !***********************************************************************
# !***********************************************************************
# !     END OF DYNAMIC IF CONSTRUCT
# !***********************************************************************
#       ENDIF
# !-----------------------------------------------------------------------
#
#       RETURN
#       END SUBROUTINE WATBAL
    SNOW = 0.0
    DRAIN = 0.0
    WINF = 0.0
    FLOODWAT = ''
    MULCH = ''


    return DRAIN, DRN, SNOW, SW, SWDELTS, TDFC, TDLNO, UPFLOW, WINF, FLOODWAT, MULCH, SWDELTU
# C=====================================================================
#       SUBROUTINE SUMSW(NLAYR, DLAYR, SW, SWTOT)
# !     Returns profile soil water content in mm.
#
#       USE MODULEDEFS
#
#       INTEGER L, NLAYR
#       REAL SW(NL), DLAYR(NL)
#       REAL SWTOT
#
#       SWTOT = 0.0
#       DO L = 1, NLAYR
#         SWTOT = SWTOT + SW(L) * DLAYR(L)
#       ENDDO
#       SWTOT = SWTOT * 10.0
#
#       END SUBROUTINE SUMSW
# C=====================================================================
#
# C=====================================================================
# !     VARIABLE DEFINITIONS: (updated 12 Feb 2004)
# !-----------------------------------------------------------------------
# ! BUNDED      Logical variable indicating if field is currently bunded
# ! CN          Runoff Curve Number - measure of runoff potential based on
# !               soil type and current soil water content.
# ! CONTROL     Composite variable containing variables related to control
# !               and/or timing of simulation.    See Appendix A.
# ! CRAIN       Cumulative precipitation (mm)
# ! DLAYR(L)    Thickness of soil layer L (cm)
# ! DRAIN       Drainage rate from soil profile (mm/d)
# ! DRN(L)      Drainage rate through soil layer L (cm/d)
# ! DS(L)       Cumulative depth in soil layer L (cm)
# ! DUL(L)      Volumetric soil water content at Drained Upper Limit in soil
# !               layer L (cm3[water]/cm3[soil])
# ! ES          Actual soil evaporation rate (mm/d)
# ! EXCS        Excess water to be added to runoff (cm/d)
# ! FLOOD       Current depth of flooding (mm)
# ! FLOODWAT    Composite variable containing information related to bund
# !               management. Structure of variable is defined in
# !               ModuleDefs.for.
# ! UPFLOW(L)   Movement of water between unsaturated soil layers due to soil
# !               evaporation: + = upward, -- = downward (cm/d)
# ! INFILT      Infiltration rate (mm / d)
# ! IRRAMT      Irrigation amount for today (mm / d)
# ! ISWWAT      Water simulation control switch (Y or N)
# ! LL(L)       Volumetric soil water content in soil layer L at lower limit
# !              (cm3 [water] / cm3 [soil])
# ! NLAYR       Actual number of soil layers
# ! PINF        Potential water available for infiltration (cm)
# ! PUDDLED     Logical variable indicating whether lowland field has been
# !               puddled
# ! PUDPERC     Potential percolation rate for puddled field (mm/d)
# ! RAIN        Precipitation depth for current day (mm)
# ! RUNOFF      Calculated runoff (mm/d)
# ! SAT(L)      Volumetric soil water content in layer L at saturation
# !              (cm3 [water] / cm3 [soil])
# ! SNOW        Snow accumulation (mm)
# ! SW(L)       Volumetric soil water content in layer L
# !              (cm3 [water] / cm3 [soil])
# ! SW_AVAIL(L) Soil water content in layer L available for evaporation,
# !               plant extraction, or movement through soil
# !               (cm3 [water] / cm3 [soil])
# ! SWCN(L)     Saturated hydraulic conductivity in layer L (cm/hr)
# ! SWCON       Soil water conductivity constant; whole profile drainage rate
# !               coefficient (1/d)
# ! SWDELTL(L)  Change in soil water content due to tillage in layer L
# ! SWDELTS(L)  Change in soil water content due to drainage in layer L
# !              (cm3 [water] / cm3 [soil])
# ! SWDELTT(L)  Change in soil water content due to tiledrain in layer L
# ! SWDELTU(L)  Change in soil water content due to evaporation and/or upward
# !               flow in layer L (cm3 [water] / cm3 [soil])
# ! SWDELTX(L)  Change in soil water content due to root water uptake in
# !               layer L (cm3 [water] / cm3 [soil])
# ! TDRAIN      Cumulative daily drainage from profile (mm)
# ! TMAX        Maximum daily temperature (�C)
# ! TRUNOF      Cumulative runoff (mm)
# ! TSW         Total soil water in profile (cm)
# ! TSWINI      Initial soil water content (cm)
# ! WATAVL      Water available for infiltration or runoff (rainfall plus
# !               irrigation) (mm/d)
# ! WINF        Water available for infiltration - rainfall minus runoff plus
# !               net irrigation (mm / d)
# ! MgmtWTD     Depth to water table (cm)
# ! WTDEP       Depth to perched water table (cm)
# !-----------------------------------------------------------------------
# !     END SUBROUTINE WATBAL
# C=====================================================================


#Test
from ModuleDefs import ControlType, SwitchType, SoilType, NL, WeatherType

# CONTROL = ControlType()
# CONTROL.RNMODE = 'B'
# CONTROL.FILEIO = 'DSSAT48.INP'
# CONTROL.RUN = 1
# CONTROL.YRDOY = 2017274
# CONTROL.YRSIM = 2017274
# CONTROL.DYNAMIC = 1
#
# ISWITCH = SwitchType()
#
#
# ES = 0.0
# IRRAMT = 0.0
# SOILPROP = SoilType()
# SWDELTX = [0.0] * NL
# TILLVALS = ""
# WEATHER = WeatherType()
# FLOODWAT = ''
# MULCH = ''
# SWDELTU = []
#
# print(WATBAL(CONTROL, ISWITCH, ES, IRRAMT, SOILPROP, SWDELTX,TILLVALS, WEATHER, FLOODWAT, MULCH, SWDELTU))