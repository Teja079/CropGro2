#=======================================================================
#  SPAM, Subroutine
#  Calculates soil-plant-atmosphere interface energy balance components.
#-----------------------------------------------------------------------
#  Called by: Main
#  Calls:     XTRACT, OPSPAM    (File SPSUBS.for)
#             PET     (File PET.for)
#             PSE     (File PET.for)
#             ROOTWU  (File ROOTWU.for)
#             SOILEV  (File SOILEV.for)
#             TRANS   (File TRANS.for)
#=======================================================================
#
def SPAM(CONTROL, ISWITCH,CANHT, EORATIO, KSEVAP, KTRANS, MULCH, PSTRES1,
         PORMIN, RLV, RWUMX, SOILPROP, SW, SWDELTS, UH2O, WEATHER, WINF,
         XHLAI, XLAI,FLOODWAT, SWDELTU):
    from ROOTWU import ROOTWU
    from ETPHOT import ETPHOT
    import numpy as np
    from ModuleDefs import NL
# !-----------------------------------------------------------------------
#       USE ModuleDefs
#       USE ModuleData
#       USE FloodModule
#
#       IMPLICIT NONE
#       EXTERNAL ETPHOT, STEMP_EPIC, STEMP, ROOTWU, SOILEV, TRANS
#       EXTERNAL MULCH_EVAP, OPSPAM, PET, PSE, FLOOD_EVAP, ESR_SOILEVAP
#       EXTERNAL XTRACT
#       SAVE
#
#       CHARACTER*1  IDETW, ISWWAT
#       CHARACTER*1  MEEVP, MEINF, MEPHO, MESEV, METMP
#       CHARACTER*2  CROP
#       CHARACTER*6, PARAMETER :: ERRKEY = "SPAM  "
# !      CHARACTER*78 MSG(2)
#
#       INTEGER DYNAMIC, L, NLAYR
#
#       REAL CANHT, CO2, SRAD, TAVG,
#      &    TMAX, TMIN, WINDSP, XHLAI, XLAI
#       REAL CEF, CEM, CEO, CEP, CES, CET, CEVAP
#       REAL EF, EM, EO, EP, ES, ET, EVAP
#       REAL TRWU, TRWUP, U
#       REAL EOS, EOP, WINF, MSALB, ET_ALB
#       REAL XLAT, TAV, TAMP, SRFTEMP
#       REAL EORATIO, KSEVAP, KTRANS
#
#       REAL DLAYR(NL), DUL(NL), LL(NL), RLV(NL), RWU(NL),
#      &    SAT(NL), ST(NL), SW(NL), SW_AVAIL(NL), !SWAD(NL),
#      &    SWDELTS(NL), SWDELTU(NL), SWDELTX(NL), UPFLOW(NL)
#       REAL ES_LYR(NL)
    ST = np.zeros(NL, dtype=float)
#
# !     Root water uptake computed by some plant routines (optional)
#       REAL UH2O(NL)
#
# !     Species-dependant variables imported from PLANT module:
#       REAL PORMIN, RWUMX
#
# !     Flood management variables:
#       REAL FLOOD, EOS_SOIL
#
# !     P Stress on photosynthesis
#       REAL PSTRES1
# !     Hourly transpiration for MEEVP=H
#       REAL, DIMENSION(TS)    :: ET0
#
# !-----------------------------------------------------------------------
# !     Define constructed variable types based on definitions in
# !     ModuleDefs.for.
#       TYPE (ControlType) CONTROL
#       TYPE (SoilType)    SOILPROP
#       TYPE (SwitchType)  ISWITCH
#       TYPE (FloodWatType)FLOODWAT
#       TYPE (MulchType)   MULCH
#       TYPE (WeatherType) WEATHER
#
#      Transfer values from constructed data types into local variables.
    CROP    = CONTROL.CROP
    DYNAMIC = CONTROL.DYNAMIC

    DLAYR  = SOILPROP.DLAYR
    DUL    = SOILPROP.DUL
    LL     = SOILPROP.LL
    MSALB  = SOILPROP.MSALB
    NLAYR  = SOILPROP.NLAYR
    SAT    = SOILPROP.SAT
    U      = SOILPROP.U

    ISWWAT = ISWITCH.ISWWAT
    IDETW  = ISWITCH.IDETW
    MEEVP  = ISWITCH.MEEVP
    MEINF  = ISWITCH.MEINF
    MEPHO  = ISWITCH.MEPHO
    METMP  = ISWITCH.METMP
    MESEV  = ISWITCH.MESEV

    FLOOD  = FLOODWAT.FLOOD

    CO2    = WEATHER.CO2
    SRAD   = WEATHER.SRAD
    TAMP   = WEATHER.TAMP
    TAV    = WEATHER.TAV
    TAVG   = WEATHER.TAVG
    TMAX   = WEATHER.TMAX
    TMIN   = WEATHER.TMIN
    WINDSP = WEATHER.WINDSP
    XLAT   = WEATHER.XLAT
#
# !***********************************************************************
# !***********************************************************************
# !     Run Initialization - Called once per simulation
# !***********************************************************************
    if DYNAMIC == RUNINIT:
# !-----------------------------------------------------------------------
        if MEPHO == 'L' or MEEVP == 'Z':
            EOP, EP, ES, RWU, TRWUP = ETPHOT(CONTROL, ISWITCH,PORMIN, PSTRES1,
                                             RLV, RWUMX, SOILPROP, ST, SW,WEATHER, XLAI)

#      Initialize ASCE dual KC ET variables (KRT)
            PUT('SPAM', 'REFET', -99.0)
            PUT('SPAM', 'KCB', -99.0)
            PUT('SPAM', 'KE', -99.0)
            PUT('SPAM', 'KC', -99.0)
#
#***********************************************************************
#***********************************************************************
#     Seasonal initialization - run once per season
#***********************************************************************
    elif DYNAMIC == SEASINIT:
#-----------------------------------------------------------------------
        EF   = 0.0
        CEF  = 0.0
        EM   = 0.0
        CEM  = 0.0
        EO   = 0.0
        CEO  = 0.0
        EP   = 0.0
        EOP  = 0.0
        CEP  = 0.0
        ES   = 0.0
        EOS  = 0.0
        CES  = 0.0
        ET   = 0.0
        CET  = 0.0
        EVAP = 0.0
        CEVAP =0.0
        ES_LYR = 0.0
        SWDELTX = 0.0
        TRWU = 0.0
        XHLAI = 0.0
        ET0 = 0.0
#
# ---------------------------------------------------------
        if MEEVP !='Z':
            pass
     #    SELECT CASE (METMP)
     #    CASE ('E')    !EPIC soil temperature routine
     #      CALL STEMP_EPIC(CONTROL, ISWITCH,
     # &      SOILPROP, SW, TAVG, TMAX, TMIN, TAV, WEATHER,    !Input
     # &      SRFTEMP, ST)                                     !Output
     #    CASE DEFAULT  !DSSAT soil temperature
     #      CALL STEMP(CONTROL, ISWITCH,
     # &      SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP, !Input
     # &      SRFTEMP, ST)                                     !Output
     #    END SELECT
#      ---------------------------------------------------------
        if MEEVP != 'Z':
            RWU, TRWUP = ROOTWU(SEASINIT,DLAYR, LL, NLAYR,
                                PORMIN, RLV, RWUMX, SAT, SW)

# !       Initialize soil evaporation variables
#         SELECT CASE (MESEV)
# !     ----------------------------
#         CASE ('R')  !Original soil evaporation routine
#           CALL SOILEV(SEASINIT,
#      &      DLAYR, DUL, EOS, LL, SW, SW_AVAIL(1),         !Input
#      &      U, WINF,                                      !Input
#      &      ES)                                           !Output
# !     ----------------------------
#         END SELECT
#
# !       Initialize plant transpiration variables
        EOP = TRANS(DYNAMIC, MEEVP,CO2, CROP, EO, ET0, EVAP, KTRANS,
                    WINDSP, XHLAI, WEATHER)
#
#       CALL MULCH_EVAP(DYNAMIC, MULCH, EOS, EM)
#
# !     ---------------------------------------------------------
        if CROP != 'FA':
            if MEPHO == 'L' or MEEVP == 'Z':
                EOP, EP, ES, RWU, TRWUP = (ETPHOT(CONTROL, ISWITCH,PORMIN,
                    PSTRES1, RLV, RWUMX, SOILPROP, ST, SW, WEATHER, XLAI))
#
# !     Call OPSPAM to open and write headers to output file
        if IDETW == 'Y':
            OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,CEF, CEM, CEO, CEP, CES, CET,
                   CEVAP, EF, EM,EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP, SRAD,
                   ES_LYR, SOILPROP)
#
#      Transfer data to storage routine
            PUT('SPAM', 'CEF', CEF)
            PUT('SPAM', 'CEM', CEM)
            PUT('SPAM', 'CEO', CEO)
            PUT('SPAM', 'CEP', CEP)
            PUT('SPAM', 'CES', CES)
            PUT('SPAM', 'CET', CET)
            PUT('SPAM', 'CEVAP',CEVAP)
            PUT('SPAM', 'EF',  EF)
            PUT('SPAM', 'EM',  EM)
            PUT('SPAM', 'EO',  EO)
            PUT('SPAM', 'EP',  EP)
            PUT('SPAM', 'ES',  ES)
            PUT('SPAM', 'ET',  ET)
#
# !***********************************************************************
# !***********************************************************************
# !     DAILY RATE CALCULATIONS
# !***********************************************************************
    elif DYNAMIC == RATE:
# !-----------------------------------------------------------------------
        SWDELTX = 0.0
#     ---------------------------------------------------------
#         if MEEVP .NE.'Z') THEN  !LPM 02dec14 use values from ETPHOT
#         SELECT CASE (METMP)
#         CASE ('E')    !EPIC soil temperature routine
#           CALL STEMP_EPIC(CONTROL, ISWITCH,
#      &      SOILPROP, SW, TAVG, TMAX, TMIN, TAV, WEATHER,   !Input
#      &      SRFTEMP, ST)                                    !Output
#         CASE DEFAULT
# !       7/21/2016 - DSSAT method is default, per GH
# !       CASE ('D')  !DSSAT soil temperature
#           CALL STEMP(CONTROL, ISWITCH,
#      &      SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
#      &      SRFTEMP, ST)                                    !Output
#         END SELECT
#       ENDIF
# !-----------------------------------------------------------------------
# !     POTENTIAL ROOT WATER UPTAKE
# !-----------------------------------------------------------------------
        if ISWWAT == 'Y':
#       Calculate the availability of soil water for use in SOILEV.
            for L in range (1, NLAYR+1):
                SW_AVAIL[L] = max(0.0, SW[L] + SWDELTS[L] + SWDELTU[L])

#       These processes are done by ETPHOT for hourly (Zonal) energy
#       balance method.
        if MEEVP != 'Z':
#         Calculate potential root water uptake rate for each soil layer
#         and total potential water uptake rate.
            if XHLAI > 0.0:
                RWU, TRWUP = ROOTWU(RATE,DLAYR, LL, NLAYR, PORMIN, RLV,
                                    RWUMX, SAT, SW)
            else:
                RWU   = 0.0
                TRWUP = 0.0
#
#-----------------------------------------------------------------------
#         POTENTIAL EVAPOTRANSPIRATION
#-----------------------------------------------------------------------
        if FLOOD > 0.0:
            # Set albedo to 0.08 under flooded conditions
            # US - change to 0.05 Feb2004
            ET_ALB = 0.05
        else:
            ET_ALB = MSALB

        PET(CONTROL,
            ET_ALB, XHLAI, MEEVP, WEATHER,  #Input for all
            EORATIO, #Needed by Penman-Monteith
            CANHT,   #Needed by dynamic Penman-Monteith
            EO,      #Output
            ET0)     #Output hourly Priestly-Taylor with VPD effect

# !-----------------------------------------------------------------------
# !         POTENTIAL SOIL EVAPORATION
# !-----------------------------------------------------------------------
# !         05/26/2007 CHP/MJ Use XLAI instead of XHLAI
# !         This was important for Canegro and affects CROPGRO crops
# !             only very slightly (max 0.5% yield diff for one peanut
# !             experiment).  No difference to other crop models.
#           CALL PSE(EO, KSEVAP, XLAI, EOS)
#
# !-----------------------------------------------------------------------
# !         ACTUAL SOIL, MULCH AND FLOOD EVAPORATION
# !-----------------------------------------------------------------------
# !         Initialize soil, mulch and flood evaporation
#           ES = 0.; EM = 0.; EF = 0.; EVAP = 0.0
#           UPFLOW = 0.0; ES_LYR = 0.0
#
# !         First meet evaporative demand from floodwater
#           IF (FLOOD .GT. 1.E-4) THEN
#             CALL FLOOD_EVAP(XLAI, EO, EF)
#             IF (EF > FLOOD) THEN
# !             Floodwater not enough to supply EOS demand
#               EOS_SOIL = MIN(EF - FLOOD, EOS)
#               EF = FLOOD
#             ELSE
#               EOS_SOIL = 0.0
#             ENDIF
#           ELSE
#             EOS_SOIL = EOS
#           ENDIF
#
# !         Next meet evaporative demand from mulch
#           IF (EOS_SOIL > 1.E-6 .AND. INDEX('RSM',MEINF) > 0) THEN
#             CALL MULCH_EVAP(DYNAMIC, MULCH, EOS_SOIL, EM)
#             IF (EOS_SOIL > EM) THEN
# !             Some evaporative demand leftover for soil
#               EOS_SOIL = EOS_SOIL - EM
#             ELSE
#               EOS_SOIL = 0.0
#             ENDIF
#           ENDIF
#
# !         Soil evaporation after flood and mulch evaporation
#           IF (EOS_SOIL > 1.E-6) THEN
#             SELECT CASE(MESEV)
# !           ------------------------
#             CASE ('S')  ! Sulieman-Ritchie soil evaporation routine
# !             Note that this routine calculates UPFLOW, unlike the SOILEV.
#               CALL ESR_SoilEvap(
#      &          EOS_SOIL, SOILPROP, SW, SWDELTS,          !Input
#      &          ES, ES_LYR, SWDELTU, UPFLOW)              !Output
# !           ------------------------
#             CASE DEFAULT
# !           CASE ('R')  !Ritchie soil evaporation routine
# !             Calculate the availability of soil water for use in SOILEV.
#               DO L = 1, NLAYR
#                 SW_AVAIL(L) = MAX(0.0, SW(L) + SWDELTS(L) + SWDELTU(L))
#               ENDDO
#               CALL SOILEV(RATE,
#      &          DLAYR, DUL, EOS_SOIL, LL, SW,             !Input
#      &          SW_AVAIL(1), U, WINF,                     !Input
#      &          ES)                                       !Output
#             END SELECT
# !           ------------------------
#           ENDIF
#
# !         Total evaporation from soil, mulch, flood
#           EVAP = ES + EM + EF
#
# !-----------------------------------------------------------------------
# !         Potential transpiration - model dependent
# !-----------------------------------------------------------------------
        if XHLAI > 1.E-6:
            EOP = TRANS(RATE, MEEVP,CO2, CROP, EO, ET0, EVAP, KTRANS,WINDSP, XHLAI,WEATHER)
        else:
            EOP = 0.0
# !-----------------------------------------------------------------------
# !         ACTUAL TRANSPIRATION
# !-----------------------------------------------------------------------
        if XHLAI > 1.E-4 and EOP > 1.E-4:
            #These calcs replace the old SWFACS subroutine
            #Stress factors now calculated as needed in PLANT routines.
            EP = min(EOP, TRWUP*10.)
        else:
            EP = 0.0
# !-----------------------------------------------------------------------
# !     ALTERNATE CALL TO ENERGY BALANCE ROUTINES
# !-----------------------------------------------------------------------
        if CROP != 'FA':
            if MEEVP == 'Z' or (MEPHO == 'L' and XHLAI > 0.0):
            #ETPHOT called for photosynthesis only
            #    (MEPHO = 'L' and MEEVP <> 'Z')
            #or for both photosynthesis and evapotranspiration
            #   (MEPHO = 'L' and MEEVP = 'Z').
                EOP, EP, ES, RWU, TRWUP = ETPHOT(CONTROL, ISWITCH,PORMIN, PSTRES1,
                                             RLV, RWUMX, SOILPROP, ST, SW,WEATHER, XLAI)
# -----------------------------------------------------------------------
#        ACTUAL ROOT WATER EXTRACTION
# -----------------------------------------------------------------------
        if ISWWAT == 'Y':
#          Adjust available soil water for evaporation
            if MESEV =='R':
                SW_AVAIL[1] = max(0.0, SW_AVAIL[1] - 0.1 * ES / DLAYR[1])

            else:
                for L in range (1, NLAYR+1):
                    SW_AVAIL[L] = max(0.0,SW_AVAIL[L] -0.1*ES_LYR[L]/DLAYR[L])

# !         Calculate actual soil water uptake and transpiration rates
            SWDELTX, TRWU = XTRACT(NLAYR, DLAYR, LL, SW, SW_AVAIL, TRWUP, UH2O, EP, RWU)
#
#     Transfer computed value of potential floodwater evaporation to
#     flood variable.
        FLOODWAT.EF = EF
#
#     Transfer data to storage routine
        PUT('SPAM', 'EF',  EF)
        PUT('SPAM', 'EM',  EM)
        PUT('SPAM', 'EO',  EO)
        PUT('SPAM', 'EP',  EP)
        PUT('SPAM', 'ES',  ES)
        PUT('SPAM', 'EOP', EOP)
        PUT('SPAM', 'EVAP',EVAP)
        PUT('SPAM', 'UH2O',RWU)
#
# !***********************************************************************
# !***********************************************************************
# !     DAILY INTEGRATION
# !***********************************************************************
    elif DYNAMIC == INTEGR:
# !-----------------------------------------------------------------------
        if ISWWAT == 'Y':
#        Perform daily summation of water balance variables.
            ET  = EVAP + EP
            CEF = CEF + EF
            CEM = CEM + EM
            CEO = CEO + EO
            CEP = CEP + EP
            CES = CES + ES
            CEVAP=CEVAP + EVAP
            CET = CET + ET

        if IDETW == 'Y':
            OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,CEF, CEM, CEO, CEP, CES, CET,
                   CEVAP, EF, EM, EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP,
                   SRAD,ES_LYR, SOILPROP)
#
#      Transfer data to storage routine
        PUT('SPAM', 'CEF', CEF)
        PUT('SPAM', 'CEM', CEM)
        PUT('SPAM', 'CEO', CEO)
        PUT('SPAM', 'CEP', CEP)
        PUT('SPAM', 'CES', CES)
        PUT('SPAM', 'CET', CET)
        PUT('SPAM', 'ET',  ET)
        PUT('SPAM', 'CEVAP', CEVAP)
#
# !***********************************************************************
# !***********************************************************************
# !     OUTPUT - daily output
# !***********************************************************************
    elif DYNAMIC == OUTPUT:
# C-----------------------------------------------------------------------
# !     Flood water evaporation can be modified by Paddy_Mgmt routine.
        EF = FLOODWAT.EF
#
# !     ---------------------------------------------------------
#       IF (MEEVP .NE.'Z') THEN  !LPM 02dec14 use values from ETPHOT
#           SELECT CASE (METMP)
#           CASE ('E')    !EPIC soil temperature routine
#             CALL STEMP_EPIC(CONTROL, ISWITCH,
#      &        SOILPROP, SW, TAVG, TMAX, TMIN, TAV, WEATHER,   !Input
#      &        SRFTEMP, ST)                                    !Output
#           CASE DEFAULT  !DSSAT soilt temperature
#             CALL STEMP(CONTROL, ISWITCH,
#      &        SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
#      &        SRFTEMP, ST)                                    !Output
#           END SELECT
#       ENDIF
#
        OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,CEF, CEM, CEO, CEP, CES, CET,
               CEVAP, EF, EM,EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP,
               SRAD,ES_LYR, SOILPROP)
#
        if CROP != 'FA' and MEPHO == 'L':
            EOP, EP, ES, RWU, TRWUP = ETPHOT(CONTROL, ISWITCH,PORMIN, PSTRES1, RLV,
                                             RWUMX, SOILPROP, ST, SW, WEATHER, XLAI)
#
#       CALL OPSTRESS(CONTROL, ET=ET, EP=EP)
#
# ***********************************************************************
# ***********************************************************************
#      SEASEND - seasonal output
# ***********************************************************************
    elif DYNAMIC == SEASEND:
# -----------------------------------------------------------------------
        OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,CEF, CEM, CEO, CEP, CES,
               CET, CEVAP, EF, EM,EO, EOP, EOS, EP, ES, ET, TMAX, TMIN,
               TRWUP, SRAD,ES_LYR, SOILPROP)

# !     ---------------------------------------------------------
#       IF (MEEVP .NE.'Z') THEN  !LPM 02dec14 use values from ETPHOT
#           SELECT CASE (METMP)
#           CASE ('E')    !EPIC soil temperature routine
#             CALL STEMP_EPIC(CONTROL, ISWITCH,
#      &        SOILPROP, SW, TAVG, TMAX, TMIN, TAV, WEATHER,   !Input
#      &        SRFTEMP, ST)                                    !Output
#           CASE DEFAULT  !DSSAT soilt temperature
#             CALL STEMP(CONTROL, ISWITCH,
#      &        SOILPROP, SRAD, SW, TAVG, TMAX, XLAT, TAV, TAMP,!Input
#      &        SRFTEMP, ST)                                    !Output
#           END SELECT
#       ENDIF
#
        if MEPHO == 'L':
            EOP, EP, ES, RWU, TRWUP = ETPHOT(CONTROL, ISWITCH,PORMIN, PSTRES1,
                                             RLV, RWUMX, SOILPROP, ST, SW,WEATHER, XLAI)

#     Transfer data to storage routine
        PUT('SPAM', 'CEF', CEF)
        PUT('SPAM', 'CEM', CEM)
        PUT('SPAM', 'CEO', CEO)
        PUT('SPAM', 'CEP', CEP)
        PUT('SPAM', 'CES', CES)
        PUT('SPAM', 'CET', CET)
        PUT('SPAM', 'ET',  ET)
        PUT('SPAM', 'CEVAP', CEVAP)
#
# !      CALL OPSTRESS(CONTROL, ET=ET, EP=EP)
#
# !***********************************************************************
# !***********************************************************************
# !     END OF DYNAMIC IF CONSTRUCT
# !***********************************************************************
#       ENDIF
# !-----------------------------------------------------------------------
#       RETURN
#       END SUBROUTINE SPAM
#
    return (EO,EOP,EOS,EP,ES,RWU,SRFTEMP,ST,SWDELTX,TRWU,TRWUP,UPFLOW)
# !-----------------------------------------------------------------------
# !     VARIABLE DEFINITIONS: (updated 12 Feb 2004)
# !-----------------------------------------------------------------------
# ! CANHT       Canopy height (m)
# ! CEF         Cumulative seasonal evaporation from floodwater surface (mm)
# ! CEM         Cumulative evaporation from surface mulch layer (mm)
# ! CEO         Cumulative potential evapotranspiration (mm)
# ! CEP         Cumulative transpiration (mm)
# ! CES         Cumulative evaporation (mm)
# ! CET         Cumulative evapotranspiration (mm)
# ! CLOUDS      Relative cloudiness factor (0-1)
# ! CO2         Atmospheric carbon dioxide concentration
# !              (µmol[CO2] / mol[air])
# ! CONTROL     Composite variable containing variables related to control
# !               and/or timing of simulation.    See Appendix A.
# ! CROP        Crop identification code
# ! DLAYR(L)    Thickness of soil layer L (cm)
# ! DUL(L)      Volumetric soil water content at Drained Upper Limit in soil
# !               layer L (cm3[water]/cm3[soil])
# ! EF          Evaporation rate from flood surface (mm / d)
# ! EM          Evaporation rate from surface mulch layer (mm / d)
# ! EO          Potential evapotranspiration rate (mm/d)
# ! EOP         Potential plant transpiration rate (mm/d)
# ! EORATIO     Ratio of increase in potential evapotranspiration with
# !               increase in LAI (up to LAI=6.0) for use with FAO-56 Penman
# !               reference potential evapotranspiration.
# ! EOS         Potential rate of soil evaporation (mm/d)
# ! EP          Actual plant transpiration rate (mm/d)
# ! ES          Actual soil evaporation rate (mm/d)
# ! ET          Actual evapotranspiration rate (mm/d)
# ! FLOOD       Current depth of flooding (mm)
# ! FLOODWAT    Composite variable containing information related to bund
# !               management. Structure of variable is defined in
# !               ModuleDefs.for.
# ! IDETW       Y=detailed water balance output, N=no detailed output
# ! ISWITCH     Composite variable containing switches which control flow of
# !               execution for model.  The structure of the variable
# !               (SwitchType) is defined in ModuleDefs.for.
# ! ISWWAT      Water simulation control switch (Y or N)
# ! KSEVAP      Light extinction coefficient used for computation of soil
# !               evaporation
# ! KTRANS      Light extinction coefficient used for computation of plant
# !               transpiration
# ! LL(L)       Volumetric soil water content in soil layer L at lower limit
# !              (cm3 [water] / cm3 [soil])
# ! MEEVP       Method of evapotranspiration ('P'=Penman,
# !               'R'=Priestly-Taylor, 'Z'=Zonal)
# ! MEPHO       Method for photosynthesis computation ('C'=Canopy or daily,
# !               'L'=hedgerow or hourly)
# ! NLAYR       Actual number of soil layers
# ! PORMIN      Minimum pore space required for supplying oxygen to roots for
# !               optimal growth and function (cm3/cm3)
# ! RLV(L)      Root length density for soil layer L (cm[root] / cm3[soil])
# ! RWU(L)      Root water uptake from soil layer L (cm/d)
# ! RWUMX       Maximum water uptake per unit root length, constrained by
# !               soil water (cm3[water] / cm [root])
# ! MSALB       Soil albedo with mulch and soil water effects (fraction)
# ! SAT(L)      Volumetric soil water content in layer L at saturation
# !              (cm3 [water] / cm3 [soil])
# ! SOILPROP    Composite variable containing soil properties including bulk
# !               density, drained upper limit, lower limit, pH, saturation
# !               water content.  Structure defined in ModuleDefs.
# ! SRAD        Solar radiation (MJ/m2-d)
# ! SRFTEMP     Temperature of soil surface litter (°C)
# ! ST(L)       Soil temperature in soil layer L (°C)
# ! SUMES1      Cumulative soil evaporation in stage 1 (mm)
# ! SUMES2      Cumulative soil evaporation in stage 2 (mm)
# ! SW(L)       Volumetric soil water content in layer L
# !              (cm3 [water] / cm3 [soil])
# ! SW_AVAIL(L) Soil water content in layer L available for evaporation,
# !               plant extraction, or movement through soil
# !               (cm3 [water] / cm3 [soil])
# ! SWDELTS(L)  Change in soil water content due to drainage in layer L
# !              (cm3 [water] / cm3 [soil])
# ! SWDELTU(L)  Change in soil water content due to evaporation and/or upward
# !               flow in layer L (cm3 [water] / cm3 [soil])
# ! SWDELTX(L)  Change in soil water content due to root water uptake in
# !               layer L (cm3 [water] / cm3 [soil])
# ! T           Number of days into Stage 2 evaporation (WATBAL); or time
# !               factor for hourly temperature calculations
# ! TA          Daily normal temperature (°C)
# ! TAMP        Amplitude of temperature function used to calculate soil
# !               temperatures (°C)
# ! TAV         Average annual soil temperature, used with TAMP to calculate
# !               soil temperature. (°C)
# ! TAVG        Average daily temperature (°C)
# ! TMAX        Maximum daily temperature (°C)
# ! TMIN        Minimum daily temperature (°C)
# ! TRWU        Actual daily root water uptake over soil profile (cm/d)
# ! TRWUP       Potential daily root water uptake over soil profile (cm/d)
# ! U           Evaporation limit (cm)
# ! WINDSP      Wind speed at 2m (km/d)
# ! WINF        Water available for infiltration - rainfall minus runoff plus
# !               net irrigation (mm / d)
# ! XHLAI       Healthy leaf area index (m2[leaf] / m2[ground])
# ! XLAT        Latitude (deg.)
# !-----------------------------------------------------------------------
# !     END SUBROUTINE SPAM
# !-----------------------------------------------------------------------
#
