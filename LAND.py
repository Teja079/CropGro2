# =======================================================================
#   Land Unit Module.  Provides the interface between soil, weather
#   and crops.  Based on the original CROPGRO routine
# =======================================================================
# from Scratch.phenol import RUNINIT
# from Scratch.test import CONTROL

def LAND(CONTROL, ISWITCH, YRPLT, YREND):
    from ModuleDefs import NL, RunConstants as RC, FertType, ResidueType, MgmtType, OrgMatAppType, \
        TillType, FloodNType, FloodWatType, MulchType, WEATHER, ISWITCH
      #USE FloodModule

    from ERROR import ERROR
    from WARNING import WARNING
    from SOIL import SOIL
    from IPIBS import IPIBS
    from weathr import WEATHR
    # from SPAM import SPAM
    # from plant import PLANT

      #EXTERNAL INFO, ERROR, WARNING, IPIBS, WEATHR, SOIL, SPAM, PLANT,
     #&  OPSUM, MGMTOPS
      #SAVE
    # -----------------------------------------------------------------------
    #      Crop, Experiment, Command line Variables
    # -----------------------------------------------------------------------
    ERRKEY = 'LAND  '
    MSG = ['']*2
    # NELEM = 3

    ES : float = 0.0
    KTRANS : float = 0.0
    XHLAI : float = 0.0
    CANHT : float = 0.0
    EORATIO : float = 0.0
    KSEVAP : float = 0.0
    PSTRES1 : float = 0.0
    PORMIN : float = 0.0
    RWUMX : float = 0.0
    XLAI : float = 0.0
    maxnlayers : int = 1

    NH4_plant = [0.0] * NL
    NO3_plant = [0.0] * NL
    SPi_Avail = [0.0] * NL
    SKi_Avail = [0.0] * NL
    ST = [0.0] * NL
    UPPM = [0.0] * NL
    SW = [0.0] * NL
    SWDELTS = [0.0] * NL
    UPFLOW = [0.0] * NL
    # SomLitC = [0.0] * (NL+1)
    # SomLitE = [[0.0]*NELEM] * (NL+1)
    SWDELTU = [0.0] * NL
    SWDELTX = [0.0] * NL
    UH2O = [0.0] * NL
    RWU = [0.0] * NL
    PUptake = [0.0] * NL
    RLV = [0.0] * NL
    FracRts = [0.0] * NL
    UNH4 = [0.0] * NL
    UNO3 = [0.0] * NL
    KUptake = [0.0] * NL
    HARVFRAC = [0.0] * 2

    FERTDATA = FertType()
    HARVRES = ResidueType()
    SENESCE = ResidueType()
    IRRAMT = MgmtType()
    OMAData = OrgMatAppType()
    TILLVALS = TillType()
    FLOODN = FloodNType()
    FLOODWAT = FloodWatType()
    MULCH = MulchType()

    CROP    = CONTROL.CROP
    DYNAMIC = CONTROL.DYNAMIC
    FILEIO  = CONTROL.FILEIO
    MODEL   = CONTROL.MODEL
    YRDOY   = CONTROL.YRDOY
    YRSIM   = CONTROL.YRSIM

    IPLTI   = ISWITCH.IPLTI

# ***********************************************************************
# ***********************************************************************
#      Run Initialization - Called once per simulation
# ***********************************************************************
    if DYNAMIC == RC.RUNINIT:
        CROP, ISWITCH.IDETS, MODEL = IPIBS (CONTROL, ISWITCH)
# -----------------------------------------------------------------------
#      Read input parameters for weather routines
# -----------------------------------------------------------------------
        WEATHR(CONTROL, ISWITCH, WEATHER, YREND)
# -----------------------------------------------------------------------
#      Read initial soil data
# -----------------------------------------------------------------------
        (NH4_plant, NO3_plant, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, SomLitC,
            SomLitE, SW, SWDELTS, SWDELTU, UPPM, WINF,
            YREND) = SOIL(CONTROL, ISWITCH,
                          ES, FERTDATA, FracRts, HARVRES, IRRAMT, KTRANS,
                          KUptake, OMAData, PUptake, RLV, SENESCE, ST, SWDELTX,
                          TILLVALS, UNH4, UNO3, WEATHER, XHLAI, FLOODN,
                          FLOODWAT, MULCH, UPFLOW)

# -----------------------------------------------------------------------
#      Read initial soil-plant-atmosphere data
# -----------------------------------------------------------------------
        (EO,EOP,EOS,EP,ES,RWU,SRFTEMP,ST,SWDELTX,TRWU,TRWUP,UPFLOW) = (
        SPAM(CONTROL, ISWITCH, CANHT, EORATIO, KSEVAP, KTRANS, MULCH, PSTRES1,
         PORMIN, RLV, RWUMX, SOILPROP, SW, SWDELTS, UH2O, WEATHER, WINF,
         XHLAI, XLAI, FLOODWAT, SWDELTU))

# -----------------------------------------------------------------------
#      Read initial plant module data
# -----------------------------------------------------------------------
        (CANHT, EORATIO, HARVRES, KSEVAP, KTRANS, KUptake, MDATE, NSTRES, PSTRES1,
            PUptake, PORMIN, RLV, RWUMX, SENESCE, STGDOY, FracRts, UH2O, UNH4, UNO3,
            XHLAI, XLAI) =  (PLANT(CONTROL, ISWITCH, EO, EOP, EOS, EP, ES,
                                FLOODWAT, HARVFRAC, IRRAMT, NH4_plant, NO3_plant, SKi_Avail,
                                SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW, TRWUP,
                                WEATHER, YREND, YRPLT, FLOODN))

# -----------------------------------------------------------------------
#      Initialize summary.out information
# -----------------------------------------------------------------------
      #CALL OPSUM (CONTROL, ISWITCH, YRPLT)

# ***********************************************************************
# ***********************************************************************
#      SEASONAL INITIALIZATION
# ***********************************************************************
    elif DYNAMIC == RC.SEASINIT:
# -----------------------------------------------------------------------
#      Call WEATHR for initialization - reads first day of weather
#      data for use in soil N and soil temp initialization.
# -----------------------------------------------------------------------
        WEATHR(CONTROL, ISWITCH, WEATHER, YREND)

# -----------------------------------------------------------------------
#      Set planting date, adjust operations dates for seasonal or
#      sequenced runs.
# -----------------------------------------------------------------------
#       CALL MGMTOPS(CONTROL, ISWITCH,
#      &    FLOODWAT, HARVRES, SOILPROP, ST,                !Input
#      &    STGDOY, SW, WEATHER,                            !Input
#      &    YREND, FERTDATA, HARVFRAC, IRRAMT,              !Output
#      &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output

#-----------------------------------------------------------------------
        if YRPLT < YRSIM and CROP != 'FA' and IPLTI.find('AF') == -1:
            ERROR(ERRKEY,2,' ',0)
            return

# -----------------------------------------------------------------------
#      Seasonal initialization for soil processes
# -----------------------------------------------------------------------
        (NH4_plant, NO3_plant, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, SomLitC,
             SomLitE, SW, SWDELTS, SWDELTU, UPPM, WINF,
             YREND) = (SOIL(CONTROL, ISWITCH, ES, FERTDATA, FracRts, HARVRES,
                           IRRAMT, KTRANS, KUptake, OMAData, PUptake, RLV,
                           SENESCE, ST, SWDELTX, TILLVALS, UNH4, UNO3, WEATHER,
                           XHLAI, FLOODN, FLOODWAT, MULCH, UPFLOW))

# C-----------------------------------------------------------------------
# C     Seasonal initialization for soil-plant-atmosphere processes
# C-----------------------------------------------------------------------
        (EO, EOP, EOS, EP, ES, RWU, SRFTEMP, ST, SWDELTX, TRWU, TRWUP, UPFLOW) = (
            SPAM(CONTROL, ISWITCH, CANHT, EORATIO, KSEVAP, KTRANS, MULCH, PSTRES1,
                PORMIN, RLV, RWUMX, SOILPROP, SW, SWDELTS, UH2O, WEATHER, WINF,
                XHLAI, XLAI, FLOODWAT, SWDELTU))

# -----------------------------------------------------------------------
#      Initialize PLANT routines (including phenology and pest)
# -----------------------------------------------------------------------
        (CANHT, EORATIO, HARVRES, KSEVAP, KTRANS, KUptake, MDATE, NSTRES, PSTRES1,
            PUptake, PORMIN, RLV, RWUMX, SENESCE, STGDOY, FracRts, UH2O, UNH4, UNO3,
            XHLAI, XLAI) = (PLANT(CONTROL, ISWITCH, EO, EOP, EOS, EP, ES,
                                FLOODWAT, HARVFRAC, IRRAMT, NH4_plant, NO3_plant, SKi_Avail,
                                SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW, TRWUP,
                                WEATHER, YREND, YRPLT, FLOODN))

# -----------------------------------------------------------------------
#      Initialize summary output file - possible output from
#      various modules.
# -----------------------------------------------------------------------
        if ISWITCH.IDETS == 'Y' or ISWITCH.IDETS == 'A':
            pass
            #OPSUM (CONTROL, ISWITCH, YRPLT)

# ***********************************************************************
# ***********************************************************************
#      DAILY RATE CALCULATIONS
# ***********************************************************************
    elif DYNAMIC == RC.RATE:
# -----------------------------------------------------------------------
#      Call WEATHER Subroutine to input weather data and to
#      calculate hourly radiation and air temperature values
#      Note: First day of weather has already been read by
#        initialization call to WEATHR.
# -----------------------------------------------------------------------
        WEATHR(CONTROL, ISWITCH, WEATHER, YREND)

# -----------------------------------------------------------------------
#      Call Operations Management module to determine today's
#      applications of irrigation, tillage, etc.
# -----------------------------------------------------------------------
#       CALL MGMTOPS(CONTROL, ISWITCH,
#      &    FLOODWAT, HARVRES, SOILPROP, ST,                !Input
#      &    STGDOY, SW, WEATHER,                            !Input
#      &    YREND, FERTDATA, HARVFRAC, IRRAMT,              !Output
#      &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output

# -----------------------------------------------------------------------
#      Call Soil processes module to determine today's rates of
#      change of soil properties.
# -----------------------------------------------------------------------
        (NH4_plant, NO3_plant, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, SomLitC,
             SomLitE, SW, SWDELTS, SWDELTU, UPPM, WINF,
             YREND) = (SOIL(CONTROL, ISWITCH, ES, FERTDATA, FracRts, HARVRES,
                           IRRAMT, KTRANS, KUptake, OMAData, PUptake, RLV,
                           SENESCE, ST, SWDELTX, TILLVALS, UNH4, UNO3, WEATHER,
                           XHLAI, FLOODN, FLOODWAT, MULCH, UPFLOW))

#-----------------------------------------------------------------------
#     Call Soil-plant-atmosphere module to determine today's
#     rates of evapotranspiration.
#-----------------------------------------------------------------------
        (EO,EOP,EOS,EP,ES,RWU,SRFTEMP,ST,SWDELTX,TRWU,TRWUP,UPFLOW) = (
            SPAM(CONTROL, ISWITCH, CANHT, EORATIO, KSEVAP, KTRANS, MULCH, PSTRES1,
                PORMIN, RLV, RWUMX, SOILPROP, SW, SWDELTS, UH2O, WEATHER, WINF,
                XHLAI, XLAI, FLOODWAT, SWDELTU))

# -----------------------------------------------------------------------
#      Call PLANT Subroutine to calculate crop growth and
#      development rates.
#      Skip plant growth and development routines for fallow runs
# -----------------------------------------------------------------------
        (CANHT, EORATIO, HARVRES, KSEVAP, KTRANS, KUptake, MDATE, NSTRES, PSTRES1,
            PUptake, PORMIN, RLV, RWUMX, SENESCE, STGDOY, FracRts, UH2O, UNH4, UNO3,
            XHLAI, XLAI) = (PLANT(CONTROL, ISWITCH, EO, EOP, EOS, EP, ES,
                                FLOODWAT, HARVFRAC, IRRAMT, NH4_plant, NO3_plant, SKi_Avail,
                                SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW, TRWUP,
                                WEATHER, YREND, YRPLT, FLOODN))

# ***********************************************************************
#      DAILY INTEGRATION
# ***********************************************************************
#      ELSE IF (DYNAMIC .EQ. INTEGR) THEN
# ***********************************************************************
#      Integrate soil state variables
# -----------------------------------------------------------------------
        (NH4_plant, NO3_plant, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, SomLitC,
             SomLitE, SW, SWDELTS, SWDELTU, UPPM, WINF,
             YREND) = (SOIL(CONTROL, ISWITCH, ES, FERTDATA, FracRts, HARVRES,
                           IRRAMT, KTRANS, KUptake, OMAData, PUptake, RLV,
                           SENESCE, ST, SWDELTX, TILLVALS, UNH4, UNO3, WEATHER,
                           XHLAI, FLOODN, FLOODWAT, MULCH, UPFLOW))

# -----------------------------------------------------------------------
#      Compute cumulative totals for soil-plant-atmosphere processes
# -----------------------------------------------------------------------
        (EO,EOP,EOS,EP,ES,RWU,SRFTEMP,ST,SWDELTX,TRWU,TRWUP,UPFLOW) = (
            SPAM(CONTROL, ISWITCH, CANHT, EORATIO, KSEVAP, KTRANS, MULCH, PSTRES1,
                PORMIN, RLV, RWUMX, SOILPROP, SW, SWDELTS, UH2O, WEATHER, WINF,
                XHLAI, XLAI, FLOODWAT, SWDELTU))
# -----------------------------------------------------------------------
#      Call Plant module to integrate daily plant processes and update
#      plant state variables.
# -----------------------------------------------------------------------
        if CROP != ' FA' and YRDOY >= YRPLT and YRPLT  != -99:
            pass

        (CANHT, EORATIO, HARVRES, KSEVAP, KTRANS, KUptake, MDATE, NSTRES, PSTRES1,
        PUptake, PORMIN, RLV, RWUMX, SENESCE, STGDOY, FracRts, UH2O, UNH4, UNO3,
        XHLAI, XLAI) = (PLANT(CONTROL, ISWITCH, EO, EOP, EOS, EP, ES,
                            FLOODWAT, HARVFRAC, IRRAMT, NH4_plant, NO3_plant, SKi_Avail,
                            SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW, TRWUP,
                            WEATHER, YREND, YRPLT, FLOODN))

# -----------------------------------------------------------------------
#      Call Operations Management module to check for harvest end,
#      accumulate variables.
# -----------------------------------------------------------------------
#       CALL MGMTOPS(CONTROL, ISWITCH,
#      &    FLOODWAT, HARVRES, SOILPROP, ST,                !Input
#      &    STGDOY, SW, WEATHER,                            !Input
#      &    YREND, FERTDATA, HARVFRAC, IRRAMT,              !Output
#      &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output

# ***********************************************************************
# ***********************************************************************
#      Daily Output
# ***********************************************************************
    elif DYNAMIC == RC.OUTPUT:
        WEATHR(CONTROL, ISWITCH, WEATHER, YREND)
     #

        (NH4_plant, NO3_plant, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, SomLitC,
            SomLitE, SW, SWDELTS, SWDELTU, UPPM, WINF, YREND) = (SOIL(CONTROL, ISWITCH,
            ES, FERTDATA, FracRts, HARVRES, IRRAMT, KTRANS,
            KUptake, OMAData, PUptake, RLV, SENESCE, ST, SWDELTX, TILLVALS, UNH4,
            UNO3, WEATHER, XHLAI, FLOODN, FLOODWAT, MULCH, UPFLOW))

        (EO,EOP,EOS,EP,ES,RWU,SRFTEMP,ST,SWDELTX,TRWU,TRWUP,UPFLOW) = (
            SPAM(CONTROL, ISWITCH, CANHT, EORATIO, KSEVAP, KTRANS, MULCH, PSTRES1,
                PORMIN, RLV, RWUMX, SOILPROP, SW, SWDELTS, UH2O, WEATHER, WINF,
                XHLAI, XLAI, FLOODWAT, SWDELTU))

# -----------------------------------------------------------------------
#      Call plant module for daily printout.
# -----------------------------------------------------------------------
        if CROP != 'FA':
            pass
            (CANHT, EORATIO, HARVRES, KSEVAP, KTRANS, KUptake, MDATE, NSTRES, PSTRES1,
                PUptake, PORMIN, RLV, RWUMX, SENESCE, STGDOY, FracRts, UH2O, UNH4, UNO3,
                XHLAI, XLAI) = (PLANT(CONTROL, ISWITCH, EO, EOP, EOS, EP, ES,
                                FLOODWAT, HARVFRAC, IRRAMT, NH4_plant, NO3_plant, SKi_Avail,
                                SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW, TRWUP,
                                WEATHER, YREND, YRPLT, FLOODN))
     #
     #    CALL MGMTOPS(CONTROL, ISWITCH,
     # &    FLOODWAT, HARVRES, SOILPROP, ST,                !Input
     # &    STGDOY, SW, WEATHER,                            !Input
     # &    YREND, FERTDATA, HARVFRAC, IRRAMT,              !Output
     # &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output

# ***********************************************************************
# ***********************************************************************
#      Seasonal Output
# ***********************************************************************
    elif DYNAMIC  == RC.SEASEND:
        WEATHR(CONTROL, ISWITCH, WEATHER, YREND)

#     Print seasonal summaries and close files.
        (NH4_plant, NO3_plant, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, SomLitC,
            SomLitE, SW, SWDELTS, SWDELTU, UPPM, WINF,
            YREND) = (SOIL(CONTROL, ISWITCH, ES, FERTDATA, FracRts, HARVRES,
                     IRRAMT, KTRANS, KUptake, OMAData, PUptake, RLV,
                     SENESCE, ST, SWDELTX, TILLVALS, UNH4, UNO3, WEATHER,
                     XHLAI, FLOODN, FLOODWAT, MULCH, UPFLOW))
#
        (EO,EOP,EOS,EP,ES,RWU,SRFTEMP,ST,SWDELTX,TRWU,TRWUP,UPFLOW) = (
            SPAM(CONTROL, ISWITCH, CANHT, EORATIO, KSEVAP, KTRANS, MULCH, PSTRES1,
                PORMIN, RLV, RWUMX, SOILPROP, SW, SWDELTS, UH2O, WEATHER, WINF,
                XHLAI, XLAI, FLOODWAT, SWDELTU))

        (CANHT, EORATIO, HARVRES, KSEVAP, KTRANS, KUptake, MDATE, NSTRES, PSTRES1,
                PUptake, PORMIN, RLV, RWUMX, SENESCE, STGDOY, FracRts, UH2O, UNH4, UNO3,
                XHLAI, XLAI) = PLANT(CONTROL, ISWITCH, EO, EOP, EOS, EP, ES,
                                    FLOODWAT, HARVFRAC, IRRAMT, NH4_plant, NO3_plant, SKi_Avail,
                                    SPi_AVAIL, SNOW, SOILPROP, SRFTEMP, ST, SW, TRWUP,
                                    WEATHER, YREND, YRPLT, FLOODN)

#     Call management operations module for seasonal printout.
#       CALL MGMTOPS(CONTROL, ISWITCH,
#      &    FLOODWAT, HARVRES, SOILPROP, ST,                !Input
#      &    STGDOY, SW, WEATHER,                            !Input
#      &    YREND, FERTDATA, HARVFRAC, IRRAMT,              !Output
#      &    MDATE, OMADATA, TILLVALS, YRPLT)                !Output
#
# -----------------------------------------------------------------------
#      Seasonal Output
#      Call end of season and summary output subroutines
# -----------------------------------------------------------------------
#      CALL OPSUM (CONTROL, ISWITCH, YRPLT)

        if CONTROL.ERRCODE > 0:
            MSG[0] = "End of run " + CONTROL.RUN
            MSG[1] = "Simulation ended with error code " + CONTROL.ERRCODE
            WARNING(2,'ENDRUN',MSG)
            #CALL INFO(2,'ENDRUN',MSG)
        else:
            MSG[0] = "Normal end of run " + CONTROL.RUN
            WARNING(2, 'ENDRUN', MSG)
            #CALL INFO(1,'ENDRUN',MSG)

        if SOILPROP.NLAYR > maxnlayers:
            maxnlayers = SOILPROP.NLAYR
# ***********************************************************************
# ***********************************************************************
#      End of Run
# ***********************************************************************
    elif DYNAMIC == RC.ENDRUN:
        (NH4_plant, NO3_plant, SKi_AVAIL, SNOW, SPi_AVAIL, SOILPROP, SomLitC,
             SomLitE, SW, SWDELTS, SWDELTU, UPPM, WINF,
             YREND) = (SOIL(CONTROL, ISWITCH, ES, FERTDATA, FracRts, HARVRES,
                           IRRAMT, KTRANS, KUptake, OMAData, PUptake, RLV,
                           SENESCE, ST, SWDELTX, TILLVALS, UNH4, UNO3, WEATHER,
                           XHLAI, FLOODN, FLOODWAT, MULCH, UPFLOW))

     #   IF (ISWITCH % FMOPT == 'C') THEN
     #      CALL CsvOutputs(CONTROL % MODEL(1:5), CONTROL % N_ELEMS,
     # & maxnlayers)
     #    END IF

# ***********************************************************************
# ***********************************************************************
#      END OF DYNAMIC IF CONSTRUCT
# ***********************************************************************
#       ENDIF
#       RETURN
#       END SUBROUTINE LAND
    return YRPLT, MDATE, YREND
