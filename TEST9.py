# C=======================================================================
# C  OPSPAM, Subroutine, C.H.Porter from Soil Water portions of OPDAY
# C  Generates output for daily soil water data
# C-----------------------------------------------------------------------
# C  REVISION       HISTORY
# C  11/16/2001 CHP Written
# C  06/07/2002 GH  Modified for crop rotations
# C  08/20/2002 GH  Modified for Y2K
# C  02/04/2005 CHP Added new variables to Summary.out: EPCM, ESCM
# !  08/11/2009 CHP Added potential root water uptake to output
# C-----------------------------------------------------------------------
# C  Called from:   SPAM
# C  Calls:         None
# C=======================================================================
import os
from DATES import YR_DOY
from OPHEAD import HEADER
from OPSUM import SUMVALS
import numpy as np
from ModuleDefs import GET, RunConstants as RC,NL
def OPSPAM(CONTROL, ISWITCH, FLOODWAT, TRWU,
           CEF, CEM, CEO, CEP, CES, CET, CEVAP, EF, EM,
           EO, EOP, EOS, EP, ES, ET, TMAX, TMIN, TRWUP,
           SRAD, ES_LYR, SOILPROP):
#
# !-----------------------------------------------------------------------
#       USE ModuleDefs
#       USE ModuleData
#       USE FloodModule
# !     VSH
#       USE CsvOutput
#       USE Linklist
#
#       IMPLICIT NONE
#       EXTERNAL GETLUN, HEADER, YR_DOY, SUMVALS
#       SAVE
#
#       CHARACTER*1  IDETW, ISWWAT, RNMODE
#       CHARACTER*8  OUTET
#       CHARACTER*56 FMT
#
#       INTEGER DAS, DOY, DYNAMIC, FROP, LUN
#       INTEGER NAVWB, RUN, YEAR, YRDOY, L
#       INTEGER REPNO, N_LYR
#
#       REAL EF, EM, EO, EP, ES, ET, EOS, EOP, TRWU !JZW add TRWU
#       REAL REFET, KCB, KE, KC
#       REAL CEF, CEM, CEO, CEP, CES, CET, CEVAP
#       REAL ESAA, EMAA, EPAA, ETAA, EFAA, EOAA, EOPA, EOSA
#       REAL REFA, KCAA, KBSA, KEAA
#       REAL AVTMX, AVTMN, AVSRAD, AVRWUP, AVRWU
#       REAL TMAX, TMIN, SRAD, TRWUP
# !     REAL SALB, SWALB, MSALB, CMSALB
#       REAL  ES10
#       LOGICAL FEXIST
#
# !     Arrays which contain data for printing in SUMMARY.OUT file
    SUMNUM = 3
    LABEL  = np.full(SUMNUM, '', dtype='U4')
    VALUE  = np.zeros(SUMNUM, dtype=float)
#
#       CHARACTER*20 FRMT  ! VSH
#
# !-----------------------------------------------------------------------
# !     Define constructed variable types based on definitions in
# !     ModuleDefs.for.
#       TYPE (ControlType) CONTROL
#       TYPE (SwitchType) ISWITCH
#       TYPE (FloodWatType) FLOODWAT
#       TYPE (SoilType) SOILPROP
#
    ISWWAT = ISWITCH.ISWWAT
    if ISWWAT == 'N':
        return

    DAS     = CONTROL.DAS
    DYNAMIC = CONTROL.DYNAMIC
    FROP    = CONTROL.FROP
    REPNO   = CONTROL.REPNO
    RUN     = CONTROL.RUN
    RNMODE  = CONTROL.RNMODE
    YRDOY   = CONTROL.YRDOY

    IDETW = ISWITCH.IDETW
    FMOPT = ISWITCH.FMOPT   # VSH

    CEF = FLOODWAT.CEF

    # SALB   = SOILPROP.SALB
    # SWALB  = SOILPROP.SWALB
    # MSALB  = SOILPROP.MSALB
    # CMSALB = SOILPROP.CMSALB

    REFET = GET('SPAM', 'REFET')
    KCB   = GET('SPAM', 'KCB')
    KE    = GET('SPAM', 'KE')
    KC    = GET('SPAM', 'KC')

#
# !***********************************************************************
# !***********************************************************************
# !     Seasonal initialization - run once per season
# !***********************************************************************
    if DYNAMIC == RC.SEASINIT:
# !-----------------------------------------------------------------------
        if FMOPT in ('A', ' '):
            OUTET = 'ET.OUT'
            LUN   = GETLUN('OUTET')

            FEXIST = os.path.isfile(OUTET)
            if FEXIST:
                LUN = open(OUTET, 'a')
            else:
                LUN = open(OUTET, 'w')
                LUN.write("*SOIL-PLANT-ATMOSPHERE MODULE OUTPUT FILE\n")   # VSH
#
# !       Number of soil layers to print between 4 and 10.
        N_LYR = min(10, max(4, SOILPROP.NLAYR))

#
# C-----------------------------------------------------------------------
# C     Variable heading for ET.OUT
# C-----------------------------------------------------------------------
        if RNMODE != 'Q' or RUN == 1:
            if FMOPT in ('A', ' '):  # VSH
                # For sequenced run, use replicate
                # number instead of run number in header
                if RNMODE == 'Q':
                    HEADER(RC.SEASINIT, LUN, REPNO)
                else:
                    HEADER(RC.SEASINIT, LUN, RUN) #! VSH
#
            if ISWITCH.MESEV == 'S':
# !               Include soil evap by soil layer for Suleiman-Ritchie method
#
                if FMOPT in ('A', ' '):   #! VSH
                    LUN.write('!' + ' ' * 185 +'Soil evaporation (mm/d) by soil depth (cm):\n')
                    LUN.write('!' + ' ' * 180 +''.join(f'{SOILPROP.LayerText(L):>8}'for L in range(1, N_LYR + 1)) +'\n')
#
                    LUN.write('@YEAR DOY   DAS   SRAA  TMAXA  TMINA'
                          '    REFA    EOAA    EOPA    EOSA    ETAA    EPAA'
                          '    ESAA    EFAA    EMAA    EOAC    ETAC    EPAC'
                          '    ESAC    EFAC    EMAC    KCAA    KBSA    KEAA')
#
                    if N_LYR < 10:
# !                      VSH
# !                      WRITE (LUN,121) ("ES",L,"D",L=1,N_LYR), "   TRWU" ! ADD by JZW
# !  121                 FORMAT(9("    ",A2,I1,A1), A8)
                        FRMT   = f'({N_LYR}(4X,A2,I1,A1),A)'
                        header = ''.join(f'    ES{L}D' for L in range(1, N_LYR + 1))  # L = 1,N_LYR
                        LUN.write(f'{header}   TRWUD   TWUPD\n')
                    else:
# !                     WRITE (LUN,122)("ES",L,"D",L=1,9, "        ES10D    RWUD")
                        header = ''.join(f'    ES{L}D' for L in range(1, 10))  # L = 1,9
                        LUN.write(f'{header}  ES10D   TRWUD   TWUPD\n')
#                 ENDIF
#                 END IF   ! VSH
            else:
                if FMOPT in ('A', ' '):
                    LUN.write('@YEAR DOY   DAS   SRAA  TMAXA  TMINA'
                    '    REFA    EOAA    EOPA    EOSA    ETAA    EPAA'
                    '    ESAA    EFAA    EMAA    EOAC    ETAC    EPAC'
                    '    ESAC    EFAC    EMAC    KCAA    KBSA    KEAA')
#           ENDIF
#         ENDIF
#
# C-----------------------------------------------------------------------
# C   Set initial values to calculate average values
# C-----------------------------------------------------------------------
        NAVWB  = 0
        EFAA   = 0.0
        EMAA   = 0.0
        EPAA   = 0.0
        ESAA   = 0.0
        ETAA   = 0.0
        EOAA   = 0.0
        EOPA   = 0.0
        EOSA   = 0.0
        AVTMX  = 0.0
        AVTMN  = 0.0
        AVSRAD = 0.0
        KEAA   = 0.0
        KBSA   = 0.0
        KCAA   = 0.0
        REFA   = 0.0
        AVRWUP = 0.0
        AVRWU  = 0.0
#
# !***********************************************************************
# !***********************************************************************
# !     Daily integration
# !***********************************************************************
    elif DYNAMIC == RC.INTEGR:
# C-----------------------------------------------------------------------
# C   Summations for calculation of average values per print interval
# C-----------------------------------------------------------------------
        NAVWB = NAVWB + 1
        EFAA = EFAA + EF
        EMAA = EMAA + EM
        EPAA = EPAA + EP
        ESAA = ESAA + ES
        ETAA = ETAA + ET
        EOAA = EOAA + EO
        EOPA = EOPA + EOP
        EOSA = EOSA + EOS
        AVTMX = AVTMX + TMAX
        AVTMN = AVTMN + TMIN
        AVSRAD = AVSRAD + SRAD
        KEAA = KEAA + (ES + EM + EF) / REFET
        KBSA = KBSA+(EP / REFET)
        KCAA = KCAA+(ET / REFET)
        REFA = REFA + REFET
        AVRWUP = AVRWUP + TRWUP  # cm
        AVRWU = AVRWU + TRWU  # cm

#
# !***********************************************************************
# !***********************************************************************
# !     Daily Output
# !***********************************************************************
    elif DYNAMIC == RC.OUTPUT or DYNAMIC == RC.SEASEND:
# C-----------------------------------------------------------------------
        if IDETW == 'Y':
            if ((DYNAMIC == RC.OUTPUT and DAS % FROP == 0) or
                (DYNAMIC == RC.SEASEND and DAS % FROP != 0) or
                DAS == 1):
#
# C         Calculate average values as a function of the output interval
# C-----------------------------------------------------------------------
                if NAVWB < 1:NAVWB = 1

                EFAA   = EFAA/NAVWB
                EMAA   = EMAA/NAVWB
                EPAA   = EPAA   / NAVWB
                ESAA   = ESAA   / NAVWB
                ETAA   = ETAA   / NAVWB
                EOAA   = EOAA   / NAVWB
                EOPA   = EOPA   / NAVWB
                EOSA   = EOSA   / NAVWB
                AVTMX  = AVTMX  / NAVWB
                AVTMN  = AVTMN  / NAVWB
                AVSRAD = AVSRAD / NAVWB
                KEAA   = KEAA   / NAVWB
                KBSA   = KBSA   / NAVWB
                KCAA   = KCAA   / NAVWB
                REFA   = REFA   / NAVWB
                AVRWUP = AVRWUP / NAVWB
                AVRWU  = AVRWU  / NAVWB

#
                YEAR, DOY = YR_DOY(YRDOY)
#
                if FMOPT in ('A', ' '):   # VSH
                    # !Daily printout
                    FMT = "(1X,I4,1X,I3.3,1X,I5,3(1X,F6.2),9(F8.3),"
                    if (CEO > 1000.0 or CET > 1000.0 or CEP > 1000.0 or
                        CES > 1000.0 or CEF > 1000.0 or CEM > 1000.0):
                        FMT = FMT.strip() + "6(F8.0),"
                    else:
                        FMT = FMT.strip() + "6(F8.2),"
                    FMT = FMT.strip() + "3(F8.3))"

                    if REFA < 0.0:
                        KCAA = -99.0
                        KBSA = -99.0
                        KEAA = -99.0

#
                    LUN.write(FMT.format(
                        YEAR, DOY, DAS,
                        AVSRAD, AVTMX, AVTMN,
                        REFA, EOAA, EOPA, EOSA, ETAA, EPAA,
                        ESAA, EFAA, EMAA,
                        CEO, CET, CEP, CES, CEF, CEM,
                        KCAA, KBSA, KEAA
                    ))

#
                    if ISWITCH.MESEV == 'S':
                        if SOILPROP.NLAYR < 11:
                            LUN.write(
                                ''.join(f'{ES_LYR[L]:8.3f}' for L in range(1, SOILPROP.NLAYR + 1))
                                + f'{AVRWU:8.3f}{AVRWUP:8.3f}'
                            )
                        else:
                            ES10 = 0.0
                            for L in range(10, SOILPROP.NLAYR + 1):
                                ES10 += ES_LYR[L]
                            LUN.write(
                                ''.join(f'{ES_LYR[L]:8.3f}' for L in range(1, 10))
                                + f'{ES10:8.3f}{AVRWU:8.3f}{AVRWUP:8.3f}'
                            )
                    else:
                        LUN.write(" ")# VSH
#
# !             VSH CSV output corresponding to ET.OUT
                if FMOPT == 'C':
# !               N_LYR = MIN(10, MAX(4,SOILPROP%NLAYR))
                    N_LYR = SOILPROP.NLAYR
                    CsvOutET(
                        EXPNAME,
                        CONTROL.RUN, CONTROL.TRTNUM, CONTROL.ROTNUM, CONTROL.REPNO,
                        YEAR, DOY, DAS,
                        AVSRAD, AVTMX, AVTMN,
                        REFA, EOAA, EOPA, EOSA, ETAA, EPAA,
                        ESAA, EFAA, EMAA,
                        CEO, CET, CEP, CES, CEF, CEM, KCAA, KBSA, KEAA,
                        N_LYR, ES_LYR, AVRWU, AVRWUP,
                        vCsvlineET, vpCsvlineET, vlngthET
                    )
                    LinklstET(vCsvlineET)

#           ENDIF
#
                NAVWB  = 0
                EFAA   = 0.0
                EMAA   = 0.0
                EPAA   = 0.0
                ESAA   = 0.0
                ETAA   = 0.0
                EOAA   = 0.0
                EOPA   = 0.0
                EOSA   = 0.0
                AVTMX  = 0.0
                AVTMN  = 0.0
                AVSRAD = 0.0
                KEAA   = 0.0
                KBSA   = 0.0
                KCAA   = 0.0
                REFA   = 0.0
                AVRWUP = 0.0
                AVRWU  = 0.0
#
#         ENDIF
#       ENDIF
#
# !***********************************************************************
# !***********************************************************************
# !     Seasonal Output
# !***********************************************************************
            if DYNAMIC == RC.SEASEND:
# C-----------------------------------------------------------------------
#           !IF (IDETS .EQ. 'Y' .OR. IDETS .EQ. 'A') THEN
# !           Store Summary.out labels and values in arrays to send to
# !           OPSUM routines for printing.  Integers are temporarily
# !           saved aS real numbers for placement in real array.
                LABEL[1] = 'ETCM'
                VALUE[1] = CET
                LABEL[2] = 'EPCM'
                VALUE[2] = CEP
                LABEL[3] = 'ESCM'
                VALUE[3] = CEVAP

                # Send labels and values to OPSUM
                SUMVALS(SUMNUM, LABEL, VALUE)

                # Close daily output files.
                LUN.close()
#         ENDIF
#
# !***********************************************************************
# !***********************************************************************
# !     END OF DYNAMIC IF CONSTRUCT
# !***********************************************************************
#       ENDIF
# !***********************************************************************
#       RETURN
#       END !SUBROUTINE OPSPAM
# !***********************************************************************
# !-----------------------------------------------------------------------
# !     OPSPAM VARIABLE DEFINITIONS:
# !-----------------------------------------------------------------------
# ! EOAA    Average potential evapotranspiration since last printout (mm/d)
# ! EPAA    Average plant transpiration since last printout (mm/d)
# ! ESAA    Average soil evaporation since last printout (mm/d)
# ! EMAA    Average mulch evaporation since last printout (mm/d)
# ! ETAA    Average evapotranspiration since last printout (mm/d)
# ! AVSRAD  Average solar radiation since last printout (MJ/m2-d)
# ! AVTMN   Average min temperature since last printout (oC)
# ! AVTMX   Average max temperature since last printout (oC)
# ! AVWTD   Average water table depth since last printout (cm)
# ! CEP     Cumulative transpiration (mm)
# ! CES     Cumulative evaporation (mm)
# ! CET     Cumulative evapotranspiration (mm)
# ! CRAIN   Cumulative precipitation (mm)
# ! EM      Actual mulch evaporation rate (mm/d)
# ! EO      Potential evapotranspiration rate (mm/d)
# ! EP      Actual plant transpiration rate (mm/d)
# ! ES      Actual soil evaporation rate (mm/d)
# ! ET      Actual evapotranspiration rate (mm/d)
# ! KBSA    FAO-56 basal crop coefficient * stress coefficient (Kcb*Ks)
# ! KCAA    FAO-56 single crop coefficient (Kc)
# ! KEAA    FAO-56 evaporation coefficient (Ke)
# ! MODEL   Name of CROPGRO executable file
# ! NAP     Number of irrigation applications
# ! NAVWB   Number of days since last printout (d)
# ! NL      Maximum number of soil layers = 20
# ! LUN     Unit number for spam output file
# ! OUTW    Filename for soil water output file (set in IPIBS)
# ! REFA    Actual reference evapotranspiration (ETo or ETr)
# ! ST(L)   Soil temperature in soil layer L (oC)
# ! SW(L)   Volumetric soil water content in layer L
# !           (cm3 [water] / cm3 [soil])
# ! TDRAIN  Cumulative daily drainage from profile (mm)
# ! TIMDIF  Integer function which calculates the number of days between two
# !           Julian dates (da)
# ! TMAX    Maximum daily temperature (oC)
# ! TMIN    Minimum daily temperature (oC)
# ! TOTIR   Total seasonal irrigation (mm)
# ! TRUNOF  Cumulative runoff (mm)
# ! WTDEP   Water table depth  (cm)
# ! YRDOY   Current day of simulation (YYDDD)
# ! YRPLT   Planting date (YYDDD)
# !-----------------------------------------------------------------------
# !     END OPSPAM Subroutine
# !-----------------------------------------------------------------------
#
#
# C=======================================================================
# C  XTRACT, Subroutine, J.T. Ritchie
# C  Calculates root extraction for each soil layer.
# C-----------------------------------------------------------------------
# C  REVISION HISTORY
# C  01/01/89 JR  Written
# C  12/05/93 NBP Made into subroutine
# C  07/11/96 GH  Set TRWU and RWU to 0 if EP = 0
# !  10/13/97 CHP Modified for modular format.
# !  07/20/2011 chp added option for root uptake from plant routines
# !-----------------------------------------------------------------------
# !  Called by: SPAM
# !  Calls:     None
# C=======================================================================
def XTRACT(NLAYR, DLAYR, LL, SW, SW_AVAIL, TRWUP, UH2O, EP, RWU, SWDELTX, TRWU):

#
# !     ------------------------------------------------------------------
#       USE ModuleDefs
#
#       IMPLICIT NONE
#       SAVE
#       INTEGER NLAYR
#       INTEGER L
#
#       REAL EP, TRWU, TRWUP, Tot_plant_RWU
#       REAL WUF
#       REAL DLAYR(NL), LL(NL), SW(NL)
#       REAL RWU(NL)
#       REAL  SWDELTX(NL),    !Change in SW due to root extraction
#      &      SWTEMP(NL),     !New SW value based only on root extraction
#      &      SW_AVAIL(NL),   !Water available for root extraction
#      &      UH2O(NL)        !Root water uptake from plant routine (opt.)
    SWTEMP   = np.zeros(NL, dtype=float)
# !-----------------------------------------------------------------------
    for L in range(1, NLAYR+1):
        SWDELTX[L]   = 0.0
        SWTEMP[L]    = SW[L]
        SW_AVAIL[L]  = max(0.0, SW_AVAIL[L] - LL[L])

#       ENDDO
#
# !     Check to see if actual transpiration already done by plant routine
    Tot_plant_RWU = UH2O.sum()
    if Tot_plant_RWU > 1e-6:
# !       Use root water uptake from plant routines
        TRWU = 0.0
        for L in range(1, NLAYR+1):
            RWU[L] = UH2O[L] / 10.0
            SWTEMP[L] = SWTEMP[L] - RWU[L] / DLAYR[L]
            TRWU += RWU[L]

#
    else:
# !       Calculate root water uptake here
        if EP > 0.0:
            if 0.1 * EP <= TRWUP:
                WUF = 0.1 * EP / TRWUP
            else:
                WUF = 1.0
            TRWU = 0.0
            for L in range(1, NLAYR + 1):
                if SWTEMP[L] > LL[L]:
                    RWU[L] = RWU[L] * WUF
                    if RWU[L] / DLAYR[L] > SW_AVAIL[L]:
                        RWU[L] = SW_AVAIL[L] * DLAYR[L]
                    SWTEMP[L] = SWTEMP[L] - RWU[L] / DLAYR[L]
                    TRWU += RWU[L]

#
        else:        #No root extraction of soil water
            TRWU = 0.0
            RWU  = 0.0

        EP = TRWU * 10.0
        for L in range(1, NLAYR + 1):
            SWDELTX[L] = SWTEMP[L] - SW[L]

    return EP, RWU, SWDELTX, TRWU
#       END SUBROUTINE XTRACT
#
# !-----------------------------------------------------------------------
# !     XTRACT VARIABLE DEFINITIONS:
# !-----------------------------------------------------------------------
# ! DLAYR(L)    Soil thickness in layer L (cm)
# ! EP          Actual plant transpiration rate (mm/d)
# ! LL(L)       Volumetric soil water content in soil layer L at lower limit
# !               (cm3/cm3)
# ! NL          Maximum number of soil layers = 20
# ! NLAYR       Actual number of soil layers
# ! RWU(L)      Root water uptake from soil layer L (cm/d)
# ! SW(L)       Volumetric soil water content in layer L
# !               (cm3 [water] / cm3 [soil])
# ! SW_AVAIL(L) Soil water content in layer L available for evaporation,
# !               plant extraction, or movement through soil
# !               (cm3 [water] / cm3 [soil])
# ! SWDELTX (L) Change in soil water content due to root uptake in layer L
# !               (cm3/cm3)
# ! SWTEMP(L)   Soil water content in layer L (temporary value to be modified
# !               based on drainage, root uptake and upward flow through soil
# !               layers). (cm3/cm3)
# ! TRWU        Total potential daily root water uptake (cm/d)
# ! WUF         Root water uptake reduction factor
# !-----------------------------------------------------------------------
# !     END SUBROUTINE XTRACT
# C=======================================================================
# !     END SPAM MODULE
# !=======================================================================
#
