#=======================================================================
#  PODDET, Subroutine
#-----------------------------------------------------------------------
#  Computes pod detachment rates.
#-----------------------------------------------------------------------
#  Called from:  PLANT
#  Calls:        ERROR, FIND, IGNORE
#=======================================================================
#
def PODDET(FILECC, TGRO, WTLF, YRDOY, YRNR2, DYNAMIC):
    import math
    from ModuleDefs import RunConstants as RC,TS,NCOHORTS
    from ERROR import ERROR
    from READS import FIND, IGNORE
    import numpy as np
    from DSSATUtils import curv
#-----------------------------------------------------------------------
#       USE ModuleDefs     !Definitions of constructed variable types,
#                          ! which contain control information, soil
#                          ! parameters, hourly weather data.
#       IMPLICIT NONE
#       EXTERNAL GETLUN, FIND, ERROR, IGNORE, CURV
#       SAVE
#
#       CHARACTER*6 ERRKEY
    ERRKEY = 'PODDET'
#
#       CHARACTER*6 SECTION
#       CHARACTER*80 C80
#       CHARACTER*92 FILECC
#
#       INTEGER LUNCRP, ERR, LINC, LNUM, FOUND, ISECT, I
#       INTEGER DYNAMIC, YRDOY
#       INTEGER YRNR2, NPP
#
#       REAL SWIDOT,WSHIDT,WTLF
#       REAL XPD,PODWTD,DWC
#       REAL TPODM,RLMPM,SL10
#       REAL FT
#       REAL FTHR,CURV
#       REAL PR1DET,PR2DET,XP1DET,XP2DET
    PR1DET : float = -99.0
    PR2DET : float = -99.0
    DWC : float = -99.0
    XP1DET : float = -99.0
    XP2DET : float = -99.0
    PODWTD : float = -99.0
    SWIDOT : float = -99.0
    WSHIDT : float = -99.0

#       REAL  SDDAM,SHDAM,SUMSD,SUMSH
#
    TB     = np.zeros(5)
    TO1    = np.zeros(5)
    TO2    = np.zeros(5)
    TM     = np.zeros(5)
    TDLM   = np.zeros(20)
    WTSD   = np.zeros(NCOHORTS)
    SDNO   = np.zeros(NCOHORTS)
    WTSHE  = np.zeros(NCOHORTS)
    WPODY  = np.zeros(NCOHORTS)
    SHELN  = np.zeros(NCOHORTS)
    PDET   = np.zeros(NCOHORTS)
    DAYS   = np.zeros(NCOHORTS)
    MSHELN = np.zeros(NCOHORTS)
    DTC    = np.zeros(NCOHORTS)
#
# ***********************************************************************
# ***********************************************************************
#      Run Initialization - Called once per simulation
# ***********************************************************************
    if DYNAMIC == RC.RUNINIT:
# -----------------------------------------------------------------------
#      Read in values from input file, which were previously input
#        in Subroutine IPCROP.
# -----------------------------------------------------------------------
        LUNCRP = GETLUN('FILEC')
        try:
            FILE_HANDLE = open(FILECC, 'r')
        except IOError as ERR:
            ERROR(ERRKEY, ERR.errno, FILECC, 0)
        LNUM = 0
#-----------------------------------------------------------------------
#    Find and Read Pod Loss Section
#-----------------------------------------------------------------------
#     Subroutine FIND finds appropriate SECTION in a file by
#     searching for the specified 6-character string at beginning
#     of each line.
#-----------------------------------------------------------------------
        SECTION = '!*POD '
        LINC, FOUND = FIND(LUNCRP, SECTION)
        LNUM += LINC
        if FOUND == 0:
            ERROR(SECTION, 42, FILECC, LNUM)
        else:
            ISECT, C80 = IGNORE(LUNCRP, LNUM)
            try:
                line = C80.readline()
                DWC    = float(line[ 6:12])
                PR1DET = float(line[12:18])
                PR2DET = float(line[18:24])
                XP1DET = float(line[24:30])
                XP2DET = float(line[30:36])
            except Exception as ERR:
                FILECC, LNUM = ERROR(ERRKEY, ERR)
#-----------------------------------------------------------------------
#    Find and Read Phenology Section
#-----------------------------------------------------------------------
        SECTION = '!*PHEN'
        LINC, FOUND = FIND(LUNCRP, SECTION)
        LNUM += LINC
        if FOUND == 0:
            ERROR(SECTION, 42, FILECC, LNUM)
        else:
            ISECT, C80 = IGNORE(LUNCRP, LNUM)
            try:
                LINE = C80.readline()
                TB[1]  = float(LINE[0:6])
                TO1[1] = float(LINE[6:12])
                TO2[1] = float(LINE[12:18])
                TM[1]  = float(LINE[18:24])
            except Exception as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)
            ISECT, C80 = IGNORE(LUNCRP, LNUM)
            try:
                LINE = C80.readline()
                TB[2]  = float(LINE[0:6])
                TO1[2] = float(LINE[6:12])
                TO2[2] = float(LINE[12:18])
                TM[2]  = float(LINE[18:24])
            except Exception as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)
            ISECT, C80 = IGNORE(LUNCRP, LNUM)
            try:
                LINE = C80.readline()
                TB[3]  = float(LINE[0:6])
                TO1[3] = float(LINE[6:12])
                TO2[3] = float(LINE[12:18])
                TM[3]  = float(LINE[18:24])
            except Exception as ERR:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

        LUNCRP.close()
        WSHIDT = 0.0
        WTSD   = 0.0
        WTSHE  = 0.0
#
#***********************************************************************
#***********************************************************************
#     EMERGENCE CALCULATIONS - Performed once per season upon emergence
#         or transplanting of plants
#***********************************************************************
    elif DYNAMIC == RC.EMERG:
#-----------------------------------------------------------------------
        for I in range(1, NCOHORTS+1):
            DTC[I]    = 0.0
            MSHELN[I] = 0.0
            WPODY[I]  = 0.0
            DAYS[I]   = 0.0

        PODWTD = 0.0
#***********************************************************************
#***********************************************************************
#     DAILY RATE/INTEGRATION
#***********************************************************************
    elif DYNAMIC == RC.INTEGR:
#-----------------------------------------------------------------------
#     Compute thermal time using hourly predicted air temperature
#     based on observed max and min temperature.
#--------------------------------------------------------------------
        FT = 0.0
        for I in range(1, TS+1):
            FTHR = curv('LIN', TB[3], TO1[3], TO2[3], TM[3], TGRO[I])
            FT = FT + FTHR/TS
#     24 changed to TS on 5 July 2017 by Bruce Kimball
#-------------------------------------------------------------------
# Compute ratio of leaf area per pod cm2/pod
# and leaf mass per pod mass g/g
#-------------------------------------------------------------------
        TPODM = 0.0
        RLMPM = 1.0
#-------------------------------------------------------------------
#     Compute 10 day running average of leaf mass and PGAVL
#-------------------------------------------------------------------
        for I in range(10, 1, -1):
            TDLM[I] = TDLM[I-1]
        TDLM[1] = WTLF
#-------------------------------------------------------------------
#    Compute slope of leaf mass curve
#-------------------------------------------------------------------
        SL10 = (TDLM[1] - TDLM[10]) / 10.0
#--------------------------------------------------------------------
        if YRNR2 >= 0:
#---------------------------------------------------------------------
            for NPP in range(1, YRDOY - YRNR2 + 1):
                TPODM = TPODM + WTSHE[NPP] + WTSD[NPP]
            if TPODM > 10.0:
                RLMPM = WTLF / TPODM
#-------------------------------------------------------------------
#     Main loop that cycles through detachment model
#--------------------------------------------------------------------
        for NPP in range(1, YRDOY - YRNR2 + 1):
#--------------------------------------------------------------------
#     Determine maximum cohort shell mass and accumulate
#     days without carbohydrate on a cohort basis
#--------------------------------------------------------------------
            if SHELN[NPP] > MSHELN[NPP]:
                MSHELN[NPP] = SHELN[NPP]
            if WTSD[NPP] + WTSHE[NPP] >= 0.01:
                if (WTSD[NPP] + WTSHE[NPP] <= WPODY[NPP] and WTSD[NPP] > 0.0):
                    DAYS[NPP] = DAYS[NPP] + 1.0
                if WTSD[NPP] + WTSHE[NPP] > WPODY[NPP]:
                    DAYS[NPP] = 0.0
#
#-----------------------------------------------------------------------
#     Accumulate pod detachment thermal time counter (DTC) based on
#     ratio of LFM/PDM and 10 day average slope of the leaf mass curve
#-----------------------------------------------------------------------
#           IF(RLMPM .GT. PR1DET .OR. SL10 .GT. PR2DET) GOTO 700
            if RLMPM <= PR1DET and SL10 <= PR2DET:
                if SL10 <= PR2DET or DAYS[NPP] > DWC or WTLF <= 10.0:
                    DTC[NPP] = DTC[NPP] + FT
            else:
#           Accumulate DTC based on days without carbon before RLMPM < PR1DET
#           and SL10 < PR2DET
                if DAYS[NPP] > DWC or WTLF <= 10.0:
                        DTC[NPP] = DTC[NPP] + FT
#-----------------------------------------------------------------------
#         ENDIF
#1000   ENDDO
#--------------------------------------------------------------------
#     Compute detachment for each cohort
#--------------------------------------------------------------------
        # DO 2000 NPP = 1, YRDOY - YRNR2
        for NPP in range(1, YRDOY - YRNR2 + 1):
            if DTC[NPP] > 0.0:
                XPD = MSHELN[NPP] * (1.0 - XP1DET * math.exp(XP2DET * DTC[NPP]) / 100.0)
                XPD = max(0.0, XPD)
                if SHELN[NPP] > XPD:
                    if SHELN[NPP] >= 0.01 and DTC[NPP] <= 34.0:
                        PDET[NPP] = SHELN[NPP] - XPD
                        PDET[NPP] = max(0.0, PDET[NPP])
                        PODWTD += (WTSHE[NPP] + WTSD[NPP]) * PDET[NPP] / SHELN[NPP]
                        SDDAM = WTSD[NPP] * PDET[NPP] / SHELN[NPP]
                        SWIDOT += WTSD[NPP] if SDDAM > WTSD[NPP] else SDDAM
                        SHDAM = WTSHE[NPP] * PDET[NPP] / SHELN[NPP]
                        WSHIDT += WTSHE[NPP] if SHDAM > WTSHE[NPP] else SHDAM
                        WTSD[NPP]  *= (1.0 - PDET[NPP] / SHELN[NPP])
                        SDNO[NPP]  *= (1.0 - PDET[NPP] / SHELN[NPP])
                        WTSHE[NPP] *= (1.0 - PDET[NPP] / SHELN[NPP])
                        SHELN[NPP] *= (1.0 - PDET[NPP] / SHELN[NPP])
                        WTSHE[NPP] = max(0.0, WTSHE[NPP])
                        SHELN[NPP] = max(0.0, SHELN[NPP])
                        WTSD[NPP]  = max(0.0, WTSD[NPP])
                        SDNO[NPP]  = max(0.0, SDNO[NPP])
            WPODY[NPP] = WTSD[NPP] + WTSHE[NPP]
        # 2000 ENDDO
        SUMSD = 0.0
        SUMSH = 0.0
        for NPP in range(1, YRDOY - YRNR2 + 1):
            SUMSD += WTSD[NPP]
            SUMSH += WTSHE[NPP]
#***********************************************************************
#***********************************************************************
#     END OF DYNAMIC IF CONSTRUCT
#***********************************************************************
#***********************************************************************
    return PODWTD, SDNO, SHELN, SWIDOT, WSHIDT, WTSD, WTSHE
#=======================================================================
#***********************************************************************
#       Variable definitions
#***********************************************************************
# CURV      Function subroutine
# DAYS(J)   Days without carbohydrate on a cohort basis (days)
# DTC       Pod detachment thermal time counter
# DWC       Threshold number of days without carbon to trigger pod
#             detachment (days)
# ERRKEY    Subroutine name for error file
# FILECC    Path plus filename for species file (*.spe)
# FT        Temperature function (0-1)
# FTHR      Used to calculate hourly air temperature (°C)
# LUNCRP    Logical unit number for FILEC (*.spe file)
# MSHELN(J) Maximum cohort shell mass (#/m2)
# NPP       Cohort number used as index in loops
# PDET(J)   # of detached pods by cohort (# / m2)
# PODWTD    Mass of detached pods (g[pods] / m2[ground])
# PR1DET    Threshold for comparison of ratio of leaf mass to pod mass
#             (RLMPM)
# PR2DET    Threshold for comparison of slope of leaf mass (for
#             defoliation)
# RLMPM     Ratio of leaf mass to pod mass
# SDDAM     Mass of seeds destroyed by detachment (g/m2)
# SDNO(J)   Number of seeds for cohort J (#/m2)
# SHDAM     Mass of shells destroyed by detachment (g/m2)
# SHELN(J)  Number of shells for cohort J (#/m2)
# SL10      Slope of leaf mass curve
# SUMSD     Total seed mass (g/m2)
# SUMSH     Total shell mass (g/m2)
# SWIDOT    Daily seed mass damage (g/m2/day)
# TB,       |
# TO1,      | Coefficients which define daily temperature distribution:
# TO2,      | TB=base temp, T01=1st optimum, T02=2nd optimum, TM=max temp. (°C)
# TM        |
# TDLM      Last 10 days values of leaf mass (g[leaf] / m2[ground])
# TGRO(I)   Hourly air temperature (°C)
# TPODM     Total pod mass (g/m2)
# TS        Number of intermediate time steps (=24)
# WPODY(J)  Pod mass  for cohort J (g/m2)
# WSHIDT    Weight of shell tissue consumed by pests today (g[shell]/m2-d)
# WTLF      Dry mass of leaf tissue including C and N
#             (g[leaf] / m2[ground])
# WTSD(J)   Seed mass  for cohort J (g/m2)
# WTSHE(J)  Shell mass  for cohort J (g/m2)
# XP1DET    Coefficient which defines pod detachment equation
# XP2DET    Coefficient which defines pod detachment equation
# XPD       Number of shells which can be supported by plant (?) (#/m2)
# YRDOY     Current day of simulation (YYDDD)
# YRNR2     Day when 50% of plants have one peg (peanuts only) (YYDDD)
#***********************************************************************
