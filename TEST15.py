#=======================================================================
#  PHOTIP, Subroutine, N.B. Pickering
#  Read input parameters for daily photosynthesis.
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#  Output:
#  Local :
#-----------------------------------------------------------------------
#  Called: PG
#  Calls : None
#=======================================================================
#
def PHOTIP(FILEIO, CCEFF, CCMAX, CCMP, FNPGN, FNPGT, LMXSTD, LNREF,
           PARMAX, PGREF, PHTHRS10, PHTMAX, ROWSPC, TYPPGN, TYPPGT,
           XPGSLW, YPGSLW):
    from ERROR import ERROR
    from READS import FIND, IGNORE
    import fortranformat as ff

#
# !-----------------------------------------------------------------------
#       IMPLICIT NONE
#       EXTERNAL GETLUN, FIND, ERROR, IGNORE
#
#       CHARACTER*1  BLANK
#       CHARACTER*3  TYPPGN, TYPPGT
#       CHARACTER*6  ERRKEY, SECTION
#       CHARACTER*12 FILEC
#       CHARACTER*30 FILEIO
#       CHARACTER*80 PATHCR,CHAR
#       CHARACTER*92 FILECC
#
#       INTEGER LUNIO, LINC, LNUM, FOUND
#       INTEGER II, PATHL, LUNCRP, ERR, ISECT
#
#       REAL CCEFF, CCMAX, CCMP, LMXSTD, LNREF,
#      &  PARMAX, PGREF, PHTHRS10, PHTMAX, ROWSPC, XPGSLW(15)
#
#       REAL FNPGN(4),FNPGT(4)
#       REAL YPGSLW(15)
#
    BLANK  = ' '
    ERRKEY = 'PHOTIP'
#
# !-----------------------------------------------------------------------
# !     Open and read FILEIO
# !-----------------------------------------------------------------------
    try:
        LUNIO = open(FILEIO, 'r')
    except OSError as ERR:
        ERROR(ERRKEY, ERR.errno, FILEIO, 0)

#
# !-----------------------------------------------------------------------
    LNUM = 7
    try:
        for _ in range(6):
            next(LUNIO)
        line = LUNIO.readline()
        FILEC  = line[15:27].strip()
        PATHCR = line[28:108].strip()
    except OSError as ERR:
        ERROR(ERRKEY, ERR.errno, FILEIO, LNUM)
#
# !-----------------------------------------------------------------------
# C    Read Planting Details Section
# !-----------------------------------------------------------------------
    SECTION = '*PLANT'
    LINC, FOUND = FIND(LUNIO, SECTION)
    LNUM = LNUM + LINC
    f10 = ff.FortranRecordReader('42X,F6.0')
    if FOUND == 0:
        ERROR(SECTION, 42, FILEIO, LNUM)
    else:
        # ROWSPC = READ(LUNIO, '(42X,F6.0)', IOSTAT=ERR)
        ROWSPC = f10.read(LUNIO[LNUM])
        LNUM = LNUM + 1
        if ERR != 0:
            ERROR(ERRKEY, ERR, FILEIO, LNUM)

#
# C     CROPGRO uses ROWSPC as m
    ROWSPC = ROWSPC / 100.0
#
# !-----------------------------------------------------------------------
# C    Read Cultivars Section
# !-----------------------------------------------------------------------
    SECTION = '*CULTI'
    FIND(LUNIO, SECTION, LINC, FOUND)
    LNUM = LNUM + LINC

    if FOUND == 0:
        ERROR(SECTION, 42, FILEIO, LNUM)
    else:
        #PHTHRS10, LMXSTD = READ(LUNIO, '(60X,F6.0,6X,F6.0)', IOSTAT=ERR)
        f25 = ff.FortranRecordReader('(60X,F6.0,6X,F6.0)')
        PHTHRS10, LMXSTD = f25.read(LUNIO[LNUM])
        LNUM = LNUM + 1
        if ERR != 0:
            ERROR(ERRKEY, ERR, FILEIO, LNUM)
#
# C-----------------------------------------------------------------------
    LUNIO.close()
#
# C-----------------------------------------------------------------------
    LNUM = 0
    PATHL = PATHCR.find(BLANK) + 1
    if PATHL <= 1:
        FILECC = FILEC
    else:
        FILECC = PATHCR[:PATHL-1] + FILEC

    GETLUN('FILEC', LUNCRP)
    LUNCRP = open(FILECC, 'r')
    if ERR != 0:
        ERROR(ERRKEY, ERR, FILECC, 0)
#
# C-----------------------------------------------------------------------
# C READ PHOTOSYNTHESIS PARAMETERS *******************
# C-----------------------------------------------------------------------
# 5 CONTINUE
    ISECT, CHAR = IGNORE(LUNCRP, LNUM)
    if ISECT == 0:
        ERROR(ERRKEY, ERR, FILECC, LNUM)
    if ISECT == 2:
        goto 5
    f20 = ff.FortranRecordReader('(5F6.2)')
    PARMAX, PHTMAX = f20.read(CHAR[LNUM])
    #PARMAX, PHTMAX = READ(CHAR, '(5F6.2)', IOSTAT=ERR)
    if ERR != 0:
        ERROR(ERRKEY, ERR, FILECC, LNUM)

    ISECT, CHAR = IGNORE(LUNCRP, LNUM)
    f30 = ff.FortranRecordReader('(3F6.1)')
    CCMP, CCMAX, CCEFF = f30.read(CHAR[LNUM])
    #CCMP, CCMAX, CCEFF = READ(CHAR, '(3F6.1)', IOSTAT=ERR)
    if ERR != 0:
        ERROR(ERRKEY, ERR, FILECC, LNUM)

    ISECT, CHAR = IGNORE(LUNCRP, LNUM)
    #FNPGN[1], FNPGN[2], FNPGN[3], FNPGN[4], TYPPGN = READ(CHAR, '(4F6.0,3X,A3)', IOSTAT=ERR)
    f40 = ff.FortranRecordReader('(4F6.0,3X,A3)')
    FNPGN[1], FNPGN[2], FNPGN[3], FNPGN[4], TYPPGN = f40.read(CHAR[LNUM])
    if ERR != 0:
        ERROR(ERRKEY, ERR, FILECC, LNUM)

    ISECT, CHAR = IGNORE(LUNCRP, LNUM)
    #FNPGT[1], FNPGT[2], FNPGT[3], FNPGT[4], TYPPGT = READ(CHAR, '(4F6.0,3X,A3)', IOSTAT=ERR)
    f50 = ff.FortranRecordReader('(4F6.0,3X,A3)')
    FNPGN[1], FNPGN[2], FNPGN[3], FNPGN[4], TYPPGN = f50.read(CHAR[LNUM])
    if ERR != 0:
        ERROR(ERRKEY, ERR, FILECC, LNUM)

    ISECT, CHAR = IGNORE(LUNCRP, LNUM)
    ISECT, CHAR = IGNORE(LUNCRP, LNUM)
    ISECT, CHAR = IGNORE(LUNCRP, LNUM)
    ISECT, CHAR = IGNORE(LUNCRP, LNUM)
    ISECT, CHAR = IGNORE(LUNCRP, LNUM)
    #LNREF, PGREF = READ(CHAR, '(18X,2F6.0)', IOSTAT=ERR)
    f60 = ff.FortranRecordReader('(18X,2F6.0)')
    LNREF, PGRE = f60.read(CHAR[LNUM])
    if ERR != 0:
        ERROR(ERRKEY, ERR, FILECC, LNUM)

    ISECT, CHAR = IGNORE(LUNCRP, LNUM)
    #XPGSLW[1], XPGSLW[2], XPGSLW[3], XPGSLW[4], XPGSLW[5], XPGSLW[6], XPGSLW[7], XPGSLW[8], XPGSLW[9], XPGSLW[10] = READ(CHAR, '(10F6.0)', IOSTAT=ERR)
    f70 = ff.FortranRecordReader('(10F6.0)')
    XPGSLW[1], XPGSLW[2], XPGSLW[3], XPGSLW[4], XPGSLW[5], XPGSLW[6], XPGSLW[7], XPGSLW[8], XPGSLW[9], XPGSLW[10] = f70.read(CHAR[LNUM])
    if ERR != 0:
        ERROR(ERRKEY, ERR, FILECC, LNUM)

    ISECT, CHAR = IGNORE(LUNCRP, LNUM)
    #YPGSLW[1], YPGSLW[2], YPGSLW[3], YPGSLW[4], YPGSLW[5], YPGSLW[6], YPGSLW[7], YPGSLW[8], YPGSLW[9], YPGSLW[10] = READ(CHAR, '(10F6.0)', IOSTAT=ERR)
    f80 = ff.FortranRecordReader('(10F6.0)')
    YPGSLW[1], YPGSLW[2], YPGSLW[3], YPGSLW[4], YPGSLW[5], YPGSLW[6], YPGSLW[7], YPGSLW[8], YPGSLW[9], YPGSLW[10] = f80.read(CHAR[LNUM])
    if ERR != 0:
        ERROR(ERRKEY, ERR, FILECC, LNUM)

#
#-----------------------------------------------------------------------
    LUNCRP.close()
#
#     END !SUBROUTINE PHOTIP
#=======================================================================
# Variable definitions for PHOTO and PHOTIP
#=======================================================================
# AGEFAC   Relative effect of current leaf N on canopy photosynthesis (0-1)
#            (fraction)
# AGEFCC   Effect of AGEFAC on photosynthesis
# AGEREF   Reference value calculated at reference leaf N value to
#            normalize the effect of N for different plant species
# BETN     Spacing between plants along a row (m / plant)
# BLANK    Blank character
# CCEFF    Relative efficiency of CO2 assimilation used in equation to
#            adjust canopy photosynthesis with CO2 concentrations
# CCK      Computed exponent for relationship between CO2 and canopy
#            photosynthesis (=CCEFF / CCMAX)
# CCMAX    Maximum daily canopy photosynthesis relative to photosynthesis
#            at a CO2 concentration of 330 vpm
# CCMP     Canopy CO2 compensation point (CO2 at which daily PG is 0.0)
# CHAR     Contains the contents of last record read
# CO2      Atmospheric carbon dioxide concentration (µmol[CO2] / mol[air])
# COLDSTR  Cold weather stress factor for photosynthesis (not currently
#            used)
# CUMSTR   Cumulative stress factor for photosynthesis after start of seed
#            production
# CURV     Function subroutine
# DAS      Days after start of simulation (d)
# DXR57    Relative time between first seed (NR5) and physiological
#            maturity (NR7)
# ERR      Error code for file operation
# ERRKEY   Subroutine name for error file
# EXCESS   Factor based on excess PG used to affect tomorrow's PG
#            calculation
# FILEC    Filename for SPE file (e.g., SBGRO980.SPE)
# FILECC   Path plus filename for species file (*.spe)
# FILEIO   Filename for input file (e.g., IBSNAT35.INP)
# FNPGN(I) Critical leaf N concentration for function to reduce
#            photosynthesis due to low leaf N levels (4 values for function
#            CURV)
# FNPGT(I) Critical values of temperature for the functions to reduce
#            canopy PG under non-optimal temperatures (in function CURV)
# FOUND    Indicator that good data was read from file by subroutine FIND
#            (0 - End-of-file encountered, 1 - NAME was found)
# ISECT    Indicator of completion of IGNORE routine: 0 - End of file
#            encountered, 1 - Found a good line to read, 2 - End of Section
#            in file encountered denoted by * in column 1.
# KCAN     Canopy light extinction coefficient for daily PAR, for
#            equidistant plant spacing, modified when in-row and between
#            row spacing are not equal
# KCANR    Canopy light extinction coefficient, reduced for incomplete
#            canopy
# LMXSTD   Maximum leaf photosyntheses for standard cultivar
# LNREF    Value of leaf N above which canopy PG is maximum (for standard
#            cultivar)
# LNUM     Current line number of input file
# LUNCRP   Logical unit number for FILEC (*.spe file)
# LUNIO    Logical unit number for FILEIO
# NR5      Day when 50% of plants have pods with beginning seeds (days)
# PAR      Daily photosynthetically active radiation or photon flux density
#            (moles[quanta]/m2-d)
# PARMAX   Value of PAR at which photosynthesis is 63% of the maximum value
#            (PHTMAX). Used in daily canopy photosynthesis calculations
#            (moles[quanta]/m2-d)
# PATHCR   Pathname for SPE file or FILEE.
# PATHL    Number of characters in path name (path plus filename for FILEC)
#
# PG       Daily gross photosynthesis (g[CH2O] / m2 / d)
# PGFAC    Multiplier to compute daily canoy PG as a function of leaf area
#            index (LAI)
# PGLFMX   Multiplier for daily canopy photosynthesis to account for
#            cultivar differences in leaf photosynthesis capabilities
# PGREF    Reference value for leaf level photosynthesis used in canopy
#            light response curve (µmol[CO2] / m2-s)
# PGSLW    Relative effect of leaf thickness (SLW) on daily canopy PG
# PHTHRS10 Threshold time that must accumulate in phase 10 for the next
#            stage to occur.  Equivalent to PHTHRS(10) in Subroutine
#            PHENOLOG.
# PHTMAX   Maximum amount of CH20 which can be produced if
#            photosynthetically active radiation (PAR) is very high (3
#            times PARMAX) and all other factors are optimal (g[CH2O]/m2-d)
# PRATIO   Relative effect of CO2 on canopy PG, for multiplying by PG
#            computed for 330 vpm
# PTSMAX   Potential amount of CH20 which can be produced at the specified
#            PAR for full canopy (LAI>8), all other factors optimal
#            (g[CH2O]/m2-d)
# RNITP    True nitrogen concentration in leaf tissue for photosynthesis
#            reduction. (%)
# ROWSPC   Row spacing (m)
# SECTION  Section name in input file
# SLAAD    Specific leaf area, excluding weight of C stored in leaves
#            (cm2[leaf] / g[leaf])
# SLPF     Empirical multiplier to adjust daily canopy PG due to unknown
#            soil or environmental factors that persist in a given location
# SLW      Specific leaf weight (g[leaf] / m2[leaf])
# SPACNG   Ratio of distance between plants in a row to distance between
#            rows (or vice versa - always < 1)
# SWFAC    Effect of soil-water stress on photosynthesis, 1.0=no stress,
#            0.0=max stress
# TABEX    Function subroutine - Lookup utility
# TDAY     Average temperature during daylight hours (°C)
# TIMDIF   Integer function which calculates the number of days between two
#            Julian dates (da)
# TPGFAC   Reduction in specific leaf area due to daytime temperature being
#            less than optimal (0-1)
# TYPPGN   Type of function for the leaf N effects on PG
# TYPPGT   Character variable specifying the type of function to use for
#            the relationship between temperature and PG (for use in
#            function subroutine CURV)
# XHLAI    Leaf area index (m2[leaf] / m2[ground])
# XPGSLW(I) Array of SLW values for table look-up function, used with YPGSLW
#            (g[leaf] / m2[leaf])
# XPOD     Growth partitioning to pods which slows node appearance
#            (fraction)
# YPGSLW(I) Array of PG values corresponding to SLW values in array XPGSLW
#            (g[CH2O] / m2 / d)
#=======================================================================
# END SUBROUTINE PHOTO
#=======================================================================
