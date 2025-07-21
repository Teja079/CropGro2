# =======================================================================
#   PHOTO, Subroutine, K.J.Boote, J.W.Jones, G. Hoogenboom
#   Compute daily photosynthetic using canopy (C) method.
# -----------------------------------------------------------------------
#   Called from:   Main
#   Calls:         PHOTIP
# =======================================================================
from ModuleDefs import *
import math
from DSSATUtils import curv as CURV

def PHOTO(CONTROL,
     BETN, CO2, DXR57, EXCESS, KCAN, KC_SLOPE,       #Input
     NR5, PAR, PStres1, SLPF, RNITP, SLAAD,          #Input
     SWFAC, TDAY, XHLAI, XPOD,                       #Input
     AGEFAC, PG):                                    #Output

    #IMPLICIT NONE
    #SAVE
    #
    TYPPGN, TYPPGT = "  "*2
    FILEIO = " "*30
    #
    DYNAMIC = 0
    DAS, NR5 = 0
    #
    AGEFAC, AGEFCC, AGEREF, A0, BETN, CCEFF, CCK, CCMAX = [0.0]*8
    CCMP, CO2, COLDSTR, CUMSTR, CURV, DXR57, EXCESS = [0.0]*8
    KCAN, KCANR, KC_SLOPE, LMXSTD, LNREF, PAR, PARMAX, PG, PGFAC, = [0.0]*8
    SLPF, PGLFMX, PGREF, PGSLW, PHTHRS10, PHTMAX, PRATIO = [0.0]*8
    PTSMAX, RNITP, ROWSPC, SLAAD, SLW, SPACNG, SWFAC = [0.0]*8
    TABEX, TDAY, TPGFAC, XHLAI, XPOD = [0.0]*8
    E_FAC = [0.0]*8
    #
    FNPGN = [0.0]*4
    FNPGT = [0.0]*4
    XPGSLW = [0.0]*15
    YPGSLW = [0.0]*15
    #
    #Added with P module
    PStres1 = 0.0
    #------------------------------------------------------------------
    #Define constructed variable types based on definitions in
    #ModuleDefs.for.
    #
    #The variable "CONTROL" is of type "ControlType".
    CONTROL = CONTROL
    DAS = CONTROL.DAS
    DYNAMIC = CONTROL.DYNAMIC
    FILEIO = CONTROL.workdir #CONTROL.FILEIO

    # C***********************************************************************
    # C***********************************************************************
    # C     Run Initialization - Called once per simulation
    # C***********************************************************************
    if DYNAMIC == RUNINIT:
        PHOTIP(FILEIO,CCEFF, CCMAX, CCMP, FNPGN, FNPGT, LMXSTD, LNREF, PARMAX,
               PGREF, PHTHRS10, PHTMAX, ROWSPC, TYPPGN, TYPPGT, XPGSLW, YPGSLW)
    #-----------------------------------------------------------------------
    #     Adjust canopy photosynthesis for GENETIC input value of
    #     maximum leaf photosyntheses (LMXSTD).  Exponential curve from
    #     from the hedgerow photosynthesis model (Boote et al, 199?).
    #-----------------------------------------------------------------------
        if PGREF > 0.0:
            PGLFMX = (1.0 - math.exp(-1.6 * LMXSTD)) / (1.0 - math.exp(-1.6 * PGREF))
        else:
            PGLFMX = 1.0

    #***********************************************************************
    #***********************************************************************
    #     Seasonal initialization - run once per season
    #***********************************************************************
    elif DYNAMIC == SEASINIT:
    #-----------------------------------------------------------------------
        AGEFAC  = 1.0
        CUMSTR  = 0.0
        COLDSTR = 0.0
        PG      = 0.0

    #***********************************************************************
    #***********************************************************************
    #     Daily rate calculations
    #***********************************************************************
    elif DYNAMIC == RATE:
    #-----------------------------------------------------------------------
    #     Calculate maximum photosynthesis as function of PAR, g CH2O/m2
    #-----------------------------------------------------------------------
        PTSMAX = PHTMAX * (1.0 - math.exp(-(1.0 / PARMAX) * PAR))

    #-----------------------------------------------------------------------
    #     Calculate reduction in photosynthesis due to incomplete canopy.
    #-----------------------------------------------------------------------
        if BETN <= ROWSPC:
            SPACNG = BETN / ROWSPC
        else:
            SPACNG = ROWSPC / BETN

    #chp per CDM:      KCANR = KCAN - (1. - SPACNG) * 0.1
        KCANR = KCAN - (1.0 - SPACNG) * KC_SLOPE
        PGFAC = 1.0 - math.exp(-KCANR * XHLAI)

    #-----------------------------------------------------------------------
    #     Compute reduction in PG based on the average daylight temperature.
    #-----------------------------------------------------------------------
        TPGFAC = CURV(TYPPGT, FNPGT[0], FNPGT[1], FNPGT[2], FNPGT[3], TDAY)

    #-----------------------------------------------------------------------
    #     Compute reduction in PG as a function of leaf N concentration.
    #-----------------------------------------------------------------------
        AGEFAC = CURV(TYPPGN, FNPGN[0], FNPGN[1], FNPGN[2], FNPGN[3], RNITP)
        AGEREF = CURV(TYPPGN, FNPGN[0], FNPGN[1], FNPGN[2], FNPGN[3], LNREF)
        AGEFAC = AGEFAC / AGEREF

    #-----------------------------------------------------------------------
    #     9/24/95 KJB,JWJ Decrease sensitivity of canopy PG to N function
    #     to mimic behavior of leaf version.  AGEFAC corresponds to leaf
    #     PG vs. leaf N. Function does not act strongly on QE, thus, we
    #     need to scale effect back on daily canopy version.
    #-----------------------------------------------------------------------
        AGEFCC = (1.0 - math.exp(-2.0 * AGEFAC)) / (1.0 - math.exp(-2.0 * 1.0))

    #-----------------------------------------------------------------------
    #     Compute canopy pg response to changes in specific leaf weight.
    #     (Dornhoff and Shibles, 197?).
    #-----------------------------------------------------------------------
        if SLAAD > 0.0:
            SLW = 1.0 / SLAAD
        else:
            SLW = 0.0099

        PGSLW = TABEX(YPGSLW, XPGSLW, SLW, 10)

    #-----------------------------------------------------------------------
    #     Adjust canopy photosynthesis for CO2 concentration assuming a
    #     reference value of CO2 of 330 ppmv.
    #-----------------------------------------------------------------------
        CCK = CCEFF / CCMAX
        A0 = -CCMAX * (1.0 - math.exp(-CCK * CCMP))
        PRATIO = A0 + CCMAX * (1.0 - math.exp(-CCK * CO2))

    #***********************************************************************
    #***********************************************************************
    #     Daily integration
    #***********************************************************************
    #-----------------------------------------------------------------------
    #     Effect of daylength on daily gross PG, computed by KJB with
    #     stand alone model, and used by Ernie Piper in his dissertation
    #     see page 127 for the equation and conditions.  Normalized to
    #     about 13 hours where the function is 1.00.  Actually
    #     normalized to a bit higher between 13 and 14 hours.
    #
    #     DLFAC = 1.0 + 0.6128 - 0.01786*DAYL + 0.006875*DAYL*DAYL
    #    &        - 0.000247*DAYL*DAYL*DAYL
    #
    #     Compute daily gross photosynthesis (g CH2O/m2/d)
    #-----------------------------------------------------------------------
    #     PG =  PTSMAX * SLPF * PGFAC * TPGFAC * AGEFCC * PGSLW

    #     PG =  PTSMAX * SLPF * PGFAC * TPGFAC * MIN(AGEFCC, PSTRES2) *
    #    &            PGSLW * PRATIO * PGLFMX * SWFAC

    #     CHP 05/07/2004
    #     AGEFCC can be > 1.0, so don't want to use minimum of
    #     PStres1 and AGEFCC.  (PStres1 is always 1.0 or below).
        if AGEFCC >= 1.0:
            E_FAC = AGEFCC * PStres1
        else:
            E_FAC = min(AGEFCC, PStres1)

        PG = PTSMAX * SLPF * PGFAC * TPGFAC * E_FAC * PGSLW * PRATIO * PGLFMX * SWFAC

    #From WDB (chp 10/21/03):
    #        PG = PG * MIN(SWFAC ,2*(1-SATFAC) )
    #        PGN = PGN * MIN(SWFAC,2*(1-SATFAC) )

    #-----------------------------------------------------------------------
    #     9/27/95 KJB added cumulative water stress effect on PG after R5.
    #     CUMULATIVE STRESS (WATER, TEMP) UPON PG CAPACITY.  REDUCE FROM POT.
    #     PG AFTER R5, WITH TWO FUNCTIONS.  ONE DEPENDING ON THE PRIMARY
    #     STRESS AND THE OTHER DEPENDING ON DISTANCE FROM R5 TO R7, DXR57
    #     INITIALLY THE STRESS IS SOIL WATER, BUT THIS MAY WORK FOR COLD TEMP.
    #     0.5 IS A SCALAR, POSSIBLY COULD GO HIGHER.  MAX CUMSTR IS 0.2856
    #     FOR 78-RF, 0.1858 FOR EGLIN-88, THEN 0.528 FOR 84-RF.  DECREASES
    #     YIELD 77, 52, 50, AND 16 kg/ha FOR 78RF, EGLIN-88, 84RF, AND 81REP
    #     MINOR DECREASE IN SEED SIZE.
    #     1/19/96, USING XPOD CAUSES IT TO BE LESS EFFECTIVE EARLY UNTIL FULL
    #     PARTITIONING TO POD OCCURS (MAYBE JUST SEED, SEE XPOD CALCULATION).
    #     12/19/95 Changed scalar to 0.4.  Too strong for peanut.  Also,
    #     2/6/96 Changed scalar to 0.3.  Too strong for 78RF too. Also,
    #     the problem is really with seed growth potential, not as much on PG.
    #-----------------------------------------------------------------------
    #     NEEDS A FUNCTION.  SEE TMIN AND CHILL IN LEAF SUBROUTINE
    #     AND THEN ADD A SCALAR?
    #       COLDSTR =  COLDSTR + DXR57 * (F(TMIN?)*XPOD / PHTHRS(10)
    #       PG = PG * (1.0 - MAX(0.4*CUMSTR,1.0*COLDSTR))
    #-----------------------------------------------------------------------
        if DAS > NR5:
            CUMSTR = CUMSTR + DXR57 * (1.0 - SWFAC) * XPOD / PHTHRS10
            COLDSTR = 0.0
            PG = PG * (1.0 - 0.3 * CUMSTR)
        else:
            CUMSTR = 0.0
            COLDSTR = 0.0

        PG = PG * EXCESS

    #***********************************************************************
    #***********************************************************************
    #     END OF DYNAMIC IF CONSTRUCT
    #***********************************************************************

# C=======================================================================
# C  PHOTIP, Subroutine, N.B. Pickering
# C  Read input parameters for daily photosynthesis.
# C-----------------------------------------------------------------------
# C  Output:
# C  Local :
# !-----------------------------------------------------------------------
# !  Called: PG
# !  Calls : None
# C=======================================================================
def PHOTIP(FILEIO,
           CCEFF, CCMAX, CCMP, FNPGN, FNPGT, LMXSTD, LNREF, PARMAX,
           PGREF, PHTHRS10, PHTMAX, ROWSPC, TYPPGN, TYPPGT, XPGSLW, YPGSLW):
    return


# !=======================================================================
# ! Variable definitions for PHOTO and PHOTIP
# !=======================================================================
# ! AGEFAC   Relative effect of current leaf N on canopy photosynthesis (0-1)
# !            (fraction)
# ! AGEFCC   Effect of AGEFAC on photosynthesis
# ! AGEREF   Reference value calculated at reference leaf N value to
# !            normalize the effect of N for different plant species
# ! BETN     Spacing between plants along a row (m / plant)
# ! BLANK    Blank character
# ! CCEFF    Relative efficiency of CO2 assimilation used in equation to
# !            adjust canopy photosynthesis with CO2 concentrations
# ! CCK      Computed exponent for relationship between CO2 and canopy
# !            photosynthesis (=CCEFF / CCMAX)
# ! CCMAX    Maximum daily canopy photosynthesis relative to photosynthesis
# !            at a CO2 concentration of 330 vpm
# ! CCMP     Canopy CO2 compensation point (CO2 at which daily PG is 0.0)
# ! CHAR     Contains the contents of last record read
# ! CO2      Atmospheric carbon dioxide concentration (µmol[CO2] / mol[air])
# ! COLDSTR  Cold weather stress factor for photosynthesis (not currently
# !            used)
# ! CUMSTR   Cumulative stress factor for photosynthesis after start of seed
# !            production
# ! CURV     Function subroutine
# ! DAS      Days after start of simulation (d)
# ! DXR57    Relative time between first seed (NR5) and physiological
# !            maturity (NR7)
# ! ERR      Error code for file operation
# ! ERRKEY   Subroutine name for error file
# ! EXCESS   Factor based on excess PG used to affect tomorrow's PG
# !            calculation
# ! FILEC    Filename for SPE file (e.g., SBGRO980.SPE)
# ! FILECC   Path plus filename for species file (*.spe)
# ! FILEIO   Filename for input file (e.g., IBSNAT35.INP)
# ! FNPGN(I) Critical leaf N concentration for function to reduce
# !            photosynthesis due to low leaf N levels (4 values for function
# !            CURV)
# ! FNPGT(I) Critical values of temperature for the functions to reduce
# !            canopy PG under non-optimal temperatures (in function CURV)
# ! FOUND    Indicator that good data was read from file by subroutine FIND
# !            (0 - End-of-file encountered, 1 - NAME was found)
# ! ISECT    Indicator of completion of IGNORE routine: 0 - End of file
# !            encountered, 1 - Found a good line to read, 2 - End of Section
# !            in file encountered denoted by * in column 1.
# ! KCAN     Canopy light extinction coefficient for daily PAR, for
# !            equidistant plant spacing, modified when in-row and between
# !            row spacing are not equal
# ! KCANR    Canopy light extinction coefficient, reduced for incomplete
# !            canopy
# ! LMXSTD   Maximum leaf photosyntheses for standard cultivar
# ! LNREF    Value of leaf N above which canopy PG is maximum (for standard
# !            cultivar)
# ! LNUM     Current line number of input file
# ! LUNCRP   Logical unit number for FILEC (*.spe file)
# ! LUNIO    Logical unit number for FILEIO
# ! NR5      Day when 50% of plants have pods with beginning seeds (days)
# ! PAR      Daily photosynthetically active radiation or photon flux density
# !            (moles[quanta]/m2-d)
# ! PARMAX   Value of PAR at which photosynthesis is 63% of the maximum value
# !            (PHTMAX). Used in daily canopy photosynthesis calculations
# !            (moles[quanta]/m2-d)
# ! PATHCR   Pathname for SPE file or FILEE.
# ! PATHL    Number of characters in path name (path plus filename for FILEC)
# !
# ! PG       Daily gross photosynthesis (g[CH2O] / m2 / d)
# ! PGFAC    Multiplier to compute daily canoy PG as a function of leaf area
# !            index (LAI)
# ! PGLFMX   Multiplier for daily canopy photosynthesis to account for
# !            cultivar differences in leaf photosynthesis capabilities
# ! PGREF    Reference value for leaf level photosynthesis used in canopy
# !            light response curve (µmol[CO2] / m2-s)
# ! PGSLW    Relative effect of leaf thickness (SLW) on daily canopy PG
# ! PHTHRS10 Threshold time that must accumulate in phase 10 for the next
# !            stage to occur.  Equivalent to PHTHRS(10) in Subroutine
# !            PHENOLOG.
# ! PHTMAX   Maximum amount of CH20 which can be produced if
# !            photosynthetically active radiation (PAR) is very high (3
# !            times PARMAX) and all other factors are optimal (g[CH2O]/m2-d)
# ! PRATIO   Relative effect of CO2 on canopy PG, for multiplying by PG
# !            computed for 330 vpm
# ! PTSMAX   Potential amount of CH20 which can be produced at the specified
# !            PAR for full canopy (LAI>8), all other factors optimal
# !            (g[CH2O]/m2-d)
# ! RNITP    True nitrogen concentration in leaf tissue for photosynthesis
# !            reduction. (%)
# ! ROWSPC   Row spacing (m)
# ! SECTION  Section name in input file
# ! SLAAD    Specific leaf area, excluding weight of C stored in leaves
# !            (cm2[leaf] / g[leaf])
# ! SLPF     Empirical multiplier to adjust daily canopy PG due to unknown
# !            soil or environmental factors that persist in a given location
# ! SLW      Specific leaf weight (g[leaf] / m2[leaf])
# ! SPACNG   Ratio of distance between plants in a row to distance between
# !            rows (or vice versa - always < 1)
# ! SWFAC    Effect of soil-water stress on photosynthesis, 1.0=no stress,
# !            0.0=max stress
# ! TABEX    Function subroutine - Lookup utility
# ! TDAY     Average temperature during daylight hours (°C)
# ! TIMDIF   Integer function which calculates the number of days between two
# !            Julian dates (da)
# ! TPGFAC   Reduction in specific leaf area due to daytime temperature being
# !            less than optimal (0-1)
# ! TYPPGN   Type of function for the leaf N effects on PG
# ! TYPPGT   Character variable specifying the type of function to use for
# !            the relationship between temperature and PG (for use in
# !            function subroutine CURV)
# ! XHLAI    Leaf area index (m2[leaf] / m2[ground])
# ! XPGSLW(I) Array of SLW values for table look-up function, used with YPGSLW
# !            (g[leaf] / m2[leaf])
# ! XPOD     Growth partitioning to pods which slows node appearance
# !            (fraction)
# ! YPGSLW(I) Array of PG values corresponding to SLW values in array XPGSLW
# !            (g[CH2O] / m2 / d)
# !=======================================================================
# ! END SUBROUTINE PHOTO
# !=======================================================================


