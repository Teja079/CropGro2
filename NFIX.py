# !=======================================================================
# !  NFIX, Subroutine, J.W. Jones, G. Hoogenboom, and K.J. Boote
# !-----------------------------------------------------------------------
# !  Calculates nitrogen fixation
# !-----------------------------------------------------------------------
# !  Called from:  PLANT
# !  Calls:        FIND, ERROR, IGNORE
# !=======================================================================
import numpy as np

from ModuleDefs import *

def NFIX(DYNAMIC, AGRNOD, CNODMN, CTONOD, DLAYR, DXR57, FILECC, FILEIO, #Input
         NLAYR, NR7, PLTPOP, SAT, ST, SW, TURFAC, #Input
         CNOD, DWNOD, DWNODA, #Output
         NDTH, NFIXN, NODGR, WTNFX, SENNOD): #Output

    TYPFXT,TYPNGT,TYPFXD,TYPFXW,TYPFXA = "   "*5
    ERRKEY = 'NFIX'
    #PARAMETER (ERRKEY = 'NFIX')

    SECTION = ' '*6
    FILEIO = ' '*30
    C80 = ' '*80
    FILECC = ' '*92

    LUNIO = 0

    LUNCRP, ERR, LINC, LNUM, FOUND, ISECT = [0]*6
    I, II, NR7, NLAYR, J, DAS = [0]*6
    DYNAMIC = 0
    SDWNOD = 0

    NDTHMX, NFIXN, NODRGM, NDTH, NODGR = [0.0]*5
    NODRGR, NFXAGE = [0.0]*2
    SWFACT, FLDACT, CNFACT, FRCNM = [0.0]*4
    ACSTF, ACSTG, DSW, FLDSUM, FLAYR, TNFIX, TNGRO = [0.0]*7
    CLEFT, SNACT, RNDTH, CTONOD, DWNODI, DWNOD, CNOD = [0.0]*7
    CUSFIX, DNOD, SNACTM, EFNFIX, EFINOC, CNODCR, CNODGR = [0.0]*7
    AGRNOD, RFIXN, DXR57 = [0.0]*3
    CURV = 0.0
    EPORS, PLTPOP, DWNODA, WTNFX, PRONOD, CNODMN, TURFAC = [0.0]*7
    SWMEM8, PCSFIX, CNOFIX, PNFIXN = [0.0]*4
    FNFXT, FNNGT, FNFXD, FNFXW, FNFXA = (np.zeros(4) for _ in range(5))
    SWMEM = np.zeros(9)
    DLAYR, SAT, SW, ST = (np.zeros(NL) for _ in range(4))
    LAYERFRAC, SENNOD = (np.zeros(NL) for _ in range(2))
    DSWP = 0.0

    CONTROL = ControlType()
    #***********************************************************************
    # Run Initialization - Called once per simulation
    # ***********************************************************************
    if DYNAMIC == RUNINIT:

        with open(FILEIO, 'r') as f:
            lines = f.readlines()

        for line in lines:
            if line.startswith('*INITI'):
                values = line[28:].split()
                EFINOC = float(values[0]) if float(values[0]) > 0 else 1.0
                EFNFIX = float(values[1]) if float(values[1]) > 0 else 1.0
                break

        with open(FILECC, 'r') as f:
            lines = f.readlines()

        for i, line in enumerate(lines):
            if line.startswith('!*RESP'):
                RFIXN = float(lines[i + 2][18:24])
            elif line.startswith('!*PLAN'):
                PRONOD = float(lines[i + 3][12:18])
            elif line.startswith('!*NITR'):
                values = lines[i + 1].split()
                SNACTM, NODRGM, DWNODI, NDTHMX, CNODCR = map(float, values[:5])
                FNFXT = list(map(float, lines[i + 3].split()[:4]))
                FNNGT = list(map(float, lines[i + 4].split()[:4]))
                FNFXD = list(map(float, lines[i + 5].split()[:4]))
                FNFXW = list(map(float, lines[i + 6].split()[:4]))
                FNFXA = list(map(float, lines[i + 7].split()[:4]))
                break
    # ***********************************************************************
    # Seasonal initialization - run once per season
    # ***********************************************************************
    elif DYNAMIC == SEASINIT:
        CNOD = DWNOD = DWNODA = NDTH = NFIXN = NODGR = WTNFX = 0.0
        SENNOD = np.zeros(NL)
        DNOD = 30.0

    # ***********************************************************************
    # DAILY RATE/INTEGRATION
    # ***********************************************************************
    elif DYNAMIC == INTEGR:
        GET(CONTROL)
        DAS = CONTROL.DAS
        #-----------------------------------------------------------------------
        #Set initial nodule mass to DWNODI as read from crop species file
        #-----------------------------------------------------------------------
        if SDWNOD < 1:
            DWNOD = DWNODI * PLTPOP
            SDWNOD = 1
            DWNODA = DWNODI * PLTPOP
            WTNFX = DWNODA * 0.16 * PRONOD
            SWMEM[:7] = 1.0
        # -----------------------------------------------------------------------
        # Initialize soil water and temperature factors (top DNOD cm of soil)
        # -----------------------------------------------------------------------
        SWFACT = FLDACT = 1.0
        ACSTF = ACSTG = DSW = FLDSUM = 0.0
        # -----------------------------------------------------------------------
        # Calculate carbon allocated per unit of nodule biomass:
        # CNODCR = C requirement for nodule respiration (g C/g nodule/d)
        # -----------------------------------------------------------------------
        CNFACT = 1.0
        if DWNOD > 1E-4:
            FRCNM = CTONOD / DWNOD
            if FRCNM < CNODCR:
                CNFACT = FRCNM / CNODCR

        # -----------------------------------------------------------------------
        # Initialize soil water and temperature factors (top DNOD cm of soil)
        # -----------------------------------------------------------------------
        LAYERFRAC = 0.0
        DSWP = 0.0
        DNOD = 50.0
        for i in range(NLAYR):
            FLAYR = 1.0
            DSW += DLAYR[i]
            if DSW > DNOD:
                FLAYR = (DNOD - (DSW - DLAYR[i])) / DLAYR[i]

            ACSTF += DLAYR[i] * FLAYR * CURV(TYPFXT,FNFXT[:4], ST[i])
            ACSTG += DLAYR[i] * FLAYR * CURV(TYPNGT,FNNGT[:4], ST[i])
            EPORS = max(SAT[i] - SW[i], 0.0)
            FLDSUM += DLAYR[i] * FLAYR * CURV(TYPFXW,FNFXW[:3], EPORS)

            if i == 0:
                LAYERFRAC[0] = DSW / DNOD
            else:
                LAYERFRAC[i] = (DSW - DSWP) * FLAYR / DNOD
            DSWP = DSW
            if FLAYR < 1.0:
                break

        TNFIX = ACSTF / DNOD
        TNGRO = ACSTG / DNOD
        FLDACT = FLDSUM / DNOD
        SWFACT = CURV(TYPFXD, FNFXD[:4], TURFAC)
        NFXAGE = CURV(TYPFXA, FNFXA[:4], DXR57)

        # -----------------------------------------------------------------------
        # DETERMINE MEMORY OF PREVIOUS EIGHT DAYS OF SOIL WATER DEFICITS
        # -----------------------------------------------------------------------
        for j in range(7,1,-1):
            SWMEM[j] = SWMEM[j-1]
        SWMEM[0] = SWFACT

        SWMEM8 = sum(SWMEM) / 8

        CLEFT = CTONOD - CNODMN
        SNACT = SNACTM * EFNFIX
        #-----------------------------------------------------------------------
        # Compute nodule death rate as function of SW deficit, SW flooding,
        #                       and carbon deficit (chp)
        #-----------------------------------------------------------------------
        RNDTH = NDTHMX * max((1. - FLDACT), (1. - SWFACT), (1. - CNFACT))
        NDTH = min(1.0, RNDTH) * DWNOD
        SENNOD = [NDTH * LAYERFRAC[i] * 10.0 for i in range(NLAYR)]

        #-----------------------------------------------------------------------
        #    Compute N - Fixation
        #-----------------------------------------------------------------------
        if DAS < NR7:
            PNFIXN = min((CLEFT * 0.16 / RFIXN), (DWNOD * SNACT)) * TNFIX
            NFIXN = PNFIXN * min(SWFACT, SWMEM8, FLDACT)
        else:
            PNFIXN = NFIXN = 0.0
        #-----------------------------------------------------------------------
        #    Compute C for N-Fixation
        #-----------------------------------------------------------------------
        PCSFIX = (PNFIXN / 0.16) * RFIXN
        CUSFIX = (NFIXN / 0.16) * RFIXN
        CNOFIX = PCSFIX - CUSFIX

        CLEFT = max(0.0, CLEFT - CUSFIX - 0.9 * CNOFIX) + CNODMN

        if DAS < NR7:
            NODRGR = NODRGM * EFNFIX * EFINOC
        else:
            NODRGR = 0.0

        NODGR = min(CLEFT / AGRNOD, DWNOD * NODRGR) * TNGRO * min(SWFACT, FLDACT) * NFXAGE
        CNODGR = NODGR * AGRNOD
        CNOD = CUSFIX + CNODGR

# !=======================================================================
# ! ACSTF    Weighted average soil temperature effect on N fixation (cm)
# ! ACSTG    Weighted average soil temperature effect on nodule growth (cm)
# ! AGRNOD   CH2O requirement for nodule growth (g[CH2O] / g[nodule])
# ! CLEFT    C left to grow new nodule mass (g[CH2O] / m2 / d)
# ! CNFACT   Ratio of C needed from nodules for vegetative and reproductive
# !            growth to C needed for nodule respiration (stress factor)
# ! CNOD     C used in N-Fixation and nodule growth (including respiration
# !            costs) today (g[CH2O] / m2 / d)
# ! CNODCR   C requirement for nodule respiration (g[C] / g[nodule] / d)
# ! CNODGR   C used in nodule growth today (g[CH2O] / m2 / d)
# ! CNODMN   Minimum C reserved for nodule growth (g[CH2O] / m2 / d)
# ! CNOFIX   C used for N fixation (g[CH2O] / m2 / d)
# ! CTONOD   C to allocate to nodules to fix N needed for reproductive and
# !            vegetative growth (g[CH2O] / m2 / d)
# ! CURV     Function subroutine
# ! CUSFIX   C used in N-Fixation today (g[CH2O] / m2 / d)
# ! DAS      Days after start of simulation (days)
# ! DLAYR(L) Soil thickness in layer L (cm)
# ! DNOD     Depth of nodule zone (cm)
# ! DSW      Accumulated soil depth (cm)
# ! DWNOD    Current nodule mass (g[nodule] / m2)
# ! DWNODA   Cumulative nodule growth (g[nodule] / m2)
# ! DWNODI   Initial nodule mass per plant (g[nodule] / plant)
# ! DXR57    Relative time between first seed (NR5) and physiological
# !            maturity (NR7)
# ! EFINOC   Inoculation effectiveness (or rhizobium density factor)
# ! EFNFIX   Strain efficiency
# ! EPORS    Soil water content above saturation (cm3/cm3)
# ! ERR      Error code for file operation
# ! ERRKEY   Subroutine name for error file
# ! FILECC   Path plus filename for species file (*.spe)
# ! FILEIO   Filename for INP file (e.g., IBSNAT35.INP)
# ! FLAYR    Fraction of layer within nodule zone
# ! FLDACT   Soil water flooding effect on N2 fixation and nodule growth
# !            (0-1)
# ! FLDSUM   Weighted average flooding conditions effect on N fixation and
# !            nodule growth and nodule death rate (cm)
# ! FNFXA(I) Critical points in development from first seed to physiological
# !            maturity for reducing N fixation due to canopy physiological age
# ! FNFXD(I) Effects of soil water stress factor (TURFAC) on N fixation
# ! FNFXT(I) Critical temperature points for function to reduce N fixation
# !            rate when temperature is not optimal (°C)
# ! FNFXW(I) Critical soil volumetric water contents for reducing N fixation
# !            when the soil is too wet (flooded) (cm3/cm3)
# ! FNNGT(I) Critical temperature points for function to reduce Nodule growth
# !            rates when temperature is not optimal (°C)
# ! FRCNM    C required for reproductive and vegetative growth per nodule
# !            mass (g[CH2O] / g[nodule] / d)
# ! LUNCRP   Logical unit number for FILEC (*.spe file)
# ! LUNIO    Logical unit number for FILEIO
# ! NDTH     Nodule death rate (g[nodule] / m2 / d)
# ! NDTHMX   Maximum relative death rate of nodules under flooded or dry
# !            conditions (g[nodule] / g[nodule] / d)
# ! NFIXN    Amount of N fixed during the day (g[N] / m2 / d)
# ! NFXAGE   Average nodule age effect on nodule growth (0-1)
# ! NL       maximum number of soil layers = 20
# ! NLAYR    Number of soil layers
# ! NODGR    New nodule growth (g[nod] / m2 / d)
# ! NODRGM   Maximum nodule relative growth rate (g[nodule] / g[nodule] / d)
# ! NODRGR   Effective nodule relative growth rate
# !            (g[nodule] / g[nodule] / d)
# ! NR7      Day when 50% of plants first have yellowing or maturing pods
# !            (days)
# ! PCSFIX   Potential C used in N-Fixation today (g[CH2O] / m2 / d)
# ! PLTPOP   Plant population (# plants / m2)
# ! PNFIXN   Potential amount of N fixation today (g[N] / m2 / d)
# ! PRONOD   Protein composition in nodules (g[protein] / g[nodule])
# ! RFIXN    CH2O required for biological N fixation (g[CH2O] / g[protein])
# ! RNDTH    Nodule death rate (g[nodule] / g[nodule] / d)
# ! SAT(L)   Volumetric soil water content in layer L at saturation
# !            (cm3 [water] / cm3 [soil])
# ! SDWNOD   Denotes first entrance to NFIX subroutine
# ! SNACT    Specific nodule activity (g [nodule])
# ! SNACTM   Maximum activity of the nodules (g [nodule])
# ! ST(L)    Soil temperature in soil layer L (°C)
# ! SW(L)    Volumetric soil water content in layer L
# !            (cm3 [water] / cm3 [soil])
# ! SWFACT   Soil water deficit effect on N2 fixation and nodule growth (0-1)
# !
# ! SWMEM(I) Values of last 8 days of soil water deficit factor (SWFACT)
# ! SWMEM8   Average of last 8 days of soil water deficit factor
# ! TNFIX    Soil temperature effect on N2 fixation (0-1)
# ! TNGRO    Soil temperature effect on nodule growth (0-1)
# ! TURFAC   Water stress factor for expansion (0 - 1)
# ! TYPFXA   Type of function for canopy age effects on N fixation
# ! TYPFXD   Type of function for dry soil effects on N fixation
# ! TYPFXT   Type of function for temperature effects on N fixation rates
# ! TYPFXW   Type of function for wet soil effects on N fixation
# ! TYPNGT   Type of function for temperature effects on Nodule growth rates
# ! WTNFX    Cumulative weight of N fixed (g[N] / m2)
# !-----------------------------------------------------------------------
# !       END SUBROUTINE NFIX
# !=======================================================================