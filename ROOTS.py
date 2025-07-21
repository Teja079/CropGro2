# C=======================================================================
# C  ROOTS, Subroutine, G. Hoogenboom, J.W. Jones
# C-----------------------------------------------------------------------
# C  Calculates root growth, extension, respiration, and senescence
# !-----------------------------------------------------------------------
# !  Called by  :  PLANT
# !  Calls      :  IPROOT, INROOT
# !=======================================================================
from ModuleDefs import *
import numpy as np

def ROOTS(DYNAMIC,
     AGRRT, CROP, DLAYR, DS, DTX, DUL, FILECC, FRRT, #Input
     ISWWAT, LL, NLAYR, PG, PLTPOP, RO, RP, RTWT,    #Input
     SAT, SW, SWFAC, VSTAGE, WR, WRDOTN, WTNEW,      #Input
     RLV, RTDEP, SATFAC, SENRT, SRDOT):              #Output

    iswwat = ''
    crop = ''*2
    filecc = ''*92
    L, L1, NLAYR = np.zeros(3,dtype = int)
    DYNAMIC = DYNAMIC

    CUMDEP, DEPMAX, DTX, FRRT = np.zeros(4)
    PG, RFAC1, RFAC2, RFAC3 = np.zeros(4)
    RLDSM, RLNEW, RO, RP = np.zeros(4)
    RTDEP, RTSDF, RTSEN, RTWT, SRDOT, SWDF, SWFAC = np.zeros(7)
    TRLDF, TRLV, WRDOTN = np.zeros(3)
    CGRRT, AGRRT = np.zeros(2)
    SWEXF, PORMIN, RTEXF, RTSURV = np.zeros(4)
    RTDEPI, SUMEX, SUMRL, SATFAC = np.zeros(4)
    PLTPOP, WTNEW, VSTAGE = np.zeros(3)
    TABEX = 0.0
    XRTFAC, YRTFAC = np.zeros(4), np.zeros(4)

    DLAYR, DS, DUL, ESW, LL, RLDF = (np.zeros(NL) for _ in range(6))
    RLGRW, RLSEN, RLV, RLV_WS, RRLF = (np.zeros(NL) for _ in range(5))
    SW, SAT, WR = (np.zeros(NL) for _ in range(3))
    GRESPR, MRESPR, RESPS = (np.zeros(NL) for _ in range(3))
    SENRT = np.zeros(NL)
    TRLV_MIN, RLSENTOT, FACTOR, RTWTMIN = 0.0*4
    TotRootMass, CumRootMass = 0.0*2

    if DYNAMIC == RUNINIT:
        # ***********************************************************************
        #     Run Initialization - Called once per simulation
        # ***********************************************************************
        # -----------------------------------------------------------------------
        IPROOT(FILECC, PORMIN, RFAC1, RLDSM, RTDEPI, RTEXF, RTSEN, RTSDF, RTWTMIN, XRTFAC, YRTFAC)

        DEPMAX = DS[NLAYR - 1]

    elif DYNAMIC == SEASINIT:
        # ***********************************************************************
        #     Seasonal initialization - run once per season
        # ***********************************************************************
        # -----------------------------------------------------------------------
        SRDOT = 0.0
        RLV.fill(0.0)
        RTDEP = 0.0
        SENRT.fill(0.0)
        SUMEX = 0.0
        SUMRL = 0.0
        SATFAC = 0.0

        # -----------------------------------------------------------------------
        #     ROOT DEPTH INCREASE RATE WITH TIME, cm/physiological day
        # -----------------------------------------------------------------------
        if CROP != 'FA':
            RFAC2 = TABEX(YRTFAC, XRTFAC, 0.0, 4)

        CumRootMass = 0.0

    elif DYNAMIC == EMERG:
        # ***********************************************************************
        #     EMERGENCE CALCULATIONS - Performed once per season upon emergence
        #         or transplanting of plants
        # ***********************************************************************
        # -----------------------------------------------------------------------
        #       Call INROOT for initialization of root variables on
        #       day of emergence.  (GROW emergence initialization
        #       must precede call to INROOT.)
        # -----------------------------------------------------------------------
        INROOT(DLAYR, FRRT, NLAYR, PLTPOP, RFAC1, RTDEPI, WTNEW,
               RLV, RTDEP) #outputs

        RFAC3 = RFAC1

        TRLV = np.sum(RLV * DLAYR)

        CumRootMass = WTNEW * FRRT * PLTPOP * 10.0
# !        kg[root]  g[tissue] g[root]    plants   kg/ha
# !        -------- = ----- * --------- * ------ * -----
# !           ha      plant   g[tissue]     m2      g/m2


    elif DYNAMIC == INTEGR:
        # ***********************************************************************
        #     DAILY RATE/INTEGRATION
        # ***********************************************************************
        # -----------------------------------------------------------------------
        #     Calculate Root Depth Rate of Increase, Physiological Day (RFAC2)
        # -----------------------------------------------------------------------
        RFAC2 = TABEX(YRTFAC, XRTFAC, VSTAGE, 4)
        RLNEW = WRDOTN * RFAC1 / 10000.0
        CGRRT = AGRRT * WRDOTN

        # -----------------------------------------------------------------------
        #     Calculate root length per cm2 soil and initiate growth,
        #     respiration and senescence by layer
        # -----------------------------------------------------------------------

        RRLF = np.zeros(NLAYR)
        RLSEN = np.zeros(NLAYR)
        RLGRW = np.zeros(NLAYR)
        MRESPR = np.zeros(NLAYR)
        GRESPR = np.zeros(NLAYR)
        RESPS = np.zeros(NLAYR)
        RLV_WS = np.zeros(NLAYR)

        # -----------------------------------------------------------------------
        #     Update RFAC3 based on yesterday's RTWT and TRLV
        # -----------------------------------------------------------------------
        if (RTWT - WRDOTN >= 0.0001 and TRLV >= 0.00001):
            RFAC3 = TRLV * 10000.0 / RTWT
        else:
            RFAC3 = RFAC1

        if RTWTMIN > 0.0:
            TRLV_MIN = RTWTMIN * RFAC3 / 1.E4  # same units as TRLV
           #cm / cm2 = (g / m2) * (cm / g) / (cm2 / m2)
        else:
            TRLV_MIN = 0.0

        # -----------------------------------------------------------------------
        TRLDF = 0.0
        CUMDEP = 0.0
        SUMEX = 0.0
        SUMRL = 0.0

        for L in range(NLAYR):
            CUMDEP += DLAYR[L]
            SWDF = 1.0
            SWEXF = 1.0

            if ISWWAT == 'Y':
                if SAT[L] - SW[L] < PORMIN:
                    SWEXF = min((SAT[L] - SW[L]) / PORMIN, 1.0)

                SUMEX += DLAYR[L] * RLV[L] * (1.0 - SWEXF)
                SUMRL += DLAYR[L] * RLV[L]

                ESW = DUL[L] - LL[L]
                if SW[L] - LL[L] < 0.25 * ESW:
                    SWDF = max((SW[L] - LL[L]) / (0.25 * ESW), 0.0)
#---------------------------------------------------------------------------------------
            RTSURV = min(1.0, (1. - RTSDF * (1. - SWDF)), (1. - RTEXF * (1. - SWEXF)))
            if RLV[L] > RLDSM and TRLV + RLNEW > TRLV_MIN:
                RLV_WS[L] = RLV[L] * (1.0 - RTSURV)
            else:
                RLV_WS[L] = 0.0
#---------------------------------------------------------------------------------------
            RLDF[L] = WR[L] * DLAYR[L] * min(SWDF, SWEXF)

            if CUMDEP < RTDEP:
                TRLDF += RLDF[L]
            else:
                if WR[L] > 0.0 and RLNEW > 0.0:
                    if L == 0:  # Adjusted for 0-based indexing in Python
                        RTDEP += DTX * RFAC2
                    else:
                        RTDEP += DTX * RFAC2 * min(SWDF, SWEXF) * (1.0 + 0.25 * (1.0 - max(SWFAC, 0.40)))
                    if RTDEP > DEPMAX:
                        RTDEP = DEPMAX

                RLDF[L] = RLDF(L) * (1. - (CUMDEP - RTDEP) / DLAYR(L))
                TRLDF = TRLDF + RLDF(L)
                break #GOTO 2900


        # -----------------------------------------------------------------------
        # Calculate root senescence, growth, maintenance and growth
        # respiration, and update root length density for each layer.
        # -----------------------------------------------------------------------

    #2900 continue
        if SUMRL > 0.0:
            SATFAC = SUMEX / SUMRL
        else:
            SATFAC = 0.0

        for L in range(L1):  # Python uses 0-based index, so range(L1) goes from 0 to L1-1
            if TRLDF < 0.00001:
                RRLF[L] = 1.0
            else:
                RRLF[L] = RLDF[L] / TRLDF

            # -----------------------------------------------------------------------
            # MRESPR, GRESPR, and RESPS are not used anywhere
            # chp 9/22/98
            # -----------------------------------------------------------------------
            MRESPR[L] = (RLV[L] / RFAC1 * RO * DLAYR[L] * 100.0
                         + RRLF[L] * FRRT * PG * RP) * 44.0 / 30.0
            GRESPR[L] = RRLF[L] * (CGRRT - WRDOTN) * 44.0 / 30.0
            RESPS[L] = MRESPR[L] + GRESPR[L]

            RLGRW[L] = RRLF[L] * RLNEW / DLAYR[L]  # cm[root] / cm3[ground]

            if TRLV + RLNEW > TRLV_MIN:
                RLSEN[L] = RLV[L] * RTSEN * DTX
            else:
                RLSEN[L] = 0.0

            # Limit total senescence in each layer to existing RLV
            if (RLV[L] - RLSEN[L] - RLV_WS[L] + RLGRW[L] < 0.0):
                RLSEN[L] = RLV[L] + RLGRW[L] - RLV_WS[L]

            # RLSENTOT is profile senescence, water stress and natural cm/cm2
            RLSENTOT += (RLSEN[L] + RLV_WS[L]) * DLAYR[L]

        # If senescence is too high (results in TRLV < TRLV_MIN), then
        # reduce senescence in each layer by factor.
        if RLSENTOT > 1.E-6 and TRLV + RLNEW - RLSENTOT < TRLV_MIN:
            FACTOR = (TRLV + RLNEW - TRLV_MIN) / RLSENTOT
            FACTOR = max(0.0, min(1.0, FACTOR))
            RLSEN = RLSEN * FACTOR
            RLV_WS = RLV_WS * FACTOR

        # Update RLV and TRLV based on today's growth and senescence
        TRLV = 0.0
        for L in range(NLAYR):  # Python uses 0-based index, so range(NLAYR) goes from 0 to NLAYR-1
            RLV[L] = RLV[L] + RLGRW[L] - RLSEN[L] - RLV_WS[L]
            TRLV += RLV[L] * DLAYR[L]

            # Keep senescence in each layer for adding C and N to soil
            # SENRT[L] = RLSEN[L] * DLAYR[L] / RFAC1 * 10000. * 10.  # kg/ha
            # 1/14/2005 CHP - water stress senescence needs to be included.
            SENRT[L] = (RLSEN[L] + RLV_WS[L]) * DLAYR[L] / RFAC3 * 1.E5
            # cm[root]              g[root]   1000 cm2   10(kg/ha)
            # kg/ha  =  -------- * cm[soil] * ------- * -------- * ---------
            #              cm3[soil]             cm[root]     m2         (g/m2)

            SENRT[L] = max(SENRT[L], 0.0)
            SRDOT += SENRT[L] / 10.  # g/m2

            # Not used:
            # TRLGRW = TRLGRW + RLGRW[L] * DLAYR[L]
            # TRLSEN = TRLSEN + RLSEN[L] * DLAYR[L]

        # 11/13/2000 CHP Sum RLSEN for total root senescence today.
        # SRDOT = TRLSEN / RFAC3 * 10000.     # g/m2

        # Total root senescence = water stress + natural senescence
        # 10/3/2005 SJR
        # SRDOT = (TRTDY + RLNEW - TRLV) * 10000.0 / RFAC3    # g/m2
        SRDOT = max(SRDOT, 0.0)

        TotRootMass = TRLV / RFAC3 * 1.E5
        # cm[root]   g[root]   10000 cm2   10(kg/ha)
        # kg/ha  = -------- * ------- * -------- * ---------
        #            cm2[soil]   cm[root]     m2         (g/m2)

        CumRootMass += WRDOTN * 10. - SRDOT * 10.

    return

# !=======================================================================
# !  IPROOT Subroutine
# !  Reads root parameters from input files.
# !-----------------------------------------------------------------------
# !  Called : ROOTS
# !  Calls  : FIND, ERROR, IGNORE
# C=======================================================================
def IPROOT(
       FILECC,                                           #Input
       PORMIN, RFAC1, RLDSM, RTDEPI, RTEXF,              #Output
       RTSEN, RTSDF, RTWTMIN, XRTFAC, YRTFAC):            #Output
     return
# !=======================================================================
#
# C=======================================================================
# C  INROOT Subroutine
# C  Initializes root variables at emergence.
# C-----------------------------------------------------------------------
# C  Called : CROPGRO
# C  Calls  : None
# C=======================================================================
def INROOT(
     DLAYR, FRRT, NLAYR, PLTPOP, RFAC1, RTDEPI, WTNEW, #Input
     RLV, RTDEP)       :                                #Output

    L, NLAYR = [0,NLAYR]

    DEP,RLINIT = [0.0] *2
    RTDEP,RTDEPI,CUMDEP = [0.0] *3
    RFAC1 = 0.0
    WTNEW, FRRT, PLTPOP = [0.0] *3
    RLV, DLAYR = (np.zeros(NL) for _ in range(2))

#***********************************************************************
#     INITIALIZE ROOT DEPTH AT EMERGENCE
#-----------------------------------------------------------------------
    RTDEP = RTDEPI
#-----------------------------------------------------------------------
#     DISTRIBUTE ROOT LENGTH EVENLY IN ALL LAYERS TO A DEPTH OF
#     RTDEPTI (ROOT DEPTH AT EMERGENCE)
#-----------------------------------------------------------------------
    CUMDEP = 0.0
    RLV = np.zeros(NLAYR)  # Initialize RLV as a list of zeros

    for L in range(NLAYR):
        DEP = min(RTDEP - CUMDEP, DLAYR[L])  # Depth for current layer
        RLINIT = WTNEW * FRRT * PLTPOP * RFAC1 * DEP / (RTDEP * 10000)
# !        cm[root]   g[root]    plants  cm[root]   m2
# !        -------- = -------- * ------ * ------- * ---
# !      cm2[ground]   plant       m2     g[root]   cm2

        CUMDEP += DEP
        RLV[L] = RLINIT / DLAYR[L]
        if CUMDEP >= RTDEP:
            break

    return RLV

# !=======================================================================
#
# !-----------------------------------------------------------------------
# !       Variable definitions
# !-----------------------------------------------------------------------
# ! AGRRT     Mass of CH2O required for new root growth (g[CH2O] / g[root])
# ! CGRRT     Carbon demand for new root growth (g[CH2O] / m2 / d)
# ! CROP      Crop identification code
# ! CUMDEP    Cumulative depth of soil profile (cm)
# ! DEP       Cumulative soil depth (cm)
# ! DEPMAX    Maximum depth of reported soil layers (cm)
# ! DLAYR(L)  Soil thickness in layer L (cm)
# ! DS(L)     Cumulative depth in soil layer L (cm)
# ! DTX       Thermal time that occurs in a real day based on vegetative
# !             development temperature function (thermal days / day)
# ! DUL(L)    Volumetric soil water content at Drained Upper Limit in soil
# !             layer L (cm3 [H2O] /cm3 [soil])
# ! ESW(L)    Plant extractable soil water by layer (= DUL - LL) (cm3/cm3)
# ! FILECC    Path plus filename for species file (*.spe)
# ! RFAC3     Ratio of root length to root weight at the current time (cm/g)
# ! FRRT      Fraction of vegetative tissue growth that goes to roots on a
# !             day (g[root] / g[veg])
# ! GRESPR(L) Growth respiration for new root growth in layer L
# ! LL(L)     Volumetric soil water content in soil layer L at lower limit
# !             ( cm3/cm3)
# ! LUNCRP    Logical unit number for FILEC (*.spe file)
# ! LUNIO     Logical unit number for FILEIO
# ! MRESPR(L) Maintenance respiration for new root growth in layer L
# ! NL        Maximum number of soil layers = 20
# ! NLAYR     Number of soil layers
# ! PG        Daily gross photosynthesis (g[CH2O] / m2 / d)
# ! PLTPOP    Plant population (# plants / m2)
# ! PORMIN    Minimum pore space required for supplying oxygen to roots for
# !             optimal growth and function (cm3/cm3)
# ! RESPS(L)  Total respiration for new root growth in layer L
# ! RFAC1     Root length per unit  root weight. (cm/g)
# ! RFAC2     Root depth increase rate with time (cm / physiol. day)
# ! RFAC3     Ratio of root length to root weight at the current time (cm/g)
# ! RLDF(L)   Combined weighting factor to determine root growth distribution
# ! RLDSM     Minimum root length density in a given layer, below which
# !             drought-induced senescence is not allowed.
# !             (cm [root ]/ cm3 [soil])
# ! RLGRW(L)  Incremental root length density in soil layer L
# !             (cm[root] / cm3[soil])
# ! RLINIT    Initial root density (cm[root]/cm2[ground])
# ! RLNEW     New root growth added (cm[root]/cm2[ground]/d)
# ! RLSEN(L)  Root length density senesced today (cm[root]/ cm3[soil])
# ! RLV(L)    Root length density for soil layer L (cm[root] / cm3[soil])
# ! RO        Respiration coefficient that depends on total plant mass
# !             (g[CH2O] / g[tissue])
# ! RP        proportion of the day's photosynthesis which is respired in the
# !             maintenance process
# ! RRLF(L)   Root length density factor ratio (RLDF(L) / TRLDF)
# ! RTDEP     Root depth (cm)
# ! RTDEPI    Depth of roots on day of plant emergence. (cm)
# ! RTEXF     Fraction root death per day under oxygen depleted soil
# ! RTSDF     Maximum fraction of root length senesced in a given layer per
# !             physiological day when water content in a given layer falls
# !             below 25 % of extractable soil water.
# ! RTSEN     Fraction of existing root length which can be senesced per
# !             physiological day. (fraction / ptd)
# ! RTSURV(L) Fraction survival of roots on a given day, taking into account
# !             death due to excess or deficit water conditions
# ! RTWT      Dry mass of root tissue, including C and N
# !             (g[root] / m2[ground])
# ! SAT(L)    Volumetric soil water content in layer L at saturation
# !             (cm3 [water] / cm3 [soil])
# ! SRDOT     Daily root senescence (g / m2 / d)
# ! SW(L)     Volumetric soil water content in layer L
# !             (cm3 [water] / cm3 [soil])
# ! SWDF      Soil water deficit factor for layer with deepest roots (0-1)
# ! SWEXF     Excess water stress factor for layer with deepest roots (0-1)
# ! SWFAC     Effect of soil-water stress on photosynthesis, 1.0=no stress,
# !             0.0=max stress
# ! TABEX     Function subroutine - Lookup utility
# ! TRLDF     Total root length density factor for root depth (cm)
# ! TRLGRW    Total new root length density in soil layer L
# !             (cm[root] / cm2[soil])
# ! TRLSEN    Total root length density senesced today (cm[root]/ cm2[soil])
# ! TRLV      Total root length per square cm soil today
# !             (cm[root]/cm2[soil])
# ! TRTDY     Total root length per square cm soil yesterday
# !             (cm[root]/cm2[soil])
# ! VSTAGE    Number of nodes on main stem of plant
# ! WR(L)     Root hospitality factor, used to compute root growth
# ! WRDOTN    Dry weight growth rate of new root tissue including N but not C
# !             reserves (g[root] / m2[ground]-d)
# ! WTNEW     Initial mass of seedling or seed (g / plant)
# ! XRTFAC(I) V-stage at which rate of increase in root depth per
# !             physiological day is YRTFAC(I). (# leaf nodes)
# ! YRTFAC(I) Rate of increase in root depth per degree day at V-stage
# !             XRTFAC(I). (cm / (physiol. day))
# !***********************************************************************
# !      END SUBROUTINES ROOTS, IPROOT, and INROOT
# !=======================================================================