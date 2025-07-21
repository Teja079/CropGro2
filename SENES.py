# C=======================================================================
# C  SENES, Subroutine, K.J. Boote, J.W. Jones and G. Hoogenboom
# C  Calculates leaf senescence due to natural aging, drought stress,
# C  light stress, and physiological maturity.
# C------------------------------------------------------------------------
# C  Called : PLANT
# C  Calls  : ERROR, FIND, IGNORE
# C========================================================================
import numpy as np

from ModuleDefs import *
import ModuleData

def SENES(DYNAMIC,
          FILECC, CLW, DTX, KCAN, NR7, NRUSLF, PAR,  # Input
          RHOL, SLAAD, STMWT, SWFAC, VSTAGE, WTLF, XLAI,  # Input
          SLDOT, SLNDOT, SSDOT, SSNDOT):  # Output

    # -----------------------------------------------------------------------
    # Variable Declarations (translated from Fortran)
    # -----------------------------------------------------------------------
    ERRKEY = 'SENES'
    SECTION = ' '*6
    CHAR = ' '*80
    FILECC = ' '*92

    I, II, LUNCRP, ERR, LINC, LNUM , ISECT = [0]*7
    DYNAMIC = 0
    NR7, DAS = [0]*2
    FOUND = 0
    NSWAB = 0
    NSWAB = 5  # PARAMETER value

    DTX, NRUSLF, PAR, RATTP, RHOL, SLAAD = [0.0]*6
    STMWT, VSTAGE, CLW, WTLF, XLAI = [0.0] * 5
    SLDOT, SLNDOT, SSDOT, SSNDOT = [0.0] *4
    ICMP, KCAN, PORPT, SENDAY = [0.0] * 4
    SENRT2, SENRTE, TCMP = [0.0] * 3
    LCMP, LTSEN, PORLFT, LFSEN, SWFAC, WSLOSS, TABEX = [0.0]*6
    SENMAX, SENPOR, XSENMX, XSTAGE = (np.zeros(4) for _ in range(4))
    SWFCAB = np.zeros(NSWAB)

    CONTROL = ControlType()

    # ***********************************************************************
    # Run Initialization - Called once per simulation
    # ***********************************************************************
    if DYNAMIC == RUNINIT:
        # -----------------------------------------------------------------------
        # Read in values from input file, which were previously input
        # in Subroutine IPCROP.
        # -----------------------------------------------------------------------
        GETLUN('FILEC', LUNCRP)
        OPEN(LUNCRP, FILECC, 'OLD', IOSTAT = ERR)
        if(ERR != 0): ERROR(ERRKEY, ERR, FILECC, 0)
        LNUM = 0

        # -----------------------------------------------------------------------
        # Find and Read Partitioning Section
        # -----------------------------------------------------------------------
        SECTION = '!*VEGE'
        FIND(LUNCRP, SECTION,LINC, FOUND)
        LNUM += LINC

        if FOUND == 0:
            ERROR(SECTION, 42, FILECC, LNUM)
        else:
            for I in range(4):
                ISECT = 2
                while ISECT != 1:
                     IGNORE(LUNCRP, LNUM, ISECT, CHAR)
            READ(CHAR, '(6X,F6.0)',IOSTAT = ERR, PORPT)
            if (ERR != 0): ERROR(ERRKEY,ERR,FILECC,LNUM)

        # -----------------------------------------------------------------------
        # Find and Read Senescence Section
        # -----------------------------------------------------------------------
        SECTION = '!*LEAF'
        FIND(LUNCRP, SECTION, LINC, FOUND)
        LNUM += LINC
        if FOUND == 0:
            ERROR(SECTION, 42, FILECC, LNUM)

        FIND(LUNCRP, SECTION, LINC, FOUND)
        LNUM += LINC
        if FOUND == 0:
            ERROR(SECTION, 42, FILECC, LNUM)
        else:
            IGNORE(LUNCRP, LNUM, ISECT, CHAR)
            SENRTE, SENRT2, SENDAY = READ(CHAR, '(3F6.0)')

            ISECT, CHAR = IGNORE(LUNCRP, LNUM)
            ICMP, TCMP = READ(CHAR, '(2F6.0)')

            ISECT, CHAR = IGNORE(LUNCRP, LNUM)
            XSTAGE[:4], XSENMX[:4] = READ(CHAR, '(8F6.0)')

            ISECT, CHAR = IGNORE(LUNCRP, LNUM)
            SENPOR[:4], SENMAX[:4] = READ(CHAR, '(8F6.0)')

        CLOSE(LUNCRP)

    # ***********************************************************************
    # Seasonal Initialization - Run once per season
    # ***********************************************************************
    elif DYNAMIC == SEASINIT:
        SSDOT = 0.0
        SLDOT = 0.0
        SLNDOT = 0.0
        SSNDOT = 0.0
        RATTP = 1.0

        for I in range(5):
            SWFCAB[I] = 1.0

    # ***********************************************************************
    # Daily Rate/Integration
    # ***********************************************************************
    elif DYNAMIC == INTEGR:
        GET(CONTROL)
        DAS = CONTROL.DAS

        for I in range(NSWAB - 1, 1, -1):
            SWFCAB[I] = SWFCAB[I - 1]
        SWFCAB[0] = SWFAC
        RATTP = SWFCAB[NSWAB - 1]

        SSDOT = 0.0
        SLDOT = 0.0
        SLNDOT = 0.0
        SSNDOT = 0.0

        if DAS <= NR7 and VSTAGE >= 1.0:
        #-----------------------------------------------------------------------
        #This section calculates natural senescence prior to the
        #beginning of seed growth
        #-----------------------------------------------------------------------
            if VSTAGE >= 5.0:
                PORLFT = 1.0 - TABEX(SENPOR, XSTAGE, VSTAGE, 4)
                if WTLF * (1.0 - RHOL) > CLW * PORLFT:
                    SLDOT = WTLF * (1.0 - RHOL) - CLW * PORLFT

        #-----------------------------------------------------------------------
        #This section calculates leaf senescence due to N mobilization.
        #Concentration of N in stems and leaves must
        #be recalculated since senescing leaves and petioles are assumed
        #to contain no more mineable Protein--i.e., N content of senesced
        #leaves and petioles is different from canopy average.
        #-----------------------------------------------------------------------
            LFSEN = SENRTE * NRUSLF / 0.16
            LFSEN = min(WTLF, LFSEN)
            SLDOT += LFSEN
            SLDOT = min(WTLF, SLDOT)

        # -----------------------------------------------------------------------
        #This section calculates senescence due to low light in lower
        #canopy.  First compute LAI at which light compensation is reached
        #then allow LAI above this amount to be senesced over TCMP thermal
        #days.
        # -----------------------------------------------------------------------
            LTSEN = 0.0
            if PAR > 0.0:
                LCMP = -(1.0 / KCAN) * np.log(ICMP / PAR)
                LTSEN = DTX * (XLAI - LCMP) / TCMP
                LTSEN = max(0.0, LTSEN)

            SLDOT += LTSEN * 10000.0 / SLAAD

            WSLOSS = SENDAY * (1.0 - RATTP) * WTLF
            if WSLOSS > 0.0:
                PORLFT = 1.0 - TABEX(SENMAX, XSENMX, VSTAGE, 4)
                WSLOSS = min(WSLOSS, WTLF - CLW * PORLFT)
                WSLOSS = max(WSLOSS, 0.0)
                SLNDOT = WSLOSS

            SLDOT += SLNDOT
            SSDOT = SLDOT * PORPT
            SSDOT = min(SSDOT, 0.1 * STMWT)
            SSNDOT = SLNDOT * PORPT
            SSNDOT = min(SSDOT, SSNDOT)

        elif DAS > NR7:
            if WTLF > 0.0001:
                SLDOT = WTLF * SENRT2
                SLNDOT = SLDOT
                SSDOT = SLDOT * PORPT
                SSNDOT = SSDOT
            else:
                SLDOT = 0.0
                SSDOT = 0.0
                SLNDOT = 0.0
                SSNDOT = 0.0

            if STMWT < 0.0001:
                SLNDOT = 0.0
                SSNDOT = 0.0

    return

# !***********************************************************************
# !     SENES VARIABLE DEFINITIONS:
# !-----------------------------------------------------------------------
# ! CHAR      Contains the contents of last record read
# ! CLW       Cumulative leaf growth (g[leaf]/m2)
# ! DAS       Days after start of simulation (days)
# ! DTX       Thermal time that occurs in a real day based on vegetative
# !             development temperature function (thermal days / day)
# ! ERR       Error code for file operation
# ! FILECC    Path plus filename for species file (*.spe)
# ! FOUND     Indicator that good data was read from file by subroutine FIND
# !             (0 - End-of-file encountered, 1 - NAME was found)
# ! ICMP      Light compensation point for senescence of lower leaves because
# !             of excessive self-shading by crop canopy (moles / m2 / day)
# ! ISECT     Data record code (0 - End of file encountered, 1 - Found a good
# !             line to read, 2 - End of Section in file encountered, denoted
# !             by * in column 1
# ! KCAN      Canopy light extinction coefficient for daily PAR, for
# !             equidistant plant spacing, modified when in-row and between
# !             row spacings are not equal
# ! LCMP      LAI at which today's light compensation (ICMP) is reached
# !             (m2[leaf] / m2[ground])
# ! LNUM      Current line number of input file
# ! LTSEN     Senescence of lower leaves due to self-shading of canopy
# !             (1/day)
# ! LUNCRP    Logical unit number for FILEC (*.spe file)
# ! NR7       Day when 50% of plants first have yellowing or maturing pods
# !             (days)
# ! NRUSLF    N actually mobilized from leaves in a day (g[N]/m2-d)
# ! PAR       Daily photosynthetically active radiation or photon flux
# !             density (moles[quanta]/m2-d)
# ! PORLFT    Proportion of leaf weight grown which will have been senesced
# !             if no water stress has occurred prior to this V-stage
# ! PORPT     Ratio of petiole to leaf weight
# ! RATTP     Factor used in determining increased senescence due to water
# !             stress
# ! RHOL      Fraction of leaf which is carbohydrate (g [CH20] / g[leaf])
# ! SECTION   Section name in input file
# ! SENDAY    Maximum fraction of existing leaf weight which can be senesced
# !             on day N as a function of severe water stress 4 days earlier.
# ! SENMAX(I) Maximum proportion of total leaf weight as a function of
# !             V-stage (XSENMX(I)) which can be senesced due to water stress.
# ! SENPOR(I) Proportion of leaf weight grown which will have been senesced
# !             by a given V- stage (XSTAGE(I)) if no water stress has
# !             occurred prior to this V-stage (XSTAGE(I)) -- normal
# !             vegetative senescence does not occur if prior water stress
# !             has already  reduced leaf
# ! SENRT2    Factor by which leaf weight is multiplied to determine
# !             senescence each day after NR7 (g(leaf) / g(protein loss))
# ! SENRTE    Factor by which protein mined from leaves each day is
# !             multiplied to determine LEAF senescence.
# !             (g(leaf) / g(protein loss))
# ! LFSEN     Leaf senescence due to N mobilization (g[leaf] / m2[ground])
# ! SLAAD     Specific leaf area, excluding weight of C stored in leaves
# !             (cm2[leaf] / g[leaf])
# ! SLDOT     Defoliation due to daily leaf senescence (g/m2/day)
# ! SLNDOT    Leaf senescence due to water stress (g/m2/day)
# ! SSDOT     Daily senescence of petioles (g / m2 / d)
# ! SSNDOT    Petiole senescence due to water stress (g/m2/day)
# ! STMWT     Dry mass of stem tissue, including C and N
# !             (g[stem] / m2[ground)
# ! TABEX     Function subroutine - Lookup utility
# ! TCMP      Time constant for senescence of lower leaves because of
# !             excessive self-shading by crop canopy (thermal days)
# ! TIMDIF    Integer function which calculates the number of days between
# !             two Julian dates (da)
# ! VSTAGE    Number of nodes on main stem of plant
# ! WSLOSS    Leaf senescence due to water stress (g/m2/day)
# ! WTLF      Dry mass of leaf tissue including C and N
# !             (g[leaf] / m2[ground])
# ! XLAI      Leaf area (one side) per unit of ground area
# !             (m2[leaf] / m2[ground])
# ! XSENMX(I) V-stage at which maximum fraction of cumulative leaf growth
# !             vulnerable to loss due to water stress is SENMAX(I).
# !             (# leaf nodes)
# ! XSTAGE(I) V-stage at which SENPOR(I) fraction of cumulative leaf growth
# !             will have been senesced if no water stress occurred.
# !             (# leaf nodes)
# ! YRDOY     Current day of simulation (YYDDD)
# ! YRSIM     Start of simulation date (YYDDD)
# !-----------------------------------------------------------------------
# !     END SUBROUTINE SENES
# !-----------------------------------------------------------------------
