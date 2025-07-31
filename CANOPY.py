#=======================================================================
#  CANOPY
#  Calculates canopy height and canopy width as a function of V-Stage,
#  air temperature, drought stress, daylength, and radiation.
#-----------------------------------------------------------------------
#  Called : VEGGR
#  Calls  : ERROR, FIND, IGNORE
#========================================================================


def CANOPY(DYNAMIC, ECONO, FILECC, FILEGC, KCAN, PAR, ROWSPC, RVSTGE,
           TGRO, TURFAC, VSTAGE, XLAI):
    from ModuleDefs import RunConstants as RC, TS
    from ERROR import ERROR
    from READS import FIND, IGNORE
    import numpy as np

    ERRKEY = "CANOPY" #PARAMETER

    SECTION = " "*6
    ECOTYP, ECONO = " "*6, " "*6
    FILECC, FILEGC = " "*92, " "*92
    C255 = " "*255

    I, II, LUNCRP, LUNECO, ERR, LINC, LNUM, ISECT = [0]*8
    DYNAMIC = 0
    FOUND = 0

    PAR, ROWSPC, RVSTGE, TURFAC, VSTAGE = [0.0]*5
    CANHT, CANWH, XLAI = [0.0]*3
    KCAN, RHGHT, RWIDTH = [0.0]*3
    HWTEM, RCANHT, RCANWH, PARNOD, HPAR, WPAR = [0.0]*6
    TABEX = 0.0

    XHWPAR = np.zeros(10)
    XHWTEM = np.zeros(10)
    YHWPAR = np.zeros(10)
    YHWTEM = np.zeros(10)
    XVSHT = np.zeros(15)
    YVSHT = np.zeros(15)
    YVSWH = np.zeros(15)
    TGRO = np.zeros(len(TGRO))
    # ***********************************************************************
    #     Run Initialization - Called once per simulation
    # ***********************************************************************
    if DYNAMIC == RC.RUNINIT:

        GETLUN('FILEC', LUNCRP)
        OPEN(LUNCRP, FILECC, 'OLD', ERR)
        if ERR != 0:
            ERROR(ERRKEY, ERR, FILECC, 0)

        LNUM = 0
    # ***********************************************************************
    #     ***** READ CANOPY HEIGHT & WIDTH PARAMETERS ******************
    # ***********************************************************************
        SECTION = '!*CANO'
        LINC, FOUND = FIND(LUNCRP, SECTION)
        LNUM += LINC

        if FOUND == 0:
            ERROR(SECTION, 42, FILECC, LNUM)
        else:
            LNUM, ISECT, C255 = IGNORE(LUNCRP, LNUM)
            READ(C255, '(10F6.0)', ERR, XVSHT)
            if ERR != 0:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            LNUM, ISECT, C255 = IGNORE(LUNCRP, LNUM)
            READ(C255, '(10F6.0)', ERR, YVSHT)
            if ERR != 0:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            LNUM, ISECT, C255 = IGNORE(LUNCRP, LNUM)
            READ(C255, '(10F6.0)', ERR, YVSWH)
            if ERR != 0:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            LNUM, ISECT, C255 = IGNORE(LUNCRP, LNUM)
            READ(C255, '(5F6.0)', ERR, XHWTEM)
            if ERR != 0:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            LNUM, ISECT, C255 = IGNORE(LUNCRP, LNUM)
            READ(C255, '(5F6.0)', ERR, YHWTEM)
            if ERR != 0:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            LNUM, ISECT, C255 = IGNORE(LUNCRP, LNUM)
            READ(C255, '(8F6.0)', ERR, XHWPAR)
            if ERR != 0:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

            LNUM, ISECT, C255 = IGNORE(LUNCRP, LNUM)
            READ(C255, '(8F6.0)', ERR, YHWPAR)
            if ERR != 0:
                ERROR(ERRKEY, ERR, FILECC, LNUM)

        CLOSE(LUNCRP)

        GETLUN('FILEE', LUNECO)
        OPEN(LUNECO, FILEGC, 'OLD', ERR)
        if ERR != 0:
            ERROR(ERRKEY, ERR, FILEGC, 0)

        ECOTYP = ''
        LNUM = 0
        while ECOTYP != ECONO:
            LNUM, ISECT, C255 = IGNORE(LUNECO, LNUM)
            if ISECT == 1 and C255[0] not in [' ', '*']:
                READ(C255, '(A6,90X,2(1X,F5.0))', ERR, ECOTYP, RWIDTH, RHGHT)
                if ERR != 0:
                    ERROR(ERRKEY, ERR, FILEGC, LNUM)
                if ECOTYP == ECONO:
                    break
            elif ISECT == 0:
                if ECONO == 'DFAULT':
                    ERROR(ERRKEY, 35, FILEGC, LNUM)
                ECONO = 'DFAULT'
                REWIND(LUNECO)
                LNUM = 0

        CLOSE(LUNECO)

        CANHT = 0.0
        CANWH = 0.0
    # ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
    #    SEASONAL INITIALIZATION
    # ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
    elif DYNAMIC == RC.SEASINIT:
        CANHT = 0.0
        CANWH = 0.0

    # ***********************************************************************
    # EMERGENCE CALCULATIONS - Performed once per season upon emergence
    #     or transplanting of plants
    # ***********************************************************************
    elif DYNAMIC == RC.EMERG:
        CANHT = TABEX(YVSHT, XVSHT, VSTAGE, 10)
        CANWH = TABEX(YVSWH, XVSHT, VSTAGE, 10)

    # ***********************************************************************
    # DAILY RATE/INTEGRATION
    # ***********************************************************************
    elif DYNAMIC == RC.INTEGR:
    #-----------------------------------------------------------------------
    # Calculate effect of temperature on canopy expansion, HWTEM
    #-----------------------------------------------------------------------
        HWTEM = 0.0
        for I in range(TS):
            HWTEM += TABEX(YHWTEM, XHWTEM, TGRO[I], 5)
        HWTEM /= TS

        PARNOD = PAR * np.exp(-KCAN * (0.3 * XLAI))
        HPAR = TABEX(YHWPAR, XHWPAR, PARNOD, 8)
        WPAR = TABEX(YHWPAR, XHWPAR, PAR, 8)
    # -----------------------------------------------------------------------
    # Calculate rate of increase in canopy height and update height, CANHT
    # -----------------------------------------------------------------------
        RCANHT = RVSTGE * TABEX(YVSHT, XVSHT, VSTAGE, 10) * HWTEM * TURFAC * HPAR * RHGHT
        CANHT += RCANHT
        CANHT = max(CANHT, TABEX(YVSHT, XVSHT, 0.0, 10))

        RCANWH = max(0.0, RVSTGE) * TABEX(YVSWH, XVSHT, VSTAGE, 10) * HWTEM * TURFAC * WPAR * RWIDTH
        CANWH += RCANWH

        CANWH = max(CANWH, TABEX(YVSWH, XVSHT, 0.0, 10))
        CANWH = min(CANWH, ROWSPC)

    return CANHT, CANWH
# ======================================================================
# CANOPY Definitions:
# ----------------------------------------------------------------
# C255      255-character record read from file
# CANHT     Canopy height (m)
# CANWH     Canopy width normal to row (m)
# ECONO     Ecotype code - used to match ECOTYP in .ECO file
# ECOTYP    Ecotype code for this simulation
# ERR       Error code for file operation
# FILECC    Path plus filename for species file (*.spe)
# FILEGC    Pathname plus filename for ECO file
# FOUND     Indicator that good data was read from file by subroutine FIND
#             (0 - End-of-file encountered, 1 - NAME was found)
# HPAR      Effect of day's PAR on canopy expansion
# HWTEM     Effect of temperature on canopy expansion
# ISECT     Indicator of completion of IGNORE routine: 0 - End of file
#             encountered, 1 - Found a good line to read, 2 - End of
#             Section in file encountered denoted by * in column 1.
# KCAN      Canopy light extinction coefficient for daily PAR, for
#             equidistant plant spacing, modified when in-row and between
#             row spacing are not equal
# LINC      Line number of input file
# LNUM      Current line number of input file
# LUNCRP    Logical unit number for FILEC (*.spe file)
# LUNECO    Logical unit number for FILEE (*.eco file)
# PAR       Daily photosynthetically active radiation or photon flux
#             density (moles[quanta]/m2-d)
# PARNOD    Effective PAR at growth point (moles[quanta]/m2-d)
# RCANHT    Rate of increase in canopy height (m/d)
# RCANWH    Rate of increase in canopy width (m/d)
# RHGHT     Relative height of this ecotype in comparison to the standard
#             height per node (YVSHT) defined in the species file (*.SPE)
# ROWSPC    Row spacing (m)
# RVSTGE    Rate of VSTAGE change (nodes/day)
# RWIDTH    Relative width of this ecotype in comparison to the standard
#             width per node (YVSWH) defined in the species file (*.SPE) (m)
# SECTION   Section name in input file
# TGRO(I)   Hourly canopy temperature (°C)
# TURFAC    Water stress factor for expansion (0 - 1)
# VSTAGE    Number of nodes on main stem of plant (nodes)
# WPAR      Effect of PAR on canopy width
# XHWPAR(I) PAR values for table look-up for modifying height and width
#             growth rate, particularly to allow etiliolation at low PAR
#             values (mol/day)
# XHWTEM    Temperatures in a table look-up function for modifying height
#             and width growth rates (°C)
# XLAI      Leaf area (one side) per unit of ground area
#            (m2[leaf] / m2[ground])
# XVSHT     Node number on main stem for use in computing height and width
#             growth rates
# YHWPAR(I) Relative increase in height and width growth rates with low PAR
#             as given in XHWPAR
# YHWTEM(I) Relative (0-1) expansion in height and width with temperatures
#             given in XHWTEM
# YVSHT     Length of internode (m) Vs position on the main stem defined by
#             XVSHT (m/node)
# YVSWH     Increase in canopy width per node developed on the main stem
#            (m/node)
#***********************************************************************


