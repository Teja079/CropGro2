# =======================================================================
#   IPWBAL, Subroutine, C. H. Porter
# -----------------------------------------------------------------------
#   Reads input variables for soil-water routine.
# -----------------------------------------------------------------------
#   Called by: WATBAL
#   Calls    : ERROR, FIND
# =======================================================================
def IPWBAL(CONTROL, LL, NLAYR):
    from ModuleDefs import NL, RunConstants as RC, SAVE_data
    from READS import find
    from ERROR import ERROR
    import fortranformat as ff

    SW = [-99.0] * NL
    SW_INIT =  [-99.0] * NL
    ERRKEY = 'IPWBAL'

    DYNAMIC = CONTROL.DYNAMIC
    FILEIO = CONTROL.FILEIO
    RNMODE = CONTROL.RNMODE
    RUN = CONTROL.RUN

    if DYNAMIC == RC.RUNINIT:
        if not any(char in RNMODE for char in 'FQ' or RUN == 1):
            LNUM = 0
            try:
                with open(FILEIO, 'r') as f:
                    lines = f.readlines()
                    SECTION = '*INITI'
                    LNUM, found = find(lines, SECTION)
                    LNUM += 1
                    if not found:
                        ERROR(SECTION, 42, FILEIO, LNUM)
                        return
                    fmt = ff.FortranRecordReader("(40X,F6.0)")
                    ICWD = fmt.read(lines[LNUM])[0]
                    LNUM += 1
                    if ICWD < 0.0:
                        ICWD = 9999.0
                    ActWTD = ICWD

                    fmt = ff.FortranRecordReader('(9X,F5.3)')
                    for L in range(NLAYR):
                        SW[L] = fmt.read(lines[LNUM])[0]
                        LNUM += 1

                        if SW[L] < LL[L]:
                            if L == 0:
                                SWAD = 0.30 * LL[L]
                                if SW[0] < SWAD:
                                    SW[0] = SWAD
                            else:
                                SW[L] = LL[L]
            except Exception as e:
                ERROR(ERRKEY, e, FILEIO, LNUM)
                return
        IPWBAL.SW_INIT = SW
        IPWBAL.ICWD_INIT = ICWD
        SAVE_data.MGMT.ICWD = ICWD

    elif DYNAMIC == RC.SEASINIT:
        SW = IPWBAL.SW_INIT
        ICWD = IPWBAL.ICWD_INIT
        ActWTD = ICWD
        SAVE_data.MGMT.ICWD = ICWD
        SAVE_data.MGMT.ActWTD = ActWTD

    return SW, ActWTD


# =======================================================================
#   WTDEPT, Subroutine
#   Determines water table depth
#   Allows perched water table (uses highest free water surface in profile)
# -----------------------------------------------------------------------
#   REVISION HISTORY
#   01/06/1997 GH  Written
#   10/20/1997 CHP Modified for modular format.
#   03/17/2001 CHP Allows water table between top and bottom of layer
#                  (no longer a step function).  Uses highest free water
#                  surface.
#   05/23/2007 CHP Start at bottom of profile and determine 1st unsaturated
#                    layer.  Add factor to decrease step function appearance.
# -----------------------------------------------------------------------
#   Called by: WATBAL
#   Calls    : None
# =======================================================================
def WTDEPT(NLAYR, DLAYR, DS, DUL, SAT, SW, WTDEP):
    """
    Compute depth to water table (WTDEP)

    Parameters:
    - NLAYR : int
    - DLAYR, DS, DUL, SAT, SW : list or array of length NLAYR

    Returns:
    - WTDEP : float
    """
    # ------------------------------------------------------------------
    TOL = 0.95
    SATFRAC = [0.0] * NLAYR

    # -----------------------------------------------------------------------
    for L in range(NLAYR - 1, -1, -1):
        SATFRAC[L] = (SW[L] - DUL[L]) / (SAT[L] - DUL[L])
        SATFRAC[L] = min(max(0.0, SATFRAC[L]), 1.0)
        if SATFRAC[L] > TOL:
            # Layer is saturated, continue up to next layer
            continue
        elif L == NLAYR - 1:
            # Bottom layer is unsaturated
            WTDEP = DS[NLAYR - 1] - DLAYR[NLAYR - 1] * SATFRAC[NLAYR - 1]
            break
        else:
            # Layer is unsaturated. Interpolate water table depth.
            # FACTOR prevents a step function when transitioning between layers.
            FACTOR = min(max(0.0, (SATFRAC[L + 1] - TOL) / (1.0 - TOL)), 1.0)
            WTDEP = DS[L] - DLAYR[L] * SATFRAC[L] * FACTOR
            break

    return WTDEP

# -----------------------------------------------------------------------
#      WTDEPT VARIABLE DEFINITIONS:
# -----------------------------------------------------------------------
#  DLAYR(L) Soil thickness in layer L (cm)
#  DS(L)    Cumulative depth in soil layer L (cm)
#  NL       Maximum number of soil layers = 20
#  NLAYR    Actual number of soil layers
#  SAT(L)   Volumetric soil water content in layer L at saturation
#             (cm3 [water] / cm3 [soil])
#  SW(L)    Volumetric soil water content in layer L (cm3[water]/cm3[soil])
#  WTDEP    Water table depth  (cm)
#  SATFRAC   Fraction of layer L which is saturated
# -----------------------------------------------------------------------
#      END SUBROUTINE WTDEPT
# =======================================================================