# =======================================================================
#   CapFringe, Subroutine, C.H.Porter
#   Computes capillary fringe above water table.
#   Sends back ThetaCap, an array of volumetric soil water contents at
#   the midpoint of each soil layer.  These are computed based on the
#   water characteristic curve and height above the managed water table.
# =======================================================================
import numpy as np

def CapFringe(ActWTD, SOILPROP):
    """
    ActWTD: Actual Water Table Depth
    SOILPROP: SoilType with soil layer properties
    Returns: ThetaCap
    """
    from ModuleDefs import NL

    ThetaCap = SOILPROP.DUL  # Default to DUL

    if ActWTD < 1.e-6 or ActWTD > 9999.:
        return ThetaCap

    NLAYR = SOILPROP.NLAYR
    DLAYR = SOILPROP.DLAYR
    DS    = SOILPROP.DS
    DUL   = SOILPROP.DUL
    SAT   = SOILPROP.SAT
    WCR   = SOILPROP.WCR
    alphaVG = SOILPROP.alphaVG
    mVG   = SOILPROP.mVG
    nVG   = SOILPROP.nVG

    Se_boundary = -99.
    Se_mid = np.zeros(NL)
    Se_top = np.zeros(NL)

    for L in range(NLAYR-1, -1, -1):  # Fortran 1-based -> Python 0-based
        if L == 0:
            Top = 0.
        else:
            Top = DS[L-1]
        Bottom = DS[L]
        MidPt = (Top + Bottom) / 2.0

        if ActWTD > Bottom:
            if Se_boundary < 0.:
                H_mid = ActWTD - MidPt
                H_equiv = H_mid - (Bottom - MidPt)
            else:
                H_equiv = (((1.0 / Se_boundary ** (1.0 / mVG[L]) - 1.0) ** (1.0 / nVG[L])) / alphaVG[L])
                H_mid = H_equiv + (Bottom - MidPt)

            Se_mid[L] = (1.0 / (1.0 + (alphaVG[L] * H_mid) ** nVG[L])) ** mVG[L]
            ThetaCap[L] = WCR[L] + Se_mid[L] * (SAT[L] - WCR[L])

            H_top = H_equiv + DLAYR[L]
            Se_top[L] = (1.0 / (1.0 + (alphaVG[L] * H_top) ** nVG[L])) ** mVG[L]
            Se_boundary = Se_top[L]

        elif ActWTD > Top:
            if ActWTD <= MidPt:
                ThetaCap[L] = SAT[L]
                Se_mid[L] = 1.0
            else:
                H_mid = ActWTD - MidPt
                Se_mid[L] = (1.0 / (1.0 + (alphaVG[L] * H_mid) ** nVG[L])) ** mVG[L]
                ThetaCap[L] = WCR[L] + Se_mid[L] * (SAT[L] - WCR[L])

            H_top = ActWTD - Top
            Se_top[L] = (1.0 / (1.0 + (alphaVG[L] * H_top) ** nVG[L])) ** mVG[L]
            Se_boundary = Se_top[L]

        else:
            Se_mid[L] = 1.0
            Se_top[L] = 1.0
            ThetaCap[L] = SAT[L]

    return ThetaCap

# =======================================================================
# =======================================================================
#      CapFringe VARIABLE DEFINITIONS:
# -----------------------------------------------------------------------
#  Bottom      Depth to bottom of the current layer
#  H_equiv     Equivalent height for the bottom of the layer
#              Assume: homogeneous soil; the normalized soil water content is Se_boundary
#              Measured from the water table surface
#  ActWTD      Water table depth (cm) below surface.
#  MidPt       Distance between midpoint of current layer to water table surface
#  Se_boundary Normalized soil water content at layer boundary.
#              Assume the Se at top of layer L+1 as the boundary of layer L and layer L+1
#  Se_mid(L)   Normalized soil water content at middle of given layer
#  Se_top(L)   Normalized soil water content at the top of given layer
#  ThetaCap    An array of volumetric soil water contents at the midpoint of each soil layer.
#              Calculated from the water characteristic curve at the height above the
#              water table. At equilibrium state, soil matric potential h = water table depth z
#  Top         Depth to top of the current layer
# -----------------------------------------------------------------------
#      END SUBROUTINE CapFringe
# =======================================================================
