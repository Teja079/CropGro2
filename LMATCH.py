#=======================================================================
#  LMATCH, Subroutine
#
#  Converts input soil layer data into fixed output soil layers
#-----------------------------------------------------------------------
#  INPUT  : NLAYRI,DI,VI,DLAYR
#
#  LOCAL  : J,K,L,VS,ZIL,ZOL,SUMZ,SUMV,ZT,ZB
#
#  OUTPUT : DS,VS
#-----------------------------------------------------------------------
#  Called : IPSOIL IPSLIN
#
#  Calls  : None
#-----------------------------------------------------------------------
#                         DEFINITIONS
#
#  HDLAY  :
#=======================================================================
from ModuleDefs import NL
from WARNING import WARNING
from UTILS import NINT

def LMATCH (NLAYRI,DSI,VI,NLAYRO,DSO):
    import sys
      # INTEGER  J,K,L,NLAYRI,NLAYRO
      #
      # REAL     VI(NL),DSO(NL),VS(NL),DSI(NL)
      # REAL     ZIL,ZOL,SUMZ,SUMV,ZT,ZB
      #
      # LOGICAL MISSING
    if NLAYRI < 1: return

# *----------------------------------------------------------------------
#      Definition of layers and soil depths moved to IPEXP
# *----------------------------------------------------------------------
#      This subroutine assumes that DSI(L) values are depths to the
#      bottom of layer L
    VS = [-99.] * NL
    K   = 0
    ZIL = 0.0
    ZOL = 0.0

    for L in range(NLAYRO):
        SUMZ = 0.0
        SUMV = 0.0
        MISSING = False

        while True:
            ZT = max(ZOL, ZIL)
            ZB = min(DSO[L], DSI[K])
            SUMZ = SUMZ + (ZB - ZT)
            SUMV = SUMV + VI[K] * (ZB - ZT)
            if abs(VI[K] + 99.) < 0.001 and ZB > ZT:
                MISSING = True
            if DSO[L] < DSI[K]: break
            if K + 1 == NLAYRI: #goto 20
                VS[L] = VI[K]
                if abs(SUMV + 99.0) > 0.001:
                    if SUMZ > 0.0:
                        VS[L] = SUMV / SUMZ
                else:
                    VS[L] = -99.0
                break
            ZIL = DSI[K]
            K = K + 1
            if K >= NL:
                print(f" More than NL layers in soil profile : {K}\n Please fix soil profile")
                sys.exit()
        if K+1 == NLAYRI and DSO[L] >= DSI[K]: break #mimicing go to20
        VS[L] = VI[K]
        if SUMZ > 0.0:
            VS[L] = SUMV/SUMZ
        if MISSING:
            VS[L] = -99.0
        ZOL = DSO[L]

    for J in range(NLAYRO):
        VI[J] = VS[J]
    for J in range(NLAYRO+1,NL):
        VI[J] = -99.
    return VI

# =======================================================================
#   LYRSET, Subroutine
#
#   Converts input soil layer data into fixed output soil layers
#   This subroutine assumes that DI(L) values are depths to the
#   bottom of layer L
#   Created by J. W. Jones to create fixed increments for soil
# -----------------------------------------------------------------------
#   INPUT  : NLAYRI,DI,ZLAYR,DS
#
#   LOCAL  : J,K,L,ZIL,ZOL,DI
#
#   OUTPUT : NLAYRO,DS,VS
# -----------------------------------------------------------------------
#   Called : IPSOIL
#
#   Calls  : None
# -----------------------------------------------------------------------
#                          DEFINITIONS
#
#   HDLAY  :
# =======================================================================
def LYRSET(NLAYRI, ZLAYR):
    DS = [0.0] * NL
    DI = [0.0] * NL
    DLAYR = [0.0] * NL
    MSG = [""] * 2

    # M = Soil layer at which profile transitions from
    #     30 cm to 60 cm thickness.
    M = 18  # soil layers 18, 19, 20 with thickness of 60 cm.

    DS[0] = 5.0
    DS[1] = 15.0
    DS[2] = 30.0
    DS[3] = 45.0
    DS[4] = 60.0

    for L in range(5, M - 1):
        DS[L] = DS[L - 1] + 30.0

    for L in range(M - 1, NL):
        DS[L] = DS[L - 1] + 60.0

    for L in range(NL):
        DI[L] = ZLAYR[L]

    K = 0

    for L in range(NL):
        while True:
            if DS[L] < DI[K]:
                break
            if K == NLAYRI - 1:
                break
            K += 1
            if K >= NL:
                MSG[0] = f"More than {NL:3d} layers in modified soil profile."
                MSG[1] = f"Only first {NL:3d} layers will be used."
                WARNING(2, "LMATCH", MSG)
                K = NL - 1
                break
        if K == NLAYRI - 1 or K >= NL - 1:
            break

    DS[L] = DI[K]
    NLAYRO = L + 1
    DLAYR[0] = DS[0]

    if NLAYRO > 1:
        for J in range(1, NLAYRO):
            DLAYR[J] = DS[J] - DS[J - 1]

        if DLAYR[NLAYRO - 1] < DLAYR[NLAYRO - 2] and DLAYR[NLAYRO - 1] < 15.0:
            DLAYR[NLAYRO - 1] = (DLAYR[NLAYRO - 1] + DLAYR[NLAYRO - 2]) / 2
            DLAYR[NLAYRO - 2] = DLAYR[NLAYRO - 1]
            DS[NLAYRO - 2] = DS[NLAYRO - 3] + DLAYR[NLAYRO - 2]

    DEPMAX = DS[NLAYRO - 1]

    return NLAYRO, DS[:NLAYRO], DLAYR[:NLAYRO], DEPMAX


# =======================================================================
#   LYRSET2, Subroutine
#
#   Converts input soil layer data into fixed output soil layers
#   Created by J. W. Jones to create fixed increments for soil
#   Alternate routine written by CHP splits thick layers into
#      2, 3, or 4 homogeneous layers -- minimizes interpolation of
#      soil factors.  WAS: Top 30 cm still broken into 5, 10, 15 cm layers.
#                     NOW: Top 15 cm still broken into 5, 10 cm layers.
# =======================================================================
def LYRSET2(NLAYRI, ZLAYR):
    DS = [0.0] * NL
    DLAYR = [0.0] * NL
    CUMDEP = 0.0
    THICKNESS = 0.0

    # Redistribute soil layers. Keep top two layers at fixed depths.
    DS[0] = 5.0
    DS[1] = 15.0

    # Remaining soil layers set based on thicknesses read from file.
    # Split into 2, 3 or 4 equal layers if thickness > 15 cm.
    # ZLAYR values are depths to bottom of soil layer (cm).
    M = 3
    for L in range(NLAYRI):
        CUMDEP = ZLAYR[L]
        THICKNESS = CUMDEP - DS[M - 2]

        if CUMDEP <= DS[1]:
            # Within top 2 fixed depth layers, depths already set.
            continue

        elif THICKNESS < 2.0:
            # Add to next layer if thin
            continue

        elif ((CUMDEP < 90. and THICKNESS <= 15.0) or
              (CUMDEP >= 90. and THICKNESS <= 30.0) or
              (CUMDEP >= 200. and THICKNESS <= 90.0)):
            # Single layer
            DS[M - 1] = CUMDEP
            M += 1

        elif ((CUMDEP < 90. and THICKNESS <= 30.0) or
              (CUMDEP >= 90. and THICKNESS <= 60.0) or
              (CUMDEP >= 200. and THICKNESS <= 180.0)):
            # Split into two equal layers
            DS[M - 1] = CUMDEP - round(THICKNESS * 0.50)
            DS[M] = CUMDEP
            M += 2

        elif ((CUMDEP < 90. and THICKNESS <= 45.0) or
              (CUMDEP >= 90. and THICKNESS <= 90.0) or
              (CUMDEP >= 200. and THICKNESS <= 270.0)):
            # Split into three equal layers
            DS[M - 1] = CUMDEP - NINT(THICKNESS * 0.66667)
            DS[M] = CUMDEP - NINT(THICKNESS * 0.33333)
            DS[M + 1] = CUMDEP
            M += 3

        else:
            # Split into four equal layers
            DS[M - 1] = CUMDEP - NINT(THICKNESS * 0.75)
            DS[M] = CUMDEP - NINT(THICKNESS * 0.50)
            DS[M + 1] = CUMDEP - NINT(THICKNESS * 0.25)
            DS[M + 2] = CUMDEP
            M += 4

    NLAYRO = M - 1

    # Check for thin bottom layer -- add to previous layer
    if ZLAYR[NLAYRI - 1] > DS[NLAYRO - 1]:
        DS[NLAYRO - 1] = ZLAYR[NLAYRI - 1]

    DLAYR[0] = DS[0]
    for L in range(1, NLAYRO):
        DLAYR[L] = DS[L] - DS[L - 1]

    DEPMAX = DS[NLAYRO - 1]

    return NLAYRO, DS[:NLAYRO], DLAYR[:NLAYRO], DEPMAX
