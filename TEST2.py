# !C=======================================================================
# !
# !!-----------------------------------------------------------------------
# !!  DEFINITIONS
# !!  FILESX - Name of SOIL.AUX file
# !!  FILESS - Path plus FILESX string
# !!  LUNSL - File number used to open FILESX
# !!  PATHSL- path pointing to SOIL.AUX (passed in from CERES.for)
# !!  PATHL - length of directory path of SOIL.AUX file
# !!  SLNO - 10 character string containing soil id name
# !!  LN - line number for soil depth when reading hard pan factor
# !!  RGIMPF(L) - Root growth impedance factor, subscripted for each soil layer
# !!  SDEP(L) - soil depth associated with each hard pan factor
# !!=======================================================================
#
# !=======================================================================
# !   SoilLayerText, Subroutine
# !-----------------------------------------------------------------------
# !     Labels for soil layer depth info
# !-----------------------------------------------------------------------
# !  REVISION HISTORY
# !  11/17/2008
# !-----------------------------------------------------------------------
# !  Called by: SoilDYN, CellInit_2D
# !  Calls    :
# !=======================================================================
from ModuleDefs import NL
import numpy as np
#from SOILDYN import LayerText


def SoilLayerText(DS, NLAYR):
#       USE ModuleDefs
#       INTEGER NLAYR, L
#       REAL, DIMENSION(NL) :: DS
#       CHARACTER*8 LayerText(11)
#       INTEGER, DIMENSION(NL) :: ZB, ZT
#       CHARACTER*14 FMT
    DS = np.zeros(NL, dtype=float)
    #ZB = np.zeros(NL, dtype=int)
    #ZT = np.zeros(NL, dtype=int)
#!Establish soil layer depths for headers
#!Text describing soil layer depth data
#!1-9 describe depths for layers 1-9
#!10 depths for layers 10 thru NLAYR (if NLAYR > 9)
#!11 depths for layers 5 thru NLAYR (if NLAYR > 4)
    LayerText ='        '
    ZB = [0] * NL
    ZT = [0] * NL
    for L in range(1, 12):  # 1 to 11 inclusive
        if L == 1:
# !       For layers 1-10, write depths to label, "LayerText"
            ZT[1] = 0
            ZB[1] = round(DS[1])
        elif 2 <= L <= 9:
            if L <= NLAYR:
                ZT[L] = ZB[L - 1]
                ZB[L] = round(DS[L])
        elif L == 10:
# !         Label 10 includes layers 10 thru NLAYR
            if NLAYR > 9:
                ZT[L] = ZB[L-1]
                ZB[L] = DS[NLAYR]
        elif L == 11:
            if NLAYR > 4:
# !           Label 11 includes layers 5 thru NLAYR
                ZT[11] = ZB[4]
                ZB[11] = DS[NLAYR]
#
# !       Format dependant on # digits

        if ZB[L] > 0:
            ZT = ZT[L]
            ZB = ZB[L]
            if ZT <= 9:
                if ZB <= 9:
                    fmt = f"{' ' * 5}{ZT}-{ZB}"
                elif 10 <= ZB <= 99:
                    fmt = f"{' ' * 4}{ZT}-{ZB}"
                elif ZB>100
                    fmt = f"{' ' * 3}{ZT}-{ZB}"
            elif 10 <= ZT <= 99:
                if 10 <= ZB <= 99:
                    fmt = f"{' ' * 3}{ZT}-{ZB}"
                else:
                    fmt = f"{' ' * 2}{ZT}-{ZB}"
            else:
                fmt = f"{' ' * 1}{ZT}-{ZB}"
            LayerText[L] = fmt.ljust(8)
    return LayerText

# C=======================================================================
DS=5
NLAYR=9
Layertext = SoilLayerText(DS, NLAYR)
print(Layertext)