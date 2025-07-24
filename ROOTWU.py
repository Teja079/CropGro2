# C=======================================================================
# C  ROOTWU, Subroutine, J.T. Ritchie
# C  Calculates root water uptake rate for each soil layer and total rate.
# C-----------------------------------------------------------------------
# C  REVISION       HISTORY
# C  01/01/1989 JR  Written
# C  12/05/1993 NBP Made into subroutine.
# C  01/18/1996 JWJ Added flooding effect on water uptake
# C  01/06/1996 GH  Added soil water excess stress
# C  10/10/1997 CHP Updated for modular format.
# C  09/01/1999 GH  Incorporated in CROPGRO
# C  01/10/2000 NBP Added SAVE for stored variables and set SWCON2=RWU=0.0
# C  01/12/2000 NBP Removed FILECC from input
# C  01/25/2000 NBP Added IOSTAT to READ statements to set ERRNUM.  Cleaned.
# C  06/21/2001 GH  Added seasonal initialiation
# C  09/17/2001 CHP Input PORMIN and RWUMX from Plant module.
# C
# C-----------------------------------------------------------------------
# C Called by: SPAM, ETPHOT
# C Calls:     None
# C=======================================================================
import math
from ModuleDefs import RunConstants as RC
import numpy as np
from ModuleDefs import NL
def ROOTWU(DYNAMIC, DLAYR, LL, NLAYR, PORMIN, RLV, RWUMX, SAT, SW):
# C-----------------------------------------------------------------------
#       USE ModuleDefs     !Definitions of constructed variable types,
#                          ! which contain control information, soil
#                          ! parameters, hourly weather data.
#       IMPLICIT NONE
#       SAVE
# C-----------------------------------------------------------------------
#       INTEGER DYNAMIC
#
#       INTEGER L, NLAYR
#
#       REAL SWEXF, TRWUP
#       REAL SWCON1, SWCON3, PORMIN, RWUMX
    DLAYR = np.zeros(NL, dtype=float)
    LL = np.zeros(NL, dtype=float)
    RLV = np.zeros(NL, dtype=float)
    RWU = np.zeros(NL, dtype=float)
    SAT = np.zeros(NL, dtype=float)
    SW = np.zeros(NL, dtype=float)
    SWCON2 = np.zeros(NL, dtype=float)
    TSS = np.zeros(NL, dtype=float)
#       REAL DENOMINATOR
#
    SWCON1 = 1.32e-3
    SWCON3 = 7.01
#
# !***********************************************************************
# !***********************************************************************
# !     Seasonal Initialization - Called once per season
# !***********************************************************************
    if (DYNAMIC == RC.SEASINIT):
# !-----------------------------------------------------------------------
# C     Compute SWCON2 for each soil layer.  Adjust SWCON2 for extremely
# C     high LL to avoid water uptake limitations.
# !-----------------------------------------------------------------------
        SWCON2 = 0.0
        RWU    = 0.0
        TSS    = 0.0
        TRWUP  = 0.0
#
#
#
# !***********************************************************************
# !***********************************************************************
# !     DAILY RATE CALCULATIONS
# !***********************************************************************
    elif (DYNAMIC == RC.RATE):
# C-----------------------------------------------------------------------
        TRWUP  = 0.0
        for L in range(NLAYR):
            SWCON2[L] = 120.0 - 250.0 * LL[L]
            if LL[L] > 0.30:
                SWCON2[L] = 45.0
            RWU[L] = 0.0  # WDB - CIMMYT 2002
#
        for L in range(NLAYR):
            if RLV[L] <= 0.00001 or SW[L] <= LL[L]:
                RWU[L] = 0.0
            else:
                if RLV[L] > math.exp(SWCON3):
                    DENOMINATOR = SWCON3 - math.log(SWCON3)
                else:
                    DENOMINATOR = SWCON3 - math.log(RLV[L])
#
                RWU[L] = SWCON1 * math.exp(min(SWCON2[L] * (SW[L] - LL[L]), 40.0)) / DENOMINATOR

# !           Previous denominator - could explode for large RLV - problem with RLV?
# !     &      (SWCON3-ALOG(RLV(L)))
# !           RWU in cm3[water]/cm[root]-d
#
# C-----------------------------------------------------------------------
# C           PORMIN = MINIMUM PORE SPACE  REQUIRED FOR SUPPLYING OXYGEN
# C                TO ROOTS FOR OPTIMAL GROWTH AND FUNCTION
# C     TSS(L) = number of days soil layer L has been saturated
# C-----------------------------------------------------------------------
# !         CHP 6/27/2011 Add check for SW ~= SAT and PORMIN = 0.0 (Flooded rice)
# !         IF ((SAT(L)-SW(L)) .GE. PORMIN) THEN
            if (SAT[L] - SW[L]) >= PORMIN or PORMIN < 1e-6:
                TSS[L] = 0.0
            else:
                TSS[L] = TSS[L] + 1.0

# C-----------------------------------------------------------------------
# C           Delay of 2 days after soil layer is saturated before root
# C           water uptake is affected
# C-----------------------------------------------------------------------
            if TSS[L] > 2.0:
                SWEXF = (SAT[L] - SW[L]) / PORMIN
                SWEXF = max(SWEXF, 0.0)
            else:
                SWEXF = 1.0
            SWEXF = min(SWEXF, 1.0)
            RWU[L] = min(RWU[L], RWUMX * SWEXF)
            RWU[L] = min(RWU[L], RWUMX)
#         ENDIF
            RWU[L] = RWU[L] * DLAYR[L] * RLV[L]
# !       cm[water]   cm3[water]   cm3[soil]   cm[root]
# !       --------- = ---------- * --------- * ---------
# !           d       cm[root]-d   cm2[soil]   cm3[soil]
#
            TRWUP = TRWUP + RWU[L]     #!cm/d
#       ENDDO
#
# !***********************************************************************
# !***********************************************************************
# !     END OF DYNAMIC IF CONSTRUCT
# !***********************************************************************
#       ENDIF
# !***********************************************************************
    return RWU,TRWUP
NLAYR = 9
DYNAMIC = RC.RATE
DLAYR = np.array([15.0, 30.0, 45.0])         # cm
LL = np.array([-99.00, -99.00, 0.15])             # lower limit
PORMIN = 0.05                                # minimum pore space
RLV = np.array([0.00, 0.00, 0.000])          # root length density
RWUMX = 0.5                                  # max uptake per root length
SAT = np.array([-99.00, -99.00, 0.15])            # saturation
SW = np.array([0.3, 0.28, 0.27])             # current soil water

RWU, TRWUP = ROOTWU(DYNAMIC, DLAYR, LL, NLAYR, PORMIN, RLV, RWUMX, SAT, SW)

print( RWU)
print( TRWUP)

#       END SUBROUTINE ROOTWU
#
# !-----------------------------------------------------------------------
# !     ROOTWU VARIABLE DEFINITIONS:
# !-----------------------------------------------------------------------
# ! DLAYR(L)  Soil thickness in layer L (cm)
# ! LL(L)     Volumetric soil water content in soil layer L at lower limit
# !             (cm3/cm3)
# ! NL        Maximum number of soil layers = 20
# ! NLAYR     Actual number of soil layers
# ! PORMIN    Minimum pore space required for supplying oxygen to roots for
# !             optimal growth and function (cm3/cm3)
# ! RLV(L)    Root length density for soil layer L ((cm root / cm3 soil))
# ! RWU(L)    Root water uptake from soil layer L (cm/d)
# ! RWUMX     Maximum water uptake per unit root length, constrained by soil
# !             water (cm3[water] / cm [root])
# ! SAT(L)    Volumetric soil water content in layer L at saturation
# !             (cm3 [water] / cm3 [soil])
# ! SATFAC    Root length weighted soil water excess stress factor ( 0 = no
# !             stress; 1 = saturated stress )
# ! SUMEX     Sum of water excess factor times depth times root length
# !             density
# ! SUMRL     Sum of root length density (integrated over depth)
# ! SW(L)     Volumetric soil water content in layer L
# !             (cm3 [water] / cm3 [soil])
# ! SWCON1    Constant used in determining root water uptake
# ! SWCON2(L) Variable used in determining root water uptake, dependant on
# !             lower limit in layer L
# ! SWCON3    Constant used in determining root water uptake
# ! SWEXF     Excess water stress factor for layer with deepest roots (0-1)
# ! TRWUP     Total potential daily root water uptake (cm/d)
# ! TSS(L)    Number of days soil layer L has been saturated (d)
# !-----------------------------------------------------------------------
# !     END SUBROUTINE ROOTWU
# !-----------------------------------------------------------------------
