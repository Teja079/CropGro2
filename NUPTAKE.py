# C=======================================================================
# C  NUPTAK, Subroutine
# C  Determines N uptake (adapted from CERES)
# C-----------------------------------------------------------------------
# C  Called from:  PLANT
# C  Calls:        ERROR, FIND, IGNORE
# C=======================================================================
import numpy as np
import math
from ModuleDefs import *

def NUPTAK(DYNAMIC,                                             #input
           DLAYR, DUL, FILECC, KG2PPM, LL, NDMSDR, NDMTOT,      #input
           NH4, NO3, NLAYR, RLV, SAT, SW,                       #input
           TRNH4U, TRNO3U, TRNU, UNH4, UNO3):                   #output


    ERRKEY = "NUPTAK"
    #PARAMETER (ERRKEY = 'NUPTAK')
    SECTION = ' '*6
    CHAR = ' '*80
    FILECC = ' '*92

    I, LUNCRP, ERR, LNUM, ISECT, FOUND = [0]*6
    L, NLAYR, DYNAMIC = [0]*3

    NUF, XMIN = [0.0]*2
    DLAYR, LL, DUL, SAT, SW, RLV = (np.zeros(NL) for _ in range(6))
    SNO3, SNH4, KG2PPM, NO3, NH4 = (np.zeros(NL) for _ in range(5))
    RNO3U, RNH4U, UNO3, UNH4 = (np.zeros(NL) for _ in range(4))
    TRNO3U, TRNH4U, TRNU = [0.0]*3
    NDMTOT, NDMSDR, ANDEM, FNH4, FNO3, SMDFR, RFAC = [0.0]*7
    RTNO3, RTNH4, MXNH4U, MXNO3U = [0.0]*4


    # -----------------------------------------------------------------------
    # Run Initialization - Called once per simulation
    # -----------------------------------------------------------------------
    if DYNAMIC == RUNINIT:
        SECTION = "!*ROOT"
        # Open input file and read root growth parameters
        try:
            with open(FILECC, "r") as file:
                lines = file.readlines()
        except IOError:
            raise ValueError(f"Error opening file: {FILECC}")

        # Find and Read Photosynthesis Section
        found = False
        for i, line in enumerate(lines):
            if line.startswith(SECTION):
                found = True
                break

        if not found:
            raise ValueError(f"Section {SECTION} not found in {FILECC}")

        try:
            RTNO3, RTNH4 = map(float, lines[i + 3].split()[:2])
        except:
            raise ValueError(f"Error reading RTNO3 and RTNH4 in {FILECC}")

    # -----------------------------------------------------------------------
    # Seasonal initialization - run once per season
    # -----------------------------------------------------------------------
    elif DYNAMIC == SEASINIT:
        TRNO3U = 0.0
        TRNH4U = 0.0
        TRNU = 0.0
        UNH4 = [0.0] * NLAYR
        UNO3 = [0.0] * NLAYR

    # -----------------------------------------------------------------------
    # DAILY RATE/INTEGRATION
    # -----------------------------------------------------------------------
    elif DYNAMIC == INTEGR:

        TRNU = 0.0
        TRNO3U = 0.0
        TRNH4U = 0.0
        NUF = 0.0
        XMIN = 0.0
        RNO3U = [0.0] * NLAYR
        RNH4U = [0.0] * NLAYR
        UNH4 = [0.0] * NLAYR
        UNO3 = [0.0] * NLAYR
        SNO3 = [NO3[l] / KG2PPM[l] for l in range(NLAYR)]
        SNH4 = [NH4[l] / KG2PPM[l] for l in range(NLAYR)]

        # Determine crop N demand (kg N/ha)
        ANDEM = (NDMTOT - NDMSDR) * 10.0

        if ANDEM > 1e-9:

            # Calculate potential N uptake in soil layers with roots
            for l in range(NLAYR):
                if RLV[l] > 1e-6:
                    FNH4 = 1.0 - math.exp(-0.08 * NH4[l])
                    FNO3 = 1.0 - math.exp(-0.08 * NO3[l])
                    if FNO3 < 0.04: FNO3 = 0.0
                    if FNO3 > 1.0:  FNO3 = 1.0
                    if FNH4 < 0.04: FNH4 = 0.0
                    if FNH4 > 1.0:  FNH4 = 1.0

                    # Relative drought factor
                    SMDFR = (SW[l] - LL[l]) / (DUL[l] - LL[l]) if SW[l] > LL[l] else 0.0
                    if SW[l] > DUL[l]:
                        SMDFR = 1.0 - (SW[l] - DUL[l]) / (SAT[l] - DUL[l])

                    RFAC = RLV[l] * SMDFR ** 2 * DLAYR[l] * 100.0

                    RNO3U[l] = max(0.0, RFAC * FNO3 * RTNO3)
                    RNH4U[l] = max(0.0, RFAC * FNH4 * RTNH4)

                    TRNU += RNO3U[l] + RNH4U[l]

            # Calculate N uptake in soil layers based on demand
            ANDEM = min(ANDEM, TRNU)

            if TRNU > 0.001:
                NUF = ANDEM / TRNU
                for l in range(NLAYR):
                    if RLV[l] > 0.0:
                        UNO3[l] = RNO3U[l] * NUF
                        UNH4[l] = RNH4U[l] * NUF

                        XMIN = 0.25 / KG2PPM[l]
                        MXNO3U = max(0.0, SNO3[l] - XMIN)
                        UNO3[l] = min(UNO3[l], MXNO3U)

                        XMIN = 0.5 / KG2PPM[l]
                        MXNH4U = max(0.0, SNH4[l] - XMIN)
                        UNH4[l] = min(UNH4[l], MXNH4U)

                        TRNO3U += UNO3[l]
                        TRNH4U += UNH4[l]

                # Convert uptake to g/m^2
                TRNO3U /= 10.0
                TRNH4U /= 10.0
                TRNU = TRNO3U + TRNH4U
    return

# !-----------------------------------------------------------------------
# !       Variable definitions
# !-----------------------------------------------------------------------
# ! ANDEM    Total crop N demand (kg[N]/ha)
# ! CHAR     Contains the contents of last record read
# ! DLAYR(L) Soil thickness in layer L (cm)
# ! DUL(L)   Volumetric soil water content at Drained Upper Limit in soil
# !            layer L (cm3 [H2O] /cm3 [soil])
# ! ERR      Error code for file operation
# ! ERRKEY   Subroutine name for error file
# ! FILECC   Path plus filename for species file (*.spe)
# ! FNH4     Potential NH4 availability factor
# ! FNO3     Potential NO3 availability factor
# ! KG2PPM(L) Conversion factor to switch from kg [N] / ha to ug [N] / g
# !            [soil] for soil layer L
# ! LL(L)    Volumetric soil water content in soil layer L at lower limit
# !            ( cm3/cm3)
# ! LUNCRP   Logical unit number for FILEC (*.spe file)
# ! MXNH4U   Maximum NH4 uptake from soil (kg N/ha)
# ! MXNO3U   Maximum NO3 uptake from soil (kg N/ha)
# ! NDMSDR   Amount of Mobilized N which can be used for seed growth
# !            (g[N] / m2 / d)
# ! NDMTOT   Total N demand (g[N] / m2 / d)
# ! NH4(L)   Ammonium N in soil layer L (µg[N] / g[soil])
# ! NL       maximum number of soil layers = 20
# ! NLAYR    Number of soil layers
# ! NO3(L)   Nitrate in soil layer L (µg[N] / g[soil])
# ! NUF      N uptake fraction (ratio of demand to N uptake), <= 1.0
# ! RFAC     Nitrogen uptake conversion factor ((kg N/ha) / (mg N / cm root))
# ! RLV(L)   Root length density for soil layer L ((cm root / cm3 soil))
# ! RNH4U(L) Ammonium uptake (kg N/ha)
# ! RNO3U(L) Nitrate uptake (kg N/ha)
# ! RTNH4    Ammonium uptake per unit root length (mg N / cm)
# ! RTNO3    Nitrate uptake per unit root length (mg N / cm)
# ! SAT(L)   Volumetric soil water content in layer L at saturation
# !            (cm3 [water] / cm3 [soil])
# ! SMDFR    Relative drought factor
# ! SNH4(L)  Total extractable ammonium N in soil layer L (kg [N] / ha)
# ! SNO3(L)  Total extractable nitrate N in soil layer L (kg [N] / ha)
# ! SW(L)    Volumetric soil water content in layer L
# !            (cm3 [water] / cm3 [soil])
# ! TRNH4U   Total N uptake in ammonium form in a day (g[N] / m2 / d)
# ! TRNO3U   Total N uptake in nitrate form in a day (g[N] / m2 / d)
# ! TRNU     Total N uptake in a day (kg[N] / ha / d)
# ! UNH4     Uptake of NH4 from soil (interim value) (kg N/ha)
# ! UNO3     Uptake of NO3 from soil (interim value) (kg N/ha)
# ! XMIN     Amount of NH4 that cannot be immobilized but stays behind in
# !            soil as NH4; Also, Amount of NO3 that cannot denitrify but
# !            stays behind in the soil as NO3 (kg [N] / ha)
# !-----------------------------------------------------------------------
# !       END SUBROUTINE NUPTAK
# !=======================================================================
