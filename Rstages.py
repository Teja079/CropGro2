from ModuleDefs import *
import pandas as pd
# Variables global to Rstages
crop_status:int = -99
nvalp0:int = -99
jpend:int = 0
mdate:int = -99
ndleaf:int = 0
ndset:int = 0
ndvst:int = 0
nveg0:int = 0
nveg1:int = 0
nr0:int = 0
nr1:int = 0
nr2:int = 0
nr3:int = 0
nr5:int = 0
nr7:int = 0
rstage:int = -99
seed_frac:float = 0.0
veg_frac:float = 0.0
veg_time:float = 0.0
yrnr1:pd.Timestamp = pd.Timestamp("0001-01-01")
yrnr2:pd.Timestamp = pd.Timestamp("0001-01-01")
yrnr3 = pd.Timestamp("0001-01-01")
yrnr5 = pd.Timestamp("0001-01-01")
yrnr7 = pd.Timestamp("0001-01-01")
phtem:float = 0.0

phzacc = [0.0]*20
prog = [0.0]*20
rem = [0.0]*20
nvalph = [0]*20
stgdoy = [9999999]*20
# Variable types in FORTRAN
      # CHARACTER*1 ISIMI, PLME
      #
      # INTEGER DYNAMIC
      # INTEGER I, J, NVALP0, DAS, YRDOY, YRPLT, YRSIM, CropStatus
      # INTEGER NDLEAF,  NDSET, NDVST, JPEND  !, TIMDIF
      # INTEGER RSTAGE,  NVEG0, NVEG1, NR0, NR1, NR2, NR3, NR5, NR7
      # INTEGER YRNR1, YRNR2, YRNR3, YRNR5, YRNR7, MDATE, YREMRG
      # INTEGER NPRIOR(20), STGDOY(20), NVALPH(20)
      #
      # REAL PHTEM, SDEPTH
      # REAL FT(20), FUDAY(20), FSW(20), FNSTR(20), FPSTR(20), PHTHRS(20)
      # REAL PHZACC(20), PROG(20), REM(20)
      # REAL SeedFrac, veg_frac, veg_time

def rstages(control:ControlType, fnstr:list, fpstr:list, fsw:list, ft:list, fuday:list,
            isimi:str,nprior:list,phthrs:list, plme:str,
            sdepth:float, yrdoy:int, yrplt:int, yrsim:int):             # Input TODO change int to pd.Timestamp for dates

    # Variables to be changed outside of function.
    global crop_status # Output
    global jpend, mdate, ndleaf, ndset, ndvst, nvalp0  # Output
    global nveg0, nveg1, nr0, nr1, nr2, nr3, nr5, nr7  # Output
    global rstage, seed_frac, veg_frac, yremrg, veg_time # Output
    global yrnr1, yrnr2, yrnr3, yrnr5, yrnr7, phtem  # Output
    global phzacc, prog, rem, nvalph, stgdoy    #Arrays

    dynamic = control.DYNAMIC
    das = control.DAS
#***********************************************************************
#     Seasonal initialization - run once per season
#***********************************************************************
    if dynamic == SEASINIT:
        # Initialization block similar to the original Fortran code
        nvalp0 = 10000
        rstage = 0

        # Initialize arrays with 20 elements
        phzacc = [0.0] * 20
        nvalph = [nvalp0] * 20
        stgdoy = [9999999] * 20
        prog = [0.0] * 20

        nvalph[0] = 1
        stgdoy[13] = yrsim
        stgdoy[14] = yrplt

        # Assigning other variables to nvalp0 or specific values
        nveg0 = nvalp0
        nveg1 = nvalp0
        jpend = nvalp0
        nr0 = nvalp0
        nr1 = nvalp0
        nr2 = nvalp0
        nr3 = nvalp0
        nr5 = nvalp0
        ndleaf = nvalp0
        ndvst = nvalp0
        ndset = nvalp0
        nr7 = nvalp0

        yrnr1 = pd.Timestamp("0001-01-01")
        yrnr2 = pd.Timestamp("0001-01-01")
        yrnr3 = pd.Timestamp("0001-01-01")
        yrnr5 = pd.Timestamp("0001-01-01")
        yrnr7 = pd.Timestamp("0001-01-01")
        mdate = pd.Timestamp("0001-01-01")
        yremrg = pd.Timestamp("0001-01-01")

        # Additional variables
        phtem = 0.0

        # For P module
        seed_frac = 0.0
        veg_frac = 0.0
        veg_time = phthrs[2] + phthrs[3] + phthrs[4] + phthrs[7]  # PHTHRS(3), PHTHRS(4), PHTHRS(5), PHTHRS(8)
    elif dynamic == INTEGR:
        # Initialization block for 'INTEGR'
        # Initialize array REM with 20 elements, all set to 1.0
        rem = [1.0] * 20
        if yrdoy == yrplt:
            stgdoy[14] = yrplt

        # - ----------------------------------------------------------------------
        #Transplants
        # - ----------------------------------------------------------------------
        if plme == 'T' and yrplt == yrdoy:
            # Update initial vegetative stage
            nveg0 = das
            nvalph[1] = nveg0
            yremrg = yrdoy
            if phzacc[1] - phthrs[1] > -1.0e-6:
                nveg1 = das
                nvalph[2] = nveg1
                phzacc[2] = phzacc[1] - phthrs[1]
                if phzacc[2] - phthrs[2] > -1.0e-6:
                    jpend = das
                    nvalph[3] = jpend
                    phzacc[3] = phzacc[2] - phthrs[2]
                    if phzacc[3] - phthrs[3] > -1.0e-6:
                        nr0 = das
                        nvalph[4] = nr0
                        rstage = 0
                        phzacc[4] = phzacc[3] - phthrs[3]
                        if phzacc[4] - phthrs[4] > -1.0e-6:
                            nr1 = das
                            nvalph[5] = nr1
                            yrnr1 = yrdoy
                            rstage = 1
                            phzacc[5] = phzacc[4] - phthrs[4]
        #-----------------------------------------------------------------------------
        #     Check for emergence, if NVEG0 has been set to less than its initial value
        #-----------------------------------------------------------------------------
        if nveg0 >= nvalp0:
            phtem = phthrs[0] + sdepth * 0.6
            prog[0] = ft[0] * fuday[0] * min(fsw[0], fnstr[0], fpstr[0])
            phzacc[0] += prog[0]

            # Check if accumulated value meets or exceeds the threshold (or ISIMI indicates 'E')
            if (phzacc[0] - phtem > -1.0e-6) or (isimi == 'E'):
                # Emergence, next stage, occurs on das
                nveg0 = das
                nvalph[1] = nveg0
                yremrg = yrdoy
                stgdoy[0] = yrdoy

                # Account for the part of today that contributes to the next phase(s)
                if isimi != 'E':
                    rem[1] = (phzacc[0] - phtem) / (prog[0] + 0.00001)

        # Check for veg stage, V1
        # -------------------------------------------------------------------------------
        # Skip accumulator section if not time to start accumulating for phase 2
        #  also skips this accumulator on day when stage 2 occurred, with
        #  initial value of PHZACC(2) already computed

        #Skip if stage 3 has occured
        if (das >= nvalph[nprior[1]]) and (nveg1 >= nvalp0):
            prog[1] = ft[1] * fuday[1] * min(fsw[1], fnstr[1], fpstr[1]) * rem[nprior[1]]
            phzacc[1] += prog[1]

            # Check if accumulated value meets or exceeds the threshold for phase 2 (PHTHRS[2])
            if phzacc[1] - phthrs[1] > -1.0e-6:
                #V1 occurs on DAS
                nveg1 = das
                nvalph[2] = nveg1
                stgdoy[1] = yrdoy
                # Account for the part of today that contributes to the next phase(s)
                rem[2] = (phzacc[1] - phthrs[1]) / (prog[1] + 0.00001)

        #-----------------------------------------------------------------------------
        #     Check for juvenile phase
        #-----------------------------------------------------------------------------
        if (das >= nvalph[nprior[2]]) and (jpend >= nvalp0):
            prog[3] = ft[2] * fuday[2] * min(fsw[2], fnstr[2], fpstr[2]) * rem[nprior[2]]
            phzacc[2] += prog[3]

            # CHP added for Plant P - 9/20/2004
            if veg_time > 0:
                veg_frac = phzacc[2] / veg_time
            else:
                veg_frac = 0.0

            if (phzacc[2] - phthrs[2]) > -1.0e-6:
                # End of juvenile phase occurs on day DAS
                jpend = das
                nvalph[3] = jpend
                stgdoy[2] = yrdoy
                rem[3] = (phzacc[2] - phthrs[2]) / (prog[3] + 0.00001)
        #-----------------------------------------------------------------------------
        #  Check for floral induction, end of induction phase
        #-----------------------------------------------------------------------------
        if (das >= nvalph[nprior[3]]) and (nr0 >= nvalp0):
            prog[3] = ft[3] * fuday[3] * min(fsw[3], fnstr[3], fpstr[3]) * rem[nprior[3]]
            phzacc[3] += prog[3]

            veg_frac = (phthrs[2] + phzacc[3]) / veg_time

            if (phzacc[3] - phthrs[3]) > 1.0e-6:
                # Floral induction occurs on day DAS, end of phase 4
                nr0 = das
                nvalph[4] = nr0
                rstage = 0
                stgdoy[3] = yrdoy
                rem[4] = (phzacc[3] - phthrs[3]) / (prog[3] + 0.00001)
        #-----------------------------------------------------------------------------
        #  Check for first flower, stage 6, end of phase 5
        #-----------------------------------------------------------------------------
        if (das >= nvalph[nprior[4]]) and (nr1 >= nvalp0):
            prog[4] = ft[4] * fuday[4] * min(fsw[4], fnstr[4], fpstr[4]) * rem[nprior[4]]
            phzacc[4] += prog[4]

            veg_frac = (phthrs[2] + phthrs[3] + phzacc[4]) / veg_time

            if (phzacc[4] - phthrs[4]) > -1.0e-6:
                # First flower occurs on day DAS
                nr1 = das
                stgdoy[4] = yrdoy
                nvalph[5] = nr1
                yrnr1 = yrdoy
                rstage = 1
                rem[5] = (phzacc[4] - phthrs[4]) / (prog[4] + 0.00001)
        #-----------------------------------------------------------------------------
        #  Check for beginning ovule (peg), stage 7, end of phase 6
        #-----------------------------------------------------------------------------
        if (das >= nvalph[nprior[5]]) and (nr2 >= nvalp0):
            prog[5] = ft[5] * fuday[5] * min(fsw[5], fnstr[5], fpstr[5]) * rem[nprior[5]]
            phzacc[5] += prog[5]

            if (phzacc[5] - phthrs[5]) > -1.0e-6:
                # First peg occurs on day DAS
                nr2 = das
                stgdoy[5] = yrdoy
                nvalph[6] = nr2
                yrnr2 = yrdoy
                rstage = 2

                # Account for the part of today that contributes to the next phase(s)
                rem[6] = (phzacc[5] - phthrs[5]) / (prog[5] + 0.00001)
        #-----------------------------------------------------------------------------
        #  Check for stage beginning shell, stage 8, end of phase 7
        #-----------------------------------------------------------------------------
        if (das >= nvalph[nprior[6]]) and (nr3 >= nvalp0):
            prog[6] = ft[6] * fuday[6] * min(fsw[6], fnstr[6], fpstr[6]) * rem[nprior[6]]
            phzacc[6] += prog[6]

            if (phzacc[6] - phthrs[6]) > -1.0e-6:
                # Stage R3 occurs on day DAS
                nr3 = das
                stgdoy[6] = yrdoy

                if stgdoy[6] == stgdoy[5]:
                    stgdoy[6] = 9999999

                nvalph[7] = nr3
                yrnr3 = yrdoy
                rstage = 3

                # Account for the part of today that contributes to the next phase(s)
                rem[7] = (phzacc[6] - phthrs[6]) / (prog[6] + 0.00001)

        #-----------------------------------------------------------------------------
        #  Check for stage beginning seed (R5), stage 9, end of phase 8
        #-----------------------------------------------------------------------------
        if (das >= nvalph[nprior[7]]) and (nr5 >= nvalp0):
            prog[7] = ft[7] * fuday[7] * min(fsw[7], fnstr[7], fpstr[7]) * rem[nprior[7]]
            phzacc[7] += prog[7]

            # veg_frac calculation, updated after phase 8
            veg_frac = (phthrs[2] + phthrs[3] + phthrs[4] + phzacc[7]) / veg_time

            # Check if Stage R5 occurs
            if (phzacc[7] - phthrs[7]) > -1.0e-6:
                # Stage R5 occurs on day DAS
                nr5 = das
                stgdoy[7] = yrdoy
                nvalph[8] = nr5
                yrnr5 = yrdoy
                rstage = 5

                # Account for the part of today that contributes to the next phase(s)
                rem[8] = (phzacc[7] - phthrs[7]) / (prog[7] + 0.00001)
        #-----------------------------------------------------------------------------
        #  Check for stage NDSET, stage 10, end of phase 9
        #-----------------------------------------------------------------------------
        if (das >= nvalph[nprior[8]]) and (ndset >= nvalp0):
            prog[8] = ft[8] * fuday[8] * max(fsw[8], fnstr[8], fpstr[8]) * rem[nprior[8]]
            phzacc[8] += prog[8]

            # Check if Stage NDSET occurs
            if (phzacc[8] - phthrs[8]) > -1.0e-6:
                # Stage NDSET occurs on day DAS
                ndset = das
                stgdoy[8] = yrdoy
                nvalph[9] = ndset

                # Account for the part of today that contributes to the next phase(s)
                rem[9] = (phzacc[8] - phthrs[8]) / (prog[8] + 0.00001)
        #-----------------------------------------------------------------------------
        #   Check for stage NR7, stage 11, end of phase 10
        #-----------------------------------------------------------------------------
        if (das >= nvalph[nprior[9]]) and (nr7 >= nvalp0):
            prog[9] = ft[9] * fuday[9] * max(fsw[9], fnstr[9], fpstr[9]) * rem[nprior[9]]
            phzacc[9] += prog[9]
            seed_frac = phzacc[9] / phthrs[9]

            # Check if Stage NR7 occurs
            if (phzacc[9] - phthrs[9]) > -1.0e-6:
                # Stage NR7, physiological maturity, occurs on day DAS
                nr7 = das
                stgdoy[9] = yrdoy
                nvalph[10] = nr7
                yrnr7 = yrdoy
                rstage = 7

                # Account for the part of today that contributes to the next phase(s)
                rem[10] = (phzacc[9] - phthrs[9]) / (prog[9] + 0.00001)
        #-----------------------------------------------------------------------------
        #   Check for stage NR8, stage 12, end of phase 11
        #-----------------------------------------------------------------------------
        if (das >= nvalph[nprior[10]]) and (mdate <= yrsim):
            prog[10] = ft[10] * fuday[10] * min(fsw[10], fnstr[10], fpstr[10]) * rem[nprior[10]]
            phzacc[10] += prog[10]

            # Check if Stage NR8 occurs
            if (phzacc[10] - phthrs[10]) > 1.0e-6:
                # Stage NR8, harvest maturity, occurs on day DAS
                stgdoy[10] = yrdoy
                nvalph[11] = das
                mdate = yrdoy
                crop_status = 1
                rstage = 8

                # Account for the part of today that contributes to the next phase(s)
                rem[11] = (phzacc[10] - phthrs[10]) / (prog[10] + 0.00001)
        # -----------------------------------------------------------------------------
        #   Check for stage NDVST, end of V-stage addition, stage 13, end of phase 12
        # -----------------------------------------------------------------------------
        if (das >= nvalph[nprior[11]]) and (ndvst >= nvalp0):
            prog[11] = ft[11] * fuday[11] * min(fsw[11], fnstr[11], fpstr[11]) * rem[nprior[11]]
            phzacc[11] += prog[11]

            # Check if Stage NDVST occurs
            if (phzacc[11] - phthrs[11]) > 1.0e-6:
                # Stage NDVST, end of V-stage addition, occurs on day DAS
                ndvst = das
                stgdoy[11] = yrdoy
                nvalph[12] = ndvst

                # Account for the part of today that contributes to the next phase(s)
                rem[12] = (phzacc[11] - phthrs[11]) / (prog[11] + 0.00001)
        # -----------------------------------------------------------------------------
        #   Check for stage NDLEAF, end of leaf growth, stage 14, end of phase 13
        # -----------------------------------------------------------------------------
        if (das >= nvalph[nprior[12]]) and (ndleaf >= nvalp0):
            prog[12] = ft[12] * fuday[12] * min(fsw[12], fnstr[12], fpstr[12]) * rem[nprior[12]]
            phzacc[12] += prog[12]

            # Check if Stage NDLEAF occurs
            if (phzacc[12] - phthrs[12]) > -1.0e-6:
                # Stage NDLEAF, end of leaf growth, occurs on day DAS
                ndleaf = das
                stgdoy[12] = yrdoy
                nvalph[13] = ndleaf

                # Account for the part of today that contributes to the next phase(s)
                rem[13] = (phzacc[12] - phthrs[12]) / (prog[12] + 0.00001)

# !------------------------------------------------------------------------
# !     Rstages Variables:
# !------------------------------------------------------------------------
# ! das       Days after start of simulation (days)
# ! fnstr(i)  Nitrogen stress function (0 to 1) for phase I
# ! fpstr(i)  Phosphorus stress function (0 to 1) for phase I
# ! fsw(i)    Water stress function (0.0 to 1.0) for phase I
# ! ft(i)     Temperature function (0-1) for phase I
# ! fuday(i)  Effect of daylength on development progress (0-1) for phase I
# ! isimi     Start of simulation code:     E = On reported emergence day, I
# !             = When initial conditions measured, P = On reported planting
# !             date, S = On specified date
# ! jpend     Day when juvenile phase ends and plants first become sensitive
# !             to photoperiod (days)
# ! ndleaf    Day when leaf expansion ceased (days)
# ! ndset     Normal time by which a pod load (full number) should be
# !             achieved with no water stress (days)
# ! ndvst     Day on which last main stem node formed (days)
# ! nprior(i) The phase of growth at which phase I accumulator can start
# ! nr0       Day when floral induction occurs (days)
# ! nr1       Day when 50% of plants have at least one flower (days)
# ! nr2       Day when 50% of plants have one peg (peanuts only) (days)
# ! nr3       Day when 50% of plants have at least one pod (days)
# ! nr5       Day when 50% of plants have pods with beginning seeds (days)
# ! nr7       Day when 50% of plants first have yellowing or maturing pods
# !             (days)
# ! nvalp0    Set to 100,000 in PHENOLOG, used for comparison of times of
# !             plant stages  (days)
# ! nvalph(i) Day when stage (I) occurred. (days)
# ! nveg0     Day of emergence (days)
# ! nveg1     1st day with 50% of plants w/ completely unrolled leaf at
# !             unifoliate node (days)
# ! phtem     Threshold time for emergence (thermal days)
# ! phthrs(i) Threshold time that must accumulate in phase I for the next
# !             stage to occur  (thermal or photothermal days)
# ! phzacc(i) Cumulative. time of progression from the start of phase I
# !             (thermal or photothermal days)
# ! plme      Planting method; T = transplant, S = seed, P = pre-germinated
# !             seed, N = nursery
# ! prog (i)  Thermal or photo-thermal time that occurs in a real day for
# !             Phase I (Thermal or photothermal days)
# ! rem (i)   Remainder of thermal or photo-thermal time after a threshold is
# !             reached on a day (to be used to start progression into the
# !             next phase) (thermal or photothermal days / day)
# ! rstage    Number of RSTAGES which have occurred.
# ! sdepth    Planting depth (cm)
# ! stgdoy(i) Day when stage I occurred (YYDDD)
# ! timdif    Integer function which calculates the number of days between
# !             two Julian dates (da)
# ! yrdoy     Current day of simulation (YYDDD)
# ! yremrg    Day of emergence (YYDDD)
# ! yrnr1     Day when 50% of plants have at least one flower (YYDDD)
# ! yrnr2     Day when 50% of plants have one peg (peanuts only) (YYDDD)
# ! yrnr3     Day when 50% of plants have at least one beginning pod (YYDDD)
# ! yrnr5     Day when 50% of plants have pods with beginning seeds (YYDDD)
# ! yrnr7     Day when 50% of plants first have yellowing or maturing pods
# !             (YYDDD)
# ! mdate     Date of harvest maturity (YYDDD)
# ! yrplt     Planting date (YYDDD)
# ! yrsim     Start of simulation date (YYDDD)
# !-----------------------------------------------------------------------
# !     End Subroutine RSTAGES
# !-----------------------------------------------------------------------