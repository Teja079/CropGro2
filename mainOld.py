# Translating phenol subroutine to Python
# Need to be changed to function later
# Might need to change rate, integr into seperate functions

from Ipphenol import ipphenol # Read function are being used instead

# Import required modules
from ModuleDefs import *
from Read import readX,readCUL,readSPE,readECO
import numpy as np
import Rstages as rs
import Vstages as vs
import Phenol as pl
from DSSATUtils import curv

# Suppress scientific notation when printing numpy arrays
np.set_printoptions(suppress=True)

# Initialize arrays of zeros.
TB = np.zeros([5], dtype=float)
TO1 = np.zeros([5], dtype=float)
TO2 = np.zeros([5], dtype=float)
TM = np.zeros([5], dtype=float)
ctmp = np.zeros([20], dtype='U3')   # 'U3' is three character strings
dltyp = np.zeros([20], dtype='U3')
nprior = np.zeros([20], dtype=int)
tselc = np.zeros([20], dtype=int)
nsenp = np.zeros([20], dtype=float)
phthrs = np.zeros([20], dtype=float)
psenp = np.zeros([20], dtype=float)
wsenp = np.zeros([20], dtype=float)


# Phenol Original Inputs
#       SUBROUTINE PHENOL(CONTROL, ISWITCH,
#      &    DAYL, NSTRES, PStres2, SOILPROP, ST,            !Input
#      &    SW, SWFAC, TGRO, TMIN, TURFAC, XPOD, YRPLT,     !Input
#      &    CropStatus,                                     !Output
#      &    DRPP, DTX, DXR57, FRACDN, MDATE, NDLEAF,        !Output
#      &    NDSET, NR1, NR2, NR5, NR7, NVEG0, PHTHRS,       !Output
#      &    RSTAGE, RVSTGE, STGDOY, SeedFrac, TDUMX,        !Output
#      &    TDUMX2, VegFrac, VSTAGE, YREMRG, YRNR1,         !Output
#      &    YRNR2, YRNR3, YRNR5, YRNR7)                     !Output

# -------------------------Temporarily set necessary inputs for Phenol--------------------------------------
control = ControlClass(DAS=0, DYNAMIC=2, YRDOY=0, YRSIM=0,
                       workdir='C:/DSSAT48/Tomato/',exp_code='ABGA2401',CROP='TM', CropModel = 'GRO')
iswitch = SwitchClass(ISWWAT='N',ISIMI='S')
dayl = 0.0
nstres = 1.0
pstres2 = 0.0
# soilprop = SoilClass()
st = 20*[0.0]
sw = 20*[0.0]
swfac = 0.0
tgro = 20*[0.0]
tmin = 0.0
turfac = 0.0
xpod : float = 0.0
yrplt = -9999

dlayr = 20*[0.0]
dul = 20*[0.0]
ll = 20*[0.0]
nlayr = 15
sat = 20*[0.0]

(crop_status, dtx, dxr57, fracdn, mdate, ndleaf, ndset, nr1, nr2, nr5, nr7,
 nveg0, phthrs, rstage, rvstge, stgdoy, seed_frac, tdumx, tdumx2, veg_frac,
 vstage, yremrg, yrnr1, yrnr2, yrnr3, yrnr5, yrnr7) = pl.phenol(control, iswitch, dayl, nstres, pstres2,
                                                                st, sw, swfac, tgro, tmin, turfac, xpod, yrplt,
                                                                dlayr, dul, ll, nlayr, sat)

# _____________________Phenol subroutine____________________

# Initialize control and other variables
blank = ' '
errkey = 'IPPHEN'
workdir = control.workdir
exp_code = control.exp_code
crop = control.CROP
cmodel = control.CropModel
dynamic = control.DYNAMIC
yrdoy = control.YRDOY
yrsim = control.YRSIM
das = control.DAS

#Initialize switch variables
iswwat = iswitch.ISWWAT
isimi = iswitch.ISIMI


#----------------Ipphenol section-----------------------------------
# Phenology parameter dictionaries
exp_dict = {'trtNum': 1 }
cul_dict = {}
spe_dict = {'nprior':nprior, 'dltyp': dltyp,'ctmp':ctmp,
            'tselc':tselc,'wsenp':wsenp,
            'nsenp':nsenp,'psenp':psenp }
CT1 = {'TB':0,'TO1':0,'TO2':0,'TM':0}
CT2 = {'TB':0,'TO1':0,'TO2':0,'TM':0}
CT3 = {'TB':0,'TO1':0,'TO2':0,'TM':0}
leaf_dict = {'FINREF':0,'SLAREF':0,'SIZREF':0,'VSSINK':0,'EVMODC':0,
             'SLAMAX':0,'SLAMIN':0,'SLAPAR':0,'TURSLA':0,'NSLA':0}
eco_dict = {}

# Call read statements to initialize dictionaries
readX(workdir,exp_code,crop,exp_dict)
readCUL(crop,exp_dict['cult_code'],cmodel,cul_dict)
readSPE(crop,cmodel,13,spe_dict,leaf_dict,CT1,CT2,CT3)
readECO(crop,cmodel,cul_dict['ECO#'],eco_dict)

#readX testing
print('\n--------------readx-------------------')
print(exp_dict)

#readCUL testing
print('\n--------------readCUL-------------------')
print(cul_dict)

#readSPE testing
print('\n--------------readSPE-------------------')
for i in spe_dict:
    print(i,": ", spe_dict[i],"\n")
print('\nLeaf Dictionary:\n',leaf_dict)
print('\nCardinal Temperatures:\n',CT1)
print(CT2),
print(CT3)

#readECO testing
print('\n--------------readECO-------------------')
print(eco_dict)

#Print phthrs
# print('\nphthrs list:')
# for (i, item) in enumerate(phthrs, start=1):
#     print(i,"| ",item)

# Ipphenol section (initialization section of ipphenol)
# Initializes required phenology variables from dictionaries
# Experiment
plme = exp_dict['PLME']
sdepth = float(exp_dict['PLDP'])
sdage = float(exp_dict['PAGE'])
atemp = float(exp_dict['PENV'])

# Cultivar
econo = cul_dict['ECO#']
csdvar = cul_dict['CSDL']
ppsen = cul_dict['PPSEN']
ph2t5 = cul_dict['EM-FL']
phthrs[5], phthrs[7], phthrs[9], phthrs[12] = (cul_dict['FL-SH'],cul_dict['FL-SD'],
                                               cul_dict['SD-PM'],cul_dict['FL-LF'])
if crop != 'FA':
    # Leaf
    evmodc = leaf_dict['EVMODC']
    # Species
    TB[:3] = CT1["TB"],CT2["TB"],CT3["TB"]
    TO1[:3] = CT1["TO1"],CT2["TO1"],CT3["TO1"]
    TO2[:3] = CT1["TO2"],CT2["TO2"],CT3["TO2"]
    TM[:3] = CT1["TM"],CT2["TM"],CT3["TM"]
    nprior = spe_dict['nprior']
    dltyp  = spe_dict['dltyp']
    ctmp   = spe_dict['ctmp']
    tselc  = spe_dict['tselc']
    wsenp  = spe_dict['wsenp']
    nsenp  = spe_dict['nsenp']
    psenp  = spe_dict['psenp']

    # Ecotype
    ecotyp = eco_dict['@ECO#']
    econam = eco_dict['ECONAME']
    ivrgrp = eco_dict['MG']
    ivrtem = eco_dict['TM']
    thvar  =  eco_dict['THVAR']
    pm06   = eco_dict['PM06']
    pm09   = eco_dict['PM09']
    trifol = eco_dict['TRIFL']
    r1ppo  = eco_dict['R1PPO']
    optbi  = eco_dict['OPTBI']
    slobi  = eco_dict['SLOBI']

    phthrs[:4] = [eco_dict['PL-EM'],eco_dict['EM-V1'],eco_dict['V1-JU'],eco_dict['JU-R0']]
    phthrs[10:12] = [eco_dict['R7-R8'],eco_dict['FL-VS']]

    phthrs[4] = max(0.0, ph2t5 - phthrs[2] - phthrs[3])
    phthrs[6] = phthrs[5] + max(0., (phthrs[7] - phthrs[5]) * pm06)
    phthrs[8] = max(0., phthrs[9] * pm09)

    if ppsen >= 0.0:
        cldvar = csdvar + (1. - thvar) / max(ppsen, 1e-6)
    else:
        cldvar = csdvar + (1. - thvar) / min(ppsen, -1e-6)

    csdvrr = csdvar - r1ppo
    cldvrr = cldvar - r1ppo
#_____________________________________End of Ipphenol section __________________________________________________

#Print phthrs list
print('\nphthrs list:')
for (i, item) in enumerate(phthrs, start=1):
    print(i,"| ",item)

# -----------------------------------------------------------------------
# Set minimum days for phenological events under optimum conditions
# (temperature and short photoperiod)
# -----------------------------------------------------------------------
if crop != 'FA':
    # Minimum days from emergence to Vegetative Growth Stage 1:
    mnemv1 = phthrs[1]

    # Minimum days from start of flowering to last leaf appearance:
    mnflll = phthrs[12]

    # Number of days from flowering to harvest maturity
    mnflhm = phthrs[7] + phthrs[9] + phthrs[10]

# ***********************************************************************
# ***********************************************************************
# Seasonal initialization - run once per season
# ***********************************************************************
if dynamic == SEASINIT:
    # -----------------------------------------------------------------------
    # Initialization variables from INPLNT
    # -----------------------------------------------------------------------
    drpp = 0.0
    dtx = 0.0
    dxr57 = 0.0
    fracdn = 0.0
    tdumx = 0.0
    tdumx2 = 0.0
    tntfac = 0.0
    tntfc2 = 0.0

    # Initialize lists with 20 elements
    fnstr = 20 * [1.0]
    fpstr = 20 * [1.0]
    fsw = 20 * [1.0]
    ft = 20 * [0.0]
    fuday = 20 * [0.0]


    rs.rstages(control, fnstr, fpstr, fsw, ft, fuday, isimi, nprior, phthrs, plme, sdepth, yrdoy, yrplt, yrsim)
    # Unpack variables
    crop_status = rs.crop_status
    jpend = rs.jpend
    mdate = rs.mdate
    ndleaf = rs.ndleaf
    ndset = rs.ndset
    ndvst = rs.ndvst
    nvalph = rs.nvalph
    nveg0 = rs.nveg0
    nveg1 = rs.nveg1
    nr1 = rs.nr1
    nr2 = rs.nr2
    nr5 = rs.nr5
    nr7 = rs.nr7
    phzacc = rs.phzacc
    rstage = rs.rstage
    stgdoy = rs.stgdoy
    seed_frac = rs.seed_frac
    veg_frac = rs.veg_frac
    yremrg = rs.yremrg
    yrnr1 = rs.yrnr1
    yrnr2 = rs.yrnr2
    yrnr3 = rs.yrnr3
    yrnr5 = rs.yrnr5
    yrnr7 = rs.yrnr7

    vs.vstages(
            das, dtx, evmodc, mnemv1, ndvst,  # Input
            nveg0, nveg1, phzacc, plme, trifol,  # Input
            turfac, xpod, yrdoy, yrplt,  # Input
            dynamic  # Control
    )
    rvstge = vs.rvstge
    vstage = vs.vstage

if dynamic == RATE: #Suppose to be elif
    # Emergence Phase Only
    if nveg0 > das:
        fuday[0] = 1.0
        fnstr[0] = 1.0
        fpstr[0] = 1.0
        k = tselc[0]

        # Compute average soil temp and water in top 10 cm for emergence phase
        xdep = 0.0
        swfem = 0.0
        tsdep = 0.0

        for i in range(nlayr):
            xdepl = xdep
            xdep = xdep + dlayr[i]
            dtry = min(dlayr[i], 10.0 - xdepl)

            if iswwat == 'Y':
                if sw[i] <= dul[i]:
                    swfem += dtry * (max(sw[i] - ll[i], 0.0)) / (dul[i] - ll[i])
                else:
                    swfem += dtry * (max(sat[i] - sw[i], 0.0)) / (sat[i] - dul[i])

            tsdep += dtry * st[i]

            if xdep >= 10.0:
                break

        tsdep = tsdep / 10.0

        # Compute temperature and soil water effects for phase 1, emergence
        ft[0] = curv(ctmp[0], TB[k], TO1[k], TO2[k], TM[k], tsdep)

        if iswwat == 'Y':
            swfem = (swfem / 10.0) * 100.0
            fsw[0] = curv('LIN', 0.0, 20.0, 100.0, 1000.0, swfem)
        else:
            fsw[0] = 1.0

        fsw[0] = 1.0 + (1.0 - fsw[0]) * wsenp[0]

    # Compute dev rates for all other phases, using hourly air temp
    for j in range(1, NPHS):
        k = tselc[j]
        ft[j] = 0.0
        for i in range(TS):
            fthr = curv(ctmp[j], TB[k], TO1[k], TO2[k], TM[k], tgro[i])
            ft[j] += fthr / float(TS)

        if das < nr1:
            fuday[j] = curv(dltyp[j], 1.0, csdvar, cldvar, thvar, dayl)
        else:
            fuday[j] = curv(dltyp[j], 1.0, csdvrr, cldvrr, thvar, dayl)

        fsw[j] = 1.0 + (1.0 - swfac) * wsenp[j]
        fnstr[j] = 1.0 + (1.0 - nstres) * nsenp[j]
        fpstr[j] = 1.0 + (1.0 - pstres2) * psenp[j]

    # Transplants
    if plme == 'T' and yrplt == yrdoy:
        k = tselc[1]
        ft[1] = curv(ctmp[1], TB[k], TO1[k], TO2[k], TM[k], atemp)
        phzacc[1] = ft[1] * sdage

    # The effect of Tmin on development rate from emergence to flowering
    zmodte = 1.0
    if tmin < optbi:
        zmodte = 1.0 - (slobi * (optbi - tmin))
        zmodte = max(0.0, zmodte)
        zmodte = min(1.0, zmodte)

    ft[3] *= zmodte
    ft[4] *= zmodte

    # Compute rates of development for vegetative, early rep, and late rep temp sensitivities
    dtx = ft[1]
    tntfac = ft[5]
    tntfc2 = ft[9]

    # DRPP affects seed & shell numbers, seed growth rates, etc.
    drpp = fuday[5]
    tdumx = tntfac * drpp
    tdumx2 = tntfc2 * fuday[9]

    # Calculate rate of V-stage change for height and width determination
    vs.vstages(
        das, dtx, evmodc, mnemv1, ndvst,  # Input
        nveg0, nveg1, phzacc, plme, trifol,  # Input
        turfac, xpod, yrdoy, yrplt,  # Input
        dynamic  # Control
    )
    rvstge = vs.rvstge
    vstage = vs.vstage

elif dynamic == INTEGR:
    # Check to see if stages occur today, and set them in RSTAGES
    # Call the rstages function
    rs.rstages(control, fnstr, fpstr, fsw, ft, fuday, isimi, nprior, phthrs, plme, sdepth, yrdoy, yrplt, yrsim)
    # Unpack rs attributes into local variables in one line
    (crop_status, jpend, mdate, ndleaf, ndset, ndvst, nvalph, nveg0, nveg1, nr1, nr2, nr5, nr7,
     phzacc, rstage, stgdoy, seed_frac, veg_frac, yremrg, yrnr1, yrnr2, yrnr3, yrnr5, yrnr7) = (
        rs.crop_status, rs.jpend, rs.mdate, rs.ndleaf, rs.ndset, rs.ndvst, rs.nvalph, rs.nveg0,
        rs.nveg1, rs.nr1, rs.nr2, rs.nr5, rs.nr7, rs.phzacc, rs.rstage, rs.stgdoy, rs.seed_frac,
        rs.veg_frac, rs.yremrg, rs.yrnr1, rs.yrnr2, rs.yrnr3, rs.yrnr5, rs.yrnr7
    )

    # Special accumulators
    if das >= nr1:
        fracdn = phzacc[12] / mnflll
        fracdn = min(1.0, fracdn)

    if das > nr5:
        dxr57 = phzacc[9] / phthrs[9]
        dxr57 = min(dxr57, 1.0)
    else:
        dxr57 = 0.0

    # Calculate V-stages
    vs.vstages(
        das, dtx, evmodc, mnemv1, ndvst,  # Input
        nveg0, nveg1, phzacc, plme, trifol,  # Input
        turfac, xpod, yrdoy, yrplt,  # Input
        dynamic  # Control
    )
    rvstge = vs.rvstge
    vstage = vs.vstage

# Testing curve function
#print(curv('LIN',7,22,28,48, 12.87181))

#!-----------------------------------------------------------------------
# !     PHENOLOGY VARIABLES LIST
# !-----------------------------------------------------------------------
# ! atemp     Temperature of transplant environment (°C)
# ! cldvar    Critical daylength above which development rate remains at min
# !             value (prior to flowering) (hours)
# ! cldvrr    Critical daylength above which development rate remains at min
# !             value (after flowering) (hours)
# ! crop      Crop identification code
# ! csdvar    Critical daylength above which development rate decreases
# !             (prior to flowering) (hours)
# ! csdvrr    Critical daylength above which development rate decreases
# !             (after flowering) (hours)
# ! ctmp(i)   Type of curve used for temp. function for phase I: LIN=linear,
# !             QDR=quadratic, SIN=sine function
# ! curv      Function subroutine
# ! das       Days after start of simulation (days)
# ! dayl      Day length on day of simulation (from sunrise to sunset) (hr)
# ! dlayr(l)  Soil thickness in layer L (cm)
# ! dltyp(i)  Type of curve used for daylength function for phase I:  NON=no
# !             photoperiod sensitivity, INL=inverse linear
# ! drpp      Photoperiod days which occur in a real day
# !             (photoperiod days / day)
# ! dtry      Effective depth of current soil layer (cm)
# ! dtx       Thermal time that occurs in a real day based on vegetative
# !             development temperature function (thermal days / day)
# ! dul(l)    Volumetric soil water content at Drained Upper Limit in soil
# !             layer L (cm3 [H2O] /cm3 [soil])
# ! dxr57     Relative time between first seed (NR5) and physiological
# !             maturity (NR7)
# ! evmod     Modifies rate of development - see EVMODC
# ! evmodc    Modifier of rate of vegetative node appearance for the first
# !             few nodes, primarily used for peanut
# ! fnstr(i)  Nitrogen stress function (0 to 1) for phase I
# ! fpstr(i)  Phosphorus stress function (0 to 1) for phase I
# ! fracdn    Relative time between flowering (NR1) and last leaf appearance
# !             (NDLEAF)
# ! fsw(i)    Water stress function (0.0 to 1.0) for phase I
# ! ft(i)     Temperature function (0-1) for phase I
# ! fthr      Used to calculate hourly air temperature (°C)
# ! fuday(i)  Effect of daylength on development progress (0-1) for phase I
# ! isimi      Start of simulation code
# !               E = On reported emergence day
# !               I = When initial conditions measured
# !               P = On reported planting date
# !               S = On specified date
# ! iswwat     Simulation control switch
# !               Y = Simulate water process
# !               N = Don't simulate water process
# ! jpend     Day when juvenile phase ends and plants first become sensitive
# !             to photoperiod (days)
# ! ll(l)     Volumetric soil water content in soil layer L at lower limit
# !             (cm3/cm3)
# ! mnemv1    Minimum time from emergence to unifoliate (V1) (thermal days)
# ! mnflhm    Minimum time from flowering to harvest maturity (thermal days)
# ! mnflll    Minimum time from flowering to last leaf (thermal days)
# ! ndleaf    Day when leaf expansion ceased (days)
# ! ndset     Normal time by which a pod load (full number) should be
# !             achieved with no water stress (days)
# ! ndvst     Day on which last main stem node formed (days)
# ! nl        Maximum number of soil layers = 20
# ! nphs      Number of plant phases
# ! nprior(i) The phase of growth at which phase I accumulator can start
# ! nr1       Day when 50% of plants have at least one flower (days)
# ! nr2       Day when 50% of plants have one peg (peanuts only) (days)
# ! nr5       Day when 50% of plants have pods with beginning seeds (days)
# ! nr7       Day when 50% of plants first have yellowing or maturing pods
# !             (days)
# ! nsenp(i)  Sensitivity of phase I to Nitrogen stress. Varies from -1
# !             (slows dev) to +1 (hastens dev)
# ! nstres    Nitrogen stress factor (1=no stress, 0=max stress)
# ! nvalph(i) Day when stage (I) occurred. (days)
# ! nveg0     Day of emergence (days)
# ! nveg1     1st day with 50% of plants w/ completely unrolled leaf at
# !             unifoliate node (days)
# ! optbi     Temperature below which growth rate is slowed from emergence to
# !             flowering (°C)
# ! phthrs      Time that must accumulate (by phase) for the next
# !                 stage to occur (thermal or photo-thermal days)
# !                 under optimal temp. and daylength
# !                 (1): seed germination
# !                 (2): emergence to 1st true leaf
# !                 (3): emergence to end of juv. phase
# !                 (4): floral induction
# !                 (6): 1st flower to 1st peg > 0.5 cm
# !                 (8): 1st flower to 1st seed
# !                 (10): 1st seed to physiological maturity
# !                 (11): physiological and harvest maturity
# !                 (12): 1st flower to last leaf
# !                 (13): 1st flower to end of leaf growth
# ! phzacc(i) Cumulative. time of progression from the start of phase I
# !             (thermal or photothermal days)
# ! plme       Planting method (read from FILEX)
# !               T = transplant
# !               S = seed
# !               P = pre-germinated seed
# !               N = nursery
# ! rstage    Number of RSTAGES which have occurred.
# ! rvstge    Rate of VSTAGE change (nodes/day)
# ! sat(l)    Volumetric soil water content in layer L at saturation
# !             (cm3 [water] / cm3 [soil])
# ! sdage     Transplant age (days)
# ! sdepth    Planting depth (cm)
# ! slobi     Sensitivity of growth rate to minimum temperatures from
# !             emergence to flowering
# ! st(l)     Soil temperature in soil layer L (°C)
# ! stgdoy(i) Day when stage I occurred (YYDDD)
# ! stname    Output headings for specified crops
# ! sw(l)     Volumetric soil water content in layer L
# !             (cm3 [water] / cm3 [soil])
# ! swfac     Effect of soil-water stress on photosynthesis, 1.0=no stress,
# !             0.0=max stress
# ! swfem     Average soil water content in top 10 cm of soil.  Used to
# !             modify emergence rate. (cm3/cm3)
# !            TO2, and  TM Coefficients which define daily temperature distr
# ! tdumx     Photo-thermal time that occurs in a real day based on early
# !             reproductive development temperature function
# !             (photo-thermal days / day)
# ! tdumx2    Photo-thermal time that occurs in a real day based on late
# !             reproductive development temperature function
# !             (photo-thermal days / day)
# ! tgro(i)   Hourly air temperature (°C)
# ! tgroav    Average daily canopy temperature (°C)
# ! thvar     Minimum relative rate of reproductive development under long
# !             days and optimal temperature
# ! timdif    Integer function which calculates the number of days between
# !             two Julian dates (da)
# ! tmin      Minimum daily temperature (°C)
# ! tntfac    Thermal time that occurs in a single real day based on early
# !             reproductive development temperature function
# !             (thermal days / day)
# ! tntfc2    Thermal time that occurs in a single real day based on late
# !             reproductive development temperature function
# !             (thermal days / day)
# ! trifol    Rate of appearance on leaves on mainstem. Maximum rate of
# !             V-stage formation (leaves per thermal day)
# ! tsdep     Average temperature in top 10 cm of soil. Used to modify
# !             emergence rate of development. (°C)
# ! tselc      Number of temperature curve (by phase)
# !                 1 = vegetative
# !                 2 = early reproductive
# !                 3 = late reproductive
# ! turfac    Water stress factor for expansion (0 - 1)
# ! vstage    Number of nodes on main stem of plant
# ! vstagp    Previous VSTAGE
# ! vstged    Duration of time for new node development (days/node)
# ! wsenp(i)  Sensitivity of phase I to water stress, varying from -1 (slows
# !             dev) to 1 (hastens dev)
# ! xdep      Depth to bottom of current soil layer (cm)
# ! xdepl     Depth to top of current soil layer (cm)
# ! xpod      Growth partitioning to pods which slows node appearance
# !             (fraction)
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
# ! zmodte    Factor which accounts for effect of TMIN on rate of development
# !             from emergence to flowering
# C=======================================================================

#   Old code
#         ecotyp, econam = c255[0:6].strip(), c255[7:26].strip()
#         vals = c255[24:].split()
#         ivrgrp, ivrtem = int(vals[0]), int(vals[1])
#         thvar, phthrs[:4], pm06, pm09, phthrs[10:12], trifol, r1ppo, optbi, slobi = \
#             (float(vals[2]),
#             [float(i) for i in vals[3:7]],
#             float(vals[7]),
#             float(vals[8]),
#             [float(i) for i in vals[10:12]],
#              float(vals[12]),
#             float(vals[14]),
#             float(vals[15]),
#             float(vals[16]))


# # ipphenol testing
# print('\n--------------ipphenol-------------------')
# test = ipphenol(ControlDict)
# for var in test:
#     print(var)
# print(test[9])


# #readCULdf testing
# print('\n--------------readCULdf-------------------')
# df = readCULdf('TM','HYDFAV','GRO')
# #df.index = df['@VAR#']
# df.set_index("@VAR#", inplace = True)
# print(df.to_string())
# print("\n",float(df.loc['HYDFAV','CSDL']))
# print(float(df.loc['HYDFAV','FL-SH']))

#Scratch section
# key = 'hello'
# mydict = {key:1}
# mydict2 = mydict
# mydict['hi'] = 2
# k = key
# k = key+'n'
# print(mydict,mydict2)
# print(key,k)
