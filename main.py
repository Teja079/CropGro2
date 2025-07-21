# Translating phenol subroutine to Python
# main.py emulates csm call to phenol

# Import required modules
from ModuleDefs import *
import numpy as np
import Phenol as pl
import pandas as pd
from Weather import daylen,tgro_gen


# Suppress scientific notation when printing numpy arrays
np.set_printoptions(suppress=True)

# -------------------------Temporarily set necessary inputs for Phenol--------------------------------------
# control = ControlClass(DAS=0, DYNAMIC=1, YRDOY=pd.Timestamp("2024-02-08"), YRSIM=pd.Timestamp("2024-02-08"),
#                        workdir='C:/DSSAT48/Tomato/',exp_code='ABGA2401',CROP='TM', CropModel = 'GRO',trtnum = 4)
# iswitch = SwitchClass(ISWWAT='N',ISIMI='S')
control = ControlType()
control.DAS = 0
control.DYNAMIC = 1
control.YRDOY = pd.Timestamp("2024-02-08")
control.YRSIM = pd.Timestamp("2024-02-08")
control.workdir = 'C:/DSSAT48/Tomato/'
control.exp_code = 'ABGA2401'
control.CROP = 'TM'
control.CropModel = 'GRO'
control.trtnum = 4

iswitch = SwitchType()
iswitch.ISWWAT = 'N'
iswitch.ISIMI = 'S'

dayl = 10.0          # Day length
nstres = 1.0        # Nitrogen stress factor, 0 is max
pstres2 = 0.0       # phosphorus stress factor?
# soilprop = SoilClass()
st = 20*[20.0]        # Soil temp at each layer
sw = 20*[0.2]       # Soil water content at each layer
swfac = 1.0         # soil water stress (0 is max)
tgro = 24*[0.0]     # Hourly air temp
tmin = 0.0          # Minimum temperature
turfac = 1.0        # Water stress factor for expansion
xpod : float = 0.0  # Fraction of growth partitioned to pods
yrplt = pd.Timestamp("2024-08-21")     # Planting date YYDDD

#Temporary soil settings
dlayr = 20*[10.0]   # Depth of each layer
dul = 20*[0.12]     # Drained upper soil water content
ll = 20*[0.03]      # Lower limit of soil water content
nlayr = 15          # Number of soil layers
sat = 20*[0.4]      # Saturated soil water content

# RUNINIT
pl.phenol(control, iswitch, dayl, nstres, pstres2, st,
          sw, swfac, tgro, tmin, turfac, xpod, yrplt,
          dlayr, dul, ll, nlayr, sat)

#SEASINIT
control.DYNAMIC = 2
pl.phenol(control, iswitch, dayl, nstres, pstres2, st,
          sw, swfac, tgro, tmin, turfac, xpod, yrplt,
          dlayr, dul, ll, nlayr, sat)

# Read weather data
xlat = 23 # Same as DSSAT input
weather_df = pd.read_excel('C:/CropGro/Weather/ABGA2402_West.xlsx')
#tgro = [(10 + 20 * math.sin(i / 24 * math.pi)) for i in range(25)] #Min 10C, max 20c
transplant_date = yrplt
lastharvest_date = pd.Timestamp("2024-10-24")

# Temporary Simulation
for d in range(0,len(weather_df)):
    year = weather_df['Date'][d].year
    doy = weather_df['Date'][d].timetuple().tm_yday


    control.YRDOY = weather_df['Date'][d]#int(str(year) + str(doy)) # Need to check for changing years
    #if control.YRDOY >= yrplt:
    if  transplant_date <= weather_df['Date'][d] <= lastharvest_date:
        control.DAS += 1                #Put here temporarily
        tmin = weather_df['Tmin'][d]
        tmax = weather_df['Tmax'][d]
        dayl,sndn,snup = daylen(doy,xlat)
        tgro = tgro_gen(dayl,sndn,snup,tmax,tmin)

        control.DYNAMIC = RATE
        pl.phenol(control, iswitch, dayl, nstres, pstres2, st,
                  sw, swfac, tgro, tmin, turfac, xpod, yrplt,
                  dlayr, dul, ll, nlayr, sat)

        control.DYNAMIC = INTEGR
        #Unload variables from Phenol
        (crop_status, dtx, dxr57, fracdn, mdate, ndleaf, ndset, nr1, nr2, nr5, nr7,
         nveg0, phthrs, rstage, rvstge, stgdoy, seed_frac, tdumx, tdumx2, veg_frac,
         vstage, yremrg, yrnr1, yrnr2, yrnr3, yrnr5, yrnr7) = pl.phenol(control, iswitch, dayl, nstres, pstres2, st,
                  sw, swfac, tgro, tmin, turfac, xpod, yrplt,
                  dlayr, dul, ll, nlayr, sat)
        print(f"rstage: {rstage}")
        print(f"rvstge: {rvstge}")
        print(f"vstage: {vstage}")

print("\n--------- Final Values ----------------")
print(f"crop_status: {crop_status}")
print(f"dtx: {dtx}")
print(f"dxr57: {dxr57}")
print(f"fracdn: {fracdn}")
print(f"mdate: {mdate}")
print(f"ndleaf: {ndleaf}")
print(f"ndset: {ndset}")
print(f"nr1: {nr1}")
print(f"nr2: {nr2}")
print(f"nr5: {nr5}")
print(f"nr7: {nr7}")
print(f"nveg0: {nveg0}")
print(f"phthrs: {phthrs}")
print(f"rstage: {rstage}")
print(f"rvstge: {rvstge}")
print(f"stgdoy: {stgdoy}") #Day when stage i occured
print(f"seed_frac: {seed_frac}")
print(f"tdumx: {tdumx}")
print(f"tdumx2: {tdumx2}")
print(f"veg_frac: {veg_frac}")
print(f"vstage: {vstage}")
print(f"yremrg: {yremrg}")

#Phenological dates in YYDDD (when 50% of plants have reached it)
print(f"yrnr1: {yrnr1} (Anthesis)") #1st flower Anthesis
print(f"yrnr2: {yrnr2}") #1st peg (Peanut Only)
print(f"yrnr3: {yrnr3} (1st Pod)") #1st pod
print(f"yrnr5: {yrnr5}") #1st seed
print(f"yrnr7: {yrnr7} (Last Harvest)") #Maturity
print(f"mdate: {mdate} (Last Harvest)") #Harvest Maturity

#Example of estimate hourly temperature
#April 11th Auburn
dayl,sndn,snup = daylen(100,23)
tgro = tgro_gen(dayl,sndn,snup,22,10)
hourlytemp_df = pd.DataFrame(tgro, columns=['Temperature'])
print(dayl)

# hourlytemp_df.to_excel("hourlytemperatureEx.xlsx", index=True)
#!-----------------------------------------------------------------------
# !     PHENOL OUTPUT VARIABLES LIST
# !-----------------------------------------------------------------------
# ! dtx       Thermal time that occurs in a real day based on vegetative
# !             development temperature function (thermal days / day)
# ! dxr57     Relative time between first seed (NR5) and physiological
# !             maturity (NR7)
# !               Y = Simulate water process
# !               N = Don't simulate water process
# ! ndleaf    Day when leaf expansion ceased (days)
# ! ndset     Normal time by which a pod load (full number) should be
# !             achieved with no water stress (days)
# ! ndvst     Day on which last main stem node formed (days)
# ! nr1       Day when 50% of plants have at least one flower (days)
# ! nr2       Day when 50% of plants have one peg (peanuts only) (days)
# ! nr5       Day when 50% of plants have pods with beginning seeds (days)
# ! nr7       Day when 50% of plants first have yellowing or maturing pods
# !             (days)
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
# ! rstage    Number of RSTAGES which have occurred.
# ! rvstge    Rate of VSTAGE change (nodes/day)
# ! stgdoy(i) Day when stage I occurred (YYDDD)
# ! sw(l)     Volumetric soil water content in layer L
# !             (cm3 [water] / cm3 [soil])
# ! tdumx     Photo-thermal time that occurs in a real day based on early
# !             reproductive development temperature function
# !             (photo-thermal days / day)
# ! tdumx2    Photo-thermal time that occurs in a real day based on late
# !             reproductive development temperature function
# !             (photo-thermal days / day)# ! vstage    Number of nodes on main stem of plant
# ! vstagp    Previous VSTAGE
# ! vstged    Duration of time for new node development (days/node)
# ! yremrg    Day of emergence (YYDDD)
# ! yrnr1     Day when 50% of plants have at least one flower (YYDDD)
# ! yrnr2     Day when 50% of plants have one peg (peanuts only) (YYDDD)
# ! yrnr3     Day when 50% of plants have at least one beginning pod (YYDDD)
# ! yrnr5     Day when 50% of plants have pods with beginning seeds (YYDDD)
# ! yrnr7     Day when 50% of plants first have yellowing or maturing pods
# !             (YYDDD)
# ! mdate     Date of harvest maturity (YYDDD)
# C=======================================================================
# Ceate temperature graph of east vs west vs outside weather. Try in R first.
# Compile potential courses for next semester