from ModuleDefs import *

# Vstage global variables
vstage:float = 0.0
rvstge:float = 0.0
vstged:float = 0.0
vstagp:float = 0.0

      # CHARACTER*1 PLME
      # INTEGER DYNAMIC
      # INTEGER DAS, NVEG0, NVEG1, NDVST
      # INTEGER YRPLT, YRDOY
      # REAL VSTAGE, RVSTGE, VSTGED, VSTAGP
      # REAL MNEMV1, TRIFOL, EVMODC, EVMOD, DTX
      # REAL TURFAC, XPOD
      # REAL PHZACC(20)

def vstages(
        das:int, dtx:float, evmodc:float, mnemv1:float, ndvst:int,  # Input
        nveg0:int, nveg1:int, phzacc:list, plme:str, trifol:float,  # Input
        turfac:float, xpod:float, yrdoy:int, yrplt:int,  # Input
        dynamic:int  # Control
):
    """
    Args:
        das: Days after simulation (integer)
        dtx: Thermal time that occurs in a float day based on vegetative
             development temperature function (thermal days / day) (float)
        evmodc: Modifier of rate of vegetative node appearance for the first
                few nodes, primarily used for peanut (float)
        mnemv1: Minimum time from emergence to unifoliate (V1) (thermal days) (float)
        ndvst: Day on which last main stem node formed (days) (integer)
        nveg0: Day of emergence (days) (integer)
        nveg1: 1st day with 50% of plants w/ completely unrolled leaf at
               unifoliate node (days) (integer)
        phzacc: Physiological accumulator (list of 20 float numbers)
        plme: Planting method ('T' for transplant, etc.)
        trifol: Rate of appearance on leaves on mainstem. Maximum rate of
                V-stage formation (leaves per thermal day) (float)
        turfac: Water stress factor for expansion (0 - 1) (float)
        xpod: Fraction of growth partitioned to pods which slows node appearance (float)
        yrdoy: Year and day of year (YYDDD)(integer)
        yrplt: Planting dat (YYDDD) (integer)
        dynamic: Control variable to determine what part of the routine to run (integer)

    Parameters changed:
        vstage: Number of nodes on main stem of plant (float)
        rvstge: Rate of VSTAGE change (nodes/day) (float)
        vstged: Duration of time for new node development (days/node) (float)
        vstagp: Previous VSTAGE (float)
    """

    global vstage, rvstge, vstged, vstagp

    # Seasonal initialization - run once per season
    if dynamic == SEASINIT:
        vstage = 0.0
        rvstge = 0.0
        vstged = 0.0
        vstagp = 0.0

    # Daily Rate Calculations
    if dynamic == RATE:
        rvstge = 0.0

        if das >= nveg0 and das <= ndvst + round(vstged):
            if das > ndvst and vstged > 0.001:
                rvstge = 1.0 / vstged
            else:
                rvstge = vstage - vstagp

    # Daily Integration
    if dynamic == INTEGR:
        # V-stage between emergence and unifoliate (V-1)
        if rvstge > 1e-6:
            vstged = 1.0 / rvstge
        else:
            vstged = 0.0

        vstagp = vstage

        # V-Stage for transplants
        if plme == 'T' and yrplt == yrdoy:
            vstage = 1.0 + (phzacc[1] - mnemv1) * trifol  # Python uses zero-based indexing

        # Growth phase calculations
        if das >= nveg0 and das <= ndvst:
            if das < nveg1:
                vstage = phzacc[1] / mnemv1
            else:
                if vstage < abs(evmodc) and abs(evmodc) > 0.0001:
                    evmod = 1.0 + (abs(evmodc) - vstage) / evmodc
                    evmod = min(2.0, evmod)
                    evmod = max(0.0, evmod)
                else:
                    evmod = 1.0
                vstage += dtx * trifol * evmod * turfac * (1.0 - xpod)
