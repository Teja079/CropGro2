# Calculating of the Mobilization of N

def MOBIL(DYNAMIC,                           #Control
     NDMNEW, NMINEP, NMOBR, RPRO, TRNU,              #Input
     WNRLF, WNRRT, WNRSH, WNRST,                     #Input
     NMINEA, NRUSLF, NRUSRT, NRUSSH, NRUSST) :       #Output

    from ModuleDefs import *

    DYNAMIC = 1
    CNMINE, NDMNEW, NMINEA, NMINEP, NMINER, NMOBR = [0.0] * 6
    NRUSLF, NRUSRT, NRUSSH, NRUSST, RPRO = [0.0] * 5
    TRNU, WNRLF, WNRRT, WNRSH, WNRST = [0.0] * 5

    # Seasonal initialization - run once per season
    if DYNAMIC == SEASINIT:
        # moved from INPLNT
        CNMINE = 0.0
        NMINEA = 0.0
        NRUSLF = 0.0
        NRUSST = 0.0
        NRUSRT = 0.0
        NRUSSH = 0.0
    # DAILY RATE / INTEGRATION
    elif DYNAMIC == INTEGR:
        CNMINE = 0.0
        NMINEA = 0.0
        NRUSLF = 0.0
        NRUSST = 0.0
        NRUSRT = 0.0
        NRUSSH = 0.0

    # Leave MOBIL with N Mined from Leaf, Stem, Root, Shell, and
    # Total Plant Tissue, and CH2O used in the Re-synthesis of Protein
    if (NDMNEW - TRNU > 1.e-5) and (NMINEP > 1.e-4):
        NMINEA = NDMNEW - TRNU
        if NMINEA > NMINEP: NMINEA = NMINEP
        NMINER = NMINEA / NMINEP * NMOBR
        NRUSLF = NMINER * WNRLF
        NRUSST = NMINER * WNRST
        NRUSRT = NMINER * WNRRT
        NRUSSH = NMINER * WNRSH
        CNMINE = NMINEA / 0.16 * RPRO        #Not used
    return

# CNMINE  Protein re-synthesis cost (g[CH2O] / m2)
# NDMNEW  Total N demand for new growth (g[N] / m2 / d)
# NMINEA  Actual Nitrogen mined from existing tissue (g[N] / m2 / d)
# NMINEP  Potential N mobilization from storage (g[N] / m2 / d)
# NMINER  Total N actually mobilized from plant in a day (g[N]/m2-d)
# NMOBR   Stage dependent N mining rate
# NRUSLF  N actually mobilized from leaves in a day (g[N]/m2-d)
# NRUSRT  N actually mobilized from roots in a day (g[N]/m2-d)
# NRUSSH  N actually mobilized from shells in a day (g[N]/m2-d)
# NRUSST  N actually mobilized from stems in a day (g[N]/m2-d)
# RPRO    Respiration required for re-synthesizing protein from mobilized N
#           (g[CH2O] / g[protein])
# TRNU    Total N uptake in a day (g[N] / m2 / d)
# WNRLF   N available for mobilization from leaves above lower limit of
#           mining (g[N] / m2)
# WNRRT   N available for mobilization from roots above lower limit of
#           mining (g[N] / m2)
# WNRSH   N available for mobilization from shells above lower limit of
#           mining (g[N] / m2)
# WNRST   N available for mobilization from stems above lower limit of
#           mining (g[N] / m2)