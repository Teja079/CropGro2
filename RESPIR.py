# Calculates maintainence respiration and net photosythate
#  available.  PGAVL is the net photosynthesis after maintenance
#  respiration is subtracted from gross photosynthesis.

def RESPIR(
     PG, R30C2, RES30C, TGRO, WTMAIN,                # Input
     RO, RP,                                         # Input/Output
     MAINR) :                                        # Output

    import ModuleDefs

    H = 1

    MAINR, PG, R30C2, RES30C, RO, RP, TRSFAC, WTMAIN, SCLTS = [0.0] * 9

#    REAL TGRO(TS)  dynamic array to  check

#    Temperature effect on maintenance respiration (McCree, 1974)
    TRSFAC = 0.0
    SCLTS = 24. / TS
    for H in range(1, TS+1) :
        TRSFAC = TRSFAC + (0.044 + 0.0019 * TGRO(H) + 0.001 * TGRO(H) ** 2) * SCLTS
        # 24 changed to TS on 3 July 2017 by Bruce Kimball
        # This equation look really suspicious because TRSFAC very
        # dependent on number of times through the loop!

    # Convert maintainence respiration to actual temperature. RES30C is
    # the g CH2O/g DW/hr used in maintenance respiration at 30 C.
    RO = RES30C * TRSFAC
    RP = R30C2 * TRSFAC
    MAINR = RO * WTMAIN + RP * PG

    return

#     RESPIR variables:
#-----------------------------------------------------------------------
# MAINR   Maintenance respiration (g[CH2O] / m2 / d)
# PG      Daily gross photosynthesis (g[CH2O] / m2 / d)
# R30C2   Respiration coefficient that depends on gross photosynthesis,
#           value at 30C (g[CH2O] used / g[CH2O] fixed / hr)
# RES30C  Respiration coefficient that depends on total plant mass,
#           value at 30C (g CH2O/g DW/hr)
# RO      Respiration coefficient that depends on total plant mass
#           (g[CH2O] / g[tissue])
# RP      proportion of the day's photosynthesis which is respired in the
#           maintenance process
# TGRO(I) Hourly air temperature (°C)
# TRSFAC  Temperature effect on maintenance respiration
# TS      Number of intermediate time steps (=24)
# WTMAIN  Mass of tissue assumed to require maintenance (g[tissue] / m2)