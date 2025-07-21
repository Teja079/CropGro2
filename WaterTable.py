# =======================================================================
#   WaterTable, Subroutine, C.H.Porter
#   Computes lateral flows necessary to maintain a managed water table depth.
#   Actual computed water table may rise above or fall below managed water
#   table.
# =======================================================================

def WaterTable(DYNAMIC, SOILPROP, SW):
    from ModuleDefs import NL, RunConstants as RC, SAVE_data
    from CapFringe import CapFringe

    WT = WaterTable
    SWDELTW = [0.0] * NL
    TOL = 0.5
    Kd = 0.5

    LatInflow = 0.0
    LatOutflow = 0.0

    DLAYR = SOILPROP.DLAYR
    DS = SOILPROP.DS
    DUL = SOILPROP.DUL
    NLAYR = SOILPROP.NLAYR
    SAT = SOILPROP.SAT
    #WCR = SOILPROP.WCR

    if DYNAMIC == RC.SEASINIT:
        WT.SW_TEMP = SW

        MgmtWTD = SAVE_data.MGMT.ICWD
        if MgmtWTD < 1.0e-6:
            MgmtWTD = 1000.

        WT.ActWTD = MgmtWTD
        TargetWTD = MgmtWTD
        SAVE_data.MGMT.WTDEP = WT.ActWTD
        SAVE_data.MGMT.WATTAB = MgmtWTD

        if MgmtWTD < DS[NLAYR - 1]:
            for L in range(NLAYR, 0, -1):
                if L == 1:
                    Top = 0.0
                else:
                    Top = DS[L - 2]
                Bottom = DS[L - 1]
                if MgmtWTD > Bottom:
                    break
                elif MgmtWTD > Top:
                    Thick = Bottom - MgmtWTD
                    WT.SW_TEMP[L - 1] = (SAT[L - 1] * Thick + DUL[L - 1] * (DLAYR[L - 1] - Thick)) / DLAYR[L - 1]
                    WT.SW_TEMP[L - 1] = max(WT.SW_TEMP[L - 1], SW[L - 1])
                else:
                    WT.SW_TEMP[L - 1] = SAT[L - 1]

        SAVE_data.WATER.WTDEP = WT.ActWTD

    elif DYNAMIC == RC.RATE:
        MgmtWTD = SAVE_data.MGMT.WATTAB
        if MgmtWTD > DS(NLAYR) and WT.ActWTD > DS[NLAYR]:
            return WT.ActWTD, LatInflow, LatOutflow, MgmtWTD, SWDELTW

        # -----------------------------------------------------------------------
        #   Compute lateral flow to maintain managed water table depth
        #   Use tolerance in cm to get target water table depth close to management
        #   depth.  If the exact management depth is used, instabilities occur due
        #   to soil water changes with other simultaneous processes.

        #   Actual water table lower than management.  Lateral Inflow calculations
        if WT.ActWTD - MgmtWTD > TOL:
            # initial guess at today's water table depth
            TargetWTD = (WT.ActWTD - MgmtWTD) * Kd + MgmtWTD

            # Water content below the target water table will be set to SAT.
            # Calculate lateral inflow needed to raise water table to target depth
            for L in range(NLAYR, 0, -1):
                if L == 1:
                    Top = 0.0
                else:
                    Top = DS[L - 2]
                Bottom = DS[L - 1]
                if TargetWTD > Bottom:
                    # This layer is entirely above the target water table; done.
                    break
                elif TargetWTD > Top:
                    # This layer is partially in target water table
                    Thick = Bottom - TargetWTD
                else:
                    # This layer is entirely within target water table.
                    Thick = DLAYR[L - 1]
                SWDELTW[L - 1] = max(0.0, (SAT[L - 1] - SW[L - 1]) * Thick / DLAYR[L - 1])
                LatInflow += SWDELTW[L - 1] * DLAYR[L - 1] * 10.0  # mm

        # -------------------------------------------------------------------------
        #   Actual water table higher than management - drawdown using Kd
        elif MgmtWTD - WT.ActWTD > TOL and MgmtWTD < 9999.0:
            # Calculate lateral outflow needed to draw water table down to specified depth

            # initial guess at today's water table depth
            TargetWTD = (WT.ActWTD - MgmtWTD) * Kd + MgmtWTD

            # Water content above the target water table will be set to DUL.
            # The capillary rise routine will then reset theta values
            # just above the water table.

            # Calculate lateral outflow needed to lower water table to target depth
            for L in range(1, NLAYR + 1):
                if L == 1:
                    Top = 0.0
                else:
                    Top = DS[L - 2]
                Bottom = DS[L - 1]
                if TargetWTD > Bottom:
                    # This layer is entirely above the target water table. Set the
                    # water content to DUL in the entire layer.
                    Thick = DLAYR[L - 1]
                elif TargetWTD > Top:
                    # This layer is partially in target water table. The top portion
                    # of the layer (above the TargetWTD) is set to DUL.
                    Thick = TargetWTD - Top
                else:
                    # This layer is entirely within target water table. Do nothing
                    # because it was in the water table yesterday, too.
                    break
                SWDELTW[L - 1] = max(0.0, -(SW[L - 1] - DUL[L - 1]) * Thick / DLAYR[L - 1])
                # Lateral outflow values are negative (so are SWDELTW here)
                LatOutflow += SWDELTW[L - 1] * DLAYR[L - 1] * 10.0  # mm

        else:
            TargetWTD = MgmtWTD

        WT.ActWTD = TargetWTD
        SAVE_data.WATER.WTDEP = WT.ActWTD

        for L in range(NLAYR):
            WT.SW_TEMP[L] = WT.SW_TEMP[L] + SWDELTW[L]

    if WT.ActWTD >= DS[NLAYR]:
        return WT.ActWTD, LatInflow, LatOutflow, MgmtWTD, SWDELTW

    # Calculate water content within capillary fringe, ThetaCap
    ThetaCap = CapFringe(WT.ActWTD, SOILPROP)

    # Update temporary soil water content with ThetaCap
    for L in range(NLAYR):
        # ThetaCap[L] = max(SW_TEMP[L], ThetaCap[L])
        DeltaSW = max(0.0, ThetaCap[L] - WT.SW_TEMP[L])
        WT.SW_TEMP[L] = WT.SW_TEMP[L] + DeltaSW
        LatInflow = LatInflow + DeltaSW * DLAYR[L] * 10.0

    # Flux in soil water content due to changes in water table and capillary flow
    for L in range(NLAYR):
        SWDELTW[L] = WT.SW_TEMP[L] - SW[L]

    return WT.ActWTD, LatInflow, LatOutflow, MgmtWTD, SWDELTW