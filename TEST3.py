#C=======================================================================
#C SUBROUTINE MethaneDynamics
#C
#C Subroutine to act as interface between CERES-Rice and Arah methane model.
#C-----------------------------------------------------------------------
#C  Revision history
#C
#C  1. Written     R.B.M. March 1998.
#! 2021-06-30 CHP and US adapt for DSSAT-CSM v4.8
#! 2023-01-24 chp added SAEA to soil analysis in FileX
#! 2023-05-14 EHFMS changed the starting condition for methane production
#C=======================================================================
import numpy as np
from ModuleDefs import NL
from ModuleDefs import RunConstants as RC

def MethaneDynamics(CONTROL, ISWITCH, SOILPROP, FERTDATA, FLOODWAT, SW, RLV, newCO2, DRAIN):
    # USE GHG_mod
    # USE FloodModule
    # USE MethaneConstants
    # USE MethaneVariables
    # IMPLICIT NONE
    # EXTERNAL OpMethane, SteadyState, setup
    #  SAVE
    dlayr = np.zeros(NL, dtype=float)
    # SW = np.zeros(NL, dtype=float)
    # DLL = np.zeros(NL, dtype=float)
    # RLV = np.zeros(NL, dtype=float)
    # CSubstrate = np.zeros(NL, dtype=float)
    # BD = np.zeros(NL, dtype=float)
    Buffer = np.zeros((NL, 2), dtype=float)
    # afp = np.zeros(NL, dtype=float)
    afp = np.zeros(NL, dtype=float)
    # SAEA = np.zeros(NL, dtype=float)
    # SAT = np.zeros(NL, dtype=float)
    # DUL = np.zeros(NL, dtype=float)
#      INTEGER n1,NLAYR,i,j, DYNAMIC
#
    drain = 0.0
    flood = 0.0
    x = 0.0
    CH4Emission = 0.0
    buffconc = 0.0
    rCO2 = 0.0
    rCH4 = 0.0
    TCH4Substrate = 0.0
    rbuff = 0.0
    afpmax = 0.0
    ProductionFrac = 0.0
    ConsumptionFrac = 0.0
    EmissionFrac = 0.0
    PlantFrac = 0.0
    EbullitionFrac = 0.0
    DiffusionFrac = 0.0
    LeachingFrac = 0.0
    CH4Production = 0.0
    CH4Consumption = 0.0
    CH4PlantFlux = 0.0
    CH4Ebullition = 0.0
    CH4Diffusion = 0.0
    CH4Leaching = 0.0
    CH4Stored = 0.0
    CumCH4Consumpt = 0.0
    CumCH4Leaching = 0.0
    newCO2Tot = 0.0
    CH4_balance = 0.0
    CumCH4Consumpt = 0.0
    CumCH4Leaching = 0.0
    newCO2Tot = 0.0
    CH4_balance = 0.0
    #      REAL CumCH4Emission, CumCO2Emission, CO2emission, CumNewCO2
#      REAL StorageFlux, Cum_CH4_bal, CH4Stored_Y
#!     REAL CH4_correction !, ReductFact
#
#      REAL TCO2, TCH4, FloodCH4
#
#      TYPE (ControlType) CONTROL
#      TYPE (SwitchType)  ISWITCH
#      TYPE (SoilType)    SOILPROP
#      TYPE (FloodWatType)FLOODWAT
#      TYPE (FertType)    FERTDATA
#      TYPE (CH4_type)    CH4_data
#
#!-----------------------------------------------------------------------
    spd = 24.*3600.   #seconds per day
#!     Reference height for the Arah model to be the top of the bund
    RefHeight = 100.0 # mm
#      !1/d EHFMS changed, before was 0.06
    BufferRegenRate = 0.070
#      !EHFMS created this parameter
    frac_afpmax = 0.30
    DYNAMIC = CONTROL.DYNAMIC
    DLAYR = SOILPROP.DLAYR
    DLL   = SOILPROP.LL
    BD    = SOILPROP.BD
    NLAYR = SOILPROP.NLAYR
#
#C***********************************************************************
#C***********************************************************************
#C    Input and Initialization
#C***********************************************************************
    if DYNAMIC == RC.INIT:
#C-----------------------------------------------------------------------
        FirstTime = True
        TCO2 = 0.0
        TCH4 = 0.0
        newCO2Tot = 0.0
        CO2emission    = 0.0
        CH4Emission    = 0.0
        CH4Consumption = 0.0
        CH4Leaching    = 0.0
        CH4Stored      = 0.0
        CumCO2Emission = 0.0
        CumCH4Emission = 0.0
        CumCH4Consumpt = 0.0
        CumCH4Leaching = 0.0
        CumNewCO2     = 0.0

        CH4_data.CO2emission    = 0.0
        CH4_data.CH4Emission    = 0.0
        CH4_data.CH4Consumption = 0.0
        CH4_data.CH4Leaching    = 0.0
        CH4_data.CumCO2Emission = 0.0
        CH4_data.CumCH4Emission = 0.0
        CH4_data.CumCH4Consumpt = 0.0
        CH4_data.CumCH4Leaching = 0.0

        if CONTROL.RUN == 1 or 'QF' not in CONTROL.RNMODE:
            CH4Stored     = 0.0
            CH4Stored_Y   = 0.0
            CH4_data.CH4Stored = 0.0

#!     SAEA = Soil Alternative Electron Acceptors (mol Ceq/m3)
#!     SAEA = 26.5
            SAEA = SOILPROP.SAEA

            FloodCH4 = 0.0
            for i in range(1,NLAYR):
#!       Convert the alternate electron acceptors in each layer
#!       from mol Ceq/m3 to kgC/ha
#!       Buffer(i,1) = Buffer(i,1) * 12.*(dlayr(i)/100.)*10. ! kg Ceq/ha
                Buffer[i, 0] = SAEA[i] * 12.0 * (dlayr[i] / 100.0) * 10.0
                Buffer[i, 1] = 0.0

#!     proportionality constant for root transmissivity and RLV
#!     (0.00015 m air/(m root))
#!     lamda_rho = lamdarho  ! 0.00015
            lamda_rho = 0.00015
#
        #OpMethane(CONTROL, ISWITCH,newCO2Tot, CO2emission, TCH4Substrate, StorageFlux, CH4Stored,CH4Production, CH4Consumption, CH4Leaching, CH4Emission,CH4PlantFlux, CH4Ebullition, CH4Diffusion, CH4_balance,CumNewCO2, CumCO2Emission, CumCH4Emission, CumCH4Consumpt,CumCH4Leaching, Cum_CH4_bal)
#C***********************************************************************
#C***********************************************************************
#C     Rate Calculations
#C***********************************************************************
    elif DYNAMIC == RC.RATE:
#C-----------------------------------------------------------------------
        CO2emission = 0.0
        CH4emission = 0.0
        CH4Consumption = 0.0
        CH4Leaching    = 0.0
        CH4Production  = 0.0
        CH4PlantFlux   = 0.0
        CH4Ebullition  = 0.0
        CH4Diffusion   = 0.0

#!     Calculate total CO2 coming in from decomposition
#!     of organic matter, newCO2Tot
#!     Transfer newCO2 from soil organic matter modules
#!     to CSubstrate variable
        CSubstrate = 0.0
        newCO2Tot = 0.0
        for i in range(1, SOILPROP.NLAYR):
            if i == 1:
                newCO2Tot = newCO2[0] + newCO2[1]
                CSubstrate[1] = newCO2Tot
            else:
                CSubstrate[i] = newCO2[i]
                newCO2Tot += newCO2[i]
#
        FLOOD = FLOODWAT.FLOOD
#
#!     Arah model parameters for flood-water layer
        n1 = 2
        z[1] = (RefHeight - FLOOD)/1000.
        z[n1] = RefHeight/1000.
        for i in range(1, n1 + 1):
            theta[i]   = 1.0
            epsilon[i] = 0.0
            lamda[i]   = 0.0
            VV[om, i]  = 0.0
            VV[o2, i]  = 0.0
            VV[ch4, i] = 1.5e-5
#!     Parameters for soil layers
#!     CO2_Cflux = 0.0
        TCH4Substrate = 0.0
        CO2emission   = 0.0
#
        for i in range(1,NLAYR):
#!       calculate air-filled porosity (v/v)
            if FLOOD > 0.0:
                afp[i] = 0.0
            else:
                afp[i] = max(0.0, 1.0 - BD[i] / 2.65 - SW[i])

            afpmax = 1.0 - BD[i] / 2.65
#!       Update buffer from new fertilizer
            Buffer[i, 0] += FERTDATA.AddBuffer[i]
#!       calculate buffer concentration (molCeq/m3)
            buffconc = Buffer[i, 0] / 10.0 / 12.0 / (dlayr[i] / 100.0)
#!       calculate reoxidisation of buffer if soil is aerated
#!         IF (afp(i).GT.0.0) THEN !
#! EHFMS: Methane Prod. under conditions of partially saturated soil
            if afp[i] > frac_afpmax * afpmax:
                rCH4 = 0.0
                rCO2 = CSubstrate[i]
                rbuff = -min(BufferRegenRate * afp[i] / afpmax * Buffer[i, 1],Buffer[i, 1])
            else:
# calculate methane production
                if buffconc > 0.0:
                    rCH4 = 0.3 * (1.0 - buffconc / 24.0)
                    rCH4 *= dlayr[i] / 100.0 * 12.0 * 10.0
                else:
                    rCH4 = CSubstrate[i] / 2.0
            rCH4 = max(0.0, min(rCH4, CSubstrate(i)/2.0))
            rCO2 = CSubstrate(i) - (2.0 * rCH4)
            if rCO2 > Buffer[i, 0]:
                rCO2 = Buffer[i, 0]
                rCH4 = (CSubstrate[i] - rCO2) / 2.0

            rbuff = rCO2
            Buffer[i,1] = Buffer[i,1] - rbuff
            Buffer[i,2] = Buffer[i,2] + rbuff
#
#!       Total CH4 substrate (kgC/ha)
            TCH4Substrate = TCH4Substrate + rCH4
#
#!       Calculate soil profile parameters for Arah model
            j = i + n1
            z[j] = z[j-1] + dlayr[i]/100.0     # depth of each layer
            theta[j] = SW[i]                  # soil water content (v/v)
            epsilon[j] = afp[i]               # air-filled porosity (v/v)
            lamda[j] = RLV[i] * lamda_rho     # root transmissivity
#!       maximum rate of methanogenesis (Vm, mol CH4/m3/s)
#!       (assume all is consumed in a day)
#!       (i.e. convert kgC/ha/d -->moleCH2O/m3/s)
            VV[om,j] = rCH4/10./12./(dlayr[i]/100.0)/spd
#!       maximum rate of aerobic respiration (Vr, mol CO2/m3/s)
#!       (convert kgC/ha/d -->moleCH2O/m3/s)
            VV[o2,j] = rCH4/10./12./(dlayr[i]/100.)/spd
#!       maximum rate of methane oxidation   (Vo, mol CH4/m3/s)
            VV[ch4,j] = 1.5e-5
#      ENDDO
#
#!     Leaching rate
        Lz = drain * 1.e-3/spd    # mm/d --> m3/m2/s
#
#!     Call the steady-state routine of the Arah model
#!     TSubstrate comes out of these routines,
#!     ~10% of TCH4Substrate for IRLB9701.RIX, trt 4
        setup(NLAYR+n1)
        SteadyState()
#!     Calculate fractions of C in each methane flux
        if (TSubstrate > 0.0):
            ProductionFrac  = meth.Production /TSubstrate
            ConsumptionFrac = meth.Consumption/TSubstrate
            PlantFrac       = meth.RootFluxOut/TSubstrate
            EbullitionFrac  = meth.Ebullition /TSubstrate
            LeachingFrac    = meth.Leaching   /TSubstrate
        else:
            ProductionFrac  = 0.0
            ConsumptionFrac = 0.0
            PlantFrac       = 0.0
            EbullitionFrac  = 0.0
            LeachingFrac    = 0.0
#      ENDIF
#
#!!     Limit leaching fraction + consumption fraction
#!      to no more than production
#!      IF (ConsumptionFrac + LeachingFrac .GT. ProductionFrac) THEN
#!        ReductFact = ProductionFrac / (ConsumptionFrac + LeachingFrac)
#!        ConsumptionFrac = ConsumptionFrac * ReductFact
#!        LeachingFrac = LeachingFrac * ReductFact
#!      ENDIF
#
        EmissionFrac = ProductionFrac - ConsumptionFrac - LeachingFrac
        DiffusionFrac = EmissionFrac - (PlantFrac + EbullitionFrac)
#!     Calculate actual flux rates (kgC/ha/d) based on
#!     CERES-Rice substrate calculations
        CH4Production  = TCH4Substrate * ProductionFrac
        CH4Consumption = TCH4Substrate * ConsumptionFrac
        CH4PlantFlux   = TCH4Substrate * PlantFrac
        CH4Ebullition  = TCH4Substrate * EbullitionFrac
        CH4Diffusion   = TCH4Substrate * DiffusionFrac
        CH4Leaching    = TCH4Substrate * LeachingFrac
        CH4Emission    = TCH4Substrate * EmissionFrac
#
#C***********************************************************************
#C***********************************************************************
#C     Daily integration
#C***********************************************************************
    elif (DYNAMIC == INTEGR):
#C-----------------------------------------------------------------------
#!     Calculate emissions from dissolved CH4 on draining
        if (FLOOD > 0.0):
            CH4Stored = meth.Storage #chp * 12. * 10.	! kgC/ha
        else:
            x = CH4Stored * 0.5
            CH4Emission = CH4Emission + x
            CH4Stored =	CH4Stored - x
#      endif
#
        StorageFlux = CH4Stored - CH4Stored_Y
        CH4Stored_Y = CH4Stored
#
#!!     chp 2022-03-23 prevent negative CH4 emissions
#!      IF (CH4Emission < -1.E-6) THEN
#!        CH4_correction = CH4Emission  ! value is negative
#!        CH4Emission = 0.0
#!        CH4Diffusion = CH4Diffusion - CH4_correction
#!        CH4Production = CH4Production - CH4_correction
#!      ENDIF
#
        CO2Emission    = newCO2Tot - CH4Production - StorageFlux
        CO2Emission = AMIN1(CO2Emission, newCO2Tot)
#
        CumNewCO2        = CumNewCO2        + newCO2Tot
        CumCH4Emission   = CumCH4Emission   + CH4Emission
        CumCH4Consumpt   = CumCH4Consumpt   + CH4Consumption
        CumCH4Leaching   = CumCH4Leaching   + CH4Leaching
        CumCO2Emission   = CumCO2Emission   + CO2emission
#
        CH4_balance = newCO2Tot - (CO2emission + CH4Emission + StorageFlux + CH4Leaching + CH4Consumption)
        Cum_CH4_bal = CumNewCO2 - (CumCO2Emission + CumCH4Emission + CH4stored + CumCH4Leaching + CumCH4Consumpt)
#
#!***********************************************************************
#!***********************************************************************
#!     OUTPUT or SEASEND
#!***********************************************************************
    elif (DYNAMIC == OUTPUT or DYNAMIC == SEASEND):
#!-----------------------------------------------
        #OpMethane(CONTROL, ISWITCH, newCO2Tot, CO2emission, TCH4Substrate, StorageFlux, CH4Stored, CH4Production, CH4Consumption, CH4Leaching, CH4Emission, CH4PlantFlux, CH4Ebullition, CH4Diffusion, CH4_balance, CumNewCO2, CumCO2Emission, CumCH4Emission, CumCH4Consumpt,CumCH4Leaching, Cum_CH4_bal)
#
#C***********************************************************************
#C***********************************************************************
#C     END OF DYNAMIC IF CONSTRUCT
#C***********************************************************************
#      ENDIF
#C***********************************************************************
    CH4_data.CO2emission = CO2Emission
    CH4_data.CH4Emission = CH4Emission
    CH4_data.CH4Consumption=CH4Consumption
    CH4_data.CH4Leaching = CH4Leaching
    CH4_data.CH4Stored   = CH4Stored
    CH4_data.CumCO2Emission = CumCO2Emission
    CH4_data.CumCH4Emission = CumCH4Emission
    CH4_data.CumCH4Consumpt = CumCH4Consumpt
    CH4_data.CumCH4Leaching = CumCH4Leaching
#
    return CH4_data
#      END
#
#C=======================================================================
#C  OpMethane, Subroutine, C.H.Porter, P. Grace
#C  Generates daily output for methane emissions
#C-----------------------------------------------------------------------
#C  REVISION       HISTORY
#C  07/02/2021 CHP Written
#!=======================================================================