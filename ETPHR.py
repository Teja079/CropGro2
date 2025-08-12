#=======================================================================
#  ETPHR, Subroutine
#  Computes canopy ET (mm/h) and gross photosynthesis (µmol CO2/m2/s)
#  for each hour.
#-----------------------------------------------------------------------
#  Called from: ETPHOT
#  Calls:       CANPET,CANOPG,HSOILT
#=======================================================================
#
from CropGro2.ModuleDefs import GET_float


def ETPHR(CANHT, CEC, CEN, CLOUDS, CO2HR, DAYTIM, DLAYR2, DULE, FNPGL,
          FNPGN, FRACSH, FRSHV, KDIRBL, LAISH, LAISHV, LAISL, LAISLV, LLE,
          LMXREF, LNREF, LWIDTH, MEEVP, MEPHO, NLAYR, NSLOPE, PARSH, PARSUN,
          QEREF, RABS, RCUTIC, REFHT, RHUMHR, RNITP, RWUH, SHCAP, SLAAD,
          SLWREF, SLWSLO, STCOND, SWE, TAIRHR, TA, TMIN, TYPPGL, TYPPGN,
          WINDHR, XLAI, XLMAXT, YLMAXT, CCNEFF, CICAD, CMXSF, CQESF, PGPATH):
    import numpy as np
# !     ------------------------------------------------------------------
#       USE ModuleDefs     !Definitions of constructed variable types,
#                          ! which contain control information, soil
#                          ! parameters, hourly weather data.
#       IMPLICIT NONE
#       EXTERNAL CANOPG, CANPET, HSOILT
#       SAVE
#
#       CHARACTER MEEVP*1,MEPHO*1,TYPPGN*3,TYPPGL*3
#
#       INTEGER ITER,NLAYR
#
#       LOGICAL DAYTIM,REPEAT,STRESS
#
#       REAL AGEFAC,CANHT,CEC,CEN,CLOUDS,CO2HR,CONDSH,
#      &  CONDSL,CSHPRV,CSHSTR,CSLPRV,CSLSTR,DLAYR2(NL),DULE,EHR,EMAXTC,
#      &  EMAXTR,ERRBND,ERRTC,ERRTR,FNPGN(4),FNPGL(4),FRACSH,FRSHV,HOLD,
#      &  KDIRBL,LAISH,LAISHV,LAISL,LAISLV,LFMXSH,LFMXSL,LLE,LNREF,
#      &  LWIDTH,LMXREF,NSLOPE,PARSH,PARSUN(3),PCNLSH,PCNLSL,PGHR,QEREF,
#      &  RA,RABS(3),RCUTIC,REFHT,RHUMHR,RNITP,RWUH,SHCAP(NL),SLAAD,
#      &  SLWREF,SLWSH,SLWSL,SLWSLO,STCOND(NL),SWE,T0HR,TAIRHR,TA,TMIN,
#      &  TCAN,TCPREV,THR,TPREV,TSHR(NL),TSUM,USTAR,
#      &  WINDHR,XLAI,XLMAXT(6),YLMAXT(6)

    TSURF =np.array((3, 1),dtype=float)

# !     Added by BAK
#       REAL RB(3),RSURF(3),RNET(3,1),
#      &  G, LH, LHEAT(3,1), SH, SHEAT(3,1),  !RSSH, RSSL, RSSS,
#      &  RBSH, RBSL, RBSS
#       CHARACTER PGPATH*2
#       REAL CCNEFF, CICAD, CMXSF, CQESF
#       REAL AGEQESL, CO2QESL, QEFFSL
#
    ERRBND=0.01

#   Initialize.
    EMAXTC = ERRBND * 30.0
    EMAXTR = ERRBND * 1.0

    AGEFAC = 1.0
    EHR = 0.0
    LFMXSH = 0.0
    LFMXSL = 0.0
    PCNLSH = 0.0
    PCNLSL = 0.0
    PGHR = 0.0
    RA = 0.0
    USTAR = 0.0
    SLWSH = 0.0
    SLWSL = 0.0
    T0HR = 0.0
    TCAN = TAIRHR
    THR = 0.0
    TSURF[1,1] = TAIRHR
    TSURF[2,1] = TAIRHR
    TSURF[3,1] = TAIRHR
    STRESS = False

#     Daylight hours with canopy.

    if DAYTIM and XLAI > 0.0:
        REPEAT = True
        ITER = 1
        TSUM = 0.0

        #  Loop until evapotranspiration and photosynthesis are stable.
        if MEEVP == 'Z' and MEPHO == 'L':
            while True:
                TCPREV = TCAN
                (AGEFAC, CONDSH, CONDSL, CSHSTR, CSLSTR, LFMXSH, LFMXSL, PCNLSH,
                 PCNLSL, PGHR, SLWREF, SLWSH, SLWSL, STRESS, AGEQESL, CO2QESL, QEFFSL) = (
                CANOPG (CO2HR, FNPGL, FNPGN, LAISH, LAISL, LMXREF,LNREF,
                       NSLOPE, PARSH, PARSUN, QEREF, RNITP,SLAAD, SLWSLO,
                       TMIN, TSURF, TYPPGL, TYPPGN,XLMAXT, YLMAXT,CCNEFF,
                       CICAD,CMXSF,CQESF,PGPATH))

                (EHR, RA, TCAN, THR, TSHR, TSURF, USTAR,RB, RSURF,
                 RNET,G, LH, LHEAT, SH, SHEAT, RBSL, RBSL, RBSS) = (
                    CANPET(CANHT, CEC, CEN, CLOUDS, CONDSH, CONDSL,DLAYR2, FRACSH,
                           FRSHV, KDIRBL, LAISH,LAISHV, LAISL, LAISLV, LWIDTH, RABS,
                           RCUTIC, REFHT, RHUMHR, STCOND, TAIRHR, WINDHR))

                TSUM = TSUM + TCAN
                if ITER > 5:
                  TCAN = TSUM / ITER
                else:
                  TCAN = (TCAN+TCPREV) / 2.0

                ERRTC = abs(TCAN-TCPREV)
                REPEAT = ERRTC > EMAXTC
                ITER = ITER + 1
                if not (REPEAT and ITER <= 5):
                    break
            T0HR = THR

            # Water stress loop.  Evapotranspiration limited to by soil water
            # supply by adjusting leaf conductances.  Photosynthesis recalulated.
            if THR > RWUH:
                CSLPRV = CONDSL
                CONDSL = CONDSL * RWUH/THR
                CSHPRV = CONDSH
                CONDSH = CONDSH * RWUH/THR
                REPEAT = True
                ITER = 1
                TSUM = 0.0
                while True:
                    TCPREV = TCAN
                    TPREV = THR
                    (EHR, RA, TCAN, THR, TSHR, TSURF, USTAR, RB, RSURF,
                     RNET,G, LH, LHEAT, SH, SHEAT, RBSH, RBSL, RBSS) = (
                    CANPET(CANHT, CEC, CEN, CLOUDS, CONDSH, CONDSL,DLAYR2, FRACSH,
                    FRSHV, KDIRBL, LAISH,LAISHV, LAISL, LAISLV, LWIDTH, RABS,
                    RCUTIC, REFHT, RHUMHR, STCOND, TAIRHR, WINDHR))
                    TSUM = TSUM + TCAN
                    if ITER > 5:
                       TCAN = TSUM / ITER
                    else:
                       TCAN = (TCAN+TCPREV) / 2.0
                    ERRTC = abs(TCAN-TCPREV)
                    ERRTR = abs(THR-RWUH)
                    HOLD = CSLPRV+(CONDSL-CSLPRV)/(THR-TPREV)*(RWUH-TPREV)
                    CSLPRV = CONDSL
                    CONDSL = max(1.0/RCUTIC,HOLD)
                    HOLD = CSHPRV+(CONDSH-CSHPRV)/(THR-TPREV)*(RWUH-TPREV)
                    CSHPRV = CONDSH
                    CONDSH = max(1.0/RCUTIC,HOLD)
                    REPEAT = ERRTR > EMAXTR and ERRTC > EMAXTC
                    ITER = ITER + 1
                    if not (REPEAT and ITER <= 5):
                        break

                CSLSTR = CONDSL
                CSHSTR = CONDSH
                STRESS = True

                (AGEFAC, CONDSH, CONDSL, CSHSTR, CSLSTR, LFMXSH, LFMXSL,
                PCNLSH, PCNLSL, PGHR, SLWREF, SLWSH, SLWSL, STRESS, AGEQESL, CO2QESL, QEFFSL) = (
                CANOPG(CO2HR, FNPGL, FNPGN, LAISH, LAISL, LMXREF,LNREF,
                       NSLOPE, PARSH, PARSUN, QEREF, RNITP, SLAAD, SLWSLO,
                       TMIN, TSURF, TYPPGL, TYPPGN,XLMAXT, YLMAXT, CCNEFF,
                       CICAD,CMXSF,CQESF,PGPATH))

                STRESS = False
        else:
            (AGEFAC, CONDSH, CONDSL, CSHSTR, CSLSTR, LFMXSH, LFMXSL, PCNLSH, PCNLSL,
             PGHR, SLWREF, SLWSH, SLWSL, STRESS, AGEQESL,CO2QESL,QEFFSL) = (
            CANOPG(CO2HR, FNPGL, FNPGN, LAISH, LAISL, LMXREF,LNREF, NSLOPE,
                   PARSH, PARSUN, QEREF, RNITP,SLAAD, SLWSLO, TMIN, TSURF,
                   TYPPGL, TYPPGN, XLMAXT, YLMAXT, CCNEFF,CICAD,CMXSF,CQESF,PGPATH))
    elif (MEEVP == 'Z'): #   Night hours or bare soil.
        CONDSH = 0.0
        CONDSL = 0.0
        REPEAT = True
        ITER = 1
        TSUM = 0.0
        #DO WHILE (REPEAT .AND. ITER .LE. 5)
        while True:
            TCPREV = TCAN
            (EHR, RA, TCAN, THR, TSHR, TSURF, USTAR, RB, RSURF,
            RNET, G, LH, LHEAT, SH, SHEAT, RBSL, RBSL, RBSS) = (
            CANPET(CANHT, CEC, CEN, CLOUDS, CONDSH, CONDSL, DLAYR2, FRACSH,
            FRSHV, KDIRBL, LAISH, LAISHV, LAISL, LAISLV, LWIDTH, RABS,
            RCUTIC, REFHT, RHUMHR, STCOND, TAIRHR, WINDHR))

            TSUM = TSUM + TCAN
            if ITER > 5:
                TCAN = TSUM / ITER
            else:
                TCAN = (TCAN+TCPREV) / 2.0

            ERRTC = abs(TCAN-TCPREV)
            REPEAT = ERRTC > EMAXTC
            ITER = ITER + 1
            if not (REPEAT and ITER <= 5):
                break
        T0HR = THR
    else:
        pass

    PGHR = max(PGHR,0.0)
    T0HR = max(T0HR,0.0)
    THR = max(THR,0.0)
    EHR = max(EHR,0.0)

#      Update soil moisture in upper layer, soil and canopy-air temperature
#      difference.
    if MEEVP == 'Z':
        TSHR = HSOILT(DLAYR2, NLAYR, SHCAP, STCOND, TA, TSURF(3,1))
        SWE = max(SWE-EHR,0.0)
        CEN = (DULE-SWE) / (DULE-LLE) * 100.0

    return (AGEFAC, EHR, LFMXSH, LFMXSL, PCNLSH, PCNLSL, PGHR, SLWSH, SLWSL,T0HR,
            TCAN, THR, TSHR,TSURF,  CONDSH, CONDSL, RA, RB, RSURF, RNET, G, LH,
            LHEAT, SH, SHEAT, RBSH, RBSL, RBSS, AGEQESL, CO2QESL, QEFFSL)
#========================================================================

#========================================================================
#  CANOPG, Subroutine
#  Computes instantaneous canopy photosynthesis (µmol CO2/m2/s) and leaf
#  CO2 conductance (cm/s) of shaded and sunlit leaves.  Uses shaded
#  and sunlit leaf areas, light intensity on each, and integrates
#  over leaf angle classes.
#-----------------------------------------------------------------------
#  Called from: ETPHR
#  Calls:       PGLFEQ,PGLEAF
#=======================================================================
def CANOPG(CO2HR, FNPGL, FNPGN, LAISH, LAISL, LMXREF,LNREF, NSLOPE, PARSH, PARSUN,
           QEREF, RNITP,SLAAD, SLWSLO, TMIN, TSURF, TYPPGL, TYPPGN,XLMAXT, YLMAXT,
           CCNEFF,CICAD,CMXSF,CQESF,PGPATH):
    #
    #   CHARACTER TYPPGN*3,TYPPGL*3
    #   INTEGER I
    STRESS : bool = False
    #   REAL AGEFAC,AGMXSH,AGMXSL,CO2HR,CONDSH,CONDSL,CONSUM,CONSUN,
    #  &  LAISH,LAISL,LMXREF,LFMXSH,LFMXSL,LNREF,NSLOPE,PARSH,
    #  &  PARSUN(3),PARSL,PGHR,PGSUM,PGSUN,PGSH,PGSL,QEREF,QEFFSH,
    #  &  QEFFSL,RNITP,SLAAD,TEMPSH,TEMPSL,TSURF(3,1),FNPGN(4),
    #  &  FNPGL(4),XLMAXT(6),XLAI,YLMAXT(6),SLWSL,
    CSLSTR : float = -99.0
    CSHSTR : float = -99.0
    #  &  SLWSH,PCNLSL,PCNLSH,SLWSLO,SLWREF,TMIN
    #
    #   CHARACTER PGPATH*2
    #   REAL CCNEFF, CICAD, CMXSF, CQESF
    #   REAL AGEQESH, AGEQESL, CO2QESH, CO2QESL
    #
    # Initialize.

    SLWREF : float = -99.0

    TEMPSL = TSURF[1,1]
    TEMPSH = TSURF[2,1]
    XLAI = LAISL + LAISH
    PARSL = PARSUN[2]
#
#   Calculate leaf photosynthesis parameters with separate layering
#   of SLW and leaf N.  SLW and leaf N decrease linearly with
#   increasing LAI (Sinclair et al., 1993).  Assume sunlit (SL)
#   leaves are physically above shaded (SH) leaves.

    SLWSL = 1./SLAAD + 0.5*SLWSLO*LAISH
    SLWSH = 1./SLAAD - 0.5*SLWSLO*LAISL
    PCNLSL = RNITP + 0.5*NSLOPE*LAISH
    PCNLSH = RNITP - 0.5*NSLOPE*LAISL

     # Calulate leaf photosynthesis for SH and SL leaves.

    (AGMXSH, LFMXSH, QEFFSH, CO2QESH,AGEQESH) = (
        PGLFEQ(CO2HR, FNPGL, FNPGN, LMXREF, LNREF, QEREF,PCNLSH, SLWSH,
               SLWREF, TEMPSH, TMIN, TYPPGL,TYPPGN, XLMAXT, YLMAXT,CCNEFF,
               CICAD, CMXSF,CQESF,PGPATH))

    (AGMXSL, LFMXSL, QEFFSL, CO2QESL,AGEQESL) = (
    PGLFEQ(CO2HR, FNPGL, FNPGN, LMXREF, LNREF, QEREF,PCNLSL, SLWSL, SLWREF,
    TEMPSL, TMIN, TYPPGL,TYPPGN, XLMAXT, YLMAXT,CCNEFF, CICAD, CMXSF,CQESF,PGPATH))

#
# C     Gaussian integration of photosynthesis and leaf CO2 conductance
# C     over three leaf classes for sunlit leaves.
#
    PGSUM = 0.0
    CONSUM = 0.0
    for I in range (1,5):
        CONSUN, PGSUN = PGLEAF(CO2HR, LFMXSL, PARSUN(I), QEFFSL, TEMPSL, CCNEFF,CICAD,PGPATH)
        if I == 2:
            PGSUM = PGSUM + PGSUN*1.6
            CONSUM = CONSUM + CONSUN*1.6
        else:
            PGSUM = PGSUM + PGSUN
            CONSUM = CONSUM + CONSUN
    PGSL = PGSUM / 3.6
    CONDSL = CONSUM / 3.6

# C     Compute photosynthesis and leaf CO2 conductance for shaded leaves
    CONDSH, PGSH = PGLEAF(CO2HR, LFMXSH, PARSH, QEFFSH, TEMPSH,CCNEFF,CICAD,PGPATH)

#     Compute canopy photosynthesis (µmol CO2/m2/s).

    if STRESS:
        if CONDSL > 0.0:
            PGSL = PGSL * CSLSTR/CONDSL
        else:
            PGSL = 0.0
        if CONDSH > 0.0:
            PGSH = PGSH * CSHSTR/CONDSH
        else:
            PGSH = 0.0
    PGHR = PGSL*LAISL + PGSH*LAISH

    if XLAI > 0.0:
        AGEFAC = (LAISL*AGMXSL+LAISH*AGMXSH) / XLAI
    else:
        AGEFAC = 0.0

    return (AGEFAC, CONDSH, CONDSL, CSHSTR, CSLSTR,LFMXSH, LFMXSL, PCNLSH, PCNLSL,
            PGHR,SLWREF, SLWSH, SLWSL, STRESS, AGEQESL,CO2QESL,QEFFSL)
#=======================================================================
#  PGLFEQ, Subroutine
#  Calculates leaf quantum efficiency and maximum photosynthesis for
#  leaf photosynthesis equation as a function of temp, CO2, leaf N,
#  and specific leaf area.
#-----------------------------------------------------------------------
#  Called from: CANOPG
#  Calls:       CURV,TABEX
#  Notes : Standard conditions and suggested values for QEREF and LXREF.
#          LXREF is for upper sunlit leaves in the canopy.  Lower
#          leaves will have a lower rate because of less SLW and N.
#          QEREF : 30 oC, 350 µL/L CO2, 2% O2, <100 µmol/m2/S PFD
#                 0.0541 µmol/µmol (Ehleringer and Bjorkman, 1977)
#                 (converted from 30 oC, 325 µL/L CO2)
#          LXREF: 30 oC, 350 µL/L CO2, 2% O2, 2000 µmol/m2/S PFD
#                 measured at SLWREF and LNREF
#                 BEAN=28, PEANUT=28, SOYBEAN=28 µmol/m2/s
#========================================================================
def PGLFEQ( CO2HR, FNPGL, FNPGN, LMXREF, LNREF, QEREF, RNITP, SLW, SLWREF,
            TEMPHR, TMIN, TYPPGL, TYPPGN, XLMAXT, YLMAXT,CCNEFF, CICAD, CMXSF,
            CQESF, PGPATH):
    import math
    from DSSATUtils import curv
    from UTILS import TABEX

#       CHARACTER TYPPGN*3,TYPPGL*3
#       REAL AGEMXL,AGEQE,CICA,CINT,CO2HR,CO2MAX,CO2QE,
#      &  CURV,FNPGN(4),FNPGL(4),GAMST,LFMAX,LNREF,LMXREF,LXREF,
#      &  O2,QEFF, QEREF,RGAS,RNITP,RT,SLW,SLWMAX,SLWREF,TABEX,TAU,
#      &  TEMPHR,TEMPMX,TK,XLMAXT(6),YLMAXT(6),TMIN,CHILL
#
#       CHARACTER PGPATH*2
#       REAL CCNEFF, CICAD, CMXSF, CQESF
#
    O2 = 210000.0
    RGAS = 8.314

    PDLA : float = 0.0
    BETALS: float = 0.0
#
#       REAL BETAMX
#
#      Initialization.  Convert LMXREF from mgCO2/m2/s to µmol/m2/s.
#
    TK = TEMPHR + 273.
    RT = RGAS * TK
    LXREF = LMXREF * 1000.0 / 44.0

#   Temperature and CO2 effects on QEFF AND LMXREF are both modeled using
#   Farquhar and Caemmerer's (1982) equation (16.60 a,b) for limiting RuBP,
#   combined with the temperature effect on the specificity factor (TAU)
#   and the compensation point in the absence of dark respiration (GAMST).
#       !CHP 4/15/03 Prevent overflow
    if RT > 1000.:
        if PGPATH == "C4" or PGPATH == 'c4':
            tau = math.exp(-3.949 + 28990.0/RT)*CCNEFF
        else:
            TAU = math.exp(-3.949 + 28990.0/RT)
        GAMST = 0.5 * O2 / TAU
    else:
        TAU = 1E10
        GAMST = 0.0

#      EFFECTS ON MAXIMUM LEAF PHOTOSYNTHESIS (LMXREF).
#
#      SLW effect on LMXREF assumed linear (Dornhoff and Shibles, 1970).
#
    SLWMAX = SLW / SLWREF
#
#      Temperature and non-saturating CO2.
#
#      For the computation of LMXREF, Ci/Ca = 0.7 for CO2=350 µL/L.  The factor
#      7.179 scales CO2MAX to 1.0 at 30 oC and 350 µL/L CO2.
#
#      CICA = 0.4+0.6*EXP(-0.002*CO2HR)
    if PGPATH == "C4" or PGPATH == "c4":
        CICA = CICAD
        CINT = CICA*CO2HR + (1.0-CICA)*GAMST
        CINT = max(CINT,GAMST)
        CO2MAX = CMXSF * (CINT-GAMST) / (4.0*CINT+8.0*GAMST)
    else:
        CICA = 0.7
        CINT = CICA*CO2HR + (1.0-CICA)*GAMST
        CINT = max(CINT,GAMST)
        CO2MAX = 7.179 * (CINT-GAMST) / (4.0*CINT+8.0*GAMST)

#       Temperature and saturating CO2.
#
#      Temperature effect on LMXREF at saturating CO2 via lookup table.
#      Sawtooth shape i.e. linear increase to peak, then linear decrease.
#      Based on analysis of LMXREF using Tenhunen's (1976) data with QEFF
#      from Ehleringer and Bjorkman (1977).  TEMPMX scaled to 1.0 at 30 oC.
#
        TEMPMX = TABEX(YLMAXT,XLMAXT,TEMPHR,6)/TABEX(YLMAXT,XLMAXT,30.0,6)
#
#      Minimum night temp effect on Pmax next day, quadratic function
#      after Mike Bell, peanut.  ONLY the first two numbers are used.
#      KJB, 9/7/94.  OKAY TO USE NIGHT MINIMUM CANOPY TEMPERATURE LATER
#
        CHILL = curv(TYPPGL,FNPGL(1),FNPGL(2),FNPGL(3),FNPGL(4),TMIN)
#
#      Nitrogen effects on LMXREF.  Photosynthesis is affected both by
#      plant nutrition and age.  Quadratic from (1) to (2).  AGEMXL scaled
#      to 1.0 at LNREF.
#
        AGEMXL = curv(TYPPGN,FNPGN(1),FNPGN(2),FNPGN(3),FNPGN(4),RNITP) / \
                 curv(TYPPGN,FNPGN(1),FNPGN(2),FNPGN(3),FNPGN(4),LNREF)
#
#      EFFECTS ON QUANTUM EFFICIENCY (QEFF).
#
#      Temperature and non-saturating CO2.
#
#      For the computation of QEFF, Ci/Ca = 1.0.  The factor 6.225 scales CO2QE
#      to 1.0 at 30 oC and 350 µL/L CO2.
#
        CINT = max(CO2HR,GAMST)
        if PGPATH == "C4" or PGPATH == "c4":
            CO2QE = CQESF * (CINT-GAMST) / (4.*CINT+8.*GAMST)
        else:
            CO2QE = 6.225 * (CINT-GAMST) / (4.*CINT+8.*GAMST)

#      Nitrogen effects on QEFF.  Photosynthesis is affected both by
#      plant nutrition (small in legumes) and age.
#       AGEQE = 0.0094 + (1.0-EXP(-4.4*AGEMAX))
#      7/17/94.  KJB INCREASED EXTENT OF N EFFECT ON QEFF.  COEFFICIENT
#      CHANGED FROM -4.4 TO -3.0,  NOW 0.97 AT AGEMAX=0.75, 0.819 AT
#      0.50, AND 0.560 AT 0.25.  ALSO, NORMALIZED.  THE 0.0094 VALUE
#      NOW KEEPS THE VALUE AT 0.01
#      CHANGE AGAIN 9/24/95 KJB, CHANGE FROM -2.5 TO -2.0, NOW 0.923
#      AT 0.8, 0.734 AT 0.5, 0.388 AT 0.2, 1.0 AT 1, 0.01 AT 0.
#
        AGEQE =  (0.0094 + (1.0-math.exp(-2.0*AGEMXL))) / (0.0094 + (1.0-math.exp(-2.0*1.0)))
        AGEQE = min(max(AGEQE,0.0),1.0)
#
#     25 Apr 2011 KJB,PDA,MPS added code for beta function: PDLA effects on lfmax and QE
        GET_float('PDLABETA','BETA',BETALS)
        GET_float('PDLABETA','PDLA',PDLA)
        BETAMX = (1.0-PDLA/100.)**BETALS
#
#      Calculate QEFF and LFMAX at ambient conditions.
#
        QEFF = QEREF * CO2QE * AGEQE * BETAMX
        LFMAX = LXREF * SLWMAX * TEMPMX * AGEMXL * CO2MAX * CHILL * BETAMX
#
    return (AGEMXL, LFMAX, QEFF, CO2QE, AGEQE)
#
#=======================================================================
#  PGLEAF, Subroutine,
#  Calculate instantaneous leaf photosynthesis as a function of PAR
#  and leaf characteristics (µmol/m2/s).  Leaf conductance calculated
#  as a function of net photosynthesis and Ci/Ca ratio (cm/s)
#-----------------------------------------------------------------------
#  Called from: CANOPG
#  Calls:
#=======================================================================
def PGLEAF(CO2HR, LFMAX, PARLF, QEFF, TEMPHR,CCNEFF, CICAD, PGPATH):
    import math
#
#       REAL A,B,C,CICA,CINT,CO2HR,CCO2LF,CONDLF,CVTURE,GAMST,LFMAX,QEFF,
#      &  PARLF,PATM,PGLF,PNLF,RGAS,RT,TAU,TEMPHR
#
    PGPATH = '  '
#       REAL CCNEFF,
    CICAD : float
#
    CVTURE=0.8
    PATM=101300.0
    RGAS=8.314
#
# C     Initialization.
#
    RT = RGAS * (TEMPHR+273.0)
#
#   Calculate leaf photosynthesis (µmol CO2/m2/s) using a non-rectangular
#   hyperbola (Rabinowitch, 1951; Lommen et al, 1971; Evans and Farquhar,
#   Norman and Arkebauer, Gutschick, In: Boote and Loomis, 1991)
#
    A = CVTURE
    B = (QEFF*PARLF) + LFMAX
    C = QEFF * PARLF * LFMAX
     # CHP Added checks for floating underflow 1/16/03
    if LFMAX > 0.0:
        if ((QEFF*PARLF/LFMAX) < 20. and (QEFF*PARLF/LFMAX) > -20.0):
            PGLF = LFMAX * (1.0 - math.exp(-QEFF*PARLF/LFMAX))
        else:
            PGLF = max(LFMAX, 0.0)
    else:
        PGLF = max(LFMAX, 0.0)
    PNLF = PGLF
#
#   Calculate leaf CO2 conductance (mol/m2/s) using assumption of constant
#   CI/CA ratio.  Compensation point (GAMST) is temperature dependent.
#   Leaf respiration neglected so PNLF=PGLF.
#
      # !CHP 4/15/03 Prevent overflow
    if RT > 1000.:
        if PGPATH == "C4" or PGPATH == "c4":
            TAU = math.exp(-3.9489 + 28990.0/RT) * CCNEFF
        else:
            TAU = math.exp(-3.9489 + 28990.0/RT)
        GAMST = 1.0E6 * 0.5 * 0.21 / TAU
    else:
        TAU = 1E10
        GAMST = 0.0

    if PGPATH == 'C4' or PGPATH == 'c4':
        CICA = CICAD
    else:
        CICA = 0.7

    CINT = CICA*CO2HR + (1.0-CICA)*GAMST
    CCO2LF = max(PNLF/(CO2HR-CINT),0.0)

#     Convert units from mol/m2/s CO2 to m/s H20.

    CONDLF = 1.6 * CCO2LF * RT / PATM

    return  CONDLF, PGLF

#=======================================================================
#  CANPET, Subroutine,
#  Computes instantaneous canopy evapotranspiration (mm/h) and
#  surface temperatures of soil and shaded and sunlit leaves (C).
#  Soil temperatures also computed.
#-----------------------------------------------------------------------
#  Called from: ETPHR
#  Calls:       ETRES,ETSOLV,RADB,VPSAT
#=======================================================================

def CANPET(CANHT, CEC, CEN, CLOUDS, CONDSH, CONDSL, DLAYR2, FRACSH, FRSHV, KDIRBL,
           LAISH, LAISHV, LAISL, LAISLV, LWIDTH, RABS,RCUTIC, REFHT, RHUMHR, STCOND,
           TAIRHR, WINDHR):
    import numpy as np
    from HMET import VPSAT
    from ModuleDefs import NL

#       INTEGER I
#       REAL CANHT,CONDSH,CONDSL,CEC,CEN,DLAYR1,DLAYR2(NL),
#      &  EAIRHR,ECAN,ESATHR,EHR,ETHR,FRACSH,FRSHV,G,KDIRBL,LAISH,
#      &  LAISL,LH,LHEAT(3,1),LHVAP,LWIDTH,PATM,PSYCON,RA,REFHT,RHUMHR,
#      &  RCUTIC,RL(3,3),RABS(3),RNTOT,RNET(3,1),RS(3,3),SHAIR,STCND1,
#      &  STCOND(NL),TAIRHR,,THR,TSHR1,TSURF(3,1),VHCAIR,
    TSHR = np.full(NL, 0.0, dtype=float)
    RNET = np.full((4, 2),(0.0,0.0), dtype=float)
#      &  VPSAT,WINDHR,CLOUDS,DAIR,DAIRD,DVAPOR,Q,SH,SHEAT(3,1),
#      &  SHAIRD,TK,MWATER,RGAS,MAIR,LAISHV,LAISLV,RADBK(3),
#      &  USTAR,XLAI,ZERO
    TCAN : float
    VPD = np.zeros((3, 1),dtype=float)
#
#       REAL RB(3), RSURF(3),RBSH,RBSL,RBSS !,RSSH,RSSL,RSSS
# C         RB, RSURF RSSH RSSL RSSS added DEC2014 by Bruce Kimball
# C         RBSH,RBSL,RBSS added by BAK on 10DEC15
#
    RGAS = 8.314
    MWATER = 0.01802
    MAIR = 0.02897
    PATM = 101300.0
    SHAIRD = 1005.0
    ZERO = 1.0E-6
#
#      Initialize.
    XLAI = LAISH + LAISL
    STCND1 = STCOND[1]
    TSHR1 = TSHR[1]
    DLAYR1 = DLAYR2[1] / 100.0

#      Calculate air/water properties as a function of temperature.
    ESATHR = VPSAT(TAIRHR)                              # Pa
    EAIRHR = ESATHR * RHUMHR / 100.0                    # Pa
    TK = TAIRHR + 273.0                                 # K
    DVAPOR = MWATER * EAIRHR / (RGAS * TK)              # kg/m3
    DAIRD = MAIR * (PATM-EAIRHR) / (RGAS * TK)          # kg/m3
    DAIR = DVAPOR + DAIRD                               # kg/m3
    Q = DVAPOR / DAIR
    SHAIR = SHAIRD * (1.0+0.84*Q)                       # J/kg/K
    VHCAIR = DAIR * SHAIR                               # J/m3/K
    LHVAP = (2501.0-2.373*TAIRHR) * 1000.0              # J/kg
    PSYCON = SHAIR * PATM / (0.622*LHVAP)               # Pa/K

#     Create vpd and resistance matrices.

    for I in range (1,4):
        VPD[I,1] = ESATHR - EAIRHR

    (RA, RL, RS, USTAR,RB,RSURF) = ETRES(CANHT, CEC, CEN, CONDSH, CONDSL, FRACSH,
                                         FRSHV,KDIRBL, LAISH, LAISL, LWIDTH, RCUTIC,
                                         REFHT,TAIRHR, TCAN, WINDHR)

#   RB and RSURF Added by BAK on 1DEC2014
    RBSL = RB[1]
    RBSH = RB[2]
    RBSS = RB[3]
    if RBSL > 20000.: RBSL = 20000.
    if RBSH > 20000.: RBSH = 20000.
    if RBSS > 20000.: RBSS = 20000.
# C       Obtain the resistances of sunlit, shaded leaves and
# C         soil surface. Added by BAK on 18MAR15
#
# C     Calculate NET total by subtracting net (back) longwave radiation.
#
    RADBK = RADB(CLOUDS, EAIRHR, FRSHV, LAISHV,LAISLV, TAIRHR, TCAN)
    RNET[1,1] = RABS[1] - RADBK[1]
    RNET[2,1] = RABS[2] - RADBK[2]
    RNET[3,1] = RABS[3] - RADBK[3]
    RNTOT = RNET[1,1] + RNET[2,1] + RNET[3,1]

# C     Solve 3-zone model for ET and E (mm/h).

    (ECAN, G, LH, LHEAT, SH, SHEAT, TCAN, TSURF) = (ETSOLV(DLAYR1, EAIRHR, PSYCON, RL,
                                                           RNET, RS, STCND1,TAIRHR, TSHR1, VHCAIR, VPD))
    ETHR = LH / LHVAP * 3600.0
    EHR = LHEAT(3,1) / LHVAP * 3600.0

    if XLAI > 0.0:
        THR = ETHR - EHR
    else:
        TSURF[1,1] = 0.0
        TSURF[2,1] = 0.0
        THR = 0.0
        EHR = ETHR

    return  (EHR, RA, TCAN, THR, TSHR, TSURF, USTAR, RB, RSURF, RNET, G, LH, LHEAT, SH, SHEAT,RBSH, RBSL, RBSS)
#
#========================================================================
#  ETRES, Subroutine
#  Computes resistances to latent and sensible heat and inserts them
#  into matrices for ETSOLV.
#------------------------------------------------------------------------
#  Called from: CANPET
#  Calls:       RESBLR
#========================================================================
def ETRES(CANHT, CEC, CEN, CONDSH, CONDSL, FRACSH, FRSHV,KDIRBL, LAISH,
          LAISL, LWIDTH, RCUTIC, REFHT,TAIRHR, TCAN, WINDHR):
    import math
    import numpy as np
#       INTEGER I,J
#       REAL CANHT,CEC,CEN,CONDSH,CONDSL,FRACSH,FRSHV,KDIRBL,
#      &  LAISH,LAISL,LWIDTH,RCUTIC,RA,RB(3),REFHT,
#      &  RMAX,RSSH,RSSL,RSSS,TAIRHR,TCAN,
#      &  WINDHR,XLAI,USTAR
    RS = np.zeros((3,3), dtype=float)
    RL = np.zeros((3,3), dtype=float)
    RSURF = np.zeros(4, dtype=float)

    RMAX=1.0E4

#      Initialization.
    XLAI = LAISH + LAISL

#      Calculate canopy and soil boundary layer resistances.
    (RA, RB, USTAR) = RESBLR(CANHT, FRACSH, FRSHV, KDIRBL, LAISH, LAISL,
                             LWIDTH, REFHT, TAIRHR, TCAN, WINDHR)

# C     Calculate leaf surface resistances.
#
    if XLAI > 0.0:
        if CONDSL > 0.0:
            RSSL = 1.0/(CONDSL*LAISL) - RB[1]
            RSSL = max(RSSL,1.0)
        else:
            RSSL = RCUTIC / LAISL

        if CONDSH > 0.0:
            RSSH = 1.0/(CONDSH*LAISH) - RB[2]
            RSSH = max(RSSH,1.)
        else:
            RSSH = RCUTIC / LAISL
    else:
#       For XLAI=0.0 set RSURF = 0.0  RBLYR = RMAX in RESBLR, so
#       this means RLEAF = RMAX for both heat and vapor.
        RSSL = 0.0
        RSSH = 0.0
    RSURF[1] = min(RSSL,RMAX)
    RSURF[2] = min(RSSH,RMAX)

#      Calculate soil surface resistance. Radiation component (a+b*rna)
#      reduced to constant 0.0117 using the average RNA from Jagtap (1976).
#      If RNET is included again, may need a RNMIN too.
#
    if CEN <= CEC:
#         RSSS = 100.0 commented out by Bruce Kimball on 10DEC15
#           RSSS = 100.0
#   RESET JULY 10 2017 BK AND KJB
        RSSS=0.0
#   set back to zero per DSSAT 3.5 on 11Jul17
    else:
        RSSS = 100.0 + 154.0*(math.exp(0.0117*(CEN-CEC)**1.37)-1.0)
#   uncommented back to DSSAT 3.5 on 11Jul17 by BK and KB
#   RESET FOR RSSS ON JULY 10 2017 BK AND KJB
#         RSSS = 100.0 + 154.0*(EXP(0.0117*(CEN-CEC)**1.275)-1.0)
# commented out by Bruce Kimball on 10DEC15
# C          RSSS = 10000
    RSURF[3] = min(RSSS,RMAX)
#
# C     Create resistance matrices (RS, RL).
#
    for I in range(1,4):
        for  J in range(1,4):
            if I == J:
                RS[I,J] = RA + RB[I]
                RL[I,J] = RA + RB[I] + RSURF[I]
            else:
                RS[I,J] = RA
                RL[I,J] = RA

    return (RA, RL, RS, USTAR, RB, RSURF)
#
#=======================================================================
#  RESBLR, Subroutine,
#  Calculates canopy and soil boundary-layer resistances to heat and
#  vapor.  Based on Choudhury and Monteith (1988).
#-----------------------------------------------------------------------
#  Called from: ETRES
#  Calls:
#=======================================================================
def RESBLR(CANHT, FRACSH, FRSHV, KDIRBL, LAISH, LAISL, LWIDTH, REFHT,
           TAIRHR, TCAN, WINDHR):
    import numpy as np
    import math
#       REAL CANHT,D,ETAK,ETAKMX,ETAW,ETAWMX,FRSHV,FRACSH,H,KDIRBL,
#      &  KH,LAISH,LAISL,ZMD,HMD,K1,K2,LWIDTH,PSIM,PSIH,RA,
#      &  RBLF,RBSH,RBSL,REFHT,RMAX,TAIRHR,TCAN,TKAIR,
#      &  WINDHR,WINDSP,XLAI,Z0H,Z0M,ZS0H,ZS0M,LHZ0M,LZZ0H,LZZ0M,DT,
#      &  MO,USTAR,X,A,B,PI,RATIO,WINDH,RBSS,ZERO
    RB = np.zeros(4, dtype=float)

    ETAKMX = 2.0
    ETAWMX = 3.0
    PI = 3.14159
    RMAX = 1.0E4
    ZERO = 1.0E-6

# C     Initialization and calculation of zero plane displacement height and
# C     canopy surface roughness (Brutsaert, 1982).

    XLAI = LAISH + LAISL
    WINDSP = max(WINDHR,0.1)
# C     changed on 1Dec2014 by Bruce Kimball. 1.0 m/s is too high a
# C       wind speed to be the minimum.
# C     WINDSP = MAX(WINDHR,1.0)
    H = CANHT
    ZS0M = 0.03                                                  # m
    Z0M = max(ZS0M,0.13*H)                                       # m
    if FRSHV > 0.0 and FRSHV < 1.0:
        Z0M = 4.0 * Z0M
    D = 0.77 * H                                                 # m
    Z0H = Z0M/5.0                                                # m
    ZS0H = ZS0M/5.0                                              # m
    HMD = H - D                                                  # m
    ZMD = REFHT - D                                              # m
    LZZ0M = math.log(ZMD/Z0M)
    LZZ0H = math.log(ZMD/Z0H)
    TKAIR = TAIRHR + 273.
    DT = TCAN - TAIRHR
    if abs(DT) <= ZERO:
        USTAR = 0.4 * WINDSP / LZZ0M
        RA = LZZ0H / (0.4 * USTAR)

    ETAK = ETAKMX
    ETAW = ETAWMX

#   Stability correction for RA (Choudhury et al, 1986).
    MO = -0.4*9.81*DT*ZMD / (RA*TKAIR*USTAR**3)
    if abs(MO) <= ZERO:                         # neutral
        PSIM = 0.0
        PSIH = 0.0
    elif MO > ZERO:                         # stable
        PSIM = -5.0 * MO
        PSIM = max(PSIM,-5.0)
        PSIH = PSIM
    elif MO < -ZERO:                        # unstable
        X = (1.0-16.0*MO)**0.25
        A = ((1.0+X)/2.0)**2
        B = (1.0+X**2)/2.0
        PSIM = math.log(A*B) - 2.0*math.atan(X) + PI/2.0
        PSIH = 2.0 * math.log(B)
        RATIO = PSIH / PSIM
        PSIM = min(PSIM,LZZ0M-0.1)
        PSIH = RATIO * PSIM

#       Aerodynamic resistance.
        USTAR = 0.4 * WINDSP / (LZZ0M-PSIM)
        RA = (LZZ0H-PSIH) / (0.4 * USTAR)                   # s/m

#      Canopy calculations.
    if XLAI > 0.0:

#       Calculate windspeed at the top of the canopy.

        LHZ0M = math.log(HMD/Z0M)
        WINDH = max(WINDSP*LHZ0M/LZZ0M,0.1)               # m/s

#       Calculate leaf boundary layer resistances (extension of Choudhury
#       and Montieth, 1988).
        RBLF = ETAW * math.sqrt(LWIDTH/WINDH) / (2.0*0.01*XLAI*(1.0-math.exp(-ETAW/2.0)))        # s/m
        K1 = ETAW/2.0+KDIRBL*XLAI/FRACSH
        RBSL = math.sqrt(LWIDTH/WINDH) * K1 / (0.01*XLAI*(1.0-math.exp(-K1)))
        RBSL = min(RBSL,RMAX)
        RBSH = 1.0 / (1.0/RBLF-1.0/RBSL)
        RBSH = max(RBSH,1.0)
        RBSL = FRSHV * RBSL
        RBSH = FRSHV * RBSL

#       Calculate soil aerodynamic resistance (extension of Choudhury
#       and Montieth, 1988).
        KH = 0.16 * WINDSP * HMD / LZZ0M                           # m/s
        K2 = math.exp(-ETAK*ZS0H/H) - math.exp(-ETAK*(D+Z0H)/H)
        RBSS = H*math.exp(ETAK)*K2 / (ETAK*KH)                          # s/m
        RBSS = FRSHV * RBSS

    # Bare soil.
    else:
        RBSL = RMAX
        RBSH = RMAX
        RBSS = 0.0

#     Set boundary layer array.
        RB[1] = RBSL
        RB[2] = RBSH
        RB[3] = RBSS

    return (RA, RB, USTAR)

#=======================================================================
#  Computes latent and sensible heat, and temperatures for zonal ET
#  model. Solves system of 10 equations using previous time step's
#  top-layer soil temperature.  Modified from Jagtap (1976), Jagtap
#  and Jones (1989).  Small errors in papers; equations here correct.
#-----------------------------------------------------------------------
#  Called from: ETPHR
#  Calls:       GAUSSJ,MATADD,MATCON,MATPRO,VPSLOP
#=======================================================================
def ETSOLV(DLAYR1, EAIRHR, PSYCON, RL, RNET, RS, STCND1,TAIRHR, TSHR1,
           VHCAIR, VPD):
    import numpy as np
    from HMET import VPSLOP

     #  REAL STCND1,DLAYR1,DZ1,EAIRHR,ECAN,G,PSYCON,RBLCN,SH,LH,
     # &  TAIRHR,TCAN,TSHR1,VHCAIR,VP,VPSLOP,VSP,HOLD
     #  REAL IRL(3,3),IRS(3,3),ONE(1,3),
     # &  RL(3,3),RNET(3,1),RS(3,3),RSL(3,3),RSLRN(3,1),
     # &  VHAIRS(3,3),VPD(3,1),
     # &  VPIRLD(3,1),VSPIRL(3,3),XMAT(3,3)
    LHEAT = np.zeros((4, 2), dtype=float)
    SHEAT = np.zeros((4, 2), dtype=float)
    TSURF = np.zeros((4, 2), dtype=float)
    YMAT = np.zeros((3, 1), dtype=float)
    VPIRLD = np.zeros((3, 1), dtype=float)
    CMAT = np.zeros((3, 1), dtype=float)
    RSLRN = np.zeros((3, 1), dtype=float)
    TDIFF = np.zeros((3, 1), dtype=float)
    LMAT = np.zeros((1, 1), dtype=float)
    SMAT = np.zeros((1, 1), dtype=float)

    ONE = [1.0,1.0,1.0]
     #
     # Initialize

    VSP = VHCAIR * VPSLOP(TAIRHR) / PSYCON
    VP = VHCAIR / PSYCON
    DZ1 = DLAYR1 / 2.0

    # Invert latent and sensible heat matrices to get [iRL] and [iRS].
    # Create temporary [XMAT] = [iRSL] = [C4] then adjust for G estimated from
    # last E-zone temperature.  [XMAT] inverted to get [RSL].
    # VHAIRS and VSPIRL saved for later use.

    IRL = GAUSSJ(RL,3)
    IRS =  GAUSSJ(RS,3)
    VSPIRL = MATCON(VSP,IRL,3,3,'*')
    VHAIRS = MATCON(VHCAIR,IRS,3,3,'*')
    XMAT = MATADD(VHAIRS,VSPIRL,3,3,'+')
    XMAT[3,3] = XMAT[3,3] + STCND1/DZ1
    RSL = GAUSSJ(XMAT,3)

    # Create [CMAT] = VP*[RSL]*[IRL]*[VPD].  Adjust [VPiRLD] = [C3] for G
    # estimated from last E-zone temp.  Original matrix [VPIRLD] restored
    # for later use.

    MATPRO(IRL,VPD,3,3,1,YMAT)
    MATCON(VP,YMAT,3,1,'*',VPIRLD)
    HOLD = VPIRLD[3,1]
    VPIRLD[3,1] = HOLD - STCND1/DZ1*(TSHR1-TAIRHR)
    MATPRO(RSL,VPIRLD,3,3,1,CMAT)
    VPIRLD[3,1] = HOLD

    #  Solve for TDIFF matrix.
    MATPRO(RSL,RNET,3,3,1,RSLRN)
    MATADD(VHAIRS,VSPIRL,3,3,'+',XMAT) # Check this--XMAT used?
    MATADD(RSLRN,CMAT,3,1,'-',TDIFF)
    MATCON(TAIRHR,TDIFF,3,1,'+',TSURF)

    # Solve remaining 6 equations for latent and sensible heat fluxes and
    # sum up latent and sensible heats for 3 zones.
    MATPRO(VHAIRS,TDIFF,3,3,1,SHEAT)
    MATPRO(VSPIRL,TDIFF,3,3,1,YMAT)
    MATADD(YMAT,VPIRLD,3,1,'+',LHEAT)
    MATPRO(ONE,LHEAT,1,3,1,LMAT)
    MATPRO(ONE,SHEAT,1,3,1,SMAT)
    LH = LMAT[1,1]
    SH = SMAT[1,1]

    # Calculate other values.  RBLCN is any off-diagonal element of RS.

    RBLCN = RS[1,2]
    ECAN = (LH*RBLCN/VP) + EAIRHR
    TCAN = (SH*RBLCN/VHCAIR) + TAIRHR
    G = STCND1 / DZ1 * (TSURF[3,1]-TSHR1)

    return (ECAN, G, LH, LHEAT, SH, SHEAT, TCAN, TSURF)
#=======================================================================
#  GAUSSJ, Subroutine, Numerical Recipes,
#  Inverts N x N matrix using Gauss-Jordan elimination with full
#  pivoting (Numerical Recipes, Press et al., 1986, pg. 28).
#  The original matrix is AMAT and the inverse AINV.
#-----------------------------------------------------------------------
#  Called from: ETSOLV
#  Calls:
#=======================================================================
def GAUSSJ(AMAT,N):
    import numpy as np
    from WARNING import WARNING

    NMAX = 10
#       INTEGER I,ICOL,IIROW,J,K,L,LL,N
#       REAL AMAT(N,N),,BIG,DUM,PIVINV,ZERO

    AINV = np.zeros((N, N),dt=float)
    IPIV = np.zeros(NMAX, dtype=float)
    INDXR = np.zeros(NMAX, dtype=float)
    INDXC = np.zeros(NMAX, dtype=float)

    ZERO = 1.0E-6
    MSG = ['']

#      Initialize.
    for J in range (1,N+1):
        for K in range (1,N+1):
            AINV[J,K] = AMAT[J,K]

    for J in range(1,N+1):
        IPIV[J] = 0

#   Main loop over columns to be reduced.
    for I in range(1,N):
        BIG = 0.0

#  Search for pivot (or largest) element and store position.
    for J in range(1,N+1):
        if IPIV[J] != 1:
            for K in range (1,N+1):
                if IPIV[K] == 0:
                    if abs(AINV[J,K]) >= BIG:
                        BIG = abs(AINV[J,K])
                        IROW = J
                        ICOL = K
                elif IPIV[K] > 1:
                    MSG[1] = "SINGULAR MATRIX IN GAUSSJ."
                    WARNING(1,"GAUSSJ",MSG)
                    # WRITE(*,*) MSG(1)
                    # PAUSE 'SINGULAR MATRIX IN GAUSSJ.'
    IPIV[ICOL] = IPIV[ICOL]+1

#        Interchange rows by relabeling.  INDXC(I) = col of i'th pivot
#        element, INDXR(I) = original row where pivot element was located.

    if IROW != ICOL:
        for L in range(1,N+1):
            DUM = AINV[IROW,L]
            AINV[IROW,L] = AINV[ICOL,L]
            AINV[ICOL,L] = DUM

# C       Divide pivot row by pivot element at (IROW,ICOL).

    INDXR[I] = IROW
    INDXC[I] = ICOL
    if abs(AINV[ICOL,ICOL]) <= ZERO:
        MSG[1] = "SINGULAR MATRIX IN GAUSSJ."
        WARNING(1,"GAUSSJ",MSG)
        # WRITE(*,*) MSG(1)
        # PAUSE 'SINGULAR MATRIX - GAUSSJ'
    PIVINV = 1.0/AINV[ICOL,ICOL]
    AINV[ICOL,ICOL] = 1.0
    for L in range (1,N+1):
        AINV[ICOL,L] = AINV[ICOL,L]*PIVINV

#      Reduce pivot rows except for pivot one.
    for LL in range (1,N):
        if LL != ICOL:
            DUM = AINV[LL,ICOL]
            AINV[LL,ICOL] = 0.0
            for L in range(1,N):
                AINV[LL,L] = AINV[LL,L]-AINV[ICOL,L]*DUM

#      Unscramble AINV by interchanging col pairs in the reverse order
#      that the permutation was created.  INDXR(I)<>INDXR(I) ==> column
#      interchange needed.

    for L in range(N,1,-1):
        if INDXR[L] != INDXC[L]:
            for K in range (1,N):
                DUM = AINV[K,INDXR[L]]
                AINV[K,INDXR[L]] = AINV[K,INDXC[L]]
                AINV[K,INDXC[L]] = DUM

    return AINV
#=======================================================================
#  MATADD, Subroutine,
#  Matrix addition.
#-----------------------------------------------------------------------
#  Called from: ETSOLV
#  Calls:
#=======================================================================
def MATADD(AMAT,BMAT,NROW,NCOL,OPERND):
    import numpy as np
#       SAVE
#
#       CHARACTER*1 OPERND
#       INTEGER I,J,NCOL,NROW
#       REAL AMAT(NROW,NCOL),BMAT(NROW,NCOL),CMAT(NROW,NCOL)

    CMAT = np.zeros((NROW, NCOL), dtype=float)
#
# C     Addition of two matrices.
#
    for I in range (1,NROW+1):
        for J in range(1,NCOL+1):
            if OPERND == '+':
                CMAT[I,J] = AMAT[I,J] + BMAT[I,J]
            elif OPERND == '-':
                CMAT[I,J] = AMAT[I,J] - BMAT[I,J]

    return CMAT
#=======================================================================
#  MATCON, Subroutine
#  Matrix multiplication/addition of a constant (CONST).
#-----------------------------------------------------------------------
#  Called from: ETSOLV
#  Calls:
#=======================================================================

def MATCON(CONST,BMAT,NROW,NCOL,OPERND):
    import numpy as np

#       CHARACTER*1 OPERND
#       INTEGER I,J,NROW,NCOL
#       REAL CONST,BMAT(NROW,NCOL),CMAT(NROW,NCOL)
    CMAT = np.zeros((NROW, NCOL), dtype=float)
#
# C     Loop for all elements of [CMAT].

    for I in range(1,NROW+1):
        for J in range(1,NCOL+1):
            if OPERND == '+':
                CMAT[I,J] = CONST + BMAT[I,J]
            elif OPERND == '*':
                CMAT[I,J] = CONST * BMAT[I,J]

    return CMAT

#=======================================================================
#  MATPRO, Subroutine
#  Matrix product or multiplication.
#-----------------------------------------------------------------------
#  Called from: ETSOLV
#  Calls:
#=======================================================================

def MATPRO(AMAT,BMAT,NROWA,NCOM,NCOLB):
    import numpy as np

#       INTEGER I,J,K,NROWA,NCOM,NCOLB
#       REAL AMAT(NROWA,NCOM),BMAT(NCOM,NCOLB),,SUM

    CMAT= np.zeros((NROWA, NCOLB), dtype=float)
#
# C     Multiplication of two matrices.  Loop for all elements of [CMAT].
#
    for I in range(1,NROWA+1):
        for J in range(1,NCOLB+1):
            SUM = 0.0

            # Sum products for each element of [CMAT]--along [AMAT] and down [BMAT]

            for K in range(1,NCOM+1):
                SUM = SUM + AMAT[I,K] * BMAT[K,J]

            # Set each element of [CMAT] equal to the sum.

            CMAT[I,J] = SUM

    return CMAT

#=======================================================================
#  RADB, Subroutine, N.B. Pickering
#  Calculates net/back long wave or thermal radiation from leaves
#  and soil.
#-----------------------------------------------------------------------
#  Called from: CANPET
#  Calls:
#=======================================================================
def RADB(CLOUDS, EAIRHR, FRSHV, LAISHV,LAISLV, TAIRHR, TCAN):
    import numpy as np
#
#       REAL CLOUDS,DELT,EMISA,EMISA0,EMISL,EMISS,EMISS0,FRSHV,
#      &  LAISHV,LAISLV,RADBK(3),EAIRHR,SBZCON,TAIRHR,TKAIR,TK4SKY,
#      &  TSKY,XLAI,EMISAV,RBKLF,RBACK,TCAN,TK4CAN,ZERO
    RADBK = np.zeros(3, dtype=float)

    SBZCON = 5.675E-8
    DELT = 11.0
    EMISL = 0.97
    EMISS0 = 0.9
    ZERO = 1.0E-6

    # Initialize.  Apparent atmospheric emissivity from Brutsaert (1982)
    # and Monteith and Undsworth (1990)

    XLAI = LAISLV + LAISHV
    TKAIR = TAIRHR + 273.0
    TK4CAN = (TCAN+273.0)**4
    EMISA0 = 1.24 * (EAIRHR/100.0/TKAIR)**(1.0/7.0)     #EAIRHR in Pa
    EMISA = CLOUDS*(1.0-(1.0-EMISA0)*4*DELT/TKAIR) + (1.0-CLOUDS)*EMISA0
    TK4SKY = EMISA * TKAIR**4
    TSKY = TK4SKY**0.25 - 273.0
    EMISS = EMISS0 + 0.07

    # Calculate thermal losses from sunlit leaves and soil.  Soil surface
    # assumed to have view factor of FRSHV with TEMPSH and (1.-FRSHV)
    # with sky temperature.  Energy loss from leaf is porportionally
    # weighted according to leaf area index.  NEED VIEW FACTOR FOR LEAVES!
    EMISAV = FRSHV*EMISL + (1.0-FRSHV)*EMISS
    RBACK =  EMISAV * SBZCON * (TK4CAN-TK4SKY)

    if XLAI > 0.0:
        RBKLF = FRSHV * RBACK
        RADBK[1] = 0.7 * RBKLF
        RADBK[2] = 0.3 * RBKLF
    else:
        RADBK[1] = 0.0
        RADBK[2] = 0.0

    RADBK[3] = (1.0-FRSHV) * RBACK

    return RADBK

#=======================================================================
#  HSOILT, Subroutine
#  Updates soil temperatures by layer (from Hillel, 1975) using
#  soil surface temperature calculated in ETSOLV.
#-----------------------------------------------------------------------
#  Called from: ETPHR
#  Calls:
#=======================================================================
def HSOILT(DLAYR2, NLAYR, SHCAP, STCOND, TA, TEMPSS):
    import numpy as np
    from ModuleDefs import TS, NL
# !     ------------------------------------------------------------------

#       INTEGER I,NLAYR
#       REAL DT,DZ,DLAYR2(NL),INFLOW,OUTFLO,SCOND,STCOND(NL),SHCAP(NL),
#      &  TA,TINCR,,TEMPSS,
    TSHR =  np.zeros(NL, dtype=float)
    VHCAP =np.zeros(NL, dtype=float)
    TINCR=24.0/TS*3600.0
#
# C     Calculate volumetric heat capacity at each node.

    for I in range(1,NLAYR+1):
        VHCAP[I] = TSHR[I] * SHCAP[I] * DLAYR2[I]/100.0

#      Loop for flux between soil layers.  Influx at top and bottom of profile
#      passes through half the layer thickness.  Bottom of profile is assumed to be
#      a slowly-varying temperature boundary condition (TA).

    for I in range(1,NLAYR+1):
        if I == 1:
            DT = TEMPSS - TSHR[1]
            DZ = 0.5 * DLAYR2[1]/100.0
            SCOND = STCOND[I]
            INFLOW = DT / DZ * SCOND
        else:
            INFLOW = OUTFLO

        if I == NLAYR:
            DT = TSHR[I] - TA
            DZ = 0.5 * DLAYR2[I]/100.0
            SCOND = STCOND[I]
            OUTFLO = DT / DZ * SCOND
        else:
            DT = TSHR[I] - TSHR[I+1]
            DZ = 0.5 * (DLAYR2[I]+DLAYR2[I+1])/100.0
            SCOND = 0.5 * (STCOND[I]+STCOND[I+1])
            OUTFLO = DT / DZ * SCOND
        VHCAP[I] = VHCAP[I] + (INFLOW-OUTFLO)*TINCR

        # Update soil temperatures.
        for I in range(1,NLAYR+1):
            TSHR[I] = VHCAP[I] / (DLAYR2[I]/100.0*SHCAP[I])

    return TSHR
#=======================================================================