#etinp
#PGINP

#ETIND
#PGIND
#SOIL10
#SOIL05
# C=======================================================================
# C  RADABS, Subroutine, N.B. Pickering
# C  Calculates hourly canopy absorption of PAR and IR radiation.
# C-----------------------------------------------------------------------
# C  REVISION HISTORY
# C  03/31/91 NBP Written
# C-----------------------------------------------------------------------
# C  Called from: ETPHOT
# C  Calls:       CANABS,LFEXTN,SHADOW
# C=======================================================================
import math
from ERROR import ERROR
from READS import FIND, IGNORE
import numpy as np
from WARNING import WARNING
from ModuleDefs import NL
def RADABS(AZIR, AZZON, BETA, BETN, CANHT, CANWH, DAYTIM, FRDIFP, FRDIFR, H, LFANGD, MEEVP, MEPHO, PALB, PARHR, RADHR, ROWSPC, SALB, SCVIR, SCVP, XLAI):

#       IMPLICIT NONE
#       EXTERNAL WARNING, SHADOW, LFEXTN, CANABS
#       SAVE
    PARSUN = [0.0, 0.0, 0.0]
    IRSUN  = [0.0, 0.0, 0.0]
    RABS   = [0.0, 0.0, 0.0]
#       CHARACTER MEEVP*1,MEPHO*1
    MESSAGE = np.full(10, '', dtype='U78')
#       INTEGER H
#       LOGICAL DAYTIM
#       REAL AZIR,AZZON,BETA,BETN,CANHT,CANWH,FRACSH,FRDIFP,FRDIFR,
#      &  FRSHV,KDIFBL,KDIRBL,KDRBLV,LAISH,LAISL,LAISHV,LAISLV,
#      &  LFANGD(3),PALB,PARHR,PARSH,PARSUN(3),PARSL,PARSS,PCINTP,
#      &  PCABSP,PCINTR,PCABSR,PCREFP,PCREFR,RADHR,RABS(3),RNG,
#      &  PTOT,RTOT,ROWSPC,SALB,SCVP,SCVIR,XLAI,PARQC,PARW,IRHR,
#      &  IRALB,FRDIFI,PCABSI,PCINTI,PCREFI,IRSH,IRSUN(3),IRSL,IRSS
    PARQC=4.6
    FRSHV = LAISHV = LAISLV = 0.0
    KDRBLV = 0.0
    RNG = 0.0
# C     Initialize.
#
    if H == 1:
        FRSHV = 0.0
        LAISHV = 0.0
        LAISLV = 0.0

    FRACSH = 0.0
    KDIRBL = 0.0
    LAISH = 0.0
    LAISL = 0.0
    PCABSP = 0.0
    PCINTP = 0.0
    PCREFP = 0.0
    PARSUN[1] = 0.0
    PARSUN[2] = 0.0
    PARSUN[3] = 0.0
    PARSL = 0.0
    PARSH = 0.0
    PARSS = 0.0
    PCABSR = 0.0
    PCINTR = 0.0
    PCREFR = 0.0
    RABS[1] = 0.0
    RABS[2] = 0.0
    RABS[3] = 0.0

    if XLAI > 0.0:
#
# C       Calculate fraction shaded and LAI's for vertical sun position.
#
        if H == 1:
            FRSHV = SHADOW(AZIR, AZZON, 90.0, BETN, CANHT, CANWH, ROWSPC)
            KDIFBL, KDRBLV, LAISHV, LAISLV,RNG = LFEXTN(90.0, FRSHV, LFANGD, XLAI)

#
        if DAYTIM:
#
# C         Calculate fraction shaded, leaf extinction and LAI's.
#
            FRACSH = SHADOW(AZIR, AZZON, BETA, BETN, CANHT, CANWH, ROWSPC)
            KDIFBL, KDIRBL, LAISH, LAISL = LFEXTN(BETA, FRACSH, LFANGD, XLAI)
# C         Calculate PAR absorbed by canopy during day.
#
            if MEPHO == 'L' or MEEVP == 'Z':
                PCABSP, PCINTP, PCREFP, PARSH, PARSS, PARSUN = CANABS(
                    PALB, BETA, BETN, CANHT, CANWH, FRACSH,
                    FRDIFP, KDIFBL, KDIRBL, LAISH, LAISL, PARHR,
                    RNG, ROWSPC, SCVP
                )
                PARSL = PARSUN[2]
#
# C         Calculate infrared radiation absorbed by canopy during day.
#
            if MEEVP == 'Z':
                PARW = PARHR / PARQC
                IRHR = RADHR - PARW
                if IRHR > 0.0:
                    IRALB = min((SALB * RADHR - PALB * PARW) / IRHR, 1.0)
                    FRDIFI = min((FRDIFR * RADHR - FRDIFP * PARW) / IRHR, 1.0)
                else:
                    MESSAGE[1] = f'Error in RADHR or PARHR for hour {H}.'
                    MESSAGE[2] = 'Program will stop.'
                    WARNING(2, 'RADABS', MESSAGE)
                    print(MESSAGE[1])
                    exit()
                PCABSI, PCINTI, PCREFI, IRSH, IRSS, IRSUN = CANABS(
                    IRALB, BETA, BETN, CANHT, CANWH, FRACSH,
                    FRDIFI, KDIFBL, KDIRBL, LAISH, LAISL, IRHR,
                    RNG, ROWSPC, SCVIR
                )
                IRSL = IRSUN[2]

            # C           Convert total radiation to land area basis.

                RABS[1] = (IRSL + PARSL / PARQC) * LAISL
                RABS[2] = (IRSH + PARSH / PARQC) * LAISH
                RABS[3] = IRSS + PARSS / PARQC
                if RADHR > 0.:
                    PCABSR = (PCABSI * IRHR + PCABSP * PARW) / RADHR
                    PCINTR = (PCINTI * IRHR + PCINTP * PARW) / RADHR
                    PCREFR = (PCREFI * IRHR + PCREFP * PARW) / RADHR

        else:
#
# C         Night time with canopy.
#
            FRACSH = FRSHV
            KDIRBL = KDRBLV
            LAISH = LAISHV
            LAISL = LAISLV
    else:
#
# C       Bare soil (day or night).
#
        if MEPHO == 'L' or MEEVP == 'Z':
            PCABSP = (1.0 - PALB) * 100.0
            PCREFP = PALB * 100.0
            PARSS = (1.0 - PALB) * PARHR
        if MEEVP == 'Z':
            PCABSR = (1.0 - SALB) * 100.0
            PCREFR = SALB * 100.0
            RABS[3] = (1.0 - SALB) * RADHR

#
# C     Energy balance check.
#
    PTOT = PARSL * LAISL + PARSH * LAISH + PARSS + PARHR * PCREFP / 100.0
    RTOT = RABS[1] + RABS[2] + RABS[3] + RADHR * PCREFR / 100.0

#
    return FRACSH, FRSHV, KDIRBL, KDRBLV, LAISH, LAISHV, LAISL, LAISLV, PARSH, PARSUN, PCABSP, PCABSR, PCINTP, PCINTR, RABS
#       END SUBROUTINE RADABS
# C=======================================================================
# C  SHADOW, Subroutine, N.B. Pickering, J.W. Jones
# C  Calculates fraction shaded for sun and row geometry using ellipses.
# C-----------------------------------------------------------------------
# C  REVISION HISTORY
# C  03/14/91 NBP Written
# C  11/15/91 NBP Modified
# C  11/23/93 NBP Included more error checks and limits
# C  08/20/21 CHP Added error protection
# C  09/13/21 FO  Updated error call because of compiler issue.
# C-----------------------------------------------------------------------
# C  Called from: RADABS
# C  Calls:
# C=======================================================================
#
def SHADOW(AZIR, AZZON, BETA, BETN, CANHT, CANWH, ROWSPC):
#
#       IMPLICIT NONE
#       EXTERNAL ERROR
#       SAVE
#
#       CHARACTER*6 ERRKEY
#       REAL A,B,C1,C2,C3,C4,AZIMD,AZIR,AZZON,BETA,BETN,CANHT,CANWH,ETA,
#      &  FRACSH,GAMMA,PI,RAD,RBETA,ROWSPC,SHADE,SHLEN,SHPERP,STOUCH,ZERO
#       PARAMETER (PI=3.14159, RAD=PI/180.0, ZERO=1.0E-6)
#       PARAMETER (ERRKEY = 'SHADOW')
    PI=3.14159
    RAD=PI/180.0
    ZERO=1.0E-6
    ERRKEY = 'SHADOW'
# C     Set fraction shaded to 0.0 for zero width or height.
#
    if CANWH <= 0.0 or CANHT <= 0.0:
        FRACSH = 0.0

    # C     Set fraction shaded to 1.0 for full cover.

    #       ELSE IF (CANWH .GE. ROWSPC) THEN
    elif CANWH >= ROWSPC:
        FRACSH = 1.0

    # C     Calculate fraction shaded.

    #       ELSE
    else:
        # C       Adjust BETA if sun exactly overhead or at horizon.  Calculate
        # C       acute positive angle between sun and row azimuths. Initialize
        # C       other constants.

        RBETA = min(max(BETA * RAD, 1.0e-6), PI / 2.0 - 1.0e-6)
        AZIMD = abs(AZZON - AZIR) * RAD
        if AZIMD > PI / 2.0:
            AZIMD = PI - AZIMD
        A = (CANWH / CANHT) ** 2
        GAMMA = math.atan(A * math.tan(RBETA))
        C1 = A * (math.tan(RBETA)) ** 2
        C2 = (A * math.tan(RBETA)) ** 2

        # C       Calculate shadow length assuming elliptical plant.
        SHLEN = CANHT * (math.cos(RBETA - GAMMA) / math.sin(RBETA)) * (math.sqrt((1.0 + C2) / (1.0 + C1)))

        # !       CHP 2021-08-20
        SHLEN = max(0.0, SHLEN)

        B = (SHLEN / CANWH) ** 2
        C3 = B * (math.tan(AZIMD)) ** 2
        C4 = (B * math.tan(AZIMD)) ** 2
        STOUCH = SHLEN / (math.cos(AZIMD) * math.sqrt(1.0 + C3))

        # C       CALCULATE FRACTION SHADED.

        # C       Sun parallel to row.  Shadow limited to BETN.

        #         IF (AZIMD .LE. ZERO) THEN
        if AZIMD <= ZERO:
            SHLEN = min(SHLEN, BETN)
            SHADE = 0.25 * PI * SHLEN * CANWH

        # C       Sun not parallel to row.

        #         ELSE
        else:
            # C         Calculate perpendicular shadow length.

            AZIMD = max(AZIMD, 1.0e-6)
            ETA = math.atan(1.0 / (B * math.tan(AZIMD)))
            SHPERP = CANWH * math.sin(AZIMD + ETA) * math.sqrt((1.0 + C4) / (1.0 + C3))

            # C         Hedgerow (plant shadows overlap).

            #           IF (STOUCH .GE. BETN) THEN
            if STOUCH >= BETN:

                # C           Shadow length is perpendicular and limited to ROWSPC.

                SHLEN = min(SHPERP, ROWSPC)
                SHADE = SHLEN * BETN

            # C         Individual plants.

            #           ELSE
            else:

                # C           Limit shadow length to within one ROWSPC.
                # C FO/GH 11/14/2020 Code protections for divisions by zero.
                #             IF (SHPERP .GT. 0.0 .AND. SHPERP .GT. ROWSPC) THEN
                if SHPERP > 0.0 and SHPERP > ROWSPC:
                    SHLEN = SHLEN * ROWSPC / SHPERP
                # !            ELSE
                # !              SHLEN = 0.0
                #             ENDIF

                SHADE = 0.25 * PI * SHLEN * CANWH

            #           ENDIF
        #         ENDIF

        # C FO/GH 11/14/2020 Code protections for divisions by zero.
        #         IF(ROWSPC .GT. 0.0 .AND. BETN .GT. 0.0) THEN
        if ROWSPC > 0.0 and BETN > 0.0:
            FRACSH = min(SHADE / (ROWSPC * BETN), 1.0)
        else:
            # !         chp 2021-08-20 Added error protection.
            raise ValueError("ERROR in SHADOW: ROWSPC or BETN invalid")

    #       ENDIF

    # !     FRACSH = MIN(MAX(FRACSH,1.0E-6),1.0)
    FRACSH = min(max(FRACSH, 0.0), 1.0)

    return FRACSH
#       END SUBROUTINE SHADOW

# C=======================================================================
# C  LFEXTN, Subroutine, N.B. Pickering, K.J. Boote
# C  Computes leaf extinction coefficients based on leaf angle distribution
# C  (Goudriaan, 1988) and sunlit and shaded leaf area indices.
# C-----------------------------------------------------------------------
# C  REVISION HISTORY
# C  ??/??/?? KJB Written
# C  05/14/91 NBP Removed COMMON and reorganized.
# C  11/14/20 FO/GH Code protections for divisions by zero.
# C  12/03/21 FO/GH/CHP Protections to avoid negative leaf are index (LAI)
# C-----------------------------------------------------------------------
# C  Called from: RADABS
# C  Calls:
# C=======================================================================
#
def LFEXTN(BETA, FRACSH, LFANGD, XLAI):
#
#       IMPLICIT NONE
#       SAVE
#
#       REAL BETA,F15,F45,F75,FRACSH,K15,K45,K75,KDIRBL,KDIFBL,LAISH,
#      &  LAISL,LFANGD(3),O15,O45,O75,OAV,PI,RAD,RNG,SINB,VARSIN,XLAI,
#      &  FRAKDI
#     BETA = 0.0
#     FRACSH = 0.0
    PI = 3.14159
    RAD = PI/180.0
# C     Initialization.  F15, F45, and F75 are the proportion of leaves
# C     in the three leaf classes: 0-30, 30-60 and 60-90 degrees.
#
# C FO/GH 11/14/2020 Code protections for divisions by zero.
#       !SINB = MAX(0.00001,SIN(BETA*RAD))
    SINB = math.sin(BETA * RAD)

    F15 = LFANGD[0]
    F45 = LFANGD[1]
    F75 = LFANGD[2]

#
# C     Calculate direct light extinction coefficient for black leaves and
# C     the range of sine of incidence (used in Gaussian integration).
#
    O15 = max(0.26,0.93*SINB)
    O45 = max(0.47,0.68*SINB)
    O75 = 1.0 - 0.268*O15 - 0.732*O45
    OAV = F15*O15 + F45*O45 + F75*O75
#
# C FO/GH 11/14/2020 Code protections for divisions by zero.
    if SINB > 0.0:
        KDIRBL = OAV / SINB
    else:
        KDIRBL = 0.0
    VARSIN = (0.06 * F15 + 0.25 * F45 + 0.467 * F75 +
              (0.81 * F15 + 0.25 * F45 - 0.4 * F75) * SINB ** 2 - OAV ** 2)
    VARSIN = max(VARSIN, 0.0)
    RNG = math.sqrt(12.0 * VARSIN)

#
# C     Calculate diffuse light extinction coefficient for black leaves.
#
    K15 = 1.00 * F15 + 1.82 * F45 + 2.26 * F75
    K45 = 0.93 * F15 + 0.68 * F45 + 0.67 * F75
    K75 = 0.93 * F15 + 0.68 * F45 + 0.29 * F75

#
    if XLAI > 0.0:
        KDIFBL = -math.log(0.25 * math.exp(-K15 * XLAI) + 0.5 * math.exp(-K45 * XLAI) +
                         0.25 * math.exp(-K75 * XLAI)) / XLAI
    else:
        KDIFBL = 0.0

    #
# C     Calculate sunlit and shaded leaf area indices.
# !CHP added check to prevent underflow 1/16/03
# C FO/GH 11/14/2020 Code protections for divisions by zero.
    if KDIRBL > 0.0 and FRACSH > 0.0:
        FRAKDI = FRACSH / KDIRBL
        LAISL = FRAKDI * (1.0 - math.exp(-XLAI / FRAKDI))
    else:
        LAISL = 0.0

    LAISL = min(LAISL, XLAI)

# C-KRT*******************************
# C-KRT  LAISH = XLAI - LAISL
# !-CHP  LAISH = MAX(0.02,XLAI - LAISL)
# C FO/GH 11/14/2020 Code protections for divisions by zero.
    LAISH = max(0.0, XLAI - LAISL)
#
# C-KRT*******************************
    return KDIFBL, KDIRBL, LAISH, LAISL, RNG
#       END SUBROUTINE LFEXTN

# C=======================================================================
# C  CANABS, Subroutine, K.J. Boote, N.B. Pickering
# C  Computes radiation absorbed by soil, sunlit and shaded leaves.
# C-----------------------------------------------------------------------
# C  REVISION HISTORY
# C  ??/??/?? KJB Written
# C  05/14/91 NBP Removed COMMON and reorganized.
# C  12/03/21 FO/GH/CHP Protections to compute diffuse/scattered
# C               components of the direct beam.
# C-----------------------------------------------------------------------
# C  Called from: RADABS
# C  Calls:
# C=======================================================================
#def CANABS(ALBEDO, BETA, BETN, CANHT, CANWH, FRACSH, FRDIF, KDIFBL, KDIRBL,LAISH, LAISL, RADHR, RNG, ROWSPC, SCVR):PCTABS, PCTINT, PCTREF, RADSH, RADSS, RADSUN):

#       SUBROUTINE CANABS(
#      &  ALBEDO, BETA, BETN, CANHT, CANWH, FRACSH,
#      &  FRDIF, KDIFBL, KDIRBL, LAISH, LAISL, RADHR,
#      &  RNG, ROWSPC, SCVR,
#      &  PCTABS, PCTINT, PCTREF, RADSH, RADSS, RADSUN)


def CANABS(ALBEDO, BETA, BETN, CANHT, CANWH, FRACSH, FRDIF, KDIFBL, KDIRBL,
           LAISH, LAISL, RADHR, RNG, ROWSPC, SCVR):
# assuming PCTABS, PCTINT, PCTREF, RADSH, RADSS, RADSUN are outputs

#       IMPLICIT NONE
#       SAVE
#
#       INTEGER I
#       REAL ADDF,ADDR,ADIF,ADIR,AREF,ATOT,ADIRSL,ADDRSL,ADDFSL,
#      &  ADDFSH,ADIFSL,ADIFSH,AREFSL,AREFSH,RDIFSL,ALBEDO,BETA,
#      &  BETN,CANHT,CANWH,DELWP,DELWR,DIFP,DIFPR,DIFR,FRACSH,FRDIF,
#      &  INCSOI,INTCAN,KDIFBL,KDIRBL,LAISH,LAISL,O,OAV,PATHP,PATHR,
#      &  PCTABS,PCTINT,PCTREF,RADDIF,RADDIR,RADHR,     PI,RAD,
#      &  RADSH,RADSS,RADSUN(3),REFDF,REFDIF,REFDIR,REFDR,REFH,
#      &  REFSOI,REFTOT,RNG,ROWSPC,SCVR,SINB,SQV,XLAI,RADTOT
# #
    PI = 3.14159
    RAD = PI / 180.0
    # Initialization
    SQV = math.sqrt(1.0 - SCVR)
    SINB = math.sin(BETA * RAD)
    XLAI = LAISH+LAISL
    RADSUN = np.zeros(4, dtype=float)

# C     Compute reflection coefficients for direct and diffuse light.
    REFH = (1.0 - SQV) / (1.0 + SQV)
    REFDR = 2.0 * KDIRBL / (1.0 + KDIRBL) * REFH
    REFDF = REFH
# C     Split total radiation into direct and diffuse components.
    RADDIF = FRDIF * RADHR
    RADDIR = RADHR - RADDIF
#
# C     Direct radiation absorbed in shaded zone by considering the direct
# C     (ADDR) and diffuse/scattered (ADDF) components of the direct beam.
#
# !     CHP - Added checks to prevent underflows 1/16/03
    if FRACSH > 0.0:
        # !        IF ((KDIRBL*SQV*XLAI/FRACSH) .LT. 20.) THEN
        ADIR = FRACSH * (1.0 - REFDR) * RADDIR * (1.0 - math.exp(-KDIRBL * SQV * XLAI / FRACSH))
# !        ELSE
        # !          ADIR = 0.0
        # !        ENDIF

        # !        IF ((KDIRBL*XLAI/FRACSH) .LT. 20.) THEN
        ADDR = FRACSH * (1.0 - SCVR) * RADDIR * (1.0 - math.exp(-KDIRBL * XLAI / FRACSH))
        # !        ELSE
        # !          ADDR = 0.0
        # !        ENDIF
        # C-KRT****************************
        # C-KRT   ADDF = ADIR - ADDR
        # !-CHP   ADDF = MAX(0.0,ADIR-ADDR)
        # !-FO        ADDF = ADIR - ADDR

        ADDR = min(ADDR, ADIR)
        ADDF = max(0.0, ADIR - ADDR)

        # C-KRT****************************
        # !        IF ((KDIRBL*SQV*LAISL/FRACSH) .LT. 20.) THEN
        ADIRSL = FRACSH * (1.0 - REFDR) * RADDIR * (1.0 - math.exp(-KDIRBL * SQV * LAISL / FRACSH))
        # !        ELSE
        # !          ADIRSL = 0.0
        # !        ENDIF

        # !        IF ((KDIRBL*LAISL/FRACSH) .LT. 20.) THEN
        ADDRSL = FRACSH * (1.0 - SCVR) * RADDIR * (1.0 - math.exp(-KDIRBL * LAISL / FRACSH))
        # !        ELSE
        # !          ADDRSL = 0.0
        # !        ENDIF
        # C-KRT************************************
        # C-KRT   ADDFSL = ADIRSL - ADDRSL
        # C-KRT   ADDFSH = ADDF - ADDFSL
        # !-CHP   ADDFSL = MAX(0.0,ADIRSL - ADDRSL)
        # !-CHP   ADDFSH = MAX(0.0,ADDF - ADDFSL)
        # !-FO        ADDFSL = ADIRSL - ADDRSL

        ADDRSL = min(ADDRSL, ADIRSL)
        ADDFSL = max(0.0, ADIRSL - ADDRSL)

        # !-FO        ADDFSH = ADDF - ADDFSL

        ADDFSL = min(ADDFSL, ADDF)
        ADDFSH = max(0.0, ADDF - ADDFSL)

        # C-KRT************************************
    else:
        ADIR = 0.0
        ADDR = 0.0
        ADDF = 0.0
        ADIRSL = 0.0
        ADDRSL = 0.0
        ADDFSL = 0.0
        ADDFSH = 0.0

    # C     Diffuse skylight is absorbed over an effective area equal to the
    # C     canopy height plus width for an isolated row.  For interfering rows,
    # C     Eqns. 2.130 and 2.128 from Goudriaan (1977) are used.  Concept
    # C     extended for both between plants (P) and rows (R).

    if CANWH < BETN:
        PATHP = BETN - CANWH
        DELWP = PATHP + CANHT - math.sqrt(PATHP**2 + CANHT**2)
        DIFP = min((CANWH + DELWP) / BETN, 1.0)
    else:
        DIFP = 1.0

    if CANWH < ROWSPC:
        PATHR = ROWSPC - CANWH
        DELWR = PATHR + CANHT - math.sqrt(PATHR**2 + CANHT**2)
        DIFR = min((CANWH + DELWR) / ROWSPC, 1.0)
    else:
        DIFR = 1.0

    DIFPR = min(max(DIFP * DIFR, 1.0E-6), 1.0)
    ADIF = DIFPR * (1.0 - REFDF) * RADDIF * (1.0 - math.exp(-KDIFBL * SQV * XLAI / DIFPR))
    ADIFSL = DIFPR * (1.0 - REFDF) * RADDIF * (1.0 - math.exp(-KDIFBL * SQV * LAISL / DIFPR))

    # C-KRT********************************
    # C-KRT ADIFSH = ADIF - ADIFSL
    # !-CHP ADIFSH = MAX(0.0,ADIF - ADIFSL)
    # !-FO      ADIFSH = ADIF - ADIFSL

    ADIFSL = min(ADIFSL, ADIF)
    ADIFSH = max(0.0, ADIF - ADIFSL)

    # C-KRT********************************

    # C     Light reflected from the soil assumed to be isotropic and diffuse.
    # C     Absorption handled in the same manner as diffuse skylight.

    REFDIR = FRACSH * REFDR * RADDIR
    REFDIF = DIFPR * REFDF * RADDIF
    INTCAN = REFDIR + REFDIF + ADIR + ADIF
    INCSOI = RADHR - INTCAN
    REFSOI = ALBEDO * INCSOI
    AREF = DIFPR * (1.0 - REFDF) * REFSOI * (1.0 - math.exp(-KDIFBL * SQV * XLAI / DIFPR))
    AREFSH = DIFPR * (1.0 - REFDF) * REFSOI * (1.0 - math.exp(-KDIFBL * SQV * LAISH / DIFPR))

    # C-KRT********************************
    # C-KRT AREFSL = AREF - AREFSH
    # !-CHP AREFSL = MAX(0.0,AREF - AREFSH)
    # !-FO      AREFSL = AREF - AREFSH

    AREFSH = min(AREFSH, AREF)
    AREFSL = max(0.0, AREF - AREFSH)

    # C-KRT********************************
    ATOT = ADIR + ADIF + AREF
    REFTOT = REFDIR + REFDIF + REFSOI - AREF

    # C     Determine sunlit radiation for each sunlit leaf class,
    # C     shaded leaves and soil.  The average sunlit radiation =RABS(2).

    RADSS = INCSOI * (1.0 - ALBEDO)
    # C     RADSH = (ADDF+ADIF+AREF) / XLAI
    # C FO/GH 11/14/2020 Code protections for divisions by zero.
    if LAISH > 0.0:
        RADSH = (ADDFSH + ADIFSH + AREFSH) / LAISH
    else:
        RADSH = 0.0

    # C FO/GH 11/14/2020 Code protections for divisions by zero.
    if LAISL > 0.0:
        RDIFSL = (ADDFSL + ADIFSL + AREFSL) / LAISL
    else:
        RDIFSL = 0.0

    OAV = KDIRBL * SINB
    # C FO/GH 11/14/2020 Code protections for divisions by zero.
    if SINB > 0.0:
        for I in range(4):
            O = OAV + math.sqrt(0.15) * RNG * (2 - I)
            # C       RADSUN(I) = RADSH + (1.-SCVR)*RADDIR*O/SINB
            RADSUN[I] = RDIFSL + (1.0 - SCVR) * RADDIR * O / SINB
    else:
        RADSUN[:] = 0.0

    # C     Set radiation array and calculate ratios of components.

    if RADHR > 0.0:
        PCTINT = 100.0 * INTCAN / RADHR
        PCTABS = 100.0 * ATOT / RADHR
        PCTREF = 100.0 * REFTOT / RADHR
    else:
        PCTINT = 0.0
        PCTABS = 0.0
        PCTREF = 0.0

    # C     Energy balance check (RADTOT=RADHR).

    RADTOT = ATOT + REFTOT + RADSS
    RADTOT = RADSH * LAISH + RADSUN[1] * LAISL + REFTOT + RADSS

    # C FO/GH 11/14/2020 Code protections for divisions by zero.
    if SINB > 0.0:
        RADTOT = RADSH * LAISH + RDIFSL * LAISL + REFTOT + RADSS + \
            (1.0 - SCVR) * RADDIR * OAV / SINB * LAISL
#
#       RETURN
    return PCTABS, PCTINT, PCTREF, RADSH, RADSS, RADSUN[1:]
#       END SUBROUTINE CANABS

#
# C=======================================================================
# C  ETIND, Subroutine, N.B. Pickering
# C  Initializes daily input variables for ET.
# C-----------------------------------------------------------------------
# C  REVISION HISTORY
# C  10/14/92 NBP Written
# C  11/23/93 NBP SWE returned to ETPHOT.
# C  12/11/93 NBP Made calc. of Rsoil larger, removed AWES1, RWUH greater.
# C               Added noon variable initialization.
# C  03/13/94 NBP Added soil layer conversion: 5,15.. cm to 10,10.. cm.
# C  04/24/94 NBP Replaced 24.0 with TS.
# C-----------------------------------------------------------------------
# C  Called from: ETPHOT
# C  Calls:       SOIL10,TABEX
# C=======================================================================
#
def ETIND(DUL2, RLV, SALBW, SW):
#      &  CEN,DAYRAD,DLAYR2,DULE,DYABSR,DYINTR,EDAY,        !Output
#      &  EOP,ETNOON,FRDFRN,LLE,NELAYR,NLAYR,PCABRN,        !Output
#      &  PCINRN,RADN,RLV2, SALB, SHCAP,ST2,STCOND,SW2,     !Output
#      &  SWE, TDAY,TEMPN,TSRF,TSRFN,XSW,YSCOND,YSHCAP)     !Output
#
# !     ------------------------------------------------------------------
#       USE ModuleDefs     !Definitions of constructed variable types,
#                          ! which contain control information, soil
#                          ! parameters, hourly weather data.
# !     NL, TS defined in ModuleDefs.for
#
#       IMPLICIT NONE
#       EXTERNAL SOIL10, TABEX
#       SAVE
#
#       INTEGER I,J,NELAYR,NLAYR
#       REAL CEN,DAYRAD,DLAYR2(NL),DULE,DYABSR,DYINTR,EDAY,ETNOON,FRDFRN,
#      &  LLE,PCINRN,PCABRN,RADN,SHCAP(NL),ST2(NL),STCOND(NL),
#      &  SW(NL),SW2(NL),SWE,EOP,TABEX,TDAY,TEMPN,TSRF(3),
#      &  TSRFN(3),XC(3),XSW(NL,3),YHC(3),YTC(3),YSCOND(NL,3),
#      &  YSHCAP(NL,3)
#       REAL SALB, SALBW, SALBD
#       REAL, DIMENSION(NL) :: DUL2, RLV, RLV2
#
# C     Initialize.

    SHCAP = np.zeros(NLAYR, dtype=float)
    STCOND = np.zeros(NLAYR, dtype=float)

    EDAY   = 0.0
    ETNOON = 0.0
    FRDFRN = 0.0
    EOP  = 0.0
    TDAY   = 0.0
    DAYRAD = 0.0
    DYABSR = 0.0
    DYINTR = 0.0
    PCINRN = 0.0
    PCABRN = 0.0
    RADN   = 0.0
    ST2 = np.zeros(NL, dtype=float)
    TSRF = np.zeros(3, dtype=float)
    TSRFN = np.zeros(3, dtype=float)
    for I in range(NLAYR+1):
        ST2[I] = 0.0

    TEMPN = 0.0

    for I in range(1,4):
        TSRF[I] = 0.0
        TSRFN[I] = 0.0

#
# C     Transform soil layers for SW and RLV.
#
    DUMMY, NLAYR, SW2 = SOIL10(SW)   # Output
    DUMMY, NLAYR, RLV2 = SOIL10(RLV) # Output

#
# C     Calculate SW in the evaporation zone, soil heat capacity,
# C     and soil thermal conductivity by layer.
#
    SWE = 0.0
    for I in range(1,NELAYR+1):
        SWE = SWE + SW2[I]*DLAYR2[I]*10.0

    #       CEN = MIN(MAX((DULE-SWE)/(DULE-LLE)*100.0,0.0),100.0)
    CEN = min(max((DULE-SWE)/(DULE-LLE)*100.0,0.0),100.0)
    for I in range(1,NLAYR+1):
        for J in range(1,4):
            XC[J] = XSW[I][J]
            YHC[J] = YSHCAP[I][J]
            YTC[J] = YSCOND[I][J]
        SHCAP[I] = TABEX(YHC,XC,SW2[I],3)
        STCOND[I] = TABEX(YTC,XC,SW2[I],3)
#
# C     Calulation of albedo as a function of SW.
#
# !     CHP 7/24/2006
# !     SOILPROP Now includes MSALB (with soil water and mulch effects
# !     on soil albedo) and CMSALB (also includes canopy cover effects)
    if SW2[0] >= DUL2[0]:
        #         SALB = SALBW
        SALB = SALBW
    else:
        #         SALBD = SALBW * 1.25
        SALBD = SALBW * 1.25
        #         SALB = SALBD - (SALBD-SALBW)/DUL2(1)*SW2(1)
        SALB = SALBD - (SALBD - SALBW) / DUL2[0] * SW2[0]

    return (CEN, DAYRAD, DLAYR2, DULE, DYABSR, DYINTR, EDAY,EOP, ETNOON, FRDFRN, LLE, NELAYR, NLAYR, PCABRN,PCINRN, RADN, RLV2, SALB, SHCAP, ST2, STCOND,SW2, SWE, TDAY, TEMPN, TSRF, TSRFN, XSW, YSCOND, YSHCAP)
#       END SUBROUTINE ETIND
#
# C=======================================================================
# C  PGIND, Subroutine, N.B. Pickering
# C  Initializes daily input variables for PG.
# C-----------------------------------------------------------------------
# C  REVISION HISTORY
# C  10/14/92 NBP Written
# C  12/11/93 NBP Added noon variable initialization.
# C-----------------------------------------------------------------------
# C  Called from: ETPHOT
# C  Calls:
# C=======================================================================
#
def PGIND(NLAYR, PALBW, DUL2, SW):
#
#       USE MODULEDEFS
#       IMPLICIT NONE
#       EXTERNAL SOIL10
#       SAVE
#
#       INTEGER NLAYR
#       REAL DAYPAR,DYABSP,DYINTP,DUL2(NL),FRDFPN,LMXSLN,LMXSHN,PALB,
#      &  PALBD,PALBW,PARN,PGCO2,PGDAY,PGNOON,PCABPN,PCINPN,PNLSLN,
#      &  PNLSHN,SLWSLN,SLWSHN,SW(NL),SW2(NL)
#
# C     Initialize.
#
    DAYPAR = 0.0
    DYABSP = 0.0
    DYINTP = 0.0
    FRDFPN = 0.0
    LMXSLN = 0.0
    LMXSHN = 0.0
    PARN   = 0.0
    PGCO2  = 0.0
    PGDAY  = 0.0
    PGNOON = 0.0
    PCABPN = 0.0
    PCINPN = 0.0
    PNLSLN = 0.0
    PNLSHN = 0.0
    SLWSLN = 0.0
    SLWSHN = 0.0
#
# C     Calculate SW in the transformed layers.
#
#       CALL SOIL10(
#      &  SW,                                               !Input
#      &  0,NLAYR,SW2)                                      !Output

    SW2 = SOIL10(SW,0,NLAYR)  # Python equivalent of CALL SOIL10

    #
    # C     Calulation of albedo as a function of SW.
    #
    if SW2[0] >= DUL2[0]:
        PALB = PALBW
    else:
        PALBD = PALBW * 1.25
        PALB = PALBD - (PALBD - PALBW) / DUL2[0] * SW2[0]

#
    return DAYPAR, DYABSP, DYINTP, FRDFPN, LMXSLN, LMXSHN, PALB, PARN, PGCO2, PGDAY, PGNOON, PCABPN, PCINPN, PNLSLN, PNLSHN, SLWSLN, SLWSHN, SW2
#       END SUBROUTINE PGIND
# C=======================================================================
# C  SOIL10, Subroutine, N.B. Pickering
# C  Converts from soil layers of 5,15.. cm to 10,10.. cm.
# C-----------------------------------------------------------------------
# C  REVISION HISTORY
# C  03/13/94 NBP Written
# C-----------------------------------------------------------------------
# C  Called from: ETIND,ETINP
# C  Calls:
# C=======================================================================
#
def SOIL10(ARRAY, ICODE, NLAYR):
# !     ------------------------------------------------------------------
#       USE ModuleDefs     !Definitions of constructed variable types,
#                          ! which contain control information, soil
#                          ! parameters, hourly weather data.
#       IMPLICIT NONE
#       SAVE
#
    ARRAY2 = np.zeros(NL+1, dtype=float)
    for I in range(1, NL+1):
        ARRAY2[I] = 0.0
    if ICODE == 1:
        ARRAY2[1] = 10.0
        ARRAY2[2] = 10.0
    else:
        ARRAY2[1] = 0.5 * (ARRAY[1] + ARRAY[2])
        ARRAY2[2] = ARRAY[2]
    for L in range(3, NLAYR+1):
        ARRAY2[L] = ARRAY[L]

    return ARRAY2[1:]
#       END SUBROUTINE SOIL10
# C=======================================================================
# C  SOIL05, Subroutine, N.B. Pickering
# C  Converts from soil layers of 10,10.. cm to 5,15.. cm.
# C-----------------------------------------------------------------------
# C  REVISION HISTORY
# C  03/13/94 NBP Written
# C-----------------------------------------------------------------------
# C  Called from: ETPHOT
# C  Calls:
# C=======================================================================
#
def SOIL05(ARRAY2, ICODE, NLAYR):
#
# !     ------------------------------------------------------------------
#       USE ModuleDefs     !Definitions of constructed variable types,
#                          ! which contain control information, soil
#                          ! parameters, hourly weather data.
#       IMPLICIT NONE
#       SAVE
#
#       INTEGER I,ICODE,L,NLAYR
    ARRAY = np.zeros(NLAYR, dtype=float)         # ARRAY[0] to ARRAY[NL-1]
    ARRAY2 = np.zeros(ARRAY2, dtype=float)
#
#       DO I=1,NL
    for I in range(1, NL):
        ARRAY[I] = 0.0
    #       ENDDO
    if ICODE == 1:
        ARRAY[1] = 5.0
        ARRAY[2] = 15.0
    else:
        ARRAY[1] = ARRAY2[1]
        ARRAY[2] = (ARRAY2[1] + 2.0 * ARRAY2[2]) / 3.0
    for L in range(3, NLAYR + 1):
        ARRAY[L] = ARRAY2[L]
    #       ENDDO

#
    return ARRAY
#       END SUBROUTINE SOIL05
#
# C=======================================================================
# C  ETINP, Subroutine, N.B. Pickering, 12/12/90
# C  Reads in and initializes fixed ET parameters.
# C------------------------------------------------------------------------
# C  REVISION HISTORY
# C  12/05/1990 NBP Written.
# C  05/10/1994 NBP Added PGFAC3,SLWSLO,LNREF,SLWREF
# C  09/21/1999 NBP Moved weather parameters to ETPSIM.
# C  09/25/1999 NBP Separate ET and PHOT routines.
# C  01/20/2000 NBP Put ETINF in ETINP.
# C  08/12/2003 CHP  Added I/O error checking
# C-----------------------------------------------------------------------
# C  Called from: ETPHOT
# C  Calls:       ERROR,FIND,SOIL10
# C=======================================================================
#
def ETINP(BD, DLAYR, DUL, FILEIO, LL, LUNIO, NLAYR,SALBW, SAT):

# !     ------------------------------------------------------------------
#       USE ModuleDefs
#       IMPLICIT NONE
#       EXTERNAL FIND, GETLUN, IGNORE, SOIL10, ERROR
#       SAVE
#
#       CHARACTER BLANK*1,ERRKEY*6,FILEC*12,FILECC*92,FILEIO*30,
#      &  PATHCR*80,SECTION*6
#       CHARACTER*80 C80
#       INTEGER ERRNUM,FOUND,I,J,LNUM,LUNCRP,LUNIO,NELAYR,NLAYR,
#      &  PATHL, ISECT
    BLANK = ' '
    ERRKEY = 'ETPINP'

#       REAL AZIR,BD(NL),BD2(NL),BETN,CEC,CORRN, DLAYR(NL),
#      &  DLAYR2(NL),DUL(NL),DUL2(NL),DULE,GA,GC,HCAPS(NL),HCAPS2(NL),
#      &  KSOIL,LFANGB,LFANGD(3),LL(NL),LL2(NL),LLE,LWIDTH,N1,PALBW,
#      &  PLTPOP,RCUTIC,ROWSPC,SALBW, SCVIR,SCVP,
#      &  SWEF,TCAIR,TCNDS2(NL),TCONDS(NL),TCWATR,VHCWAT,XAIR,
#      &  XPORE,XSOIL,XSW(NL,3),XWATER,YSCOND(NL,3),YSHCAP(NL,3),ZERO
    NL = NLAYR
    XSW     = np.zeros((NL, 3), dtype=float)
    YSCOND  = np.zeros((NL, 3), dtype=float)
    YSHCAP  = np.zeros((NL, 3), dtype=float)
    DLAYR2 = np.zeros(NL + 1)
    DUL2   = np.zeros(NL + 1)
    LFANGD = np.zeros(4, dtype=float)
    LL2    = np.zeros(NL, dtype=float)
    HCAPS = np.zeros(NL, dtype=float)
    TCONDS = np.zeros(NL, dtype=float)
    VHCWAT=4.18E+6
    TCAIR=0.025
    TCWATR=0.57
    ZERO=1.0E-6
#       REAL PHTHRS10
#       REAL, DIMENSION(NL) :: SAT, SAT2
#
# C     Read IBSNAT35.INP file.
#
    try:
        with open(FILEIO, 'r') as f:
            pass
    except FileNotFoundError as e:
        raise RuntimeError(f"{ERRKEY}: cannot open file {FILEIO}") from e

#
    SECTION = "*FILES"
    LNUM, FOUND=FIND(LUNIO, SECTION)
    if FOUND == 0:
        ERROR(SECTION, 42, FILEIO, LNUM)
#
    #FILEC, PATHCR = READ(LUNIO, '(////,15X,A,1X,A)', IOSTAT=ERRNUM)-------------------------------------TP
    for _ in range(4):
        LUNIO.readline();
        LNUM += 1

    line = LUNIO.readline();
    LNUM += 1
    try:
        tail = line[15:]
        parts = tail.rstrip('\n')
        filec, pathcr = parts.split(None, 1) if ' ' in parts.strip() else (parts.strip(), "")
        FILEC, PATHCR = filec, pathcr
        ERRNUM = 0
    except Exception:
        ERRNUM = 1
        ERROR(ERRKEY, ERRNUM, FILEIO, LNUM)
    LNUM = LNUM + 5
    if ERRNUM != 0:
        ERROR(ERRKEY, ERRNUM, FILEIO, LNUM)
#
    PATHL = INDEX(PATHCR, BLANK)
    if PATHL <= 1:
        FILECC = FILEC
    else:
        FILECC = PATHCR[0:PATHL - 1] + FILEC

#
    LUNIO.seek(0)
    SECTION = "*PLANT"
    LNUM, FOUND=FIND(LUNIO, SECTION)
    if FOUND == 0:
        ERROR(SECTION, 42, FILEIO, LNUM)


    try:
        line = LUNIO.readline()
        PLTPOP = float(line[24:30])
        ROWSPC = float(line[42:48])
        AZIR = float(line[48:54])
        ERRNUM = 0
    except Exception:
        ERRNUM = 1
        ERROR(ERRKEY, ERRNUM, FILEIO, LNUM + 1)

    LNUM = LNUM + 1
    if ERRNUM != 0:
        ERROR(ERRKEY, ERRNUM, FILEIO, LNUM + 1)

        for I in range(1, NL + 1):
            HCAPS[I - 1] = 0.0
            TCONDS[I - 1] = 0.0

#
# !     PHTHRS(10) needed for calculation of CUMSTR
    LUNIO.seek(0)
    SECTION = "*CULTI"


    LNUM, FOUND= FIND(LUNIO, SECTION)
    if FOUND == 0:
        ERROR(SECTION, 42, FILEIO, LNUM)

    ERRNUM = 0
    #PHTHRS10 = READ(LUNIO, '(60X,F6.0)', IOSTAT=ERRNUM)---TP

    LNUM = LNUM + 1
    if ERRNUM != 0:
        ERROR(ERRKEY, ERRNUM, FILEIO, LNUM)

    LUNIO.close()
#
# C     Read species file.
    GETLUN('FILEC', LUNCRP)
    try:
        LUNCRP = open(FILECC, 'r')
    except FileNotFoundError as e:
        ERRNUM = e.errno
        ERROR(ERRKEY, ERRNUM, FILECC, 0)

#
    SECTION = '!*PHOT'
    LNUM, FOUND = FIND(LUNCRP, SECTION)
    for I in range(1, 9):  # Read 8th line of photosynthesis section
        ISECT, C80 = IGNORE(LUNCRP, LNUM)
    try:
        SCVP = float(C80[6:12])
        LFANGB = float(C80[18:24])
        ERRNUM = 0
    except Exception:
        ERRNUM = 1
        ERROR(ERRKEY, ERRNUM, FILECC, LNUM)

        if ERRNUM != 0:
            ERROR(ERRKEY, ERRNUM, FILECC, LNUM)

    LUNCRP.close()

# C     Initialize some parameters.
#
    SCVIR = 4.0 * SCVP
    PALBW = 0.6 * SALBW

    ROWSPC = ROWSPC/100.0
    if ROWSPC > 0.0 and PLTPOP > 0.0:
        BETN = 1.0 / (ROWSPC * PLTPOP)
    else:
        BETN = 0.0

    NELAYR = 1
    LWIDTH = 0.02
    RCUTIC = 5000.0
    for I in range(1, NLAYR + 1):
        HCAPS[I - 1] = 2.17e6
        TCONDS[I - 1] = 7.8

    SWEF = 0.9 - 0.00038 * (DLAYR[0] - 30.0) ** 2

#
# C     Transform soil layers from 5,15,etc. to 10,10,etc. (10 in top layer
# C     is necessary to prevent instability in ETPHOT).
#
    DLAYR2  = SOIL10(DLAYR, 1, NLAYR)
    LL2     = SOIL10(LL, 0, NLAYR)
    DUL2    = SOIL10(DUL, 0, NLAYR)
    SAT2    = SOIL10(SAT, 0, NLAYR)
    BD2     = SOIL10(BD, 0, NLAYR)
    HCAPS2  = SOIL10(HCAPS, 0, NLAYR)
    TCNDS2  = SOIL10(TCONDS, 0, NLAYR)

#
# C     Calculate parameters for E depth (DULE and LLE in mm). CEC starts
# C     at 0.1 * the 1st stage evaporation amount.
#
    DULE = 0.0
    LLE = 0.0
    for I in range(1, NELAYR + 1):
        DULE = DULE + DUL2[I - 1] * DLAYR2[I - 1] * 10.0
        LLE = LLE + LL2[I - 1] * DLAYR2[I - 1] * 10.0
    # C     CEC = 0.45 * U / (DULE-LLE) * 100.0
    CEC = 0.0

#
# C     Calculate soil thermal properties.  Arrays YSHCAP and YSCOND store
# C     results for daily table lookup as a function of moisture content.
#
    N1 = 5.0
    GA = 1.0 / (2.0 + N1)
    GC = 1.0 - 2.0 * GA

#
    for I in range(1, NL+1):
        for J in range(1,4):
            XSW[I, J] = 0.0
            YSHCAP[I, J] = 0.0
            YSCOND[I, J] = 0.0

    for I in range(1, NLAYR+1):
        XSOIL = BD2[I] / 2.65
        XPORE = 1.0 - XSOIL

#
# C       Calculate soil heat capacity and thermal conductivity parameters
# C       using De Vries (1963) for dry, field capacity and saturated
# C       moisture contents (XSW).
#
        for J in range(1, 4):

        # C         Dry soil.

            if J == 1:
                XAIR = XPORE
                XWATER = 0.0
                KSOIL = (2.0 / (1.0 + (TCNDS2[I] / TCAIR - 1.0) * GA)
                       + 1.0 / (1.0 + (TCNDS2[I] / TCAIR - 1.0) * GC)) / 3.0
                CORRN = 1.25

# C             Field capacity soil.
#
            elif J == 2:
                XAIR = XPORE - DUL2[I]
                XWATER = DUL2[I]
                KSOIL = (2.0 / (1.0 + (TCNDS2[I] / TCWATR - 1.0) * GA)
                         + 1.0 / (1.0 + (TCNDS2[I] / TCWATR - 1.0) * GC)) / 3.0
                CORRN = 1.0

#
# C         Saturated soil.
#
            else:
                XAIR = 0.0
                XWATER = XPORE
                KSOIL = (2.0 / (1.0 + (TCNDS2[I] / TCWATR - 1.0) * GA)
                       + 1.0 / (1.0 + (TCNDS2[I] / TCWATR - 1.0) * GC)) / 3.0
                CORRN = 1.0
            XSW[I, J] = XWATER
            YSHCAP[I, J] = HCAPS2[I] * XSOIL + VHCWAT * XWATER
            YSCOND[I, J] = ((KSOIL * XSOIL * TCNDS2[I] + XWATER * TCWATR + XAIR * TCAIR)/ (KSOIL * XSOIL + XWATER + XAIR)) * CORRN

# C     Compute leaf angles in three classes (0-30, 30-60, 60-90) using
# C     the ellipsoidal distribution.  Approx. eqn. for CDF at 30 and 60 deg.
#
    LFANGD[1] = 0.936 * (1.0 - 0.630 * math.exp(-0.719 * LFANGB)) ** 4.950
    LFANGD[2] = 0.974 * (1.0 - 1.109 * math.exp(-1.037 * LFANGB)) ** 1.408
    LFANGD[3] = 1.0 - LFANGD[2]
    LFANGD[2] = LFANGD[2] - LFANGD[1]

    return (AZIR, BETN, CEC, DLAYR2, DUL2, DULE, LFANGD,LL2, LLE, LWIDTH, NELAYR, PALBW, PHTHRS10,RCUTIC, ROWSPC, SAT2, SCVIR, SCVP, SWEF, XSW,YSCOND, YSHCAP)
#       END SUBROUTINE ETINP

# C=======================================================================
# C  PGINP, Subroutine, N.B. Pickering, 12/12/90
# C  Reads and initializes fixed PG parameters.
# C-----------------------------------------------------------------------
# C  REVISION HISTORY
# C  12/05/1990 NBP Written.
# C  05/10/1994 NBP Added PGFAC3,SLWSLO,LNREF,SLWREF
# C  09/21/1999 NBP Moved weather parameters to ETPSIM.
# C  09/25/1999 NBP Separate ET and PHOT routines.
# C  01/20/2000 NBP Put PGINF in PGINP.
# C  07/03/2000 GH  Included  PHTHRS10
# C  08/12/2003 CHP Added I/O error checking
# C-----------------------------------------------------------------------
# C  Called from: ETPHOT
# C  Calls:       ERROR,FIND
# C=======================================================================
#

def INDEX(s, sub):
    pos = s.find(sub)
    return pos + 1 if pos != -1 else 0

def PGINP(model,FILEIO, LUNIO, SALBW):
    from ERROR import ERROR
    from READS import FIND, IGNORE
#      &  AZIR, BETN, FNPGL, FNPGN, LFANGD, LMXREF,         !Output
#      &  LNREF, NSLOPE, PALBW, QEREF, ROWSPC,              !Output
#      &  SCVP, SLWREF, SLWSLO, TYPPGL, TYPPGN,             !Output
#      &  XLMAXT, YLMAXT, PHTHRS10,                         !Output
#      &  ccneff, cicad, cmxsf, cqesf, pgpath)              !Output
#
#       IMPLICIT NONE
#       EXTERNAL FIND, GETLUN, IGNORE, ERROR
#       SAVE
#
#       CHARACTER BLANK*1,ERRKEY*6,FILEC*12,FILECC*92,FILEIO*30,
#      &  PATHCR*80,SECTION*6,TYPPGL*3,TYPPGN*3
#       CHARACTER*80 C80
#
#       INTEGER ERRNUM,FOUND,I,LINC,LNUM,LUNCRP,LUNIO,PATHL
#       INTEGER ISECT
#
    ERRKEY = 'PGINP '
    BLANK = ' '
    XLMAXT = np.zeros(6, dtype=float)
    YLMAXT = np.zeros(6, dtype=float)
    FNPGN = np.zeros(4, dtype=float)
    LFANGD = np.zeros(3, dtype=float)
    FNPGL = np.zeros(4, dtype=float)
#       REAL AZIR,BETN,FNPGL(4),LFANGB,LFANGD(3),LMXREF,LNREF,NSLOPE,
#      &  PALBW,PLTPOP,QEREF,ROWSPC,SALBW,SCVP,SLWREF,
#      &  SLWSLO,FNPGN(4),XLMAXT(6),YLMAXT(6),PHTHRS10
#
#       character(len=2) pgpath
#       character(len=8) model
#       real ccneff, cicad, cmxsf, cqesf
#
# C     Read IBSNAT35.INP file.
#
    try:
        LUNIO = open(FILEIO, 'r')
    except OSError as e:
        ERRNUM = e.errno
        ERROR(ERRKEY, ERRNUM, FILEIO, 0)

#
    SECTION = "*FILES"
    LNUM, FOUND=FIND(LUNIO, SECTION)
    if FOUND == 0:
        ERROR(SECTION, 42, FILEIO, LNUM)

#
    #FILEC, PATHCR = READ(LUNIO, '(////15X,A,1X,A)', IOSTAT=ERRNUM)-------------------------------------TP
    for _ in range(4):
        LUNIO.readline()
        LNUM += 1

    line = LUNIO.readline();
    LNUM += 1

    try:
        FILEC = line[15:27].strip()
        PATHCR = line[28:].strip()
        ERRNUM = 0
    except Exception:
        ERRNUM = 1
        ERROR(ERRKEY, ERRNUM, FILEIO, LNUM)

    if ERRNUM != 0:
        ERROR(ERRKEY, ERRNUM, FILEIO, LNUM + 5)
    PATHL = INDEX(PATHCR, BLANK)
    if PATHL <= 1:
        FILECC = FILEC
    else:
        FILECC = PATHCR[1:PATHL] + FILEC

#
    LUNIO.seek(0)
    SECTION = "*PLANT"
    LNUM, FOUND=FIND(LUNIO, SECTION)
    if FOUND == 0:
        ERROR(SECTION, 42, FILEIO, LNUM)

    PLTPOP, ROWSPC, AZIR = READ(LUNIO, '(24X,F6.0,12X,2F6.0)', IOSTAT=ERRNUM)
    LNUM = LNUM + 1
    if ERRNUM != 0:
        ERROR(ERRKEY, ERRNUM, FILEIO, LNUM + 1)

    LUNIO.seek(0)
    SECTION = "*CULTI"
    LNUM, FOUND=FIND(LUNIO, SECTION)
    LNUM = LNUM + LINC
    if FOUND == 0:
        ERROR(SECTION, 42, FILEIO, LNUM)

#
# C-GH  READ(LUNIO,'(72X,F6.0)') LMXREF
    try:
        line = LUNIO.readline()
        PHTHRS10 = float(line[60:66])
        LMXREF = float(line[72:78])
        ERRNUM = 0
    except Exception as e:
        ERRNUM = getattr(e, 'errno', 1)
        ERROR(ERRKEY, ERRNUM, FILEIO, LNUM + 1)

#
# C     Read species file.
    GETLUN('FILEC', LUNCRP)
    try:
        LUNCRP = open(FILECC, 'r')
    except OSError as e:
        ERRNUM = e.errno
        ERROR(ERRKEY, ERRNUM, FILECC, 0)

#
    SECTION = '!*PHOT'
    LNUM, FOUND=FIND(LUNCRP, SECTION)
#
# !     Read 3rd line of photosynthesis section of species file
    ISECT,C80=IGNORE(LUNCRP,LNUM)
    ISECT,C80=IGNORE(LUNCRP,LNUM)
    ISECT,C80=IGNORE(LUNCRP,LNUM)
    try:
        FNPGN[1] = float(C80[0:6])
        FNPGN[2] = float(C80[6:12])
        FNPGN[3] = float(C80[12:18])
        FNPGN[4] = float(C80[18:24])
        TYPPGN   = C80[27:].strip()
        ERRNUM = 0
    except Exception as e:
        ERRNUM = getattr(e, 'errno', 1)
        ERROR(ERRKEY, ERRNUM, FILECC, LNUM)
#
    ISECT, C80=IGNORE(LUNCRP, LNUM)
    ISECT, C80=IGNORE(LUNCRP, LNUM)  # 5th line
    try:
        XLMAXT[1] = float(C80[0:6])
        XLMAXT[2] = float(C80[6:12])
        XLMAXT[3] = float(C80[12:18])
        XLMAXT[4] = float(C80[18:24])
        XLMAXT[5] = float(C80[24:30])
        XLMAXT[6] = float(C80[30:36])
        ERRNUM = 0
    except Exception as e:
        ERRNUM = 1
        ERROR(ERRKEY, ERRNUM, FILECC, LNUM)
#
    ISECT, C80=IGNORE(LUNCRP, LNUM)  # 6th line

    try:
        YLMAXT[1] = float(C80[0:6])
        YLMAXT[2] = float(C80[6:12])
        YLMAXT[3] = float(C80[12:18])
        YLMAXT[4] = float(C80[18:24])
        YLMAXT[5] = float(C80[24:30])
        YLMAXT[6] = float(C80[30:36])
        ERRNUM = 0
    except Exception:
        ERRNUM = 1
        ERROR(ERRKEY, ERRNUM, FILECC, LNUM)

#
    ISECT, C80=IGNORE(LUNCRP, LNUM)  # 7th line
    try:
        FNPGL[1] = float(C80[0:6])
        FNPGL[2] = float(C80[6:12])
        FNPGL[3] = float(C80[12:18])
        FNPGL[4] = float(C80[18:24])
        TYPPGL = C80[27:].strip()
        ERRNUM = 0
    except Exception:
        ERRNUM = 1
        ERROR(ERRKEY, ERRNUM, FILECC, LNUM + 7)

    ISECT, C80=IGNORE(LUNCRP, LNUM)  # 8th line
    try:
        QEREF  = float(C80[0:6])
        SCVP   = float(C80[6:12])
        LFANGB = float(C80[18:24])
        ERRNUM = 0
    except Exception:
        ERRNUM = 1
        ERROR(ERRKEY, ERRNUM, FILECC, LNUM)

    ISECT, C80=IGNORE(LUNCRP, LNUM)  # 9th line
    try:
        SLWREF = float(C80[0:6])
        SLWSLO = float(C80[6:12])
        NSLOPE = float(C80[12:18])
        LNREF  = float(C80[18:24])
        ERRNUM = 0
    except Exception:
        ERRNUM = 1
        ERROR(ERRKEY, ERRNUM, FILECC, LNUM)

#
    if model[0:5] == 'PRFRM':
        ISECT, C80=IGNORE(LUNCRP, LNUM)
        ISECT, C80=IGNORE(LUNCRP, LNUM)
        ISECT, C80=IGNORE(LUNCRP, LNUM)  # 12th line
        try:
            CICAD  = float(C80[0:6])
            CCNEFF = float(C80[6:12])
            CMXSF  = float(C80[12:18])
            CQESF  = float(C80[18:24])
            PGPATH = C80[26:].strip()
            ERRNUM = 0
        except Exception:
            ERRNUM = 1
            ERROR(ERRKEY, ERRNUM, FILECC, LNUM)
    else:
        PGPATH = '  '
        CICAD = -99
        CCNEFF = -99
        CMXSF = -99
        CQESF = -99

    LUNCRP.close()

#
# C     Initialize some parameters.
#
    PALBW = 0.6 * SALBW
    # PGFAC3 = SLPF

    ROWSPC = ROWSPC / 100.0
    if ROWSPC > 0.0 and PLTPOP > 0.0:
        BETN = 1.0 / (ROWSPC * PLTPOP)
    else:
        BETN = 0.0

#
# C     Compute leaf angles in three classes (0-30, 30-60, 60-90) using
# C     the ellipsoidal distribution.  Approx. eqn. for CDF at 30 and 60 deg.
#
    LFANGD[1] = 0.936 * (1.0 - 0.630 * math.exp(-0.719 * LFANGB)) ** 4.950
    LFANGD[2] = 0.974 * (1.0 - 1.109 * math.exp(-1.037 * LFANGB)) ** 1.408
    LFANGD[3] = 1.0 - LFANGD[2]
    LFANGD[2] = LFANGD[2] - LFANGD[1]

    return (AZIR, BETN, FNPGL, FNPGN, LFANGD, LMXREF,LNREF, NSLOPE, PALBW, QEREF, ROWSPC,SCVP, SLWREF, SLWSLO, TYPPGL, TYPPGN,XLMAXT, YLMAXT, PHTHRS10,ccneff, cicad, cmxsf, cqesf, pgpath)
#       END SUBROUTINE PGINP



#Test Drive RADABS

# AZIR=90.00000
# AZZON=-37.99238
# BETA=-65.37078
# BETN=0.1921968
# CANHT=1.3000000E-02
# CANWH=1.9500000E-02
# DAYTIM=False
# FRDIFP=1.000000
# FRDIFR=1.000000
# H=1
# LFANGD=0.4197626
# MEEVP='N'
# MEPHO='L'
# PALB=0.0000000E+00
# PARHR=0.0000000E+00
# RADHR=0.0000000E+00
# ROWSPC=1.210000
# SALB=0.0000000E+00
# SCVIR=0.0000000E+00
# SCVP=0.0000000E+00
# XLAI=6.5209083E-02
# FRACSH, FRSHV, KDIRBL, KDRBLV, LAISH, LAISHV, LAISL, LAISLV, PARSH, PARSUN, PCABSP, PCABSR, PCINTP, PCINTR, RABS = RADABS(AZIR, AZZON, BETA, BETN, CANHT, CANWH, DAYTIM, FRDIFP, FRDIFR, H, LFANGD, MEEVP, MEPHO, PALB, PARHR, RADHR, ROWSPC, SALB, SCVIR, SCVP, XLAI)
# print(FRACSH)


# Test Drive SHADOW----CHECK

# AZIR=90.00000
# AZZON=-37.99238
# BETA=90.00000
# BETN=0.1921968
# CANHT=1.3000000E-02
# CANWH=1.9500000E-02
# ROWSPC=1.210000
# print(SHADOW(AZIR, AZZON, BETA, BETN, CANHT, CANWH, ROWSPC))


# Test Drive LFEXTN----CHECK

# BETA=9.507017
# FRACSH=5.2709733E-03
# LFANGD = [0.4197626, 0.3686843, 0.2115532]
# XLAI=6.5209083E-02
# print(LFEXTN(BETA, FRACSH, LFANGD, XLAI))


# Test Drive CANABS --------------CHECK

# ALBEDO=0.0000000E+00
# BETA=9.507017
# BETN=0.1921968
# CANHT=1.3000000E-02
# CANWH=1.9500000E-02
# FRACSH=5.2709733E-03
# FRDIF=0.8642097
# KDIFBL=0.9552019
# KDIRBL=2.460819
# LAISH=6.3067123E-02
# LAISL=2.1419590E-03
# RADHR=210.2170
# RNG=0.8515824
# ROWSPC=1.210000
# SCVR=0.2000000
# print(CANABS(ALBEDO, BETA, BETN, CANHT, CANWH, FRACSH,FRDIF, KDIFBL, KDIRBL, LAISH, LAISL, RADHR,RNG, ROWSPC, SCVR))


#Test Drive ETIND



# Test Drive PGIND
#
#
# NLAYR=9
# PALBW=0.0000000E+00
# DUL2 = [0.0000000e+00]*20
# SW=[9.0999998E-02,9.0999998E-02,7.5999998E-02,7.1000002E-02,7.1000002E-02,8.1000000E-02,7.8000002E-02,8.3999999E-02,8.3999999E-02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+020,0.0000000E+02]
# print(PGIND(NLAYR, PALBW, DUL2, SW))


# Test drive SOIL10

# ARRAY=[9.0999998E-02,9.0999998E-02,7.5999998E-02,7.1000002E-02,7.1000002E-02,8.1000000E-02,7.8000002E-02,8.3999999E-02,8.3999999E-02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+02,0.0000000E+020,0.0000000E+02]
# ICODE=0
# NLAYR=9
# print(SOIL10(ARRAY, ICODE, NLAYR))


#Test drive SOIL05

# ARRAY2=
# ICODE=
# NLAYR=