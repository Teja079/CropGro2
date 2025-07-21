#-----------------------------------------------------------------------
#  Computes fresh pod weight
#-----------------------------------------------------------------------
#  Called from:  PODS
#=======================================================================
import numpy as np
from  ModuleDefs import *

def FRESHWT(DYNAMIC, ISWFWT,
            YRPLT, XMAGE, NR2TIM, PHTIM,
            WTSD, SDNO, WTSHE, SHELN,
            HPODWT, HSDWT, HSHELWT):
    from WARNING import WARNING
    from ERROR import ERROR
    from ModuleDefs import GET
    from DATES import YR_DOY

    ISWFWT = ''
    CROP = ''
    ERRKEY = 'FRSHWT'
    FWFile = ''
    CROPD = ''
    MSG = [''] * 4

    DAP = DAS = DOY = DYNAMIC = ERRNUM = I = 1
    NOUTPF = NPP = NR2TIM = TIMDIF = 1
    YEAR = YRDOY = YRPLT = HARVF = 1
    CHNUM = CPODN = CMFNM = 1

    AvgDMC = AvgDPW = AvgFPW = PodDiam = PodLen = 0.0
    PAGE = XMAGE = PodAge = 1
    TDPW = TFPW = 1
    CLASS = [0.0] * 7

    DMC = np.zeros(NCOHORTS)
    DryPodWt = np.zeros(NCOHORTS)
    FreshPodWt = np.zeros(NCOHORTS)
    XPAGE = np.zeros(NCOHORTS)

    SDNO = np.zeros(NCOHORTS)
    SHELN = np.zeros(NCOHORTS)
    WTSD = np.zeros(NCOHORTS)
    WTSHE = np.zeros(NCOHORTS)

    TWTSH = HRPN = 0.0
    HRSN = HRDSD = HRDSH = 0.0
    TOSDN = TOWSD = TOSHN = TOWSH = TOPOW = TOFPW = 0.0
    MTFPW = MTDPW = MTDSD = MTDSH = MFNUM = 0.0
    RTFPW = HPODWT = HSDWT = HSHELWT = HFPOW = 0.0
    HRVD = HRVF = 0.0
    CHPDT = CHFPW = 0.0

    ISH_wt = 0.0

    CONTROL = ControlType()
    ISWITCH = SwitchType()

    YRDOY = CONTROL.YRDOY
#***********************************************************************
#***********************************************************************
#     Run initialization - run once per simulation
#***********************************************************************
    if DYNAMIC == RUNINIT:
        # -----------------------------------------------------------------------
        if 'Y' not in ISWFWT or 'N' in ISWITCH.IDETL or '0' in ISWITCH.IDETL:
            FWFile = 'FreshWt.OUT '
            # CALL GETLUN('FWOUT',  NOUTPF)


#***********************************************************************
#***********************************************************************
#     Seasonal initialization - run once per season
#***********************************************************************
    elif DYNAMIC == SEASINIT:
    #-----------------------------------------------------------------------
    #        CALL GET(ISWITCH)

    #     Switch for fresh weight calculations
        if 'Y' not in ISWFWT or 'N' in ISWITCH.IDETL or '0' in ISWITCH.IDETL:
            return

        CROP = CONTROL.CROP

    #     Currently only works for tomato, green bean, bell pepper, strawberry,
    #         and cucumber. Add other crops later.
    #     Send a message if not available crop
        if all(code not in CROP for code in ['CU', 'GB', 'PR', 'SR', 'TM']):
            GET_CROPD(CROP, CROPD)
            MSG[0] = "Fresh weight calculations not currently available for "
            MSG[1] = f"{CROP} {CROPD}"
            #INFO(2, ERRKEY, MSG)

        if any(code in CROP for code in ['CU', 'GB', 'PR', 'SR', 'TM']) and XMAGE < 1.0:
            GET_CROPD(CROP, CROPD)
            MSG[0] = 'Please check the value of XMAGE in Ecotype file.'
            MSG[1] = 'XMAGE cannot be lower than 1.0.'
            MSG[2] = f"XMAGE = {XMAGE:8.2f}"
            MSG[3] = f"{CROP} {CROPD}"
            WARNING(4, ERRKEY, MSG)
            ERROR(ERRKEY, 1, '', 0)

    #::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        if os.path.exists(FWFile):
            NOUTPF = open(FWFile, 'a')
        else:
            NOUTPF = open(FWFile, 'w')
            NOUTPF.write("*Fresh Weight Output File\n")

    #         2023-01-13 FO - Added Header variables description
    #         Note: Please follow the standard code. Comment of the section
    #         in the first line. Next line '!'variable name separed by two
    #         spaces the description and units in parenthesis.
        NOUTPF.write("\n")
        NOUTPF.write("!----------------------------\n")
        NOUTPF.write("! Variable Descriptions (unit)\n")
        NOUTPF.write("!----------------------------\n")
        NOUTPF.write("!YEAR  Year of current date of simulation\n")
        NOUTPF.write("!DOY  Day of year (d)\n")
        NOUTPF.write("!DAS  Days after start of simulation (d)\n")
        NOUTPF.write("!PDMCD  Dry matter con. of harvested product (fraction)\n")
        NOUTPF.write("!AFPWD  Average fresh fruit (pod, ear) weight (g/fruit)\n")
        NOUTPF.write("!ADPWD  Average dry fruit (pod, ear) weight (g/fruit)\n")
        NOUTPF.write("!PAGED  Age of oldest pod or ear (days)\n")

        if CROP == 'GB':
            NOUTPF.write("!FCULD  Fresh weight for cull fruit/pods (kg/ha)\n")
            NOUTPF.write("!FSZ1D  Fresh weight for size class 1 (kg/ha)\n")
            NOUTPF.write("!FSZ2D  Fresh weight for size class 2 (kg/ha)\n")
            NOUTPF.write("!FSZ3D  Fresh weight for size class 3 (kg/ha)\n")
            NOUTPF.write("!FSZ4D  Fresh weight for size class 4 (kg/ha)\n")
            NOUTPF.write("!FSZ5D  Fresh weight for size class 5 (kg/ha)\n")
            NOUTPF.write("!FSZ6D  Fresh weight for size class 6 (kg/ha)\n")

        NOUTPF.write("!XMAGE  Required pod age for Multi-Harvest (days)\n")
        NOUTPF.write("!TOSDN  Total Number of seeds (#/ha)\n")
        NOUTPF.write("!TOWSD  Total Seed mass (kg/ha)\n")
        NOUTPF.write("!TOSHN  Total Number of shells (#/ha)\n")
        NOUTPF.write("!TOWSH  Total Shell mass (kg/ha)\n")
        NOUTPF.write("!TOPOW  Total pod weight (kg/ha)\n")
        NOUTPF.write("!TOFPW  Total pod (ear) fresh weight (kg/ha)\n")
        NOUTPF.write("!MTFPW  Fresh weight of mature fruits (kg/ha)\n")
        NOUTPF.write("!MTDPW  Dry weight of mature fruits (sd and sh)(kg/ha)\n")
        NOUTPF.write("!MTDSD  Seed mass of mature fruits (kg/ha)\n")
        NOUTPF.write("!MTDSH  Shell mass of mature fruits (kg/ha)\n")
        NOUTPF.write("!HSHELWT  Harvested shell weight (kg/ha)\n")
        NOUTPF.write("!HSDWT  Harvested seed weight (kg/ha)\n")
        NOUTPF.write("!HPODWT  Harvested pod weight (kg/ha)\n")
        NOUTPF.write("!HFPOW  Harvested fresh pod weight (kg/ha)\n")
        NOUTPF.write("!CPODN  Cum. pod weight (kg/ha)\n")
        NOUTPF.write("!CMFNM  Cum. mature fruit number (#)\n")
        NOUTPF.write("!CHPDT  Cum. har. pod weight of mat. fruits (kg/ha)\n")
        NOUTPF.write("!CHFPW  Cum. har. fresh weight of mat. fruits (kg/ha)\n")
        NOUTPF.write("!CHNUM  Cum. harvest number (#)\n")


#            CALL HEADER(SEASINIT, NOUTPF, CONTROL.RUN)


#     Change header to PWAD1 (was PWAD) because GBuild requires
#     unique headers (PlantGro also lists PWAD).  Should have same
#     value, but slightly off. Why?

        header_230 = (
            "@YEAR DOY   DAS   DAP"
            "    PDMCD    AFPWD"
            "    ADPWD    PAGED"
            "    XMAGE    CHNUM"
            "    TOSHN    TOWSH    MTDSH    HSHEL"
            "    TOPOW    HPODW    CHPDT    CPODN"
            "    TOFPW    MTFPW    MTDPW    HFPOW    CHFPW    CMFNM"
            "    TOSDN    TOWSD    MTDSD    HSDWT\n"
        )

        header_231 = (
            "@YEAR DOY   DAS   DAP"
            "    PDMCD    AFPWD"
            "    ADPWD    PAGED"
            " FCULD FSZ1D FSZ2D FSZ3D FSZ4D FSZ5D FSZ6D"
            "    XMAGE    CHNUM"
            "    TOSHN    TOWSH    MTDSH    HSHEL"
            "    TOPOW    HPODW    CHPDT    CPODN"
            "    TOFPW    MTFPW    MTDPW    HFPOW    CHFPW    CMFNM"
            "    TOSDN    TOWSD    MTDSD    HSDWT\n"
        )

        match CROP:
            case 'GB':
                NOUTPF.write(header_231)
            case 'CU' | 'PR' | 'SR' | 'TM':
                NOUTPF.write(header_230)
            case _:
                NOUTPF.write(header_230)

        AvgDMC  = 0.0
        AvgDPW  = 0.0
        AvgFPW  = 0.0
        PodAge  = 0.0
        TDPW    = 0.0
        TFPW    = 0.0

        TWTSH   = 0.0
        HSDWT   = 0.0
        HSHELWT = 0.0
        RTFPW   = 0.0
        HPODWT  = 0.0
        HFPOW   = 0.0

        CHPDT   = 0.0
        CHFPW   = 0.0
        CMFNM   = 0
        CHNUM   = 0
        CPODN   = 0

        HRVD    = 0.0
        HRVF    = 0.0
        HRSN    = 0.0
        HRPN    = 0.0
        HRDSD   = 0.0
        HRDSH   = 0.0

        PUT_Integer('MHARVEST','ISH_date',-99)
        PUT_Integer('MHARVEST','ISH_wt',  -99.)

#***********************************************************************
#***********************************************************************
#     DAILY RATE/INTEGRATION
#***********************************************************************
    elif DYNAMIC == INTEGR:
#-----------------------------------------------------------------------
        if 'Y' not in ISWFWT or any(x in ISWITCH.IDETL for x in ['N', '0']): return

        # Total values
        TOSDN   = 0.0
        TOWSD   = 0.0
        TOSHN   = 0.0
        TOWSH   = 0.0
        TOPOW   = 0.0
        TOFPW   = 0.0

        # Mature in the basket
        MTFPW   = 0.0
        MTDPW   = 0.0
        MTDSD   = 0.0
        MTDSH   = 0.0
        MFNUM   = 0.0

        # Ready to harvest
        HSDWT   = 0.0
        HSHELWT = 0.0
        HPODWT  = 0.0
        HFPOW   = 0.0
        HARVF   = 0
        GET('MHARVEST','HARVF',HARVF)


        for i in range(7):
            CLASS[i] = 0.0
        # -----------------------------------------------------------------------
        for NPP in range(NR2TIM + 1):
            PAGE = PHTIM[NR2TIM] - PHTIM[NPP]
            XPAGE[NPP] = PAGE

            match CROP:
                case 'CU':  # Cucumber
                    DMC[NPP] = (5. + 7.2 * np.exp(-7.5 * PAGE / 40.)) / 100.
                case 'GB':  # Snap bean
                    DMC[NPP] = 0.023 + 0.0277 * np.exp(0.116 * PAGE)
                case 'PR':  # Bell pepper
                    DMC[NPP] = (5. + 7.2 * np.exp(-7.5 * PAGE / 40.)) / 100.
                case 'SR':  # Strawberry
                    DMC[NPP] = 0.16
                case 'TM':  # Tomato
                    DMC[NPP] = (5. + 7.2 * np.exp(-7.5 * PAGE / 40.)) / 100.
                case _:  # Default
                    DMC[NPP] = (5. + 7.2 * np.exp(-7.5 * PAGE / 40.)) / 100.

            if SHELN[NPP] > 0.0:
                FreshPodWt[NPP] = (WTSD[NPP] + WTSHE[NPP]) / DMC[NPP] / SHELN[NPP]
                DryPodWt[NPP] = (WTSD[NPP] + WTSHE[NPP]) / SHELN[NPP]
            else:
                FreshPodWt[NPP] = 0.0

            if CROP == 'GB':
                PodDiam = 8.991 * (1.0 - np.exp(-0.438 * (FreshPodWt[NPP] + 0.5)))
                PodLen = 14.24 * (1.0 - np.exp(-0.634 * (FreshPodWt[NPP] + 0.46)))

                if PodDiam < 4.7625:
                    CLASS[6] += (WTSD[NPP] + WTSHE[NPP]) / DMC[NPP]  # CLASS(7) -> index 6
                elif PodDiam < 5.7547:
                    CLASS[0] += (WTSD[NPP] + WTSHE[NPP]) / DMC[NPP]
                elif PodDiam < 7.3422:
                    CLASS[1] += (WTSD[NPP] + WTSHE[NPP]) / DMC[NPP]
                elif PodDiam < 8.3344:
                    CLASS[2] += (WTSD[NPP] + WTSHE[NPP]) / DMC[NPP]
                elif PodDiam < 9.5250:
                    CLASS[3] += (WTSD[NPP] + WTSHE[NPP]) / DMC[NPP]
                elif PodDiam < 10.7156:
                    CLASS[4] += (WTSD[NPP] + WTSHE[NPP]) / DMC[NPP]
                else:
                    CLASS[5] += (WTSD[NPP] + WTSHE[NPP]) / DMC[NPP]


            #Total Seed number
            TOSDN = TOSDN + SDNO(NPP)

            #Total weight seed
            TOWSD = TOWSD + WTSD(NPP)

            #Total shell number
            TOSHN = TOSHN + SHELN(NPP)

            #Total weight shell
            TOWSH = TOWSH + WTSHE(NPP)

            #Total  Pod weight
            TOPOW = TOPOW + WTSD(NPP) + WTSHE(NPP)

            #Total Fresh Pod weight
            TOFPW = TOFPW + (WTSD(NPP) + WTSHE(NPP)) / DMC(NPP)


            # Accumulating in the basket for harvesting (MultiHarvest)
            if PAGE >= XMAGE:
                # Fresh weight of mature fruits
                MTFPW = MTFPW + (WTSD[NPP] + WTSHE[NPP]) / DMC[NPP]
                # Dry weight of mature fruits (seed and shell)
                MTDPW = MTDPW + WTSD[NPP] + WTSHE[NPP]
                # Seed mass of mature fruits - wtsd
                # = seed mass for cohort
                MTDSD = MTDSD + WTSD[NPP]
                # Shell mass of mature fruits - wtshe
                # = shell mass for cohort
                MTDSH = MTDSH + WTSHE[NPP]
                # Number of mature fruits
                MFNUM = MFNUM + SHELN[NPP]

        # Apply Harvest
        if HARVF == 1 and PAGE >= XMAGE:
            HSHELWT = MTDSH
            HSDWT = MTDSD
            HPODWT = MTDPW
            HFPOW = MTFPW
            SHELN[NPP] = 0.0
            WTSHE[NPP] = 0.0
            WTSD[NPP] = 0.0
            SDNO[NPP] = 0.0
            ISH_wt = HSHELWT + HSDWT + HPODWT + HFPOW
            PUT('MHARVEST', 'ISH_date', YRDOY)
            PUT('MHARVEST', 'ISH_wt', ISH_wt)

        # End loop over NPP

        # Prepare model outputs
        PodAge = XPAGE[0]

        # Avg. total shell number and pod weight
        if TOSHN > 0.0:
            AvgFPW = TOFPW / TOSHN
            AvgDPW = TOPOW / TOSHN
            CPODN = CPODN + round(TOPOW)
            CMFNM = CMFNM + round(MFNUM)
        else:
            AvgFPW = 0.0
            AvgDPW = 0.0

        # Avg. Dry matter content
        if TOFPW > 0.0:
            AvgDMC = TOPOW / TOFPW
        else:
            AvgDMC = 0.0

        # Cumulative har. amounts
        if HARVF == 1:
            CHPDT = CHPDT + HPODWT
            CHFPW = CHFPW + MTFPW
            CHNUM = CHNUM + 1

 #***********************************************************************
#***********************************************************************
#     DAILY OUTPUT
#***********************************************************************
    elif DYNAMIC == OUTPUT:
    # -----------------------------------------------------------------------
        if ('Y' not in ISWFWT or any(c in ISWITCH.IDETL for c in 'N0')):
            return

        YRDOY = CONTROL.YRDOY
        if YRDOY < YRPLT or YRPLT < 0:
            return

        DAS = CONTROL.DAS

        # Daily output every FROP days
        if DAS % CONTROL.FROP == 0:
            YEAR, DOY = YR_DOY(YRDOY)
            DAP = max(0, TIMDIF(YRPLT, YRDOY))
            if DAP > DAS:
                DAP = 0

            # 1000   FORMAT(1X,I4,1X,I3.3,2(1X,I5),
            # &    F9.1,F9.1,F9.1,F9.1,
            # &    F9.1, I9,
            # &    4(F9.1),
            # &    3(F9.1),I9,
            # &    I9,4(F9.1),I9,
            # &    4(F9.1))
            # 2000   FORMAT(1X,I4,1X,I3.3,2(1X,I5),
            # &    F9.1,F9.1,F9.1,F9.1,
            # &    7(1X,I5),
            # &    F9.1, I9,
            # &    4(F9.1),
            # &    3(F9.1),I9,
            # &    I9,4(F9.1),I9,
            # &    4(F9.1))
            fmt1000 = (
                "{:4d} {:03d} {:5d} {:5d} "
                "{:9.1f}{:9.1f}{:9.1f}{:9.1f}"
                "{:9.1f} {:9d}"
                "{:9.1f}{:9.1f}{:9.1f}{:9.1f}"
                "{:9.1f}{:9.1f}{:9.1f} {:9d}"
                "{:9d}{:9.1f}{:9.1f}{:9.1f} {:9d}"
                "{:9.1f}{:9.1f}{:9.1f}{:9.1f}"
            )

            fmt2000 = (
                "{:4d} {:03d} {:5d} {:5d} "
                "{:9.1f}{:9.1f}{:9.1f}{:9.1f}"
                "{:5d} {:5d} {:5d} {:5d} {:5d} {:5d} {:5d} "
                "{:9.1f} {:9d} "
                "{:9.1f}{:9.1f}{:9.1f}{:9.1f} "
                "{:9.1f}{:9.1f}{:9.1f} {:9d} "
                "{:9d}{:9.1f}{:9.1f}{:9.1f} {:9d} "
                "{:9.1f}{:9.1f}{:9.1f}{:9.1f}"
            )
            match CROP:
                case 'GB':  # Snap bean
                    line = fmt2000.format(
                        YEAR, DOY, DAS, DAP,
                        AvgDMC, AvgFPW, AvgDPW, PodAge,
                        round(CLASS[6]), round(CLASS[0]), round(CLASS[1]), round(CLASS[2]), round(CLASS[3]),
                        round(CLASS[4]), round(CLASS[5]),
                        XMAGE, CHNUM,
                        TOSHN, TOWSH * 10, MTDSH * 10, HSHELWT * 10,
                               TOPOW * 10, HPODWT * 10, CHPDT * 10, CPODN * 10,
                        round(TOFPW * 10), MTFPW * 10, MTDPW * 10,
                               HFPOW * 10, CHFPW * 10, CMFNM,
                        TOSDN, TOWSD * 10, MTDSD * 10, HSDWT * 10
                    )
                case 'CU' | 'PR' | 'SR' | 'TM' | _:  # All other crops use fmt1000
                    line = fmt1000.format(
                        YEAR, DOY, DAS, DAP,
                        AvgDMC, AvgFPW, AvgDPW, PodAge,
                        XMAGE, CHNUM,
                        TOSHN, TOWSH * 10, MTDSH * 10, HSHELWT * 10,
                               TOPOW * 10, HPODWT * 10, CHPDT * 10, CPODN * 10,
                        round(TOFPW * 10), MTFPW * 10, MTDPW * 10,
                               HFPOW * 10, CHFPW * 10, CMFNM,
                        TOSDN, TOWSD * 10, MTDSD * 10, HSDWT * 10
                    )

            NOUTPF.write(line + '\n')


#***********************************************************************
#***********************************************************************
#     SEASONAL SUMMARY
#***********************************************************************
    elif DYNAMIC == SEASEND:
#-----------------------------------------------------------------------
        NOUTPF.close()
#***********************************************************************
#***********************************************************************
#     END OF DYNAMIC IF CONSTRUCT
#***********************************************************************
#***********************************************************************
    return
#=======================================================================