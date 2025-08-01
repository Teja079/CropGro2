#=======================================================================
#  OPGROW
#-----------------------------------------------------------------------
#  Generates output file for daily growth variables
#-----------------------------------------------------------------------
#  Called by: PLANT
#  Calls:     None
#=======================================================================
def OPGROW(CONTROL, ISWITCH, SOILPROP,
    CADLF, CADST, CANHT, CANWH, CMINEA, DWNOD, GROWTH,
    GRWRES, KSTRES, MAINR, MDATE, NFIXN, NLAYR, NSTRES,
    PCLSD, PCCSD, PCNL, PCNRT, PCNSD, PCNSH, PCNST, PG,
    PODNO, PODWT, PODWTD, PSTRES1, PSTRES2, RHOL, RHOS,
    RLV, RSTAGE, RTDEP, RTWT, SATFAC, SDWT, SEEDNO,
    SENESCE, SLA, STMWT, SWFAC, TGRO, TGROAV, TOPWT,
    TOTWT, TURFAC, VSTAGE, WTLF, WTNCAN, WTNLF, WTNST,
    WTNSD, WTNUP, WTNFX, XLAI, YRPLT, LINTW, LINTP):

    import os
    from ModuleDefs import RunConstants as RC, NL, TS
    import numpy as np
    from DATES import YR_DOY
    from UTILS import NINT

    IDETG, ISWPHO, ISWPOT = ' '*3
    CROP = ' '
    ERRKEY = 'OPGROW'
    OUTG, OUTPC, OUTPN = '            '*3

    COUNT, DAP, DAS, DOY, DYNAMIC, ERRNUM, FROP, I, L = [0]*9
    N_LYR, NOUTDG, NOUTPC, NOUTPN, RUN = [0]*5
    TIMDIF, YEAR, YRDOY, VWAD = [0]*4

    HI, HIP = [0.0]*2
    SDSIZE, SHELLW, SHELPC = [0.0]*3

    PCCSDP, PCLSDP, PCNLP, PCNRTP, PCNSDP = [0.0]*5
    PCNSTP, RHOLP, RHOSP, SLAP = [0.0]*4

    # RLV = np.zeros(NL)
    # TGRO = np.zeros(NL)

    WTNVEG,PCNVEG     = [0.0]*2

    CUMSENSURF,  CUMSENSOIL  = [0.0]*2
    CUMSENSURFN, CUMSENSOILN  = [0.0]*2

    SWF_AV, TUR_AV, NST_AV, EXW_AV, PS1_AV, PS2_AV, KST_AV = [0.0]*7

    FEXIST, FIRST = (False,False)

    SENSURFT, SENSOILT = [0.0]*2

    CROP    = CONTROL.CROP
    IDETG   = ISWITCH.IDETG
    if CROP == 'FA' or IDETG == 'N': return

    DAS     = CONTROL.DAS
    DYNAMIC = CONTROL.DYNAMIC
    FROP    = CONTROL.FROP
    RUN     = CONTROL.RUN
    YRDOY   = CONTROL.YRDOY

    ISWPHO = ISWITCH.ISWPHO
    ISWPOT = ISWITCH.ISWPOT

    FMOPT  = ISWITCH.FMOPT

#***********************************************************************
#     Run initialization - run once per simulation
#***********************************************************************
    if DYNAMIC == RC.RUNINIT:
#-----------------------------------------------------------------------
        if FMOPT == 'A' or FMOPT == ' ':
            OUTG  = 'PlantGro.OUT'
            GETLUN('OUTG',  NOUTDG)

            OUTPN  = 'PlantN.OUT  '
            GETLUN('OUTPN', NOUTPN)

            OUTPC  = 'PlantC.OUT  '
            GETLUN('OUTPC', NOUTPC)

    if DYNAMIC == RC.SEASINIT:
        # -----------------------------------------------------------------------
        if FMOPT == 'A' or FMOPT == ' ':
            # Initialize daily growth output file
            FEXIST = os.path.exists(OUTG)  # Check if file exists

            if FEXIST:
                with open(OUTG, 'a') as NOUTDG:  # Open file in append mode
                    FIRST = False
            else:
                with open(OUTG, 'w') as NOUTDG:  # Open file in write mode
                    NOUTDG.write("*GROWTH ASPECTS OUTPUT FILE\n")
                    FIRST = True

            # Write headers
            HEADER(RC.SEASINIT, OUTG, RUN)

        N_LYR = min(10, max(4, SOILPROP.NLAYR))

        if FMOPT == 'A' or FMOPT == ' ':  # VSH
            with open(OUTG, 'a') as NOUTDG:
                NOUTDG.write(f"!{' ' * 244}Root Dens. (cm/cm3) by soil depth (cm):\n")
                layer_texts = [f"{text:>8}" for text in SOILPROP.LayerText[:N_LYR]]  # Right-align with width 8
                NOUTDG.write(f"!{' ' * 239}{''.join(layer_texts)}\n")

                # Write main headers
                NOUTDG.write(
                    "@YEAR DOY   DAS   DAP   L#SD   GSTD   LAID   LWAD   SWAD   GWAD"
                    "   RWAD   VWAD   CWAD   G#AD    GWGD   HIAD   PWAD"
                    "   P#AD   WSPD   WSGD   NSTD\n"
                )

                NOUTDG.write("  PST1A  PST2A")
                NOUTDG.write("   KSTD")

                # Additional headers continued
                NOUTDG.write(
                    "   EWSD    LN%D   SH%D   HIPD   PWDD   PWTD   SLAD   CHTD"
                    "   CWID   NWAD   RDPD"
                )

                # Write soil layer depth headers
                for L in range(1, N_LYR + 1):
                    if L < 10:
                        NOUTDG.write(f"    RL{L}D")
                    else:
                        NOUTDG.write(f"   RL{L}D")

                # Final header line
                NOUTDG.write("    SNW0C   SNW1C")

            #-----------------------------------------------------------------------
                #Initialize daily plant nitrogen output file
                FEXIST = os.path.exists(OUTPN)
                with open(OUTPN, 'a' if FEXIST else 'w') as NOUTPN:
                    if not FEXIST:
                        NOUTPN.write("*PLANT N OUTPUT FILE")
                        FIRST = True
                    else:
                        FIRST = False

                    HEADER(RC.SEASINIT, NOUTPN, RUN)

                    NOUTPN.write(
                        "@YEAR DOY   DAS   DAP"
                        "    CNAD    GNAD    VNAD    GN%D    VN%D     NFXC    NUPC"
                        "    LNAD    SNAD    LN%D    SN%D    SHND"
                        "   RN%D   NFXD   SNN0C   SNN1C"
                    )
            #-----------------------------------------------------------------------
                #       Initialize daily plant carbon output file
                FEXIST = os.path.exists(OUTPC)
                with open(OUTPC, 'a' if FEXIST else 'w') as NOUTPC:
                    if not FEXIST:
                        NOUTPN.write("*PLANT C OUTPUT FILE")
                        FIRST = True
                    else:
                        FIRST = False

                    HEADER(RC.SEASINIT, NOUTPC, RUN)
                    NOUTPC.write('@YEAR DOY   DAS   DAP   TWAD    PHAD'
                    '    CMAD    CGRD    GRAD    MRAD    CHAD   CL%D   CS%D'
                    '   TGNN   TGAV    GN%D    GL%D    GC%D')

        CUMSENSURF = 0.0
        CUMSENSOIL = 0.0
        CUMSENSURFN = 0.0
        CUMSENSOILN = 0.0
        SWF_AV = 0.0
        TUR_AV = 0.0
        NST_AV = 0.0
        EXW_AV = 0.0
        PS1_AV = 0.0
        PS2_AV = 0.0
        KST_AV = 0.0
    # ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
    #     DAILY OUTPUT
    # ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** ** *
    if DYNAMIC == RC.OUTPUT:
        # -----------------------------------------------------------------------
        # CHECK FOR OUTPUT FREQUENCY
        # -----------------------------------------------------------------------
        if YRDOY < YRPLT or YRPLT < 0:
            return

        # Compute average stress factors since last printout
        SWF_AV += (1.0 - SWFAC)
        TUR_AV += (1.0 - TURFAC)
        NST_AV += (1.0 - NSTRES)
        EXW_AV += SATFAC
        PS1_AV += (1.0 - PSTRES1)
        PS2_AV += (1.0 - PSTRES2)
        KST_AV += (1.0 - KSTRES)
        COUNT += 1

        # Accumulate senesced matter for surface and soil.
        SENSURFT = SENESCE.ResWt[0]
        CUMSENSURF += SENESCE.ResWt[0]
        CUMSENSURFN += SENESCE.ResE[0, 1]

        SENSOILT = 0.0
        for L in range(1, NLAYR + 1):
            SENSOILT += SENESCE.ResWt[L]
            CUMSENSOIL += SENESCE.ResWt[L]
            CUMSENSOILN += SENESCE.ResE[L, 1]

        if (DAS % FROP == 0) or (YRDOY == YRPLT) or (YRDOY == MDATE):
            DAP = max(0, TIMDIF(YRPLT, YRDOY))
            if DAP > DAS:
                DAP = 0
            YEAR, DOY = YR_DOY(YRDOY)

            # Prior to emergence, do not report %N, %C values
            RHOLP = RHOL * 100.0
            RHOSP = RHOS * 100.0
            SLAP = SLA
            PCNLP = PCNL
            PCNSTP = PCNST
            PCNRTP = PCNRT

            # Prior to grain growth, do not report % compositions of seed
            PCNSDP = PCNSD
            PCLSDP = PCLSD
            PCCSDP = PCCSD
            PCNSHP = PCNSH

            if PODWT > 0.1:
                SHELPC = SDWT * 100.0 / PODWT
            else:
                SHELPC = 0.0

            SHELPC = min(SHELPC, 99.99)
            SHELLW = PODWT - SDWT  # Not used

            if SEEDNO > 1e-4:
                SDSIZE = (SDWT / SEEDNO) * 1000
            else:
                SDSIZE = 0.0

            if TOPWT > 1e-4 and SDWT >= 1e-4:
                HI = SDWT / TOPWT
            else:
                HI = 0.0

            if TOPWT > 1e-4 and PODWT >= 1e-4:
                HIP = PODWT / TOPWT
            else:
                HIP = 0.0

            # Compute average stress factors since last printout
            if COUNT > 0:
                SWF_AV /= COUNT
                TUR_AV /= COUNT
                NST_AV /= COUNT
                EXW_AV /= COUNT
                PS1_AV /= COUNT
                PS2_AV /= COUNT
                KST_AV /= COUNT
                COUNT = 0

            VWAD = NINT(WTLF * 10 + STMWT * 10)

            if FMOPT == 'A' or FMOPT == ' ':
                with open(OUTG, 'a') as NOUTDG:  # Open file in append mode
                    NOUTDG.write(f" {YEAR:4d} {DOY:03d} {DAS:5d} {DAP:5d} {VSTAGE:6.1f} {RSTAGE:6d} {XLAI:6.3f} "
                                 f"{round(WTLF * 10):6d} {round(STMWT * 10):6d} {round(SDWT * 10):6d} "
                                 f"{round(RTWT * 10):6d} {VWAD:6d} {round(TOPWT * 10):6d} {round(SEEDNO):6d} "
                                 f"{SDSIZE:7.1f} {HI:6.3f} {round(PODWT * 10):6d} {round(PODNO):6d} {SWF_AV:6.3f} "
                                 f"{TUR_AV:6.3f} {NST_AV:6.3f}\n")

                    if ISWPHO != 'N':
                        NOUTDG.write(f" {PS1_AV:6.3f} {PS2_AV:6.3f}")

                    if ISWPOT == 'Y':
                        NOUTDG.write(f" {KST_AV:6.3f}")

                    NOUTDG.write(f" {EXW_AV:6.3f} {PCNLP:7.2f} {SHELPC:6.2f} {HIP:6.2f} {round(PODWTD * 10):6d} "
                                 f"{round((PODWTD + PODWT) * 10):6d} {SLAP:6.1f} {CANHT:6.2f} {CANWH:6.2f} "
                                 f"{DWNOD * 10:6.1f} {RTDEP / 100:6.2f} ")

                    for I in range(1, N_LYR + 1):
                        NOUTDG.write(f" {RLV[I]:7.2f}")

                    NOUTDG.write("\n")

                    NOUTDG.write(f"{round(CUMSENSURF):8d} {round(CUMSENSOIL):7d}\n")
            #CSV output
            # if FMOPT == 'C':
            #     CsvOut(EXPNAME, CONTROL.RUN, CONTROL.TRTNUM, CONTROL.ROTNUM,
            #     CONTROL.REPNO, YEAR, DOY, DAS, DAP, VSTAGE, RSTAGE, XLAI,
            #     WTLF, STMWT, SDWT, RTWT, VWAD, TOPWT, SEEDNO, SDSIZE, HI, PODWT,
            #     PODNO, SWF_AV, TUR_AV, NST_AV, PS1_AV, PS2_AV, KST_AV, EXW_AV,
            #     PCNLP, SHELPC, HIP, PODWTD, SLAP, CANHT, CANWH,
            #     DWNOD, RTDEP, N_LYR, RLV, CUMSENSURF, CUMSENSOIL,
            #     vCsvline, vpCsvline, vlngth)
            #
            #     Linklst(vCsvline)

#           Set average stress factors since last printout back to zero
            SWF_AV = 0.0
            TUR_AV = 0.0
            NST_AV = 0.0
            EXW_AV = 0.0
            PS1_AV = 0.0
            PS2_AV = 0.0
            KST_AV = 0.0

            WTNVEG = (WTNLF + WTNST)

            if (WTLF + STMWT) > 0.0001 :
                PCNVEG = (WTNLF + WTNST) / (WTLF + STMWT) * 100.
            else:
                PCNVEG = 0.

            if FMOPT == 'A' or FMOPT == ' ':
                with open(OUTPN, 'a') as NOUTPN:
                    NOUTPN.write(
                        f" {YEAR:4d} {DOY:03d} {DAS:5d} {DAP:5d} "
                        f"{WTNCAN * 10:7.1f} {WTNSD * 10:7.1f} {WTNVEG * 10:7.1f} {PCNSDP:7.2f} {PCNVEG:7.2f} "
                        f"{WTNFX * 10:7.1f} {WTNUP * 10:7.1f} {WTNLF * 10:7.1f} {WTNST * 10:7.1f} "
                        f"{PCNLP:7.2f} {PCNSTP:7.2f} {PCNSHP:7.1f} {PCNRTP:6.1f} "
                        f"{NFIXN * 10:7.2f} {CUMSENSURFN:7.2f} {CUMSENSOILN:7.2f}\n"
                    )
            # if FMOPT == 'C':
            #     CsvOutPlNCrGro(EXPNAME, CONTROL.RUN, CONTROL.TRTNUM,
            #                    CONTROL.ROTNUM, CONTROL.REPNO, YEAR, DOY, DAS, DAP,
            #                    WTNCAN, WTNSD, WTNVEG, PCNSDP, PCNVEG, WTNFX, WTNUP,
            #                    WTNLF, WTNST, PCNLP, PCNSTP, PCNSHP, PCNRTP, NFIXN,
            #                    CUMSENSURFN, CUMSENSOILN,
            #                    vCsvlinePlNCrGro, vpCsvlinePlNCrGro, vlngthPlNCrGro)
            #     LinklstPlNCrGro(vCsvlinePlNCrGro)
            #     CsvOutPlCCrGro(EXPNAME, CONTROL.RUN, CONTROL.TRTNUM,
            #                    CONTROL.ROTNUM, CONTROL.REPNO, YEAR, DOY, DAS, DAP,
            #                    TOTWT, PG, CMINEA, GROWTH, GRWRES, MAINR, CADLF,
            #                    CADST, RHOLP, RHOSP, TGRO, TGROAV, PCNSDP, PCLSDP,
            #                    PCCSDP, TS,
            #                    vCsvlinePlCCrGro, vpCsvlinePlCCrGro, vlngthPlCCrGro)
            #     LinklstPlCCrGro(vCsvlinePlCCrGro)

            if FMOPT == 'A' or FMOPT == ' ':
                with open(OUTPC, 'a') as NOUTPC:
                    NOUTPC.write(
                        f" {YEAR:4d} {DOY:03d} {DAS:5d} {DAP:5d} "
                        f"{int(TOTWT * 10):6d} {PG:8.4f} {CMINEA:8.4f} {GROWTH:8.4f} {GRWRES:8.4f} {MAINR:8.4f} "
                        f"{(CADLF + CADST):8.5f} {RHOLP:7.3f} {RHOSP:7.3f} "
                        f"{TGRO[TS // 2]:6.3f} {TGROAV:6.3f} {PCNSDP:7.4f} {PCLSDP:7.4f} {PCCSDP:7.4f}\n"
                    )
    # ***********************************************************************
    #     Seasonal Output
    # ***********************************************************************
    elif DYNAMIC == RC.SEASEND :
        if FMOPT == 'A' or FMOPT == ' ':
            NOUTPN.close()
            NOUTDG.close()
            NOUTPC.close()

    return