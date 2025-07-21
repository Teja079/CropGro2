# =======================================================================
#   OPTEMPX, Subroutine
#   Writes out "gray" .INH file with only one treatment in FILEX format, after
#   reading in a standard FILEX
# -----------------------------------------------------------------------
#   INPUT  : YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,
#            NYRS,VARNO,VRNAME,CROP,PATHMO,FROP,NREP,FILEIO
#
#   LOCAL  :
#
#   OUTPUT :
# -----------------------------------------------------------------------
#   Called : INPUT
#
#   Calls  : ERROR
# -----------------------------------------------------------------------
#                          DEFINITIONS
#
# =======================================================================
def OPTEMPXY2K(YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,
               SWINIT,INH4,INO3,NYRS,VARNO,VRNAME,CROP,
               FILEIO,FROP,ECONO,ATLINE,
               LNSIM,LNCU,LNHAR,LNENV,LNTIL,LNCHE,
               LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
               NFORC,PLTFOR,PMTYPE,NDOF,CHEXTR, MODEL, PATHEX, PMWD):

    from COMSOI import SOI01 as s1, SOI02 as s2, SOI03 as s3
    from COMIBS import IBS01 as ib1, IBS02 as ib2, IBS03 as ib3, IBS04 as ib4
    from COMSWI import SWITCH as sw, SWITCH1 as sw1, SWITCH2 as sw2
    from COMGEN import GEN01 as g1
    from ERROR import  ERROR
    import fortranformat as ff

    ERRKEY = 'OPTEMPX'

    LNSIMTMP = LNSIM
    if LNSIM <= 0: LNSIM = 1
    if len(ib1.TITSIM.rstrip()) <= 0: ib1.TITSIM = "MISSING"
    LN = len(FILEIO.strip())
    FILEIOH = FILEIO
    FILEIOH = FILEIOH[:-1] + 'H'
# -----------------------------------------------------------------------
#
# -----------------------------------------------------------------------
    try:
        f = open(FILEIOH, 'w')
        f.close()
    except Exception:
        ERROR (ERRKEY,-99,FILEIOH,0)
        return

    with open(FILEIOH, 'w') as f:
        fmt = ff.FortranRecordWriter("'*EXP.DETAILS: ',A8,A2,1X,A60, /,'*FILES'")
        f.write(fmt.write([ib1.EXPER, sw.CG, sw.ENAME])+'\n')

        f.write('@N  EXP.DATA    AFILE        ADIR\n')
        fmt = ff.FortranRecordWriter("I3,1X,'EXP         ',A12,1X,A80")
        f.write(fmt.write([LNSIM,sw.FILEA,PATHEX])+'\n')

        f.write('@N  SPECIES     SPFILE       SPDIR'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,'SPE         ',A12,1X,A80")
        f.write(fmt.write([LNSIM, sw.FILEC, sw.PATHCR])+'\n')

        f.write('@N  ECOTYPE     EFILE        EDIR'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,'ECO         ',A12,1X,A80")
        f.write(fmt.write([LNSIM, sw.FILEE, sw.PATHEC])+'\n')

        f.write('@N  CULTIVAR    CFILE        CDIR'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,'CUL         ',A12,1X,A80")
        f.write(fmt.write([LNSIM, sw.FILEG, sw.PATHGE])+'\n')

        f.write('@N  SOIL        SFILE        SDIR'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,'SOIL        ',A12,1X,A80")
        f.write(fmt.write([LNSIM, sw.FILES, sw.PATHSL])+'\n')

        f.write('@N  WEATHER     WFILE        WDIR'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,'WEATH       ',A12,1X,A80")
        f.write(fmt.write([LNSIM, sw.FILEW, sw.PATHWT])+'\n')

        f.write('\n*SIMULATION CONTROL'+'\n')
        LABL = 'GE'
        f.write('@N  GENERAL     NYERS NREPS START   SDATE RSEED SNAME.................... MODEL...'+'\n')
        if ib1.TITSIM[0] == ' ': ib1.TITSIM = '.' + ib1.TITSIM[1:]
        if MODEL[0] == ' ': MODEL = '.' + MODEL[1:]
        fmt = ff.FortranRecordWriter("I3,1X,A2,9X,1X,I5,1X,I5,5X,A1,1X,I7,1X,I5,1X,A25,1X,A8")
        f.write(fmt.write([LNSIM, LABL, NYRS, ib2.NREPSQ, ib1.ISIMI, ib2.YRSIM, ib2.RSEED1, ib1.TITSIM,MODEL])+'\n')

        f.write('@N  OPTIONS     WATER NITRO SYMBI PHOSP POTAS DISES  CHEM  TILL   CO2'+'\n')
        LABL = 'OP'
        fmt910 = ff.FortranRecordWriter("I3,1X,A2,9X,9(5X,A1)")
        f.write(fmt910.write([LNSIM,LABL, sw.ISWWAT, sw.ISWNIT, sw.ISWSYM, sw.ISWPHO,
                           sw.ISWPOT, sw.ISWDIS, sw.ISWCHE, sw.ISWTIL, sw.ICO2])+'\n')

        f.write('@N  METHODS     WTHER INCON LIGHT EVAPO INFIL PHOTO HYDRO NSWIT SOMDY MESEV MESOL METMP MEGHG'+'\n')
        LABL = 'OP'
        fmt = ff.FortranRecordWriter("I3,1X,A2,9X,7(5X,A1),1X,I5,5(5X,A1),5(5X,A1)")
        f.write(fmt.write([LNSIM,LABL, sw.MEWTH, sw.MESIC, sw.MELI, sw.MEEVP, sw.MEINF, sw.MEPHO, sw.MEHYD,
                           sw1.NSWITCH, sw.MESOM, sw1.MESEV, sw1.MESOL, sw.METMP, sw.MEGHG])+'\n')

        f.write('@N  MANAGEMENT  PLANT IRRIG FERTI RESID HARVS'+'\n')
        LABL = 'MA'
        f.write(fmt910.write([LNSIM, LABL, sw.IPLTI, sw.IIRRI, sw.IFERI, sw.IRESI, sw.IHARI])+'\n')

        f.write('@N  OUTPUTS     FNAME OVVEW SUMRY FROPT GROUT CAOUT WAOUT NIOUT MIOUT DIOUT  LONG CHOUT OPOUT'+'\n')
        LABL = 'OU'
        fmt = ff.FortranRecordWriter("I3,1X,A2,9X,3(5X,A1),4X,I2,9(5X,A1)")
        f.write(fmt.write([LNSIM,LABL, sw.IOX, sw.IDETO, sw.IDETS,FROP, sw.IDETG, sw.IDETC, sw.IDETW,
                           sw.IDETN, sw.IDETP, sw.IDETD, sw.IDETL, sw.IDETH, sw.IDETR])+'\n')

        f.write('\n!AUTOMATIC MANAGEM'+'\n')
        f.write('@N  PLANTING      PFRST   PLAST PH2OL PH2OU PH2OD PSTMX PSTMN'+'\n')
        LABL = 'PL'
        fmt = ff.FortranRecordWriter("I3,1X,A2,9X,2(1X,I7),5(1X,F5.0)")
        f.write(fmt.write([LNSIM,LABL, ib2.PWDINF, ib2.PWDINL, ib3.SWPLTL,
                           ib3.SWPLTH, ib3.SWPLTD, ib3.PTX, ib3.PTTN])+'\n')

        f.write('@N  IRRIGATION  IMDEP ITHRL ITHRU IROFF IMETH IRAMT IREFF'+'\n')
        LABL = 'IR'
        fmt = ff.FortranRecordWriter("I3,1X,A2,9X,3(1X,F5.0),2(1X,A5),1X,F5.1,1X,F5.3")
        f.write(fmt.write([LNSIM, LABL, ib3.DSOIL, ib3.THETAC, ib3.IEPT, ib1.IOFF,
                           ib1.IAME, ib3.AIRAMT, ib3.EFFIRR])+'\n')

        f.write('@N  NITROGEN    NMDEP NMTHR NAMNT NCODE NAOFF'+'\n')
        LABL = 'NI'
        fmt = ff.FortranRecordWriter("I3,1X,A2,9X,3(1X,F5.0),2(1X,A5),1X,F5.1,1X,F5.3")
        f.write(fmt.write([LNSIM, LABL, ib3.DSOILN, ib3.SOILNC, ib3.SOILNX, ib1.NCODE, ib1.NEND])+'\n')

        f.write('@N  RESIDUES    RIPCN RTIME RIDEP'+'\n')
        LABL = 'RE'
        fmt = ff.FortranRecordWriter("I3,1X,A2,10X,F5.0,1X,I5,1X,F5.0")
        f.write(fmt.write([LNSIM, LABL, ib3.RIP, ib2.NRESDL, ib3.DRESMG])+'\n')

        f.write('@N  HARVEST     HFRST   HLAST HPCNP HRCNR'+'\n')
        LABL = 'HA'
        fmt = ff.FortranRecordWriter("I3,1X,A2,9X,1X,I5,1X,I7,5(1X,F5.0)")
        f.write(fmt.write([LNSIM, LABL, ib2.HDLAY, ib2.HLATE, ib3.HPP, ib3.HRP])+'\n')

        f.write('\n*TREATMENTS",\n, @N   R O C TNAME....................  CU  FL  SA  IC  MP  '
                'MI  MF  MR  MC  MT  ME  MH  SM'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,I2,2(1X,I1),1X,A25,14(1X,I3)")
        f.write(fmt.write([ib2.TRTNO, ib2.ROTNO, ib2.ROTOPT, ib2.CRPNO, sw.TITLER, LNCU, LNFLD,
                           LNSA, LNIC, LNPLT, LNIR, LNFER, LNRES, LNCHE, LNTIL, LNENV, LNHAR, LNSIM])+'\n')
        LNSIM = LNSIMTMP

        #     Cultivar
        f.write('\n*CULTIVARS'+'\n')
        ATLINE = ATLINE[0] + 'C  CR INGENO CNAME... ' + ATLINE[23:]
        fmt = ff.FortranRecordWriter("I3,1X,I2,2(1X,I1),1X,A25,14(1X,I3)")
        f.write(fmt.write([ib2.TRTNO, ib2.ROTNO, ib2.ROTOPT, ib2.CRPNO, sw.TITLER, LNCU, LNFLD,
                           LNSA, LNIC, LNPLT, LNIR, LNFER, LNRES, LNCHE, LNTIL, LNENV, LNHAR, LNSIM])+'\n')
        LNSIM = LNSIMTMP

        f.write('@C  CR INGENO CNAME'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,A2,1X,A6,1X,A16")
        f.write(fmt.write([LNCU, CROP, VARNO, VRNAME])+'\n')
        LNSIM = LNSIMTMP

        f.write('\n*FIELDS')
        f.write('@L  ID_FIELD WSTA....  FLSA  FLOB  FLDT  FLDD  FLDS  FLST SLTX   SLDP ID_SOIL    PMALB  PMWD'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,A8,1X,A8,1X,F5.1,1X,F5.0,1X,A5,2(1X,F5.0),"
                                     "2(1X,A5),1X,F5.0,1X,A10,F6.2,2F6.1")
        f.write(fmt.write([LNFLD, ib1.FLDNAM, sw.FILEW[0:8], ib3.SLOPE, ib3.FLOB, ib1.DFDRN, ib3.FLDD, ib3.SFDRN,
                           ib1.FLST, ib1.SLTX, ib4.SLDP, s1.SLNO, ib4.PMALB, PMWD])+'\n')

        f.write('@L             XCRD            YCRD      ELEV              AREA  SLEN  FLWR  SLAS PRMGT'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,2(F15.5,1X),F9.2,1X,F17.1,1X,F5.0,2(1X,F5.1),1X,A5,I6")
        try:
            f.write(fmt.write([LNFLD, ib3.XCRD, ib3.YCRD, ib3.ELEV, ib3.AREA, ib3.SLEN, ib3.FLWR,
                               ib3.SLAS, ib1.FldHist, ib2.FHDur])+'\n')
        except Exception:
            ERROR(ERRKEY,-99,FILEIOH,-99)
            return

        if sw.ISWWAT != 'N':
            ib4.ICREN = max(ib4.ICREN, -9.0)
            ib4.ICREP = max(ib4.ICREP, -9.0)
            f.write('\n*FIELDS'+'\n')
            f.write("\n*INITIAL CONDITIONS"+'\n')
            f.write("@C  PCR     ICDAT  ICRT  ICND  ICRN  ICRE  ICWD ICRES ICREN ICREP ICRIP ICRID"+'\n')
            fmt = ff.FortranRecordWriter("I3,1X,A2,4X,I7,2(1X,I5),2(1X,F5.2),1X,F5.1,1X,I5, 2(1X,F5.2),2(1X,F5.0)")
            try:
                f.write(fmt.write([LNIC, PRCROP, YRIC, round(WRESR), round(WRESND), EFINOC, EFNFIX, ib4.ICWD,
                                   int(ib4.ICRES), ib4.ICREN, ib4.ICREP, ib4.ICRIP, ib4.ICRID])+'\n')
            except Exception:
                ERROR(ERRKEY, -99, FILEIOH, -99)
                return
            f.write("@C   ICBL  SH2O  SNH4  SNO3"+'\n')
            fmt = ff.FortranRecordWriter("I3,1X,F5.0,1X,F5.3,2(1X,F5.1)")
            for I in range(s2.NLAYR):
                f.write(fmt.write([LNIC, s3.DS[I], SWINIT[I], INH4[I], INO3[I]])+'\n')

        f.write('\n*PLANTING DETAILS\n@P    PDATE   EDATE  PPOP  PPOE  PLME  PLDS  PLRS  PLRD  '
                'PLDP  PLWT  PAGE  PENV  PLPH  SPRL'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,A8,1X,A8,1X,F5.1,1X,F5.0,1X,A5,2(1X,F5.0),"
                                     "2(1X,A5),1X,F5.0,1X,A10,F6.2,2F6.1")
        f.write(fmt.write([LNFLD, ib1.FLDNAM, sw.FILEW[0:8], ib3.SLOPE, ib3.FLOB, ib1.DFDRN, ib3.FLDD, ib3.SFDRN,
                           ib1.FLST, ib1.SLTX, ib4.SLDP, s1.SLNO, ib4.PMALB, PMWD])+'\n')

        f70 = ff.FortranRecordWriter("I3,1X,I7,1X,I7,2(1X,F5.1),2(5X,A1),2(1X,F5.0),"
                                     "1X,F5.1,2(1X,F5.0),3(1X,F5.1),I6,F6.1,2I6")
        f71 = ff.FortranRecordWriter("I3,1X,I7,1X,I7,2(1X,F5.1),2(5X,A1),2(1X,F5.0),"
                                     "1X,F5.1,I6,1X,F5.0,3(1X,F5.1),I6,F6.1,2I6")
        if 'PI' in CROP:
            f.write(f70.write([LNPLT, ib2.YRPLT, ib2.IEMRG, ib3.PLANTS, ib3.PLTPOP, ib1.PLME, ib1.PLDS, ib3.ROWSPC,
                               ib3.AZIR, ib3.SDEPTH, ib3.SDWTPL, ib3.SDAGE, ib3.ATEMP, ib3.PLPH, ib3.SPRLAP,
                               NFORC, PLTFOR, NDOF, PMTYPE])+'\n')
        elif ib3.SDWTPL <= 9999.:
            f.write(f70.write([LNPLT, ib2.YRPLT, ib2.IEMRG, ib3.PLANTS, ib3.PLTPOP, ib1.PLME, ib1.PLDS, ib3.ROWSPC,
                               ib3.AZIR, ib3.SDEPTH, ib3.SDWTPL, ib3.SDAGE, ib3.ATEMP, ib3.PLPH, ib3.SPRLAP])+'\n')
        else:
            f.write(f71.write([LNPLT,ib2.YRPLT, ib2.IEMRG, ib3.PLANTS, ib3.PLTPOP, ib1.PLME, ib1.PLDS, ib3.ROWSPC,
                               ib3.AZIR, ib3.SDEPTH, round(ib3.SDWTPL), ib3.SDAGE, ib3.ATEMP, ib3.PLPH, ib3.SPRLAP])+'\n')

        f.write('\n*IRRIGATION\n@I   IEFF  IDEP  ITHR  IEPT  IOFF  IAME  IAMT'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,F5.3,3(1X,F5.0),2(1X,A5),1X,F5.1")
        f.write(fmt.write([LNIR, ib3.EFFIRX, ib3.DSOILX, ib3.THETCX, ib3.IEPTX, ib1.IOFFX, ib1.IAMEX, ib3.AIRAMX])+'\n')

        f.write('@I    IDATE  IROP IRVAL'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,I7 ,1X,A5,1X,F5.1")
        for I in range(ib2.NIRR):
            f.write(fmt.write([LNIR, ib2.IDLAPL[I], ib1.IRRCOD[I], ib3.AMT[I]])+'\n')

        f.write('\n*FERTILIZERS\n@F    FDATE  FMCD  FACD  FDEP  FAMN  FAMP  FAMK  FAMC  FAMO  FOCD'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,I7,2(1X,A5),6(1X,F5.0),1X,A5")
        for I in range(ib2.NFERT):
            f.write(fmt.write([LNFER, ib2.FDAY[I], ib1.IFTYPE[I], ib1.FERCOD[I], ib3.DFERT[I], ib3.ANFER[I],
                               ib3.APFER[I], ib3.AKFER[I], ib3.ACFER[I], ib3.AOFER[I], ib1.FOCOD[I]])+'\n')

        f.write('\n*RESIDUES\n@R    RDATE  RCOD  RAMT  RESN  RESP  RESK  RINP  RDEP  RMET'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,I7,1X,A5,1X,I5,3(1X,F5.2),2(1X,F5.0),1X,A5")
        for I in range(ib2.NARES):
            f.write(fmt.write([LNRES, ib2.RESDAY[I], ib1.RESCOD[I],int(ib3.RESIDUE[I]), ib3.RESN[I], ib3.RESP[I],
                               ib3.RESK[I], ib3.RINP[I], ib3.DEPRES[I], ib1.RMET[I]])+'\n')

        #-----------------------------------------------------------------------
        #     Chemicals ....
        #-----------------------------------------------------------------------
        f.write('\n*CHEMICALS\n@C    CDATE CHCOD CHAMT  CHME CHDEP   CHT'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,I7,1X,A5,1X,F5.2,1X,A5,1X,F5.1,1X,A5,A42")
        for I in range(ib2.NCHEM):
            try:
                f.write(fmt.write([LNCHE, ib2.CDATE[I], ib1.CHCOD[I], ib3.CHAMT[I], ib1.CHMET[I], ib3.CHDEP[I],
                                   ib1.CHT[I], CHEXTR[I]])+'\n')
            except Exception:
                ERROR(ERRKEY, -99, FILEIOH, -99)
                return


# C-----------------------------------------------------------------------
# C    Tillage  ....
# C-----------------------------------------------------------------------
#       WRITE (LUNIO,'(/,"*TILLAGE",/,
#      &     "@T    TDATE TIMPL  TDEP")')
#       DO I = 1,NTIL
#          WRITE (LUNIO,85,IOSTAT=ERRNUM) LNTIL,TDATE[I],
#      &         TIMPL[I],TDEP[I]
#          IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIOH,LUNIO)
#       END DO
#    85 FORMAT (I3,1X,I7,1X,A5,1X,F5.1)

        f.write('\n*ENVIRONMENT\n@E    ODATE  EDAY  ERAD  EMAX  EMIN ERAIN  ECO2  EDEW EWIND'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,I7,5(1X,A1,F4.1),1X,A1,I4,2(1X,A1,F4.1)")
        for I in range(ib2.NEV):
            f.write(fmt.write([LNENV, ib2.WMDATE[I], ib1.DAYFAC[I], ib3.DAYADJ[I], ib1.RADFAC[I],
                                ib3.RADADJ[I], ib1.TXFAC[I],  ib3.TXADJ[I], ib1.TMFAC[I], ib3.TMADJ[I],
                                ib1.PRCFAC[I], ib3.PRCADJ[I], ib1.CO2FAC[I],int( ib3.CO2ADJ[I]),
                                ib1.DPTFAC[I], ib3.DPTADJ[I], ib1.WNDFAC[I], ib3.WNDADJ[I]])+'\n')

        f.write('\n*HARVEST\n@H    HDATE  HSTG  HCOM HSIZE   HPC  HBPC'+'\n')
        fmt = ff.FortranRecordWriter("I3,1X,I7,3(1X,A5),2(1X,F5.0)")
        for I in range(ib2.NHAR):
            f.write(fmt.write([LNHAR, ib2.HDATE[I], ib1.HSTG[I], ib1.HCOM[I], ib1.HSIZ[I], ib3.HPC[I], ib3.HBPC[I]])+'\n')


#Test driver for OPTEMPXY2K
# YRIC    = 0
# PRCROP  = ' '
# WRESR   = 0.0
# WRESND  = 0.0
# EFINOC  = 1.0
# EFNFIX  = 1.0
# SWINIT  = [9.1e-2,9.1e-2,7.6e-2,7.1e-2,7.1e-2,8.1e-2,7.8e-2,8.4e-2, 8.4e-2]
# INH4    = [0.0]*10
# INO3    = [0.0]*10
# NYRS    = 1
# VARNO   = 'SR0001'
# VRNAME  = 'Radiance   '
# CROP    = 'SR'
# FILEIO  = "DSSAT48.INP"
# FROP    = 1
# ECONO   = 'SR0001'
# ATLINE  = '@VAR#  VRNAME..........  ECO#  CSDL PPSEN EM-FL FL-SH FL-SD SD-PM FL-LF LFMAX SLAVR SIZLF  XFRT WTPSD SFDUR SDPDV PODUR THRSH SDPRO SDLIP                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               '
# LNSIM   = 1
# LNCU    = 1
# LNHAR   = 1
# LNENV   = 0
# LNTIL   = 0
# LNCHE   = 0
# LNFLD   = 1
# LNSA    = 0
# LNIC    = 0
# LNPLT   = 1
# LNIR    = 0
# LNFER   = 0
# LNRES   = 0
# NFORC   = 0
# PLTFOR  = 0.0
# PMTYPE  = 0
# NDOF    = 0
# CHEXTR  = ['']
# MODEL   = 'CRGRO48'
# PATHEX  = 'C:/DSSAT48/Strawberry/'
# PMWD    = -99.0
# from COMSOI import SOI02 as s2
# s2.NLAYR = 9
# OPTEMPXY2K(YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,
#                SWINIT,INH4,INO3,NYRS,VARNO,VRNAME,CROP,
#                FILEIO,FROP,ECONO,ATLINE,
#                LNSIM,LNCU,LNHAR,LNENV,LNTIL,LNCHE,
#                LNFLD,LNSA,LNIC,LNPLT,LNIR,LNFER,LNRES,
#                NFORC,PLTFOR,PMTYPE,NDOF,CHEXTR, MODEL, PATHEX, PMWD)