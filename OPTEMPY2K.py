# C=======================================================================
# C  OPTEMPY2K, Subroutine
# C
# C  Determines experiment and treatment selection
# C-----------------------------------------------------------------------
# C  INPUT  : YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,
# C           TOTN,NYRS,VARNO,VRNAME,CROP,MODEL,PATHMO,ECONO,FROP,RUN,FILEIO
# C
# C  LOCAL  :
# C
# C  OUTPUT :
# C-----------------------------------------------------------------------
# C  Called : INPUT
# C
# C  Calls  : ERROR
# C=======================================================================
from ModuleDefs import *
def OPTEMPY2K(RNMODE, FILEX,PATHEX,
              YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,
              NYRS,VARNO,VRNAME,CROP,MODEL,RUN,FILEIO,EXPN,ECONO,FROP,TRTALL,
              TRTN,CHEXTR,NFORC,PLTFOR,NDOF,PMTYPE,ISENS,PMWD):

    import fortranformat as ff

    from COMIBS import IBS01 as ib1, IBS02 as ib2, IBS03 as ib3, IBS04 as ib4
    from COMSOI import SOI01 as s1, SOI02 as s2, SOI03 as s3
    from COMSWI import SWITCH as sw, SWITCH1 as sw1
    from COMGEN import GEN03 as g3

    ERRKEY = 'OPTEMPY2K'
    LINIO = 0

    formats = {
        40: ff.FortranRecordWriter('A20'),
        50: ff.FortranRecordWriter('I3,A8,1X,A2,1X,A60'),
        55: ff.FortranRecordWriter('I3,I2,2(1X,I1),1X,A25'),
        56: ff.FortranRecordWriter('3X,A2,1X,A6,1X,A16'),
        59: ff.FortranRecordWriter('(3X,A8,1X,A8,1X,F5.1,1X,F5.0,1X,A5,1X,F5.0,1X,F5.1,'
                                   '2(1X,A5),1X,F5.0,1X,A10,F6.2,2F6.1)'),
        60: ff.FortranRecordWriter('3X,2(F15.10,1X),F9.3,1X,F17.1,1X,F5.0,2(1X,F5.1),1X,A5,I6'),
        61: ff.FortranRecordWriter('(3X,A2,4X,I7,2(1X,I5),2(1X,F5.2),1X,F5.1,1X,I5, 2(1X,F5.2),2(1X,F5.0))'),
        70: ff.FortranRecordWriter('3X,I7,1X,I7,2F6.1,2(5X,A1),2(1X,F5.0),1X,F5.1,2(1X,F5.0),3(1X,F5.1),I6,F6.1,2I6'),
        71: ff.FortranRecordWriter('3X,I7,1X,I7,2F6.1,2(5X,A1),2(1X,F5.0),1X,F5.1,I6,1X,F5.0,3(1X,F5.1),I6,F6.1,2I6'),
        72: ff.FortranRecordWriter('3X,I7,1X,I7,2I6,2(5X,A1),2(1X,F5.0),1X,F5.1,2(1X,F5.0),3(1X,F5.1),I6,F6.1,2I6'),
        73: ff.FortranRecordWriter('3X,I7,1X,I7,2I6,2(5X,A1),2(1X,F5.0),1X,F5.1,I6,1(1X,F5.0),3(1X,F5.1),I6,F6.1,2I6'),
        74: ff.FortranRecordWriter('3X,I7,1X,I7,2I6,2(5X,A1),2(1X,F5.0),1X,F5.1,I6,1X,F5.0,3(1X,F5.1),I6,F6.1,2I6'),
        75: ff.FortranRecordWriter('2X,1X,F5.3,3(1X,F5.0),2(1X,A5),1X,F5.1'),
        76: ff.FortranRecordWriter('3X,I7,1X,A5,1X,F5.1'),
        77: ff.FortranRecordWriter('3X,I7,2(1X,A5),6(1X,F5.0),1X,A5'),
        78: ff.FortranRecordWriter('3X,I7,1X,A5,1X,F5.2,1X,A5,1X,F5.1,1X,A5,A42'),
        79: ff.FortranRecordWriter('3X,I7,1X,A5,1X,I5,3(1X,F5.2),2(1X,F5.0),1X,A5'),
        80: ff.FortranRecordWriter('3X,I7,1X,A5,1X,F5.1'),
        90: ff.FortranRecordWriter('3X,I7,5(1X,A1,F4.1),1X,A1,I4,2(1X,A1,F4.1)'),
        91: ff.FortranRecordWriter('3X,I7,1X,A1,F4.2,4(1X,A1,F4.1),1X,A1,I4,2(1X,A1,F4.1)'),
        92: ff.FortranRecordWriter('3X,I7,1X,A1,F4.1,1X,A1,F4.2,3(1X,A1,F4.1),1X,A1,I4,2(1X,A1,F4.1)'),
        93: ff.FortranRecordWriter('3X,I7,4(1X,A1,F4.1),1X,A1,F4.2,1X,A1,I4,2(1X,A1,F4.1)'),
        94: ff.FortranRecordWriter('3X,I7,2(1X,A1,F4.1),2(1X,A1,F4.0),1X,A1,F4.1,1X,A1,I4,2(1X,A1,F4.1)'),
        100: ff.FortranRecordWriter('3X,I7,3(1X,A5),2(1X,F5.0)'),
        900: ff.FortranRecordWriter('14X,I6,1X,I5,5X,A1,1X,I7,1X,I5,1X,A25'),
        910: ff.FortranRecordWriter('14X,9(5X,A1),2I6'),
        915: ff.FortranRecordWriter('14X,7(5X,A1),5X,I1,5(5X,A1)'),
        920: ff.FortranRecordWriter('14X,3(5X,A1),4X,I2,9(5X,A1)'),
        930: ff.FortranRecordWriter('14X,2(1X,I7),5(1X,F5.0)'),
        940: ff.FortranRecordWriter('14X,3(1X,F5.0),2(1X,A5),1X,F5.1,1X,F5.3'),
        950: ff.FortranRecordWriter('15X,F5.0,1X,I5,1X,F5.0'),
        960: ff.FortranRecordWriter('1X,A10,2X,A11,1X,A5,1X,F5.0,1X,A50'),
        970: ff.FortranRecordWriter('2(1X,A11),2(F8.3),1X,A50'),
        980: ff.FortranRecordWriter('1X,A5,1X,F5.2,1X,F5.1,1X,F5.2,1X,F5.0,2(1X,F5.2),4(1X,A5),F6.2'),
        991: ff.FortranRecordWriter('1X,F5.0,F6.2,9(1X,F5.1),F6.2,5F6.1'),
        992: ff.FortranRecordWriter('1X, F5.0, 4F6.2'),
        1000: ff.FortranRecordWriter('A6,1X,A16,1X,A6,1X,7(F6.1)'),
        1050: ff.FortranRecordWriter('A6,1X,A16,1X,A6,9(1X,F5.0),1X,I5,1X,F5.2,1X,F5.0,1X,F5.1'),
        1055: ff.FortranRecordWriter(
            'A6,1X,A16,1X,A6,F6.3,F6.0,2F6.3,F6.2,5F6.3,F6.2,F6.0,2F6.2,F6.2,7F6.1,2F6.2,1X,F5.4,7F6.1,F6.2,2F6.0,F6.1,F6.2,F6.2,F6.2,3F6.2,2F6.2'),
        1400: ff.FortranRecordWriter('A6,1X,A16,1X,A6,1X,F6.0,4(F6.1)'),
        1500: ff.FortranRecordWriter('A6,1X,A16,1X,A6,F6.2,F6.3,5F6.2,F6.3,2F6.1,F6.2,F6.3,3F6.2,F6.1,2F6.3'),
        1550: ff.FortranRecordWriter('A6,1X,A16,1X,A6,A'),
        1600: ff.FortranRecordWriter('A6,1X,A16,1X,A6,2(F6.1),F6.2,2(F6.1),F6.2,F6.0,F6.3,F6.2,6(F6.0)'),
        1700: ff.FortranRecordWriter('A6,1X,A16,1X,A6,1X,5(F6.1),F6.2,F6.1'),
        1800: ff.FortranRecordWriter('A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),2(F6.2)'),
        1850: ff.FortranRecordWriter('A6,1X,A16,1X,A6,1X,F6.2,F6.2,F6.1,F6.1,F6.1,F6.1,F6.2,F6.2,F6.1'),
        1855: ff.FortranRecordWriter('A6,1X,A16,1X,A6,1X,F6.2,F6.2,F6.1,F6.1,F6.1,F6.1,F6.2,F6.2,F6.1'),
        1801: ff.FortranRecordWriter('A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),2(F6.2),2(F6.1),I4'),
        1802: ff.FortranRecordWriter('A6,1X,A16,1X,A6,1X,F6.1,F6.3,2(F6.1),5(F6.2)'),
        1900: ff.FortranRecordWriter('A6,1X,A16,1X,A6,1X,F6.1,F6.2,4(F6.1),F6.2,4(F6.1)'),
        1901: ff.FortranRecordWriter('2F6.2'),
        1950: ff.FortranRecordWriter('A6,1X,A16,1X,A6,1X,F6.1,F6.2,2(F6.1),3(F6.2),2(F6.0)'),
        1960: ff.FortranRecordWriter('A6,1X,A16,1X,A6,1X,F5.1,1X,F5.2,1X,F5.1,1X,F5.0,1X,F5.2,1X,F5.0'),
        1975: ff.FortranRecordWriter('A6,1X,A16,1X,A6,4(F6.0),2(F6.2),3(F6.1)'),
        1995: ff.FortranRecordWriter('A6,1X,A16,1X,A6,F6.1,3(F6.3),F6.2,F6.1'),
        2000: ff.FortranRecordWriter("'*MODEL INPUT FILE   ',9X,A1,5(1X,I5)"),
        2040: ff.FortranRecordWriter("'MODEL          ',A8,5X,A80"),
        2050: ff.FortranRecordWriter("'FILEX          ',A12,1X,A80"),
        2100: ff.FortranRecordWriter("'FILEA          ',A12,1X,A80"),
        2200: ff.FortranRecordWriter("'FILET          ',A12,1X,A80"),
        2300: ff.FortranRecordWriter("'SPECIES        ',A12,1X,A80"),
        2400: ff.FortranRecordWriter("'ECOTYPE        ',A12,1X,A80"),
        2500: ff.FortranRecordWriter("'CULTIVAR       ',A12,1X,A80"),
        2600: ff.FortranRecordWriter("'PESTS          ',A12,1X,A80"),
        2700: ff.FortranRecordWriter("'SOILS          ',A12,1X,A80"),
        2900: ff.FortranRecordWriter("'OUTPUT         ',A8"),
        3000: ff.FortranRecordWriter('A6,1X,A16,1X,A255'),
    }

    with open(FILEIO, "w") as file: # IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,0)
        # Write temp. required variables on top of file
        LINIO += 1
        formatted_line = formats[2000].write([RNMODE,RUN,EXPN,TRTN,TRTALL,ISENS])
        file.write(formatted_line+"\n")
        #---------------------------------------------
        LINIO += 1
        formatted_line = formats[40].write(['*FILES              '])
        file.write(formatted_line + '\n')
        LINIO += 1
        formatted_line = formats[2040].write([MODEL])
        file.write(formatted_line + '\n')
        #IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

        LINIO += 1
        formatted_line = formats[2050].write([FILEX, PATHEX])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[2100].write([sw.FILEA, PATHEX])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[2200].write([sw.FILET, PATHEX])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[2300].write([sw.FILEC, sw.PATHCR])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[2400].write([sw.FILEE, sw.PATHEC])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[2500].write([sw.FILEG, sw.PATHGE])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[2600].write([sw.FILEP, sw.PATHPE])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[2700].write([sw.FILES, sw.PATHSL])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)
        if sw.MEWTH == 'M' or RNMODE == 'Y':
            LINIO += 1
            formatted_line = f"{'WEATHERW':<8}{' ':7}{sw.FILEW:<12} {' '}{sw.PATHWTW:<80}"
            file.write(formatted_line + '\n')
            # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        if sw.MEWTH == 'S' or sw.MEWTH == 'W':
            LINIO += 1
            formatted_line = f"{'WEATHERC':<8}{' ':7}{sw.FILEWC:<12} {' '}{sw.PATHWTC:<80}"
            file.write(formatted_line + '\n')
            # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        if sw.MEWTH == 'G':
            LINIO += 1
            formatted_line = f"{'WEATHERG':<8}{' ':7}{sw.FILEWG:<12} {' '}{sw.PATHWTG:<80}"
            file.write(formatted_line + '\n')
            # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        # Increment LINIO and write values to file using the format dictionary
        LINIO += 1
        formatted_line = formats[2900].write([*sw.OUTO[:8]])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        file.write(formats[40].write(['*SIMULATION CONTROL ']) + '\n')

        LINIO += 1
        formatted_line = formats[900].write([NYRS, ib2.NREPSQ, ib1.ISIMI, ib2.YRSIM, ib2.RSEED1, ib1.TITSIM])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[910].write([sw.ISWWAT, sw.ISWNIT, sw.ISWSYM, sw.ISWPHO, sw.ISWPOT, sw.ISWDIS, sw.ISWCHE, sw.ISWTIL, sw.ICO2])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[915].write([sw.MEWTH, sw.MESIC, sw.MELI, sw.MEEVP, sw.MEINF, sw.MEPHO, sw.MEHYD,
                                             sw1.NSWITCH, sw.MESOM, sw1.MESEV, sw1.MESOL, sw.METMP, sw.MEGHG])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[910].write([sw.IPLTI, sw.IIRRI, sw.IFERI, sw.IRESI, sw.IHARI])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[920].write([sw.IOX, sw.IDETO, sw.IDETS, FROP, sw.IDETG, sw.IDETC, sw.IDETW, sw.IDETN,
                                             sw.IDETP, sw.IDETD, sw.IDETL, sw.IDETH, sw.IDETR])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        file.write(formats[40].write(['!AUTOMATIC MANAGEM  ']) + '\n')

        LINIO += 1
        formatted_line = formats[930].write([ib2.PWDINF, ib2.PWDINL, ib3.SWPLTL, ib3.SWPLTH, ib3.SWPLTD, ib3.PTX, ib3.PTTN])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[940].write([ib3.DSOIL, ib3.THETAC, ib3.IEPT, ib1.IOFF, ib1.IAME, ib3.AIRAMT, ib3.EFFIRR])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[940].write([ib3.DSOILN, ib3.SOILNC, ib3.SOILNX, ib1.NCODE, ib1.NEND])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[950].write([ib3.RIP, ib2.NRESDL, ib3.DRESMG])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[930].write([ib2.HDLAY, ib2.HLATE, ib3.HPP, ib3.HRP])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        file.write(formats[40].write(['*EXP.DETAILS        ']) + '\n')

        LINIO += 1
        formatted_line = formats[50].write([EXPN, ib1.EXPER, sw.CG, sw.ENAME])
        file.write(formatted_line + '\n')

        LINIO += 1
        file.write(formats[40].write(['*TREATMENTS         ']) + '\n')

        LINIO += 1
        formatted_line = formats[55].write([ib2.TRTNO, ib2.ROTNO, ib2.ROTOPT, ib2.CRPNO, sw.TITLER])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        file.write(formats[40].write(['*CULTIVARS          ']) + '\n')

        LINIO += 1
        formatted_line = formats[56].write([CROP, VARNO, VRNAME])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        file.write(formats[40].write(['*FIELDS             ']) + '\n')

        LINIO += 1
        formatted_line = formats[59].write([ib1.FLDNAM, sw.FILEW[:8], ib3.SLOPE, ib3.FLOB, ib1.DFDRN, ib3.FLDD, ib3.SFDRN,
                                            ib1.FLST, ib1.SLTX, ib4.SLDP, s1.SLNO, PMWD, ib4.PMALB])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        formatted_line = formats[60].write([ib3.XCRD, ib3.YCRD, ib3.ELEV, ib3.AREA, ib3.SLEN, ib3.FLWR,
                                            ib3.SLAS, ib1.FldHist, ib2.FHDur])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        LINIO += 1
        file.write(formats[40].write(['*INITIAL CONDITIONS ']) + '\n')

        LINIO += 1
        EFINOC = max(EFINOC, -9.0)
        EFNFIX = max(EFNFIX, -9.0)
        ICREN = max(ib4.ICREN, -9.0)
        ICREP = max(ib4.ICREP, -9.0)

        formatted_line = formats[61].write([PRCROP, YRIC, round(WRESR), round(WRESND), EFINOC, EFNFIX, ib4.ICWD, int(ib4.ICRES), ICREN,
                                            ICREP, ib4.ICRIP, ib4.ICRID])
        file.write(formatted_line + '\n')
        # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

        for I in range(s2.NLAYR):
            LINIO += 1
            FMT = "(3X,F5.0,1X,F5.3,"
            if INH4[I] < 1.0 and INH4[I] > 0:
                FMT = FMT.strip() + "F6.3,"
            elif INH4[I] < 10.0 and INH4[I] > 0:
                FMT = FMT.strip() + "F6.2,"
            else:
                FMT = FMT.strip() + "F6.1,"

            if INO3[I] < 1.0 and INO3[I] > 0:
                FMT = FMT.strip() + "F6.3)"
            elif INO3[I] < 10.0 and INO3[I] > 0:
                FMT = FMT.strip() + "F6.2)"
            else:
                FMT = FMT.strip() + "F6.1)"
            fFMT = ff.FortranRecordWriter(FMT)
            formatted_line = fFMT.write([s3.DS[I], SWINIT[I], INH4[I], INO3[I]])
            file.write(formatted_line + '\n')
            #IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

        LINIO += 1
        file.write(formats[40].write(["*PLANTING DETAILS   "]) + '\n')
        LINIO += 1

        if "PI" in CROP:
            formatted_line = formats[70].write([ib2.YRPLT, ib2.IEMRG, ib3.PLANTS, ib3.PLTPOP, ib1.PLME,
                                                ib1.PLDS, ib3.ROWSPC, ib3.AZIR, ib3.SDEPTH, ib3.SDWTPL, ib3.SDAGE,
                                                ib3.ATEMP, ib3.PLPH, ib3.SPRLAP,
                                                NFORC, PLTFOR, NDOF, PMTYPE])
            file.write(formatted_line + '\n')
        else:
            if ib3.SDWTPL <= 9999. and ib3.PLANTS <= 9999.:
                formatted_line = formats[70].write([ib2.YRPLT, ib2.IEMRG, ib3.PLANTS, ib3.PLTPOP, ib1.PLME,
                                                    ib1.PLDS, ib3.ROWSPC, ib3.AZIR, ib3.SDEPTH, ib3.SDWTPL, ib3.SDAGE,
                                                    ib3.ATEMP, ib3.PLPH, ib3.SPRLAP])
            elif ib3.SDWTPL > 9999. >= ib3.PLANTS:
                formatted_line = formats[71].write([ib2.YRPLT, ib2.IEMRG, ib3.PLANTS, ib3.PLTPOP, ib1.PLME,
                                                    ib1.PLDS, ib3.ROWSPC, ib3.AZIR, ib3.SDEPTH, round(ib3.SDWTPL), ib3.SDAGE,
                                                    ib3.ATEMP, ib3.PLPH, ib3.SPRLAP])
            elif ib3.SDWTPL <= 9999. < ib3.PLANTS:
                formatted_line = formats[72].write([ib2.YRPLT, ib2.IEMRG, round(ib3.PLANTS), round(ib3.PLTPOP), ib1.PLME,
                                                    ib1.PLDS, ib3.ROWSPC, ib3.AZIR, ib3.SDEPTH, ib3.SDWTPL, ib3.SDAGE,
                                                    ib3.ATEMP, ib3.PLPH, ib3.SPRLAP])
            else:
                formatted_line = formats[73].write([ib2.YRPLT, ib2.IEMRG, round(ib3.PLANTS), round(ib3.PLTPOP), ib1.PLME,
                                                    ib1.PLDS, ib3.ROWSPC, ib3.AZIR, ib3.SDEPTH, round(ib3.SDWTPL), ib3.SDAGE,
                                                    ib3.ATEMP, ib3.PLPH, ib3.SPRLAP])

            file.write(formatted_line + '\n')
        #IF (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)
        #C-----------------------------------------------------------------------
        #C     IRRIGATION
        #C-----------------------------------------------------------------------
        LINIO += 1
        file.write(formats[40].write(["*IRRIGATION         "]) + '\n')
        LINIO += 1

        formatted_line = formats[75].write([ib3.EFFIRX, ib3.DSOILX, ib3.THETCX, ib3.IEPTX, ib1.IOFFX,
                                            ib1.IAMEX, ib3.AIRAMX])
        file.write(formatted_line + '\n')
        # if (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

        if ib2.NIRR > 0:
            for I in range(ib2.NIRR):
                LINIO += 1
                formatted_line = formats[76].write([ib2.IDLAPL[I], ib1.IRRCOD[I], ib3.AMT[I]])
                file.write(formatted_line + '\n')
                # if (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

        LINIO += 1
        file.write(formats[40].write(["*FERTILIZERS        "]) + '\n')

        if ib2.NFERT > 0:
            for I in range(ib2.NFERT):
                LINIO += 1
                formatted_line = formats[77].write([ib2.FDAY[I], ib1.IFTYPE[I], ib1.FERCOD[I], ib3.DFERT[I], ib3.ANFER[I],
                                                    ib3.APFER[I], ib3.AKFER[I], ib3.ACFER[I], ib3.AOFER[I], ib1.FOCOD[I]])
                file.write(formatted_line + '\n')
                # if (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

        LINIO += 1
        file.write(formats[40].write(["*RESIDUES           "]) + '\n')

        if ib2.NARES > 0:
            for I in range(ib2.NARES):
                LINIO += 1
                formatted_line = formats[79].write([ib2.RESDAY[I], ib1.RESCOD[I],
                                                    int(ib3.RESIDUE[I]), ib3.RESN[I], ib3.RESP[I], ib3.RESK[I], ib3.RINP[I],
                                                    ib3.DEPRES[I], ib1.RMET[I]])
                file.write(formatted_line + '\n')
                # if (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

        #C-----------------------------------------------------------------------
        #C     Chemicals ....
        #C-----------------------------------------------------------------------
        LINIO += 1
        file.write(formats[40].write(["*CHEMICALS          "]) + '\n')

        if ib2.NCHEM > 0:
            for I in range(ib2.NCHEM):
                LINIO += 1
                formatted_line = formats[78].write([ib2.CDATE[I], ib1.CHCOD[I],
                                                    ib3.CHAMT[I], ib1.CHMET[I], ib3.CHDEP[I], ib1.CHT[I], CHEXTR[I]])
                file.write(formatted_line + '\n')
                # if (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

        # C - ----------------------------------------------------------------------
        # C   Tillage....
        # C - ----------------------------------------------------------------------
        LINIO += 1
        file.write(formats[40].write(["*TILLAGE            "]) + '\n')

        if ib2.NTIL > 0:
            for I in range(ib2.NTIL):
                LINIO += 1
                formatted_line = formats[80].write([ib2.TDATE[I], ib1.TIMPL[I], ib3.TDEP[I]])
                file.write(formatted_line + '\n')
                # if (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

        LINIO += 1
        file.write(formats[40].write(["*ENVIRONMENT        "]) + '\n')

        if ib2.NEV > 0:
            for I in range(ib2.NEV):
                LINIO += 1
                if ib1.DAYFAC[I] == 'M' and ib3.DAYADJ[I] <= 10.0:
                    formatted_line = formats[91].write([ib2.WMDATE[I], ib1.DAYFAC[I], ib3.DAYADJ[I], ib1.RADFAC[I],
                                                        ib3.RADADJ[I], ib1.TXFAC[I], ib3.TXADJ[I], ib1.TMFAC[I],
                                                        ib3.TMADJ[I], ib1.PRCFAC[I], ib3.PRCADJ[I], ib1.CO2FAC[I],
                                                        int(ib3.CO2ADJ[I]), ib1.DPTFAC[I], ib3.DPTADJ[I], ib1.WNDFAC[I],
                                                        ib3.WNDADJ[I]])
                elif ib1.RADFAC[I] == 'M' and ib3.RADADJ[I] <= 10.0:
                    formatted_line = formats[92].write([ib2.WMDATE[I], ib1.DAYFAC[I], ib3.DAYADJ[I], ib1.RADFAC[I],
                                                        ib3.RADADJ[I], ib1.TXFAC[I], ib3.TXADJ[I], ib1.TMFAC[I],
                                                        ib3.TMADJ[I], ib1.PRCFAC[I], ib3.PRCADJ[I], ib1.CO2FAC[I],
                                                        int(ib3.CO2ADJ[I]), ib1.DPTFAC[I], ib3.DPTADJ[I], ib1.WNDFAC[I],
                                                        ib3.WNDADJ[I]])
                elif ib1.PRCFAC[I] == 'M' and ib3.PRCADJ[I] <= 10.0:
                    formatted_line = formats[93].write([ib2.WMDATE[I], ib1.DAYFAC[I], ib3.DAYADJ[I], ib1.RADFAC[I],
                                                        ib3.RADADJ[I], ib1.TXFAC[I], ib3.TXADJ[I], ib1.TMFAC[I],
                                                        ib3.TMADJ[I], ib1.PRCFAC[I], ib3.PRCADJ[I], ib1.CO2FAC[I],
                                                        int(ib3.CO2ADJ[I]), ib1.DPTFAC[I], ib3.DPTADJ[I], ib1.WNDFAC[I],
                                                        ib3.WNDADJ[I]])

                elif (ib1.TXFAC[I] == 'R' and ib3.TXADJ[I] <= -10.0) or (ib1.TMFAC[I] == 'R' and ib3.TMADJ[I] <= -10.0):
                    formatted_line = formats[94].write([ib2.WMDATE[I], ib1.DAYFAC[I], ib3.DAYADJ[I], ib1.RADFAC[I],
                                                        ib3.RADADJ[I], ib1.TXFAC[I], ib3.TXADJ[I], ib1.TMFAC[I],
                                                        ib3.TMADJ[I], ib1.PRCFAC[I], ib3.PRCADJ[I], ib1.CO2FAC[I],
                                                        int(ib3.CO2ADJ[I]), ib1.DPTFAC[I], ib3.DPTADJ[I], ib1.WNDFAC[I],
                                                        ib3.WNDADJ[I]])
                else:
                    formatted_line = formats[90].write([ib2.WMDATE[I], ib1.DAYFAC[I], ib3.DAYADJ[I], ib1.RADFAC[I],
                                                        ib3.RADADJ[I], ib1.TXFAC[I], ib3.TXADJ[I], ib1.TMFAC[I],
                                                        ib3.TMADJ[I], ib1.PRCFAC[I], ib3.PRCADJ[I], ib1.CO2FAC[I],
                                                        int(ib3.CO2ADJ[I]), ib1.DPTFAC[I], ib3.DPTADJ[I], ib1.WNDFAC[I],
                                                        ib3.WNDADJ[I]])

                file.write(formatted_line + '\n')
                # if (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

        LINIO += 1
        file.write(formats[40].write(["*HARVEST            "]) + '\n')
        if ib2.NHAR > 0:
            for I in range(ib2.NHAR):
                LINIO += 1
                formatted_line = formats[100].write([ib2.HDATE[I], ib1.HSTG[I], ib1.HCOM[I],
                                                     ib1.HSIZ[I], ib3.HPC[I], ib3.HBPC[I]])
                file.write(formatted_line + '\n')
                # if (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

        # -----------------------------------------------------------------------
        # SOIL DATA
        # Do not need soil input for sequenced runs (except first year)
        if ('FQ' not in RNMODE or RUN == 1):
            LINIO += 1
            file.write(formats[40].write(["*SOIL               "]) + '\n')

            LINIO += 1
            formatted_line = formats[960].write([s1.SLNO, s1.SLSOUR, ib1.SLTX, ib4.SLDP, s1.SLDESC])
            file.write(formatted_line + '\n')
            # if (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

            LINIO += 1
            formatted_line = formats[970].write([s1.SSITE, s1.SCOUNT, s3.SLAT, s3.SLONG, s1.TAXON])
            file.write(formatted_line + '\n')
            # if (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

            formatted_line = formats[980].write([s1.SCOM, s3.SALB, s3.U, s3.SWCON, s3.CN2, s3.SLNF, s3.SLPF,
                                                 s1.SMHB, s1.SMPX, s1.SMKE, s1.SGRP])
            file.write(formatted_line + '\n')
            LINIO += 1
            # if (ERRNUM .NE. 0) CALL ERROR (ERRKEY,ERRNUM,FILEIO,LINIO)

            for I in range(s2.NLAYR):
                LINIO+=1
                FMT = "{:5.0f}      {:5.3f} {:5.3f} {:5.3f} {:5.3f}"

                if s3.SWCN[I] < 1E-6:
                    FMT += " {:5.0f} {:5.2f}"  # "  -99."
                elif s3.SWCN[I] < 0.1:
                    FMT += " {:5.4f} {:5.2f}"  # "0.0001" to "0.0999"
                elif s3.SWCN[I] < 1.0:
                    FMT += " {:5.3f} {:5.2f}"  # " 0.100" to " 0.999"
                elif s3.SWCN[I] < 10.0:
                    FMT += " {:5.2f} {:5.2f}"  # "  1.00" to "  9.99"
                elif s3.SWCN[I] < 100.0:
                    FMT += " {:5.1f} {:5.2f}"  # "  10.0" to "  99.9"
                else:
                    FMT += " {:5.0f} {:5.2f}"  # "  100." to " 9999."

                if 0.0 < s3.OC[I] < 9.99:
                    FMT += " {:5.3f} {:5.1f} {:5.1f} {:5.1f}"
                else:
                    FMT += " {:5.1f} {:5.1f} {:5.1f} {:5.1f}"

                if s3.TOTN[I] > 10.0:
                    FMT += " {:6.2f} {:6.2f} {:6.2f} {:6.2f} {:6.2f}"
                elif s3.TOTN[I] > 0.0:
                    FMT += " {:6.3f} {:6.2f} {:6.2f} {:6.2f} {:6.2f}"
                else:
                    FMT += " {:6.1f} {:6.2f} {:6.2f} {:6.2f} {:6.2f}"

                formatted_line = FMT.format(
                    s3.DS[I], s3.LL[I], s3.DUL[I], s3.SAT[I], s3.SHF[I], s3.SWCN[I], s3.BD[I],
                    s3.OC[I], s3.CLAY[I], s3.SILT[I], s3.STONES[I], s3.TOTN[I],
                    s3.PH[I], s3.PHKCL[I], s3.CEC[I], s3.ADCOEF[I]
                )
                file.write(formatted_line + '\n')
                # if ERRNUM != 0: CALL ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

            #!       2nd tier soils
            LINIO += 1
            file.write("                    \n")

            for I in range(s2.NLAYR):
                LINIO += 1

                # Writing the main data line
                formatted_line = formats[991].write([
                    s3.DS[I], s3.EXTP[I], s3.TOTP[I], s3.ORGP[I], s3.CACO[I], s3.EXTAL[I],
                    s3.EXTFE[I], s3.EXTMN[I], s3.TOTBAS[I], s3.PTERMA[I], s3.PTERMB[I],
                    s3.EXK[I], s3.EXMG[I], s3.EXNA[I], s3.EXTS[I], s3.SLEC[I], s3.EXCA[I]
                ])
                file.write(formatted_line)

                # Handling SASC(I) condition
                if s3.SASC[I] > 0:
                    file.write(f" {s3.SASC[I]:6.3f}\n")  # Equivalent to WRITE (LUNIO,'(F6.3)',IOSTAT=ERRNUM) SASC(I)
                else:
                    file.write("  -99.\n")  # Equivalent to WRITE (LUNIO,'("  -99.")',IOSTAT=ERRNUM)
                # if ERRNUM != 0: ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)

            LINIO += 1
            # 3rd tier soils
            LINIO += 1
            file.write("                    \n")

            for I in range(s2.NLAYR):
                LINIO += 1
                formatted_line = formats[992].write([s3.DS[I], s3.alphaVG[I], s3.mVG[I], s3.nVG[I], s3.WCR[I]])
                file.write(formatted_line + "\n")  # Ensure newline after writing
                # if ERRNUM != 0: ERROR(ERRKEY, ERRNUM, FILEIO, LINIO)  # Assuming an `error` function exists

        #End of non-sequence soils write
        if CROP != 'FA':
            formatted_line = formats[40].write(['*CULTIVAR           '])
            file.write(formatted_line + "\n")
            LINIO = LINIO + 1
            if MODEL[:5] in ("CRGRO", "PRFRM"):
                formatted_line = formats[1500].write([VARNO, VRNAME, ECONO, g3.CSDVAR,
                                                      g3.PPSEN, g3.PH2T5, g3.PHTHRS[5], g3.PHTHRS[7], g3.PHTHRS[9],
                                                      g3.PHTHRS[12],g3.LFMAX, g3.SLAVAR, g3.SIZELF, g3.XFRUIT, g3.WTPSD,
                                                      g3.SFDUR, g3.SDPDVR, g3.PODUR, g3.THRESH, g3.SDPRO, g3.SDLIP
                                                      ])
                file.write(formatted_line)

#Test driver for OPTEMPY2K
# from ModuleDefs import NL
# from COMSOI import SOI02 as s2
# RNMODE = 'B'
# FILEX = 'UFBA1701.SRX'
# PATHEX = 'C://DSSAT48//Strawberry//'
# YRIC = 1
# PRCROP  = ' '
# WRESR = 0.0
# WRESND = 0.0
# EFINOC = 1.0
# EFNFIX = 1.0
# SWINIT = [9.1e-2, 9.1e-2, 7.6e-2, 7.1e-2, 7.1e-2, 8.1e-2, 7.8e-2, 8.4e-2, 8.4e-2]+[0.0] * 10
# INH4 = [0.0] * 20
# INO3 = [0.0] * 20
# NYRS = 1
# VARNO = 'SR0001' + ' '*80
# VRNAME = 'Radiance   '+ ' '*80
# CROP = 'SR'+ ' '*80
# MODEL = 'CRGRO048'+ ' '*80
# RUN = 1
# FILEIO = 'DSSAT48.INP'+ ' '*80
# EXPN = 1
# ECONO = 'SR0001'+ ' '*80
# FROP = 1
# TRTALL = 999
# TRTN = 1
# CHEXTR = ['  ']
# NFORC = 0
# PLTFOR = 0.0
# NDOF = 0
# PMTYPE = 0
# ISENS = 0
# PMWD = -99.0
# s2.NLAYR = 9
#
# OPTEMPY2K(RNMODE, FILEX,PATHEX,
#           YRIC,PRCROP,WRESR,WRESND,EFINOC,EFNFIX,SWINIT,INH4,INO3,
#           NYRS,VARNO,VRNAME,CROP,MODEL,RUN,FILEIO,EXPN,ECONO,FROP,TRTALL,
#           TRTN,CHEXTR,NFORC,PLTFOR,NDOF,PMTYPE,ISENS,PMWD)