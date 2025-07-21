
def TILEDRAIN(CONTROL, DLAYR, DUL, ETDR, NLAYR, SAT, SW, SWDELTS):
    from ModuleDefs import NL, RunConstants as RC
    from READS import find
    from ERROR import ERROR
    import fortranformat as ff

    ERRKEY = 'TILDR'


    FILEIO = CONTROL.FILEIO
    DYNAMIC = CONTROL.DYNAMIC

    if DYNAMIC == RC.RUNINIT:
        LNUM = 0
        try:
            with open(FILEIO, 'r') as f:
                lines = f.readlines()
                SECTION = '*FIELD'
                LNUM, FOUND = find(lines, SECTION)
                if not FOUND:
                    ERROR(SECTION, 42, FILEIO, LNUM)
                    return
                LNUM += 1
                fmt = ff.FortranRecordReader('(38X,2F6.0)')
                FLDD, SFDRN = fmt.read(lines[LNUM])
        except Exception as e:
            ERROR(ERRKEY,e,FILEIO,LNUM)
            return

        if FLDD <= 0.0:
            TDLNO = -99
        else:
            CUMDEP = DLAYR[0]
            for L in range(1, NLAYR):
                CUMDEP = CUMDEP + DLAYR[L]
                if CUMDEP >= FLDD and CUMDEP - DLAYR[L] < FLDD:
                    TDLNO = L + 1

        DRN = [0.0] * NL
        SWDELTT = [0.0] * NL
        TDFC = 0.0
        TDFD = 0.0
    # elif DYNAMIC == RC.SEASINIT:
    #     TDFC = 0.0

    # elif DYNAMIC == RC.RATE:
    #     DRN = [0.0] * NL
    #     SWDELTT = [0.0] * NL
    #     HEAD = 0.0
    #     TDF_AVAIL = 0.0
    #
    #     for L in range(TDLNO - 1, -1, -1):
    #         if SW[L] >= 0.98 * SAT[L]:
    #             HEAD = HEAD + DLAYR[L]
    #             TDF_AVAIL = TDF_AVAIL + (SW[L] + SWDELTS[L] - DUL[L]) * DLAYR[L]
    #             TOPSAT = L + 1
    #         else:
    #             HEAD = HEAD + (SW[L] - DUL[L]) / (SAT[L] - DUL[L]) * DLAYR[L]
    #             HEAD = max(HEAD, 0.0)
    #             TDF_AVAIL = TDF_AVAIL + (SW[L] + SWDELTS[L] - DUL[L]) * DLAYR[L]
    #             TOPSAT = L + 1
    #             break

    return DRN, SWDELTT, TDFC, TDFD, TDLNO