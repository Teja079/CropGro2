# =======================================================================
#   RunList, Subroutine
#   This routine writes a list of simulations to RUNList.OUT file
# =======================================================================

# from pickle import FALSE

def RunList(CONTROL):
    from ModuleDefs import ISWITCH, GET_ISWITCH
    import fortranformat as ff
    #import ModuleData


    DYNAMIC = CONTROL.DYNAMIC
    FILEX   = CONTROL.FILEX
    NYRS    = CONTROL.NYRS
    RNMODE  = CONTROL.RNMODE
    RUN     = CONTROL.RUN
    TRTNUM  = CONTROL.TRTNUM

    FIRST = True #TODO this variable needs to be saved in consecutive calls
    if FIRST:
        FIRST = False
        with open('RunList.OUT', 'w') as f:
            f.write(' NREP  EXP  TRT  NYR FILEX        DESCRIPTION\n')
        FILEX_LAST = '99999999.999'  #TODO this variable needs to be saved in consecutive calls
        EXPNO = 0 #TODO this variable needs to be saved in consecutive calls

    if DYNAMIC == 1:
        if FILEX != FILEX_LAST:
            EXPNO += 1
            FILEX_LAST = FILEX

        with open('RunList.OUT', 'a') as f:
            f200 = ff.FortranRecordWriter('4I5,1X,A12')
            f.write(f200.write([RUN, EXPNO, TRTNUM, NYRS, FILEX]))
    elif DYNAMIC > 5:  #TODO In Fortran, closes all open files at end of simulation. This case is probably not needed here
        ISWITCH = GET_ISWITCH()
        # CALL GET(ISWITCH)
        # IF (INDEX('0N', ISWITCH%IDETL) < 1) THEN
        #   CLOSE(RLUN)
        # ELSE
        #   CLOSE(RLUN, STATUS='DELETE')
        #
        #   !CALL GETLUN('LUN.LST',LUN)
        #   !CLOSE(LUN, STATUS='DELETE')
        #
        #   CALL GETLUN('FINPUT', LUN)
        #   OPEN (UNIT=LUN, FILE='HEADER.OUT')
        #   CLOSE(LUN,STATUS='DELETE')
        #
        #   CALL GETLUN('OUTINFO', LUN)
        #   CLOSE(LUN,STATUS='DELETE')

