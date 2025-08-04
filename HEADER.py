# #TODO needs to placed in OPHEAD
# def HEADER(DYNAMIC, f, RUN):
#     """
#     :param DYNAMIC: CSM control integer
#     :param f: Open file
#     :param RUN: Run number
#     """
#
#     from ModuleDefs import RunConstants as RC, Headers, Version, VBranch
#     from UTILS import date_and_time
#     import fortranformat as ff
#
#     ICOUNT = Headers.ICOUNT
#     ShortCount = Headers.ShortCount
#
#     if hasattr(HEADER, "PREV_RUN"):
#         if RUN != HEADER.PREV_RUN:
#             HEADER.NOHEADER = True
#
#     HEADER.PREV_RUN = RUN
#
#     if ICOUNT > 0:
#         f.write('\n',Headers.Header[0])
#     else:
#         DATE_TIME = date_and_time()
#         f500 = ff.FortranRecordWriter('"*DSSAT Cropping System Model Ver. ",I1,".",I1,".",I1,'
#                                       '".",I3.3,1X,A,4X,'
#                                       'A3," ",I2.2,", ",I4,"; ",I2.2,":",I2.2,":",I2.2')
#         f.write(f500.write([Version, VBranch, DATE_TIME[1], DATE_TIME[2], DATE_TIME[0],
#                             DATE_TIME[4], DATE_TIME[5], DATE_TIME[6]]) + '\n')
#         f.write(f"\n*RUN {RUN}\n\n")
#     if DYNAMIC == RC.RUNINIT:
#         for i in range(1,ICOUNT):
#             try:
#                 f.write(Headers.Header[i].strip())
#             except:
#                 break
#     elif DYNAMIC == RC.SEASINIT:
#         if HEADER.NOHEADER and RUN > 0 and ICOUNT > 0:
#             for i in range(1,ShortCount):
#                 f.write(Headers.Header[i])
#             f.write(" ")
#             HEADER.NOHEADER = False
