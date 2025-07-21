from io import FileIO
from Read import find, error, ignore
import numpy as np

def ipphenol(control: dict,
             exp_dict, cul_dict, spe_dict, eco_dict,CT1,CT2,CT3,leaf_dict):
    """
    Translating ipphenol Fortran subroutine to Python
    Need to add strict type declaration
    Need to add ignore function
    Need to change error catching

    :param: control
    :return: All ipphenol output variables as a tuple.

    need to add error catching to list reading
    try:
    except IndexError:

    Read from file ECO and SPE directly instead of reading the directories from INP
    Create additional module, to read fileX for species and cultivar ID.
    Add read function for each file.
    """

    # Variable declaration Section
    TB = np.empty([5], dtype=float)
    TO1 = np.empty([5], dtype=float)
    TO2 = np.empty([5], dtype=float)
    TM = np.empty([5], dtype=float)
    ctmp = np.empty([20], dtype='U3')
    dltyp = np.empty([20], dtype='U3')
    nprior = np.empty([20], dtype=int)
    tselc = np.empty([20], dtype=int)
    nsenp = np.empty([20], dtype=float)
    phthrs = np.empty([20], dtype=float)
    psenp = np.empty([20], dtype=float)
    wsenp = np.empty([20], dtype=float)

    #Initialize parameter variavles
    fileio = control["FILEIO"]
    blank = ' '
    NPHS =  13
    errkey = 'IPPHEN'

    # Read important file paths for i/o
    # Read in fileio as an array and get filec, pathcr, fileee, and pathec variables by indexing array.
    try:
        with (open(fileio, 'r') as file):
            lines = file.readlines()
            # Reading paths for files
            filec = lines[6][15:27]
            pathcr = lines[6][28:80].strip()
            filee = lines[7][15:27]
            pathec = lines[7][28:80].strip()
            lnum = 7
    except IOError as e:
        error(errkey, e.errno, fileio, 0)
        return
    # Find cultivar section and read two character crop code
    section = '*CULTI'
    lnum, found = find(lines, section)  # return line number
    if not found:
        error(section, 42, fileio, lnum)
        return
    lnum += 1
    crop = lines[lnum][3:5]


    # If there is a crop and not fallow, read in crop variables
    if crop != 'FA':
        # Find and read in 'PLANT' section
        section = '*PLANT'
        lnum, found = find(lines, section)
        if not found:
            error(section, 42, fileio, lnum)
            return
        lnum = lnum + 1
        plant_details = lines[lnum].split()
        plme = plant_details[4]
        sdepth = float(plant_details[8])
        sdage = float(plant_details[10])
        atemp = float(plant_details[11])

        # Find and read section cultivar section in the file
        section = '*CULTI'
        lnum, found = find(lines, section,2)
        if not found:
            error(section, 42, fileio, lnum)
            return
        lnum = lnum + 1
        cultivar_details = lines[lnum].split()
        econo = cultivar_details[2]
        csdvar = float(cultivar_details[3])
        ppsen = float(cultivar_details[4])
        ph2t5 = float(cultivar_details[5])
        phthrs[5], phthrs[7], phthrs[9], phthrs[12] =[float(i) for i in cultivar_details[6:10]]
#INP file is finished. -----------------------------------------------------------------------------

    if crop != 'FA':
        #Open filec .SPE file
        #first set the full path for species file.
        filecc = pathcr.strip() + filec
        try:
            with open(filecc, 'r') as file:
                lines = file.readlines()
        except IOError as e:
            error(errkey, e.errno, filecc, 0)
            return

        # Find Leaf Growth Parameters in FILEC, .SPE file
        section = '!*LEAF'
        lnum, found = find(lines, section)
        if not found:
            error(section, 42, filecc, lnum)
            return
        # Read data under this section
        lnum += 1
        leaf_details = lines[lnum].split()
        evmodc = float(leaf_details[4])

        # Find phenology parameters in FILEC, .SPE file
        section = '!*PHEN'
        lnum, found = find(lines, section)
        if not found:
            error(section, 42, filecc, lnum)
            return
        # Look for data under this section. Ignore comment lines
        for i in range(0,3):
            lnum, isect = ignore(lines,lnum)
            phen_details = lines[lnum].split()
            lnum += 1
            TB[i],TO1[i],TO2[i],TM[i] = (float(phen_details[0]), float(phen_details[1])
                             , float(phen_details[2]),float(phen_details[3]))

        for i in range(0,NPHS):
            lnum, isect = ignore(lines, lnum)
            phen_details = lines[lnum].split()
            lnum+=1
            j = int(phen_details[0])-1
            nprior[j],dltyp[j],ctmp[j],tselc[j],wsenp[j],nsenp[j],psenp[j] = (int(phen_details[1]),
                phen_details[2],phen_details[3],int(phen_details[4]),float(phen_details[5]),
                float(phen_details[6]),float(phen_details[7]))

        #Read Ecotype file---------------------------
        filegc = pathcr.strip() + filee
        lnum = 0
        try:
            with open(filegc, 'r') as file:
                lines = file.readlines()
        except IOError as e:
            error(errkey, e.errno, filegc, 0)
            return

        # "econo" is the target ecotype name.
        # While loop will read each ecotype line until the target is found.
        ecotyp:str = ''
        while (ecotyp != econo):
            lnum,isect = ignore(lines,lnum)
            c255 = lines[lnum]
            lnum+=1
            if c255[0] != ' ' and c255[0] != '*':
                ecotyp, econam = c255[0:6].strip(), c255[7:23].strip()
                vals = c255[24:].split()
                ivrgrp, ivrtem = int(vals[0]),int(vals[1])
                thvar, phthrs[:4], pm06, pm09, phthrs[10:12],trifol, r1ppo, optbi, slobi = (float(vals[2]),
                                            [float(i) for i in vals[3:7]],float(vals[7]),
                                            float(vals[8]),[float(i) for i in vals[10:12]],float(vals[12]),
                                            float(vals[14]),float(vals[15]),float(vals[16]))
                #-1 because XMAGE is not read
            #If target ecotype is not in the file
            elif(isect == 0):
                #Error if 'DFAULT' is the target ecotype, but was not found in the file
                if econo == 'DFAULT':
                    error(errkey, 35, filegc, lnum)
                    return
                #Set target ecotyp to DFAULT and search again
                econo = 'DFAULT'
                lnum = 0

        phthrs[5] = max(0.0, ph2t5-phthrs[3]-phthrs[4])
        phthrs[7] = phthrs[6] + max(0., (phthrs[8] - phthrs[6]) * pm06)
        phthrs[9] = max(0., phthrs[10] * pm09)

        if ppsen >= 0.0:
            cldvar = csdvar + (1. - thvar) / max(ppsen, 1e-6)
        else:
            cldvar = csdvar + (1. - thvar) / min(ppsen, -1e-6)

        csdvrr = csdvar - r1ppo
        cldvrr = cldvar - r1ppo

    return(crop,plme,sdepth,sdage,atemp,econo,csdvar,ppsen,ph2t5,phthrs,evmodc,TB,TO1,TO2,TM,nprior,dltyp,ctmp,tselc,
           wsenp,nsenp,psenp,ecotyp,econam,ivrgrp,ivrtem,thvar,pm06,pm09,trifol,r1ppo,optbi,slobi)