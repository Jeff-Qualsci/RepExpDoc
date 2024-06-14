source(file = 'R/fun_msx.R')

MSD20Data <- read_csv('Data/MSD20Data320.csv')
MSD_320_20 <- repexp.efficacy(MSD20Data)
repexp.save(report = MSD_320_20, path = 'MSD20')

ScreenData <- read_csv('Data/ScrnMSD.csv')
ScreenReport <- repexp.efficacy(ScreenData)
repexp.save(report = ScreenReport, path = 'Screen')

MSR3Data <- read_csv('Data/MSR3data32.csv')
MSR3Report <- repexp.potency(MSR3Data)
repexp.save(report = MSR3Report, path = 'MSR3')
