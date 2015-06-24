########## scriptForImaging.py  ################################################################
#
# TWHya Band 7 imaging script
#
# MS file: TWHydra_corrected.ms
#
# Fields: 1
#    ID   Code Name                RA               Decl           Epoch   SrcId      nRows
#    0    none TW Hya              11:01:51.844983 -34.42.17.16088 J2000   0         126900
#
# Spectral Windows:  (4 unique spectral windows and 1 unique polarization setups)
#    SpwID  Name   #Chans   Frame   Ch0(MHz)  ChanWid(kHz)  TotBW(kHz) BBC Num  Corrs
#    0              3840   TOPO  356497.936       122.070    468750.0       1  XX  YY
#    1              3840   TOPO  357734.314       122.070    468750.0       4  XX  YY
#    2              3840   TOPO  346034.314       122.070    468750.0       3  XX  YY
#    3              3840   TOPO  343955.936       122.070    468750.0       2  XX  YY
# Sources: 4
#    ID   Name                SpwId RestFreq(MHz)  SysVel(km/s)
#    0    TW Hya              0     -              -
#    0    TW Hya              1     -              -
#    0    TW Hya              2     -              -
#    0    TW Hya              3     -              -
#
#
# version4: use uvcontsub for continuum, fixed error messages, 2014-07-04 AH
################################################################################################


#######################################################################
# Prepare Continuum Data for Further Processing
#######################################################################
print "# continuum subtraction (contsub)."
os.system('rm -rf TWHydra_corrected.ms.cont')
os.system('rm -rf TWHydra_corrected.ms.contsub')
uvcontsub(vis='TWHydra_corrected.ms',fitorder=1,          fitspw='0:20~1500;1900~3800,1,2:20~2000;2400~3800,3',combine='spw',want_cont=True)

print "# averaging continuum."
os.system('rm -rf TWHydra_cont.ms')
split(vis='TWHydra_corrected.ms.cont',outputvis='TWHydra_cont.ms',    width=3840,datacolumn='data')

plotms(vis='TWHydra_cont.ms',spw='0~3',xaxis='uvwave',yaxis='amp', avgtime='1e8',avgscan=T,coloraxis='spw',xselfscale=T,  plotfile='cont_uvplot.png',overwrite=T)

#######################################################################
# Create Initial Clean Image
#######################################################################
print "# making dirty image"
os.system('rm -rf TWHydra_cont_dirty.*')
clean(vis='TWHydra_cont.ms',imagename='TWHydra_cont_dirty',
      mode='mfs',imagermode='csclean',
      imsize=[512,512],cell=['0.1arcsec','0.1arcsec'],spw='',
      weighting='briggs',robust=0.0,
      interactive=F,
      multiscale=[],
      niter=0)

print "# Running clean."
os.system('rm -rf TWHydra_cont_clean.*')
clean(vis='TWHydra_cont.ms',imagename='TWHydra_cont_clean',
      mode='mfs', imagermode='csclean',
      imsize=[512,512],cell=['0.1arcsec','0.1arcsec'],spw='',
      weighting='briggs',robust=0.0,
      mask= 'circle [ [ 11h1m51.853s, -34.42.17.338] ,3arcsec ]',usescratch=False,
      multiscale=[],
      interactive=F,threshold='90mJy',niter=10000)




clean(vis='TWHydra_cont.ms',imagename='TWHydra_cont_clean_manual', mode='mfs', imagermode='csclean', imsize=[512,512],cell=['0.1arcsec','0.1arcsec'],spw='',  weighting='briggs',robust=0.0, mask= 'circle [ [ 11h1m51.853s, -34.42.17.338] ,3arcsec ]',usescratch=False,  multiscale=[],  interactive=T,threshold='90mJy')






ia.open('TWHydra_cont_clean2.image')
stat_cont=ia.statistics(axes=[0,1],plotstats=['mean','median','sigma','rms'],
                        mask='mask(TWHydra_cont_clean.mask)',robust=T)
ia.close()
thres=stat_cont['sigma'][0]*3.0


#######################################################################
# 1st Round of Continuum Data Phase Self-calibration
#######################################################################

gaincal(vis='TWHydra_cont.ms',caltable='self_1.pcal',
        solint='30s',combine='',gaintype='T',
        refant='DV06',spw='',minblperant=4,
        calmode='p',minsnr=2)


plotcal(caltable='self_1.pcal',xaxis='time',yaxis='phase',
        spw='',field='',antenna='1~8',iteration='antenna',
        subplot=421,plotrange=[0,0,-80,80],figfile='self_1_phase.png')


applycal(vis='TWHydra_cont.ms',field='',gaintable=['self_1.pcal'],calwt=F)

os.system('rm -rf TWHydra_cont_clean_1pcal.*')
clean(vis='TWHydra_cont.ms',imagename='TWHydra_cont_clean_1pcal',
      mode='mfs',imagermode='csclean',
      imsize=[512,512],cell=['0.1arcsec','0.1arcsec'],spw='',
      weighting='briggs',robust=0.0,
      mask='TWHydra_cont_clean.mask',usescratch=False,
      multiscale=[],
      interactive=F,threshold=str(thres)+'Jy',niter=10000)

ia.open('TWHydra_cont_clean_1pcal.image')
stat_cont=ia.statistics(axes=[0,1],plotstats=['mean','median','sigma','rms'],
                        mask='mask(TWHydra_cont_clean_1pcal.mask)',robust=T)
ia.close()
thres=stat_cont['sigma'][0]*3.0

#######################################################################
# 2nd Round of Continuum Data Phase Self-calibration
#######################################################################

gaincal(vis='TWHydra_cont.ms',caltable='self_2.pcal',
        solint='int',combine='',gaintype='T',
        refant='DV06',spw='',minblperant=4,
        calmode='p',minsnr=2)

plotcal(caltable='self_2.pcal',xaxis='time',yaxis='phase',
        spw='',field='',antenna='1~8',iteration='antenna',
        subplot=421,plotrange=[0,0,-80,80],figfile='self_2_phase.png')


applycal(vis='TWHydra_cont.ms',field='',gaintable=['self_2.pcal'],calwt=F)


os.system('rm -rf TWHydra_cont_clean_2pcal.*')
clean(vis='TWHydra_cont.ms',imagename='TWHydra_cont_clean_2pcal',
      mode='mfs',imagermode='csclean',
      imsize=[512,512],cell=['0.1arcsec','0.1arcsec'],spw='',
      weighting='briggs',robust=0.0,
      mask='TWHydra_cont_clean_1pcal.mask',
      multiscale=[],
      interactive=F,threshold=str(thres)+'Jy',niter=10000)

ia.open('TWHydra_cont_clean_2pcal.image')
stat_cont=ia.statistics(axes=[0,1],plotstats=['mean','median','sigma','rms'],
                        mask='mask(TWHydra_cont_clean_2pcal.mask)',robust=T)
ia.close()
thres=stat_cont['sigma'][0]*3.0

#######################################################################
# Continuum Data Amplitude Self-Calibration
#######################################################################

gaincal(vis='TWHydra_cont.ms',caltable='self_ap.cal',
        solint='inf',combine='',gaintype='T',
        refant='DV06',spw='',minblperant=4,
        gaintable=['self_2.pcal'],
        calmode='ap',minsnr=2)

plotcal(caltable='self_ap.cal',xaxis='time',yaxis='phase',
        spw='',field='',antenna='1~8',iteration='antenna',
        subplot=421,plotrange=[0,0,-1,1],figfile='self_ap_phase.png')

plotcal(caltable='self_ap.cal',xaxis='time',yaxis='amp',
        spw='',field='',antenna='1~8',iteration='antenna',
        subplot=421,plotrange=[0,0,0.6,1.4],figfile='self_ap_amp.png')


applycal(vis='TWHydra_cont.ms',field='',gaintable=['self_2.pcal','self_ap.cal'],calwt=F)


plotms(vis='TWHydra_cont.ms',xaxis='time',yaxis='amp',
       avgchannel='38',ydatacolumn='corrected',coloraxis='spw',
       plotfile='selfcal_time.png',overwrite=T)


plotms(vis='TWHydra_cont.ms',xaxis='uvdist',yaxis='amp',
       avgchannel='38',ydatacolumn='corrected',coloraxis='spw',
       plotfile='selfcal_uvdist.png',overwrite=T)


os.system('rm -rf TWHydra_cont_clean_apcal.*')
clean(vis='TWHydra_cont.ms',imagename='TWHydra_cont_clean_apcal',
      mode='mfs',imagermode='csclean',
      imsize=[512,512],cell=['0.1arcsec','0.1arcsec'],spw='',
      weighting='briggs',robust=0.0,
      mask='TWHydra_cont_clean_2pcal.mask',
      multiscale=[],
      interactive=F,threshold=str(thres)+'Jy',niter=10000)

ia.open('TWHydra_cont_clean_apcal.image')
stat_cont=ia.statistics(axes=[0,1],plotstats=['mean','median','sigma','rms'],
                        mask='mask(TWHydra_cont_clean_apcal.mask)',robust=T)
ia.close()
cont_sigma=float(stat_cont['sigma'][0])

#######################################################################
# Apply self-calibration to Full Dataset and Split Line Data
#######################################################################

applycal(vis='TWHydra_corrected.ms.contsub',gaintable=['self_2.pcal','self_ap.cal'],calwt=F)

### For CO(3-2)
os.system('rm -rf TWHydra_CO3_2.ms*')
split(vis='TWHydra_corrected.ms.contsub',outputvis='TWHydra_CO3_2.ms',datacolumn='corrected',spw='2')

### For HCO+(4-3)
os.system('rm -rf TWHydra_HCOplus.ms*')
split(vis='TWHydra_corrected.ms.contsub',outputvis='TWHydra_HCOplus.ms',datacolumn='corrected',spw='0')


#######################################################################
# Spectral Line Imaging
#######################################################################

### clean maps of emission lines ###

###### interactive cleaning ######
## clean map: CO(3-2) Imaging
print "# Running clean ;CO(3-2)"
os.system('rm -rf TWHydra_CO3_2line.*')
###### non-interactive cleaning ######
## clean map: CO(3-2) Imaging

print "# Running clean ;CO(3-2)"
os.system('rm -rf TWHydra_CO3_2line.*')
clean(vis='TWHydra_CO3_2.ms',imagename='TWHydra_CO3_2line',
      imagermode='csclean',spw='',
      imsize=[512,512],cell=['0.1arcsec','0.1arcsec'],
      mode='velocity',start='-4km/s',width='0.12km/s',nchan=118,
      restfreq='345.79599GHz',outframe='LSRK',
      weighting='briggs',robust=0.0,
      mask= 'circle [ [ 11h1m51.853s, -34.42.17.338] ,5arcsec ]',usescratch=False,
      interactive=F,threshold='60mJy',niter=100000)

###### interactive cleaning ######
## clean map: HCO+(4-3) Imaging
print "# Running clean ; HCO+(4-3)"
os.system('rm -rf TWHydra_HCOplusline.*')
clean(vis='TWHydra_HCOplus.ms',imagename='TWHydra_HCOplusline',
      imagermode='csclean',spw='',
      imsize=[512,512],cell=['0.1arcsec','0.1arcsec'],
      mode='velocity',start='-4km/s',nchan=118,width='0.12km/s',
      restfreq='356.7342GHz',outframe='LSRK',
      weighting='briggs',robust=0.0,
      mask= 'circle [ [ 11h1m51.853s, -34.42.17.338] ,5arcsec ]',usescratch=False,
      interactive=F,threshold='60mJy',niter=100000)

## image statistics
ia.open('TWHydra_CO3_2line.image')
stat_CO=ia.statistics(axes=[0,1],plotstats=['mean','median','sigma','rms'],robust=T)
ia.close()
ia.open('TWHydra_HCOplusline.image')
stat_HCOp=ia.statistics(axes=[0,1],plotstats=['mean','median','sigma','rms'],robust=T)
ia.close()

#######################################################################
# Image Analysis
#######################################################################

### Make Moment 0 Maps

### For CO(3-2)
os.system('rm -rf TWHydra_CO3_2line.image.mom0')
immoments(imagename='TWHydra_CO3_2line.image',moments=[0],
          outfile='TWHydra_CO3_2line.image.mom0',
          chans='40~76')

### For HCO+(4-3)
os.system('rm -rf TWHydra_HCOplusline.image.mom0')
immoments(imagename='TWHydra_HCOplusline.image',moments=[0],
          outfile='TWHydra_HCOplusline.image.mom0',
          chans='43~74')

### Check Images
imview(raster=[ {'file':'TWHydra_CO3_2line.image.mom0',
               'range':[0.0,10.],
               'scaling':-1},
               {'file':'TWHydra_HCOplusline.image.mom0',
               'range':[0.0,10.],
               'scaling':-1}],
      contour={'file':'TWHydra_cont_clean_apcal.image', 'base':0, 'unit':cont_sigma, 'levels':[3,100]})


### Make Higher Order Moment Maps for CO(3-2)
#For higher order moments it is very important to set a conservative flux threshold. Typically something like 6 sigma,
#using sigma from peak line channel, works well.

### For CO(3-2)
os.system('rm -rf TWHydra_CO3_2line.image.mom')
os.system('rm -rf TWHydra_CO3_2line.image.mom.weighted_coord')
os.system('rm -rf TWHydra_CO3_2line.image.mom.weighted_dispersion_coord')
immoments(imagename='TWHydra_CO3_2line.image',moments=[1,2],
          outfile='TWHydra_CO3_2line.image.mom',
          chans='40~76',includepix=[stat_CO['sigma'][0]*6.0,100])

imview(raster=[ {'file':'TWHydra_CO3_2line.image.mom0'},
                {'file':'TWHydra_CO3_2line.image.mom.weighted_coord'},
                {'file':'TWHydra_CO3_2line.image.mom.weighted_dispersion_coord'} ],
       contour={'file':'TWHydra_cont_clean_apcal.image', 'base':0, 'unit':cont_sigma, 'levels':[3,100]})


### For HCO+(3-2)

os.system('rm -rf TWHydra_HCOplusline.image.mom')
os.system('rm -rf TWHydra_HCOplusline.image.mom.weighted_coord')
os.system('rm -rf TWHydra_HCOplusline.image.mom.weighted_dispersion_coord')
immoments(imagename='TWHydra_HCOplusline.image',moments=[1,2],
          outfile='TWHydra_HCOplusline.image.mom',
          chans='43~74',includepix=[stat_HCOp['sigma'][0]*6.0,100])


#######################################################################
# Exporting Fits Images
#######################################################################

os.system('rm -rf TWHydra_CO3_2line.image.fits')
exportfits(imagename='TWHydra_CO3_2line.image',fitsimage='TWHydra_CO3_2line.image.fits')

os.system('rm -rf TWHydra_HCOplusline.image.fits')
exportfits(imagename='TWHydra_HCOplusline.image',fitsimage='TWHydra_HCOplusline.image.fits')

os.system('rm -rf TWHydra_CO3_2line.image.mom0.fits')
exportfits(imagename='TWHydra_CO3_2line.image.mom0',
           fitsimage='TWHydra_CO3_2line.image.mom0.fits')

os.system('rm -rf TWHydra_CO3_2line.image.mom.weighted_coord.fits')
exportfits(imagename='TWHydra_CO3_2line.image.mom.weighted_coord',
           fitsimage='TWHydra_CO3_2line.image.mom.weighted_coord.fits')

os.system('rm -rf TWHydra_CO3_2line.image.mom.weighted_dispersion_coord.fits')
exportfits(imagename='TWHydra_CO3_2line.image.mom.weighted_dispersion_coord',
           fitsimage='TWHydra_CO3_2line.image.mom.weighted_dispersion_coord.fits')

os.system('rm -rf TWHydra_HCOplusline.image.mom0.fits')
exportfits(imagename='TWHydra_HCOplusline.image.mom0',
           fitsimage='TWHydra_HCOplusline.image.mom0.fits')

os.system('rm -rf TWHydra_HCOplusline.image.mom.weighted_coord.fits')
exportfits(imagename='TWHydra_HCOplusline.image.mom.weighted_coord',
           fitsimage='TWHydra_HCOplusline.image.mom.weighted_coord.fits')

os.system('rm -rf TWHydra_HCOplusline.image.mom.weighted_dispersion_coord.fits')
exportfits(imagename='TWHydra_HCOplusline.image.mom.weighted_dispersion_coord',
           fitsimage='TWHydra_HCOplusline.image.mom.weighted_dispersion_coord.fits')

os.system('rm -rf TWHydra_cont_clean_apcal.image.fits')
exportfits(imagename='TWHydra_cont_clean_apcal.image',fitsimage='TWHydra_cont_clean_apcal.image.fits')

