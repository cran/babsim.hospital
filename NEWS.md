# babsim.hospital
## Release Policy:
  * Even release numbers refer to unstable versions, e.g., 1.5.12

## 11.7.12
   * BUGFIX:
    Found the following (possibly) invalid URLs:
    URL: https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/_inhalt.html
     From: man/GermanCounties.Rd
           man/GermanStates.Rd
     Status: 400
     Message: Bad Request


## 11.7.10
   * CHANGES:
      * icudata.R, icudataFull.R, rkidata.R, and rkidataFull.R files added 

## 11.7.8
   * BUGFIX
      * mc.coresN is handled correctly
      * babsimHospital example uses conf correctly

## 11.7.6
  * BUGFIX: 
     * meesagef messageDataRange are exported

## 11.7.4
   * NEW:
      * vaccine counts added

## 11.7.2
   * package plotly not suggested anymore

## 11.7.0
   * New data structures

## 11.6.0
   * Improved prediction /conf intervals

## 11.5.18
   * more regions
   * envToTibble does not use a default conf argument

## 11.5.16
  * Faster evalutions:
     * getDecision() rewritten
     * updateP() rewritten
  
## 11.5.14
  * CRAN comments fixed

## 11.5.12
  * CHANGES:
     * Cleanup
     * Accelerate testing (region is set to 5374 whenever possible)

## 11.5.10
  * CHANGES:
     * babsimrun returns list 

## 11.5.8
   * CHANGES:
      * importFrom dependencies reduced. The following packages were removed from the 
        DESCRIPTION Imports list:
          * Metrics,   simmer.plot,   rpart.plot
      * examples use smaller data sets

## 11.5.6
   *new para file:
      * obkpara with 29 variables (135 obs)

## 11.5.4
  * new para file:
      * nrwpara with 29 variables (5 results)

## 11.5.2
   * new para file:
      * koelnpara with 29 variables
      
## 11.5.0
   * WARNING:
      runoptDirect() uses x0 <- getStartParameter(region= -1),
      because there is no previous region parameter set
   * NEW: 
      * edge: IntensiveAftercare -> Healthy
   * UPDATE: 
       * Probabilities and Durations refined based on feedback from doctors
  
## 11.4.6.
    * R0 ranges from 0.1
    * Error function tests
    * Hall of Fame
  
## 11.4.4
    * CHANGES
       * getBounds(): new lower limit (1 instead of 0.5) for Risk Male

## 11.4.2 
    * CHANGES:
       * funEvalsFactor = 0 

# 11.4.0 
    * UPDATES:
       * config for new local runs (over)

## 11.3.10
    * UPDATES
       * local koeln results (maans03)

## 11.3.8 
    * UPDATES:
       * local obkresults (maans05)

## 11.3.6
    * UPDATES:
       * rkidata, icudata 25.11.2020
       * local nrwresults (maans04)

## 11.3.4
    * NEWS:
       * *over run configurations

## 11.3.2
   * CHANGES:
      * koelnpara.R and nrwpara.R use the same run config as obkpara (initial designs, funevals, ..)
  * UPDATES:
      * rkidata 24.11.2020

## 11.3.0
   * BUGFIX
      * getError fixed


## 11.2.2
   * UPDATE:
      * new obkpara

## 11.2.0
  * CHANGES:
    * New train/test set splittimg procedure
    * runoptDirect uses new parameter: tryOnTestSet (default: TRUE)

## 11.1.16
  * UPDATES:
     * locally optimized parameter sets koelnpara (tbb)

## 11.1.14
  * UPDATES
     * locally optimized parameter sets (tbb)

## 11.1.12
  * UPDATES:
     * Cleanup results

## 11.1.10
   * UPDATES:
      * rkidata 22.11.2020
      * icudata 22.11.2020

## 11.1.8
   * BUGFIX: 
     * getStartParameter() generates vector, not a list

## 11.1.6
   * CHANGES: Typo fixed
  
## 11.1.4
   * UPDATES:
     * getStartParameter() uses region information (new arg "region")
     * nrwpara, nrwarchive fixed

## 11.1.2
    * UPDATES:
       * current best parameter setting: 
          * obkpara.rda, koelnpara.rda, and nrwpara.rda contain the current best
         known parameter set
       * archived parameter sets:
          * results from all runs in obkarchive, koelnarchive, nrwarchive

## 11.1.0
   * CHANGES:
     * nrwpara.R
        * funevals = 2000 (before: 1000)
        * size = 500 (before: 300)
        * simrepeats = 5 (before: 10)

## 11.0.64 
   * CHANGES: 
      * Organize results

## 11.0.62
   * NEW:
      * results dir added

## 11.0.60
   * NEW:
      * First draft of analysis script

## 11.0.58
   * CHANGES:
      * *para.R files use "_" instead of "-" as separator
      * archive folder

## 11.0.56
   * NEW:
      * testing archive

## 11.0.54
   * CHANGES:
      * obkpara.R uses 1000 repeats (complete opt. runs)

## 11.0.52
  * CHANGES:
     * meta runs uses icuWeights = c(1,1)

## 11.0.50
  * CHANGES: testRepeats = 10

## 11.0.48:
   * UPDATES: 
       * nrwpara 20.11.20 (17 obs) with y min = 912.14

## 11.0.46:
   * UPDATES:
      * koelnpara with icuWeights (1,1) (73 obs) with min y = 12.40

## 11.0.44
   * UPDATES:
      * nrwpara 20.11.20 (28 obs) with y min = 912.14

## 11.0.42:
   * UPDATES:
      * koelnpara 20.11.20 (100 obs) with min y = 71.14

## 11.0.40
    * CHANGES:
       * koelnpara.R uses: icuWeights = c(1,1)  

## 10.0.38:
   * UPDATES:
      * koelnpara 20.11.20 (100 obs)
      
## 11.0.36
    * CHANGES:
       * obkpara.R uses: icuWeights = c(1,1)  

## 11.0.34
    * CHANGES:
       * nrwpara.R uses: icuWeights = c(1,1)  
       
## 11.0.32
    * UPDATES:
       * koelnpara 20.11.20 (42 obs.)

## 11.0.28
    * UPDATES:
       * nrwpara 20.11.20 

## 11.0.26:
   * UPDATES:
      * koelnpara 20.11.20 (28 obs.)

## 11.0.24:
   * NEWS:
      * koelnparameta.R
      * obkpara.R
      
## 11.0.22:
   * CHANGES:
      * Slightly increased bounds (constraints)
      * Risk male = 1.5 (from 2)

## 11.0.20
   * UPDATES:
      * koelnpara (version 11 format, 27 variables, 100 observations)

## 11.0.18:
   * CHANGES:
       * Training and testing  times modiefied (extended warm-up periods)
       * Prob./duration graphs updated

## 11.0.16
   * UPDATES:
      * nrwpara (10 observations)

## 11.0.14
   * UPDATES:
      * koelnpara (version 11 format, 27 variables, 67 observations)

## 11.0.12
   * UPDATES:
      * koelnpara (version 11 format, 27 variables, 47 observations)

## 11.0.10
   * BUGFIX:
     * preloadFunctions.R handles parameterfiles with flexible dimensions 

## 11.0.8
   * BUGFIX:
      * ggVisualizeRkiAge: Day <- value <- variable <- NULL

## 11.0.6:
   * BUGFIX:
      * app_server.R handles parameterfiles with flexible dimensions 

## 11.0.4 
   * UPDATES:
      * koelnpara (version 11 foramt, 27 variables)

## 11.0.2
   * UPDATES:
      * obkpara (version 11 format, 27 variables)

## 11.0.0
   * CHANGES:
      * Transitions removed: Intensive -> Healthy, Ventilation -> Aftercare, and
      IntensiveAftercare -> Healthy
      * This affects variables: x9, x25, x10, x28, and x14
      * The number of states remains the same

## 10.6.6
   * UPDATE:
      * nrwpara 18.11.2020

## 10.6.4
   * UPDATE:
      * obkpara 18.11.2020

## 10.6.2
   * BUGFIX:
      * icudata, icudataFull: double entries removed using unique

## 10.6.0
   * UPDATES:
     * New parameter bounds (2nd Update)

## 10.5.2 
   * UPDATES:
      * nrwpara 18.11.2020

## 10.5.0
   * CHANGES:
     * Modified parameter bounds (stricter)
   *  NEWS:
     * Old bound values can be accessed via getBoundsOld()

## 10.4.8
   * UPDATES:
      * deutschlandpara from 15.11.2020 run
      * nrwpara from 15.11.2020 run
      * koelnpara from 15.11.2020 run
      * obkpara from 15.11.2020 run

## 10.4.6
  * CHANGES:
     * Optimization runs use no overlap and shorter warm-up period (2 weeks)

## 10.4.4
  * UPDATES:
     * rkidata, icudata from 15.11.2020

## 10.4.2
  * UPDATES:
     * obk, koeln, nrw: first results from 15.11.2020

## 10.4.0
  * CHANGES:
     * new upper bound AmntDaysInfectedToHospital: 14 instead of 21 days

## 10.3.6
   * BUGFIX:
     * Correct End Days in runoptDirect()

## 10.3.4
   * BUGFIX:
     * Correct Starting Days in runoptDirect()

## 10.3.2
   * BUGFIX:
     * Correct order in getRkiData()

## 10.3.0
   * BUGFIX:
      * Perform data correction after the one case per row only correction in getRkiData()

## 10.2.22
   * koelnpara, nrwpara, deutschlandpara results

## 10.2.20
   * obkpara.R uses 400 funevals

## 10.2.18
  * obkpara

## 10.2.16
  * visualizeGraph() imports plot.igraph from igraph

## 10.2.14
  * BUGFIX:
     * rkidata manually fixed (should be done automatically, see [issue 37](http://owos.gm.fh-koeln.de:8055/bartz/babsim.hospital/-/issues/37))

## 10.2.12
   * CHANGES
      *  testRepeats accepts new parameter: testRepeats. Default value = 3

## 10.2.10
  * UPDATES:
     * new obkpara.R, koelnpara.R, nrwpara.R and deutschlandpara.R settings

## 10.2.8
  * BUGFIX:
     * runoptDirect:
        * date handling fixed

## 10.2.6
  * BUGFIX:
     * updateRkidataFile() uses Meldedatum to select rkidata:  
     rkidata <- rkidataFull[which(rkidataFull$Meldedatum >= as.Date("2020-09-01")), ]
     * getRkiData() checks if (length(date_range[!date_range %in% rki$Refdatum]) >0)
  * CHANGES:
    * run configs (dates) in obk, koeln, nrw, deutschland modified (dates mut be later than Sep 1)


## 10.2.4
  * CHANGES:
     * print/verbosity restructured
     * The following functions require an explicit specification 
       of their parameters:
        * getRegionRki(): region


## babsimHospital 10.2.2
   * CHANGES:
      * runopt() replaced by runoptdirect()

## babsimHospital 10.2.0
   * CHANGES:
      * The following functions require an explicit specification of the 
        conf parameter (no default is used to prevent errors): 
          * getConfFromData() does not provide an default settings
          * getDailyMaxResults() 
          * babsimHospital()


## babsim.hospital 10.1.0.6
   * CHANGES:
      * getParameterDataFrame() returns min max values

## babsim.hospital 10.1.0.4
   * CHANGES:
      * reorganization of file contents:
         * new files babsimPara.R and babsimConf.R for parameter (optmization)
         and configuration
      * new version numbering   

## babsim.hospital 10.11.13.2
  * UPDATES:
     * obkpara from obk-2020-Nov.13-01.23-V10.11.12.10.RData

## babsim.hospital 10.11.13.0
  * CHANGES:
     * getError() does not use a default conf
     * babsimToolsConf() implements verbosity (default: 0)
     * runoptDirect() uses verbosity

## babsim.hospital 10.11.12.10
  * CHANGES:
     * babsimHospital() detects OS (Win, Linux, Mac) and turns of
     parallel processing for Win systems

## babsim.hospital 10.11.12.8
  * UPDATES:
     * obkpara
     * koelnpara
     * nrwpara
     * deutschlandpara
     
## babsim.hospital 10.11.12.6
  * UPDATES:
     * obkpara
     * koelnpara
     * nrwpara

## babsim.hospital 10.11.12.4
  * BUGIXES:
     * icudata: double entries manually removed (develop branch)

## babsim.hospital 10.11.12.2
  * UPDATES:
     * icudata (DIVI) 12.11.2020
     * rkidata 12.11.2020

## babsim.hospital 10.11.12.0
  * UPDATE:
     * cleanup interfaces and default parameters
  * NEW:
     * babsimFun.R collects optimization functions

## babsim.hospital 10.11.11.0
  * NEW:
     * direct option in SPOT used

## babsim.hospital 10.11.10.2
  * UPDATES:
    * obkpara 20201109PM
    * nrwpara 20201109PM

## babsim.hospital 10.11.10.0
  * UPDATES:
     * icudata (DIVI) 9.11.2020
     * rkidata 10.11.2020
     
## babsim.hospital 10.11.9.0
  * UPDATES:
    * obkpara 20201108PM
  * NEWS:
    * runoptLasso(): Optimization using Lasso instead of Kriging (experimental)

## babsim.hospital 10.11.8.2
  * UPDATES:
     * icudata (DIVI) 8.11.2020
     * rkidata 8.11.2020
  
## babsim.hospital 10.11.8.0
  * CHANGES:
     * data download and handling for icudata and rkidata reorganized:
         * rkidata and rkidataFull
         * icudata and icudataFull
     
## babsim.hospital 10.11.7.0
  * CHANGES:
     * modelOptimizeHospital() removed
     * visualizeGraph() uses checkSimPara()
  * UPDATES:
     * icudata (DIVI) 7.11.2020

## babsim.hospital 10.11.6.12
 * UPDATES:
    * parameter sets updated (optimization results from 5. Nov. 2020):
       * nrwpara
       * koelnpara
       * obkpara

## babsim.hospital 10.11.6.10
 * UPDATES:
    * icudata DIVI 6.11.2020

## babsim.hospital 10.11.6.8
   * CHANGES:
      * simmerplot mv to man/figures

## babsim.hospital 10.11.6.6
   * CHANGES:
      * simmerplot mv to man/figures

## babsim.hospital 10.11.6.4
   * CHANGES:
      * vignette PDF compacted via 
      tools::compactPDF("vignettes", gs_quality = "ebook")

## babsim.hospital 10.11.6.2
   * CHANGES:
      * cleanup: 
         * vignettes renamed
         * subfolder ForTheExpert
         * INTERNAL_DATA.md included in bundeslanddata.R
         * INTERNAL_DATA.md removed
         * README.Rmd: fig.path = "man/figures/README-" changed to fig.path = ".",
   
## babsim.hospital 10.11.6.0
   * CHANGES:
      * rkidata (switched Refdatum <-> Meldedatum) 6.11.2020

## babsim.hospital 10.11.5.8
   * CHANGES:
      * sysdata.rda removed, were replaced by bundeslanddata.rda

## babsim.hospital 10.11.5.6
   * UPDATE:
      * icudata 5.11.2020

## babsim.hospital 10.11.5.4
   * UPDATE:
      * results koelnpara 2020.11.4 PM 
      
## babsim.hospital 10.11.5.2
   * NEW:
      * runopt() accepts argument funEvalsFactor with default value 0

## babsim.hospital 10.11.5.0
   * CHANGES:
      * rkidata (switched Refdatum <-> Meldedatum) 5.11.2020
      * switchRkiRefdatumToMeldedatum updated

## babsim.hospital 10.11.4.18
   * NEW:
      * getBestParameter()
      * smoothParameter()

## babsim.hospital 10.11.4.16
   * deutschlandpara 20201103PM

## babsim.hospital 10.11.4.14
   * runopt() uses  
      * retries=100
      * designControl does not use inequalityConstraint=g anymore

## babsim.hospital 10.11.4.12
   * CHANGES:
   * ggVisualizeIcu()  considers theat faelle_covid_aktuell include faelle_covid_aktuell_beatmet, so 
     we substract faelle_covid_aktuell_beatmet
   
## babsim.hospital 10.11.4.10
 * CHANGES:
   * koelnpara 20201103PM2
   * nrwpara 20201103PM2
   * deutschlandpara 20201103PM
 
## babsim.hospital 10.11.4.8
 * CHANGES:
     * obkpara 20201104AM-5 
     * icudata DIVI 4.11.2020

## babsim.hospital 10.11.4.6
 * CHANGES:
     * rkidata uses Meldedatum

## babsim.hospital 10.11.4.4
    * CHANGES:
     * rkidata as provided by RKI from 4.11.2020

## babsim.hospital 10.11.4.2
   * BUG (partially fixed):
      * TestSimStartDate = "2020-09-20" does not work for OBK data. Use
    TestSimStartDate = "2020-09-21" instead. Possible reason: not suffient data in obkdata?
   
## babsim.hospital 10.11.4.0
  * NEWS:
     * runopt() accepts Overlap (= 7)

## babsim.hospital 10.11.03.14
  * NEWS:
     * runopt() accepts TestFieldStartDate = "2020-10-20" and  TestSimStartDate = "2020-09-20"

## babsim.hospital 10.11.03.12
  * CHANGES:
     * examples reduced (dontrun)
     
## babsim.hospital 10.11.03.10
  * CHANGES:
     * runopt() stores results in subfolder results

## babsim.hospital 10.11.03.8
  * CHANGES:
     * runopt() stops if dates are incorrect
  
## babsim.hospital 10.11.03.6
  * CHANGES:
     * icudata (DIVI) Stand 3.11.2020

## babsim.hospital 10.11.03.4
  * CHANGES:
     * rkidata uses Meldedatum
     
## babsim.hospital 10.11.03.2
  * BUGFIX:
     * runopt() uses correct configuration 
  * CHANGES:
     * rkidata as provided by RKI
     * nowcast vignette: eval = FALSE for all snipplets
     
## babsim.hospital 9.11.03.0
  * BUGFIX:
     * runopt() uses train and test data 

## babsim.hospital 9.11.02.2
  * BUGFIX:
     * runopt() uses corrected data 
     
## babsim.hospital 9.11.02.0
  * BUGFIX:
     * runopt() uses full simData and fieldData 

## babsim.hospital 8.11.02.10
  * CHANGES:
     * runoptsynth() accepts data argument
     
## babsim.hospital 8.11.02.08
  * CHANGES:
     * koelnpara20201102AM-7

## babsim.hospital 8.11.02.06
  * CHANGES:
     * getObkData() accepts data argument

## babsim.hospital 8.11.02.04
  * UPDATE:
     * icudata (DIVI) 2.11.2020
     * obkpara 20201102AM-10

## babsim.hospital 8.11.02.02
  * UPDATE:
     * Vignette Nowcast updated
   
## babsim.hospital 8.11.02.00
  * UPDATE:
     * rkidata (switched to Meldedatum) from Nov 2nd 2020
  * NEW:
     * Vignette Nowcast
     * koelnpara run 20201102AM

## babsim.hospital 8.11.01.20
   *CHANGES:
     * extendRki uses median( tail(dataAgg, 7)$Freq)

## babsim.hospital 8.11.01.18
  * UPDATE:
     * obkpara 20201031PM-22

## babsim.hospital 8.11.01.16
  * NEW:
     * ggVisualizeIcu()
     * ggVisualizeRki()
     
## babsim.hospital 8.11.01.14
  * NEW:
     * app has analysis tab

## babsim.hospital 8.11.01.12
  * CHANGES:
     * typos fixed: rkidata uses Meldedatum

## babsim.hospital 8.11.01.10
* NEW:
   * switchRkiRefdatumToMeldedatum()
   * switchRkiMeldedatumToRefdatum()
   * rkidata uses Meldedatum

## babsim.hospital 8.11.01.8
  * CHANGES:
     * icudata (DIVI) 1.11.2020
  
## babsim.hospital 8.11.01.6
  * CHANGES:
     * new plot arrangement for simulations
  
## babsim.hospital 8.11.01.4
  * CHANGES:
    * ggVisualize functions accept option `simplify`. Defaault is true.  

## babsim.hospital 8.11.01.2
  * CHANGES:
    * rkidata

## babsim.hospital 8.11.01.0
   * NEW:
      * App display preview (extended data)

## babsim.hospital 7.10.31.18
   * NEW:
      * function ggVisualizeRkiExtended()

## babsim.hospital 7.10.31.16
  * NEW:
    * function ggVisualizeRkiAge()

## babsim.hospital 7.10.31.14
  * CHANGES: 
     * deutschlandpara AM 20201031A-8
  
## babsim.hospital 7.10.31.12
  * NEW:
    * preparing new function ggVisualizeRkiAge()
    
## babsim.hospital 7.10.31.10
  * CHANGES:
     * nrwpara20201031A-8
   
## babsim.hospital 7.10.31.8
  * CHANGES:
     * bundeslanddata (replacement for sysdata.rda)
        * bundeslanddata uses ASCII characters only
     
## babsim.hospital 7.10.31.6
  * CHANGES:
     * icudata 20201031 update
     * obkpara 20201031-17 update
     * rkiToBabsimData() handles age and gender

## babsim.hospital 7.10.31.4
  * CHANGES:
     * under preparation: ggVisualizeRkiEvents()

## babsim.hospital 7.10.31.2
  * CHANGES:
     * nrwpara 20201030-26

## babsim.hospital 7.10.31.0
  * CHANGES:
    * obkpara20201031-23
    * rkidata

## babsim.hospital 7.10.30.2
  * CHANGES:
    * obkpara 30.10.2020

## babsim.hospital 7.10.30.0
  * CHANGES:
     * new bounds based on info from OBK 

## babsim.hospital 6.10.30.2
  * CHANGES:
     * obkpara20201030-16 

## babsim.hospital 6.10.30.0
  * CHANGES: 
     * icudata 30.10.2020
     * rkidata 30.10.2020
     * compacted ‘babsim-vignette-RKI-DIVI.pdf’ via 
       tools::compactPDF(gs_quality = "ebook", 
             paths = "/Users/bartz/workspace/babsim.hospital/vignettes")
  * BUGFIX:
     * getError() fixed and extended


## babsim.hospital 5.10.29.18
  * CHANGES: 
     * koelnpara202010262 (required by vignette RKI/DIVI)

## babsim.hospital 5.10.29.16
  * CHANGES: 
     * koelnpara20201029-19

## babsim.hospital 5.10.29.14
  * CHANGES: 
     * koelnpara20201029-13

## babsim.hospital 5.10.29.12
  * CHANGES:
     * obkdata20201028-122
     
## babsim.hospital 5.10.29.10
  * CHANGES:
     * obkpara20201028-118
     * icudata 29.20.2020

## babsim.hospital 5.10.29.8
  * CHANGES:
     * deutschlandpara and 
     * nrwpara combined TBB+FR
     
## babsim.hospital 5.10.29.6
  * CHANGES:
     * deutschlandpara20201028-113

## babsim.hospital 5.10.29.4
  * CHANGES:
     * nrwpara20201028-210

## babsim.hospital 5.10.29.2
  * CHANGES: 
     * obkdata20201028-117
     * koelnpara20201028-114

## babsim.hospital 5.10.29.0
  * CHANGES: 
     * rkidata 29.10.2020
     
## babsim.hospital 5.10.28.26
  * CHANGES: 
     * deutschlandpara20201028-16
     
## babsim.hospital 5.10.28.24
  * CHANGES: 
     * vignette updated
  
## babsim.hospital 5.10.28.22
  * CHANGES: 
     * README.Rmd updated

## babsim.hospital 5.10.28.20
  * CHANGES: 
     * app() does not show Bochum

## babsim.hospital 5.10.28.18
  * CHANGES: 
     * nrwpara20201028-21

## babsim.hospital 5.10.28.16
  * CHANGES: 
     * extendRkiData accepts R0 vector (start, end)
     * app implements R0 ranges, e.g., value = c(1.4, 1.5),

## babsim.hospital 5.10.28.14
  * CHANGES: 
     * deutschlandpara 20201028-14
     
## babsim.hospital 5.10.28.12
  * CHANGES: 
     * visualizeGraph() uses babsim.hospital::synthpara

## babsim.hospital 5.10.28.10
  * CHANGES: 
     * deutschlandpara 20201028-12

## babsim.hospital 5.10.28.8
  * CHANGES: 
     * extendRki() first ideas on how to use a dynamic R0 value (only comments)
     * app() shows Germany

## babsim.hospital 5.10.28.6
  * CHANGES: 
     * deutschlandpara 20201027 (many short runs)

## babsim.hospital 5.10.28.4
  * CHANGES: 
     * icudata (DIVI) 28.10.2020
     * rkidata 28.10.2020

## babsim.hospital 5.10.28.2
  * CHANGES: 
     * owos version   
    
## babsim.hospital 5.10.28.0
  * CHANGES: 
     * merged owos+bab10 version   

## babsim.hospital 5.10.27.4
  * CHANGES: 
     * vignette updated/extended

## babsim.hospital 5.10.27.2
  * CHANGES: 
     * code cleanup
     * icudata (DIVI) 27.20.2020
     
## babsim.hospital 5.10.27.0
  * CHANGES: 
     * renamed babsim -> babsim.hospital
     * moved to OWOS

## babsim.hospital 4.10.26.22
  * CHANGES: cleanup

## babsim.hospital 4.10.26.20
  * CHANGES: vignette-rki-dvi sources fixed

## babsim.hospital 4.10.26.18
  * NEW: koelnpara202010262

## babsim.hospital 4.10.26.16
  * CHANGES:
    * runopt returns reslist
    
## babsim.hospital 4.10.26.14
  * CHANGES
     * koelnpara 20201026-12

## babsim.hospital 4.10.26.12
  * NEW
     * visualizeRkiEvents() can handle StartDate and Region 

## babsim.hospital 4.10.26.10
  * NEW
     * visualizeRkiEvents() to visualize preprocessed RKI data


## babsim.hospital 4.10.26.8
  * CHANGES: 
     * getError() reverted
     * icuWeight conf$w2 is a vector with default setting:
       w2 = c(1,10)

## babsim.hospital 4.10.26.6
  * CHANGES: getError() was rewritten

## babsim.hospital 4.10.26.4
  * CHANGES: icudata 26.10.2020 

## babsim.hospital 4.10.26.2
  * CHANGES: rkidata 26.10.2020 

## babsim.hospital 4.10.26.0
  * CHANGES: koelnpara  20201025 updated

## babsim.hospital 4.10.25.10
  * CHANGES: tests run faster 

## babsim.hospital 4.10.25.8
  * NEW: updateParaFile() 

## babsim.hospital 4.10.25.6
 * NEW:
  * vignettes added

## babsim.hospital 4.10.25.4
 * NEW:
   * babsim.hospitalconstraints with constraintG()
   * icudata 25.20.2020
   * runopt() modified 

## babsim.hospital 4.10.25.2
 * CHANGES:
   * rkidata 25.10.2020

## babsim.hospital 4.10.24.8
 * CHANGES: icudata 24.10.2020, bochumdata 20201023

## babsim.hospital 4.10.24.6
 * CHANGES:
   * deutschland, koeln, nrw, obk 202010231

## babsim.hospital 4.10.24.4
 * CHANGES:
   * bochum 2nd run

## babsim.hospital 4.10.24.2
 * CHANGES:
   * rkidata 24.10.2020

## babsim.hospital 4.10.24.0
 * NEW:
   * bochum 

## babsim.hospital 4.10.23.0
 * CHANGES:
   * icu
   * rki 

## babsim.hospital 4.10.22.2
 * CHANGES:
     * getBounds():
     upper bound for x1 AmntDaysInfectedToHospital changed from 14 back to 21

## babsim.hospital 4.10.22.0
 * CHANGES:
    * koelnpara
    * nrwpara
    * obkpara
    * rkidata

## babsim.hospital 4.10.21.4
 * CHANGES:
   * rkidata 21.10.2020
   * obkpara20201020
   * nrwpara (1st run)
   * koelnpara (1st run)
   * resulceNames, resourceEval in babsim.hospitalrun
 
## babsim.hospital 4.10.21.0
 * BUGFIX:
    * funOptimizeSim(): Missing conf=conf argument added to  call 
      getError(res=res, conf=conf) 

## babsim.hospital 3.10.20.2
 * CHANGES:
   * icudata updated 2020-10-20
   
## babsim.hospital 3.10.20.0
 * CHANGES:
   * getBounds():
     upper bound for x1 AmntDaysInfectedToHospital changed from 21 to 14
   * English version

## babsim.hospital 2.10.20.2
* CHANGES:
  * nrwpara 20.1.2020

## babsim.hospital 2.10.20.0
* CHANGES:
  * rkidata 20.1.2020
  
## babsim.hospital 2.10.19.8
* CHANGES:
  * icudata 19.10.2020

## babsim.hospital 2.10.19.6
* CHANGES:
  * obkpara 19.10.2020
  * deutschlandpara 19.10.2020
  * rkidata 19.10.2020
  * Impressum
  
## babsim.hospital 2.10.19.4
* CHANGES:
  * rkidata 19.10.2020

## babsim.hospital 2.10.19.2
* CHANGES:
  * koelnpara 18.10.2020

## babsim.hospital 2.10.19.0
* CHANGES:
  * Versionsnummern anhand der Tage
  
## babsim.hospital 2.2.22
* CHANGES:
  * berlinpara, koelnpara, magdeburgpara, obkpara

## babsim.hospital 2.2.20
* CHANGES:
  * nrwpara

## babsim.hospital 2.2.18
* CHANGES:
   * dividata 18.10.2020 hinzugefuegt
   * farben angepasst
   * App aktualisiert

## babsim.hospital 2.2.16
* CHANGES:
   * deutschlandpara update

## babsim.hospital 2.2.14
* CHANGES:
   * nrwdata update

## babsim.hospital 2.2.12
* CHANGES:
   * rkidata update

## babsim.hospital 2.2.10
* CHANGES:
   * koelnpara update
   
## babsim.hospital 2.2.8
* CHANGES:
   * icudata fixed

## babsim.hospital 2.2.6
* CHANGES:
   * Added Berlin and Magdeburg

## babsim.hospital 2.2.4
* CHANGES:
   * Naming 

## babsim.hospital 2.2.2

* CHANGES:
   * New file structure


## babsim.hospital 2.2.0

* CHANGES:
  * getIcuBeds() erzeugt intensiveBed statt bed
  * getError() uses resource <- conf$ResourceEval
  * getDailyMaxResults() loscht nicht mehr die bed Kategorie
  * envToTibble() loscht nicht mehr die bed Kategorie
  
* NEW:
  *  conf$ResourceEval spezifiziert die auszuwertenden Ressourcen, z.B. 
     c("intensiveBed", "intensiveBedVentilation") für DIVI

## babsim.hospital 2.1.8

* CHANGES:
  * MC Plots added

## babsim.hospital 2.1.6

* CHANGES:
  * Titel ergaenzt 

## babsim.hospital 2.1.4

* BUG FIXED
    * dailyMaxResults() and envToTibble() can handle
    empty resources 

## babsim.hospital 2.1.2

* CHANGES
   * Koeln data

## babsim.hospital 2.1.0

* NEW
   * Shiny App
   
* CHANGES
   * ifelse fixed

## babsim.hospital 2.0.6

* CHANGES
   * data bug fixed in extendRki()

## babsim.hospital 2.0.4

* CHANGES
   * envToTibble() uses fieldEvent information

## babsim.hospital 2.0.2

* NEW
   * envToTibble() 
  
* CHANGES
   * Wahrscheinlichkeiten angepasst
   * getDailyMaxResults: bed labels changed
             * no bed, only intensiveBed and intensiveBedVentilation 

## babsim.hospital 1.8.2
  
* CHANGES
   * Fixed S4 class problem

## babsim.hospital 1.8.0

* NEW
  * getMatrixD()
  * plotting results

## babsim.hospital 1.7.8

* CHANGES
  * Data importing and archiving strategy

## babsim.hospital 1.7.6

* CHANGES
  * Consistent data handling via conf

## babsim.hospital 1.7.4

* NEW:
  * icu20201001
  * rki20201001

## babsim.hospital 1.7.2

* CHANGES:
   * babsim.hospitalHospital(): sim.repeats bug fix

## babsim.hospital 1.7.0

* CHANGES:
   * babsim.hospitalHospital(): new parallel / sequential handling 

* NEW:
   * constraint handling (experimental), see runQuickConstraints.R

## babsim.hospital 1.6.4

* CHANGES:
   * risk factor B adapted

## babsim.hospital 1.6.2

* CHANGES:
   * updateMatrixP(): value check for risk multiplier 
   
## babsim.hospital 1.6.0

* CHANGES:
   * train/test data
   * weighted error
   * test script runICU100TrainTest.R

## babsim.hospital 1.5.6

* CHANGES:
   * parallel option works again

## babsim.hospital 1.5.4

* CHANGES
   * otimization interface can be used with SPOT

## babsim.hospital 1.5.2

* NEW
   * risk attributes are used to update probabilities


## babsim.hospital 1.5.0

* NEW
   * risk attributes implemented


## babsim.hospital 1.4.2

Merged Versions TBB+FR

## babsim.hospital 1.4.0

Testing, not stable version.

* CHANGES
   * new traj hadling in babsim.hospitalHospital() 
   

## babsim.hospital 1.3.10

* NEW
   * getRkiData() preprocesses raw RKI data
   * getRkiRisk() determines risk for RKI patients based on age and gender


## babsim.hospital 1.3.8

* CHANGES
   * trajList is generated from with babsim.hospitalHospital()


## babsim.hospital 1.3.6

* NEW
   * Parameter extended: risk factors are included

## babsim.hospital 1.3.4

* NEW
   * Preparing risk calculations for RKI data


## babsim.hospital 1.3.2

* CHANGES
   * getRealBeds() uses resource=c("bed", "intensiveBed", "intensiveBedVentilation")
     as default parameter

## babsim.hospital 1.3.0

* NEW:
    * data structures: 
    1. simData
    2. fieldData


## babsim.hospital 1.2.22

* NEW:
    * getConfFromData()

## babsim.hospital 1.2.20

* CHANGES:
    * ICUBeds20200822 geloescht

## babsim.hospital 1.2.18

* NEW:
    * Aktualisierte RKI/DIVI Daten
  

## babsim.hospital 1.2.16

* CHANGES:
  * getDailyMaxResults() uses conf list

## babsim.hospital 1.2.14

* CHANGES:
  * getRealBeds() handles getIcuBeds()

## babsim.hospital 1.2.12

* CHANGES:
   * getError() handles getIcuError()
   * getIcuError() deleted


## babsim.hospital 1.2.10

* CHANGES:
   * getError() simplified
   
   
## babsim.hospital 1.2.8

* CHANGES:
    * getRealBedsOLD() removed
    * simulateHospital() removed, use the following code instead:
         x <- rep(0.2,30)
         para <- mapXToPara(x)
         conf <-  babsim.hospitalToolsConf()
         res <- modelResultHospital(para=para, conf=conf)
         getError(res)
         p <- plotDailyMaxResults(res)
         print(p)



## babsim.hospital 1.2.6

* NEWS:
   * getBounds()
   * Sim100 data 

* CHANGES
   * Default values of x15 and x19 modified
   * Default values of DurationInfected2Hospital (shift = 1) changed

## babsim.hospital 1.2.4

* NEWS:
   * mapXToPara (replaces simulateHospital)
   * synthSimCovid20201130: synthetic data
   * synthBeds20201130: synthetic data
   
* CHANGES:
   * babsim.hospitalToolsConf implements
       * simulationData = babsim.hospital::dataCovidBeds20200624, 
       * fieldData = babsim.hospital::GABeds220200624
       
## babsim.hospital 1.2.3*

* CHANGES: 
   * babsim.hospitalToolsConf has the following additional list entries:
         * logLevel (int) 0 = no logging, 1 = logging. Default: 0.
         * maxCapacity (num) Maximum capacity used for babsim.hospitalHospital resources. Default: 1e6.
         * dataset (chr) Can be "GA" or "ICU". Default: "GA".
         * infectionDates List with the following entries: 
            * StartDate (chr) Start date of the infection data, first day. Default: "2020-03-03"
            * EndDate (chr) End date of the infection data, last day. Default: "2020-06-24"
         * bedDates List with the following entries:
            * StartDate (chr) Start date of the bed data, first day. Default: "2020-03-03"
            * EndDate (chr) End date of the bed data, last day. Default: "2020-06-24"
         * predictionDates List with the following entries:
            * StartDate (chr) Start date of the prediction interval, first day. Default: "2020-03-03"
            * EndDate (chr) End date of the prediction interval, last day. Default: "2020-06-24"

## babsim.hospital 1.2.1

* CHANGES:
   * Aktualisierung der Ergebnisdateien der Simulationslaeufe
   * bart20h.Rmd aktualisiert

## babsim.hospital 1.2.0

* CHANGES:
   * Tests fur getError und getIcuError hinzugefuegt


## babsim.hospital 1.1.10

* CHANGES:
   * getDailyMaxResults errors fixed (simStartDay anstelle von StartDay)
   * test.getDailyMaxResults hinzugefuegt fuer GA und ICU

## babsim.hospital 1.1.8

* CHANGES
   * getIcuBeds benötigt kein Start- und EndDate als eigenstaendige Parameter
   * getRealBeds erzeugt time Information, die mit 0 (und nicht, wie vorher, bei 1) anfaengt
   * getIcuBeds erzeugt time Information, die mit 0 (und nicht, wie vorher, bei 1) anfaengt
   * checkSimPara verwendet minDays = 1e-6 statt minDays = 0

* NEWS:
   * test.bed testet ICU Betten
   
* ERRORS:
   * Das R-Skript source("runICU1000NotParallel.R")   (Verzeichnis babsim.hospital/bart20h/Runs.d) stuerzt mit der folgenden Fehlermeldung ab:
   Error: 'patient112184' at 107.42 in [Branch]->Timeout->[Release]:
   missing value (NA or NaN returned)  
   In addition: Warning message: 
   qgamma(u, shape = shape, rate = rate) : NaNs produced           
   
## babsim.hospital 1.1.4

* CHANGES
   * babsim.hospitalHospitalControl renamed to babsim.hospitalHospitalPara
   * babsim.hospitalHospitalPara uses minVal instead of 0.0 to avoid division by zero
   * ICU Betten verwenden Date Format: ICUBeds20200822$date <- as.Date(ICUBeds20200822)
   * rkiTobabsim.hospitalArrivals uses time label: return(data.frame(time = getArrivalTimes( rkiAgg$Freq ))) 
     instead return(data.frame(getArrivalTimes( rkiAgg$Freq )))
   * conf$maxCapacity = 1e6 (increased from 1e4)
   * getArrivalTimes verwendet RNGkind("Wich") 
   * rtgamma  verwendet RNGkind("Wich") 
 
* NEW:
   * test.bed Bettendaten werden getestet
   * test.trajectories hat einen Test fuer riesige Datenmengen (bundesweite COVID-19 Faelle)
   
* ERRORS:
   * Bei grossen Datensaetzen (> 200.000): arrival Times nicht eindeutig, z.B.:
     dim(arrivalTimes) ergibt 230187 aber  dim(unique(arrivalTimes)) ergibt 230186.
     Daher : RNGkind("Wich") . 
     sum(duplicated(runif(1e8))) >0, aber RNGkind("Wich")  sum(duplicated(runif(1e8))) == 0.
     Siehe: https://stackoverflow.com/questions/51425656/why-does-runif-have-less-unique-values-than-rnorm


## babsim.hospital 1.1.2

* CHANGES:
   *  New:  arrivalTimes <- c(arrivalTimes, runif(xDaily[i],0,1) + (i -1) ) 
   *  Old:  arrivalTimes <- c(arrivalTimes, runif(xDaily[i],0,1) + i )
   
* ERRORS:
   * source("runGA1000.R") ergibt (packageVersion("SPOT") ‘2.0.62’):
   * Warnmeldungen:
1: In duplicateAndReplicateHandling(xnew, x, lower, upper, control) :
  SPOT created a duplicated solution. This can be due to early convergence or bad configuration. Duplicate is replaced by random solution.
2: In duplicateAndReplicateHandling(xnew, x, lower, upper, control) :
  SPOT created a duplicated solution. This can be due to early convergence or bad configuration. Duplicate is replaced by random solution.
3: In duplicateAndReplicateHandling(xnew, x, lower, upper, control) :
  SPOT created a duplicated solution. This can be due to early convergence or bad configuration. Duplicate is replaced by random solution.
4: In duplicateAndReplicateHandling(xnew, x, lower, upper, control) :
  SPOT created a duplicated solution. This can be due to early convergence or bad configuration. Duplicate is replaced by random solution.

* source("runICU1000.R") ergibt:
  * Error in UseMethod("get_mon_resources", unlist(list(.envs))[[1]]) : 
  no applicable method for 'get_mon_resources' applied to an object of class "character"
In addition: Warning message:
In mclapply(1:simRepeats, function(i) { :
  scheduled core 1 encountered error in user code, all values of the job will be affected


## babsim.hospital 1.1.0

* CHANGES:
   * babsim.hospitalHospitalTrajectories() benutzt getDecision() 


## babsim.hospital 1.0.46 

* COMMENT:
   * ICU data does crash

* CHANGES:
   * getDailyMaxResults() clean up

* NEW:
   * getDecision() to prepare babsim.hospital 1.1.0
   * more test functions

## babsim.hospital 1.0.44

* NEW:
   * checkSimPara()
   * ensureRangeOpen()
   * weitere Tests in test.tools

* CHANGES:
   * maxCapacity wird in conf und nicht mehr in para gespeichert
   * modelResultHospital korrigiert para Werte mittels checkSimPara


## babsim.hospital 1.0.42

* NEW:
   * data GABeds20200624
   * data dataICUBeds20200821
   * function getRealBedsOLD (compatibility)
   * test rtgamma

*CHANGES
   * function getIcuBeds simplified
   * function getRealBeds simplified



## babsim.hospital 1.0.40

* CHANGES:
   * modelResultHospital uses rkiTobabsim.hospitalArrivals

* NEW
   * function rkiTobabsim.hospitalArrivals

## babsim.hospital 1.0.38

* CHANGES:
   * rtgamma implements a shift 8Translation) parameter and alpha upper quantile 
   of gamma distribution. All values above alpha are truncated.

## babsim.hospital 1.0.36

* NEW:
   * babsim.hospitalToolsConf to configure conf

* CHANGES:
    * babsim.hospitalHospitalPara list speichert GammaShapeParameter
    * funOptimizeSim erzeugt para mit GammaShapeParameter
    * Durchgaengig conf und para 


## babsim.hospital 1.0.34

* NEWS:
   * resRki data added
   * Abstract vorbereitet
   * babsim.hospital benutzt trunkierte Gamma Verteilung mit 0.95 Grenze
   * experiment runSPOTICU1000TruncDist


## babsim.hospital 1.0.32

* CHANGES: gamma dist

## babsim.hospital 1.0.30

* CHANGES:
   * funWrapOptimize: new parameter
    
## babsim.hospital 1.0.28

* NEW:
   * getIcuBeds()
   * getDailyMaxResults() has new parameter ICU
    

## babsim.hospital 1.0.26

* NEW: 
   * readCsvRecursively()
   * getDiviData()
   * Data: icu20200822 und rki 20200822

## babsim.hospital 1.0.24

* Cleanup:
   * babsim.hospitalHospital does not use argument GammaShapeParameter 
   * babsim.hospitalHospitalTrajectories does not use arguments arrivalTimes and simRepeats
   * babsim.hospitalHospitalTrajectories used truncated Gamma dist. (rtgamma) Max. val = alpha x mean value
   * DurationInfected2Hospital hat eine untere Grenze (min = 2 Tage)
   * Ergebnisse von runSpot500b hinzugefügt

## babsim.hospital 1.0.22

* Grundgerüst des Reports bart20h erstellt.

## babsim.hospital 1.0.20

* Report bart20h bis SPOT Aufruf (Optimierung)
    
## babsim.hospital 1.0.18

* new: postprocessEnvs()

## babsim.hospital 1.0.16:

* new: Dokumentation und Tests funktionieren mit babsim.hospitalHospitalTrajectories()

## babsim.hospital 1.0.14

* new: babsim.hospitalHospitalTrajectories

## babsim.hospital 1.0.12

* Last version before "cleanup"

## babsim.hospital 1.0.10

* New state "healthy" added
* Documentation and new examples

## babsim.hospital 1.0.8

* Only cleanup. Added res250.rda

## babsim.hospital 1.0.7 

* new option parallel in babsim.hospitalHospitalPara

## babsim.hospital 1.0.6

* new data dataCovidBeds20200624

## babsim.hospital 1.0.4

* SPOT optimization as an example

## babsim.hospital 1.0.0:

* Initial version
