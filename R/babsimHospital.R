#' @title babsimHospital
#' 
#' @description Simulate resource allocation in hospitals. 
#' 
#' @param arrivalTimes Arrival times as generated using \code{getArrivalTimes}. 
#' @param conf list with the following entries:
#'       \describe{
#'       \item{\code{seed}}{seed. Default: 123}
#'       \item{\code{simRepeats}}{simmer repeats}
#'       \item{\code{parallel}}{simmer parallel runs. Default: FALSE}
#'       \item{\code{perCores}}{percentage of cores used for parallel simmer simulations. Default: 0.5 (=50 percent)}
#'       \item{\code{ICU}}{use ICU infection data. Default: FALSE}
#'       \item{\code{logLevel}}{log leved (0 or 1). Default: 0 (no output)}
#'       }
#' @param para List with parameter settings. Can be generated with \code{\link{babsimHospitalPara}}.
#' @param ... additional parameters passed to \code{fun}.
#' 
#' @importFrom stats rgamma
#' @importFrom stats runif
#' @importFrom dplyr %>%
#' @importFrom simmer trajectory
#' @importFrom simmer set_attribute
#' @importFrom simmer get_attribute
#' @importFrom simmer set_global
#' @importFrom simmer release_all
#' @importFrom simmer seize
#' @importFrom simmer timeout
#' @importFrom simmer join
#' @importFrom simmer branch
#' @importFrom simmer release
#' @importFrom simmer simmer
#' @importFrom simmer add_dataframe
#' @importFrom simmer add_resource
#' @importFrom simmer run
#' @importFrom simmer get_mon_resources
#' @importFrom simmer log_
#' @importFrom simmer wrap
#' @importFrom parallel mclapply
#' @importFrom parallel detectCores
#' @importFrom scales brewer_pal
#' 
#' @return This function returns an env list with:
#' \describe{
#'		\item{\code{xbest}}{Parameters of the best found solution (matrix).}
#'	}
#'
#' @examples
#' 
#' require("simmer")
#' require("dplyr")
#' # Generate simulation data based on number of infected persons per day: 
#' x <- dataCovidBeds20200624
#' arrivalTimes <- getArrivalTimes(x$Infected) 
#' conf = babsimToolsConf()
#' y <- babsimHospital(arrivalTimes = arrivalTimes,
#'                    conf = babsimToolsConf(),
#'                    para = babsimHospitalPara())
#' resources <- get_mon_resources(y)
#' # resources <- resources %>% filter(resource != "nurse")
#' mean(resources$server)
#' 
#' ## 2nd example (shows details):
#' # Generate simulation data based on number of infected persons per day: 
#' x <- dataCovidBeds20200624
#' arrivalTimes <- getArrivalTimes(x$Infected) 
#' para <- babsimHospitalPara()
#' conf <- babsimToolsConf()
#' conf$logLevel = 1
#' conf$simRepeats = 1
#' para$GammaShapeParameter = 0.8 
#' y <- babsimHospital(arrivalTimes = arrivalTimes, 
#'                     conf = conf, 
#'                     para = para)
#' 
#' @export

babsimHospital <- function(arrivalTimes = NULL,
                           conf = list(),
                           para = list(),
                           ...) {
  simRepeats = conf$simRepeats
  Amnt_Normal_Beds = conf$maxCapacity
  Amnt_Intensive_Care_Beds = conf$maxCapacity
  Amnt_Intensive_Care_Beds_Ventilation = conf$maxCapacity
  GammaShapeParameter = para$GammaShapeParameter
  
  ## manage time and risk
  ## if column "risk" exists do nothing, otherwise
  ## create a risk column with a dummy risk = 1
  if(!"risk" %in% colnames(arrivalTimes)){
    arrivalTimes$risk = rep(1,length(arrivalTimes$time))
  }
  
  P <- getMatrixP(para = para)
  # x1:
  DurationInfected2Hospital <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysInfectedToHospital, shift = 1.0, alpha = 0.95)
  # x2:
  DurationNormal2Healthy <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysNormalToHealthy, alpha = 0.95)
  # x3:
  DurationNormal2Intensive <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysNormalToIntensive, alpha = 0.95)
  # x4:
  DurationNormal2Ventilation <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysNormalToVentilation, alpha = 0.95)
  # x5:
  DurationNormal2Death <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysNormalToDeath, alpha = 0.95)
  # x6:
  DurationIntensive2Aftercare <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysIntensiveToAftercare, alpha = 0.95)
  # x7:
  DurationIntensive2Ventilation <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysIntensiveToVentilation, alpha = 0.95)
  # x8:
  DurationIntensive2Death <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysIntensiveToDeath, alpha = 0.95)
  # x9:
  DurationVentilation2IntensiveAfter <- function() rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysVentilationToIntensiveAfter, alpha = 0.95)
  # x10:
  DurationVentilation2Death <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysVentilationToDeath, alpha = 0.95)
  # x11:
  DurationIntensiveAfter2Aftercare <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysIntensiveAfterToAftercare, alpha = 0.95)
  # x12:
  DurationIntensiveAfter2Death <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysIntensiveAfterToDeath, alpha = 0.95)
  # x24:
  DurationAftercare2Healthy <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysAftercareToHealthy, alpha = 0.95)
  # x28 (new in 11.5.0)
  DurationIntensiveAfter2Healthy <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysIntensiveAfterToHealthy, alpha = 0.95)
  #
  # Removed in v11: DurationIntensive2Healthy <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysIntensiveToHealthy, alpha = 0.95)
  # Removed in v11: DurationVentilation2Aftercare <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysVentilationToAftercare, alpha = 0.95)
  
  simFun <- function(i){
    # first, instantiate the environment
    env <- simmer("Simulation", log_level = max(0, conf$logLevel-10)) 

    transferout <- trajectory("No Hospital") %>% 
      log_("transferout", level = 1) %>%
      set_global("No Hospital Required", 1, mod="+")

    healthy <- trajectory("healthy") %>% 
      log_("healthy", level = 1) %>%
      set_global("Healed", 1, mod="+")

    death <- trajectory("death") %>% 
      log_("death", level = 1) %>%
      set_global("Dead", 1, mod="+")

    aftercare <- trajectory("aftercare") %>%
      log_("aftercare", level = 1) %>%
      ## FactorPatientsAftercareToHealthy == 1, so no branching required
      seize("bed",1) %>%
      join(
        trajectory() %>%
          timeout(DurationAftercare2Healthy)  %>% 
          release_all("bed"),
        healthy)

    intensiveAfter <- join(
      trajectory("intensiveAfter") %>%
        log_("intensiveAfter", level = 1) %>%
        ## Bed switches to intensive Care2 bed:
        seize("intensiveBed", 1) %>%
        ## Branching what happens to intensive care2 patients
        branch(function() {
          r <- get_attribute(env,"risk")
          R <- updateMatrixP(P=P, u = list(k=r))
          p <- c(R[7,8],
                 R[7,9], 
                 R[7,10] )
          getDecision(p)
        },
        continue = FALSE,
        ## 1 death
        join(
          trajectory() %>% timeout(DurationIntensiveAfter2Death) %>% release_all("intensiveBed"),
          death), 
        ## Added again in v11.5.0: 2 healthy
        join(
           trajectory() %>%  timeout(DurationIntensiveAfter2Healthy) %>% release_all("intensiveBed"),
           healthy)
        ) %>%
        ## 0
        ## Remaining Patients go to aftercare
        timeout(DurationIntensiveAfter2Aftercare) %>% release_all("intensiveBed"),
      aftercare
    )

    ventilation <- join(
      trajectory("Intensive Care Ventilation") %>%
        log_("ventilation", level = 1) %>%
        seize("intensiveBedVentilation", 1) %>%
        branch(function() {
          r <- get_attribute(env,"risk")
          R <- updateMatrixP(P=P, u = list(k=r))
          p <- c(R[6,9] ,
                 R[6,7] )
          getDecision(p)
        },
        continue = FALSE,
        ## 1 IntensiveAfter
        join(
          trajectory() %>% timeout(DurationVentilation2IntensiveAfter)%>% release_all("intensiveBedVentilation"),
          intensiveAfter
        )) %>%
        ## New v11: Death
        timeout(DurationVentilation2Death) %>% release_all("intensiveBedVentilation"),
      death
    )
  
    intensive <- join(
      trajectory("Intensive Care") %>%
        log_("intensive", level = 1) %>%
        ## Bed switches to intensive Care bed:
        seize("intensiveBed", 1) %>%
        ## Branching what happens to intensive care patients
        branch(
          function() {
            r <- get_attribute(env,"risk")
            R <- updateMatrixP(P=P, u = list(k=r))
            p <- c(R[5,8] ,
                   R[5,6] ,
                   R[5,9] )
            getDecision(p)
          },
          continue = FALSE,
          ## 1 ventilation
          join(
            trajectory() %>% timeout(DurationIntensive2Ventilation) %>% release_all("intensiveBed"),
            ventilation
          ),
          ## 2 death
          join(
            trajectory() %>% timeout(DurationIntensive2Death) %>% release_all("intensiveBed"),
            death)
        ) %>%
        ## Remaining Patients go to aftercare
        ## 0
        timeout(DurationIntensive2Aftercare) %>% release_all("intensiveBed"),
      aftercare
    )
  
    normalStation <- trajectory("Normal Station") %>%
      ## If patient stays in hospital, resources are required:
      log_("normalStation", level = 1) %>%
      seize("bed") %>%
      branch(
        function() {
          r <- get_attribute(env,"risk")
          R <- updateMatrixP(P=P, u = list(k=r))
          p <- c(R[4,10] ,
                 R[4,5] ,
                 R[4,9] ,
                 R[4,6])
          getDecision(p)
        },
        continue = FALSE,
        # 1 intensive
        join(
          trajectory() %>% timeout(DurationNormal2Intensive) %>% release_all("bed"),
          intensive
        ),
        # 2 death
        join(
          trajectory() %>% timeout(DurationNormal2Death) %>% release_all("bed"),
          death
        ),
        # 3 ventilation
        join(
          trajectory() %>% timeout(DurationNormal2Ventilation) %>% release_all("bed"),
          ventilation
        )
      ) %>%
      ## Patients which dont go to Intensive Care stay a few more days, then go home
      # 0 Healthy
      join(
        trajectory() %>%  timeout(DurationNormal2Healthy) %>% release_all("bed"),
        healthy)
    
   
    hospital <- join(
      trajectory("hospital") %>%
        log_("hospital", level = 1) %>%
        branch(
          function() {
            r <- get_attribute(env,"risk")
            R <- updateMatrixP(P=P, u = list(k=r))
            p <- c(R[3,4] ,
                   R[3,5] ,
                   R[3,6] )
            getDecision(p)
          },
          continue = FALSE,
          # 1
          intensive,
          # 2
          ventilation
        ),
      # 0
      normalStation
    )
    
   
    infected <- join(
      trajectory("New Infected") %>%
        log_("infected", level = 1) %>% 
        ##log_(function() as.character(simmer::now(env))) %>%
        ## Newly infected do not immediately get sick
        timeout(DurationInfected2Hospital) %>%
        ## Patient that doesnt need hospital is sent home
        branch(function() {
          r <- get_attribute(env,"risk")
          R <- updateMatrixP(P=P, u = list(k=r))
          p <- c(R[1,2] ,
                 R[1,3] )
          getDecision(p)
        },
        continue = FALSE, 
        # 1:
        hospital),
      # 0:
      transferout
    )

    add_dataframe(
      env,
      name_prefix = "patient",
      trajectory = infected,
      data = arrivalTimes,
      mon = 1,
      col_time = "time",
      time = "absolute",
      col_attributes = c("risk")
    )
    
    env %>%
      add_resource("bed", Amnt_Normal_Beds) %>%
      add_resource("intensiveBed", Amnt_Intensive_Care_Beds) %>%
      add_resource("intensiveBedVentilation",
                   Amnt_Intensive_Care_Beds_Ventilation) %>%
      run() %>%
      wrap()
  }
  
## Simulate:
  switch(Sys.info()[['sysname']],
         Windows= {print("Windows detected. Turning off parallel processing.")
           conf$parallel <- FALSE},
         Linux  = {if (conf$verbosity > 1000){
           print("Linux detected. Parallel processing possible.")}
           },
         Darwin = {if (conf$verbosity > 1000){
           print("Mac detected. Parallel processing possible.")}
           })
  
  if (conf$verbosity > 1000){
    print("BEGIN: babsimHospital(): simFun calling simFun: ###########################")
    printConf(conf)
    print("END: babsimHospital(): simFun: ###########################")
  }
  if (conf$parallel) {
    nCores <- detectCores(logical = FALSE)
    envs <- mclapply(1:simRepeats, simFun, mc.cores = getOption("mc.cores", min(
      nCores - 1, round(conf$percCores * nCores)
    )))
  } else {
    envs <- lapply(1:simRepeats, simFun)
  }
  return(envs)
}


#' @title babsimHospitalPara 
#' 
#' @description Default Control list for babsimHospital 
#' This function returns the default controls for the functions \code{\link{babsimHospital}}.
#' Control is a list of the following settings.
#' Note: dependent parameters that are based on other parameters (e.g., probabilities that add to 1.0) are marked with an asterisk (*).
#' Note: parameters that are currently not used, are marked with a double asterisk (**).
#' \describe{
#'   \item{\code{logLevel}}{if larger than 10, shown detailed simmer output. simmer \code{log_} level, default is \code{0}.}
#' 1
#'   \item{\code{FactorPatientsInfectedToHealthy*}}{Z1: Infected -> Healthy: percentage of patients that move from state infected to healthy, default is \code{0.831}. 
#'   Note: not used. Value is internally calculated as: \code{1 - FactorPatientsInfectedToHospital}}
#'   \item{\code{AmntDaysInfectedToHealthy**}}{Z1: Infected -> Healthy: duration (in days) if patients move from state infected to healthy, default is \code{20.5}. 
#'   Note: not used, because not modeled.}
#' 2   
#'   \item{\code{FactorPatientsInfectedToHospital}}{Z2: Infected -> Hospital: percentage of patients that move from state infected to hospital, default is \code{0.169}.}
#'   \item{\code{AmntDaysInfectedToHospital}}{Z2: Infected -> Hospital: duration (in days) if patients move from state infected to hospital, default is \code{8.4}.}
#'   
#' 3   
#'   \item{\code{FactorPatientsHospitalToNormal*}}{Z3: Hospital -> Normal: percentage of patients that move from state hospital to normal, default is \code{0}. 
#'   Note: not used. Value is internally calculated as: \code{1 - FactorPatientsHospitalToIntensive - FactorPatientsHospitalToVentilation} }
#'   \item{\code{AmntDaysHospitalToNormal*}}{Z3: Hospital -> Normal: duration (in days) if patients move from state hospital to normal, default is \code{1e6}. 
#'   Note: not used. Patients move from hospital to normal immediately}
#' 4   
#'   \item{\code{FactorPatientsHospitalToIntensive}}{Z4: Hospital -> Intensive: percentage of patients that move from state hospital to intensive, default is \code{0.012}.}
#'   \item{\code{AmntDaysHospitalToIntensiv*}}{Z4: Hospital -> Intensive: duration (in days) if patients move from state hospital to intensive, default is \code{1e6}. Note: not used.
#'   Patients move from hospital to intensive immediately}
#' 5   
#'   \item{\code{FactorPatientsHospitalToVentilation}}{Z5: Hospital -> Ventilation: percentage of patients that move from state hospital to ventilation, default is \code{0.036}.}
#'   \item{\code{AmntDaysHospitalToVentilation*}}{Z5: Hospital -> Ventilation: duration (in days) if patients move from state hospitel to ventilation, default is \code{1e6}. Note: not used.
#'   Patients move from hospital to intensive immediately}
#' 6   
#'   \item{\code{FactorPatientsNormalToHealthy*}}{Z6: Normal -> Healthy: percentage of patients that move from state normal to healthy, default is \code{0}. Note: not used. 
#'   Value is internally calculated as: \code{1 - FactorPatientsNormalToIntensive - FactorPatientsNormalToVentilation - FactorPatientsNormalToDeath }}
#'   \item{\code{AmntDaysNormalToHealthy}}{Z6: Normal -> Healthy: duration (in days) if patients move from state normal to healthy, default is \code{11.6}.}
#' 7  
#'   \item{\code{FactorPatientsNormalToIntensive}}{Z7: Normal -> Intensive: percentage of patients that move from state normal to intensive, default is \code{0.0506}.}
#'   \item{\code{AmntDaysNormalToIntensive}}{Z7: Normal -> Intensive: duration (in days) if patients move from state normal to intensive, default is \code{1.25}.}
#' 8  
#'   \item{\code{FactorPatientsNormalToVentilation}}{Z8: Normal -> Ventilation: percentage of patients that move from state normal to ventilation, default is \code{0.1013}.}
#'   \item{\code{AmntDaysNormalToVentilation}}{Z8: Normal -> Ventilation: duration (in days) if patients move from state normal to ventilation, default is \code{3.63}.}
#' 9   
#'   \item{\code{FactorPatientsNormalToDeath}}{Z9: Normal -> Death: percentage of patients that move from state normal to death, default is \code{0.139}.}
#'   \item{\code{AmntDaysNormalToDeath}}{Z9: Normal -> Death: duration (in days) if patients move from state normal to death, default is \code{11.4}.}
#' 10   
#'   \item{\code{FactorPatientsIntensiveToAftercare*}}{Z10: Intensive -> Aftercare: percentage of patients that move from state intensive to aftercare, default is \code{0.25}. Note: not used. 
#'   Value is internally calculated as: \code{1 - FactorPatientsIntensiveToVentilation - FactorPatientsIntensiveToDeath - FactorPatientsIntensiveToHealthy}}
#'   \item{\code{AmntDaysIntensiveToAftercare}}{Z10: Intensive -> Aftercare: duration (in days) if patients move from state intensive to aftercare, default is \code{7.0}.}
#' 11   
#'   \item{\code{FactorPatientsIntensiveToVentilation}}{Z11: Intensive > Ventilation: percentage of patients that move from state intensive to ventilation, default is \code{0.25}.}
#'   \item{\code{AmntDaysIntensiveToVentilation}}{Z11: Intensive > Ventilation: duration (in days) if patients move from state intensive to ventilation, default is \code{2.0}.}
#' 12   
#'   \item{\code{FactorPatientsIntensiveToDeath}}{Z12: Intensive -> Death: percentage of patients that move from state intensive to death, default is \code{0.25}.}
#'   \item{\code{AmntDaysIntensiveToDeath}}{Z12: Intensive -> Death: duration (in days) if patients move from state intensive to death, default is \code{2.0}.}
#' 12a   
#'   \item{Removed in v11: \code{FactorPatientsIntensiveToHealthy}}{Z12a: Intensive -> Healthy: percentage of patients that move from state intensive  to healthy, default is \code{0.25}.}
#'   \item{\code{AmntDaysIntensiveToHealthy}}{Z12a: Intensive -> Healthy: duration (in days) if patients move from state intensive to healthy, default is \code{13.0}.}
#' 13  
#'   \item{Removed in v11: \code{FactorPatientsVentilationToAftercare*}}{Z13: Ventilation -> Aftercare: percentage of patients that move from state ventilation to aftercare, default is \code{0.08}. Note: not used. 
#'   Value is internally calculated as: \code{1 - FactorPatientsVentilationToIntensiveAfter - FactorPatientsVentilationToDeath}}
#'   \item{Removed in v11: \code{AmntDaysVentilationToAftercare}}{Z13: Ventilation -> Aftercare: duration (in days) if patients move from state ventilation to aftercare, default is \code{9.0}.}
#' 14  
#'   \item{\code{FactorPatientsVentilationToIntensiveAfter}}{Z14: Ventilation -> IntensiveAfter: percentage of patients that move from state ventilation to intensiveAfter, default is \code{0.42}.}
#'   \item{\code{AmntDaysVentilationToIntensiveAfter}}{Z14: Ventilation -> IntensiveAfter: duration (in days) if patients move from state ventilation to intensiveAfter, default is \code{23.0}.}
#' 15   
#'   \item{Removed in v11: \code{FactorPatientsVentilationToDeath}}{Z15: Ventilation -> Death: percentage of patients that move from state ventilation to death, default is \code{0.5}.}
#'   \item{\code{AmntDaysVentilationToDeath}}{Z15: Ventilation -> Death: duration (in days) if patients move from state  ventilation to death, default is \code{16.0}.}
#' 16  
#'   \item{\code{FactorPatientsAftercareToHealthy*}}{Z16: Aftercare -> Healthy: percentage of patients that move from state aftercare to healthy, default is \code{1.0}. Note: not used.
#'   Value is \code{1}. No branching required, because there is no alternative.}
#'   \item{\code{AmntDaysAftercareToHealthy}}{Z16: Aftercare -> Healthy: duration (in days) if patients move from state aftercare to healthy, default is \code{21.0}.}
#' 17 I  
#'   \item{\code{FactorPatientsIntensiveAfterToAftercare*}}{Z17I: IntensiveAfter -> Aftercare: percentage of patients that move from state intensiveAfter to aftercare, default is \code{0.5}. Note: not used. 
#'   Value is internally calculated as: \code{1 - FactorPatientsIntensiveAfterToHealthy - FactorPatientsIntensiveAfterToDeath - FactorPatientsIntensiveAfterToHealthy}}
#'   \item{\code{AmntDaysIntensiveAfterToAftercare}}{Z17I: IntensiveAfter -> Aftercare: duration (in days) if patients move from state intensiveAfter to aftercare, default is \code{7.0}.}
#' 17 II  
#'   \item{Removed in v11: \code{AmntDaysIntensiveAfterToHealthy}}{Z17II: IntensiveAfter -> Healthy: duration (in days) if patients move from state intensiveAfter to healthy, default is \code{18.0}.}
#' 18  
#'   \item{\code{FactorPatientsIntensiveAfterToDeath}}{IntensiveAfter -> Death: percentage of patients that move from state intensiveAfter to death, default is \code{0.0}.}
#'   \item{\code{AmntDaysIntensiveAfterToDeath}}{IntensiveAfter -> Death: duration (in days) if patients move from state intensiveAfter to death, default is \code{1e-6}.}
#'   \item{\code{GammaShapeParameter}}{Gamma shape parameter, default is \code{1} (exponential distribution).}
#'   \item{\code{RiskFactorA}}{Parameter a in the exponential function r(x) = a exp(b x) that models the risk r as a function of the age x, default is \code{0.02048948}.}
#'   \item{\code{RiskFactorB}}{Parameter b in the exponential function r(x) = a exp(b x) that models the risk r as a function of the age x, default is \code{0.07138200}.}
#'   \item{\code{RiskMale}}{Death risk of male patients compared to female , default is \code{2}.}
#'  \item{\code{AmntDaysIntensiveAfterToHealthy}}{IntensiveAfter -> Healthy: duration (in days) if patients move from state intensiveAfter to death, default is \code{3}.}
#'   \item{\code{FactorPatientsIntensiveAfterToHealthy}}{IntensiveAfter -> Healthy: percentage of patients that move from state intensiveAfter to healthy, default is \code{0.67}.}
#'   }
#' @return a list
#' 
#' @examples 
#' 
#' # change Gamma parameter
#' x <- babsimHospitalPara()
#' x$GammaShapeParameter = 1.0
#' 
#' @export
babsimHospitalPara <- function(){
  minVal <- 1e-6 
  list(
    AmntDaysInfectedToHospital = 9.5, #8.4, # x1
    AmntDaysNormalToHealthy = 10, # 11.6, #x2
    AmntDaysNormalToIntensive = 5,# 1.25, # x3
    AmntDaysNormalToVentilation = 3.63, #x4
    AmntDaysNormalToDeath  = 5, # 11.4, # x5
    AmntDaysIntensiveToAftercare = 7.0, # x6
    AmntDaysIntensiveToVentilation = 4, # 2.0, # x7
    AmntDaysIntensiveToDeath = 5, #6 , # 2.0, # x8
    AmntDaysVentilationToIntensiveAfter = 30, # 23.0, #  x9
    AmntDaysVentilationToDeath = 20, # 16, # x10
    AmntDaysIntensiveAfterToAftercare = 3.0, # x11
    AmntDaysIntensiveAfterToDeath = 4, # 1, # x12
    GammaShapeParameter = 1.0, # x13
    FactorPatientsInfectedToHospital = 0.1, #0.169, # x14
    FactorPatientsHospitalToIntensive = 0.09, #0.04,  #0.012, # x15
    FactorPatientsHospitalToVentilation = 0.01, # 0.036, #x16
    FactorPatientsNormalToIntensive = 0.1, # 0.0506, #x17
    FactorPatientsNormalToVentilation = 0.001, #0.1013, #x18
    FactorPatientsNormalToDeath = 0.1, # 0.139, # x19
    FactorPatientsIntensiveToVentilation = 0.3,# 0.25, # x20
    FactorPatientsIntensiveToDeath = 0.1,# 0.25, # x21
    FactorPatientsVentilationToIntensiveAfter = 0.7,# 0.42, # x22
    FactorPatientsIntensiveAfterToDeath = 1e-5, # x23
    AmntDaysAftercareToHealthy = 3,# 21, # x24
    RiskFactorA = 0.02048948, #x25
    RiskFactorB = 0.01, #26
    RiskMale = 1.5, #x27
    # Added again in v11.5: 
    AmntDaysIntensiveAfterToHealthy = 3.0, # x28 
    # Added in v11.5: 
    FactorPatientsIntensiveAfterToHealthy = 0.67 # x29
    )
}

