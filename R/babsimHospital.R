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
#'       \item{\code{logLevel}}{log leved (0 or 1). Default: 0 (no output)}. Values larger than 1 are mapped to 1.
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
#' 		\item{\code{xbest}}{Parameters of the best found solution (matrix).}
#' 	}
#'
#' @examples
#'
#' require("simmer")
#' require("dplyr")
#' # Generate simulation data based on number of infected persons per day:
#' x <- dataCovidBeds20200624
#' arrivalTimes <- getArrivalTimes(x$Infected)
#' conf <- babsimToolsConf()
#' y <- babsimHospital(
#'   arrivalTimes = arrivalTimes,
#'   conf = conf,
#'   para = babsimHospitalPara()
#' )
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
#' conf$logLevel <- 1
#' conf$simRepeats <- 1
#' para$GammaShapeParameter <- 0.8
#' y <- babsimHospital(
#'   arrivalTimes = arrivalTimes,
#'   conf = conf,
#'   para = para
#' )
#' @export

babsimHospital <- function(arrivalTimes = NULL,
                           conf = list(),
                           para = list(),
                           ...) {
  RNGkind("Wich")
  # larger log_levels are set to 1:
  conf$logLevel <- min(1, conf$logLevel)
  simRepeats <- conf$simRepeats
  Amnt_Normal_Beds <- conf$maxCapacity
  Amnt_Intensive_Care_Beds <- conf$maxCapacity
  Amnt_Intensive_Care_Beds_Ventilation <- conf$maxCapacity
  GammaShapeParameter <- para$GammaShapeParameter

  ## manage time and risk
  ## if column "risk" exists do nothing, otherwise
  ## create a risk column with a dummy risk = 1
  if (!"risk" %in% colnames(arrivalTimes)) {
    arrivalTimes$risk <- rep(1, length(arrivalTimes$time))
  }

  P <- getMatrixP(para = para)
  calculateAllPMatrices <- function() {
    possibleRisks <- unique(arrivalTimes$risk)
    getSingleMatrix <- function(singleRisk) {
      updateMatrixP(P = P, u = list(k = singleRisk))
    }
    possibleMatrices <- lapply(as.list(possibleRisks), getSingleMatrix)
    names(possibleMatrices) <- round(possibleRisks, 5)
    return(possibleMatrices)
  }
  Ps <- calculateAllPMatrices()

  # print(names(Ps))
  # print(unique(arrivalTimes$risk))

  # if(names(Ps)[[1]] == "1.08042"){
  #    browser()
  # }

  # x1:
  DurationInfected2Hospital <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysInfectedToHospital, shift = 1.0, alpha = 0.95)
  # x2:
  DurationNormal2Healthy <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysNormalToHealthy, alpha = 0.95)
  # x3:
  DurationNormal2Intensive <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysNormalToIntensive, alpha = 0.95)
  # x4:
  DurationNormal2Ventilation <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysNormalToVentilation, alpha = 0.95)
  # x5:
  DurationNormal2Death <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysNormalToDeath, alpha = 0.95)
  # x6:
  DurationIntensive2Aftercare <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysIntensiveToAftercare, alpha = 0.95)
  # x7:
  DurationIntensive2Ventilation <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysIntensiveToVentilation, alpha = 0.95)
  # x8:
  DurationIntensive2Death <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysIntensiveToDeath, alpha = 0.95)
  # x9:
  DurationVentilation2IntensiveAfter <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysVentilationToIntensiveAfter, alpha = 0.95)
  # x10:
  DurationVentilation2Death <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysVentilationToDeath, alpha = 0.95)
  # x11:
  DurationIntensiveAfter2Aftercare <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysIntensiveAfterToAftercare, alpha = 0.95)
  # x12:
  DurationIntensiveAfter2Death <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysIntensiveAfterToDeath, alpha = 0.95)
  # x24:
  DurationAftercare2Healthy <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysAftercareToHealthy, alpha = 0.95)
  # x28 (new in 11.5.0)
  DurationIntensiveAfter2Healthy <- function() rtgamma(n = 1, shape = GammaShapeParameter, rate = 1 / para$AmntDaysIntensiveAfterToHealthy, alpha = 0.95)
  #
  # Removed in v11: DurationIntensive2Healthy <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysIntensiveToHealthy, alpha = 0.95)
  # Removed in v11: DurationVentilation2Aftercare <- function () rtgamma(n = 1, shape = GammaShapeParameter,  rate = 1/para$AmntDaysVentilationToAftercare, alpha = 0.95)

  simFun <- function(i) {
    # first, instantiate the environment
    env <- simmer("Simulation", log_level = conf$logLevel)
    
    transferout <- trajectory("No Hospital") %>%
      log_("transferout", level = 1) %>%
      set_global("No Hospital Required", 1, mod = "+")

    healthy <- trajectory("healthy") %>%
      log_("healthy", level = 1) %>%
      set_global("Healed", 1, mod = "+")

    death <- trajectory("death") %>%
      log_("death", level = 1) %>%
      set_global("Dead", 1, mod = "+")

    aftercare <- trajectory("aftercare") %>%
      log_("aftercare", level = 1) %>%
      ## FactorPatientsAftercareToHealthy == 1, so no branching required
      seize("bed", 1) %>%
      join(
        trajectory() %>%
          timeout(DurationAftercare2Healthy) %>%
          release_all("bed"),
        healthy
      )

    intensiveAfter <- join(
      trajectory("intensiveAfter") %>%
        log_("intensiveAfter", level = 1) %>%
        ## Bed switches to intensive Care2 bed:
        seize("intensiveBed", 1) %>%
        ## Branching what happens to intensive care2 patients
        branch(function() {
          r <- get_attribute(env, "risk")
          R <- Ps[[as.character(round(r, 5))]] # updateMatrixP(P=P, u = list(k=r))
          p <- c(
            R[7, 8],
            R[7, 9],
            R[7, 10]
          )
          getDecision(p)
        },
        continue = FALSE,
        ## 1 death
        join(
          trajectory() %>% timeout(DurationIntensiveAfter2Death) %>% release_all("intensiveBed"),
          death
        ),
        ## Added again in v11.5.0: 2 healthy
        join(
          trajectory() %>% timeout(DurationIntensiveAfter2Healthy) %>% release_all("intensiveBed"),
          healthy
        )
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
          r <- get_attribute(env, "risk")
          R <- Ps[[as.character(round(r, 5))]] # updateMatrixP(P=P, u = list(k=r))
          p <- c(
            R[6, 9],
            R[6, 7]
          )
          getDecision(p)
        },
        continue = FALSE,
        ## 1 IntensiveAfter
        join(
          trajectory() %>% timeout(DurationVentilation2IntensiveAfter) %>% release_all("intensiveBedVentilation"),
          intensiveAfter
        )
        ) %>%
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
            r <- get_attribute(env, "risk")
            R <- Ps[[as.character(round(r, 5))]] # updateMatrixP(P=P, u = list(k=r))
            p <- c(
              R[5, 8],
              R[5, 6],
              R[5, 9]
            )
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
            death
          )
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
          r <- get_attribute(env, "risk")
          R <- Ps[[as.character(round(r, 5))]] # updateMatrixP(P=P, u = list(k=r))
          p <- c(
            R[4, 10],
            R[4, 5],
            R[4, 9],
            R[4, 6]
          )
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
        trajectory() %>% timeout(DurationNormal2Healthy) %>% release_all("bed"),
        healthy
      )


    hospital <- join(
      trajectory("hospital") %>%
        log_("hospital", level = 1) %>%
        branch(
          function() {
            r <- get_attribute(env, "risk")
            R <- Ps[[as.character(round(r, 5))]] # updateMatrixP(P=P, u = list(k=r))
            p <- c(
              R[3, 4],
              R[3, 5],
              R[3, 6]
            )
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
        ## log_(function() as.character(simmer::now(env))) %>%
        ## Newly infected do not immediately get sick
        timeout(DurationInfected2Hospital) %>%
        ## Patient that doesnt need hospital is sent home
        branch(function() {
          r <- get_attribute(env, "risk")
          R <- Ps[[as.character(round(r, 5))]] # updateMatrixP(P=P, u = list(k=r))
          p <- c(
            R[1, 2],
            R[1, 3]
          )
          getDecision(p)
        },
        continue = FALSE,
        # 1:
        hospital
        ),
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
      add_resource(
        "intensiveBedVentilation",
        Amnt_Intensive_Care_Beds_Ventilation
      ) %>%
      run() %>%
      wrap()
  }

  ## Simulate:
  switch(Sys.info()[["sysname"]],
    Windows = {
      messagef("Windows detected. Turning off parallel processing.")
      conf$parallel <- FALSE
    },
    Linux = {
      if (conf$verbosity > 1e3) {
        messagef("Linux detected. Parallel processing possible.")
      }
    },
    Darwin = {
      if (conf$verbosity > 1e3) {
        messagef("Mac detected. Parallel processing possible.")
      }
    }
  )
  if (conf$verbosity > 1e3) {
    messagef("simFun() uses the following %s arrival times:", 
             length(arrivalTimes$time))
    print(arrivalTimes$time)
  } 
  if (conf$parallel) {
    nCores <- detectCores(logical = FALSE)
    mc.coresN = min(nCores - 1, round(conf$percCores * nCores))
    if (conf$verbosity > 1e2) {
      messagef("BEGIN: babsimHospital() calling parallel simFun() with %s cores: ###########################", 
               mc.coresN)
    } 
    envs <- mclapply(1:simRepeats, simFun, mc.cores = getOption("mc.cores", mc.coresN))
  } else {
    if (conf$verbosity > 1e2) {
      messagef("BEGIN: babsimHospital() calling sequential simFun(): ###########################")
    } 
    envs <- lapply(1:simRepeats, simFun)
  }
  if (conf$verbosity > 1e2) {
   printConf(conf)
    messagef("END: babsimHospital(): simFun: ###########################")
  }
  return(envs)
}
