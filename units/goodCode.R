# Chris Paciorek
# July 2011
# code for parallel kriging computations
# API v.2 that does the loglik calculation, loglik optimization and prediction use cases by wrapping Tina's code v3.5; differs from v.1 by allowing user-defined covariance functions

# open issues:
# (1) the problem object should probably be changed to an environment so that we can act on it based on pass by reference - otherwise we need to keep outputting the object at each method call - actually this could be done by implementing a replacement function or as a closure (I think we need a closure because not everything we need here may involve replacement?) - see Chambers sec 5.4
# (2) we still need code to deal with memory issues and even this may not be enough if the users starts multiple krigingProblems or uses a large set of test locations
# (3) currently the API uses mpi.send.Robj and mpi.recv.Robj and sends and receives as matrices rather than mpi.isend and mpi.recv - this may be ok for sending the vectors and small matrices sent from the master, but may need to change in the future if we end up sending larger objects; also check on using as.vector
# (4) I'm currently putting predictLocs on all the slaves, but only needed on diagonal processes
# (5) code is clunky in handling mean parameter - 'mu' should be part of problem env't but currently in global env't; also I assume there is a single mean parameter

##############################
# to test, run the following
##############################

if(FALSE){
  # create some fake data
  nx <- ny <- 9
  trainLocs <- expand.grid(seq(0, 1, len = nx), seq(0, 1, len = ny))
  library(fields)
  tmpL <- t(chol(exp(-rdist(trainLocs) / 1)))
  set.seed(0)
  y <- rnorm(nx * ny, 8 + tmpL %*% rnorm(nx * ny), .5)

  # core code for fitting
  library(Rmpi)
  source('api2.R')
  e <- initialize.krigingProblem('e', P = 2, numNodes = 2, exponentialCov, trainLocs, initSlaves = TRUE, apiCodeFile = 'api2.R')
  inits <- c(.5, .25, .1) # initial params: marg var, range, nugget (or marg var, range, nu, nugget for Matern)
  initMu <- 0
  e <- calcLogLik(e, initMu, inits, y)  # example of calculating logLik a single time
  print(e$logLik)
  e <- optimizeLogLik(e, initMu, inits, y, method = 'Nelder-Mead', control = list(maxit = 1000),print = TRUE)

  # fake prediction locations
  set.seed(0)
  predLocs <- cbind(runif(100), runif(100))

  # do the prediction
  preds <- predict(e, y, predLocs)
  stopSlaves(e)
  mpi.quit()
  
}

### end test code

### begin code
 
initialize.krigingProblem <- function(problemName = 'myProblem', P = 2, numNodes = 3, covFxn = exponentialCov, trainLocs, initSlaves = TRUE, spherical = FALSE, apiCodeFile = 'api2.R'){
  #  this sets up (1) the core object on the master node that keeps track of the status of the calculations and the name of the environment in which objects on the slaves are stored and (2) the environment on the slave nodes and the initial information about the problem in that environment

  N <- nrow(trainLocs)
  if(N%%P == 0){
    subN <- N/P
  } else {
    subN <- floor(N/P) + 1
  }

  if(is.function(covFxn))  # need to reference covFxn by its name as a character
    covFxn <- deparse(substitute(covFxn))
  
  # create krigingProblem object
  object <- list(covFxn = covFxn,
                 N = N,
                 subN = subN,
                 numNodes = numNodes,
                 numProc = P*(P+1)/2,
                 P = P,
                 spherical = spherical,
    trainLocs = trainLocs) # include y in object? - probably not as we could apply the same covariance model to multiple datasets
  class(object) <- 'krigingProblem'

  # set up calculation status indicators; we want to keep track of whether the distance matrix exists and whether the covariance and Cholesky are consistent with the current value of the parameters
  object$distMatExists <- FALSE
  object$distMatTrainTestExists <- FALSE
  object$covMatCurrent <- FALSE
  object$covMatTrainTestCurrent <- FALSE
  object$cholCurrent <- FALSE

  object$slaveEnv <- problemName
  if(initSlaves){ # otherwise, assume nodes are already started
    object <- startSlaves(object, apiCodeFile)
  }

  # create environment with the name in the character variable 'problemName' on the slaves; could probably have this be a list rather than an environment
  cmd <- paste(problemName," <- new.env(parent=globalenv())",sep='')
  mpi.bcast.Robj2slave(cmd)
  mpi.bcast.cmd(eval(parse(text = cmd)))

  mpi.bcast.Robj2slaveEnv(object, P)
  mpi.bcast.Robj2slaveEnv(object, N)
  mpi.bcast.Robj2slaveEnv(object, subN)
  mpi.bcast.Robj2slaveEnv(object, covFxn)
  mpi.bcast.Robj2slaveEnv(object, spherical)
    

  # put I and J in environment so slaves know what position each is
  for (i in 1:P){
    for (j in 1:i){
      dest <- ProcRank(i, j, P)
      mpi.send.Robj(i, dest, 1)
    }
  }
  mpi.remote.exec(localPull, object$slaveEnv, 'I', 1, ret = FALSE)

  for (i in 1:P){
    for (j in 1:i){
      dest <- ProcRank(i, j, P)
      mpi.send.Robj(j, dest, 2) 
    }
  }
  mpi.remote.exec(localPull, object$slaveEnv, 'J', 2, ret = FALSE)

  # pushByComponent(object,y,objname = 'y',addToProblemEnv = FALSE, tag = 2)
  pushByComponent(object, trainLocs, objname = 'subTrainLocs1', tag = 3)
  pushByComponent(object, trainLocs, objname = 'subTrainLocs2', breakByRow = FALSE, tag = 4)

  mpi.bcast.cmd(require(fields))
  return(object)
}



