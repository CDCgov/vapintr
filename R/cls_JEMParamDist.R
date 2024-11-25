#' @title Define distributions for model input parameters
#'
#' @description An R6 class used to define input parameter distributions for
#'   stochastic simulations. For more information on this class and how to use
#'   it, see `vignette("StochasticSimulations", package = "vapintr")`.
#'
#' @examples
#' #Create an example JEMParamDist object for a parameter with a uniform distribution
#' JEMParamDist$new("Lb",
#'                  name = "Depth below grade to base of foundation",
#'                  units = "m",
#'                  dist_type = "Uniform",
#'                  minimum = 0.1,
#'                  maximum = 0.5)
#'
#' #Create an example JEMParamDist object for a parameter with a truncated normal distribution
#' JEMParamDist$new("ach",
#'                  name = "Indoor air exchange rate",
#'                  units = "1/hr",
#'                  dist_type = "Truncated Normal",
#'                  minimum = 0.15,
#'                  arith_mean = 0.45,
#'                  arith_stdev = 0.1,
#'                  maximum = 1.26)
#'
#'
#' @export

JEMParamDist <- R6::R6Class(
  classname = "JEMParamDist",
  public = list(

    #' @param symbol Symbol used to define the parameter in the Johnson & Ettinger model
    #' @param name Parameter name in the Johnson & Ettinger model or the contaminant name for concentration distributions
    #' @param units Units associated with the distribution
    #' @param dist_type Distribution type. Supported distributions are "Constant", "Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal", and "Custom"
    #' @param constant Value of a constant distribution
    #' @param minimum Distribution minimum
    #' @param mode Distribution mode
    #' @param maximum Distribution maximum
    #' @param arith_mean Distribution arithmetic mean
    #' @param arith_stdev Distribution standard deviation
    #' @param geom_mean Distribution geometric mean
    #' @param geom_stdev Distribution geometric standard deviation
    #' @param template_data_dfx Data frame containing data imported from the vapintr data import template
    #' @param custom_distribution_data Custom distribution data generated outside of the `JEMParamDist` object
    #'
    #' @description Function to create a new `JEMParamDist` object
    initialize = function(symbol, name = NULL, units = NULL, dist_type = NULL, constant = NULL, minimum = NULL, mode = NULL, maximum = NULL, arith_mean = NULL, arith_stdev = NULL, geom_mean = NULL, geom_stdev = NULL, template_data_dfx = NULL, custom_distribution_data = NULL)
    {
      if (is.null(symbol) || is.na(symbol)){
        stop ("Error: no symbol passed to JEMParamDist constructor")
      }

      private$symbol <- symbol
      private$name <- name

      #Assign distribution parameters directly
      if (is.null(template_data_dfx))
      {
        private$units <- units
        private$dist_type <- dist_type
        private$constant <- constant
        private$minimum <- minimum
        private$mode <- mode
        private$maximum <- maximum
        private$arith_mean <- arith_mean
        private$arith_stdev <- arith_stdev
        private$geom_mean <- geom_mean
        private$geom_stdev <- geom_stdev
        private$custom_distribution_data <- custom_distribution_data
      }
      #Assign distribution parameters from template
      else
      {
        #Subset the input data to just those values associated with the symbol
        param_data_dfx <- template_data_dfx[template_data_dfx$JEMSymbol == symbol, ]

        if(nrow(param_data_dfx) == 0) {
          stop("Error: symbol passed to JEMParamDist constructor not found in template data frame")
        }

        private$name <- param_data_dfx$Parameter[1]
        private$units <- param_data_dfx$Units[1]
        private$dist_type <- param_data_dfx$Distribution[1]

        private$constant <- ifelse(any(param_data_dfx$DistributionParameterType == "Constant"), param_data_dfx$DistributionParameterValue[param_data_dfx$DistributionParameterType == "Constant"][1], NA)
        private$minimum <- ifelse(any(param_data_dfx$DistributionParameterType == "Minimum"), param_data_dfx$DistributionParameterValue[param_data_dfx$DistributionParameterType == "Minimum"][1], NA)
        private$mode <- ifelse(any(param_data_dfx$DistributionParameterType == "Mode"), param_data_dfx$DistributionParameterValue[param_data_dfx$DistributionParameterType == "Mode"][1], NA)
        private$maximum <- ifelse(any(param_data_dfx$DistributionParameterType == "Maximum"), param_data_dfx$DistributionParameterValue[param_data_dfx$DistributionParameterType == "Maximum"][1], NA)
        private$arith_mean <- ifelse(any(param_data_dfx$DistributionParameterType == "Mean"), param_data_dfx$DistributionParameterValue[param_data_dfx$DistributionParameterType == "Mean"][1], NA)
        private$arith_stdev <- ifelse(any(param_data_dfx$DistributionParameterType == "Standard Deviation"), param_data_dfx$DistributionParameterValue[param_data_dfx$DistributionParameterType == "Standard Deviation"][1], NA)
        private$geom_mean <- ifelse(any(param_data_dfx$DistributionParameterType == "Geometric Mean"), param_data_dfx$DistributionParameterValue[param_data_dfx$DistributionParameterType == "Geometric Mean"][1], NA)
        private$geom_stdev <- ifelse(any(param_data_dfx$DistributionParameterType == "Geometric Standard Deviation"), param_data_dfx$DistributionParameterValue[param_data_dfx$DistributionParameterType == "Geometric Standard Deviation"][1], NA)
        private$custom_distribution_data <- NA
      }

      private$validate()
    },

    #' @description Function to return the value of the object's private "symbol" attribute
    getSymbol = function(){return(private$symbol)},

    #' @description Function to return a data frame of all the object's private attributes
    getDataFrameOfProperties = function()
    {
      properties_dfx <- data.frame(symbol = if(is.null(private$symbol)) {NA} else {private$symbol},
                               name = if(is.null(private$name)) {NA} else {private$name},
                               units = if(is.null(private$units)) {NA} else {private$units},
                               dist_type = if(is.null(private$dist_type)) {NA} else {private$dist_type},
                               constant = if(is.null(private$constant)) {NA} else {private$constant},
                               minimum = if(is.null(private$minimum)) {NA} else {private$minimum},
                               mode = if(is.null(private$mode)) {NA} else {private$mode},
                               maximum = if(is.null(private$maximum)) {NA} else {private$maximum},
                               arith_mean = if(is.null(private$arith_mean)) {NA} else {private$arith_mean},
                               arith_stdev = if(is.null(private$arith_stdev)) {NA} else {private$arith_stdev},
                               geom_mean = if(is.null(private$geom_mean)) {NA} else {private$geom_mean},
                               geom_stdev = if(is.null(private$geom_stdev)) {NA} else {private$geom_stdev})

      return (properties_dfx)
    },

    #' @description Generate stochastic simulation data for the parameter based
    #'   on the object's assigned distribution and distribution parameter values
    getDistributionData = function()
    {
      if (private$dist_type == "Constant"){
        return(mc2d::mcdata(data = private$constant, type = "U"))
      } else if (private$dist_type == "Uniform") {
        return(mc2d::mcstoc(type = "U", min = private$minimum, max = private$maximum))
      } else if (private$dist_type == "Triangular") {
        return(mc2d::mcstoc(func = rtriang, type = "U", min = private$minimum, mode = private$mode, max = private$maximum))
      } else if (private$dist_type == "PERT") {
        return(mc2d::mcstoc(func = rpert, type = "U", min = private$minimum, mode = private$mode, max = private$maximum))
      } else if (private$dist_type == "Truncated Normal") {
        return(mc2d::mcstoc(func = rnorm, type = "U", mean = private$arith_mean, sd = private$arith_stdev, rtrunc = TRUE, linf = private$minimum, lsup = private$maximum))
      } else if (private$dist_type == "Truncated Lognormal") {
        #Truncated lognormal distribution uses the geometric mean and standard deviation as inputs
        #Truncation bounds are both input in natural space, so for continuity people enter the geometric mean and st. dev. in natural space as well rather than the log of these parameters
        #The code takes the logarithm of these values here before sampling the distribution
        #The parameterization used here matches the LN6 parameterization from Swat et al. 2017 (ProbOnto 2.5 -> Ontology and Knowledge Base of Probability Distributions)
        return(mc2d::mcstoc(func = rlnorm, type = "U", meanlog = log(private$geom_mean), sd = log(private$geom_stdev), rtrunc = TRUE, linf = private$minimum, lsup = private$maximum))
      } else if (private$dist_type == "Custom") {
        return(private$custom_distribution_data)
      }
    },

    #' @description Validate that all distribution parameter values are
    #'   non-negative. Distribution parameters assigned a value of zero are
    #'   allowed in this function. This function was not included as part of the
    #'   private validation function because values less than zero are
    #'   physically possible for some parameters (e.g., temperature).
    checkParamsAreNonNegative = function(){
      #Constant
      if(private$dist_type == "Constant"){
        if (private$constant < 0){
          stop(paste("Error: the assigned constant value in the '", private$symbol, "' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero.", sep = ""))
        }
      }

      #Maximum
      if(private$dist_type %in% c("Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal")){
        if (private$maximum < 0){
          stop(paste("Error: the assigned maximum value in the '", private$symbol, "' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero.", sep = ""))
        }
      }

      #Mode
      if(private$dist_type %in% c("Triangular", "PERT")){
        if (private$mode < 0){
          stop(paste("Error: the assigned mode in the '", private$symbol, "' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero.", sep = ""))
        }
      }

      #Arithmetic mean
      if(private$dist_type %in% c("Truncated Normal")){
        if (private$arith_mean < 0){
          stop(paste("Error: the assigned arithmetic mean in the '", private$symbol, "' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero.", sep = ""))
        }
      }
      #Geometric mean
      if(private$dist_type %in% c("Truncated Lognormal")){
        if (private$geom_mean < 0){
          stop(paste("Error: the assigned geometric mean in the '", private$symbol, "' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero.", sep = ""))
        }
      }

      #Minimum
      if(private$dist_type %in% c("Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal")){
        if (private$minimum < 0){
          stop(paste("Error: the assigned minimum value in the '", private$symbol, "' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero.", sep = ""))
        }
      }


      #Arithmetic standard deviation
      if(private$dist_type %in% c("Truncated Normal")){
        if (private$arith_stdev < 0){
          stop(paste("Error: the assigned arithmetic standard deviation in the '", private$symbol, "' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero.", sep = ""))
        }
      }

      #Geometric standard deviation
      if(private$dist_type %in% c("Truncated Lognormal")){
        if (private$geom_stdev < 0){
          stop(paste("Error: the assigned geometric standard deviation in the '", private$symbol, "' JEMParamDist object is less than zero. Values of this parameter must be greater than or equal to zero.", sep = ""))
        }
      }

      #return
      invisible(self)
    },

    #' @description Validate that all distribution parameter values are less
    #'   than or equal to some upper limit
    #'
    #' @param upperBound The upper limit of the distribution parameter values
    #' @param allowEqual A true/false flag identifying if the distribution
    #'   parameter values can equal the upper limit.
    paramsAreLessThanUpperBound = function(upperBound, allowEqual = FALSE){

      parametersAreLessThanUpperBound <- TRUE

      #Constant
      if(private$dist_type == "Constant"){
        if ((allowEqual && private$constant > upperBound) ||
          (!allowEqual && private$constant >= upperBound)){
          parametersAreLessThanUpperBound <- FALSE
        }
      }

      #Minimum
      if(private$dist_type %in% c("Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal")){
        if ((allowEqual && private$minimum > upperBound) ||
            (!allowEqual && private$minimum >= upperBound)){
          parametersAreLessThanUpperBound <- FALSE
        }
      }

      #Maximum
      if(private$dist_type %in% c("Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal")){
        if ((allowEqual && private$maximum > upperBound) ||
            (!allowEqual && private$maximum >= upperBound)){
          parametersAreLessThanUpperBound <- FALSE
        }
      }

      #Mode
      if(private$dist_type %in% c("Triangular", "PERT")){
        if ((allowEqual && private$mode > upperBound) ||
            (!allowEqual && private$mode >= upperBound)){
          parametersAreLessThanUpperBound <- FALSE
        }
      }

      #Arithmetic mean
      if(private$dist_type %in% c("Truncated Normal", "Truncated Normal")){
        if ((allowEqual && private$arith_mean > upperBound) ||
            (!allowEqual && private$arith_mean >= upperBound)){
          parametersAreLessThanUpperBound <- FALSE
        }
      }

      #Geometric mean
      if(private$dist_type %in% c("Truncated Lognormal", "Truncated Lognormal")){
        if ((allowEqual && private$geom_mean > upperBound) ||
            (!allowEqual && private$geom_mean >= upperBound)){
          parametersAreLessThanUpperBound <- FALSE
        }
      }

      #Custom
      if(private$dist_type == "Custom"){
        max_value <- max(private$custom_distribution_data)
        if ((allowEqual && max_value > upperBound) ||
            (!allowEqual && max_value >= upperBound)){
          parametersAreLessThanUpperBound <- FALSE
        }
      }

      #return
      return(parametersAreLessThanUpperBound)
    },

    #' @description Validate that all distribution parameter values are greater
    #'   than or equal to some lower limit
    #'
    #' @param lowerBound The lower limit of the distribution parameter values
    #' @param allowEqual A true/false flag identifying if the distribution
    #'   parameter values can equal the lower limit
    paramsAreGreaterThanLowerBound = function(lowerBound, allowEqual = FALSE){

      parametersAreGreaterThanLowerBound <- TRUE

      #Constant
      if(private$dist_type == "Constant"){
        if ((allowEqual && private$constant < lowerBound) ||
            (!allowEqual && private$constant <= lowerBound)){
          parametersAreGreaterThanLowerBound <- FALSE
        }
      }

      #Minimum
      if(private$dist_type %in% c("Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal")){
        if ((allowEqual && private$minimum < lowerBound) ||
            (!allowEqual && private$minimum <= lowerBound)){
          parametersAreGreaterThanLowerBound <- FALSE
        }
      }

      #Maximum
      if(private$dist_type %in% c("Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal")){
        if ((allowEqual && private$maximum < lowerBound) ||
            (!allowEqual && private$maximum <= lowerBound)){
          parametersAreGreaterThanLowerBound <- FALSE
        }
      }

      #Mode
      if(private$dist_type %in% c("Triangular", "PERT")){
        if ((allowEqual && private$mode < lowerBound) ||
            (!allowEqual && private$mode <= lowerBound)){
          parametersAreGreaterThanLowerBound <- FALSE
        }
      }

      #Arithmetic mean
      if(private$dist_type %in% c("Truncated Normal", "Truncated Normal")){
        if ((allowEqual && private$arith_mean < lowerBound) ||
            (!allowEqual && private$arith_mean <= lowerBound)){
          parametersAreGreaterThanLowerBound <- FALSE
        }
      }

      #Geometric mean
      if(private$dist_type %in% c("Truncated Lognormal", "Truncated Lognormal")){
        if ((allowEqual && private$geom_mean < lowerBound) ||
            (!allowEqual && private$geom_mean <= lowerBound)){
          parametersAreGreaterThanLowerBound <- FALSE
        }
      }

      #Custom
      if(private$dist_type == "Custom"){
        min_value <- min(private$custom_distribution_data)
        if ((allowEqual && min_value < lowerBound) ||
            (!allowEqual && min_value <= lowerBound)){
          parametersAreGreaterThanLowerBound <- FALSE
        }
      }

      #return
      return(parametersAreGreaterThanLowerBound)
    },

    #' @description Gets the minimum possible value for the parameter based
    #'   on the distribution type and distribution parameter values
    getDistributionMinimum = function()
    {
      if(private$dist_type == "Custom"){
          return(min(private$custom_distribution_data))
      } else if(private$dist_type == "Constant"){
          return(private$constant)
      } else {
          return(private$minimum)
      }
    },

    #' @description Gets the maximum possible value for the parameter based
    #'   on the distribution type and distribution parameter values
    getDistributionMaximum = function()
    {
      if(private$dist_type == "Custom"){
        return(max(private$custom_distribution_data))
      } else if(private$dist_type == "Constant"){
        return(private$constant)
      } else {
        return(private$maximum)
      }
    }
  ),
  private = list(
      symbol = NULL,
      name = NULL,
      units = NULL,
      dist_type = NULL,
      constant = NULL,
      minimum = NULL,
      mode = NULL,
      maximum = NULL,
      arith_mean = NULL,
      arith_stdev = NULL,
      geom_mean = NULL,
      geom_stdev = NULL,
      custom_distribution_data = NULL,

    validate = function()
    {
      #check that distribution units are assigned
      if (is.null(private$units) || is.na(private$units)){
        stop(paste("Error: parameter units missing from the '", private$symbol, "' JEMParamDist object", sep = ""))
      }

      #check that distribution type is one of the allowed parameter types
      if (!private$dist_type %in% c("Constant", "Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal", "Custom")){
        stop(paste("Error: incorrect distribution type assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
      }

      #Note, the distribution name isn't required, so there's not a check for it here

      ############################ Check that all required parameters are assigned for each supported distribution type ####################

      #Constant
      if(private$dist_type == "Constant"){
        if (is.na(private$constant) || is.null(private$constant)){
          stop(paste("Error: constant value not assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Minimum
      if(private$dist_type %in% c("Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal")){
        if (is.na(private$minimum) || is.null(private$minimum)){
          stop(paste("Error: minimum value not assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Maximum
      if(private$dist_type %in% c("Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal")){
        if (is.na(private$maximum) || is.null(private$maximum)){
          stop(paste("Error: maximum value not assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Mode
      if(private$dist_type %in% c("Triangular", "PERT")){
        if (is.na(private$mode) || is.null(private$mode)){
          stop(paste("Error: mode not assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Arithmetic mean
      if(private$dist_type %in% c("Truncated Normal")){
        if (is.na(private$arith_mean) || is.null(private$arith_mean)){
          stop(paste("Error: arithmetic mean not assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Arithmetic standard deviation
      if(private$dist_type %in% c("Truncated Normal")){
        if (is.na(private$arith_stdev) || is.null(private$arith_stdev)){
          stop(paste("Error: arithmetic standard deviation not assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Geometric mean
      if(private$dist_type %in% c("Truncated Lognormal")){
        if (is.na(private$geom_mean) || is.null(private$geom_mean)){
          stop(paste("Error: geometric mean not assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Geometric standard deviation
      if(private$dist_type %in% c("Truncated Lognormal")){
        if (is.na(private$geom_stdev) || is.null(private$geom_stdev)){
          stop(paste("Error: geometric standard deviation not assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      ############################ Check that all required parameters are numeric for each supported distribution type ####################

      #Constant
      if(private$dist_type == "Constant"){
        if (!is.numeric(private$constant)){
          stop(paste("Error: non-numeric constant value assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Minimum
      if(private$dist_type %in% c("Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal")){
        if (!is.numeric(private$minimum)){
          stop(paste("Error: non-numeric minimum value assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Maximum
      if(private$dist_type %in% c("Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal")){
        if (!is.numeric(private$maximum)){
          stop(paste("Error: non-numeric maximum value assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Mode
      if(private$dist_type %in% c("Triangular", "PERT")){
        if (!is.numeric(private$mode)){
          stop(paste("Error: non-numeric mode assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Arithmetic mean
      if(private$dist_type %in% c("Truncated Normal")){
        if (!is.numeric(private$arith_mean)){
          stop(paste("Error: non-numeric arithmetic mean assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Arithmetic standard deviation
      if(private$dist_type %in% c("Truncated Normal")){
        if (!is.numeric(private$arith_stdev)){
          stop(paste("Error: non-numeric arithmetic standard deviation assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Geometric mean
      if(private$dist_type %in% c("Truncated Lognormal")){
        if (!is.numeric(private$geom_mean)){
          stop(paste("Error: non-numeric geometric mean assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Geometric standard deviation
      if(private$dist_type %in% c("Truncated Lognormal")){
        if (!is.numeric(private$geom_stdev)){
          stop(paste("Error: non-numeric geometric standard deviation assigned in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      ############################ Check that the distribution parameters make logical sense ###################

      #Minimum is less than maximum
      if(private$dist_type %in% c("Uniform", "Triangular", "PERT", "Truncated Normal", "Truncated Lognormal")){
        if (!(private$minimum < private$maximum)){
          stop(paste("Error: the minimum is not less than the maximum in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Minimum is less than arithmetic mean
      if(private$dist_type %in% c("Truncated Normal")){
        if (!(private$minimum < private$arith_mean)){
          stop(paste("Error: the minimum is not less than the arithmetic mean in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Arithmetic mean is less than maximum
      if(private$dist_type %in% c("Truncated Normal")){
        if (!(private$arith_mean < private$maximum)){
          stop(paste("Error: the arithmetic mean is not less than the maximum in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Minimum is less than geometric mean
      if(private$dist_type %in% c("Truncated Lognormal")){
        if (!(private$minimum < private$geom_mean)){
          stop(paste("Error: the minimum is not less than the geometric mean in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #Geometric mean is less than maximum
      if(private$dist_type %in% c("Truncated Lognormal")){
        if (!(private$geom_mean < private$maximum)){
          stop(paste("Error: the geometric mean is not less than the maximum in the '", private$symbol, "' JEMParamDist object", sep = ""))
        }
      }

      #return
      invisible(self)
    }
  )
)


