library(ggplot2)
#' A RC class for linear regression models
#'@field formula A formula
#'@field data A dataset which we apply the formula (data.frame)
#'@export linreg
#'@export
linreg <- setRefClass(
  "linreg",
  fields = list(
    formula = "formula",
    data = "data.frame",
    call = "vector",
    beta_hat = "matrix",
    y_hat = "matrix",
    e_hat = "matrix",
    degf = "numeric",
    res_var = "matrix",
    var_beta = "vector",
    t_vals = "numeric"
  ),
  methods = list(
    initialize = function(formula, data) {
      # Intercept formula and data before assignment
      call <<- c("linreg(formula = ",
                 Reduce(paste,deparse(formula)),
                 ", data = ",
                 deparse(substitute(data)),
                 ")")
      formula <<- formula
      data <<- data

      # Initialize X matrix and dependent variable
      X <- model.matrix(formula, data)
      label <- all.vars(formula)[1]
      y <- data[[label]]

      # Variables for class instantiation

      # Regression coefficients
      beta_hat <<- solve(t(X) %*% X) %*% t(X) %*% y

      # Fitted values
      y_hat <<- X %*% beta_hat

      # Residuals
      e_hat <<- y - y_hat

      # Degrees of freedom
      degf <<- nrow(X) - ncol(X)

      # Residual variance
      res_var <<- (t(e_hat) %*% e_hat) / degf

      # Variance of regression coefficients
      var_beta <<- diag(as.numeric(res_var) * solve(t(X) %*% X))

      # t-values for each coefficient
      t_vals <<- as.numeric(beta_hat) / sqrt(var_beta)
    },
    print = function() {
      "Prints the call of the class and regression coefficients in named vector"
      # Print call
      cat("\nCall:\n")
      lapply(call, cat)

      # Print coefficients
      cat("\n\n")
      cat("Coefficients:\n")
      coef()
    },
    plot = function() {
      "Provides a plot for \'Scale-Location\' and \'Residuals vs. Fitted\',
      returns a list of the two plots, accessed $p1 and $p2"
      plot_dat <- data.frame(dat_e = e_hat, dat_y = y_hat)
      liu_col_blu <- "#68B7D1"
      liu_col_blk <- "#333333"
      plot_theme <-
        theme(plot.background = element_rect(fill = "#e8e8e8"),
              panel.background = element_rect(fill = "white"),
              axis.line.x = element_line(color=liu_col_blk, size = 1),
              axis.line.y = element_line(color=liu_col_blk, size = 1))
      plot_rvf <-
        ggplot(data = plot_dat, aes(x = y_hat, y = e_hat)) +
        xlab(paste("Fitted values\n", call[2])) +
        ylab("Residuals") +
        ggtitle("Residuals vs Fitted") +
        geom_point(color=liu_col_blk) +
        geom_smooth(se=FALSE,color=liu_col_blu) +
        geom_abline(slope=0,intercept=0,linetype="dotted") +
        plot_theme

      plot_scale <-
        ggplot(data = plot_dat,
               aes(x = y_hat, y = sqrt(abs((dat_e- mean(dat_e)) / sqrt(res_var))))) +
        ylab(expression(sqrt("Standardized residuals"))) +
        xlab(paste("Fitted values\n", call[2])) +
        ggtitle("Scale-Location") +
        geom_point(color=liu_col_blk) +
        geom_smooth(se=FALSE,color=liu_col_blu) +
        plot_theme
      return(list(p1 = plot_scale, p2 = plot_rvf))
    },
    resid = function() {
      "Returns residual values"
      return(e_hat)
    },
    pred = function() {
      "Returns fitted values"
      return(y_hat)
    },
    coef = function() {
      "Returns the regression coefficients"
      dummy <- c(t(beta_hat))
      names(dummy) <- row.names(beta_hat)
      return(dummy)
    },
    summary = function() {
      "Provides a summary of Estimate, standard error, t values and
      p-values for linear regression coefficients"
      # Print call
      cat("Call:\n")
      lapply(call, cat)

      # Print coefficients
      cat("\n\n")
      cat("Coefficients:\n")

      dummy <- cbind(round(beta_hat,3),
                     round(as.numeric(lapply(var_beta, sqrt)),3),
                     round(t_vals,3),
                     round(as.numeric(pt(abs(t_vals),df = degf)),3))
      colnames(dummy) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
      print.default(dummy)
      temp <- c("--- \n\nResidual standard error: ",
                round(sqrt(res_var),4),
                " on ",
                degf,
                " degrees of freedom")
      lapply(temp, function(x) {cat(x)})
      cat("\n") # Needed for some reason... flush?
    }
  )
)


data(iris)
X <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
