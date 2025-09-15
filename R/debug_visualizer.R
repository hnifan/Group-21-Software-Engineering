#' RStudio Debug Visualizer
#' HUANG JUNHAO 2025/9/9

#' 确定RStudio
launchDebugVisualizer <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("This plugin requires RStudio")
  }
#' 加载shiny app
  shiny::runApp(
    appDir = system.file("shiny-app", package = "RStudioDebugVisualizer"),
    launch.browser = TRUE,
    port = getOption("shiny.port", 8080)
  )
}
#' 添加断点（提供用户反馈）
addBreakpoint <- function(file, line) {
  if (rstudioapi::isAvailable()) {
    tryCatch({
      rstudioapi::executeCommand("addBreakpoint", list(file = file, line = line))
      message(paste("Breakpoint added at", file, "line", line))
    }, error = function(e) {
      warning("Could not add breakpoint: ", e$message)
    })
  }
}
#' 移除断点
removeBreakpoint <- function(file, line) {
  if (rstudioapi::isAvailable()) {
    tryCatch({
      rstudioapi::executeCommand("removeBreakpoint", list(file = file, line = line))
      message(paste("Breakpoint removed from", file, "line", line))
    }, error = function(e) {
      warning("Could not remove breakpoint: ", e$message)
    })
  }
}
#' 增加监控列表
watchVariable <- function(var_name) {
  # Store watched variables in global environment
  if (!exists(".debug_watched_vars", envir = .GlobalEnv)) {
    assign(".debug_watched_vars", character(0), envir = .GlobalEnv)
  }
  watched_vars <- get(".debug_watched_vars", envir = .GlobalEnv)
  if (!var_name %in% watched_vars) {
    watched_vars <- c(watched_vars, var_name)
    assign(".debug_watched_vars", watched_vars, envir = .GlobalEnv)
    message(paste("Now watching variable:", var_name))
  }
}
#' 移除监控列表
unwatchVariable <- function(var_name) {
  if (exists(".debug_watched_vars", envir = .GlobalEnv)) {
    watched_vars <- get(".debug_watched_vars", envir = .GlobalEnv)
    watched_vars <- watched_vars[watched_vars != var_name]
    assign(".debug_watched_vars", watched_vars, envir = .GlobalEnv)
    message(paste("Stopped watching variable:", var_name))
  }
}
#' 获取当前环境的变量
.getCurrentEnvVars <- function() {
  env_vars <- ls(envir = parent.frame(2))
  var_list <- list()
  
  for (var in env_vars) {
    tryCatch({
      value <- get(var, envir = parent.frame(2))
      var_list[[var]] <- list(
        name = var,
        type = class(value)[1],
        value = if (is.atomic(value) && length(value) <= 10) {
          paste(value, collapse = ", ")
        } else {
          paste(class(value), "of length", length(value))
        },
        size = object.size(value)
      )
    }, error = function(e) {
      var_list[[var]] <<- list(
        name = var,
        type = "Error",
        value = "Cannot access",
        size = 0
      )
    })
  }
  
  return(var_list)
}
#' 调用当前栈
.getCallStack <- function() {
  calls <- sys.calls()
  stack_info <- list()
  
  for (i in seq_along(calls)) {
    call_text <- deparse(calls[[i]])
    stack_info[[i]] <- list(
      level = i,
      call = call_text[1],
      full_call = paste(call_text, collapse = " ")
    )
  }
  
  return(stack_info)
}