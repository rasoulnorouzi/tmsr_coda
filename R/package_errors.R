if(FALSE){
cat("Could not find required packages; please run ",

    paste0(c(c("install.packages('targets')", "")[TRUE+1L], c("install.packages('tarchetypes')", "")[FALSE+1L]), collapse = "; "),
    " then try again."); cat("Could not find required packages; please run ",

                             paste0(c(c("install.packages('targets')", "")[FALSE+1L], c("install.packages('tarchetypes')", "")[TRUE+1L]), collapse = "; "),
                             " then try again."); cat("Could not find required packages; please run ",

                                                      paste0(c(c("install.packages('targets')", "")[FALSE+1L], c("install.packages('tarchetypes')", "")[FALSE+1L]), collapse = "; "),
                                                      " then try again.")


cat("Could not find required packages; please run ",

    paste0(c("install.packages('targets')", "")[TRUE+1L], c("install.packages('tarchetypes')", "")[FALSE+1L], collapse = "; "),
    " then try again.")
cat("Could not find required packages; please run ",

    paste0(c("install.packages('targets')", "")[FALSE+1L], c("install.packages('tarchetypes')", "")[TRUE+1L], collapse = "; "),
    " then try again.")
cat("Could not find required packages; please run the following code, then try again:\n",
    paste0(c(c("install.packages('targets')", "")[FALSE+1L], c("install.packages('tarchetypes')", "")[TRUE+1L]), collapse = "\n "))

cat("Could not find required packages; please run the following code, then try again:\n",
    do.call(paste0, list(
      c("install.packages('targets')", NULL)[FALSE+1L],
      c("install.packages('tarchetypes')", NULL)[TRUE+1L], collapse = "\n ")))
}
