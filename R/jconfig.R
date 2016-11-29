## Simple wrapper over RJSONIO to load configs
## Beware that all the field names must be escaped

JC.load.includes <- function(CFG)
{ 
    nms <- names(CFG)
    if(is.null(nms)) return(CFG)
    if(!"include" %in% nms) return(lapply(CFG, JC.load.includes))
    SCFG <- loadJConfig(CFG[["include"]])
    CFG <- c(SCFG, CFG)
    CFG[["include"]] <- NULL
    lapply(CFG, JC.load.includes)
}

## So everything is a list
JC.reformat <- function(CFG)
{    
    if(!is.list(CFG))
    {
        if(!is.null(names(CFG))) return(as.list(CFG))
        return(CFG)
    }
    lapply(CFG, JC.reformat)
}

#' Config Value
#'
#' Get the value of a given configuration field with a default in case this field doesn't exist.
#' @param CFG configuration object
#' @param keys vector of character giving the path to the desired field
#' @param default default value to be used in case no field is found
#' @export
JC.get <- function(CFG,
                   keys,
                   default)
{
    V <- jsutils::deep.list(CFG, keys)
    if(is.null(V)) return(default)
    V
}

#' Loading configs
#'
#' @details
#' Will load the config from a JSON file or a R file. Sub configs can be included with clauses of the form: "include : path/to/other/config".
#' @param ... Parameters to be passed to the RJSONIO fromJSON function or source
#' @return A list containing the config parameters. Everything is recursively converted to a list so acess has to be done through: "[[" or jsutils::deep.list
#' @seealso set.config, get.config
#' @export
loadJConfig <- function(file,
                        ...)
{
    CFG <- tryCatch({
        if(grepl("*.R$", file))
        {
            CFG <- source(file,
                          ...)
            CFG$value
        }
        else
            RJSONIO::fromJSON(file,
                              ...)
    },
    error = function(cond) stop(sprintf("File %s couldn't be loaded reason: %s", file, cond$message)))
    
    CFG <- JC.load.includes(CFG)
    JC.reformat(CFG)
}

global.configs <- new.env()

#' @name set.get.config
#' @title Setting / setting global config
#' @param CFG new function to assign
#' @return The global config for get.config
#' @description
#' Functions to set and access a global config
NULL

#' @rdname set.get.config
#' @export
set.config <- function(CFG, handle = "global") global.configs[[handle]] <- CFG

#' @rdname set.get.config
#' @export
get.config <- function(handle = "global") global.configs[[handle]]
