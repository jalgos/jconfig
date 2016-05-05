#' jconfig: Managing configuration files
#'
#' This package provides a way to manage configuration files. Files can be loaded from disk. They can be either JSON or R. Configurations can include other configuration files with clauses of the form: "include": "path/to/other/config"
#' 
#' @section Functions:
#' Users need to use 'loadJConfig', 'get.config' and 'set.config'
#' 
#'
#' @docType package
#' @name package-jconfig
NULL

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
#' @export
loadJConfig <- function(file,
                        ...)
{
    if(grep("*.R$", file))
    {
        CFG <- source(file,
                      ...)
        CFG <- CFG$value
    }
    else
        CFG <- RJSONIO::fromJSON(file,
                                 ...)
    CFG <- JC.load.includes(CFG)
    JC.reformat(CFG)
}

global.config <- NULL

#' @name set.get.config
#' @title Setting / setting global config
#' @param CFG new function to assign
#' @return The global config for get.config
#' @description
#' Functions to set and access a global config
NULL

#' @rdname set.get.config
#' @export
set.config <- function(CFG) global.config <<- CFG

#' @rdname set.get.config
#' @export
get.config <- function() global.config
