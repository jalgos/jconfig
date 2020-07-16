## Simple wrapper over RJSONIO to load configs
## Beware that all the field names must be escaped

load.includes <- function(CFG)
{
    nms <- names(CFG)
    if(is.null(nms) && !is.list(CFG))
        return(CFG)
    if(!"include" %in% nms)
        return(lapply(CFG, load.includes))

    SCFG <- load.config(CFG[["include"]])
    fields <- setdiff(nms, "include")

    if(is.null(names(SCFG)))
        CFG <- c(CFG[fields], SCFG)
    else
        CFG <- c(CFG[fields],
                 SCFG[setdiff(names(SCFG), fields)])

    lapply(CFG, load.includes)
}

## So everything is a list
reformat <- function(CFG)
{
    if(!is.list(CFG))
    {
        if(!is.null(names(CFG))) return(as.list(CFG))
        return(CFG)
    }
    lapply(CFG, reformat)
}

#' Config Value
#'
#' Get the value of a given configuration field with a default in case this field doesn't exist.
#' @param CFG configuration object
#' @param keys vector of character giving the path to the desired field
#' @param default default value to be used in case no field is found
#' @export
get.field <- function(CFG,
                      keys,
                      default = NULL)
{
    V <- deep.list(CFG, keys)
    if(is.null(V)) return(default)
    V
}

#' List assignment
#'
#' Request or assigns values deep in the list at any arbitrary depth.
#' Function moved from jsutils in order to avoid circular dependencies
#' @param L the list
#' @param keys Vector of the keys. A path in the tree formed in the
#' @param value Optional value to be assigned
deep.list <- function(L = list(),
                      keys,
                      value)
{
    if(length(keys) == 1)
    {
        if(missing(value)) return(L[[keys]])
        L[[keys]] <- value
        return(L)
    }
    if(missing(value)) return(deep.list(L[[keys[1]]], keys[-1]))
    L[[keys[1]]] <- as.list(deep.list(L[[keys[1]]], keys[-1], value))
    L
}


#' Loading configs
#'
#' @details
#' Will load the config from a JSON file or a R file. Sub configs can be included with clauses of the form: "include : path/to/other/config".
#' @param ... Parameters to be passed to the RJSONIO fromJSON function or source
#' @return A list containing the config parameters. Everything is recursively converted to a list so acess has to be done through: "[[" or jsutils::deep.list
#' @seealso set.config, get.config
#' @importFrom RJSONIO fromJSON
#' @export
load.config <- function(file,
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

    CFG <- load.includes(CFG)
    reformat(CFG)
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
get.config <- function(handle = "global")
{
    TRY <- global.configs[[handle]]
    if(handle == "global" || !is.null(TRY))
        return(TRY)
    CONF <- get.config()
    if(is.null(CONF))
        return(CONF)
    CONF[[handle]]
}
