#' Create a directory and its parent directories if they don't exist
#'
#' The `create.dir` function is used to create a directory in the file system and its parent directories if they don't exist. If the specified directory already exists, no action is taken.
#'
#' @param path The path of the directory to be created.
#'
#' @return The function does not return any value.
#'
#' @examples
#' create.dir("path/to/directory")
#'
#' @export
create.dir <- function(path) {
  if (!dir.exists(path)) {
    spath <- strsplit(path, "/")[[1]]
    P <- ""
    for (i in 1:length(spath)) {
      P <- if (i == 1) spath[1] else pst(P, "/", spath[i])
      if (!dir.exists(P)) dir.create(P)
    }
  }
}

#' Read and scan a CSV file
#'
#' The `csv.scanner` function is used to read and scan a CSV file. It returns the scanned data as a list.
#'
#' @param file The path to the CSV file.
#' @param lista An optional list object to append the scanned data to. If not provided, a new list is created.
#' @param basis A basis string to prepend to each scanned line. This can be useful for adding a prefix to the scanned data.
#'
#' @return A list object containing the scanned data.
#'
#' @examples
#' csv.scanner("path/to/file.csv")
#'
#' @export
csv.scanner <- function(file, lista = NULL, basis = "") {
  return(csv.scanner_(file, lista, basis)$get())
}

csv.scanner_ <- function(file, lista, basis) {
  if (is.null(lista)) {
    basis <- file
    lista <- newList(basis)
    file <- ""
  }
  df <- dir(if (file != "") pst(basis, "/", file) else basis)
  if (length(df) != 0) {
    for (d in df) {
      nfile <- if (file != "") pst(file, "/", d) else d
      if (file.info(pst(basis, "/", nfile))$isdir) {
        csv.scanner(nfile, lista = lista, basis = basis)
      } else {
        nc <- nchar(d)
        if (nchar(d) > 4) {
          if (substr(d, nc - 3, nc) == ".csv") {
            lista$add(list(file = file, name = nfile, value = read.csv(pst(basis, "/", nfile))))
          }
        }
      }
    }
  }
  lista
}

#' Print data from a list of CSV scans
#'
#' The `csv.printer` function is used to print data from a list of CSV scans. It creates directories and writes CSV files based on the scanned data.
#'
#' @param lista The list object containing the scanned data.
#' @param basis An optional basis string to prepend to each file path. If not provided, the first scanned file path is used as the basis.
#'
#' @examples
#' csv.printer(lista)
#'
#' @export
csv.printer <- function(lista, basis = NULL) {
  if (is.null(basis)) basis <- lista[[1]]

  if (basis != "") basis <- pst(basis, "/")

  lista[[1]] <- NULL
  n <- length(lista)
  if (n > 0) {
    for (i in 1:length(lista)) {
      create.dir(pst(basis, lista[[i]]$file))
      write.csv(lista[[i]]$value, pst(basis, lista[[i]]$name), na = "", row.names = F)
    }
  }
}

#' Generate a modified sequence
#'
#' The `mseq` function generates a modified sequence of numbers between a starting point and an ending point. It provides flexibility in defining the number of elements in the sequence and the step size.
#'
#' @param from The starting point of the sequence.
#' @param to The ending point of the sequence.
#' @param n The number of elements in the sequence. Default is 2.
#' @param close A logical value indicating whether the sequence should include the endpoint (`to`) or not. Default is `TRUE`.
#' @param by The step size between consecutive elements in the sequence. If `NULL`, the step size is automatically determined based on the number of elements (`n`) and the range between `from` and `to`. Default is `NULL`.
#' @param ... Additional arguments to be passed to the `seq` function.
#'
#' @return A sequence of numbers between `from` and `to`, with the specified number of elements (`n`), step size (`by`), and endpoint inclusion (`close`).
#'
#' @examples
#' mseq(1, 10, n = 5)
#' mseq(1, 10, n = 5, close = FALSE)
#' mseq(1, 10, by = 2)
#'
#' @export
mseq <- function(from, to, n = 2, close = T, by = NULL, ...) {
  if (is.null(by)) {
    if (close) {
      return(seq(from = from, to = to, by = (to - from) / (n - 1), ...))
    }
    return(seq(from = from, to = to, by = (to - from) / n, ...)[-n - 1])
  }
  seq(from = from, to = to, by = by, ...)
}

#' Create a list with specified elements
#'
#' The `l` function creates a list with the specified elements. It is a shorthand way to create a list object in R.
#'
#' @param ... The elements to be included in the list.
#'
#' @return A list object containing the specified elements.
#'
#' @examples
#' l(1, 2, 3)
#' l("a", "b", "c")
#' l(1, "a", TRUE)
#'
#' @export
l <- function(...) list(...)

#' Set or retrieve the working auxiliary directory
#'
#' The `auxiliarWD` function is used to set or retrieve the working auxiliary directory. If no argument is provided, it returns the current working directory. If a file path is provided as an argument, it sets the working auxiliary directory to that path.
#'
#' @param file The file path for the working auxiliary directory.
#'
#' @return The current working auxiliary directory if no argument is provided. Otherwise, NULL is returned.
#'
#' @examples
#' auxiliarWD() # Retrieves the current working auxiliary directory
#' auxiliarWD("path/to/directory") # Sets the working auxiliary directory to the specified path
#'
#' @export
auxiliarWD <- function(file = "") {
  code <- "WD.00000000wNZ8piTdFAWCqzbqnRBXJB5K9A9vmRqSc2"
  if (file == "") {
    dir <- globalVar(code)
    if (!base::is.null(dir)) {
      if (dir.exists(dir)) {
        return(dir)
      }
    }
    return(getwd())
  }
  globalVar(code, file)
  cat("Working auxiliary directory set:")
  cat("\n\n    ", auxiliarWD(), "\n")
}

#' Convert List Elements to a Vector
#'
#' The `listToVec` function is used to convert specific elements of a list to a vector. It takes a list and an index of the elements to be extracted from the list, and returns a vector containing those elements.
#'
#' @param List The list from which elements will be extracted.
#' @param index A numeric vector specifying the indices of the elements to be extracted.
#'
#' @return A vector containing the elements extracted from the list.
#'
#' @examples
#' myList <- list("apple", "banana", "orange", "grape")
#' listToVec(myList, c(2, 4)) # Extracts the second and fourth elements from the list
#'
#' @export
listToVec <- function(List, index) {
  vec <- NULL
  for (i in index) vec <- c(vec, List[[i]])
  return(vec)
}

#' Extract Sublist from a List
#'
#' The `subList` function is used to extract a sublist from a given list. It takes a list and an index of the elements to be included in the sublist, and returns a new list containing only those elements.
#'
#' @param List The original list from which the sublist will be extracted.
#' @param index A numeric vector specifying the indices of the elements to be included in the sublist.
#'
#' @return A new list containing the elements specified by the index.
#'
#' @examples
#' myList <- list("apple", "banana", "orange", "grape")
#' subList(myList, c(2, 4)) # Extracts the second and fourth elements from the list
#'
#' @export
subList <- function(List, index) {
  listy <- list()
  for (i in 1:length(index)) listy[[i]] <- List[[index[i]]]
  return(listy)
}

#' Reparametrization
#'
#' The `reparam` function is used to perform reparametrization of a value based on given coordinates and corresponding values. It calculates a new value `x0` based on the linear interpolation between the given coordinates `x` and values `y`.
#'
#' @param x0 The new coordinate value for which the reparametrized value will be calculated.
#' @param x A numeric vector of length 2 specifying the original coordinate values.
#' @param y A numeric vector of length 2 specifying the corresponding values for the original coordinate values.
#'
#' @return The reparametrized value for `x0`.
#'
#' @examples
#' x <- c(0, 1)
#' y <- c(2, 4)
#' reparam(0.5, x, y) # Performs reparametrization at x0 = 0.5
#'
#' @export
reparam <- function(x0, x, y) {
  return(y[1] + (x0 - x[1]) * (y[2] - y[1]) / (x[2] - x[1]))
}

#' Get Seconds
#'
#' The `getSeconds` function is used to retrieve the current system time in seconds.
#'
#' @return The current system time in seconds.
#'
#' @examples
#' getSeconds() # Retrieves the current system time in seconds
#'
#' @export
getSeconds <- function() as.numeric(proc.time()[3])

#' Paste with Separator
#'
#' The `pst` function is a wrapper around the base `paste` function that allows specifying a custom separator.
#'
#' @param ... One or more character vectors to be concatenated.
#' @param sep The separator to be used between the elements. Default is an empty string.
#'
#' @return A single character vector obtained by concatenating the input vectors with the specified separator.
#'
#' @examples
#' pst("Hello", "world", sep = ", ") # Returns "Hello, world"
#'
#' @export
pst <- function(..., sep = "") paste(..., sep = sep)

#' Join Strings
#'
#' The `join` function concatenates multiple strings into a single string using a specified separator.
#'
#' @param ... One or more character vectors to be concatenated.
#' @param sep The separator to be used between the elements. Default is an empty string.
#'
#' @return A single character vector obtained by concatenating the input vectors with the specified separator.
#'
#' @examples
#' join("Hello", "world", sep = ", ") # Returns "Hello, world"
#' join("a", "b", "c", sep = "-") # Returns "a-b-c"
#'
#' @export
join <- function(..., sep = "") {
  xjoin <- NULL
  y <- list(...)
  for (j in 1:length(y)) {
    x <- y[[j]]
    for (i in 1:length(x)) {
      if (i == 1 & j == 1) {
        xjoin <- pst(xjoin, x[i], sep = "")
      } else {
        xjoin <- pst(xjoin, x[i], sep = sep)
      }
    }
  }
  return(xjoin)
}

#' Generate a Random File Name
#'
#' The `randFile` function generates a random file name by combining a prefix, a sequence of zeros, and a random selection of characters.
#'
#' @return A character string representing a random file name.
#'
#' @examples
#' randFile() # Returns something like "0x00000A1b3FgH"
#' randFile() # Returns something like "0x00000CB2xYzTg"
#'
#' @export
randFile <- function() join("0x", rep(0, runif(1, 5, 10)), sample(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, letters, LETTERS), runif(1, 30, 50), replace = T))

#' Try-Catch Function
#'
#' The `TRY` function is a convenient wrapper around the `tryCatch` function in R. It allows you to evaluate an expression and catch any errors that occur, providing an alternative value `y` if an error is encountered.
#'
#' @param x The expression to be evaluated.
#' @param y The alternative value to return if an error occurs. Defaults to `NULL`.
#' @return The result of evaluating the expression `x`, or the alternative value `y` if an error occurs.
#'
#' @examples
#' TRY(sqrt(4)) # Returns 2
#' TRY(sqrt(-4)) # Returns NULL
#' TRY(sqrt(-4), "Error occurred") # Returns "Error occurred"
#'
#' @export
TRY <- function(x, y = NULL) tryCatch(x, error = function(e) y)

#' Conditional Function
#'
#' The `IF` function is a convenient wrapper around the `if` statement in R. It allows you to evaluate a condition and return different values based on the outcome.
#'
#' @param cond The condition to evaluate.
#' @param x The value to return if the condition is `TRUE`.
#' @param y The value to return if the condition is `FALSE`. Defaults to `NULL`.
#' @return The value of `x` if the condition is `TRUE`, or the value of `y` if the condition is `FALSE`.
#'
#' @examples
#' IF(TRUE, "Condition is true") # Returns "Condition is true"
#' IF(FALSE, "Condition is true") # Returns NULL
#' IF(FALSE, "Condition is true", "Condition is false") # Returns "Condition is false"
#'
#' @export
IF <- function(cond, x, y = NULL) {
  if (cond) {
    x
  } else {
    y
  }
}

#' Compare Item in List
#'
#' The `list.compareItem` function compares an item in a list with a specified value and returns `TRUE` if they are equal.
#'
#' @param lista The list to search for the item.
#' @param name The name of the item in the list.
#' @param item The value to compare with the item.
#' @return `TRUE` if the item in the list is equal to the specified value, `FALSE` otherwise.
#'
#' @examples
#' my_list <- list(a = 1, b = 2, c = 3)
#' list.compareItem(my_list, "a", 1) # Returns TRUE
#' list.compareItem(my_list, "b", 4) # Returns FALSE
#'
#' @export
list.compareItem <- function(lista, name, item) {
  item_ <- TRY(lista[[name]])
  if (!base::is.null(item_)) {
    if (item_ == item) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Check if Value is not Null
#'
#' The `isnt.null` function checks if a value is not null and returns the value itself if it is not null, or a specified default value otherwise.
#'
#' @param x The value to check.
#' @param y The default value to return if the value is null.
#' @return The value itself if it is not null, or the default value if it is null.
#'
#' @examples
#' isnt.null("Hello", "Default") # Returns "Hello"
#' isnt.null(NULL, "Default") # Returns "Default"
#'
#' @export
isnt.null <- function(x, y) if (is.null(x)) y else x

#' Check if Value is not NA
#'
#' The `isnt.na` function checks if a value is not NA and returns the value itself if it is not NA, or a specified default value otherwise.
#'
#' @param x The value to check.
#' @param y The default value to return if the value is NA.
#' @return The value itself if it is not NA, or the default value if it is NA.
#'
#' @examples
#' isnt.na(5, "Default") # Returns 5
#' isnt.na(NA, "Default") # Returns "Default"
#'
#' @export
isnt.na <- function(x, y) if (is.na(x)) y else x

#' Check if Value is not NASD
#'
#' The `isnt.nasd` function checks if a value is not NASD (Not A Significant Data) and returns the value itself if it is not NASD, or a specified default value otherwise.
#'
#' @param x The value to check.
#' @param y The default value to return if the value is NASD.
#' @return The value itself if it is not NASD, or the default value if it is NASD.
#'
#' @examples
#' isnt.nasd(42, "Default") # Returns 42
#' isnt.nasd(nasd(), "Default") # Returns "Default"
#'
#' @export
isnt.nasd <- function(x, y) if (is.nasd(x)) y else x

#' NASD (Not A Significant Data) Identifier
#'
#' The `nasd` function returns the NASD (Not A Significant Data) identifier.
#'
#' @return The NASD identifier.
#'
#' @examples
#' nasd() # Returns "nasd.00000000n3F8zpfpCoa5NnO6ELu8GhgA86qVNRnGw0gp93ekXeYgaPxu"
#'
#' @export
nasd <- function() "nasd.00000000n3F8zpfpCoa5NnO6ELu8GhgA86qVNRnGw0gp93ekXeYgaPxu"

#' Null Identifier
#'
#' The `null` function returns the null identifier.
#'
#' @return The null identifier.
#'
#' @examples
#' null() # Returns "null.000000000GBxRkEka3IDpOK5h0FNBnzLXMq1AIewoVOn64wincCVyl"
#'
#' @export
null <- function() "null.000000000GBxRkEka3IDpOK5h0FNBnzLXMq1AIewoVOn64wincCVyl"

#' Check if Value is Null
#'
#' The `is.null` function checks if a value is null or the null identifier and returns TRUE if it is, or FALSE otherwise.
#'
#' @param x The value to check.
#' @return TRUE if the value is null or the null identifier, FALSE otherwise.
#'
#' @examples
#' is.null(NULL) # Returns TRUE
#' is.null("null.000000000GBxRkEka3IDpOK5h0FNBnzLXMq1AIewoVOn64wincCVyl") # Returns TRUE
#' is.null("Hello") # Returns FALSE
#'
#' @export
is.null <- function(x) {
  if (base::is.null(x)) {
    return(TRUE)
  }
  if (is.character(x)) {
    TRY(if (x[1] == "null.000000000GBxRkEka3IDpOK5h0FNBnzLXMq1AIewoVOn64wincCVyl") {
      return(TRUE)
    })
  }
  return(FALSE)
}

#' Check if Value is NASD
#'
#' The `is.nasd` function checks if a value is the NASD identifier and returns TRUE if it is, or FALSE otherwise.
#'
#' @param x The value to check.
#' @return TRUE if the value is the NASD identifier, FALSE otherwise.
#'
#' @examples
#' is.nasd("nasd.00000000n3F8zpfpCoa5NnO6ELu8GhgA86qVNRnGw0gp93ekXeYgaPxu") # Returns TRUE
#' is.nasd("Hello") # Returns FALSE
#'
#' @export
is.nasd <- function(x) {
  if (is.character(x)) {
    TRY(if (x[1] == "nasd.00000000n3F8zpfpCoa5NnO6ELu8GhgA86qVNRnGw0gp93ekXeYgaPxu") {
      return(TRUE)
    })
  }
  return(FALSE)
}

#' Check if Value is Color Class
#'
#' The `is.color.class` function checks if a value is of the color class by comparing the "class" attribute.
#' It returns TRUE if the value is of the color class, or FALSE otherwise.
#'
#' @param x The value to check.
#' @return TRUE if the value is of the color class, FALSE otherwise.
#'
#' @examples
#' x <- list(class = "color.000000IM9fKDwnkPL8A0dxaV3zT64Q6cjKifaT0BA9kmcvCAQ1NT")
#' is.color.class(x) # Returns TRUE
#'
#' @export
is.color.class <- function(x) list.compareItem(x, "class", "color.000000IM9fKDwnkPL8A0dxaV3zT64Q6cjKifaT0BA9kmcvCAQ1NT")

#' Check if Value is Group Class
#'
#' The `is.group.class` function checks if a value is of the group class by comparing the "class" attribute.
#' It returns TRUE if the value is of the group class, or FALSE otherwise.
#'
#' @param x The value to check.
#' @return TRUE if the value is of the group class, FALSE otherwise.
#'
#' @examples
#' x <- list(class = "group.00000004intEgVdJq9HfQwRwDXWRlZYxDSX646IH6s51zkcT1B")
#' is.group.class(x) # Returns TRUE
#'
#' @export
is.group.class <- function(x) list.compareItem(x, "class", "group.00000004intEgVdJq9HfQwRwDXWRlZYxDSX646IH6s51zkcT1B")

#' Check if Value is Clonable
#'
#' The `is.clonable` function checks if a value is clonable by comparing the "clonable" attribute.
#' It returns TRUE if the value is clonable, or FALSE otherwise.
#'
#' @param x The value to check.
#' @return TRUE if the value is clonable, FALSE otherwise.
#'
#' @examples
#' x <- list(clonable = "clonable.0000000WJ3bLybo9e7EvjO8mFmdiHKTX3lh3BYsPiPst")
#' is.clonable(x) # Returns TRUE
#'
#' @export
is.clonable <- function(x) list.compareItem(x, "clonable", "clonable.0000000WJ3bLybo9e7EvjO8mFmdiHKTX3lh3BYsPiPst")

#' Color Class Identifier
#'
#' The `color.class` function returns the identifier for the color class.
#'
#' @return The identifier for the color class.
#'
#' @examples
#' color.class() # Returns "color.000000IM9fKDwnkPL8A0dxaV3zT64Q6cjKifaT0BA9kmcvCAQ1NT"
#'
#' @export
color.class <- function() "color.000000IM9fKDwnkPL8A0dxaV3zT64Q6cjKifaT0BA9kmcvCAQ1NT"

#' Group Class Identifier
#'
#' The `group.class` function returns the identifier for the group class.
#'
#' @return The identifier for the group class.
#'
#' @examples
#' group.class() # Returns "group.00000004intEgVdJq9HfQwRwDXWRlZYxDSX646IH6s51zkcT1B"
#'
#' @export
group.class <- function() "group.00000004intEgVdJq9HfQwRwDXWRlZYxDSX646IH6s51zkcT1B"


#' @export
.vector <- R6::R6Class("vector",
  private = list(vector = NULL),
  public = list(
    class = "vector",
    initialize = function(x = NULL) {
      private$vector <- x
    },
    length = function() {
      length(private$vector)
    },
    get = function(index = NA) {
      if (is.na(index[1])) {
        return(private$vector)
      }
      return(private$vector[index])
    },
    set = function(x, index = NA) {
      if (is.na(index[1])) {
        private$vector <- x
      } else {
        private$vector[index] <- x
      }
    },
    add = function(x) {
      private$vector <- c(private$vector, x)
    },
    iter = function(cyclic = FALSE, defVal = NA) iterVector(get(), cyclic, defVal)
  )
)

#' Create a new vector object
#'
#' This function creates a new instance of the `.vector` class. It can be
#' initialized with a vector `x` if provided.
#'
#' @param x An optional vector to initialize the vector object.
#' @return A new instance of the `.vector` class.
#' @examples
#' # Create an empty vector object
#' v1 <- newVec()
#' v1$get() # Returns an empty vector
#' v1$length() # Returns the length of the vector: 0
#'
#' # Create a vector object with initial values
#' v2 <- newVec(c(1, 2, 3, 4, 5))
#' v2$get() # Returns the vector [1, 2, 3, 4, 5]
#' v2$length() # Returns the length of the vector: 5
#'
#' # Set a value at a specific index
#' v2$set(10, index = 3)
#' v2$get() # Returns the vector [1, 2, 10, 4, 5]
#'
#' # Add a value to the end of the vector
#' v2$add(6)
#' v2$get() # Returns the vector [1, 2, 10, 4, 5, 6]
#'
#' # Iterate over the vector
#' v2$iter(cyclic = TRUE, defVal = NA)
#' # Iterates over the vector elements with cyclic behavior and a default value of NA
#'
#' @export
newVec <- function(x = NULL) {
  .vector$new(x)
}


#' Create a new list object
#'
#' This function creates a new instance of the `.vector` class and wraps it in a list.
#' It can optionally be initialized with one or more elements.
#'
#' @param ... Optional elements to initialize the list object.
#' @return A new instance of the `.vector` class wrapped in a list.
#' @examples
#' # Create an empty list object
#' l1 <- newList()
#' l1$length() # Returns the length of the list: 0
#'
#' # Create a list object with initial elements
#' l2 <- newList("a", 1, TRUE)
#' l2$get() # Returns the list ["a", 1, TRUE]
#' l2$length() # Returns the length of the list: 3
#'
#' # Get values from the list
#' l2$get(2) # Returns the element at index 2: 1
#' l2$get("a") # Returns the element with name "a": NULL
#' l2$get(4) # Returns NULL since the list has no element at index 4
#'
#' @export
newList <- function(...) {
  lista <- newVec(list(...))

  LENGTH <- function() lista$length()

  get <- function(name = "", index = NA) {
    if (is.numeric(name)) {
      index <- name
      name <- ""
    }
    if (name != "") {
      item <- lista$get()[[name]]
      if (is.null(item)) {
        return(NULL)
      }
      return(item)
    }
    if (is.na(index)) {
      return(lista$get())
    }
    item <- lista$get()[[index]]
    if (is.null(item)) {
      return(NULL)
    }
    return(item)
  }

  set <- function(x, name = "", index = NA) {
    if (is.numeric(name)) {
      index <- name
      name <- ""
    }

    if (is.null(x)) x <- null()
    if (name != "") {
      lst <- lista$get()
      lst[[name]] <- x
      lista$set(lst)
    } else if (is.na(index)) {
      lista$set(x)
    } else {
      lst <- lista$get()
      lst[[index]] <- x
      lista$set(lst)
    }
  }

  add <- function(x) set(x, index = LENGTH() + 1)

  iter <- function(cyclic = FALSE, defVal = NULL) iterList(get(), cyclic, defVal)

  clone <- function() {
    lst <- newList()
    if (LENGTH() > 0) {
      this.lista <- lista$get()
      names <- names(this.lista)
      for (i in 1:LENGTH()) {
        item <- this.lista[[i]]
        ### Recordar crear una función para copiar items
        if (names[i] == "") {
          lst$add(item)
        } else {
          lst$set(item, name = names[i])
        }
      }
    }
    return(lst)
  }
  return(list(get = get, set = set, add = add, length = LENGTH, iter = iter, clone = clone, clonable = "clonable.0000000WJ3bLybo9e7EvjO8mFmdiHKTX3lh3BYsPiPst"))
}

#' Create a new function object
#'
#' This function creates a new function object by generating a random file name and
#' defining a function code based on the provided arguments.
#'
#' @param ... Arguments to be used as the function's arguments.
#' @return A list containing methods to manipulate and execute the generated function.
#' @examples
#' # Create a new function object
#' f <- newFunction(x, y, z)
#'
#' # Add code to the function
#' f$add("result <- x + y * z")
#'
#' # Get the generated code
#' f$getCode()
#' # Returns the code:
#' # "function(x, y, z) {
#' #    result <- x + y * z
#' # }"
#'
#' # Compile and execute the function
#' result <- f$run(2, 3, 4)
#' # Returns the result: 14
#'
#' @export
newFunction <- function(...) {
  file <- pst(randFile(), ".R")
  while (file.exists(file)) file <- pst(randFile(), ".R")
  code <- newVec(pst("function(", join(..., sep = ", "), "){"))
  add <- function(...) code$add(join(...))
  getCode <- function() join(code$get(), "}", sep = "\n")
  getFunction <- function() {
    write(getCode(), file)
    fun <- source(file)$value
    unlink(file)
    return(fun)
  }
  run <- function(...) getFunction()(...)
  return(list(add = add, getCode = getCode, getFunction = getFunction, run = run, file = file))
}

#' Set or retrieve a global variable
#'
#' This function allows you to set or retrieve a global variable. Global variables
#' are stored in a persistent list, which is stored as an internal object.
#'
#' @param name The name of the global variable.
#' @param ... Optional value to set the global variable. If not provided,
#'   retrieves the current value of the global variable.
#' @return If no value is provided, returns the current value of the global variable.
#'   If a value is provided, sets the global variable to that value.
#' @examples
#' # Set a global variable
#' globalVar("count", 10)
#'
#' # Retrieve the value of the global variable
#' globalVar("count")
#' # Returns: 10
#'
#' # Update the value of the global variable
#' globalVar("count", 15)
#'
#' # Retrieve the updated value of the global variable
#' globalVar("count")
#' # Returns: 15
#'
#' @export
globalVar <- function(name, ...) {
  TRY(
    is.list(list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57),
    list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57 <<- list()
  )
  if (base::is.null(
    list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57$
      x00000000Izt1yRF1SYvtSO2AHge1WegOIYLzoOn
  )) {
    list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57$
      x00000000Izt1yRF1SYvtSO2AHge1WegOIYLzoOn <<- list()
  }

  x <- list(...)
  if (length(x) == 0) {
    return(list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57$
      x00000000Izt1yRF1SYvtSO2AHge1WegOIYLzoOn[[name]])
  }
  list.0000007ZSX0szBnmgtrcptzdLCQWesqEafV3Sejilsn57$
    x00000000Izt1yRF1SYvtSO2AHge1WegOIYLzoOn[[name]] <<- x[[1]]
}

newGlobalParam <- function(ID, DEFAULT) {
  if (base::is.null(globalVar(ID))) {
    Names <- newVec()
    List <- newList()
    DEFAULT(function(name, x) {
      Names$add(name)
      List$set(x, name = name)
    })
    globalVar(ID, list(Names = Names, List = List))
  }
  return(
    function(name, ...) {
      if (name == "\\default.values") {
        globalVar(ID, NULL)
        print("Default values were set")
      } else if (name == "\\get.list") {
        return(globalVar(ID)$List$get())
      } else if (name == "\\get.names") {
        return(globalVar(ID)$Names$get())
      } else if (name == "\\get.code") {
        names <- globalVar(ID)$Names$get()
        return(join(paste("if(is.nasd(", names, "))", names, "<-mlst$", names, "", sep = ""), sep = "; "))
      } else {
        if (length(list(...)) == 0) {
          return(globalVar(ID)$List$get(name))
        }
        globalVar(ID)$List$set(..., name)
      }
    }
  )
}

#' Create a counter object
#'
#' This function creates a new counter object that can be used to track and
#' manipulate a numeric count. The counter can be incremented or decremented
#' using the `plus` and `less` functions respectively. The current count can
#' be retrieved using the `get` function.
#'
#' @param start The starting value of the counter. Default is NA.
#' @param step The increment or decrement step value. Default is 1.
#' @param limits A numeric vector indicating the lower and upper limits of
#'   the counter. If specified, the counter will wrap around within the limits.
#'   Default is NA, indicating no limits.
#' @return A new counter object.
#' @examples
#' # Create a counter with default values
#' count <- newCount()
#' count$get() # Returns the current count: 0
#' count$plus() # Increment the count
#' count$get() # Returns the updated count: 1
#' count$less() # Decrement the count
#' count$get() # Returns the updated count: 0
#'
#' # Create a counter with custom values
#' count <- newCount(start = 5, step = 2, limits = c(0, 9))
#' count$get() # Returns the current count: 5
#' count$plus() # Increment the count with wrap-around
#' count$get() # Returns the updated count: 7
#' count$less() # Decrement the count with wrap-around
#' count$get() # Returns the updated count: 5
#'
#' @export
newCount <- function(start = NA, step = 1, limits = NA) {
  if (is.na(limits[1])) {
    if (is.na(start)) start <- 0
    cont <- newVec(start)
    plus <- function() cont$set(cont$get() + step)
    less <- function() cont$set(cont$get() - step)
  } else {
    mod <- limits[2] - limits[1] + 1
    start <- IF(
      is.na(start), limits[1],
      limits[1] + ((start - limits[1]) %% mod)
    )
    cont <- newVec(start)
    plus <- function() cont$set(limits[1] + ((cont$get() + step - limits[1]) %% mod))
    less <- function() cont$set(limits[1] + ((cont$get() - step - limits[1]) %% mod))
  }
  get <- function() cont$get()
  getPlus <- function() {
    GET <- get()
    plus()
    return(GET)
  }
  getLess <- function() {
    GET <- get()
    less()
    return(GET)
  }
  plusGet <- function() {
    plus()
    return(get())
  }
  lessGet <- function() {
    less()
    return(get())
  }
  restart <- function() {
    if (is.na(limits[1])) {
      cont$set(IF(is.na(start), 0, start))
    } else {
      cont$set(IF(
        is.na(start), limits[1],
        limits[1] + ((start - limits[1]) %% (limits[2] - limits[1] + 1))
      ))
    }
  }
  return(list(get = get, plus = plus, less = less, getPlus = getPlus, getLess = getLess, plusGet = plusGet, lessGet = lessGet, restart = restart))
}

#' Create an iterator for a list
#'
#' This function creates an iterator object that allows iterating over the elements
#' of a list. The iterator provides methods for accessing the next element (`Next`),
#' the previous element (`Prev`), the current element (`This`), checking if there
#' are more elements (`HasNext`), and restarting the iteration (`Restart`).
#'
#' @param lista The list to iterate over.
#' @param cyclic Logical indicating whether the iteration should be cyclic, i.e.,
#'   whether it should wrap around to the beginning of the list after reaching the end.
#'   Default is FALSE.
#' @param defVal The default value to return when there are no more elements in the
#'   list. Default is NULL.
#' @param decreasing Logical indicating whether the iteration should be in decreasing
#'   order. If TRUE, the iterator starts from the last element and moves towards the
#'   first element. Default is FALSE, indicating increasing order.
#' @return An iterator object for the list.
#' @examples
#' # Create an iterator for a list
#' lista <- list("a", "b", "c")
#' iter <- iterList(lista)
#'
#' # Iterate over the list
#' iter$HasNext() # Returns TRUE
#' iter$Next() # Returns "a"
#' iter$Next() # Returns "b"
#' iter$Next() # Returns "c"
#' iter$Next() # Returns NULL (end of list)
#' iter$HasNext() # Returns FALSE
#'
#' @export
iterList <- function(lista, cyclic = FALSE, defVal = NULL, decreasing = F) {
  if (length(lista) > 0) {
    index <- newCount(
      start = IF(decreasing, length(lista), 1),
      limits = IF(cyclic, c(1, length(lista)), NA)
    )
  } else {
    index <- newCount(1)
  }
  HasNext <- function() index$get() > 0 & index$get() <= length(lista)
  if (decreasing) {
    Next <- function() IF(HasNext(), lista[[index$getLess()]], defVal)
    Prev <- function() IF(HasNext(), lista[[index$getPlus()]], defVal)
  } else {
    Next <- function() IF(HasNext(), lista[[index$getPlus()]], defVal)
    Prev <- function() IF(HasNext(), lista[[index$getLess()]], defVal)
  }
  This <- function() IF(HasNext(), lista[[index$get()]], defVal)
  Restart <- function() index$restart()
  return(list(HasNext = HasNext, Next = Next, Prev = Prev, This = This, Restart = Restart))
}

#' Create an iterator for a vector
#'
#' This function creates an iterator object that allows iterating over the elements
#' of a vector. The iterator provides methods for accessing the next element (`Next`),
#' the previous element (`Prev`), the current element (`This`), checking if there
#' are more elements (`HasNext`), and restarting the iteration (`Restart`).
#'
#' @param vector The vector to iterate over.
#' @param cyclic Logical indicating whether the iteration should be cyclic, i.e.,
#'   whether it should wrap around to the beginning of the vector after reaching the end.
#'   Default is FALSE.
#' @param defVal The default value to return when there are no more elements in the
#'   vector. Default is NA.
#' @param decreasing Logical indicating whether the iteration should be in decreasing
#'   order. If TRUE, the iterator starts from the last element and moves towards the
#'   first element. Default is FALSE, indicating increasing order.
#' @return An iterator object for the vector.
#' @examples
#' # Create an iterator for a vector
#' vector <- c(1, 2, 3, 4, 5)
#' iter <- iterVector(vector)
#'
#' # Iterate over the vector
#' iter$HasNext() # Returns TRUE
#' iter$Next() # Returns 1
#' iter$Next() # Returns 2
#' iter$Next() # Returns 3
#' iter$Next() # Returns 4
#' iter$Next() # Returns 5
#' iter$Next() # Returns NA (end of vector)
#' iter$HasNext() # Returns FALSE
#'
#' @export
iterVector <- function(vector, cyclic = FALSE, defVal = NA, decreasing = F) {
  if (length(vector) > 0) {
    index <- newCount(
      start = IF(decreasing, length(vector), 1),
      limits = IF(cyclic, c(1, length(vector)), NA)
    )
  } else {
    index <- newCount(1)
  }
  HasNext <- function() index$get() > 0 & index$get() <= length(vector)
  if (decreasing) {
    Next <- function() IF(HasNext(), vector[index$getLess()], defVal)
    Prev <- function() IF(HasNext(), vector[index$getPlus()], defVal)
  } else {
    Next <- function() IF(HasNext(), vector[index$getPlus()], defVal)
    Prev <- function() IF(HasNext(), vector[index$getLess()], defVal)
  }
  This <- function() IF(HasNext(), vector[index$get()], defVal)
  Restart <- function() index$restart()
  return(list(HasNext = HasNext, Next = Next, Prev = Prev, This = This, Restart = Restart))
}

#' Create an iterator for a color object
#'
#' This function creates an iterator object that allows iterating over the elements
#' of a color object. The color object should be a list containing color values.
#' The iterator provides methods for accessing the next element (`Next`), the previous
#' element (`Prev`), the current element (`This`), checking if there are more elements
#' (`HasNext`), and restarting the iteration (`Restart`).
#'
#' @param col The color object to iterate over.
#' @param cyclic Logical indicating whether the iteration should be cyclic, i.e.,
#'   whether it should wrap around to the beginning of the color object after reaching
#'   the end. Default is FALSE.
#' @param defVal The default value to return when there are no more elements in the
#'   color object. Default is NULL.
#' @param decreasing Logical indicating whether the iteration should be in decreasing
#'   order. If TRUE, the iterator starts from the last element and moves towards the
#'   first element. Default is FALSE, indicating increasing order.
#' @return An iterator object for the color object.
#' @examples
#' # Create an iterator for a color object
#' col <- list("#FF0000", "#00FF00", "#0000FF")
#' iter <- iterCol(col)
#'
#' # Iterate over the color object
#' iter$HasNext() # Returns TRUE
#' iter$Next() # Returns "#FF0000"
#' iter$Next() # Returns "#00FF00"
#' iter$Next() # Returns "#0000FF"
#' iter$Next() # Returns NULL (end of color object)
#' iter$HasNext() # Returns FALSE
#'
#' @export
iterCol <- function(col, cyclic = FALSE, defVal = NULL, decreasing = F) {
  if (is.color.class(col)) col <- list(col)
  iterList(lista = col, cyclic = cyclic, defVal = defVal, decreasing = decreasing)
}

#' Apply a function to each element of a vector
#'
#' This function applies a specified function to each element of a vector and
#' returns a new vector with the results.
#'
#' @param fun The function to apply to each element of the vector.
#' @param x The input vector.
#' @param ... Additional arguments to be passed to the function.
#' @return A new vector with the results of applying the function to each element of `x`.
#' @examples
#' # Define a function to calculate the square of a number
#' square <- function(x) x^2
#'
#' # Apply the square function to each element of a vector
#' vec <- c(1, 2, 3, 4, 5)
#' result <- funVec(square, vec)
#' result # Returns [1, 4, 9, 16, 25]
#'
#' @export
funVec <- function(fun, x, ...) {
  vec <- NULL
  for (i in 1:length(x)) {
    vec[i] <- fun(x[i], ...)
  }
  return(vec)
}

#' Apply a function to each element of a list
#'
#' This function applies a specified function to each element of a list and returns
#' a new list with the results.
#'
#' @param fun The function to apply to each element of the list.
#' @param x The input list.
#' @param ... Additional arguments to be passed to the function.
#' @return A new list with the results of applying the function to each element of `x`.
#' @examples
#' # Define a function to calculate the square of a number
#' square <- function(x) x^2
#'
#' # Apply the square function to each element of a list
#' lst <- list(1, 2, 3, 4, 5)
#' result <- funList(square, lst)
#' result # Returns [1, 4, 9, 16, 25]
#'
#' @export
funList <- function(fun, x, ...) {
  lis <- list()
  for (i in 1:length(x)) {
    lis[[i]] <- fun(x[i], ...)
  }
  return(lis)
}

#' Interpolate values on a line segment.
#'
#' This function interpolates values on a line segment defined by x and y coordinates. Given a set of x-coordinates, y-coordinates, and target values, the function calculates the interpolated values at the target points on the line segment.
#'
#' @param x The x-coordinates of the line segment.
#' @param y The y-coordinates of the line segment.
#' @param z The target points for interpolation.
#' @return A numeric vector of interpolated values.
#' @examples
#' x <- c(0, 1, 2, 3)
#' y <- c(0, 2, 4, 6)
#' z <- c(0.5, 1.5, 2.5)
#' interpolated <- linesInter(x, y, z)
#' interpolated
#' # Output: 1 3 5
#' @export
linesInter <- function(x, y, z) {
  ret <- rep(NA, length(z))
  for (i in 1:length(z)) {
    if (z[i] >= x[1] & z[i] <= x[length(x)]) {
      ret[i] <- 0
      for (j in 1:length(x)) {
        if (x[j] <= z[i]) a <- j
        if (x[j] >= z[i]) {
          b <- j
          break
        }
      }
      ret[i] <- if (a == b) y[a] else reparam(z[i], c(x[a], x[b]), c(y[a], y[b]))
    }
  }
  return(ret)
}

#' Calculate interpolated values for multiple line segments.
#'
#' This function calculates interpolated values for multiple line segments defined by X and Y coordinates. Given a list of x-coordinates and y-coordinates, the function calculates the interpolated values at common points along the line segments. It returns a list containing the interpolated values, along with additional summary statistics.
#'
#' @param X A list of x-coordinates for the line segments.
#' @param Y A list of y-coordinates for the line segments.
#' @return A list containing the interpolated values and additional summary statistics.
#' @examples
#' x1 <- c(0, 1, 2, 3)
#' y1 <- c(0, 2, 4, 6)
#' x2 <- c(1, 2, 3, 4)
#' y2 <- c(2, 4, 6, 8)
#' interpolated <- tableLinesInter(list(x1, x2), list(y1, y2))
#' interpolated$z # Common points along the line segments
#' interpolated$tab # Interpolated values for each line segment at the common points
#' interpolated$min # Minimum interpolated value at each common point
#' interpolated$max # Maximum interpolated value at each common point
#' interpolated$mean # Mean interpolated value at each common point
#' interpolated$sd # Standard deviation of interpolated values at each common point
#' @export
tableLinesInter <- function(X, Y) {
  z <- NULL
  for (i in 1:length(X)) {
    notNA <- !is.na(X[[i]]) & !is.na(Y[[i]])
    X[[i]] <- X[[i]][notNA]
    Y[[i]] <- Y[[i]][notNA]
    ord <- order(X[[i]])
    X[[i]] <- X[[i]][ord]
    Y[[i]] <- Y[[i]][ord]
    z <- c(z, X[[i]])
  }
  z <- z[order(z)]
  z <- unique(z)
  tab <- NULL
  for (i in 1:length(X)) {
    tab <- cbind(tab, linesInter(X[[i]], Y[[i]], z))
  }
  return(list(
    z = z, tab = tab,
    min = apply(tab, 1, min, na.rm = T),
    max = apply(tab, 1, max, na.rm = T),
    mean = apply(tab, 1, mean, na.rm = F),
    sd = apply(tab, 1, sd, na.rm = F)
  ))
}

#' Perform benchmark testing of functions X and Y.
#'
#' This function performs benchmark testing of two functions, X and Y, by measuring the execution time of each function. The execution time is measured using the `getSeconds` function. The benchmarking is repeated `n` times, and the execution times for X and Y are recorded. The results are returned as a list with two vectors, `x` and `y`, containing the execution times for X and Y, respectively.
#'
#' @param n The number of times to repeat the benchmark.
#' @param X The function to benchmark.
#' @param Y The function to benchmark.
#' @return A list containing two vectors of execution times for X and Y.
#' @examples
#' fun1 <- function(x) sqrt(x)
#' fun2 <- function(x) log(x)
#' results <- test(10, fun1, fun2, 100, 1000)
#' results$x # Execution times for function fun1
#' results$y # Execution times for function fun2
#' @export
test <- function(n, X, Y, ...) {
  v <- newVec()
  for (i in 1:(2 * n)) {
    a <- getSeconds()
    X(...)
    b <- getSeconds()
    Y(...)
    c <- getSeconds()
    v$add(b - a)
    v$add(c - b)
  }
  return(list(x = v$get()[2 * (1:n) - 1], y = v$get()[2 * (1:n)]))
}

#' Find the optimal parameter values for a given function within a specified interval.
#'
#' This function finds the optimal parameter values for a given function by performing optimization using the `optimize` function. The optimization is performed for each parameter specified in the `param` argument within the corresponding interval specified in the `interval` argument. The optimal parameter values are returned as a list along with the minimum value of the function.
#'
#' @param fun The function to optimize.
#' @param param A character vector specifying the parameter names.
#' @param interval A list of intervals for each parameter. If only one interval is provided, it is recycled for all parameters.
#' @return A list containing the optimal parameter values and the minimum value of the function.
#' @examples
#' fun <- function(x, y) x^2 + y^2
#' result <- optima(fun, c("x", "y"), list(c(-1, 1), c(-2, 2)))
#' result$min # Minimum value of the function
#' result$x # Optimal value for parameter x
#' result$y # Optimal value for parameter y
#' @export
optima <- function(fun, param, interval, ...) {
  if (!is.list(interval)) interval <- list(interval)
  code <- join("\\optimitz", param, listToVec(interval, 1:length(interval)), sep = "_")
  op <- globalVar(code)
  if (!base::is.null(op)) {
    return(op(fun, interval, ...))
  }
  n <- length(param)
  optim <- newFunction("fun, interval, ...")
  jprm <- join(pst(param, "=", param), sep = ", ")
  optim$add("f0<-function(", join(param, sep = ", "), ") fun(", jprm, ", ...)")
  if (n > 1) {
    for (i in 1:(n - 1)) {
      parm <- param[(i + 1):n]
      optim$add(
        "f", i, "<-function(", join(parm, sep = ", "), ") optimize(f", i - 1, ", ",
        join(pst(parm, "=", parm), sep = ", "), ", interval=interval[[", i, "]])$objective"
      )
    }
  }
  optim$add(param[n], "<-optimize(f", n - 1, ", interval=interval[[", n, "]])$minimum")
  if (n > 1) {
    for (i in (n - 1):1) {
      parm <- param[(i + 1):n]
      optim$add(
        param[i], "<-optimize(f", i - 1, ", ",
        join(pst(parm, "=", parm), sep = ", "), ", interval=interval[[", i, "]])$minimum"
      )
    }
  }
  optim$add("list(", jprm, ", min=f0(", jprm, "))")
  op <- optim$getFunction()
  globalVar(code, op)
  op(fun, interval, ...)
}

#' Find the optimal parameter values for a given function within a specified interval using an adaptive optimization algorithm.
#'
#' This function finds the optimal parameter values for a given function using an adaptive optimization algorithm. It performs optimization for each parameter specified in the `names` argument within the corresponding interval specified in the `interval` argument. The optimization algorithm adjusts the precision of the intervals based on the variation in the function values to achieve accurate results. The optimal parameter values, along with other information such as the number of iterations and precision, are returned as a list.
#'
#' @param f The function to optimize.
#' @param interval A list of intervals for each parameter.
#' @param names A character vector specifying the names of the parameters.
#' @param min.partitions The minimum number of partitions for adaptive optimization.
#' @param max.partitions The maximum number of partitions for adaptive optimization.
#' @param ... Additional arguments passed to the function `f`.
#' @return A list containing information about the optimization process, including the number of iterations, precision, and the optimal parameter values.
#' @examples
#' fun <- function(x, y) x^2 + y^2
#' result <- optimizer(fun, list(c(-1, 1), c(-2, 2)), c("x", "y"))
#' result$loops # Number of iterations
#' result$fmin.prescision # Precision of the minimum value
#' result$var.prescision # Precision of each parameter
#' result$optim # Optimal parameter values and minimum value
#' result$interval # Final interval for each parameter
#' @export
optimizer <- function(f, interval, names = NULL, min.partitions = 5, max.partitions = min.partitions + length(interval), ...) {
  n <- length(interval)
  code <- join("\\optimizer", n, names, sep = "_")
  op <- globalVar(code)
  if (is.null(op)) {
    i <- 1:n
    fun <- newFunction("fn,
         interval,
         names,
         var.precision,
         min.partitions,
         max.partitions,
         lst,
         ...")
    fun$add("fmin_prev<-lst$get(\"fmin_prev\")")
    fun$add("fmin<-lst$get(\"fmin\")")
    fun$add("loops<-0")
    fun$add("partitions<-min.partitions#")
    fun$add("var.dif<-2*10^-(var.precision+1)")
    fun$add("N<-2*partitions+1")
    fun$add("med<-2*(1:partitions)")
    fun$add(pst("ival", i, "<-interval[[", i, "]]"), sep = "\n")
    fun$add(pst("stop", i, "<-F"), sep = "\n")
    fun$add("newmin<-F")
    fun$add("while(T){")
    fun$add(pst("  stop", i, "<-if(diff(ival", i, ")<var.dif){
    if(!stop", i, "){
      index", i, "<-1
      seq", i, "<-(ival", i, "[1]+ival", i, "[2])/2
    }
    T
  }else{
    index", i, "<-med
    seq", i, "<-mseq(ival", i, "[1],ival", i, "[2],N)
    if(length(seq", i, ")<N){
      index", i, "<-1
      seq", i, "<-(ival", i, "[1]+ival", i, "[2])/2
      T
    }else{
      F
    }
  }"), sep = "\n")
    fun$add("  if(", join(pst("stop", i), sep = " & "), ") break")
    fun$add(join(pst("  for(i", i, " in index", i, "){")))
    fun$add("    loops<-loops+1")
    vars <- if (is.null(names)) {
      join(pst("seq", i, "[i", i, "]"), sep = ", ")
    } else {
      join(pst(names, "=seq", i, "[i", i, "]"), sep = ", ")
    }
    fun$add("    f<-fn(", vars, ", ...)")
    fun$add("    if(f<fmin){")
    fun$add(pst("      i", i, "min<-i", i), sep = "\n")
    fun$add("      fmin_prev<-fmin")
    fun$add("      fmin<-f")
    fun$add("      newmin<-T")
    fun$add("    }")
    fun$add(rep("  }", n))
    fun$add("  if(newmin){")
    fun$add(pst("    if(!stop", i, ") ival", i, "<-seq", i, "[c(i", i, "min-1, i", i, "min+1)]"), sep = "\n")
    fun$add("    newmin<-F")
    fun$add("  }else{")
    fun$add("    if(partitions==max.partitions) break")
    fun$add("    partitions<-partitions+1")
    fun$add("    N<-2*partitions+1")
    fun$add("    med<-2*(1:partitions)")
    fun$add("  }")
    fun$add("}")
    names <- isnt.null(names, pst("x", i))
    fun$add("var.prescision=c(", join(pst("trunc(-log(diff(ival", i, ")/2)/log(10))"), sep = ","), ")")
    fun$add("names(var.prescision)<-c(", join(pst("\"", names, "\""), sep = ","), ")")
    fun$add("list(loops=loops,
     fmin_prev=fmin_prev,
     fmin=fmin,
     fmin.prescision=trunc(-log(fmin_prev-fmin)/log(10)),
     var.prescision=var.prescision,
     interval=list(", join(pst(names, "=ival", i), sep = ", "), "),
     optim=list(", join(pst(names, "=mean(ival", i, ")"), sep = ", "), ", min=fmin))")
    op <- fun$getFunction()
    globalVar(code, op)
  }
  lst <- newList()
  lst$set(0, "cumloops")
  lst$set(NA, "fmin_prev")
  lst$set(Inf, "fmin")
  lst$set(interval, "interval")
  optim <- function(var.precision = 10) {
    lista <- op(f, lst$get("interval"), names, var.precision, min.partitions, max.partitions, lst, ...)
    lst$set(lista$fmin_prev, "fmin_prev")
    lst$set(lista$fmin, "fmin")
    lst$set(lista$interval, "interval")
    lst$set(lista$loops + lst$get("cumloops"), "cumloops")
    list(
      cumulateloops = lst$get("cumloops"),
      loops = lista$loops,
      fmin.prescision = lista$fmin.prescision,
      var.prescision = lista$var.prescision,
      optim = lista$optim,
      interval = lista$interval
    )
  }
  optim
}

#' Generate a simulated sample from a given dataset or empirical cumulative distribution function.
#'
#' This function generates a simulated sample from a given dataset or empirical cumulative distribution function (ecdf). If the argument `Nsim` is specified, it returns a simulated sample of size `Nsim` using the ecdf of the input dataset `x`. If `Nsim` is not provided, it returns a closure function that takes `Nsim` as an argument and generates a simulated sample of size `Nsim` using the stored ecdf.
#'
#' @param x The input dataset.
#' @param Nsim The size of the simulated sample.
#' @return If `Nsim` is provided, a simulated sample of size `Nsim`. If `Nsim` is not provided, a closure function for generating simulated samples.
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' sample <- simSample(data)
#' sample(10) # Generate a simulated sample of size 10
#' sample(100) # Generate a simulated sample of size 100
#' @export
simSample <- function(x, Nsim = NA) {
  if (!is.na(Nsim)) {
    return(as.numeric(quantile(ecdf(x), runif(Nsim))))
  }
  ecdf <- ecdf(x)
  function(Nsim) as.numeric(quantile(ecdf, runif(Nsim)))
}

#' Generate simulated samples from a given table or dataset.
#'
#' This function generates simulated samples from a given table or dataset. If the argument `Nsim` is specified, it returns a simulated table where each column contains a simulated sample of size `Nsim` based on the corresponding column of the input table `tab`. If `Nsim` is not provided, it returns a closure function that takes `Nsim` as an argument and generates a simulated table with samples of size `Nsim` based on the stored samples from the input table.
#'
#' @param tab The input table or dataset.
#' @param Nsim The size of the simulated samples.
#' @return If `Nsim` is provided, a simulated table where each column contains a simulated sample of size `Nsim`. If `Nsim` is not provided, a closure function for generating simulated tables.
#' @examples
#' data <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6))
#' sim <- simTable(data)
#' sim(10) # Generate a simulated table with samples of size 10
#' sim(100) # Generate a simulated table with samples of size 100
#' @export
simTable <- function(tab, Nsim = NA) {
  if (is.null(ncol(tab))) tab <- cbind(tab)
  if (!is.na(Nsim)) {
    simtab <- NULL
    for (i in 1:ncol(tab)) {
      simtab <- cbind(simtab, simSample(tab[, i], Nsim))
    }
    colnames(simtab) <- colnames(tab)
    return(as.data.frame(simtab))
  }
  simsamp <- NULL
  for (i in 1:ncol(tab)) {
    simsamp[[i]] <- simSample(tab[, i])
  }
  function(Nsim) {
    simtab <- NULL
    for (i in 1:ncol(tab)) {
      simtab <- cbind(simtab, simsamp[[i]](Nsim))
    }
    colnames(simtab) <- colnames(tab)
    return(as.data.frame(simtab))
  }
}

#' Calculate confidence interval for a numeric vector or matrix.
#'
#' This function calculates the confidence interval for a given numeric vector or matrix. If the input is a matrix, it returns a list of confidence intervals for each column of the matrix. If the input is a vector, it returns the confidence interval for the vector.
#'
#' @param x The input numeric vector or matrix.
#' @param level The confidence level, expressed as a percentage (e.g., 95 for a 95% confidence level).
#' @return If the input is a matrix, a list of confidence intervals for each column. If the input is a vector, a list with 'inf' and 'sup' indicating the lower and upper bounds of the confidence interval.
#' @examples
#' data <- c(1, 2, 3, 4, 5)
#' interval(data, 95) # Calculate the 95% confidence interval for the vector
#' matrix <- matrix(1:10, ncol = 2)
#' interval(matrix, 90) # Calculate the 90% confidence interval for each column of the matrix
#' @export
interval <- function(x, level) {
  if (is.matrix(x)) {
    inf <- NULL
    sup <- NULL
    for (i in 1:ncol(x)) {
      interv <- interval(x[, i], level)
      inf <- c(inf, interv$inf)
      sup <- c(sup, interv$sup)
    }
    return(list(inf = inf, sup = sup))
  } else {
    alpha <- 1 - level / 100
    inf <- quantile(x, p = alpha / 2)
    sup <- quantile(x, p = 1 - alpha / 2)
    return(list(inf = inf, sup = sup))
  }
}
