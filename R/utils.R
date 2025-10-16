find_variable_in_nested_list <- function(nested_list, target_variable_name) {
  # Initialize a variable to store the found element, if any
  found_element <- NULL

  # Iterate through each element in the current level of the list
  for (item in nested_list) {
    # Check if the current item is a list (indicating further nesting)
    if (is.list(item)) {
      # If it's a list, recursively call the function to search within it
      found_element <- find_variable_in_nested_list(item, target_variable_name)
      # If the variable is found in the nested list, return it immediately
      if (!is.null(found_element)) {
        return(found_element)
      }
    } else {
      # If it's not a list, check if it's the target variable
      # This assumes the target is a named element within a list
      if (!is.null(names(nested_list)) && target_variable_name %in% names(nested_list)) {
        # If the current item's name matches the target variable name
        # and the item itself is part of the current 'nested_list'
        # (this handles cases where the target_variable_name might be a value, not a name)
        if (identical(item, nested_list[[target_variable_name]])) {
          return(item)
        }
      }
    }
  }
  # If the loop finishes and the variable is not found in this level or its nested lists
  return(found_element)
}


#' Search a Nested List to Get a List and its Respective Elements
#'
#' @param nested_list a list with nested elements
#' @param target_name identifier for element and its associated elements
#'
#' @returns list
#' @export
#'
#' @examples nested_list_example <- list(
#' teamA = list(
#' offense = list(
#'   quarterback = "Patrick Mahomes",
#'   running_back = "Isiah Pacheco",
#'   wide_receivers = c("Rashee Rice", "Hollywood Brown")
#' ),
#' defense = list(
#'   linebacker = "Nick Bolton",
#'   cornerbacks = c("Trent McDuffie", "Joshua Williams")
#' )
#' ),
#' teamB = list(
#'   offense = list(
#'     quarterback = "Josh Allen",
#'     running_back = "James Cook",
#'     wide_receivers = c("Stefon Diggs", "Khalil Shakir")
#'   ),
#'   defense = list(
#'     linebacker = "Matt Milano",
#'     cornerbacks = c("Kaiir Elam", "Taron Johnson")
#'   )
#' )
#' )
#' find_list_with_name(nested_list_example, "Josh Allen")
find_list_with_name <- function(nested_list, target_name) {
  # Ensure input is a list
  if (!is.list(nested_list)) {
    return(NULL)
  }

  # If the list has names and one matches target_name, return the list
  if (!is.null(names(nested_list)) && target_name %in% names(nested_list)) {
    return(nested_list[[target_name]])
  }

  # Otherwise, recursively search sublists
  for (item in nested_list) {
    if (is.list(item)) {
      found <- find_list_with_name(item, target_name)
      if (!is.null(found)) {
        return(found)
      }
    }
  }

  # If not found
  return(NULL)
}
