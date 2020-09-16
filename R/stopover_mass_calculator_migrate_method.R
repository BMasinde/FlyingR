# @title Stopover mass calculator
# @description During stop-overs birds replenish fat mass. Using simplifications
# from Lindstöm 1991.
# @name stopover.mass.calculator.migrate
# @param migrate_obj Results from function migrate
# @param duration Days spent at stopover
# @param ... extra-arguments
# @return body mass, fat mass, fat fraction,

# stopover.mass.calculator <- function(migrate_obj, duration = 1L, ...) {
#  UseMethod("stopover.mass.calculator", migrate_obj)
# }
#
# stopover.mass.calculator.migrate <-
#   function(migrate_obj, duration = 1L) {
#     # migrate_obj is of class migrate?
#     if (class(migrate_obj)[2] != migrate) {
#       stop("migrate_obj should be results from function migrate")
#     }
#
#     # get lean mass
#
#     return(migrate_obj)
#   }
