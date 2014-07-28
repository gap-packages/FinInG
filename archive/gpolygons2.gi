#############################################################################
##
##  gpolygons.gi              FinInG package
##                                                              John Bamberg
##                                                              Anton Betten
##                                                              Jan De Beule
##                                                             Philippe Cara
##                                                            Michel Lavrauw
##                                                           Max Neunhoeffer
##
##  Copyright 2014	Colorado State University, Fort Collins
##					Università degli Studi di Padova
##					Universeit Gent
##					University of St. Andrews
##					University of Western Australia, Perth
##                  Vrije Universiteit Brussel
##                 
##
##  Implementation stuff for generalised polygons.
##
#############################################################################

########################################
#
# Things To Do:
#
# - Twisted triality hexagon (test it, but hard to)
# - Iterators for Kantor families, or enumerators maybe
# - iterators for generalised hexagons?
#   at the moment we just take the full set, could be difficult!
# - join and meet operations
# - shadow of elements for models of GHs.
# - VectorSpaceToElement for models of GHs.
#
# Documentation check-list
# - SplitCayleyHexagon
# - TwistedTrialityHexagon
# - EGQByKantorFamily
# - IsKantorFamily
# - IsqClan
# - EGQByqClan
# - BLTSetByqClan
# - EGQByBLTSet
#
########################################

Print(", gpolygons\c");

#############################################################################
#
# Basic methods and iterators
# This section is generic, i.e. if someone constructs a GP and obeys the representation,
# these methods will work.
#
#############################################################################


