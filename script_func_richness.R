
richness_function = function(sf,grid){
  sf_list=st_intersects(sf,r_grid)
  return(unlist(lapply((t(sf_list)),FUN = length)))
}

grid$richness = richness_function(sf,grid)