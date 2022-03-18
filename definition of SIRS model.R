reactions_intra <- unlist(lapply(seq_len(U),function(i) 
{
  Si <- paste0("S", i)
  Ii <- paste0("I", i)
  Ri <- paste0("R", i)
  Ji <- paste0("J", i)
  Ni <- paste0("(",Si,"+",Ii,"+",Ri,")")
  
  list(
    reaction(
      propensity = paste0("beta * ", Si, " * ", Ii, "/", Ni), 
      effect = setNames(c(-1, +1, +1), c(Si, Ii, Ji)),
      name = paste0("intra_patch_infection_", i)
    ),
    reaction(
      propensity = paste0("gamma * ", Ii),
      effect = setNames(c(-1, +1), c(Ii, Ri)),
      name = paste0("recovery_from_infection_", i)
    ),
    reaction(
      propensity = paste0("rho * ", Ri),
      effect = setNames(c(-1, +1), c(Ri, Si)),
      name = paste0("loss_of_immunity_", i)
    ),
    reaction(
      propensity = paste0("delta * ", Si),
      effect = setNames(c(-1), c(Si)),
      name = paste0("susceptible_death_rate", i)
    ),
    reaction(
      propensity = paste0("delta * ", Ii),
      effect = setNames(c(-1), c(Ii)),
      name = paste0("infected_death_rate", i)
    ),
    reaction(
      propensity = paste0("delta * ", Ri),
      effect = setNames(c(-1), c(Ri)),
      name = paste0("recovered_death_rate", i)
    ),
    reaction(
      propensity = paste0("epsilon * ", Ni),
      effect = setNames(c(+1), c(Si)),
      name = paste0("birth_rate", i)
    ))
}
), recursive = FALSE)
# list of reactions between patches (migration)	
reactions_inter <- unlist(lapply(seq_len(U-1), function(i)
{
  Si <- paste0("S", i)
  Ii <- paste0("I", i)
  Ri <- paste0("R", i)
  Ni <- paste0("(",Si,"+",Ii,"+",Ri,")")
  
  Su <- paste0("S", U)
  Iu <- paste0("I", U)
  Ru <- paste0("R", U)
  Nu <- paste0("(",Su,"+",Iu,"+",Ru,")")
  
  
  list(
    reaction(
      propensity = paste0("mu_in * ", Si),
      effect = setNames(c(-1, +1), c(Si, Su)),
      name = paste0("susceptible_migration_to_large_patch_from_", i)
    ),
    reaction(
      propensity = paste0("mu_in * ", Ii),
      effect = setNames(c(-1, +1), c(Ii, Iu)),
      name = paste0("infected_migration_to_large_patch_from_", i)
    ),
    reaction(
      propensity = paste0("mu_in * ", Ri),
      effect = setNames(c(-1, +1), c(Ri, Ru)),
      name = paste0("recovered_migration_to_large_patch_from_", i)
    ),
    reaction(
      propensity = paste0("mu_out * ", Su),
      effect = setNames(c(-1, +1), c(Su, Si)),
      name = paste0("susceptible_migration_from_large_patch_to_", i)
    ),
    reaction(
      propensity = paste0("mu_out * ", Iu),
      effect = setNames(c(-1, +1), c(Iu, Ii)),
      name = paste0("infected_migration_from_large_patch_to_", i)
    ),
    reaction(
      propensity = paste0("mu_out * ", Ru),
      effect = setNames(c(-1, +1), c(Ru, Ri)),
      name = paste0("recovered_migration_from_large_patch_to_", i)
    )
 )
}
), recursive = FALSE)
