#################################################################################################
## DECISION TREE MODEL DIAGRAM
###################################################################################################******************************************************************************

dir.exists(ppsv23_figures_dir)

svg_txt <- grViz("
digraph decision_tree {
  graph [rankdir = TB]

  ## Root node
  decision [label = 'Vaccination\nDecision', shape = box, 
            style = filled, fillcolor = lightblue, 
            width = 4.0, height = 2.0, 
            fontsize = 30]

   ## Node defaults
  node [shape = ellipse, style = filled, fillcolor = grey97,
        width = 4.2, height = 2.2, fixedsize = true,
        fontsize = 35]   

  ## Decision branches
  node [shape = ellipse, style = filled]
  ppsv23 [label = 'PPSV23\nvs\nNo PPSV23']

  decision -> ppsv23

  ## PPSV23 subtree
  ipd_inf [label = 'Invasive\nPneumococcal Disease\n(IPD)', width = 5.2, height = 2.2]
  nonipd_inf [label = 'Non-invasive\nPneumococcal Disease\n(nIPD)', width = 5.2, height = 2.2]
  no_inf [label = 'No Infection', width = 5.2, height = 2.2]
  
  ppsv23 -> ipd_inf
  ppsv23 -> nonipd_inf
  ppsv23 -> no_inf

  ## Invasive: Meningitis, Pneumococcal Pneumonia, Non-meningitis
  meningitis [label = 'Pneumococcal\nMeningitis']
  pneumo [label = 'Pneumococcal\nPneumonia']
  non_mening [label = 'Non-meningitis\nnon-pneumonia']
  
  ipd_inf -> meningitis
  ipd_inf -> pneumo
  ipd_inf -> non_mening

  men_hosp [label = 'Hospitalized']
  seq      [label = 'Sequelae', width = 2.2, height = 2.0]
  noseq    [label = 'No\nSequelae', width = 2.2, height = 2.0]
  dead1    [label = 'Deceased', width = 2.2, height = 2.0]

  meningitis -> men_hosp
  men_hosp -> seq
  men_hosp -> noseq
  men_hosp -> dead1

  pneumo_hosp [label = 'Hospitalized']
  pneumo_alive [label = 'Alive', width = 2.2, height = 2.0]
  pneumo_dead [label = 'Deceased', width = 2.2, height = 2.0]

  pneumo -> pneumo_hosp
  pneumo_hosp -> pneumo_alive
  pneumo_hosp -> pneumo_dead
  
  nm_hosp [label = 'Hospitalized']
  nm_alive [label = 'Alive', width = 2.2, height = 2.0]
  nm_dead [label = 'Deceased', width = 2.2, height = 2.0]

  non_mening -> nm_hosp
  nm_hosp -> nm_alive
  nm_hosp -> nm_dead

  ## Non-invasive: Pneumonia, AOM
  pneumonia [label = 'Non-invasive\nPneumonia']
  aom [label = 'Acute\notitis media\n(AOM)']
  
  nonipd_inf -> pneumonia
  nonipd_inf -> aom
  
  pneu_hosp [label = 'Hospitalized']
  pneu_no_treat [label = 'No treatment']

  pneumonia -> pneu_hosp
  pneumonia -> pneu_no_treat

  pneu_hosp_alive [label = 'Alive', width = 2.2, height = 2.0]
  pneu_hosp_dead [label = 'Deceased', width = 2.2, height = 2.0]

  pneu_hosp -> pneu_hosp_alive
  pneu_hosp -> pneu_hosp_dead
  
  pneu_untreated_alive [label = 'Alive', width = 2.2, height = 2.0]
  pneu_untreated_dead [label = 'Deceased', width = 2.2, height = 2.0]

  pneu_no_treat -> pneu_untreated_alive
  pneu_no_treat -> pneu_untreated_dead

  aom_treated [label = 'Treated at health\ncare facility']
  aom_not_treated [label = 'No treatment']
  
  aom -> aom_treated
  aom -> aom_not_treated
  
  aom_alive1 [label = 'Alive', width = 2.2, height = 2.0]
  aom_alive2 [label = 'Alive', width = 2.2, height = 2.0]
  
  aom_treated -> aom_alive1
  aom_not_treated -> aom_alive2

}") %>%
DiagrammeRsvg::export_svg() 

svg_raw <- charToRaw(svg_txt)
print(length(svg_raw))

rsvg::rsvg_png(
    svg = svg_raw,
    file = file.path(ppsv23_figures_dir, "decisiontree.png")#,
    #width  = 4000,
    #height = 3000
  )

#################################################################################################
## END OF MODULE
#################################################################################################