################## READ ME ###################

### This script requires the 'job' package, which runs in RStudio
### and allows each model fitting 'job' to run simultaneously. I believe 
### that when running multiple jobs at a time, each uses 1 core, so keep 
### that in mind - it's a simple way to run things in parallel. When a 
### job is complete, it will load resulting objects into the environment. 

### I setup four separate jobs below: one fits a 'base' model, which 
### only includes the stand fixed effects and tree-level random effect, 
### the next one runs both size model options (e.g., base + dbh,  base + tree ba),
### then competition variables, then site variables. 
### If you'd rather it all happen in one single job, then go ahead, but it may 
### take longer.

### The models fit here are using the same variables that were selected 
### for the final 'SCPt' model in my thesis. If we want to re-do
### the model selection from scratch (starting with the fixed and random effects)
### then we will have to run a bunch more models than this. Those models are 
### commented out at the bottom of this script. 

### IMPORTANT
### The model objects will be stored locally in the /00.fitmodels/model_objects
### directory, but the files will likely be too big to share on git, so you'll 
### need to share them with me in a compressed folder on box, or some other alternative.


### note: working directory should be set to /mercado_research

require(job)
training.data <- readRDS('00.fitmodels/data/model_fitting_data.rds')

#Jobs ####
#Run these

#model with only tree random effects and stand fixed effects
job::job(import = c(training.data), {
  require(mgcv)
  try(base.model <- gam(bai.cm~stand + s(unique.tree.f, bs = 're'), 
                        family = 'Gamma'(link = log), 
                        data = training.data, method = 'ML'))
  try(saveRDS(base.model, '00.fitmodels/model_objects.1/base_model.rds'))
  
})


#size vars
## dbh, tree basal area
job::job(import = c(training.data), {
  require(mgcv)
  try(size.dbh <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm), 
             family = 'Gamma'(link = log), 
             data = training.data, method = 'ML'))
  try(saveRDS(size.dbh, '00.fitmodels/model_objects.1/size_dbh.rds'))
  
  try(size.tba <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(treeba.cm), 
             family = 'Gamma'(link = log), 
             data = training.data, method = 'ML'))
  try(saveRDS(size.tba, '00.fitmodels/model_objects.1/size_tba.rds'))
})


#competition/density
## cr + bal , cr + bal + BAH
job::job(import = c(training.data), {
  require(mgcv)
  try(comp.ind.crbal <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
                        s(cr, k = 9) + s(bal.pl.ratio), 
                      family = 'Gamma'(link = log), 
                      data = training.data, method = 'ML'))
  try(saveRDS(comp.ind.crbal, '00.fitmodels/model_objects.1/comp_ind_crbal.rds'))
  try(comp.crbalbah <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
                       s(cr, k = 9) + s(bal.pl.ratio) + s(bah.pl), 
                     family = 'Gamma'(link = log), 
                     data = training.data, method = 'ML'))
  try(saveRDS(comp.crbalbah, '00.fitmodels/model_objects.1/comp_crbalbah.rds'))
})






#site models
job::job(import=c(training.data), {
  try(site.slaspel <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
                            s(cr, k = 9) + s(bal.pl.ratio) + s(bah.pl) + 
                            s(slope_pct) + s(asp.trasp) + s(elev_m), 
                          family = 'Gamma'(link = log), 
                          data = training.data, method = 'ML'))
  try(saveRDS(site.slaspel, '00.fitmodels/model_objects.1/site_slaspel.rds'))
  try(site.chili <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
                          s(cr, k = 9) + s(bal.pl.ratio) + s(bah.pl) + 
                          s(heatload), 
                        family = 'Gamma'(link = log), 
                        data = training.data, method = 'ML'))
  try(saveRDS(site.chili, '00.fitmodels/model_objects.1/site_chili.rds'))
  try(site.siteindex <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
                              s(cr, k = 9) + s(bal.pl.ratio) + s(bah.pl) + 
                              s(mean_si), 
                            family = 'Gamma'(link = log), 
                            data = training.data, method = 'ML'))
  try(saveRDS(site.siteindex, '00.fitmodels/model_objects.1/site_siteindex.rds'))
})



# More models #####
##### These are setup to fit every model if re-visiting model selection from from the beginnig.
#### Not doing that at this time - commented out for now
# #size vars
# ## tree ba, DIAMETER
# size.dbh <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm), 
#                 family = 'Gamma'(link = log), 
#                 data = training.data, method = 'ML')
# size2.tba <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(treeba.cm), 
#                  family = 'Gamma'(link = log), 
#                  data = training.data, method = 'ML')
# 
# 
# #competition/density
# # high correlation variables - TPA-DQ, BAH-QMD, CCF-BA
# ##competitive position - cr, bal, dq (dbh-qmd ratio)
# ### all three
# comp.ind.all <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
#                       s(cr, k = 9) + s(bal.pl.ratio) + s(dq.pl.all), 
#     family = 'Gamma'(link = log), 
#     data = training.data, method = 'ML')
# 
# ### pairs
# ####cr + bal
# comp.ind.crbal <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
#                         s(cr, k = 9) + s(bal.pl.ratio), 
#                       family = 'Gamma'(link = log), 
#                       data = training.data, method = 'ML')
# #### cr + dq
# comp.ind.crdq <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
#                         s(cr, k = 9) + s(dq.pl.all), 
#                       family = 'Gamma'(link = log), 
#                       data = training.data, method = 'ML')
# #### dq + bal
# comp.ind.dqbal <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
#                         s(dq.pl.all) + s(bal.pl.ratio), 
#                       family = 'Gamma'(link = log), 
#                       data = training.data, method = 'ML')
# ### individual
# #### cr
# comp.cr <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
#                         s(cr, k = 9), 
#                       family = 'Gamma'(link = log), 
#                       data = training.data, method = 'ML')
# #### bal
# comp.bal <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
#                  s(bal.pl.ratio), 
#                family = 'Gamma'(link = log), 
#                data = training.data, method = 'ML')
# #### dbh-qmd ratio
# comp.dq <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
#                   s(dq.pl.all), 
#                 family = 'Gamma'(link = log), 
#                 data = training.data, method = 'ML')
# 
# #####
# ############ competitive environment - qmd.pl.all, tpa.pl.all, ba.pl
# ### cr + bal ...
# 
# #### cr + BAL + BAH
# comp.crbalbah <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) + 
#                         s(cr, k = 9) + s(bal.pl.ratio) + s(bah.pl), 
#                       family = 'Gamma'(link = log), 
#                       data = training.data, method = 'ML')
# #####
# #### .. + TPA
# comp.crbaltph <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                    s(cr, k = 9) + s(bal.pl.ratio) + s(tph.pl),
#                  family = 'Gamma'(link = log),
#                  data = training.data, method = 'ML')
# #### .. + QMD
# comp.crbalqmd <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                    s(cr, k = 9) + s(bal.pl.ratio) + s(qmd.pl.all.cm),
#                  family = 'Gamma'(link = log),
#                  data = training.data, method = 'ML')
# 
# ### cr + dq ...
# #### cr + dq + BAH
# comp.crdqbah <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                        s(cr, k = 9) + s(dq.pl.all) + s(bah.pl),
#                      family = 'Gamma'(link = log),
#                      data = training.data, method = 'ML')
# 
# #### cr + dq + QMD
# comp.crbalqmd <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                        s(cr, k = 9) + s(dq.pl.all) + s(qmd.pl.all.cm),
#                      family = 'Gamma'(link = log),
#                      data = training.data, method = 'ML')
# 
# 
# ### cr +.. options
# #### cr + BAH
# comp.crbah <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                        s(cr, k = 9) + s(bah.pl),
#                      family = 'Gamma'(link = log),
#                      data = training.data, method = 'ML')
# #### cr + TPH
# comp.crtph <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                        s(cr, k = 9) + s(tph.pl),
#                      family = 'Gamma'(link = log),
#                      data = training.data, method = 'ML')
# 
# #### cr + qmd
# comp.crqmd <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                        s(cr, k = 9) + s(qmd.pl.all.cm),
#                      family = 'Gamma'(link = log),
#                      data = training.data, method = 'ML')
# #### cr + qmd + tph
# comp.crqmdtph <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                     s(cr, k = 9) + s(qmd.pl.all.cm) + s(tph.pl),
#                   family = 'Gamma'(link = log),
#                   data = training.data, method = 'ML')
# 
# ### bal + .. options
# #### bal + BAH
# comp.balbah <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                     s(bal.pl.ratio) + s(bah.pl),
#                   family = 'Gamma'(link = log),
#                   data = training.data, method = 'ML')
# #### bal + TPH
# comp.baltph <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                     s(bal.pl.ratio) + s(tph.pl),
#                   family = 'Gamma'(link = log),
#                   data = training.data, method = 'ML')
# 
# #### bal + qmd
# comp.balqmd <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                     s(bal.pl.ratio) + s(qmd.pl.all.cm),
#                   family = 'Gamma'(link = log),
#                   data = training.data, method = 'ML')
# #### bal + qmd + tph
# comp.balqmdtph <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                      s(bal.pl.ratio) + s(qmd.pl.all.cm) + s(tph.pl),
#                    family = 'Gamma'(link = log),
#                    data = training.data, method = 'ML')
# #### bal + bah + tph
# comp.balbahtph <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                      s(bal.pl.ratio) + s(bah.pl) + s(tph.pl),
#                    family = 'Gamma'(link = log),
#                    data = training.data, method = 'ML')
# #### bal + BAH + qmd
# comp.balbahqmd <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                         s(bal.pl.ratio) + s(bah.pl) + s(qmd.pl.all.cm),
#                       family = 'Gamma'(link = log),
#                       data = training.data, method = 'ML')
# 
# ### dq + ..
# #### dq + BAH
# comp.dqbah <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                      s(dq.pl.all) + s(bah.pl),
#                    family = 'Gamma'(link = log),
#                    data = training.data, method = 'ML')
# 
# #### dq + qmd
# comp.dqqmd <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                      s(dq.pl.all) + s(qmd.pl.all.cm),
#                    family = 'Gamma'(link = log),
#                    data = training.data, method = 'ML')
# #### dq + bah + qmd
# comp.dqbahqmd <- gam(bai.cm~stand + s(unique.tree.f, bs = 're') + s(dbh.cm) +
#                     s(dq.pl.all) + s(bah.pl) + s(qmd.pl.all.cm),
#                   family = 'Gamma'(link = log),
#                   data = training.data, method = 'ML')
# 
# 
# #site variables
# 


