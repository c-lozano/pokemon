# Setup ####
library(tidyverse)
library(tidymodels)
library(modelsummary)
library(sandwich)
library(lmtest)

overwriteFigures <- F # set to T to write new figures to ../figures/


# Part 1 - Descriptive Summary of Data ########################################

## Data Import and Cleaning ####
pokemon <- read_csv('pokemon.csv',show_col_types = FALSE)

pokemon <- pokemon |> 
  mutate(generation=factor(generation,ordered=T),
         type1=factor(type1),
         type2=factor(type2),
         is_legendary=as.logical(is_legendary))

## Data Description ####

### Types ####

twoTypes <- pokemon |> drop_na(type2) |> nrow()

### "Against" Modifiers ####

charizardAgainst <- pokemon |> 
  filter(name=='Charizard') |> 
  select(name, attack, against_steel, against_water)

startersAgainst <- pokemon |> 
  filter(pokedex_number==1 | pokedex_number==4 | pokedex_number==7) |> 
  select(name, type1, attack, against_fire, against_electric, against_grass)

uniqueMultipliers <- pokemon |> 
  select(starts_with('against')) |> 
  t() |> c() |> unique() |> sort()

### Other features ####

missingVals <- pokemon |> 
  select(!type2) |> 
  select(where(~any(is.na(.x)))) |> 
  pivot_longer(everything(),names_to='feature',values_to='val') |> 
  group_by(feature) |> 
  summarise('number of missing values'=sum(is.na(val))) |> 
  bind_rows(tibble(feature='type2','number of missing values'=sum(is.na(pokemon$type2))))


# Part 2 - Trends and Predicting Type #########################################

## Species and Types over Generations ####

### Data Visualizations ####

#### Type-data wrangling and means ####

typeExpand <- pokemon |> 
  select(generation,type1,type2) |> 
  pivot_longer(starts_with('type'), names_to='rank', values_to='type') |> 
  drop_na()

genTypes <- as_tibble(prop.table(table(generation=typeExpand$generation,type=typeExpand$type),1))

names(genTypes)[3] <- 'prop'

genTypeMeans <- genTypes |> 
  group_by(type) |> 
  summarise(meanProp=mean(prop),se=sd(prop)/sqrt(n()))

#### Plotting ####

propTypesOverGensCombined <- genTypes |>
  ggplot(aes(x=generation,y=prop,group=type,color=type))+
  # facet_wrap(~type)+
  geom_line(lwd=1)+
  geom_hline(aes(lty='Equal proportion (1/18)', yintercept=1/18))+
  scale_linetype_manual( name =NULL,
                         values = 2,
                         guide = guide_legend(override.aes = list(color = "black")))+
  # geom_rect(aes(xmin=0,xmax=8,ymin=typePropMeanSE$ymin,ymax=typePropMeanSE$ymax, fill='gray',alpha=0.5))+
  labs(y='Proportion of new species', title='Distribution of new species\' types over generations',x='Generation')+
  theme_bw()

propTypesOverGensSeparated <- genTypes |>
  ggplot(aes(x=generation,y=prop,group=type,color=type))+
  facet_wrap(~type)+
  geom_line(lwd=1)+
  geom_hline(aes(lty='Equal proportion (1/18)', yintercept=1/18))+
  # geom_rect(aes(xmin=0,xmax=8,ymin=typePropMeanSE$ymin,ymax=typePropMeanSE$ymax, fill='gray',alpha=0.5))+
  labs(y='Proportion of new species', title='Distribution of new species\' types over generations',x='Generation')+
  scale_linetype_manual( name =NULL,
                         values = 2,
                         guide = guide_legend(override.aes = list(color = "black")))+
  scale_color_discrete(guide='none')+
  theme_bw()

meanPropTypes <- genTypeMeans |> 
  ggplot(aes(x=type,y=meanProp,fill=type))+
  geom_col()+
  scale_fill_discrete(guide='none')+
  geom_errorbar(aes(ymin=meanProp-se,ymax=meanProp+se))+
  labs(y='Mean proportion of new species',title='Mean proportion of new species of each type per generation',x='Type')+
  theme_bw()+
  theme(panel.grid.major.x=element_blank())

totalSpeciesPerType <- typeExpand |> 
  group_by(type) |> 
  summarise(n=n()) |> 
  ggplot(aes(x=type,y=n,fill=type))+
  geom_col()+
  scale_fill_discrete(guide='none')+
  labs(y='Number of species',title='Total number of species per type',subtitle='As of generation 7',x='Type')+
  theme_bw()+
  theme(panel.grid.major.x=element_blank())

newSpeciesNumOverGens <- pokemon |> 
  select(generation) |> 
  group_by(generation) |> 
  summarise(n=n()) |> 
  ggplot(aes(x=generation,y=n))+
  geom_col()+
  labs(y='Number of new species', title='Number of new species introduced per generation',x='Generation')+
  theme_bw()

### Regressions ####

#### Type-proportions over Generations ####

propFits <- list()

for (t in 1:length(unique(genTypes$type))){
  typet <- unique(genTypes$type)[t]
  genType <- genTypes |> 
    filter(type==typet) |> 
    mutate(generation=as.numeric(generation)) |> 
    select(!type)
  propFits[[t]] <- lm(prop ~ generation, data=genType)
}
names(propFits) <- unique(genTypes$type)

genCoefs <- coeftest(propFits[[1]], vcov.=vcovHC)[2,] |> 
  as_tibble(rownames='stat') |> 
  pivot_wider(names_from=stat,values_from=value) |> 
  mutate(type=unique(genTypes$type)[1],.before=1) |> 
  mutate(R2=summary(propFits[[1]])$r.squared)

for (t in 2:length(unique(genTypes$type))){
  temp <- coeftest(propFits[[t]], vcov.=vcovHC)[2,] |> 
    as_tibble(rownames='stat') |> 
    pivot_wider(names_from=stat,values_from=value) |> 
    mutate(type=unique(genTypes$type)[t],.before=1) |> 
    mutate(R2=summary(propFits[[t]])$r.squared)
  genCoefs <- genCoefs |> 
    bind_rows(temp)
}

#### With Species-number Control ####

propFits_num <- list()
for (t in 1:length(unique(genTypes$type))){
  temp <- typeExpand |> 
    group_by(generation) |> 
    summarise(outOf=n())
  typet <- unique(genTypes$type)[t]
  genType <- genTypes |> 
    filter(type==typet) |> 
    bind_cols(temp[,'outOf']) |> 
    mutate(generation=as.numeric(generation)) |> 
    select(!type)
  propFits_num[[t]] <- lm(prop ~ generation + outOf, data=genType)
}
names(propFits_num) <- unique(genTypes$type)

genCoefs_num <- coeftest(propFits_num[[1]], vcov.=vcovHC)[2:3,] |>
  as_tibble(rownames='predictor') |>
  mutate(type=unique(genTypes$type)[1],.before=1) |> 
  mutate(R2=summary(propFits_num[[1]])$r.squared)

for (t in 2:length(unique(genTypes$type))){
  temp <- coeftest(propFits_num[[t]], vcov.=vcovHC)[2:3,] |>
    as_tibble(rownames='predictor') |>
    mutate(type=unique(genTypes$type)[t],.before=1) |> 
    mutate(R2=summary(propFits_num[[t]])$r.squared)
  genCoefs_num <- genCoefs_num |>
    bind_rows(temp) |> 
    mutate(predictor=str_replace_all(predictor,'outOf','new species'))
}

## Attack Modifiers and Types ####

### Data Visualization ####

attackModsByType <- pokemon |> 
  select(type1,type2,starts_with('against'),pokedex_number) |> 
  pivot_longer(cols=starts_with('against'),names_to = 'against',values_to='strength') |> 
  pivot_longer(cols=starts_with('type'), names_to='type_num',values_to='type') |> 
  mutate(against=str_replace(against,'against_','')) |> 
  drop_na() |> 
  group_by(type,against) |> 
  summarise(m=mean(strength),.groups='drop_last') |> 
  
  ggplot(aes(x=against,y=m,fill=against))+
  facet_wrap(~type)+
  geom_col()+
  scale_fill_discrete(guide='none')+
  theme_linedraw()+
  theme(axis.text.x=element_text(angle=45, vjust = 1, hjust=1),panel.grid.major.x=element_blank())+
  labs(y='Attacker strength multiplier', x='Opponent type',title='Attack modifiers against each type, by attacker type')

### Model for Predicting Type from Attack Modifiers ####

#### Training and Testing Model ####

attackTypes <- pokemon |> select(type1, starts_with('against'))

attackRec <- recipe(type1 ~ ., data = attackTypes)

attackFolds <- vfold_cv(attackTypes, v=10)

forestSpec <- rand_forest() |> # mtry = floor(sqrt(.cols())) [the default]
  set_engine('randomForest',importance=T) |> 
  set_mode('classification')

attackWork <- workflow() |>
  add_model(forestSpec) |>
  add_recipe(attackRec)

attackFits <- fit_resamples(attackWork, attackFolds, control = control_resamples(save_pred=T))

#### Visualizing Performance ####

performanceMetrics <- collect_metrics(attackFits)

confMat <- conf_mat(collect_predictions(attackFits), truth=type1, estimate=.pred_class)

confMatHeatmap <- autoplot(confMat, type='heatmap')+
  scale_fill_gradient(low='white',high='blue',guide='none')+
  labs(x='True primary type',y='Predicted primary type',title='Confusion matrix for species\' primary types',subtitle='Predicted from opponent-type attack multipliers')+
  theme_bw()

## Writing Plots to File ####

if(overwriteFigures){
  figs <- c('propTypesOverGensCombined',
            'propTypesOverGensSeparated',
            'meanPropTypes',
            'totalSpeciesPerType',
            'newSpeciesNumOverGens',
            'confMatHeatmap',
            'attackModsByType')
  
  fignames <- c('new species type proportions over gens - combined',
                'new species type proportions over gens - separated',
                'new species type mean props',
                'total species per type',
                'total new species over gens',
                'confusion matrix heatmap of model predictions',
                'attack modifiers by attacker and opponent type')
  
  folderpath <- paste(getwd(),'/figures/', sep='')
  dir.create(folderpath)
  
  widths <- c(rep(2200,6),4300)
  heights <- c(rep(1440,6),1600)
  ress <- c(rep(250,7))
  
  for (i in 1:length(figs)) {
    fig <- figs[i]
    png(filename=paste(folderpath, fignames[i], '.png', sep=''), width=widths[i], height=heights[i], res=ress[i])
    print(get(fig))
    dev.off()
  }
}


# Chimp ####

#        .--.  .-'''-.  .--.
#       /."".v'.-. .-.`v.""\\
#       ||  / / O| | O\ \  ||
#       \\_/| \__| |__/ |\_//
#        `-'\  .-n-n-.  /`-'
#         _.-\/       \/-._
#       .'   (\`.___.'/)   `.
#      /      \`.___.'/      `.
#     /        `.___.'         \
#     |     |             \     \
#     |     |   .      .  |\     \
#     |     |             | \     \
#      \     \            |  \     \
#       \     \           |.' `.    \
#        `.    \         .'     `.   \
#   _.._   `.   `-. ___ /        /`.  `.
# '    "-._|`\    `.__)       .'  /    `.
# |         `-.\     \/      .'   / /\  )|\.
#  \          _/ / /|/     .'    (_/ / / | \)
#   `._      (__/_/-/   ..'         (_/| |\_)
#      ``--._____.-(     `.            `-'
#                   `--.   `.
#                     (_/\ \\\
#                        /_///
