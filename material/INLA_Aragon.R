#' 
### --- 0. Loading the packages --- ####
library(sf)
library(spdep)
library(lattice)
library(latticeExtra)
library(viridis)
library(gridExtra)
library(RColorBrewer)
library(INLA)
library(sf)
library(ggthemes)


#BiocManager::install(c("graph", "Rgraphviz"), dep=TRUE)
library(Rgraphviz)
library(graph)
library(leaflet)
library(ggplot2)
library(patchwork)

### --- 1. Loading the data --- ####
#Dataset
load("material/Aragon.Rdata") #datos
data <- Aragon.df
Nareas <- length(data[,1]) #número de áreas

# sf
aragon_shape <- read_sf("material/aragon.shp") 

aragon_shape <- st_set_crs(aragon_shape, 27700)

ggplot(aragon_shape) +
  geom_sf() +
  theme_void()


### --- 2. Checking if the data of the sp and the data.frame match --- ####
# The order of the areas needs to be the same between 
# the data and the spatial polygon object obtained importing 
# the shapefile, so we re-order the data.
data$CODMUNI == aragon_shape$CODMUNI 
aragon_shape <- aragon_shape[ order(aragon_shape$CODMUNI), ]


### ----- 2.1. Plotting the data --- ####
### ------- 2.1.1. ISQ --- ####
aragon_shape$isq_raw <- data$O/data$E
isq_raw.cutoff<- c(0.5, 0.9, 1.3, 8,  1.8)
isq_raw_disc = cut(aragon_shape$isq_raw,
                   breaks         = isq_raw.cutoff,
                   include.lowest = TRUE)

aragon_shape$isq_raw_disc <- isq_raw_disc

plot(aragon_shape["isq_raw_disc"], max.plot = 1,
     pal =  brewer.pal(9,'Blues')[c(2,4,6,8)],
     key.pos = 1)


### --- 3. Defining neighbor relation --- ####
temp <- poly2nb(aragon_shape)

#This create a file called ``ARG.graph'' with the graph for INLA
nb2INLA("ARG.graph", temp)

### ----- 3.1. Plotting the generated graph --- ####
H <- inla.read.graph(filename="ARG.graph")
image(inla.graph2matrix(H),xlab="",ylab="") #identifica los vecinos de cada barrio, el barrio 1 tiene por vecinos el 1, 19,20,21,22, etc.

### ----- 3.2. More plotting --- ####
plot(H)

### ----- 3.3. Plotting the neighbors --- ####
plot_map_neig_ggplot <- function(neig, aragon_shape, temp) {
  
  # Base map of London
  p <- ggplot() +
    geom_sf(data = aragon_shape, fill = "white", color = "black") +
    
    # Highlight the selected region in red
    geom_sf(data = aragon_shape[neig, ], fill = "red", color = "black") +
    
    # Highlight the neighbors in blue
    geom_sf(data = aragon_shape[temp[[neig]], ], fill = "blue", color = "black") +
    
    # Set the theme
    theme_minimal() +
    ggtitle(paste("Selected region:", aragon_shape$NOMBRE[neig])) +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Print the plot
  print(p)
  
  # Print information about the selected region and its neighbors
  cat("You have selected", aragon_shape$NOMBRE[neig], "and its neighbors are:", "\n")
  cat(aragon_shape$NOMBRE[temp[[neig]]], "\n")
}
#esta funcion identifica los vecinos de cada vecindario en concreto

plot_map_neig_ggplot(neig = 30, aragon_shape, temp = temp) 
plot_map_neig_ggplot(neig = 25, aragon_shape, temp = temp)
plot_map_neig_ggplot(neig = 23, aragon_shape, temp = temp)

### --- 4. Fitting a model with bym effect --- ####
### ----- 4.0. Adding ids for the random effects --- ####
S <- U <- seq(1,729) # crea dos veces el mismo índice, uno para el efecto aleatorio espacial y otro para el iid. 
data <- cbind(data, S, U)


### ----- 4.1. Formula --- ####
formula <- O ~ 1 + f(S, 
                     model       = "besag", 
                     graph       = H,
                     scale.model = TRUE,
                     hyper       = 
                       list(prec = list(prior="loggamma",param = c(1,0.001)))) +
  f(U, 
    model       = "iid",
    hyper       = 
      list(prec = list(prior="loggamma",param = c(1,0.001))))


### ----- 4.2. Model --- ####
mod.isq <- inla(formula,
                family          = "poisson",
                data            = data,
                E               = E, #es el offset, se puede poner como offset o como esperados
                control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                control.predictor = list(compute=TRUE, cdf=c(log(1)))) #cdf = cumulative density function, calcula la probabilidad de que la función de desnidad acumulada sea mayor que (valor introducido, en este caso 1). En logaritmo porque a INLA le gusta más y porque la prior es loggamma. Se puede calcular después. Si pones verbose = TRUE aparece el proceso de computación, si no estará en el log file. 

summary(mod.isq)



### ----- 4.3. Posterior distribution of the random effects --- ####
aragon_shape$SPmean <- round(mod.isq$summary.random$S[["mean"]], 4)
aragon_shape$SPsd <- round(mod.isq$summary.random$S[["sd"]],5)

#Mean posterior distribution
a <- ggplot(data = aragon_shape) +
  geom_sf(aes(fill = SPmean), color = "white") +
  scale_fill_viridis_c(option = "magma",begin = 0.1, direction = -1) +
  theme_void() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 20,
                                  face = "bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "Gray60")) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1.5)) +  # Adjusting the legend size
  ggtitle("Mean posterior of S") 

#Sd posterior distribution
b <- ggplot(data = aragon_shape) +
  geom_sf(aes(fill = SPsd), color = "white") +
  scale_fill_viridis_c(option = "magma",begin = 0.1, direction = -1) +
  theme_void() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 20,
                                  face = "bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "Gray60")) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1.5)) +  # Adjusting the legend size
  ggtitle("SD posterior of S") 

a | b


aragon_shape$UPmean <- round(mod.isq$summary.random$U[["mean"]], 4)
aragon_shape$UPsd <- round(mod.isq$summary.random$U[["sd"]],5)

#Mean posterior distribution
c <- ggplot(data = aragon_shape) +
  geom_sf(aes(fill = UPmean), color = "white") +
  scale_fill_viridis_c(option = "magma",begin = 0.1, direction = -1) +
  theme_void() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 20,
                                  face = "bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "Gray60")) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1.5)) +  # Adjusting the legend size
  ggtitle("Mean posterior of U") 

#Sd posterior distribution
d <- ggplot(data = aragon_shape) +
  geom_sf(aes(fill = UPsd), color = "white") +
  scale_fill_viridis_c(option = "magma",begin = 0.1, direction = -1) +
  theme_void() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 20,
                                  face = "bold"),
        legend.title = element_blank(),
        plot.subtitle = element_text(color = "blue"),
        plot.caption = element_text(color = "Gray60")) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 1.5)) +  # Adjusting the legend size
  ggtitle("SD posterior of U") 

c | d
### --- 4.4. Posterior distribution of suicides mortality --- ####
aragon_shape$isq_mean <- mod.isq$summary.fitted.values$mean # mean
aragon_shape$isq_sd <- mod.isq$summary.fitted.values$sd #s
aragon_shape$isq_median <- mod.isq$summary.fitted.values$`0.5quant` # median
aragon_shape$isq_q025 <- mod.isq$summary.fitted.values$`0.025quant` # quantile
aragon_shape$isq_q975 <- mod.isq$summary.fitted.values$`0.975quant` # quantile
aragon_shape$isq_p1 <- 1 - mod.isq$summary.fitted.values$`1cdf` # probability to be greater than 1

### --- 4.5. Posterior distribution of suicides SMR with cutoff--- ####
## Also, the probability for SMR to be greater than 1.

isq_disc1 <- ggplot(data = aragon_shape) +
  geom_sf(aes(fill = isq_mean), color = "white") +
  scale_fill_viridis_b(option = "magma", begin = 0.1, direction = -1) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      hjust = 0.5,
      color = "Gray40",
      size = 20,
      face = "bold"
    ),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "blue"),
    plot.caption = element_text(color = "Gray60")
  ) +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 1.5)) +
  ggtitle("Mean posterior RME")


isq_disc2 <- ggplot(data = aragon_shape) +
  geom_sf(aes(fill = isq_p1), color = "white") +
  scale_fill_viridis_b(option = "magma", begin = 0.1, direction = -1) +
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(
      hjust = 0.5,
      color = "Gray40",
      size = 20,
      face = "bold"
    ),
    legend.title = element_blank(),
    plot.subtitle = element_text(color = "blue"),
    plot.caption = element_text(color = "Gray60")
  ) +
  guides(fill = guide_colourbar(barwidth = 10, barheight = 1.5)) +
  ggtitle("P(RME>1)")


isq_disc1 | isq_disc2



inla_results <- mod.isq$summary.fixed
inla_results


bugs_results <- res.mod1$summary
bugs_results

