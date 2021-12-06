library(stringr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(ggforce)

setwd("C:/Users/oldst/Desktop/graph-builder/")
source("constant.R")
source("lib.R")

inferences_per_model = 330

process_model_data <- function(model) {
	
	if (model == "V1") {
		file_names = file_names_v1
		acc = acc_v1
	} else if (model == "V2") {
		file_names = file_names_v2
		acc = acc_v2
	} else if (model == "V3") {
		file_names = file_names_v3
		acc = acc_v3
	}
	
	df <- data.frame()
	for (i in seq(from=1, to=length(file_names), by=1)) {
		csv_data = read.csv(file_names[[i]])
		df_temp = process_data(file_names[[i]], csv_data, acc[[i]], inferences_per_model)
		df <- rbind(df, df_temp)
	}
	return(df)
}

build_all_models_graph <- function() {
	
	df = process_model_data("V1")
	df_temp = process_model_data("V2")
	df = rbind(df, df_temp)
	df_temp = process_model_data("V3")
	df = rbind(df, df_temp)
	df = remove_dominated(df)
	df = fix_x(df)

	ggplot() +
		geom_point(data=df, aes(x=latency, y=acc, color=model, shape=acceleration, size=precision)) +
		#scale_shape_manual(values=c(21,17,23)) +  
		#scale_fill_discrete(na.value=NA, guide="none") +
		geom_text() + scale_color_manual(values=c("red", "yellow" , "#000000", "blue", "#80FFFF")) +
		xlab("Srednie opoznienie [ms]") + ylab("Top1 Trafnosc [%]") + 
		geom_text_repel(aes(x=df$latency, y=df$acc, label = df$model_labels),box.padding = 0.35, point.padding = 0.5,segment.color = 'grey50', size = 3) +
		theme_bw() + theme(plot.title = element_text(size=22), axis.title = element_text(size=14), axis.text = element_text(size=13)) +
		scale_size_manual(name = "precision", values=c(6,4))
}

build_all_models_graph()



