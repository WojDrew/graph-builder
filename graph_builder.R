library(stringr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(ggforce)


setwd("C:/Users/oldst/Desktop/graph-builder/") #
source("constant.R")
source("lib.R")
setwd("C:/Users/oldst/Desktop/graph-builder/data") #

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
	  csv_data = read.csv(file_names[[i]]) #
	  # file_path = paste("data", file_names[[i]], sep="/")
		# csv_data = read.csv(file_path)
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

  ggplot(data=subset(df, dominated==0), aes(x=latency, y=acc)) +
    geom_point(aes(
      stroke=1.5,
      shape=acceleration,
      size=precision,#just to have precision in the legend
      color=model,
      fill=ifelse(precision == "float32", model, NA)
    )) +
    scale_shape_manual(values=c(24,21,23)) +
    scale_fill_discrete(na.value=NA, guide="none") +
    scale_color_manual(name = "model", values=c("red", "yellow" , "#000000", "blue", "#80FFFF"), aesthetics = c("colour", "fill")) +
    guides(colour = guide_legend(override.aes = list(shape = 15))) + #use a shape that is not reserved for a specific purpose
    guides(shape = guide_legend(override.aes = list(fill = "black"))) + #use a fill color that is not reserved for a specific purpose
    scale_size_manual(name = "precision", values=c(2, 2)) + #we want to have both float32 and uint8 of the same size
    guides(size = guide_legend(override.aes=list(shape = c(15,0) ))) +
    xlab("Inference latency [ms]") + ylab("Top1 accuracy [%]") + 
    geom_text_repel(aes(label = model_labels), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50', size = 3) +
    theme_bw() + theme(plot.title = element_text(size=22), axis.title = element_text(size=14), axis.text = element_text(size=13))
}

build_all_models_graph()





