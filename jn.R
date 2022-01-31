library(stringr)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(ggforce)

setwd("C:/Users/oldst/Desktop/graph-builder/") 
source("constant.R")
source("lib.R")
setwd("C:/Users/oldst/Desktop/graph-builder/data/Jetson/Nano") #

csv_data = read.csv("jn_results_mobilenet_v1_GPU.csv") 
df = process_data("jn_results_mobilenet_v1_GPU.csv", csv_data, acc_v1_top1)
csv_data = read.csv("jn_results_mobilenet_v2_GPU.csv") 
df_temp = process_data("jn_results_mobilenet_v2_GPU.csv", csv_data, acc_v2_top1)
df <- rbind(df, df_temp)
csv_data = read.csv("jn_results_mobilenet_v3_GPU.csv") 
df_temp = process_data("jn_results_mobilenet_v3_GPU.csv", csv_data, acc_v3_top1)
df <- rbind(df, df_temp)

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


