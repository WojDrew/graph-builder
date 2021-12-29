# mobilenet v1 accuracy with quantized
acc_v1_top1 = list(c(41.5, 39.5, 45.5, 43.4, 47.7, 46.0, 49.8, 48.0, 56.3, 54.5, 59.1, 57.7, 61.7, 60.0, 63.3, 60.7, 62.1, 55.8, 65.3, 62.3, 67.2, 66.1, 68.4, 66.8, 65.2, 63.4, 68.0, 67.2, 70.0, 69.2, 70.9, 70.1),
			 c(0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0),
			 c(128, 128, 160, 160, 192, 192, 224, 224, 128, 128, 160, 160, 192, 192, 224, 224, 128, 128, 160, 160, 192, 192, 224, 224, 128, 128, 160, 160, 192, 192, 224, 224),
			 c("n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q"))
acc_v1_top5 = c(66.3, 64.4, 70.3, 68.5, 72.3, 71.2, 74.2, 72.8, 79.4, 77.7, 81.9, 80.4, 83.6, 82.2, 84.9, 83.2, 83.9, 78.8, 86.0, 83.8, 87.3, 86.4, 88.2, 87.0, 85.8, 84.2, 87.7, 86.7, 89.2, 88.3, 89.9, 88.9)

# mobilenet v1 accuracy without quantized
acc_v1_top1_nq = list(c(41.5, 45.5, 47.7, 49.8, 56.3, 59.1, 61.7, 63.3, 62.1, 65.3, 67.2, 68.4, 65.2, 68.0, 70.0, 70.9),
			    c(0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.5, 0.75, 0.75, 0.75, 0.75, 1.0, 1.0, 1.0, 1.0),
			    c(128, 160, 192, 224, 128, 160, 192, 224, 128, 160, 192, 224, 128, 160, 192, 224),
			    c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n"))
# mobilenet v2 accuracy with quantized
# 1.3 224 does not have quantized version
acc_v2_top1 = list(c(50.8, 50.8, 55.7, 55.7, 58.2, 58.2, 60.3, 60.3, 45.5, 45.5, 57.7, 57.7, 61.0, 61.0, 63.9, 63.9, 65.4, 65.4, 51.2, 51.2, 63.2, 63.2, 66.4, 66.4, 68.7, 68.7, 69.8, 69.8, 58.8, 58.8, 65.3, 65.3, 68.8, 68.8, 70.7, 70.7, 71.8, 71.8, 60.3, 60.3, 74.4, 75.0, 75.0),
			 c(0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 0.75, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.3, 1.4, 1.4),
			 c(128, 128, 160, 160, 192, 192, 224, 224, 96, 96, 128, 128, 160, 160, 192, 192, 224, 224, 96, 96, 128, 128, 160, 160, 192, 192, 224, 224, 96, 96, 128, 128, 160, 160, 192, 192, 224, 224, 96, 96, 224, 224, 224),
			 c("n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "q", "n", "n", "q"))
# mobilenet v2 accuracy without quantized
acc_v2_top1_nq = list(c(50.8, 55.7, 58.2, 60.3, 45.5, 57.7, 61.0, 63.9, 65.4, 51.2, 63.2, 66.4, 68.7, 69.8, 58.8, 65.3, 68.8, 70.7, 71.8, 60.3, 74.4, 75.0),
			    c(0.35, 0.35, 0.35, 0.35, 0.35, 0.5, 0.5, 0.5, 0.5, 0.5, 0.75, 0.75, 0.75, 0.75, 0.75, 1.0, 1.0, 1.0, 1.0, 1.0, 1.3, 1.4, 1.4),
			    c(128, 160, 192, 224, 96, 128, 160, 192, 224, 96, 128, 160, 192, 224, 96, 128, 160, 192, 224, 96, 224, 224),
			    c("n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n", "n"))

# mobilenet v3 accuracy without quantized
acc_v3_top1 = list(c(75.2, 73.9, 73.3, 67.5, 64.9, 65.4, 72.3, 71.3, 61.9, 73.5, 75.6),
			 c(1.0, 1.0, 0.75, 1.0, 1.0, 0.75, 1.0, 1.0, 1.0, 0.75, 1.0),
			 c(224, 224, 224, 224, 224, 224, 224, 224, 224, 224, 224),
			 c("large", "large", "large", "small", "small", "small", "large", "large", "small", "edgetpu", "edgetpu"),
			 c("n", "n", "n", "n", "n", "n", "minimalistic", "minimalistic", "minimalistic", "n", "n"),
			 c("n", "q", "n", "n", "q", "n", "n", "q", "n", "n", "n"))


c("edgetpu_224_0.75", "edgetpu_224_1.0","v3-large-minimalistic_224_1.0", "v3-large_224_0.75","v3-large_224_1.0", "v3-small-minimalistic_224_1.0","v3-small_224_0.75", "v3-small_224_1.0")

# strings to be removed from model label
str_to_remove = c("mobilenet_", "_quant", "_float", "v3-small_", "v3-large_", "v3-small-", "v3-large-", "v2_", "v1_")


file_names_CPU = c("results_mobilenet_v1_CPU.csv",
			 "results_mobilenet_v2_CPU.csv",
			 "results_mobilenet_v3_CPU.csv")

file_names_GPU = c("results_mobilenet_v1_GPU.csv",
			 "results_mobilenet_v2_GPU.csv",
			 "results_mobilenet_v3_GPU.csv")

file_names_NNAPI = c("results_mobilenet_v1_NNAPI.csv",
			   "results_mobilenet_v2_NNAPI.csv",
			   "results_mobilenet_v3_NNAPI.csv")

file_names_v1 = c("results_mobilenet_v1_CPU.csv",
			"results_mobilenet_v1_GPU.csv",
			"results_mobilenet_v1_NNAPI.csv")

file_names_v2 = c("results_mobilenet_v2_CPU.csv",
			"results_mobilenet_v2_GPU.csv",
			"results_mobilenet_v2_NNAPI.csv")

file_names_v3 = c("results_mobilenet_v3_CPU.csv",
			"results_mobilenet_v3_GPU.csv",
			"results_mobilenet_v3_NNAPI.csv")
#file names
files_names_all = c(file_names_CPU, file_names_GPU, file_names_NNAPI)

acc_all = list(acc_v1_top1, acc_v2_top1, acc_v3_top1,
		   acc_v1_top1_nq, acc_v2_top1_nq, acc_v3_top1,
		   acc_v1_top1, acc_v2_top1, acc_v3_top1)

acc_v1 = list(acc_v1_top1, acc_v1_top1_nq, acc_v1_top1)
acc_v2 = list(acc_v2_top1, acc_v2_top1_nq, acc_v2_top1)
acc_v3 = list(acc_v3_top1, acc_v3_top1, acc_v3_top1)
