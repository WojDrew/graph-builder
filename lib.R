
rows_per_inference = 6

process_data <- function(file_name, csv_data, acc, inferences_per_model) {
	rows_per_model = rows_per_inference * inferences_per_model
	
	num_of_models = nrow(csv_data) / rows_per_model
	x = seq(1,num_of_models,1)
	
	df = get_model_details(csv_data, file_name, rows_per_model)
	df <- cbind(df, x)
	
	df = add_acc(file_name, df, acc)
	df = add_lat(df, csv_data, inferences_per_model)
	
	if (grepl("jn", file_name))
		df = jn_fix_float(df)
	
	df = fix_labels(df)
	
	return(df)
	
}

fix_labels <- function(df) {
	for (i in seq(from=1, to=nrow(df), by=1)) {
		str = df[i, "model_labels"]
		for (rem in str_to_remove) {
			str = str_remove(str, rem)
		}
		df[i, "model_labels"] = str	
	}
	return(df)
}

jn_fix_float <- function(df) {
	for (i in seq(from=1, to=nrow(df), by=1)) {
		if (df[i, "precision"]  == "float32")
			df[i, "precision"] = "float16"
	}
	return(df)
}

get_model_details <- function(csv_data, file_name, rows_per_model) {
	model_labels = c()
	precision = c()
	model = c()
	alpha = c()
	rho = c()
	acceleration = c()
	
	for (row in seq(from=1, to=nrow(csv_data), by=rows_per_model)) {
		model_name = csv_data[row, "ModelName"]
		model_labels = c(model_labels, model_name)
		
		if (grepl("quant", model_name)) {
			precision = c(precision, "uint8")
		} else {
			precision = c(precision, "float32")
		}
		
		if (grepl("v1", model_name)) {
			model = c(model, "v1")
		} else if (grepl("v2", model_name)) {
			model = c(model, "v2")
		} else {
			if (grepl("small", model_name)) {
				model = c(model, "v3-small")
			} else if (grepl("large", model_name) || 
				     grepl("edgetpu", model_name)) {
				model = c(model, "v3-large")
			}
		}
		
		parts = strsplit(model_name, "[_]")
		
		if (grepl("edgetpu", model_name)) {
			rho = c(rho, parts[[1]][3])
			alpha = c(alpha, parts[[1]][4])
		} else if (!grepl("v3", model_name) || grepl("jn", file_name)) {
			rho = c(rho, parts[[1]][4])
			alpha = c(alpha, parts[[1]][3])
		} else {
			rho = c(rho, parts[[1]][2])
			alpha = c(alpha, parts[[1]][3])
		}
		
		if (grepl("CPU", file_name)) {
			acceleration = c(acceleration, "CPU")
		} else if (grepl("GPU", file_name)) {
			acceleration = c(acceleration, "GPU")
		} else if (grepl("NNAPI", file_name)) {
			acceleration = c(acceleration, "NNAPI")
		}
	}
	
	df <- data.frame(model_labels, precision, model, alpha, rho, acceleration)
	return(df)
}

find_row <- function(df, alpha, rho, quant) {
	to_find = ""
	if (quant == "q")
		to_find = "uint8"
	else
		to_find = "float32"

	for (i in seq(from=1, to=nrow(df), by=1)) {
		if (as.double(df[['alpha']][[i]]) == alpha &&
		    as.double(df[['rho']][[i]]) == rho &&
		    df[['precision']][[i]] == to_find) {
			return(i)
		}
	}
	return(-1)
}

find_row_v3 <- function(df, alpha, rho, larsmal, mini, quant) {
	to_find = ""
	if (quant == "q")
		to_find = "uint8"
	else
		to_find = "float32"
	
	for (i in seq(from=1, to=nrow(df), by=1)) {
		if (as.double(df[['alpha']][[i]]) == alpha &&
		    as.double(df[['rho']][[i]]) == rho &&
		    df[['precision']][[i]] == to_find &&
		    grepl(larsmal, df[['model_labels']][[i]])) {

			if (mini == "minimalistic" &&
			    grepl(mini, df[['model_labels']][[i]]) ||
			    mini == "n" &&
			    !grepl("minimalistic", df[['model_labels']][[i]]))
				return(i)
		}
	}
	return(-1)
}

add_acc <- function(file_name, df, acc) {
	acc_to_add = c()
	to_delete = 0
	for (i in seq(from=1, to=length(acc[[1]]), by=1)) {
		acc_to_add = c(acc_to_add, 0)
	}
	for (i in seq(from=1, to=length(acc[[1]]), by=1)) {
		if (df[['model']][[1]] == "v1" || df[['model']][[1]] == "v2") {
			j = find_row(df, acc[[2]][[i]], acc[[3]][[i]], acc[[4]][[i]])
			if (j != -1)
				acc_to_add[j] = acc[[1]][[i]]
			else
				to_delete = to_delete + 1
		} else {
			j = find_row_v3(df, acc[[2]][[i]], acc[[3]][[i]],
					    acc[[4]][[i]], acc[[5]][[i]], acc[[6]][[i]])
			if (j != -1)
				acc_to_add[j] = acc[[1]][[i]]
			else
				to_delete = to_delete + 1
		}
	}
	acc = acc_to_add
	if (to_delete != 0)
		acc <- head(acc, -to_delete)
	df <- cbind(df, acc)
	return(df)
}

add_lat <- function(df, csv_data, inferences_per_model) {
	done_models = c()
	avg = 0
	latency = c()
	
	for (row in seq(from=1, to=nrow(csv_data), by=rows_per_inference)) {
		if (!csv_data[row,"ModelName"] %in% done_models) {
			done_models = c(done_models, csv_data[row,"ModelName"])
			
			if (row != 1) {
				avg = avg / inferences_per_model
				latency = c(latency, avg*1000)
				avg = 0
				
			}
		}
		avg = avg + csv_data[row, "InferenceTime"]
	}
	avg = avg / inferences_per_model
	latency = c(latency, avg*1000)
	df <- cbind(df, latency)
	return(df)
}

remove_dominated <- function(df) {
	range = 0.3
	dominated = c()
	for (i in seq(from=1, to=nrow(df), by=1)) {
		dominated = c(dominated, 0)
	}
	
	for (i in seq(from=1, to=nrow(df), by=1)) {
		for (j in seq(from=i, to=nrow(df), by=1)) {
			if (i != j && abs(df[i, "acc"] - df[j, "acc"]) < range) {
				if (df[i, "latency"] < df[j, "latency"]) {
					dominated[j] = 1
				} else {
					dominated[i] = 1
				}
			}
		}
	}
	df <- cbind(df, dominated)
	return(df)
}

fix_x <- function(df) {
	
	for (i in seq(from=1, to=nrow(df), by=1)) {
		df[i, "x"] = i
	}
	return(df)
}