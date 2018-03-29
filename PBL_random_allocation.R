PBL_random_allocation_function<-function(){
	#data preparation
	file_questionID <- choose.files(default = paste("\\questionID.csv", sep=""), 
				caption = "Please select the questionID file.")
	file_studentID <- choose.files(default = paste("\\studentID.csv", sep=""), 
				caption = "Please select the studentID file.")
	file_question_times <- choose.files(default = paste("\\question_times.csv", sep=""), 
				caption = "Please select the question_times file.")
	outputs_fold <- choose.dir(default = paste("\\outputs_fold", sep=""), 
		caption = "Please select the outputs_fold.")
	questionID_list <- read.table(file_questionID, header=TRUE, as.is=FALSE, sep = ",")
	studentID_list <- read.table(file_studentID, header=TRUE, as.is=FALSE, sep = ",")
	question_times_list <- read.table(file_question_times, header=TRUE, as.is=FALSE, sep = ",")
	question_number <-length(questionID_list$ID_list)
	student_number <-length(studentID_list$ID_list)
	question_times <-question_times_list$question_times[1]
	question_range<-c(1:question_number)
	origin_question_range<-c(1:question_number)
	question_number_need<-ceiling(question_times*question_number/student_number)
	studentIDlist <- rep(list(NA), student_number)
	student_term_list<-rep(list(NA), student_number)
	question_times_list <- rep(list(question_times), question_number)
	missed_question_list <- c()
	missed_question_list_number=0
	#reorder of student names
	studentID_list$ID_list<-order(rnorm(c(1:student_number)))
	final_studentID_list<-studentID_list[order(studentID_list[,1]),] 
	#start selection part
	for(index in 1:student_number){
		#browser();
		if(question_number>= question_number_need){
			#do question selection
			studentIDlist[[index]]<-sample(question_range, size= question_number_need)
			#process output file part
			student_term_list[[index]][1]<-paste(final_studentID_list[[2]][index])
			for(term_index in 1:question_number_need){
				student_term_list[[index]][term_index +1]<-paste(questionID_list[[2]][studentIDlist[[index]][term_index]])
			}
			#count question number
			for(q_index in 1:question_number_need){
				question_times_list[[studentIDlist[[index]][q_index]]]=question_times_list[[studentIDlist[[index]][q_index]]]-1
				if(question_times_list[[studentIDlist[[index]][q_index]]]==0){
					question_range<-question_range[!question_range==studentIDlist[[index]][q_index]]
					question_number= question_number-1
				}
			}
		}else{
			dif_number=question_number_need -question_number
			studentIDlist[[index]]<-sample(question_range, size= question_number)
			#create temp range
			temp_question_range<-origin_question_range
			for(temp_index in 1:question_number){
				temp_question_range<-temp_question_range[!temp_question_range==question_range[temp_index]]
			}
			#do question selection
			studentIDlist[[index]]<-c(studentIDlist[[index]],sample(temp_question_range, size= dif_number))
			#process output file part
			student_term_list[[index]][1]<-paste(final_studentID_list[[2]][index])
			for(term_index in 1:question_number_need){
				student_term_list[[index]][term_index +1]<-paste(questionID_list[[2]][studentIDlist[[index]][term_index]])
			}
			#count question number
			for(q_index in 1:question_number){
				question_times_list[[studentIDlist[[index]][q_index]]]=question_times_list[[studentIDlist[[index]][q_index]]]-1
				if(question_times_list[[studentIDlist[[index]][q_index]]]==0){
					question_range<-question_range[!question_range==studentIDlist[[index]][q_index]]
					question_number= question_number-1
				}
			}
		}
	}
	#create question_selection_list info
	
	#create missed_question_list info
	for(index in 1:student_number){
		if(question_times_list[[index]]==question_number_need){
		cat(paste("question ",index," was not selected",sep=""))
		missed_question_list<-c(missed_question_list,index)
		missed_question_list_number=missed_question_list_number+1
		}
	}
	if(missed_question_list_number!=0){
		missed_question_c<- c()
		for(missed_index in 1:length(missed_question_list)){
			current_question_missed<- paste("question ",missed_index," :",questionID_list[[2]][missed_index],sep="")
			missed_question_c<- c(missed_question_c,current_question_missed)
		}
		info_missed<- paste(paste(missed_question_c), "was missed;",sep="")
	}else{
		info_missed<- paste("no questions were missed", sep="")
		
	}
	
	write.table(info_missed, file = paste(outputs_fold,"\\", "info_missed.txt", sep=""),
			quote = FALSE,row.names = FALSE,col.names = FALSE)
	write.csv(student_term_list, file = paste(outputs_fold,"\\", "student_term_list.csv", sep=""),
			quote = FALSE,row.names = FALSE)
	question_selected <- read.table(paste(outputs_fold,"\\", "student_term_list.csv", sep=""), header=FALSE, sep=",")
	question_selected <-question_selected[-1,]
	write.csv(question_selected, file = paste(outputs_fold,"\\", "question_allocation.csv", sep=""),
			quote = FALSE,row.names = FALSE)
	file.remove(paste(outputs_fold,"\\", "student_term_list.csv", sep=""))
}

PBL_random_allocation_function()